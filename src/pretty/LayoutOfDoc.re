/*

 //  hazelgrove/hazel@layout-into-core

  TODO: check memoization
    TODO: is fix_holes doing deep changes
    TODO: print doc diff from previous memoization

  TODO: width intervals
  TODO: wrong-but-fast implementation
  TODO: count number of docs
  TODO: strings are fixed

  TODO: cursor map generation

  */

// TODO: compute actual layout size and use instead of t_of_layout
let rec all: 'annot. Doc.t('annot) => list(Layout.t('annot)) = {
  doc => {
    switch (doc.doc) {
    | Text(string) => [Layout.Text(string)]
    | Cat(d1, d2) =>
      let ls1 = all(d1);
      let ls2 = all(d2);
      List.concat(
        List.map(l1 => List.map(l2 => Layout.Cat(l1, l2), ls2), ls1),
      );
    | Linebreak(_) => [Layout.Linebreak]
    | Align(d) => List.map(l => Layout.Align(l), all(d))
    | Annot(annot, d) => List.map(l => Layout.Annot(annot, l), all(d))
    | Fail(_) => []
    | Choice(d1, d2) => all(d1) @ all(d2)
    };
  };
};

// Note: This union is left biased
let m'_union: 'a. (Doc.m'('a), Doc.m'('a)) => Doc.m'('a) =
  (p1, p2) => {
    let cost_union = ((cost1: Cost.t, _) as t1, (cost2: Cost.t, _) as t2) =>
      if (Cost.leq(cost1, cost2)) {
        t1;
      } else {
        t2;
      };
    PosMap.union(cost_union, p1, p2);
  };

type layout_result =
  ( int /* ocaml size + 1 */,
    Array.t(int) /*position*/,
    Array.t(int) /*cost*/,
    Array.t(Layout.t(unit)),
  );

module Js = Js_of_ocaml.Js;
type memo;

let create_memo: unit => memo =
  Js.Unsafe.js_expr("function create_memo_js(mem) { return [] }");
let flush_memo: (memo) => unit =
  Js.Unsafe.js_expr(
    "function flush_memo_js(mem) {
      mem.length = 0;
    }");
let get_memo: (memo, /*key:*/ int) => layout_result = // This function is slow (~10% of the runtime)
  Js.Unsafe.js_expr(
    "function get_memo_js(mem, key) {
      var x = mem[key];
      if (x === undefined) {
        return [0, 1, [0], [0], [0]];
      } else {
        return x;
      }
    }");
let set_memo: (memo, /*key:*/ int, /*value:*/ layout_result) => unit = // This function is very slow (2x of the runtime)
  Js.Unsafe.js_expr(
    "function set_memo_js(mem, key, value) {
      mem[key] = value;
    }");

type mem_doc('annot) =
  | MemText(ref(int), memo, string)
  | MemFail
  | MemLinebreak
  | MemCat(ref(int), memo, mem_doc('annot), mem_doc('annot))
  | MemAlign(ref(int), memo, mem_doc('annot))
  | MemAnnot(ref(int), memo, 'annot, mem_doc('annot))
  | MemChoice(ref(int), memo, mem_doc('annot), mem_doc('annot));

let mk_gen = (): ref(int) => ref(0);
let mk_text = (s: string): mem_doc('annot) => MemText(mk_gen(), create_memo(), s);
let mk_fail = (): mem_doc('annot) => MemFail;
let mk_linebreak = (): mem_doc('annot) => MemLinebreak;
let mk_cat = (d1: mem_doc('annot), d2: mem_doc('annot)): mem_doc('annot) => MemCat(mk_gen(), create_memo(), d1, d2);
let mk_align = (d: mem_doc('annot)): mem_doc('annot) => MemAlign(mk_gen(), create_memo(), d);
let mk_annot = (a: 'annot, d: mem_doc('annot)): mem_doc('annot) => MemAnnot(mk_gen(), create_memo(), a, d);
let mk_choice = (d1: mem_doc('annot), d2: mem_doc('annot)): mem_doc('annot) => MemChoice(mk_gen(), create_memo(), d1, d2);

module EqHash = {
  type t = Doc.t(unit);
  let equal = (===);
  let hash = Hashtbl.hash;
};

module EqHashtbl = Hashtbl.Make(EqHash);

let mk_mem_doc = (old: Doc.t('annot)): mem_doc('annot) => {
  let seen: EqHashtbl.t(mem_doc('annot)) = EqHashtbl.create(0);
  let rec go = (old: Doc.t('annot)): mem_doc('annot) => {
    switch (EqHashtbl.find_opt(seen, Obj.magic(old))) {
    | Some(new_doc) => new_doc
    | None =>
      let new_doc =
        switch (old.doc) {
        | Text(s) => mk_text(s)
        | Fail(_) => mk_fail() // TODO: handle fail argument
        | Cat(old1, old2) => mk_cat(go(old1), go(old2))
        | Linebreak(_) => mk_linebreak()
        | Align(old) => mk_align(go(old))
        | Annot(annot, old) => mk_annot(annot, go(old)) // TODO: handle annotation
        | Choice(old1, old2) => mk_choice(go(old1), go(old2))
        };
      EqHashtbl.add(seen, Obj.magic(old), new_doc);
      new_doc;
    };
  };
  go(old);
};
let mk_mem_doc: 'annot. Doc.t('annot) => mem_doc('annot) = mk_mem_doc;

let count = ref(0);
let mem_count = ref(0);
let linebreak_cost =
  PosMap.singleton(0, (Cost.mk_height(1), Layout.Linebreak));

let gensym: ref(int) = ref(0);

// * Discovery:
//
//       { let a = 0; [|a + 0, a + 1, a + 2, a + 3, a + 4, a + 5, a + 6, a + 7, a + 8, a + 9 |] }
//
//   is faster than
//
//       [|0, 1, 2, 3, 4, 5, 6, 7, 8, 9 |]
//
//   Cause:  js_of_ocaml translates the second code into a global variable and a
//   reference to it. js_of_ocaml also likes to put .slice() on references to
//   array (global?) variables. This can make it slower than just locally
//   computing the result.

///////////////////////////////

// Possible interface types:
//
// mylist
// List.t((int, int))
// Array.t((int, int))
// Array.t(int) (interleaved)
// (Array.t(int), Array.t(int)) (with or without passing both)
// (int, Array.t(int))

// with or without .length (separate param or element of array)
// use size+1 (b/c less aritmetic)
// prealloc the array or not

// TODO: parallel arrays due to heterogenious nature
// TODO: parallel arrays allow reuse of "pos" array


//////////

// Parallel with external size

// 29ns
// 34-36ns (complete layout_map)


// Omits pos argument
let layout_map_align =
  Obj.magic(Js.Unsafe.js_expr(
    "function layout_map_align_js(size, input) {
      var output = new Array(size);
      output[0] = 0;
      for (var i =  1; i < size; i++) {
        var res = input[i]
        output[i] = Align_share(res);
      }
      return output;
      \"layout_map_align\";
    }
    "
  ));
let layout_map_align:
  'annot. (int, Array.t(Layout.t('annot))) => Array.t(Layout.t('annot)) = layout_map_align;

let layout_map_annot =
  Obj.magic(Js.Unsafe.js_expr(
    "function layout_map_annot_js(annot, size, input) {
      var output = new Array(size);
      output[0] = 0;
      for (var i =  1; i < size; i++) {
        var res = input[i]
        output[i] = Annot_share(annot, res); // TODO: apply wrapper top
      }
      return output;
      \"layout_map_annot\";
    }
    ",
  ));
let layout_map_annot:
  'annot. ('annot, int, Array.t(Layout.t('annot))) => Array.t(Layout.t('annot)) = layout_map_annot;

let layout_merge =
  Obj.magic(Js.Unsafe.js_expr(
    "function layout_merge_js(size1, pos1, cost1, res1, size2, pos2, cost2, res2) {
      var js_size = size1;
      var end = size1 + size2 | 0;
      end = end - 1 | 0;
      var pos = new Array(end);
      pos[0] = 0;
      var cost = new Array(end);
      cost[0] = 0;
      var res = new Array(end);
      res[0] = 0;
      var i = 1;
      var i1 = 1;
      var i2 = 1;
      while (i1 < size1 && i2 < size2) {
        if (pos1[i1] < pos2[i2]) {
          pos[i] = pos1[i1];
          res[i] = res1[i1];
          cost[i] = cost1[i1];
          i1 = i1 + 1 | 0;
        } else if (pos1[i1] > pos2[i2]) {
          pos[i] = pos2[i2];
          res[i] = res2[i2];
          cost[i] = cost2[i2];
          i2 = i2 + 1 | 0;
        } else {
          // TODO: res1[i1] <=> res2[i2]
          // Note: `<=` makes choice be left biased
          if (cost1[i1] <= cost2[i2]) {
            pos[i] = pos1[i1];
            cost[i] = cost1[i1];
            res[i] = res1[i1];
          } else {
            pos[i] = pos2[i2];
            cost[i] = cost2[i2];
            res[i] = res2[i2];
          }
          i1 = i1 + 1 | 0;
          i2 = i2 + 1 | 0;
        }
        i = i + 1 | 0;
      }
      while (i1 < size1) {
        pos[i] = pos1[i1];
        res[i] = res1[i1];
        cost[i] = cost1[i1];
        i++;
        i1++; // TODO: |0
      }
      while (i2 < size2) {
        pos[i] = pos2[i2];
        res[i] = res2[i2];
        cost[i] = cost2[i2];
        i++;
        i2++;
      }
      return [0, Math.min(i, 11), pos, cost, res];
    }
    "
  ));
let layout_merge:
  'annot.
  (
    int,
    Array.t(int),
    Array.t(int),
    Array.t(Layout.t('annot)),
    int,
    Array.t(int),
    Array.t(int),
    Array.t(Layout.t('annot))
  ) =>
  (int, Array.t(int), Array.t(int), Array.t(Layout.t('annot))) = layout_merge;

let layout_fold =
  Obj.magic(Js.Unsafe.js_expr(
    "function layout_fold_js(benchmark, width, pos, f2, size1, pos1, cost1, res1) {
      if (size1 == 1) { return [0, 1, [0], [0], [0]]; }
      var xxx = new_layout_of_doc_go_share(benchmark, width, pos1[1], f2); // TODO: add cost1[i] to each of xxx
      {
        var xxx_len = xxx[1];
        var xxx_cost = xxx[3];
        var xxx_res = xxx[4];
        for (var j = 1; j < xxx_len; j++) {
          xxx_cost[j] = xxx_cost[j] + cost1[j] | 0;
          xxx_res[j] = Cat_share(res1[j], xxx_res[j]);
        }
      }
      var i = 2;
      while (i < size1) {
        var p = pos1[i];
        var yyy = new_layout_of_doc_go_share(benchmark, width, p, f2);
        var yyy_len = yyy[1];
        var yyy_cost = yyy[3];
        var yyy_res = yyy[4];
        for (var j = 1; j < yyy_len; j++) {
          yyy_cost[j] = yyy_cost[j] + cost1[j] | 0;
          yyy_res[j] = Cat_share(res1[j], yyy_res[j]);
        }
        xxx = layout_merge_share(xxx[1], xxx[2], xxx[3], xxx[4], yyy[1], yyy[2], yyy[3], yyy[4]);
        i = i + 1 | 0;
      }
      return xxx;
    }
    "
  ));
let layout_fold:
  'annot.
  (
    bool,
    int,
    int,
    mem_doc('annot),
    int,
    Array.t(int),
    Array.t(int),
    Array.t(Layout.t('annot))
  ) =>
  (int, Array.t(int), Array.t(int), Array.t(Layout.t('annot))) = layout_fold;

let rec new_layout_of_doc_go =
        (~benchmark: bool, ~width: int, ~pos: int, x: mem_doc('annot))
        : layout_result => {
  count := count^ + 1;
  switch (x) {
  | MemText(gen, memo, text) =>
    if (benchmark && gen^ != gensym^) { flush_memo(memo); gen := gensym^; }
    // TODO: optimize the memo here?
    let memo_key = pos * 80 + width;
    let (memo_s, _, _, _) as m = get_memo(memo, memo_key);
    if (memo_s != 1) {
      m
    } else {
      // TODO: should we cache the string length in MemText?
      let new_pos = pos + String.length(text);
      let r =
        if (!benchmark) {
          if (new_pos > width) {
            (1, [||], [||], [||])
            // TODO: optimize to avoid memoization when possible
          } else {
            (2, [|new_pos|], [|0|], [|Layout.Text(text)|])
          }
        } else {
          (
            11,
            [|
              new_pos + 0,
              new_pos + 1,
              new_pos + 2,
              new_pos + 3,
              new_pos + 4,
              new_pos + 5,
              new_pos + 6,
              new_pos + 7,
              new_pos + 8,
              new_pos + 9,
            |],
            [|0, 0, 0, 0, 0, 0, 0, 0, 0, 0|],
            [|
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
              Layout.Text(text),
            |],
          )
        };
      set_memo(memo, memo_key, r);
      r
    };
  | MemFail =>
    // We can return without memoization only because there are no pointer equality concerns
    (1, [||], [||], [||])
  | MemLinebreak =>
    // We can return without memoization only because there are no pointer equality concerns (is this actually valid?)
    (2, [|0|], [|1|], [|Layout.Linebreak|])
  | MemAlign(gen, memo, f) =>
    if (benchmark && gen^ != gensym^) { flush_memo(memo); gen := gensym^; }
    let memo_key = pos * 80 + width;
    let (memo_s, _, _, _) as m = get_memo(memo, memo_key);
    if (memo_s != 1) {
      m
    } else {
      let (out1s, out1p, out1c, out1r) = new_layout_of_doc_go(~benchmark, ~width=width - pos, ~pos=0, f);
      let out = layout_map_align(out1s, out1r);
      let r = (out1s, out1p, out1c, out);
      set_memo(memo, memo_key, r);
      r
    };
  | MemAnnot(gen, memo, annot, f) =>
    // TODO: optimize to avoid memoization when possible
    if (benchmark && gen^ != gensym^) { flush_memo(memo); gen := gensym^; }
    let memo_key = pos * 80 + width;
    let (memo_s, _, _, _) as m = get_memo(memo, memo_key);
    if (memo_s != 1) {
      m
    } else {
      let (out1s, out1p, out1c, out1r) = new_layout_of_doc_go(~benchmark, ~width, ~pos, f);
      let out = layout_map_annot(annot, out1s, out1r);
      let r = (out1s, out1p, out1c, out);
      set_memo(memo, memo_key, r);
      r
    };
  | MemCat(gen, memo, f1, f2) =>
    // TODO: maybe without memoization?
    if (benchmark && gen^ != gensym^) { flush_memo(memo); gen := gensym^; }
    let memo_key = pos * 80 + width;
    let (memo_s, _, _, _) as m = get_memo(memo, memo_key);
    if (memo_s != 1) {
      m
    } else {
      let (out1s, out1p, out1c, out1r) = new_layout_of_doc_go(~benchmark, ~width, ~pos, f1);
      let r = layout_fold(benchmark, width, pos, f2, out1s, out1p, out1c, out1r);
      set_memo(memo, memo_key, r);
      r
    };
  | MemChoice(gen, memo, f1, f2) =>
    if (benchmark && gen^ != gensym^) { flush_memo(memo); gen := gensym^; }
    let memo_key = pos * 80 + width;
    let (memo_s, _, _, _) as m = get_memo(memo, memo_key);
    if (memo_s != 1) {
      m
    } else {
      let (out1s, out1p, out1c, out1r) = new_layout_of_doc_go(~benchmark, ~width, ~pos, f1);
      let (out2s, out2p, out2c, out2r) = new_layout_of_doc_go(~benchmark, ~width, ~pos, f2);
      let r = layout_merge(out1s, out1p, out1c, out1r, out2s, out2p, out2c, out2r);
      set_memo(memo, memo_key, r);
      r
    };
  };
};
let new_layout_of_doc_go = Obj.magic(new_layout_of_doc_go);
let new_layout_of_doc_go:
  'annot. (~benchmark: bool, ~width: int, ~pos: int, mem_doc('annot)) => layout_result = new_layout_of_doc_go;

let _ = Js.export("Cat_share", (x, y) => Layout.Cat(x, y));
let _ = Js.export("Align_share", x => Layout.Align(x));
let _ = Js.export("Annot_share", (x, y) => Layout.Annot(x, y));
let _ = Js.export("new_layout_of_doc_go_share", new_layout_of_doc_go);
let _ = Js.export("layout_merge_share", layout_merge);

let rec take = (n: int, lst: list('a)): list('a) => {
  switch (n, lst) {
  | (0, _) => []
  | (_, []) => []
  | (n, [x, ...xs]) => [x, ...take(n - 1, xs)]
  };
};

let rec layout_of_doc' = (doc: Doc.t(unit)): Doc.m(Layout.t(unit)) => {
  let g = (~width: int, ~pos: int): Doc.m'(Layout.t(unit)) => {
    // TODO: lift the switch(doc.doc) outside the lambda
    count := count^ + 1;
    switch (doc.doc) {
    | Text(string) =>
      // TODO: cache text length in Text?
      let pos' = pos + String.length(string); //Unicode.length(string);
      let cost =
        if (pos' <= width) {
          Cost.zero;
        } else {
          let overflow = pos' - width;
          // overflow_cost = sum i from 1 to overflow
          let overflow_cost = overflow * (overflow + 1) / 2;
          Cost.mk_overflow(overflow_cost);
        };
      let r = (cost, Layout.Text(string));
      [
        (0 + pos', r),
        (1 + pos', r),
        //(2 + pos', r),
      ];
    //PosMap.singleton(pos', (cost, Layout.Text(string)));
    | Cat(d1, d2) =>
      let l1 = take(1, layout_of_doc'(d1, ~width, ~pos));
      PosMap.fold_left(
        (pos, z, (cost1, layout1)) => {
          let l2 = layout_of_doc'(d2, ~width, ~pos);
          let layouts =
            PosMap.map(
              ((cost2, layout2)) =>
                (Cost.add(cost1, cost2), Layout.Cat(layout1, layout2)),
              l2,
            );
          m'_union(z, layouts);
        },
        PosMap.empty,
        l1,
      );
    | Linebreak(_) =>
      PosMap.singleton(0, (Cost.mk_height(1), Layout.Linebreak))
    | Align(d) =>
      let layout = layout_of_doc'(d, ~width=width - pos, ~pos=0);
      PosMap.mapk(
        (p, (c, l)) => (p + pos, (c, Layout.Align(l))),
        layout,
      );
    | Annot(annot, d) =>
      let layout = layout_of_doc'(d, ~width, ~pos);
      PosMap.map(((c, l)) => (c, Layout.Annot(annot, l)), layout);
    | Fail(_) => PosMap.empty
    | Choice(d1, d2) =>
      let l1 = layout_of_doc'(d1, ~width, ~pos);
      let l2 = layout_of_doc'(d2, ~width, ~pos);
      m'_union(l1, l2);
    };
  };
  let h = (~width: int, ~pos: int): Doc.m'(Layout.t(unit)) => {
    let key = (width, pos);
    switch (Doc.M.find_opt(doc.mem, key)) {
    | Some(value) => value
    | None =>
      let value = g(~width, ~pos);
      //Doc.M.add(doc.mem, key, value);
      take(2, value);
    };
  };
  h;
};

let rec make_test_doc = (x: int): mem_doc('annot) =>
  if (x == 0) {
    mk_linebreak();
  } else if (x == 1) {
    mk_text("abc");
  } else {
    switch (x mod 4) {
    | 0 => mk_annot(x, make_test_doc(x - 1))
    | 1 => mk_align(make_test_doc(x - 1))
    | 2 => mk_cat(make_test_doc(x - 1), make_test_doc(x - 2)) // must be 2, so that linebreak can happen
    | 3 => mk_choice(make_test_doc(x - 1), make_test_doc(x - 2))
    | _ => failwith(__LOC__)
    };
  };
// let benchmark_doc = make_test_doc(40);
let benchmark_doc = make_test_doc(26);

let layout_of_doc_25 = (~width: int, ~pos: int): option(Layout.t('annot)) => {
  gensym := gensym^ + 1;
  ignore(new_layout_of_doc_go(benchmark_doc, ~benchmark=true, ~width, ~pos));
  None;
};

let new_layout_of_doc =
    (doc: mem_doc('annot), ~width: int, ~pos: int): option(Layout.t('annot)) => {
  gensym := gensym^ + 1;
  let (layout_s, layout_p, layout_c, layout_r) = new_layout_of_doc_go(doc, ~benchmark=false, ~width, ~pos);
  let pos = ref(max_int);
  let cost = ref(max_int);
  let res = ref(None);
  for (i in 0 to layout_s - 2) { // remember that _s is 1 greater than the ocaml length (b/c _s is the javascript length)
    if (layout_c[i] < cost^ || layout_c[i] == cost^ && layout_p[i] < pos^) {
      pos := layout_p[i];
      cost := layout_c[i];
      res := Some(layout_r[i]);
    }
  };

  res^;
};
let new_layout_of_doc = Obj.magic(new_layout_of_doc);
let new_layout_of_doc:
  'annot. (mem_doc('annot), ~width: int, ~pos: int) => option(Layout.t('annot)) = new_layout_of_doc;

let layout_of_doc =
    (doc: Doc.t('annot), ~width: int, ~pos: int): option(Layout.t('annot)) => {
  let rec minimum =
          ((pos, (cost, t)): (int, (Cost.t, option('a))))
          : (list((int, (Cost.t, 'a))) => option('a)) => {
    fun
    | [] => t
    | [(x_pos, (x_cost, x)), ...rest] =>
      // Prefer lowest cost, or if same cost, prefer ending at an earlier column
      // (Columns are unique by construction of PosMap.)
      if (Cost.lt(x_cost, cost) || Cost.eq(x_cost, cost) && x_pos < pos) {
        minimum((x_pos, (x_cost, Some(x))), rest);
      } else {
        minimum((pos, (cost, t)), rest);
      };
  };
  let l =
    minimum(
      (max_int, (Cost.inf, None)),
      Obj.magic(layout_of_doc'(Obj.magic(doc), ~width, ~pos)),
    );
  l;
};
