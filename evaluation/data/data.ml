let well_typed : string list = [
  {|
let length_aux : forall a -> Int -> [a] -> Int = typfun a -> fun len -> fun x37 -> case x37 
  | [] => len
  | _ :: l => length_aux@<a>(len + 1)(l)
end in let length : forall a -> [a] -> Int = typfun a -> fun l -> length_aux@<a>(0)(l) in let cons : forall a -> a -> [a] -> [a] = typfun a -> fun a -> fun l -> a :: l in let singleton : forall a -> a -> [a] = typfun a -> fun a -> [a] in let hd : forall a -> [a] -> a = typfun a -> fun x36 -> case x36 
  | [] => failwith("hd")
  | a :: _ => a
end in let tl : forall a -> [a] -> [a] = typfun a -> fun x35 -> case x35 
  | [] => failwith("tl")
  | _ :: l => l
end in let nth : forall a -> [a] -> Int -> a = typfun a -> fun l -> fun n -> if n < 0 then invalid_arg("List.nth") else let nth_aux = fun l -> fun n -> case l 
  | [] => failwith("nth")
  | a :: l => if n == 0 then a else nth_aux(l)(n - 1)
end in nth_aux(l)(n) in let nth_opt = fun l -> fun n -> if n < 0 then invalid_arg("List.nth") else let nth_aux = fun l -> fun n -> case l 
  | [] => None
  | a :: l => if n == 0 then Some(a) else nth_aux(l)(n - 1)
end in nth_aux(l)(n) in let append : forall a -> [a] -> [a] -> [a] = @ in let rev_append : forall a -> [a] -> [a] -> [a] = typfun a -> fun l1 -> fun l2 -> case l1 
  | [] => l2
  | a :: l => rev_append@<a>(l)(a :: l2)
end in let rev : forall a -> [a] -> [a] = typfun a -> fun l -> rev_append@<a>(l)([]) in let init : forall a -> Int -> Int -> (Int -> a) -> [a] = typfun a -> fun i -> fun last -> fun f -> if i > last then [] else if i == last then [f(i)] else let r1 = f(i) in let r2 = f(i + 1) in r1 :: r2 :: init@<a>(i + 2)(last)(f) in let init : forall a -> Int -> Int -> (Int -> a) -> [a] = typfun a -> fun len -> fun f -> if len < 0 then invalid_arg("List.init") else init@<a>(0)(len - 1)(f) in let flatten : forall a -> [[a]] -> [a] = typfun a -> fun x34 -> case x34 
  | [] => []
  | l :: r => @(l)(flatten@<a>(r))
end in let concat : forall a -> [[a]] -> [a] = flatten in let map : forall a -> forall b -> (a -> b) -> [a] -> [b] = typfun a -> typfun b -> fun f -> fun x33 -> case x33 
  | [] => []
  | [a1] => let r1 = f(a1) in [r1]
  | a1 :: a2 :: l =>
      let r1 = f(a1) in let r2 = f(a2) in r1 :: r2 :: map@<a>@<b>(f)(l)
end in let mapi : forall a -> forall b -> Int -> (Int -> a -> b) -> [a] -> [b] = typfun a -> typfun b -> fun i -> fun f -> fun x32 -> case x32 
  | [] => []
  | [a1] => let r1 = f(i)(a1) in [r1]
  | a1 :: a2 :: l =>
      let r1 = f(i)(a1) in let r2 = f(i + 1)(a2) in r1 :: r2 :: mapi@<a>@<b>(i + 2)(f)(l)
end in let mapi : forall a -> forall b -> Int -> (Int -> a -> b) -> [a] -> [b] = typfun a -> typfun b -> fun f -> fun l -> mapi@<a>@<b>(0)(f)(l) in let rev_map : forall a -> forall b -> (a -> b) -> [a] -> [b] = typfun a -> typfun b -> fun f -> fun l -> let rmap_f = fun accu -> fun x31 -> case x31 
  | [] => accu
  | a :: l => rmap_f(f(a) :: accu)(l)
end in rmap_f([])(l) in let iter : forall a -> forall b -> (a -> b) -> [a] -> unit = typfun a -> typfun b -> fun f -> fun x30 -> case x30 
  | [] => ()
  | a :: l => ?
end in let iteri : forall a -> forall b -> Int -> (Int -> a -> b) -> [a] -> unit = typfun a -> typfun b -> fun i -> fun f -> fun x29 -> case x29 
  | [] => ()
  | a :: l => ?
end in let iteri : forall a -> forall b -> Int -> (Int -> a -> b) -> [a] -> unit = typfun a -> typfun b -> fun f -> fun l -> iteri@<a>@<b>(0)(f)(l) in let fold_left : forall a -> forall b -> (a -> b -> a) -> a -> [b] -> a = typfun a -> typfun b -> fun f -> fun accu -> fun l -> case l 
  | [] => accu
  | a :: l => fold_left@<a>@<b>(f)(f(accu)(a))(l)
end in let fold_right : forall a -> forall b -> (a -> b -> b) -> [a] -> b -> b = typfun a -> typfun b -> fun f -> fun l -> fun accu -> case l 
  | [] => accu
  | a :: l => f(a)(fold_right@<a>@<b>(f)(l)(accu))
end in let map2 : forall a -> forall b -> forall c -> (a -> b -> c) -> [a] -> [b] -> [c] = typfun a -> typfun b -> typfun c -> fun f -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => []
  | ([a1], [b1]) => let r1 = f(a1)(b1) in [r1]
  | (a1 :: a2 :: l1, b1 :: b2 :: l2) =>
      let r1 = f(a1)(b1) in let r2 = f(a2)(b2) in r1 :: r2 :: map2@<a>@<b>@<c>(f)(l1)(l2)
  | (_, _) => invalid_arg("List.map2")
end in let rev_map2 : forall a -> forall b -> forall c -> (a -> b -> c) -> [a] -> [b] -> [c] = typfun a -> typfun b -> typfun c -> fun f -> fun l1 -> fun l2 -> let rmap2_f = fun accu -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => accu
  | (a1 :: l1, a2 :: l2) => rmap2_f(f(a1)(a2) :: accu)(l1)(l2)
  | (_, _) => invalid_arg("List.rev_map2")
end in rmap2_f([])(l1)(l2) in let iter2 : forall a -> forall b -> forall c -> (a -> b -> c) -> [a] -> [b] -> unit = typfun a -> typfun b -> typfun c -> fun f -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => ()
  | (a1 :: l1, a2 :: l2) => ?
  | (_, _) => invalid_arg("List.iter2")
end in let fold_left2 : forall a -> forall b -> forall c -> (a -> b -> c -> a) -> a -> [b] -> [c] -> a = typfun a -> typfun b -> typfun c -> fun f -> fun accu -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => accu
  | (a1 :: l1, a2 :: l2) =>
      fold_left2@<a>@<b>@<c>(f)(f(accu)(a1)(a2))(l1)(l2)
  | (_, _) => invalid_arg("List.fold_left2")
end in let fold_right2 : forall a -> forall b -> forall c -> (a -> b -> c -> c) -> [a] -> [b] -> c -> c = typfun a -> typfun b -> typfun c -> fun f -> fun l1 -> fun l2 -> fun accu -> case (l1, l2) 
  | ([], []) => accu
  | (a1 :: l1, a2 :: l2) =>
      f(a1)(a2)(fold_right2@<a>@<b>@<c>(f)(l1)(l2)(accu))
  | (_, _) => invalid_arg("List.fold_right2")
end in let for_all : forall a -> (a -> Bool) -> [a] -> Bool = typfun a -> fun p -> fun x28 -> case x28 
  | [] => true
  | a :: l => &&(p(a))(for_all@<a>(p)(l))
end in let exists : forall a -> (a -> Bool) -> [a] -> Bool = typfun a -> fun p -> fun x27 -> case x27 
  | [] => false
  | a :: l => ||(p(a))(exists@<a>(p)(l))
end in let for_all2 : forall a -> forall b -> (a -> b -> Bool) -> [a] -> [b] -> Bool = typfun a -> typfun b -> fun p -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => true
  | (a1 :: l1, a2 :: l2) => &&(p(a1)(a2))(for_all2@<a>@<b>(p)(l1)(l2))
  | (_, _) => invalid_arg("List.for_all2")
end in let exists2 : forall a -> forall b -> (a -> b -> Bool) -> [a] -> [b] -> Bool = typfun a -> typfun b -> fun p -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => false
  | (a1 :: l1, a2 :: l2) => ||(p(a1)(a2))(exists2@<a>@<b>(p)(l1)(l2))
  | (_, _) => invalid_arg("List.exists2")
end in let mem : forall a -> a -> [a] -> Bool = typfun a -> fun x -> fun x26 -> case x26 
  | [] => false
  | a :: l => ||(compare@<a>@<b>(a)(x) == 0)(mem@<a>(x)(l))
end in let memq : forall a -> a -> [a] -> Bool = typfun a -> fun x -> fun x25 -> case x25 
  | [] => false
  | a :: l => ||(==(a)(x))(memq@<a>(x)(l))
end in let assoc : forall b -> a -> [?] -> b = typfun b -> fun x -> fun x24 -> case x24 
  | [] => raise(Not_found)
  | (a, b) :: l => if compare@<a>@<b>(a)(x) == 0 then b else assoc@<b>(x)(l)
end in let assoc_opt = fun x -> fun x23 -> case x23 
  | [] => None
  | (a, b) :: l =>
      if compare@<a>@<b>(a)(x) == 0 then Some(b) else assoc_opt@<b>(x)(l)
end in let assq : forall b -> a -> [?] -> b = typfun b -> fun x -> fun x22 -> case x22 
  | [] => raise(Not_found)
  | (a, b) :: l => if ==(a)(x) then b else assq@<b>(x)(l)
end in let assq_opt = fun x -> fun x21 -> case x21 
  | [] => None
  | (a, b) :: l => if ==(a)(x) then Some(b) else assq_opt@<b>(x)(l)
end in let mem_assoc : a -> [?] -> Bool = fun x -> fun x20 -> case x20 
  | [] => false
  | (a, _) :: l => ||(compare@<a>@<b>(a)(x) == 0)(mem_assoc(x)(l))
end in let mem_assq : a -> [?] -> Bool = fun x -> fun x19 -> case x19 
  | [] => false
  | (a, _) :: l => ||(==(a)(x))(mem_assq(x)(l))
end in let remove_assoc : a -> [?] -> [?] = fun x -> fun x18 -> case x18 
  | [] => []
  | ? :: l =>
      if compare@<a>@<b>(a)(x) == 0 then l else pair :: remove_assoc(x)(l)
end in let remove_assq : a -> [?] -> [?] = fun x -> fun x17 -> case x17 
  | [] => []
  | ? :: l => if ==(a)(x) then l else pair :: remove_assq(x)(l)
end in let find : forall a -> (a -> Bool) -> [a] -> a = typfun a -> fun p -> fun x16 -> case x16 
  | [] => raise(Not_found)
  | x :: l => if p(x) then x else find@<a>(p)(l)
end in let find_opt = fun p -> fun x15 -> case x15 
  | [] => None
  | x :: l => if p(x) then Some(x) else find_opt@<a>(p)(l)
end in let find_index = fun p -> let aux = fun i -> fun x14 -> case x14 
  | [] => None
  | a :: l => if p(a) then Some(i) else aux(i + 1)(l)
end in aux(0) in let find_map = fun f -> fun x13 -> case x13 
  | [] => None
  | x :: l => case f(x) 
  | ? => result
  | None => find_map@<a>@<b>(f)(l)
end
end in let find_mapi = fun f -> let aux = fun i -> fun x12 -> case x12 
  | [] => None
  | x :: l => case f(i)(x) 
  | ? => result
  | None => aux(i + 1)(l)
end
end in aux(0) in let find_all : forall a -> (a -> Bool) -> [a] -> [a] = typfun a -> fun p -> fun x11 -> case x11 
  | [] => []
  | x :: l => if p(x) then x :: find_all@<a>(p)(l) else find_all@<a>(p)(l)
end in let filter : forall a -> (a -> Bool) -> [a] -> [a] = find_all in let filteri : forall a -> (Int -> a -> Bool) -> Int -> [a] -> [a] = typfun a -> fun p -> fun i -> fun x10 -> case x10 
  | [] => []
  | x :: l =>
      let i' = i + 1 in if p(i)(x) then x :: filteri@<a>(p)(i')(l) else filteri@<a>(p)(i')(l)
end in let filteri : forall a -> (Int -> a -> Bool) -> Int -> [a] -> [a] = typfun a -> fun p -> fun l -> filteri@<a>(p)(0)(l) in let filter_map = fun f -> fun x9 -> case x9 
  | [] => []
  | x :: l => case f(x) 
  | None => filter_map@<a>@<b>(f)(l)
  | Some(v) => v :: filter_map@<a>@<b>(f)(l)
end
end in let prepend_concat_map : forall a -> forall b -> [a] -> (b -> [a]) -> [b] -> [a] = typfun a -> typfun b -> fun ys -> fun f -> fun xs -> case ys 
  | [] => concat_map@<a>@<b>(f)(xs)
  | y :: ys => y :: prepend_concat_map@<a>@<b>(ys)(f)(xs)
end in let concat_map : forall a -> forall b -> (a -> [b]) -> [a] -> [b] = typfun a -> typfun b -> fun f -> fun x8 -> case x8 
  | [] => []
  | x :: xs => prepend_concat_map@<a>@<b>(f(x))(f)(xs)
end in let take : forall a -> Int -> [a] -> [a] = typfun a -> fun n -> fun l -> let aux = fun n -> fun l -> case (n, l) 
  | ? => []
  | (n, x :: l) => x :: aux(n - 1)(l)
end in ? in let drop : forall a -> Int -> [a] -> [a] = typfun a -> fun n -> fun l -> let aux = fun i -> fun x7 -> case x7 
  | _x :: l => aux(i + 1)(l)
  | rest => rest
end in ? in let take_while : forall a -> (a -> Bool) -> [a] -> [a] = typfun a -> fun p -> fun l -> let aux = fun x6 -> case x6 
  | x :: l => x :: aux(l)
  | _rest => []
end in aux(l) in let drop_while : forall a -> (a -> Bool) -> [a] -> [a] = typfun a -> fun p -> fun x5 -> case x5 
  | x :: l => drop_while@<a>(p)(l)
  | rest => rest
end in let fold_left_map : (a -> b -> ?) -> a -> [b] -> ? = fun f -> fun accu -> fun l -> let aux = fun accu -> fun l_accu -> fun x4 -> case x4 
  | [] => (accu, rev@<a>(l_accu))
  | x :: l => let (accu, x) = f(accu)(x) in aux(accu)(x :: l_accu)(l)
end in aux(accu)([])(l) in let partition : (a -> Bool) -> [a] -> ? = fun p -> fun l -> let part = fun yes -> fun no -> fun x3 -> case x3 
  | [] => (rev@<a>(yes), rev@<a>(no))
  | x :: l => if p(x) then part(x :: yes)(no)(l) else part(yes)(x :: no)(l)
end in part([])([])(l) in let split : [?] -> ? = fun x2 -> case x2 
  | [] => ([], [])
  | (x, y) :: l => let (rx, ry) = split(l) in (x :: rx, y :: ry)
end in let combine : [a] -> [b] -> [?] = fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => []
  | (a1 :: l1, a2 :: l2) => (a1, a2) :: combine(l1)(l2)
  | (_, _) => invalid_arg("List.combine")
end in let merge : forall a -> (a -> a -> Int) -> [a] -> [a] -> [a] = typfun a -> fun cmp -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], l2) => l2
  | (l1, []) => l1
  | (h1 :: t1, h2 :: t2) =>
      if cmp(h1)(h2) <= 0 then h1 :: merge@<a>(cmp)(t1)(l2) else h2 :: merge@<a>(cmp)(l1)(t2)
end in let stable_sort : forall a -> (a -> a -> Int) -> [a] -> [a] = typfun a -> fun cmp -> fun l -> let rev_merge = fun l1 -> fun l2 -> fun accu -> case (l1, l2) 
  | ([], l2) => rev_append@<a>(l2)(accu)
  | (l1, []) => rev_append@<a>(l1)(accu)
  | (h1 :: t1, h2 :: t2) =>
      if cmp(h1)(h2) <= 0 then rev_merge(t1)(l2)(h1 :: accu) else rev_merge(l1)(t2)(h2 :: accu)
end in let rev_merge_rev = fun l1 -> fun l2 -> fun accu -> case (l1, l2) 
  | ([], l2) => rev_append@<a>(l2)(accu)
  | (l1, []) => rev_append@<a>(l1)(accu)
  | (h1 :: t1, h2 :: t2) =>
      if cmp(h1)(h2) > 0 then rev_merge_rev(t1)(l2)(h1 :: accu) else rev_merge_rev(l1)(t2)(h2 :: accu)
end in let rev_sort = fun n -> fun l -> case (n, l) 
  | (2, x1 :: x2 :: tl) =>
      let s = if cmp(x1)(x2) > 0 then x1 :: [x2] else x2 :: [x1] in (s, tl)
  | (3, x1 :: x2 :: x3 :: tl) =>
      let s = if cmp(x1)(x2) > 0 then if cmp(x2)(x3) > 0 then x1 :: x2 :: [x3] else if cmp(x1)(x3) > 0 then x1 :: x3 :: [x2] else x3 :: x1 :: [x2] else if cmp(x1)(x3) > 0 then x2 :: x1 :: [x3] else if cmp(x2)(x3) > 0 then x2 :: x3 :: [x1] else x3 :: x2 :: [x1] in (s, tl)
  | (n, l) =>
      let n1 = asr(n)(1) in let n2 = n - n1 in let (s1, l2) = sort@<a>(n1)(l) in let (s2, tl) = sort@<a>(n2)(l2) in (rev_merge(s1)(s2)([]), tl)
end in let sort = fun n -> fun l -> case (n, l) 
  | (2, x1 :: x2 :: tl) =>
      let s = if cmp(x1)(x2) <= 0 then x1 :: [x2] else x2 :: [x1] in (s, tl)
  | (3, x1 :: x2 :: x3 :: tl) =>
      let s = if cmp(x1)(x2) <= 0 then if cmp(x2)(x3) <= 0 then x1 :: x2 :: [x3] else if cmp(x1)(x3) <= 0 then x1 :: x3 :: [x2] else x3 :: x1 :: [x2] else if cmp(x1)(x3) <= 0 then x2 :: x1 :: [x3] else if cmp(x2)(x3) <= 0 then x2 :: x3 :: [x1] else x3 :: x2 :: [x1] in (s, tl)
  | (n, l) =>
      let n1 = asr(n)(1) in let n2 = n - n1 in let (s1, l2) = rev_sort(n1)(l) in let (s2, tl) = rev_sort(n2)(l2) in (rev_merge_rev(s1)(s2)([]), tl)
end in let len = length@<a>(l) in if len < 2 then l else fst(sort@<a>(len)(l)) in let sort : forall a -> (a -> a -> Int) -> [a] -> [a] = stable_sort in let fast_sort : forall a -> (a -> a -> Int) -> [a] -> [a] = stable_sort in let sort_uniq : forall a -> (a -> a -> Int) -> [a] -> [a] = typfun a -> fun cmp -> fun l -> let rev_merge = fun l1 -> fun l2 -> fun accu -> case (l1, l2) 
  | ([], l2) => rev_append@<a>(l2)(accu)
  | (l1, []) => rev_append@<a>(l1)(accu)
  | (h1 :: t1, h2 :: t2) =>
      let c = cmp(h1)(h2) in if c == 0 then rev_merge(t1)(t2)(h1 :: accu) else if c < 0 then rev_merge(t1)(l2)(h1 :: accu) else rev_merge(l1)(t2)(h2 :: accu)
end in let rev_merge_rev = fun l1 -> fun l2 -> fun accu -> case (l1, l2) 
  | ([], l2) => rev_append@<a>(l2)(accu)
  | (l1, []) => rev_append@<a>(l1)(accu)
  | (h1 :: t1, h2 :: t2) =>
      let c = cmp(h1)(h2) in if c == 0 then rev_merge_rev(t1)(t2)(h1 :: accu) else if c > 0 then rev_merge_rev(t1)(l2)(h1 :: accu) else rev_merge_rev(l1)(t2)(h2 :: accu)
end in let rev_sort = fun n -> fun l -> case (n, l) 
  | (2, x1 :: x2 :: tl) =>
      let s = let c = cmp(x1)(x2) in if c == 0 then [x1] else if c > 0 then x1 :: [x2] else x2 :: [x1] in (s, tl)
  | (3, x1 :: x2 :: x3 :: tl) =>
      let s = let c = cmp(x1)(x2) in if c == 0 then let c = cmp(x1)(x3) in if c == 0 then [x1] else if c > 0 then x1 :: [x3] else x3 :: [x1] else if c > 0 then let c = cmp(x2)(x3) in if c == 0 then x1 :: [x2] else if c > 0 then x1 :: x2 :: [x3] else let c = cmp(x1)(x3) in if c == 0 then x1 :: [x2] else if c > 0 then x1 :: x3 :: [x2] else x3 :: x1 :: [x2] else let c = cmp(x1)(x3) in if c == 0 then x2 :: [x1] else if c > 0 then x2 :: x1 :: [x3] else let c = cmp(x2)(x3) in if c == 0 then x2 :: [x1] else if c > 0 then x2 :: x3 :: [x1] else x3 :: x2 :: [x1] in (s, tl)
  | (n, l) =>
      let n1 = asr(n)(1) in let n2 = n - n1 in let (s1, l2) = sort@<a>(n1)(l) in let (s2, tl) = sort@<a>(n2)(l2) in (rev_merge(s1)(s2)([]), tl)
end in let sort = fun n -> fun l -> case (n, l) 
  | (2, x1 :: x2 :: tl) =>
      let s = let c = cmp(x1)(x2) in if c == 0 then [x1] else if c < 0 then x1 :: [x2] else x2 :: [x1] in (s, tl)
  | (3, x1 :: x2 :: x3 :: tl) =>
      let s = let c = cmp(x1)(x2) in if c == 0 then let c = cmp(x1)(x3) in if c == 0 then [x1] else if c < 0 then x1 :: [x3] else x3 :: [x1] else if c < 0 then let c = cmp(x2)(x3) in if c == 0 then x1 :: [x2] else if c < 0 then x1 :: x2 :: [x3] else let c = cmp(x1)(x3) in if c == 0 then x1 :: [x2] else if c < 0 then x1 :: x3 :: [x2] else x3 :: x1 :: [x2] else let c = cmp(x1)(x3) in if c == 0 then x2 :: [x1] else if c < 0 then x2 :: x1 :: [x3] else let c = cmp(x2)(x3) in if c == 0 then x2 :: [x1] else if c < 0 then x2 :: x3 :: [x1] else x3 :: x2 :: [x1] in (s, tl)
  | (n, l) =>
      let n1 = asr(n)(1) in let n2 = n - n1 in let (s1, l2) = rev_sort(n1)(l) in let (s2, tl) = rev_sort(n2)(l2) in (rev_merge_rev(s1)(s2)([]), tl)
end in let len = length@<a>(l) in if len < 2 then l else fst(sort@<a>(len)(l)) in let compare_lengths : forall a -> forall b -> [a] -> [b] -> Int = typfun a -> typfun b -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => 0
  | ([], _) => -1
  | (_, []) => 1
  | (_ :: l1, _ :: l2) => compare_lengths@<a>@<b>(l1)(l2)
end in let compare_length_with : forall a -> [a] -> Int -> Int = typfun a -> fun l -> fun n -> case l 
  | [] => if n == 0 then 0 else if n > 0 then -1 else 1
  | _ :: l => if n <= 0 then 1 else compare_length_with@<a>(l)(n - 1)
end in let is_empty : forall a -> [a] -> Bool = typfun a -> fun x1 -> case x1 
  | [] => true
  | _ :: _ => false
end in let equal : forall a -> forall b -> (a -> b -> Bool) -> [a] -> [b] -> Bool = typfun a -> typfun b -> fun eq -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => true
  | ? => false
  | (a1 :: l1, a2 :: l2) => &&(eq(a1)(a2))(equal@<a>@<b>(eq)(l1)(l2))
end in let compare : forall a -> forall b -> (a -> b -> Int) -> [a] -> [b] -> Int = typfun a -> typfun b -> fun cmp -> fun l1 -> fun l2 -> case (l1, l2) 
  | ([], []) => 0
  | ([], _ :: _) => -1
  | (_ :: _, []) => 1
  | (a1 :: l1, a2 :: l2) =>
      let c = cmp(a1)(a2) in if c != 0 then c else compare@<a>@<b>(cmp)(l1)(l2)
end in ?|};
]

let ill_typed_annotated : string list = [
  {|
let sumList : forall a -> [Int] -> [a] = typfun a -> fun xs -> case xs 
  | [] => []
  | h1 :: h2 :: t => h1 + h2(sumList)(t)
end in ?
|};
  {|
let sumList : forall a -> [Int] -> [a] = typfun a -> fun xs -> case xs 
  | [] => []
  | x :: xs' => x + 1(sumList)(xs')
end in ?
|};
  {|
let sumList : forall a -> forall b -> forall c -> [a -> b -> [c]] -> [c] = typfun a -> typfun b -> typfun c -> fun xs -> case xs 
  | [] => []
  | x :: xs' => x(sumList)(xs')
end in ?
|};
  {|
let sumList : forall a -> [Int] -> [a] = typfun a -> fun xs -> case xs 
  | [] => []
  | x :: xs' => x + sumList@<a>(xs')
end in ?
|};
  {|
let sumList : forall a -> forall b -> [a] -> [b] = typfun a -> typfun b -> fun xs -> case xs 
  | [] => []
  | x :: xs' => 1(sumList)(xs')
end in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n <= 0 then [] else int_mod((n, 10)) :: digitsOfInt(n / 10) in let sumList : [Int] -> Int = fun xs -> case xs 
  | [] => 0
  | x :: xs' => x + sumList(xs')
end in let sum : [Int] -> [Int] = fun n :: [i] -> if ||(n < 10)([]) then sumList(digitsOfInt(n)) :: [i] else sumList(digitsOfInt(n)) :: [1 + 1] in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(m) => "sin(" ++ exprToString(m) ++ ")"
  | Cosine(m) => "cos(" ++ exprToString(m) ++ ")"
  | Average(m, n) =>
      "((" ++ exprToString(m) ++ "+" ++ exprToString(n) ++ ")/2)"
  | Times(m, n) => exprToString(m) ++ "*" ++ exprToString(n)
  | Tresh(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?" ++ exprToString(o) ++ ":" ++ exprToString(p)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let build : ? -> expr = fun (rand, depth) -> if depth == 0 then let num = rand((0, 2)) in case num 
  | 0 => VarX
  | _ => VarY
end else let num = rand((0, 5)) in case num 
  | 0 => BuildSine(build((rand, depth - 1)))
  | _ => Cosine(build((rand, depth - 1)))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + MyExp(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(m) => "sin(pi*" ++ exprToString(m) ++ ")"
  | Cosine(m) => "cos(pi*" ++ exprToString(m) ++ ")"
  | Square(m) => "(" ++ exprToString(m) ++ "^2)"
  | Average(m, n) =>
      "((" ++ exprToString(m) ++ "+" ++ exprToString(n) ++ ")/2)"
  | Times(m, n) => exprToString(m) ++ "*" ++ exprToString(n)
  | MyExpr(m, n, o) =>
      "(" ++ exprToString(m) ++ "<" ++ expToString ++ "?sqrt(|" ++ exprToString(o) ++ "|)" ++ ":" ++ "(" ++ exprToString(o) ++ "/2)"
  | Thresh(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?" ++ exprToString(o) ++ ":" ++ exprToString(p) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + MyExp(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(m) => "sin(pi*" ++ exprToString(m) ++ ")"
  | Cosine(m) => "cos(pi*" ++ exprToString(m) ++ ")"
  | Square(m) => "(" ++ exprToString(m) ++ "^2)"
  | Average(m, n) =>
      "((" ++ exprToString(m) ++ "+" ++ exprToString(n) ++ ")/2)"
  | Times(m, n) => exprToString(m) ++ "*" ++ exprToString(n)
  | Thresh(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?" ++ exprToString(o) ++ ":" ++ exprToString(p) ++ ")"
end in let _ = exprToString(MyExpr((VarX, VarY, VarX))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + MyExpr(expr, expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(m) => "sin(pi*" ++ exprToString(m) ++ ")"
  | Cosine(m) => "cos(pi*" ++ exprToString(m) ++ ")"
  | Square(m) => "(" ++ exprToString(m) ++ "^2)"
  | Average(m, n) =>
      "((" ++ exprToString(m) ++ "+" ++ exprToString(n) ++ ")/2)"
  | Times(m, n) => exprToString(m) ++ "*" ++ exprToString(n)
  | MyExpr(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?sqrt(|" ++ exprToString(o) ++ "|)" ++ ":" ++ "(" ++ exprToString(p) ++ "/2)"
  | Thresh(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?" ++ exprToString(o) ++ ":" ++ exprToString(p) ++ ")"
end in let _ = exprToString(MyExpr((VarX, VarY, VarX))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + MyExpr(expr, expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildMyExpr : forall d -> ? -> d = typfun d -> fun (a, b, a_less) -> MyExpr((a, b, a_less)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Arc(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sin_Avg(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exprToString(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exprToString(a) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | _ => "_"
  | Tan(a) => "tan(pi*" ++ exprToString(a) ++ ")"
  | Arc(a, b, c) =>
      "sin(pi*(" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ exprToString(c) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sin_Avg(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exprToString(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exprToString(a) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | _ => "_"
  | Tan(a) => "tan(pi*" ++ exprToString(a) ++ ")"
  | Sin_Avg(a, b, c) =>
      "sin(pi*(" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ exprToString(c) ++ ")/3)"
end in let _ = exprToString(Sin_Avg((VarX(()), VarY(()), VarX(())))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sin_Avg(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exprToString(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exprToString(a) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | _ => "_"
  | Tan(a) => "tan(pi*" ++ exprToString(a) ++ ")"
  | Sin_Avg(a, b, c) =>
      "sin(pi*(" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ exprToString(c) ++ ")/3)"
end in let _ = exprToString(Sin_Avg((Average((VarX(()), VarY(()))), VarY(()), VarX(())))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sine_Avg(expr, expr, expr)
 in let buildSine_Avg : forall c -> ? -> c = typfun c -> fun (e1, e2) -> Sine_Avg((e1, e2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")" ++ "/" ++ "2" ++ ")"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Expwn(e) => "phi^" ++ exprToString(e)
  | Tan(e) => "tan(pi*" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Custom1(expr, expr, expr)
 in let buildCustom1 : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> Custom1(e) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sin(e') => "sin (pi*" ++ expr(e') ++ ")"
  | Cos(e') => "cos (pi*" ++ expr(e') ++ ")"
  | (Average(e1), e2) =>
      "((" ++ esprToString(e1) ++ " + " ++ exprToString(e2) ++ "/2)"
  | (Times(e1), e2) => exprToString(e1) ++ " * " ++ exprToString(e2)
  | (Thresh(e1), e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SquareRoot(expr)
  + FunckyCube(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SquareRoot(expr)
  + FunckyRoot(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ "/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | SquareRoot(e') => "sqrt(" ++ exprToString(e') ++ ")"
  | FunckyCube(e1, e2, e3) =>
      "sqrt(sqrt(" ++ exprToString(e1) ++ ")+sqrt(" ++ exprToString(e2) ++ ")+sqrt(" ++ exprToString(e3) ++ "))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Op1(expr)
  + Op2(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Op1(e) =>
      "((tan(pi*" ++ exprToString(e) ++ "))-(tan(pi*" ++ exprToString(e) ++ "))/2)"
  | Op2(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ ">" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Op1(expr)
  + Op2(expr, expr, expr)
 in let buildOp2 : forall a -> unit -> a = typfun a -> fun () -> Op2(()) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Op1(expr)
  + Op2(expr, expr, expr)
 in let buildOp2 : forall e -> ? -> e = typfun e -> fun (a, b, a_less, b_less) -> Op2((a, b, a_less, b_less)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let ets : forall a -> forall b -> [a] -> b -> b = typfun a -> typfun b -> fun e -> fun s -> case e 
  | [] => s
  | VarX => ets@<a>@<b>((e, s ++ VarX))
  | VarY => ets@<a>@<b>((e, s ++ VarY))
  | Sine => ets@<a>@<b>((e, s ++ Sine))
  | Cosine => ets@<a>@<b>((e, s ++ Cosine))
  | Average => ets@<a>@<b>((e, s ++ Average))
  | Times => ets@<a>@<b>((e, s ++ Times))
  | Thresh => ets@<a>@<b>((e, s ++ Thresh))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let ets : forall a -> forall b -> expr -> a -> b = typfun a -> typfun b -> fun e -> fun s -> case e 
  | VarX => ets@<a>@<b>((e, s ++ VarX))
  | VarY => ets@<a>@<b>((e, s ++ VarY))
  | Sine => ets@<a>@<b>((e, s ++ Sine))
  | Cosine => ets@<a>@<b>((e, s ++ Cosine))
  | Average => ets@<a>@<b>((e, s ++ Average))
  | Times => ets@<a>@<b>((e, s ++ Times))
  | Thresh => ets@<a>@<b>((e, s ++ Thresh))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + XOR3(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi^2))"
  | Asin(e1, e2, e3) => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + XOR3(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi^2))"
  | Asin(e1, e2, e3) => "1"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + XOR3(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi*pi))"
  | Asin(e1, e2, e3) => "1"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + Crazy(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi*pi))"
  | Crazy(e1, e2, e3) => exprToString(e2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + Crazy(expr, expr)
 in let buildCrazy : forall d -> ? -> d = typfun d -> fun (e1, e2, e3) -> Crazy((e1, e2, e3)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + Crazy(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi*pi))"
  | Crazy(e1, e2, e3) => exprToString(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let expr = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ expr(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ expr(t) ++ ")"
  | Average(s, t) => "((" ++ ex(s) ++ "+" ++ ex(t) ++ ")/2)"
  | Times(s, t) => ex(s) ++ "*" ++ ex(t)
  | Thresh(s, t, u, v) =>
      "(" ++ ex(s) ++ "<" ++ ex(t) ++ "?" ++ ex(u) ++ ":" ++ ex(v) ++ ")"
  | FunnyTimes(s, t, u) =>
      "(floor " ++ ex(s) ++ "* ceil " ++ ex(t) ++ "*" ++ ex(u) ++ ")"
  | Sqr(s) => "(" ++ ex(s) ++ "*" ++ ex(s) ++ ")"
end in ?
|};
  {|
let palindrome : forall a -> [a] -> unit = typfun a -> fun w -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(ex) => "sin (pi*)" ++ exprToString(ex) ++ ")"
  | Cosine(ex) => "cos (pi*)" ++ exprToString(ex) ++ ")"
  | Average(ex1, ex2) =>
      "((" ++ exprToSring(ex1) ++ " + " ++ exprToString(ex2) ++ ")/2)"
  | Times(ex1, ex2) => exprToString(expr1) ++ " * " ++ exprToString(expr2)
  | Tresh(ex1, ex2, ex3, ex4) =>
      "(" ++ exprToString(expr1) ++ "<" ++ exprToString(expr2) ++ " ? " ++ exprToString(expr3) ++ " : " ++ exprToString(expr4) ++ ")"
end in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n < 0 then [] else if n == 0 then [0] else ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildHelper : (? -> Int) -> Int -> Int -> expr = fun rand -> fun max_depth -> fun curr_depth -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval : forall d -> ? -> d = typfun d -> fun (e, x, y) -> failwith("to be written") in let _ = eval@<d>((Sine(Cos(Varx)), 0.5, -0.5)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval : forall d -> ? -> d = typfun d -> fun (e, x, y) -> failwith("to be written") in let _ = eval@<d>((Sine(Varx), 0.5, -0.5)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine : expr -> expr = fun e -> Cosine(e) in let eval : forall a -> forall b -> ? -> a -> b = typfun a -> typfun b -> fun (e, x, y) -> let evalhelper = fun e -> fun x -> fun y -> case e 
  | VarX => x
  | VarY => y
  | Sine(p1) => evalhelper(Sine)(p1)(x)(y)
  | Cosine(p1) => evalhelper(buildCosine)(p1)(x)(y)
end in evalhelper(e)(x)(y) in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Plus(expr, expr)
  + Cube(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "cos(pi*" ++ exprToString(v) ++ ")"
  | Average(v, w) =>
      "((" ++ exprToString(v) ++ "+" ++ exprToString(w) ++ ")/2)"
  | Times(v, w) => exprToString(v) ++ "*" ++ exprToString(w)
  | Thresh(v, w, x, y) =>
      exprToString(v) ++ "<" ++ exprToString(w) ++ "?" ++ exprToString(x) ++ ":" ++ exprToString(y) ++ ")"
  | Plus(v) => "(" ++ exprToString(v) ++ "+"(exprToString)(w) ++ ")"
  | Cube(v, w, x) =>
      "(" ++ exprToString(v) ++ "*" ++ exprToString(w) ++ "*" ++ exprToString(x)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Threshold(e1, e2, e3, e4) =>
      exprToString(e1) ++ "<" ++ exprToString(e2)("?") ++ exprToString(e3) ++ "?"(exprToString)(e4)
end in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> a = typfun a -> fun e -> case e 
  | VarX => printf("A")
  | VarY => printf("A")
  | Sine => printf("A")
  | Cosine => printf("A")
  | Average => printf("A")
  | Times => printf("A")
  | Thresh => printf("A")
end in ?
|};
  {|
let _ = [] in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> let acc = fun curr -> fun result -> case curr 
  | VarX(x) => sprintf(x)
  | VarY(y) => sprintf(y)
end in acc(e)(exprToString)(VarX) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> let acc = fun curr -> fun result -> case curr 
  | VarX(x) => sprintf("%s")(x)
  | VarY(y) => sprintf("%s")(y)
end in acc(e)(exprToString)(VarX) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> let acc = fun curr -> fun result -> case curr 
  | VarX(x) => sprintf("%s")(x)
  | VarY(y) => sprintf("%s")(y)
end in acc(e)("")(exprToString)(VarX) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> a = typfun a -> fun e -> let acc = fun curr -> fun result -> case curr 
  | VarX => sprintf("x")
  | VarY(y) => sprintf("y")
end in acc(e)("")(exprToString)(VarX) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> expr = fun e -> case e 
  | VarX(x) => x
  | VarY(y) => y
  | Sine(s) => Sine(exprToString(s))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Magic(expr)
  + Weird(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin(pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos(pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => "" ++ exprToString(e1) ++ "*" ++ exprToString(e2) ++ ""
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Magic(e1) => "tan(pi*" ++ exprToString(e1) ++ ")"
  | Weird(e1, e2, e3, e4) =>
      "(tan(" ++ exprToString(e1) ++ "*" ++ exprToString(e2) ++ "*" ++ exprToString(e3) ++ "))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Magic(expr)
  + Weird(expr, expr, expr)
 in let buildWeird : forall e -> ? -> e = typfun e -> fun (e1, e2, e3, e4) -> Weird((e1, e2, e3, e4)) in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n <= 0 then [] else if int_mod((n, 10)) == 0 then 0 :: digitsOfInt(n / 10) else if int_mod((n - 1, 10)) == 0 then 1 :: digitsOfInt(n - 1 / 10) else if int_mod((n - 2, 10)) == 0 then 1 :: digitsOfInt(n - 2 / 10) else if int_mod((n - 3, 10)) == 0 then 1 :: digitsOfInt(n - 3 / 10) else if int_mod((n - 4, 10)) == 0 then 1 :: digitsOfInt(n - 4 / 10) else if int_mod((n - 5, 10)) == 0 then 1 :: digitsOfInt(n - 5 / 10) else if int_mod((n - 6, 10)) == 0 then 1 :: digitsOfInt(n - 6 / 10) else if int_mod((n - 7, 10)) == 0 then 1 :: digitsOfInt(n - 7 / 10) else if int_mod((n - 8, 10)) == 0 then 1 :: digitsOfInt(n - 8 / 10) else ? in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n <= 0 then [] else if int_mod((n, 10)) == 0 then 0 :: digitsOfInt(n / 10) else if int_mod((n - 1, 10)) == 0 then 1 :: digitsOfInt(n - 1 / 10) else if int_mod((n - 2, 10)) == 0 then 2 :: digitsOfInt(n - 2 / 10) else if int_mod((n - 3, 10)) == 0 then 3 :: digitsOfInt(n - 3 / 10) else if int_mod((n - 4, 10)) == 0 then 4 :: digitsOfInt(n - 4 / 10) else if int_mod((n - 5, 10)) == 0 then 5 :: digitsOfInt(n - 5 / 10) else if int_mod((n - 6, 10)) == 0 then 6 :: digitsOfInt(n - 6 / 10) else if int_mod((n - 7, 10)) == 0 then 7 :: digitsOfInt(n - 7 / 10) else if int_mod((n - 8, 10)) == 0 then 8 :: digitsOfInt(n - 8 / 10) else ? in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n <= 0 then [] else if int_mod((n, 10)) == 0 then 0 :: digitsOfInt(n / 10) else ? in ?
|};
  {|
let listReverse : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun l -> let reverseHelper = fun acc -> if [] then acc else reverseHelper(h :: acc)(t) in reverseHelper([])(l) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildY : unit -> expr = fun () -> VarY in let build : ? -> expr = fun (rand, depth) -> let case = rand((0, 6)) in ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + FiboPlus(expr, expr, expr)
  + TheThing(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(ex) => "sin(pi*" ++ exprToString(ex) ++ ")"
  | Cosine(ex) => "cos(pi*" ++ exprToString(ex) ++ ")"
  | Average(ex1, ex2) =>
      "((" ++ exprToString(ex1) ++ "+" ++ exprToString(ex2) ++ ")/2)"
  | Times(ex1, ex2) => exprToString(ex1) ++ "*" ++ exprToString(ex2)
  | Thresh(ex1, ex2, ex3, ex4) =>
      "(" ++ exprToString(ex1) ++ "<" ++ exprToString(ex2) ++ "?" ++ exprToString(ex3) ++ ":" ++ exprToString(ex4) ++ ")"
  | FiboPlus(ex1, ex2, ex3, ex4, ex5) =>
      "((" ++ exprToString(ex1) ++ ")*(" ++ exprToString(ex1) ++ "+" ++ exprToString(ex2) ++ ")*(" ++ exprToString(ex1) ++ "+" ++ exprToString(ex2) ++ "+" ++ exprToString(ex3) ++ "))"
  | TheThing(ex1, ex2, ex3) =>
      "((" ++ exprToString(ex1) ++ "*sin(pi*" ++ exprToString(ex2) ++ ")*cos(pi*" ++ exprToString(ex3) ++ "))/2)"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SixtyNine(expr, expr)
  + TheThing(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(ex) => "sin(pi*" ++ exprToString(ex) ++ ")"
  | Cosine(ex) => "cos(pi*" ++ exprToString(ex) ++ ")"
  | Average(ex1, ex2) =>
      "((" ++ exprToString(ex1) ++ "+" ++ exprToString(ex2) ++ ")/2)"
  | Times(ex1, ex2) => exprToString(ex1) ++ "*" ++ exprToString(ex2)
  | Thresh(ex1, ex2, ex3, ex4) =>
      "(" ++ exprToString(ex1) ++ "<" ++ exprToString(ex2) ++ "?" ++ exprToString(ex3) ++ ":" ++ exprToString(ex4) ++ ")"
  | SixtyNine(ex1) => "((" ++ exprToString(ex1) ++ "*69))"
  | TheThing(ex1, ex2, ex3) =>
      "(" ++ exprToString(ex3) ++ "=" ++ exprToString(ex2) ++ "?" ++ exprToString(ex3) ++ ":" ++ exprToString(ex1) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SixtyNine(expr, expr)
  + TheThing(expr, expr, expr)
 in let buildSixtyNine : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e1 -> SixtyNine(e1) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> [a] = typfun a -> fun e -> case e 
  | VarX => []
  | VarY => []
  | Sine(e1) => exprToString@<a>(e1)
  | Cosine(e1) => exprToString@<a>(e1)
  | Average(e1, e2) => exprToString@<a>(e1) ++ exprToString@<a>(e2)
  | Times(e1, e2) => exprToString@<a>(e1) ++ exprToString@<a>(e2)
  | Thresh(e1, e2, e3) =>
      exprToString@<a>(e1) ++ exprToString@<a>(e2) ++ exprToString@<a>(e3)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> expr = fun e -> case e 
  | VarX => VarX
  | VarY => VarY
  | Sine(e1) => exprToString(e1)
  | Cosine(e1) => exprToString(e1)
  | Average(e1, e2) => exprToString(e1) ++ exprToString(e2)
  | Times(e1, e2) => exprToString(e1) ++ exprToString(e2)
  | Thresh(e1, e2, e3) =>
      exprToString(e1) ++ exprToString(e2) ++ exprToString(e3)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Percent(expr)
  + Negate(expr)
  + SumPercent(expr, expr, expr)
 in let buildSumPercent : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> SumPercent(e) in ?
|};
  {|
let t : Int -> Int = fun x -> x + 1 in let sepConcat : unit -> [String] -> String = fun sep -> fun sl -> case sl 
  | [] => ""
  | h :: t =>
      let f = fun a -> fun x -> ? in let base = sep in let l = t in fold_left(f)(base)(l)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let _ = Sine in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let _ = Sine(Cosine) in ?
|};
  {|
type tree = 
  + Leaf(Int)
  + Node(tree, tree)
 in let _ = let foo = fun t -> case t 
  | Leaf(n) => 1
  | Node(t1, t2) => foo(t1) + foo(t2)
end in foo(Node((Node((Leaf(1), Leaf(2))), Leaf3))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => Sine ++ exprToString(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin(pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos(pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")" ++ "/2)"
  | Time(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr)
  + KellysOp(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin(pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e2) => "cos(pi*" ++ exprToString(e2) ++ ")"
  | Average(e3, e4) =>
      "((" ++ exprToString(e3) ++ "+" ++ exprToString(e4) ++ ")/2)"
  | Times(e5, e6) => exprToString(e5) ++ "*" ++ exprToString(e6)
  | Thresh(e7, e8, e9, e10) =>
      "(" ++ exprToString(e7) ++ "<" ++ exprToString(e8) ++ "?" ++ exprToString(e9) ++ ":" ++ exprToString(e10) ++ ")"
  | Power(e11) => "((" ++ exprToString(e11) ++ ")^2)"
  | KellysOp(e1, e2, e3) =>
      "(" ++ exprToString(e1) ++ ">" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":0.0"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr)
  + KellysOp(expr, expr, expr, expr)
 in let buildKellysOp : forall d -> ? -> d = typfun d -> fun (a, b, a_more) -> KellysOp((a, b, a_more)) in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> case n 
  | _ => ?
end in ?
|};
  {|
let _ = let n = 0 in ? in ?
|};
  {|
let digitsOfInt : forall a -> Int -> [a] = typfun a -> fun n -> if n <= 0 then [] else ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "Sine" ++ exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ exprToString(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ exprToString(x) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z) ++ ")"
  | Half(x) => ".5*" ++ exprToString(x)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Half(expr)
  + Timestwo(expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ exprToString(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ exprToString(x) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z) ++ ")"
  | Half(x) => ".5*" ++ exprToString(x)
  | Third(x) => "0.33*" ++ exprToString(x)
end in ?
|};
  {|
let sepConcat : String -> [String] -> String = fun sep -> fun sl -> case sl 
  | [] => ""
  | h :: t =>
      let f = fun a -> fun x -> if length(()) == 0 then a ++ x else a ++ x ++ sep in let base = "" in let l = sl in fold_left(f)(base)(l)
end in ?
|};
  {|
let digitsOfInt : forall a -> Int -> [a] = typfun a -> fun n -> if n < 0 then [] else let a = n / 10 in let b = int_mod((n, 10)) in let c = a :: [b] in ? in ?
|};
  {|
let digitsOfInt : forall a -> Int -> [a] = typfun a -> fun n -> if n < 0 then [] else let a = n / 10 in let b = int_mod((n, 10)) in let c = a :: [b] in ? in ?
|};
  {|
let digitsOfInt : forall a -> Int -> [a] = typfun a -> fun n -> if n < 0 then [] else let a = n / 10 in let b = int_mod((n, 10)) in let c = a :: [b] in ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> a -> String = typfun a -> fun e -> case e 
  | VarX(x) => "x"
  | VarY(y) => "y"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + AbsThresh(expr, expr, expr)
  + ModThresh(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin(pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos(pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | AbsTresh(e1, e2, e3) =>
      let s = exprToString(e3) in "(abs(" ++ exprToString(e1) ++ ")<abs(" ++ exprToString(e2) ++ "?" ++ s ++ ":abs(" ++ exprToString(e4) ++ "))"
end in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> let myList = [] in ? in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> let myList = [] in ? in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n <= 0 then [] else rev(int_mod((n, 10)) :: rev(digitsOfInt(n / 10))) in let sumList : [Int] -> Int = fun xs -> case xs 
  | [] => 0
  | h :: t => h + sumList(t)
  | _ => -1
end in let additivePersistence : Int -> unit = fun n -> let count = [] in ? in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n <= 0 then [] else rev(int_mod((n, 10)) :: rev(digitsOfInt(n / 10))) in let sumList : [Int] -> Int = fun xs -> case xs 
  | [] => 0
  | h :: t => h + sumList(t)
  | _ => -1
end in let additivePersistence : Int -> unit = fun n -> let count = [0] in ? in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n <= 0 then [] else rev(int_mod((n, 10)) :: rev(digitsOfInt(n / 10))) in let sumList : [Int] -> Int = fun xs -> case xs 
  | [] => 0
  | h :: t => h + sumList(t)
  | _ => -1
end in let additivePersistence : Int -> Int = fun n -> let count = [0] in if sumList(digitsOfInt(n)) > 9 then &(1 :: count)(additivePersistence(sumList(digitsOfInt(n)))) else sumList(count) in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n <= 0 then [] else rev(int_mod((n, 10)) :: rev(digitsOfInt(n / 10))) in let sumList : [Int] -> Int = fun xs -> case xs 
  | [] => 0
  | h :: t => h + sumList(t)
  | _ => -1
end in let additivePersistence : Int -> Bool = fun n -> let count = [0] in if sumList(digitsOfInt(n)) > 9 then &&(1 :: count)(additivePersistence(sumList(digitsOfInt(n)))) else sumList(count) in ?
|};
  {|
let removeDuplicates : forall a -> [a] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates : forall a -> [a] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates : forall a -> [a] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates : forall a -> [a] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> case e 
  | VarX(x) => sprintf("%s")(x)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> case e 
  | VarX(a) => sprintf("%s")(a)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let eval : ? -> expr = fun (e, x, y) -> case e 
  | VarX => let vx = x in vx
  | VarY => let vy = y in vy
  | Average => buildAverage((vx, vy))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let build : ? -> expr = fun (rand, depth) -> case rand((1, 7)) 
  | _ =>
      buildTimes((buildCosine(buildSine(buildX(()))), buildCosine(buildSine(buildX(())))))
  | 1 => buildX(())
  | 2 => buildY(())
  | 3 => buildSine(())
  | 4 => buildSine(buildX(()))
  | 5 => buildSine(buildX(()))
  | 6 => buildSine(buildX(()))
  | 7 => buildSine(buildX(()))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let build : ? -> expr = fun (rand, depth) -> case rand((1, 7)) 
  | _ => buildCosine(())
  | 1 => buildX(())
  | 2 => buildY(())
  | 3 =>
      buildSine(if depth == 0 then buildX(()) else build((rand, depth - 1)))
  | 4 =>
      buildCosine(if depth == 0 then buildY(()) else build((rand, depth - 1)))
  | 5 =>
      buildAverage((if depth == 0 then buildX(()) else build((rand, depth - 1)), if depth == 0 then buildY(()) else build((rand, depth - 1))))
  | 6 =>
      buildTimes((if depth == 0 then buildX(()) else build((rand, depth - 1)), if depth == 0 then buildY(()) else build((rand, depth - 1))))
  | 7 =>
      buildThresh((if depth == 0 then buildX(()) else build((rand, depth - 1)), if depth == 0 then buildY(()) else build((rand, depth - 1)), if depth == 0 then buildX(()) else build((rand, depth - 1)), if depth == 0 then buildY(()) else build((rand, depth - 1))))
end in ?
|};
  {|
let removeDuplicates : forall a -> [a] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = ? in let rest' = failwith("to be written") in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let padZero : [a] -> [b] -> ? = fun l1 -> fun l2 -> if length(l1) == length(l2) then (l1, l2) else ? in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> let int = fun list -> fun digInt -> int_mod((n, 10)) :: digInt in ? in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> let int = fun list -> fun digInt -> [] in ? in ?
|};
  {|
let f : [Int] -> ? -> [Int] = fun a -> fun x -> let intlist = fun l -> if l < 10 then [l] else @(intlist(l / 10))([int_mod((l, 10))]) in case x 
  | (z, y) => case a 
  | [] => let sum = z + y in intlist(sum)
  | h :: t => let sum = h + z + y in @(intlist(sum))(t)
end
end in let _ = f([]) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let exprToString : forall a -> expr -> ? -> a = typfun a -> fun e -> case e 
  | Thresh(a, b, c, d) => exprToString@<a>(buildThresh)((a, b, c, d))
  | Times(a, b) => exprToString@<a>(buildTimes)((a, b))
  | Average(a, b) => exprToString@<a>(buildAverage)((a, b))
  | Cosine(a) => exprToString@<a>(buildCosine)(a)
  | Sine(a) => exprToString@<a>(buildSine)(a)
  | VarY => exprToString@<a>(buildY)
  | VarX => exprToString@<a>(buildX)
  | None => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let exprToString : forall a -> expr -> ? -> a = typfun a -> fun e -> case e 
  | Thresh(a, b, c, d) => exprToString@<a>(buildThresh)((a, b, c, d))
  | Times(a, b) => exprToString@<a>(buildTimes)((a, b))
  | Average(a, b) => exprToString@<a>(buildAverage)((a, b))
  | Cosine(a) => exprToString@<a>(buildCosine)(a)
  | Sine(a) => exprToString@<a>(buildSine)(a)
  | VarY => exprToString@<a>(buildY)
  | VarX => exprToString@<a>(buildX)
  | None => 0
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let exprToString : forall a -> expr -> ? -> a = typfun a -> fun e -> case e 
  | Thresh(a, b, c, d) => exprToString@<a>(Thresh)((a, b, c, d))
  | Times(a, b) => exprToString@<a>(buildTimes)((a, b))
  | Average(a, b) => exprToString@<a>(buildAverage)((a, b))
  | Cosine(a) => exprToString@<a>(buildCosine)(a)
  | Sine(a) => exprToString@<a>(buildSine)(a)
  | VarY => exprToString@<a>(buildY)
  | VarX => exprToString@<a>(buildX)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let exprToString : forall a -> expr -> Int -> a = typfun a -> fun e -> case e 
  | Thresh(a, b, c, d) => exprToString@<a>(Thresh)(a * b * c * d)
  | Times(a, b) => exprToString@<a>(buildTimes)((a, b))
  | Average(a, b) => exprToString@<a>(buildAverage)((a, b))
  | Cosine(a) => exprToString@<a>(buildCosine)(a)
  | Sine(a) => exprToString@<a>(buildSine)(a)
  | VarY => exprToString@<a>(buildY)
  | VarX => exprToString@<a>(buildX)
end in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone@<a>(x)(n - 1))([x])
end in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone@<a>(0)(length(l1) - length(l2)))(l2)) else (append(clone@<a>(0)(length(l2) - length(l1)))(l1), l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case x 
  | ([], []) => []
  | ([h1 :: t1], [h2 :: t2]) => ?
end in let base = [] in let args = l1(l2) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone@<a>(x)(n - 1))([x])
end in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone@<a>(0)(length(l1) - length(l2)))(l2)) else (append(clone@<a>(0)(length(l2) - length(l1)))(l1), l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case x 
  | ([], []) => []
  | ([h1 :: t1], [h2 :: t2]) => ?
end in let base = [] in let args = l1(l2) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone@<a>(x)(n - 1))([x])
end in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone@<a>(0)(length(l1) - length(l2)))(l2)) else (append(clone@<a>(0)(length(l2) - length(l1)))(l1), l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case x 
  | ([], []) => []
  | ([h1 :: t1], [h2 :: t2]) => ?
end in let base = [] in let args = l1(l2) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone@<a>(x)(n - 1))([x])
end in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone@<a>(0)(length(l1) - length(l2)))(l2)) else (append(clone@<a>(0)(length(l2) - length(l1)))(l1), l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case (a, x) 
  | (h :: t, (x1, x2)) => ?
end in let base = [0] in let args = rev(combine(l1)(l2)) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone@<a>(x)(n - 1))([x])
end in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone@<a>(0)(length(l1) - length(l2)))(l2)) else (append(clone@<a>(0)(length(l2) - length(l1)))(l1), l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case (a, x) 
  | (h :: t, (x1, x2)) => ?
end in let base = [0] in let args = rev(combine(l1)(l2)) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone@<a>(x)(n - 1))([x])
end in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone@<a>(0)(length(l1) - length(l2)))(l2)) else (append(clone@<a>(0)(length(l2) - length(l1)))(l1), l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case (a, x) 
  | (h :: t, (x1, x2) :: t2) => ?
end in let base = [0] in let args = rev(combine(l1)(l2)) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "sine(pi*" ++ exprToString(d) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "sine(pi*" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
let removeDuplicates : forall a -> [unit] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates : forall a -> forall b -> [a] -> [b] = typfun a -> typfun b -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates : forall a -> forall b -> [a] -> [b] = typfun a -> typfun b -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX(x) => "x"
  | VarY(y) => "y"
  | Sine(s) => "sin (pi*" ++ exprString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos (pi*" ++ exprToString(e) ++ ")"
  | Averages => "((" ++ exprToString(e) ++ "*" ++ exprToString(e) ++ ")/2)"
  | Times => "(" ++ exprToString(e) ++ "*" ++ exprToString(e) ++ ")"
  | Thresh =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(e) ++ "?" ++ exprToString(e) ++ ":" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos (pi*" ++ exprToString(e) ++ ")"
  | Average(e) => "((" ++ exprToString(e) ++ "*" ++ exprToString(e) ++ ")/2)"
  | Times(e) => "(" ++ exprToString(e) ++ "*" ++ exprToString(e) ++ ")"
  | Thresh(e) =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(e) ++ "?" ++ exprToString(e) ++ ":" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Timmy1(expr, expr, expr)
  + Timmy2(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e, f) =>
      "((" ++ exprToString(e) ++ "*" ++ exprToString(f) ++ ")/2)"
  | Times(e, f) => "(" ++ exprToString(e) ++ "*" ++ exprToString(f) ++ ")"
  | Thresh(e, f, g, h) =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(f) ++ "?" ++ exprToString(g) ++ ":" ++ exprToString(h) ++ ")"
  | Timmy1(e1, e2, e3) =>
      "(sin(pi*" ++ exprToString(e1) ++ ")+" ++ "cos(pi*" ++ exprToString(e2) ++ "))*" ++ "cos(pi*" ++ exprToString(e) ++ ")"
  | Timmy2(e1, e2) =>
      "(sin(pi*" ++ exprToString(e1) ++ ")/" ++ "cos(pi*" ++ exprToString(e2) ++ "))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Timmy1(expr, expr, expr)
  + Timmy2(expr, expr, expr, expr)
 in let buildTimmy2 : forall c -> ? -> c = typfun c -> fun (e1, e2) -> Timmy2((e1, e2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Timmy1(expr, expr)
  + Timmy2(expr, expr, expr)
 in let buildTimmy2 : forall c -> ? -> c = typfun c -> fun (e1, e2) -> Timmy2((e1, e2)) in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> if n <= 0 then [] else x :: clone@<a>(x)(n - 1) in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> let s1 = length(l1) in let s2 = length(l2) in if s1 < s2 then (@(clone@<a>(0)(s2 - s1))(l1), l2) else if s2 < s1 then (l1, @(clone@<a>(0)(s1 - s2))(l2)) else (l1, l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => []
  | h :: t => if !=(h)(0) then h :: t else removeZero(t)
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case a 
  | (_, []) => (fst(x) + snd(x) / 10, [int_mod((fst(x) + snd(x), 10))])
  | (c, h :: t) =>
      let sum = c + fst(x) + snd(x) in (sum / 10, int_mod((sum, 10)) :: snd(a))
end in let base = (0, []) in let args = @(combine(rev(l1))(rev(l2)))([(0, 0)]) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in let bigMul : forall a -> forall b -> a -> [b] -> [Int] = typfun a -> typfun b -> fun l1 -> fun l2 -> let f = fun a -> fun x -> (fst(a), bigAdd(())(())) in let base = (0, []) in let args = rev(l2) in let (_, res) = fold_left(f)(base)(args) in res in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "%s"(e)
  | VarY => "%s"(e)
  | Sine => "%s"(e)
  | Cosine => "%s"(e)
  | Average => "%s"(e)
  | Times => "%s"(e)
  | Thresh => "%s"(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "%s"(e)
  | VarY => "%s"(e)
  | Sine => "%s %s"(e)(e)
  | Cosine => "%s"(e)
  | Average => "%s"(e)
  | Times => "%s"(e)
  | Thresh => "%s"(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "%s"(e)
  | VarY => "%s"(e)
  | Sine => "%s %s"(e)
  | Cosine => "%s"(e)
  | Average => "%s"(e)
  | Times => "%s"(e)
  | Thresh => "%s"(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Special1(expr, expr, expr)
  + Special2(expr, expr)
 in let buildSpecial1 : forall c -> ? -> c = typfun c -> fun (e1, e2) -> Special1((e1, e2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => @(VarY)(@("/")(VarX))
  | Cosine => @(VarX)(@("/")(VarY))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => VarY ++ "/" ++ VarX
  | Cosine => VarX ++ "/" ++ VarY
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => exprToString(e)
  | Cosine => exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => exprToString(VarX)
  | Cosine => exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => exprToString(e)
  | Cosine => exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e) => "(" ++ exprToString(e) ++ "+" ++ exprToString(e) ++ ")/2"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildSine : expr -> expr = fun e -> Sine(e) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let eval : ? -> unit -> expr = fun (e, x, y) -> case e 
  | VarX => buildX
  | VarY => buildY
  | Sine => buildSine(e)
end in ?
|};
  {|
let bigAdd : forall a -> forall b -> forall c -> a -> b -> c = typfun a -> typfun b -> typfun c -> fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> map(fun x -> x + a)(x) in let base = hd(L1) in let args = l2 in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> a = typfun a -> fun e -> case e 
  | VarX(e) => printf("%s")(e)
  | VarY(e) => printf("%s")(e)
  | Sine(e) => printf("sin(%s)")(e)
  | Cosine(e) => printf("cos(%s)")(e)
  | (Average(e1), e2) => printf("%s+%s/2")(e1)(e2)
  | (Times(e1), e2) => printf("%s*%s")(e1)(e2)
  | (Thresh(e1), e2, e3, e4) => printf("(%s<%s?%s:%s)")(e1)(e2)(e3)(e4)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squared(expr)
  + Flatten(expr, expr, expr)
 in let buildFlatten : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> Flatten(e) in ?
|};
  {|
let _ = () in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let a = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ ex(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ ex(t) ++ ")"
  | Average(s, t) => "((" ++ ex(s) ++ "+" ++ ex(t) ++ ")/2)"
  | Times(s, t) => ex(s) ++ "*" ++ ex(t)
  | Thresh(s, t, u, v) =>
      "(" ++ ex(s) ++ "<" ++ ex(t) ++ "?" ++ ex(u) ++ ":" ++ ex(v) ++ ")"
  | Extra(s, t, u) =>
      "sin(pi*" ++ ex(s) ++ ") * cos (" ++ ex(t) ++ ") * sin(" ++ ex(u) ++ ":" ++ ex(v) ++ ")"
  | Stuff(t) => "cos(pi*" ++ "(sin(pi*" ++ ex(t) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let a = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ ex(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ ex(t) ++ ")"
  | Average(s, t) => "((" ++ ex(s) ++ "+" ++ ex(t) ++ ")/2)"
  | Times(s, t) => ex(s) ++ "*" ++ ex(t)
  | Thresh(s, t, u, v) =>
      "(" ++ ex(s) ++ "<" ++ ex(t) ++ "?" ++ ex(u) ++ ":" ++ ex(v) ++ ")"
  | Extra(s, t, u) =>
      "sin(pi*" ++ ex(s) ++ ") * cos (" ++ ex(t) ++ ") * sin(" ++ ex(u) ++ ")"
  | Stuff(t) => "cos(pi*" ++ "(sin(pi*" ++ ex(t) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let a = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ e(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ e(t) ++ ")"
  | Average(s, t) => "((" ++ e(s) ++ "+" ++ e(t) ++ ")/2)"
  | Times(s, t) => e(s) ++ "*" ++ e(t)
  | Thresh(s, t, u, v) =>
      "(" ++ e(s) ++ "<" ++ e(t) ++ "?" ++ e(u) ++ ":" ++ e(v) ++ ")"
  | Extra(s, t, u) =>
      "sin(pi*" ++ e(s) ++ ") * cos (" ++ e(t) ++ ") * sin(" ++ e(u) ++ ")"
  | Stuff(t) => "cos(pi*" ++ "(sin(pi*" ++ e(t) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let a = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ e(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ e(t) ++ ")"
  | Average(s, t) => "((" ++ e(s) ++ "+" ++ e(t) ++ ")/2)"
  | Times(s, t) => e(s) ++ "*" ++ e(t)
  | Thresh(s, t, u, v) =>
      "(" ++ e(s) ++ "<" ++ e(t) ++ "?" ++ e(u) ++ ":" ++ e(v) ++ ")"
  | Stuff(t) => "cos(pi*" ++ "(sin(pi*" ++ e(t) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ exprToString(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ exprToString(t) ++ ")"
  | Average(s, t) =>
      "((" ++ exprToString(s) ++ "+" ++ exprToString(t) ++ ")/2)"
  | Times(s, t) => exprToString(s) ++ "*" ++ exprToString(t)
  | Thresh(s, t, u, v) =>
      "(" ++ exprToString(s) ++ "<" ++ exprToString(t) ++ "?" ++ exprToString(u) ++ ":" ++ exprToString(v) ++ ")"
  | Square(s) => "(" ++ exprToString(s) ++ ")^2"
  | Volume(s, t, u) =>
      "Vol(H: " ++ exprToString(s) ++ ", W: " ++ exprToString(t) ++ ", L: " ++ exprToString(u) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr)
  + Volume(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqaure(expr)
  + Volume(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ exprToString(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ exprToString(t) ++ ")"
  | Average(s, t) =>
      "((" ++ exprToString(s) ++ "+" ++ exprToString(t) ++ ")/2)"
  | Times(s, t) => exprToString(s) ++ "*" ++ exprToString(t)
  | Thresh(s, t, u, v) =>
      "(" ++ exprToString(s) ++ "<" ++ exprToString(t) ++ "?" ++ exprToString(u) ++ ":" ++ exprToString(v) ++ ")"
  | Square(s) => "(" ++ exprToString(s) ++ ")^2"
  | Volume(s, t, u) =>
      "Vol(H: " ++ exprToString(s) ++ ", W: " ++ exprToString(t) ++ ", L: " ++ exprToString(u) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "cos(pi*" ++ exprToString(v) ++ ")"
  | Average(v) => "((" ++ exprToString(v) ++ "+" ++ exprToString(v) ++ ")/2)"
  | Times(v) => exprToString(v) ++ "*" ++ exprToString(v)
  | Thresh(v) =>
      "(" ++ exprToString(v) ++ "<" ++ exprToString(v) ++ "?" ++ exprToString(v) ++ ":" ++ exprToString(v) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exprToString(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exprToString(a) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | SquareAv(a, b) =>
      "(" ++ exprToString(a) ++ "^2 + " ++ exprToString(b) ++ "^2)/2"
  | MultHalf(a, b, c) =>
      "(" ++ exprToString(a) ++ "*" ++ exprToString(b) ++ "*" ++ exprToString(c) ++ ")/2"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> case e 
  | VarX(x) => printf("%s")(x)
  | VarY(y) => printf("%s")(y)
end in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> if n <= 0 then [] else x :: clone@<a>(x)(n - 1) in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> let difference = length(l1) - length(l2) in if difference > 0 then (l1, @(clone@<a>(0)(difference))(l2)) else if difference < 0 then (@(clone@<a>(0)(-1 * difference))(l1), l2) else (l1, l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else h :: t
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> let sum = case x 
  | (x1, x2) => x1 + x2
end in ? in let base = [] in let args = rev(combine(l1)(l2)) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> if n <= 0 then [] else x :: clone@<a>(x)(n - 1) in let padZero : [Int] -> [Int] -> ? = fun l1 -> fun l2 -> let difference = length(l1) - length(l2) in if difference > 0 then (l1, @(clone@<a>(0)(difference))(l2)) else if difference < 0 then (@(clone@<a>(0)(-1 * difference))(l1), l2) else (l1, l2) in let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else h :: t
end in let bigAdd : [Int] -> [Int] -> [Int] = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> ? in let base = (0, []) in let args = let combine = fun (a, b) -> a + b in map(combine)(rev(combine(l1)(l2))) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> case e 
  | VarX(x) => int_to_string(x)
  | VarY(y) => int_to_string(y)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let build : ? -> expr = fun (rand, depth) -> ? in ?
|};
  {|
let lastListElement : forall a -> [a] -> a = typfun a -> fun n -> case n 
  | [] => failwith("ERROR: List must be of size 1 or greater")
  | [x] => x
  | x :: y => lastListElement@<a>(y)
end in let catLists : Bool -> [Bool] -> [Bool] = fun x -> fun y -> if not(x) == [] then case x 
  | [x] => x :: y
  | h :: t => catLists(t)(lastListElement@<a>(x) :: y)
end else if x == [] then y else ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX(n) => "x"
  | VarY(n) => "y"
  | Sine(n) => "sin(" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(" ++ exprToString(n) ++ ")"
  | Average(n) =>
      let (x, y) = n in "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(n) =>
      let (x, y) = n in "((" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ ")"
  | Thresh(n) =>
      let (x, y, z, w) = n in "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(n) => "sin(" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(" ++ exprToString(n) ++ ")"
  | Average(n) =>
      let (x, y) = n in "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(n) =>
      let (x, y) = n in "((" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ ")"
  | Thresh(n) =>
      let (x, y, z, w) = n in "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval : ? -> Float = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(n) => sin(3.14 *. eval((n, x, y)))
  | Consine(n) => cos(3.14 *. eval((n, x, y)))
  | Average(m, n) => eval((m, x, y)) +. eval((n, x, y)) /. 2
  | Times(m, n) => eval((m, x, y)) *. eval((n, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let build : ? -> expr = fun (rand, depth) -> if depth == 0 then let g = rand((0, 1)) in case g 
  | 0 => VarX
  | 1 => VarY
end else let g = rand((0, 4)) in case g 
  | 0 => Sine(build((rand, depth - 1)))
  | 1 => Cosine(build((rand, depth - 1)))
  | 2 => Average((build((rand, depth - 1)), build((rand, depth - 1))))
  | 3 => Times((build((rand, depth - 1)), build((rand, depth - 1))))
  | 4 => Thresh((build((rand, depth - 1)), build((rand, depth - 1))))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Log(expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Op(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(n) => "sin(pi*" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(pi*" ++ exprToString(n) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, w) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w) ++ ")"
  | Power(x, y) => exprToString(x) ++ "**" ++ exprToString(y)
  | Op(x, y, z) =>
      "(" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ ")/" ++ exprToString(z)
end in let _ = exprToString(Log(VarX)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Op(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(n) => "sin(pi*" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(pi*" ++ exprToString(n) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, w) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w) ++ ")"
  | Power(x, y) => exprToString(x) ++ "**" ++ exprToString(y)
  | Op(x, y, z) =>
      "(" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ ")/" ++ exprToString(z)
end in let _ = exprToString(Op((VarX, VarY))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Op(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqrt(expr)
  + Op(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(n) => "sin(pi*" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(pi*" ++ exprToString(n) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, w) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w) ++ ")"
  | Sqrt(x) => "sqrt(" ++ exprToString(x) ++ ")"
  | Op(x, y, z) =>
      "(" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ "*" ++ exprToString(z) ++ ")/(" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ "+" ++ exprToString(z) ++ ")"
end in let _ = exprToString(Power((VarX, VarY))) in ?
|};
  {|
let pipe : forall a -> [[a]] -> [a] = typfun a -> fun fs -> let f = fun a -> fun x -> case fs 
  | h :: t => h
end in let base = [] in fold_left(f)(base)(fs) in ?
|};
  {|
let pipe : [Int -> Int] -> Int = fun fs -> let f = fun a -> fun x -> x(a) in let base = 0 in fold_left(f)(base)(fs) in let _ = pipe([]) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "e"
  | VarY => "e"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e) => "((" ++ exprToString(e) ++ "+" ++ exprToString(e) ++ ")/2)"
  | Times(e) => exprToString(e) ++ "" ++ exprToString(e)
  | Thresh(e) =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(e) ++ " ? " ++ exprToString(e) ++ " : " ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + TimesThree(expr, expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | TimesThree(e) =>
      exprToString(e) ++ "*" ++ exprToString(f) ++ "*" ++ exprToString(f)
  | Average(e, f) =>
      "((" ++ exprToString(e) ++ "+" ++ exprToString(f) ++ ")/2)"
  | Times(e, f) => exprToString(e) ++ "*" ++ exprToString(f)
  | Thresh(e, f, g, h) =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(f) ++ "?" ++ exprToString(g) ++ ":" ++ exprToString(h) ++ ")"
end in ?
|};
  {|
let listReverse : forall a -> [[a]] -> [a] = typfun a -> fun l -> ? in ?
|};
  {|
let removeDuplicates : forall a -> [a] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates : forall a -> [a] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> case e 
  | VarX(x) => x
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> a = typfun a -> fun e -> case e 
  | VarX(X) => X
  | VarY(Y) => Y
  | Sine(N) => Sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> a = typfun a -> fun e -> case e 
  | VarX => X
  | VarY(Y) => Y
  | Sine(N) => Sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> a = typfun a -> fun e -> case e 
  | VarX => X
  | VarY => Y
  | Sine(N) => Sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> Float = fun e -> case e 
  | VarX => X
  | VarY => Y
  | Sine(N) => sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> Float = fun e -> case e 
  | VarX => X
  | VarY => Y
  | Sine => sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> Float = fun e -> case e 
  | VarX => exprToString(X)
  | VarY => Y
  | Sine(e1) => sin(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine => sin(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "sin"
  | Cosine => "cos"
  | Average => "avg"
  | Times => "*"
  | Thresh => "/"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin"
  | Cosine => "cos"
  | Average => "avg"
  | Times => "*"
  | Thresh => "/"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin" + exprToSring(e1)
  | Cosine(e1) => "cos" + exprToString(e1)
  | Average(e1, e2) => "avg"
  | Times(e1, e2) => exprToSring(e1) + "*"
  | Thresh => "/"
end in ?
|};
  {|
let clone : forall a -> [a] -> Int -> unit = typfun a -> fun x -> fun n -> ? in ?
|};
  {|
let removeZero : [Int] -> [Int] = fun l -> case l 
  | [] => []
  | h :: t => if h == 0 then removeZero(t) else h :: t
end in let mulByDigit : Int -> [Int] -> [Int] = fun i -> fun l -> let f = fun a -> fun x -> let carry = i * x in case a 
  | h :: t => h + carry / 10 :: int_mod((h + carry, 10)) :: t
  | _ => carry / 10 :: [int_mod((carry, 10))]
end in let base = [] in removeZero(fold_left(f)(base)(rev(l))) in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> Float = fun e -> case e 
  | VarX(x) => x
  | VarY(y) => y
  | Sine(e1) => sin(e1)
  | Cosine(e1) => cos(e1)
  | Average(e1, e2) => e1 + e2 / 2
  | Times(e1, e2) => e1 * e2
  | Thresh(e1, e2, e3, e4) => e1 * e2 * e3 * e4
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + SqXPlusY(expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + SqXPlusYDiv2(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin (pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos (pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ " + " ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ " * " ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | TimesTimes(e1, e2, e3) =>
      exprToString(e1) ++ " * " ++ exprToString(e2) ++ " * " ++ exprToString(e3)
  | SqXPlusY(e1, e2) =>
      "(" ++ exprToString(e1) ++ " * " ++ exprToString(e1) ++ ") + (" ++ exprToString(e2) ++ "/2"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + Cube(expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin (pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos (pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ " + " ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ " * " ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | TimesTimes(e1, e2, e3) =>
      exprToString(e1) ++ " * " ++ exprToString(e2) ++ " * " ++ exprToString(e3)
  | Cube =>
      exprToString(e1) ++ " * " ++ exprToString(e1) ++ " * " ++ exprToString(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + Cube(expr)
  + MultDivBy6(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin (pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos (pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ " + " ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ " * " ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | TimesTimes(e1, e2, e3) =>
      exprToString(e1) ++ " * " ++ exprToString(e2) ++ " * " ++ exprToString(e3)
  | Cube(e1) =>
      exprToString(e1) ++ " * " ++ exprToString(e1) ++ " * " ++ exprToString(e1)
  | MultDivBy6 =>
      "(("(exprToString(e1)) ++ " * " ++ exprToString(e2) ++ ") /6)"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> [expr] -> [a] = typfun a -> fun e -> case e 
  | [] => []
  | h :: e' => case h 
  | VarX => "x" ++ exprToString@<a>(e')
  | VarY => "y" ++ exprToString@<a>(e')
  | Sine => "sin(pi*" ++ exprToString@<a>(e') ++ ")"
  | Cosine => "cos(pi*" ++ exprToString@<a>(e') ++ ")"
  | Average =>
      let (e1, e2) = h in "((" ++ exprToString@<a>(e1) ++ "+" ++ exprToString@<a>(e2) ++ ")/2)" ++ exprToString@<a>(e')
  | Times =>
      let (e1, e2) = h in exprToString@<a>(e1) ++ "*" ++ exprToString@<a>(e2) ++ exprToString@<a>(e')
  | Thresh =>
      let (e1, e2, e3, e4) = h in "(" ++ exprToString@<a>(e1) ++ "<" ++ exprToString@<a>(e2) ++ "?" ++ exprToString@<a>(e3) ++ ":" ++ exprToString@<a>(e4) ++ ")" ++ exprToString@<a>(e')
end
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let build : ? -> expr = fun (rand, depth) -> if depth == 0 then if rand(0)(1) == 0 then buildX(()) else buildY(()) else let x = rand(0)(6) in case x 
  | 0 => buildX(())
  | 1 => buildY(())
  | 2 => buildSine(build((rand, depth - 1)))
  | 3 => buildCosine(build((rand, depth - 1)))
  | 4 => buildAverage((build((rand, depth - 1)), build((rand, depth - 1))))
  | 5 => buildTimes((build((rand, depth - 1)), build((rand, depth - 1))))
  | 6 =>
      buildThresh((build((rand, depth - 1)), build((rand, depth - 1)), build((rand, depth - 1)), build((rand, depth - 1))))
  | _ => []
end in ?
|};
  {|
let wwhile : forall a -> ? -> a = typfun a -> fun (f, b) -> let x = f(b) in case x 
  | h :: t => if t == true then wwhile@<a>((f, h)) else h
end in ?
|};
  {|
let wwhile : forall a -> ? -> a = typfun a -> fun (f, b) -> let x = f(b) in case x 
  | h :: t => if t == false then h else wwhile@<a>((f, h))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Square(i1) => exprToString(i1)("*")(exprToString)(i1)
  | Exponential(i1, i2) => exprToString(i1)("*")(exprToString)(i2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
 in let buildSquare : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> Square(e) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
  + Exponential(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Square(i1) => exprToString(i1)("*")(exprToString)(i1)
  | Exponential(i1, i2) => exprToString(i1)("*")(exprToString)(i2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
  + Exponential(expr, expr)
 in let buildSquare : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> Square(e) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
  + Exponential(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Square(i) => exprToString(i)("*")(exprToString)(i)
  | Exponential(i1, i2) => exprToString(i1)("*")(exprToString)(i2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr)
  + Exponential(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Cubic(i1, i2, i3) =>
      exprToString(i1) ++ "*" ++ exprToString(i2) ++ "*" ++ exprToString(i3)
  | Exponential(i1, i2) => exprToString(i1) ++ "^" ++ exprToString(i2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr, expr)
  + Exponential(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Cubic(i1, i2, i3) =>
      exprToString(i1) ++ "*" ++ exprToString(i2) ++ "*" ++ exprToString(i3)
  | Exponential(i1, i2) => exprToString(i1) ++ "^" ++ exprToString(i2)
end in ?
|};
  {|
let clone : forall a -> a -> Int -> unit = typfun a -> fun x -> fun n -> ? in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> ? in ?
|};
  {|
let equiv : forall a -> forall b -> [a] -> [b] -> unit = typfun a -> typfun b -> fun x -> fun y -> case x 
  | [] => ?
end in ?
|};
  {|
let equiv : forall a -> forall b -> [a] -> [b] -> unit = typfun a -> typfun b -> fun x -> fun y -> case x 
  | [] => ?
end in ?
|};
  {|
let equiv : forall a -> [a] -> [a] -> unit = typfun a -> fun x -> fun y -> case x 
  | h :: tl => ?
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Square(e) => "%s*%s"(exprToString)(e)(exprToString)(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Golden(expr)
  + MeanPi(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(expr) => "sin(pi*" ++ exprToString(expr) ++ ")"
  | Cosine(expr) => "cos(pi*" ++ exprToString(expr) ++ ")"
  | Average(expr1, expr2) =>
      "(" ++ exprToString(expr1) ++ "+" ++ exprToString(expr2) ++ ")/2"
  | Times(expr1, expr2) => exprToString(expr1) ++ "*" ++ exprToString(expr2)
  | Thresh(expr1, expr2, expr3, expr4) =>
      "(" ++ exprToString(expr1) ++ "<" ++ exprToString(expr2) ++ "?" ++ exprToString(expr3) ++ ":" ++ exprToString(expr4) ++ ")"
  | Golden => ""
  | MeanPi => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Golden(expr)
  + MeanPi(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(expr) => "sin(pi*" ++ exprToString(expr) ++ ")"
  | Cosine(expr) => "cos(pi*" ++ exprToString(expr) ++ ")"
  | Average(expr1, expr2) =>
      "(" ++ exprToString(expr1) ++ "+" ++ exprToString(expr2) ++ ")/2"
  | Times(expr1, expr2) => exprToString(expr1) ++ "*" ++ exprToString(expr2)
  | Thresh(expr1, expr2, expr3, expr4) =>
      "(" ++ exprToString(expr1) ++ "<" ++ exprToString(expr2) ++ "?" ++ exprToString(expr3) ++ ":" ++ exprToString(expr4) ++ ")"
  | Golden => ""
  | MeanPi => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(rest) => "sin(pi" ++ exprToString(rest) ++ ")"
  | Cosine(rest) => "cos(pi" ++ exprToString(rest) ++ ")"
  | Average(expr1, expr2) =>
      "(" ++ exprToString(expr1) ++ "+" ++ exprToString(expr2) ++ "/2)"
  | Times(expr1, expr2) => exprToString(expr1) ++ "*" ++ exprToString(expr2)
  | Thresh =>
      "(" ++ exprToString(expr1) ++ "<" ++ exprToString(expr2) ++ "?" ++ exprToString(expr3) ++ ":" ++ exprToString(expr4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqrt(expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(rest) => "sin(pi*" ++ exprToString(rest) ++ ")"
  | Cosine(rest) => "cos(pi*" ++ exprToString(rest) ++ ")"
  | Average(expr1, expr2) =>
      "(" ++ exprToString(expr1) ++ "+" ++ exprToString(expr2) ++ "/2)"
  | Times(expr1, expr2) => exprToString(expr1) ++ "*" ++ exprToString(expr2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Sqrt(e1) => "sqrt(" ++ exprToString(e1) ++ ")"
end in ?
|};
  {|
let wwhile : forall b -> ? -> [b] = typfun b -> fun (f, b) -> let x = wwhile@<b>((f, b)) in let h :: t = x in case t 
  | false => h
  | true => wwhile@<b>((f, h))
end in ?
|};
  {|
let wwhile : forall b -> ? -> [b] = typfun b -> fun (f, b) -> let x = wwhile@<b>((f, b)) in let h :: t = x in case [t] 
  | false => h
  | true => wwhile@<b>((f, h))
end in ?
|};
  {|
let wwhile : forall a -> ? -> a = typfun a -> fun (f, b) -> let x = f(b) in let h :: t = x in let r :: l = t in case t 
  | false => h
  | true => wwhile@<a>((f, h))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> a = typfun a -> fun e -> case e 
  | VarX(s) => printf("%s")
  | VarY => printf("%s")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> expr -> a = typfun a -> fun e -> case e 
  | VarX(s) => s
  | VarY => printf("%s")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "sin(pi*" + exprToString + ")"
  | Cosine => "cos(pi*" + exprToString + ")"
  | Average => "((" + exprToString + "+" + exprToString + ")/2"
  | Times => exprToString + "*" + exprToString
  | Thresh =>
      "(" + exprToString + "?" + exprToString + ":" + exprToString + ")"
  | _ => 0
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let eval : ? -> expr = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine => buildSine(e)
  | Cosine => buildCosine(e)
  | Average => buildAverage((e1, e2))
  | Times => buildTimes((e1, e2))
  | Thresh => buildThresh((a, b, a_less, b_less))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(s) => "sin(pi*" ++ exprToString(s) ++ ")"
  | Cosine(s) => "cos(pi*" ++ exprToString(s) ++ ")"
  | Average(s, p) =>
      "((" ++ exprToString(s) ++ "+" ++ exprToString(p) ++ ")/2"
  | Times(s, p) => exprToString(s) ++ "*" ++ exprToString(p)
  | Thresh(s, p, r, d) =>
      "(" ++ exprToString(s) ++ "<" ++ exprToString(p) ++ "?" ++ exprToString(r) ++ ":" ++ exprToString(d) ++ ")"
  | AllMult(s, p, r) =>
      exprToString(s) ++ "*" ++ exprToString(p) ++ "*" ++ exprToString(p)
  | AvgThree(s, p, r) =>
      "((" ++ exprToString(s) ++ "+" ++ exprToString(p) ++ "+" ++ exprToString(p) ++ ")/2"
end in ?
|};
  {|
let pipe : forall a -> forall b -> [(a -> [b]) -> a -> [b]] -> a -> [b] = typfun a -> typfun b -> fun fs -> let f = fun a -> fun x -> x(a) in let base = fun x -> [] in fold_left(f)(base)(fs) in ?
|};
  {|
let filter : forall a -> [a] -> a -> [a] = typfun a -> fun l -> fun a -> case l 
  | [] => []
  | h :: t => if a == h then filter@<a>(t)(a) else h :: filter@<a>(t)(a)
end in let removeDuplicates : forall a -> [[a]] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = h in let rest' = h :: filter@<a>(t)(h) in helper((seen', rest'))
end in rev(helper(([], l))) in let removeDuplicates : forall a -> [[a]] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = h in let rest' = h :: filter@<a>(t)(h) in helper((seen', rest'))
end in removeDuplicates@<a>(helper([])) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Expn(expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(b) => "sin(pi*" ++ exprToString(b) ++ ")"
  | Cosine(b) => "cos(pi*" ++ exprToString(b) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | Eval(a, b) => "(" ++ exprToString(a) ++ "^" ++ exprToString(b) ++ ")"
  | _ => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Expn(expr, expr)
  + TripMult(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(b) => "sin(pi*" ++ exprToString(b) ++ ")"
  | Cosine(b) => "cos(pi*" ++ exprToString(b) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | Expn(b) => "(0.5^" ++ exprToString(b) ++ ")"
  | TripMult(a, b, c) =>
      "(" ++ exprToString(a) ++ "*" ++ exprToString(b) ++ "*" ++ exprToString(c) ++ ")"
  | _ => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Expn(expr, expr)
  + TripMult(expr, expr, expr)
 in let buildExpn : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun b -> Expn(b) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage : ? -> expr = fun (e1, e2) -> Average((e1, e2)) in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildThresh : ? -> expr = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes : ? -> expr = fun (e1, e2) -> Times((e1, e2)) in let eval : forall a -> ? -> a = typfun a -> fun (e, x, y) -> case e 
  | VarX(a) => x
  | VarY(b) => y
  | Sine(x1) => eval@<a>((buildSine(x1), x, y))
  | Cosine(x2) => eval@<a>((buildCosine(x2), x, y))
  | Average(x3, x4) => eval@<a>((buildAverage((x3, x4)), x, y))
  | Times(x5, x6) => eval@<a>((buildTimes((x5, x6)), x, y))
  | Thresh(x7, x8, x9, x0) => eval@<a>((buildThresh((x7, x8, x9, x0)), x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Root(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Pivot(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Flip(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Pivot(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x1) => "sin(pi*" ++ exprToString(x1) ++ ")"
  | Cosine(x2) => "cos(pi*" ++ exprToString(x2) ++ ")"
  | Root(x3) => "sqrt(" ++ exprToString(x3) ++ ")"
  | Average(x4, x5) =>
      "((" ++ exprToString(x4) ++ "+" ++ exprToString(x5) ++ ")/2)"
  | Times(x6, x7) => exprToString(x6) ++ "*" ++ exprToString(x7)
  | Thresh(x8, x9, x10, x11) =>
      "(" ++ exprToString(x8) ++ "<" ++ exprToString(x9) ++ "?" ++ exprToString(x10) ++ ":" ++ exprToString(x11) ++ ")"
  | Pivot(x12, x13, x14) =>
      "(" ++ exprToString(x12) ++ "<0?" ++ exprToString(x13) ++ ":" ++ exprToString(x14) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine => "Sine(" ++ exprToString(e) ++ ")"
  | Cosine => "Cosine(" ++ exprToString(e) ++ ")"
  | Average => "Average(" ++ exprToString(e) ++ ")"
  | Times => "Times(" ++ exprToString(e) ++ ")"
  | Thresh(a, b, c, d) =>
      "Thresh(" ++ exprToString(a) ++ "," ++ exprToString(b) ++ "," ++ exprToString(c) ++ "," ++ exprToString(d) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ exprToString(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ exprToString(x) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, s) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(s) ++ ")"
  | Trip(x, y, z) =>
      "((" ++ exprToString(x) ++ "%30.0)" ++ exprToString ++ "%" ++ exprToString(z) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Trip(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ exprToString(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ exprToString(x) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, s) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(s) ++ ")"
  | Trip(x, y, z) =>
      "((" ++ exprToString(x) ++ "/30.0)+" ++ exprToString(y) ++ "/" ++ exprToString(z) ++ ")"
  | Greater(x, y) =>
      "(" ++ exprToString(x) ++ ">" ++ exprToString(y) ++ "?" ++ exprToString(x) ++ ":" ++ exprToString(y) ++ ")"
end in ?
|};
  {|
let removeDuplicates : forall a -> forall b -> [a] -> [b] = typfun a -> typfun b -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = [] in let rest' = rev(t) in if mem(h)(rest') then rest == t else h :: seen'(helper)((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates : forall a -> forall b -> [a] -> [b] = typfun a -> typfun b -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let rest' = rev(t) in let seen' = seen in ?
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates : forall a -> forall b -> [a] -> [b] = typfun a -> typfun b -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let rest' = rev(t) in let seen' = seen in ?
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VaryY => "y"
  | Sine(ex) => "sin(pi*" ++ exprToString(ex) ++ ")"
  | Cosine(ex) => "cos(pi*" ++ exprToString(ex) ++ ")"
  | Average(ex1, ex2) =>
      "(" ++ exprToString(ex1) ++ "*" ++ exprToString(ex2) ++ ")/2"
  | Times(ex1, ex2) => exprToString(ex1) ++ "*" ++ exprToString(ex2)
  | Thresh(ex1, ex2, ex3, ex4) =>
      "(" ++ exprToString(ex1) ++ "<" ++ exprToString(ex2) ++ "?" ++ exprToString(ex3) ++ ":" ++ exprToString(ex4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SinCos(expr)
 in let eval : ? -> Float = fun (e, x, y) -> let pi = 3.14 in case e 
  | VarX => x
  | VarY => y
  | Sine(ex) => sin(pi *. eval((ex, x, y)))
  | Cosine(ex) => cos(pi *. eval((ex, x, y)))
  | Average(ex1, ex2) => eval((ex1, x, y)) +. eval((ex2, x, y)) /. 2.
  | Times(ex1, ex2) => eval((ex1, x, y)) *. eval((ex2, x, y))
  | Thresh(ex1, ex2, ex3, ex4) =>
      if eval((ex1, x, y)) < eval((ex2, x, y)) then eval((ex3, x, y)) else eval((ex4, x, y))
  | SinCos(ex) => sin(pi *. eval((ex, x, y))) *. cos(pi *. eval((ex, x, y)))
  | Three(ex1, ex2, ex3) =>
      eval((ex1, x, y)) *. cos(pi *. eval((ex2, x, y))) *. sin(pi *. eval((ex3, x, y)))
end in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> if n < 1 then [] else x :: clone@<a>(x)(n - 1) in let padZero : forall a -> forall b -> [a] -> [b] -> unit = typfun a -> typfun b -> fun l1 -> fun l2 -> let difference = length(l1) - length(l2) in ? in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> if n < 1 then [] else x :: clone@<a>(x)(n - 1) in let padZero : forall a -> [[Int]] -> [a] -> [[Int]] = typfun a -> fun l1 -> fun l2 -> let difference1 = length(l1) - length(l2) in let difference2 = length(l2) - length(l1) in if difference1 > 0 then clone@<a>(0)(difference1) :: l1 else ? in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> if n < 1 then [] else x :: clone@<a>(x)(n - 1) in let padZero : forall a -> forall b -> [a] -> [b] -> unit = typfun a -> typfun b -> fun l1 -> fun l2 -> let difference1 = length(l1) - length(l2) in let difference2 = length(l2) - length(l1) in ? in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> if n < 1 then [] else x :: clone@<a>(x)(n - 1) in let padZero : forall a -> forall b -> [a] -> [b] -> unit = typfun a -> typfun b -> fun l1 -> fun l2 -> let difference1 = length(l1) - length(l2) in let difference2 = length(l2) - length(l1) in ? in ?
|};
  {|
let clone : forall a -> a -> Int -> [a] = typfun a -> fun x -> fun n -> if n < 1 then [] else x :: clone@<a>(x)(n - 1) in let padZero : forall a -> forall b -> [a] -> [b] -> unit = typfun a -> typfun b -> fun l1 -> fun l2 -> let difference1 = length(l1) - length(l2) in let difference2 = length(l2) - length(l1) in ? in ?
|};
  {|
let digitsOfInt : Int -> [Int] = fun n -> if n < 10 then [n] else [int_mod((n, 10))] in ?
|};
  {|
let listReverse : forall a -> [a] -> [a] = typfun a -> fun l -> case l 
  | [] => []
  | h :: t => [h]
end in ?
|};
  {|
let removeDuplicates : forall a -> forall b -> [a] -> [b] = typfun a -> typfun b -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = mem(h)(t) in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*"(exprToString)(e)(")")
  | Cosine(e) => "cos(pi*"(exprToString)(e)(")")
  | Average(e) =>
      "(("(exprToString)(e) ++ "+" ++ exprToString(e)(")") / 2(")")
  | Times(e) => exprToString(e)("*")(exprToString)(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*"(exprToString)(e)(")")
  | Cosine(e) => "cos(pi*"(exprToString)(e)(")")
  | Average(e) => ("(("(exprToString)(e), exprToString(e)(")") / 2(")"))
  | Times(e) => exprToString(e)("*")(exprToString)(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*"(exprToString)(e)(")")
  | Cosine(e) => "cos(pi*"(exprToString)(e)(")")
  | Average(x, y) =>
      "(("(exprToString)(e) ++ "+" ++ exprToString(e)(")") / 2(")")
  | Times(e) => exprToString(e)("*")(exprToString)(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotanget(e) => "(" ++ 1. /. "(" ++ tan ++ "("(exprToString)(e) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => "(" ++ 1. /. "(" ++ tan ++ "("(exprToString)(e) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => 1 ++ "/cot"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => "contan" ++ exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => "contan(" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => "cot(" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Cotangent(expr)
  + Volume(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Squares(e) => exprToString(e)("*")(exprToString)(e)
  | Volume(l, w, h) =>
      "(" ++ exprToString(e) ++ "*(" ++ exprToString(e) ++ ")*" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr, expr)
  + Volume(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Squares(e) => exprToString(e) ++ "*" ++ exprToString(e)
  | Volume(l, w, h) =>
      "(" ++ exprToString(e) ++ "*(" ++ exprToString(e) ++ ")*" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr, expr)
  + Volume(expr, expr, expr)
 in let buildSquares : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> Squares(e) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr)
  + Volume(expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Squares(e) => exprToString(e) ++ "*" ++ exprToString(e)
  | Substract(j, k) => "(" ++ exprToString(e) ++ "-" ++ exprToString(e)(")")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr)
  + Volume(expr, expr, expr)
 in let buildSubstract : forall c -> ? -> c = typfun c -> fun (j, k) -> Volume((j, k)) in ?
|};
  {|
let padZero : [a] -> [b] -> [?] = fun l1 -> fun l2 -> if length(l1) == length(l2) then [(l1, l2)] else let numZeros = length(l1) - length(l2) in ? in ?
|};
  {|
let padZero : forall a -> forall b -> [a] -> [b] -> unit = typfun a -> typfun b -> fun l1 -> fun l2 -> ? in ?
|};
  {|
let removeDuplicates : forall a -> [[Bool]] -> [a] = typfun a -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = mem(seen)(h) in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun e -> case e 
  | VarX(s) => printf("%s")(s)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine => "Sine"
  | Cosine => "Cosine"
  | Average => "Average"
  | Times => "Times"
  | Thresh => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine(e) => "Sine"
  | Cosine(e) => "Cosine"
  | Average(e) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine(e) => "Sine"
  | Cosine(e) => "Cosine"
  | Average(expr) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "VarY"
  | Sine(e) => "Sine"
  | Cosine(e) => "Cosine"
  | Average(expr) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => %("Sin (pi*%s)")(s)
  | Cosine(e) => "Cosine"
  | Average(expr) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"
  | Average(expr) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => %("(%s*%s)/2")(e)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => %("(%s*%s)/2")(e(e))
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => "(%s + %s)/2"(e)(e)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average => "(%s + %s)/2"(e)(p)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => "(%s + %s)/2"(e)(p)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => "(%s + %s)/2"(e)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e, ex) => "("(exprToString)(e)("+")(exprToString)(ex)(")/2")
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e, ex) => "(("(exprToString)(e)("+")(exprToString)(ex)(")/2)")
  | Times(e, ex) => exprToString(e)("*")(exprToString)(ex)
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let buildX : unit -> expr = fun () -> VarX in let buildY : unit -> expr = fun () -> VarY in let build : ? -> expr = fun (rand, depth) -> if depth > 0 then case rand 
  | (0, 2) => buildX(())
  | (3, 5) => buildY(())
  | (6, 10) => buildSine(build((rand, depth - 1)))
  | (11, 18) => buildCosine(build((rand, depth - 1)))
end else () in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let build : ? -> expr = fun (rand, depth) -> if depth > 0 then case rand 
  | (6, 10) => buildSine(build((rand, depth - 1)))
  | (11, 18) => buildCosine(build((rand, depth - 1)))
end else () in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Half(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Half(e) => exprToString(e) ++ "/2"
  | Neg(e) => "-" ++ exprToString(e)
  | Average(e, ex) =>
      "((" ++ exprToString(e) ++ "+" ++ exprToString(ex) ++ ")/2)"
  | Times(e, ex) => exprToString(e) ++ "*" ++ exprToString(ex)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + AddMul(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Neg(e) => exprToString(e) ++ " * -1.0"
  | Average(e, ex) =>
      "((" ++ exprToString(e) ++ "+" ++ exprToString(ex) ++ ")/2)"
  | Times(e, ex) => exprToString(e) ++ "*" ++ exprToString(ex)
  | AveThree(e1, e2, e3) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ "+" ++ exprToString(e3)(")/3")
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + AddMul(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Neg(e) => exprToString(e) ++ " * -1.0"
  | Average(e, ex) =>
      "((" ++ exprToString(e) ++ "+" ++ exprToString(ex) ++ ")/2)"
  | Times(e, ex) => exprToString(e) ++ "*" ++ exprToString(ex)
  | AveThree(e1, e2, e3) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ "+" ++ exprToString(e3) ++ ")/3"
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine : expr -> expr = fun e -> Cosine(e) in let buildSine : expr -> expr = fun e -> Sine(e) in let _ = Thresh((buildSine(buildCosine(VarX)), VarX, VarY, VarZ)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Tower(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let ex = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ ex(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ ex(x) ++ ")"
  | Average(x, y) => "((" ++ ex(x) ++ "+" ++ ex(y) ++ ")/2)"
  | Times(x, y) => ex(x) ++ "*" ++ ex(y)
  | Thresh(w, x, y, z) =>
      "(" ++ ex(w) ++ "<" ++ ex(x) ++ "?" ++ ex(y) ++ ":" ++ ex(z) ++ ")"
  | Power(x, y) => ex(x) ++ "^" ++ ex(y)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr, expr)
 in let exprToString : expr -> String = fun e -> let ex = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ ex(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ ex(x) ++ ")"
  | Average(x, y) => "((" ++ ex(x) ++ "+" ++ ex(y) ++ ")/2)"
  | Times(x, y) => ex(x) ++ "*" ++ ex(y)
  | Thresh(w, x, y, z) =>
      "(" ++ ex(w) ++ "<" ++ ex(x) ++ "?" ++ ex(y) ++ ":" ++ ex(z) ++ ")"
  | SqDist(x, y) => ex(x) ++ "^2+" ++ ex(y) ++ "^2"
end in ?
|};
  {|
let wwhile : forall a -> ? -> a = typfun a -> fun (f, b) -> let (b', c') = f(b) in if c' == true then wwhile@<a>((f, b')) else b' in let fixpoint : forall a -> ? -> a = typfun a -> fun (f, b) -> wwhile@<a>((fun x -> (f(b), NOT(b == f(b))), b)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Average(v) => "(" ++ exprToString(v) ++ "+" ++ exprToString(v) ++ ")/2"
  | Times(v) => exprToString(v) ++ "*" ++ exprToString(v)
  | Thresh(v) =>
      exprToString(v) ++ "<" ++ exprToString(v) ++ "?" ++ exprToString(v) ++ ":" ++ exprToString(v)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Average(v, w) =>
      "(" ++ exprToString(v) ++ "+" ++ exprToString(w) ++ ")/2"
  | Times(v) => exprToString(v) ++ "*" ++ exprToString(v)
  | Thresh(v) =>
      exprToString(v) ++ "<" ++ exprToString(v) ++ "?" ++ exprToString(v) ++ ":" ++ exprToString(v)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval : ? -> Float = fun (e, x, y) -> let pi = 3.142 in case e 
  | VarX => x
  | VarY => y
  | Sine(v) => sin(pi *. eval((v, x, y)))
  | Cosine(v) => cos(pi *. eval((v, x, y)))
  | Average(v, w) => eval((v, x, y)) +. eval((w, x, y)) /. 2.
  | Times(v, w) => eval((v, x, y)) *. eval((w, x, y))
  | Thresh(v, w, q, r) =>
      if eval((v, x, y)) < eval((w, x, y)) then eval((q, x, y)) else eval((r, x, y))
  | Divide(v, w) => eval((v, x, y)) / eval((w, x, y))
  | Super(v, w) => eval((v, x, y)) + eval((w, x, y)) * eval((v, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Average(v, w) =>
      "(" ++ exprToString(v) ++ "+" ++ exprToString(w) ++ ")/2"
  | Times(v, w) => exprToString(v) ++ "*" ++ exprToString(w)
  | Thresh(v, w, x, y) =>
      exprToString(v) ++ "<" ++ exprToString(w) ++ "?" ++ exprToString(x) ++ ":" ++ exprToString(y)
  | Divide(v, w) => exprToString(v) ++ "/" ++ exprToString(w)
  | Super(v, w) =>
      "(" ++ exprToString(v) ++ "+" ++ exprToString(w) ++ ") *" ++ exprToString(v)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval : forall c -> ? -> c = typfun c -> fun (e, x, y) -> case e 
  | VarX(x') => printf("%s")(x)
  | VarY(y') => printf("%s")(y)
  | Sine(sin) => printf("sin(%s)")(sin)
  | Cosine(cos) => printf("cos(%s)")(cos)
  | Average(ave) => printf("((%s+%s)/2)")(ave)
  | Times(t) => printf("%s*%s")(t)
  | Thresh(th) => printf("(%s<*%s?%s:%s)")(th)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(sine) => "sin(pi*" ++ exprToString(sine) ++ ")"
  | Cosine(cosine) => "cos(pi*" ++ exprToString(cosine) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(t1, t2) => exprToString(t1) ++ "*" ++ exprToString(t2)
  | Thresh(th1, th2, th3, th4) =>
      "(" ++ exprToString(th1) ++ "<" ++ exprToString(th2) ++ "?" ++ exprToString(th3) ++ ":" ++ exprToString(th4) ++ ")"
  | Circ(circ1, circ2) =>
      "(" ++ exprToString(circ1) ++ "^2+" ++ exprToString(circ2) ++ ")"
  | NatLog(nlog) => "ln(" ++ nlog ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Fibonacci(expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(sine) => "sin(pi*" ++ exprToString(sine) ++ ")"
  | Cosine(cosine) => "cos(pi*" ++ exprToString(cosine) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(t1, t2) => exprToString(t1) ++ "*" ++ exprToString(t2)
  | Thresh(th1, th2, th3, th4) =>
      "(" ++ exprToString(th1) ++ "<" ++ exprToString(th2) ++ "?" ++ exprToString(th3) ++ ":" ++ exprToString(th4) ++ ")"
  | Circ(circ1, circ2) =>
      "(" ++ exprToString(circ1) ++ "^2+" ++ exprToString(circ2) ++ ")"
  | Arcsin(m4) => "asin(" ++ exprToString(m4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Oscillate(expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(sine) => "sin(pi*" ++ exprToString(sine) ++ ")"
  | Cosine(cosine) => "cos(pi*" ++ exprToString(cosine) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(t1, t2) => exprToString(t1) ++ "*" ++ exprToString(t2)
  | Thresh(th1, th2, th3, th4) =>
      "(" ++ exprToString(th1) ++ "<" ++ exprToString(th2) ++ "?" ++ exprToString(th3) ++ ":" ++ exprToString(th4) ++ ")"
  | Circ(circ1) => "sqrt(|1-" ++ exprToString(circ1) ++ "^2|)"
  | Oscillate(m4) =>
      "(" ++ exprToString(m4) ++ "/((1-" ++ exprToString(m4) ++ ")^2+" ++ exprToString(m4) ++ "^2))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Oscillate(expr)
 in let buildCirc : forall a -> forall b -> a -> b = typfun a -> typfun b -> fun c1 -> Circ(c1) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr, expr)
  + Oscillate(expr)
 in let buildCirc : forall c -> ? -> c = typfun c -> fun (c1, c2) -> Circ((c1, c2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VaryY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ " +" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ " * " ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Tan(a) => "sin(pi*" ++ exp(a) ++ ")/(cos(pi*" ++ exp(a) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tan(expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Tan(a) => "sin(pi*" ++ exp(a) ++ ")/(cos(pi*" ++ exp(a) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tan(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Tangent(a) => "sin(pi*" ++ exp(a) ++ ")/(cos(pi*" ++ exp(a) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Hoi(expr, expr)
  + Average(expr, expr)
  + Times(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Hoi(a, b, c) =>
      "sin(pi*" ++ exp(a) ++ ")"("*")("cos(pi*" ++ exp(b) ++ ")")("/2")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Hoi(expr, expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Boo(a) => "((" ++ exp(a) ++ "+" ++ exp(a) ++ ")/100)"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Hoi(a, b, c) =>
      "sin(pi*" ++ exp(a) ++ ")*cos(pi*" ++ exp(b) ++ ")/(" ++ exp(c) ++ ")"
end in ?
|};
  {|
let digitsOfInt : Int -> Int = fun n -> let numL = [] in if n / 10 > 0 then &&(int_mod((n, 10)) :: numL)(digitsOfInt(n) / 10) else numL in ?
|};
  {|
let digitsOfInt : Int -> unit = fun n -> let sumL = [] in ? in ?
|};
  {|
let removeDuplicates : forall a -> forall b -> [a] -> [b] = typfun a -> typfun b -> fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = if mem(h)(t) then true else false in let rest' = failwith("to be written") in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : forall a -> forall b -> a -> [b] = typfun a -> typfun b -> fun e -> [Thresh(?)] in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString : expr -> String = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Exp(e') => "e^" ++ exprToString(e')
end in ?
|};
]

let ill_typed_dynamic : string list = [
  {|
let sumList = fun xs -> case xs 
  | [] => []
  | h1 :: h2 :: t => h1 + h2(sumList)(t)
end in ?
|};
  {|
let sumList = fun xs -> case xs 
  | [] => []
  | x :: xs' => x + 1(sumList)(xs')
end in ?
|};
  {|
let sumList = fun xs -> case xs 
  | [] => []
  | x :: xs' => x(sumList)(xs')
end in ?
|};
  {|
let sumList = fun xs -> case xs 
  | [] => []
  | x :: xs' => x + sumList(xs')
end in ?
|};
  {|
let sumList = fun xs -> case xs 
  | [] => []
  | x :: xs' => 1(sumList)(xs')
end in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else int_mod((n, 10)) :: digitsOfInt(n / 10) in let sumList = fun xs -> case xs 
  | [] => 0
  | x :: xs' => x + sumList(xs')
end in let sum = fun n :: [i] -> if ||(n < 10)([]) then sumList(digitsOfInt(n)) :: [i] else sumList(digitsOfInt(n)) :: [1 + 1] in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(m) => "sin(" ++ exprToString(m) ++ ")"
  | Cosine(m) => "cos(" ++ exprToString(m) ++ ")"
  | Average(m, n) =>
      "((" ++ exprToString(m) ++ "+" ++ exprToString(n) ++ ")/2)"
  | Times(m, n) => exprToString(m) ++ "*" ++ exprToString(n)
  | Tresh(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?" ++ exprToString(o) ++ ":" ++ exprToString(p)
end in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | Sine(m) => sin(pi *. eval((m, x, y)))
  | _ => x
end in let _ = eval((Sine, 0.5, 0.)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let build = fun (rand, depth) -> if depth == 0 then let num = rand((0, 2)) in case num 
  | 0 => VarX
  | _ => VarY
end else let num = rand((0, 5)) in case num 
  | 0 => BuildSine(build((rand, depth - 1)))
  | _ => Cosine(build((rand, depth - 1)))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + MyExp(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(m) => "sin(pi*" ++ exprToString(m) ++ ")"
  | Cosine(m) => "cos(pi*" ++ exprToString(m) ++ ")"
  | Square(m) => "(" ++ exprToString(m) ++ "^2)"
  | Average(m, n) =>
      "((" ++ exprToString(m) ++ "+" ++ exprToString(n) ++ ")/2)"
  | Times(m, n) => exprToString(m) ++ "*" ++ exprToString(n)
  | MyExpr(m, n, o) =>
      "(" ++ exprToString(m) ++ "<" ++ expToString ++ "?sqrt(|" ++ exprToString(o) ++ "|)" ++ ":" ++ "(" ++ exprToString(o) ++ "/2)"
  | Thresh(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?" ++ exprToString(o) ++ ":" ++ exprToString(p) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + MyExp(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(m) => "sin(pi*" ++ exprToString(m) ++ ")"
  | Cosine(m) => "cos(pi*" ++ exprToString(m) ++ ")"
  | Square(m) => "(" ++ exprToString(m) ++ "^2)"
  | Average(m, n) =>
      "((" ++ exprToString(m) ++ "+" ++ exprToString(n) ++ ")/2)"
  | Times(m, n) => exprToString(m) ++ "*" ++ exprToString(n)
  | Thresh(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?" ++ exprToString(o) ++ ":" ++ exprToString(p) ++ ")"
end in let _ = exprToString(MyExpr((VarX, VarY, VarX))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + MyExpr(expr, expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(m) => "sin(pi*" ++ exprToString(m) ++ ")"
  | Cosine(m) => "cos(pi*" ++ exprToString(m) ++ ")"
  | Square(m) => "(" ++ exprToString(m) ++ "^2)"
  | Average(m, n) =>
      "((" ++ exprToString(m) ++ "+" ++ exprToString(n) ++ ")/2)"
  | Times(m, n) => exprToString(m) ++ "*" ++ exprToString(n)
  | MyExpr(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?sqrt(|" ++ exprToString(o) ++ "|)" ++ ":" ++ "(" ++ exprToString(p) ++ "/2)"
  | Thresh(m, n, o, p) =>
      "(" ++ exprToString(m) ++ "<" ++ exprToString(n) ++ "?" ++ exprToString(o) ++ ":" ++ exprToString(p) ++ ")"
end in let _ = exprToString(MyExpr((VarX, VarY, VarX))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + MyExpr(expr, expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildMyExpr = fun (a, b, a_less) -> MyExpr((a, b, a_less)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Arc(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sin_Avg(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exprToString(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exprToString(a) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | _ => "_"
  | Tan(a) => "tan(pi*" ++ exprToString(a) ++ ")"
  | Arc(a, b, c) =>
      "sin(pi*(" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ exprToString(c) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sin_Avg(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exprToString(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exprToString(a) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | _ => "_"
  | Tan(a) => "tan(pi*" ++ exprToString(a) ++ ")"
  | Sin_Avg(a, b, c) =>
      "sin(pi*(" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ exprToString(c) ++ ")/3)"
end in let _ = exprToString(Sin_Avg((VarX(()), VarY(()), VarX(())))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sin_Avg(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exprToString(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exprToString(a) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | _ => "_"
  | Tan(a) => "tan(pi*" ++ exprToString(a) ++ ")"
  | Sin_Avg(a, b, c) =>
      "sin(pi*(" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ exprToString(c) ++ ")/3)"
end in let _ = exprToString(Sin_Avg((Average((VarX(()), VarY(()))), VarY(()), VarX(())))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sin_Avg(expr, expr, expr)
 in let x = Sin_Avg((VarX(()), VarY(()), VarX(()))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr)
  + Sine_Avg(expr, expr, expr)
 in let buildSine_Avg = fun (e1, e2) -> Sine_Avg((e1, e2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")" ++ "/" ++ "2" ++ ")"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Expwn(e) => "phi^" ++ exprToString(e)
  | Tan(e) => "tan(pi*" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Expwn(e) => phi ** eval((e, x, y))
  | Tan(e) => sin(pi *. eval((e, x, y))) /. cos(pi *. eval((e, x, y)))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Custom1(expr, expr, expr)
 in let buildCustom1 = fun e -> Custom1(e) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sin(e') => "sin (pi*" ++ expr(e') ++ ")"
  | Cos(e') => "cos (pi*" ++ expr(e') ++ ")"
  | (Average(e1), e2) =>
      "((" ++ esprToString(e1) ++ " + " ++ exprToString(e2) ++ "/2)"
  | (Times(e1), e2) => exprToString(e1) ++ " * " ++ exprToString(e2)
  | (Thresh(e1), e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(x', y') => x +. y /. 2.
  | Times(x', y') => x *. y
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in let _ = eval(Thresh((VarX, VarY, Sine(VarX), Cos(VarY), 1., 2.))) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(x', y') => x +. y /. 2.
  | Times(x', y') => x *. y
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in let _ = eval((Thresh((VarX, VarY, Sine(VarX), Cos(VarY))), 1., 2.)) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in let _ = eval((Cosine(Average), 0.5, 0.2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SquareRoot(expr)
  + FunckyCube(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | SquareRoot(e') => sqrt(eval)((e', x, y))
  | FunckyRoot(e1, e2, e3) =>
      sqrt(sqrt(eval)((e', x, x))(sqrt(eval)((e', x, y)))(sqrt(eval)((e', y, y))))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SquareRoot(expr)
  + FunckyCube(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SquareRoot(expr)
  + FunckyRoot(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ "/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | SquareRoot(e') => "sqrt(" ++ exprToString(e') ++ ")"
  | FunckyCube(e1, e2, e3) =>
      "sqrt(sqrt(" ++ exprToString(e1) ++ ")+sqrt(" ++ exprToString(e2) ++ ")+sqrt(" ++ exprToString(e3) ++ "))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Op1(expr)
  + Op2(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Op1(e) =>
      "((tan(pi*" ++ exprToString(e) ++ "))-(tan(pi*" ++ exprToString(e) ++ "))/2)"
  | Op2(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ ">" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Op1(expr)
  + Op2(expr, expr, expr)
 in let buildOp2 = fun () -> Op2(()) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Op1(expr)
  + Op2(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Op1(e) => tan(pi *. eval((e, x, y))) -. tan(pi *. eval((e, x, y))) / 2.
  | Op2(e1, e2, e3, e4) =>
      if eval((e1, x, y)) > eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Op1(expr)
  + Op2(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Op1(e) => tan(pi *. eval((e, x, y))) -. tan(pi *. eval((e, x, y))) / 2.
  | Op2(e1, e2, e3, e4) =>
      if eval((e1, x, y)) > eval((e2, x, y)) then eval((e3, x, y)) else eval((e1, x, y)) -. eval((e2, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Op1(expr)
  + Op2(expr, expr, expr)
 in let buildOp2 = fun (a, b, a_less, b_less) -> Op2((a, b, a_less, b_less)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let ets = fun e -> fun s -> case e 
  | [] => s
  | VarX => ets((e, s ++ VarX))
  | VarY => ets((e, s ++ VarY))
  | Sine => ets((e, s ++ Sine))
  | Cosine => ets((e, s ++ Cosine))
  | Average => ets((e, s ++ Average))
  | Times => ets((e, s ++ Times))
  | Thresh => ets((e, s ++ Thresh))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let ets = fun e -> fun s -> case e 
  | VarX => ets((e, s ++ VarX))
  | VarY => ets((e, s ++ VarY))
  | Sine => ets((e, s ++ Sine))
  | Cosine => ets((e, s ++ Cosine))
  | Average => ets((e, s ++ Average))
  | Times => ets((e, s ++ Times))
  | Thresh => ets((e, s ++ Thresh))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + XOR3(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi^2))"
  | Asin(e1, e2, e3) => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + XOR3(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi^2))"
  | Asin(e1, e2, e3) => "1"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + XOR3(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi*pi))"
  | Asin(e1, e2, e3) => "1"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Accossin(e1, e2) => acos(eval(e1)) *. asin(eval(e2)) *. 2. /. pi *. pi
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + Crazy(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi*pi))"
  | Crazy(e1, e2, e3) => exprToString(e2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + Crazy(expr, expr)
 in let buildCrazy = fun (e1, e2, e3) -> Crazy((e1, e2, e3)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + Crazy(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Acossin(e1, e2) =>
      acos(eval((e1, x, y))) *. asin(eval((e2, x, y))) *. 2. /. pi *. pi
  | Crazy(e1, e2, e3) => eval(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + Crazy(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Acossin(e1, e2) =>
      acos(eval((e1, x, y))) *. asin(eval((e2, x, y))) *. 2. /. pi *. pi
  | Crazy(e1, e2, e3) => eval((e1, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Acossin(expr, expr)
  + Crazy(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Acossin(e1, e2) =>
      "(acos(" ++ exprToString(e1) ++ ")*asin(" ++ exprToString(e2) ++ ")*2/(pi*pi))"
  | Crazy(e1, e2, e3) => exprToString(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Thresh((VarXarY, VarX, Times((Sine(VarX), Cosine(erage((VarX, VarY))))))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let expr = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ expr(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ expr(t) ++ ")"
  | Average(s, t) => "((" ++ ex(s) ++ "+" ++ ex(t) ++ ")/2)"
  | Times(s, t) => ex(s) ++ "*" ++ ex(t)
  | Thresh(s, t, u, v) =>
      "(" ++ ex(s) ++ "<" ++ ex(t) ++ "?" ++ ex(u) ++ ":" ++ ex(v) ++ ")"
  | FunnyTimes(s, t, u) =>
      "(floor " ++ ex(s) ++ "* ceil " ++ ex(t) ++ "*" ++ ex(u) ++ ")"
  | Sqr(s) => "(" ++ ex(s) ++ "*" ++ ex(s) ++ ")"
end in ?
|};
  {|
let palindrome = fun w -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(ex) => "sin (pi*)" ++ exprToString(ex) ++ ")"
  | Cosine(ex) => "cos (pi*)" ++ exprToString(ex) ++ ")"
  | Average(ex1, ex2) =>
      "((" ++ exprToSring(ex1) ++ " + " ++ exprToString(ex2) ++ ")/2)"
  | Times(ex1, ex2) => exprToString(expr1) ++ " * " ++ exprToString(expr2)
  | Tresh(ex1, ex2, ex3, ex4) =>
      "(" ++ exprToString(expr1) ++ "<" ++ exprToString(expr2) ++ " ? " ++ exprToString(expr3) ++ " : " ++ exprToString(expr4) ++ ")"
end in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in let _ = eval((NewExprA((VarX, Vary)), 1., -1.)) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + NewExprA(expr, expr)
  + NewExprB(expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | NewExprA(e1, e2) =>
      if eval((e1, x, y)) > eval((e2, x, y)) then eval((e1, x, y)) else eval((e2, x, y))
  | NewExprB(e1, e2, e3) =>
      eval((e1, x, y)) +. eval((e2, x, y)) -. eval((e3, x, y))
end in let _ = eval((NewExprA((VarX, Vary)), 1., -1.)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + NewExprA(expr, expr)
  + NewExprB(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | NewExprA(e1, e2) =>
      if eval((e1, x, y)) > eval((e2, x, y)) then eval((e1, x, y)) else eval((e2, x, y))
  | NewExprB(e1, e2, e3) =>
      eval((e1, x, y)) +. eval((e2, x, y)) *. eval((e3, x, y))
end in ?
|};
  {|
let digitsOfInt = fun n -> if n < 0 then [] else if n == 0 then [0] else ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildHelper = fun rand -> fun max_depth -> fun curr_depth -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> failwith("to be written") in let _ = eval((Sine(Cos(Varx)), 0.5, -0.5)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> failwith("to be written") in let _ = eval((Sine(Varx), 0.5, -0.5)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine = fun e -> Cosine(e) in let eval = fun (e, x, y) -> let evalhelper = fun e -> fun x -> fun y -> case e 
  | VarX => x
  | VarY => y
  | Sine(p1) => evalhelper(Sine)(p1)(x)(y)
  | Cosine(p1) => evalhelper(buildCosine)(p1)(x)(y)
end in evalhelper(e)(x)(y) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> let evalhelper = fun e -> fun x -> fun y -> case e 
  | VarX => float(x)
  | VarY => float(y)
  | Sine(p1) => sin(pi *. evalhelper(p1)(x)(y))
  | Cosine(p1) => cos(pi *. evalhelper(p1)(x)(y))
end in evalhelper(e)(x)(y) in let _ = eval((Sine(Varx), 0.5, -0.5)) in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Plus(expr, expr)
  + Cube(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "cos(pi*" ++ exprToString(v) ++ ")"
  | Average(v, w) =>
      "((" ++ exprToString(v) ++ "+" ++ exprToString(w) ++ ")/2)"
  | Times(v, w) => exprToString(v) ++ "*" ++ exprToString(w)
  | Thresh(v, w, x, y) =>
      exprToString(v) ++ "<" ++ exprToString(w) ++ "?" ++ exprToString(x) ++ ":" ++ exprToString(y) ++ ")"
  | Plus(v) => "(" ++ exprToString(v) ++ "+"(exprToString)(w) ++ ")"
  | Cube(v, w, x) =>
      "(" ++ exprToString(v) ++ "*" ++ exprToString(w) ++ "*" ++ exprToString(x)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Threshold(e1, e2, e3, e4) =>
      exprToString(e1) ++ "<" ++ exprToString(e2)("?") ++ exprToString(e3) ++ "?"(exprToString)(e4)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Log(expr)
  + SumOfSquares(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e1) => sin(pi *. eval((e1, x, y)))
  | Cosine(e1) => cos(pi *. eval((e1, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | ModF(e1) => case modf(eval((e1, x, y)) *. 10) 
  | (f, i) => f
end
| SumOfSquares(e1, e2, e3) =>
    eval((e1, x, y)) ** 2. +. eval((e2, x, y)) ** 2. +. eval((e3, x, y)) ** 2. /. 3.
end in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => printf("A")
  | VarY => printf("A")
  | Sine => printf("A")
  | Cosine => printf("A")
  | Average => printf("A")
  | Times => printf("A")
  | Thresh => printf("A")
end in ?
|};
  {|
let a = (1, 2) in let (c, d) = (1, 2) in let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Foo(expr, expr)
  + Clamp(expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(eval((a, x, y)) *. pi)
  | Cosine(a) => cos(eval((a, x, y)) *. pi)
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Foo(a, b) =>
      if eval((a, x, y)) < eval((b, x, y)) then 0.9 *. eval((a, x, y)) else 0.1 *. eval((a, x, y))
  | Clamp(a, b, c) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((b, x, y)) else if eval((a, x, y)) > eval((c, x, y)) then eval((c, x, y)) else eval((a, x, y))
end in let _ = eval((Clamp((Sine(Varx), VarX, VarY)), 1, 2)) in ?
|};
  {|
let _ = [] in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let acc = fun curr -> fun result -> case curr 
  | VarX(x) => sprintf(x)
  | VarY(y) => sprintf(y)
end in acc(e)(exprToString)(VarX) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let acc = fun curr -> fun result -> case curr 
  | VarX(x) => sprintf("%s")(x)
  | VarY(y) => sprintf("%s")(y)
end in acc(e)(exprToString)(VarX) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let acc = fun curr -> fun result -> case curr 
  | VarX(x) => sprintf("%s")(x)
  | VarY(y) => sprintf("%s")(y)
end in acc(e)("")(exprToString)(VarX) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let acc = fun curr -> fun result -> case curr 
  | VarX => sprintf("x")
  | VarY(y) => sprintf("y")
end in acc(e)("")(exprToString)(VarX) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqrt(expr)
  + Abs(expr)
  + Quad(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqrt(expr)
  + Abs(expr)
  + Gauss(expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Sqrt(e) => sqrt(abs_float(eval((e, x, y))))
  | Gauss(e1, e2, e3) =>
      2. *. exp(eval((e1, x, y)) -. eval((e2, x, y)) ** 2. /. eval((e3, x, y))) -. 1.
  | _ => failwith("we are seriously writing a lisp compiler god save us all")
end in let _ = eval((Quad((VarX, VarY, VarX)), 0.5, 0.5)) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqrt(expr)
  + Abs(expr)
  + Gauss(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqrt(expr)
  + Abs(expr)
  + Logistic(expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e') => sin(pi *. eval((e', x, y)))
  | Cosine(e') => cos(pi *. eval((e', x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Sqrt(e) => sqrt(abs_float(eval((e, x, y))))
  | Logistic(e1, e2, e3) =>
      2. /. 1. -. exp(~-.(eval((e1, x, y)) *. eval((e2, x, y)))) -. 1. ** eval((e3, x, y))
  | _ => failwith("error")
end in let _ = eval((Gauss((VarX, VarY, VarX)), 0.5, 0.5)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Tangent(expr, expr)
  + Square2(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Diff2(expr, expr)
  + Square2(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExprTangent = Thresh((VarX, VarY, VarX, Tangent((Sine(VarX), Cosine(Average((VarX, VarY))))))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(x) => x
  | VarY(y) => y
  | Sine(s) => Sine(exprToString(s))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e1) => sin(pi *. eval((e1, x, y)))
  | Cosine(e1) => cos(pi *. eval((e1, x, y)))
  | Average(e1, e2) =>
      eval((e1, x, y)) +. eval((e2, x, y)) /. float_of_int(2)
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Power(e1, e2) => eval((e1, x, y)) ** eval((e2, x, y))
  | Comp(e1, e2, e3) =>
      -1 * eval((e1, x, y)) * eval((e2, x, y)) * eval((e3, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Magic(expr)
  + Weird(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin(pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos(pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => "" ++ exprToString(e1) ++ "*" ++ exprToString(e2) ++ ""
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Magic(e1) => "tan(pi*" ++ exprToString(e1) ++ ")"
  | Weird(e1, e2, e3, e4) =>
      "(tan(" ++ exprToString(e1) ++ "*" ++ exprToString(e2) ++ "*" ++ exprToString(e3) ++ "))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Magic(expr)
  + Weird(expr, expr, expr)
 in let buildWeird = fun (e1, e2, e3, e4) -> Weird((e1, e2, e3, e4)) in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else if int_mod((n, 10)) == 0 then 0 :: digitsOfInt(n / 10) else if int_mod((n - 1, 10)) == 0 then 1 :: digitsOfInt(n - 1 / 10) else if int_mod((n - 2, 10)) == 0 then 1 :: digitsOfInt(n - 2 / 10) else if int_mod((n - 3, 10)) == 0 then 1 :: digitsOfInt(n - 3 / 10) else if int_mod((n - 4, 10)) == 0 then 1 :: digitsOfInt(n - 4 / 10) else if int_mod((n - 5, 10)) == 0 then 1 :: digitsOfInt(n - 5 / 10) else if int_mod((n - 6, 10)) == 0 then 1 :: digitsOfInt(n - 6 / 10) else if int_mod((n - 7, 10)) == 0 then 1 :: digitsOfInt(n - 7 / 10) else if int_mod((n - 8, 10)) == 0 then 1 :: digitsOfInt(n - 8 / 10) else ? in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else if int_mod((n, 10)) == 0 then 0 :: digitsOfInt(n / 10) else if int_mod((n - 1, 10)) == 0 then 1 :: digitsOfInt(n - 1 / 10) else if int_mod((n - 2, 10)) == 0 then 2 :: digitsOfInt(n - 2 / 10) else if int_mod((n - 3, 10)) == 0 then 3 :: digitsOfInt(n - 3 / 10) else if int_mod((n - 4, 10)) == 0 then 4 :: digitsOfInt(n - 4 / 10) else if int_mod((n - 5, 10)) == 0 then 5 :: digitsOfInt(n - 5 / 10) else if int_mod((n - 6, 10)) == 0 then 6 :: digitsOfInt(n - 6 / 10) else if int_mod((n - 7, 10)) == 0 then 7 :: digitsOfInt(n - 7 / 10) else if int_mod((n - 8, 10)) == 0 then 8 :: digitsOfInt(n - 8 / 10) else ? in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else if int_mod((n, 10)) == 0 then 0 :: digitsOfInt(n / 10) else ? in ?
|};
  {|
let listReverse = fun l -> let reverseHelper = fun acc -> if [] then acc else reverseHelper(h :: acc)(t) in reverseHelper([])(l) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildY = fun () -> VarY in let build = fun (rand, depth) -> let case = rand((0, 6)) in ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + FiboPlus(expr, expr, expr)
  + TheThing(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(ex) => "sin(pi*" ++ exprToString(ex) ++ ")"
  | Cosine(ex) => "cos(pi*" ++ exprToString(ex) ++ ")"
  | Average(ex1, ex2) =>
      "((" ++ exprToString(ex1) ++ "+" ++ exprToString(ex2) ++ ")/2)"
  | Times(ex1, ex2) => exprToString(ex1) ++ "*" ++ exprToString(ex2)
  | Thresh(ex1, ex2, ex3, ex4) =>
      "(" ++ exprToString(ex1) ++ "<" ++ exprToString(ex2) ++ "?" ++ exprToString(ex3) ++ ":" ++ exprToString(ex4) ++ ")"
  | FiboPlus(ex1, ex2, ex3, ex4, ex5) =>
      "((" ++ exprToString(ex1) ++ ")*(" ++ exprToString(ex1) ++ "+" ++ exprToString(ex2) ++ ")*(" ++ exprToString(ex1) ++ "+" ++ exprToString(ex2) ++ "+" ++ exprToString(ex3) ++ "))"
  | TheThing(ex1, ex2, ex3) =>
      "((" ++ exprToString(ex1) ++ "*sin(pi*" ++ exprToString(ex2) ++ ")*cos(pi*" ++ exprToString(ex3) ++ "))/2)"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + FiboPlus(expr, expr, expr)
  + TheThing(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(ex) => sin(pi *. eval((ex, x, y)))
  | Cosine(ex) => cos(pi *. eval((ex, x, y)))
  | Average(ex1, ex2) => eval((ex1, x, y)) +. eval((ex2, x, y)) /. 2.
  | Times(ex1, ex2) => eval((ex1, x, y)) *. eval((ex2, x, y))
  | Thresh(ex1, ex2, ex3, ex4) =>
      if eval((ex1, x, y)) < eval((ex2, x, y)) then eval((ex3, x, y)) else eval((ex4, x, y))
  | FiboPlus(ex1, ex2, ex3, ex4, ex5) =>
      eval((ex1, x, y)) *. eval((ex1, x, y)) +. eval((ex2, x, y)) *. eval((ex1, x, y)) +. eval((ex2, x, y)) +. eval((ex3, x, y))
  | TheThing(ex1, ex2, ex3) =>
      eval((ex1, x, y)) *. sin(pi *. eval((ex2, x, y))) *. cos(pi *. eval((ex3, x, y))) /. 2.
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + FiboPlus(expr, expr)
  + TheThing(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(ex) => sin(pi *. eval((ex, x, y)))
  | Cosine(ex) => cos(pi *. eval((ex, x, y)))
  | Average(ex1, ex2) => eval((ex1, x, y)) +. eval((ex2, x, y)) /. 2.
  | Times(ex1, ex2) => eval((ex1, x, y)) *. eval((ex2, x, y))
  | Thresh(ex1, ex2, ex3, ex4) =>
      if eval((ex1, x, y)) < eval((ex2, x, y)) then eval((ex3, x, y)) else eval((ex4, x, y))
  | FiboPlus(ex1, ex2, ex3, ex4, ex5) =>
      eval((ex1, x, y)) *. eval((ex1, x, y)) +. eval((ex2, x, y))
  | TheThing(ex1, ex2, ex3) =>
      eval((ex1, x, y)) *. sin(pi *. eval((ex2, x, y))) *. cos(pi *. eval((ex3, x, y))) /. 2.
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SixtyNine(expr, expr)
  + TheThing(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(ex) => "sin(pi*" ++ exprToString(ex) ++ ")"
  | Cosine(ex) => "cos(pi*" ++ exprToString(ex) ++ ")"
  | Average(ex1, ex2) =>
      "((" ++ exprToString(ex1) ++ "+" ++ exprToString(ex2) ++ ")/2)"
  | Times(ex1, ex2) => exprToString(ex1) ++ "*" ++ exprToString(ex2)
  | Thresh(ex1, ex2, ex3, ex4) =>
      "(" ++ exprToString(ex1) ++ "<" ++ exprToString(ex2) ++ "?" ++ exprToString(ex3) ++ ":" ++ exprToString(ex4) ++ ")"
  | SixtyNine(ex1) => "((" ++ exprToString(ex1) ++ "*69))"
  | TheThing(ex1, ex2, ex3) =>
      "(" ++ exprToString(ex3) ++ "=" ++ exprToString(ex2) ++ "?" ++ exprToString(ex3) ++ ":" ++ exprToString(ex1) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SixtyNine(expr, expr)
  + TheThing(expr, expr, expr)
 in let buildSixtyNine = fun e1 -> SixtyNine(e1) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SixtyNine(expr, expr)
  + TheThing(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(ex) => sin(pi *. eval((ex, x, y)))
  | Cosine(ex) => cos(pi *. eval((ex, x, y)))
  | Average(ex1, ex2) => eval((ex1, x, y)) +. eval((ex2, x, y)) /. 2.
  | Times(ex1, ex2) => eval((ex1, x, y)) *. eval((ex2, x, y))
  | Thresh(ex1, ex2, ex3, ex4) =>
      if eval((ex1, x, y)) < eval((ex2, x, y)) then eval((ex3, x, y)) else eval((ex4, x, y))
  | SixtyNine(ex1) => eval((ex1, x, y)) *. 69.
  | TheThing(ex1, ex2, ex3) =>
      eval((ex1, x, y)) *. sin(pi *. eval((ex2, x, y))) *. cos(pi *. eval((ex3, x, y))) /. 2.
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => []
  | VarY => []
  | Sine(e1) => exprToString(e1)
  | Cosine(e1) => exprToString(e1)
  | Average(e1, e2) => exprToString(e1) ++ exprToString(e2)
  | Times(e1, e2) => exprToString(e1) ++ exprToString(e2)
  | Thresh(e1, e2, e3) =>
      exprToString(e1) ++ exprToString(e2) ++ exprToString(e3)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => VarX
  | VarY => VarY
  | Sine(e1) => exprToString(e1)
  | Cosine(e1) => exprToString(e1)
  | Average(e1, e2) => exprToString(e1) ++ exprToString(e2)
  | Times(e1, e2) => exprToString(e1) ++ exprToString(e2)
  | Thresh(e1, e2, e3) =>
      exprToString(e1) ++ exprToString(e2) ++ exprToString(e3)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Percent(expr)
  + Negate(expr)
  + SumPercent(expr, expr, expr)
 in let buildSumPercent = fun e -> SumPercent(e) in ?
|};
  {|
let t = fun x -> x + 1 in let sepConcat = fun sep -> fun sl -> case sl 
  | [] => ""
  | h :: t =>
      let f = fun a -> fun x -> ? in let base = sep in let l = t in fold_left(f)(base)(l)
end in ?
|};
  {|
let seal = 1 :: 2 :: [3] in let _ = ? in ?
|};
  {|
let seal = 1 :: 2 :: [3] in let _ = ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let _ = Sine in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let _ = Sine(Cosine) in ?
|};
  {|
type tree = 
  + Leaf(Int)
  + Node(tree, tree)
 in let _ = let foo = fun t -> case t 
  | Leaf(n) => 1
  | Node(t1, t2) => foo(t1) + foo(t2)
end in foo(Node((Node((Leaf(1), Leaf(2))), Leaf3))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => Sine ++ exprToString(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin(pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos(pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")" ++ "/2)"
  | Time(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
end in ?
|};
  {|
let hi = [] in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + MyExpr1(expr, expr, expr)
  + MyExpr2(expr)
 in let sampleExpr1 = MyExpr2(MyExpr1((Varx, VarY, Thresh((VarX, VarY, VarX, Times((Sine(VarX), Cosine(Average((VarX, VarY)))))))))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr)
  + KellysOp(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin(pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e2) => "cos(pi*" ++ exprToString(e2) ++ ")"
  | Average(e3, e4) =>
      "((" ++ exprToString(e3) ++ "+" ++ exprToString(e4) ++ ")/2)"
  | Times(e5, e6) => exprToString(e5) ++ "*" ++ exprToString(e6)
  | Thresh(e7, e8, e9, e10) =>
      "(" ++ exprToString(e7) ++ "<" ++ exprToString(e8) ++ "?" ++ exprToString(e9) ++ ":" ++ exprToString(e10) ++ ")"
  | Power(e11) => "((" ++ exprToString(e11) ++ ")^2)"
  | KellysOp(e1, e2, e3) =>
      "(" ++ exprToString(e1) ++ ">" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":0.0"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr)
  + KellysOp(expr, expr, expr, expr)
 in let buildKellysOp = fun (a, b, a_more) -> KellysOp((a, b, a_more)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr)
  + KellysOp(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(a, b, a_less, b_less) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((a_less, x, y)) else eval((b_less, x, y))
  | Power(e) => eval((e, x, y)) *. eval((e, x, y))
  | KellysOp(a, b, a_more) =>
      if eval((a, x, y)) > eval((b, x, y)) then eval((a_more, x, y)) else 0.
end in ?
|};
  {|
let digitsOfInt = fun n -> case n 
  | _ => ?
end in ?
|};
  {|
let _ = let n = 0 in ? in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "Sine" ++ exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ exprToString(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ exprToString(x) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z) ++ ")"
  | Half(x) => ".5*" ++ exprToString(x)
end in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Half(expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Half(a) => 0.5 *. eval((a, x, y))
end in let _ = eval((Half, 0.3, 0.3)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Half(expr)
  + Timestwo(expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ exprToString(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ exprToString(x) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z) ++ ")"
  | Half(x) => ".5*" ++ exprToString(x)
  | Third(x) => "0.33*" ++ exprToString(x)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Half(expr)
  + Timestwo(expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Half(a) => 0.5 *. eval((a, x, y))
  | Third(a) => 0.33 *. eal((a, x, y))
end in ?
|};
  {|
let sepConcat = fun sep -> fun sl -> case sl 
  | [] => ""
  | h :: t =>
      let f = fun a -> fun x -> if length(()) == 0 then a ++ x else a ++ x ++ sep in let base = "" in let l = sl in fold_left(f)(base)(l)
end in ?
|};
  {|
let digitsOfInt = fun n -> if n < 0 then [] else let a = n / 10 in let b = int_mod((n, 10)) in let c = a :: [b] in ? in ?
|};
  {|
let digitsOfInt = fun n -> if n < 0 then [] else let a = n / 10 in let b = int_mod((n, 10)) in let c = a :: [b] in ? in ?
|};
  {|
let digitsOfInt = fun n -> if n < 0 then [] else let a = n / 10 in let b = int_mod((n, 10)) in let c = a :: [b] in ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(x) => "x"
  | VarY(y) => "y"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + AbsThresh(expr, expr, expr)
  + ModThresh(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin(pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos(pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | AbsTresh(e1, e2, e3) =>
      let s = exprToString(e3) in "(abs(" ++ exprToString(e1) ++ ")<abs(" ++ exprToString(e2) ++ "?" ++ s ++ ":abs(" ++ exprToString(e4) ++ "))"
end in ?
|};
  {|
let digitsOfInt = fun n -> let myList = [] in ? in ?
|};
  {|
let digitsOfInt = fun n -> let myList = [] in ? in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else rev(int_mod((n, 10)) :: rev(digitsOfInt(n / 10))) in let sumList = fun xs -> case xs 
  | [] => 0
  | h :: t => h + sumList(t)
  | _ => -1
end in let additivePersistence = fun n -> let count = [] in ? in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else rev(int_mod((n, 10)) :: rev(digitsOfInt(n / 10))) in let sumList = fun xs -> case xs 
  | [] => 0
  | h :: t => h + sumList(t)
  | _ => -1
end in let additivePersistence = fun n -> let count = [0] in ? in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else rev(int_mod((n, 10)) :: rev(digitsOfInt(n / 10))) in let sumList = fun xs -> case xs 
  | [] => 0
  | h :: t => h + sumList(t)
  | _ => -1
end in let additivePersistence = fun n -> let count = [0] in if sumList(digitsOfInt(n)) > 9 then &(1 :: count)(additivePersistence(sumList(digitsOfInt(n)))) else sumList(count) in ?
|};
  {|
let digitsOfInt = fun n -> if n <= 0 then [] else rev(int_mod((n, 10)) :: rev(digitsOfInt(n / 10))) in let sumList = fun xs -> case xs 
  | [] => 0
  | h :: t => h + sumList(t)
  | _ => -1
end in let additivePersistence = fun n -> let count = [0] in if sumList(digitsOfInt(n)) > 9 then &&(1 :: count)(additivePersistence(sumList(digitsOfInt(n)))) else sumList(count) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(x) => sprintf("%s")(x)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(a) => sprintf("%s")(a)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let cool = VarX(2.) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let eval = fun (e, x, y) -> case e 
  | VarX => let vx = x in vx
  | VarY => let vy = y in vy
  | Average => buildAverage((vx, vy))
end in ?
|};
  {|
let c1 = fun () -> failwith("to be implemented") in let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x +. 0.
  | VarY => y +. 0.
  | Sine(s1) => sin(pi *. eval((s1, x, y)))
  | Cosine(c1) => cos(pi *. eval((c1, x, y)))
  | Average(a1, a2) => eval((a1, x, y)) +. eval((a2, x, y)) /. 2.
  | Times(t1, t2) => eval((t1, x, y)) *. eval((t2, x, y))
  | Thresh(h1, h2, h3, h4) =>
      if eval((h1, x, y)) < eval((h2, x, y)) then eval((h3, x, y)) else eval((h4, x, y))
end in let _ = eval((Sine(Average((Varx, VarY))), 0.5, -0.5)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let build = fun (rand, depth) -> case rand((1, 7)) 
  | _ =>
      buildTimes((buildCosine(buildSine(buildX(()))), buildCosine(buildSine(buildX(())))))
  | 1 => buildX(())
  | 2 => buildY(())
  | 3 => buildSine(())
  | 4 => buildSine(buildX(()))
  | 5 => buildSine(buildX(()))
  | 6 => buildSine(buildX(()))
  | 7 => buildSine(buildX(()))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let build = fun (rand, depth) -> case rand((1, 7)) 
  | _ => buildCosine(())
  | 1 => buildX(())
  | 2 => buildY(())
  | 3 =>
      buildSine(if depth == 0 then buildX(()) else build((rand, depth - 1)))
  | 4 =>
      buildCosine(if depth == 0 then buildY(()) else build((rand, depth - 1)))
  | 5 =>
      buildAverage((if depth == 0 then buildX(()) else build((rand, depth - 1)), if depth == 0 then buildY(()) else build((rand, depth - 1))))
  | 6 =>
      buildTimes((if depth == 0 then buildX(()) else build((rand, depth - 1)), if depth == 0 then buildY(()) else build((rand, depth - 1))))
  | 7 =>
      buildThresh((if depth == 0 then buildX(()) else build((rand, depth - 1)), if depth == 0 then buildY(()) else build((rand, depth - 1)), if depth == 0 then buildX(()) else build((rand, depth - 1)), if depth == 0 then buildY(()) else build((rand, depth - 1))))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Hello1(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Hello2(expr, expr, expr, expr)
 in let sampleExpr4 = Hello2((VarX, VarY, VarX, SinX)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Divide(expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Hello(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Hello1(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Hello2(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Divide(e1, e2) => eval((e1, x, y)) /. eval((e2, x, y))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Hello1(e1, e2, e3) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e1, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Hello2(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e4, x, y)) else eval((e3, x, y))
end in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = ? in let rest' = failwith("to be written") in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let padZero = fun l1 -> fun l2 -> if length(l1) == length(l2) then (l1, l2) else ? in ?
|};
  {|
let digitsOfInt = fun n -> let int = fun list -> fun digInt -> int_mod((n, 10)) :: digInt in ? in ?
|};
  {|
let digitsOfInt = fun n -> let int = fun list -> fun digInt -> [] in ? in ?
|};
  {|
let f = fun a -> fun x -> let intlist = fun l -> if l < 10 then [l] else @(intlist(l / 10))([int_mod((l, 10))]) in case x 
  | (z, y) => case a 
  | [] => let sum = z + y in intlist(sum)
  | h :: t => let sum = h + z + y in @(intlist(sum))(t)
end
end in let _ = f([]) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let exprToString = fun e -> case e 
  | Thresh(a, b, c, d) => exprToString(buildThresh)((a, b, c, d))
  | Times(a, b) => exprToString(buildTimes)((a, b))
  | Average(a, b) => exprToString(buildAverage)((a, b))
  | Cosine(a) => exprToString(buildCosine)(a)
  | Sine(a) => exprToString(buildSine)(a)
  | VarY => exprToString(buildY)
  | VarX => exprToString(buildX)
  | None => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let exprToString = fun e -> case e 
  | Thresh(a, b, c, d) => exprToString(buildThresh)((a, b, c, d))
  | Times(a, b) => exprToString(buildTimes)((a, b))
  | Average(a, b) => exprToString(buildAverage)((a, b))
  | Cosine(a) => exprToString(buildCosine)(a)
  | Sine(a) => exprToString(buildSine)(a)
  | VarY => exprToString(buildY)
  | VarX => exprToString(buildX)
  | None => 0
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let exprToString = fun e -> case e 
  | Thresh(a, b, c, d) => exprToString(Thresh)((a, b, c, d))
  | Times(a, b) => exprToString(buildTimes)((a, b))
  | Average(a, b) => exprToString(buildAverage)((a, b))
  | Cosine(a) => exprToString(buildCosine)(a)
  | Sine(a) => exprToString(buildSine)(a)
  | VarY => exprToString(buildY)
  | VarX => exprToString(buildX)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let exprToString = fun e -> case e 
  | Thresh(a, b, c, d) => exprToString(Thresh)(a * b * c * d)
  | Times(a, b) => exprToString(buildTimes)((a, b))
  | Average(a, b) => exprToString(buildAverage)((a, b))
  | Cosine(a) => exprToString(buildCosine)(a)
  | Sine(a) => exprToString(buildSine)(a)
  | VarY => exprToString(buildY)
  | VarX => exprToString(buildX)
end in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Average(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Cosine(a) => cos(pi ** eval((a, x, y)))
  | Sine(a) => sin(pi ** eval((a, x, y)))
  | VarY => y
  | VarX => x
end in let _ = eval((Time((VarX, VarY)), 1., 2.)) in ?
|};
  {|
let clone = fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone(x)(n - 1))([x])
end in let padZero = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone(0)(length(l1) - length(l2)))(l2)) else (append(clone(0)(length(l2) - length(l1)))(l1), l2) in let removeZero = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case x 
  | ([], []) => []
  | ([h1 :: t1], [h2 :: t2]) => ?
end in let base = [] in let args = l1(l2) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone = fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone(x)(n - 1))([x])
end in let padZero = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone(0)(length(l1) - length(l2)))(l2)) else (append(clone(0)(length(l2) - length(l1)))(l1), l2) in let removeZero = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case x 
  | ([], []) => []
  | ([h1 :: t1], [h2 :: t2]) => ?
end in let base = [] in let args = l1(l2) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone = fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone(x)(n - 1))([x])
end in let padZero = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone(0)(length(l1) - length(l2)))(l2)) else (append(clone(0)(length(l2) - length(l1)))(l1), l2) in let removeZero = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case x 
  | ([], []) => []
  | ([h1 :: t1], [h2 :: t2]) => ?
end in let base = [] in let args = l1(l2) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone = fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone(x)(n - 1))([x])
end in let padZero = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone(0)(length(l1) - length(l2)))(l2)) else (append(clone(0)(length(l2) - length(l1)))(l1), l2) in let removeZero = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case (a, x) 
  | (h :: t, (x1, x2)) => ?
end in let base = [0] in let args = rev(combine(l1)(l2)) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone = fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone(x)(n - 1))([x])
end in let padZero = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone(0)(length(l1) - length(l2)))(l2)) else (append(clone(0)(length(l2) - length(l1)))(l1), l2) in let removeZero = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case (a, x) 
  | (h :: t, (x1, x2)) => ?
end in let base = [0] in let args = rev(combine(l1)(l2)) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone = fun x -> fun n -> case n 
  | 0 => []
  | a => if a < 0 then [] else @(clone(x)(n - 1))([x])
end in let padZero = fun l1 -> fun l2 -> if length(l1) > length(l2) then (l1, append(clone(0)(length(l1) - length(l2)))(l2)) else (append(clone(0)(length(l2) - length(l1)))(l1), l2) in let removeZero = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else l
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case (a, x) 
  | (h :: t, (x1, x2) :: t2) => ?
end in let base = [0] in let args = rev(combine(l1)(l2)) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "sine(pi*" ++ exprToString(d) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "sine(pi*" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
end in let _ = eval((Sine(Varx), 1, 1)) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(x) => "x"
  | VarY(y) => "y"
  | Sine(s) => "sin (pi*" ++ exprString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos (pi*" ++ exprToString(e) ++ ")"
  | Averages => "((" ++ exprToString(e) ++ "*" ++ exprToString(e) ++ ")/2)"
  | Times => "(" ++ exprToString(e) ++ "*" ++ exprToString(e) ++ ")"
  | Thresh =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(e) ++ "?" ++ exprToString(e) ++ ":" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos (pi*" ++ exprToString(e) ++ ")"
  | Average(e) => "((" ++ exprToString(e) ++ "*" ++ exprToString(e) ++ ")/2)"
  | Times(e) => "(" ++ exprToString(e) ++ "*" ++ exprToString(e) ++ ")"
  | Thresh(e) =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(e) ++ "?" ++ exprToString(e) ++ ":" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e) => x +. y /. 2.
  | Times(x, y) => x *. y
  | Thresh(e, f, g, h) => failwith("sad")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX(x) => x
  | VarY(y) => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(x, y) => eval((e, x, y)) +. eval((e, x, y)) /. 2.
  | Times(x, y) => eval((e, x, y)) *. eval((e, x, y))
  | Thresh(e1, e2, e3, e4) => failwith("sad")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Timmy1(expr, expr, expr)
  + Timmy2(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e, f) =>
      "((" ++ exprToString(e) ++ "*" ++ exprToString(f) ++ ")/2)"
  | Times(e, f) => "(" ++ exprToString(e) ++ "*" ++ exprToString(f) ++ ")"
  | Thresh(e, f, g, h) =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(f) ++ "?" ++ exprToString(g) ++ ":" ++ exprToString(h) ++ ")"
  | Timmy1(e1, e2, e3) =>
      "(sin(pi*" ++ exprToString(e1) ++ ")+" ++ "cos(pi*" ++ exprToString(e2) ++ "))*" ++ "cos(pi*" ++ exprToString(e) ++ ")"
  | Timmy2(e1, e2) =>
      "(sin(pi*" ++ exprToString(e1) ++ ")/" ++ "cos(pi*" ++ exprToString(e2) ++ "))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Timmy1(expr, expr, expr)
  + Timmy2(expr, expr, expr, expr)
 in let buildTimmy2 = fun (e1, e2) -> Timmy2((e1, e2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Timmy1(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Timmy1(e1, e2, e3) =>
      sin(pi *. eval((e, x, y))) ** 2. *. cos(pi *. eval((e, x, y)))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Timmy1(expr, expr)
  + Timmy2(expr, expr, expr)
 in let buildTimmy2 = fun (e1, e2) -> Timmy2((e1, e2)) in ?
|};
  {|
let clone = fun x -> fun n -> if n <= 0 then [] else x :: clone(x)(n - 1) in let padZero = fun l1 -> fun l2 -> let s1 = length(l1) in let s2 = length(l2) in if s1 < s2 then (@(clone(0)(s2 - s1))(l1), l2) else if s2 < s1 then (l1, @(clone(0)(s1 - s2))(l2)) else (l1, l2) in let removeZero = fun l -> case l 
  | [] => []
  | h :: t => if !=(h)(0) then h :: t else removeZero(t)
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> case a 
  | (_, []) => (fst(x) + snd(x) / 10, [int_mod((fst(x) + snd(x), 10))])
  | (c, h :: t) =>
      let sum = c + fst(x) + snd(x) in (sum / 10, int_mod((sum, 10)) :: snd(a))
end in let base = (0, []) in let args = @(combine(rev(l1))(rev(l2)))([(0, 0)]) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in let bigMul = fun l1 -> fun l2 -> let f = fun a -> fun x -> (fst(a), bigAdd(())(())) in let base = (0, []) in let args = rev(l2) in let (_, res) = fold_left(f)(base)(args) in res in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Nom(expr, expr, expr)
  + Squa(expr)
 in let sampleExpr2 = Times((Squa(Nom((VarX, VarY, VarX))), Sine(Varx))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Nom(expr, expr, expr)
  + Squa(expr)
 in let sampleExpr2 = Nom((VarX, VarY, Sin(VarX))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "%s"(e)
  | VarY => "%s"(e)
  | Sine => "%s"(e)
  | Cosine => "%s"(e)
  | Average => "%s"(e)
  | Times => "%s"(e)
  | Thresh => "%s"(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "%s"(e)
  | VarY => "%s"(e)
  | Sine => "%s %s"(e)(e)
  | Cosine => "%s"(e)
  | Average => "%s"(e)
  | Times => "%s"(e)
  | Thresh => "%s"(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "%s"(e)
  | VarY => "%s"(e)
  | Sine => "%s %s"(e)
  | Cosine => "%s"(e)
  | Average => "%s"(e)
  | Times => "%s"(e)
  | Thresh => "%s"(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(u) => sin(pi *. eval(u))
  | Cos(u) => cos(pi *. eval(u))
  | Average(u, v) => eval(u) +. eval(v) /. 2
  | Times(u, v) => eval(u) *. eval(v)
  | Thresh(s, t, u, v) => if eval(s) < eval(t) then eval(u) else eval(v)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(u) => sin(pi *. eval((u, x, y)))
  | Cosine(u) => cos(pi *. eval((u, x, y)))
  | Average(u, v) => eval((u, x, y)) +. eval((v, x, y)) /. 2.
  | Times(u, v) => eval((u, x, y)) *. eval((v, x, y))
  | Thresh(s, t, u, v) =>
      if eval((s, x, y)) < eval((t, x, y)) then eval((u, x, y)) else eval((v, x, y))
  | Halve(u) => eval((u, x, y)) /. 2
  | Wow(u, v, w) =>
      sqrt(abs(eval((u, x, y))) *. abs(eval((v, x, y))) *. abs(eval((w, x, y))))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Special1(expr, expr, expr)
  + Special2(expr, expr)
 in let buildSpecial1 = fun (e1, e2) -> Special1((e1, e2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => @(VarY)(@("/")(VarX))
  | Cosine => @(VarX)(@("/")(VarY))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => VarY ++ "/" ++ VarX
  | Cosine => VarX ++ "/" ++ VarY
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => exprToString(e)
  | Cosine => exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => exprToString(VarX)
  | Cosine => exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => exprToString(e)
  | Cosine => exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e) => "(" ++ exprToString(e) ++ "+" ++ exprToString(e) ++ ")/2"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildSine = fun e -> Sine(e) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let eval = fun (e, x, y) -> case e 
  | VarX => buildX
  | VarY => buildY
  | Sine => buildSine(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => sin(pi *. eval((e, x, y)))
  | ArcSine(e) => 1 /. sin(pi *. eval((e, x, y)))
  | ArcCosine(e) => 1 /. cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in ?
|};
  {|
let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> map(fun x -> x + a)(x) in let base = hd(L1) in let args = l2 in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(e) => printf("%s")(e)
  | VarY(e) => printf("%s")(e)
  | Sine(e) => printf("sin(%s)")(e)
  | Cosine(e) => printf("cos(%s)")(e)
  | (Average(e1), e2) => printf("%s+%s/2")(e1)(e2)
  | (Times(e1), e2) => printf("%s*%s")(e1)(e2)
  | (Thresh(e1), e2, e3, e4) => printf("(%s<%s?%s:%s)")(e1)(e2)(e3)(e4)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squared(expr)
  + Root(expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Square(e) => eval(e ** 2)
  | Root(e) => eval(e ** 1 / 2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squared(expr)
  + Flatten(expr, expr, expr)
 in let buildFlatten = fun e -> Flatten(e) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squared(expr)
  + Root(expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squared(expr)
  + Flatten(expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | Squared(e) => eval((e, x, y)) ** 2.
  | Flatten(e1, e2, e3) =>
      eval((e1, x, y)) /. eval((e2, x, y)) /. eval((e3, x, y))
end in let _ = eval((Root(VarX), 0.5, 1.)) in ?
|};
  {|
let _ = () in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let a = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ ex(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ ex(t) ++ ")"
  | Average(s, t) => "((" ++ ex(s) ++ "+" ++ ex(t) ++ ")/2)"
  | Times(s, t) => ex(s) ++ "*" ++ ex(t)
  | Thresh(s, t, u, v) =>
      "(" ++ ex(s) ++ "<" ++ ex(t) ++ "?" ++ ex(u) ++ ":" ++ ex(v) ++ ")"
  | Extra(s, t, u) =>
      "sin(pi*" ++ ex(s) ++ ") * cos (" ++ ex(t) ++ ") * sin(" ++ ex(u) ++ ":" ++ ex(v) ++ ")"
  | Stuff(t) => "cos(pi*" ++ "(sin(pi*" ++ ex(t) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let a = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ ex(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ ex(t) ++ ")"
  | Average(s, t) => "((" ++ ex(s) ++ "+" ++ ex(t) ++ ")/2)"
  | Times(s, t) => ex(s) ++ "*" ++ ex(t)
  | Thresh(s, t, u, v) =>
      "(" ++ ex(s) ++ "<" ++ ex(t) ++ "?" ++ ex(u) ++ ":" ++ ex(v) ++ ")"
  | Extra(s, t, u) =>
      "sin(pi*" ++ ex(s) ++ ") * cos (" ++ ex(t) ++ ") * sin(" ++ ex(u) ++ ")"
  | Stuff(t) => "cos(pi*" ++ "(sin(pi*" ++ ex(t) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let a = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ e(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ e(t) ++ ")"
  | Average(s, t) => "((" ++ e(s) ++ "+" ++ e(t) ++ ")/2)"
  | Times(s, t) => e(s) ++ "*" ++ e(t)
  | Thresh(s, t, u, v) =>
      "(" ++ e(s) ++ "<" ++ e(t) ++ "?" ++ e(u) ++ ":" ++ e(v) ++ ")"
  | Extra(s, t, u) =>
      "sin(pi*" ++ e(s) ++ ") * cos (" ++ e(t) ++ ") * sin(" ++ e(u) ++ ")"
  | Stuff(t) => "cos(pi*" ++ "(sin(pi*" ++ e(t) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let a = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ e(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ e(t) ++ ")"
  | Average(s, t) => "((" ++ e(s) ++ "+" ++ e(t) ++ ")/2)"
  | Times(s, t) => e(s) ++ "*" ++ e(t)
  | Thresh(s, t, u, v) =>
      "(" ++ e(s) ++ "<" ++ e(t) ++ "?" ++ e(u) ++ ":" ++ e(v) ++ ")"
  | Stuff(t) => "cos(pi*" ++ "(sin(pi*" ++ e(t) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ exprToString(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ exprToString(t) ++ ")"
  | Average(s, t) =>
      "((" ++ exprToString(s) ++ "+" ++ exprToString(t) ++ ")/2)"
  | Times(s, t) => exprToString(s) ++ "*" ++ exprToString(t)
  | Thresh(s, t, u, v) =>
      "(" ++ exprToString(s) ++ "<" ++ exprToString(t) ++ "?" ++ exprToString(u) ++ ":" ++ exprToString(v) ++ ")"
  | Square(s) => "(" ++ exprToString(s) ++ ")^2"
  | Volume(s, t, u) =>
      "Vol(H: " ++ exprToString(s) ++ ", W: " ++ exprToString(t) ++ ", L: " ++ exprToString(u) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Square(a) => eval((a, x, y)) *. eval((a, x, y))
  | Volume(vol_1, vol_2, vol_3) =>
      eval((vol_1, x, y)) *. eval((vol_2, x, y)) *. eval((vol_3, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr)
  + Volume(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqaure(expr)
  + Volume(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(t) => "sin(pi*" ++ exprToString(t) ++ ")"
  | Cosine(t) => "cos(pi*" ++ exprToString(t) ++ ")"
  | Average(s, t) =>
      "((" ++ exprToString(s) ++ "+" ++ exprToString(t) ++ ")/2)"
  | Times(s, t) => exprToString(s) ++ "*" ++ exprToString(t)
  | Thresh(s, t, u, v) =>
      "(" ++ exprToString(s) ++ "<" ++ exprToString(t) ++ "?" ++ exprToString(u) ++ ":" ++ exprToString(v) ++ ")"
  | Square(s) => "(" ++ exprToString(s) ++ ")^2"
  | Volume(s, t, u) =>
      "Vol(H: " ++ exprToString(s) ++ ", W: " ++ exprToString(t) ++ ", L: " ++ exprToString(u) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr)
  + Volume(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqaure(expr)
  + Volume(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Square(a) => eval((a, x, y)) *. eval((a, x, y))
  | Volume(vol_1, vol_2, vol_3) =>
      eval((vol_1, x, y)) *. eval((vol_2, x, y)) *. eval((vol_3, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "cos(pi*" ++ exprToString(v) ++ ")"
  | Average(v) => "((" ++ exprToString(v) ++ "+" ++ exprToString(v) ++ ")/2)"
  | Times(v) => exprToString(v) ++ "*" ++ exprToString(v)
  | Thresh(v) =>
      "(" ++ exprToString(v) ++ "<" ++ exprToString(v) ++ "?" ++ exprToString(v) ++ ":" ++ exprToString(v) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exprToString(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exprToString(a) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | SquareAv(a, b) =>
      "(" ++ exprToString(a) ++ "^2 + " ++ exprToString(b) ++ "^2)/2"
  | MultHalf(a, b, c) =>
      "(" ++ exprToString(a) ++ "*" ++ exprToString(b) ++ "*" ++ exprToString(c) ++ ")/2"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | SquareAv(a, b) => a *. a +. b *. b / 2.
  | MultHalf(a, b, c) => a *. b *. c / 2.
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(x) => printf("%s")(x)
  | VarY(y) => printf("%s")(y)
end in ?
|};
  {|
let clone = fun x -> fun n -> if n <= 0 then [] else x :: clone(x)(n - 1) in let padZero = fun l1 -> fun l2 -> let difference = length(l1) - length(l2) in if difference > 0 then (l1, @(clone(0)(difference))(l2)) else if difference < 0 then (@(clone(0)(-1 * difference))(l1), l2) else (l1, l2) in let removeZero = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else h :: t
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> let sum = case x 
  | (x1, x2) => x1 + x2
end in ? in let base = [] in let args = rev(combine(l1)(l2)) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let clone = fun x -> fun n -> if n <= 0 then [] else x :: clone(x)(n - 1) in let padZero = fun l1 -> fun l2 -> let difference = length(l1) - length(l2) in if difference > 0 then (l1, @(clone(0)(difference))(l2)) else if difference < 0 then (@(clone(0)(-1 * difference))(l1), l2) else (l1, l2) in let removeZero = fun l -> case l 
  | [] => l
  | h :: t => if h == 0 then removeZero(t) else h :: t
end in let bigAdd = fun l1 -> fun l2 -> let add = fun (l1, l2) -> let f = fun a -> fun x -> ? in let base = (0, []) in let args = let combine = fun (a, b) -> a + b in map(combine)(rev(combine(l1)(l2))) in let (_, res) = fold_left(f)(base)(args) in res in removeZero(add(padZero(l1)(l2))) in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(x) => int_to_string(x)
  | VarY(y) => int_to_string(y)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let build = fun (rand, depth) -> ? in ?
|};
  {|
let lastListElement = fun n -> case n 
  | [] => failwith("ERROR: List must be of size 1 or greater")
  | [x] => x
  | x :: y => lastListElement(y)
end in let catLists = fun x -> fun y -> if not(x) == [] then case x 
  | [x] => x :: y
  | h :: t => catLists(t)(lastListElement(x) :: y)
end else if x == [] then y else ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(n) => "x"
  | VarY(n) => "y"
  | Sine(n) => "sin(" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(" ++ exprToString(n) ++ ")"
  | Average(n) =>
      let (x, y) = n in "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(n) =>
      let (x, y) = n in "((" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ ")"
  | Thresh(n) =>
      let (x, y, z, w) = n in "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(n) => "sin(" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(" ++ exprToString(n) ++ ")"
  | Average(n) =>
      let (x, y) = n in "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(n) =>
      let (x, y) = n in "((" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ ")"
  | Thresh(n) =>
      let (x, y, z, w) = n in "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(n) => sin(3.14 *. eval((n, x, y)))
  | Consine(n) => cos(3.14 *. eval((n, x, y)))
  | Average(m, n) => eval((m, x, y)) +. eval((n, x, y)) /. 2
  | Times(m, n) => eval((m, x, y)) *. eval((n, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let build = fun (rand, depth) -> if depth == 0 then let g = rand((0, 1)) in case g 
  | 0 => VarX
  | 1 => VarY
end else let g = rand((0, 4)) in case g 
  | 0 => Sine(build((rand, depth - 1)))
  | 1 => Cosine(build((rand, depth - 1)))
  | 2 => Average((build((rand, depth - 1)), build((rand, depth - 1))))
  | 3 => Times((build((rand, depth - 1)), build((rand, depth - 1))))
  | 4 => Thresh((build((rand, depth - 1)), build((rand, depth - 1))))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Log(expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Op(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(n) => "sin(pi*" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(pi*" ++ exprToString(n) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, w) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w) ++ ")"
  | Power(x, y) => exprToString(x) ++ "**" ++ exprToString(y)
  | Op(x, y, z) =>
      "(" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ ")/" ++ exprToString(z)
end in let _ = exprToString(Log(VarX)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Op(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(n) => "sin(pi*" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(pi*" ++ exprToString(n) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, w) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w) ++ ")"
  | Power(x, y) => exprToString(x) ++ "**" ++ exprToString(y)
  | Op(x, y, z) =>
      "(" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ ")/" ++ exprToString(z)
end in let _ = exprToString(Op((VarX, VarY))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Op(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqrt(expr)
  + Op(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(n) => "sin(pi*" ++ exprToString(n) ++ ")"
  | Cosine(n) => "cos(pi*" ++ exprToString(n) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, w) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(w) ++ ")"
  | Sqrt(x) => "sqrt(" ++ exprToString(x) ++ ")"
  | Op(x, y, z) =>
      "(" ++ exprToString(x) ++ "*" ++ exprToString(y) ++ "*" ++ exprToString(z) ++ ")/(" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ "+" ++ exprToString(z) ++ ")"
end in let _ = exprToString(Power((VarX, VarY))) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Op(expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(n) => sin(pi *. eval((n, x, y)))
  | Cosine(n) => cos(pi *. eval((n, x, y)))
  | Average(m, n) => eval((m, x, y)) +. eval((n, x, y)) /. 2.
  | Times(m, n) => eval((m, x, y)) *. eval((n, x, y))
  | Thresh(m, n, o, p) =>
      if eval((m, x, y)) < eval((n, x, y)) then eval((o, x, y)) else eval((p, x, y))
  | Power(m, n) => eval((m, x, y)) ** eval((n, x, y))
  | Op(m, n, o) =>
      sqrt(eval((m, x, y)) +. eval((n, x, y)) +. eval((o, x, y))) /. 3.
end in let _ = eval(Power((VarX, VarY, 0.5, -0.5))) in ?
|};
  {|
let pipe = fun fs -> let f = fun a -> fun x -> case fs 
  | h :: t => h
end in let base = [] in fold_left(f)(base)(fs) in ?
|};
  {|
let pipe = fun fs -> let f = fun a -> fun x -> x(a) in let base = 0 in fold_left(f)(base)(fs) in let _ = pipe([]) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "e"
  | VarY => "e"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e) => "((" ++ exprToString(e) ++ "+" ++ exprToString(e) ++ ")/2)"
  | Times(e) => exprToString(e) ++ "" ++ exprToString(e)
  | Thresh(e) =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(e) ++ " ? " ++ exprToString(e) ++ " : " ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + TimesThree(expr, expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | TimesThree(e) =>
      exprToString(e) ++ "*" ++ exprToString(f) ++ "*" ++ exprToString(f)
  | Average(e, f) =>
      "((" ++ exprToString(e) ++ "+" ++ exprToString(f) ++ ")/2)"
  | Times(e, f) => exprToString(e) ++ "*" ++ exprToString(f)
  | Thresh(e, f, g, h) =>
      "(" ++ exprToString(e) ++ "<" ++ exprToString(f) ++ "?" ++ exprToString(g) ++ ":" ++ exprToString(h) ++ ")"
end in ?
|};
  {|
let listReverse = fun l -> ? in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let seen' = ? in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(x) => x
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(X) => X
  | VarY(Y) => Y
  | Sine(N) => Sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => X
  | VarY(Y) => Y
  | Sine(N) => Sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => X
  | VarY => Y
  | Sine(N) => Sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => X
  | VarY => Y
  | Sine(N) => sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => X
  | VarY => Y
  | Sine => sin(N)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => exprToString(X)
  | VarY => Y
  | Sine(e1) => sin(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine => sin(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "sin"
  | Cosine => "cos"
  | Average => "avg"
  | Times => "*"
  | Thresh => "/"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin"
  | Cosine => "cos"
  | Average => "avg"
  | Times => "*"
  | Thresh => "/"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin" + exprToSring(e1)
  | Cosine(e1) => "cos" + exprToString(e1)
  | Average(e1, e2) => "avg"
  | Times(e1, e2) => exprToSring(e1) + "*"
  | Thresh => "/"
end in ?
|};
  {|
let clone = fun x -> fun n -> ? in ?
|};
  {|
let removeZero = fun l -> case l 
  | [] => []
  | h :: t => if h == 0 then removeZero(t) else h :: t
end in let mulByDigit = fun i -> fun l -> let f = fun a -> fun x -> let carry = i * x in case a 
  | h :: t => h + carry / 10 :: int_mod((h + carry, 10)) :: t
  | _ => carry / 10 :: [int_mod((carry, 10))]
end in let base = [] in removeZero(fold_left(f)(base)(rev(l))) in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(x) => x
  | VarY(y) => y
  | Sine(e1) => sin(e1)
  | Cosine(e1) => cos(e1)
  | Average(e1, e2) => e1 + e2 / 2
  | Times(e1, e2) => e1 * e2
  | Thresh(e1, e2, e3, e4) => e1 * e2 * e3 * e4
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + SqXPlusY(expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + SqXPlusYDiv2(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin (pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos (pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ " + " ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ " * " ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | TimesTimes(e1, e2, e3) =>
      exprToString(e1) ++ " * " ++ exprToString(e2) ++ " * " ++ exprToString(e3)
  | SqXPlusY(e1, e2) =>
      "(" ++ exprToString(e1) ++ " * " ++ exprToString(e1) ++ ") + (" ++ exprToString(e2) ++ "/2"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + SqXPlusY(expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + SqXPlusYDiv2(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e1) => sin(pi *. eval((e1, x, y)))
  | Cosine(e1) => cos(pi *. eval((e1, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
  | TimesTimes(e1, e2, e3) =>
      eval((e1, x, y)) *. eval((e2, x, y)) *. eval((e3, x, y))
  | SqXPlusY(e1, e2) =>
      eval((e1, x, y)) *. eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + Cube(expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin (pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos (pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ " + " ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ " * " ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | TimesTimes(e1, e2, e3) =>
      exprToString(e1) ++ " * " ++ exprToString(e2) ++ " * " ++ exprToString(e3)
  | Cube =>
      exprToString(e1) ++ " * " ++ exprToString(e1) ++ " * " ++ exprToString(e1)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + TimesTimes(expr, expr, expr)
  + Cube(expr)
  + MultDivBy6(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e1) => "sin (pi*" ++ exprToString(e1) ++ ")"
  | Cosine(e1) => "cos (pi*" ++ exprToString(e1) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ " + " ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ " * " ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ " ? " ++ exprToString(e3) ++ " : " ++ exprToString(e4) ++ ")"
  | TimesTimes(e1, e2, e3) =>
      exprToString(e1) ++ " * " ++ exprToString(e2) ++ " * " ++ exprToString(e3)
  | Cube(e1) =>
      exprToString(e1) ++ " * " ++ exprToString(e1) ++ " * " ++ exprToString(e1)
  | MultDivBy6 =>
      "(("(exprToString(e1)) ++ " * " ++ exprToString(e2) ++ ") /6)"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | [] => []
  | h :: e' => case h 
  | VarX => "x" ++ exprToString(e')
  | VarY => "y" ++ exprToString(e')
  | Sine => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average =>
      let (e1, e2) = h in "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)" ++ exprToString(e')
  | Times =>
      let (e1, e2) = h in exprToString(e1) ++ "*" ++ exprToString(e2) ++ exprToString(e')
  | Thresh =>
      let (e1, e2, e3, e4) = h in "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")" ++ exprToString(e')
end
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let build = fun (rand, depth) -> if depth == 0 then if rand(0)(1) == 0 then buildX(()) else buildY(()) else let x = rand(0)(6) in case x 
  | 0 => buildX(())
  | 1 => buildY(())
  | 2 => buildSine(build((rand, depth - 1)))
  | 3 => buildCosine(build((rand, depth - 1)))
  | 4 => buildAverage((build((rand, depth - 1)), build((rand, depth - 1))))
  | 5 => buildTimes((build((rand, depth - 1)), build((rand, depth - 1))))
  | 6 =>
      buildThresh((build((rand, depth - 1)), build((rand, depth - 1)), build((rand, depth - 1)), build((rand, depth - 1))))
  | _ => []
end in ?
|};
  {|
let wwhile = fun (f, b) -> let x = f(b) in case x 
  | h :: t => if t == true then wwhile((f, h)) else h
end in ?
|};
  {|
let wwhile = fun (f, b) -> let x = f(b) in case x 
  | h :: t => if t == false then h else wwhile((f, h))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Square(i1) => exprToString(i1)("*")(exprToString)(i1)
  | Exponential(i1, i2) => exprToString(i1)("*")(exprToString)(i2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
 in let buildSquare = fun e -> Square(e) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(i) => sin(pi *. eval((i, x, y)))
  | Cosine(i) => cos(pi *. eval((i, x, y)))
  | Average(i1, i2) => eval((i1, x, y)) +. eval((i2, x, y)) /. 2.
  | Times(i1, i2) => eval((i1, x, y)) *. eval((i2, x, y))
  | Thresh(i1, i2, i3, i4) =>
      if eval((i1, x, y)) < eval((i2, x, y)) then eval((i3, x, y)) else eval((i4, x, y))
  | Square(i) => eval((i, x, y)) *. eval((i, x, y))
  | Exponential(i1, i2) => eval((i1, x, y)) *. eval((i2, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
  + Exponential(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Square(i1) => exprToString(i1)("*")(exprToString)(i1)
  | Exponential(i1, i2) => exprToString(i1)("*")(exprToString)(i2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
  + Exponential(expr, expr)
 in let buildSquare = fun e -> Square(e) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
  + Exponential(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(i) => sin(pi *. eval((i, x, y)))
  | Cosine(i) => cos(pi *. eval((i, x, y)))
  | Average(i1, i2) => eval((i1, x, y)) +. eval((i2, x, y)) /. 2.
  | Times(i1, i2) => eval((i1, x, y)) *. eval((i2, x, y))
  | Thresh(i1, i2, i3, i4) =>
      if eval((i1, x, y)) < eval((i2, x, y)) then eval((i3, x, y)) else eval((i4, x, y))
  | Square(i) => eval((i, x, y)) *. eval((i, x, y))
  | Exponential(i1, i2) => eval((i1, x, y)) *. eval((i2, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
  + Exponential(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Square(i) => exprToString(i)("*")(exprToString)(i)
  | Exponential(i1, i2) => exprToString(i1)("*")(exprToString)(i2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr)
  + Exponential(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(i) => sin(pi *. eval((i, x, y)))
  | Cosine(i) => cos(pi *. eval((i, x, y)))
  | Average(i1, i2) => eval((i1, x, y)) +. eval((i2, x, y)) /. 2.
  | Times(i1, i2) => eval((i1, x, y)) *. eval((i2, x, y))
  | Thresh(i1, i2, i3, i4) =>
      if eval((i1, x, y)) < eval((i2, x, y)) then eval((i3, x, y)) else eval((i4, x, y))
  | Square(i) => eval((i, x, y)) *. eval((i, x, y))
  | Exponential(i1, i2) => **.(eval((i1, x, y)))(eval((i2, x, y)))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr)
  + Exponential(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Cubic(i1, i2, i3) =>
      exprToString(i1) ++ "*" ++ exprToString(i2) ++ "*" ++ exprToString(i3)
  | Exponential(i1, i2) => exprToString(i1) ++ "^" ++ exprToString(i2)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr)
  + Exponential(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(i) => sin(pi *. eval((i, x, y)))
  | Cosine(i) => cos(pi *. eval((i, x, y)))
  | Average(i1, i2) => eval((i1, x, y)) +. eval((i2, x, y)) /. 2.
  | Times(i1, i2) => eval((i1, x, y)) *. eval((i2, x, y))
  | Thresh(i1, i2, i3, i4) =>
      if eval((i1, x, y)) < eval((i2, x, y)) then eval((i3, x, y)) else eval((i4, x, y))
  | Cubic(i1, i2, i3) =>
      eval((i1, x, y)) *. eval((i2, x, y)) *. eval((i3, x, y))
  | Exponential(i1, i2) => eval((i1, x, y)) ** eval((i2, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Square(expr, expr, expr)
  + Exponential(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(i) => "sin(pi*" ++ exprToString(i) ++ ")"
  | Cosine(i) => "cos(pi*" ++ exprToString(i) ++ ")"
  | Average(i1, i2) =>
      "((" ++ exprToString(i1) ++ " + " ++ exprToString(i2) ++ ")/2)"
  | Times(i1, i2) => exprToString(i1) ++ "*" ++ exprToString(i2)
  | Thresh(i1, i2, i3, i4) =>
      "(" ++ exprToString(i1) ++ "<" ++ exprToString(i2) ++ " ? " ++ exprToString(i3) ++ ":" ++ exprToString(i4) ++ ")"
  | Cubic(i1, i2, i3) =>
      exprToString(i1) ++ "*" ++ exprToString(i2) ++ "*" ++ exprToString(i3)
  | Exponential(i1, i2) => exprToString(i1) ++ "^" ++ exprToString(i2)
end in ?
|};
  {|
let clone = fun x -> fun n -> ? in ?
|};
  {|
let digitsOfInt = fun n -> ? in ?
|};
  {|
let equiv = fun x -> fun y -> case x 
  | [] => ?
end in ?
|};
  {|
let equiv = fun x -> fun y -> case x 
  | [] => ?
end in ?
|};
  {|
let equiv = fun x -> fun y -> case x 
  | h :: tl => ?
end in ?
|};
  {|
type binop = 
  + Plus
 in type expr = 
  + Const(Int)
  + Var(String)
  + Bin(expr, binop, expr)
  + Let(String, expr, expr)
  + App(expr, expr)
  + Fun(String, expr)
 in let e3' = App((Let(("z", Const(10), Fun(("y", Plus((Var("y"), Plus, Var("z"))))))), Var("z"))) in ?
|};
  {|
type binop = 
  + Plus
 in type expr = 
  + Const(Int)
  + Var(String)
  + Bin(expr, binop, expr)
  + Let(String, expr, expr)
  + App(expr, expr)
  + Fun(String, expr)
 in let e3 = Let(("x", Const(10), App((Fun(("y", Plus((Var("x"), Plus, Var("y"))))), Var("x"))))) in ?
|};
  {|
type binop = 
  + Plus
 in type expr = 
  + Const(Int)
  + Var(String)
  + Bin(expr, binop, expr)
  + Let(String, expr, expr)
  + App(expr, expr)
  + Fun(String, expr)
 in let e3 = Let(("x", Const(10), App(Fun(("y", Bin((Var("x"), Plus, Var("y")))))), Var("x"))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sin(e) => sin(pi * e)
  | Cosine(e) => cos(pi * eval((e, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) + eval((e2, x, y)) / 2
  | Times(e1, e2) => eval((e1, x, y)) * eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Square(e) => "%s*%s"(exprToString)(e)(exprToString)(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Golden(expr)
  + MeanPi(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(expr) => "sin(pi*" ++ exprToString(expr) ++ ")"
  | Cosine(expr) => "cos(pi*" ++ exprToString(expr) ++ ")"
  | Average(expr1, expr2) =>
      "(" ++ exprToString(expr1) ++ "+" ++ exprToString(expr2) ++ ")/2"
  | Times(expr1, expr2) => exprToString(expr1) ++ "*" ++ exprToString(expr2)
  | Thresh(expr1, expr2, expr3, expr4) =>
      "(" ++ exprToString(expr1) ++ "<" ++ exprToString(expr2) ++ "?" ++ exprToString(expr3) ++ ":" ++ exprToString(expr4) ++ ")"
  | Golden => ""
  | MeanPi => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Golden(expr)
  + MeanPi(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(expr) => "sin(pi*" ++ exprToString(expr) ++ ")"
  | Cosine(expr) => "cos(pi*" ++ exprToString(expr) ++ ")"
  | Average(expr1, expr2) =>
      "(" ++ exprToString(expr1) ++ "+" ++ exprToString(expr2) ++ ")/2"
  | Times(expr1, expr2) => exprToString(expr1) ++ "*" ++ exprToString(expr2)
  | Thresh(expr1, expr2, expr3, expr4) =>
      "(" ++ exprToString(expr1) ++ "<" ++ exprToString(expr2) ++ "?" ++ exprToString(expr3) ++ ":" ++ exprToString(expr4) ++ ")"
  | Golden => ""
  | MeanPi => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(rest) => "sin(pi" ++ exprToString(rest) ++ ")"
  | Cosine(rest) => "cos(pi" ++ exprToString(rest) ++ ")"
  | Average(expr1, expr2) =>
      "(" ++ exprToString(expr1) ++ "+" ++ exprToString(expr2) ++ "/2)"
  | Times(expr1, expr2) => exprToString(expr1) ++ "*" ++ exprToString(expr2)
  | Thresh =>
      "(" ++ exprToString(expr1) ++ "<" ++ exprToString(expr2) ++ "?" ++ exprToString(expr3) ++ ":" ++ exprToString(expr4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Sqrt(expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(rest) => "sin(pi*" ++ exprToString(rest) ++ ")"
  | Cosine(rest) => "cos(pi*" ++ exprToString(rest) ++ ")"
  | Average(expr1, expr2) =>
      "(" ++ exprToString(expr1) ++ "+" ++ exprToString(expr2) ++ "/2)"
  | Times(expr1, expr2) => exprToString(expr1) ++ "*" ++ exprToString(expr2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Sqrt(e1) => "sqrt(" ++ exprToString(e1) ++ ")"
end in ?
|};
  {|
let listReverse = fun l -> case l 
  | [] => None
  | front :: back => listReverse(back) :: [front]
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Uncreative(expr, expr, expr)
  + Creative(expr)
 in let sampleExpr5 = Uncreative(Creative(Thresh((VarX, VarY, VarX, Times((Sine(VarX), Cosine(Average((VarX, VarY))))))))) in ?
|};
  {|
let wwhile = fun (f, b) -> let x = wwhile((f, b)) in let h :: t = x in case t 
  | false => h
  | true => wwhile((f, h))
end in ?
|};
  {|
let wwhile = fun (f, b) -> let x = wwhile((f, b)) in let h :: t = x in case [t] 
  | false => h
  | true => wwhile((f, h))
end in ?
|};
  {|
let wwhile = fun (f, b) -> let x = f(b) in let h :: t = x in let r :: l = t in case t 
  | false => h
  | true => wwhile((f, h))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(s) => printf("%s")
  | VarY => printf("%s")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(s) => s
  | VarY => printf("%s")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine => "sin(pi*" + exprToString + ")"
  | Cosine => "cos(pi*" + exprToString + ")"
  | Average => "((" + exprToString + "+" + exprToString + ")/2"
  | Times => exprToString + "*" + exprToString
  | Thresh =>
      "(" + exprToString + "?" + exprToString + ":" + exprToString + ")"
  | _ => 0
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine => buildSine(e)
  | Cosine => buildCosine(e)
  | Average => buildAverage((e1, e2))
  | Times => buildTimes((e1, e2))
  | Thresh => buildThresh((a, b, a_less, b_less))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(s) => "sin(pi*" ++ exprToString(s) ++ ")"
  | Cosine(s) => "cos(pi*" ++ exprToString(s) ++ ")"
  | Average(s, p) =>
      "((" ++ exprToString(s) ++ "+" ++ exprToString(p) ++ ")/2"
  | Times(s, p) => exprToString(s) ++ "*" ++ exprToString(p)
  | Thresh(s, p, r, d) =>
      "(" ++ exprToString(s) ++ "<" ++ exprToString(p) ++ "?" ++ exprToString(r) ++ ":" ++ exprToString(d) ++ ")"
  | AllMult(s, p, r) =>
      exprToString(s) ++ "*" ++ exprToString(p) ++ "*" ++ exprToString(p)
  | AvgThree(s, p, r) =>
      "((" ++ exprToString(s) ++ "+" ++ exprToString(p) ++ "+" ++ exprToString(p) ++ ")/2"
end in ?
|};
  {|
let pipe = fun fs -> let f = fun a -> fun x -> x(a) in let base = fun x -> [] in fold_left(f)(base)(fs) in ?
|};
  {|
let filter = fun l -> fun a -> case l 
  | [] => []
  | h :: t => if a == h then filter(t)(a) else h :: filter(t)(a)
end in let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = h in let rest' = h :: filter(t)(h) in helper((seen', rest'))
end in rev(helper(([], l))) in let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = h in let rest' = h :: filter(t)(h) in helper((seen', rest'))
end in removeDuplicates(helper([])) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Cube(expr, expr)
  + Square(expr)
 in let sampleExpr1 = Thresh((VarX, VarY, VarX, Cube(Times((Sine(VarX), Cosine(Average((VarX, VarY)))))))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Mean3(expr, expr, expr)
  + Square(expr)
 in let sampleExpr1 = Thresh((VarX, VarY, VarX, Times((Sine(Mean((VarX, VarX, VarY))), Cosine(Average((Square(VarX), VarY))))))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Expn(expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(b) => "sin(pi*" ++ exprToString(b) ++ ")"
  | Cosine(b) => "cos(pi*" ++ exprToString(b) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | Eval(a, b) => "(" ++ exprToString(a) ++ "^" ++ exprToString(b) ++ ")"
  | _ => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Expn(expr, expr)
  + TripMult(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(b) => "sin(pi*" ++ exprToString(b) ++ ")"
  | Cosine(b) => "cos(pi*" ++ exprToString(b) ++ ")"
  | Average(a, b) =>
      "((" ++ exprToString(a) ++ "+" ++ exprToString(b) ++ ")/2)"
  | Times(a, b) => exprToString(a) ++ "*" ++ exprToString(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exprToString(a) ++ "<" ++ exprToString(b) ++ "?" ++ exprToString(c) ++ ":" ++ exprToString(d) ++ ")"
  | Expn(b) => "(0.5^" ++ exprToString(b) ++ ")"
  | TripMult(a, b, c) =>
      "(" ++ exprToString(a) ++ "*" ++ exprToString(b) ++ "*" ++ exprToString(c) ++ ")"
  | _ => ""
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Expn(expr, expr)
  + TripMult(expr, expr, expr)
 in let buildExpn = fun b -> Expn(b) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildAverage = fun (e1, e2) -> Average((e1, e2)) in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildThresh = fun (a, b, a_less, b_less) -> Thresh((a, b, a_less, b_less)) in let buildTimes = fun (e1, e2) -> Times((e1, e2)) in let eval = fun (e, x, y) -> case e 
  | VarX(a) => x
  | VarY(b) => y
  | Sine(x1) => eval((buildSine(x1), x, y))
  | Cosine(x2) => eval((buildCosine(x2), x, y))
  | Average(x3, x4) => eval((buildAverage((x3, x4)), x, y))
  | Times(x5, x6) => eval((buildTimes((x5, x6)), x, y))
  | Thresh(x7, x8, x9, x0) => eval((buildThresh((x7, x8, x9, x0)), x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Root(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Pivot(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Flip(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Pivot(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x1) => "sin(pi*" ++ exprToString(x1) ++ ")"
  | Cosine(x2) => "cos(pi*" ++ exprToString(x2) ++ ")"
  | Root(x3) => "sqrt(" ++ exprToString(x3) ++ ")"
  | Average(x4, x5) =>
      "((" ++ exprToString(x4) ++ "+" ++ exprToString(x5) ++ ")/2)"
  | Times(x6, x7) => exprToString(x6) ++ "*" ++ exprToString(x7)
  | Thresh(x8, x9, x10, x11) =>
      "(" ++ exprToString(x8) ++ "<" ++ exprToString(x9) ++ "?" ++ exprToString(x10) ++ ":" ++ exprToString(x11) ++ ")"
  | Pivot(x12, x13, x14) =>
      "(" ++ exprToString(x12) ++ "<0?" ++ exprToString(x13) ++ ":" ++ exprToString(x14) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine => "Sine(" ++ exprToString(e) ++ ")"
  | Cosine => "Cosine(" ++ exprToString(e) ++ ")"
  | Average => "Average(" ++ exprToString(e) ++ ")"
  | Times => "Times(" ++ exprToString(e) ++ ")"
  | Thresh(a, b, c, d) =>
      "Thresh(" ++ exprToString(a) ++ "," ++ exprToString(b) ++ "," ++ exprToString(c) ++ "," ++ exprToString(d) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Sine(Average(VarX(VarY))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Average(VarX(VarY)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ exprToString(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ exprToString(x) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, s) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(s) ++ ")"
  | Trip(x, y, z) =>
      "((" ++ exprToString(x) ++ "%30.0)" ++ exprToString ++ "%" ++ exprToString(z) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Trip(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ exprToString(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ exprToString(x) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(x) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(x, y, z, s) =>
      "(" ++ exprToString(x) ++ "<" ++ exprToString(y) ++ "?" ++ exprToString(z) ++ ":" ++ exprToString(s) ++ ")"
  | Trip(x, y, z) =>
      "((" ++ exprToString(x) ++ "/30.0)+" ++ exprToString(y) ++ "/" ++ exprToString(z) ++ ")"
  | Greater(x, y) =>
      "(" ++ exprToString(x) ++ ">" ++ exprToString(y) ++ "?" ++ exprToString(x) ++ ":" ++ exprToString(y) ++ ")"
end in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = [] in let rest' = rev(t) in if mem(h)(rest') then rest == t else h :: seen'(helper)((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let rest' = rev(t) in let seen' = seen in ?
end in rev(helper(([], l))) in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t => let rest' = rev(t) in let seen' = seen in ?
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VaryY => "y"
  | Sine(ex) => "sin(pi*" ++ exprToString(ex) ++ ")"
  | Cosine(ex) => "cos(pi*" ++ exprToString(ex) ++ ")"
  | Average(ex1, ex2) =>
      "(" ++ exprToString(ex1) ++ "*" ++ exprToString(ex2) ++ ")/2"
  | Times(ex1, ex2) => exprToString(ex1) ++ "*" ++ exprToString(ex2)
  | Thresh(ex1, ex2, ex3, ex4) =>
      "(" ++ exprToString(ex1) ++ "<" ++ exprToString(ex2) ++ "?" ++ exprToString(ex3) ++ ":" ++ exprToString(ex4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SinCos(expr)
 in let eval = fun (e, x, y) -> let pi = 3.14 in case e 
  | VarX => x
  | VarY => y
  | Sine(ex) => sin(pi *. eval((ex, x, y)))
  | Cosine(ex) => cos(pi *. eval((ex, x, y)))
  | Average(ex1, ex2) => eval((ex1, x, y)) +. eval((ex2, x, y)) /. 2.
  | Times(ex1, ex2) => eval((ex1, x, y)) *. eval((ex2, x, y))
  | Thresh(ex1, ex2, ex3, ex4) =>
      if eval((ex1, x, y)) < eval((ex2, x, y)) then eval((ex3, x, y)) else eval((ex4, x, y))
  | SinCos(ex) => sin(pi *. eval((ex, x, y))) *. cos(pi *. eval((ex, x, y)))
  | Three(ex1, ex2, ex3) =>
      eval((ex1, x, y)) *. cos(pi *. eval((ex2, x, y))) *. sin(pi *. eval((ex3, x, y)))
end in ?
|};
  {|
let clone = fun x -> fun n -> if n < 1 then [] else x :: clone(x)(n - 1) in let padZero = fun l1 -> fun l2 -> let difference = length(l1) - length(l2) in ? in ?
|};
  {|
let clone = fun x -> fun n -> if n < 1 then [] else x :: clone(x)(n - 1) in let padZero = fun l1 -> fun l2 -> let difference1 = length(l1) - length(l2) in let difference2 = length(l2) - length(l1) in if difference1 > 0 then clone(0)(difference1) :: l1 else ? in ?
|};
  {|
let clone = fun x -> fun n -> if n < 1 then [] else x :: clone(x)(n - 1) in let padZero = fun l1 -> fun l2 -> let difference1 = length(l1) - length(l2) in let difference2 = length(l2) - length(l1) in ? in ?
|};
  {|
let clone = fun x -> fun n -> if n < 1 then [] else x :: clone(x)(n - 1) in let padZero = fun l1 -> fun l2 -> let difference1 = length(l1) - length(l2) in let difference2 = length(l2) - length(l1) in ? in ?
|};
  {|
let clone = fun x -> fun n -> if n < 1 then [] else x :: clone(x)(n - 1) in let padZero = fun l1 -> fun l2 -> let difference1 = length(l1) - length(l2) in let difference2 = length(l2) - length(l1) in ? in ?
|};
  {|
let digitsOfInt = fun n -> if n < 10 then [n] else [int_mod((n, 10))] in ?
|};
  {|
let listReverse = fun l -> case l 
  | [] => []
  | h :: t => [h]
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let exprToString = fun e -> case e 
  | VarX => e
  | Sine(e') => sin(pi * e')
  | Cosine(e') => cos(pi * e')
  | Average => e' + e' / 2
  | Times => e' * e'
  | Thresh => ?
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let exprToString = fun e -> case e 
  | VarX => e
  | Sine(e') => sin(pi * e')
  | Cosine(e') => cos(pi * e')
  | Average => e' + e'
  | Times => e' * e'
  | Thresh => ?
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let exprToString = fun e -> case e 
  | VarX => e
  | Sine(e') => sin(pi * e')
  | Cosine(e') => cos(pi * e')
  | Average => e' + exprToString(e') / 2
  | Times => e' * e'
  | Thresh => ?
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let exprToString = fun e -> case e 
  | VarX => e
  | Sine(e') => sin(pi * e')
  | Cosine(e') => cos(pi * e')
  | Average(e') => e' + e' / 2
  | Times(e') => e' * e'
  | Thresh(e') => ?
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let exprToString = fun e -> case e 
  | VarX => e
  | Sine(e') => sin(pi * e')
  | Cosine(e') => cos(pi * e')
  | Average(a, b) => a + b / 2
  | Times(e') => e' * e'
  | Thresh(e') => ?
end in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = mem(h)(t) in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*"(exprToString)(e)(")")
  | Cosine(e) => "cos(pi*"(exprToString)(e)(")")
  | Average(e) =>
      "(("(exprToString)(e) ++ "+" ++ exprToString(e)(")") / 2(")")
  | Times(e) => exprToString(e)("*")(exprToString)(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*"(exprToString)(e)(")")
  | Cosine(e) => "cos(pi*"(exprToString)(e)(")")
  | Average(e) => ("(("(exprToString)(e), exprToString(e)(")") / 2(")"))
  | Times(e) => exprToString(e)("*")(exprToString)(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*"(exprToString)(e)(")")
  | Cosine(e) => "cos(pi*"(exprToString)(e)(")")
  | Average(x, y) =>
      "(("(exprToString)(e) ++ "+" ++ exprToString(e)(")") / 2(")")
  | Times(e) => exprToString(e)("*")(exprToString)(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(x, y) => Average(eval((e, x, y)) +. eval((e, x, y)) / 2.)
  | Times(x, y) => eval((e, x, y)) *. eval((e, x, y))
  | Thresh(w, x, y, z) =>
      eval((e, x, y)) *. eval((e, x, y)) *. eval((e, x, y)) *. eval((e, x, y))(uncomment)(after)(implementing)(eval)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotanget(e) => "(" ++ 1. /. "(" ++ tan ++ "("(exprToString)(e) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => "(" ++ 1. /. "(" ++ tan ++ "("(exprToString)(e) ++ ")))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => 1 ++ "/cot"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => "contan" ++ exprToString(e)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => "contan(" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Cotangent(e) => "cot(" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Cotangent(expr)
  + Volume(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Squares(e) => exprToString(e)("*")(exprToString)(e)
  | Volume(l, w, h) =>
      "(" ++ exprToString(e) ++ "*(" ++ exprToString(e) ++ ")*" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Cotangent(expr)
  + Volume(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Squares(e) => eval((e, x, y)) * eval((e, x, y))
  | Volume(l, w, h) => eval((l, x, y)) *. eval((w, x, y)) *. eval((h, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Cotangent(expr)
  + Volume(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Squares(e) => eval((e, x, y)) *. eval((e, x, y))
  | Volume(l, w, h) => eval((l, x, y)) *. eval((w, x, y)) *. eval((h, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr, expr)
  + Volume(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Squares(e) => exprToString(e) ++ "*" ++ exprToString(e)
  | Volume(l, w, h) =>
      "(" ++ exprToString(e) ++ "*(" ++ exprToString(e) ++ ")*" ++ exprToString(e) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr, expr)
  + Volume(expr, expr, expr)
 in let buildSquares = fun e -> Squares(e) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr, expr)
  + Volume(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Squares(e) => eval((e, x, y)) *. eval((e, x, y))
  | Volume(l, w, h) => eval((l, x, y)) *. eval((w, x, y)) *. eval((h, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr)
  + Volume(expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Average(x, y) =>
      "((" ++ exprToString(y) ++ "+" ++ exprToString(y) ++ ")/2)"
  | Times(x, y) => exprToString(x) ++ "*" ++ exprToString(y)
  | Thresh(w, x, y, z) =>
      "(" ++ exprToString(w) ++ "<" ++ exprToString(x) ++ "?" ++ exprToString(y) ++ ":" ++ exprToString(z)
  | Squares(e) => exprToString(e) ++ "*" ++ exprToString(e)
  | Substract(j, k) => "(" ++ exprToString(e) ++ "-" ++ exprToString(e)(")")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr)
  + Volume(expr, expr, expr)
 in let buildSubstract = fun (j, k) -> Volume((j, k)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Squares(expr)
  + Volume(expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Squares(e) => eval((e, x, y)) *. eval((e, x, y))
  | Substract(j, k) => eval((j, x, y)) -. eval((k, x, y))
end in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SumInts(expr)
  + Power(expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(expr) => sin(pi *. eval((expr, x, y)))
  | Cosine(expr) => cos(pi *. eval((expr, x, y)))
  | Average(expr1, expr2) => eval((expr1, x, y)) +. eval((expr2, x, y)) /. 2.
  | Times(expr1, expr2) => eval((expr1, x, y)) *. eval((expr2, x, y))
  | Thresh(expr1, expr2, expr3, expr4) =>
      if eval((expr1, x, y)) < eval((expr2, x, y)) then eval((expr3, x, y)) else eval((expr4, x, y))
  | SumInts(expr) => eval((expr, x, y)) *. eval((expr, x, y)) +. 1. /. 2.
  | Power(expr1, expr2, expr3) =>
      eval((expr1, x, y)) ** abs_float(eval((expr2, x, y)) +. eval((expr3, x, y)))
end in let _ = eval((Power((SumInts(Var), VarY, VarX)), -0.999999, 0.99999)) in ?
|};
  {|
let padZero = fun l1 -> fun l2 -> if length(l1) == length(l2) then [(l1, l2)] else let numZeros = length(l1) - length(l2) in ? in ?
|};
  {|
let padZero = fun l1 -> fun l2 -> ? in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = mem(seen)(h) in let rest' = t in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX(s) => printf("%s")(s)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine => "Sine"
  | Cosine => "Cosine"
  | Average => "Average"
  | Times => "Times"
  | Thresh => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine(e) => "Sine"
  | Cosine(e) => "Cosine"
  | Average(e) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "VarX"
  | VarY => "VarY"
  | Sine(e) => "Sine"
  | Cosine(e) => "Cosine"
  | Average(expr) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let test = VarX(x) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "VarY"
  | Sine(e) => "Sine"
  | Cosine(e) => "Cosine"
  | Average(expr) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => %("Sin (pi*%s)")(s)
  | Cosine(e) => "Cosine"
  | Average(expr) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"
  | Average(expr) => "Average"
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => %("(%s*%s)/2")(e)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => %("(%s*%s)/2")(e(e))
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => "(%s + %s)/2"(e)(e)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average => "(%s + %s)/2"(e)(p)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => "(%s + %s)/2"(e)(p)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e) => "(%s + %s)/2"(e)
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e, ex) => "("(exprToString)(e)("+")(exprToString)(ex)(")/2")
  | Times(e) => "Times"
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin (pi*%s)"(e)
  | Cosine(e) => "cos (pi*%s)"(e)
  | Average(e, ex) => "(("(exprToString)(e)("+")(exprToString)(ex)(")/2)")
  | Times(e, ex) => exprToString(e)("*")(exprToString)(ex)
  | Thresh(e) => "Thresh"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let buildX = fun () -> VarX in let buildY = fun () -> VarY in let build = fun (rand, depth) -> if depth > 0 then case rand 
  | (0, 2) => buildX(())
  | (3, 5) => buildY(())
  | (6, 10) => buildSine(build((rand, depth - 1)))
  | (11, 18) => buildCosine(build((rand, depth - 1)))
end else () in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let build = fun (rand, depth) -> if depth > 0 then case rand 
  | (6, 10) => buildSine(build((rand, depth - 1)))
  | (11, 18) => buildCosine(build((rand, depth - 1)))
end else () in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Half(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Half(e) => exprToString(e) ++ "/2"
  | Neg(e) => "-" ++ exprToString(e)
  | Average(e, ex) =>
      "((" ++ exprToString(e) ++ "+" ++ exprToString(ex) ++ ")/2)"
  | Times(e, ex) => exprToString(e) ++ "*" ++ exprToString(ex)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Half(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Neg(e) => -1. *. eval((e, x, y))
  | Half(e) => eval((e, x, y)) /. 2.
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Half(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Half(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + AddDivide(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr3 = Neg(AddDivide((VarX, VaryX, VarY))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Half(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Half(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Divadd(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr3 = Neg(Divadd((VarX, VaryX, VarY))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Half(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Divadd(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr3 = Half(Divadd((VarX, VaryX, VarY))) in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Divadd(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(e) => sin(pi *. eval((e, x, y)))
  | Cosine(e) => cos(pi *. eval((e, x, y)))
  | Neg(e) => -1. *. eval((e, x, y))
  | Divadd(e1, e2, e3) =>
      eval((e, x, y)) +. eval((e, x, y)) /. eval((e, x, y))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in let _ = eval((Sine(Neg(Divadd((VarX, VarY, Vary)))), 0.8, 0.8)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Divadd(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + AddMul(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr3 = Neg(Divadd((VarX, VarY, VarY))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + AddMul(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Neg(e) => exprToString(e) ++ " * -1.0"
  | Average(e, ex) =>
      "((" ++ exprToString(e) ++ "+" ++ exprToString(ex) ++ ")/2)"
  | Times(e, ex) => exprToString(e) ++ "*" ++ exprToString(ex)
  | AveThree(e1, e2, e3) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ "+" ++ exprToString(e3)(")/3")
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Neg(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + AddMul(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e) => "sin(pi*" ++ exprToString(e) ++ ")"
  | Cosine(e) => "cos(pi*" ++ exprToString(e) ++ ")"
  | Neg(e) => exprToString(e) ++ " * -1.0"
  | Average(e, ex) =>
      "((" ++ exprToString(e) ++ "+" ++ exprToString(ex) ++ ")/2)"
  | Times(e, ex) => exprToString(e) ++ "*" ++ exprToString(ex)
  | AveThree(e1, e2, e3) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ "+" ++ exprToString(e3) ++ ")/3"
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let buildCosine = fun e -> Cosine(e) in let buildSine = fun e -> Sine(e) in let _ = Thresh((buildSine(buildCosine(VarX)), VarX, VarY, VarZ)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + TowerNeg(expr, expr, expr)
 in let sampleExpr5 = TowerNeg((VarX, VarY, VarZ)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Tower(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let ex = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ ex(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ ex(x) ++ ")"
  | Average(x, y) => "((" ++ ex(x) ++ "+" ++ ex(y) ++ ")/2)"
  | Times(x, y) => ex(x) ++ "*" ++ ex(y)
  | Thresh(w, x, y, z) =>
      "(" ++ ex(w) ++ "<" ++ ex(x) ++ "?" ++ ex(y) ++ ":" ++ ex(z) ++ ")"
  | Power(x, y) => ex(x) ++ "^" ++ ex(y)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Power(expr, expr)
  + Tower(expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | Power(a, b) =>
      if &&(x < 1.)(&&(x > -1.)(&&(y < 1.)(y > -1.))) then x *. y else eval((a, x, y)) ** eval((b, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr, expr)
 in let exprToString = fun e -> let ex = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(x) => "sin(pi*" ++ ex(x) ++ ")"
  | Cosine(x) => "cos(pi*" ++ ex(x) ++ ")"
  | Average(x, y) => "((" ++ ex(x) ++ "+" ++ ex(y) ++ ")/2)"
  | Times(x, y) => ex(x) ++ "*" ++ ex(y)
  | Thresh(w, x, y, z) =>
      "(" ++ ex(w) ++ "<" ++ ex(x) ++ "?" ++ ex(y) ++ ":" ++ ex(z) ++ ")"
  | SqDist(x, y) => ex(x) ++ "^2+" ++ ex(y) ++ "^2"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Tan(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | SqDist(a, b) => eval((a, x, y)) ** 2. + eval((b, x, y)) ** 2.
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SqDist(expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + SinCos(expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
  | SqDist(a, b) => eval((a, x, y)) ** 2. +. eval((b, x, y)) ** 2.
end in ?
|};
  {|
let wwhile = fun (f, b) -> let (b', c') = f(b) in if c' == true then wwhile((f, b')) else b' in let fixpoint = fun (f, b) -> wwhile((fun x -> (f(b), NOT(b == f(b))), b)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Average(v) => "(" ++ exprToString(v) ++ "+" ++ exprToString(v) ++ ")/2"
  | Times(v) => exprToString(v) ++ "*" ++ exprToString(v)
  | Thresh(v) =>
      exprToString(v) ++ "<" ++ exprToString(v) ++ "?" ++ exprToString(v) ++ ":" ++ exprToString(v)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Average(v, w) =>
      "(" ++ exprToString(v) ++ "+" ++ exprToString(w) ++ ")/2"
  | Times(v) => exprToString(v) ++ "*" ++ exprToString(v)
  | Thresh(v) =>
      exprToString(v) ++ "<" ++ exprToString(v) ++ "?" ++ exprToString(v) ++ ":" ++ exprToString(v)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> let pi = 3.142 in case e 
  | VarX => x
  | VarY => y
  | Sine(v) => sin(pi *. eval((v, x, y)))
  | Cosine(v) => cos(pi *. eval((v, x, y)))
  | Average(v, w) => eval((v, x, y)) +. eval((w, x, y)) /. 2.
  | Times(v, w) => eval((v, x, y)) *. eval((w, x, y))
  | Thresh(v, w, q, r) =>
      if eval((v, x, y)) < eval((w, x, y)) then eval((q, x, y)) else eval((r, x, y))
  | Divide(v, w) => eval((v, x, y)) / eval((w, x, y))
  | Super(v, w) => eval((v, x, y)) + eval((w, x, y)) * eval((v, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Cosine(v) => "sin(pi*" ++ exprToString(v) ++ ")"
  | Average(v, w) =>
      "(" ++ exprToString(v) ++ "+" ++ exprToString(w) ++ ")/2"
  | Times(v, w) => exprToString(v) ++ "*" ++ exprToString(w)
  | Thresh(v, w, x, y) =>
      exprToString(v) ++ "<" ++ exprToString(w) ++ "?" ++ exprToString(x) ++ ":" ++ exprToString(y)
  | Divide(v, w) => exprToString(v) ++ "/" ++ exprToString(w)
  | Super(v, w) =>
      "(" ++ exprToString(v) ++ "+" ++ exprToString(w) ++ ") *" ++ exprToString(v)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX(x') => printf("%s")(x)
  | VarY(y') => printf("%s")(y)
  | Sine(sin) => printf("sin(%s)")(sin)
  | Cosine(cos) => printf("cos(%s)")(cos)
  | Average(ave) => printf("((%s+%s)/2)")(ave)
  | Times(t) => printf("%s*%s")(t)
  | Thresh(th) => printf("(%s<*%s?%s:%s)")(th)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(sine) => "sin(pi*" ++ exprToString(sine) ++ ")"
  | Cosine(cosine) => "cos(pi*" ++ exprToString(cosine) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(t1, t2) => exprToString(t1) ++ "*" ++ exprToString(t2)
  | Thresh(th1, th2, th3, th4) =>
      "(" ++ exprToString(th1) ++ "<" ++ exprToString(th2) ++ "?" ++ exprToString(th3) ++ ":" ++ exprToString(th4) ++ ")"
  | Circ(circ1, circ2) =>
      "(" ++ exprToString(circ1) ++ "^2+" ++ exprToString(circ2) ++ ")"
  | NatLog(nlog) => "ln(" ++ nlog ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Fibonacci(expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(sine) => "sin(pi*" ++ exprToString(sine) ++ ")"
  | Cosine(cosine) => "cos(pi*" ++ exprToString(cosine) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(t1, t2) => exprToString(t1) ++ "*" ++ exprToString(t2)
  | Thresh(th1, th2, th3, th4) =>
      "(" ++ exprToString(th1) ++ "<" ++ exprToString(th2) ++ "?" ++ exprToString(th3) ++ ":" ++ exprToString(th4) ++ ")"
  | Circ(circ1, circ2) =>
      "(" ++ exprToString(circ1) ++ "^2+" ++ exprToString(circ2) ++ ")"
  | Arcsin(m4) => "asin(" ++ exprToString(m4) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Fibonacci(expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(sine) => sin(pi *. eval((sine, x, y)))
  | Cosine(cosine) => cos(pi *. eval((cosine, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(t1, t2) => eval((t1, x, y)) *. eval((t2, x, y))
  | Thresh(th1, th2, th3, th4) =>
      if eval((th1, x, y)) < eval((th2, x, y)) then eval((th3, x, y)) else eval((th4, x, y))
  | Circ(circ1, circ2) =>
      eval((circ1, x, y)) ** 2. +. eval((circ2, x, y)) ** 2.
  | Arcsin(m4) => asin(eval((nlog, x, y)))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Arcsin(expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Quad(expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(sine) => sin(pi *. eval((sine, x, y)))
  | Cosine(cosine) => cos(pi *. eval((cosine, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(t1, t2) => eval((t1, x, y)) *. eval((t2, x, y))
  | Thresh(th1, th2, th3, th4) =>
      if eval((th1, x, y)) < eval((th2, x, y)) then eval((th3, x, y)) else eval((th4, x, y))
  | Circ(circ1, circ2) =>
      eval((circ1, x, y)) ** 2. +. eval((circ2, x, y)) ** 2.
  | Arcsin(m4) => eval((m4, x, y)) ** 4.
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Oscillate(expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(sine) => "sin(pi*" ++ exprToString(sine) ++ ")"
  | Cosine(cosine) => "cos(pi*" ++ exprToString(cosine) ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(t1, t2) => exprToString(t1) ++ "*" ++ exprToString(t2)
  | Thresh(th1, th2, th3, th4) =>
      "(" ++ exprToString(th1) ++ "<" ++ exprToString(th2) ++ "?" ++ exprToString(th3) ++ ":" ++ exprToString(th4) ++ ")"
  | Circ(circ1) => "sqrt(|1-" ++ exprToString(circ1) ++ "^2|)"
  | Oscillate(m4) =>
      "(" ++ exprToString(m4) ++ "/((1-" ++ exprToString(m4) ++ ")^2+" ++ exprToString(m4) ++ "^2))"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Oscillate(expr)
 in let buildCirc = fun c1 -> Circ(c1) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr)
  + Oscillate(expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(sine) => sin(pi *. eval((sine, x, y)))
  | Cosine(cosine) => cos(pi *. eval((cosine, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(t1, t2) => eval((t1, x, y)) *. eval((t2, x, y))
  | Thresh(th1, th2, th3, th4) =>
      if eval((th1, x, y)) < eval((th2, x, y)) then eval((th3, x, y)) else eval((th4, x, y))
  | Circ(circ1) => sqrt(abs_float(1. -. eval((circ1, x, y)) ** 2.))
  | Oscillate(m4) =>
      let x = eval((m4, x, y)) in x /. sqrt(1. -. x ** 2. +. x ** 2.)
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
  + Circ(expr, expr, expr)
  + Oscillate(expr)
 in let buildCirc = fun (c1, c2) -> Circ((c1, c2)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VaryY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ " +" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ " * " ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Tan(a) => "sin(pi*" ++ exp(a) ++ ")/(cos(pi*" ++ exp(a) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Thresh((VarX, VarY, VarX, Times((Tan(Sine(VarX)), Cosine(Average((VarX, VarY))))))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tan(expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Tan(a) => "sin(pi*" ++ exp(a) ++ ")/(cos(pi*" ++ exp(a) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tan(expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Thresh((VarX, VarY, VarX, Times((Tan(Sine(VarX)), Cosine(Average((VarX, VarY))))))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tan(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Tangent(a) => sin(pi *. eval((a, x, y))) /. cos(pi *. eval((a, x, y)))
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tan(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Tangent(a) => "sin(pi*" ++ exp(a) ++ ")/(cos(pi*" ++ exp(a) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tan(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tangent(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Thresh((VarX, VarY, VarX, Times((Tan(Sine(VarX)), Cosine(Average((VarX, VarY))))))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tangent(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Divide(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Divide(Thresh((VarX, VarY, VarX, (Times(Sine(VarX)), Cosine(Average((VarX, VarY)))), VarY))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Tangent(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Divide(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Thresh((VarX, VarY, VarX, (Times(Sine(VarX)), Cosine(Average((VarX, VarY)))), VarY)) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Hoi(expr, expr)
  + Average(expr, expr)
  + Times(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Hoi(a, b, c) =>
      "sin(pi*" ++ exp(a) ++ ")"("*")("cos(pi*" ++ exp(b) ++ ")")("/2")
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Hoi(expr, expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> let exp = exprToString in case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(a) => "sin(pi*" ++ exp(a) ++ ")"
  | Cosine(a) => "cos(pi*" ++ exp(a) ++ ")"
  | Boo(a) => "((" ++ exp(a) ++ "+" ++ exp(a) ++ ")/100)"
  | Average(a, b) => "((" ++ exp(a) ++ "+" ++ exp(b) ++ ")/2)"
  | Times(a, b) => exp(a) ++ "*" ++ exp(b)
  | Thresh(a, b, c, d) =>
      "(" ++ exp(a) ++ "<" ++ exp(b) ++ "?" ++ exp(c) ++ ":" ++ exp(d) ++ ")"
  | Hoi(a, b, c) =>
      "sin(pi*" ++ exp(a) ++ ")*cos(pi*" ++ exp(b) ++ ")/(" ++ exp(c) ++ ")"
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Square(expr)
  + Hoi(expr, expr, expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let pi = 4. *. atan(1.) in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Sine(a) => sin(pi *. eval((a, x, y)))
  | Cosine(a) => cos(pi *. eval((a, x, y)))
  | Boo(a) => eval((a, x, y)) +. eval((a, x, y)) /. 100.
  | Average(a, b) => eval((a, x, y)) +. eval((b, x, y)) /. 2.
  | Times(a, b) => eval((a, x, y)) *. eval((b, x, y))
  | Hoi(a, b, c) =>
      sin(pi *. eval((a, x, y))) *. cos(pi *. eval((b, x, y))) /. eval((c, x, y))
  | Thresh(a, b, c, d) =>
      if eval((a, x, y)) < eval((b, x, y)) then eval((c, x, y)) else eval((d, x, y))
end in ?
|};
  {|
let digitsOfInt = fun n -> let numL = [] in if n / 10 > 0 then &&(int_mod((n, 10)) :: numL)(digitsOfInt(n) / 10) else numL in ?
|};
  {|
let digitsOfInt = fun n -> let sumL = [] in ? in ?
|};
  {|
let removeDuplicates = fun l -> let helper = fun (seen, rest) -> case rest 
  | [] => seen
  | h :: t =>
      let seen' = if mem(h)(t) then true else false in let rest' = failwith("to be written") in helper((seen', rest'))
end in rev(helper(([], l))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> [Thresh(?)] in ?
|};
  {|
let pi = 4. *. atan(1.) in type expr = 
  + VarX
  + VarY
  + Neg(expr)
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Smallest(expr, expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let eval = fun (e, x, y) -> case e 
  | VarX => x
  | VarY => y
  | Neg(e1) => eval((e1, x, y)) *. -1.
  | Sine(e1) => sin(pi *. eval((e1, x, y)))
  | Cosine(e1) => cos(pi *. eval((e1, x, y)))
  | Average(e1, e2) => eval((e1, x, y)) +. eval((e2, x, y)) /. 2.
  | Times(e1, e2) => eval((e1, x, y)) *. eval((e2, x, y))
  | Smallest(e1, e2, e3) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then if eval((e1, x, y)) < eval((e3, x, y)) then eval((e1, x, y)) else eval((e3, x, y)) else if eval((e2, x, y)) < eval((e3, x, y)) then eval((e2, x, y)) else eval((e3, x, y))
  | Thresh(e1, e2, e3, e4) =>
      if eval((e1, x, y)) < eval((e2, x, y)) then eval((e3, x, y)) else eval((e4, x, y))
end in let _ = eval(Smallest((VarX, VarY, Neg(VarX), 1, 2))) in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let exprToString = fun e -> case e 
  | VarX => "x"
  | VarY => "y"
  | Sine(e') => "sin(pi*" ++ exprToString(e') ++ ")"
  | Cosine(e') => "cos(pi*" ++ exprToString(e') ++ ")"
  | Average(e1, e2) =>
      "((" ++ exprToString(e1) ++ "+" ++ exprToString(e2) ++ ")/2)"
  | Times(e1, e2) => exprToString(e1) ++ "*" ++ exprToString(e2)
  | Thresh(e1, e2, e3, e4) =>
      "(" ++ exprToString(e1) ++ "<" ++ exprToString(e2) ++ "?" ++ exprToString(e3) ++ ":" ++ exprToString(e4) ++ ")"
  | Exp(e') => "e^" ++ exprToString(e')
end in ?
|};
  {|
type expr = 
  + VarX
  + VarY
  + Sine(expr)
  + Cosine(expr)
  + Average(expr, expr)
  + Times(expr, expr)
  + Thresh(expr, expr, expr, expr)
 in let sampleExpr1 = Thresh((VarX, VarY, VarX, Times((Sine(Exp(VarX)), Cosine(Average((VarX, VarY))))))) in ?
|};
]
