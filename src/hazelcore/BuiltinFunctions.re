let ctx: VarCtx.t = [
  ("length", HTyp.Arrow(String, Int)),
  ("string_of_int", Arrow(Int, String)),
  ("int_of_string", Arrow(String, Int)),
  ("string_of_bool", Arrow(Bool, String)),
  ("bool_of_string", Arrow(String, Bool)),
  ("string_of_float", Arrow(Float, String)),
  ("float_of_string", Arrow(String, Float)),
  ("int_of_float", Arrow(Float, Int)),
  ("float_of_int", Arrow(Int, Float)),
  ("equal", Arrow(String, Arrow(String, Bool))),
  ("compare", Arrow(String, Arrow(String, Int))),
  ("trim", Arrow(String, String)),
  ("escaped", Arrow(String, String)),
  ("assert", Arrow(Bool, Prod([]))),
];

let lookup = x => VarMap.lookup(ctx, x);

let rec is_int_of_string_mix = (s, hex) =>
  if (s == "") {
    false;
  } else if (String.length(s) == 1) {
    if (s.[0] >= '0' && s.[0] <= hex) {
      true;
    } else {
      false;
    };
  } else if (s.[0] >= '0' && s.[0] <= hex) {
    is_int_of_string_mix(String.sub(s, 1, String.length(s) - 1), hex);
  } else {
    false;
  };

let rec is_int_of_string_hex = s =>
  if (s == "") {
    false;
  } else if (String.length(s) == 1) {
    let ch1 = Char.lowercase_ascii(s.[0]);
    if (ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f') {
      true;
    } else {
      false;
    };
  } else {
    let ch1 = Char.lowercase_ascii(s.[0]);
    if (ch1 >= '0' && ch1 <= '9' || ch1 >= 'a' && ch1 <= 'f') {
      is_int_of_string_hex(String.sub(s, 1, String.length(s) - 1));
    } else {
      false;
    };
  };

let is_int_of_string = s =>
  if (s == "") {
    false;
  } else if (String.length(s) == 1) {
    if (s.[0] >= '0' && s.[0] <= '9') {
      true;
    } else {
      false;
    };
  } else {
    let hex = String.sub(s, 0, 2);
    switch (hex) {
    | "0b" =>
      is_int_of_string_mix(String.sub(s, 2, String.length(s) - 2), '1')
    | "0o" =>
      is_int_of_string_mix(String.sub(s, 2, String.length(s) - 2), '7')
    | "0x" => is_int_of_string_hex(String.sub(s, 2, String.length(s) - 2))
    | _ => is_int_of_string_mix(String.sub(s, 2, String.length(s) - 2), '9')
    };
  };
