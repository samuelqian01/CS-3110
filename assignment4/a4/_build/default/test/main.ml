open OUnit2
open Jocalf

(* You're free to change any of the code in this file, but the test
   cases themselves demonstrate the exact output your interpreter has to
   produce. So I don't recommend changing the strings in the provided
   test cases. *)

(* All of these provided tests will currently fail because you have not
   yet implemented interpretation of any of these syntactic forms. A
   completed interpreter should pass all of them, though. OCaml allows
   {|...|} as a syntax for strings in which the ... can contain
   unescaped quotes. This is super useful for constructing test cases,
   as shown below. *)
let provided_tests =
  [
    ("int constant", {|42|}, "42");
    ("negative int constant", {|-1|}, "-1");
    (let str_max_int = string_of_int max_int in
     ("max int", str_max_int, str_max_int));
    (let str_min_int = string_of_int min_int in
     ("min int", str_min_int, str_min_int));
    ("true", {|true|}, "true");
    ("false", {|false|}, "false");
    ("undefined", {|undefined|}, "undefined");
    ("magic word", {|"xyzzy"|}, {|"xyzzy"|});
    ("div by 0", {|4/0|}, {|Exception: "Error: Division by zero"|});
    ("mod by 0", {|4 mod 0|}, {|Exception: "Error: Division by zero"|});
    ( "unbound var",
      {|let x = 0 in y|},
      {|Exception: "Error: Unbound variable"|} );
    ("throw", {|throw 0|}, "Exception: 0");
    ("anonymous function", {|fun (x) -> 0|}, "<function>");
    ( "apply non-function",
      {|0 0|},
      {|Exception: "Error: Not a function"|} );
    ( "apply wrong arity",
      {|(fun (x) -> 0) 1 2|},
      {|Exception: "Error: Wrong number of arguments"|} );
    ("ref", {|ref 0|}, "<reference>");
    ( "assign non location",
      {|1 := 0|},
      {|Exception: "Error: Assignment to non-location"|} );
    ("object", {|{"x":1}|}, "<object>");
    ("length", {|length "bigred"|}, "6");
    ("typeof int", {|typeof 42|}, {|"int"|});
    ("typeof string", {|typeof "xyzzy"|}, {|"string"|});
    ("typeof bool", {|typeof true|}, {|"bool"|});
    ("typeof undefined", {|typeof undefined|}, {|"undefined"|});
    ("typeof object", {|typeof {"x":1}|}, {|"object"|});
    ("typeof reference", {|typeof (ref 0)|}, {|"reference"|});
    ("typeof closure", {|typeof (fun (x) -> 0)|}, {|"function"|});
    ("typeof extern", {|typeof length|}, {|"function"|});
    ("defined", {|is_defined undefined|}, "false");
    ("has field", {|has_field {"x":1} "x"|}, "true");
  ]

(* Here's a start at some test cases of your own. *)
let my_tests = [ ("true constant", {|true|}, "true"); 
("false constant", {|false|}, "false");
("positive int constant", {|3110|}, "3110");
("random string with spaces", {|"project is hard"|}, {|"project is hard"|});
("empty string", {|""|}, {|""|});
("undefined", {|undefined|}, "undefined");
("typeof int", {|typeof 43|}, {|"int"|});
("typeof string", {|typeof "xyzzy"|}, {|"string"|});
("typeof bool", {|typeof true|}, {|"bool"|});
("typeof undefined", {|typeof undefined|}, {|"undefined"|});
("not on an int", {|not 3|}, "false");
("not on an int", {|not 0|}, "true");
("not on an empty string", {|not ""|}, "true");
("not on a non-empty string", {|not "a"|}, "false");
("minus on an int", {|- 3|}, "-3");
("minus on an false bool", {|- false|}, "0");
("minus on an true bool", {|- true|}, "-1");

]

let tests = (**provided_tests @*) my_tests

let make_interp_expr_test n in_str out_str =
  n >:: fun _ ->
  assert_equal out_str (Interp.interp_expr in_str) ~printer:Fun.id

let suite =
  "suite"
  >::: List.map (fun (n, i, o) -> make_interp_expr_test n i o) tests

let _ = run_test_tt_main suite
