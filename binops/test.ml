open Compile
open Runner
open Printf
open OUnit2

let t name program expected = name>::test_run program name expected;;

let myTestList = []

let suite =
"suite">:::myTestList

let () =
  run_test_tt_main suite
