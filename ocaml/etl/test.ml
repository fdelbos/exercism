(* Test/exercise version: "1.0.0" *)

open Core.Std
open OUnit2
open Etl

let ae exp got _test_ctxt =
  let sexpr_of l = List.sexp_of_t (Tuple2.sexp_of_t Char.sexp_of_t Int.sexp_of_t) l in
  let printer l = Sexp.to_string (sexpr_of l) in
  assert_equal exp got ~printer

let tests = [
   "a single letter" >::
      ae [('a', 1)] 
         (transform [(1, ['A'])]);
   "single score with multiple letters" >::
      ae [('a', 1); ('e', 1); ('i', 1); ('o', 1); ('u', 1)] 
         (transform [(1, ['A'; 'E'; 'I'; 'O'; 'U'])]);
   "multiple scores with multiple letters" >::
      ae [('a', 1); ('d', 2); ('e', 1); ('g', 2)] 
         (transform [(1, ['A'; 'E']); (2, ['D'; 'G'])]);
   "multiple scores with differing numbers of letters" >::
      ae [('a', 1); ('b', 3); ('c', 3); ('d', 2); ('e', 1); 
          ('f', 4); ('g', 2); ('h', 4); ('i', 1); ('j', 8); 
          ('k', 5); ('l', 1); ('m', 3); ('n', 1); ('o', 1); 
          ('p', 3); ('q', 10); ('r', 1); ('s', 1); ('t', 1); 
          ('u', 1); ('v', 4); ('w', 4); ('x', 8); ('y', 4); 
          ('z', 10)] 
         (transform [(1, ['A'; 'E'; 'I'; 'O'; 'U'; 'L'; 'N'; 'R'; 'S'; 'T']); 
                     (2, ['D'; 'G']); 
                     (3, ['B'; 'C'; 'M'; 'P']); 
                     (4, ['F'; 'H'; 'V'; 'W'; 'Y']); 
                     (5, ['K']); 
                     (8, ['J'; 'X']); 
                     (10, ['Q'; 'Z'])]);
]

let () =
  run_test_tt_main ("etl tests" >::: tests)
