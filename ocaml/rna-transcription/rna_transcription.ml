type dna = [ `A | `C | `G | `T ]
type rna = [ `A | `C | `G | `U ]

let to_rna l =
  let conv = function
    | `A -> `U | `C -> `G | `G -> `C| `T -> `A
  in
  List.map conv l
