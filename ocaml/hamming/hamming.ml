type nucleotide = A | C | G | T

let hamming_distance l1 l2 = 
  let rec count n = function
    | (h1::t1, h2::t2) when h1 = h2 -> count n (t1, t2)
    | (_::t1, _::t2) -> count (n + 1) (t1, t2)
    | ([], []) -> Some n
    | _ -> None
  in
  count 0 (l1, l2)
