let raindrop i =
  let rec aux res = function
    | (nb, msg) :: t when i mod nb = 0 -> aux (res ^ msg) t
    | _ :: t -> aux res t
    | _ -> if res = "" then string_of_int i else res
  in
  aux "" [(3, "Pling"); (5, "Plang"); (7, "Plong")]
