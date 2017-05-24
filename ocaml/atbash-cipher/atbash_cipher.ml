open Core.Std

let cipher = "zyxwvutsrqponmlkjihgfedcba"
let ascii_a = 97

let encode ?block_size msg =
  let bs = match block_size with None -> 5 | Some x -> x in
  let conv ch =
    if Char.is_digit ch then Some(ch)
    else if (Char.is_lowercase ch) = false then None
    else Some(String.get cipher ((Char.to_int ch) - ascii_a))
  in
  let transform res ch = 
    match (conv ch) with
    | None -> res
    | Some(ch) ->
      if (String.length res) mod (bs + 1) = bs then
        Printf.sprintf "%s %c" res ch
      else Printf.sprintf "%s%c" res ch
  in
  String.fold (String.lowercase msg) ~f:transform ~init:""  

let decode msg =
  let transform res ch =
    if Char.is_digit ch then Printf.sprintf "%s%c" res ch
    else
      try
        let idx = String.index_exn cipher ch in
        Printf.sprintf "%s%c" res (Char.of_int_exn (idx + ascii_a))
      with _ -> res
  in
  String.fold msg ~f:transform ~init:"" 
