type base = int

let rec pow nb e =  if e = 0 then 1 else nb * (pow nb (e - 1))

let convert_bases ~from:f ~digits:d ~target:t =
  let rec read acc = function
    | h::_ when h >= f || h < 0 -> None 
    | h::m ->
        let p = pow f (List.length m) in
        read (acc + (h * p)) m
    | _ -> Some acc
  in
  let rec convert = function
    | 0 -> []
    | nb -> (nb mod t) :: convert (nb / t)
  in
  if f <= 1 || t <= 1 || List.length d = 0 then None
  else match (read 0 d) with
    | Some(0) -> Some [0]
    | Some(nb) -> Some (List.rev (convert nb))
    | _ -> None  
