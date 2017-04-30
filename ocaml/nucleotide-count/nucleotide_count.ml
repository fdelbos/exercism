open Core.Std
module CMap = Char.Map
                
let count str ch = String.count str ~f:(Char.equal ch)

let nucleotide_counts str =
  let incr m ch =
    match CMap.find m ch with
    | Some(nb) -> nb + 1
    | None -> 1
  in
  let count m ch = CMap.add m ~key:ch ~data:(incr m ch) in
  String.fold str ~f:count ~init:CMap.empty
