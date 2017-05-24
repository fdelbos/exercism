let transform data =
  data
  |> List.map (fun (p, l) -> List.map (fun c -> (Char.lowercase_ascii c, p)) l)
  |> List.flatten
  |> List.sort (fun (a, _) (b, _) -> Char.compare a b)
