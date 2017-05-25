let transform data =
  let transform' = (fun (p, l) -> List.map (fun c -> (Char.lowercase_ascii c, p)) l) in
  data
  |> Core.Std.List.concat_map ~f:transform'
  |> List.sort (fun (a, _) (b, _) -> Char.compare a b)
