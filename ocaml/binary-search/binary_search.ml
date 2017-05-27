let find arr v =
  let rec find l r =
    if l > r then None
    else
      let m = (l + r) / 2 in
      match Array.get arr m with
      | v' when v' < v -> find (m + 1) r
      | v' when v' > v -> find l (m - 1)
      | _ -> Some(m)
  in find 0 (Array.length arr - 1)
