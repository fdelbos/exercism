let opening = "[({"
let closing = "])}" 

let are_balanced str =
  let get_closing c = String.index opening c |> String.get closing in
  let rec find_closing c = function
    | c' :: t when c' = c -> (true, t)
    | c' :: t when String.contains opening c' ->
      let (res, t) = find_closing (get_closing c') t in
      if res then find_closing c t else (false, [])
    | c' :: _ when String.contains closing c' -> (false, [])
    | _ :: t -> find_closing c t
    | [] -> (false, [])
  in
  let rec search = function
    | c :: t when String.contains opening c ->
      let (res, t) = find_closing (get_closing c) t in
      if res = false then false else search t
    | c :: _ when String.contains closing c -> false
    | _ :: t -> search t
    | [] -> true
  in search (Core.Std.String.to_list str)
