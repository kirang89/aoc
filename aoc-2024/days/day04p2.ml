let directions =
  [ (0, 1); (1, 0); (1, 1); (0, -1); (-1, 0); (-1, -1); (1, -1); (-1, 1) ]

let grid_get_opt grid row col =
  if row < 0 || row >= Array.length grid then None
  else
    let row_val = Array.get grid row in
    if col < 0 || col >= String.length row_val then None
    else Some (String.get row_val col)

let rec match_direction grid word row col dx dy =
  match word with
  | [] -> true
  | x :: xs -> (
      match grid_get_opt grid row col with
      | Some e when e = x -> match_direction grid xs (row + dx) (col + dy) dx dy
      | _ -> false)

let search_directions grid word row col directions =
  let word_list = word |> String.to_seq |> List.of_seq in
  directions
  |> List.map (fun (dx, dy) ->
         if match_direction grid word_list row col dx dy then 1 else 0)
  |> List.fold_left ( + ) 0

let search_x_directions grid word row col =
  let rdd = (1, 1) in
  let ldd = (1, -1) in
  let rdu = (-1, 1) in
  let ldu = (-1, -1) in
  [
    search_directions grid word row col [ rdd ]
    + search_directions grid word row (col + 2) [ ldd ]
    + search_directions grid word (row + 2) col [ rdu ];
    search_directions grid word row col [ ldd ]
    + search_directions grid word row (col - 2) [ rdd ]
    + search_directions grid word (row + 2) col [ ldu ];
    search_directions grid word row col [ rdu ]
    + search_directions grid word row (col + 2) [ ldu ]
    + search_directions grid word (row - 2) col [ rdd ];
    search_directions grid word row col [ ldu ]
    + search_directions grid word row (col - 2) [ rdu ]
    + search_directions grid word (row - 2) col [ ldd ];
  ]
  |> (fun x ->
  Printf.printf "Row %d Column %d matches: %s and count: %d\n" row col
    (String.concat " " (List.map string_of_int x))
    (x |> List.filter (fun x -> x = 2) |> List.length);
  x)
  |> List.filter (fun x -> x = 2)
  |> List.length

let count_row_occurrences grid word row =
  Array.get grid row |> String.to_seq |> List.of_seq
  |> List.mapi (fun col _ -> search_x_directions grid word row col)
  |> List.fold_left ( + ) 0

let count_occurrences grid word =
  let first_letter = String.get word 0 in
  Array.mapi
    (fun idx row ->
      if not (String.contains row first_letter) then 0
      else count_row_occurrences grid word idx)
    grid
  |> Array.fold_left ( + ) 0

let run () =
  let word = "MAS" in
  let grid = Utils.read_lines "inputs/day04.txt" |> Array.of_list in
  let count = count_occurrences grid word in
  Printf.printf "# of occurrences of %s: %d\n" word count

let run_tests () =
  List.iter
    (fun (word, file, expected_count) ->
      let grid = Utils.read_lines file |> Array.of_list in
      let actual_count = count_occurrences grid word in
      Utils.assert_equal ~expected:expected_count ~actual:actual_count)
    [ ("XMAS", "inputs/day04ex.txt", 18); ("XMAS", "inputs/day04.txt", 2593) ]
