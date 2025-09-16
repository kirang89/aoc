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

let count_row_occurrences grid word directions row =
  Array.get grid row |> String.to_seq |> List.of_seq
  |> List.mapi (fun col _ -> search_directions grid word row col directions)
  |> List.fold_left ( + ) 0

let count_occurrences grid word directions =
  let first_letter = String.get word 0 in
  Array.mapi
    (fun idx row ->
      if not (String.contains row first_letter) then 0
      else count_row_occurrences grid word directions idx)
    grid
  |> Array.fold_left ( + ) 0

let search_word word file =
  let data = Utils.read_lines file in
  let grid = Array.of_list data in
  count_occurrences grid word directions

let run () =
  let word = "XMAS" in
  let count = search_word word "inputs/day04.txt" in
  Printf.printf "# of occurrences of %s: %d\n" word count

let run_tests () =
  List.iter
    (fun (word, file, expected_count) ->
      let actual_count = search_word word file in
      Utils.assert_equal ~expected:expected_count ~actual:actual_count)
    [ ("XMAS", "inputs/day04ex.txt", 18); ("XMAS", "inputs/day04.txt", 2593) ]
