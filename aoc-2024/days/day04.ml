let fetch_grid_elem grid row col = String.get grid.(row) col

let reverse_string string =
  let len = String.length string in
  String.init len (fun i -> String.get string (len - 1 - i))

let search_occurrence_forward search_string row idx =
  let length = String.length search_string in
  if idx + (length - 1) >= String.length row then false
  else if String.sub row idx length = search_string then
    (* Printf.printf "forward search in row %s at pos %d\n" row idx; *)
    true
  else false

let search_occurrence_backward search_string row idx =
  let length = String.length search_string in
  let string_rev = reverse_string search_string in
  if idx - (length - 1) < 0 then false
  else if String.sub row (idx - (length - 1)) length = string_rev then true
  else false

let search_occurrence_upward search_string grid row idx =
  (* TODO: find_index won't work if there are duplicate lines *)
  let row_idx = grid |> Array.find_index (fun l -> l = row) |> Option.get in
  let search_length = String.length search_string - 1 in
  let search_idxs = List.init (String.length search_string) (fun i -> i) in
  if row_idx - search_length < 0 then false
  else
    (* Printf.printf "Searching upwards from row: %s idx %d\n" row row_idx; *)
    List.for_all
      (fun offset ->
        fetch_grid_elem grid (row_idx - offset) idx
        = String.get search_string offset)
      search_idxs

let search_occurrence_downward search_string grid row idx =
  let row_idx = grid |> Array.find_index (fun l -> l = row) |> Option.get in
  let search_length = String.length search_string - 1 in
  let search_idxs = List.init (String.length search_string) (fun i -> i) in
  if row_idx + search_length >= Array.length grid then false
  else
    (* Printf.printf "Searching downwards from row: %s idx %d\n" row row_idx; *)
    List.for_all
      (fun offset ->
        fetch_grid_elem grid (row_idx + offset) idx
        = String.get search_string offset)
      search_idxs

let search_occurrence_left_diagonal_upward search_string grid row idx =
  (* Printf.printf "Search ldu, row: %s, idx: %d\n" row idx; *)
  let row_idx = grid |> Array.find_index (fun l -> l = row) |> Option.get in
  let search_length = String.length search_string - 1 in
  let search_idxs = List.init (String.length search_string) (fun i -> i) in
  if row_idx - search_length < 0 || idx - search_length < 0 then false
  else
    List.for_all
      (fun offset ->
        fetch_grid_elem grid (row_idx - offset) (idx - offset)
        = String.get search_string offset)
      search_idxs

let search_occurrence_left_diagonal_downward search_string grid row idx =
  (* Printf.printf "Search ldd, row: %s, idx: %d\n" row idx; *)
  let row_idx = grid |> Array.find_index (fun l -> l = row) |> Option.get in
  let search_length = String.length search_string - 1 in
  let search_idxs = List.init (String.length search_string) (fun i -> i) in
  let grid_len = Array.length grid in
  if row_idx + search_length >= grid_len || idx - search_length < 0 then false
  else
    (* Printf.printf "Search ldd, row: %s, row_len: %d, row_idx: %d\n" row row_len *)
    (* row_idx; *)
    List.for_all
      (fun offset ->
        fetch_grid_elem grid (row_idx + offset) (idx - offset)
        = String.get search_string offset)
      search_idxs

let search_occurrence_right_diagonal_upward search_string grid row idx =
  (* Printf.printf "Search rdu, row: %s, idx: %d\n" row idx; *)
  let row_idx = grid |> Array.find_index (fun l -> l = row) |> Option.get in
  let search_length = String.length search_string - 1 in
  let search_idxs = List.init (String.length search_string) (fun i -> i) in
  let row_len = String.length row in
  if row_idx - search_length < 0 || idx + search_length >= row_len then false
  else
    List.for_all
      (fun offset ->
        fetch_grid_elem grid (row_idx - offset) (idx + offset)
        = String.get search_string offset)
      search_idxs

let search_occurrence_right_diagonal_downward search_string grid row idx =
  (* Printf.printf "Search rdd, row: %s, idx: %d\n" row idx; *)
  let row_idx = grid |> Array.find_index (fun l -> l = row) |> Option.get in
  let search_length = String.length search_string - 1 in
  let search_idxs = List.init (String.length search_string) (fun i -> i) in
  let grid_len = Array.length grid in
  let row_len = String.length row in
  if row_idx + search_length >= grid_len || idx + search_length >= row_len then
    false
  else
    (* Printf.printf "Search rdd, row: %s, row_len: %d, row_idx: %d\n" row row_len *)
    (* row_idx; *)
    List.for_all
      (fun offset ->
        fetch_grid_elem grid (row_idx + offset) (idx + offset)
        = String.get search_string offset)
      search_idxs

let count_occurrences_from_pos search_string grid row idx =
  [
    search_occurrence_forward search_string row idx;
    search_occurrence_backward search_string row idx;
    search_occurrence_upward search_string grid row idx;
    search_occurrence_downward search_string grid row idx;
    search_occurrence_left_diagonal_upward search_string grid row idx;
    search_occurrence_left_diagonal_downward search_string grid row idx;
    search_occurrence_right_diagonal_upward search_string grid row idx;
    search_occurrence_right_diagonal_downward search_string grid row idx;
  ]
  |> List.filter (fun x -> x = true)
  |> List.length

let count_occurrences_from_row search_string grid row =
  let first_letter = String.get search_string 0 |> Char.uppercase_ascii in
  let rec loop acc idx =
    if idx >= String.length row then acc
    else if String.get row idx <> first_letter then loop acc (idx + 1)
    else
      loop
        (acc + count_occurrences_from_pos search_string grid row idx)
        (idx + 1)
  in
  loop 0 0

let count_occurrences_grid search_string grid =
  let first_letter = String.get search_string 0 |> Char.uppercase_ascii in
  Array.map
    (fun row ->
      if not (String.contains row first_letter) then 0
      else
        let c = count_occurrences_from_row search_string grid row in
        c)
    grid
  |> Array.fold_left ( + ) 0

let count_occurrences string file =
  let data = Utils.read_lines file in
  let grid = Array.of_list data in
  let search_string = string in
  count_occurrences_grid search_string grid

let run () =
  let search_string = "XMAS" in
  let count = count_occurrences search_string "inputs/day04.txt" in
  Printf.printf "# of occurrences of %s: %d\n" search_string count

let run_tests () =
  List.iter
    (fun (search_string, file, expected_count) ->
      let actual_count = count_occurrences search_string file in
      Utils.assert_equal ~expected:expected_count ~actual:actual_count)
    [ ("XMAS", "inputs/day04ex.txt", 18); ("XMAS", "inputs/day04.txt", 2593) ]
