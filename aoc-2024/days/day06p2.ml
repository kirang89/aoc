module StateSet = Set.Make (struct
  type t = (int * int) * string

  let compare = compare
end)

let find_guard_position grid =
  let rec row_iter idx =
    if idx = Array.length grid then None
    else
      let rec col_iter cidx =
        if cidx = Array.length grid.(idx) then row_iter (idx + 1)
        else
          match grid.(idx).(cidx) with
          | '^' -> Some (idx, cidx)
          | _ -> col_iter (cidx + 1)
      in
      col_iter 0
  in
  row_iter 0

let parse_map file =
  file
  |> Utils.read_lines
  |> List.map (fun l -> l |> String.to_seq |> Array.of_seq)
  |> Array.of_list

let leaving_area grid (x, y) direction =
  match direction with
  | "up" -> x - 1 < 0
  | "right" -> y + 1 >= Array.length grid.(x)
  | "left" -> y - 1 < 0
  | "down" -> x + 1 >= Array.length grid
  | _ -> failwith "Unexpected direction"

let does_loop (guard_pos, guard_dir) obs_pos grid =
  let rec sim_move grid pos direction visited =
    let gx, gy =
      match pos with
      | Some (x, y) -> (x, y)
      | _ -> failwith "Unexpected grid position"
    in
    if StateSet.mem ((gx, gy), direction) visited then true
    else if leaving_area grid (gx, gy) direction then false
    else
      let visited' = StateSet.add ((gx, gy), direction) visited in
      match direction with
      | "up" ->
          if grid.(gx - 1).(gy) = '#' || Some (gx - 1, gy) = obs_pos then
            sim_move grid pos "right" visited'
          else sim_move grid (Some (gx - 1, gy)) "up" visited'
      | "right" ->
          if grid.(gx).(gy + 1) = '#' || Some (gx, gy + 1) = obs_pos then
            sim_move grid pos "down" visited'
          else sim_move grid (Some (gx, gy + 1)) "right" visited'
      | "down" ->
          if grid.(gx + 1).(gy) = '#' || Some (gx + 1, gy) = obs_pos then
            sim_move grid pos "left" visited'
          else sim_move grid (Some (gx + 1, gy)) "down" visited'
      | "left" ->
          if grid.(gx).(gy - 1) = '#' || Some (gx, gy - 1) = obs_pos then
            sim_move grid pos "up" visited'
          else sim_move grid (Some (gx, gy - 1)) "left" visited'
      | _ -> false
  in
  sim_move grid guard_pos guard_dir StateSet.empty

let print_map grid =
  print_string "MAP:";
  print_newline ();
  Array.iter
    (fun r ->
      Array.iter (fun c -> Printf.printf "%c" c) r;
      print_newline ())
    grid;
  ()

let obstruction_candidates grid =
  Array.mapi (fun ridx row -> Array.mapi (fun cidx _ -> (ridx, cidx)) row) grid

let potential_obstructions guard_pos candidates grid =
  candidates
  |> Array.to_list
  |> List.filter (fun (cx, cy) ->
         grid.(cx).(cy) <> '#' || grid.(cx).(cy) <> '^')
  |> List.filter (fun (cx, cy) ->
         let result = does_loop (guard_pos, "up") (Some (cx, cy)) grid in
         Printf.printf "Loop when obs at (%d, %d): %b" cx cy result;
         print_newline ();
         result)
  |> Array.of_list

let run () =
  let grid = parse_map "inputs/day06.txt" in
  let guard_pos = find_guard_position grid in
  let obs_candidates =
    grid |> obstruction_candidates |> Array.fold_left Array.append [||]
  in
  let loop_candidates = potential_obstructions guard_pos obs_candidates grid in
  Printf.printf "# of obstruction candidates: %d" (Array.length obs_candidates);
  print_newline ();
  Printf.printf "# of potential obstructions: %d" (Array.length loop_candidates);
  print_newline ()
