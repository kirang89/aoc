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
  Printf.printf "Check leaving area for direction: %s, position: (%d, %d) and length: %d" direction x y (Array.length grid);
  match direction with 
  | "up" -> x - 1 < 0
  | "right" -> y + 1 >= Array.length grid.(x)
  | "left" -> y - 1 < 0
  | "down" -> x + 1 >= Array.length grid
  | _ -> failwith "Unexpected direction"

let move guard_pos grid =
  let rec sim_move grid pos direction =
    Printf.printf "Sim move to direction: %s" direction ;
    let gx, gy =
      match pos with
      | Some (x, y) -> Printf.printf "Direction: %s, Guard position: (%d, %d) " direction x y; print_newline (); (x, y)
      | _ -> failwith "Unexpected grid position"
    in
    if leaving_area grid (gx, gy) direction then (grid.(gx).(gy) <- 'X'; grid)
    else (
      Printf.printf "Not leaving area"; 
      grid.(gx).(gy) <- 'X';
      match direction with
      | "up" ->
        if grid.(gx - 1).(gy) = '#' then sim_move grid pos "right"
        else sim_move grid (Some (gx - 1, gy)) "up"
      | "right" ->  
        if grid.(gx).(gy + 1) = '#' then sim_move grid pos "down" 
        else sim_move grid (Some (gx, gy + 1)) "right"
      | "down" -> ( 
          Printf.printf "Moving direction: %s from position %d %d" direction gx gy;
          print_newline ();
          Printf.printf "Grid len: %d" (Array.length grid);
          Printf.printf "Grid next row len: %d" (Array.length grid.(gx + 1));
          Printf.printf "Next Pos elem: %c" grid.(gx + 1).(gy);
          if grid.(gx + 1).(gy) = '#' then sim_move grid pos "left"
          else sim_move grid (Some (gx + 1, gy)) "down")
      | "left" -> if grid.(gx).(gy - 1) = '#' then sim_move grid pos "up" 
        else sim_move grid (Some (gx, gy - 1)) "left"
      | _ -> grid)
  in
  sim_move grid guard_pos "up"

let distinct_move_positions grid =
  Array.fold_right
    (fun row acc ->
       acc + (row |> Array.to_list |> List.filter (fun x -> x = 'X') |> List.length))
    grid
    0

let run () =
  let grid = parse_map "inputs/day06.txt" in
  let guard_pos = find_guard_position grid in
  let sim_grid = move guard_pos grid in
  let dpos = distinct_move_positions  sim_grid in
  print_string "MAP:";
  print_newline ();
  Array.iter
    (fun r ->
       Array.iter (fun c -> Printf.printf "%c" c) r;
       print_newline ())
    sim_grid;
  Printf.printf "Distinct Positions: %d" dpos;
  print_newline ();
  match guard_pos with
  | Some (x, y) ->
    Printf.printf "Guard Pos: %d, %d" x y;
    print_newline ()
  | None -> failwith "Guard not found"
