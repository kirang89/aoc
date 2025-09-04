let read_lines filename =
  let ic = open_in filename in
  let rec lines acc =
    try
      let line = input_line ic in
      lines (line :: acc)
    with End_of_file ->
      close_in ic;
      List.rev acc
  in
  lines []

let locations lines =
  let rec loop loc1 loc2 = function
    | [] -> (loc1, loc2)
    | [ first; second ] :: rest -> loop (first :: loc1) (second :: loc2) rest
    | _ -> failwith "Unexpected state"
  in
  loop [] [] lines

let location_pair_list lines =
  let loc_pair line_str =
    String.split_on_char ' ' line_str
    |> List.filter (fun s -> s <> "")
    |> List.map int_of_string
  in
  let rec loop acc = function
    | [] -> acc
    | first :: rest -> loop (loc_pair first :: acc) rest
  in
  let loc_pair_str_list = loop [] lines in
  let first_list =
    loc_pair_str_list |> List.map List.hd |> List.sort Int.compare
  in
  let second_list =
    loc_pair_str_list
    |> List.map (fun l ->
           match l with
           | [ _; y ] -> y
           | _ -> failwith "Unexpected inner list format")
    |> List.sort Int.compare
  in
  (first_list, second_list)

let print_list list = List.iter (fun x -> Printf.printf "%d\n" x) list

let list_freq elem list =
  list |> List.filter (fun x -> x == elem) |> List.length

let dot_product list1 list2 =
  List.fold_left2 (fun acc x y -> acc + (x * y)) 0 list1 list2

let run () =
  let lines = read_lines "inputs/day01.txt" in
  let loc1, loc2 = location_pair_list lines in
  assert (List.length loc1 == List.length loc2);
  let distances = List.map2 (fun x y -> abs (x - y)) loc1 loc2 in
  let distance_total = List.fold_left ( + ) 0 distances in
  let loc1_freqs = List.map (fun x -> list_freq x loc2) loc1 in
  let sim_score = dot_product loc1 loc1_freqs in
  Printf.printf "Total Distance: %d\n" distance_total;
  Printf.printf "Total Similarity Score: %d\n" sim_score
