module IntMap = Map.Make (Int)
module StringMap = Map.Make (String)

type inner_map = int list StringMap.t
type page_order = inner_map IntMap.t
type position = Before | After

let add_page key position target page_order =
  let ordering =
    match IntMap.find_opt key page_order with
    | Some value -> value
    | None -> StringMap.empty
  in
  let existing_in_pos =
    match StringMap.find_opt position ordering with Some b -> b | None -> []
  in
  let new_position = existing_in_pos @ [ target ] in
  let new_ordering = StringMap.add position new_position ordering in
  IntMap.add key new_ordering page_order

let parse_rule r =
  match String.split_on_char '|' r with
  | [ f; s ] -> [ int_of_string f; int_of_string s ]
  | _ -> failwith "unexpected rule format"

let parse_rules rules =
  List.fold_left
    (fun m r ->
      match parse_rule r with
      | [ first; second ] ->
          m |> add_page first "after" second |> add_page second "before" first
      | _ -> failwith "unexpected rule format")
    IntMap.empty rules

let print_map map =
  IntMap.iter
    (fun key inner ->
      let before_list =
        match StringMap.find_opt "before" inner with
        | Some lst -> lst
        | None -> []
      in
      let after_list =
        match StringMap.find_opt "after" inner with
        | Some lst -> lst
        | None -> []
      in
      Printf.printf "%d: { \"before\": [%s], \"after\": [%s] }\n" key
        (String.concat "; " (List.map string_of_int before_list))
        (String.concat "; " (List.map string_of_int after_list)))
    map

let is_after key target rules =
  (* Printf.printf "Checking %d against %d\n" key target; *)
  let inner_map = rules |> IntMap.find key in
  match StringMap.find_opt "after" inner_map with
  | Some al -> List.exists (fun p -> p = target) al
  | None -> false

let is_ordered rules update =
  update
  |> List.mapi (fun i x ->
         update
         |> List.drop (i + 1)
         |> List.for_all (fun y -> is_after x y rules))
  |> List.for_all (fun x -> x = true)

let filter_ordered rules updates =
  List.filter
    (fun u ->
      u |> String.split_on_char ',' |> List.map int_of_string
      |> is_ordered rules)
    updates

let run () =
  let input = Utils.read_lines "inputs/day05.txt" in
  let newline_pos = input |> List.find_index (fun x -> x = "") |> Option.get in
  let ordering_rules = List.init newline_pos (fun i -> List.nth input i) in
  let parsed_rules = parse_rules ordering_rules in
  (* print_endline "--------------------------"; *)
  (* print_map parsed_rules; *)
  let remaining = List.length input - (newline_pos + 1) in
  let new_start = newline_pos + 1 in
  let updates = List.init remaining (fun i -> List.nth input (new_start + i)) in
  let ordered_updates = filter_ordered parsed_rules updates in
  let sum =
    List.fold_left
      (fun acc u ->
        let ul = u |> String.split_on_char ',' |> List.map int_of_string in
        let idx = List.length ul / 2 in
        acc + List.nth ul idx)
      0 ordered_updates
  in
  Printf.printf "Sum of ordered updates: %d\n" sum
