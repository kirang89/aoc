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

let print_list formatter list =
  List.iter (fun x -> Printf.printf "%s\n" (formatter x)) list

let list_freq elem list =
  list |> List.filter (fun x -> x == elem) |> List.length

let dot_product list1 list2 =
  List.fold_left2 (fun acc x y -> acc + (x * y)) 0 list1 list2

let check_while pred list =
  let rec loop acc list =
    match (acc, list) with
    | false, _ -> false
    | true, x :: y :: xs -> loop (pred x y) (y :: xs)
    | true, _ :: xs -> loop true xs
    | true, [] -> true
  in
  loop true list

let list_remove_nth n list = List.take n list @ List.drop (n + 1) list

let assert_equal ~expected ~actual =
  if expected <> actual then (
    Printf.printf "Assertion failed:\n";
    Printf.printf "  Expected: %d\n" expected;
    Printf.printf "  Actual: %d\n" actual;
    failwith "Assertion failed")
