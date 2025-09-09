let block_trim_regex = Re.Pcre.regexp {|(^|do\(\))(.*?)($|don't\(\))|}
let op_regex = Re.Pcre.regexp {|mul\(\d{1,3},\d{1,3}\)|}
let mul_args_regex = Re.Pcre.regexp {|\b\d+\b|}

let retrieve_operations data_str =
  data_str |> Re.all block_trim_regex
  |> List.map (fun m -> Re.Group.get m 2)
  |> String.concat "" |> Re.all op_regex
  |> List.map (fun m -> Re.Group.get m 0)

let parse_mul operations =
  List.map
    (fun op ->
      let args =
        Re.all mul_args_regex op |> List.map (fun m -> Re.Group.get m 0)
      in
      assert (List.length args == 2);
      List.map int_of_string args)
    operations

let run () =
  let corrupted_data =
    "inputs/day03.txt" |> Utils.read_lines |> String.concat ""
  in
  let operations = retrieve_operations corrupted_data in
  let operation_args_list = parse_mul operations in
  let result_sum =
    List.fold_left
      (fun acc lst ->
        match lst with
        | [ x; y ] -> acc + (x * y)
        | _ -> failwith "Unexpected argument")
      0 operation_args_list
  in
  Printf.printf "Result %d\n" result_sum
