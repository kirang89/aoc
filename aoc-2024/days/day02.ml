let reports =
  let raw_data = Utils.read_lines "inputs/day02.txt" in
  let rec loop acc = function
    | [] -> acc
    | x :: xs ->
        let report =
          x |> String.split_on_char ' '
          |> List.filter (fun s -> s <> "")
          |> List.map (fun s -> int_of_string s)
        in
        loop (report :: acc) xs
  in
  loop [] raw_data

let is_sorted report =
  Utils.check_while ( <= ) report || Utils.check_while ( >= ) report

let () =
  assert (is_sorted [ 1; 2; 3 ]);
  assert (is_sorted [ 3; 2; 1 ]);
  assert (not (is_sorted [ 1; 3; 2; 5; 4; 3 ]))

let is_marginal_level_diff report =
  Utils.check_while (fun x y -> abs (x - y) >= 1 && abs (x - y) <= 3) report

let is_safe report = is_sorted report && is_marginal_level_diff report

let is_tolerable report =
  let sub_reports =
    List.mapi (fun i _ -> Utils.list_remove_nth i report) report
  in
  let safety_report_count =
    sub_reports |> List.map is_safe
    |> List.filter (fun x -> x = true)
    |> List.length
  in
  safety_report_count >= 1

let safety_report rs =
  List.map (fun r -> if is_safe r then true else is_tolerable r) rs

(* Tests *)
let () =
  assert (is_marginal_level_diff [ 1; 3; 4; 7 ]);
  assert (not (is_marginal_level_diff [ 7; 3; 4; 7 ]));
  assert (not (is_marginal_level_diff [ 1; 3; 4; 7; 11 ]));
  assert (not (is_marginal_level_diff [ 1; 3; 12; 7; 11 ]));
  assert (is_tolerable [ 8; 6; 4; 4; 1 ]);
  assert (not (is_tolerable [ 1; 2; 7; 2; 9 ]))

let run () =
  let safe_report_count =
    reports |> safety_report |> List.filter (fun x -> x = true) |> List.length
  in
  Printf.printf "Safe reports: %d\n" safe_report_count
