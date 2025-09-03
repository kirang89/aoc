let () =
  match Array.length Sys.argv with
  | 1 -> Printf.printf "%s\n" "Specify the day to run, e.g: 3"
  | _ -> (
      let day = Sys.argv.(1) in
      match day with
      | "1" -> Days.Day01.run ()
      | _ -> raise (Invalid_argument "Unknown day value\n"))
