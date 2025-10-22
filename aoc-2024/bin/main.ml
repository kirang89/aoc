let () =
  match Array.length Sys.argv with
  | 1 -> Printf.printf "%s\n" "Specify the day to run, e.g: 3"
  | _ -> (
      let day = Sys.argv.(1) in
      match day with
      | "1" -> Days.Day01.run ()
      | "2" -> Days.Day02.run ()
      | "3" -> Days.Day03.run ()
      | "4p1" -> Days.Day04.run ()
      | "4p2" -> Days.Day04p2.run ()
      | "5p1" -> Days.Day05p1.run ()
      | "5p2" -> Days.Day05p2.run ()
      | "6" -> Days.Day06.run ()
      | _ -> raise (Invalid_argument "Unknown day value\n"))
