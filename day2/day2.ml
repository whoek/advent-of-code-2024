(****** START of helper functions ******)

(* read file into a string list - every line is an element *)
let lines f =
  let contents = In_channel.with_open_bin f
      In_channel.input_all in
  String.split_on_char '\n' contents

(****** END of helper functions ******)


(* let file = "test.txt" *)
let file = "input.txt"

(* get data and filter out empty lines -- e.g. last one might be *)
let data =  lines file
            |> List.filter (fun x -> (String.length x) > 0)
            |> List.map (fun x -> String.split_on_char ' ' x
                                  |> List.map int_of_string)

let rec delta lst = match lst with
  | x1 :: x2 :: x3 -> (x2 - x1) :: delta (x2 :: x3)
  | x1 -> []

let report_safe lst =
  List.for_all (fun x -> x > 0 && x <= 3) lst ||
  List.for_all (fun x -> x < 0 && x >= -3) lst


let () =
  let safe_lines =
    data
    |> List.map delta
    |> List.filter report_safe
  in
  Printf.printf "Part 1 - sum of safe reports:  %i\n" @@ List.length safe_lines


(* PART 1 = 549  *)


(*  Approach followed for Part two

Where a report is Unsafe - remove a level one-by-one to see if it is Safe
If a Safe record is found, use that as Damped report

*)


let copy lst =
  List.init (List.length lst) (fun _ -> lst)

let () = assert (copy [1;2] = [[1;2];[1;2]])

let remove_level n lst =
  List.filteri (fun i _ -> i <> n) lst

let () = assert (remove_level 1 [1;2;3;4] = [1;3;4])

(* remove a different element from list of list *)
let remove_report lst =
  List.mapi (fun i x -> remove_level i x) lst

let apply_damp rep =
  if report_safe rep then rep
  else
    let best_report =
      rep
      |> copy |> remove_report
      |> List.find_opt (fun x -> report_safe @@ delta x) in
    match best_report  with
    | Some x -> x      (* Safe solution found *)
    | None -> rep      (* No Safe damping repor, keep original report *)

let () =
  let safe_lines =
    data
    |> List.map apply_damp
    |> List.map delta
    |> List.filter report_safe
  in
  Printf.printf "Part 2 - sum of safe DAMPED reports:  %i\n" @@ List.length safe_lines


(*

# ocaml day2.ml

Part 1 - sum of safe reports:  549
Part 2 - sum of safe DAMPED reports:  589

*)
