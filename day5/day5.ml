
(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let d =  load input_file
         |> List.filter (fun x -> (String.length x) > 0)

let _ = assert (List.nth d 100 = "52|15")
let _ = assert (List.nth d 1200 = "75,58,48,42,29,78,66,55,83,87,94")

(* string list list *)
let rules = d
            |> List.map (String.split_on_char '|')
            |> List.filter (fun x -> (List.length x) > 1)

(* string list list *)
let updates = d
              |> List.map (String.split_on_char ',')
              |> List.filter (fun x -> (List.length x) > 1)


let find_dest x =
  List.filter (fun y -> List.hd y = x) rules

let rec check_x_to_y main   = match lst with
  | x :: y :: _ when x = y -> true
  | _ -> begin
      let targets = find_dest lst in
      let
    end




let rec check_pages lst = match lst with
  | x :: y :: tl -> begin if check_x_to_y [x; y] then check_pages @@ y :: tl else false end
  | x :: [] -> true
  | [] -> failwith "not possible"

let p1 = List.filter check_pages updates
         |> List.length

let () = Printf.printf "Part 1 - %i" p1



{* PART 1 IN PRORESS *)
{* PART 1 IN PRORESS *)
{* PART 1 IN PRORESS *)
{* PART 1 IN PRORESS *)

(*

DAG of sample set

https://edotor.net/?engine=dot#digraph%20finite_state_machine%20%7B%0A%0A%0Aa47%20-%3E%20a53%0Aa97%20-%3E%20a13%0Aa97%20-%3E%20a61%0Aa97%20-%3E%20a47%0Aa75%20-%3E%20a29%0Aa61%20-%3E%20a13%0Aa75%20-%3E%20a53%0Aa29%20-%3E%20a13%0Aa97%20-%3E%20a29%0Aa53%20-%3E%20a29%0Aa61%20-%3E%20a53%0Aa97%20-%3E%20a53%0Aa61%20-%3E%20a29%0Aa47%20-%3E%20a13%0Aa75%20-%3E%20a47%0Aa97%20-%3E%20a75%0Aa47%20-%3E%20a61%0Aa75%20-%3E%20a61%0Aa47%20-%3E%20a29%0Aa75%20-%3E%20a13%0Aa53%20-%3E%20a13%0A%0A%0A%0A%7D%0A

for every

*)
