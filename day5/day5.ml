
(* let input file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let d =  load input_file
         |> List.filter (fun x -> (String.length x) > 0)

let _ = assert (List.nth d 100 = "52|15")
let _ = assert (List.nth d 1200 = "75,58,48,42,29,78,66,55,83,87,94")

let mkpair l =
  match l with
  | a :: b :: _ -> (a, b)
  | [] | [_] -> failwith "Invalid state"

(* string list list *)
let rules = d
            |> List.map (String.split_on_char '|')
            |> List.filter (fun x -> (List.length x) > 1)
            |> List.map mkpair

(* string list list *)
let pages = d
              |> List.map (String.split_on_char ',')
              |> List.filter (fun x -> (List.length x) > 1)


let find_rules (u, _) =
  List.filter (fun (x, y) -> x = u) rules

let rec check_from_to acc (u, v)=
  if acc > 10 then false
  else
  if u = v then true     (* reached destination *)
  else
    let targets = find_rules (u, v) in
    let () = Printf.printf "check_from_to: %s - %s - len: %i acc: %i\n%!"
        u v (List.length targets) acc in
    List.exists (check_from_to (acc + 1))  targets

let rec check_pages lst =
match lst with
  | x :: y :: rest -> begin
      Printf.printf "check_pages: %s - %s\n%!" x y;
      if check_from_to 0 (x, y)
      then check_pages @@ y :: rest
      else false
    end
  | x :: [] -> true
  | [] -> failwith "not possible"



let p1 = pages
         |> List.filter check_pages
         |> List.length

let () = Printf.printf "Part 1 - %i" p1




(*

DAG of sample set

https://edotor.net/?engine=dot#digraph%20finite_state_machine%20%7B%0A%0A%0Aa47%20-%3E%20a53%0Aa97%20-%3E%20a13%0Aa97%20-%3E%20a61%0Aa97%20-%3E%20a47%0Aa75%20-%3E%20a29%0Aa61%20-%3E%20a13%0Aa75%20-%3E%20a53%0Aa29%20-%3E%20a13%0Aa97%20-%3E%20a29%0Aa53%20-%3E%20a29%0Aa61%20-%3E%20a53%0Aa97%20-%3E%20a53%0Aa61%20-%3E%20a29%0Aa47%20-%3E%20a13%0Aa75%20-%3E%20a47%0Aa97%20-%3E%20a75%0Aa47%20-%3E%20a61%0Aa75%20-%3E%20a61%0Aa47%20-%3E%20a29%0Aa75%20-%3E%20a13%0Aa53%20-%3E%20a13%0A%0A%0A%0A%7D%0A

for every

*)
