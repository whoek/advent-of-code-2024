
(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
            |> List.map (fun x -> String.split_on_char ' ' x)

let totals =
  List.map (fun x ->
      let hd = List.hd x in
      String.sub hd 0 ((String.length hd) - 1)
      |> int_of_string
    ) data

let values =
  List.map (fun x -> List.tl x
                     |> List.map (fun y -> int_of_string y)
           ) data

let rec check_p1 total vals =
  match vals with
  | v1 :: v2 :: rest ->
    let sum = v1 + v2 in
    let prod = v1 * v2 in
    (if sum <= total then check_p1 total (sum :: rest) else []) @
    (if prod <= total then check_p1 total (prod :: rest) else [])
  | v1 :: []  -> if v1 = total then [v1] else []
  | [] -> []

let () = assert (check_p1 50 [2;5;40] = [50]   )
let () = assert (check_p1 50 [2;50]   = []     )
let () = assert (check_p1  4 [2;2]    = [4; 4] )

let p1 = List.map2 (fun x y ->
    check_p1 x y
  ) totals values
         |> List.map (fun x ->     (* take 1st value else 0 if []  *)
             match x with
             | v1 :: _ -> v1
             | [] -> 0
           )
         |> List.fold_left ( + ) 0

let () = Printf.printf "Part 1 - %i \n%!" p1

(*  Part 1 - 4998764814652      *)

(*  For Part 2 -- simply add the conc line...   *)

let rec check_p2 total vals =
  match vals with
  | v1 :: v2 :: rest ->
    let sum = v1 + v2 in
    let prod = v1 * v2 in
    let conc = int_of_string @@ string_of_int v1 ^ string_of_int v2 in
    (if sum <= total then check_p2 total (sum :: rest) else []) @
    (if prod <= total then check_p2 total (prod :: rest) else []) @
    (if conc <= total then check_p2 total  (conc :: rest) else [])
  | v1 :: []  -> if v1 = total then [v1] else []
  | [] -> []

let p2 = List.map2 (fun x y ->
    check_p2 x y
  ) totals values
         |> List.map (fun x ->     (* take 1st value else 0 if []  *)
             match x with
             | v1 :: _ -> v1
             | [] -> 0
           )
         |> List.fold_left ( + ) 0


let () = Printf.printf "Part 2 - %i \n%!" p2

(* Part 2 - 37598910447546   *)
