let input_file = "sample.txt"
(* let input_file = "input.txt" *)

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
            |> List.map (fun x -> String.split_on_char '-' x)

(* 3380 pairs -- 520 unique names *)
let unique_data =
  data
  |> List.flatten
  |> List.sort_uniq compare

(* Helper functions  *)
let find tbl k = Hashtbl.find tbl k
let add tbl k v= Hashtbl.add tbl k v
let find_all tbl k = Hashtbl.find_all tbl k
let remove tbl k =  Hashtbl.remove tbl k        (* remove current binding  *)
let replace tbl k v = Hashtbl.replace tbl k v   (* replace current binding *)
let rec remove_all tbl k =                      (* remove all bindings     *)
  if Hashtbl.mem tbl k then begin
    Hashtbl.remove tbl k;
    remove_all tbl k
  end

(* create pairs --[ (a,b); (b,a); ..] *)
let pairs =
  List.map (fun x ->
      [(List.nth x 0, List.nth x 1);
       (List.nth x 1, List.nth x 0)]
) data
  |> List.flatten

let trip_sort (a, b, c) =
  if a < b && b < c then (a,b,c)
  else if a < c && c < b then (a, c, b)
  else if b < c && c < a then (b, c, a)
  else if b < a && a < c then (b, a, c)
  else if c < a && a < b then (c, a, b)
  else if c < b && b < a then (c, b, a)
  else failwith "impossible state"

let pairs_with_t =
  pairs
  |> List.filter (fun (x,y) -> x.[0] = 't' || y.[0] = 't')

let () = Printf.printf "length pairs: %i \n%!" (List.length pairs)
let () = Printf.printf "length pairs_with_t: %i \n%!" (List.length pairs_with_t)

let part1 =
  let ret = ref [] in
  List.iter (fun (x1, y1) ->
      List.iter (fun (x2, y2) ->
          List.iter (fun (x3, y3) ->
              if x1 = y3 && y1 = x2 && y2 = x3
                 && x1 <> x2 && x2 <> x3
              then ret := trip_sort (x1, x2, x3) :: !ret
              else ()
            ) pairs
        ) pairs_with_t
    ) pairs_with_t;
  !ret
  |> List.sort_uniq compare


let () = Printf.printf "Part 1 - %i \n%!" (List.length part1)

(* Part 1
length pairs: 6760
length pairs_with_t: 604
Part 1 answer: 1437
runtime: 0m15.458s
*)

let match2 (a,b,c) (x,y,z) =
  (
    (if a = x || a = y || a = z then 1 else 0) +
    (if b = x || b = y || b = z then 1 else 0) +
    (if c = x || c = y || c = z then 1 else 0)
  ) = 2


let all_linked (a,b,c) (x,y,z) =
  List.exists (fun (m,n) -> m = a && n = x) pairs &&
  List.exists (fun (m,n) -> m = a && n = y) pairs &&
  List.exists (fun (m,n) -> m = a && n = z) pairs &&
  List.exists (fun (m,n) -> m = b && n = x) pairs &&
  List.exists (fun (m,n) -> m = b && n = y) pairs &&
  List.exists (fun (m,n) -> m = b && n = z) pairs &&
  List.exists (fun (m,n) -> m = c && n = x) pairs &&
  List.exists (fun (m,n) -> m = c && n = y) pairs &&
  List.exists (fun (m,n) -> m = c && n = z) pairs




let list_of_trip (a,b,c) = [a;b;c]

let part2 =
  List.map (fun x -> (
        x :: List.map (fun y ->
            (if all_linked x y then y else ("","",""))
          ) part1
        |> List.filter (fun (a,b,c) -> a <> "")
      )
    ) part1
  |> List.filter (fun x -> x <> [])
  |> List.map (fun x ->
      List.map (fun y ->
          list_of_trip y
        ) x
      |> List.flatten
      |> List.sort_uniq compare
    )
  |> List.sort_uniq compare



(* NOT DONE YET :)    *)
