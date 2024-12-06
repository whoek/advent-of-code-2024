
(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

(* get data and filter out empty lines *)
let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)

(* data : string list *)
let ht = Hashtbl.create 20000

(* load in Hashtable *)
let () = List.iteri (fun iy y ->
    String.iteri (fun ix x -> Hashtbl.replace ht (ix, iy) x
                 ) y ) data

let Some ((start_x,start_y), start_direction)   =
  Hashtbl.fold (fun k v accu ->
      match v with
      | '^' | '<' | '>' | 'v'->  Some (k, v)
      | _ -> accu
    ) ht None

let x = ref start_x
let y = ref start_y
let direction = ref start_direction


(*
let () =
  match (!direction) with
  | '^' -> begin
      if ABAOVE != 'hash' then  make current X and move curser up
else if ABOVE = # then go right
 x< new
y new
    end
  | '<' ->  begin  end
  | '>' -> begin  end
  | 'v'->  begin  end
  | _ -> failwith "Invalid state"

*)

(*
let () = Hashtbl.add ht 1 "one "
let () = Hashtbl.replace ht 2 "two"    (* remove + add *)

let a = Hashtbl.find ht 1
let b = Hashtbl.find ht 2

*)






(* find starting point *)

(* check for obst -- then advance *)
