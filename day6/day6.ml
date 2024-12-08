
(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let find h k = Hashtbl.find h k
let replace h k v  = Hashtbl.replace h k v

(* get data and filter out empty lines *)
let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)

(* load in ht : (x:int * y:int, char) Hashtbl *)
let ht = Hashtbl.create 20000

let () = List.iteri (fun iy y ->
    String.iteri (fun ix x -> replace ht (ix, iy) x
                 ) y ) data

(* find start *)
let (start_x, start_y), start_point =
  Hashtbl.fold (fun k v accu ->
      match v with
      | '^' | '<' | '>' | 'v'->  (k, v)
      | _ -> accu
    ) ht ((0,0),' ')

let () =
  let point = ref start_point in
  let x = ref start_x in
  let y = ref start_y in
  let step = ref 0 in
  try
    while true do
      step := !step + 1;
      point := find ht (!x, !y);
      Printf.printf "step: %i - (x: %i y: %i)   %c\n%!" !step !x !y !point;
      replace ht (!x,!y)  'X';
      match !point with
      | '^' ->
        if find ht (!x,!y - 1) = '#'
        then (replace ht (!x + 1,!y) '>'; x := !x + 1;)
        else (replace ht (!x,!y - 1) '^'; y := !y - 1;)
      | 'v'->
        if find ht (!x,!y + 1) = '#'
        then (replace ht (!x - 1,!y) '<'; x := !x - 1;)
        else (replace ht (!x,!y + 1) 'v'; y := !y + 1;)
      | '<' ->
        if find ht (!x - 1, !y) = '#'
        then (replace ht (!x, !y - 1) '^'; y := !y - 1;)
        else (replace ht (!x - 1, !y) '<'; x := !x - 1;)
      | '>' ->
        if find ht (!x + 1, !y) = '#'
        then (replace ht (!x, !y + 1) 'v'; y := !y + 1; )
        else (replace ht (!x + 1,!y)  '>'; x := !x + 1; )
      | _ -> failwith "Invalid state"
    done
  with _ -> ()

let p1 =
  Hashtbl.fold (fun k v accu ->
      match v with
      | 'X' -> accu + 1
      | _ -> accu
    ) ht 0

let () = Printf.printf "part 1 = %i\n" p1

(* part 1 -- 4977  *)
