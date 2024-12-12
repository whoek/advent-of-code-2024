
let input_file = "sample.txt"
(* let input_file = "input.txt" *)

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents


let find h k = Hashtbl.find h k
let replace h k v  = Hashtbl.replace h k v
let mem h k = Hashtbl.mem h k

let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)

(* load in ht : (x:int * y:int, char) Hashtbl *)
let ht = Hashtbl.create 20000

let () = List.iteri (fun iy y ->
    String.iteri (fun ix x -> replace ht (ix, iy) x
                 ) y ) data

(* size : char , (area: int, peri: int) *)
let size = Hashtbl.create 30

let plus_one_area (area, peri) = (area + 1, peri)


let calc_area_peri =
  let max_y = List.length data in
  let max_x = List.hd data |> String.length in
  for x = 0 to max_x - 1 do
    for y = 0 to max_y  - 1 do
      let cell = find ht (x, y) in
      (* add area *)
      if mem size cell
      then replace size cell (find size cell |> plus_one_area)
      else replace size cell  (1,0);

      if x = 0 then ()
      else if x = max_x then ()
      else ();

      if y = 0 then ()
      else if y = max_y then ()
      else ();

    done
  done



(* day 12


*)
