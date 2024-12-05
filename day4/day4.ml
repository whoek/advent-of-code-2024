

let input_file = "sample.txt"
(* let input_file = "input.txt" *)

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents


let explode s = Array.init (String.length s) (String.get s)

(* char array array *)
let d =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
            |> Array.of_list
            |> Array.map (fun x -> explode x)



(* scan through all the rows collumsn  *)

(* only check if char is X *)


let check_u x y = if y < 3 &&  d.(x).(y) = 'M' && d.(x).(y) = 'A' && d.(x).(y) = 'S' then 1 else 0
let check_d x y = 1
let check_r x y = 1
let check_l x y = 1
let check_ur x y = 1
let check_ul x y = 1
let check_dr x y = 1
let check_dl x y = 1


(*  NOTES



*)
