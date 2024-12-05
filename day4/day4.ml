

(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let explode s = Array.init (String.length s) (String.get s)

(* char array array *)
let d =  load input_file
         |> List.filter (fun x -> (String.length x) > 0)
         |> Array.of_list
         |> Array.map explode

let xx = Array.length d.(0)
let yy = Array.length d

(*
   +--------> x
 y |
   |
   V

var d : char array array
e.g    d.(y).(x) so, d.(0).(0) to see top / left character

*)

let xmas = "MAS"            (* the X is checked first *)
let s c = String.make 1 c   (* char -> string         *)

let rr y x =
  if (xx - x) > 3 &&
     (s (d.(y).(x+1)) ^ s (d.(y).(x+2)) ^ s (d.(y).(x+3))) = xmas then 1 else 0

let ll y x =
  if x > 2 &&
     (s (d.(y).(x-1)) ^ s (d.(y).(x-2)) ^ s (d.(y).(x-3))) = xmas then 1 else 0

let dd y x =
  if (yy - y) > 3 &&
     (s (d.(y+1).(x)) ^ s (d.(y+2).(x)) ^ s (d.(y+3).(x))) = xmas then 1 else 0

let uu y x =
  if y > 2 &&
     (s (d.(y-1).(x)) ^ s (d.(y-2).(x)) ^ s (d.(y-3).(x))) = xmas then 1 else 0

let ur y x =
  if (xx - x) > 3 && y > 2 &&
     (s (d.(y-1).(x+1)) ^ s (d.(y-2).(x+2)) ^ s (d.(y-3).(x+3))) = xmas then 1 else 0

let ul y x =
  if x > 2 && y > 2 &&
     (s (d.(y-1).(x-1)) ^ s (d.(y-2).(x-2)) ^ s (d.(y-3).(x-3))) = xmas then 1 else 0

let dr y x =
  if (yy - y) > 3 && (xx - x) > 3 &&
     (s (d.(y+1).(x+1)) ^ s (d.(y+2).(x+2)) ^ s (d.(y+3).(x+3))) = xmas then 1 else 0

let dl y x =
  if (yy - y) > 3 &&  x > 2 &&
     (s (d.(y+1).(x-1)) ^ s (d.(y+2).(x-2)) ^ s (d.(y+3).(x-3))) = xmas then 1 else 0

let p =
  let n = ref 0 in
  for y = 0 to yy - 1 do
    for x = 0 to xx - 1 do
      if d.(y).(x) = 'X' then
        n := !n + (rr y x) + (ll y x) + (dd y x) + (uu  y x) +
             (ur y x) + (ul y x) + (dr y x) + (dl y x)
    done
  done;
  !n

let () = Printf.printf "Part 1 - %i" p

(* 2575 *)
