
let input_file = "sample.txt"
(* let input_file = "input.txt" *)

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents


let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
            |> List.map (String.split_on_char ',')
            |> List.map (List.map int_of_string)

type cell = Normal | Occupied

let y_max = 8
let x_max = y_max

(* true = open    false = obstacle *)

let board = Array.make_matrix x_max y_max Normal
let from  = Array.make_matrix x_max y_max (max_int, max_int)
let visited = Array.make_matrix x_max y_max false
let g = Array.make_matrix x_max y_max max_int
let h = Array.make_matrix x_max y_max max_int



let () = List.iter (fun z ->
    let x = List.nth z 0 in
    let y = List.nth z 1 in
    board.(x).(y) <- Occupied
  ) data

let find_path (x, y) =
  let neighbours (x, y) =
    [(0, 1); (0,-1); (1,0); (-1,0)]
    |> List.map (fun (x', y') -> (x + x', y + y'))
    |> List.filter (fun (x, y) ->
        x >= 0 && x < x_max && y >= 0 && y < y_max)
    |> List.filter (fun (x, y) -> board.(x).(y) = Normal)
    |> List.filter (fun (x, y) -> not visited.(x).(y))
  in
  let h (x0, y0) (x1, y1) =  abs (x0 - x1) + abs (y0 - y1)
  in
  let () = Printf.printf "\ndone %i\n %!"   (List.length @@ neighbours (0,0))



(*

https://github.com/veeenu/adventofcode2024/blob/main/day18.ml

*)
