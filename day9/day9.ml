
(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all
  in
  (* String.split_on_char '\n' contents *)
  contents

let explode s = List.init (String.length s) (String.get s)

let () = assert (explode "123" = ['1'; '2'; '3'])

let rec pair l =
  match l with
  | v1 :: v2 :: rest -> (v1, v2) :: pair rest
  | v1 :: [] -> [(v1, 0)]
  | [] -> []

let () = assert (pair [1; 2; 3; 4] =  [(1, 2); (3, 4)])
let () = assert (pair [1; 2; 3; 4; 5] =  [(1, 2); (3, 4); (5, 0)])


let dense =  load input_file
                |> String.trim
                |> explode
                |> List.map (fun x -> int_of_char x - 48)
                |> pair
                |> List.mapi (fun i x -> i mod 10, x)   (* allocate ID *)


let id_files (id_no, (file_count, space_count)) =
  List.init file_count (fun _ -> Some id_no  )
  @   List.init space_count (fun _ -> None )

let expand_disk = dense
                  |> List.map id_files
                  |> List.concat

let to_move_qty = List.filter (fun x -> x <> None) expand_disk

let to_move = expand_disk
  |> List.filter (fun x -> x <> None)
  |> List.rev


(*

expand disk
   44955 with '.'
   49767 with a number
 = 94722 TOTAL

All the numbers must be in first 49767 elements

*)
