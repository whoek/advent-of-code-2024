
(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
            (* |> List.map (fun x -> String.split_on_char '\n' x) *)


let find h k = Hashtbl.find h k
let replace h k v  = Hashtbl.replace h k v

let input = Hashtbl.create 20000

let () = List.hd data
         |> String.iteri (fun i x ->  replace input i @@ (int_of_char x) - 48 )

let output = Hashtbl.create 100_000






(* NOTES -- delete when done

12345

0..111....22222


file_length
free_length

number of digits = 19_999



start with id 0
file / space / file / space etc

move fields -- 1-by-1 to LEFT

calc checksum
SUM OF (value x position )


54707 .... 61433

*)
