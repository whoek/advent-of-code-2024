
let input_file = "sample.txt"
(* let input_file = "input.txt" *)

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents


let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
