
(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents

let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
            |> List.map (fun x -> String.split_on_char ' ' x)




(* NOTES -- delete when done



Thinking

Create 2 Hashtables
1 - Input file
2 - Showing result

Load grid in Hashtbl  key is letter in grid

For every letter (key) in Hahtable - iterate through all positions.
If result is within bondries -- add '#' to Hashtable 2.

When done -- count number of '#'s in hashtable 2


*)
