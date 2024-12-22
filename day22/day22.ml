(* let input_file = "sample.txt" *)
let input_file = "input.txt"

let load file =
  let contents = In_channel.with_open_bin file
      In_channel.input_all in
  String.split_on_char '\n' contents


let data =  load input_file
            |> List.filter (fun x -> (String.length x) > 0)
            |> List.map int_of_string

let mix ~secret ~n = secret lxor n
let prune secret = secret mod 16777216

let next secret =
  let calc1 = mix ~n:(secret * 64) ~secret:secret |> prune in
  let calc2 = mix ~n:(calc1 / 32) ~secret:calc1   |> prune in
  let calc3 = mix ~n:(calc2 * 2048) ~secret:calc2 |> prune
  in calc3

(* https://stackoverflow.com/questions/15285386/  *)
let rec foldi i f acc =
  if i <= 0 then acc else foldi (pred i) f (f acc)

let part1 = List.fold_left (fun acc x ->
    acc + foldi 2000 next x) 0 data

let () = Printf.printf "Part 1 - %i\n" part1

(* 16999668565 *)
