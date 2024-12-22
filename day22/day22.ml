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
    acc + foldi 2000 next x
  ) 0 data

let () = Printf.printf "Part 1 - %i\n" part1

(* 16999668565 *)

(* part 2 *)

let prices_2000 i =
  let rec prices secret n =
    let secret_price = secret mod 10 in
    match n with
    | 0 -> []
    | _ -> secret_price :: prices (next secret) (n - 1)
  in prices i 2000

(* prices_2000 123 -> [3; 0; 6; 5; 4; 4; 6; 4; 4; ...] *)

let rec seq lst =
  match lst with
  | e1 :: e2 :: e3 :: e4 :: e5 :: tl -> begin
      let seq_str = string_of_int (e2 - e1) ^
          string_of_int (e3 - e2) ^
          string_of_int (e4 - e3) ^
          string_of_int (e5 - e4) in
      (e5, seq_str) :: seq (e2 :: e3 :: e4 :: e5 :: tl)
    end
    | _ -> []


let part2 = List.map (fun x ->
    prices_2000 x
    |> seq
  ) data

(* get unique sequences *)
let uniq_seq  =
  List.map (fun x ->
      List.map (fun (p, s) -> s
        ) x
    ) part2
  |> List.flatten
  |> List.sort_uniq compare
  |> List.length

(* 40951 unique 4 number sequinces  *)
