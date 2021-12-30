(*
#require "base";;
#require "stdio";;
*)

open Base
open Stdio

let data = In_channel.read_lines "../data/day03.txt"

let number_length = List.nth_exn data 0 |> String.length

type counts = {
  zeros: int;
  ones: int
}

let convert_to_int char_l =
  String.of_char_list char_l |> (fun s -> "0b" ^ s) |> Int.of_string

let result1 =
  let counts_result =
    List.fold data
      ~init:(Array.create ~len:number_length {zeros = 0; ones = 0}) (* Use mutability locally *)
      ~f:begin fun acc x ->
        List.foldi (String.to_list x) ~init:acc
          ~f:(fun index acc char ->
              let {zeros; ones} as counts = Array.get acc index in
              match char with
              | '0' -> Array.set acc index {counts with zeros = zeros + 1}; acc
              | '1' -> Array.set acc index {counts with ones = ones + 1}; acc
              | _ -> failwith "Character not allowed")
      end
    |> Array.to_list in (* Convert to immutable data *)
  let gamma_rate =
    List.map counts_result ~f:(fun {zeros; ones } -> if zeros > ones then '0' else '1')
    |> convert_to_int in
  let epsilon_rate =
    List.map counts_result ~f:(fun {zeros; ones } -> if ones > zeros then '0' else '1')
    |> convert_to_int in
  gamma_rate * epsilon_rate

let result2 =
  let oxygen_criteria count = if count.ones >= count.zeros then '1' else '0' in
  let co2_criteria count = if count.ones >= count.zeros then '0' else '1' in
  let rec find_rating index criteria_fn candidates =
    let length = List.length candidates in
    if length = 1 then
      List.nth_exn candidates 0
      |> Array.to_list
      |> convert_to_int
    else if length < 1 then failwith "No oxygen result"
    else
      let count = List.fold candidates
          ~init:{zeros = 0; ones = 0}
          ~f:(fun ({zeros; ones} as acc) elem ->
              match Array.get elem index with
              | '0' -> {acc with zeros = zeros + 1}
              | '1' -> {acc with ones = ones + 1}
              | _ -> failwith "Character not allowed") in
      List.filter candidates
        ~f:Char.(fun elem -> Array.get elem index = criteria_fn count)
      |> find_rating (index + 1) criteria_fn in
  let data_array = List.map data ~f:String.to_array in
  let oxygen_rating = find_rating 0 oxygen_criteria data_array in
  let co2_rating = find_rating 0 co2_criteria data_array in
  oxygen_rating * co2_rating
