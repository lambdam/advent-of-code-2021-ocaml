(*
#require "base";;
#require "stdio";;
*)

open Base
open Stdio

let data = In_channel.read_lines "../data/day03.txt"

let number_length =
  List.nth_exn data 0
  |> String.length

exception Unknown_state of string

type counts = {
  zeros: int;
  ones: int
}

let result1 =
  let counts_result =
    (* Use mutability locally *)
    List.fold data
      ~init:(Array.create ~len:number_length {zeros = 0; ones = 0})
      ~f:begin fun acc x ->
        List.foldi (String.to_list x) ~init:acc
          ~f:(fun index acc char ->
              let {zeros; ones} as counts = Array.get acc index in
              match char with
              | '0' -> Array.set acc index {counts with zeros = zeros + 1}; acc
              | '1' -> Array.set acc index {counts with ones = ones + 1}; acc
              | _ -> raise @@ Unknown_state "Character not allowed")
      end
    (* Convert to immutable data *)
    |> Array.to_list in
  let convert_to_int char_l =
    String.of_char_list char_l
    |> (fun s -> "0b" ^ s)
    |> Int.of_string in
  let gamma_rate =
    List.map counts_result
      ~f:(fun {zeros; ones } -> if zeros > ones then '0' else '1')
    |> convert_to_int in
  let epsilon_rate =
    List.map counts_result
      ~f:(fun {zeros; ones } -> if ones > zeros then '0' else '1')
    |> convert_to_int
  in
  gamma_rate * epsilon_rate
