(*
#require "base";;
#require "stdio";;
*)

open Base
open Stdio

type command =
  | Up of int
  | Down of int
  | Forward of int

exception Invalid_input of string

let data =
  In_channel.read_lines "../data/day02.txt"
  |> List.map ~f:begin fun line ->
    line |> Caml.String.trim |> String.split ~on:' '
    |> (function
        | ["up"; x] -> Up (Int.of_string x)
        | ["down"; x] -> Down (Int.of_string x)
        | ["forward";  x] -> Forward (Int.of_string x)
        | line -> Invalid_input (String.concat ~sep:" " line) |> raise)
  end

(* Part 1 *)

type position_v1 = {
  position: int;
  depth: int
}

let result1 =
  List.fold data
    ~init: { position = 0; depth = 0 }
    ~f:begin fun ({position; depth} as acc) command ->
      match command with
      | Up x -> let new_depth = depth - x in
        {acc with depth = if new_depth < 0 then 0 else new_depth}
      | Down x -> {acc with depth = depth + x}
      | Forward x -> {acc with position = position + x}
    end
  |> (fun {position; depth} -> position * depth)

(* Part 2 *)

type position_v2 = {
  position: int;
  depth: int;
  aim: int
}

let result2 =
  List.fold data
    ~init: { position = 0; depth = 0; aim = 0 }
    ~f:begin fun ({position; depth; aim} as acc) command ->
      match command with
      | Up x -> let new_aim = aim - x in
        {acc with aim = if new_aim < 0 then 0 else new_aim}
      | Down x -> {acc with aim = aim + x}
      | Forward x ->
        {acc with
         position = position + x;
         depth = depth + (aim * x)}
    end
  |> (fun {position; depth; _} -> position * depth)
