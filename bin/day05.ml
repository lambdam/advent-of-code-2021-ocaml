(*
#require "base";;
#require "stdio";;
#require "re"
#require "ppx_deriving.show";;
*)

open Base
open Stdio

type entry = {x1: int; y1: int; x2: int; y2: int;}

let data =
  let arrow_re = " -> " |> Re.Perl.re |> Re.compile in
  let comma_re = "," |> Re.Perl.re |> Re.compile in
  let to_xy s = Re.split comma_re s |> List.map ~f:Int.of_string in
  In_channel.read_lines "../data/day05.txt"
  |> List.map ~f:(Re.split arrow_re)
  |> List.map ~f:begin function
    | [x1y1; x2y2] -> begin
        match (to_xy x1y1, to_xy x2y2) with
        | [x1; y1], [x2; y2] -> {x1; y1; x2; y2}
        | _ -> failwith "Wrong entry format" end
    | _ -> failwith "Cannot construct entry" end
