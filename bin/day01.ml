(*
#require "base";;
#require "stdio";;
#require "ppx_deriving.show";;
*)

open Base
open Stdio

type evolution_count = {
  previous: int option ;
  inc: int;
  dec: int;
  stag: int }
[@@deriving show]

let data =
  In_channel.read_lines "../data/day01.txt"
  |> List.map ~f:Int.of_string

let result1 =
  List.fold data
    ~init:{ previous = None; inc = 0; dec = 0; stag = 0 }
    ~f:begin fun acc x -> let { previous; inc; dec; stag } = acc in
      match previous with
      | None -> { acc with previous = Some x }
      | Some prev_x -> let acc' = { acc with previous = Some x; } in
        if x > prev_x then { acc' with inc = Int.succ inc }
        else if x < prev_x then { acc' with dec = Int.succ dec }
        else { acc' with stag = Int.succ stag}
    end
  |> (fun x -> x.inc)
