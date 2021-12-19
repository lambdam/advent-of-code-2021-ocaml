(*
#require "base";;
#require "stdio";;
#require "ppx_deriving.show";;
*)

open Base
open Stdio

let data =
  In_channel.read_lines "../data/day01.txt"
  |> List.map ~f:Int.of_string

(* Part 1 *)

type evolution_count = {
  previous: int option ;
  inc: int;
  dec: int;
  stag: int }
[@@deriving show]

let result1 =
  List.fold data
    ~init:{ previous = None; inc = 0; dec = 0; stag = 0 }
    ~f:begin fun ({ previous; inc; dec; stag } as acc) x ->
      match previous with
      | None -> { acc with previous = Some x }
      | Some prev_x -> let acc' = { acc with previous = Some x; } in
        if x > prev_x then { acc' with inc = Int.succ inc }
        else if x < prev_x then { acc' with dec = Int.succ dec }
        else { acc' with stag = Int.succ stag}
    end
  |> (fun x -> x.inc)

(* Part 2 *)

type window_count = {
  prev3: int option;
  prev2: int option;
  prev1: int option;
  inc: int;
  dec: int;
  stag: int
}
[@@deriving show]

let result2 =
  List.fold data
    ~init: { prev1 = None; prev2 = None; prev3 = None; inc = 0; dec = 0; stag = 0 }
    ~f:begin fun ({ prev1; prev2; prev3; inc; dec; stag } as acc) current_value ->
      match (prev1, prev2, prev3) with
      | None, _, _ -> { acc with prev1 = Some current_value }
      | Some _, None, _ -> { acc with prev2 = prev1; prev1 = Some current_value }
      | Some _, Some _, None -> { acc with prev3 = prev2; prev2 = prev1; prev1 = Some current_value }
      | Some x, Some y, Some z ->
        let prev_sum = x + y + z in
        let current_sum = current_value + x + y in
        let new_acc = { acc with
                        prev3 = prev2;
                        prev2 = prev1;
                        prev1 = Some current_value } in
        if prev_sum < current_sum then { new_acc with inc = Int.succ inc }
        else if prev_sum > current_sum then { new_acc with dec = Int.succ dec }
        else { new_acc with stag = Int.succ stag }
    end
  |> (fun x -> x.inc)
