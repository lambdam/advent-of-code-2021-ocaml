(*
#require "base";;
#require "stdio";;
*)

open Base
open Stdio

let data = In_channel.read_lines "../data/day04.txt"

let matrix_size = 5

let numbers = data |> List.hd_exn |> String.split ~on:',' |> List.map ~f:Int.of_string

type board = int array array
type boards = board list
type state = Pending | Reading
type line_acc = {
  numbers : int list;
  state : state;
  int_acc : char list;
}

let read_board_line s =
  let cons_number int_acc numbers =
    int_acc |> List.rev |> String.of_char_list |> Int.of_string |> (fun x -> x :: numbers)
  in
  String.to_list s
  |> List.fold ~init:{numbers=[]; state=Pending; int_acc=[]}
    ~f:begin fun acc c ->
      match acc, c with
      | {state = Pending; _}, ' ' -> acc
      | {state = Pending; int_acc; _}, c -> {acc with state=Reading; int_acc = c :: int_acc}
      | {state = Reading; int_acc; numbers}, ' ' -> {state = Pending;
                                                     numbers = cons_number int_acc numbers;
                                                     int_acc = []}
      | {state=Reading; int_acc; _}, c -> {acc with int_acc = c :: int_acc}
    end
  |> (fun ({int_acc; numbers; _} as acc) ->
      if List.is_empty int_acc
      then acc
      else {acc with numbers = cons_number int_acc numbers; int_acc = []})
  |> (fun {numbers; _} -> List.rev numbers)

(*
read_board_line " 7 96 13 33 85";;
*)

type parsing_acc = {
  boards : board list;
  state : state;
  board_acc : int list list
}

let boards =
  let cons_board board_acc boards =
    board_acc |> List.map ~f:Array.of_list |> List.rev |> Array.of_list |> (fun board -> board :: boards)
  in
  List.tl_exn data
  |> List.fold ~init:{boards=[]; state=Pending; board_acc=[]}
    ~f:begin fun acc elem ->
      match (acc, elem) with
      | {state = Pending; _}, "" -> acc
      | {state = Pending; _}, s -> {acc with board_acc = [read_board_line s];
                                             state = Reading}
      | {state = Reading; board_acc; boards}, "" -> {state = Pending; board_acc = []; boards = cons_board board_acc boards}
      | {state = Reading; board_acc; _}, s -> {acc with board_acc = (read_board_line s) :: board_acc}
    end
  |> (fun ({board_acc; boards; _} as acc) ->
      if List.is_empty board_acc
      then acc
      else {acc with boards = cons_board board_acc boards; board_acc = []})
  |> (fun {boards; _} -> List.rev boards)

let get_line (board : board) line =
  Array.get board line

let get_column (board : board) column =
  Array.map board ~f:(fun line -> Array.get line column)

let is_wining_board board numbers_set =
  let rec is_wining_board' board numbers_set i =
    if i >= matrix_size then false
    else if Set.is_subset (get_line board i |> Set.of_array (module Int))
        ~of_:numbers_set then true
    else if Set.is_subset (get_column board i |> Set.of_array (module Int))
        ~of_:numbers_set then true
    else is_wining_board' board numbers_set (i + 1)
  in
  is_wining_board' board numbers_set 0

let calculate_score board drawn_numbers_set last_number =
  let board_numbers =
    Array.map board ~f:(fun line -> Set.of_array (module Int) line)
    |> Array.fold ~init:(Set.empty (module Int)) ~f:Set.union
  in
  Set.diff board_numbers drawn_numbers_set
  |> Set.fold ~init:0 ~f:(+)
  |> ( * ) last_number
  |> Option.some

type board_traversal = {
  pending : boards;
  wining : boards;
}

let result1 =
  let rec try_number numbers boards drawn_numbers_set =
    match numbers with
    | [] -> None
    | drawn_number :: rest ->
      let drawn_numbers_set' = Set.add drawn_numbers_set drawn_number in
      List.fold boards
        ~init:{pending = []; wining = []}
        ~f:(fun ({wining; _} as acc) board ->
            if is_wining_board board drawn_numbers_set'
            then {acc with wining = board :: wining}
            else acc)
      |> (fun {wining; _} ->
          if List.is_empty wining
          then try_number rest boards drawn_numbers_set'
          else Some (drawn_number, drawn_numbers_set', List.last_exn wining))
  in
  match try_number numbers boards (Set.empty (module Int)) with
  | None -> None
  | Some (drawn_number, drawn_numbers_set, board) ->
    calculate_score board drawn_numbers_set drawn_number

let result2 =
  let rec try_number numbers boards drawn_numbers_set last_number last_board =
    match numbers, boards with
    | [], _ -> None
    | _, [] -> Some (Option.value_exn last_number, drawn_numbers_set, Option.value_exn last_board)
    | drawn_number :: rest, boards ->
      let drawn_numbers_set' = Set.add drawn_numbers_set drawn_number in
      List.fold boards
        ~init:{pending = []; wining = []}
        ~f:(fun ({wining; pending} as acc) board ->
            if is_wining_board board drawn_numbers_set'
            then {acc with wining = board :: wining}
            else {acc with pending = board :: pending})
      |> (fun {wining; pending} ->
          if List.is_empty wining
          then try_number rest boards drawn_numbers_set' (Some drawn_number) last_board
          else try_number rest (List.rev pending) drawn_numbers_set' (Some drawn_number) (List.hd wining))
  in
  match try_number numbers boards (Set.empty (module Int)) None None with
  | None -> None
  | Some (drawn_number, drawn_numbers_set, board) ->
    calculate_score board drawn_numbers_set drawn_number
