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

let result1 =
  let rec try_number numbers boards drawn_numbers_set =
    match numbers with
    | [] -> None
    | drawn_number :: rest ->
      let drawn_numbers_set' = Set.add drawn_numbers_set drawn_number in
      let rec test_line_and_col boards' i =
        if i >= matrix_size
        then try_number rest boards drawn_numbers_set'
        else
          match boards' with
          | [] -> test_line_and_col boards (i + 1)
          | board :: rest_boards ->
            if Set.is_subset (get_line board i |> Set.of_array (module Int))
                ~of_:drawn_numbers_set'
            then Some (drawn_number, drawn_numbers_set', board)
            else if Set.is_subset (get_column board i |> Set.of_array (module Int))
                ~of_:drawn_numbers_set'
            then Some (drawn_number, drawn_numbers_set', board)
            else test_line_and_col rest_boards i
      in
      test_line_and_col boards 0
  in
  match try_number numbers boards (Set.empty (module Int)) with
  | None -> None
  | Some (drawn_number, drawn_numbers_set,  board) ->
    let board_numbers =
      Array.map board ~f:(fun line -> Set.of_array (module Int) line)
      |> Array.fold ~init:(Set.empty (module Int)) ~f:Set.union
    in
    Set.diff board_numbers drawn_numbers_set
    |> Set.fold ~init:0 ~f:(+)
    |> ( * ) drawn_number
    |> Option.some
