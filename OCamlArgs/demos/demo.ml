open Format
open Ocamlargs_lib
module Interpret = Interpret.InterpretResult
open Result

let () =
  match Parser.parse_program Stdio.In_channel.(input_all stdin) with
  | Error err -> printf "Error: %s\n%!" err
  | Ok p ->
    (match Inference.infer_program p with
     | Error err -> printf "Error: %a\n%!" Inference.pp_error err
     | Ok p ->
       (match Interpret.run p with
        | Error err -> printf "Error: %a\n%!" Interpret.pp_r_err err
        | Ok ok -> printf "%a\n%!" Interpret.pp_r_ok ok))
;;
