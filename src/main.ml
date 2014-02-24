open Format
open Lexing

(*
Parse command line and open files
*)

let ifile = ref ""
let ofile = ref "out.bc"
let split = ref false

let options =
  ["-o", Arg.String ((:=) ofile), " <file> Set output file name";
   "-s", Arg.Set split, "split reactions with more than 2 reactants"]

let usage = "usage: biocomp [options] [file]"

let locate_error pos =
  let l = pos.pos_lnum in
  let c = pos.pos_cnum - pos.pos_bol + 1 in
  printf "File \"%s\", line %d, characters %d-%d:@." !ifile l (c-1) c

let () =
  Arg.parse options ((:=) ifile) usage;
  let f =
    if !ifile = "" then stdin
    else open_in !ifile
  in
(*
Lexing and parsing of input
*)
  let buf = Lexing.from_channel f in
  try
    let source = Opt.precompile (Parser.source Lexer.token buf) in
    close_in f;
    let inlined = Opt.flatten (Opt.inline_source Opt.stdlib Opt.Env.empty source) in
    let with_alive, _ = Opt.alive (Opt.base_vars (Opt.S.add "__unit" Opt.S.empty) source) inlined in
    let reactions_with_conditions, _ = Gen.make_reactions Gen.RC.empty [[]] with_alive in
    (*let buffered_reactions = Gen.make_all_buffers reactions_with_conditions in
    let reactions = Gen.remove_all_conditions buffered_reactions in*)
    let reactions = Gen.remove_all_conditions reactions_with_conditions in
    if !split then
      let splitted_reactions = Gen.split_all_reactions reactions in
      Gen.print_all_reactions splitted_reactions
    else
      Gen.print_all_reactions reactions
(*
Process errors
*)
  with
  | Lexer.Bad_character c ->
    locate_error (Lexing.lexeme_start_p buf);
    printf "Illegal character: %c@." c;
    exit 1
  | Parser.Error ->
    locate_error (Lexing.lexeme_start_p buf);
    printf "Parse error@.";
    exit 1
