(*********************************************************
 * Arbogen-lib : fast uniform random generation of trees *
 *********************************************************
 * Module: ParseUtil                                     *
 * -------                                               *
 * Options Parser                                        *
 * -------                                               *
 * (C) 2011, Xuming Zhan, Frederic Peschanski            *
 *           Antonine Genitrini, Matthieu Dien           *
 *           Marwan Ghanem                               *
 *           under the                                   *
 *           GNU GPL v.3 licence (cf. LICENSE file)      *
 *********************************************************)


(** Specification parsing and configuration options management *)


(** {2 Configuration options management} *)

module Options = Options
(** Configuration options available on the command line of the executable, and
    in specification files in the form of `set option value` directives. *)


(** {2 Parsing functions} *)

(** Parse a specification, possibly preceded by configuration options, given via
    an stdlib input channel. *)
let parse_from_channel chan =
  let lexbuf = Lexing.from_channel chan in
  let options, parsetree = Parser.start Lexer.token lexbuf in
  options, ParseTree.to_grammar parsetree

(** Parse a specification, possibly preceded by configuration options, given via
    its file name. *)
let parse_from_file filename =
  let chan = open_in filename in
  let res = parse_from_channel chan in
  close_in chan;
  res
