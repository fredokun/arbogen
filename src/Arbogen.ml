open Printf

let version_str = "arbogen v0.20121006 (beta)"

let usage = "Usage: arbogen <opt> <specfile>.arb"
let banner = 
"                       v .   ._, |_  .,\n" ^
"                    `-._\\/  .  \\ /    |/_\n" ^
"  ARBOGEN                \\\\  _\\, y | \\//\n" ^
"  °°°°°°°          _\\_.___\\\\, \\\\/ -.\\||\n" ^
"  *Fast*             `7-,--.`._||  / / ,\n" ^
"    Uniform          /'     `-. `./ / |/_.'\n" ^
"      Random                   |    |//\n" ^
"        Tree                   |_    /\n" ^
"         Generator             |-   |\n" ^
"                               |   =|\n" ^
" (C) 2012 F.Peschanski et. al  |    |\n" ^
"------------------------------/ ,  . \\---------- (jg)\n"

let load_file : string option ref = ref None;;
let debug_mode = ref false;;

Arg.parse [
  ("-version", Arg.Unit (fun () -> printf "%s\n%!" version_str ; exit 0),
   "print version information")
]
  (fun arg -> printf "Load file: %s (... not yet implemented...)\n%!" arg ; exit 0)
  usage;
;;

printf "%s\n%!" banner;;

