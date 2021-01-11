
module type COMP_CONFIG = sig
  val debug : bool
  val use_tp : bool
end;;

module CompConfig : COMP_CONFIG = struct

(* set the debug flag if present
   so that they will see the flag *)
(* it has to be defined here because each '#use' seems
   to import a new module instance each time and thus
   other modules do not see the change in the flag *)
let debug = Array.mem "-d" Sys.argv;;
if debug then begin
  Printf.printf "; DEBUG flag is ON\n";
end

let use_tp = not (Array.mem "--no-tp" Sys.argv);;
if not use_tp then begin
  Printf.printf "; TAIL CALL optimization is OFF\n";
end

end;;
