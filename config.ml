
module type COMP_CONFIG = sig
  val debug : bool
end;;

module CompConfig : COMP_CONFIG = struct

(* set the debug flag if present
   so that they will see the flag *)
(* it has to be defined here because each use seemse
   to import a new module instance each time and thus
   other modules do not see the change in the flag *)
let debug = Array.mem "-d" Sys.argv;;
if debug then begin
  Printf.printf "; debug flag is on\n";
end;

end;;
