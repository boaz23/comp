
module type COMP_CONFIG = sig
  val debug: bool
end;;

module CompConfig : COMP_CONFIG = struct

let debug = false;;

end;;
