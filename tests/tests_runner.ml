let tc_fg_bright_red     = "\027[91m";;
let tc_fg_bright_green   = "\027[92m";;
let tc_fg_bright_yellow  = "\027[93m";;
let tc_fg_bright_blue    = "\027[94m";;
let tc_fg_bright_magenta = "\027[95m";;
let tc_fg_bright_cyan    = "\027[96m";;
let tc_fg_end = "\027[0m";;

let print_in_color color str = Printf.printf "%s%s%s\n" color str tc_fg_end;;

let string_ends_with suffix str =
  let str_len = String.length str in
  let suffix_len = String.length suffix in
  if suffix_len > str_len then false
  else let str_suffix = String.sub str (str_len - suffix_len) suffix_len in
  suffix = str_suffix;;

let str_sub_from_last_char_occur char string =
  let string_len = String.length string in
  let last_char_index_opt = String.rindex_from_opt string (string_len - 1) char in
  match last_char_index_opt with
  | Some last_char_index ->
    let start_index = last_char_index + 1 in
    let sub_len = (string_len - last_char_index - 1) in
    String.sub string start_index sub_len
  | None -> string;;

let str_sub_up_to_last_char_occur char string =
  let string_len = String.length string in
  let last_char_index_opt = String.rindex_from_opt string (string_len - 1) char in
  match last_char_index_opt with
  | Some last_char_index -> String.sub string 0 last_char_index
  | None -> string;;

let file_dir = str_sub_up_to_last_char_occur '/';;
let file_name_from_path = str_sub_from_last_char_occur '/';;
let file_name_without_ext = str_sub_up_to_last_char_occur '.';;
let file_ext = str_sub_from_last_char_occur '.';;
let has_file_ext = string_ends_with;;

let combine_path dir path =
  let dir =
    if not (string_ends_with dir "/") then
      dir ^ "/"
    else dir in
  dir ^ path;;


let get_dir_entries dir_path =
  let entries_names_arr = Sys.readdir dir_path in
  let entries_names_list = Array.to_list entries_names_arr in
  let entries_paths = List.map (fun entry_name -> combine_path dir_path entry_name) entries_names_list in
  List.partition (fun entry_path -> not (Sys.is_directory entry_path)) entries_paths;;

let find_test_files_in_dir dir_path =
  let (files, dirs) = get_dir_entries dir_path in
  List.filter (has_file_ext ".scm") files;;

let read_file file_path =
  let ic = open_in file_path in
  let s = really_input_string ic (in_channel_length ic) in
  close_in ic;
  s;;

let read_file_trimmed file_path =
  let s = read_file file_path in
  String.trim s;;

let write_to_file file_path str =
  let file_out = open_out file_path in
  Printf.fprintf file_out "%s" str;
  flush file_out;
  close_out file_out;;

let copy_file_to_dir file_path dir_path =
  Sys.command (Printf.sprintf "cp %s %s" file_path dir_path);;

let move_file_to_dir file_path dir_path =
  let file_name = file_name_from_path file_path in
  let new_path = combine_path dir_path file_name in
  Sys.rename file_path new_path;;

let remove_file = Sys.remove;;

let rec execute_commands = function
  | [] -> true
  | command :: rest ->
    let result = command () in
    if result then execute_commands rest
    else false;;

let run_test temp_dir test_file =
  let file_name = file_name_from_path test_file in
  let file_name_no_ext = file_name_without_ext file_name in
  let file_path_no_ext = file_name_without_ext test_file in
  let temp_file_no_ext = combine_path temp_dir file_name_no_ext in
  let test_name = file_name in

  let exe_path = temp_file_no_ext in
  let cmp_results_exp_file = temp_file_no_ext ^ ".cmp_command.scm" in
  let out_file = file_path_no_ext ^ ".actual" in
  let scheme_out_file = file_path_no_ext ^ ".scm.actual" in
  let result_file = file_path_no_ext ^ ".result" in

  let compile_test_file () =
    let status = Sys.command ("make -f Makefile " ^ test_file) in
    if status != 0 then begin
      print_in_color tc_fg_bright_red ("Compilition failed on test: " ^ test_name);
      false
    end else
      true in

  let move_compilation_files_to_temp_dir () =
    move_file_to_dir file_name_no_ext temp_dir;
    move_file_to_dir (file_path_no_ext ^ ".s") temp_dir;
    move_file_to_dir (file_path_no_ext ^ ".o") temp_dir;
    true in

  let execute_compiler () =
    let execution_status = Sys.command (Printf.sprintf "%s > %s" exe_path out_file) in
    if execution_status != 0 then begin
      print_in_color tc_fg_bright_red ("Executable didn't exit correctly for test: " ^ test_name);
      false
    end else
      true in

  let execute_scheme () =
    let _ = Sys.command (Printf.sprintf "scheme -q < %s > %s" test_file scheme_out_file) in
    true in

  let execute_comparison () =
    let actual = read_file_trimmed out_file in
    let scheme_actual = read_file_trimmed scheme_out_file in
    write_to_file cmp_results_exp_file (Printf.sprintf "(equal? '(%s) '(%s))" scheme_actual actual);
    let _ = Sys.command (Printf.sprintf "scheme -q < %s > %s" cmp_results_exp_file result_file) in
    true in

  let check_result () =
    let result_scheme_bool = read_file_trimmed result_file in
    let result = result_scheme_bool = "#t" in
    if not result then begin
      print_in_color tc_fg_bright_red (Printf.sprintf "Test failed, incorrect result: %s" test_name);
      false
    end else
      true in

  let _ = execute_commands [
    compile_test_file;
    move_compilation_files_to_temp_dir;
    execute_compiler;
    execute_scheme;
    execute_comparison;
    check_result;
  ] in
  ();;

let temp_dir = "tests/.tests_runner_files/";;

let run_tests_in_dir f_cleanup dir_path =
  let test_files = find_test_files_in_dir dir_path in
  List.iter (run_test temp_dir) test_files;
  f_cleanup dir_path;;

let clean_nothing dir_path = ();;

let cleanup_temp_files dir_path =
  let _ = Sys.command ("rm -v " ^ temp_dir ^ "*") in
  ();;

let cleanup_files_test_dir dir_path =
  let (files, _) = get_dir_entries dir_path in
  let exts = [
    ".actual";
    ".scm.actual";
    ".result"
  ] in
  let filter = (fun file -> List.exists (fun ext -> has_file_ext ext file) exts) in
  let files = List.filter filter files in
  List.iter remove_file files;;

let cleanup_all_files dir_path =
  cleanup_temp_files dir_path;
  cleanup_files_test_dir dir_path;;
