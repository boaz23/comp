(*
TODO:
* convert the commands execution to a pipeline
* normalize the results of procedure (i.e. a line which starts with '#<procedure'):
  * if comparing with chez, ignore these
  * otherwise, do not change anythng?
* proper cleanup per test. failed tests can still get cleaned up right now
* format the output test file name: remove the base directory
* print a success message if all tests pass or a report of all tests failed
* custom scheme for chez scheme instead of the same code for both
* fix printing noy immediate, for some reason it is delayed until the end
* hide the makefile printing commands and instead print (in normal color) the test beign run
* refactor f_cleaup to f_cleanup_on_success add a f_cleanup_on_fail
* rename of cleanup functions meant for the run tests functions to begin with f_cleanup instead of cleanup
* sort the file and direcory names in the enties enumeration for better predictions of the tests to be followed after the current one
*)

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

let cleanup_nothing _ _ = ();;

let cleanup_temp_files temp_dir out_files =
  let _ = Sys.command ("rm " ^ temp_dir ^ "*") in
  ();;

let cleanup_files_test_dir temp_dir out_files =
  List.iter remove_file out_files;;

let cleanup_all_files temp_dir out_files =
  cleanup_temp_files temp_dir out_files;
  cleanup_files_test_dir temp_dir out_files;;

let rec execute_commands = function
  | [] -> true
  | command :: rest ->
    let result = command () in
    if result then execute_commands rest
    else false;;

let run_test f_cleanup temp_dir test_file =
  let file_name = file_name_from_path test_file in
  let file_name_no_ext = file_name_without_ext file_name in
  let file_path_no_ext = file_name_without_ext test_file in
  let temp_file_no_ext = combine_path temp_dir file_name_no_ext in
  let test_name = file_name in

  let exe_path = temp_file_no_ext in
  let cmp_results_exp_file = temp_file_no_ext ^ ".cmp_command.scm" in
  let expected_out_file = file_path_no_ext ^ ".out" in
  let out_file = file_path_no_ext ^ ".actual" in
  let scheme_out_file = file_path_no_ext ^ ".actual.chez" in
  let result_file = file_path_no_ext ^ ".result" in

  let compile_test_file () =
    let status = Sys.command ("make -f Makefile " ^ file_path_no_ext) in
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

  let check_result_of_comparison_with_scheme () =
    let result_scheme_bool = read_file_trimmed result_file in
    let result = result_scheme_bool = "#t" in
    if not result then begin
      print_in_color tc_fg_bright_red (Printf.sprintf "Test failed, incorrect result: %s" test_name);
      false
    end else
      true in

  let check_result_with_expected_file () =
    let actual = read_file_trimmed out_file in
    let expected = read_file_trimmed expected_out_file in
    if actual <> expected then begin
      print_in_color tc_fg_bright_red (Printf.sprintf "Test failed, incorrect result: %s" test_name);
      false
    end else
      true in

  let base_commands = [
    compile_test_file;
    move_compilation_files_to_temp_dir;
    execute_compiler;
  ] in
  let rest_commands =
    if Sys.file_exists expected_out_file then [
      check_result_with_expected_file;
    ]
    else [
      execute_scheme;
      execute_comparison;
      check_result_of_comparison_with_scheme;
    ] in
  let success = execute_commands (base_commands @ rest_commands) in
  if success then begin
    f_cleanup temp_dir [out_file; scheme_out_file; result_file];
  end
  else begin
    cleanup_temp_files temp_dir [];
  end;
  ();;

let find_test_files_from_files files =
  List.filter (has_file_ext ".scm") files;;
let run_tests_from_files f_cleanup temp_dir test_files =
  List.iter (run_test f_cleanup temp_dir) test_files;;

let run_tests_in_dir f_cleanup temp_dir dir_path =
  let (files, _) = get_dir_entries dir_path in
  let test_files = find_test_files_from_files files in
  run_tests_from_files f_cleanup temp_dir test_files;;

let rec run_tests_in_dirs_recursive f_cleanup temp_dir dir_path =
  let (files, dirs) = get_dir_entries dir_path in
  let test_files = find_test_files_from_files files in
  if test_files <> [] then begin
    Printf.printf "%s" ("testing in dir: " ^ dir_path);
    run_tests_from_files f_cleanup temp_dir test_files;
  end;
  List.iter (run_tests_in_dirs_recursive f_cleanup temp_dir) dirs;;

let temp_dir = "tests/.tests_runner_files/";;
let tests_dir = "tests";;
let cleanup_all_temp_files () = cleanup_temp_files temp_dir [];;
let run_all () = run_tests_in_dirs_recursive cleanup_all_files temp_dir tests_dir;;
