
let process_files filename1 filename2 =
	let read_all_lines file_name =
	let in_channel = open_in file_name in
	let rec read_recursive lines =
	try
	  Scanf.fscanf in_channel "%[^\r\n]\n" (fun x -> read_recursive (x :: lines))
	with
	  End_of_file ->
	    lines in
	let lines = read_recursive [] in
	let _ = close_in_noerr in_channel in
	List.rev (lines) in 

	let concat = List.fold_left (fun a x -> a ^ x) "" 
in " \n " ^ concat (read_all_lines filename1) ^ " \n " ^ concat (read_all_lines filename2)
