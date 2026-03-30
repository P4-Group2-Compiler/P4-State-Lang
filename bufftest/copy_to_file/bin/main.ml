(*Function reads and copies the text of one file into another*)
let copy_to_file () =
  (* Step 1: Open source file, load file's content into the input-channel ic*)
  let ic = In_channel.open_text "loremipsum.txt" in
  (*Load content via input-channel ic into string variable content *)
  let content = In_channel.input_all ic in

  (* Step 2: Open destination file, creating an output-channel oc to it*)
  let oc = Out_channel.open_text "lorem_copy.txt" in
  (*Load the  string variable via output-channel oc into destination file*)
  Out_channel.output_string oc content;

  (*Close input and output channels*)
  In_channel.close ic;
  Out_channel.close oc;;

let () =
  print_endline "RUNNING";
  ignore (copy_to_file ());

  let cc = In_channel.open_text "lorem_copy.txt" in
  let copy_content = In_channel.input_all cc in
  print_string copy_content;
  print_endline "\nEnd of function";