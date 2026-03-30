(*Function reads .txt file and makes it a .gv file*)
let () =
  (* Step 1: Open source file, load file's content into the input-channel ic*)
  let ic = In_channel.open_text "dot_syntax.txt" in
  (*Load content via input-channel ic into string variable content *)
  let content = In_channel.input_all ic in
  (*Close input and output channels*)
  In_channel.close ic;

  (*Put the string into a buffer*)
  let buf = Buffer.create 16 in
  Buffer.add_string buf content;

  (*Creating a sub-string, just to have a look*)
  let s = Buffer.sub buf 0 20 in
  let s = s ^ "\nThis is a substring." in 

  (*Rewrite .txt file into .gv file*)
  let oc = open_out "dot_syntax.gv" in
  Buffer.output_buffer oc buf;
  close_out oc;
  
  (*Print the substring*)
  print_endline s;
;