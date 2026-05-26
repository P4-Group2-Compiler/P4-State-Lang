------------------------------------------------------------------- README ----------------------------------------------------------------------

StateLang is a small, visual language for modelling state machines into C code and images using Graphviz, DOT and gcc. 

To run it, please follow these instructions:

1) Run git clone https://github.com/P4-Group2-Compiler/P4-State-Lang.git in your terminal


2) Install OPAM, the official Ocaml package manager.
	- Windows: https://ocaml.org/docs/ocaml-on-windows#installing-opam-on-windows
	- Mac/Linux: https://ocaml.org/docs/installing-ocaml


3) Run "opam install . --deps-only" to install dependencies
   

4) Install Graphviz locally on your system.
	- https://graphviz.org/download/


5) Install gcc locally on your system.
   - https://gcc.gnu.org/install/index.html
   
	
This project was made in VS Code, with the help of the Graphviz Interactive Preview extension for immediate visualization of DOT-files.

The extension can be found here:

- https://marketplace.visualstudio.com/items?itemName=tintinweb.graphviz-interactive-preview

	
	
------------------------------------------------------------ HOW TO USE THE COMPILER ------------------------------------------------------------

In the "test.sm" file, you can design any state machine you like with the syntax of StateLang.
If you wish, you can run test.sm as it is to get a preview of what it looks like.

In your IDE or terminal, run "./run.sh" for Mac/Linux, or "./run.bat" for Windows. 
In a folder named "output", you will find two seperate output folders - one for the C-code, and one for the DOT-code. 

The C-code will run immediately in the terminal, where you can interact with the state machine you've created. 
An image of the state machine in .png format will appear in the DOT-folder.

Have fun! (╯°□°）╯︵ ┻━┻

~ Group 2 SW4 AAU, Spring 2026 
