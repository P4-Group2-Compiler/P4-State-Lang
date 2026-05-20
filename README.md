![Auto Assign](https://github.com/P4-Group2-Compiler/demo-repository/actions/workflows/auto-assign.yml/badge.svg)

![Proof HTML](https://github.com/P4-Group2-Compiler/demo-repository/actions/workflows/proof-html.yml/badge.svg)

# Welcome to your organization's demo respository
This code repository (or "repo") is designed to demonstrate the best GitHub has to offer with the least amount of noise.

--------------------------- DEN SPRØDE START TIL EN README ---------------------------

(*WIP*)

StateLang is a small, visual language for modelling state machines into C code and images. 
(**************************************************************************************************************************************************)
1) Run git clone https://github.com/P4-Group2-Compiler/P4-State-Lang.git in your terminal 

2) Install OPAM, the official Ocaml package manager.
	- Windows: https://ocaml.org/docs/ocaml-on-windows#installing-opam-on-windows
	- Mac/Linux: https://ocaml.org/docs/installing-ocaml

3) Install Graphviz locally on your system.
	- https://graphviz.org/download/

4) GCC needs to be installed on your system.
	
This project was made in VS Code, with the help of the Graphviz Interactive Preview extension for immediate visualization of DOT-files. The extension can be found here:
	- https://marketplace.visualstudio.com/items?itemName=tintinweb.graphviz-interactive-preview
	
* HOW TO USE THE COMPILER
In the "test.sm" file, you can design any state machine you like with the syntax of StateLang. If you wish, you can run test.sm as it is to get a preview of what it looks like.

In your IDE or terminal, run "./run.sh" for Mac/Linux, or "./run.bat" for Windows. 
In a folder named "output", you will find two seperate output folders - one for the C-code, and one for the DOT-code. 

The C-code will run immediately in the terminal, where you can interact with the state machine you've created. 
An image of the state machine in .png format will appear in the DOT-folder.

(*WIP*)

--------------------------- INSTRUCTIONS FOR PROJECT WORKFLOW ----------------------------

- MOST files (parser, lexer, AST, etc) belongs under /lib (libraries)
	- /lib/dune decides what files in /lib are run, and how they are run
	- /lib/dune should be sufficiently updated when new files in /lib is created

- /bin/main.ml will be the entry point. This will be the "exe" or whatever, that runs the language and results in some terminal output or some file created. This is similar to /bin/www in node.js/express from IWP, 2. semester



Made a cheatsheet for working with GIT in the terminal for this project:

The workflow concept is:
Main is latests stable snapshot of Dev
Dev accepts PRs, rejects if unstable. Whenever Dev is stable with more features than Main, Dev merge -> Main

Therefore all feature branches is a copy of Dev ----> Merge into Dev when done, gets tested automatically


------ CREATE NEW FEATURE BRANCH ------
Start from the latest dev branch
1. git checkout dev
2. git pull
3. git checkout -b [your name]/feature/[branch name]


------ COMMIT AND PUSH YOUR WORK ------
4. git status
5. git add [file1] [file2]     (specific files)
   git add .                   (everything)
6. git commit -m "[what you did]"
7. git push
   First push on a new branch? Run:
   git push --set-upstream origin [your name]/feature/[branch name] (This is telling git what local <-> remote branch should be linked, such that you dont have to specify it every time, and can just use git push and git pull in the future)


------ OPEN A PULL REQUEST ------
8. Go to GitHub, click "Compare & pull request"
9. Set base branch to dev (not main)
10. Fill in the PR template
11. CI runs automatically -- fix any failures, push again, CI reruns


------ AFTER YOUR PR IS MERGED ------
12. git checkout dev
13. git pull
14. git branch -d [your name]/feature/[branch name]


------ QUICK REFERENCE ------
git status                    show changed files
git add .                     stage all changes
git add [file]                stage specific file
git commit -m "[msg]"         commit staged changes
git push                      push to GitHub
git pull                      pull latest from GitHub
git checkout [branch]         switch branch
git checkout -b [branch]      create and switch to new branch
git branch -d [branch]        delete local branch
