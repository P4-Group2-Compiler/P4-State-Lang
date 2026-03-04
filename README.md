![Auto Assign](https://github.com/P4-Group2-Compiler/demo-repository/actions/workflows/auto-assign.yml/badge.svg)

![Proof HTML](https://github.com/P4-Group2-Compiler/demo-repository/actions/workflows/proof-html.yml/badge.svg)

# Welcome to your organization's demo respository
This code repository (or "repo") is designed to demonstrate the best GitHub has to offer with the least amount of noise.

--------------------------- INSTRUCTIONS FOR PROJECT WORKFLOW ----------------------------

- MOST files (parser, lexer, AST, etc) belongs under /lib (libraries)
	- /lib/dune decides what files in /lib are run, and how they are run
	- /lib/dune should be sufficiently updated when new files in /lib is created

- /bin/main.ml will be the entry point. This will be the "exe" or whatever, that runs the language and results in some terminal output or some file created. This is similar to /bin/www in node.js/express from IWP, 2. semester
