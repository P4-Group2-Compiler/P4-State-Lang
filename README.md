![Auto Assign](https://github.com/P4-Group2-Compiler/demo-repository/actions/workflows/auto-assign.yml/badge.svg)

![Proof HTML](https://github.com/P4-Group2-Compiler/demo-repository/actions/workflows/proof-html.yml/badge.svg)

# Welcome to your organization's demo respository
This code repository (or "repo") is designed to demonstrate the best GitHub has to offer with the least amount of noise.

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
