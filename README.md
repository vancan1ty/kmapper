kmapper  -- Currell Berry
==========================================

About
------------------------------------------
This program creates karnaugh maps from truth table inputs.  The program is accessible online at my website www.cvberry.com, but it's also easy to run locally.

Installation and Running
------------------------------------------
This program includes a web interface.  In a common lisp environment with quicklisp
installed, running the program should be as simple as calling (load "kmapper.lisp") from
the REPL, and navigating to localhost:8080 in a web browser. Quicklisp will handle installing
the dependencies "

To use the REPL plain text interface:
1. switch to the package :kmapweb
    (in-package :kmapweb) 
2. save the truth table you wish to generate k maps from as a variable (refer to the 
example truth tables at the bottom of the file for how to format this.)
Let's say you named the variable *my-truth-table*
3. call (create-k-maps *my-truth-table*)
4. answer the prompt for number of inputs

Notes
---------------------------------------------

for both interfaces, the truth table must be entered in whitespace separated form 
-- this is automatically done if you paste from a spreadsheet for example.

note, "tlist" as referred to within the code below means a key value lisp p-list with the
column header as key and a list of the binary values associated with that
column header as values.

You may use or modify this program for any purpose, but please 
include my name in the source.


