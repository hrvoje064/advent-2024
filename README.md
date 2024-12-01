This is a collection of my (future) solutions for advent of code 2024 project. 
Top level files are just some utility files that every, day problem will need. Mostly reading and parsing problem input files:

utils.scm and utils.so are for Chez Scheme solutions
Every day code file using them need this at the top: 
(load "../../utils.so")

utils.ml and utils.mli are OCaml module file and OCaml interface file
for OCaml solutions. Unlike for Chez Scheme, where only one copy of utils.so is needed at top level directory, for OCaml, utils.cmo and utils.cmi must be copied to every day-x/OCaml directory for
#load "utils.cmo";;
open Utils;;
to work. That is probably my fault, because I just don't know enough of OCaml to fix that.

Every day directory has two sub-folders Chez and OCaml.

A word of caution; I'm just an amateur (and pretty old at that - 73), so I do not expect to be able to solve all 2024 problems.

