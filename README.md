# COMS W4115: Programming Languages and Translators, Spring 2017

# Project
JSTEM: A Matrix  Manipulation Language

# Contributors
Julia Troxell<br>
Sam Stultz<br>
Tessa Hurr<br>
Emily Song<br>
Michelle Lu<br>

# Overview
JSTEM is a programming language specifically designed for manipulating matrices. The compiler is written in OCaml and compiled down to LLVM.

# Installing
Install OCaml Package Manager (OPAM) and make sure that the version of the OCaml LLVM library matches the version of LLVM installed on your system.

# Running
Run "make" in the top-level directory to build JSTEM.native, a file which will then be used to generate LLVM code from JSTEM files. To run an individual JSTEM file, run "./JSTEM.native -c <filename>.JSTEM stdlib.JSTEM".

# Testing
To run all tests, run "make tests" in the top-level directory. To run individual tests, go into the tests directory and run the bash files one by one (i.e. "./compiler_testing.sh").
