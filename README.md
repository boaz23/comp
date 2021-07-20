## General
A compiler for a simple dialect of _Scheme_ written in _OCaml_ and _x86 Assembly_.
It supports tail-call optimization, boxing and lexical addressing.

This was built as a continous semester project divided to multiple parts, each part was an assignment.
The compiler was almost entierly built by us. However the staff did gave us some base files.
The files contain a parsing combinator library and they covered much of the run-time support implementation.

The files we were provided with are found in the course git repository.  
To clone it, run `git clone https://www.cs.bgu.ac.il/~comp211/compiler`.  
The assignments full description can be found [here](https://www.cs.bgu.ac.il/~comp211/Assignments).

## Assignments
The following sub-sections list the different parts we implemented (in order of the assignments):
1. **_Reader_**: A parser which translates the concrete syntax of scheme to an AST of S-Expressions.  
Implemented in `reader.ml`.  
It uses the parsing combinator library the staff gave us, which is implemented in `pc.ml`.
2. **_Tag parser_**: A parser which translates the S-Expressions AST to a Scheme AST. It is also responsible for expanding macros.  
Implemented in `tag-parser.ml`.
3. **_Sementic analysis_**: Performs semantic analysis and converts the Scheme AST to an internal implementation specific AST to help tag nodes with specific optimization and semantic analysis information such as the lexical address of a variable.  
The analyses it performs are: _Lexical addressing_, _Tail call annotation_, _Boxing_.  
Implemented in `semantic-analyser.ml`.
4. **_Code generation and run-time support_**: Responsible for generating the assembly code of the program and provide the standard scheme library.
This part is implemented by the rest of the files. An explaination for them can be found in the `Final project` assignment description under `section 3`.

   This part constructs the constants table and free variable table and then generates the code.

## Implementation notes
- The entry function of the compiler is found at `compiler.ml` and the high-level flow of the compiler is implemented in the function `generate_code_from_files`.  
Under it, at the very buttom of the file, is the entry code for the file which calls the above function with the input file.
- The hard parts of the code generation were handling and manipulating the stack for tail-call optimizations, optional arguments and the `apply` procedure (which uses a run-time dynamic tail-call).
