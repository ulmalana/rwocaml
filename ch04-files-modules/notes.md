# Chapter 04 - Files, Modules, and Programs

## Single file

* Sometimes compiling with `ocamlopt` doesnt work because some
  packages/libraries are unrecognized. 
* We can link some packages/libraries during the compilation time with
  `ocamlfind`
    * Ex linking `Base` and `Stdio`: `$ ocamlfind ocamlopt -linkpkg -package base -packages stdio freq.ml -o freq`
* To simplify this process, we can use dune by creating `dune-project` file and
  `dune` files.
    * ```
        ** dune-project file **
        (lang dune 3.0)


        ** dune file **
        (executable
            (name freq)
            (libraries base stdio))
      ```

## Bytecode vs Native code

OCaml has two compilers:

* `ocamlopt`: for compiling to **native code**. endings with `.exe`.
* `ocamlc`: for compiling to **bytecode that is run on a virtual machine**. endings
  with `.bc`.

### Some remarks

* **Both compilers** generate executables with **nearly identical behaviour**.
* Bytecode compiler can be used on more architectures and has some tools that
  are not available for native code.
* Bytecode compiler is **quicker** than native code compiler.
* To run bytecode executables, we need to have OCaml installed.
* **Rule of thumb**: Native code for production, bytecode for development.

## Signatures and Abstract types

* The implementation details of a module can be hidden by attaching **an
interface** or adding `.mli` file. 
    *This way, we can change our module without changing the file that calls that module (ie. leave it alone).

* **A type is abstract** if its **name is exposed** in the interface, but its
  **definition is hidden**.
