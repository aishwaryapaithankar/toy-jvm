# toy-jvm

Toy-jvm is a Java virtual machine interpreter with the following features:

- int, float, and string data types.
- arithmetic and logical operations.
- loops (for, while) and conditional statements (if-else).
- int and float array.
- classes, object creation and method invokation.
- inheritance, method overriding, and overloading.
- recursive function calls
- printing (uses ocaml print_endline by formatting the input given to System.out.println)

## Build
Requires: Ocaml version 4.12.1 or greater.

The repository includes a make file, simply run following command to build the project:

```bash
make
```

This will generate a toy-jvm executable, which can be added to the PATH as follows:

```bash
export PATH=$PATH:/path/to/toy-jvm
```
Replace /path/to/toy-jvm with the actual path to the toy-jvm executable on your system.

## Run

First, create a .class file using the `javac` command. For example, `javac Test.java` will generate `Test.class`.
To run this file, use the `toy-jvm <class_file_name>` command. For example, `toy-jvm Test`.
Note: The file should have a `main` method.

The `-debug` flag can be used while running `toy-jvm` to see the line-by-line stack progression and instructions executed.


