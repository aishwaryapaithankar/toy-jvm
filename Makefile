default:
	ocamlopt -o toy-jvm types.ml stack.ml class_file_parse.ml jvm.ml
