#use "stack.ml";;

type const = {
   tag : int; 
   cNameIndex : int;
   classIndex : int;
   nameAndTypeIndex : int;
   stringIndex : int;
   descIndex : int;
   cString : string;
}

type attr = {
  aNameIndex : int;
  len : int;
  info : int list
}

(* Field type is used for both, fields and method *)
type field = { 
  accessFlag : int;
  fNameIndex : int;
  descIndex : int;
  attrCount : int;
  attrInfo : attr list
}

type cls = {
  constPool : const list;
  name : string;
  super : string;
  accessFlags : int;
  interfaces : string list;
  fields : field list;
  methods : field list;
  attributes : attr list;
}

type jvmframe = {
  class_file: cls;
  method_name: string;
	ip: int;
	code: int list;
	locals: var array;
	stack: Stack.t;
}

type descriptor =
  | I 
  | S  
  | B 
  | F 