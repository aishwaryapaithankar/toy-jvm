
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

type interface = {
  iNameIndex : int;
}

type cls = {
  constPool : const list;
  name : string;
  super : string;
  accessFlags : int;
  interfaces : interface list;
  fields : field list;
  methods : field list;
  attributes : attr list;
}

type 'a field_list = {
  name : string;
  value: 'a;
}

type var = 
  | Int of int
  | Str of string
  | Void 
  | CRef of (cls * var field_list list ref)

type descriptor =
  | I (*Int*)
  | S (*String*)
  | B (*Signed Byte*)
  | F (*Float*)
  | Z (*Boolean*) 
  | V (*Void*)