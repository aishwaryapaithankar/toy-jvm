exception NotFound of string
exception UnexcpectedType of string
exception EmptyStack

type const = {
  tag : int;
  cNameIndex : int;
  classIndex : int;
  nameAndTypeIndex : int;
  stringIndex : int;
  descIndex : int;
  cString : string;
  bootstrapMethodAttrIndex : int;
  referenceKind : int;
  referenceIndex : int;
  float_num: float;
}

type bootstrapMethodsAttr = {
  bootstrapMethodRef : int;
  numBootstrapArguments : int;
  bootstrapArguments : int list;
}

type attr = {
  aNameIndex : int;
  len : int;
  info : int list;
  bootstrapMethods : bootstrapMethodsAttr list;
}

(* Field type is used for both, fields and method *)
type field = {
  accessFlag : int;
  fNameIndex : int;
  descIndex : int;
  attrCount : int;
  attrInfo : attr list;
}

type interface = { iNameIndex : int }

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

type 'a fl = { name : string; value : 'a }
type cl = { class_name : string; class_file : cls }
type var = Int of int | Float of float | Str of string | Void | CRef of (cls * var fl list ref)

type descriptor =
  | I (*Int*)
  | S (*String*)
  | B (*Signed Byte*)
  | F (*Float*)
  | Z (*Boolean*)
  | V (*Void*)
