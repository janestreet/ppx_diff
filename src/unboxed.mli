open! Base
open Ppxlib

val is_unboxed : type_declaration -> bool
val attribute : builder:Builder.t -> attribute
