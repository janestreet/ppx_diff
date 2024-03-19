(* Helper module which allows us to write expressions in a way that are more readable to
   developers familiar with OCaml, but not so familiar with Ppxlib *)
open! Base
open Ppxlib

module type Ast_builders = sig
  include Ast_builder.S
  include Ppxlib_jane.Ast_builder.S_with_implicit_loc
end

module type S = sig
  include Ast_builders

  val p : Build_helper.t -> pattern
  val e : Build_helper.t -> expression
  val raise_error : string -> 'a
end

type t = (module S)

val create : (module Ast_builders) -> t
