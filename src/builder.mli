(* Helper module which allows us to write expressions in a way that are more readable to
   developers familiar with Ocaml, but not so familiar with Ppxlib *)
open! Core
open Ppxlib

module type S = sig
  include Ast_builder.S

  val p : Build_helper.t -> pattern
  val e : Build_helper.t -> expression
  val raise_error : string -> 'a
end

type t = (module S)

val create : (module Ast_builder.S) -> t
