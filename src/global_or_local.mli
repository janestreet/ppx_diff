open! Core
open Ppxlib

type t =
  | Global
  | Local

val attribute : t -> builder:Builder.t -> attribute
val add_to_core_type : t -> core_type -> builder:Builder.t -> core_type
