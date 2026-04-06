include Basic_diffs
include Diffable_intf
module Diff = Diff_intf
module Atomic = Atomic
module Optional_diff = Optional_diff
module Tuples = Tuples
module Set_diff = Set_diff
module Map_diff = Map_diff

[%%template
[@@@modality.default p = (portable, nonportable)]

module Of_diffable_plain = Of_diffable.Make_plain [@modality p]
module Of_diffable = Of_diffable.Make [@modality p]]

module For_ppx = struct
  include Basic_diffs
  include Tuples
  module Global = Base.Modes.Global
  module Of_variant = Of_variant
  module Optional_diff = Optional_diff
end
