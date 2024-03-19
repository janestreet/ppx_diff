include Basic_diffs
include Diffable_intf
module Diff = Diff_intf
module Atomic = Atomic
module Optional_diff = Optional_diff
module Tuples = Tuples
module Set_diff = Set_diff
module Map_diff = Map_diff

module For_ppx = struct
  include Basic_diffs
  include Tuples
  module Of_variant = Of_variant
  module Optional_diff = Optional_diff
end
