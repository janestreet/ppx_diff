module type S_plain = sig
  type t

  module Diff : Diff_intf.S_plain with type derived_on = t
end

module type S = sig
  type t

  module Diff : Diff_intf.S with type derived_on = t
end

module type S1_plain = sig
  type 'a t

  module Diff : Diff_intf.S1_plain with type 'a derived_on = 'a t
end

module type S1 = sig
  type 'a t

  module Diff : Diff_intf.S1 with type 'a derived_on = 'a t
end
