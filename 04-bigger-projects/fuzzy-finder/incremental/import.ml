open! Base

module Incr = Incremental_lib.Incremental.Make ()
module Incr_map = Incr_map.Make(Incr)
module Time = Core_kernel.Time
