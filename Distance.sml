signature DISTANCE  =
sig
  type dist = BasicTypes.dist
  (* dist const ZERO = 0 *)
  val ZERO :  dist
  (* dist const ONE = 2^16 *)
  val ONE  :  dist

  val half     :  dist -> dist
  val realMult :  real * dist -> dist   (* multiply distance with real factor *)
  val distInt  :  int -> dist           (* integer to distance *)
  val distRat  :  int * int -> dist     (* fraction to distance *)
  val distReal :  real -> dist          (* decimal fraction to distance *)
end (* signature DISTANCE *)

structure Distance: DISTANCE  =
struct
  open BasicTypes
  open Powers2; (* two16 *)

  val ZERO =  0
  val ONE  =  two16 (* 2^16 *)

  fun half      d         =  d div 2
  fun realMult (r, d)     =  round (r * real d)
  fun distInt   n         =  n * ONE
  fun distRat  (num, den) =  (ONE * num) div den
  fun distReal  r         =  realMult (r, ONE)
end (* structure Distance *)
