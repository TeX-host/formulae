signature CHANGE_STYLE  =
sig
  type style = BasicTypes.style

  val fract:  style -> style
  val script: style -> style
end  (* signature CHANGE_STYLE *)
(*----------*)

structure ChangeStyle: CHANGE_STYLE  =
struct
  open BasicTypes
  fun fract D  =  T
  |   fract T  =  S
  |   fract _  =  SS

  fun script D  =  S
  |   script T  =  S
  |   script _  =  SS
end  (* structure ChangeStyle *)
