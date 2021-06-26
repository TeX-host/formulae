signature RADICAL  =
sig
  type style = BasicTypes.style
  type delim = BasicTypes.delim
  type box = BoxTypes.box

  val makeRadical:  style -> delim -> box -> box
end  (* signature RADICAL *)
(*----------*)

structure Radical: RADICAL  =
struct
  open BasicTypes;  open BoxTypes
  fun makeRadical st del box  =  raise (NotImplemented "makeRadical")
end  (* structure Radical *)
