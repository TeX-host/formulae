signature ACCENT  =
sig
  type style = BasicTypes.style
  type charCode = BasicTypes.charCode
  type family = BasicTypes.family
  type box = BoxTypes.box

  val makeAccent:  style -> family -> charCode -> box -> box
end  (* signature ACCENT *)
(*----------*)

structure Accent: ACCENT  =
struct
  open BasicTypes;  open BoxTypes
  fun makeAccent  st fam ch box  =  raise (NotImplemented "makeAccent")
end  (* structure Accent *)
