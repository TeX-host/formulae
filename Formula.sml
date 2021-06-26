signature FORMULA  =
sig
  type mlist = MathTypes.mlist
  type hlist = BoxTypes.hlist

  val inlineFormula:   mlist -> hlist
  val displayFormula:  mlist -> hlist
end  (* signature FORMULA *)
(*----------*)

structure Formula: FORMULA  =
struct
  open BasicTypes
  open MathTranslate

  val displayFormula  =  MListToHList D false false
  val inlineFormula   =  MListToHList T false true
end  (* structure Formula *)
