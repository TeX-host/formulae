signature CHAR_FUNCTIONS  =
sig
  type style = BasicTypes.style
  type family = BasicTypes.family
  type fontNr = FontTypes.fontNr
  type charCode = BasicTypes.charCode

  val fontNumber:   style -> family -> fontNr
  val larger:       fontNr * charCode -> charCode
end  (* signature CHAR_FUNCTIONS *)
(*----------*)

structure CharFunctions: CHAR_FUNCTIONS  =
struct
  open Vector
  open BasicTypes; (* fontFamilyIdx, fontStyleIdx *)
  open FontTypes;
  open General; (* optVal *)
  open FontVector;
  open CharInfo

  fun fontNumber st fam  =  sub (fontNumberVector, 4 * fontFamilyIdx fam + fontStyleIdx st)
  fun larger  (pair as (_, ch))  =  optVal ch (charLarger pair)
end  (* structure CharFunctions *)
