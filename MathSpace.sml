signature MATH_SPACE  =
sig
  type style = BasicTypes.style
  type mathSpace = MathTypes.mathSpace
  type hlist = BoxTypes.hlist

  val makeSpace:      style -> mathSpace -> hlist
  val makeSpaceOpt:   style -> mathSpace option -> hlist
end  (* signature MATH_SPACE *)
(*----------*)

structure MathSpace: MATH_SPACE  =
struct
  open BasicTypes;  open BoxTypes;  open MathTypes
  open General; (* optFold *)
  open StyleParams

  fun translate (SKern s)  =  Kern s
  |   translate (SGlue g)  =  Glue g

  fun conv (m: dist) (s, normal)  =  (m * s, normal)
  |   conv  _         pair        =   pair

  fun convert m (SKern s)  =  Kern (m * s)
  |   convert m (SGlue {size, stretch, shrink})  =
      Glue {size = m * size,  stretch = conv m stretch,  shrink = conv m shrink}

  fun makeSpace st {isMu, always, entry}  =
  if  always  orelse  st = D  orelse  st = T
  then  [if  isMu  then  convert (MathUnit st) entry  else  translate entry]
  else  [ ]
(* Logically, a node option would be more appropriate than a node list.
   However, a node list would be formed by the caller anyway. *)

  fun makeSpaceOpt st  =  optFold [] (makeSpace st)

end
