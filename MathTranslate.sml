signature MATH_TRANSLATE  =
sig
  type style = BasicTypes.style
  type mlist = MathTypes.mlist
  type box = BoxTypes.box
  type genfraction = MathTypes.genfraction
  type delim = BasicTypes.delim
  type node = BoxTypes.node
  type dist = BasicTypes.dist
  type script = MathTypes.script
  type limits = MathTypes.limits
  type noad = MathTypes.noad
  type hlist = BoxTypes.hlist
  type ilist = IListTypes.ilist

  val cleanBox:      style -> bool -> mlist -> box
  val doGenFraction: style -> bool -> genfraction -> box
  val doLeftRight:   style -> bool -> delim -> mlist -> delim -> box
  val doNucleus:     style -> bool -> bool  -> mlist -> node * dist * bool
  val doGenScripts:  style -> bool -> bool  -> bool  -> script -> hlist
  val doBigOp:       style -> bool -> limits -> script -> hlist
  val NoadToHList:   style -> bool -> noad  -> hlist
  val MListToIList:  style -> bool -> mlist -> ilist
  val MListToHList:  style -> bool -> bool  -> mlist -> hlist
end  (* signature MATH_TRANSLATE *)
(*----------*)

structure MathTranslate: MATH_TRANSLATE  =
struct
  open BasicTypes;  open BoxTypes;  open MathTypes;  open IListTypes
  open General; (* optMap *)
  open Distance; (* ZERO *)
  open BoxPack;  open AxisCenter
  open Kind;  open ChangeStyle
  open MakeChar;  open Accent;  open Radical;  open Boundaries
  open MakeLine;  open GenFraction;  open MakeScripts;  open MakeLimOp
  open IListTranslate

  fun cleanBox st cr ml  =
      boxList (MListToHList st cr false (* no penalties! *) ml)

  and doGenFraction st cr {left, right, thickness, num, den}  =
  let val  st'      =  fract st
      val  numbox   =  cleanBox st' cr   num
      val  denbox   =  cleanBox st' true den
  in  makeGenFraction st thickness left right numbox denbox  end

  and doLeftRight st cr left ml right  =
  let val il   =  MListToIList st cr ml
      val il'  =  attachBoundaries st left right il
  in  boxList (IListToHList st false il')  end

  and doNucleus st _ isOp [MathChar (_, fam, ch)]  =  makeNucChar st isOp fam ch
  |   doNucleus st cr _    ml  =  (Box0 (cleanBox st cr ml), ZERO, false)

  and doGenScripts st cr limits isOp {nucleus, supOpt, subOpt}  =
  let val (nucNode, itCorr, isChar)  =  doNucleus st cr isOp nucleus
      val st'  =  script st
      val supOptBox  =  optMap (cleanBox st' cr)   supOpt
      val subOptBox  =  optMap (cleanBox st' true) subOpt
  in  if  limits
        then  HL  (makeLimOp st        itCorr nucNode supOptBox subOptBox)
        else  makeScripts st cr isChar itCorr nucNode supOptBox subOptBox
  end

  and doBigOp st cr lim script  =
  let val limits  =  (st = D  andalso  lim = default)  orelse  lim = yes
  in  doGenScripts st cr limits true script  end

  and NoadToHList st cr  =
  fn MathChar(_, fam, ch)  =>  makeChar st fam ch
  |  Radical    (del, ml)  =>  HL (makeRadical st del    (cleanBox st true ml))
  |  Accent (fam, ch, ml)  =>  HL (makeAccent  st fam ch (cleanBox st true ml))
  |  VCenter    ml  =>  [axisCenter   st (cleanBox st cr ml)]
  |  Overline   ml  =>  HL (makeOver  st (cleanBox st true ml))
  |  Underline  ml  =>  HL (makeUnder st (cleanBox st cr ml))
  |  GenFraction genFract  =>  HL (doGenFraction st cr genFract)
  |  LeftRight (left, ml, right)  =>  HL (doLeftRight st cr left ml right)
  |  Script script   =>  doGenScripts st cr false false script
  |  BigOp (lim, script)  =>  doBigOp st cr lim script
  |  SubBox   b    =>  HL b
  |  MList    ml   =>  HL (cleanBox st cr ml)
  |  Kind (k, ml)  =>  HL (cleanBox st cr ml)
  |  _             =>  raise CannotHappen
  (* MPen, MSpace, Style, and Choice are handled differently. *)

  and MListToIList st cr  =
  fn []  =>  []
  |  MPen   p     :: rest  =>  IPen   p   ::  MListToIList st  cr rest
  |  MSpace s     :: rest  =>  ISpace s   ::  MListToIList st  cr rest
  |  Style  st'   :: rest  =>  IStyle st' ::  MListToIList st' cr rest
  |  Choice chfun :: rest  =>  MListToIList st cr (chfun st @ rest)
  |  noad         :: rest  =>
       INoad (noadKind noad, NoadToHList st cr noad)
       :: MListToIList st cr rest

  and MListToHList st cr pen ml  =
  let val il  =  MListToIList st cr  ml
      val hl  =  IListToHList st pen il
  in  hl  end
end  (* structure MathTranslate *)
