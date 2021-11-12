signature BASIC_BOX  =
sig
  type dist = BasicTypes.dist
  type node = BoxTypes.node
  type glueSpec = BoxTypes.glueSpec
  type box = BoxTypes.box
  type dim = BoxTypes.dim
  type hlist = BoxTypes.hlist
  type vlist = BoxTypes.vlist

  val rule: dist -> dist -> node (* height and width *)
  val ssGlue:   glueSpec
  val emptyBox: box
  val hbox: dim -> hlist -> box  (* constructs hbox with given dimensions *)
  val vbox: dim -> vlist -> box  (* constructs vbox with given dimensions *)
  val extend: dist -> node -> hlist     (* extends to the right *)
end  (* signature BASIC_BOX *)
(*----------*)

structure BasicBox: BASIC_BOX  =
struct
  open BasicTypes;  open BoxTypes
  open Distance; (* ZERO, ONE *)

  fun rule h w  =  Rule {height = h, depth = ZERO, width = w}

  val ssGlue:  glueSpec   =
  let val fil1  =  (ONE, fil)
  in  {size = ZERO,  stretch = fil1,  shrink = fil1}  end

  (* makebox: boxkind -> dim -> node list -> box
        constructs a box of given kind with given dimensions and content *)
  fun makebox (boxkind: boxkind ) ({height = h, depth = d, width = w}: dim) (nl: node list)  =
          {kind    = boxkind,
           height  = h,   depth = d,  width = w,
           content = nl,  glueParam = natural}

  (* hbox: dim -> hlist -> box
        constructs a hbox with given dimensions and content *)
  val hbox  =  makebox HBox

  (* vbox: dim -> vlist -> box
        constructs a vbox with given dimensions and content *)
  val vbox  =  makebox VBox

  val emptyBox : box  =
      hbox  {width = ZERO,  depth = ZERO,  height = ZERO}  []

  fun extend dist node  =
  let val extension  =  if  dist = ZERO  then  []  else  [Kern dist]
  in  node :: extension  end

end  (* structure BasicBox *)
