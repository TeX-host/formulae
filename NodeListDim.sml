signature NODE_LIST_DIM  =
sig
  type hlist = BoxTypes.hlist
  type vlist = BoxTypes.vlist
  type dist = BasicTypes.dist

  (* Versions of width, height, and depth for node lists *)
  val hlistWidth:  hlist -> dist
  val hlistHeight: hlist -> dist
  val hlistDepth:  hlist -> dist
  val vlistVsize:  vlist -> dist
  val vlistWidth:  vlist -> dist
end  (* signature NODE_LIST_DIM *)
(*----------*)

structure NodeListDim: NODE_LIST_DIM  =
struct
  open BoxTypes; open BasicTypes;
  open General
  open NodeDim
  fun compute f g nl  =  f (map g nl)
  val hlistWidth   =  compute sum width
  val hlistHeight  =  compute maximum0 height
  val hlistDepth   =  compute maximum0 depth
  val vlistWidth   =  compute maximum0 vwidth
  val vlistVsize   =  compute sum vsize
end  (* structure NodeListDim *)
