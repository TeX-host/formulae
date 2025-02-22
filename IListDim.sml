signature ILIST_DIM  =
sig
  type ilist = IListTypes.ilist
  type dist = BasicTypes.dist

  val  ilistHeight:  ilist -> dist
  val  ilistDepth:   ilist -> dist
end  (* signature ILIST_DIM *)
(*----------*)

structure IListDim: ILIST_DIM  =
struct
  open BasicTypes;  open BoxTypes;  open IListTypes
  open Distance; (* ZERO *)
  open NodeListDim
  open General; (* max *)

  fun  ilistDim (f: hlist -> dist)  =
       fn []                     =>  ZERO
       |  INoad (_, hl) :: rest  =>  max (f hl, ilistDim f rest)
       |  _             :: rest  =>             ilistDim f rest

  val  ilistHeight  =  ilistDim  hlistHeight
  val  ilistDepth   =  ilistDim  hlistDepth
end  (* structure IListDim *)
