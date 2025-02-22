(* Translating atomic nodes into dvi *)
signature SET_NODE  =
sig
  type boxkind = BoxTypes.boxkind
  type fontNr = FontTypes.fontNr
  type charCode = BasicTypes.charCode
  type dim = BoxTypes.dim
  type dist = BasicTypes.dist
  type glueParam = BoxTypes.glueParam
  type glueSpec = BoxTypes.glueSpec


  val outChar:  boxkind -> (fontNr * charCode) -> unit
  val outRule:  boxkind -> dim -> unit
  val outKern:  boxkind -> dist -> unit
  val outGlue:  boxkind -> glueParam -> glueSpec -> unit
end
(*----------*)

structure SetNode: SET_NODE  =
struct
  open BasicTypes;  open BoxTypes
  open Distance; (* ZERO, realMult *)
  open CharInfo
  open OutHigh;  open DviCmd

  (* Invariant for horizontal stuff:
     reference point -> end point = reference point + (0, width)
     Invariant for vertical stuff:
     upper left corner -> lower left corner
  *)

  (* Characters *)
  fun outChar HBox info  =  SetChar info
  |   outChar VBox info  =
       ( Down (charHeight info);  PutChar info;  Down (charDepth info) )

  (* Rules *)
  fun outRule HBox {height, depth, width}  =
       ( Down depth;  SetRule (height + depth, width);  Up depth )
  |   outRule VBox {height, depth, width}  =
      let val vsize  =  height + depth
      in  Down vsize;  PutRule (vsize, width)  end

  (* Kerns *)
  fun outKern HBox  =  Right
  |   outKern VBox  =  Down

  (* Glue *)

  fun glueMult (r, ord) (d, ord')  =
  if  ord = ord'  then  realMult (r, d)  else  ZERO

  fun glueSize natural ({size, ...}: glueSpec)  =  size
  |   glueSize (stretching factor) {size, stretch, ...}  =
           size + glueMult factor stretch
  |   glueSize (shrinking  factor) {size, shrink,  ...}  =
           size - glueMult factor shrink

  fun outGlue kind glueParam glueSpec  =
      outKern kind (glueSize glueParam glueSpec)

end
