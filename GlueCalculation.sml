signature GLUE_CALCULATION  =
sig
  type glueSpec = BoxTypes.glueSpec
  type dist = BasicTypes.dist
  type infOrder = BoxTypes.infOrder
  type node = BoxTypes.node
  type glueParam = BoxTypes.glueParam

  exception Rigid        (* cannot stretch / shrink *)

  val extractGlue: (glueSpec -> (dist * infOrder)) ->
                   node list -> (dist * infOrder) list
  (* Given an access function (#stretch or #shrink),
     the relevant glue information is extracted from a node list. *)

  val addGlue: infOrder -> (dist * infOrder) list -> dist
  (* This function adds up the glue values of the given infinity order. *)

  val totalGlue: (dist * infOrder) list -> dist * infOrder
  (* This function adds up the glue values in the list,
     separately by the infOrder,
     and returns the highest order where the sum does not cancel out to zero,
     and this sum *)

  val getGlueParam: dist -> node list -> glueParam
  (* computes the glue parameter resulting from changing the natural size
     of the node list by the given amount *)
end  (* signature GLUE_CALCULATION *)
(*----------*)

structure GlueCalculation: GLUE_CALCULATION  =
struct
  open BasicTypes;  open BoxTypes
  open Distance; (* ZERO *)
  open BasicBox

  fun extractGlue  access  =
  let fun extr []              =  []
      |   extr (Glue gs :: t)  =  access gs :: extr t
      |   extr (_       :: t)  =               extr t
  in extr end

  fun addGlue ord  =
  let fun add            []     =  ZERO
      |   add ((s, ord') :: t)  =  if  ord = ord'  then  s + add t  else  add t
  in  add  end

  exception Rigid

  fun totalGlue gl  =
  let fun checkGlue []             =  raise Rigid
      |   checkGlue (ord :: rest)  =
          let val sum  =  addGlue ord gl
          in  if  sum = ZERO  then  checkGlue rest  else  (sum, ord)  end
  in  checkGlue [filll, fill, fil, normal]  end

  fun getGlueParam dw nl  =
    (if dw > ZERO then
       let val (str, order) = totalGlue (extractGlue #stretch nl)
       in  stretching ( (real dw) / real str, order )  end
     else if dw < ZERO then
       let val (shr, order) = totalGlue (extractGlue #shrink  nl)
       in  shrinking ( ~(real dw) / real shr, order )  end
     else natural
    )
    handle Rigid => natural

end  (* structure GlueCalculation *)
