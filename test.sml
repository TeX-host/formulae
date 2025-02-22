(*
*)
open Input (* sum *)

fun test f x  =  (f x) handle (ex as BasicTypes.NotImplemented s)
              => (print "        ---> "; print s; print " <---"; raise ex)

val disp = test Formula.displayFormula
val line = test Formula.inlineFormula

fun form dviName formula  =  ShipOut.shipOut dviName [disp formula, line formula]

val bigop  =  sum (SOME (trans "i=1")) (SOME (trans "n"))

val mlar = trans "1+2=3"
val mlop = [bigop]
val mleq = bigop :: trans "i=" @ [fraction (trans "n.(n+1)") (trans "2")]
val one  = trans "1"
fun scr s  =  let val nuc = trans s
              in  [sub nuc one, sup nuc one, supsub nuc one one]  end
val mlss  =  scr "M" @ scr "P" @ scr "g" @ scr "f" @ scr "ac" @ scr "ab"

fun fss f  =  let val One = SOME one
              in  [f One NONE, f NONE One, f One One]  end

val mlss'  =  fss sum @ fss int @ scr "a" @ scr "b" @ scr "aa" @ scr "ab"

val overmlss'  =  [overline mlss']

val out = ShipOut.shipOut

(* generate empty DVI *)
fun test0 ()  =  out "test0.dvi" []
(* generate 4 simple equtions *)
fun test1 ()  =  out "test1.dvi" [disp mlar, line mlar, disp mleq, line mleq]
(* generate 4 complex equtions *)
fun test2 ()  =  out "test2.dvi" [disp mlss', disp overmlss', line mlss', line overmlss']

(* exit REPL *)
fun exit () = OS.Process.exit OS.Process.success;
