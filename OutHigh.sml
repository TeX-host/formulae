signature OUT_HIGH  =
sig
  val outChar    :  char   -> unit
  val outStr     :  string -> unit
  val outString  :  string -> unit
  val outNat1    :  int    -> unit
  val outNat2    :  int    -> unit
  val outInt4    :  int    -> unit
  val outZero    :  int    -> unit
  val outInstrV  :  int -> int -> unit
end
(*----------*)

(* Output routines for strings and integers. *)
structure OutHigh: OUT_HIGH  =
struct
  open Powers2
  open Out

  val byteSmall =  Word8.fromInt        (* : int  -> byte *)
  val byteChar  =  byteSmall o Char.ord (* : char -> byte *)


  (* output one byte
    assert abs n < 2^7
   *)
  val outNat1  =  outByte o byteSmall
  val outChar  =  outByte o byteChar

  (* the naked string *)
  val outStr  =  List.app outChar o String.explode
  (* the string preceded by its length *)
  fun outString s  =  ( outNat1 (String.size s);  outStr s  )

  (* output n zeros *)
  fun outZero 0  =  ()
  |   outZero n  =  ( outNat1 0;  outZero (n-1) )

  fun outNat2 n  =  ( outNat1 (n div two8 );  outNat1 (n mod two8 ) )
  fun outNat3 n  =  ( outNat1 (n div two16);  outNat2 (n mod two16) )

  (* The following differs from Knuth's method since SML's integers
     have 31 Bits only
   *)
  fun splitInt4 n  =
      if  n >= 0  then  (n div two24,  n mod two24)
      else  let val n'  =  n  + two29
                val n'' =  n' + two29
            in  ((n'' div two24) + two7 + two6, n'' mod two24)  end

  (** [tex#600]
    assert abs n >= 2^23:
      dviout( x >> 24);
      dviout((x >> 16) & 255);
      dviout((x >>  8) & 255);
      dviout( x        & 255);
   *)
  fun outInt4 n  =
  let val (n1, nr)  =  splitInt4 n in
    outNat1 n1;  outNat3 nr
  end


  fun makeNat twoI n  = if  n>= 0  then  n  else  n + twoI

  (* Auto choose a `outNat_i/outInt_i` function and
      correct instruction DviCmd_i
      based on the size of the parameter `n`

    e.g:
      + fnt_def_i  (1 <= i <= 4)
      + right_i    (1 <= i <= 4)
      + down_i     (1 <= i <= 4)
    *)
  fun outInstrV code n  =
  let fun Code l  =  outNat1 (code + l)  in
    if  abs n >= two23  then ( Code 4;  outInt4 n                 ) else
    if  abs n >= two15  then ( Code 3;  outNat3 (makeNat two24 n) ) else
    if  abs n >= two7   then ( Code 2;  outNat2 (makeNat two16 n) ) else
    if      n <> 0      then ( Code 1;  outNat1 (makeNat two8  n) ) else  ()
  end

end
