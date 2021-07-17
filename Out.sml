(* Basic output routines (for bytes). *)
signature OUT  =
sig
  type byte

  val startOut  :  string -> unit
  val endOut    :  unit   -> unit
  val outByte   :  byte   -> unit
  val outPos    :  unit   -> int
  exception NoOut
end
(*----------*)

structure Out: OUT  =
struct
  open BinIO
  type byte = Word8.word
  exception NoOut


  (* hold/get file stream *)
  val out = ref (NONE: outstream option)
  fun getStream ()  =
      case  !out  of  NONE    =>  raise NoOut
                   |  SOME s  =>  s


  (* open/create a file with binary write mode

    xref: `openOut : string -> outstream`
            https://smlfamily.github.io/Basis/bin-io.html#SIG:BIN_IO.openOut:VAL
   *)
  fun startOut fileName   =
    ( case  !out  of  NONE    =>  ()
                   |  SOME s  =>  closeOut s;
      out := SOME (openOut fileName) )

  (* close file

    xref: `closeOut : outstream -> unit`
            https://smlfamily.github.io/Basis/stream-io.html#SIG:STREAM_IO.closeOut:VAL
   *)
  fun endOut ()  =  closeOut  (getStream ())


  (* output one byte

    xref: `output1 : outstream * elem -> unit`
            https://smlfamily.github.io/Basis/stream-io.html#SIG:STREAM_IO.output1:VAL
   *)
  fun outByte b  =  output1   (getStream (), b)

  (* get output position

    xref:
      + `getPosOut : outstream -> out_pos`
          https://smlfamily.github.io/Basis/stream-io.html#SIG:STREAM_IO.getPosOut:VAL
      + `filePosOut : out_pos -> pos`
          https://smlfamily.github.io/Basis/stream-io.html#SIG:STREAM_IO.getPosOut:VAL
      + `Position.int: pos -> int`
          https://smlfamily.github.io/Basis/stream-io.html#SIG:STREAM_IO.pos:TY
   *)
  fun outPos () = (Position.toInt o StreamIO.filePosOut o getPosOut o getStream) ()

end
