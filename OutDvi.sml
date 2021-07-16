signature OUT_DVI  =
sig
  type charCode = BasicTypes.charCode
  type dist = BasicTypes.dist
  type fontNr = FontTypes.fontNr

  val setChar  :     charCode -> unit
  val putChar  :     charCode -> unit
  val setRule  :  dist * dist -> unit
  val putRule  :  dist * dist -> unit
  val right    :         dist -> unit
  val down     :         dist -> unit
  val push     :         unit -> unit
  val pop      :         unit -> unit
  val font     :       fontNr -> unit
  val fontDef  :       fontNr -> unit
  val fontDefs :  fontNr list -> unit
  val bop      :    int * int -> unit
  val eop      :         unit -> unit
  val pre      :          int -> unit
  val post     :  int -> int * int * int -> unit
  val postpost :  int -> unit
  val tail     :  int -> unit
end
(*----------*)

(* Routines which output dvi instructions.
  refs:
    1. tex.pdf   https://texdoc.org/serve/tex/0
    2. DVI.format  http://mirrors.ctan.org/dviware/dvi2tty/DVI.format
 *)
structure OutDvi: OUT_DVI  =
struct
  open BasicTypes;  open FontTypes
  open OutHigh
  open Distance;  open FontVector

  val instr    =  outNat1
  fun instrArg code arg  =  (instr code;  outNat1 arg)

  fun setChar ch  =  if  ch < 128  then  instr ch  else  instrArg 128 ch
  val putChar  =  instrArg 133

  fun rule code (a, b)  =  (instr code;  outInt4 a;  outInt4 b)
  val setRule  =  rule  132
  val putRule  =  rule  137

  val right   =  outInstrV  142
  val down    =  outInstrV  156

  val push    =  fn () => instr  141
  val pop     =  fn () => instr  142


  fun font f  =  instr (171 + f)

  (* [tex-p219#588] fnt_defi (1 <= i <= 4); k[i], c[4], s[4], d[4], a[1], l[1], n[a+l]
              1,2,3,4 ubytes    TeXfontnumber for FNTDEF1 .. FNTDEF4
                    4 ubytes    checksum
                    4 ubytes    scale
                    4 ubytes    design size
                    1 byte      deflen1
                    1 byte      deflen2
    deflen1 + deflen2 bytes     fontname.

    xref: DVI.format
   *)
  fun fontDef nr   =
  let val (fam, s)  =  Vector.sub (famSizeVector, nr)
      val size  =  distInt s
      fun cmName RM  =  "cmr"    |   cmName MI  =  "cmmi"
      |   cmName SY  =  "cmsy"   |   cmName EX  =  "cmex"
      val fileName   =  cmName fam ^ Int.toString s
  in
    instrArg 243 nr;
    outZero 4;
    outInt4 size;
    outInt4 size;
    outZero 1;
    outString fileName
  end

  fun fontDefs    []     =  ()
  |   fontDefs (h :: t)  =  (fontDef h;  fontDefs t)

  (*
    begin_of_page      1 ubyte     (BOP)
    page_nr            4 sbytes    (page number)
    do_be_do          36 bytes     (filler ????)
    prev_page_offset   4 sbytes    (offset in file where previous page starts, -1 for none)

    xref: DVI.format
   *)
  fun bop (pageNr, prevPos)  =
    (
      instr 139;
      outInt4 pageNr;
      outZero 36;
      outInt4 prevPos
    )

  val eop  =  fn () => instr 140


  val version  =  fn () =>  outNat1 2
  (* [tex-p219#587] num/den = 25400000/473628672 *)
  val numDen   =  fn () => (outInt4 25400000;  outInt4 473628672)
  val banner   =  fn () =>  outString "Reinhold Heckmann's Formula Formatter"

  (* [tex-p219#587] pre; i[1], num[4], den[4], mag[4], k[1], x[k]

    preamble_marker    1 ubyte     (PRE)
    version_id         1 ubyte     (should be version 2)
    numerator          4 ubytes    (numerater must equal the one in postamble)
    denominator        4 ubytes    (denominator must equal the one in postamble)
    magnification      4 ubytes    (magnification must equal the one in postamble)
    id_len             1 ubyte     (lenght of identification string)
    id_string     id_len ubytes    (identification string)

    xref: DVI.format
   *)
  fun pre mag  =
    (
      instr 247;  (* PRE *)
      version ();
      numDen ();
      outInt4 mag;
      banner ()
    )

  (* [tex-p220#590] post; p[4], num[4], den[4], mag[4], l[4], u[4], s[2], t[2];

    postamble_marker   1 ubyte     (POST)
    last_page_offset   4 sbytes    (offset in file where last page starts)
    numerator          4 ubytes    (numerater must equal the one in preamble)
    denominator        4 ubytes    (denominator must equal the one in preamble)
    magnification      4 ubytes    (magnification must equal the one in preamble)
    max_page_height    4 ubytes    (maximum page height)
    max_page_width     4 ubytes    (maximum page width)
    max_stack          2 ubytes    (maximum stack depth needed)
    total_pages        2 ubytes    (number of pages in file)

    xref: DVI.format
   *)
  fun post mag (pageNr, prevPos, maxLevel)  =
    (
      instr 248; (* POST *)
      outInt4 prevPos;
      numDen ();
      outInt4 mag;
      outInt4 (distInt (10 * 72));   (* maxVSize *)
      outInt4 (distInt ( 7 * 72));   (* maxWidth *)
      outNat2 maxLevel;
      outNat2 pageNr
    )

  (* output n 223 *)
  fun trailer 0  =  ()
  |   trailer n  =  (instr 223;  trailer (n - 1))

  (* [tex-p220#590] post_post; q[4], i[1]; 223's

    postamble_offset   4 sbytes    (offset in file where postamble starts)
    version_id         1 ubyte     (should be version 2)
    trailer         >= 4 ubytes    (TRAILER)

    xref: DVI.format
   *)
  fun postpost postPos  =
    (
      instr 249; (* POST_POST *)
      outInt4 postPos;
      version ();
      trailer 3
    )

  fun tail ownPos  =  trailer (4 - ownPos mod 4)

end
