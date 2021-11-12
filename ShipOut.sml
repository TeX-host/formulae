(* Open a dvi file, output a list of lines, and close it. *)
signature SHIP_OUT  =
sig
  val shipOut: string -> BoxTypes.hlist list -> unit
end

structure ShipOut: SHIP_OUT  =
struct
  open Out
  open DviCmd
  open SetBox
  open Distance; (* distInt *)

  val mag       = 2000
  val lineSkip  = distInt 50

  fun lines    []     =  ( )
  |   lines   [l]     =  ( setHList l )
  |   lines (h :: t)  =  ( setHList h;  Down lineSkip;  lines t )

  fun shipOut dviName hlists  =
    (
      startOut dviName;
      Pre mag;
      Bop ();
      lines hlists;
      Eop ();
      Post mag;
      endOut ();
      ()
    )

end
