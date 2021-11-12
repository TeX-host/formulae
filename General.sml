signature GENERAL =
sig
  (* Sum the entire list *)
  val sum       : int list -> int
  (* Find the larger of two int *)
  val max       : int * int -> int
  (* Find max number in [0, list...] *)
  val maximum0  : int list -> int

  (* Check if [list] contains x *)
  val contains  : ''a list -> ''a -> bool
  (* rev a @ b *)
  val revAppend : 'a list -> 'a list -> 'a list

  val optVal    : 'a -> 'a option -> 'a
  val optMap    : ('a -> 'b) -> 'a option -> 'b option
  val optFold   : 'b -> ('a -> 'b) -> 'a option -> 'b

  val lookUp    : ''a -> (''a * 'b) list -> 'b option
end (* signature GENERAL *)

structure General: GENERAL  =
struct
(** Since functions such as `fold' and `exists' depend on the version,
   I program everything from the basic constructors. *)
  open Option; (* map *)

  (** `val round : real -> int`
    https://smlfamily.github.io/Basis/real.html#SIG:REAL.round:VAL:SPEC
   *)
  (* fun round r  =  trunc (r + 0.5)         (* "truncate" in other versions *)  *)

  (** `val foldr : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b`
    https://smlfamily.github.io/Basis/list.html#SIG:LIST.foldr:VAL:SPEC
   *)
  (* val fold = fn : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b *)
  (* fun fold g c  =  let fun f    []     =  c
                       |   f (h :: t)  =  g (h, f t)
                   in  f  end *)

  val sum       = foldr (op +) 0
  fun max(x, y) = if x > y then x else y
  val maximum0  = foldr  max   0

  (* fun contains list x  =  let fun f    []     =  false
                              |   f (h :: t)  =  (h = x)  orelse  f t
                          in  f list  end *)
  fun contains    []    x = false
    | contains (h :: t) x = x = h orelse contains t x;
  (* fun revAppend    []     yl  =  yl
  |   revAppend (x :: xl) yl  =  revAppend xl (x :: yl) *)
  fun revAppend hd tl = (rev hd) @ tl;

  (** `val map : ('a -> 'b) -> 'a option -> 'b option`
    https://smlfamily.github.io/Basis/option.html#SIG:OPTION.map:VAL:SPEC
   *)
  val optMap = map;
  (* fun optMap f  =  fn NONE    =>  NONE
                   |  SOME x  =>  SOME (f x) *)
  fun optFold y f  =  fn NONE    =>  y
                      |  SOME x  =>  f x
  (* fun optVal y  =  optFold y (fn x => x) *)
  fun optVal a opt = getOpt (opt, a);

  (** [NOT_USED] Search x in tuple list [(x, y), ...] and return y *)
  fun lookUp x  =
  let fun searchx          []        =  NONE
      |   searchx ((x', y) :: rest)  =  if  x = x'  then  SOME y
                                        else  searchx rest
  in  searchx  end

end (* structure General *)
