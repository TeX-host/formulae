structure BasicTypes  =
struct
  type charCode =  int
  type delim    =  int
  type penalty  =  int
  type dist     =  int
  type size     =  int

  (* These numbers are only needed for vector indexing. *)
  datatype family = RM | MI | SY | EX
  (* Old Name: `famNr` *)
  fun fontFamilyIdx RM = 0
    | fontFamilyIdx MI = 1
    | fontFamilyIdx SY = 2
    | fontFamilyIdx EX = 3

  datatype style  = D | T | S | SS
  (* Old Name: `styleNr` *)
  fun fontStyleIdx D  = 0
    | fontStyleIdx T  = 1
    | fontStyleIdx S  = 2
    | fontStyleIdx SS = 3

  exception NotImplemented of string
  exception CannotHappen
end  (* structure BasicTypes *)
