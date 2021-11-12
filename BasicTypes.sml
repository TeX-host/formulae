structure BasicTypes  =
struct
  type charCode =  int
  type delim    =  int
  type penalty  =  int
  type dist     =  int
  type size     =  int

  (* These numbers are only needed for vector indexing. *)
  datatype family = RM | MI | SY | EX
  fun famNr RM = 0
    | famNr MI = 1
    | famNr SY = 2
    | famNr EX = 3

  datatype style  = D | T | S | SS
  fun styleNr D  = 0
    | styleNr T  = 1
    | styleNr S  = 2
    | styleNr SS = 3

  exception NotImplemented of string
  exception CannotHappen
end  (* structure BasicTypes *)
