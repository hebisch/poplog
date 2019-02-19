/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/matchvar.ph
 > Purpose:         Definitions for matchvars
 > Author:          John Gibson, Dec  9 1995
 */

struct MATCH_VAR
  { full    MV_NAME,            ;;; name or false
            KEY,
>->         MV_IDENT,           ;;; identifier, word or false
            MV_RESTRICTION,     ;;; restriction/mapping pdr or int length or false
            MV_FLAGS;           ;;; flags
  };

lconstant macro (
    ;;; Flags in MV_FLAGS
    M_MV_SEQ_MATCH  = 2:1e0,    ;;; matches sequences
    M_MV_CONV_P     = 2:1e1,    ;;; restriction pdr is conversion pdr

    ;;; return value from Eq__Matchvar when called with seq match var
    ;;; inside = on a list
    SEQ_MATCH_RETURN = -1,
);
