/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/ident.ph
 > Purpose:
 > Author:          John Gibson, May 15 1989 (see revisions)
 */

;;; ------------------ IDENTIFIER DEFINITIONS -----------------------------


    ;;; full identifier
struct IDENT
  { { word  ID_INFO;            ;;; covers ID_IDENTPROPS etc
    | short ID_IDENTPROPS;
      { byte    ID_PERM_FLAGS;  ;;; if permanent identifier
      | byte    ID_LEX_FLAGS;   ;;; if lexical identifier
      }
      byte  ID_NUM_ATTRIBUTE;   ;;; precedence if operator
                                ;;; else multiplicity if active
    }
    full    KEY,
>->         ID_VALOF;           ;;; value cell
  };

lconstant macro (

    ;;; Flags for lexical identifiers (i.e. that have the M_ID_LEX bit set
    ;;; in the ID_IDENTPROPS field), in ID_LEX_FLAGS
    ;;; (see syscomp/wordflags.p for flags in ID_IDENTPROPS)

    M_LEX_NON_LOCAL         = 2:1e0,    ;;; is a non-local identifier
    M_LEX_USED              = 2:1e1,    ;;; was used in current procedure
    M_LEX_USED_NON_LOCALLY  = 2:1e2,    ;;; home identifier used non-locally
    M_LEX_USE_REG           = 2:1e2,    ;;; use the register id in the ID_VALOF
    M_LEX_USE_DYNAMIC       = 2:1e3,    ;;; use dynamic var instead of ref
    M_LEX_RTID_ACCESS       = 2:1e4,    ;;; access is via a run-time ident
    ;;; not lconstant
    M_LEX_PCR_LVAR          = 2:1e5,    ;;; lvar for storing a lex closure
    M_LEX_NLGOTO_LVAR       = 2:1e6,    ;;; lvar for non-local goto control
    M_LEX_DONT_USE_REG      = 2:1e7,    ;;; local lvar that shouldn't use reg
    ;;; lconstant only
    M_LEX_PUSHED            = 2:1e5,    ;;; pushed at all
    M_LEX_COUNTED_PUSH      = 2:1e6,    ;;; non-discounted push

    );



/* --- Revision History ---------------------------------------------------
--- John Gibson, May 25 1995
        Added M_LEX_DONT_USE_REG
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, Jun 20 1989
        Removed _M_... versions of macros (replaced by _:M_... in code)
--- John Gibson, May 15 1989
        Created from stuff in vmdefs.ph and syscomp/symdefs.p
 */
