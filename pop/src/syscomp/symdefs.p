/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/src/syscomp/symdefs.p
 > Purpose:
 > Author:          John Gibson (see revisions)
 */

/* -------------------------------------------------------------------------

                     COMMON STRUCT DEFINITIONS, ETC

--------------------------------------------------------------------------*/

struct ANY
  { byte    BYTE[];
  | short   SHORT[];
  | int     INT[];
  | word    WORD[];
  };

    /*  Generic pop structures
    */
struct POPREC1
  { word    POPBASE[0];     ;;; dummy empty field for start offset of records
    word    FIRST;
    full    KEY;
>->
  };
struct POPREC2
  { word    FIRST;
    full    KEY;
>-> { word  SECOND; | full ID_VALOF; }  ;;; establishes ID_VALOF at pointer
  };
struct POPREC3
  { word    FIRST;
    full    KEY;
>-> word    SECOND,
            THIRD;
  };

struct RAWSTRUCT
  { word    RAW_SIZE;       ;;; size as word offset
    full    KEY;
>-> word    RAW_WORDS[];
  };


    /*  External pointer data structure
    */
struct EXTERNAL_PTR
  { full    XP_PROPS,
            KEY;
>-> <word>  XP_PTR;             ;;; dereferenced by _call_external etc.
  };


    /*  Keys
    */
lconstant macro (
    GC_KEY_FIELDS = [
        (word)  K_GC_RELOC;     ;;; for use by non-copying garbage collector
        full    KEY;
    >-> int     K_FLAGS;        ;;; flags
        int     K_GC_TYPE;      ;;; sysint garbage collector type
        full    K_GET_SIZE;     ;;; procedure to return word offset size of instance
    ],

    SIMPLE_KEY_FIELDS = [
        GC_KEY_FIELDS
        full    K_DATAWORD,     ;;; dataword
                K_SPEC,         ;;; key spec (as for conskey)
                K_RECOGNISER,   ;;; procedure to recognise instance
                K_APPLY,        ;;; user assignable apply procedure (in a ref)
                K_SYS_EQUALS,   ;;; default system = procedure
                K_EQUALS,       ;;; user assignable = procedure (in a ref)
                K_SYS_PRINT,    ;;; default system print procedure
                K_PRINT,        ;;; user assignable print procedure (in a ref)
                K_HASH;         ;;; user assignable hashing pdr (in a ref)
        byte    K_NUMBER_TYPE,  ;;; number type
                K_PLOG_TYPE,    ;;; gross type for prolog
                K_EXTERN_TYPE,  ;;; type of coercion by _call_external
                K_SPARE_BYTE;   ;;; spare
    ],

    RECORD_KEY_FIELDS = [
        SIMPLE_KEY_FIELDS
        int     K_RECSIZE_R;    ;;; word offset size of record
        full    K_CONS_R,       ;;; record cons procedure
                K_DEST_R,       ;;; record dest procedure
                K_ACCESS_R;     ;;; vector of record field access procedures
    ],
    );


    ;;; garbage collector key
struct KEY_GC
  { GC_KEY_FIELDS
  };

    ;;; simple key
struct KEY
  { SIMPLE_KEY_FIELDS
  };

    ;;; property-entry key
struct KEY_PROP_ENTRY
  { SIMPLE_KEY_FIELDS
    full    K_PROP_TYPE_NAME;   ;;; name of type, e.g. "perm"
  };

    ;;; record-type key without full-field offset table
struct KEY_R
  { RECORD_KEY_FIELDS
  };

    ;;; record-type key with full-field offset table for not-all-full rec
struct KEY_R_NAFULL
  { RECORD_KEY_FIELDS
    int     K_FULL_OFFS_SIZE;   ;;; int offset size of table
    -int    K_FULL_OFFS_TAB[];
  };

    ;;; vector-type key
struct KEY_V
  { SIMPLE_KEY_FIELDS
    -int    K_FIELD_CODE_V;     ;;; field size code 1-4 or -ve bitsize
    full    K_INIT_V,           ;;; vector init procedure
            K_CONS_V,           ;;; vector cons procedure
            K_DEST_V,           ;;; vector dest procedure
            K_SUBSCR_V,         ;;; vector subscr procedure
            K_FAST_SUBSCR_V;    ;;; vector fast_subscr procedure
    word    K_END_V[0];         ;;; dummy field to get word length of key
  };

#_IF DEF FRAME_LEN_16BIT
deftype
  stk_off_t = short;
constant macro PD_FRAME_SPARE = [ short PD_FRAME_SPARE_S ; ] ;
#_ELSE
#_IF DEF FRAME_LEN_32BIT
deftype
  stk_off_t = int;
constant macro PD_FRAME_SPARE = [ 
        short PD_FRAME_SPARE_S ;
        int PD_FRAME_SPARE_I;
] ;
#_ELSE 
deftype
  stk_off_t = byte;
constant macro PD_FRAME_SPARE = [] ;
#_ENDIF
#_ENDIF



constant macro (
    ;;; start of all procedures
    PROCEDURE_COMMON_FIELDS = [
        full    PD_PROPS,           ;;; props
                KEY;
    >-> (code)  PD_EXECUTE;         ;;; code pointer to 1st instruction
        full    PD_UPDATER;         ;;; updater
        int     PD_LENGTH;          ;;; length of whole record in words
        byte    PD_FLAGS,           ;;; flags (says whether closure)
                PD_NARGS;           ;;; number of args (for clos, 16:FF if unassigned)
    ],

    ;;; start of proper procedures
    NON_CLOSURE_COMMON_FIELDS = [
        PROCEDURE_COMMON_FIELDS
        byte    PD_FLAGS2,          ;;; second flags byte
                PD_SPARE;           ;;; currently unused
        short   PD_REGMASK;         ;;; mask for register locals
                PD_FRAME_SPARE
        stk_off_t   PD_FRAME_LEN,       ;;; total size of stack frame in words
                PD_GC_OFFSET_LEN,   ;;; word index from frame pointer to lowest addr lword for gc scan
                PD_GC_SCAN_LEN,     ;;; no of stack frame words for gc scan
                PD_NUM_STK_VARS,    ;;; no of stack allocated locals
                PD_NUM_PSTK_VARS,   ;;; no of stack locals for gc scan
                PD_NLOCALS;         ;;; no of non register dynamic locals
        (code)  PD_EXIT;            ;;; code pointer to exit code
    ],

    CLOSURE_COMMON_FIELDS = [
        PROCEDURE_COMMON_FIELDS
        short   PD_CLOS_NFROZ;      ;;; no of frozvals
        full    PD_CLOS_PDPART;     ;;; pdpart
    ],
    );

    ;;; ordinary procedure
struct PROCEDURE
  { NON_CLOSURE_COMMON_FIELDS
    full    PD_TABLE[];             ;;; structure/literal table (code follows)
  };

    ;;; array procedure
struct ARRAY_PROCEDURE
  { NON_CLOSURE_COMMON_FIELDS
    full    PD_ARRAY_VECTOR,        ;;; array vector
            PD_ARRAY_SUBSCR_PDR,    ;;; subscriptor for accessing array vector
            PD_ARRAY_BOUNDSLIST,    ;;; array boundslist
            PD_ARRAY_MIN_SUBSCR,    ;;; minimum subscript within array vector
            PD_ARRAY_MAX_SUBSCR,    ;;; maximum subscript within array vector
            PD_ARRAY_BY_ROW,        ;;; true if by row, false if not
            PD_ARRAY_TABLE[];       ;;; structure/literal table (code follows)
  };

    ;;; composition of 2 procedures
struct COMPOSITE_PROCEDURE
  { NON_CLOSURE_COMMON_FIELDS
    full    PD_COMPOSITE_P1,        ;;; procedure 1
            PD_COMPOSITE_P2,        ;;; procedure 2
            PD_COMPOSITE_TABLE[];   ;;; structure/literal table (code follows)
  };

    ;;; closure
struct CLOSURE
  { CLOSURE_COMMON_FIELDS
    full    PD_CLOS_FROZVALS[];     ;;; frozvals/literal table (code follows)
  };

struct WORD
  { full    W_IDENTIFIER,   ;;; identifier record or integer undef code
            KEY,
>->         W_STRING,       ;;; string of word
            W_DICT_NEXT;    ;;; link to next word in dictionary
  };

struct REF
  { full    RF_CONT,
            KEY;
>->
  };

struct PAIR
  { full    P_FRONT,
            KEY,
>->         P_BACK;
  };

struct VECTOR
  { word        V_LENGTH;       ;;; length of vector (number of elements)
    full        KEY;
>->
    { 1         V_BITS[];
    | byte      V_BYTES[];
    | -byte     V_SBYTES[];
    | short     V_SHORTS[];
    | -short    V_SSHORTS[];
    | int       V_INTS[];
    | -int      V_SINTS[];
    | word      V_WORDS[];
    }
  };

    ;;; special struct to define V_DOUBLES
struct DOUBLE_VECTOR
  { word    V_LENGTH;
    full    KEY;
>-> double  V_DOUBLES[];
  };

struct BOOLEAN
  { full    BOOL_POP_VAL,   ;;; pop 0 for <false>, 1 for <true>
            KEY;
>-> word        BOOL_VAL;   ;;; machine 0 for <false>, 1 for <true>
  };

struct UNDEF
  { full    U_WORD,
            KEY;
>->
  };

struct PROP         ;;; (permanent & temporary)
  { full    PT_TABLE,       ;;; vector of "buckets" (PTE chains)
            KEY,
>->         PT_ACTIVE,      ;;; "active" default pdr (or false)
            PT_COUNT,       ;;; popint countdown to PT_TABLE expansion
            PT_DEFAULT,     ;;; default value
            PT_ENTRY_KEY,   ;;; key of PTE type used by this prop
            PT_EQ_PDR,      ;;; equality test (or false)
            PT_EXPAND,      ;;; expansion factor (or false)
            PT_HASH_PDR,    ;;; hash pdr (or false)
            PT_REHASH;      ;;; boolean: if true prop is rehashed after GC
  };


struct PROP_ENTRY       ;;; (key determines perm or temp)
  { full    PTE_ARG,
            KEY,
>->         PTE_VALUE,
            PTE_NEXT;   ;;; next entry in chain
    word    PTE_LINK;   ;;; has various uses...
  };

struct DDECIMAL
  { word    DD_1;
    full    KEY;
>->
#_IF WORD_BITS/==DOUBLE_BITS
    word    DD_2;
#_ENDIF
  };

struct PLOGVAR              ;;; prolog variable
  { full    PGV_CONT,
            KEY;
>->
  };

struct PLOGTERM             ;;; prolog term
  { word    PGT_LENGTH;         ;;; length (= arity + 1)
    full    KEY,
>->         PGT_FUNCTOR,        ;;; functor
            PGT_ARGS[];         ;;; and args
  };

    ;;; procedure label record -- for non-local gotos
struct PROCEDURE_LABEL
  { full    PLAB_OWNER,         ;;; owner procedure
            KEY,
>->         PLAB_IDENT,         ;;; stack frame identifier for procedure
            PLAB_LABEL;         ;;; label pair while compiling
    word    PLAB_OFFSET;        ;;; code offset from PD_EXECUTE address
  };


struct STACKMARK
  { word    STKMK_DUMMY;
    full    KEY;
>->
};


#_IF DEF VMS

struct DESCRIPTOR           ;;; VMS data (string) descriptor
  { full    DSC_ITEM;
    full    KEY;
>-> struct  { short     DSPEC_LENGTH;
              byte      DSPEC_DTYPE,
                        DSPEC_CLASS;
              (byte)    DSPEC_PTR;
            }
            DSC_SPEC;
  };

#_ENDIF


;;; --- STACK FRAMES -----------------------------------------------------

#_IF DEF SPARC

/* Stack Frame Layout
   ------------------

                   hi addr
        :                             :
 FP ->  :                             :
        ===============================
        :                             :
        |       non-pop dlocals       |
        |-----------------------------|                    ---
        :                             :                     |
        |         pop dlocals         |                     |
        |-----------------------------|   ---           PD_GC_SCAN_LEN
        :                             :    |                |
        |      pop on-stack lvars     |    |                |
        |-----------------------------|   PD_NUM_STK_VARS  ---
        :                             :    |                |
        |    non-pop on-stack lvars   |    |                |
        |-----------------------------|   ---               |
        |  return address into caller |  i7                 |
        |-----------------------------|                     |
        |            FP               |  i6             PD_GC_OFFSET_LEN
        |-----------------------------|                     |
        |       owner address         |  i5                 |
        |-----------------------------|                     |
        :                             :                     |
        |    non-pop reg lvars (5)    |  i0 - i4            |
        |-----------------------------|                     |
        :                             :                     |
        |      pop reg lvars (8)      |  l0 - l8            |
 SP ->  ===============================                    ---

                    lo addr
*/


deftype csword = word, csshort = short, csbyte = byte;

struct STACK_FRAME
  { word    SF_PROC_BASE[0];    ;;; start point in a process state rec
>-> word    SF_POP_REGS[8];
    { word  SF_NONPOP_REGS[5];
    | full  SF_PLGSV_NEXT_VAR;  ;;; |
      int   SF_PLGSV_CONTN_TOP, ;;; | these 3 only when PD_PLOG_CHOICE set
            SF_PLGSV_TRAIL_SP;  ;;; |
      word  SF_OTHER[2];
    }
    (struct PROCEDURE)
            SF_OWNER;
    (word)  SF_FP;
    (code)  SF_CALLER_RETURN;   ;;; return address into CALLER procedure

    ;;; non-register part
    { word  SF_LOCALS[];        ;;; on-stack local values in general
    | 1         SF_LOCALS_BIT[];
    | byte      SF_LOCALS_B[];
    | short     SF_LOCALS_S[];
    | int       SF_LOCALS_I[];
    | double    SF_LOCALS_D[];

    | (csword)
            SF_NEXT_SEG_SP,     ;;; these 2 fields only in dummy stack
            SF_NEXT_SEG_HI;     ;;; frame closing off external calls
    }
  };

    ;;; Maximum number of user register windows a machine can have
    ;;; (i.e. max number minus 1 for trap handler)
constant macro SPARC_MAX_WINDOWS = 32 - 1;


#_ELSE

    ;;; All other systems besides SPARC

    #_IF DEF STACK_GROWS_UP

        ;;; stack frame types are inverted
    lconstant macro INV = [inv];

    #_ELSE      ;;; grows down

        ;;; not inverted
    lconstant macro INV = [];

    #_ENDIF

/*  Stack Frame Layout
    ------------------

               hi (inv lo) addr
        :                             :
        :                             :
        ===============================
        :                             :
        |   saved non-pop registers   |
        |-----------------------------|                    ---
        :                             :                     |
        |     saved pop registers     |                     |
        |-----------------------------|                 PD_GC_SCAN_LEN
        :                             :                     |
        |         pop dlocals         |                     |
        |-----------------------------|                    ---
        :                             :                     |
        |       non-pop dlocals       |                     |
        |-----------------------------|   ---               |
        :                             :    |                |
        |      pop on-stack lvars     |    |                |
        |-----------------------------|   PD_NUM_STK_VARS   |
        :                             :    |                |
        |    non-pop on-stack lvars   |    |           PD_GC_OFFSET_LEN
        |-----------------------------|   ---               |
        |       owner address         |                     |
 SP ->  |-----------------------------|                     |
        |  return address into owner  |                     |
        ===============================                    ---

                lo (inv hi) addr
*/


deftype csword = INV word, csshort = INV short, csbyte = INV byte;

struct STACK_FRAME
  { INV word    SF_DUMMY;           ;;; hack to double-align start rel to ptr
    INV word    SF_PROC_BASE[0];    ;;; where a process state rec starts
    INV (code)  SF_RETURN_ADDR;     ;;; return address into THIS procedure
>-> INV (struct PROCEDURE)          ;;; where frame pointer points
                SF_OWNER;
    { INV word  SF_LOCALS[];        ;;; local values in general
    | INV 1         SF_LOCALS_BIT[];
    | INV byte      SF_LOCALS_B[];
    | INV short     SF_LOCALS_S[];
    | INV int       SF_LOCALS_I[];
    | INV double    SF_LOCALS_D[];  ;;; may be aligned

    | INV full  SF_PLGSV_NEXT_VAR;  ;;; |
      INV word  SF_PLGSV_CONTN_TOP, ;;; | these 3 only when PD_PLOG_CHOICE set
                SF_PLGSV_TRAIL_SP;  ;;; |

    | INV (csword)
                SF_NEXT_SEG_SP,     ;;; these 2 fields only in dummy stack
                SF_NEXT_SEG_HI;     ;;; frame closing off external calls
    }
  };

#_ENDIF



/* --- Revision History ---------------------------------------------------
--- John Gibson, Mar  5 1997
        Added deftypes for csshort and csbyte as well as csword
--- John Gibson, May 25 1995
        Replaced short PD_SPARE field with 2 byte fields PD_FLAGS2, PD_SPARE
--- John Gibson, Apr  8 1995
        Revised procedure header and key layouts
--- John Gibson, Jun  1 1994
        Added SF_LOCALS_* fields to stack frame definition
--- John Gibson, Sep 23 1992
        Changed PD_CLOS_NFROZ to be short instead of byte
--- John Gibson, Aug 26 1992
        Changed struct BOOLEAN to have an extra field
--- John Gibson, Sep 13 1991
        Added RAWSTRUCT
--- John Gibson, Feb 18 1991
        Added SPARC_MAX_WINDOWS
--- John Gibson, Mar 14 1990
        Added K_EXTERN_TYPE to key structure (and made number and prolog
        type fields bytes).
--- John Gibson, Jan  7 1990
        Version 13.7 for new pointers.
--- John Gibson, Nov 10 1989
        Added fields SF_NEXT_SEG_SP and SF_NEXT_SEG_HI to stack frame.
--- John Gibson, May 17 1989
        Version 13.6403 changes
--- John Gibson, Feb 19 1989
        Moved device, io buffer defs to io.ph
--- John Gibson, Feb  3 1989
        Add _M_PD_UNDEF for undef closures
--- John Gibson, Sep  2 1988
        Removed SPECIAL_VAR_BLOCK structure (no longer needed: offsets
        within _special_var_block are given by the macro _SVB_OFFS).
--- John Gibson, Aug  5 1988
        Added DOUBLE declarations
--- John Gibson, Jul 13 1988
        Added SPARC stack frame definitions.
--- John Gibson, Feb 28 1988
        Added _M_K_PLOG_VARNUM, _M_K_MATCH_VAR to key flags
--- John Gibson, Jan 17 1988
        Moved definitions of procedure flags into 'wordflags.p'.
 */
