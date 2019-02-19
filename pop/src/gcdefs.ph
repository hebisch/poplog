/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/gcdefs.ph
 > Purpose:
 > Author:          John Gibson, Aug 14 1987 (see revisions)
 */

;;;---------------- GARBAGE COLLECTOR DEFINITIONS ----------------------------

#_INCLUDE 'gctypes.ph'

    ;;; segment structure used during copying gc
struct GC_SEG
  { (word)  GC_SEG_BASE_PTR,
            GC_SEG_LIM_PTR,
            GC_SEG_COPY_BASE,   ;;; _NULL if fixed seg
            GC_SEG_COPY_PTR;    ;;; ignored if fixed seg
    word    GC_SEG_RELOC;
  };

define lconstant macro GC_SET_SEG;
    lvars x = itemread();
    [unless $-Sys$-Gc$- _curr_gcseg_base <=@(w) ^x
     and ^x <@(w) $-Sys$-Gc$- _curr_gcseg_lim then
        ;;; not in current segment
        $-Sys$-Gc$-Set_curr_seg(^x)
    endunless].dl
enddefine;

define lconstant syntax GCTYPE_GO_ON;
    lvars prefix = itemread(), lab;
    pop11_comp_expr();
    [% for lab in [
        ;;; this ordering must agree with the gctypes defined in gctypes.ph
        PROCEDURE           ;;;  1
        FULLREC1            ;;;  2
        FULLREC2            ;;;  3
        FULLREC             ;;;  4
        BYTEVEC             ;;;  5
        VECTOR              ;;;  6
        DDECIMAL            ;;;  7
        FULL2ND_REC2        ;;;  8
        WORD                ;;;  9
        KEY                 ;;; 10
        NFULLREC            ;;; 11
        PROCESS             ;;; 12
        USERNFREC           ;;; 13
        USERNFVEC           ;;; 14
        USERREC             ;;; 15
        USERVEC             ;;; 16
        DESCRIPTOR          ;;; 17
        PROPERTY            ;;; 18
        PROPENT_PERM        ;;; 19
        PROPENT_TMPARG      ;;; 20
        PROPENT_TMPVAL      ;;; 21
        PROPENT_TMPBOTH     ;;; 22
        PROPENT_TMPCLR      ;;; 23
        DESTROY_PROPENT     ;;; 24
        DESTROY_MARK        ;;; 25
        STACK               ;;; 26
        PROCSTATE           ;;; 27
        PLOGPROC            ;;; 28
        CONST_PLOGVAR       ;;; 29
        DOUBLE_PAD          ;;; 30
        OBJMOD_PAD          ;;; 31
        FREE_BLOCK          ;;; 32
        RAWSTRUCT           ;;; 33
            ]
    do
        consword(prefix <> word_string(lab))
    endfor %];
    sysGO_ON((), "ERROR")
enddefine;

    ;;; structure for doubleword-aligning things
struct DOUBLE_PAD
  { word    DPAD_1;
    full    KEY;        ;;; = double_pad_key
>-> word    DPAD_2;
  };


section $-Sys;

global constant
        double_pad_key
    ;

endsection;

section $-Sys$-Gc;

global constant
        procedure (Set_curr_seg, Copyscan, Copyscan_deferred,
        App_range, App_process, App_proc_state, App_plog_proc_state,
        App_key, Setup, Do_gc
        ),
        dead_prop_entry, pd_table_noscan_region
    ;

global vars
        _lowest_garbageable, _deferred, _callstack_lim, _copy_gc,
        _prop_tmparg_chain, _prop_tmpval_chain, _prop_tmpboth_chain,
        _destroy_prop_chain, _gcseg_table, _curr_gcseg, _curr_gcseg_base,
        _curr_gcseg_lim, _curr_gcseg_copy, _curr_gcseg_reloc
    ;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 25 1995
        gctype ID*ENT -> FULL2ND_REC2
--- John Gibson, Sep 12 1991
        Added RAWSTRUCT to GCTYPE_GO_ON
--- John Gibson, Jan 28 1990
        Added FREE_BLOCK to GCTYPE_GO_ON
--- John Gibson, Jan  7 1990
        Changes for new pointers.
--- John Gibson, Jul 30 1989
        Added "tmpclr" property entry gc type
--- John Gibson, May 24 1989
        Added new property entry gc types
--- Roger Evans, Aug 30 1988
        added DESTROY_MARK to GCTYPE_GO_ON
--- John Gibson, Aug 22 1988
        Added OBJMOD_PAD to GCTYPE_GO_ON
--- Roger Evans, Aug 19 1988
        added DESTROY_PROPENT to GCTYPE_GO_ON
--- John Gibson, Feb 25 1988
        Sectionised garbage collector
--- John Gibson, Sep 26 1987
        Changes for garbage collector reorganisation. This file
        renamed as gcdefs.ph (previously garbage.ph)
 */
