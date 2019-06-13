/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syscomp/mcdata.p
 > Purpose:
 > Author:          John Gibson, Mar 20 1995 (see revisions)
 */

/* -------------------------------------------------------------------------

                        MACHINE DATA

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section;

lconstant procedure IDread = readitem;

define :inline lconstant defcm(name=ID, val);
    #_IF not(DEF name) \n constant macro name = val; #_ENDIF
enddefine;


;;; ------------- These values can be overridden in sysdefs.p -------------

    /*
     *  Bit sizes of standard types byte, short, int, double.
     */
defcm(  BYTE_BITS,           8 );
defcm(  SHORT_BITS,         16 );
defcm(  INT_BITS,           32 );
defcm(  DOUBLE_BITS,        64 );

    /*
     *  Address offsets of standard types.
     *  These values assume a machine with uniform byte-addressing
     */
defcm(  BYTE_OFFS,            BYTE_BITS/BYTE_BITS );
defcm(  SHORT_OFFS,          SHORT_BITS/BYTE_BITS );
defcm(  INT_OFFS,              INT_BITS/BYTE_BITS );
defcm(  DOUBLE_OFFS,        DOUBLE_BITS/BYTE_BITS );

    /*
     *  Bit alignment for type access required by machine instructions.
     *  These values assume 'natural' alignment
     */
defcm(  SHORT_ALIGN_BITS,    SHORT_BITS );
defcm(  INT_ALIGN_BITS,        INT_BITS );
defcm(  DOUBLE_ALIGN_BITS,  DOUBLE_BITS );

    /*
     *  The size of a Pop word (must be int or double)
     */
defcm(  WORD_BITS,          INT_BITS );

    /*
     *  The max number of bits in a positive Pop simple integer.
     *  (Assumes popints are scaled by WORD_OFFS = WORD_BITS/BYTE_BITS,
     *  the left shift for which is integer_length(WORD_OFFS)-1.
     *  Subtracting integer_length(WORD_OFFS) accounts for the sign bit.)
     */
defcm(  POPINT_BITS,        WORD_BITS-integer_length(WORD_BITS/BYTE_BITS) );


    /*
     * The max number of bits such that after convertion to word
     * address the resulting address is positive.
    */
defcm(  WORD_COUNT_BITS,   WORD_BITS - integer_length(WORD_BITS/BYTE_BITS) );

    /*
     *  Type-names for machine code and bitfield pointers
     */
defcm(  CODE_POINTER_TYPE,  "int" );    ;;; type of pointer to machine code
defcm(  BIT_POINTER_TYPE,   "int" );    ;;; type of pointer for bitfield access

    /*
     *  Type-name for biginteger slices.
     */
defcm(  BIGINT_SPEC,        "int" );

    /*  Alignments for fields in "struct" definitions (should be the same
     *  as for the C compiler).
     *  These values assume same alignment as machine instructions
     */
defcm(  STRUCT_SHORT_ALIGN_BITS,   SHORT_ALIGN_BITS );
defcm(  STRUCT_INT_ALIGN_BITS,       INT_ALIGN_BITS );
defcm(  STRUCT_DOUBLE_ALIGN_BITS, DOUBLE_ALIGN_BITS );

    /*
     *  Bit sizes of other C types -- long and external pointer
     *  (must be either int or double)
     */
defcm(  LONG_BITS,          WORD_BITS );
defcm(  EXPTR_BITS,         WORD_BITS );



;;; --- OTHER (DERIVED) STUFF -----------------------------------------------

constant macro (
    WORD_ALIGN_BITS = #_IF WORD_BITS==INT_BITS          INT_ALIGN_BITS
                      #_ELSEIF WORD_BITS==DOUBLE_BITS   DOUBLE_ALIGN_BITS
                      #_ELSE_ERROR
                      #_ENDIF,
);

#_IF DOUBLE_ALIGN_BITS > WORD_ALIGN_BITS
    /*  Ensure stack frames are (at least) double aligned
     */
defcm(  STACK_ALIGN_BITS, DOUBLE_ALIGN_BITS );
#_ENDIF

endsection;     /* $- */


constant macro MAX_STACK_FRAME_SIZE =
  #_IF DEF FRAME_LEN_16BIT 
          16:FFFF
  #_ELSEIF DEF FRAME_LEN_32BIT
          16:FFFFFFFF
  #_ELSE
          16:FF
  #_ENDIF;

section $-Popas;

constant macro (
    WORD_OFFS   = #_IF WORD_BITS==INT_BITS          INT_OFFS
                  #_ELSE                            DOUBLE_OFFS
                  #_ENDIF,

    LONG_OFFS   = #_IF LONG_BITS==INT_BITS          INT_OFFS
                  #_ELSEIF LONG_BITS==DOUBLE_BITS   DOUBLE_OFFS
                  #_ELSE_ERROR
                  #_ENDIF,

    EXPTR_OFFS  = #_IF EXPTR_BITS==INT_BITS         INT_OFFS
                  #_ELSEIF EXPTR_BITS==DOUBLE_BITS  DOUBLE_OFFS
                  #_ELSE_ERROR
                  #_ENDIF,

    WORD_BYTES  = WORD_BITS / BYTE_BITS,
);

endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 24 1995
        Added conditional setting for STACK_ALIGN_BITS
 */
