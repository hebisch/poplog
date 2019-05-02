/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/bitwise.p
 > Purpose:         Logical operations on numbers
 > Author:          John Williams, Dec  5 1985 (see revisions)
 > Documentation:   CLtL, ch 12.6, 12.7
 > Related Files:   C.all/lisp/src/bitwise.lsp
 */

lisp_compile_mode;

section $-lisp;


define logbitp(index, int);
    testbit(int, index)
enddefine;


constant boole_op_table =
    {%  ;;; clr
        procedure() with_nargs 2; -> ->; 0 endprocedure,

        ;;; set
        procedure() with_nargs 2; -> ->; -1 endprocedure,

        ;;; 1
        procedure(x, y) -> x; endprocedure,

        ;;; 2
        procedure(x, y) -> y; endprocedure,

        ;;; c1
        procedure(x, y); ~~x endprocedure,

        ;;; c2
        procedure(x, y); ~~y endprocedure,

        ;;; and
        nonop &&,

        ;;; ior
        nonop ||,

        ;;; xor
        nonop ||/&,

        ;;; eqv
        procedure(x, y); ~~(x ||/& y) endprocedure,

        ;;; nand
        procedure(x, y); ~~(x && y) endprocedure,

        ;;; nor
        procedure(x, y); ~~(x || y) endprocedure,

        ;;; andc1
        procedure(x, y); y &&~~ x endprocedure,

        ;;; andc2
        nonop &&~~,

        ;;; orc1
        procedure(x, y); (~~x) || y endprocedure,

        ;;; orc2
        procedure(x, y); x || (~~y) endprocedure,
    %};


define checkr_boole_op(op);
    if isinteger(op) and op fi_>= 1 and op fi_<= 16 then
        fast_subscrv(op, boole_op_table)
    else
        lisp_error('Bad specifier for BOOLE: ~S', [^op])
    endif
enddefine;


define boole(op, x, y);
    fast_apply(x, y, checkr_boole_op(op))
enddefine;


constant
    boole_clr    =  1,
    boole_set    =  2,
    boole_1      =  3,
    boole_2      =  4,
    boole_c1     =  5,
    boole_c2     =  6,
    boole_and    =  7,
    boole_ior    =  8,
    boole_xor    =  9,
    boole_eqv    =  10,
    boole_nand   =  11,
    boole_nor    =  12,
    boole_andc1  =  13,
    boole_andc2  =  14,
    boole_orc1   =  15,
    boole_orc2   =  16,
    ;


/* Byte specifiers */

define isbytespecpdr with_nargs 1;
    pdpart() == #_< pdpart(integer_field(1,1)) >_#
enddefine;


define lconstant Checkr_bytespec(item) -> item;
    unless isprocedure(item) and isbytespecpdr(item) do
        lisp_error('Byte specifier needed', [^item])
    endunless
enddefine;


define byte_size(spec);
    fast_frozval(1, Checkr_bytespec(spec))
enddefine;


define byte_position(spec);
    fast_frozval(2, Checkr_bytespec(spec))
enddefine;


define ldb(spec, int);
    fast_apply(int, Checkr_bytespec(spec))
enddefine;


define ldb_test(spec, int);
    fast_apply(int, Checkr_bytespec(spec)) /== 0
enddefine;


define mask_field(spec, int);
    fast_apply(int, false, Checkr_bytespec(spec))
enddefine;


define dpb(new, spec, int);
    new -> fast_apply(int, Checkr_bytespec(spec))
enddefine;


define deposit_field(new, spec, int);
    new -> fast_apply(int, false, Checkr_bytespec(spec))
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  7 1995
        Removed redundant lvar declarations.
--- John Williams, Jun 22 1994
        tidied.
 */
