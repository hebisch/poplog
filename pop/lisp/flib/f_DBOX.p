/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/flib/f_DBOX.p
 > Purpose:         LIB * FORMAT_PRINT ~D, ~B, ~O, and ~X directives
 > Author:          John Williams, Jun 30 1992 (see revisions)
 > Documentation:   HELP * FORMAT_PRINT
 > Related Files:   LIB * FORMAT_PRINT
 */

uses format_print;

section $-lisp$-fpr;

unless isdefined("number_utils") do
    syslibcompile("number_utils", f_liblist) ->
endunless;


;;; PRINT INTEGERS IN VARIOUS RADICES  ~D, ~B, ~O, ~X


define f_DBOX(mincol, padchar, commachar, pop_pr_radix) with_props false;
    lvars int, intsign, size, threes, i;
    dlocal pop_pr_radix;
    defaults mincol 0 padchar `\s` commachar `,`;
    next_f_arg() -> int;
    if isintegral(int) then
        if mincol == 0 and not(f_colon or f_at) then
            ;;; NO NEED TO DETERMINE PRINTED SIZE OF "ARG"
            pr(int)
        else
            ;;; FIRST GET THE PRINTED SIZE OF "ARG"
            Sign_and_abs(int, f_at) -> int -> intsign;
            Int_length(int, pop_pr_radix) -> size;
            if f_colon then
                ;;; ALLOW FOR COMMAS BETWEEN GROUPS OF THREE DIGITS
                if (size // 3 -> threes) == 0 then
                    threes - 1 -> threes
                endif;
                threes + size -> size
            endif;
            ;;; DO THE PADDING (ON THE LEFT)
            size + datalength(intsign) -> size;
            repeat (mincol - size) times
                cucharout(padchar)
            endrepeat;
            ;;; NOW PRINT "ARG"
            sys_syspr(intsign);
            if f_colon then
                1 -> size;
                repeat
                    (int // pop_pr_radix -> int) + `0`;
                    quitif(int == 0);
                    if size rem 3 == 0 then commachar endif;
                    size + 1 -> size
                endrepeat;
                repeat size + threes times
                    cucharout()
                endrepeat
            else
                sys_syspr(int)
            endif
        endif
    else
        ;;; USE ~A, WITH SAME "MINCOL" AND "PADCHAR", PAD ON LEFT
        10 -> pop_pr_radix;
        true -> f_at;
        f_arg_index - 1 -> f_arg_index;
        f_proc(`A`)(mincol, false, false, padchar)
    endif
enddefine;


f_DBOX(% 10 %) -> f_proc(`D`);
f_DBOX(% 2  %) -> f_proc(`B`);
f_DBOX(% 8  %) -> f_proc(`O`);
f_DBOX(% 16 %) -> f_proc(`X`);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug 23 1995  Removed redundant lvar declarations.
--- John Williams, Jun 30 1992  uses new "number_utils" procedures
--- John Williams, Dec 17 1985  changed 'isinteger' to 'isintegral'
 */
