/* --- Copyright University of Birmingham 1997. All rights reserved. ------
 > File:            $poplocal/local/auto/ARGS.p
 >  Also in         $poplocal/local/rclib/auto/ARGS.p
 > Purpose:         Experimental macro for handling optional extra arguments
 > Author:          Aaron Sloman, Oct 27 1997
                    Suggested partly by Brian Logan
 > Documentation:   Below
 > Related Files:
 */

/*

    ARGS x, y, z, &OPTIONAL A:isword, B:isinteger, C:islist;

expands to something like:

    repeat
        if islist(z) then
            (x, y, z) -> (x, y, z, C)
        elseif isinteger(z) then
            (x, y, z) -> (x, y, z, B)
        elseif isword(z) then
            (x, y, z) -> (x, y, z, A)
        else
            quitloop()
        endif
    endrepeat;


;;; example

define silly(p, q, r) -> result;
    lvars A = false, B = 0;

    ARGS p, q, r, &OPTIONAL A:isboolean, B:isnumber;

    [%p, q, r, A, B%] -> result
enddefine;

;;; test it

silly([1], [2], [3]) =>
** [[1] [2] [3] <false> 0]
silly([1], [2], [3], true) =>
** [[1] [2] [3] <true> 0]
silly([1], [2], [3], 99) =>
** [[1] [2] [3] <false> 99]
silly([1], [2], [3], true, 99) =>
** [[1] [2] [3] <true> 99]
silly([1], [2], [3], 99, true) =>
** [[1] [2] [3] <true> 99]

*/

section;


define macro ARGS;
    lvars item, inputs, optional, lastvar;
    ;;; read in the compulsory args, separated by commas
    [%
        repeat
            readitem() -> item;
            if item == "&" then
                pop11_need_nextitem("OPTIONAL") ->;
                quitloop()
            else item, pop11_need_nextitem(",") ->;
            endif;
        endrepeat
    %] -> inputs;
    ;;; [inputs ^inputs]==>

    last(inputs) -> lastvar;
    ;;; read the optional args, with their recognisers.

    [%
        repeat
            readitem() -> item;
            if item == ";" then
                quitloop()
            else
                pop11_need_nextitem(":") ->;
                conspair(item, readitem());
                readitem() -> item;
                if item == ";" then
                    quitloop()
                elseunless item == "," then
                    mishap('EXPECTED "," IN OPTIONAL ARGS LIST', [^item])
                endif
            endif;
        endrepeat
    %] -> optional;

    ;;; [optional ^optional]==>

    ;;; now put the loop onto proglist
    lvars firsttime = true;
    [repeat %
        for item in optional do
            if firsttime then
                "if"; false -> firsttime
            else "elseif"
            endif;
            lvars (optvar, test) = sys_grbg_destpair(item);
            test, "(", lastvar, ")", "then",

            lvars var;
            "(", for var in inputs do var, "," endfor, ->, ")",
            "->",
            "(", for var in inputs do var, "," endfor, optvar, ")"
        endfor;
    %else quitloop() endif endrepeat;] nc_<> proglist -> proglist;
    ;;; proglist ==>
    sys_grbg_list(optional);
enddefine;

endsection;
