/*  --- Copyright University of Sussex 1991.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_diff.p
 >  Purpose:        compare current ved file and other one being edited
 >  Author:         Jon Cunningham 19?? (see revisions)
 >  Documentation: HELP *VEDCOMMS HELP *DIFF
 >  Related Files: LIB * VED_CDIFF
 */

section ;

global vars procedure ved_diff;

define global ved_nextdiff();
    ;;; prevent windows swapping back and forth
    dlocal vedwarpcontext = false;
    vednextline();
    vedswapfiles();
    vednextline(); vedcheck();
    vedswapfiles();
    ved_diff()
enddefine;

define lconstant comparestrings(string1, string2) -> result;
    lvars len, result, string1, string2;
    unless isstring(string1) and isstring(string2) then
        mishap(string1,string2, 2, 'STRING NEEDED');
    endunless;
    min(length(string1),length(string2)) -> len;
    1 -> result;
    while result fi_<= len
            and fast_subscrs(result,string1) == fast_subscrs(result,string2) do
        result fi_+ 1 -> result;
    endwhile;
enddefine;

define global procedure ved_diff();
    lvars this, other, t_line, o_line, diff_col, t_size, o_size;
    dlocal vedstartwindow;
    if listlength(vedbufferlist) fi_< 2 then
        vederror('Only one file being edited');
    endif;

    (vedbuffer, vedline, vvedbuffersize) -> (this, t_line, t_size);
    procedure;
        dlocal ved_current_file = vedbufferlist(2);
        (vedbuffer, vedline, vvedbuffersize)
    endprocedure() -> (other, o_line, o_size);

    if vedstartwindow = vedscreenlength then
        vedscreenlength >> 1 -> vedstartwindow;
    endif;
    repeat
        if t_line fi_> t_size and o_line fi_> o_size then
            vedputmessage('NO DIFFERENCE'); quitloop
        endif;
        unless t_line fi_<= t_size and o_line fi_<= o_size
            and fast_subscrv(t_line,this) = fast_subscrv(o_line,other)
        then
            comparestrings(fast_subscrv(t_line,this),
                                    fast_subscrv(o_line,other)) -> diff_col;
            t_line -> vedline;
            diff_col -> vedcolumn;
            vedputcommand('nextsame');
            vedswapfiles();
            o_line -> vedline;
            diff_col -> vedcolumn;
            vedsetlinesize();
            vedcheck();
            vedswapfiles();
            vedputmessage('DIFFERENCE FOUND (press REDO to get next same line)');
            quitloop()
        endunless;
        t_line fi_+ 1 -> t_line; o_line fi_+ 1 -> o_line;
    endrepeat;
    vedcheck(), vedsetcursor();
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 23 1991
        Changed to use -ved_current_file-
--- John Gibson, Jun  4 1991
        Uses vedwarpcontext
--- Aaron Sloman, Jun  1 1991
    Changed to put call of 'vednextsame' on the command line instead
    of vednextdiff. Also altered message printed when difference is
    found.
--- Aaron Sloman, Apr 25 1986
    added vedtrimline() and vedsetlinesize() to prevent
    trailing spaces in file or problems with wrong vvedlinesize
*/
