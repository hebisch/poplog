/*  --- Copyright University of Sussex 1993.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_gobble.p
 >  Purpose:        remove "redundant" space from range.  Antidote to JJ
 >  Author:         Chris Slymon (after David Roberts), June 1983 (see revisions)
 >  Documentation:  HELP * FORMAT
 >  Related Files:
 */
compile_mode :pop11 +strict;

/* Only one space allowed between two non-space characters. */

section;

define vars ved_gobble();
    lvars c, oldchanged;
    dlocal vedstatic = false, vedautowrite = false;
    lconstant macro Sh = 16:9A;     ;;; = `\Sh`
    vedpositionpush();
    vedchanged -> oldchanged;
    false ->> vedchanged -> vedstatic;
    vedmarkfind();
    until vedline fi_> vvedmarkhi do
        1 -> vedcolumn;
        unless (vedcurrentchar() ->> c) == `\Sf` or c == `\Sp` then
            ;;; remove any hair spaces first (these are always padding)
            vedtextleft();
            while vedcolumn fi_<= vvedlinesize do
                if vedcurrentchar() == Sh then
                    veddotdelete()
                else
                    vedcharright()
                endif
            endwhile;

            vedtextleft();
            while vedcolumn fi_<= vvedlinesize do
                until vedcurrentchar() == `\s` do vedcharright() enduntil;
                vedcharright();
                while vedcurrentchar() == `\s`
                and vedcolumn fi_<= vvedlinesize do
                    veddotdelete()
                endwhile
            endwhile
        endunless;
        vednextline()
    enduntil;
    vedpositionpop();
    if oldchanged then oldchanged fi_+ 1 else 1 endif -> vedchanged;
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 21 1999
        Now removes hair spaces
--- John Gibson, Apr 22 1993
        Stopped it doing anything to lines beginning with \Sf or \Sp
*/
