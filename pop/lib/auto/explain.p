/*  --- Copyright University of Sussex 1996. All rights reserved. ----------
 >  File:           C.all/lib/auto/explain.p
 >  Purpose:        finding and printing help file without ved.
 >  Author:         Aaron Sloman, March 1984 (see revisions)
 >  Documentation:  HELP * EXPLAIN
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

include sysdefs.ph;

define vars macro explain;
    lvars topic, topfile;
    dlocal popnewline = true, vvedgotoplace = false;
    rdstringto([; ^termin ^newline]) ->> topic -> topfile;
    false -> popnewline;
    sysfileok(topfile sys_>< '') -> topfile;
    vedgetlibfilename(vedhelplist, "vedhelpname", topfile) -> topfile;
    unless topfile do
        mishap('NO HELP AVAILABLE', [% topic %])
    elseif islist(topfile) then
        hd(topfile) -> topfile
    endunless;
#_IF DEF UNIX
    if vvedgotoplace then
        consstring
          (#| `+`, dest_characters(vvedgotoplace), `\s`, explode(topfile) |#)
            -> topfile
    endif;
    sysobey('more ' <> topfile);
#_ELSE
    if sys_fname_extn(topfile) = nullstring then
        topfile <> '.' -> topfile
    endif;
    sysobey('type/page ' <> topfile);
#_ENDIF
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Jul 29 1996
        Starts at correct place in REF file (Unix only).
--- John Gibson, Dec  6 1993
        Commoned between Unix & VMS
--- Mark Rubinstein, Sep 16 1985 - lvarsed, de-sectionised and made to work
    with new (optional) help list format.
 */
