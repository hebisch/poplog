/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_wiggle.p
 > Purpose:         Automatic bracket wiggling utility
 > Author:          John Williams, Oct 21 1988 (see revisions)
 > Documentation:   HELP * LISPVED
 > Related Files:   C.all/lib/ved/ved_wmp.p
 */
compile_mode :pop11 +strict;

section;

global vars vedwiggling = true;

define lconstant active Vedfiletype;
    sys_fname_extn(vedcurrent)
enddefine;

define lconstant Vdautowiggle(filetypes);
    vedinsertvedchar();
    if vedwiggling
    and not(vedonstatus)
    and fast_member(Vedfiletype, filetypes) do
        procedure();
            dlocal vedcolumn;
            vedcharleft();
            ved_wmp()
        endprocedure()
    endif
enddefine;


/* Main procedure for setting up wiggling.
    c is closing bracket character (or 1-element string),
    filetypes is list of file types for which that character should wiggle.
*/

define vedsetwiggle(c, filetype);
    lvars key, pdr, filetypes;

    if isstring(c) and datalength(c) == 1 then
        c -> key;
        fast_subscrs(1, c) -> c
    elseif isinteger(c) then
        consstring(c, 1) -> key
    else
        vederror('Bracket character needed (e.g. [)')
    endif;

    vednormaltable(c) -> pdr;
    if pdpart(pdr) == Vdautowiggle then
        fast_frozval(1, pdr) -> filetypes;
        if fast_member(filetype, filetypes) then
            ncdelete(filetype, filetypes) -> filetypes;
            if filetypes == [] then
                vedsetkey(key, vedinsertvedchar);
                return
            endif
        else
            conspair(filetype, filetypes) -> filetypes
        endif;
        filetypes -> fast_frozval(1, pdr)
    else
        unless pdr == vedinsertvedchar do
            vederror('Cannot make "' <> key <> '" a wiggly key')
        endunless;
        vedsetkey(key, writeable Vdautowiggle(% [^filetype] %))
    endif
enddefine;


/* <ENTER> wiggle <bra><ket>
    e.g. <ENTER> wiggle ()
    Invokes vedsetwiggle and reports status of <ket>.
    <bra> is effectively ignored but retained for upward compatability.
*/

define vars ved_wiggle();
    lvars c, filetype = Vedfiletype, pdr, filetypes;

    if vedargument = nullstring then
        fast_chain('set vedwiggling', veddo)
    endif;

    unless datalength(vedargument) == 2 do
        vederror('Two "bracket" characters needed (e.g. [])')
    endunless;

    fast_subscrs(2, vedargument) -> c;
    vedsetwiggle(c, filetype);
    vednormaltable(c) -> pdr;
    if pdpart(pdr) == Vdautowiggle then
        fast_frozval(1, pdr)
    else
        []
    endif -> filetypes;

    lconstant KEY_MESS = writeable 'The "X" key will ';
    c -> fast_subscrs(6, KEY_MESS);

    vedputmessage(
        KEY_MESS
        <> (if fast_member(filetype, filetypes) then
                'wiggle in '
            else
                'not wiggle in '
            endif)
        <> (if filetypes == [] then 'any' else filetype endif)
        <> ' files')
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Dec  6 1995
        Rewritten to provide vedsetwiggle, which users can call in their
        `vedinit.p' file if desired.
--- John Gibson, Aug  4 1989
        Now uses -sys_fname_extn- instead of -sysfiletype-
 */
