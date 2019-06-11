/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_noprompts.p
 > Purpose:         Remove special prompts from IM file
 > Author:          Mark Rubinstein, Nov 18 1985 (see revisions)
 > Documentation:   HELP * VED_NOPROMPTS
 > Related Files:
 */

section;

define lconstant Remove_prompts(lo, hi);
    lvars lo, hi, n, line, i, j;
    0 -> n;
    fast_for lo from lo to hi do
        subscrv(lo, vedbuffer) -> line;
        1 -> i;
        while (locchar(`\Sp`, i, line) ->> j) do
            `\s` -> fast_subscrs(j, line);
            j fi_+ 1 -> i;
            n fi_+ 1 -> n
        endwhile;
    endfast_for;
    if vedchanged then
        vedchanged fi_+ n -> vedchanged
    elseif n fi_> 0 then
        n -> vedchanged
    endif;
    vedputmessage(n sys_>< ' prompts removed')
enddefine;


define global ved_noprompts();
    Remove_prompts(1, vvedbuffersize)
enddefine;


define global ved_nopromptsmr();
    Remove_prompts(vvedmarklo, vvedmarkhi)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Sep 12 1994
        Fixed BR isl-fr.4555 by changing vvedpromptchar (which can sometimes
        be false) to `\Sp`.
--- John Williams, Jun 27 1990
        Moved from Sussex $poplocal to masters
--- John Williams, Dec  7 1989
        Fixed BR tomk.62; also tidied up alot.
 */
