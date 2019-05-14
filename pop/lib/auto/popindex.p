/*  --- Copyright University of Sussex 1997.  All rights reserved. ---------
 >  File:           C.all/lib/auto/popindex.p
 >  Purpose:        create an index of pop source and library files
 >  Author:         Chris Slymon, 1983 (see revisions)
 >  Documentation:  HELP * POPINDEX
 >  Related Files:  LIB * VED_SOURCEFILE   $usepop/pop/com/mkind
 */
compile_mode:pop11 +strict;

/*
The index does not contain entries for certain items, which are not DEFINEd
in the fullest possible sense... e.g showlib

global vars macro showlib;
    ved_showlib -> nonmac showlib;
*/

section ;

/* suffix list must be consistent with choose_index*/
global vars popindex_suffixlist = [
    'ac' 'df' 'gk' 'lo' 'pp' 'qr' 'ss' 'tu' 'vv' 'wz' 'AF' 'GR' 'SX' 'YZ'
];

global vars popindex_filename;
unless isstring(popindex_filename) then
    sysfileok('$usepop/pop/ref/popindex') nc_<> '.' -> popindex_filename
endunless;

define global choose_index(item);
    lvars c = item(1), suff;
    unless isalphacode(c) then `Z` -> c endunless;
    fast_for suff in popindex_suffixlist do
        if c >= fast_subscrs(1, suff) and fast_subscrs(2, suff) >= c then
            return(suff);
        endif
    endfor;
    mishap('INDEX FILE NOT FOUND', [^item]);
enddefine;


/* returns a list of the entries beginning with <item>, either a string or
    a word, from the relevant index */

define global popindex(item) -> entries;
    lvars char1, char, lim, len, c, n, index_name, device,
        len, item, entries;
    lconstant buffer = writeable inits(512);
    if isword(item) then
        fast_word_string(item) -> item
    endif;
    popindex_filename sys_>< choose_index(item) -> index_name;
    unless (sysopen(index_name, 0, "record") ->> device) then
        mishap('INDEX FILE MISSING', [^index_name]);
    endunless;
    datalength(item) -> lim;
    item(1) -> char1;
    ;;; look for line starting with the characters
    [%until (sysread(device, buffer, 512) ->> len) == 0 do
             fast_subscrs(1, buffer) -> char;
             if lim fi_< len  then
                 if char == char1 then ;;; line starts with right character
                     1 -> n;
                     repeat
                         if n == lim then
                            substring( 1, len fi_- 1, buffer); ;;; found it
                            quitloop();
                         else n fi_+ 1 -> n
                         endif;
                         fast_subscrs(n, buffer) -> c;
                         fast_subscrs(n,item) -> char;
                         if c fi_< char then quitloop(1);        ;;; not there yet
                         elseif c /== char then
                             quitloop(2) ;;; past it
                         endif;
                     endrepeat
                 elseif char fi_> char1 then quitloop() ;;; past it
                 endif;
             endif;
         enduntil%] -> entries;
    sysclose(device);
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Apr 18 1997
        Argument to popindex can now be a word.
--- Robert John Duncan, Dec  6 1995
        Extended popindex_suffixlist and fixed choose_index accordingly.
--- John Gibson, Oct 10 1992
        Added strict
--- John Williams, Jul 23 1990
        Changed subscrs(1, item) back to item(1), because item is sometimes
        a word!
--- Aaron Sloman, May 20 1990
    Made some efficiency improvements. Rationalised, and put in top
    level section now, because of use of lvars.
--- Robert Smith, Jul  1 1988 - '.' appended to default -popindex_filename-
    after passing through -sysfileok-, as VMS version strips trailing decimal
    point. Will cause a problem if -popindex_filename- is ever exported, as
    documentation says it should be.
--- Aaron Sloman, Sep 28 1986 slightly speeded up. tabified
--- Mark Rubinstein, May 24 1985 - altered to use sys_><
--- Aaron Sloman, Feb 1985 - popindex_filename introduced - to generalise use
    of popindex.  suffix_list altered to produce more/shorter files, and
    choose_index generalised.
 */
