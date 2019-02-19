/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/src/syscomp/w_util.p
 > Purpose:
 > Author:          John Gibson, Dec 24 1987 (see revisions)
 */

/* -------------------------------------------------------------------------

                UTILITIES FOR PROCESSING W-FILES & LIBRARIES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'
#_INCLUDE 'wdefs.ph'

section $-Popas;

define sysread_check(dev, buf, n);
    lvars n, dev, buf;
    unless f_sysread(dev, 1, buf, n) == n then
        mishap(device_full_name(dev), 1, 'PREMATURE EOF ON INPUT FILE')
    endunless
enddefine;

lconstant word_buf =  writeable inits(WINT_BYTES);

define read_next_int(dev, err);
    lvars dev, err;
    if f_sysread(dev, 1, word_buf, WINT_BYTES) == WINT_BYTES then
        subscr_nbyte(1, word_buf, WINT_BYTES)
    elseif err then
        mishap(device_full_name(dev), 1, 'PREMATURE EOF ON INPUT FILE')
    else
        false
    endif
enddefine;

define write_next_int(val, dev);
    lvars val, dev;
    val -> subscr_nbyte(1, word_buf, WINT_BYTES);
    f_syswrite(dev, 1, word_buf, WINT_BYTES)
enddefine;

define get_wlib_name(wlib) -> wlib;
    lvars wlib, fname = sysfileok(wlib), n, rel;
    returnunless(is_dir_path(fname) and fname /= nullstring);

#_IF DEF VMS
    lconstant pop = '[pop.', cdel = '[.]', dot = nullstring, dotdot = '-';
    lvars rel = subscrs(1,fname) == `[`;
#_ELSEIF DEF UNIX
    lconstant pop = 'pop/', cdel = '/', dot = '.', dotdot = '..';
    lvars rel = subscrs(1,fname) /== `/`;
#_ELSEIF DEF WINDOWS
    lconstant pop = 'pop\\', cdel = '\\', dot = '.', dotdot = '..';
    lvars (,,rel,,,) = sysfileok(fname, true);
    rel == 1 and subscrs(1,fname) /== `\\` -> rel;
#_ELSE_ERROR
#_ENDIF

    if rel then current_directory dir_>< fname -> fname endif;

    lvars usepop = sysfileok('$usepop/');
#_IF DEF UNIX
    ;;; in case usepop ended with a /
    if isendstring('//', usepop) then allbutlast(1, usepop) -> usepop endif;
#_ENDIF
    lvars usepoppop = usepop <> pop;
    if isstartstring(usepoppop, fname)
    and (allbutfirst(datalength(usepoppop), fname) ->> fname) /= nullstring
    then
        ;;; map subdirs of $usepop/pop/ to $popobjlib/name.wlb
        fast_for n to datalength(fname) do
            if strmember(fast_subscrs(n,fname), cdel) then
                `\s` -> fast_subscrs(n,fname)
            endif;
        endfor;
        false;
        sys_parse_string(fname,
                procedure(s);
                    lvars s;
                    if s = dotdot and dup() then ->
                    elseunless s = dot then s
                    endif
                endprocedure);
        WLB_EXTENSION -> fname;
        while () ->> n do n <> fname -> fname endwhile;
        '$popobjlib/' dir_>< fname -> wlib
    else
        wlib dir_>< STANDARD_WLIB_NAME -> wlib
    endif
enddefine;

define open_w_input(file_name, nonexist_err, kind);
    lvars magicnum, kind, dev, file_name, nonexist_err, version;
    get_wlib_name(file_name) -> file_name;
    if sysopen(file_name, 0, true) ->> dev then
        ;;; read magic number
        read_next_int(dev, true) -> magicnum;
        if kind == "w" then
            unless magicnum == WFILE_MAGIC then
                mishap(file_name, 1, 'FILE IS NOT A W-FILE')
            endunless
        elseif kind == "wlib" then
            unless magicnum == WLIBR_MAGIC then
                mishap(file_name, 1, 'FILE IS NOT A W-LIBRARY')
            endunless
        else
            unless magicnum == WFILE_MAGIC or magicnum == WLIBR_MAGIC then
                mishap(file_name, 1, 'FILE IS NOT A W-FILE OR W-LIBRARY')
            endunless
        endif;
        if magicnum == WLIBR_MAGIC then
            ;;; file format version number (currently unused)
            read_next_int(dev, true) -> version;
            consvector(dev, false, false, version, 4)
        else
            dev
        endif
    elseif nonexist_err then
        mishap(file_name, 1, 'CAN\'T OPEN FILE' <> sysiomessage())
    else
        false
    endif
enddefine;

define wlib_device(wlib);
    lvars wlib;
    f_subv(1, wlib)
enddefine;

define w_open_name(winput);
    lvars winput;
    if isvector(winput) then
        wlib_device(winput) -> winput
    endif;
    device_open_name(winput)
enddefine;

define lconstant get_wlib_table(wlib, read_in, vsub, offs) -> table;
    lvars table, vsub, wlib, offs, read_in;

    define lconstant get_wlib_tab(ldev, read_in, offs);
        lvars ldev, offs, tab_pos, read_in;
        ;;; seek to int pointer to table
        sysseek(ldev, offs, 0);
        ;;; where table starts
        read_next_int(ldev, true) -> tab_pos;
        if read_in then
            ;;; return vector containing whole table
            ;;; (first int is number of entries)
            sysseek(ldev, tab_pos, 0);
            {%  fast_repeat read_next_int(ldev, true) times
                    read_next_int(ldev, true)
                endrepeat
            %}
        else
            ;;; return subscripting procedure into table
            procedure(n);
                lvars n;
                sysseek(ldev, tab_pos+(n*WINT_BYTES), 0);
                read_next_int(ldev, true)
            endprocedure
        endif
    enddefine;

    unless f_subv(vsub,wlib) ->> table then
        get_wlib_tab(f_subv(1,wlib), read_in, offs) ->> table
                                                    -> f_subv(vsub,wlib)
    endunless
enddefine;

define wlib_modtab   = get_wlib_table(% 2, MODTAB_PTR %) enddefine;
define wlib_symindex = get_wlib_table(% 3, INDEX_PTR %) enddefine;


    /*  Dictionary hashing in the system use the procedure Bytevec_hashint
        (in src/vec_generic.p) -- any changes must be be made to both.
    */
define word_dict_cell(word);
    lvars string = fast_word_string(word), len = datalength(string), word;
#_IF false
    if len fi_> 2 then
        ;;; use first, middle and last chars and length
        f_subs(1,string) fi_+ len
        fi_+ ((f_subs(len,string) fi_+ len) fi_<< 3)
        fi_+ ((f_subs((len fi_>> 1) fi_+ 1,string) fi_+ len) fi_<< 6) -> len;
        (len fi_>> 10) fi_+ len
    elseif len == 2 then
        ;;; use first and last chars
        f_subs(1,string) fi_+ (f_subs(2,string) fi_<< 3)
    elseif len == 1 then
        ;;; single character word: just use first character
        f_subs(1,string)
    else
        0
    endif -> len;
#_ELSE
    lvars res = len, i = 1;
    while len fi_> 3 do
        (f_subs(i, string) fi_+ (f_subs(i fi_+ 1, string) fi_<< 8) 
           fi_+ (f_subs(i fi_+ 2, string) fi_<< 16))
           + (f_subs(i fi_+ 3, string) << 24) + res -> res;
        len fi_- 4 -> len;
        i fi_+ 4 -> i;
    endwhile;
    if len == 3 then
        (f_subs(i, string) fi_+ (f_subs(i fi_+ 1, string) fi_<< 8) 
           fi_+ (f_subs(i fi_+ 2, string) fi_<< 16))
             + res -> res;
    elseif len == 2 then
        (f_subs(i, string) fi_+ (f_subs(i fi_+ 1, string) fi_<< 8))
             +  res -> res;
    elseif len == 1 then
        f_subs(i, string) + res -> res;
    endif;
    ;;; Mix bits
    ((res && 16:FFFE0000) >> 17) ||/& res -> res;
#_ENDIF
    (res && 16:3FF) fi_+ 1
enddefine;

define path_index_cell(pathstr, word);
    lvars pathstr, word;
    word_dict_cell(word), if pathstr /= nullstring then () fi_+ 1024 endif
enddefine;

define search_wlib_index(pathname, word, wlib, popc);
    lvars   n, namelen, ldev, buf = w_buffer, pathname, pathsubs, word,
            wordlen, file_pos, wlib, popc, count, modnum;

    if isword(pathname) then fast_word_string(pathname) -> pathname endif;
    wlib_symindex(wlib,not(popc))(path_index_cell(pathname, word)) -> file_pos;
    returnif(file_pos == 0) (false);    ;;; nothing in cell
    wlib_device(wlib) -> ldev;
    sysseek(ldev, file_pos, 0);     ;;; seek to start of chain

    ;;; string to search for
    datalength(word) -> wordlen;
    wordlen fi_+ datalength(pathname) -> namelen;
    WLINDX_NAME fi_+ wordlen -> pathsubs;

    ;;; read first entry length -- thereafter, succeeding entry lengths are
    ;;; are read with preceding entry (means only 1 sysread per entry).
    2 -> n;
    sysread_check(ldev, buf, n);

    repeat
        ;;; get entry length + 2 for next entry length
        (f_subs(n,buf) fi_<< 8) fi_+ f_subs(n fi_- 1,buf) fi_+ 2 -> n;
        ;;; last entry in chain has length 0
        returnif(n == 2) (false);
        ;;; read next entry
        sysread_check(ldev, buf, n);

        quitif(
            f_subs(WLINDX_NAME_LEN,buf) == namelen
            and issubstring_lim(word, WLINDX_NAME, WLINDX_NAME, false, buf)
            and issubstring_lim(pathname, pathsubs, pathsubs, false, buf)
        )
    endrepeat;

    ;;; found -- return module number if non-zero for entry or false if zero

    subscr_nbyte(WLINDX_MOD_NUM, buf, 2) -> modnum; ;;; mod num
    if modnum == 0 then false -> modnum endif;
    returnunless(popc == 1) (modnum);

    ;;; for POPC first try, return identprops and perm flags as well

    ;;; identprops
    subscr_nbyte(WLINDX_IDPROPS, buf, 3);
    ;;; perm flags
    if dup(f_subs(WLINDX_PRMFLAGS, buf)) &&/=_0 IDT_INCREMENTAL then
        WLINDX_NAME+namelen -> n;
        f_subs(n,buf);                      ;;; flags
        f_subs(n+1,buf) -> count;
        ;;; vector of words
        consvector(wbuf_extract_strwords(n+2, count, true) ->, count)
    else
        false, false
    endif;
    modnum, true
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov 10 1995
        Fixed get_wlib_name to cope with Unix $usepop ending with a `/`
--- Robert John Duncan, Sep  5 1994
        Added case to get_wlib_name for Windows pathnames
--- John Gibson, Jul  8 1993
        Added get_wlib_name
--- John Gibson, May 27 1993
        Changes to search_wlib_index
--- John Gibson, Oct 13 1992
        14.22 changes
--- John Gibson, Sep 22 1992
        Added runtime initialisation of w_buffer
--- John Gibson, Apr 10 1992
        Improved dictionary hashing algorithm in -word_dict_cell-
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Version 13.66
--- John Gibson, Dec  5 1988
        This file and w_module.p replace old wcommon.p
--- John Gibson, Oct  9 1988
        Changes to allow w-library modules to have dates
--- John Gibson, Feb 21 1988
        Revised comment above -get_dict_cell- about dictionary hashing in
        the system.
--- John Gibson, Feb  9 1988
        Various revisions
 */
