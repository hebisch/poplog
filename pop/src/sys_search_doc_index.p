/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/sys_search_doc_index.p
 > Purpose:         Search documentation index
 > Author:          John Williams, Apr 26 1989 (see revisions)
 > Documentation:
 > Related Files:   C.all/lib/include/doc_index.ph
 */


section;

#_INCLUDE 'declare.ph'
#_INCLUDE '../lib/include/doc_index.ph'

constant procedure (issubstring_lim, word_string);


define sys_parse_doc_index_entry(line);
    lvars i, j;

    define lconstant Findchar(c, i);
        locchar(c, i, line) or (datalength(line) fi_+ 1)
    enddefine;

    define lconstant Parse_integer_between(i, j) -> num;
        0 -> num;
        fast_for i from i to (j fi_- 1) do
            (num fi_* 10) fi_+ (fast_subscrs(i, line) fi_- `0`) -> num
        endfast_for
    enddefine;

    1 -> i;
    {%
    repeat 2 times                          ;;; NAME, FILE
        Findchar(`\s`, i) -> j;
        substring(i, j fi_- i, line);
        j fi_+ 1 -> i
    endrepeat;

    repeat 2 times                          ;;; HDR_START, HDR_END
        Findchar(`\s`, i) -> j;
        Parse_integer_between(i, j);
        j fi_+ 1 -> i
    endrepeat;

    fi_min(Findchar(`\s`, i), Findchar(`\n`, i)) -> j;  ;;; TXT_END
    Parse_integer_between(i, j);
    %}
enddefine;


define lconstant Add_dir(entry, dir) -> entry;
    dir dir_>< Info_file(entry) -> Info_file(entry)
enddefine;


define sys_search_doc_index(name, dir, flags) -> _n;
    lvars dev, xdir, one_only, literal, xfiles, xfile;
    lvars _len, _index, _startlim, _compute_index, _ilen;

    dlocal 0 % (0 -> dev), (if isdevice(dev) then sysclose(dev) endif) %;

    0 -> _n;
    returnunless(Has_index_dir(dir));
    Index_dir(dir) -> xdir;

    if isinteger(flags) then
        flags &&/=_0 2:01 -> literal;
        flags &&/=_0 2:10 -> one_only;
    else
        flags ->> one_only -> literal
    endif;

    /* Sort out matching */
    unless isstring(name) do
        word_string(name) -> name
    endunless;

    datalength(name) -> _len;
    returnif(_len == 0);
    1 ->> _index -> _startlim;
    false -> _compute_index;

    unless literal do
        if fast_subscrs(1, name) == `*` and _len /== 1 then
            false -> _startlim;
            if fast_subscrs(_len, name) == `*` and _len /== 2 then
                ;;; *NAME*
                _len fi_- 2 -> _len;
                substring(2, _len, name)
            else
                ;;; *NAME
                true -> _compute_index;
                _len fi_- 1 -> _len;
                substring(2, _len, name)
            endif -> name
        elseif fast_subscrs(_len, name) == `*` then
            ;;; NAME*
            _len fi_- 1 -> _len;
            substring(1, _len, name) -> name
        else
            ;;; NAME
            true -> literal
        endif
    endunless;

    if _startlim == 1 then
        [% Index_file(name) %]
    else
        All_index_files()
    endif -> xfiles;

    /* Now do it */
    setup_LINE();
    for xfile in xfiles do
        sysopen(xdir dir_>< xfile, 0, "line") -> dev;
        nextunless(dev);
        until fast_sysread(dev, 1, LINE, LLEN) == 0 do

            /* Get size of first item in LINE */
            unless (locchar(`\s`, 1, LINE) ->> _ilen) do
                mishap(LINE, 1, 'MALFORMED DOC_INDEX ENTRY')
            endunless;
            _ilen fi_- 1 -> _ilen;

            /* Check alphabetic ordering (if matching NAME or NAME*) */
            quitif(_startlim
                        and
                    fast_subscrs(1, LINE) fi_> fast_subscrs(1, name));

            /* Compare lengths (if matching NAME) */
            nextif(literal and _ilen /== _len);

            /* Compute search start index (if matching *NAME) */
            if _compute_index then
                _ilen fi_- _len fi_+ 1 -> _index;
                nextif(_index fi_< 1)
            endif;

            /* Search */
            if issubstring_lim(name, _index, _startlim, _ilen, LINE) then
                _n fi_+ 1 -> _n;
                Add_dir(sys_parse_doc_index_entry(LINE), dir);
                quitif(one_only)
            endif
        enduntil
    endfor
enddefine;


endsection;

/* --- Revision History ---------------------------------------------------
--- John Williams, Jan  5 1996
        sys_search_doc_index third argument can now be a integer containing
        two flags: bit 0 says whether to try matching; bit 1 says whether
        to return after finding the first relevant index entry.
--- John Gibson, Apr  2 1992
        Removed vd*declare.ph
--- Robert John Duncan, Aug 20 1991
        Changed test for existence of index to use -Has_index_dir-
--- John Williams, Aug 24 1990
        Now expects -Info_file- to be just a filename; adds the directory
        at search time. Allows 'ref' directories to be moved.
--- Robert John Duncan, Aug  7 1990
        Allowed newline as terminator in -sys_parse_doc_index_entry-
--- John Williams, May 26 1989
        Added -sys_parse_doc_index_entry-
--- John Williams, May 11 1989
        Re-written using -issubstring_lim-
 */
