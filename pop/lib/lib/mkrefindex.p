/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/lib/mkrefindex.p
 > Purpose:         Construct index from REF files
 > Author:          John Williams, Apr 26 1989 (see revisions)
 > Documentation:   HELP * MKREFINDEX
 > Related Files:   C.all/lib/include/doc_index.ph
 */

section;

compile_mode:pop11 +strict;

/* Load utilities */

max(300000, popmemlim) -> popmemlim;

#_INCLUDE '$usepop/pop/lib/include/doc_index.ph'
#_INCLUDE '$usepop/pop/lib/include/sysdefs.ph'


/* Parsing identifier descriptions */

/* Note: the procedures description_hdr_start and description_hdr_end
    assume that special space characters (\Sf and \Sp) at the beginning
    of a line have been replaced by ordinary spaces.
*/

define global vars description_hdr_start(line);
    lvars len line;
    datalength(line) -> len;
    len fi_>= 72
        and fast_subscrs(len, line) == `]`
        and fast_subscrs(1, line) /== `\s`
enddefine;


define global vars description_hdr_end(line);
    lvars line;
    line /= nullstring and skipchar(`\s`, 1, line) == 9
enddefine;


define global vars description_hdr_name(line) -> item;
    lvars item items line next;
    returnif(line = nullstring)(nullstring -> item);

    define lconstant Tokenise(line);
        lvars c dstring i line n procedure (subscr);
        false -> dstring;
        0 -> n;

        define lconstant Make_token();
            if n > 0 then
                (if dstring then consdstring else consstring endif)(n)
            endif;
            false -> dstring;
            0 -> n;
        enddefine;

        if isdstring(line) then
            subscrdstring
        else
            subscrs
        endif -> subscr;

        [% for i from 1 to datalength(line) do
            subscr(i, line) -> c;
            if strmember(c, '\t\s\n') then
                Make_token();
            elseif strmember(c, '[]().,;') then
                Make_token();
                consstring(c, 1)
            else
                c;
                if c > 255 then
                    true -> dstring
                endif;
                n + 1 -> n
            endif
        endfor, Make_token() %]
    enddefine;

    define lconstant Isparameter(item);
        lvars c item;
        if isdstring(item)
        and item(1) &&/=_0 `\[i]` then
            true
        else
            for c in_string item do
                unless isuppercode(c) or isnumbercode(c) or c == `_` do
                    return(false)
                endunless
            endfor;
            true
        endif
    enddefine;

    Tokenise(line) -> items;
    dest(items) -> items -> item;
    while Isparameter(item) do
        dest(items) -> items -> next;
        if next = '->' or next = '[' then
            quitloop
        else
            next -> item
        endif
    endwhile;
enddefine;


/* Collect entries from one REF file */

define global vars mkrefindex1(file);
    lvars file fname infos line n_infos num procedure rep;

    define lconstant No_ss_line_rep(file);
        lvars file procedure rep;
        vedfile_line_repeater(file, false) -> rep;

        procedure() -> line;
            lvars line;
            rep() -> line;
            if line /== termin
            and line /= nullstring
            and strmember(fast_subscrs(1, line), '\Sf\Sp\Ss\St') then
                `\s` -> line(1);
            endif;
        endprocedure;
    enddefine;

    sys_fname_name(file) -> fname;
    [] -> infos;
    0 ->> num -> n_infos;
    No_ss_line_rep(file) -> rep;

    define lconstant Start_info();
        {% description_hdr_name(line), fname, num, num, num %} :: infos -> infos;
    enddefine;

    define lconstant End_info(num);
        lvars info num xfile;
        for info in infos do
            num -> Info_txt_end(info);
            n_infos + 1 -> n_infos;
            info
        endfor;
        [] -> infos
    enddefine;

    define lconstant Bad_file_err(num, type);
        lvars info, num, type;
        dlocal popfilename poplinenum;
        front(infos) -> info;
        End_info(num);
        Info_file(info) -> popfilename;
        num -> poplinenum;
        sysflush(popdevout);
        sysprmessage(Info_name(info), 1,
                        'Malformed REF entry ' <> type, 'WARNING -', 3);
        cucharerr(`\n`);
        sysflush(popdeverr);
    enddefine;

    define lconstant Nextline(rep) -> line;
        lvars line rep;
        fast_apply(rep) -> line;
        if line == termin then
            if infos /== [] then
                Bad_file_err(num, '(Unexpected EOF)');
            endif;
            exitfrom(n_infos, mkrefindex1);
        else
            num + 1 -> num;
        endif;
    enddefine;


    repeat
FIND_HDR_START:
        until description_hdr_start(Nextline(rep) ->> line) do enduntil;
HDR_START:
        Start_info();
FIND_HDR_END:
        until description_hdr_end(Nextline(rep) ->> line) do
            if line = nullstring then
                goto GOT_ONE_BLANK
            elseif description_hdr_start(line) then
                goto HDR_START
            else
                num -> Info_hdr_end(front(infos))
            endif
        enduntil;
FIND_TXT_END:
        until (Nextline(rep) ->> line) = nullstring do
            if description_hdr_start(line) then
                Bad_file_err(num - 1, '(New entry starts unexpectedly)');
                goto HDR_START
            endif
        enduntil;
GOT_ONE_BLANK:
        Nextline(rep) -> line;
        if line = nullstring then
            End_info(num - 2)
        elseif line(1) == `\s` then
            goto FIND_TXT_END
        else
            Bad_file_err(num - 2, '(Bad line after blank line)');
            if description_hdr_start(line) then
                goto HDR_START
            endif
        endif
    endrepeat
enddefine;


/* Process a whole set of REF files */

define lconstant Info_before(i, j);
    lvars i j x y;
    Info_name(i) -> x;
    Info_name(j) -> y;
    if x = y then
        Info_file(i) -> x;
        Info_file(j) -> y;
        if x = y then
            Info_hdr_start(i) < Info_hdr_start(j)
        else
            alphabefore(x, y)
        endif
    else
        alphabefore(x, y)
    endif
enddefine;


define global mkrefindex(dir);
    lvars dir file info xfile procedure xfile_infolist;
    newmapping([], 32, [], false) -> xfile_infolist;

    dir dir_>< nullstring -> dir;
    for file from_repeater
        #_IF DEF VMS
            sys_file_match(dir, '*.;0', false, false)
        #_ELSE
            sys_file_match(dir, '*#', false, false)
        #_ENDIF
    do
        unless sysisdirectory(file)
        or isendstring('index', sys_fname_nam(file))
        then
            repeat mkrefindex1(file) times
                -> info;
                Index_file(Info_name(info)) -> xfile;
                info :: xfile_infolist(xfile) -> xfile_infolist(xfile)
            endrepeat;
        endunless;
    endfor;

    appproperty(xfile_infolist,
                procedure(xfile, infolist);
                    lvars infolist xfile;
                    syssort(infolist, false, Info_before)
                        -> xfile_infolist(xfile)
                endprocedure);

    appproperty(xfile_infolist,
                procedure(xfile, infolist);
                    lvars info infolist xfile;
                    dlocal cucharout, pop_pr_quotes = false;
                    discout(Index_dir(dir) dir_>< xfile) -> cucharout;
                    for info in infolist do
                        appdata(info, spr);
                        cucharout(`\n`)
                    endfor;
                    cucharout(termin)
                endprocedure)
enddefine;

endsection;

#_TERMIN_IF not(pop_runtime)


/* Automatic startup if loaded from the command line */

section;

if poparg1 and sys_fname_nam(poparg1) = 'mkrefindex' then
    1 ->> popsyscall -> pop_file_versions;
    ['mkrefindex' ^^poparglist] =>
    applist(poparglist, mkrefindex);
endif;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jan  9 1996
        description_hdr_start now insists that ] must be at column 72
        (or greater).
--- John Williams, Jan  5 1996
        Allows for the fact that poparg1 might be false.
--- John Williams, Jul 19 1993
        Now copes with bold \Sf at beginning of lines as well!
        Also, improved error reporting.
--- John Williams, Jul  1 1993
        Now copes with \Sf and \Sp at beginning of lines
--- Adrian Howard, May  7 1993
        Made sure vedfile_line_repeater always reads in dstrings
--- John Williams, May  6 1993
        Fixed description_hdr_name to cope with new REF file format
        (parameters in italics etc)
--- Adrian Howard, Mar 30 1993
        Altered change of popmemlim so it can only increase
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF
--- Simon Nichols, Jun  8 1992
        Changed -args- to -poparglist- in automatic startup code.
--- John Williams, Apr 23 1992
        Now uses -poparg1-
--- John Williams, Nov 28 1991
        Can now re-define the procedure (-mkrefindex1-) that processes
        each file
--- Robert John Duncan, Aug 21 1991
        Excluded files named '*index' from list of files to be processed
--- John Gibson, Aug  2 1991
        Added include for sysdefs
--- Robert John Duncan, Apr 30 1991
        Improved "loaded_on_startup" test
--- John Williams, Mar 19 1991
        Use of -sys_file_match- now VMS compatible.
--- John Williams, Nov 14 1990
        New user-tailorable version
--- John Williams, Oct 26 1990
        Improved error signalling
--- John Williams, Aug 24 1990
        Improved "loaded_on_startup" test
--- John Williams, Aug 24 1990
        Now only stores filename in index; directory is added at search
        time by -sys_search_doc_index-. Allows 'ref' directories to be
        relocated.
--- John Williams, Apr  9 1990
        Uses -poparglist0- to determine whether to run or not.
        Better rules for distinguishing identifier and parameter names.
--- John Williams, Apr  3 1990
        Changed -sysfullfilename- to -sys_fname_name-
--- John Williams, Mar 28 1990
        -Start_info- no longer confused by all uppercase non-procedure
        names (e.g. datatypes).
--- John Williams, Aug  4 1989
        Fixed bug in -At_header_start-
--- John Williams, May 26 1989
        No longer outputs single quotes in index
--- John Williams, May 17 1989
        Now uses -sys_file_match- instead of -list_of_files_matching-
 */
