/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_do_text_action.p
 > Purpose:         Display or execute action from active text (or doc ref)
 > Author:          John Gibson, Nov  4 1995 (see revisions)
 > Documentation:   REF * VEDPROCS
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

include ved_do_text_action.ph;
include itemise.ph;


define lconstant parse_action(action, mode)
                                -> (action, message, hilite, set_cursor);
    lvars   c, action, mode, message = pop_undef, hilite = false,
            set_cursor = false, n = 0, len, whole = true, m, chars,
            string = nullstring;

    recursive_valof(action) -> action;
    if ispair(action) then destpair(action) -> (action, string) endif;
    if isstring(action) then
        action -> string
    elseunless isprocedure(action) then
        pop_undef -> action
    endif;
    datalength(string) -> len;

    if len /== 0 and subscrs(1,string) == `(` then
        fast_for n from 2 to len do
            fast_subscrs(n,string) -> c;
            quitif(c == `)`);
            if c == `S` or c == `L` then
                c -> set_cursor
            elseif c == `M` then
                true -> message
            elseif c == `N` then
                false -> message
            else
                if c == `i` then
                    VDTA_DISP_INVERT
                elseif c == `b` then
                    VDTA_DISP_BOX
                elseif c == `p` then
                    VDTA_DISP_POINTER
                else
                    vederror('INVALID TEXT ACTION STRING SYNTAX')
                endif || (hilite or 0) -> hilite;
            endif
        endfor;
        false -> whole
    endif;

    isstring(action) and n /== len and fast_subscrs(n+1,action) == `>`
                                    -> chars;

    unless hilite then
        if isstring(action) and not(chars) then
            VDTA_DISP_POINTER
        else
            VDTA_DISP_INVERT
        endif -> hilite
    endunless;

    if not(chars) and issubstring(';;;', string) ->> m then
        message and allbutfirst(m+2, string) -> message;
        m-1 -> m;
        false -> whole
    else
        len -> m
    endif;

    returnunless(isstring(action));

    if mode == VDTA_MODE_DISPLAY then
        if chars then
            [] -> action
        elseif message == pop_undef then
            true -> message
        endif;
        if message == true then
            ;;; default message is command
            if whole then action else substring(n+1, m-n, action) endif
                -> message
        endif
    else
        if chars then
            [%  fast_for n from n+2 to len do
                    fast_subscrdstring(n,action)
                endfor
            %] -> action
        else
            unless whole then
                substring(n+1, m-n, action) -> action
            endunless
        endif
    endif
enddefine;      /* parse_action */

define lconstant try_active_data(line, col, type, mode);
    lvars   type, mode, data, line, col;

    define lconstant do_action(action, line, col, type, mode);
        lvars   action, message, type, mode, hilite, set_cursor,
                line, col, org_line = line, t;

        define lconstant exec_action(action, line, col, set_cursor);
            lvars action, line, col, set_cursor, save_line, save_col;
            unless isinteger(line) then false -> set_cursor endunless;
            if set_cursor then
                vedline, vedcolumn -> (save_line, save_col);
                vedjumpto(line, col)
            endif;
            if isstring(action) then
                lconstant comm_ref = consref(0);
                dlocal vedcommand = comm_ref, vedlastcommand;
                action -> fast_cont(comm_ref);
                veddocommand()
            else
                action()
            endif;
            if set_cursor == `L` then
                vedjumpto(save_line, save_col)
            endif
        enddefine;

        if action = nullstring then
            ';;;UNCONNECTED CONTINUATION SEGMENT' -> action
        endif;
        parse_action(action, mode)
                        -> (action, message, hilite, set_cursor);

        if isstring(action) or isprocedure(action) then
            if isstring(action) then VDTA_TYPE_COMM else VDTA_TYPE_PDR endif
                                                            -> t;
            returnunless(type &&/=_0 t) (false);
            t;
            if mode == VDTA_MODE_EXECUTE then
                chain(action, line, col, set_cursor, exec_action)
            elseunless mode == VDTA_MODE_DISPLAY then
                exec_action(%action, line, col, set_cursor%) -> action
            endif
        elseif islist(action) then
            ;;; character input
            returnunless(type &&/=_0 VDTA_TYPE_CHARS) (false);
            VDTA_TYPE_CHARS
        else
            returnif(mode == VDTA_MODE_DISPLAY) (false);
            vederror('INVALID TEXT ACTION')
        endif -> type;

        if mode == VDTA_MODE_DISPLAY then
            ;;; display only (don't display message if on statusline)
            if isstring(message) and isinteger(org_line) then
                vedputmessage(message);
                hilite || VDTA_DISP_DID_MESS -> hilite
            endif;
            hilite
        else
            ;;; add action as input
            if islist(action) then
                action nc_<> ved_char_in_stream
            else
                action :: ved_char_in_stream
            endif -> ved_char_in_stream;
            type
        endif
    enddefine;

    ;;; Use chain because it resets vedline etc
    if ved_text_action_data(line, col) -> (line, col) ->> data then
        chain(data, line, col, type, mode, do_action, chain)
    else
        col     ;;; true value if on active char
    endif
enddefine;      /* try_active_data */

define vars ved_do_text_action(wline, wcol, type, mode);
    lvars   doctype, len, item, wline, wcol, type,
            mode, arg, gotoplace;
    dlocal  vedcolumn, vedline, vvedlinesize, poplastchar, ved_on_status;

    define lconstant skip_spaces();
        while strmember(vedcurrentchar(), '\s\Ss\t')
        and vedcolumn < vvedlinesize do
            vedcharright()
        endwhile
    enddefine;

    define lconstant get_doctype() -> (doctype, nline, ncol);
        lvars   col = vedcolumn, doctype = false, item, line = vedline,
                fprops, tmp, nline = line, ncol = col, last_*_col = false,
                dt;

        ;;; Treat string quotes (apostrophes), '.' and '-' as alphabetic
        define lconstant local_chartypes =
            applist(% [`'` `.` `-`], item_chartype %)
        enddefine;

        dlocal  vedcolumn = 1, vedline, vvedlinesize,
                3 % local_chartypes() % = dup(dup(ITM_ALPHA));

        if col <= vvedlinesize then
            while vedline == line and (vedcolumn->>tmp) <= col
            and (vedmoveitem() ->> item) /== termin do
                vedgetsysfilepdr(item) -> dt;
                if vedcolumn > col then
                    if dt and vednextitem() == "*" then
                        dt -> doctype;
                        vedline -> nline;
                        vedcolumn -> ncol
                    else
                        (item /== "*" and last_*_col) or tmp -> ncol
                    endif
                elseif item == "*" then
                    tmp -> last_*_col
                else
                    if dt then dt -> doctype endif;
                    false -> last_*_col
                endif
            endwhile
        endif;

        if doctype then
            doctype
        elseif isword(vedfileprops ->> fprops)
        and (vedgetsysfilepdr(lowertoupper(fprops)->>fprops) ->> item)
        and not(fast_lmember(fprops,[INCLUDE LIB SRC]))
        then
            item
        else
            "ved_help"
        endif -> doctype
    enddefine;


    false -> ved_on_status;
    returnunless(   1 fi_<= wline and wline fi_<= vedwindowlength
                and 1 fi_<= wcol and wcol fi_<= vedscreenwidth) (false);

    if wline == 1 then
        ;;; on status line -- try vedstatusline for active
        try_active_data(vedstatusline, wcol, type, mode) -> ;
        return(false)
    endif;

    wline fi_- vedwlineoffset fi_+ vedlineoffset -> vedline;
    wcol fi_- vedwcolumnoffset fi_+ vedcolumnoffset -> vedcolumn;
    vedsetlinesize();

    if try_active_data(vedline, vedcolumn, type, mode) then
        ;;; no data, but was on active
        returnif(type &&=_0 #_<VDTA_TYPE_A_DOC_REF||VDTA_TYPE_DOC_REF>_#)
                                    (false)
    else
        returnif(type &&=_0 VDTA_TYPE_DOC_REF) (false)
    endif;

    skip_spaces();
    ;;; Get documentation type specifier
    get_doctype() -> (doctype, vedline, vedcolumn);
    vedsetlinesize();

    ;;; Skip "*" (if present)
    if (vednextitem() ->> item) == "*" then
        vedmoveitem() -> ;
        if vedcolumn > vvedlinesize then vednextline() endif;
        vednextitem() -> item
    else
        returnunless(type &&/=_0 VDTA_TYPE_ANY_HELP) (false)
    endif;
    if item == termin then vederror('END OF FILE') endif;

    ;;; traverse any gap
    skip_spaces();
    ;;; Read documentation file name
    ved_get_line_filename('\s\Ss\t\n\'"()/,;:', '.') -> arg;
    datalength(arg) -> len;
    vedcolumn + len -> vedcolumn;

    ;;; Ignore hyphens around an identifier (e.g. -foo-)
    if arg(1) == `-` and arg(len) == `-` then
        substring(2, len - 2, arg) -> arg
    endif;

    ;;; Traverse dot or space.
    if vedcurrentchar() == `.` then vedcharright() endif;
    if vedcurrentchar() == `\s` then vedcharright() endif;

    ;;; value for vvedgotoplace if necessary
    if vedcurrentchar() == `/` then
        vedcharright();
        vedmoveitem() -> gotoplace;
        unless isinteger(gotoplace) then
            gotoplace sys_>< nullstring -> gotoplace
        endunless
    else
        false -> gotoplace
    endif;

    define lconstant do_command();
        lvars doctype;
        () -> (doctype, vedargument, vvedgotoplace);
        dlocal veddocsubsystem = "CURRENT";
        recursive_valof(doctype)()
    enddefine;


    if mode == VDTA_MODE_DISPLAY then
        ;;; display only
        #_< VDTA_DISP_DID_MESS||VDTA_DISP_POINTER >_#;  ;;; return hilite type
        if isstartstring('ved_', doctype) then
            allbutfirst(4, doctype) -> doctype
        endif;
        chain(doctype sys_>< '\s' <> arg,
                if gotoplace then () <> '\s/' sys_>< gotoplace endif,
                        vedputmessage)

    else
        VDTA_TYPE_COMM;         ;;; return type
        if mode == VDTA_MODE_EXECUTE then
            ;;; Use chain because it resets vedline etc
            chain(doctype, arg, gotoplace, do_command)
        else
            ;;; add input
            do_command(%doctype, arg, gotoplace%) :: ved_char_in_stream
                                    -> ved_char_in_stream
        endif
    endif
enddefine;

endsection;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 15 1996
        Fixed stupid omission where do_action was not running a procedure
        with text cursor set appropriately.
--- John Gibson, Feb 22 1996
        Test for type VDTA_TYPE_A_DOC_REF added
--- John Gibson, Feb 22 1996
        Fixed bug in parse_action (command with ;;; leaving the first ; on
        the command)
--- John Gibson, Dec 22 1995
        Made get_doctype locally set the item_chartypes of `.` and `-`
        to alphabeticas well as `'`. Removed help*char_type (redundant,
        since nothing called in ved_do_text_action uses it).
 */
