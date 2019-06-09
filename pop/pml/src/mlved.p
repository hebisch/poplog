/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/mlved.p
 > Purpose:         PML: VED procedures
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */

section $-ml =>
    ml_filetypes
    ved_ml
    ved_showdef
    ved_trace
    ved_untrace
;

global vars $-mlfiletypes;
    ;;; may not be in the system yet
identof("mlfiletypes") -> identof("ml_filetypes");
    ;;; for backwards compatibility


;;; ved_ml:
;;;     run commands in ML subsystem.

define vars ved_ml;
    veddo_in_subsystem("ml");
enddefine;

;;; ved_load:
;;;     load an ML file from VED

define lconstant mlved_load() with_props ved_load;
    lvars   name;
    dlocal  vedargument;

    define lconstant has_ml_extn(name);
        lvars e, name;
        sys_fname_extn(name) -> e;
        e = nullstring or e = ml_filetype or e = ml_sigfiletype
        or e = '.ml' or e = '.sig';
    enddefine;

    define dlocal loadwarning(name) with_props false;
        lvars name;
        vedputmessage('LOADING ' sys_>< name);
    enddefine;

    if vedargument = nullstring then
        vedvedname
    else
        vedargument
    endif -> name;
    if has_ml_extn(name) then
        unless vedargument = nullstring or (sourcefile(name) ->> name) then
            vederror('Not found');
        endunless;
        loadwarning(name);
        ml_load(name);
    else
        ':load ' <> name -> vedargument;
        vedcompilevedargument();
    endif;
enddefine;

;;; vedchartype:
;;;     group characters into classes more closely related to ML lexical
;;;     rules.

lvars procedure old_vedchartype;
define lconstant mlvedchartype(c) with_props vedchartype;
    lvars c;
    if subsystem /== "ml" or caller(1) == veddocommand then
        ;;; restore old chartype for non-ML files and the command-line
        old_vedchartype(c);
    elseif c == ` ` then
        ` `;    ;;; space
    elseif isalphacode(c) or c == `_` or c == `'` then
        `a`;    ;;; alphabetic
    elseif locchar(c, 1, '+-><*?=^!$&~|\@:/`') then
        `+`;    ;;; symbolic
    elseif locchar(c, 1, '()[]{};,;.#') then
        `.`;    ;;; separator
    elseif isnumbercode(c) then
        `0`;    ;;; numeric
    else
        c;      ;;; control
    endif;
enddefine;

;;; ved_mbp, ved_mep:
;;;     mark beginning and end of a declaration

lconstant
    declaration_keywords = [
            'val'
            'functor'
            'fun'
            'datatype'
            'type'
            'abstype'
            'exception'
            'local'
            'structure'
            'abstraction'
            'signature'
            'open'
            'pervasive'
            'external'
        ],
    openers = [%
            RESERVED '(',
            RESERVED 'let',
            RESERVED 'local',
            RESERVED 'abstype',
            RESERVED 'struct',
            RESERVED 'sig',
        %],
    closers = [%
            RESERVED ')',
            RESERVED 'end',
        %],
;

define lconstant is_dec_start(line);
    lvars word, line, c, l;
    For word in declaration_keywords do
        if issubstring_lim(word, 1, 1, false, line) then return
            /* check it's not the start of an identifier */
            ((datalength(word) ->> l) == datalength(line)
            or not(isalphacode(Subscrs(l fi_+ 1, line) ->> c)
                   or isnumbercode(c) or c == `_` or c == `'`))
        endif;
    endfor;
    false;
enddefine;

define lconstant mlved_mbp() with_props ved_mbp;
    lvars   start, line;
    dlocal  vedline, vedcolumn, vvedlinesize;
    vedline -> start;
    until vvedlinesize fi_> 0
    and isalphacode(Subscrs(1, vedthisline() ->> line))
    and is_dec_start(line)
    do
        if vedline == 1 then
            vederror('No declaration keyword before line ' sys_>< start);
        endif;
        vedcharup();
    enduntil;
    vedmarklo();
enddefine;

define lconstant mlved_mep() with_props ved_mep;
    lvars   procedure itemrep, item, nopeners = 0;
    dlocal  ml_linenum, vedline, vedcolumn, vvedlinesize;

    define dlocal lex_error(msg, culprits);
        lvars i, msg, culprits;
        if locchar(`\n`, 1, msg) ->> i then
            ;;; strip argument formatting
            substring(1, i-1, msg) -> msg;
        endif;
        vederror(msg);
    enddefine;

    new_itemiser(instream(vedrepeater)) -> itemrep;
    until (itemrep() ->> item) == termin
    or item == RESERVED ';' and nopeners fi_<= 0
    do
        if Lmember(item, openers) then
            nopeners fi_+ 1 -> nopeners;
        elseif Lmember(item, closers) then
            nopeners fi_- 1 -> nopeners;
        endif;
    enduntil;
    if nopeners fi_> 0 then vederror('Can\'t find end of declaration') endif;
    if vedcolumn == 1 then vedcharup() endif;
    vedmarkhi();
enddefine;

/*
 *  <ENTER> f <dec-type> <name>
 *      Search for a declaration of dec-type which binds name; if dec-type
 *      is omitted it defaults to fun. Works by redefining vedfindpdheader
 *      called by ved_f.
 */

    ;;; test whether an item found matches the name wanted
define lconstant match_name(found, wanted, exact);
    lvars found, wanted, exact;
    returnunless(isshortid(found))(false);
    found ml_>< nullstring -> found;
    found = wanted or (not(exact) and isstartstring(wanted, found));
enddefine;

    ;;; match the name bound by a simple binding -- i.e. one where the
    ;;; name is guaranteed to follow the declaration keyword
define lconstant match_bound_name(getitem, wanted, exact);
    lvars procedure getitem, wanted, exact;
    match_name(getitem(), wanted, exact);
enddefine;

    ;;; match the name bound by a type (or datatype or abstype) binding;
    ;;; the name is assumed to immediately precede the '=' keyword
define lconstant match_type_name(getitem, wanted, exact);
    lvars procedure getitem, wanted, exact;
    lvars (name, item) = (getitem(), getitem());
    repeat
        if item == RESERVED '=' then
            ;;; should be preceded by the bound name
            return(match_name(name, wanted, exact));
        elseif item == RESERVED ';' or item == termin then
            ;;; lost it
            return(false);
        else
            ;;; skip arguments to type constructor
            (item, getitem()) -> (name, item);
        endif;
    endrepeat;
enddefine;

    ;;; match the name bound by a fun binding; it does the minimum
    ;;; necessary to cope with infix bindings, so works pretty well for
    ;;; correct programs but might return anything if there are syntax
    ;;; errors
define lconstant match_fun_name(getitem, wanted, exact);
    lvars procedure getitem, wanted, exact;
    ;;; if item is an opening bracket, return its matching closer
    define lconstant isbracket(item) -> closer;
        lvars item, closer = false;
        if item == RESERVED '(' then
            RESERVED ')' -> closer;
        elseif item == RESERVED '[' then
            RESERVED ']' -> closer;
        elseif item == RESERVED '{' then
            RESERVED '}' -> closer;
        endif;
    enddefine;
    ;;; skip items until a specific closer is read, respecting nested
    ;;; brackets
    define lconstant skipto(closer);
        lvars closer, closers = [];
        repeat
            lvars item = getitem();
            if item == closer then
                returnif(closers == []);
                dest(closers) -> (closer, closers);
            elseif item == RESERVED ';' or item == termin then
                ;;; gone too far
                return;
            elseif isbracket(item) ->> item then
                closer :: closers -> closers;
                item -> closer;
            endif;
        endrepeat;
    enddefine;
    lvars item, name = getitem();
    if name == RESERVED 'op' then
        getitem() -> name;
    endif;
    ;;; name is probably the function name, but it could be an
    ;;; atomic pattern followed by an infix name
    unless isshortid(name) then
        ;;; must be infix: want to skip over an atomic pattern and
        ;;; find the name following, but watch out for the special
        ;;; case of a bracketed infix form
        lvars bracketed = false;
        if name == RESERVED '(' then
            ;;; could be bracketed infix
            true -> bracketed;
            getitem() -> name;
        endif;
        if isbracket(name) ->> item then
            skipto(item);
        endif;
        ;;; possible name follows
        getitem() -> name;
        ;;; if inside brackets, skip to end
        if bracketed then skipto(RESERVED ')') endif;
    endunless;
    ;;; check for following infix name
    lvars item = getitem();
    if isshortid(item) and lookup_opr(item) then
        item -> name;
    endif;
    ;;; see if it matches
    match_name(name, wanted, exact);
enddefine;

    ;;; match a name bound by a val binding: this is the worst of the
    ;;; lot because there may be several names in each binding and
    ;;; buried to arbitrary depth, and we can't -- at this stage --
    ;;; distinguish variables from constructors, so this strategy will
    ;;; also match nullary constructors and even trailing type names in
    ;;; constraints!
define lconstant match_val_name(getitem, wanted, exact);
    lvars procedure getitem, wanted, exact;
    lvars name = getitem(), in_record = 0;
    repeat
        if isshortid(name) then
            ;;; could be a variable, depending on what follows
            lvars item = getitem();
            if item == RESERVED ')'
            or item == RESERVED ']'
            or item == RESERVED '}'
            or item == RESERVED ','
            or item == RESERVED '=' and in_record == 0
            or item == RESERVED 'as'
            or item == RESERVED ':'
            or isshortid(item) and lookup_opr(item)
            then
                returnif(match_name(name, wanted, exact))(true);
            endif;
            item -> name;
            nextloop;
        elseif name == RESERVED '{' then
            in_record fi_+ 1 -> in_record;
        elseif name == RESERVED '}' and in_record fi_> 0 then
            in_record fi_- 1 -> in_record;
        elseif name == RESERVED '=' and in_record == 0
        or name == RESERVED ';' or name == termin
        then
            ;;; gone too far
            return(false);
        endif;
        getitem() -> name;
    endrepeat;
enddefine;

    ;;; associate match procedures with declaration keywords
define lconstant matcher_for =
    newproperty([
        [% RESERVED 'val',          match_val_name %]
        [% RESERVED 'fun',          match_fun_name %]
        [% RESERVED 'type',         match_type_name %]
        [% RESERVED 'datatype',     match_type_name %]
        [% RESERVED 'withtype',     match_type_name %]
        [% RESERVED 'exception',    match_bound_name %]
        [% RESERVED 'structure',    match_bound_name %]
        [% RESERVED 'signature',    match_bound_name %]
        [% RESERVED 'functor',      match_bound_name %]
    ], 16, false, "perm");
enddefine;

    ;;; search for a declaration of kind dec_type which binds name
define find_binding(name, exact, dec_type) -> line;
    lvars name, exact, dec_type, line;
    dlocal vedline, vedcolumn;

    ;;; start from the top of the file to get outside all declarations
    lvars start_line = vedline;
    vedtopfile();

    ;;; itemise the file, ignoring lexical errors
    dlvars procedure itemrep = new_itemiser(instream(vedrepeater));
    dlvars save_stacklength = stacklength();
    define lconstant getitem();
        define dlocal lex_error(msg, culprits);
            lvars msg, culprits;
            ;;; lexer may be in an inconsistent state, so recreate it
            new_itemiser(instream(vedrepeater)) -> itemrep;
            ;;; escape from this call (NB: this may be a big jump!)
            erasenum(stacklength() - save_stacklength);
            exitto(false, find_binding);
        enddefine;
        itemrep();
    enddefine;

    lvars item, this_dec = false, outer_decs = [], line = false;
    until (getitem() ->> item) == termin do
        nextunless(isreserved(item));
        if item == RESERVED 'and' then
            ;;; replace with current declaration type
            this_dec -> item;
        elseif item == RESERVED 'abstype' then
            ;;; special case -- sets up a local block
            false :: outer_decs -> outer_decs;
            RESERVED 'datatype' -> item;
        elseif item == RESERVED 'abstraction' then
            ;;; synonym for structure
            RESERVED 'structure' -> item;
        endif;
        if dec_type == item
        or dec_type == RESERVED 'val' and
             item == RESERVED 'fun'
        or dec_type == RESERVED 'type' and
            (item == RESERVED 'datatype' or
             item == RESERVED 'withtype')
        then
            ;;; try and match a name from this binding
            item -> this_dec;
            lvars found_line = vedline;
            if matcher_for(item)(getitem, name, exact) then
                if found_line fi_> start_line then
                    return(found_line -> line);
                elseunless line then
                    ;;; remember first occurrence, in case we don't
                    ;;; find another after the start line
                    found_line -> line;
                endif;
            endif;
        elseif item == RESERVED 'val'
        or item == RESERVED 'fun'
        or item == RESERVED 'type'
        or item == RESERVED 'datatype'
        or item == RESERVED 'exception'
        or item == RESERVED 'structure'
        or item == RESERVED 'signature'
        or item == RESERVED 'functor'
        or item == RESERVED 'external'
        then
            ;;; some other declaration we're not interested in
            item -> this_dec;
        elseif item == RESERVED 'let'
        or item == RESERVED 'local'
        or item == RESERVED 'struct'
        or item == RESERVED 'sig'
        then
            this_dec :: outer_decs -> outer_decs;
            false -> this_dec;
        elseif item == RESERVED 'end' and outer_decs /== [] then
            dest(outer_decs) -> (this_dec, outer_decs);
        endif;
    enduntil;
enddefine;

    ;;; ML-specific version of vedfindpdheader
define lconstant mlvedfindpdheader(name, exact) -> line
with_props vedfindpdheader;
    lvars name, exact, line, dec_type = RESERVED 'fun';
    lvars n = (#| sys_parse_string(name) |#);
    if n == 1 then
        () -> name;
    elseunless n == 0 then
        ;;; should be <dec-type> <name>: ignore anything else
        lvars (keyword, name) = erasenum(n-2);
        tryreserved(consword(keyword)) -> keyword;
        if matcher_for(keyword) then keyword -> dec_type endif;
    endif;
    find_binding(name, exact, dec_type) -> line;
enddefine;

;;; Commands defined elsewhere

vars procedure (
    ved_showdef,    ;;; "showdef.p"
    ved_trace,      ;;; "trace.p"
    ved_untrace,    ;;; "trace.p"
);

/*
 *  Ved run-time initialisation
 */

define :ved_runtime_action;
    ;;; In case mlfiletypes is not yet in the system ...
    unless islist(mlfiletypes) then
        ['.ml' '.sig'] -> mlfiletypes;
        [^^vednonbreakfiles
            mlfiletypes
        ] -> vednonbreakfiles;
        [^^vedfiletypes
            [mlfiletypes
                {subsystem      "ml}
                {vedcompileable true}
            ]
        ] -> vedfiletypes;
    endunless;
    ;;; Add ml_filetype and ml_sigfiletype to mlfiletypes
    unless Lmember("ml_sigfiletype", mlfiletypes) then
        [ml_sigfiletype ^^mlfiletypes] -> mlfiletypes;
    endunless;
    unless Lmember("ml_filetype", mlfiletypes) then
        [ml_filetype ^^mlfiletypes] -> mlfiletypes;
    endunless;
    ;;; Redefine chartype
    vedchartype -> old_vedchartype;
    mlvedchartype -> vedchartype;
    ;;; Redefine procedures for ML subsystem
    mlved_load -> subsystem_valof("ved_load", "ml");
    mlved_mbp -> subsystem_valof("ved_mbp", "ml");
    mlved_mep -> subsystem_valof("ved_mep", "ml");
    mlvedfindpdheader -> subsystem_valof("vedfindpdheader", "ml");
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Dec 20 1994
        Declared command procedures defined elsewhere (trace and showdef)
--- Robert John Duncan, Nov 24 1994
        Sectionised. Changed to use ved_runtime_action. New variable
        mlfiletypes built in to Ved from 14.5003.
--- Robert John Duncan, Oct 26 1994
        Implemented ved_f (at last).
        Got rid of ved_pml -- autoloadable.
--- Robert John Duncan, Oct 24 1994
        Change in format of arguments to lex_error
--- John Gibson, Jan 13 1993
        o popcom*piler -> subsystem in vedchartype
        o Moved ml_libpath to lib ml_subsystem
--- Robert John Duncan, Apr  6 1992
        Renamed -ml_*searchpath- to -ml_libdirs-
--- Robert John Duncan, Jun 18 1991
        Got rid of -getml*doc- procedures to simplify subsystem search-lists.
        Added -ml_libpath- as a substitute.
--- Robert John Duncan, Mar  1 1991
        Changed -ved_mep- to allow <termin> as declaration terminator.
--- Robert John Duncan, Feb 11 1991
        Added dlocal -lex_error- to -ved_mep-
--- Robert John Duncan, Aug  8 1990
        Changes for extended subsystems.
--- Rob Duncan, May 29 1990
        Added -ved_pml- as synonym for -ved_ml-
--- Rob Duncan, May 24 1990
        Created separate -veddoinsubsystem- for running a command within
        a different subsystem: it now localises -subsystem- itself (so that
        things like -getmldoc- will work correctly) and localises
        -popcom*piler- more carefully.
--- Rob Duncan, Jan 31 1990
        Changed -getmlhelp- & -getmlteach- to work if the subsystem is "ml";
        made them check for mixed-case file names, to handle 'help StdValues'
        etc.
        Added -getmllib- to enable "showlib".
        Simplified -ved_load- to behave more like the "load" command
--- Rob Duncan, Oct 26 1989
        Changed load command to use -sourcefile-.
        Added -getmlteach-
 */
