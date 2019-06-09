/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/showdef.p
 > Purpose:         PML: Command for displaying contents of the environment
 > Author:          Rob Duncan & Simon Nichols, Mar  9 1989 (see revisions)
 > Documentation:   HELP * SHOWDEF
 */


section $-ml;

;;; Declaration class qualifiers:
lconstant qualifiers = [
    'val'
    'con'
    'constructor'
    'exception'
    'type'
    'structure'
    'signature'
    'functor'
];

;;; parse_arguments:
;;;     turns the command argument string into a list of things to display

define lconstant parse_arguments(args);
    lvars arg, args, all = "all", current_class = "all";
    [%
        until args == [] do
            destpair(args) -> args -> arg;
            if member(arg, qualifiers) then
                if all == true then conspair(current_class, false) endif;
                true -> all;
                if arg = 'constructor' then 'con' -> arg endif;
                consword(arg) -> current_class;
            else
                false -> all;
                conspair(current_class, idpath(arg));
            endif;
        enduntil;
        if all then conspair(current_class, false) endif;
    %];
enddefine;

;;; get_named_entry:
;;;     looks up an entry with name -id- and type -class- ("val", "type" etc.)
;;;     If -class- is "all", then all entries with the given name are
;;;     returned.

define lconstant get_named_entry(class, id);
    lvars   class, id, e;
    dlocal  ml_autoloading;
    if class == "val" then
        lookup_val(id, false);
    elseif class == "con" then
        lookup_con(id);
    elseif class == "exception" then
        lookup_exn(id);
    elseif class == "type" then
        lookup_tycon(id) ->> e;
        ;;; if it's a datatype, include its constructors
        if e then
            For id in tycon_cons(e) do
                if (lookup_con(val_name(id)) ->> e) and not(val_isexn(e))
                then
                    e;
                endif;
            endfor;
        endif;
    elseif class == "structure" then
        lookup_str(id) ->> e;
        ;;; if it has a visible, named signature, include that
        if e and (structenv_signame(str_env(e)) ->> id) then
            lookup_sig(signame_name(id)) -> e;
            if e and sig_signame(e) == id then e endif;
        endif;
    elseif class == "signature" then
        ;;; -lookup_sig- only works for short identifiers
        isshortid(id) and lookup_sig(id);
    elseif class == "functor" then
        ;;; -lookup_fnc- only works for short identifiers
        isshortid(id) and lookup_fnc(id);
    elseif class == "all" then
        ;;; disable autoloading for short identifiers
        ml_autoloading and islongid(id) -> ml_autoloading;
        [%
            if get_named_entry("val", id) ->> e then e endif;
            if get_named_entry("type", id) ->> e then e endif;
            if get_named_entry("structure", id) ->> e then e endif;
            if get_named_entry("signature", id) ->> e then e endif;
            if get_named_entry("functor", id) ->> e then e endif;
        %] -> e;
        if e == [] then false else dl(e) endif;
    else
        false;
    endif;
enddefine;

;;; get_named_entries:
;;;     gets a sequence of named entries

define lconstant get_named_entries(class, entries);
    lvars entry, class, entries;
    For entry in entries do
        get_named_entry(class, entry_name(entry));
    endfor;
enddefine;

;;; get_entries:
;;;     get all the entries of a particular class from the environment

define lconstant get_entries(class);
    lvars class;
    if class == "val" then
        get_named_entries(class, global_vals());
    elseif class == "con" then
        get_named_entries(class, global_cons());
    elseif class == "exception" then
        get_named_entries(class, global_exns());
    elseif class == "type" then
        get_named_entries(class, global_tycons());
    elseif class == "structure" then
        get_named_entries(class, global_strs());
    elseif class == "signature" then
        get_named_entries(class, global_sigs());
    elseif class == "functor" then
        get_named_entries(class, global_fncs());
    elseif class == "all" then
        get_entries("val");
        get_entries("type");
        get_entries("structure");
        get_entries("signature");
        get_entries("functor");
    endif;
enddefine;

;;; get_all_entries:
;;;     find all the entries corresponding to the argument list

define lconstant get_all_entries(args);
    lvars e, es, arg, args;
    [%
        for arg in args do
            if not(back(arg)) then
                get_entries(front(arg));
            elseif get_named_entry(destpair(arg)) ->> e then
                e;
            elseif front(arg) == "all" then
                printf('No definitions for %p\n', [% back(arg) %]);
            else
                printf('No definition for %p %p\n', [% destpair(arg) %]);
            endif;
        endfor;
    %] -> es;
    ;;; filter out duplicates
    [%
        until es == [] do
            destpair(es) -> es -> e;
            unless lmember(e, es) then e endunless;
        enduntil;
    %];
enddefine;

;;; show_fixity:
;;;     print precedence and associativity for a list of vals
;;;     (variables or constructors)

define lconstant show_fixity(vals);
    lconstant infix = initv(10), infixr = initv(10);
    lvars val, prec, opr, found = false, vals;
    fill(Repeat 10 times [] endrepeat, infix) -> ;
    fill(Repeat 10 times [] endrepeat, infixr) -> ;
    For val in vals do
        if lookup_opr(val_name(val)) ->> opr then
            opr_prec(opr) + 1 -> prec;
            if opr_fixity(opr) == "infix" then
                [^^(infix(prec)) ^(val_name(val))] -> infix(prec);
            else
                [^^(infixr(prec)) ^(val_name(val))] -> infixr(prec);
            endif;
            true -> found;
        endif;
    endfor;
    returnunless(found);
    For prec from 9 by -1 to 0 do
        if (infix(prec+1) ->> vals) /== [] then
            sp(2), spr("infix"), spr(prec), app(vals, spr), pr(newline);
        endif;
        if (infixr(prec+1) ->> vals) /== [] then
            sp(2), spr("infixr"), spr(prec), app(vals, spr), pr(newline);
        endif;
    endfor;
enddefine;

;;; entry_<=:
;;;     defines an ordering on entries based on their printing names

define lconstant entry_<=(a, b);
    lvars a, b;
    if isfnc(a) or issig(a) then
        alphabefore(entry_name(a), entry_name(b));
    elseif isstr(a) then
        longid_<=(fullname(str_strname(a)), fullname(str_strname(b)));
    else
        longid_<=(
            fullname(entry_parent(a), entry_name(a)),
            fullname(entry_parent(b), entry_name(b)));
    endif;
enddefine;

;;; show_entries:
;;;     displays a list of entries sorted by name

define lconstant show_entries(entries);
    lvars entries;
    unless entries == [] then
        app(syssort(entries, false, entry_<=), print_top_binding);
        pr(newline);
    endunless;
enddefine;

;;; showdef:
;;;     displays information about the environment entries requested by
;;;     -args- (a string)

define showdef(args);
    lvars   args, entries, es;
    dlocal  pr = ml_pr;
    if (get_all_entries(parse_arguments(args)) ->> entries) == [] then
        printf('Nothing to show\n');
    else
        pr(newline);
        show_entries(filter(entries, issig));
        show_entries(filter(entries, isfnc));
        show_entries(filter(entries, isstr));
        show_entries(filter(entries, procedure(e);
            lvars e;
            istycon(e) and tycon_cons(e) == [];
        endprocedure));
        show_entries(filter(entries, procedure(e);
            lvars e;
            istycon(e) and tycon_cons(e) /== [];
        endprocedure));
        filter(entries, isvalcon) -> es;
        show_fixity(es);
        show_entries(es);
        filter(entries, isexn) -> es;
        show_fixity(es);
        show_entries(es);
        filter(entries, isvar) -> es;
        show_fixity(es);
        show_entries(es);
    endif;
enddefine;


/*
 *  SHOWDEF as an ML command
 */

procedure(name, args);
    lvars name, args;
    chain(args, showdef);
endprocedure -> command_table("showdef");


/*
 *  ... and as a VED command
 */

lvars
    tmpfile = false,
;

define vars ved_showdef();
    lvars args;

    define lconstant showdef_to_file(args);
        lvars   args;
        dlocal  vedpositionstack, vedbreak = false, cucharout = vedcharinsert;
        false -> vedwriteable;
        "ml" -> subsystem;
        vedendfile();
        unless vvedbuffersize == 0 then vedlinebelow() endunless;
        vedpositionpush();
        showdef(args);
        vedpositionpop();
        vedcheck();
    enddefine;

    lex_command_args(new_itemiser(instream(stringin(vedargument)))) -> args;
    unless tmpfile then
        systmpfile(false, 'showdef', nullstring) -> tmpfile;
    endunless;
    showdef_to_file(% args %) :: ved_char_in_stream -> ved_char_in_stream;
    tmpfile -> vedargument;
    chain(ved_ved);
enddefine;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- John Gibson, Jan 13 1993
        popcom*piler -> subsystem in showdef_to_file
--- Robert John Duncan, Mar  1 1991
        Revised -showdef- to expect a list of argument strings.
--- Robert John Duncan, Feb 11 1991
        Exceptions, constructors and variables now represented by a common
        "val" record.
--- Robert John Duncan, Feb  4 1991
        New representation of operators.
--- Rob Duncan, Apr 19 1990
        Changed order of tests in -parse_arguments- to check declaration
        classes before identifiers: the class "constructor" isn't a reserved
        word, so has to be checked first
 */
