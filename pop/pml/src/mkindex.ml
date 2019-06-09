(* --- Copyright University of Sussex 1991. All rights reserved. ----------
 * File:            C.all/pml/src/mkindex.ml
 * Purpose:         Make dox_index files from PML HELP files
 * Author:          Rob Duncan, Mar 15 1990 (see revisions)
 * Documentation:
 * Related Files:
 *)


pop11

#_IF not(DEF ml_valof)
mishap(0, 'ML SYSTEM NOT LOADED');
#_ENDIF

include sysdefs

section $-ml;

lconstant

    keywords = [
        'val'
        'exception'
        'con'
        'constructor'
        'type'
        'eqtype'
        'datatype'
        'infix'
        'structure'
        'signature'
        'functor'
    ],

    builtins = [
        stdtypes
        stdvalues
        stdio
        system
        compile
        memory
        printer
        int
        real
        string
        list
        combinators
    ],
;

lvars
    index = initv(26),
;

define lconstant is_entry_header(l);
    lvars w, l, i, j;
    for w in keywords do
        if isstartstring(w, l) then
            returnif(w = 'infix')(true);
            returnunless(skipchar(` `, datalength(w)+1, l) ->> i)(false);
            if l(i) == `(` then
                returnunless(locchar(`)`,  i, l) ->> i)(false);
                returnunless(skipchar(` `, i+1, l) ->> i)(false);
            elseif l(i) == `'` then
                returnunless(locchar(` `,  i, l) ->> i)(false);
                returnunless(skipchar(` `, i+1, l) ->> i)(false);
            endif;
            returnunless((locchar(` `,i,l) or locchar(`\n`,i,l)) ->> j)(false);
            return(substring(i, j-i, l));
        endif;
    endfor;
    false;
enddefine;

define lconstant is_entry_text(l);
    lvars l, i;
    l /= nullstring
    and (skipchar(` `, 1, l) ->> i) and i >= 5
    and l(i) /== `\n`;
enddefine;

define lconstant is_blank(l);
    lvars l, i;
    l /= nullstring and (skipchar(` `, 1, l) ->> i) and l(i) == `\n`;
enddefine;

define lconstant order_entries(x, y) -> z;
    lvars x, y, z;
    alphabefore(front(x), front(y)) -> z;
    if z == 1 then
        ;;; two entries for the same identifier
        back(x) -> x;
        back(y) -> y;
        if x(1) = y(1) then
            ;;; same file: put them in order of occurrence within the file
            ;;; (makes modules > types > values)
            x(2) <= y(2) -> z;
        else
            sys_fname_name(x(1)) -> x;
            sys_fname_name(y(1)) -> y;
            ;;; order by name of file initially
            alphabefore(x, y) -> z;
            ;;; unless one or other is a built-in
            if lmember(consword(x), builtins) ->> x then
                if lmember(consword(y), builtins) ->> y then
                    ;;; both built-in! Put the first first
                    length(x) >= length(y) -> z;
                else
                    ;;; x built-in: should go first
                    true -> z;
                endif;
            elseif lmember(consword(y), builtins) then
                ;;; y built-in: should go first
                false -> z;
            endif;
        endif;
    endif;
enddefine;

define lconstant start_index();
    fill(repeat 26 times [] endrepeat, index) ->
enddefine;

define lconstant finish_index();
    lvars i;
    for i to 26 do
        syssort(index(i), false, order_entries) -> index(i);
    endfor;
enddefine;

define lconstant add_to_index(ids, locn);
    lvars id, i, ids, locn;
    for id in ids do
        nextif(id = nullstring);
        uppertolower(id(1)) - `a` + 1 -> i;
        if i < 1 or i > 26 then 26 -> i endif;
        conspair(id, locn) :: index(i) -> index(i);
    endfor;
enddefine;

define lconstant index_file(f, is);
    lvars l, n = 0, id, ids, is, a, b, c, f;
    until (input_line(is) ->> l) == nullstring do
        n + 1 -> n;
        nextunless(is_entry_header(l) ->> id);
        if isstring(id) then [^id] else [] endif -> ids;
        n -> a;
        while is_entry_header(input_line(is) ->> l) ->> id do
            n + 1 -> n;
            if isstring(id) and not(member(id, ids)) then
                id :: ids -> ids;
            endif;
        endwhile;
        n -> b; n + 1 -> n;
        nextunless(is_entry_text(l));
        repeat
            while is_entry_text(input_line(is) ->> l) do
                n + 1 -> n;
            endwhile;
            n -> c; n + 1 -> n;
            while is_blank(l) do
                input_line(is) -> l;
                n + 1 -> n;
            endwhile;
            quitunless(is_entry_text(l));
        endrepeat;
        l -> input(is); n - 1 -> n;
        add_to_index(ids, {^f ^a ^b ^c});
    enduntil;
enddefine;

define lconstant build_index(fs);
    lvars f, fs;
    start_index();
    for f in fs do
        index_file(sys_fname_name(f), instream(f));
    endfor;
    finish_index();
enddefine;

define lconstant write_index(dir);
    lvars   i, j, outfile, dir;
    dlocal  pop_pr_quotes = false, pop_file_versions = false;

    define lconstant fprintf(cucharout);
        dlocal cucharout;
        printf();
    enddefine;

    for i from `a` to `z` do
        discout(dir dir_>< consstring(i,1)) -> outfile;
        for j in index(i - `a` + 1) do
            fprintf('%p %p %p %p %p\n', [% front(j), explode(back(j)) %], outfile);
        endfor;
        outfile(termin);
    endfor;
enddefine;

define mkindex(dir);
    lconstant PATTERN = #_IF DEF VMS '*.;0' #_ELSE '*#' #_ENDIF;
    lvars   procedure p, dir;
    dlocal  pop_file_versions = 1;
    sys_file_match(dir dir_>< nullstring, PATTERN, false, false) -> p;
    build_index(pdtolist(
        procedure() -> f;
            lvars f;
            until (p() ->> f) == termin do
                returnunless(isendstring('index', sys_fname_nam(f)));
            enduntil;
        endprocedure));
    write_index(dir dir_>< 'doc_index/');
enddefine;

if iscaller(ml_startup) then
    [mkpmlindex ^^poparglist] =>
    applist(poparglist, mkindex);
endif;

endsection;

ml

(* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Aug  5 1991
        Added code for VMS file-matching.
--- Robert John Duncan, Jun 18 1991
        Added module keywords.
        Changed startup message.
--- Robert John Duncan, Mar 22 1991
        Removed reference to -ml_*arglist-.
        Added 'con' as identifier keyword.
--- Robert John Duncan, Oct  3 1990
        Changed -write_index- to be more careful about localising
        -cucharout-
--- Robert John Duncan, Aug 29 1990
        Changed -build_index- to include only the basename of the file in
        the index in line with changes to -sys_search_doc_index-
--- Robert John Duncan, Jul 17 1990
        Added -order_entries- to rationalise behaviour when faced with
        two entries for the same identifier.
 *)
