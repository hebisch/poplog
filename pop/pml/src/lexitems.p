/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/pml/src/lexitems.p
 > Purpose:         PML: Basic lexical items
 > Author:          Rob Duncan & Simon Nichols, Feb 13 1989 (see revisions)
 */


section $-ml;

/*
 *  Reserved Words
 */

;;; List of reserved words
lconstant reserved_words = [

    ;;; Core reserved words

    abstype and andalso as case do datatype else
    end exception fn fun handle if in infix
    infixr let local nonfix of op open orelse
    raise rec then type val with withtype while
    ( ) '[' ']' '{' '}' , : ; '...' _ | = => -> #

    ;;; Module reserved words

    eqtype functor include sharing sig signature
    struct structure

    ;;; Non-standard reserved words

    abstraction external pervasive

];

;;; Reserved word tokens are unique
defclass lconstant reserved_word {
    reserved_word,
};

procedure(rw);
    lvars rw;
    unless pr == ml_pr then printf('<reserved_word "') endunless;
    sys_syspr(reserved_word(rw));
    unless pr == ml_pr then printf('">') endunless;
endprocedure -> class_print(reserved_word_key);

lconstant procedure reserved =
    newproperty(
        maplist(reserved_words, procedure(w);
            lvars w;
            unless isword(w) then consword(w) -> w endunless;
            [% w, consreserved_word(w) %]
        endprocedure),
        (length(reserved_words) * 5) div 4, false, "perm");


;;; isreserved:
;;;     recognises a reserved word

constant procedure isreserved = isreserved_word;

;;; tryreserved:
;;;     maps a word to a reserved word if applicable

define tryreserved(w);
    lvars w;
    reserved(w) or w;
enddefine;

;;; RESERVED:
;;;     denotes a reserved word. Takes a word or string as argument.

define macro RESERVED() -> rw;
    lvars w = readitem(), rw;
    unless isword(w) then consword(w) -> w endunless;
    unless reserved(w) ->> rw then
        mishap(w, 1, 'RESERVED WORD NEEDED');
    endunless;
enddefine;

;;; Disabling Poplog-specific words

define lconstant toggle(w, r);
    lvars w, r;
    not(cont(r));
enddefine;

define updaterof lconstant toggle(/* set, */ w, r) with_nargs 3;
    lvars w, r;
    if /* set */ then
        if cont(r) then
            (cont(r), false) -> (reserved(w), cont(r));
        endif;
    else
        if not(cont(r)) then
            (reserved(w), false) -> (cont(r), reserved(w));
        endif;
    endif;
enddefine;

define vars active ml_allow_pervasive =
    toggle(% "pervasive", consref(false) %);
enddefine;

define vars active ml_allow_external =
    toggle(% "external", consref(false) %);
enddefine;


/*
 *  Special constants
 */

;;; A special constant contains its lexical representation (a string) plus
;;; an indication of its type (int, real or string)

defclass scon {
    scon_type,
    scon_string,
};

;;; Printing
procedure(sc);
    lvars s, t, sc;

    define lconstant prc(c);
        lvars c;
        if c fi_< ` ` then
            cucharout(`\\`);
            if c == `\n` then
                cucharout(`n`);
            elseif c == `\t` then
                cucharout(`t`);
            else
                cucharout(`^`), cucharout(c fi_+ 64);
            endif;
        elseif c fi_< `\^?` then
            if c == `"` or c == `\\` then cucharout(`\\`) endif, cucharout(c);
        elseif c == `\^?` then
            cucharout(`\\`), cucharout(`^`), cucharout(`?`);
        else
            cucharout(`\\`), sys_syspr(c);
        endif;
    enddefine;

    unless pr == ml_pr then printf('<scon ') endunless;
    destscon(sc) -> (t, s);
    if t == "string" then
        cucharout(`"`), appdata(s, prc), cucharout(`"`);
    else
        appdata(s, cucharout);
    endif;
    unless pr == ml_pr then printf('>') endunless;
endprocedure -> class_print(scon_key);

define lconstant is_scon_type(item, t);
    lvars item, t;
    isscon(item) and scon_type(item) == t;
enddefine;

define is_int_scon =
    is_scon_type(% "int" %);
enddefine;

define is_real_scon =
    is_scon_type(% "real" %);
enddefine;

define is_string_scon =
    is_scon_type(% "string" %);
enddefine;

;;; scon_value:
;;;     the value of a special constant: an integer, real or string

define scon_value(sc);
    lvars sc, t, s;
    destscon(sc) -> (t, s);
    ;;; return string value unchanged
    returnif(t == "string")(s);
    ;;; number type
    lblock;
        lvars   c, i, n = 0, m = 0, e = 0, state = 1, signed = false,
                esigned = false, len = datalength(s);
        dlocal  popdprecision = true;
        compile_mode:vm -goonch;
        fast_for i to len do
            fast_subscrs(i, s) -> c;
            go_on state to state_1 state_2 state_3 state_4 state_5 state_6;
            state_1:
                ;;; start
                if c == `~` then
                    true -> signed;
                    2 -> state;
                    nextloop;
                endif;
            state_2:
                ;;; first digit
                c fi_- `0` -> n;
                3 -> state;
                nextloop;
            state_3:
                ;;; subsequent digits, up to "." or "E"
                if c == `.` then
                    4 -> state;
                elseif c == `E` then
                    5 -> state;
                else
                    n * 10 + (c fi_- `0`) -> n;
                endif;
                nextloop;
            state_4:
                ;;; digits after ".", up to "E"
                if c == `E` then
                    5 -> state;
                else
                    n * 10 + (c fi_- `0`) -> n;
                    m fi_+ 1 -> m;
                endif;
                nextloop;
            state_5:
                ;;; first character after "E"
                6 -> state;
                if c == `~` then
                    true -> esigned;
                    nextloop;
                endif;
            state_6:
                ;;; digits after "E" up to end
                e * 10 + (c fi_- `0`) -> e;
                nextloop;
        endfor;
        if t == "real" then
            if esigned then negate(e) -> e endif;
            number_coerce(n * 10 ** (e-m), 1.0d0) -> n;
        endif;
        if signed then negate(n) else n endif;
    endlblock;
enddefine;


/*
 *  Identifiers
 */

;;; Short identifiers:

constant procedure isshortid = isword;

;;; Long identifiers:

defclass longid {
    longid_first,
    longid_rest,
};

procedure(longid);
    lvars longid;
    unless pr == ml_pr then printf('<longid ') endunless;
    while islongid(longid) do
        sys_syspr(longid_first(longid));
        cucharout(`.`);
        longid_rest(longid) -> longid;
    endwhile;
    sys_syspr(longid);
    unless pr == ml_pr then printf('>') endunless;
endprocedure -> class_print(longid_key);

define isid(item);
    lvars item;
    isshortid(item) or islongid(item);
enddefine;

;;; idpath:
;;;     takes a string or a word and returns an identifier path:
;;;     words are assumed to be unqualified and are returned unchanged;
;;;     strings are parsed as long identifiers.

define idpath(id) -> id;
    lvars id, i = 1, j;
    if isstring(id) then
        popstackmark;
        while locchar(`.`, i, id) ->> j do
            consword(substring(i, j-i, id));
            j+1 -> i;
        endwhile;
        consword(if i == 1 then id else allbutfirst(i-1, id) endif) -> id;
        until (->> i) == popstackmark do conslongid(i, id) -> id enduntil;
    elseif not(isword(id)) then
        mishap(id, 1, 'STRING NEEDED');
    endif;
enddefine;

;;; longid_<=:
;;;     orders long identifiers

define longid_<=(a, b);
    lvars a, b, x;
    if islongid(a) then
        islongid(b) and alphabefore(longid_first(a), longid_first(b)) -> x;
        x == 1 and longid_<=(longid_rest(a), longid_rest(b)) or x;
    else
        islongid(b) or alphabefore(a, b);
    endif;
enddefine;

;;; Type variables:

defclass tvid {
    tvid_name,
    tvid_equality,
    tvid_imperative,
};

procedure(tvid);
    lvars tvid;
    unless pr == ml_pr then printf('<tvid ') endunless;
    sys_syspr(tvid_name(tvid));
    unless pr == ml_pr then printf('>') endunless;
endprocedure -> class_print(tvid_key);

;;; Redefine constructor procedure to give unique identifiers

lconstant procedure (
    Constvid    = constvid,
    tvids       = newproperty([], 32, false, "tmpval"),
);

compile_mode:vm -prmfix;    ;;; allow for redeclaration
define constvid(name, equality, imperative) -> tvid;
    lvars name, equality, imperative, tvid;
    unless tvids(name) ->> tvid then
        Constvid(name, equality, imperative) ->> tvids(name) -> tvid;
    endunless;
enddefine;
compile_mode:vm +prmfix;


/*
 *  Record Labels
 */

;;; is_label:
;;;     a record label is a short identifier, or an integer number 1,2,3,...

define is_label(x);
    lvars x;
    if isshortid(x) then
        x;
    elseif is_int_scon(x)
    and locchar(subscrs(1, scon_string(x)), 1, '123456789')
    then
        scon_value(x);
    else
        false;
    endif;
enddefine;

;;; label_<=:
;;;     imposes an ordering on labels, with integers coming before symbols

define label_<=(lab1, lab2);
    lvars lab1, lab2;
    if isword(lab1) then
        isword(lab2) and alphabefore(lab1, lab2);
    else
        isword(lab2) or lab1 < lab2;
    endif;
enddefine;


/*
 *  Comments
 */

vars
    ml_allow_eolc = true,   ;;; enables end-of-line comments
;

endsection; /* $-ml */


/* --- Revision History ---------------------------------------------------
--- Robert John Duncan, Nov 24 1994
        Sectionised
--- Robert John Duncan, Nov 17 1994
        Added abstraction as a reserved word for compatibility with SML/NJ
--- Robert John Duncan, Feb 22 1991
        Thorough revision to make all classes of item returned by the lexer
        disjoint.
--- Robert John Duncan, Feb  4 1991
        Changed to use -defclass-
--- Rob Duncan, Jun 22 1990
        Removed obsolete "unit" definition.
 */
