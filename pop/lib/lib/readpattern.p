/* --- Copyright University of Birmingham 2000. All rights reserved. ------
 > File:            $poplocal/local/lib/readpattern.p
 > File:            $poplocal/local/newkit/prb/lib/readpattern.p
 > Purpose:         Read in a pattern, changing variables to idents
 >                  Define new syntax word "!" for this purpose
 > Author:          Aaron Sloman, Nov 26 1995 (see revisions)
 > Documentation:   See HELP * READPATTERN, HELP * DOESMATCH
 > Related Files:   LIB * DOESMATCH, LIB * FMATCHES,
 */


/*
    ![ .... ]
        creates a new type of pattern structure for use with lvars, etc.

    ![?x ison ??y]

        This creates a list in which the words "x" and "y" are replaced
        by the corresponding current identifiers.

        If there are no current identifiers corresponding to the pattern
        variables, they are declared as lvars in the current lexical block.

The syntactic operator "!" defined below can ONLY be followed by list
expressions, and it reads them in and replaces occurrences of variable
names after "?" or "??" with identifiers. The resulting list can be used
as a pattern to be given to either the standard pattern matcher (in
Poplog Version 15.0 onwards) or the more general matcher doesmatch,
defined in LIB * DOESMATCH (available from Birmingham).

A pattern read in after "!" works with previously define lexical
variables as they are treated like dynamic variables.

(I think it is more efficient to use dlvars than lvars, but am not sure.)

Here is an example, which can be tried after compiling this file.

define list_between(item1, item2, list) -> found;
    ;;; Here found is automatically declared as lvars, as are the
    ;;; input variables in Poplog version 15, but just to make
    ;;; the point we declare it

    lvars found;
    unless list matches ![== ^item1 ??found  ^item2 ==] then
        false -> found;
    endunless;

enddefine;

;;; Now test the procedure

vars words = [a b c d e f g];

list_between("a", "g", words) =>
** [b c d e f]

;;; Unlike uses of the matcher prior to Version 15, this does not declare
;;; and assign to a global variable: found

found =>
;;; DECLARING VARIABLE found
** <undef found>

list_between("d", "e", words) =>
** []
list_between("e", "c", words) =>
** <false>

;;; "!" works also with restriction elements (See HELP * MATCHES).

lvars xw;
[[a b c]] matches ![[== ??xw:2]], xw =>
** <true> [b c]

;;; And if a word is not already declared anywhere, and the pattern
;;; is inside a procedure it declares the word as
;;; lvars, giving a warning unless pop_lvars_warning is false

cancel xw;
procedure();
    [[a b c]] matches ![[== ??xw:2]] => xw =>
endprocedure();
;;; WARNING DECLARING PATTERN VARIABLE xw AS LVARS
;;; IN FILE /home/staff/axs/readpattern.p
;;; LINE 80
** <true>
** [b c]

xw =>
;;; DECLARING VARIABLE xw
** <undef xw>

;;; But declares it as global if outside a procedure
cancel xw;
    [[a b c]] matches ![[== ??xw:2]] => xw =>

;;; You can access the global variable.
xw =>
** [b c]

;;; try again with pop_lvars_warning false
false -> pop_lvars_warning;
;;; Then make it true, the default
true -> pop_lvars_warning;


;;; "!" patterns work with restriction procedures also
lvars n;
[a b c 3 4 c d 4 5 e] matches ![== ?n == ?n:isinteger ==], n=>
** <true> 4
lvars nn;
[a b c 3 4 c d 4 5 e] matches ![== ?nn == ?nn:isword ==], nn=>
** <true> c

;;; Some test cases, showing that in vector expressions the "?" and "??"
;;; are ignored.
vars v1,v2,v3;

! [v1 ?v1 {v2 ?v2 [?v2]} [v3 ?v3]] =>
** [v1 ? <ID v1 <undef v1>> {v2 ? v2 [? v2]} [v3 ? <ID v3 <undef v3>>]]

lvars lv1, lv2, lv3;
! [lv1 ?lv1 {lv2 ?lv2 [?lv2]} [lv3 ?lv3]] =>
** [lv1 ? <ID lv1 <undef>> {lv2 ? lv2 [? lv2]} [lv3 ? <ID lv3 <undef>>]]
    
;;; Use of "!" in a pattern embedded in a vector expression works.
lvars lv1, lv2, lv3;
![{% ![?lv1] % ?lv2 } ?lv3] =>
** [{[? <ID lv1 <undef>>] ? lv2} ? <ID lv3 <undef>>]

Note: if procedures using patterns created with "!" are traced, then
identifiers will show up in patterns with both their names and their
current values, as here:

lvars lv4 = 99, lv5 = "cat" ;
![lv4 ?lv4 lv5 ?lv5] =>
** [lv4 ? <ID lv4 99> lv5 ? <ID lv5 cat>]

WARNING:
    Patterns should NOT contain evaluated sub-expressions quoting
    "?" or "??", e.g.

        ![== [?x b] == [?x h ^(consword("?" >< 'foo')] ==], x =>

    Finally note that patterns containing lexically scoped variables
    cannot be compiled as constants. (See LIB * COMPILE_MODE)

Pattern variables cannot be syntax words or macros:
![ ?if ]=>
;;; MISHAP - INAPPROPRIATE PATTERN VARIABLE
;;; INVOLVING:  if
;;; etc.

NOW THE CODE
*/

compile_mode: pop11 +strict;

#_INCLUDE '$usepop/pop/lib/include/pop11_flags.ph'
#_INCLUDE '$usepop/pop/lib/include/vm_flags.ph'

section;

;;; A variable to control whether pattern variables default to lexical
global vars pop_pattern_lvars;
if isundef(pop_pattern_lvars) then
    true -> pop_pattern_lvars;  
endif;

;;; A variable to control whether automatic declarations are notified
global vars pop_lvars_warning;
if isundef(pop_lvars_warning) then
    ;;; assign default value.
    true -> pop_lvars_warning;  
endif;

;;; Stuff to control printing of identifiers

global vars procedure word_of_ident;
if isundef(word_of_ident) then
newproperty([],64, false, "tmparg") -> word_of_ident;
endif;

global vars pop_oc_print_level; ;;; for objectclass instances

define global vars pr_patt_var_warning(word);
    printf(word, ';;; WARNING DECLARING PATTERN VARIABLE %p AS LVARS\n');
    if popfilename then
        printf(popfilename, ';;; IN FILE %p\n');
    endif;
    if isinteger(poplinenum) then
        printf(poplinenum, ';;; LINE %p\n');
    endif
enddefine;
    

define vars print_ident(id);
    ;;; User definable procedure for printing identifiers. Can be changed
    ;;; according to context.
    dlocal pop_pr_level = 3, pop_oc_print_level = 3;

    lvars word;
    if isproperty(word_of_ident) and (word_of_ident(id)->>word) then
        printf('<ID %p %p>', [%word, idval(id)%])
    else
        printf('<ident %p>', [%idval(id)%])
    endif
enddefine;

define vars sys_print_ident(id);
    ;;; call user-definable procedure
    print_ident(id)
enddefine;

sys_print_ident -> class_print(ident_key);

applist([! where readpattern sysPUSHQ], sysunprotect);

lvars procedure oldPUSHQ;

define lconstant Readpattern();

    ;;; Read in a list or vector expression minus the closing bracket,
    ;;; replacing words following "?", "??" with the corresponding
    ;;; identifier, in nested lists, but not inside nested vectors.

    dlvars
        was_query = false,
        in_vector = iscaller(nonsyntax {),
        warnings = [];

    ;;; It would be nice to make patterns constants where possible,
    dlocal pop_pop11_flags = pop_pop11_flags || POP11_CONSTRUCTOR_CONSTS;

    ;;; Discount lexical idents pushed in patterns. I.e. don't treat as type 3
    ;;; See REF * VMCODE, and REF * pop_vm_flags
    ;;; IS THIS SAFE ???? A.S. Nov 1995
    dlocal pop_vm_flags = pop_vm_flags || VM_DISCOUNT_LEX_PROC_PUSHES;

    dlocal oldPUSHQ;
    if isundef(oldPUSHQ) then sysPUSHQ -> oldPUSHQ endif;

    define dlocal sysPUSHQ(item);
        lvars item;

        lvars invec, inlist, inpat;

        if was_query == ":" then
            if isinteger(item) or isprocedure(item) or isword(item)
            then
                oldPUSHQ(item)
            else
                mishap('WRONG RESTRICTION IN PATTERN', [^item])
            endif
        elseif was_query then
            dlocal pop_autoload = false;
            ;;; after "?" or "??". Should be a word or an identifier

            if isword(item) then
                lvars
                    id = sys_current_ident(item);

                if id and identprops(id) /== 0 then
                    ;;; mishap, e.g. op, macro or syntax word used as pattern
                    ;;; variable.
                    mishap(item, 1, 'INAPPROPRIATE PATTERN VARIABLE')
                elseif isident(id) == "perm" then
                    ;;; item not declared as lex, but is declared as perm
                    ;;; decide whether to force a lexical declaration or use the
                    ;;; permanent identifier
                    if pop_pattern_lvars then ;;; force lvar
                        ;;; if outside any procedure, don't declare variables
                        unless popexecute then
                            sysLVARS(item, 0);
                            if pop_lvars_warning then
                                unless lmember(item, warnings) then
                                    pr_patt_var_warning(item);
                                    item :: warnings -> warnings
                                endunless;
                            endif;
                        endunless
                    endif
                elseif not(id) then
                    ;;; Not declared
                    if popexecute then
                        ;;; outside any procedure, so simply declare it
                        sysdeclare(item);
                    else
                        ;;; Not declared, so make it lexically scoped.
                        sysLVARS(item, 0);
                        if pop_lvars_warning then
                            unless lmember(item, warnings) then
                                pr_patt_var_warning(item);
                                item :: warnings -> warnings
                            endunless;
                        endif;
                    endif
                else
                    ;;; must be either a lexical variable already or declared
                    ;;; as "free" in some way? handle these differently??
                endif;

                sysIDENT(item);
                ;;; Associate the identifier with the word, for trace printing
                ;;;     item -> word_of_ident(identof(item));
                oldPUSHQ(item);
                sysIDENT(item);
                sysUCALL("word_of_ident");

            elseif isident(item) then
                oldPUSHQ(item)
            else
                mishap(item, 1, 'NON-WORD AFTER ' sys_>< was_query)
            endif;
            false -> was_query;
        else
            oldPUSHQ(item);
            if lmember(item, #_< [? ?? ] >_#) then
                ;;; Could the next item be a query variable?
                if lmember(nextreaditem(), #_<[% "]", "}", "%", "^", """ %]>_# ) then
                    ;;; end of expression
                    false;
                elseif in_vector then
                    ;;; make sure "{" is higher in calling chain than Readpattern
                    ;;; and that current context is a list expression lower down
                    lvars
                        invec = iscaller(nonsyntax {),
                        inlist = iscaller(nonsyntax [),
                        inpat = iscaller(Readpattern);

                    (inpat < invec) and (inlist < inpat) and item
                elseif iscaller(nonsyntax {) then
                    ;;; in an embedded vector
                    false
                else
                    item
                endif
            else
                false
            endif ->  was_query
        endif;
    enddefine;

    ;;; Read in the list expression, planting special code
    apply(nonsyntax [);
    sys_grbg_list(warnings);
enddefine;

;;; Prepare for expressions of the form
;;;     ![ ....] where <exp>
;;; to be compiled as if written
;;;     (![ ....],  procedure; <exp> endprocedure)

global constant syntax where = pop_undef;

define constant readpattern(trywhere);
    ;;; Check that a list expression follows on proglist
    ;;; Read in its items using Readpattern to replace variable
    ;;; names with identifiers.
    lvars item, trywhere;

    readitem() -> item;
    if item /== "[" then
        mishap(item, 1, 'LIST (PATTERN) EXPECTED AFTER "!"')
    endif;

    ;;; Now read in the list, replacing pattern variables with
    ;;; identifiers
    Readpattern();

    ;;; Check if a "where <test> " expression follows. If so compile it as
    ;;; procedure; <test> endprocedure; and push the procedure. This is for
    ;;; matchers which accept a third argument which is a boolean procedure
    ;;; to determine whether a match is successful.

    if trywhere then
        if pop11_try_nextreaditem("where") then
            sysPROCEDURE("where",0);
            ;;; Read expression with precedence 11. (It's debatable whether it
            ;;; makes syntactic sense to grab operators of higher precedence
            ;;; than "!" itself, but it needs to be left this way for backward
            ;;; compatibility and allows and/or expresions to be included in the where
            ;;; expression.)
            pop11_comp_prec_expr(221,false) ->;
            sysPUSHQ(sysENDPROCEDURE())
        endif
    else
        if nextitem() == "where" then
            mishap('"where" FOUND WHERE NOT PERMITTED', [])
        endif
    endif;
enddefine;

define global syntax 7 ! = readpattern(%true%)
    ;;; The precedence must be less than that of doesmatch
enddefine;

applist([! readpattern where sysPUSHQ], sysprotect);

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Aug  8 2000
    Changed to use %p, not %P in printf strings.
--- Aaron Sloman, Jan 18 1997
    Fixe mis-spellt error message
--- Aaron Sloman, Oct 14 1996
    Changed the default for pop_lvars_warning back to true, and introduced
    pr_patt_var_warning
--- Aaron Sloman, Oct 12 1996
    Changed the default of pop_lvars_warning to false. It's too much of a
    nuisance otherwise.
--- Aaron Sloman, Sep 24 1996
    Introduce pop_lvars_warning: if true causes forced lvars declarations to
    be announced, via prwarning.

    Introduced new printing for identifiers, in accordance with
    Poprulebase, i.e. using <ID <name> <val> > format

--- Aaron Sloman, Dec  2 1995
    Changed to render lexically scoped all variables that have not yet
    been declared. Should perhaps do this to variables not declared in the
    current procedure, and not declared as "free" in the current
    procedure.
--- Aaron Sloman, Dec  2 1995
    Following discussion on comp.lang.pop changed to make it declare as
    lvars any undeclared variables.
--- Aaron Sloman, Nov 28 1995
    Stopped it replacing words after ":" with identifiers because this
    is not consistent with the main Pop-11 matcher.
--- Aaron Sloman, Nov 26 1995
    Extracted code from LIB FMATCHES.
    Cleaned up in various ways, including allowing {% ![ ?x ] %} to work.
    Created "!" as new syntax word.
*/
