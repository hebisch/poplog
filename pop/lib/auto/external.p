/* --- Copyright University of Sussex 1993.  All rights reserved. ---------
 > File:            C.all/lib/auto/external.p
 > Purpose:         General interface for external loading (etc)
 > Author:          Aled Morris and Robert James Duncan (see revisions)
 >                  (Originally based on a package by David Young)
 > Documentation:   HELP * EXTERNAL, * NEWC_DEC, REF * EXTERNAL
 > Related Files:   LIB * FORTRAN_DEC, LIB * C_DEC
 >                  LIB * NEWEXTERNAL, LIB * NEWC_DEC
 */
compile_mode :pop11 +strict;

/*****************************************************************************/
/*****               external load - special syntax                      *****/
/*****************************************************************************/

/* prevent double-loading of this file */
#_TERMIN_IF DEF external


section $-external => external, endexternal;

uses
    external_runtime,
    exload,             ;;; for exload_addbatch
;


define language_name_table = newassoc([]) enddefine;


/* this is used to associate lists of (external) procedures with the
 * user-supplied tags
 */
define imported_procedures = newassoc([]) enddefine;

vars current_tag;

define add_import(popname, symname, exptr_id, acc_p);
    lvars   popname, symname, exptr_id, acc_p, vec,
            plist = imported_procedures(current_tag);
    if lmember(popname, plist) ->> vec then
        hd(tl(vec)) -> vec
    else
        initv(3) -> vec;
        [^popname ^vec] nc_<> plist -> imported_procedures(current_tag)
    endif;
    symname     -> subscrv(1,vec);  ;;; may be consref(langname)
    exptr_id    -> subscrv(2,vec);
    acc_p /== identfn and acc_p
                -> subscrv(3,vec);
enddefine;

define vars external_import_raw(popname, symname);
    lvars popname, symname, extpdr;
    sysSYNTAX(popname, 0, false);
    sysGLOBAL(popname);
    add_import(popname, symname, identof(popname), false)
enddefine;

/*  The routine which declares a Pop11 identifier and builds a closure
 *  on pop_ext_call which it assigns as the variable's value
 *
 *  The arguments are:
 *      symname     consref(language_name) where language_name is a word (in
 *                  this case popname is used for the word symbol name to
 *                  external_do_load. May also be the actual linker's name
 *                  for the external procedure (string or word).
 *      popname     the name of a Pop-11 variable.  If the variable is
 *                  undeclared, -sysSYNTAX- is used to create it.
 *      argspec     a vector containing type-specifiers, one per
 *                  formal parameter of the external procedure
 *      retspec     a single type-specifier detailing the return value of
 *                  the external procedure
 *      errpdr      an error procedure for signalling data type conflicts
 *                  (may be false)
 *      cbr         a flag indicating whether call-by-reference should be
 *                  simulated.
 *
 *  Most of these are used as frozen values for the closure on
 *  pop_ext_call (q.v.)
 */
define vars external_import(symname, popname, argspec, retspec, errpdr, cbr);
    lvars   symname, popname, argspec, retspec, errpdr, cbr,
            expdr_id = false, val, Ext_call;
    dlocal  pop_autoload = false;

    sys_current_val("ident $-external$-pop_ext_call") -> Ext_call;

    if isdeclared(popname) then
        sys_current_val(popname) -> val;
        if isclosure(val) and pdpart(val) == Ext_call then
            frozval(3, val) -> expdr_id
        endif
    else
        sysSYNTAX(popname, "procedure", false);
        sysGLOBAL(popname)
    endif;
    unless expdr_id then
        consident(0, false, "lex") -> expdr_id;
        false -> idval(expdr_id)
    endunless;

    unless errpdr then
        ;;; supply language name if given
        isref(symname) and cont(symname) -> errpdr
    endunless;

    Ext_call(% argspec, retspec, expdr_id, errpdr, cbr, 0 %)
                ->> val ->> frozval(6, val) -> sys_current_val(popname);
    false -> updater(val);              ;;; for Popc
    pop11_define_props(word_identifier(popname, current_section, true),
                        popname, false) -> pdprops(val);

    add_import(popname, symname, expdr_id, false)
enddefine;


/*****
 *  The main interface routine.
 *
 *  Has (currently) 4 modes of operation:
 *
 *      declare     invokes a compiler-type procedure defined in
 *                  a subsidiary library file to parse declarations
 *                  and construct Pop11 wrapping procedures for any
 *                  external procedures so defined.
 *
 *      load        reads a list of filenames and invokes exload_addbatch
 *
 *      unload      invokes external_unload
 *
 *  The syntax is as follows:
 *
 *      external <word:O> <word:T> [in <word:L>]
 *
 *  Where:
 *      O   operation (as listed above)
 *      T   a tag to identify the operation
 *      L   where the operation is "declare", a language must
 *          be specified
 */

vars syntax endexternal = pop_undef;

define vars syntax external;
    lvars   op, files, symname, popname, plist, lang_prefix
        ;
    dlocal  current_tag
        ;

    define lconstant read_files();
        lvars fname;
        dlocal pop_autoload = false;
        [%  until (itemread() ->> fname) == "endexternal" do
                fname;
                pop11_try_nextitem([, ;]) ->;
            enduntil;
        %]
    enddefine;

    define lconstant LoadThenDoMe(my_id, real_pdr, exptr_id);
        lvars my_id, real_pdr, exptr_id;
        unless idval(exptr_id) then exload_do_batch_load() endunless;
        chain(real_pdr ->> idval(my_id));
    enddefine;

    define lconstant do_load(files, require);
        lvars   files, vec, popname, symname, langname, exptr_id, acc_p,
                require, syms, plist = imported_procedures(current_tag);
        [%  until plist == [] do
                dest(dest(plist)) -> (popname, vec, plist);
                explode(vec) -> (symname, exptr_id, acc_p);

                if require and exptr_id /== identof(popname) then
                    ;;; backward compatibility for "require"
                    LoadThenDoMe(% identof(popname), valof(popname), exptr_id %)
                        -> valof(popname);
                endif;

                nullstring -> langname;
                if isref(symname) then
                    fast_word_string(cont(symname)) -> langname;    ;;; langname string
                    popname
                elseif isword(symname) then
                    word_string(symname)
                elseif symname then
                    symname
                else
                    popname
                endif -> symname;

                ;;; symbol vector for exload_addbatch
                {% symname, langname, undef, undef,
                        {% undef, undef, undef, exptr_id, acc_p %} %};

            enduntil
        %] -> syms;

        $-typespec_utils$-exload_addbatch(current_tag, files, syms);
        if pop_pas_mode == "popc" then
            ;;; Popc doesn't batch
            [] -> imported_procedures(current_tag)
        elseunless $-typespec_utils$-exload_isbatching or require then
            exload_do_batch_load()
        endif
    enddefine;

    unless popexecute then
        mishap(0, 'external ONLY WORKS AT TOP LEVEL');
    endunless;

    unless (readitem() ->> op).isword then
        mishap(op, 1, 'INVALID external KEYWORD (not a word)');
    endunless;
    unless (readitem() ->> current_tag).isword then
        mishap(current_tag, 1, 'INVALID external TAG (word needed)');
    endunless;

    imported_procedures(current_tag) -> plist;

    if op == "declare" then
        unless plist then [] -> imported_procedures(current_tag) endunless;
        if pop11_try_nextreaditem("in") then
            unless (readitem() ->> lang_prefix).isword then
                mishap(lang_prefix, 1, 'Invalid language name (word needed)');
            endunless;
            pop11_need_nextreaditem(";") ->;

            apply(valof(language_name_table(lang_prefix)
                        or lang_prefix <> "_dec"
                       )
                 );
        else
            pop11_need_nextreaditem(";") ->;
            until pop11_try_nextreaditem("endexternal") do
                readitem() ->> popname -> symname;
                if pop11_try_nextreaditem("=") then
                    readitem() -> symname;
                endif;
                external_import_raw(popname, symname);
                pop11_try_nextreaditem([, ;]) ->
            enduntil;
        endif;
        ;;; for Popc, don't bother about waiting for an explicit load (then
        ;;; we don't have to worry about flushing at the end of each file)
        if pop_pas_mode == "popc" then do_load([], false) endif;

    elseif op == "load" or op == "require" then
        pop11_need_nextreaditem(";") ->;
        unless plist then
            mishap(current_tag, 1, 'external TAG HAS NOT BEEN DECLARED');
        endunless;
        read_files() -> files;
        do_load(files,  op == "require"
                        and not($-typespec_utils$-exload_isbatching
                                or pop_pas_mode == "popc"))

    elseif op == "unload" then
        unless plist then
            mishap(current_tag, 1, 'external TAG HAS NOT BEEN DECLARED');
        endunless;
        external_unload(current_tag);

    else
        mishap(op, 1, 'INVALID external KEYWORD');

    endif;
enddefine;

endsection;     /* $-external */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Dec 21 1993
        Rewritten to use the exload facilities, so that it works with
        exload_batch etc. ("require" is now only different from "load" when
        not batching.)
        Runtime parts moved to lib external_runtime.
--- John Gibson, Aug 18 1992
        In check_type_range, changed test for "int" type to be isintegral
        rather than isinteger (external calling passes it mod 2**32)
--- Simon Nichols, May 13 1992
        Fixed the last fix (see bugreport davidy.62).
--- Ian Rogers, Jan 23 1992
    Made -pop_ext_call- closures writeable cf. BR rogere.53
--- Ian Rogers, Dec 18 1991
    Identifiers declared by -external_import- are now global
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- John Gibson, Apr  3 1991
        Added "sfloat" type as alternative to "float", except that "sfloat"
        means a definite single-float (not what C returns for "float").
        Replaced old -external_apply- with switch to appropriate -exacc-
        case.
--- Simon Nichols, Jan 29 1991
        Removed top level assignment of <true> to -pop_optimise-, which has
        a permanent effect.
--- Jonathan Meyer, Jan 25 1991
        fixed bug in LoadThenDoMe (made it use is_valid_external_ptr)
--- Aaron Sloman, Sep 15 1990
        Amended header to mention David Young and add more cross
        references.
--- Aaron Sloman, Jul  2 1990
        Removed pop11_need_nextreaditem - now in system
--- Ian Rogers, Feb 21 1990 (Actually Roger Evans, Nov  4 1988)
        removed lexical scoping of pop_ext_call
        (used by new fortran_dec.p)
--- Ian Rogers, Jan 17 1990
        Fixed -ADDRESS- for new 3rd-longword pointers
--- Ian Rogers, Oct 12 1989
        Added "external require"
--- Ian Rogers, Aug 17 1989
        Moved in code for run-time array-bounds checking (David Wheable -
        Aug 25, 1988) from LIB * NEWEXTERNAL
--- John Gibson, Aug 13 1989
        Replaced old sys- procedures with pop11_ versions.
--- Roger Evans, Jul 27 1988 - pop_ext_call now forces ddecimals into
    idents that originally contained dddecimals (SFR 4204)
 */
