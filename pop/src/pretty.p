/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/src/pretty.p
 > Purpose:         "pretty" printing of list and vector expressions
 > Author:          Steve Hardy (see revisions)
 */

;;;------------------- PRETTY PRINTING --------------------------------------

#_INCLUDE 'declare.ph'

global constant procedure
    (datalist, prolog_undefvar, class_print, is_poplog_item)
    ;

global vars
    pop_=>_flag,
    ;

weak global vars
    vedediting, vedscreenwidth,
    ;

;;; ---------------------------------------------------------------------

section $-Sys => pretty;


define pretty(item);
    lvars   item, ppr_lwidth, ppr_lmax, ppr_count = 0,
            procedure ppr_cucharout = cucharout;
    dlocal  poplinemax;

    define lconstant Pretty(level, item);
        lvars   item, level, break, closebracket;
        dlocal  pop_pr_level;

        define lconstant Ppr_simple(item) -> simple;
            lvars item, key, simple = true;
            if iscompound(item) then
                if is_poplog_item(item) ->> key then
                    if key!K_FLAGS _bitst (_:M_K_VECTOR _biset _:M_K_RECORD)
                    and not(key!K_FLAGS _bitst _:M_K_STRING)
                    and class_print(key) == sys_syspr
                    then
                        if key == pair_key then
                            isprocedure(fast_back(item))
                        elseif key == weakref prologvar_key then
                            weakref[prologvar_key] prolog_undefvar(item)
                        else
                            false
                        endif -> simple;
                    endif;
                endif;
            endif;
        enddefine;

        define lconstant Ppr_fits(item, nchars) -> fits;
            lvars item, nchars, fits = true;

            define dlocal cucharout(c);
                lvars c;
                if nchars fi_<= 0 then
                    false -> fits;
                    exitto(Ppr_fits);
                endif;
                nchars fi_- 1 -> nchars;
            enddefine;

            erasenum(#| pr(item) |#);
        enddefine;

        define lconstant Ppr_brackets(item);
            lvars item;
            if ispair(item) then
                '[', ']'
            elseif isvector(item) then
                '{', '}'
            else
                ('<' sys_>< dataword(item)) <> ' ', '>'
            endif
        enddefine;

        define lconstant Ppr_atend(list);
            lvars list;
            list == [] or ispair(list) and isprocedure(fast_back(list));
        enddefine;

        _checkall();    ;;; check for interrupts / recursion overflow
        if pop_pr_level == 0
        or Ppr_simple(item)
        or Ppr_fits(item, ppr_lwidth fi_- level)
        then
            pr(item);
            return;
        endif;

        pop_pr_level - 1 -> pop_pr_level;
        printf(Ppr_brackets(item) -> closebracket);
        unless ispair(item) then datalist(item) -> item endunless;
        if Ppr_simple(fast_front(item)) and Ppr_fits(fast_front(item), 9)
        then
            pr(fast_front(item));
            ppr_count -> level;
            ppr_count fi_>= ppr_lwidth -> break;
        else
            ppr_count fi_- 1 -> level;
            Pretty(ppr_count, fast_front(item));
            true -> break;
        endif;
        fast_back(item) -> item;
        while ispair(item) and not(Ppr_atend(item)) do
            _checkinterrupt();
            if break then cucharout(`\n`) endif;
            until ppr_count fi_>= level do cucharout(`\s`) enduntil;
            if Ppr_simple(fast_front(item))
            and Ppr_fits(fast_front(item), ppr_lmax fi_- ppr_count fi_- 1)
            then
                cucharout(`\s`);
                pr(fast_front(item));
                fast_back(item) -> item;
                ppr_count fi_>= ppr_lwidth -> break;
            elseif ppr_count == level then
                cucharout(`\s`);
                Pretty(ppr_count, fast_front(item));
                fast_back(item) -> item;
                true -> break;
            else
                true -> break;
            endif;
        endwhile;
        if Ppr_atend(item) then
            if ispair(item) and fast_front(item) then
                printf(' ...');
            endif;
        elseif not(break)
        and Ppr_simple(item)
        and Ppr_fits(item, ppr_lmax fi_- ppr_count fi_- 1)
        then
            cucharout(`|`);
            pr(item);
        else
            cucharout(`\n`);
            until ppr_count fi_>= level do cucharout(`\s`) enduntil;
            cucharout(`|`);
            Pretty(ppr_count, item);
        endif;
        printf(closebracket);
    enddefine;      /* Pretty */

    define dlocal cucharout(c);
        lvars c;
        ppr_count fi_+ 1 -> ppr_count;
        if c == `\n` then 0 -> ppr_count endif;
        ppr_cucharout(c);
    enddefine;

    if testdef vedscreenwidth
    and testdef vedediting and weakref vedediting
    then
        ;;; don't allow characters off-screen
        min(poplinemax, weakref vedscreenwidth - 3) -> poplinemax;
    endif;
    poplinemax - 3 ->> ppr_lmax -> ppr_lwidth;
    if (ppr_cucharout == charout or ppr_cucharout == charerr)
    and isinteger(poplinewidth) and poplinewidth fi_< ppr_lwidth
    then
        ;;; spaces printed beyond here will cause line-breaks
        poplinewidth -> ppr_lwidth;
    endif;

    appdata(pop_=>_flag, cucharout);
    Pretty(ppr_count, item);
    cucharout(`\n`);
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 17 1993
        Moved =*=> to pop11_syntax.p
--- Robert John Duncan, Mar 13 1992
        Changed use of -poplinewidth- again for better control of line-breaks
--- Robert John Duncan, Mar 12 1992
        Removed lots of redundant character counting.
        Stopped the printing of trailing spaces which could cause spurious
        blank lines to appear when printing through -charout-.
        Improved treatment of non-list pairs.
        Fixed buggy use of -poplinewidth- (may not be an integer).
        Added use of -pop_pr_level- to limit recursion.
        Tidied up, removing some redundant code.
--- Aaron Sloman, Sep 30 1990
        Altered pretty so as never to increase poplinewidth or poplinemax,
        in case set by user
--- Simon Nichols, Jun 12 1990
        Changed -Ppr_simple- to return <true> if -item- is a pair but not
        a list. See bug report garethp.1.
--- Aaron Sloman, Mar  9 1990
    changed buggy assignment to poplinewidth: made it poplinemax - 1
    not + 1.
--- Aaron Sloman, Dec 27 1989
    Altered poplinewidth to be vedscreenwidth - 3 if necessary, to prevent
    unwelcome sideways scrolling in VED.

--- Rob Duncan, Dec  1 1989
        Changed -dlvars- to -lvars- in -pretty- to make it reentrant: see
        bugreport rudil.3
--- John Gibson, Jul 28 1988
        Weakened -prolog_undefvar- reference
--- John Williams, Jun 10 1988
        Put in section $-Sys
        Fixed SFR 4175 (pretty printing 'system objects')
--- John Gibson, Sep  5 1987
        Changed access to K_PRINT field in Ppr_simple to use -class_print-
        instead.
--- Rob Duncan 6/1/87,
        Added test to -Ppr_simple- to stop the pretty printer looping on
        uninstantiated prolog vars. See bug report chrism.27.
 */
