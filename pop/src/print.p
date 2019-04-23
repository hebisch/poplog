/* --- Copyright University of Sussex 1997.  All rights reserved. ---------
 > File:        C.all/src/print.p
 > Purpose:
 > Author:      John Gibson (see revisions)
 */

;;; --------------------- PRINTING THINGS ---------------------------------

#_INCLUDE 'declare.ph'

constant
        procedure (fast_word_string, is_poplog_item, Sys$-Print_mcint,
        Sys$-Os_error_message)
    ;


;;; ---------------------------------------------------------------------

section $-Sys => printf, sys_message_printf, sys_syspr, syspr, pr, spr,
                 pop_pr_level, pop_pr_radix, pop_pr_quotes;

vars
    procedure pr    = syspr,
    pop_pr_radix    = 10,
    pop_pr_quotes   = false,
    ;


;;; --- FORMATTED PRINT ---------------------------------------------------

lblock

lvars spec, arglist;

define lconstant Char_printf(char);
    lvars char, save, _count, _item;

    if char == `%` then
        if spec == true then    ;;; second percent sign
            cucharout(char)
        endif;
        not(spec) -> spec;
        return
    elseunless spec then
        cucharout(char);
        return
    elseif char == `M` then
        ;;; print O/S error message (no arg)
        Print_str(Os_error_message());
        false -> spec;
        return
    endif;

    if arglist then
        if null(arglist) then
            mishap(0, 'ITEM LIST FOR printf TOO SHORT')
        else
            arglist -> save;
            fast_destpair(arglist) -> arglist   ;;; leave it on stack
        endif
    endif;

    if char == `x` then
        ;;; machine int in unsigned hex
        define lconstant hexpr(_n, _count);
            lvars _n, _count, _c = _pint(_n _bimask _16:F);
            if _nonzero(_count _sub _1 ->> _count) then
                hexpr(_shift(_n, _-4), _count)
            endif;
            cucharout(_c fi_+ (if _c fi_< 10 then `0` else `7` endif))
        enddefine;
        hexpr((), ##(b)[_2|w])  ;;; number of nibbles in a word

    elseif char == `b` then
        ;;; byte at address
        pr(_pint( _int()!(b) ))

    elseif char == `i` or char == `d` then
        ;;; pop int or machine int in decimal
        Print_mcint(if char == `i` then _int() endif, _10)

    elseif char == `s` then
        ;;; string
        false -> spec;
        appdata(Char_printf)

    elseif char == `S` then
        ;;; string (or word) printed literally
        if isword(dup()) then
            fast_word_string()
        else
            Check_string(dup())
        endif;
        Print_str()

    elseif char == `c` then
        cucharout()

    elseif char == `p` then
        ;;; any pop item, printed with pr
        pr()

    elseif char == `P` then
        ;;; any pop item, printed with sys_syspr
        sys_syspr()

    else
        if arglist then
            -> ;
            save -> arglist
        endif;
        cucharout(char)
    endif;
    false -> spec
enddefine;

define printf(string);
    lvars string;
    dlocal spec = false, arglist = false;
    if islist(string) then
        (), string -> (string, arglist)
    else
        Check_string(string)
    endif;
    appdata(string, Char_printf)
enddefine;

    /*  Used by sys_pr_message
    */
define sys_message_printf(string, arglist) -> arglist;
    lvars string;
    dlocal arglist;
    Check_string(string);
    unless islist(arglist) then
        mishap(arglist, 1, 'LIST NEEDED')
    endunless;
    if _nonzero(string!V_LENGTH) and fast_subscrs(1,string) == `%` then
        ;;; use printf but ignore the first %
        dlocal spec = 0;
        appdata(string, Char_printf)
    else
        Print_str(string)
    endif
enddefine;

endlblock;


;;; --- STANDARD PRINTING -------------------------------------------------

    ;;; counts printing level
lvars
    pr_level    = 1000;

define active pop_pr_level; pr_level enddefine;
;;;
define updaterof active pop_pr_level(n);
    lvars n;
    if isinteger(n) then
        if n fi_< 0 then 0 else n endif -> pr_level
    else
        mishap(n, 1, 'ASSIGNING NON-INTEGER TO pop_pr_level')
    endif
enddefine;

    ;;; Return the key for printing an object, checking for
    ;;; invalid pointers etc.

define lconstant Get_print_key(item) -> key;
    lvars item, key, _radix = pop_pr_radix;

    _checkall();

    ;;; decrement print level
    if pr_level fi_> 0 then pr_level fi_- 1 -> pr_level endif;

    ;;; check pop_pr_radix
    unless _radix == 10
    or (isinteger(_radix) and 2 fi_<= _radix and _radix fi_<= 36) then
        10 -> pop_pr_radix
    endunless;

    ;;; return key
    returnif(is_poplog_item(item) ->> key);

    ;;; system object
    printf( if item == _NULL then
                '<NULL>'
            else
                '<SYSTEM_OBJECT %x>', conspair(item, [])
            endif )
enddefine;

    ;;; print an object using the system class print procedure
define sys_syspr(item);
    lvars item, _key;
    dlocal pr_level;
    if Get_print_key(item) ->> _key then
        fast_apply(item, _key!K_SYS_PRINT)
    endif
enddefine;

    ;;; print an object using the user class print procedure
define syspr(item);
    lvars item, _key;
    dlocal pr_level;
    if Get_print_key(item) ->> _key then
        fast_apply(item, fast_cont(_key!K_PRINT))
    endif
enddefine;

define Print_str(string);
    lvars string, procedure c_out = cucharout, _coffs, _lim;
    if string!KEY!K_FLAGS _bitst _:M_K_STRING16 then
        @@V_SHORTS[_0] -> _coffs;
        @@V_SHORTS[string!V_LENGTH] -> _lim;
        while _coffs _lt _lim do
            _CHECKUSER;
            c_out(_pint( string!(w->s){_coffs} ));
            @@(s){_coffs}++ -> _coffs
        endwhile
    else
        @@V_BYTES[_0] -> _coffs;
        @@V_BYTES[string!V_LENGTH] -> _lim;
        while _coffs _lt _lim do
            _CHECKUSER;
            c_out(_pint( string!(w->b){_coffs} ));
            @@(b){_coffs}++ -> _coffs
        endwhile
    endif
enddefine;

define Default_print(arg, other);
    lvars arg, other;
    cucharout(`<`);
    unless isword(arg) then dataword(arg) -> arg endunless;
    if isword(arg) then Print_str(arg!W_STRING) else sys_syspr(arg) endif;
    if other and pop_pr_level /== 0 then cucharout(`\s`), pr(other) endif;
    cucharout(`>`)
enddefine;

constant procedure
    Minimal_print   = Default_print(%false%);

define Data_print(item);
    lvars item;
    if pop_pr_level == 0 then return(Minimal_print(item)) endif;
    cucharout(`<`);
    sys_syspr(dataword(item));
    appdata(item,   procedure(item);
                        lvars item; cucharout(`\s`), pr(item)
                    endprocedure);
    cucharout(`>`)
enddefine;

define spr(item);
    lvars item;
    pr(item);
    cucharout(`\s`)
enddefine;

endsection;     /* $-Sys */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Feb  5 1997
        String16 changes.
--- Robert Duncan, May 30 1996
        Fixed %M case in Char_printf to reset spec flag
--- John Gibson, May 13 1996
        Message_printf -> exported sys_message_printf
--- John Gibson, Apr 12 1996
        Added %M specifier to printf to mean print sys_os_error_message.
--- John Gibson, Feb  6 1996
        Added Message_printf
--- John Williams, Jan 25 1993
        Added %S option to printf (print string literally) (cf. BR johnw.1049)
--- John Gibson, Oct 19 1990
        -Char_printf- changed so that %d works with cucharout = identfn
        (instead of going into a loop!)
--- John Gibson, May  3 1990
        Made -Get_print_key- print _NULL as <NULL>.
--- John Williams, Jun 10 1988
        Changed -Get_print_key- to use -is_poplog_item-
--- John Gibson, Mar 13 1988
        Moved -pr_field- and -sysprarrow- to separate files.
--- John Gibson, Feb 22 1988
        Check_string into section Sys
--- John Gibson, Feb 11 1988
        Check_integer in section Sys
--- John Gibson, Feb  9 1988
        decimal_key -> Sys$-decimal_key etc
 */
