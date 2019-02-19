/* --- Copyright University of Sussex 2005. All rights reserved. ----------
 > File:            C.all/src/syscomp/files.p
 > Purpose:
 > Author:          John Gibson, Jun 20 1988 (see revisions)
 */

/* -------------------------------------------------------------------------

                        FILE/FILENAME ROUTINES

--------------------------------------------------------------------------*/

#_INCLUDE 'common.ph'

section $-Popas;

global constant procedure
    is_tmp_file = newproperty([], 16, false, false);

lvars
    created_file_list = [];

#_IF not(DEF device_init_buffer)

define $-device_init_buffer(dev, len);
    lvars dev, len;
    if device_encoding(dev, true) then
        inits16(len)
    else
        inits(len)
    endif
enddefine;

#_ENDIF


define new_tmp_file() -> f with_nargs 3;
    lvars f;
    systmpfile() -> f;
    true -> is_tmp_file(f)
enddefine;

define add_created_file(name);
    lvars name;
    name :: created_file_list -> created_file_list
enddefine;
;;;
define file_create_control(dlcontext);
    lvars dlcontext;
    created_file_list, [] -> created_file_list
enddefine;
;;;
define updaterof file_create_control(old, dlcontext);
    lvars f, p, old, dlcontext;
    for f in created_file_list do
        if (is_tmp_file(f) ->> p) or dlcontext == 2 then
            ;;; p can be -sysunlink- in Unix
            if isprocedure(p) then p(f) else sysdelete(f) endif ->
        endif
    endfor;
    old -> created_file_list
enddefine;


;;; --- FILENAME PROCESSING ----------------------------------------------

define is_dir_path(fname);
    ;;; changed by A.Sloman
    dlocal pop_translate_envvars = false;
    lvars v = sys_fname(fname);
    ;;; is dir path if blank from nam (4) onwards
    f_subv(4,v) fi_> datalength(f_subv(1,v))
enddefine;

define new_fname_extn(fname, new_extn);
    lvars fname, new_extn;
    dlocal pop_translate_envvars = false;
    sys_fname(fname, 1,4) <> new_extn   ;;; 1,4 = pathnam
enddefine;

define global use_file_versions();
#_IF pop_debugging
    pop_file_versions
#_ELSE
    if pop_file_versions then 1 else false endif
#_ENDIF
enddefine;


;;; ---------------------------------------------------------------------

vars pop_arglist;       ;;; used for processing poparglist

define is_option(arg);
    lvars arg;
    isstring(arg) and datalength(arg) /== 0 and fast_subscrs(1,arg) == `-`
enddefine;

define process_arglist(list);
    lvars new_h, new_t, h, t, list;

        ;;; expand file arg beginning with `@` (or already opened device)
    define lconstant do_file_arg(arg) -> l;
        lvars arg, s, n, l, procedure rep;

        ;;; Copied from setpop.p
        define lconstant Make_sublists(list, top);
            lvars list, top, pair, last = false, s;
            fast_for pair on list do
                fast_front(pair) -> s;
                if s = '(' then
                    Make_sublists(fast_back(pair), false)
                                        -> (fast_front(pair), fast_back(pair))
                elseif s = ')' then
                    if top then
                        mishap(0, 'UNMATCHED ")" IN PROCESS ARGUMENTS')
                    endif;
                    return( if last then
                                [] -> fast_back(last);
                                list
                            else
                                []
                            endif, fast_back(pair) )
                endif;
                pair -> last
            endfor;
            unless top then
                mishap(0, 'UNMATCHED "(" IN PROCESS ARGUMENTS')
            endunless
        enddefine;

        if isstring(arg) then allbutfirst(1, arg) -> arg endif;
        line_repeater(arg, w_buffer) -> rep;
        [%  until (rep() ->> s) == termin do
                if issubstring(';;;', s) ->> n then
                    substring(1, n-1, s) -> s
                endif;
                sys_parse_string(s)
            enduntil
        %] -> l;
        Make_sublists(l, true)
    enddefine;

    returnif(null(list)) ([]);
    dest(list) -> (h, t);
    (h, t) -> (new_h, new_t);
    while isdevice(new_h)
    or (isstring(new_h) and datalength(new_h) /== 0
        and fast_subscrs(1,new_h) == `@`)
    do
        do_file_arg(new_h) nc_<> new_t -> new_h;
        returnif(null(new_h)) ([]);
        sys_grbg_destpair(new_h) -> (new_h, new_t)
    endwhile;
    if islist(new_h) then process_arglist(new_h) -> new_h endif;
    process_arglist(new_t) -> new_t;
    if new_h == h and new_t == t then
        list
    elseif isinheap(list) then
        (new_h, new_t) -> (hd(list), tl(list));
        list
    else
        new_h :: new_t
    endif
enddefine;

define get_option_arg(option, single) -> arg;
    lvars arg, option, single, s;
    if null(pop_arglist) then
        mishap(option, 1, 'MISSING ARGUMENT AFTER OPTION')
    endif;
    dest(pop_arglist) -> (arg, pop_arglist);
    if isstring(arg) then
        unless single then [^arg] -> arg endunless
    ;;; else a list
    elseif single then
        mishap(option, arg, 2, 'SINGLE ARGUMENT EXPECTED AFTER OPTION')
    else
        for s in arg do
            unless isstring(s) then
                mishap(s, 1, 'UNEXPECTED SUB-LIST IN ARGUMENT LIST')
            endunless
        endfor;
        copylist(arg) -> arg    ;;; so we can safely nc_<> stuff on the end
    endif
enddefine;


endsection;     /* $-Popas */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Jan  4 2005
    Last comment about this not fully working is now out of date. Getting
        $popsys/poplink_cmnd to be generated without '$popobjlib' being
        expanded required changes to the script $popsys/pglink
        as well as the change using pop_translate_envvars
--- Aaron Sloman, Dec 31 2004
    Changed is_dir_path(fname) and new_fname_extn(fname, new_extn)
        to make pop_translate_envvars false, so that files created don't
        have hard-wired file names. (Not fully working yet).
--- John Gibson, Oct  2 1997
        Added temp definition of device_init_buffer if not defined.
--- John Gibson, May  2 1993
        Removed exp*and_wlib_arg
--- John Gibson, Oct 14 1992
        14.22 changes
--- John Gibson, Aug 31 1990
        Changes to cope with list args in poparglist from 13.83
--- John Gibson, Aug  4 1989
        Version 13.66+
--- John Gibson, Jul 17 1989
        Version 13.66
--- John Gibson, Jun  7 1989
        Included common.ph
 */
