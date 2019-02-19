/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:            C.all/src/sys_fname.p
 > Purpose:
 > Author:          John Gibson, Jul 31 1989
 > Documentation:   REF *SYSUTIL
 */

;;; ------------------ GETTING FILENAME COMPONENTS -------------------------

#_INCLUDE 'declare.ph'

;;; ------------------------------------------------------------------------

lconstant macro MAX_FLD = 6;

lconstant procedure parse = newproperty([], 32, false, "tmpclr");

procedure(string, prop);
    lvars string, procedure prop;
    consvector(sysfileok(string, true), MAX_FLD) ->> prop(string)
endprocedure -> property_active(parse);

define sys_fname(f2);
    lvars f1, f2, v, string;
    returnunless(isinteger(f2)) (parse(f2));

    unless isinteger(dup()) then f2 endunless -> f1;
    parse(/*string*/) -> v;
    unless 1 fi_<= f1 and f1 fi_<= f2 and f2 fi_<= MAX_FLD then
        mishap(f1, f2, 2, 'sys_fname: INVALID RANGE ARGUMENTS')
    endunless;
    fast_subscrv(1, v) -> string;
    if f1 == 1 then
        if f2 == MAX_FLD then
            string
        else
            substring(1, fast_subscrv(f2 fi_+ 1, v) fi_- 1, string)
        endif
    else
        fast_subscrv(f1, v) -> f1;
        substring(  f1,
                    if f2 == MAX_FLD then
                        datalength(string) fi_+ 1
                    else
                        fast_subscrv(f2 fi_+ 1, v)
                    endif fi_- f1,
                    string)
    endif
enddefine;

define sys_fname_path   = sys_fname(% 1,3 %) enddefine; ;;; host,disk,dir
define sys_fname_nam    = sys_fname(%  4  %) enddefine; ;;; nam
define sys_fname_name   = sys_fname(% 4,5 %) enddefine; ;;; nam,extn
define sys_fname_namev  = sys_fname(% 4,6 %) enddefine; ;;; nam,extn,ver
define sys_fname_extn   = sys_fname(%  5  %) enddefine; ;;; extn

/*  N.B. Not currently included (because not so useful) are

    sys_fname_host      = sys_fname(%  1  %)
    sys_fname_disk      = sys_fname(%  2  %)
    sys_fname_dir       = sys_fname(%  3  %)
    sys_fname_vers      = sys_fname(%  6  %)

    sys_fname_pathnam   = sys_fname(% 1,4 %)    ;;; all but extn,vers
    sys_fname_pathname  = sys_fname(% 1,5 %)    ;;; all but vers

    etc.
*/
