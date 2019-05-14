/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/lib/lib/shadowclass_data.p
 > Purpose:         Access to shadowkey fields
 > Author:          Roger Evans, Nov 15 1990 (see revisions)
 > Documentation:   REF *SHADOWCLASS
 > Related Files:   LIB * SHADOWKEY_KEY, * CONSSHADOWKEY, * SHADOWCLASS
 */
compile_mode:pop11 +strict;

section $-shadowkey =>
        shadowclass_recognise shadowclass_init shadowclass_refresh
        shadowclass_cons shadowclass_dest shadowclass_fill
        shadowclass_subscr shadowclass_field shadowclass_import
;

lconstant names = [
        shadowclass_recognise shadowclass_init shadowclass_refresh
        shadowclass_cons shadowclass_dest shadowclass_fill
        shadowclass_subscr shadowclass_field shadowclass_import
];
applist(names, sysunprotect);

include shadowkey.ph;

uses shadowkey_key;

lconstant unary_flags = [nc fast refresh];


;;; -- Access to class procedures -------------------------------------

define lconstant Class_proc_flags(key) -> (key,flags);
    lvars key flags = #_< [dummy] >_#;
    if islist(key) then
        key -> flags; -> key;
    elseif lmember(key, unary_flags) then
        key -> hd(flags); -> key;
    else
        [] -> flags;
    endif;
    unless isshadowkey(key) then
        mishap(key,1,'SHADOWKEY NEEDED');
    endunless;
enddefine;

define lconstant Class_proc(key,proc);
    lvars key proc;
    fast_subscrv(proc,shk_procs(key));
enddefine;

define lconstant Class_proc_nc(nc_off,c_off);
    lvars key, flags, nc_off, c_off;
    Class_proc_flags() -> (key,flags);
    Class_proc(key,if lmember("nc",flags) then nc_off else c_off endif);
enddefine;

define shadowclass_cons = Class_proc_nc(%SHK_NC_CONS,SHK_CONS%) enddefine;
define shadowclass_dest = Class_proc_nc(%SHK_NC_DEST,SHK_DEST%) enddefine;
define shadowclass_fill = Class_proc_nc(%SHK_NC_FILL,SHK_FILL%) enddefine;

define shadowclass_recognise = Class_proc(%SHK_RECOG%); enddefine;
define shadowclass_init = Class_proc(%SHK_INIT%); enddefine;
define shadowclass_refresh = Class_proc(%SHK_REFRESH%); enddefine;
define shadowclass_import = Class_proc(%SHK_IMPORT%); enddefine;

define shadowclass_subscr() -> proc with_nargs 1;
    lvars key, flags, proc;

    Class_proc_flags() -> (key,flags);

    if lmember("refresh",flags) then
        Class_proc_nc(key,flags,SHK_NC_RF_SUBSCR,SHK_RF_SUBSCR);
    else
        Class_proc_nc(key,flags,SHK_NC_SUBSCR,SHK_SUBSCR);
    endif -> proc;

    unless lmember("fast",flags) then
        Class_proc(key,SHK_CHECK) <> proc -> proc;
    endunless;
enddefine;

/* do field accessors with subscriptors by swapping args */
define shadowclass_field() -> field_p with_nargs 2;
    lvars (num, subs_p) = shadowclass_subscr(), field_p;
    shkP_Field_swap(%subs_p,num%) -> field_p;
    shkP_Field_swap(%updater(subs_p),num%) -> updater(field_p);
enddefine;

applist(names, sysprotect);

constant $-shadowclass_data = true;

endsection;     /* $-shadowkey */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Jun 11 1993
        Fixed shadowclass_field to give the field procedures updaters!
--- John Gibson, May  5 1993
        Split off from lib shadowkey;
 */
