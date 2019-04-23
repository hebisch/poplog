/* --- Copyright University of Sussex 2008. All rights reserved. ----------
 > File:            C.all/ved/src/vdfiles.p
 > Purpose:         Ved file structures
 > Author:          A.Sloman et. al. (see revisions)
 */

;;; -------------------- FILE STRUCTURES -------------------------------------

#_INCLUDE 'vddeclare.ph'
#_INCLUDE '../../lib/include/vm_flags.ph'

constant
        procedure (vedfileisonscreen, Sys$-Get_full_pathname,
            Sys$-Noprot_assign)
    ;

vars
        procedure (ved_ved, vedveddefaults, vedinitialise,
        ved_write_current_file),
        vednonbreakfiles, vedsearchlist, pop_filename_case,
    ;


;;; ----------------------------------------------------------------------

section $-Sys$-Ved =>
                    vedautowrite, vednonbreakfiles, vedfiletypes,
                    lispfiletypes, mlfiletypes, prologfiletypes,
                    wved_save_globals, wved_set_globals, wved_ved_name_hook,

                    vedcurrent, veddirectory, vedbuffer,
                    vedline, vedlineoffset, vedcolumn, vedcolumnoffset,
                    vedstatic, vedbreak, vedchanged, vedwriteable,
                    vedneedscompiling, vedcompileable,
                    vedpositionstack, vvedmarklo, vvedmarkhi, vvedmarkprops,
                    vvedbuffersize, vvedlinesize, vednamestring, vedleftmargin,
                    vedlinemax, vednotabs, vedindentstep, vedwindowlength,
                    vedscreenoffset, vedfileprops, vedprocwait,
                    vedstatusline, wvedwindow, vedpathname, vedwasonstatus,

                    ved_current_file, ved_save_file_globals, is_vedfile_local,
                    vedsearchfiletypes, vedsetfiletypes,
                    vedappfiles, vedpresent, vedbuffername,
                    ved_name, ved_mafw,
                    ved_file_change_apply, is_ved_output_file,
                    vedsaveglobals, vedsetglobals,
                ;

vars
    vedautowrite        = 1500,

    ;;; These are user assignable, used in vedfiletypes
    vednonbreakfiles    = ['.p' '.com' lispfiletypes prologfiletypes mlfiletypes],
    lispfiletypes       = ['.lsp' '.l' '.lisp' '.cl'],
    mlfiletypes         = ['.ml' '.sig'],
    prologfiletypes     = ['.pl' '.pro'],

    ;;; vedfiletypes is used by vedsetfiletypes.
    ;;; File-type represented by either a string or list of strings,
    ;;; or a procedure or list of procedures, producing a boolean result.
    ;;; Action represented by a list of name value vectors or procedures

    vedfiletypes        = [
        ;;; first the default case
        ['' {vednotabs ^true} {vedbreak ^true} {vedindentstep 4}]
        ['.p'               {subsystem "pop11}  {vedcompileable ^true}]
        [lispfiletypes      {subsystem "lisp}   {vedcompileable ^true}]
        [prologfiletypes    {subsystem "prolog} {vedcompileable ^true}]
        [mlfiletypes        {subsystem "ml}     {vedcompileable ^true}]
        [is_ved_output_file {vedcompileable ^false}]
        [vednonbreakfiles   {vedbreak ^false}]
    ],
;

vars procedure (
    ;;; Procedures for use in ved_current_file
    wved_set_globals        = erase,
    wved_save_globals       = erase,

    wved_ved_name_hook      = identfn,
);

uses-by_name (  vednotabs, vedbreak, vedindentstep, vedcompileable,
                vednonbreakfiles, subsystem, is_ved_output_file,
                lispfiletypes, prologfiletypes, mlfiletypes,
);


;;; --- PER-FILE VARIABLES ------------------------------------------------

lvars i;

    /*  See INCLUDE * VEDFILE_STRUCT for subscript macro definitions
    */

lconstant per_file_vars = [

;;;Name                Subscript           Glob Init (false if no decl/init)
;;;                                         File Init

[ vedcurrent            VF_NAME             [nullstring]
                                            [nullstring]                ]
[ veddirectory          VF_DIRECTORY        [nullstring]
                                            [nullstring]                ]
[ vedbuffer             VF_BUFFER           [{}]
                                            [consvector(nullstring,1)]  ]
[ vedline               VF_LINE             [0]
                                            [1]                         ]
[ vedlineoffset         VF_LINEOFFSET       [0]
                                            [0]                         ]
[ vedcolumnoffset       VF_COLUMNOFFSET     [0]
                                            [0]                         ]
[ vedcolumn             VF_COLUMN           [0]
                                            [1]                         ]
[ vedstatic             VF_STATIC           [false]
                                            [false]                     ]
[ vedbreak              VF_BREAK            [true]
                                            [vedbreak]                  ]
[ vedchanged            VF_CHANGED          [false]
                                            [false]                     ]
[ vedwriteable          VF_WRITEABLE        [true]
                                            [true]                      ]
[ vedneedscompiling     VF_NEEDSCOMPILING   [false]
                                            [false]                     ]
[ vedcompileable        VF_COMPILEABLE      [false]
                                            [false]                     ]
[ subsystem             VF_SUBSYSTEM        ^false
                                            [subsystem]                 ]
[ vedpositionstack      VF_POSITIONSTACK    [[]]
                                            [[]]                        ]
[ vvedmarklo            VF_MARKLO           [1e8]
                                            [1e8]                       ]
[ vvedmarkhi            VF_MARKHI           [0]
                                            [0]                         ]
[ vvedmarkprops         VF_MARKPROPS        [true]
                                            [true]                      ]
[ marked_range_stack    VF_MARKSTACK        [writeable conspair(true,[])]
                                            [conspair(true,[])]         ]
[ vvedbuffersize        VF_BUFFERSIZE       [0]
                                            [0]                         ]
[ vvedlinesize          VF_LINESIZE         [0]
                                            [0]                         ]
[ vednamestring         VF_NAMESTRING       [nullstring]
                                            [nullstring]                ]
[ vedleftmargin         VF_LEFTMARGIN       [0]
                                            [if isprocedure(vedleftmargin) then vedleftmargin else 0 endif]]
[ vedlinemax            VF_LINEMAX          [78]
                                            [vedlinemax]                ]
[ vednotabs             VF_NOTABS           [true]
                                            [vednotabs]                 ]
[ vedindentstep         VF_INDENTSTEP       [4]
                                            [vedindentstep]             ]
[ vedwindowlength       VF_WINDOWLENGTH     [0]
                                            [vedwindowlength]           ]
[ vedscreenoffset       VF_SCREENOFFSET     [0]
                                            [if isinteger(vedscreenoffset) then vedscreenoffset else 0 endif]]
[ vedfileprops          VF_FILEPROPS        [false]
                                            [false]                     ]
[ vedprocwait           VF_PROC_WAIT        [[]]
                                            [[]]                        ]
[ vedstatusline         VF_STATUSLINE       [nullstring]
                                            [nullstring]                ]
[ wvedwindow            VF_WINDOW           [false]
                                            [false]                     ]
[ vedpathname           VF_PATHNAME         [nullstring]
                                            [nullstring]                ]
[ other_locals_assoc    VF_OTHER_LOCALS     [[]]
                                            [[]]                        ]
[ vvedpromptchar        VF_PROMPT_CHAR      [false]
                                            [false]                     ]

;;; a local of Set/Save_globals representing ved_on_status
[ on_status             VF_ON_STATUS        ^false
                                            [false]                     ]

;;; This variable is redundant when using only ved_current_file
;;; and ved_save_file_globals. It should not be used (except in those
;;; procedures to implement backward compatibility).
[ vedwasonstatus        VF_WASONSTATUS      [false]
                                            [false]                     ]

];

    /*  vars statement for per-file vars
    */
#_< "vars",
    for i in per_file_vars do
        if i(3) then i(1), "=", i(3).dl, "," endif
    endfor,
    ";"
>_#

    /*  vector of identifiers of per-file vars -- used by is_vedfile_local
    */
lconstant per_file_idents = {%
#_< for i in per_file_vars do
        "ident",
        if i(1) == "on_status" then "ved_on_status" else i(1) endif,
        ","
    endfor
>_# %};

    /*  Create a new vedfile struct.
    */
define Init_vedfile() -> file;
    lvars file = initv(VF_VFLENGTH);
#_< for i in per_file_vars do
        i(4).dl, [ -> fast_subscrv(%i(2)%, file); ].dl
    endfor
>_#
enddefine;

    /*  Save the environment in the vector for the file.
    */
define lconstant Save_globals(file, on_status);
    lvars file, on_status;
    vedtrimline();
    ;;; save window globals first
    if USEWINDOWS then wved_save_globals(file) endif;

#_< for i in per_file_vars do
        i(1), [ -> fast_subscrv(%i(2)%, file); ].dl
    endfor
>_#
enddefine;

    /*  Set the environment corresponding to a file.
    */
define lconstant Set_globals(file) -> on_status;
    lvars file, on_status;

#_< for i in per_file_vars do
        [ fast_subscrv(%i(2)%, file) -> %i(1)%; ].dl
    endfor
>_#

    ;;; Now set window globals
    if USEWINDOWS then wved_set_globals(file) endif
enddefine;

define Check_vedfile(item);
    lvars item;
    unless isvector(item) and item!V_LENGTH == _:VF_VFLENGTH then
         mishap(item, 1, 'vfn: VED FILE NEEDED')
    endunless
enddefine;

define Isvedfile(item);
    lvars item;
    unless isvector(item) then
        false
    elseif item!V_LENGTH == _:VF_VFLENGTH then
        true
    else
        ;;; mishap for a vector which isn't a file
        Check_vedfile(item)
    endunless
enddefine;

define lconstant Check_vedfileorname(item);
    lvars item;
    unless Isvedfile(item) or isstring(item) then
         mishap(item, 1, 'vffn: VED FILE OR NAME NEEDED')
    endunless;
enddefine;

    /*  Switch around the values for idents in other_locals_assoc.
        Return (val, id) pairs to be assigned after setting the new file,
        and the number of them.
    */
define lconstant Switch_other_locals(file) -> n;
    lvars   l = if vedcurrentfile then other_locals_assoc else [] endif,
            new_save = [], id, val, n = 0, m,
            next_assoc = if file then fast_subscrv(VF_OTHER_LOCALS, file)
                         else [] endif,
            file;

    until l == [] do
        fast_destpair(l) -> (id, l);
        fast_front(l) -> val;
        if list_assoc(id, next_assoc) ->> m then
            cons_assoc(id, val, new_save) -> new_save;
            fast_front(m) -> val;
            Switch_other_locals -> fast_front(m)
        endif;
        val, id, n fi_+ 1 -> n;
        idval(id) -> fast_front(l);
        fast_back(l) -> l
    enduntil;

    next_assoc -> l;
    until l == [] do
        fast_destpair(fast_destpair(l)) -> (id, val, l);
        if val /== Switch_other_locals then
            cons_assoc(id, idval(id), new_save) -> new_save;
            val, id, n fi_+ 1 -> n;
        endif
    enduntil;

    sys_grbg_list(next_assoc);
    if file then
        new_save -> fast_subscrv(VF_OTHER_LOCALS, file)
    else
        [] -> other_locals_assoc
    endif
enddefine;

define active ved_current_file;
    vedcurrentfile
enddefine;
;;;
define updaterof active ved_current_file file;
    lvars file, on_status;
    dlocal pop_asts_enabled = false;

    if file then Check_vedfile(file) endif;
    ved_on_status -> on_status;
    false -> ved_on_status;
    if vedcurrentfile == file then
        ;;; do this for consistency with the other case
        if file then vedtrimline() endif
    else
        if vedcurrentfile then
            if vedwasonstatus then
                false -> vedwasonstatus, true -> on_status
            endif;
            Save_globals(vedcurrentfile, on_status)
        endif;
        ;;; save any any other locals of the current file and set them
        ;;; back to their default values
        Switch_other_locals(file) /* -> n */;
        if file ->> vedcurrentfile then
            Set_globals(file) -> on_status
        endif;
        fast_repeat (/* n */) times Noprot_assign(/*val,id*/) endrepeat
    endif;
    if file and vedwasonstatus then
        false -> vedwasonstatus, true -> on_status
    endif;
    on_status -> ved_on_status
enddefine;

define ved_save_file_globals();
    lvars on_status;
    dlocal pop_asts_enabled = false;
    returnunless(vedcurrentfile);
    ved_on_status -> on_status;
    false -> ved_on_status;
    if vedwasonstatus then
        false -> vedwasonstatus;
        true -> on_status
    endif;
    Save_globals(vedcurrentfile, on_status);
    on_status -> ved_on_status
enddefine;


;;; MAKING VARIABLES LOCAL TO FILES

define lconstant Check_is_vedfile_local(id) -> (fval, id, file, locals);
    lvars id, file, locals, fval, i;
    if Isvedfile(id) then
        ((), id) -> (id, file)
    else
        Check_vedfile(vedcurrentfile ->> file)
    endif;
    unless isident(id) then mishap(id, 1, 'IDENTIFIER NEEDED') endunless;
    fast_subscrv(VF_OTHER_LOCALS, file) -> locals;
    unless list_assoc(id, locals) ->> fval then
        ;;; for completeness' sake, check standard idents
        fast_for i to #_< datalength(per_file_idents) >_# do
            returnif(fast_subscrv(i,per_file_idents) == id) (true -> fval)
        endfor
    endunless
enddefine;

define is_vedfile_local(/*id, file*/) with_nargs 1;
    (Check_is_vedfile_local() -> (,,)) and true
enddefine;
;;;
define updaterof is_vedfile_local(/*bool, id, file*/) with_nargs 2;
    lvars bool, cval, (fval, id, file, locals) = Check_is_vedfile_local();
    () -> bool;
    if fval then
        returnif(bool);
        if fval == true then
            mishap(id, 1, 'VARIABLE IS STANDARD VED FILE LOCAL - CAN\'T DE-LOCALISE IT')
        endif;
        ;;; delete it
        del_assoc_val(id, locals) -> (fval, locals);
        if file == vedcurrentfile then Noprot_assign(fval, id) endif
    else
        returnunless(bool);
        ;;; add it
        if isprotected(id) then
            if isconstant(id) then
                mishap(id, 1, 'CAN\'T MAKE CONSTANT IDENTIFIER LOCAL TO VED FILE')
            elseif pop_vm_flags &&=_0 VM_NOPROT_PVARS then
                mishap(id, 1, 'CAN\'T MAKE PROTECTED IDENTIFIER LOCAL TO VED FILE')
            endif
        endif;

        vedcurrentfile
        and list_assoc(id, fast_subscrv(VF_OTHER_LOCALS, vedcurrentfile))
                                -> cval;
        cons_assoc(id, if cval then front(cval) else idval(id) endif, locals)
                                                        -> locals
    endif;
    locals -> fast_subscrv(VF_OTHER_LOCALS, file);
    if file == vedcurrentfile then locals -> other_locals_assoc endif
enddefine;


;;;

define Set_changed();
    returnif(ved_on_status);
    true -> vedneedscompiling;
    if vedchanged then vedchanged fi_+ 1 else 1 endif -> vedchanged;
    if vedautowrite and vedwriteable and vedchanged fi_> vedautowrite then
        vedscreenbell();
        vedputmessage('\{b}vedautowrite ...');
        ved_w1();
        vedscreenbell();
        vedputmessage('\{b}vedautowrite done');
        vedsetcursor();
    endif;
enddefine;

/* Searching vedfiletypes */

define vedsearchfiletypes(filename, varname) -> (found, val);
    lvars   condition, conditions, action, actions, rule, rules,
            filename, varname, val = false, found = false;
    sysfileok(filename) -> filename;
    rev(vedfiletypes) -> rules;
    for rule in rules do
        dest(rule) -> (conditions, actions);
        recursive_valof(conditions) -> conditions;
        unless islist(conditions) then [^conditions] -> conditions endunless;
        until null(conditions) do
            dest(conditions) -> (condition, conditions);
            recursive_valof(condition) -> condition;
            if isstring(condition) and isendstring(condition, filename)
            or isprocedure(condition) and condition(filename)
            then
                for action in actions do
                    recursive_valof(action) -> action;
                    nextunless(
                        isvector(action)
                        and (action(1) == varname
                            ;;; backward compatibility
                             or (varname == "subsystem"
                                 and action(1) == "popcompiler") )
                        );
                    true -> found;
                    action(2) -> val;
                    if val == """ then
                        action(3) -> val
                    elseif isword(val) then
                        valof(val) -> val
                    endif;
                    if action(1) /== varname then
                        ;;; backward compatibility
                        procedure();
                            dlocal subsystem;
                            () -> valof("popcompiler");
                            subsystem
                        endprocedure(val) -> val
                    endif;
                    quitloop(3)
                endfor
            elseif islist(condition) then
                condition <> conditions -> conditions;
            endif
        enduntil
    endfor;

    sys_grbg_list(rules);
enddefine;

/* Setting VED variables from vedfiletypes */

define vedsetfiletypes(list);
    ;;; Called inside Set_variables to set environment for a new
    ;;; VED file.
    lvars actions, list, type, types;

    define lconstant Set_environ(maps);
        ;;; maps is a list of the sort associated with a file type
        ;;; in vedfiletypes
        lvars map maps val _len;
        for map in maps do
            recursive_valof(map) -> map;    ;;; will autoload if necessary
            if isprocedure(map) then
                map()
            elseif isvector(map) then
                ;;; map is {var val} or {var " val}.
                ;;; Don't dereference in second case
                datalength(map) -> _len;
                unless _len == 2 or _len == 3 then
                    mishap(map, 1, 'vln: VECTOR OF LENGTH 2 OR 3 NEEDED IN VED SPECIFICATION')
                endunless;
                fast_subscrv(2, map) -> val;
                if _len == 3 and val == """ then
                    fast_subscrv(3, map)
                elseif isword(val) then
                    valof(val)  ;;; dereference once
                else
                    val
                endif -> valof(map(1))
            else
                mishap(map, 1, 'pvn: PROCEDURE OR VECTOR NEEDED IN VEDFILETYPE')
            endif
        endfor
    enddefine;


    ;;; now 'interpret' vedfiletypes
    until null(list) do
        dest(dest(list) -> list) -> actions -> types;
        recursive_valof(types) -> types;
        if isstring(types) or isprocedure(types) then
            conspair(types, nil) -> types
        elseunless islist(types) then
            mishap(types, 1, 'wft: WRONG FILE TYPE IN VEDFILETYPES')
        endif;
        until null(types) do
            fast_destpair(types) -> types -> type;
            recursive_valof(type) -> type;
            if islist(type) then
                type <> types -> types
            else
                unless isstring(type) or isprocedure(type) then
                    mishap(type, 1, 'spn: STRING OR PROCEDURE NEEDED IN VEDFILETYPES')
                endunless;
                if (isstring(type)
                    and isendstring(type, sys_fname_name(vedpathname)))
                or (isprocedure(type) and type(vedpathname)) then
                    Set_environ(actions);
                    quitloop()
                endif
            endif
        enduntil
    enduntil
enddefine;

define lconstant Is_file_name(name);
    lvars name;
    not(strmember(`\s`, name))
#_IF DEF WIN32
    ;;; spaces in filenames are OK, so we need to be careful...
    or sys_file_stat(name, {})                      ;;; file exists
    or sys_fname_path(name) /= nullstring
        and sysisdirectory(sys_fname_path(name));   ;;; directory exists
    ;;; still won't allow creation of a new file in the current
    ;;; directory with a space in the name!
#_ENDIF
enddefine;

define lconstant Title_string(name);
    lvars name;
    if veddirectory then 'FILE: ' else 'NAME: ' endif <> name
enddefine;

    ;;; used also for ved_name
define Set_variables(user_defaults_set);
    lvars user_defaults_set, len, n, name, fprops;

    unless user_defaults_set then
        true ->> vedwriteable ->> vedneedscompiling -> vedbreak;
        if Is_file_name(vedcurrent) then
            sysfileok(vedcurrent) -> vedcurrent;    ;;; get rid of env vars
#_IF DEF WIN32
            ;;; Make vedpathname the full pathname of the file, and try
            ;;; to preserve the identity:
            ;;;     veddirectory dir_>< vedcurrent -> vedpathname
            nullstring -> veddirectory;
            if Get_full_pathname(vedcurrent) ->> vedpathname then
                if isendstring(vedcurrent, vedpathname) then
                    allbutlast(datalength(vedcurrent), vedpathname)
                        -> veddirectory;
                elseif datalength(vedcurrent) fi_> 2
                and fast_subscrs(2, vedcurrent) == `:`
                and isendstring(allbutfirst(2, vedcurrent), vedpathname)
                then
                    allbutlast(datalength(vedcurrent) fi_- 2, vedpathname)
                        -> veddirectory;
                else
                    vedpathname -> vedcurrent;
                endif;
            else
                vedcurrent -> vedpathname;
            endif;
#_ELSE
    #_IF DEF UNIX
            if subscrs(1, vedcurrent) == `/` then
    #_ELSE
            if strmember(`:`, vedcurrent) or strmember(`[`, vedcurrent) then
    #_ENDIF
                nullstring, vedcurrent
            else
                current_directory, current_directory dir_>< vedcurrent
            endif -> (veddirectory, vedpathname)
#_ENDIF
        else
            ;;; contains a space -- working buffer, not a real file
            false ->> vedwriteable -> veddirectory;
            "workbuff" -> vedfileprops;
            vedcurrent ->> name -> vedpathname;
            datalength(name) -> len;
            locchar_back(`\s`, len, name) -> n;
            allbutlast(len-n, name) <> sys_fname_namev(allbutfirst(n,name))
                                        -> vedcurrent
        endif;
        ;;; set compiler defaults
        "pop11" -> subsystem;
        false -> vedcompileable;
        vedsetfiletypes(vedfiletypes);
    endunless;

    ;;; set vednamestring
    vedcurrent -> name;
    if isword(vedfileprops) then
        ;;; help file, work buffer etc
        fast_word_string(vedfileprops) -> fprops;
        if strmember(`.`, fprops) ->> n then
            ;;; remove XVed class
            allbutfirst(n, fprops) -> fprops
        endif;
        fprops <> '\s' -> fprops;
        if veddirectory then
            fprops <> sys_fname_name(name) -> name
        elseunless isstartstring(fprops, name) then
            fprops <> name -> name
        endif
    endif;
    if vedwriteable then '\{b}editing: ' else '\{b}examining: ' endif <> name
                                    -> vednamestring
enddefine;


    /*  Apply p in environments corresponding to all the files in
        vedbufferlist (with each off status)
    */
define vedappfiles(p);
    lvars   procedure p, list = copylist(vedbufferlist), save;

    dlocal 0 % (vedcurrentfile -> save),
                (unless save then
                    false
                 elseif vedpresent(save) then
                    save
                 elseif vedbufferlist /== [] then
                    hd(vedbufferlist)
                 else
                    false
                 endunless -> ved_current_file)
             %;

    for ved_current_file in list do
        ;;; note that ved_on_status must be dlocal'ed AFTER the file is set
        procedure;
            dlocal ved_on_status = false;
            p()
        endprocedure()
    endfor;
    sys_grbg_list(list)
enddefine;


    /*  Given a file or its name, return a suitable name.
        If vedsearchlist exists, get the path name that would
        be found. If not use original. The result could be a list.
        But before checking vedsearchlist, etc. look in vedbufferlist
        If given extra boolean argument return the file if present in ved
    */
define Searchlist_name(name) -> name;
    lvars name, file, filewanted = false;

    define lconstant Invedbufferlist(file) -> file;
        ;;; file can be a string or the file record. return the file or false
        lvars vfname, file, totlen, dir, tempfile;

        Check_vedfileorname(file);
        if isvector(file) then
            unless fast_lmember(file,vedbufferlist) then
                false -> file
            endunless
        elseif isstring(file) then
            if Is_file_name(file) then
#_IF DEF WIN32
                ;;; Filename comparisons must be case-insensitive; we
                ;;; choose conv_p to try and minimise the number of
                ;;; case-conversions actually done
                lvars procedure conv_p;
                Get_full_pathname(sysfileok(file)) -> file;
                if pop_filename_case == "upper" then
                    lowertoupper -> conv_p;
                else
                    uppertolower -> conv_p;
                    unless pop_filename_case == "lower" then
                        conv_p(file) -> file;
                    endunless;
                endif;
                fast_for tempfile in vedbufferlist do
                    returnif(conv_p(fast_subscrv(VF_PATHNAME,tempfile)) = file)
                        (tempfile -> file)
                endfor
#_ELSE
                sysfileok(file) -> file;
                current_directory -> dir;
                datalength(dir) fi_+ 1 fi_+ datalength(file) -> totlen;
                fast_for tempfile in vedbufferlist do
                    fast_subscrv(VF_PATHNAME,tempfile) -> vfname;
                    returnif(vfname = file
                             or (datalength(vfname) == totlen
                                 and isstartstring(dir, vfname)
                                 and isendstring(file, vfname)))
                        (tempfile -> file)
                endfor
#_ENDIF
            else
                fast_for tempfile in vedbufferlist do
                    returnif(fast_subscrv(VF_PATHNAME,tempfile) = file)
                        (tempfile -> file)
                endfor
            endif;
            false -> file;  ;;; not found
        endif;
    enddefine;      /* Invedbufferlist */

    if isboolean(name) then ((), name) -> (name, filewanted) endif;
    Check_vedfileorname(name);          ;;; must be vector or string
    if isvector(name) then
        if filewanted then Invedbufferlist(name)
        else name(VF_PATHNAME)
        endif -> name
    elseif ispair(vedsearchlist)
    and Is_file_name(name)
    and (sysfileok(name)->>name) /= nullstring
#_IF DEF UNIX
    and fast_subscrs(1,name) /== `/`    ;;; no full path name
    and not(isstartstring('./', name))  ;;; could be a bug?
    and not(isstartstring('../',name))  ;;; could be a bug?
#_ELSEIF DEF VMS
    and not(strmember(`[`,name)) and not(strmember(`:`,name))
#_ENDIF
    then
        if Invedbufferlist(name) ->> file then
            if filewanted then file else file(VF_PATHNAME) endif -> name;
        elseif filewanted then
            false -> name
        elseunless sys_file_stat(name, #_<{}>_#) then
            syssearchpath(vedsearchlist, name) -> file;
            if file then file -> name endif;
        ;;; else file in current directory - so just use the name
        endif
    elseif filewanted then              ;;; called by vedpresent
        Invedbufferlist(name) -> name
    endif;
enddefine;      /* Searchlist_name */


define vedpresent(file) ->file;
    ;;; enquire about existence of file in vedbufferlist
    ;;; now copes with vedsearchlist, and uses veddirectory if necessary
    lvars file tempfile;
        Searchlist_name(file,true) -> file;   ;;; can return a list
        if ispair(file) then front(file) -> file endif;
enddefine;

    ;;; enquire about or change name of current file
define vedbuffername(file);
    ;;; if given extra true argument return full path name. AS 15-5-86
    lvars file full;
    if file == true then file -> full; -> file else false -> full endif;
    Check_vedfile(file);
    file(if full then VF_PATHNAME else VF_NAME endif);
enddefine;


    ;;; if there's no argument, print out the name of the file
    ;;; otherwise make the argument the new name unless  name already exists
define vars ved_name();
    lvars arg = vedargument, file, work = false, args, n;

    [% sys_parse_string(arg) %] -> args;
    if ((listlength(args) ->> n)==1 or n==2) and hd(args) = '-w' then
        true -> work;
        if n == 1 then
            vedpathname -> arg;
            locchar_back(`\s`, datalength(arg), arg) -> n;
            if n then allbutfirst(n,arg) else arg endif
        else
            hd(tl(args))
        endif -> arg;
        'workbuff\s' <> arg
    elseif n fi_> 1 then
        vederror('\{b}invalid filename')
    else
        sysfileok(arg)
    endif -> arg;

    unless arg = nullstring then
        vedpresent(arg) -> file;
        if file and file /== ved_current_file then
            vederror('\{b}can\'t rename: ' <> file(VF_NAME) <> ' \{b}in Ved already');
        endif;
        unless work or sysisdirectory(sys_fname_path(arg)) then
            vederror('\{b}directory protected or nonexistent: ' <> arg)
        endunless;
        ;;; make sure this has its standard value
        false -> is_vedfile_local(ident ved_write_current_file);
        arg ->> vedcurrent -> vedvedname;
        false -> vedfileprops;
        Set_variables(false);
        Set_changed();
        vedveddefaults();
        Set_variables(true);
        vedinitialise("ved_name");
        if USEWINDOWS then wved_ved_name_hook() endif;
        vedinitfile();
        ved_save_file_globals();
    endunless;
    if vedediting then vedputmessage('\{b}name: ' <> vedpathname) endif
enddefine;

define ved_mafw();
    ;;; make all files writeable. Not to be publicised.
    not(vedwriteallfiles) -> vedwriteallfiles;
    vedputmessage(if vedwriteallfiles then '\{b}on' else '\{b}off' endif);
enddefine;

    /*  Apply a procedure p to make changes to a file file without forcing
        the file to be displayed if it's not on-screen, and without
        changing vedinputfocus.

        Also leaves the ordering of files in vedbufferlist unchanged.
    */
define ved_file_change_apply(p, file);
    lvars   file, curr, procedure p;
    dlocal  vedinputfocus, vedediting, ved_current_file,
            vedwarpcontext = false, wvedalwaysraise = false;

    define lconstant local_setonscreen(p);
        lvars procedure p;

        define lconstant Onscreen_barrier(context);
            lvars l, context;
            vedfileisonscreen(curr),
            (fast_lmember(file, vedbufferlist) ->> l)
                and (fast_back(l) ->> l) /== [] and fast_front(l)
        enddefine;
        ;;;
        define updaterof Onscreen_barrier(wason, nxtfile, context);
            lvars l, m, nf, wason, nxtfile, context;

            if wason then
                vedsetonscreen(curr, context == 2 and vedmessage)
            endif;

            ;;; shuffle file down to before nxtfile or to the end
            fast_lmember(file, vedbufferlist) -> l;
            until (fast_back(l) ->> m) == []
            or (fast_front(m) ->> nf) == nxtfile do
                nf -> fast_front(l);
                m -> l
            enduntil;
            file -> fast_front(l)
        enddefine;

        dlocal 2 % Onscreen_barrier(dlocal_context) %;

        vedsetonscreen(file, false);
        p();
        vedcheck()
    enddefine;

    Check_procedure(p);
    Check_vedfile(file);

    if vedediting and vedfileisonscreen(file) then
        if (ved_current_file ->> curr) == file then
            p()
        else
            local_setonscreen(p)
        endif
    else
        false -> vedediting;
        file -> ved_current_file;
        p()
    endif
enddefine;


/* Procedure for recognising "output" files, used in default vedfiletypes */

define is_ved_output_file(file);
    lvars file, pif = vedlmr_print_in_file;
    returnunless(isstring(pif)) (false);
    file = pif
        or
    (sys_fname_path(pif) = nullstring and sys_fname_name(file) = pif)
enddefine;


;;; --------------------------------------------------------------------
;;; These are old procedures which remain here only for upward compatibility.
;;; THEY SHOULD NOT BE USED -- use ved_save_file_globals to the save the
;;; globals of ved_current_file, and assign to ved_current_file to set
;;; the globals.  JG June 91

define vedsaveglobals(file);
    lvars file;
    returnunless(file);
    dlocal pop_asts_enabled = false;
    Check_vedfile(file);
    if ved_on_status then
        true -> vedwasonstatus;
        Set_vedcommand(false)
    endif;
    Save_globals(file, false)
enddefine;

define vedsetglobals(file);
    lvars file, on_status;
    returnunless(file);
    dlocal pop_asts_enabled = false;
    Check_vedfile(file);
    false -> ved_on_status;
    Switch_other_locals(file) /* -> n */;
    Set_globals(file) -> on_status;
    fast_repeat (/* n */) times Noprot_assign(/*val,id*/) endrepeat;
    if on_status then true -> vedwasonstatus endif
enddefine;

endsection;     /* $-Sys$-Ved */



/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Feb  6 2008
        In Searchlist*_name(name) -> name;
    
        altered
                datalength(dir) fi_+ datalength(file) -> totlen;
        to
                datalength(dir) fi_+ 1 fi_+ datalength(file) -> totlen;
--- Robert Duncan, Sep 30 1996
        Fixed Searchlist_name for Win32 to do case-insensitive matching
--- Robert Duncan, Apr 24 1996
        Fixed Is_file_name for Win32 to allow filenames containing spaces
        where it makes sense to do so
--- John Gibson, Apr 18 1996
        Made Set_variables remove any leading Class. from the vedfileprops
        added to vednamestring.
--- John Gibson, Apr 13 1996
        Changed ved_name to use sysisdirectory to check dir valid.
--- John Gibson, Mar 12 1996
        Made vvedpromptchar a per-file local (VF_PROMPT_CHAR).
        Added file initialisation expression to per_file_vars, used by
        Init_vedfile.
--- John Gibson, Jan 22 1996
        VF_MARKSTACK now initialised to a pair
--- John Gibson, Nov 28 1995
        # Changed is_vedfile_local so that file arg can be omitted to default
          to ved_current_file.
        # Made ved_name ensure that ved_write_current_file is delocalised.
--- John Gibson, Dec 13 1994
        Fixed stupid bug introduced by Aug 22 change to vedappfiles (dlocal
        expression was assigning to vedcurrentfile not ved_current_file!!!).
--- Robert John Duncan, Nov 11 1994
        Added mlfiletypes
--- Robert John Duncan, Sep  5 1994
        Added cases for Win32
--- John Gibson, Aug 22 1994
        Changed vedappfiles to set ved_current_file false at the end if
        executing the procedure caued it to be quit
--- John Gibson, May  3 1994
        Made ved_file_change_apply leave the ordering of files in
        vedbufferlist unchanged.
--- John Gibson, Apr 22 1994
        sys_si*gnals_enabled -> pop_asts_enabled
--- John Gibson, Mar  7 1994
        Got rid of ved*select
--- Adrian Howard, Feb 23 1994
        Changed >< to sys_>< in calls to vederror.
--- John Gibson, Jan 22 1994
        Improved Searchlist_name
--- John Gibson, Jan 20 1994
        Exported File_change_apply as ved_file_change_apply.
--- John Gibson, Jan 16 1994
        Changed is_vedfile_local to check for constant/protected ids, but
        allowing protected ones when VM_NOPROT_PVARS set; places where ids
        are updated now use Noprot_assign.
--- Robert John Duncan, Aug  2 1993
        Added prologfiletypes
--- John Gibson, Jun 12 1993
        Moved is_ved_output_file to here from vddoit.p
--- John Williams, May 10 1993
        Added [is_ved_output_file {vedcompileable ^false}] clause to
        vedfiletypes
--- John Gibson, Apr 21 1993
        Added support for per-file local variables and is_vedfile_local
        to create them. Redid per-file locals with macros.
--- John Gibson, Jan 12 1993
        popcom*piler -> subsystem etc
--- John Gibson, Jan 12 1993
        Moved in vedsearchfiletypes from library
--- John Gibson, Dec 21 1992
        Moved all per-file variable initialisations into this file.
--- Adrian Howard, Sep  8 1992
        ved_name does not print a message if vedwriteable is false
--- John Gibson, Nov 16 1991
        Disabled interrupts inside ved_current_file and ved_save_file_globals
--- John Gibson, Jun 21 1991
        Added -ved_save_file_globals-
--- John Gibson, Jun 15 1991
        Fix to -File_change_apply- to make sure file-on-screen is
        dlocal'ed properly
--- John Gibson, Jun 13 1991
        Renamed C*urrent_file active var as exported ved_current_file
--- John Gibson, Jun  8 1991
        wved_ rationalisation
--- John Gibson, Jan  8 1991
        Improvements to -File_change_apply-.
--- Simon Nichols, Nov  8 1990
        Changed mishap codes to lower case.
--- John Gibson, Nov  2 1990
        Whole file into Sys$-Ved, etc.
--- John Gibson, Oct 31 1990
        Moved -Set_changed- in from vdprocess.p.
        Added -File_change_apply-
--- Aaron Sloman, Oct 12 1990
        added wved_set_globals and wved_save_globals, for setting and
        saving vedwindow globals
--- Aaron Sloman, Oct 12 1990
        changed xved to wved
--- Aaron Sloman, Sep 22 1990
        changed ved_name to call xvedinitialise. Added extra argument
        for vedinitialise. Moved some declarations to .ph files.
--- John Williams, Jan 24 1990
        -vedfiletypes- now includes default setting of -vedindentstep-
--- Aaron Sloman, Dec 28 1989
    Added call of new procedure -vedinitialise- near end of ved_name
--- Aaron Sloman, Dec 27 1989
        Moved definition of -vednonbreakfiles- here from vdsetup.p
        Moved default initialisation of -popcom*piler- into Set_variables, outside
        vedsetfiletypes, so that users don't need to override it whenever they
        used vedsetfiletypes.
        Added default condition to vedfiletypes: vedbreak true, vednotabs true.
--- Rob Duncan, Dec  1 1989
        Added '.ml' and '.sig' to -vedfiletypes-
--- John Williams, Oct  9 1989
        Fixed bug in -vedsetfiletypes- caused by use of -sys_fname_name-
--- John Gibson, Aug  3 1989
        Changed to use -sys_fname_name-, -sys_fname_namev- and
        -sys_fname_extn- to get filename/extension.
--- Aaron Sloman, May 16 1989
        Changed Set_variables to call sysfileok on vedcurrent
--- Aaron Sloman, Apr  5 1989
        Added call of vedtrimline to vedsaveglobals
--- John Williams, Dec  5 1988
        Replaced -Isendstring- with -isendstring-
--- John Williams, Oct 13 1988
        Fixed -vedappfiles- to use dlocal expression to save/restore
        current file environment
--- Aaron Sloman, Apr  9 1988
        Changed to use -vedpathname- where appropriate, instead of
        combining -veddirectory- and -vedcurrent-
--- John Williams, Mar  1 1988
        Uses -lispfiletypes- instead of '.lsp' in -vedfiletypes-
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
 */
