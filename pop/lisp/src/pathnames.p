/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lisp/src/pathnames.p
 > Purpose:         Common Lisp "pathnames"
 > Author:          John Williams, May 22 1987 (see revisions)
 > Documentation:   CLtL, chapter 23
 > Related Files:
 */

lisp_compile_mode;

section $-lisp => lispfileparse;

fastprocs for, repeat, front, back, destpair, subscrs, chain;

constant VMS = hd(sys_os_type) == "vms";

defprop device_logical_pathname;

constant procedure lp_translations = newmapping([], 16, false, true);

constant procedure (file_error,
                    lispfileparse, parse_logical_pathname,
                    make_logical_pathname, merge_pathnames);


define lconstant Checkr_lp_host() with_nargs 1;
 #_IF VMS
    lowertoupper(get_simple_string())
 #_ELSE
    uppertolower(get_simple_string())
 #_ENDIF
enddefine;


define islogical_host(host);
    Checkr_lp_host(host) -> host;
    lp_translations(host) and host
enddefine;


/* PATHNAME and *DEFAULT-PATHNAME-DEFAULTS* */

define lconstant Checkr_pathname_or_string(item);
    lvars dev, lp;
    if ispathname(item) then
        item
    elseif stringp(item) then
        get_simple_string(item)
    elseif isstream(item) then
        if (device_logical_pathname(streamdev(item) ->> dev) ->> lp) then
            lp
        else
            device_open_name(dev)
        endif
    else
        simple_type_error('Cannot convert ~S to a pathname', item,
                          #_<[^@OR ^@STRING ^@FILE-STREAM ^@SYNONYM-STREAM]>_#)
    endif
enddefine;


define pathname(item) -> item;
    if isstring(Checkr_pathname_or_string(item) ->> item) then
        conspathname(lispfileparse(item)) -> item
    endif
enddefine;


lvars Default_path = conspathname(nil, nil, nil, 'temp', 'lsp', @:NEWEST);

define active default_pathname;
    Default_path
enddefine;

define updaterof active default_pathname() with_nargs 1;
    lvars type;
    pathname() -> Default_path;
    if isstring(pt_type(Default_path) ->> type) then
        '.' <> type -> pop_default_type
    endif
enddefine;

propsheet_idents default_pathname;


/* Special pathname components */

lconstant
    Special_dirs
        = [% conspair('*', @:WILD),
             conspair(if VMS then '-' else '..' endif, @:UP),
             conspair(if VMS then '-' else '..' endif, @:BACK),
             conspair(if VMS then '' else '.' endif,   @:CURRENT),
             conspair(if VMS then '-' else '..' endif, @:PARENT),
             ;;; latter for upward compatability
          %],
    Special_lp_dirs
        = [% car(Special_dirs) %],
    Special_names
        = [% car(Special_dirs) %],
    Special_types
        = [% car(Special_dirs) %],
    Special_versions
        = [% car(Special_dirs),
             conspair(0,     @:NEWEST),
             conspair(-1,    @:PREVIOUS)
          %],
    ;


define lconstant Try_keyword_to_string(item, alist);
    lvars pair;
    if (eql_rassoc(item, alist) ->> pair) then
        front(pair)
    else
        item
    endif
enddefine;


define lconstant Try_string_to_keyword(item, alist);
    lvars pair;
    if (@ASSOC(item, alist, @:TEST, @EQUAL, 4) ->> pair) /== nil then
        back(pair)
    else
        item
    endif
enddefine;


/* Case conversion */

define lconstant Common_to_local(s);
    lvars c;
    returnunless (isstring(s)) (s);
 #_IF VMS
    lowertoupper(s);
 #_ELSE
    if (all_same_case(s) ->> c) then
        if isuppercode(c) then uppertolower(s) else lowertoupper(s) endif
    else
        s
    endif;
 #_ENDIF
enddefine;


define lconstant Local_to_common(s);
    lvars c;
    returnunless (isstring(s)) (s);
 #_IF VMS
    s
 #_ELSE
    if (all_same_case(s) ->> c) then
        if islowercode(c) then lowertoupper(s) else uppertolower(s) endif
    else
        s
    endif
 #_ENDIF
enddefine;


/* Checking components of pathnames (for MAKE-PATHNAME) */

define lconstant Path_field_err(item, dir);
    chain(
        'Illegal pathname component',
        item,
        [^@OR ^@STRING ^@SYMBOL ^(if dir then @CONS endif)],
        simple_type_error)
enddefine;


define lconstant Path_case_err(case);
    chain(
        'Illegal pathname case: ~S',
        case,
        #_<[^@MEMBER ^@:LOCAL ^@:COMMON]>_#,
        simple_type_error)
enddefine;


define lconstant Checkr_path_field(item, case, specials);
    if issymbol(item) then
        ;;; Note: NIL and @:UNSPECIFIC have already been checked for.
        if Try_keyword_to_string(item, specials) == item then
            /* I.e. not a special symbol */
            symbol_name(item) -> item
        else
            return(item)
        endif
    elseif stringp(item) then
        get_simple_string(item) -> item
    else
        Path_field_err(item, false)
    endif;
    if case == @:COMMON then
        Common_to_local(item) -> item
    elseunless case == @:LOCAL then
        Path_case_err(case)
    endif;
    Try_string_to_keyword(item, specials)
enddefine;


define lconstant Checkr_host(item, case);
    returnif (item == nil or item == @:UNSPECIFIC) (item);
    if item == @:WILD then
        Path_field_err(item, false)
    endif;
    Checkr_path_field(item, case, [])
enddefine;


define lconstant Checkr_device(item, case);
    returnif (item == nil or item == @:UNSPECIFIC) (item);
    if item == @:WILD then
        Path_field_err(item, false)
    endif;
 #_IF VMS
    Checkr_path_field(item, case, [])
 #_ELSE
    warn_once('Unix pathnames do not device fields', [^item]);
    nil
 #_ENDIF
enddefine;


define lconstant Checkr_dir_list(item, procedure check_p);
    lvars list;
    returnif (item == nil or item == @:UNSPECIFIC) (item);
    if ispair(item) then
        destpair(item) -> (item, list);
        unless item == @:ABSOLUTE or item == @:RELATIVE do
            simple_type_error('Illegal first component of directory list: ~S',
                              item, #_<[^@MEMBER ^@:ABSOLUTE ^@:RELATIVE]>_#)
        endunless;
        [% item, for item in_cl_list list do check_p(item) endfor %]
    elseif stringp(item) then
        [^@:ABSOLUTE ^(check_p(item))]
    elseif item == @:WILD then
        [^@:ABSOLUTE ^@:WILD]
    else
        Path_field_err(item, true)
    endif
enddefine;


define lconstant Checkr_dir(item, case);
    dlvars case;
    Checkr_dir_list(item, procedure();
                            Checkr_path_field(case, Special_dirs)
                          endprocedure)
enddefine;


define lconstant Checkr_name(item, case);
    returnif (item == nil or item == @:UNSPECIFIC) (item);
    Checkr_path_field(item, case, Special_names)
enddefine;


define lconstant Checkr_type(item, case);
    returnif (item == nil or item == @:UNSPECIFIC or item == @:NOTYPE) (item);
    Checkr_path_field(item, case, Special_types)
enddefine;


define lconstant Checkr_version(item) -> item;
    returnif (item == nil or item == @:UNSPECIFIC);
    Try_string_to_keyword(item, Special_versions) -> item;
    unless isinteger(item)
    or Try_keyword_to_string(item, Special_versions) /== item do
        simple_type_error('Illegal pathname version', item,
                          #_<[^@OR ^@INTEGER ^@SYMBOL]>_#)
    endunless
enddefine;


define lconstant Checkr_lp_field(item);
    get_simple_string(item) -> item;
    if item = '*' then
        @:WILD
    else
     #_IF VMS
        lowertoupper(item)
     #_ELSE
        uppertolower(item)
     #_ENDIF
    endif
enddefine;


define lconstant Checkr_lp_version(item);
    lvars num;
    if isinteger(item) then
        item
    else
        returnif (item == nil or item == @:WILD) (item);
        Checkr_lp_field(item) -> item;
        returnif (item == @:WILD) (item);
        strnumber(item)
    endif -> num;
    if isinteger(num) and num > 0 then
        num
    elseif num == 0 or item = 'NEWEST' or item = 'newest' then
        @:NEWEST
    else
        mishap(item, 1, 'Illegal logical pathname version')
    endif
enddefine;


define make_pathname(host, device, dir, name, type, version,
                     default_path, case);
    lvars h;

    ;;; defaults
    if pop_true(default_path) then
        pathname(default_path)
    else
        conspathname(pt_host(default_pathname), nil, nil, nil, nil, nil)
    endif -> default_path;

    if (islogical_host(host) ->> h) then
        unless device == nil or device == @:UNSPECIFIC do
            lisp_error('Cannot specify device for logical pathname', [^device])
        endunless;
        h;
        @:LOGICAL;
        Checkr_dir_list(dir, Checkr_lp_field);
        Checkr_lp_field(name);
        Checkr_lp_field(type);
        Checkr_lp_version(version)
    else
        Checkr_host(host, case);
        Checkr_device(device, case);
        Checkr_dir(dir, case);
        Checkr_name(name, case);
        Checkr_type(type, case);
        Checkr_version(version)
    endif;

    merge_pathnames(conspathname(), default_path, nil)
enddefine;


define pathname_field(path, case, n) -> item;
    fast_record_access(n, path) -> item;
    if n == 2 and item == @:LOGICAL then
        @:UNSPECIFIC -> item
    endif;
    if case == @:COMMON then
        if ispair(item) then
            maplist(item, Local_to_common) -> item
        else
            Local_to_common(item) -> item
        endif
    elseunless case == @:LOCAL do
        Path_case_err(case)
    endif
enddefine;


/* File name parsing */

define lconstant Substr(start, len, string);
    if len == 0 then
        nil
    else
        substring(start, len, string)
    endif
enddefine;


define lconstant Thischar(i, string);
    if fast_vector_length(string) < i then
        nil
    else
        subscrs(i, string)
    endif
enddefine;


#_IF VMS

define lconstant Parse_host(string, a) -> a;
    lvars i;
    if issubstring('::', a, string) ->> i then
        Substr(a, i - a, string);
        i + 2 -> a
    else
        nil
    endif
enddefine;


define lconstant Parse_device(string, a) -> a;
    lvars i;
    if locchar(`:`, a, string) ->> i then
        Substr(a, i - a, string);
        i + 1 -> a
    else
        nil
    endif
enddefine;


define lconstant Parse_dir(string, a) -> a;
    lvars z, i;
    [% if Thischar(a, string) == `[` then
        a + 1 -> a;
        if Thischar(a, string) == `.` then
            @:RELATIVE;
            a + 1 -> a;
        else
            @:ABSOLUTE
        endif;
LOOP:   unless (locchar(`]`, a, string) ->> z) do
            mishap(string, 1, 'Missing ] in directory spec')
        endunless;
        while (locchar(`.`, a, string) ->> i) and i < z do
            substring(a, i - a, string);
            Try_string_to_keyword(Special_dirs);
            i + 1 -> a
        endwhile;
        if a == z and Thischar(z + 1, string) == `[` then
            z + 2 -> a;
            goto LOOP
        endif;
        substring(a, z - a, string);
        Try_string_to_keyword(Special_dirs);
        z + 1 -> a
    endif %]
enddefine;


define lconstant Parse_filename(string, a) -> version -> type -> name;
    lvars z, i, name = nil, type = nil, version = nil;
    fast_vector_length(string) -> z;
    if a > z then
        return
    endif;
    if (locchar(`.`, a, string) ->> i) then
        Substr(a, i - a, string) -> name;
        i -> a;
        if ((locchar(`.`, a + 1, string)
        or locchar(`;`, a + 1, string)) ->> i) then
            substring(a + 1, i - a - 1, string);
            substring(i + 1, z - i, string)
        else
            substring(a + 1, z - a, string);
            '0'
        endif -> version -> type
    elseif (locchar(`;`, a, string) ->> i) then
        Substr(a, i - a, string) -> name;
        substring(i + 1, z - i, string) -> version
    else
        Substr(a, z - a + 1, string) -> name
    endif;
    unless name == nil do
        Try_string_to_keyword(name, Special_names) -> name
    endunless;
    unless type == nil do
        Try_string_to_keyword(type, Special_types) -> type
    endunless;
    unless version == nil do
        if sys_=(version, nullstring) then
            0 -> i
        elseunless (strnumber(version) ->> i) do
            mishap(version, 1, 'Malformed version in pathname string')
        endif;
        Try_string_to_keyword(i, Special_versions) -> version
    endunless
enddefine;

#_ELSE

define lconstant Parse_host(string, a) -> a;
    lvars i;
    if (locchar(`:`, a, string) ->> i) then
        Substr(a, i - a, string);
        i + 1 -> a
    else
        nil
    endif
enddefine;


define lconstant Parse_device(string, a) -> a;
    @:UNSPECIFIC
enddefine;


define lconstant Parse_dir(string, a) -> a;
    lvars i;
    [% if (locchar(`/`, a, string) ->> i) then
        if i == a then
            a + 1 -> a;
            @:ABSOLUTE
        else
            @:RELATIVE
        endif;
        while (locchar(`/`, a, string) ->> i) do
            substring(a, i - a, string);
            Try_string_to_keyword(Special_dirs);
            i + 1 -> a
        endwhile
    endif %]
enddefine;


define lconstant Parse_filename(string, a);
    lvars z, i, version;
    fast_vector_length(string) -> z;
    if a > z then
        return(nil, nil, nil)
    endif;
    if (skipchar_back(`-`, z, string) ->> i) and i /== z then
        i - z;
        i -> z
    else
        @:UNSPECIFIC
    endif -> version;
    if (locchar_back(`.`, z, string) ->> i) and i >= a then
        Substr(a, i - a, string);
        Try_string_to_keyword(Special_names);
        substring(i + 1, z - i, string);
        Try_string_to_keyword(Special_types);
    else
        Substr(a, z - a + 1, string);
        Try_string_to_keyword(Special_names);
        nil
    endif;
    Try_string_to_keyword(version, Special_versions)
enddefine;

#_ENDIF


define lispfileparse(string);
    lvars a;
    Parse_host(string, 1) -> a;
    Parse_device(string, a) -> a;
    Parse_dir(string, a) -> a;
    Parse_filename(string, a)
enddefine;


define parse_namestring(item, host, default, start, finish, junk_ok);
    lvars lp;

    ;;; defaults
    if host then
        if (islogical_host(host) ->> lp) then
            lp
        else
            Checkr_host(host, @:LOCAL)
        endif
    else
        pathname(default) -> default;
        pt_device(default) == @:LOGICAL -> lp;
        pt_host(default)
    endif -> host;

    if ispathname(Checkr_pathname_or_string(item) ->> item) then
        start -> finish
    else
        if finish == nil then
            fast_vector_length(item) -> finish
        endif;
        if start /== 0 or finish /== fast_vector_length(item) then
            substring(start + 1, finish - start, item) -> item
        endif;
        string_trim('\s\t', item) -> item;
        conspathname(
            if lp then
                parse_logical_pathname(item, host)
            else
                lispfileparse(item)
            endif) -> item
    endif;

    if host /== nil
    and pt_host(item) /== nil
    and not(sys_=(host, pt_host(item))) then
        mishap(host, item, 2, 'Non-matching hosts')
    endif;

    item, finish
enddefine;


/* MERGE-PATHNAMES */

define lconstant Append_dirs(d1, d2) -> result;
    lvars item, prev = false;
    returnif (d1 == @:UNSPECIFIC) (d1);
    [% for item in d1 do
        if item == @:BACK and (isstring(prev) or prev == @:WILD) then
            ->; ->> prev
        else
            item ->> prev
        endif
    endfor;
    for item in d2 do
        if item == @:BACK and (isstring(prev) or prev == @:WILD) then
            ->; ->> prev
        else
            item ->> prev
        endif
    endfor %] -> result;
    if result starts_with @:ABSOLUTE then
        cadr(result) -> item;
        if item == @:BACK or item == @:UP then
            file_error(
                'Illegal item after :ABSOLUTE in pathname directory list',
                [^result],
                nil)    ;;; N.B. should be the faulty pathname!
        endif
    endif
enddefine;


define lconstant Merge_dirs(d1, d2);
    if d1 starts_with @:RELATIVE and ispair(d2) then
        Append_dirs(d2, back(d1))
    elseif d1 /== [] then
        d1
    else
        d2
    endif
enddefine;


define merge_pathnames(path, default, default_version) -> path;
    lvars host, device, dir, name, type, version;

    ;;; defaults
    if pop_true(default) then
        pathname(default)
    else
        default_pathname
    endif -> default;

    if stringp(path) and (pt_device(default) == @:LOGICAL) then
        make_logical_pathname(path, pt_host(default))
    else
        pathname(path)
    endif -> path;

    pt_host(path) -> host;
    pt_device(path) -> device;

    if host /== nil and device /== nil then
        ;;; Note: this covers logical pathnames.
        host, device
    elseif host /== nil then
        host;
        if sys_=(host, pt_host(default)) then
            pt_device(default)
        else
            nil
        endif
    elseif device /== nil then
        if sys_=(device, pt_device(default)) then
            pt_host(default)
        else
            nil
        endif;
        device
    else
        pt_host(default), pt_device(default)
    endif;

    Merge_dirs(pt_dir(path), pt_dir(default));

    if (pt_name(path) ->> name) /== nil then
        name
    else
        pt_name(default)
    endif;

    if (pt_type(path) ->> type) /== nil then
        type
    else
        pt_type(default)
    endif;

    if (pt_version(path) ->> version) /== nil then
        version
    elseif name /== nil then
        ;;; use default_version
        if default_version then
            if device == @:LOGICAL then
                Checkr_lp_version(default_version)
            else
                Checkr_version(default_version)
            endif
        else
            @:NEWEST
        endif
    else
        pt_version(default)
    endif;

    conspathname() -> path;
    if pt_device(path) == @:LOGICAL then
        /* Logical pathnames must never contain :unspecific */
        lblock;
            lvars item, n;
            for item with_index n in_record path do
                if item == @:UNSPECIFIC then
                    nil -> fast_record_access(n, path)
                endif
            endfor
        endlblock
    endif
enddefine;


define user_homedir_pathname(host) -> path;
    pathname(sysfileok('$poplib/')) -> path;
    if pop_true(host) then
        Checkr_host(host, @:LOCAL) -> pt_host(path)
    endif
enddefine;


/* Pathnames -> Strings */

define lconstant Dest_path(item);
    if ispathname(Checkr_pathname_or_string(item) ->> item) then
        destpathname(item)
    else
        lispfileparse(item)
    endif
enddefine;


define lconstant Explode_field(item, specials);
    if issymbol(item) then
        Try_keyword_to_string(item, specials) -> item
    endif;
    deststring(item) ->
enddefine;


define lconstant Explode_host(item, lp);
    returnif(item == nil or item == @:UNSPECIFIC);
    Explode_field(item, []), `:`;
 #_IF VMS
    unless lp then `:` endunless
 #_ENDIF
enddefine;


define lconstant Explode_device(item, lp);
    returnif(lp or item == nil or item == @:UNSPECIFIC);
 #_IF VMS
    Explode_field(item, []), `:`
 #_ENDIF
enddefine;


define lconstant Explode_dir(list, lp);
    lvars item;
    returnif(list == nil or list == @:UNSPECIFIC);
    destpair(list) -> (item, list);
    if lp then
        if item == @:RELATIVE then `;` endif;
        for item in list do
            Explode_field(item, Special_lp_dirs), `;`
        endfor
    else
        Append_dirs(list, []) -> list;      /* Deal with :BACK */
     #_IF VMS
        `[`;
        if item == @:RELATIVE then `.` endif;
        while ispair(list) do
            Explode_field(sys_grbg_destpair(list) -> list, Special_dirs);
            `.`
        endwhile;
        if dup() == `.` then ->; endif;
        `]`
     #_ELSE
        if item == @:ABSOLUTE then `/` endif;
        while ispair(list) do
            Explode_field(sys_grbg_destpair(list) -> list, Special_dirs);
            `/`
        endwhile
     #_ENDIF
    endif
enddefine;


define lconstant Explode_name(item);
    returnif(item == nil or item == @:UNSPECIFIC);
    Explode_field(item, Special_names)
enddefine;


define lconstant Explode_type(item);
    returnif(item == nil or item == @:UNSPECIFIC or item == @:NOTYPE);
    `.`, Explode_field(item, Special_types)
enddefine;


define lconstant Explode_version(item, lp);
    returnif (item == nil or item == @:UNSPECIFIC);
    Try_keyword_to_string(item, Special_versions) -> item;
    if lp then
        `.`;
        if item == 0 then
         #_IF VMS
            explode('NEWEST')
         #_ELSE
            explode('newest')
         #_ENDIF
        else
            dest_characters(item)
        endif
    else
     #_IF VMS
        if item /== 0 then
            `;`, dest_characters(item)
        endif
     #_ELSE
        repeat negate(item) times `-` endrepeat
     #_ENDIF
    endif
enddefine;


define host_namestring() with_nargs 1;
    lvars device, host;
    Dest_path() -> -> -> -> -> device -> host;
    consstring(#| Explode_host(host, device == @:LOGICAL) |#)
enddefine;


define directory_namestring() with_nargs 1;
    lvars dir, device;
    Dest_path() -> -> -> -> dir -> device ->;
    consstring(#| Explode_dir(dir, device == @:LOGICAL) |#)
enddefine;


define file_namestring() with_nargs 1;
    lvars version, type, name, device;
    Dest_path() -> version -> type -> name -> -> device ->;
    consstring
        (#| Explode_name(name),
            Explode_type(type),
            Explode_version(version, device == @:LOGICAL)
        |#)
enddefine;


define lconstant Namestring(path, lp);
    consstring
        (#| Explode_host(pt_host(path), lp),
            Explode_device(pt_device(path), lp),
            Explode_dir(pt_dir(path), lp),
            Explode_name(pt_name(path)),
            Explode_type(pt_type(path)),
            Explode_version(pt_version(path), lp)
        |#)
enddefine;


define namestring(item);
    if ispathname(Checkr_pathname_or_string(item) ->> item) then
        Namestring(item, pt_device(item) == @:LOGICAL)
    else
        item
    endif
enddefine;


define enough_namestring(path, default_path);
    lvars host, device, dir, name, type, version, lp;

    ;;; defaults
    if pop_true(default_path) then
        pathname(default_path)
    else
        default_pathname
    endif -> default_path;

    Dest_path(path) -> (host, device, dir, name, type, version);
    device == @:LOGICAL -> lp;

    consstring
        (#| if sys_=(host, pt_host(default_path)) then
                unless sys_=(device, pt_device(default_path)) do
                    Explode_device(device, lp)
                endunless
            else
                Explode_host(host, lp);
                Explode_device(device, lp)
            endif;
            unless sys_=(dir, pt_dir(default_path)) do
                Explode_dir(dir, lp)
            endunless;
            unless sys_=(name, pt_name(default_path)) do
                Explode_name(name)
            endunless;
            unless sys_=(type, pt_type(default_path)) do
                Explode_type(type)
            endunless;
            unless sys_=(version, pt_version(default_path))
            or sys_=(version, @:NEWEST)
            do
                Explode_version(version, lp)
            endunless
        |#)
enddefine;


/* Wild pathnames */

define is_wild(item);
    item == @:WILD
        or
    (ispair(item) and fast_lmember(@:WILD, item))
        or
    (isstring(item) and strmember(`*`, item))
enddefine;


define is_wild_pathname(item, field);
    lvars i, f;
    lconstant
        Fields = {^@:HOST ^@:DEVICE ^@:DIRECTORY ^@:NAME ^@:TYPE ^@:VERSION},
        Fields_type = conspair(@MEMBER, conslist(destvector(Fields))),
        ;

    returnif(isstream(item)) (false);       /* Stream can't be wild */

    if field /== nil then
        pathname(item) -> item
    endif;
    if ispathname(item) then
        if field /== nil then
            if (fast_vmember(field, Fields) ->> i) then
                is_wild(fast_record_access(i, item))
            else
                simple_type_error(
                    'Unknown pathname field key: ~S', field, Fields_type)
            endif
        else
            for f in_record item do
                returnif (is_wild(f)) (true)
            endfor;
            false
        endif
    else
        get_simple_string(item) -> item;
        strmember(`*`, item)
    endif
enddefine;


define wild_pathname_p(item, field);
    defaults field nil;
    if is_wild_pathname(item, field) then true else nil endif
enddefine;


define lconstant Fields_match(p, w, want_match);
    lvars wl, i, j;

    returnif (w == @:WILD) (if want_match then p else true endif);
    returnif (equal(p, w)) (if want_match then p else true endif);
    returnunless (isstring(p) and isstring(w)) (false);

    fast_vector_length(w) -> wl;
    returnif (wl == 0) (false);
    if subscrs(1, w) == `*` then
        ;;; p matches *foo
        if (isendstring(allbutfirst(1, w), p) ->> i) then
            if want_match then substring(1, i - 1, p) else true endif
        else
            false
        endif
    elseif subscrs(wl, w) == `*` then
        ;;; p matches foo*
        if isstartstring(allbutlast(1, w), p) then
            if want_match then allbutfirst(wl - 1, p) else true endif
        else
            false
        endif
    elseif (strmember(`*`, w) ->> i) then
        ;;; p matches foo*baz
        if ((isstartstring(substring(1, i - 1, w), p)
                and
             isendstring(substring(i + 1, wl - i, w), p)) ->> j)
        then
            if want_match then substring(i, j - i, p) else true endif
        else
            false
        endif
    else
        false
    endif
enddefine;


define lconstant Dirs_match(p, w);
    lvars ww;
    Append_dirs(p, []) -> p;
    Append_dirs(w, []) -> w;
    until p == [] or w == [] do
        sys_grbg_destpair(w) -> (ww, w);
        returnunless
            (Fields_match(sys_grbg_destpair(p) -> p, ww, false))
            (false)
    enduntil;
    p == w
enddefine;


define pathname_match_p(path, wild_path);
    lvars p, w, n;
    pathname(path) -> path;
    pathname(wild_path) -> wild_path;
    for p, w with_index n in_record path,wild_path do
        if n == 3 then
            returnunless (Dirs_match(p, w)) (false)
        else
            returnunless (Fields_match(p, w, false)) (false)
        endif
    endfor;
    true
enddefine;


define lconstant Get_tp_component(s, f, t);
    lvars x, c;
    if t == @:WILD or t == nil then
        if s == @:LOGICAL then
            @:UNSPECIFIC
        else
            s
        endif
    elseif is_wild(t) then
        unless (Fields_match(s, f, true) ->> x) do
            mishap(s, f, 2, 'Non-matching pathname components')
        endunless;
        consstring (#| for c in_string t do
                        if c == `*` then explode(x) else c endif
                    endfor |#)
    else
        t
    endif
enddefine;


define lconstant Get_tp_dir(s, f, t);
    lvars tt;
    Append_dirs(s, []) -> s;
    Append_dirs(f, []) -> f;
    Append_dirs(t, []) -> t;
    [% until t == [] do
        sys_grbg_destpair(t) -> (tt, t);
        if s == [] or f == [] then
            tt
        else
            Get_tp_component(sys_grbg_destpair(s) -> s,
                             sys_grbg_destpair(f) -> f,
                             tt)
        endif
    enduntil %]
enddefine;


define translate_pathname(source, from_path, to_path);
    lvars s, f, t, n;
    pathname(source) -> source;
    pathname(from_path) -> from_path;
    pathname(to_path) -> to_path;
    for s,f,t with_index n in_record source,from_path,to_path do
        if n == 3 then
            Get_tp_dir(s, f, t)
        else
            Get_tp_component(s, f, t)
        endif
    endfor;
    conspathname()
enddefine;


/* Logical pathnames */

define parse_logical_pathname(s, host);
    lvars i, j, k;

    define lconstant Substr(i, len, string);
        if len == 0 then
            nil
        else
            Checkr_lp_field(substring(i, len, string))
        endif
    enddefine;

/* HOST */
    if (locchar(`:`, 1, s) ->> i) and i > 1 then
        Substr(1, i - 1, s);
        i + 1 -> i
    elseif host then
        i or 1 -> i;
        host
    else
        mishap(s, 1, 'Missing host in logical pathname string')
    endif;
/* DEVICE */
    @:LOGICAL;
/* DIR */
    [%  if Thischar(i, s) == `;` then
            @:RELATIVE;
            i + 1 -> i
        endif;
        while (locchar(`;`, i, s) ->> j) do
            Substr(i, j - i, s);
            j + 1 -> i
        endwhile;
    %] -> j;
    if j == [] or front(j) == @:RELATIVE then
        j
    else
        conspair(@:ABSOLUTE, j)
    endif;
/* NAME, TYPE, VERS */
    if (locchar(`.`, i, s) ->> j) then
        Substr(i, j - i, s);                                        ;;; name
        if (locchar(`.`, j + 1, s) ->> k) then
            Substr(j + 1, k - j - 1, s);                            ;;; type
            Checkr_lp_version(                                      ;;; vers
                Substr(k + 1, fast_vector_length(s) - k, s))
        else
            Substr(j + 1, fast_vector_length(s) - j, s);            ;;; type
            nil                                                     ;;; vers
        endif
    else
        Substr(i, fast_vector_length(s) - i + 1, s);
        nil;
        nil
    endif;
enddefine;


define islogical_pathname(item);
    ispathname(item) and pt_device(item) == @:LOGICAL
enddefine;


built_in_class(@LOGICAL-PATHNAME, [^@PATHNAME], islogical_pathname);


define make_logical_pathname(string, host);
    conspathname(parse_logical_pathname(get_simple_string(string), host))
enddefine;


define logical_pathname(item);
    lvars lp;
    if islogical_pathname(item) then
        item
    elseif stringp(item) then
        make_logical_pathname(item, false) ->> lp;
        unless lp_translations(pt_host(lp)) do
            [] -> lp_translations(pt_host(lp))
        endunless
    elseif isstream(item)
    and (device_logical_pathname(streamdev(item)) ->> lp) then
        lp
    else
        lisp_error('Cannot convert ~S to a logical pathname', [^item])
    endif
enddefine;


define logical_pathname_translations(host) -> list;
    unless (lp_translations(Checkr_lp_host(host)) ->> list) do
        lisp_error('Unknown logical pathname host', [^host])
    endunless
enddefine;


define updaterof logical_pathname_translations(list, host);
    lvars rule, patt, trans;
    Checkr_lp_host(host) -> host;
    [% for rule in_cl_list list do
        unless listlength_>=(rule, 2) do
            lisp_error('Malformed logical pathname translation rule', [^rule])
        endunless;
        destpair(rule) -> (patt, rule);
        destpair(rule) -> (trans, rule);
        unless ispathname(patt) do
            make_logical_pathname(patt, host) -> patt
        endunless;
        pathname(trans) -> trans;
        conspair(patt, conspair(trans, rule))
    endfor %] -> lp_translations(host)
enddefine;


define load_logical_pathname_translations(host);
    lvars f;
    Checkr_lp_host(host) -> host;
    if lp_translations(host) then
        nil
    elseif (syssearchpath(lisp_modules_list, host <> '.lpt') ->> f) then
        @OPEN(f, 1) -> f;
        @EVAL(@READ(f, 1), 1) -> logical_pathname_translations(host);
        @CLOSE(f, 1) ->;
        true
    else
        lisp_error('Cannot locate .lpt file for logical host ~S', [^host])
    endif
enddefine;


define lconstant Translate_lp(lp);
    lvars rule, path;
    for rule in logical_pathname_translations(pt_host(lp)) do
        if pathname_match_p(lp, car(rule)) then
            translate_pathname(lp, car(rule), cadr(rule)) -> path;
            if pt_device(path) == @:LOGICAL then
                chain(path, Translate_lp)
            else
                if pt_host(path) == pt_host(lp) then
                    /* Can't use logical host as real host */
                    @:UNSPECIFIC -> pt_host(path)
                endif;
                return(path)
            endif
        endif
    endfor;
    file_error('Cannot translate logical pathname', [^lp], lp)
enddefine;


define translate_logical_pathname(item, keys);
    if stringp(item) then
        make_logical_pathname(item, false) -> item
    else
        pathname(item) -> item;
        returnunless (pt_device(item) == @:LOGICAL) (item)
    endif;
    Translate_lp(item)
enddefine;


/* checkr_filename */

define checkr_filename(item);
    if ispathname(Checkr_pathname_or_string(item) ->> item) then
        if pt_device(item) == @:LOGICAL then
            Translate_lp(item) -> item
        endif;
     #_IF VMS
        Namestring(item, false)
     #_ELSE
        procedure();
            dlocal % pt_host(item) % = nil;
            Namestring(item, false)
        endprocedure()
     #_ENDIF
    else
        item
    endif
enddefine;


/* Equality procedure for pathnames */


define pathname_=(p1, p2);
    lvars n, f1, f2, i1, i2;

    define lconstant Field_=(f1, f2);
        if f1 == nil or f1 == @:UNSPECIFIC then
            f2 == nil or f2 == @:UNSPECIFIC or f2 = @:NEWEST
        elseif f2 == nil or f2 == @:UNSPECIFIC then
            f1 == nil or f1 == @:UNSPECIFIC or f1 == @:NEWEST
        else
         #_IF VMS
            lvars c1, c2;
            for c1, c2 in_string f1, f2 do
                returnunless (caseless_=(c1, c2)) (false)
            endfor;
            true
         #_ELSE
            sys_=(f1, f2)
         #_ENDIF
        endif
    enddefine;


    for f1,f2 with_index n in_record p1,p2 do
        if n == 3 then
            for i1, i2 in_list f1, f2 do
                returnunless (Field_=(i1, i2)) (false)
            endfor
        else
            returnunless (Field_=(f1, f2)) (false)
        endif
    endfor;
    true
enddefine;


pathname_= -> class_=(pathname_key);


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Jul 23 1996
        :newest considered equal to nil and :unspecific when comparing
        pathnames with equal.
--- John Williams, Aug  9 1995
        Removed redundant lvar declarations.
--- John Gibson, Aug  9 1995
        Fixed missing declaration for c1,c2 in Field_= (VMS)
--- John Williams, May  5 1995
        Unix only: (pathname-version "foo") is now :unspecific.
--- John Williams, May  1 1995
        Append_dirs complains if :absolute is followved by :back or :up.
--- John Williams, Apr 25 1995
        Added load_logical_pathname_translations.
--- John Williams, Apr 12 1995
        Upgraded to Steele 1990 Ch 23.
--- John Williams, Nov  3 1994
        merge_pathnames now copes with structured directories (up to a
        point!). Also fixed bugs in Checkr_dir that caused stack empty
        error in (make-pathname :directory ".")
--- John Williams, Aug 11 1993
        sys_os_type instead of lisp_os_type.
--- John Williams, Jul 12 1993
        No longer uses cons_with.
--- John Williams, Jan  7 1993
        The updater of default_pathname no longer adds the file extension
        component to lispfiletypes.
 */
