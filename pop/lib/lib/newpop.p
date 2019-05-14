/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lib/lib/newpop.p
 > Purpose:         Installs a new POPLOG system
 > Author:          Robert John Duncan, Apr 22 1991 (see revisions)
 > Documentation:   HELP * NEWPOP
 > Related Files:   C.unix/src/newpop, C.vms/src/newpop.com
 */

#_TERMIN_IF DEF POPC_COMPILING

compile_mode:pop11 +strict;

include sysdefs.ph;

section;

lvars
    ;;; Environment for the new Poplog system
    USEPOP, POPSYS, POPSRC,
;

;;; skipto, skipover, skipbackover:
;;;     string utilities for parsing option lines

define lconstant skipto(cs, i, s) -> i;
    lvars cs, i, s, lim = datalength(s);
    until i > lim or locchar(subscrs(i, s), 1, cs) do
        i + 1 -> i;
    enduntil;
enddefine;

define lconstant skipover(cs, i, s) -> i;
    lvars cs, i, s, lim = datalength(s);
    until i > lim or not(locchar(subscrs(i, s), 1, cs)) do
        i + 1 -> i;
    enduntil;
enddefine;

define lconstant skipbackover(cs, lim, s) -> i;
    lvars cs, lim, s, i = datalength(s)+1;
    until i <= lim or not(locchar(subscrs(i-1, s), 1, cs)) do
        i - 1 -> i;
    enduntil;
enddefine;

;;; implode:
;;;     make a single string out of several items separated by spaces

define lconstant implode(items);
    lvars item, items, first = true;
    consstring(#|
        for item in items do
            unless item = nullstring then
                unless first then `\s` endunless;
                false -> first;
                dest_characters(item);
            endunless;
        endfor;
    |#);
enddefine;

;;; files_matching, try_file_move, try_file_delete:
;;;     file utilities

define lconstant files_matching(pattern);
    lvars pattern;
    expandlist(pdtolist(sys_file_match(pattern, nullstring, false, false)));
enddefine;

define lconstant try_file_move(file1, file2) -> moved;
    lvars file1, file2, moved;
    if sys_file_exists(file1) ->> moved then
        sys_file_move(file1, file2);
    endif;
enddefine;

define lconstant try_file_delete(file);
    lvars file;
#_IF DEF UNIX
    ;;; delete the named file; DON'T follow symbolic links
    sysunlink(file);
#_ELSE
    sysdelete(file);
#_ENDIF
enddefine;

;;; gen_com_name, gen_exe_name, etc.
;;;     complete the name of a file according to its type

define lconstant add_extension(name, extn) -> name;
    lvars name, extn;
    unless sys_fname_extn(name) = extn then
        name <> extn -> name;
    endunless;
enddefine;

define lconstant gen_exe_name(name) -> name;
    lvars name = sysfileok(name);
#_IF DEF VMS
    add_extension(name, '.exe') -> name;
#_ENDIF
enddefine;

define lconstant gen_stb_name =
    sysfileok <> add_extension(% '.stb' %);
enddefine;

define lconstant gen_map_name =
    sysfileok <> add_extension(% '.map' %);
enddefine;

define lconstant gen_psv_name(name) -> name;
    lvars name;
    if isword(name) then
        '$popsavelib/' dir_>< name
    else
        sysfileok(name)
    endif -> name;
    add_extension(name, '.psv') -> name;
enddefine;

define lconstant gen_com_name(name) -> name;
    lvars name;
    if isword(name) then
        '$popcom/' dir_>< name
    else
        sysfileok(name)
    endif -> name;
#_IF DEF VMS
    add_extension(name, '.com') -> name;
#_ENDIF
enddefine;

;;; gen_command:
;;;     combine a command name and a list of arguments into a string
;;;     suitable for passing to -sysobey-.
;;;     Returns <false> if the command doesn't seem to exist.

define lconstant gen_command(name, args) -> cmd;
    lvars name, arg, args, cmd = false;
    gen_com_name(name) -> name;
#_IF DEF UNIX
    returnunless(sys_fname_path(name) = nullstring or sys_file_exists(name));
#_ELSE
    returnunless(sys_file_exists(name));
    '@' <> name -> name;
#_ENDIF
    implode(name :: args) -> cmd;
enddefine;

;;; info, warn, fail:
;;;     display diagnostic messages

define lconstant message(msg, args, cucharout);
    lvars   msg, args;
    dlocal  cucharout, pr = sys_syspr, pop_pr_quotes = false;
    sysflush(pop_charerr_device); sysflush(pop_charout_device);
    printf(msg, args);
    sysflush(pop_charerr_device); sysflush(pop_charout_device);
enddefine;

define lconstant info(msg);
    lvars msg, args = [];
    if islist(msg) then msg -> args -> msg endif;
    message(msg, args, charout);
enddefine;

define lconstant warn(msg);
    lvars msg, args = [];
    if islist(msg) then msg -> args -> msg endif;
    message('!!! Warning - %s\n', [^msg ^^args], charerr);
enddefine;

define lconstant fail(msg);
    lvars msg, args = [];
    if islist(msg) then msg -> args -> msg endif;
    message('*** Error - %s\n', [^msg ^^args], charerr);
    interrupt();
enddefine;

;;; obey:
;;;     execute a command string

define lconstant obey(cmd);
    lvars cmd;
    info('=== Running: %p\n', [^cmd]);
    sysobey(cmd);
enddefine;


/**********************************************************************\
*                                                                      *
*                           Option Processing                          *
*                                                                      *
\**********************************************************************/

lconstant

        ;;; list of all available options
    option_words = [
        link        x           rsv         install
        startup     images      eliza       prolog
        logic       clisp       pml         xved
        commands    ved         help        hlp
        ref         teach       doc         im
        vt100       xpw         indexes     stripdoc
        local
    %#_IF DEF VMS
        "'ve*d'"
    #_ENDIF%
    ],

    option_dependencies = [
        ;;; noprolog => nologic
        ;;; (handled by special case below)
        ;;; nox => noxpw
        [x xpw]
        ;;; nox => noxved
        [x xved]
    ],

    default_options_file =
        '$popsrc/' dir_>< 'newpop_options',

    system_options_file =
        '$popsys/' dir_>< 'newpop_options',
;

;;; isoption:
;;;     maps option words to string values (usually nullstring, unless
;;;     the option has been qualified as <option> = <value>)

define lconstant isoption =
    newassoc([]);
enddefine;

;;; parse_option:
;;;     parses a string of the form:
;;;         option [= value]

define lconstant parse_option(string);
    lvars i, j, string, option;
    ;;; strip trailing comment
    if issubstring(';;;', string) ->> i then
        substring(1, i-1, string) -> string;
    endif;
    ;;; extract option name
    skipover('\s\t', 1, string) -> i;
    skipto('\s\t\n=', i, string) -> j;
    substring(i, j-i, string) -> option;
    returnif(option = nullstring)(false);
    ;;; followed by "=" ?
    skipover('\s\t', j, string) -> i;
    skipover('=', i, string) -> j;
    returnunless(substring(i, j-i, string) = '=')(option);
    ;;; extract value
    skipover('\s\t\n', j, string) -> i;
    skipbackover('\s\t\n', i, string) -> j;
    [% option, substring(i, j-i, string) %];
enddefine;

;;; record_option:
;;;     reads an option from -string- and adds it to the -isoption- table

define lconstant record_option(string);
    lvars string, option, value = nullstring;
    returnunless(parse_option(string) ->> option);
    if islist(option) then
        dl(option) -> (option, value);
    endif;
    if isstartstring('no', option) then
        ;;; 'noX' turns off option X
        allbutfirst(2, option) -> option;
        false -> value;
    endif;
    consword(option) -> option;
    if lmember(option, option_words) then
        value -> isoption(option);
    else
        warn('unrecognised option: %p', [^option]);
    endif;
enddefine;

;;; read_options_file:
;;;     reads options from a file, one per line

define lconstant read_options_file(file, error);
    lconstant buff = inits(512);
    lvars n, s, dev, file, error;
    if sysopen(file, 0, "line") ->> dev then
        info('Reading options file: %p\n', [% device_full_name(dev) %]);
    else
        error('can\'t open options file: %p', [^file]);
        return;
    endif;
    nullstring -> s;
    until (sysread(dev, buff, 512) ->> n) == 0 do
        if n >= 2 and buff(n) == `\n` and buff(n-1) == `\\` then
            ;;; ignore newline: join to next
            `\s` -> buff(n-1);
            s <> substring(1, n-1, buff) -> s;
        else
            record_option(s <> substring(1, n, buff));
            nullstring -> s;
        endif;
    enduntil;
    unless s = nullstring then record_option(s) endunless;
    sysclose(dev);
enddefine;

;;; read_options:
;;;     accumulates options from the base options file, any extra options
;;;     files and the command line

define lconstant read_options(args);
    lvars arg, args, options = [], file = false, files = [];
    ;;; process command line arguments
    until file or args == [] do
        destpair(args) -> (arg, args);
        if arg = '-options' then
            ;;; extra options file
            unless args == [] then
                destpair(args) -> (arg, args);
                arg :: files -> files;
            endunless;
        elseif isstartstring('-', arg) then
            ;;; explicit option
            allbutfirst(1, arg) :: options -> options;
        else
            ;;; base options file
            arg -> file;
        endif;
    enduntil;
    unless file then
        default_options_file -> file;
        if sys_file_exists(system_options_file) then
            system_options_file :: files -> files;
        endif;
    endunless;
    ;;; initialise the options table
    clearproperty(isoption);
    ;;; x, commands and images options are set by default
    nullstring ->> isoption("x") ->> isoption("commands") -> isoption("images");
    ;;; read the base options file, unless "only" was specified
    unless file = 'only' then read_options_file(file, fail) endunless;
    ;;; read supplementary options files
    for file in files do
        read_options_file(file, warn);
    endfor;
    ;;; add explicit command-line options
    applist(options, record_option);
    ;;; check dependencies
    for arg in option_dependencies do
        unless isoption(arg(1)) then
            false -> isoption(arg(2));
        endunless;
    endfor;
    ;;; special-case dependencies
    if isoption("link") and not(isoption("install")) then
        ;;; newpop11 is to be relinked, but not installed: for safety,
        ;;; we disable everything which may depend on basepop11 being
        ;;; up-to-date
        for arg in [
                xpw
                startup
                commands
                images
                indexes
                stripdoc
                local
        ] do
            false -> isoption(arg);
        endfor;
    endif;
    if isoption("logic") and not(isoption("prolog")) then
        ;;; HELP NEWPOP says you can't have logic without prolog, but
        ;;; this special case allows it to stand provided there's
        ;;; already a Prolog saved image and no other option's been
        ;;; specified that would invalidate it. This is here to provide
        ;;; an easy way of building just the logic image now that it's
        ;;; no longer made by default: just do
        ;;;     $popsrc/newpop -logic only
        if isoption("images") then
            if not(sys_file_exists(gen_psv_name("prolog")))
            or isoption("install") or isoption("startup")
            then
                false -> isoption("logic");
            endif;
        endif;
    endif;
enddefine;


/**********************************************************************\
*                                                                      *
*                       Defining Poplog commands                       *
*                                                                      *
\**********************************************************************/

lconstant

    ;;; command symbol definition files in $popsys (sourced by
    ;;; $popcom/popenv at login time)
    csh_popenv_file =
        '$popsys/' dir_>< 'popenv',
    sh_popenv_file =
        '$popsys/' dir_>< 'popenv.sh',
    dcl_popenv_file =
        '$popsys/' dir_>< 'popenv.com',

    ;;; definitions for the standard commands
    standard_commands = [
        [eliza      [pop11 -eliza]]
        [prolog     [pop11 -prolog]]
        [clisp      [pop11 -clisp]]
        [pml        [pop11 +pml]]
        [xved       [pop11 +xved]]
        [ved        [pop11 :sysinitcomp();ved]]
        [help       [pop11 :sysinitcomp();help]]
        [hlp        [pop11 :sysinitcomp();help]]
        [ref        [pop11 :sysinitcomp();ref]]
        [teach      [pop11 :sysinitcomp();teach]]
        [doc        [pop11 :sysinitcomp();doc]]
        [im         [pop11 :sysinitcomp();im]]
        [vt100      [pop11 :vedvt100(false);sysinitcomp();ved]]
        [%"'ve*d'"% [pop11 :sysinitcomp();ved]]
    ],
;

;;; sys_symbol:
;;;     returns the value of a command symbol: 'pop11', 'prolog' etc.
;;;     For Unix this is the same as -systranslate-, since all symbols
;;;     share the same environment.

define lconstant sys_symbol() with_nargs 1;
#_IF DEF UNIX
    systranslate();
#_ELSE
    sys_symbol_value();
#_ENDIF
enddefine;
;;;
define updaterof lconstant sys_symbol(value, symbol);
    lvars value, symbol;
#_IF DEF UNIX
    value -> systranslate(symbol);
#_ELSE
    ;;; avoid cancelling a symbol which isn't actually defined
    if value or sys_symbol_value(symbol) then
        value -> sys_symbol_value(symbol, true);
    endif;
#_ENDIF
enddefine;

;;; gen_command_symbol:
;;;     returns the symbol corresponding to a command name like 'pop11',
;;;     'prolog' etc.

define lconstant gen_command_symbol(name) -> name;
    lvars name;
#_IF DEF UNIX
    'pop_' sys_>< name -> name;
#_ELSE
    name sys_>< nullstring -> name;
#_ENDIF
enddefine;

;;; write_symbol_definition:
;;;     writes the definition of a global command symbol onto the popenv
;;;     files

define lconstant write_symbol_definition(symbol, value);
    lvars   symbol, value;
    dlocal  cucharout;
#_IF DEF UNIX
    discappend(csh_popenv_file) -> cucharout;
    ;;; setenv symbol "value"
    printf('setenv %p "%p"\n', [^symbol ^value]);
    cucharout(termin);
    discappend(sh_popenv_file) -> cucharout;
    ;;; symbol="value"; export symbol
    printf('%p="%p"; export %p\n', [^symbol ^value ^symbol]);
    cucharout(termin);
#_ELSE
    discappend(dcl_popenv_file) -> cucharout;
    ;;; $ symbol :== value
    printf('$ %p :== %p\n', [^symbol ^value]);
    cucharout(termin);
#_ENDIF
enddefine;

;;; add_command_definition:
;;;     defines a command symbol in the current environment and writes it
;;;     to the popenv files

define lconstant add_command_definition(name, expansion);
    lvars name, expansion, symbol, base, flag, image, images, arg, args, expr;
    ;;; the command symbol to be defined
    gen_command_symbol(name) -> symbol;
    ;;; the base command to run
    dest(expansion) -> (base, expansion);
    if base then gen_command_symbol(base) -> base endif;
    ;;; saved images to restore
    [%  while expansion /== []
        and (expansion(1) == "-" or expansion(1) == "+")
        do
            dest(expansion) -> (flag, expansion);
            dest(expansion) -> (image, expansion);
#_IF DEF VMS
            if flag == "-" then "/" -> flag endif;
#_ENDIF
            flag sys_>< gen_psv_name(image);
        endwhile;
    %] -> images;
    ;;; special arguments and start-up commands
    [%  until expansion == [] or isstartstring(':', expansion(1)) do
            dest(expansion) -> (arg, expansion);
#_IF DEF VMS
            ;;; escape special args
            if isstartstring('%', arg) then '\\' <> arg -> arg endif;
#_ENDIF
            arg;
        enduntil;
    %] -> args;
    ;;; expression(s) to evaluate
    consstring(#| applist(expansion, explode) |#) -> expr;
    ;;; define the command symbol in the current environment and add
    ;;; it to the list of symbols defined at login time
#_IF DEF UNIX
    implode([%
        if base then sys_symbol(base) endif;
        explode(images);
        explode(args);
        expr;
    %]) -> sys_symbol(symbol);
    write_symbol_definition(symbol, implode([%
        if base then '$' <> base endif;
        explode(images);
        explode(args);
        expr;
    %]));
#_ELSE
    implode([%
        if base then sys_symbol(base) else '$popsys:basepop11' endif;
        explode(images);
        explode(args);
        if expr /= nullstring then '"' <> expr endif;
    %]) -> sys_symbol(symbol);
    write_symbol_definition(symbol, implode([%
        if base then '\'' <> base <> '\'' else '$popsys:basepop11' endif;
        explode(images);
        explode(args);
        if expr /= nullstring then '"""' <> expr endif;
    %]));
#_ENDIF
enddefine;

;;; make_command:
;;;     install the definition of a standard command

define lconstant make_command(name, expansion);
    lvars name, expansion;
    returnunless(name == "pop11" or isoption(name));
#_IF DEF UNIX
    lvars link_name = '$popsys/' dir_>< name;
    try_file_delete(link_name) -> ;
    syslink('$popsys/basepop11', link_name) -> ;
    info('Linked basepop11 to %p\n', [^name]);
#_ENDIF
    add_command_definition(name, expansion);
enddefine;

;;; make_commands:
;;;     install the standard commands

define lconstant make_commands();
    if isoption("commands") then
        applist(standard_commands, explode <> make_command);
    endif;
enddefine;


/**********************************************************************\
*                                                                      *
*                          Making saved images                         *
*                                                                      *
\**********************************************************************/

lconstant

    ;;; commands to build standard saved images
    standard_images = [
        [eliza      mkeliza]
        [prolog     mkplog]
        [logic      mklogic]
        [clisp      mkclisp]
        [pml        mkpml]
        [xved       mkxved]
    ],
;

;;; make_image:
;;;     build a saved image

define lconstant make_image(name, cmdname);
    lvars name, cmdname, cmd, psvname;
    gen_psv_name(name) -> psvname;
    if (isoption(name) ->> cmd) and cmd = nullstring then
        gen_command(cmdname, []) -> cmd;
    endif;
    if cmd then
        obey(cmd);
        unless sys_file_exists(psvname) then
            warn('failed to make %p saved image', [^name]);
        endunless;
    endif;
enddefine;

;;; make_images:
;;;     build the standard images

define lconstant make_images();
    if isoption("images") then
        applist(standard_images, explode <> make_image);
    endif;
enddefine;

;;; make_startup:
;;;     build the startup saved image (if required) and define the
;;;     command symbol for 'pop11'

define lconstant make_startup();
    lvars args = [];
    if isoption("startup") then
        ;;; make the startup image ...
        make_image("startup", "mkstartup");
        ;;; ... and define pop11 to run it
        make_command("pop11", [^false -startup]);
    elseif isoption("install") then
        ;;; define pop11 to be the same as basepop11
        make_command("pop11", [^false]);
    endif;
enddefine;


/**********************************************************************\
*                                                                      *
*                     Linking newpop11 in $popsys                      *
*                                                                      *
\**********************************************************************/


;;; xlink_command:
;;;     returns command and arguments for linking newpop11 with X

define lconstant xlink_command(xlink) -> (cmd, args);
    lvars xlink, cmd, args;

    ;;; default link command
    ('./pglink', []) -> (cmd, args);

    ;;; change link command if there's a special X version
    if sys_file_exists(gen_com_name('pglink_x')) then
        './pglink_x' -> cmd;
    endif;

    ;;; check for any user-specified link options
    returnif(xlink = nullstring);   ;;; use normal default

    unless isstartstring('-x', xlink) then
        ;;; deal with actual xlink specification (for backward compatibility)
        'x=' <> xlink -> xlink;
#_IF DEF VMS
        ;;; stupid, stupid VMS is liable to delete a user-mode assignment
        ;;; when running pglink.com, so assign it in supervisor mode (which
        ;;; means it remains set after newpop finishes -- too bad)
        xlink -> systranslate('POP_XNEWPOP_EXLIBS', 2);
#_ELSE
        xlink -> systranslate('POP_XNEWPOP_EXLIBS');
#_ENDIF
        '-xnewpop' -> xlink;

    ;;; else just give arg to pglink directly
    endunless;

    [^xlink ^^args] -> args;
enddefine;

;;; link_newpop11:
;;;     run pglink to relink newpop11 in popsys

define lconstant link_newpop11();
    lvars   cmd, pglink_cmd, pglink_args, xlink;
    dlocal  current_directory = POPSYS;
    if isoption("link") then
        ;;; Run pglink
        if isoption("x") ->> xlink then
            xlink_command(xlink)
        else
            ('./pglink', ['-nox'])
        endif -> (pglink_cmd, pglink_args);
        if isoption("rsv") then
            [^^pglink_args '-rsv'] -> pglink_args;
        endif;
        ;;; always produce the map file
        [^^pglink_args '-map'] -> pglink_args;
        unless gen_command(pglink_cmd, pglink_args) ->> cmd then
            fail('can\'t link newpop11: missing command file %p',
                [^pglink_cmd]);
        elseunless sys_file_exists(gen_exe_name('corepop'))
        and sys_file_exists(gen_psv_name('poplink'))
        then
            fail('can\'t link newpop11: missing corepop/poplink.psv');
        endunless;
        try_file_delete(gen_exe_name('rsvpop11')) -> ;
        info('Linking newpop11 ...\n');
        obey(cmd);
        unless sys_file_exists(gen_exe_name('newpop11')) then
            fail('failed to link newpop11');
        endunless;
    elseif sys_file_exists(gen_exe_name('newpop11')) then
        ;;; already linked: make sure the link base and map are up-to-date
#_IF DEF UNIX
        Extern_make_base(
            gen_exe_name('newpop11'), gen_stb_name('newpop11'), true);
        obey(gen_command('nm', [%
            '-n', gen_exe_name('newpop11'),
            '>',  gen_map_name('newpop11');
        %]));
#_ELSE
        Extern_make_base(dup(gen_stb_name('newpop11')), true);
#_ENDIF
    else
        return;
    endif;
    if sys_file_exists(gen_exe_name('rsvpop11'))
    and sys_file_exists(gen_stb_name('newpop11'))
    then
        sys_file_copy('newpop11.stb', 'rsvpop11.stb');
    endif;
    ;;; purge old STB files
#_IF DEF UNIX
    while sysdelete('newpop11.stb-') do endwhile;
    while sysdelete('rsvpop11.stb-') do endwhile;
#_ELSE
    obey('purge *.stb');
#_ENDIF
enddefine;


/**********************************************************************\
*                                                                      *
*                   Installing basepop11 in $popsys                    *
*                                                                      *
\**********************************************************************/

;;; install_basepop11_command:
;;;     initialises the popsys directory, clearing out any existing
;;;     commands, then copies popsys/newpop11.* to popsys/basepop11.*

define lconstant install_basepop11_command();
    lvars   file, cmd, statb = initv(10);
    dlocal  current_directory = POPSYS, pop_file_versions = 1;
    returnunless(isoption("install"));
    unless sys_file_exists(gen_exe_name('newpop11')) then
        fail('no newpop11 image in popsys');
    endunless;
    info('Moving newpop11 to basepop11 ...\n');
    ;;; create empty popenv file(s)
#_IF DEF UNIX
    discout(csh_popenv_file)(termin);
    discout(sh_popenv_file)(termin);
#_ELSE
    discout(dcl_popenv_file)(termin);
#_ENDIF
    ;;; cancel any existing command definitions
    false -> sys_symbol(gen_command_symbol("basepop11"));
    for cmd in [[pop11] ^^standard_commands] do
        false -> sys_symbol(gen_command_symbol(cmd(1)));
#_IF DEF UNIX
        ;;; remove any associated executable
        try_file_delete(gen_exe_name(cmd(1))) -> ;
#_ENDIF
    endfor;
    ;;; clear existing images from POPSAVELIB (they won't restore)
    for file in files_matching('$popsavelib/*.psv') do
#_IF DEF sys_install_image
        ;;; we MUST ensure they're deinstalled before deleting (because shared
        ;;; memory sections for a file are keyed on the inode/file-ident
        ;;; number, not on the file name)
        sys_install_image(file, false);
#_ENDIF
        try_file_delete(file) -> ;
    endfor;
    ;;; check for existing basepop11 image
    if sys_file_stat(gen_exe_name('basepop11'), statb) then
        ;;; Existing copy of basepop11 in POPSYS:
#_IF DEF UNIX
        ;;; delete all links
        lvars inode = statb(8);
        for file in files_matching('*') do
            if file /= gen_exe_name('basepop11')
            and sys_file_stat(file, statb) and statb(8) == inode
            then
                sysunlink(file) -> ;
            endif;
        endfor;
#_ENDIF
        ;;; copy to "oldpop11"
        try_file_delete(gen_exe_name('oldpop11')) -> ;
        try_file_delete(gen_stb_name('oldpop11')) -> ;
        try_file_delete(gen_map_name('oldpop11')) -> ;
        sys_file_move(gen_exe_name('basepop11'), gen_exe_name('oldpop11'));
        try_file_move(gen_stb_name('basepop11'), gen_stb_name('oldpop11')) -> ;
        try_file_move(gen_map_name('basepop11'), gen_map_name('oldpop11')) -> ;
        warn('existing version of basepop11 in popsys saved as oldpop11');
    endif;
    ;;; now do the move
    sys_file_move(gen_exe_name('newpop11'), gen_exe_name('basepop11'));
    try_file_move(gen_stb_name('newpop11'), gen_stb_name('basepop11')) -> ;
    try_file_move(gen_map_name('newpop11'), gen_map_name('basepop11')) -> ;
#_IF DEF UNIX
    ;;; check that popexlinkbase is set correctly for subsequent commands
    if sys_file_exists('basepop11.stb') then
        POPSYS dir_>< 'basepop11.stb' -> systranslate('popexlinkbase');
    endif;
#_ELSE
    ;;; create initial command symbol for basepop11
    add_command_definition('basepop11', [^false]);
#_ENDIF
enddefine;


/**********************************************************************\
*                                                                      *
*                           Special actions                            *
*                                                                      *
\**********************************************************************/

;;; make_external_library:
;;;     build the external library '$popexternlib/libpop.<extn>'
;;;     if it doesn't already exist

#_IF DEF SHARED_LIBRARIES and DEF EXTERNAL_LIBRARY_IS_SHARED
    #_IF DEFV HPUX >= 8.0
        lconstant external_library_extn = 'sl';
    #_ELSEIF DEFV SYSTEM_V >= 4.0
        lconstant external_library_extn = 'so';
    #_ELSE ERROR
    #_ENDIF
#_ELSEIF DEF VMS
    lconstant external_library_extn = 'olb';
#_ELSE
    lconstant external_library_extn = 'a';
#_ENDIF

define lconstant make_external_library();
    lconstant external_library =
        '$popexternlib/' dir_>< ('libpop.' <> external_library_extn);
    lvars   cmd;
    dlocal  current_directory;
    unless sys_file_exists(external_library) then
        '$popexternlib/' -> current_directory;
        info('Making external library ...\n');
        if gen_command('./mklibpop', []) ->> cmd then
            obey(cmd);
        endif;
        unless sys_file_exists(external_library) then
            warn('failed to make external library');
        endunless;
    endunless;
enddefine;

;;; make_xpw:
;;;     build the Poplog Widget Set

define lconstant make_xpw();
    lvars cmd;
    if (isoption("xpw") ->> cmd) and cmd = nullstring then
        gen_command("mkXpw", []) -> cmd;
    endif;
    if cmd then
        info('Making the Poplog Widget Set ...\n');
        obey(cmd);
    endif;
enddefine;

;;; make_indexes:
;;;     make documentation indexes, and optionally strip VED char attributes
;;;     and special characters, etc.

define lconstant make_indexes();
    lvars cmd, args, strip = isoption("stripdoc");
    if isoption("indexes") or strip then
        if strip then ['-strip'] else [] endif -> args;
        if gen_command("makeindexes", args) ->> cmd then
            info('Making documentation indexes ...\n');
            obey(cmd);
        endif;
    endif;
enddefine;

;;; run_local_newpop:
;;;     run the local NEWPOP command

define lconstant run_local_newpop();
    lvars cmd;
    if isoption("local") then
        if gen_command('$poplocal/local/com/newpop', []) ->> cmd then
            info('Running local newpop ...\n');
            obey(cmd);
        endif;
    endif;
enddefine;


/**********************************************************************\
*                                                                      *
*                                NEWPOP                                *
*                                                                      *
\**********************************************************************/

;;; check_defined:
;;;     check that the environment variable -name- is set and return its
;;;     value; translate concealed names in VMS.

define lconstant check_defined(name) -> value;
    lvars name, value;
    lconstant macro CONCEALED = #_IF DEF VMS 2:1000 #_ELSE [] #_ENDIF;
    unless systranslate(name, CONCEALED) ->> value then
        fail('undefined name: %p', [^name]);
    endunless;
    info('%p = %p\n', [^name ^value]);
enddefine;

define lconstant newpop();

    define dlocal interrupt();
        false -> pop_exit_ok;
        info('*** Interrupted\n');
        sysexit();
    enddefine;

    define dlocal prmishap(msg, culprits);
        lvars msg, culprits;
        sysprmishap(msg, culprits);
        fail('mishap (see above)');
    enddefine;

    sysgarbage();
    max(popmemused+2e5, popmemlim) -> popmemlim;

    info('NEWPOP started on %p\n', [% sysdaytime() %]);

    info('system = %p\n', [% implode(sys_machine_type) %]);
    check_defined('usepop') -> USEPOP;
    check_defined('popsys') -> POPSYS;
    check_defined('popsrc') -> POPSRC;
    read_options(poparglist);

    make_external_library();
    link_newpop11();
    install_basepop11_command();
    make_xpw();
    make_startup();
    make_commands();
    make_images();
    make_indexes();
    run_local_newpop();

    info('NEWPOP finished on %p\n', [% sysdaytime() %]);
enddefine;

;;; $-Pop$-Main:
;;;     Standard entry procedure called when the newpop saved image
;;;     is restored. Runs -newpop- and exit.

define $-Pop$-Main();
    ;;; Run NEWPOP procedure only if called from NEWPOP shell command
    unless sys_symbol('POPNEWPOP') then
        mishap(0, 'CANNOT RUN NEWPOP');
    endunless;
    newpop();
    sysexit();
enddefine;

endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, Nov  9 1995
        Removed pw*m stuff
--- Robert John Duncan, May 22 1995
        Added a special case in read_options to simplify rebuilding the
        logic saved image
--- John Gibson, Dec  6 1994
        Made install_basepop11_command deinstall saved images before
        deleting them
--- Simon Nichols, Nov 25 1993
        The external library $popexternlib/libpop.a is now a static archive
        on all systems. This has necessitated yet another change to
        make_external_library to force a non-shared library file extension
        on all systems.
--- Simon Nichols, Oct 13 1993
        Further change to make_external_library: non-shared library file
        extension for $popexternlib is now '.a' on Unix (but still '.olb'
        on VMS).
--- Simon Nichols, Oct  4 1993
        Changed make_external_library to cope with shared library file
        extensions for $popexternlib (i.e. '.so' for SVR4/Solaris and '.sl'
        for HP-UX).
--- John Gibson, Jul 10 1993
        safepop11 -> $popsys/corepop; pglink now also in $popsys and newpop11
        linked there too.
--- Robert John Duncan, Jun  7 1993
        Command to build the external library -- mklibpop -- no longer takes
        an argument.
--- John Gibson, May 14 1993
        Changed xlink_command again so that 'x' without params gives
        no args to pglink (i.e. use default) and 'x = -xfoo' passes -xfoo.
        No longer looks for pglink_xtype command files (since these are
        now dealt with by poplink based on the X link type).
--- John Gibson, May 11 1993
        Changed xlink_command to use new poplink X-linking mechanism, and
        to use POP_XLINK_EXLIBS if "x" argument is supplied without any
        parameters.
--- John Gibson, May  8 1993
        Moved X link parsing stuff to LIB * sys_translate_exlibs.
--- Robert John Duncan, May  5 1993
        Changed default OLIT version for Solaris to 3001 -- not strictly
        correct, but the previous non-integer value wouldn't work as an
        argument to pglink.
--- Robert Duncan, Apr 19 1993
        Changed the install step to set popexlinkbase if the basepop11.stb
        file is present, because it may not have been set by the login script.
        Replaced file*_exists with new sys_file_exists.
--- Robert John Duncan, Apr 16 1993
        Simplified treatment of "commands" and "images" options and made "x"
        be set by default likewise.
--- Robert John Duncan, Apr 15 1993
        Made the installation step optional (flagged by option "install");
        changed the selection of X link defaults to allow more system-
        specific configurations;
        removed the requirement for a '.stb' file to be present (because not
        all systems need it now);
        made the "commands" and "images" options be set by default to
        simplify command-line argument passing.
--- John Gibson, Jan 19 1993
        Replaced redefinition of s*yssetup with $-Pop$-Main (called by
        mkimage with -entrymain arg).
--- John Gibson, Dec  8 1992
        Fixed VMS version to not put brackets around -xlibs arg to pglink
        when there is only one arg (which there always will be), since
        otherwise it goes over the limit of 8 args to VMS command files
--- John Gibson, Oct 21 1992
        Added #_TERMIN_IF for POPC
--- John Gibson, Sep 17 1992
        Added "stripdoc" option to be passed to makeindexes
--- Robert John Duncan, Aug 11 1992
        Set default X link configuration for SunOS 5 to be OLIT 3.0.1
--- Robert John Duncan, Jul 27 1992
        Changed "xpw" and "pw*m" options to allow an alternative command to
        be given
--- Robert John Duncan, May 29 1992
        Added missing definition for "im" command
--- Adrian Howard, Feb 20 1992
        Altered -xlink_command- to enable -XLINK_VERSION- to be specified
--- Robert John Duncan, Dec  2 1991
        Included IRIX in Motif-based systems
--- Robert John Duncan, Nov  5 1991
        Added VMS Motif libraries
--- Robert John Duncan, Oct 24 1991
        Changed to use 'basepop11' consistently throughout.
        Added the 'only' option for suppressing the base options file and
        added an info message for each options file read.
        Made use of -pop_file_versions- to limit creation of backup files
        during installation.
--- Robert John Duncan, Sep 23 1991
        Changed for inclusion in a saved image.
--- Robert John Duncan, Sep  5 1991
        Added definition of 'basepop11' command.
        Reordered actions in -install_pop11_command- so that the moving of
        'newpop11' is done as late as possible.
--- Robert John Duncan, Sep  2 1991
        Changed expansion of "xved" command
--- Robert John Duncan, Aug 20 1991
        Added "xved" option
--- Robert John Duncan, Aug 12 1991
        Changed -xlink_command- to support specification of both the link
        type and the link arguments in the "x" qualifier.
--- John Gibson, Aug  2 1991
        Added CONCEALED macro for optional 2nd arg to systranslate in VMS.
--- Robert John Duncan, Jul 18 1991
        Replaced most uses of -sysdelete- with -try_file_delete-:
        this does -sysunlink- on Unix, to avoid problems with symbolic
        links.
--- Robert John Duncan, Jul  4 1991
        Added cancellation of "pop11" symbol in installation procedure.
        Moved building of Xpw & PW*M up the order in case any later step
        depends on them.
--- Robert John Duncan, Jul  2 1991
        Changed 'recent' to 'oldpop11'
--- Robert John Duncan, Jun 25 1991
        Added 've*d' command symbol for VMS.
--- Robert John Duncan, Jun 20 1991
        Tested and fixed for VMS.
--- Robert John Duncan, Jun 14 1991
        Made use of new -sys_symbol_value- for VMS.
        Removed default X link option for VMS until we get proper
            information.
        Changed -read_options_file- to allow an option to extend over
            multiple lines.
--- Robert John Duncan, Jun  5 1991
        Removed libXaw from MIT X link arguments
--- Robert John Duncan, May 30 1991
        Fixed -run_local_newpop-
 */
