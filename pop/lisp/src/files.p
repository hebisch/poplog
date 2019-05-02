/* --- Copyright University of Sussex 1995. All rights reserved. ----------
 > File:            C.all/lisp/src/files.p
 > Purpose:         Common Lisp file system interface
 > Author:          John Williams, May 22 1987 (see revisions)
 > Documentation:   CLtL, p409-427
 > Related Files:   C.all/lisp/src/streams.p
 */

lisp_compile_mode;

section $-lisp;

constant _1900_1970;        /* No. seconds between 1900 and 1970 */


define checkr_filenames(files);
    if atom(files) then
        conspair(checkr_filename(files), [])
    else
        [% until endp(files) do
            checkr_filename(fast_destpair(files) -> files)
        enduntil %]
    endif
enddefine;


define file_error(message, involving, file);
    fast_chain(
        @FILE-ERROR,
        {^@:MESSAGE ^message ^@:INVOLVING ^involving ^@:PATHNAME ^file},
        lisp_error)
enddefine;


define lconstant Checkr_non_wild_filename(item);
    if is_wild_pathname(item, nil) then
        file_error('Illegal operation on wild pathname', [^item], item)
    else
        checkr_filename(item)
    endif
enddefine;


define open(file, direction, type, if_exists, if_not_exists) -> stream;
    lvars lp = false, mode, dev, fullname, spec;
    dlocal pop_file_versions;

    if islogical_pathname(file) then
        file -> lp
    endif;
    Checkr_non_wild_filename(file) -> file;

    if direction == @:INPUT
    or direction == @:PROBE then
        0
    elseif direction == @:OUTPUT then
        1
    elseif direction == @:IO then
        2
    else
        file_error('Cannot open stream with direction ~S', [^direction], file)
    endif -> mode;

    sysopen(file, mode, false) -> dev;

    if dev and mode fi_> 0 then
        device_full_name(dev) -> fullname;
        if if_exists == @:ERROR then
            sysclose(dev);
            file_error('Error opening file (already exists)',
                       [^fullname], file)
        elseif if_exists == @:NEW-VERSION
        or if_exists == @:RENAME then
            sysclose(dev);
            min(2, pop_file_versions or 2) -> pop_file_versions;
            syscreate(file, mode, false) -> dev
        elseif if_exists == @:SUPERSEDE
        or if_exists == @:RENAME-AND-DELETE then
            sysclose(dev);
            1 -> pop_file_versions;
            if sysdelete(fullname) then
                syscreate(file, mode, false) -> dev
            else
                file_error('Error deleting file' <> sysiomessage(),
                           [^fullname], file)
            endif
        elseif if_exists == @:APPEND then
            sysclose(dev);
            fast_frozval(1, discappend(file)) -> dev
        elseif if_exists == nil then
            sysclose(dev);
            false -> dev
        elseunless if_exists == @:OVERWRITE do
            sysclose(dev);
            lisp_error('Unrecognised IF-EXISTS option', [^if_exists])
        endif
    elseunless dev do
        if if_not_exists == @:ERROR then
            file_error('Error opening file' <> sysiomessage(), [^file], file)
        elseif if_not_exists == @:CREATE then
            syscreate(file, mode, false) -> dev
        elseunless if_not_exists == nil do
            lisp_error('Unrecognised IF-DOES-NOT-EXIST option',
                       [^if_not_exists])
        endif
    endif;
    unless dev do
        nil -> stream;
        return
    endunless;

    if type == @:DEFAULT then
        @CHARACTER -> type
    endif;
    unless (etype_->_spec(type) ->> spec) do
        sysclose(dev);
        lisp_error('Unrecognised stream :ELEMENT-TYPE option', [^type])
    endunless;
    if isinteger(spec) then
        mode /== 1 and bits_in(dev, spec);
        mode /== 0 and bits_out(dev, spec)
    else
        mode /== 1 and discin(dev);
        mode /== 0 and discout(dev)
    endif;
    make_stream() -> stream;
    if lp then
        lp -> device_logical_pathname(dev)
    endif;
    if direction == @:PROBE then
        sysclose(dev)
    endif
enddefine;


/* Finding, moving & deleting files */

define lconstant Probe_filename(item);
    lvars dev;
    if isstream(item) then
        streamdev(item) -> dev
    elseif (readable(Checkr_non_wild_filename(item)) ->> dev) then
        sysclose(dev)
    endif;
    dev and device_full_name(dev)
enddefine;


define probe_file(item) -> item;
    if (Probe_filename(item) ->> item) then
        pathname(item) -> item
    endif
enddefine;


define truename(item) -> path;
    unless (probe_file(item) ->> path) do
        file_error('File does not exist', [^item], item)
    endunless
enddefine;


define rename_file(old, new);
    if isstream(old) or isstream(new) then
        lisp_error('Cannot rename from/to a stream', [^old ^new])
    endif;
    truename(old) -> old;
    sys_file_move(checkr_filename(old), Checkr_non_wild_filename(new));
    new;
    old;
    truename(new)
enddefine;


define delete_file(file);
    lvars realfile;
    if (Probe_filename(file) ->> realfile) then
        ;;; check null version on VMS ?
        if sysdelete(realfile) then
            if isstream(file) then
                close_stream(file) ->
            endif;
            realfile
        else
            file_error('Can\'t delete file' <> sysiomessage(),
                       [^realfile], file)
        endif
    else
        warn('File does not exist', [^file]) ->;
        file
    endif
enddefine;


/* File status information */

define file_position(stream, pos);
    lvars dev;
    streamdev(stream) -> dev;
    if pop_true(pos) then
        sysseek(dev, pos, 0, true)
    elseif isclosed(dev) then
        nil
    else
        sysseek(dev, 0, 1, true)
    endif
enddefine;


define lconstant Stat_info(item, num);
    lconstant stat = writeable {0 0 0 0};
    if isstream(item) then
        device_full_name(streamdev(item))
    else
        Checkr_non_wild_filename(item)
    endif -> item;
    if sys_file_stat(item, stat) then
        fast_subscrv(num, stat)
    else
        nil
    endif
enddefine;


define file_write_date(item) -> item;
    if (Stat_info(item, 2) ->> item) /== nil then
        ;;; VMS returns string, not number
        item + _1900_1970 -> item
    endif
enddefine;


define file_author(item) -> item;
    if (Stat_info(item, 4) ->> item) /== nil then
#_IF VMS
        item sys_>< nullstring -> item
#_ELSE
        sysgetpasswdentry(item, #_< {1} >_#) or nil -> item
#_ENDIF
    endif
enddefine;


define file_length() with_nargs 1;
    ;;; flush if open stream?
    Stat_info(1)
enddefine;


define file_string_length(stream, item);
    streamdev(stream) ->;
    if isstring(item) then
        fast_vector_length(item)
    elseif is_string_array(item) then
        array_used_size(item)
    elseif ischaracter(item) then
        1
    else
        type_error(item, [^@OR ^@STRING ^@CHARACTER])
    endif
enddefine;


define directory(args);
    lvars item;
    car(args) -> args;
    if args == nil then
        current_directory
#_IF not(VMS)
        dir_>< '/'
#_ENDIF
    else
        checkr_filename(args)
    endif -> args;
    [%  for item from_repeater
                  valof("sys_file_match")(args, nullstring, false, false)
        do
            pathname(item)
        endfor
    %]
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Williams, Aug  8 1995
        Removed redundant lvar declarations.
--- John Williams, May  1 1995
        rename_file no longer calls merge_pathnames (now done in the Lisp
        fucntion RENAME-FILE).
--- John Williams, Apr 12 1995
        Changes for Steele 1990 Ch 23.
--- John Williams, Apr  3 1995
        Added wild_pathname_p and file_string_length.
        Various procedures now insist on non-wild pathnames.
        file-error's now signalled.
--- John Williams, Jul 22 1993
        Made Stat_info use a writeable vector.
--- John Williams, May 27 1988
        -rename_file- uses -sys_file_move-.
 */
