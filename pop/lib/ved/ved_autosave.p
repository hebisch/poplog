/* --- Copyright University of Birmingham 2001. All rights reserved. ------
 > File:            $poplocal/local/auto/ved_autosave.p
 > Purpose:         Set VED to save all changed files every N minutes
 > Author:          Aaron Sloman, Nov 19 1995   (see revisions)
 >                  (based on Poplog version)
 > Documentation:   HELP * VED_AUTOSAVE
 > Related Files:
 */

;;; Based on the system version
/* --- Copyright University of Sussex 1992. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_autosave.p
 > Purpose:         Set VED to save all changed files every N minutes
 > Author:          Aaron Sloman, June 1991 (see revisions)
 > Documentation:   HELP * VED_AUTOSAVE
 > Related Files:
 */


;;; The use of "_" instead of "-" for VMS is a temporary solution.

compile_mode:pop11 +strict;

section;

global vars

vedautosave_min_write,          ;;; Write files only if they have at least
                                ;;; this number of changes

vedautosave_preserve,           ;;; Preserve the version from session start
                                ;;; using vedautosave_done as a record

vedautosave_max,                ;;; maximum value for pop_file_versions

vedautosave_min_preserve,       ;;; Preserve as above only if there are at
                                ;;; least this number of lines in the file

vedautosave_wiggletimes,        ;;; use to replace vedwiggletimes if
                                ;;; more or less wiggling is needed.

vedautosaving = true,           ;;; prevent autosaving if false

veddoing_autosave = false,      ;;; made true during autosaving
;

;;; Set defaults
unless isinteger(vedautosave_min_write) then
    0 -> vedautosave_min_write      ;;; write files with more changes than this
endunless;

if isundef(vedautosave_preserve) then
    true -> vedautosave_preserve
endif;

unless isinteger(vedautosave_max) then
    ;;; up to this number of backups per file
    3 -> vedautosave_max
endunless;

unless isinteger(vedautosave_min_preserve) then
    1 -> vedautosave_min_preserve
endunless;

unless isinteger(vedautosave_wiggletimes) then
    15 -> vedautosave_wiggletimes;  ;;; OK as default?
endunless;

lvars
    ;;;  A property to record for each file whether it has previously
    ;;;  had an extra back_up in this session. Use newmapping so that
    ;;;  strings are recognized by contents
    vedautosave_done = newmapping([], 16, false,false),

    autosave_mins = 5,          ;;; number of minutes between invocations

    last_time_run = 0,          ;;; time last interrupt procedure run

    in_a_read = false,          ;;; set true if inside charin

    autosaving_now = false,     ;;; set true when running interrupt
;

define lconstant seconds_left() -> secs;
    ;;; Compute seconds left till next interrupt
    lvars secs;
    autosave_mins * 60 - (sys_real_time() - last_time_run) -> secs
enddefine;


#_IF hd(sys_os_type) =="unix"

define lconstant backup_string(int) -> string;
    ;;; prepare backup string. Unix only
    lvars int,string;
    lconstant strings = {'-' '--' '---' '----' '-----' '------' '-------'};
    if fi_check(int,0,false) fi_< 8 then
        fast_subscrv(int, strings)
    else
        consstring(
            #| repeat int times `-` endrepeat |#)
    endif -> string
enddefine;

#_ELSE

define lconstant backup_string(int) -> string;
    ;;; prepare backup string. Underscores for VMS
    lvars int,string;
    lconstant strings = {'_' '__' '___' '____' '_____' '______' '_______'};
    if fi_check(int,0,false) fi_< 8 then
        fast_subscrv(int, strings)
    else
        consstring(
            #| repeat int times `_` endrepeat |#)
    endif -> string
enddefine;

#_ENDIF

define lconstant copy_from_disk(newfile);
    ;;; Make backup copy of current file  to newfile if original
    ;;; exists on disk if not, write it first

    lvars newfile, old_versions = pop_file_versions;

    unless sys_file_exists(vedpathname) then
        ved_w1()    ;;; does nothing if nothing has changed
    endunless;

    ;;; Save the current version, with its creation date, as a backup
    ;;; but as this is already a backup, reduce total number of backups to be
    ;;; created by this process.

    if pop_file_versions < vedautosave_max then 
        vedautosave_max - pop_file_versions + 1 -> pop_file_versions
    endif;

    ;;; Move the file to backup. This will follow links from vedpathname
    sys_file_move(vedpathname, newfile, 2:11);

    old_versions -> pop_file_versions;

    ;;; Copy it back. Works with symbolic links
    sys_file_copy(newfile, vedpathname);
enddefine;


define lconstant do_autosave_backup();
    ;;; Create extra backup for current file first time it is autosaved
    ;;; pop_file_versions is dlocal in vedautosave_write

    lvars num = pop_file_versions;

    unless in_a_read then
        vedputmessage('EXTRA BACKUP FOR ' sys_>< vedcurrent);

        dlocal
            vedwiggletimes = vedautosave_wiggletimes;
        ;;; to show message about BACKUP
        vedwiggle(0, 15);
    endunless;

    ;;; Decide on type of extra backup
    if isinteger(vedautosave_preserve) then
        ;;; Use it to set pop_file_versions for backups

        max(vedautosave_preserve, num)
                        -> pop_file_versions ;;; local to vedautosave_write
    elseif isstring(vedautosave_preserve) then
        ;;; URGH! Emacs users like '~' as backup suffix, but on Unix '~'
        ;;;    gets interpreted as user directory.
        ;;; So to rule that out prepend current directory before testing
        ;;; if it's a directory.
        copy_from_disk(
            if
                (vedautosave_preserve = '~'
                and sysisdirectory(current_directory dir_>< vedautosave_preserve))
            or sysisdirectory(vedautosave_preserve)
            then
                ;;; copy to named directory
                vedautosave_preserve dir_>< sys_fname_namev(vedpathname)
            else
                ;;; Make a copy with the string appended
                (vedpathname sys_>< vedautosave_preserve)
            endif)
    elseif isprocedure(vedautosave_preserve) then
        ;;; Run user defined procedure to do the saving
        vedautosave_preserve(vedpathname)
    else
        ;;; Default: simply copy to file with extra backup suffixes
        copy_from_disk(vedpathname sys_>< backup_string(num))
    endif
enddefine;


define global vars vedautosave_write();
    ;;; Check if any files need to be saved, and take appropriate action
    ;;; This is user definable. It is run by the interrupt procedure

    dlocal vedautosaving;

    ;;; don't do it if vedautosaving is false
    returnunless(vedautosaving);

    ;;; make sure there are no autosaves inside this procedure
    false -> vedautosaving;

    dlocal veddoing_autosave;   ;;; set true below

    lvars belldone = false;

    define lconstant check_and_save_file();
        ;;; called by vedappfiles
        dlocal vedversions, pop_file_versions;
        if vedversions then
            if pop_file_versions then
                max(vedversions, pop_file_versions)
            else
                vedversions
            endif
        else
            pop_file_versions
        endif  -> pop_file_versions;

        false -> vedversions;       ;;; ensure that pop_file_versions is used

        if vedwriteable and vedchanged and vvedbuffersize fi_> 0
        and vedchanged > vedautosave_min_write
        then
            ;;; Ring bell ONCE if there's anything to write
/*
    ;;; Removed. A.Sloman Nov 1995
            unless belldone then vedscreenbell();
            endunless;
*/

            true -> belldone;

            if vedautosave_preserve and not(vedautosave_done(vedpathname))
            and vvedbuffersize >= vedautosave_min_preserve
            then
                ;;; It's the first time for this file so extend backups
                do_autosave_backup();
                ;;; record that it has been done for this file
                true -> vedautosave_done(vedpathname);
            endif;
            ;;; do the autosave
            vedwriterange(1, max(vvedbuffersize,1), vedpathname);
            false -> vedchanged;
        endif;
    enddefine;

    ;;; suppress things that should not be run during autosave

    true -> veddoing_autosave;
    ;;; Check each file to see if there is anything to write
    vedappfiles(check_and_save_file);

    if belldone and not(in_a_read) then vedputmessage('AUTO SAVE done') endif;
enddefine;


define global vedset_autosave(set);
    ;;; The argument is true for setting the time, otherwise false
    lvars set;

    define lconstant check_autosave;
        ;;; Interrupt procedure run every vedautosave_minutes minutes

        lvars was_autosaving = autosaving_now;

        dlocal
            autosaving_now = true,
            in_a_read =
                vedprintingdone or iscaller(vedrestorescreen)
                or iscaller(charin);

        if vedinputwaiting() then
            ;;; typing ahead, or in a read, don't save ???
        elseif was_autosaving then
            ;;; don't save - in the middle of previous save
        elseif iscaller(vedreadfile) or iscaller(vededit)
                or iscaller(vedopen)
        then
            ;;; don't save, about to start reading in a file
        elseif iscaller(vedpipein) then
            ;;; don't save - forking soon
        elseif seconds_left() <= 0.1 then
            ;;; run the save procedure
            vedautosave_write();
            vedsetcursor();
        endif;
        ;;; re-set timer
        vedset_autosave(true);
    enddefine;

    if set then
        true, sys_real_time(), round(autosave_mins * 60e6)
    else
        false, false, false
    endif -> (vedautosaving, last_time_run, sys_timer(check_autosave))

enddefine;


;;; Interrogate or set delay time for check_autosave

define global active vedautosave_minutes;
    autosave_mins
enddefine;

define updaterof active vedautosave_minutes;
    -> autosave_mins;
    vedset_autosave(true);
enddefine;


define global ved_autosave;
    ;;; For interactive setting of timer, or finding out remaining
    ;;; time. If argument is "off" turns timing off.
    lvars time, mins, secs, pos;

    dlocal pop_pr_places = 2;

    if vedargument = 'off' then
        vedset_autosave(false);
        vedputmessage('SAVING TURNED OFF');
        return();
    elseif vedargument = 'reset' then
        vedputmessage('RE-STARTING PER SESSION BACKUPS');
        clearproperty(vedautosave_done);
        return()
    elseif strnumber(vedargument) ->> time then
        ;;; reset timer
        time -> vedautosave_minutes
    elseunless last_time_run then
        vedputmessage('OFF');
        return()
    endif;

    round(seconds_left()) -> time;

    if time < 0 then
        -time -> time; false
    else
        true
    endif -> pos;

    time // 60 -> mins -> secs;

    vedputmessage(
        'Set to ' sys_>< vedautosave_minutes sys_>< ' Minutes. '
        sys_>< mins sys_>< ' minutes ' sys_>< secs sys_><
        if pos then ' seconds left.' else ' seconds LATE' endif);

enddefine;

sys_runtime_apply(
    procedure;
        ;;; Initialise things if running interactively
        if systrmdev(popdevin) then vedset_autosave(true) endif
    endprocedure);

endsection;

/* --- Revision History ---------------------------------------------------
--- Aaron Sloman, Dec  7 2001
    Made sure autosaving is not triggered while it is already happening.
--- Aaron Sloman, Jan 30 2000
    Introduced veddoing_autosave;
--- Aaron Sloman, Feb 14 1999
    Fixed cases where autosaving could lose recent changes.
--- Aaron Sloman, Dec 13 1995
    Introduced vedautosaving, so that system procedures can turn off
    autosaving temporarily.
--- Aaron Sloman, Nov 19 1995
    Introduced vedautosave_max, and changed the initial backup to ensure
    that up to that number of backups will be kept. Previously starting
    to edit a file again could lose recent backups.
    Removed call of vedscreenbell
--- Aaron Sloman, Jan  18 1994
    Introduced vedautosave_wiggletimes (after comment by Adrian Howard)
--- Aaron Sloman, Jan  13 1994
    Changed test for isundef(vedautosave_preserve) from unless to if!!
        Substantial changes to pop_file_versions and vedversions to ensure
        that a proper backup is done the first time for each file.
        Previously not enough hyphens were appended (on Unix) and the
        saved version got over written after a couple of autosaves.
    Also revised to work with "~" as suffix.
    Changed to use vedinputwaiting()
--- Aaron Sloman, May 22 1993
    Added test iscaller(vedpipein), to prevent saving in the middle of
    a fork.
--- John Gibson, Dec  1 1992
        Made initialisation be a runtime action
--- John Gibson, Mar  6 1992
        Removed code dealing with ved_on_status (not necessary, since
        vedappfiles takes care of all that, including locally setting
        each file off status).
--- Aaron Sloman, Jun 30 1991
    Added vedautosave_min_preserve
--- Aaron Sloman, Jun 13 1991
    Based on a "local" program originally called ved_auto_mins, then
    temporarily called ved_save_time.
*/
