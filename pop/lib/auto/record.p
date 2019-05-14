/* --- Copyright University of Sussex 1989. All rights reserved. ----------
 > File:           C.all/lib/auto/record.p
 > Purpose:        for recording interaction with poplog in a file.
 > Author:         Aaron Sloman, Nov 1983 (perhaps) (see revisions)
 > Documentation:  HELP * RECORD
 */


section $-record => record, endrecord;

vars record_setpop record_file;

define record_stop;
    if isprocedure(poplogfile) then
        pr('\nEnd of recording -- back to non recorded interaction\n');
        poplogfile(termin);
        false -> poplogfile;
        if popsetpop == record_setpop then identfn -> popsetpop endif;
        `\n` -> poplastchar;
    endif;
enddefine;

global vars macro endrecord =
    [if poplogfile then nil -> proglist; endif; ^record_stop();];

define record_setpop();
    if poplogfile then
        record_setpop -> popsetpop;
        printf('\nSetpop -- Interaction is being recorded in %p\n',
                [^record_file]);
        subsystem_compile(charin, "pop11");
        record_stop();
    endif;
enddefine;

define start_record(filename);
lvars filename;
    if poplogfile then printf('\nAlready recording in %p\n',[^record_file])
    else
        if filename = nullstring then 'record.log' else filename endif -> record_file;
        discappend(record_file) -> poplogfile;
        record_setpop -> popsetpop;
        printf('\nInteraction is being recorded in %p\n',[^record_file]);
        subsystem_compile(cucharin, "pop11");
        record_stop();
    endif
enddefine;

define global macro record;
dlocal popnewline = true;
lvars filename;
    erase(pop11_try_nextreaditem("in"));
    sysfileok(rdstringto([; ^termin ^newline])) -> filename;
    dl([^start_record(^filename);]);
enddefine;

section_cancel(current_section);
endsection;



/*  --- Revision History ---------------------------------------------------
--- John Gibson, Aug 13 1989
        Replaced sys- procedure with pop11_ one
--- John Gibson, Feb 14 1988
        Replaced -vednullstring- with -nullstring-
--- Mark Rubinstein, Jun 18 1985 - altered to use REDISCOUT.  Previously it
    didn't append to the file on UNIX if pop_file_versions == 1.
 */
