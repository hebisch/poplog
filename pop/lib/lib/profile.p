/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:           C.all/lib/lib/profile.p
 > Purpose:        profiling where the system is doing the most work?
 > Author:         Steve Hardy 1982 A.Sloman 1991 (see revisions)
 > Documentation:  HELP * PROFILE
 > Related Files:
 */
compile_mode:pop11 +strict;

section;

include ast;

vars
    profile_count_all = false,      ;;; whether to count all callers

    profile_interval = 10000,       ;;; interval for timer in usecs

    profile_exclude_list = [null],  ;;; names of procedures to be excluded

    profile_ignore_list = [],       ;;; names of procedures to be ignored

    profile_gc_trace = false,       ;;; whether to trace GC while profiling

    profile_show_max = 10,          ;;; maximum size of report

;

#_IF not(DEF POPC_COMPILING)
;;; preserve the old name
identof("profile_exclude_list") -> identof("profile_exclude");
#_ENDIF

    ;;; test whether a procedure should be included in the profile table
define vars profile_include(pdr) -> pdr;
    if pdprops(pdr) ->> pdr then
        if fast_member(pdr, profile_exclude_list) then false -> pdr endif
    endif;
enddefine;

    ;;; test whether a procedure activation should be ignored; argument
    ;;; is whatever was returned by profile_include, usually the pdrops
define vars profile_ignore(item);
    fast_member(item, profile_ignore_list);
enddefine;

    ;;; produce the standard report
define profile_print(cpu_time, gc_time, counts, active_counts);
    lvars save_pr = pr;
    dlocal pr = sys_syspr;

    ;;; compute total number of recorded interrupts
    lvars proc, count, total_count = 0;
    for proc, count in_property counts do
        total_count + count -> total_count;
    endfor;

    ;;; merge results from both tables into a list of vectors:
    ;;;     {^pdprops ^count ^active_count}
    lvars results = [%
        lvars proc, count;
        if active_counts then
            for proc, count in_property active_counts do
                ;;; everything in active_counts should also be in
                ;;; counts, but filtered through profile_include
                if profile_include(proc) ->> proc then
                    lvars props = isprocedure(proc) and pdprops(proc) or proc;
                    {^props ^(counts(proc) or 0) ^count};
                endif;
            endfor;
        else
            for proc, count in_property counts do
                lvars props = isprocedure(proc) and pdprops(proc) or proc;
                {^props ^count 0};
            endfor;
        endif;
    %];

    ;;; sort results by the appropriate count
    lvars (i, j) = if active_counts then (3,2) else (2,3) endif;
    syssort(results, false,
        procedure(x, y);
            x(i) > y(i) or x(i) = y(i) and x(j) > y(j);
        endprocedure
    ) -> results;

    ;;; take leading sequence for display
    if isintegral(profile_show_max) then
        lvars show_max = profile_show_max;
        [%  while results /== [] and show_max > 0 do
                dest(results) -> results;
                show_max - 1 -> show_max;
            endwhile;
        %] -> results;
    endif;

    ;;; compute cumulative sums when no active counts given
    unless active_counts then
        lvars result, sum = 0;
        for result in results do
            ;;; use field 3 to hold the cumulative value
            result(2) + sum ->> result(3) -> sum;
        endfor;
    endunless;

    ;;; print summary statistics
    dlocal pop_pr_places = (`0` << 16) || 2;
    printf('\nNumber of interrupts: %p\n', [^total_count]);
    printf('CPU time: %p seconds, GC time: %p seconds\n\n',
        [^(cpu_time/100.0) ^(gc_time/100.0)]);

    ;;; print individual results
    dlocal pop_pr_places = (`0` << 16) || 1, pop_pr_quotes = false;
    lvars cnt_field_width = max(length(total_count sys_>< ''), 5);
    ;;; headers
    sp(cnt_field_width+9);
    pr_field(active_counts and 'Active' or 'Cumulative', cnt_field_width+7,
        `-`, `-`);
    pr(newline);
    pr_field('Count\s\s%Time', cnt_field_width+7, ` `, false);
    pr_field('Count\s\s%Time', cnt_field_width+9, ` `, false);
    pr(newline);
    ;;; results
    lvars result;
    for result in results do
        lvars (proc, count, other_count) = explode(result);
        pr_field(count, cnt_field_width, ` `, false);
        pr_field((count*100.0)/total_count, 7, ` `, false);
        pr_field(other_count, cnt_field_width+2, ` `, false);
        pr_field((other_count*100.0)/total_count, 7, ` `, false);
        sp(2); save_pr(proc); pr(newline);
    endfor;
    pr(newline);
enddefine;

    ;;; produce customisable report
vars procedure profile_display; ;;; for old-style printing

define vars profile_report(cpu_time, gc_time, counts, active_counts);
    if isundef(profile_display) then
        ;;; new style printing
        profile_print(cpu_time, gc_time, counts, active_counts);
    else
        ;;; old style, kept for compatibility
        profile_display(cpu_time, counts);
    endif;
enddefine;

    ;;; the profiler
lvars profile_running = false;
define profile_apply(procedure action);

    unless isundef(profile_display) then
        ;;; old-style printing ignores active counts
        dlocal profile_count_all = false;
    endunless;
    ;;; safety checks for fast procedures
    unless islist(profile_exclude_list) then
        dlocal profile_exclude_list = [];
    endunless;
    unless islist(profile_ignore_list) then
        dlocal profile_ignore_list = [];
    endunless;

    ;;; counter for closest callers (leaf procedures)
    define lvars counts =
        newproperty([], 64, 0, "perm");
    enddefine;
    if profile_count_all then
        ;;; counter for all active callers
        define lvars active_counts =
            copy(counts);
        enddefine;
    endif;

    ;;; interrupt procedure to do the counting
    define profile_interrupt();
        returnunless(profile_running);
        ;;; record closest interesting caller
        lvars i = 1, pdr, count_active = profile_count_all;
        while (caller(i) ->> pdr) and pdr /== profile_apply do
            if profile_include(pdr) ->> pdr then
                if profile_ignore(pdr) then
                    false -> count_active;
                else
                    counts(pdr) fi_+ 1 -> counts(pdr);
                endif;
                quitloop;
            endif;
            i fi_+ 1 -> i;
        endwhile;
        if count_active then
            ;;; record all active procedures
            lvars counted = [];
            while (caller(i) ->> pdr) and pdr /== profile_apply do
                unless fast_lmember(pdr, counted) then
                    active_counts(pdr) fi_+ 1 -> active_counts(pdr);
                    conspair(pdr, counted) -> counted;
                endunless;
                i fi_+ 1 -> i;
            endwhile;
            sys_grbg_list(counted);
        endif;
        ;;; re-set timer
        profile_interval -> sys_timer(profile_interrupt, TIMER_VIRTUAL);
    enddefine;

    ;;; display closest caller after GC
    dlocal popgctrace = popgctrace or profile_gc_trace;
    if popgctrace then
        define dlocal pop_after_gc();
            lvars i = 1, pdr;
            while (caller(i) ->> pdr) and pdr /== profile_apply do
                if pdprops(pdr) ->> pdr then
                    printf(pdr, ';;; GC invoked by procedure %p\n\n');
                    quitloop;
                endif;
                i fi_+ 1 -> i;
            endwhile;
        enddefine;
    endif;

    ;;; run the action procedure with timed interrupts
    dlocal profile_running = true;
    profile_interval -> sys_timer(profile_interrupt, TIMER_VIRTUAL);
    lvars (cpu_time, gc_time) = (systime(), popgctime);
    action();
    (systime() - cpu_time, popgctime - gc_time) -> (cpu_time, gc_time);
    false ->> profile_running -> sys_timer(profile_interrupt);

    ;;; produce report
    profile_report(cpu_time, gc_time, counts,
        profile_count_all and active_counts);
enddefine;

    ;;; read a command and profile it
define macro profile;
    lvars action = [%
        dlocal popnewline = true;
        repeat
            lvars item = itemread();
            quitif(item == newline or item == termin);
            item;
        endrepeat;
    %];
    if action == [] then
        mishap(0, 'NO COMMAND GIVEN TO BE PROFILED');
    endif;
    [(procedure; ^^action endprocedure.profile_apply);].dl;
enddefine;

endsection;


/*  --- Revision History ---------------------------------------------------
--- John Williams, Mar 25 1996
        profile_print uses sys_syspr for everything except procedure names.
--- Robert Duncan, Mar 15 1996
        Changed to allow counting of all active callers.
--- John Williams, Jan 31 1996
        lvar count in procedure profile_interrupt now initialised to 3
        and not 4 (cf. BR davidy.97).  Robert Duncan will soon install
        an improved version.
--- John Williams, Mar 16 1992
        Fixed bug in -profile_apply- where system gets stuck in infinite
        loop if no procedure in the calling sequence satisfies
        -profile_include-
--- Aaron Sloman, Jun 24 1991
        Totally re-written, using sys_timer. Entirely new version of
        documentation in HELP PROFILE.
 */
