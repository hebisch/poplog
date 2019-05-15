/* --- Copyright University of Sussex 1993. All rights reserved. ----------
 > File:            C.all/plog/src/builtins.ph
 > Purpose:         Prolog: declarations for built-in predicates
 > Author:          Robert John Duncan, Jun 21 1993
 > Documentation:
 > Related Files:
 */

section;

;;; System (constant) predicates
global constant procedure (
    abolish\/2,
    abort\/0,
    arg\/3,
    assert\/1,
    asserta\/1,
    assertz\/1,
    atom\/1,
    atomic\/1,
    bagof\/3,
    break\/0,
    call\/1,
    call\/2,
    call\/3,
    clause\/2,
    clauses\/1,
    close\/1,
    compare\/3,
    compile\/1,
    compile\/2,
    consult\/1,
    current_atom\/1,
    current_functor\/2,
    current_module\/1,
    current_op\/3,
    current_predicate\/2,
    current_spy_action\/1,
    current_spy_action\/2,
    debugging\/0,
    display\/1,
    dynamic\/1,
    endmodule\/0,
    endmodule\/1,
    error\/2,
    expand_term\/2,
    export\/1,
    fail\/0,
    fast_bagof\/3,
    fast_setof\/3,
    findall\/3,
    float\/1,
    format\/1,
    format\/2,
    functor\/3,
    get\/1,
    get0\/1,
    global\/1,
    halt\/0,
    import\/1,
    include\/1,
    incore\/1,
    integer\/1,
    is\/2,
    keysort\/2,
    leash\/0,
    leash\/1,
    length\/2,
    library\/1,
    listing\/0,
    listing\/1,
    module\/0,
    module\/1,
    name\/2,
    nl\/0,
    nl\/1,
    no_clauses\/1,
    nodebug\/0,
    nonvar\/1,
    nospy\/0,
    nospy\/1,
    not\/1,
    numbervars\/3,
    op\/3,
    phrase\/2,
    phrase\/3,
    predicate_info\/2,
    print\/1,
    printq\/1,
    prolog_abolish\/2,
    prolog_abolish_command\/1,
    prolog_area_lim\/1,
    prolog_area_size\/1,
    prolog_callstack_lim\/1,
    prolog_clauses\/0,
    prolog_current_language\/1,
    prolog_error_handling\/1,
    prolog_eval\/1,
    prolog_eval\/2,
    prolog_evaltrue\/1,
    prolog_gcratio\/1,
    prolog_gctrace\/1,
    prolog_language\/1,
    prolog_lock_code\/0,
    prolog_memlim\/1,
    prolog_memused\/1,
    prolog_module\/1,
    prolog_no_clauses\/0,
    prolog_no_clauses\/1,
    prolog_restart\/0,
    prolog_setq\/2,
    prolog_syspredicates\/1,
    prolog_syserror\/2,
    prolog_system_predicate\/2,
    prolog_unlock_code\/0,
    prolog_val\/2,
    prompt\/2,
    put\/1,
    quietspy\/1,
    read\/1,
    read\/2,
    reconsult\/1,
    reinitialise\/0,
    rename\/2,
    repeat\/0,
    restore\/1,
    retract\/1,
    retractall\/1,
    save\/1,
    save\/2,
    see\/1,
    seeing\/1,
    seen\/0,
    setof\/3,
    skip\/1,
    sort\/2,
    spy\/0,
    spy\/1,
    spy_action\/1,
    spy_action\/2,
    static\/1,
    statistics\/0,
    statistics\/2,
    system_predicate\/1,
    tab\/0,
    tab\/1,
    tell\/1,
    telling\/1,
    told\/0,
    trimcore\/0,
    true\/0,
    ttyflush\/0,
    ttyget\/1,
    ttyget0\/1,
    ttynl\/0,
    ttynl\/1,
    ttyput\/1,
    ttyskip\/1,
    ttytab\/0,
    ttytab\/1,
    unleash\/0,
    unleash\/1,
    user_predicate\/1,
    var\/1,
    version\/0,
    version\/1,
    vread\/2,
    vwrite\/2,
    write\/1,
    write\/2,
    writeq\/1,

    \,\/2,          ;;; ,/2
    \;\/2,          ;;; ;/2
    \!\/0,          ;;; !/0
    \\\+\/1,        ;;; \+/1
    \=\/2,          ;;; =/2
    \\\=\/2,        ;;; \=/2
    \=\=\/2,        ;;; ==/2
    \\\=\=\/2,      ;;; \==/2
    \=\.\.\/2,      ;;; =../2
    \=\:\=\/2,      ;;; =:=/2
    \=\\\=\/2,      ;;; =\=/2
    \<\/2,          ;;; </2
    \>\/2,          ;;; >/2
    \=\<\/2,        ;;; =</2
    \>\=\/2,        ;;; >=/2
    \@\<\/2,        ;;; @</2
    \@\>\/2,        ;;; @>/2
    \@\=\<\/2,      ;;; @=</2
    \@\>\=\/2,      ;;; @>=/2
    \.\/2,          ;;; ./2
    \[\]\/0,        ;;; []/0
    \$\-\/1,        ;;; $-/1
    \$\-\/2,        ;;; $-/2
    \-\>\/2,        ;;; ->/2
    \(94)\/2,       ;;; ^/2
);

;;; User (vars) predicates
global vars procedure (
    portray\/1,
    prolog_error\/2,
    spy_action_hook\/3,
    term_expansion\/2,
);

;;; Special predicates (have to be exported to be called)
global constant procedure (
    prolog_grexpand\/4,
    prolog_toplevel\/2,
    prolog_directive\/1,
    prolog_break\/1,
);

endsection;
