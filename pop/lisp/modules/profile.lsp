#| --- Copyright University of Sussex 1996. All rights reserved. ----------
 | File:            C.all/lisp/modules/profile.lsp
 | Purpose:         Lisp interface to Pop-11 LIB * PROFILE.P
 | Author:          John Williams, Mar  4 1992 (see revisions)
 | Documentation:   HELP * PROFILE
 | Related Files:   LIB * PROFILE.P
 |#

(cl:provide :profile)

(cl:in-package :poplog)

(export '(profile
            *profile-exclude*
            *profile-excluded-packages*
            *profile-gc-trace*
            *profile-graphical*
            *profile-interval*
            *profile-lisp-only*
            *profile-output*
            *profile-show-max* ))

(pop11)


section $-lisp;

lisp_compile_mode;

uses profile_graphical;
false -> profile_graphical;

vars
    profile_excluded_packages   =   [],
    profile_lisp_only           =   true,
    profile_output              =   make_synonym_stream(@*TRACE-OUTPUT*),
    ;

vars procedure pop11_profile_apply;
if isundef(pop11_profile_apply) then
    profile_apply -> pop11_profile_apply
endif;


define global vars lisp_profile_include(pdr) -> name;
    f_name(pdr) -> name;
    if (profile_lisp_only and not(issymbol(name)))
    or (fast_member(name, profile_exclude))
    or (issymbol(name)
        and fast_member(symbol_package(name), profile_excluded_packages))
    then
        false -> name
    endif
enddefine;


define lisp_profile_print(cpu, gc, counts, _);
    lvars p, c, total = 0, list = [], argv, n = 0;
    dlocal print_circle = nil, print_pretty = nil;
    fastprocs +, >, >=;
    SET_CUCHAROUT;

    define lconstant Fmt(string, args);
        call_format_print(profile_output, string, args) ->
    enddefine;

    for p, c in_property counts do
        c + total -> total;
        insert({0 ^c ^p}, list, procedure(x, y); x(2) > y(2) endprocedure)
            -> list
    endfor;

    Fmt('~%Number of interrupts: ~D~%', {^total});

    Fmt('CPU time: ~,2,-2F seconds, GC time: ~,2,-2F seconds~2%', {^cpu ^gc});

    Fmt('%Time  Interrupts~%', []);

    for argv in list do
        quitif(n >= profile_show_max);
        argv(2) / total -> argv(1);
        Fmt('~5,1,2F  ~5D    ~S~%', argv);
        n + 1 -> n
    endfor
enddefine;


define lisp_profile_graph(cpu, gc, counts, _);

    define dlocal profile_graph_paint(p);
        if isword(p) then `\{5}\s` else `\{7}\s` endif
    enddefine;

    define dlocal pr(x);
        if cucharout == identfn then
            ;;; inside ><
            explode(if isstring(x) then x else @PRIN1-TO-STRING(x, 1) endif)
        else
            lisp_pr(x)
        endif
    enddefine;

    profile_graph(cpu, gc, counts, false)
enddefine;


define lisp_profile_report() with_nargs 4;
    if profile_graphical then
        lisp_profile_graph()
    else
        lisp_profile_print()
    endif
enddefine;


define global vars lisp_profile_apply() with_nargs 1;
    dlocal
        profile_include = lisp_profile_include,
        profile_report = lisp_profile_report,
        ;
    pop11_profile_apply()
enddefine;


define Profile(forms, nresults);
    sysPROCEDURE(false, 0);
    Progn(forms, nresults);
    sysENDPROCEDURE();
    sysPUSHQ();
    sysCALL("ident lisp_profile_apply")
enddefine;


lisp_export(Profile, @PROFILE, "special");

vars active (
    pf_gc_trace     =   boolean_variable(% ident profile_gc_trace %),
    pf_graphical    =   boolean_variable(% ident profile_graphical %),
    pf_lisp_only    =   boolean_variable(% ident profile_lisp_only %),
    );

lispsynonym(@*PROFILE-EXCLUDE*,             "profile_exclude");
lispsynonym(@*PROFILE-EXCLUDED-PACKAGES*,   "profile_excluded_packages");
lispsynonym(@*PROFILE-GRAPHICAL*,           "pf_graphical");
lispsynonym(@*PROFILE-GC-TRACE*,            "pf_gc_trace");
lispsynonym(@*PROFILE-INTERVAL*,            "profile_interval");
lispsynonym(@*PROFILE-LISP-ONLY*,           "pf_lisp_only");
lispsynonym(@*PROFILE-OUTPUT*,              "profile_output");
lispsynonym(@*PROFILE-SHOW-MAX*,            "profile_show_max");


endsection;


/* Back to Lisp for the revision history */

lisp


#| --- Revision History ---------------------------------------------------
--- John Williams, Apr 25 1996
        Added interface to LIB * PROFILE_GRAPHICAL.P
--- John Williams, Mar 22 1996
        Fixed for new LIB PROFILE (defines profile_report).
--- John Williams, Aug 23 1995
        Removed redundant lvar declarations.
--- John Williams, Sep 21 1994
        Replaced WID with new ident syntax.
 |#
