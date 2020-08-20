/* --- Copyright University of Sussex 1991.  All rights reserved. ---------
 > File:           C.all/lib/flavours/flavesyntax.p
 > Purpose:        syntax for the flavours package
 > Author:         Mark Rubinstein, Apr 17 1986 (see revisions)
 > Documentation:  TEACH * FLAVOURS
 > Related Files:  LIB * FLAVOURS, $usepop/pop/lib/flavours/flavecore.p
 */


compile_mode :pop11 +varsch;

section;

;;; inform ved_tidy of the new syntax words;
[flavour defmethod] <> vedopeners -> vedopeners;
[endflavour enddefmethod] <> vedclosers -> vedclosers;

section $-flavour => nonmacro enddefmethod
    nonsyntax (flavour endflavour ivars divars <- ^ defmethod)
    ;

true ->> popdefineprocedure -> popdefineconstant;

;;; -- DECLARATIONS --------------------------------------------------------

lconstant procedure(
    syssysPUSH = sysPUSH,
    syssysCALL = sysCALL,
    );

applist([sysPUSH sysCALL flavour endflavour defmethod enddefmethod],
    sysunprotect);

global vars flavour_flavour;
global constant syntax endflavour = undef;
global vars macro enddefmethod = undef;

vars
        flavourname,
    ;

;;; --- DECLARE RECKLESS FAST PROCEDURES -----------------------------------

lconstant
    fast_f_livars = fast_back,
    fast_i_frec = fast_front,
    ;

;;; -- UNSCOPED VARIABLES USED IN SYNTAX PROCEDURES ------------------------

vars
    lexical_ivars = false,
    dynamic_ivars = false,
    default_values = false,
    ;

;;; --- MESSAGE SENDING SYNTAX ---------------------------------------------

;;; normal send message syntax
define global syntax 4 <-;
lvars obj mssge;
dlocal
        pop_expr_update,
        pop_new_lvar_list,  ;;; IR Jun 25 1991
    ;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    sysNEW_LVAR() -> obj;
    sysPOP(obj);
    readitem() -> mssge;
    false -> pop_expr_update;
    if pop11_try_nextitem("(") then
        pop11_comp_expr_seq_to(")") -> ;
    endif;
    sysPUSHQ(mssge);
    sysPUSH(obj);
    syssendmessage -> pop_expr_item; sysCALLQ -> pop_expr_inst;
enddefine;

;;; send self message syntax
define global vars syntax 4 ^;
lvars mssge okaylab = sysNEW_LABEL();
dlocal
        pop_expr_update
    ;
    pop_expr_inst(pop_expr_item); pop11_FLUSHED -> pop_expr_inst;
    readitem() -> mssge;
    false -> pop_expr_update;
    if pop11_try_nextitem("(") then
        pop11_comp_expr_seq_to(")") -> ;
    endif;
    sysPUSHQ(mssge);
    syssysPUSH("self");
    sysOR(okaylab);
    sysPUSHQ(1);
    sysPUSHQ('ATTEMPT TO SEND MESSAGE TO SELF OUTSIDE CONTEXT OF AN INSTANCE');
    sysCALLQ(mishap);
    sysLABEL(okaylab);
    sysPUSHS(false);            ;;; re-push self;
    sysCALLQ(fast_i_frec);
    sysCALLQ(f_runmethod);
    sysCALLS -> pop_expr_inst;
enddefine;

;;; --- SYNTAX UTILITIES ---------------------------------------------------

;;; reads vars declarations adding name value pairs to the list default_values.
;;; (The values must be computable at compile time - run time when this
;;; procedure is called).
define sys_flavour_read_vars(declare_pdr);
lvars item closer initlist lv procedure declare_pdr;
    until (readitem() ->> item) == ";" do
        nextif(item == ",");

        ;;; This is obviously nonesense IR 25/6/91
        ;;; sys_read_path(item, false, false) -> item;

        unless isword(item) then
            mishap(item, 1, 'IMPERMISSIBLE ITEM IN VARS STATEMENT');
        endunless;
        declare_pdr(item);
        if nextreaditem() == "=" then
            ;;; initialisation expression
            readitem() -> ;         ;;; remove the "="
            pop11_comp_expr_to([, ) ;]) -> closer;
            sysEXECUTE() -> lv;
            conspair(lv, item) :: default_values -> default_values;
            closer :: proglist -> proglist;
        endif;
    enduntil;
    item :: proglist -> proglist;
enddefine;

define constant cancelled_ivar =
    mishap(%
        1,
        'METHOD EXECUTED OUT OF CONTEXT OR REFERS TO CANCELLED IVAR'
    %);
enddefine;

;;; -- PUSH, POP, CALL and UCALL -------------------------------------------
;;; Inside the definition of a method, references to words that have been
;;; declared to be lexical variables are not simply pushed but treated as
;;; if they had been replaced by ivalof(self, word).  In fact the planting
;;; of the ivalof code can be, and is, optimised.

;;; a version of sysPUSH which which acts specially if the word to be pushed
;;; has been declared to be a lexical instance variable.
define constant procedure fpush(w, action, syspdr);
lvars w procedure (action syspdr) okay_label;
    if getindex(w, lexical_ivars) then
        sysPUSHQ(w);                            ;;; plant in-line ivalof code
        syssysPUSH(current_lexical_names_word);     ;;; ordered list of lexical
        sysCALLQ(getindex);                         ;;; variable names
        sysOR(sysNEW_LABEL() ->> okay_label);   ;;; the ivar might be canceled
        sysPUSHQ(w);                    ;;; (-getindex- will return false) in
        sysCALLQ(cancelled_ivar);       ;;; which case complain.
        sysLABEL(okay_label);
        syssysPUSH(current_lexical_values_word);    ;;; values of same for self
        action(fast_subscrv);       ;;; action will be sysCALLQ or sysUCALLQ
    else
        syspdr(w);
    endif;
enddefine;

;;; instance variables can be in the call position.  Trap for them too.
define constant procedure fcall(w, action, syspdr);
lvars w procedure (action syspdr) okay_label;
    if getindex(w, lexical_ivars) then
        sysPUSHQ(w);        ;;; plant in-line ivalof code
        syssysPUSH(current_lexical_names_word);
        sysCALLQ(getindex);
        sysOR(sysNEW_LABEL() ->> okay_label);   ;;; if ivar has been canceled
        sysPUSHQ(w);                    ;;; (-getindex- will return false in
        sysCALLQ(cancelled_ivar);       ;;; which case complain.
        sysLABEL(okay_label);
        syssysPUSH(current_lexical_values_word);
        sysCALLQ(fast_subscrv);
        action();               ;;; action will be sysCALLS or sysUCALLS
    else
        syspdr(w);
    endif;
enddefine;

;;; --- FLAVOUR SYNTAX -----------------------------------------------------

;;; sysPUSH and sysCALL are altered when compiling methods so that pushs of
;;; lexically scoped instance variables are altered into acessess of the
;;; vector of values (set at run time).  Similarly the updater and the Calling
;;; of procedure values.
define constant flavoursysPUSH
    = fpush(% sysCALLQ, syssysPUSH %)
enddefine;

define updaterof flavoursysPUSH
    = fpush(% sysUCALLQ, updater(syssysPUSH) %)
enddefine;

define constant flavoursysCALL
    = fcall(% sysCALLS(% false %), syssysCALL %)
enddefine;

define updaterof flavoursysCALL
    = fcall(% sysUCALLS(% false %), updater(syssysCALL) %)
enddefine;

define constant sysLIVARS(word);
lvars word;
    if getindex(word, dynamic_ivars) then
        newivartype(word, "divars", flavourname);
    elseunless getindex(word, lexical_ivars) do
        Declare(word);
        addwtov(lexical_ivars, word) -> lexical_ivars;
    endif;
enddefine;

define constant sysDIVARS(word);
lvars word;
    if getindex(word, lexical_ivars) then
        newivartype(word, "ivars", flavourname);
    elseunless getindex(word, dynamic_ivars) do
        Declare(word);
        addwtov(dynamic_ivars, word) -> dynamic_ivars;
    endif;
enddefine;

define lconstant mishap_noflav(type);
    lvars   type;
    mishap(0, 'USING ' >< type >< ' OUTSIDE FLAVOUR DEFINITION');
enddefine;

define global vars syntax ivars
    = mishap_noflav(% 'ivars' %);
enddefine;

define global vars syntax divars
    = mishap_noflav(% 'divars' %);
enddefine;

define global vars syntax defmethod
    = mishap_noflav(% 'defmethod' %);
enddefine;

;;; within the scope of 'flavour..endflavour' popautolist and popuseslist are
;;; locally redefined so that is is just a list with a closure of this
;;; procedure with the list in it.  This ensures that libraries are loaded
;;; without being treated as part of method definitions (lexical ivars pushed
;;; funnily).
define constant safe_syslibcompile(w, list);
lvars w list;
dlocal
        sysPUSH         = syssysPUSH,
        sysCALL         = syssysCALL,
        lexical_ivars   = false,
        dynamic_ivars   = false,
        default_values  = false,
    ;
    syslibcompile(w, list);
enddefine;

define global constant syntax flavour;
lvars   super supername supers = [], novanilla = false, frecord = false,
        metaf = false, methods = [], before = [], after = [], frname,
        methodname
    ;
dlocal
        flavourname,
        sysPUSH         = flavoursysPUSH,
        sysCALL         = flavoursysCALL,
        popautolist     = [% safe_syslibcompile(% popautolist %) %],
        popuseslist     = [% safe_syslibcompile(% popuseslist %) %],
        dynamic_ivars   = {},
        lexical_ivars   = {},
        default_values  = [],
        nonmac enddefmethod = "endprocedure",
    ;

    define dlocal ivars;  sys_flavour_read_vars(sysLIVARS) enddefine;
    define dlocal divars; sys_flavour_read_vars(sysDIVARS) enddefine;

    define lconstant declare_record(frecord);
    lvars frecord;
        appdata(f_livars(frecord), sysLIVARS);
        appdata(f_divars(frecord), sysDIVARS);
    enddefine;

    define lconstant ivarmishap(isdivar);
    lvars isdivar;
        mishap("defmethod", methodname, 2, if isdivar then
                    'ILLEGAL DECLARATION OF DIVAR'
                  else
                    'ILLEGAL DECLARATION OF IVAR'
                  endif);
    enddefine;

    define dlocal defmethod;
    lvars methodpdr mtype = "methods", upd = false;
    dlocal %nonsyntax ivars%    = ivarmishap(%false%),
           %nonsyntax divars%   = ivarmishap(%true%),
        ;
        readitem() -> methodname;
        if lmember(methodname, [after before]) then
            methodname -> mtype; readitem() -> methodname;
        endif;
        if methodname == "updaterof" then
            readitem() -> methodname; true -> upd;
        endif;
        "procedure" :: proglist -> proglist;
        pop11_comp_expr();
        sysEXECUTE() -> methodpdr;
        unless pdprops(methodpdr) do
            flavourname <> "<-" <> methodname -> pdprops(methodpdr);
        endunless;
        consmethodrecord(methodpdr, methodname) -> methodpdr;
        if upd then
            consmethodrecord(methodpdr, "updater") -> methodpdr;
        endif;
        if mtype == "methods" then
            methodpdr :: methods -> methods;
        elseif mtype == "before" then
            methodpdr :: before -> before;
        else
            methodpdr :: after -> after;
        endif;
    enddefine;

    define lconstant declare_super(s);
    lvars s;
        unless fast_lmember(s, supers) do
            conspair(s, supers) -> supers;
        endunless;
        declare_record(s);
    enddefine;

    ;;; read the name and declare any previously invokation.
    readitem() -> flavourname;
    if (flavourrecord_of(flavourname) ->> frecord) then
        declare_record(frecord);
        I_frecord(f_myinstance(frecord)) -> metaf;
    else
        ;;; declare it anyway.
        flavourname <> "_flavour" -> frname;
        sysVARS(frname, 0);
        sysGLOBAL(frname);
    endif;
    if pop11_try_nextreaditem("a") then
        getflavourrecord_of(readitem()) -> metaf
    elseunless metaf then
        I_frecord(flavour_flavour) -> metaf;
    endif;
    pop11_try_nextreaditem("novanilla") -> novanilla;
    if pop11_try_nextreaditem("isa") then
        until (readitem() ->> supername) == ";" do
            unless getflavourrecord_of(supername) ->> super do
                mishap('name of flavour needed', [^supername]);
            endunless;
            declare_super(super);
        enduntil;
    else
        pop11_need_nextitem(";") ->;
    endif;
    unless novanilla do
        declare_super(getflavourrecord_of("vanilla"))
    endunless;
    ;;; now compile until endflavour;
    sysLBLOCK(true);
        pop11_comp_stmnt_seq_to("endflavour") ->;
        sysEXECUTE();
    sysENDLBLOCK();

    chain(flavourname, rev(supers), lexical_ivars, dynamic_ivars,
        methods, before, after,
        metaf, default_values, sysflavour <> erase);
enddefine;

;;; tidy up
applist([sysPUSH sysCALL flavour endflavour defmethod enddefmethod],
    sysprotect);

false ->> popdefineconstant -> popdefineprocedure;

endsection;     ;;; $-flavour;
endsection;



/* --- Revision History ---------------------------------------------------
--- Andreas Schoter, Sep  9 1991
    Changed occurrances of -popliblist- to -popautolist-
--- Ian Rogers, Jul 12 1991
        Moved placing of sysLBLOCK in -flavour- (to just before the
        -pop11_comp_stmnt_seq_to("endflavour")- )
--- Ian Rogers, Jun 26 1991
        Much tidying
        No longer tries to autoload "a", "isa" and the names of the
    superclasses.
        No longer tries to read the full section path of -ivars- and
    -divars- (ie. no autoloading).
        "<-", "^" and -defmethod- now use -readitem- instead of
    -itemread- to get the message/method name (no autoloading).
        dlocal-ised -pop_new_lvar_list- to "<-"
--- Andreas Schoter, Jun 24 1991
        Wrapped flavour syntax in calls to sysLBLOCK(true) and
        sysENDLBLOCK() to ensure that lexical scopes are properly created
--- John Gibson, Aug 13 1989
        Replaced remaining sys- compiler procedures with pop11_ ones.
--- James Goodlet, Jul 12 1989 - -sysxc omp- replaced by new equivalent
        -pop11_comp_expr-.
--- Ian Rogers, Jul 26 1988 - added a patch to call mishap if ivars or
        divars declarations are attempted inside a method definition.
--- Ian Rogers, Jun 20 1988 - made flavour records global as the
        documentation says they are.
*/
