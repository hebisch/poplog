/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           C.all/lib/flavours/flavecore.p
 > Purpose:        Core flavours system without any syntax.
 > Author:         Mark Rubinstein, Apr 17 1986 (see revisions)
 > Documentation:  TEACH * FLAVOURS
 > Related Files:  LIB * FLAVOURS, $usepop/pop/lib/flavours/flavesyntax.p
 */

section;

;;; this recordclass is in top section as it may be used by users
recordclass constant methodrecord m_methodpdr m_methodname;

section $-flavour => flavour_of flavour_record ivalof message
    metaflavour_flavour myflavour name self sysflavour syssendmessage
    ;

true ->> popdefineconstant -> popdefineprocedure;

;;; --- DECLARATIONS -------------------------------------------------------
recordclass constant flavourrecord
    f_messagereceiver   ;;; procedure for handling messages
    f_livars            ;;; cached vector of lexical instance variable names
    f_divars            ;;; cached vector of dynamic instance variable names
    f_name              ;;; name
    f_methods           ;;; list of methodrecords (primary methods)
    f_before            ;;; list of methodrecords (before daemons)
    f_after             ;;; list of methodrecords (after daemons)
    f_supers            ;;; list of direct ascendants
    f_subclasses        ;;; list of direct descendants
    f_myinstance        ;;; instance of flavour relating to flavour
    f_precedencelist    ;;; precedence list of super classes
    f_standardpreclist  ;;; precedence list made by standard algorithm
    f_setself           ;;; procedure for setting environment
    f_saveself          ;;; procedure for saving environment
    f_runmethod         ;;; procedure for find and running appropriate method
    f_defaultvalues     ;;; list of default-values|name pairs for variables
    ;

recordclass constant flavour_instance
    i_frec              ;;; flavour record
    i_dinstvals         ;;; vector of (dynamic) values
    i_linstvals         ;;; vector of (lexical) values
    ;

;;; two forward declarations.  Perhaps -syssendmessage- should be variable
global constant procedure(
    syssendmessage
    );

lconstant
    selfentry = 'METHOD HAS BEEN RUN OUT OF CONTEXT',
    protectedivars = [self message myflavour],
    ;

constant
    current_lexical_values_word = consword(`M, 0, `A, 0, `R, 0, `K, 7),
    current_lexical_names_word = consword(`M, 1, `A, 1, `R, 1, `K, 7),
procedure(
    Declare = sysVARS(% 0 %),
    getdefaultvalue,
    set_needscompiling,
    Onlyupd = mishap(%1, 'ONLY UPDATER OF METHOD DEFINED'%),
    );

applist(protectedivars, sysunprotect);

global vars
    flavour_record,
    message = false,
    metaflavour_flavour = false,
    myflavour = false,
    name,
    self = false,
    ;

;;; declare the current_lexical_values_word and current_lexical_names_word
sysVARS(current_lexical_values_word, 0);
sysGLOBAL(current_lexical_values_word);
section_export(current_lexical_values_word);

sysVARS(current_lexical_names_word, 0);
sysGLOBAL(current_lexical_names_word);
section_export(current_lexical_names_word);

;;; initialise their values
consvector(repeat 20 times selfentry endrepeat, 20)
    -> valof(current_lexical_values_word);
consvector(0) -> valof(current_lexical_names_word);

;;; --- DECLARE RECKLESS FAST PROCEDURES -----------------------------------

lconstant
    fast_m_methodpdr = fast_front,
    fast_m_methodname = fast_back,
    fast_destmethodrecord = fast_destpair,
    fast_f_messagereceiver = fast_front,
    fast_f_livars = fast_back,
    fast_i_frec = fast_front,
    fast_i_dinstvals = fast_back,
    fast_i_linstvals = i_linstvals,     ;;; cannot be messed with yet
    ;

;;; --- UTILITY PROCEDURES -------------------------------------------------

;;; given a list of pair structures (this works for methodrecords too) return
;;; one whose back is == to the -tag- given.
;;; NOTE: this procedure must use the fast_back procedure.
define gettaggedpair(tag, list);
lvars list entry tag;
    while ispair(list) do
        if fast_back(fast_destpair(list) -> list ->> entry) == tag then
            return(entry)
        endif;
    endwhile;
    false           ;;; not found
enddefine;

;;; find out if the base procedure is just a place holder for an updater.
define notdummypdr(pdr);
lvars procedure pdr;
    if isclosure(pdr) then
        pdpart(pdr) /== Onlyupd
    else
        true
    endif;
enddefine;

;;; gets all the methods that can be called from the precedence list of flavour
;;; records that pass the -isokmethod-
define getallokmethods(plist, isokmethod);
lvars plist flave method mnames = [], procedure(isokmethod);
    [%  fast_for flave in plist do
            fast_for method in f_methods(flave) do
                if isokmethod(fast_m_methodpdr(method)) then
                    unless fast_lmember(fast_m_methodname(method), mnames) do
                        conspair(fast_m_methodname(method), mnames) -> mnames;
                        method;
                    endunless
                endif
            endfast_for;
        endfast_for
    %]
enddefine;

constant procedure (
    getallmethods = getallokmethods(% notdummypdr %),
    getallupdmethods = getallokmethods(% updater %),
    );

;;; used for updating or adding new method (or daemon) procedures.
;;; normaly entries are of the form [<pdr>|<name>], updaters are of the form
;;; [[<pdr>|<name>] | updater]
define linkmethods(input, previous) -> previous;
lvars input previous each temp name;
    fast_for each in input do
        if (fast_m_methodname(each) ->> name) == "updater" then
            fast_m_methodname(fast_m_methodpdr(each) ->> each) -> name;
            if gettaggedpair(name, previous) ->> temp then
                ;;; update the existing record -- copy if necessary
                unless isinheap(fast_m_methodpdr(temp)) do
                    copy(fast_m_methodpdr(temp)) -> fast_m_methodpdr(temp)
                endunless;
                fast_m_methodpdr(each) -> updater(fast_m_methodpdr(temp))
            else                                            ;;; else create
                Onlyupd(% name %) -> temp;
                fast_m_methodpdr(each) -> updater(temp);
                temp -> fast_m_methodpdr(each);
                conspair(each, previous) -> previous
            endif
        elseif gettaggedpair(name, previous) ->> temp then
            ;;; update the existing record -- copy if necessary
            unless isinheap(fast_m_methodpdr(temp)) do
                copy(fast_m_methodpdr(temp)) -> fast_m_methodpdr(temp)
            endunless;
            updater(fast_m_methodpdr(temp)) -> updater(fast_m_methodpdr(each));
            fast_m_methodpdr(each) -> fast_m_methodpdr(temp)
        else
            conspair(each, previous) -> previous;
        endif
    endfast_for
enddefine;

;;; link a new default value|name pair into a list.  Must copy, but can share
;;; tails which are unchanged.
define linkdefault(pair, list);
lvars pair list;
    if list == nil then
        conspair(pair, nil)
    elseif fast_back(fast_front(list)) == fast_back(pair) then
        conspair(pair, fast_back(list))
    else
        conspair(fast_front(list), linkdefault(pair, fast_back(list)))
    endif;
enddefine;

;;; link a new list of defaults into previous.  (defaults are value|name pairs)
define linkdefaults(input, previous) -> previous;
lvars each input previous;
    fast_for each in input do
        linkdefault(each, previous) -> previous;
    endfast_for;
enddefine;

;;; find the number which would subscript the word in the vector (or false)
define getindex(word, vector);
lvars vector i word;
    unless isvector(vector) do mishap(vector, 1, 'vector needed') endunless;
    fast_for i from 1 to datalength(vector) do
        if fast_subscrv(i, vector) == word then return(i) endif;
    endfast_for;
    false;
enddefine;

;;; add a new word to the end of the vector;
define addwtov(vector, w) -> newv;
lvars vector w l = datalength(vector), newv = initv(l fi_+ 1);
    move_subvector(1, vector, 1, newv, l);
    w -> fast_subscrv(l fi_+ 1, newv);
enddefine;

;;; fast delete all occurences of -item- from -l-.  Test with == but not
;;; destructive.
define fast_delete(item, l) -> l;
lvars item l;
    if fast_lmember(item, l) then
        if item == fast_front(l) then
            fast_delete(item, fast_back(l)) -> l
        else
            conspair(fast_front(l), fast_delete(item, fast_back(l))) -> l;
        endif
    endif;
enddefine;

;;; procedure to complain if ivar is redeclared as a different type
define newivartype(ivname, oldivtype, fname);
lvars fname oldivtype ivname;
lconstant compile_word = consword('\n;;; COMPILING:  FLAVOUR');
    mishap(if oldivtype == "ivars" then
            'INSTANCE VARIABLE ALREADY DECLARED AS A LEXICAL INSTANCE VARIABLE'
        else
            'INSTANCE VARIABLE ALREADY DECLARED AS A DYNAMIC INSTANCE VARIABLE'
        endif, [redeclaring ^ivname ^compile_word ^fname]);
enddefine;

;;; --- INSTANCE VARIABLE ACCESS / UPDATE ----------------------------------
;;; this procedure provides a fast slot access/update without daemons
define global ivalof(inst, name);
lvars name inst i j vals flave;
    i_frec(inst) -> flave;
    if getindex(name, fast_f_livars(flave)) ->> i then
        i_linstvals(inst) -> vals;
        ;;; ensure that the instance has sufficient instance variables
        if i fi_> datalength(vals) then
            fast_for j from datalength(vals) fi_+ 1 to i do
                addwtov(vals,
                    getdefaultvalue(fast_subscrv(j, fast_f_livars(flave)),
                        flave)) -> vals
            endfast_for;
            vals -> i_linstvals(inst);
        endif;
        fast_subscrv(i, vals);
    elseif getindex(name, f_divars(flave)) ->> i then
        fast_i_dinstvals(inst) -> vals;
        ;;; ensure that the instance has sufficient instance variables
        if i fi_> datalength(vals) then
            fast_for j from datalength(vals) fi_+ 1 to i do
                addwtov(vals,
                    getdefaultvalue(fast_subscrv(j, f_divars(flave)), flave))
                    -> vals
            endfast_for;
            vals -> fast_i_dinstvals(inst);
        endif;
        fast_subscrv(i, vals);
    else
        mishap(name, 1, 'attempt to access unknown slot')
    endif;
enddefine;

define updaterof global ivalof(val, inst, name);
lvars name inst i j vals flave val;
    i_frec(inst) -> flave;
    if getindex(name, fast_f_livars(flave)) ->> i then
        i_linstvals(inst) -> vals;
        ;;; ensure that the instance has sufficient instance variables
        if i fi_> datalength(vals) then
            fast_for j from datalength(vals) fi_+ 1 to i do
                addwtov(vals,
                    getdefaultvalue(fast_subscrv(j, fast_f_livars(flave)),
                        flave)) -> vals
            endfast_for;
            vals -> i_linstvals(inst);
        endif;
        val -> fast_subscrv(i, vals);
    elseif getindex(name, f_divars(flave)) ->> i then
        fast_i_dinstvals(inst) -> vals;
        ;;; ensure that the instance has sufficient instance variables
        if i fi_> datalength(vals) then
            fast_for j from datalength(vals) fi_+ 1 to i do
                addwtov(vals,
                    getdefaultvalue(fast_subscrv(j, f_divars(flave)), flave))
                    -> vals
            endfast_for;
            vals -> fast_i_dinstvals(inst);
        endif;
        val -> fast_subscrv(i, vals);
        ;;; if updating dynamic ivar of current object set the word valof too
        if inst == self then val -> valof(name) endif;
    else
        mishap(val, name, 2, 'attempt to update unknown slot')
    endif;
enddefine;

;;; I_frecord will get the flavour_record from an instance of a METAFLAVOUR
constant procedure I_frecord = ivalof(% "flavour_record" %);

;;; --- CONSTRUCT STANDARD PRECEDENCE LIST ----------------------------------

;;; merges in the precedence of the left most superclass with the merged list
;;; for the rest.  All occurences of -thisfl- are left out.
define mergepreclist(thisfl, left, rest);
lvars thisfl left rest f next;
    [%  fast_for f in left do
            if fast_lmember(f, rest) then
                until fast_front(rest) == f do
                    fast_destpair(rest) -> rest -> next;
                    unless next == thisfl do next endunless;
                enduntil;
                fast_back(rest) -> rest;
            endif;
            unless f == thisfl do f endunless;
        endfor;
    %]
enddefine;

;;; merge the list of precedence lists of superclasses for flavour -fl-
define mergepreclists(thisfl, superprecs);
lvars thisfl superprecs;
    if superprecs == [] then
        []
    elseif listlength(superprecs) == 1 then
        fast_front(superprecs)
    else
        mergepreclist(thisfl,
            fast_front(superprecs),
            mergepreclists(thisfl, fast_back(superprecs)))
    endif;
enddefine;

;;; construct a precedence list for fl
define standardpreclist(fl);
lvars fl;
    conspair(fl,
        fast_delete(fl,
            mergepreclists(fl, maplist(f_supers(fl), f_standardpreclist))));
enddefine;

;;; get a precedence list.  Update the cache if necessay.
define precedencelist(flave) -> plist;
lvars plist flave metafl;
    unless (f_precedencelist(flave) ->> plist) do
        f_standardpreclist(flave) -> plist;
        ;;; get the meta flavour
        f_myinstance(fast_i_frec(f_myinstance(flave))) -> metafl;
        ;;; for the moment metaflavours must use the standard precedence list
        unless metafl == metaflavour_flavour do
            ;;; see if a precedence_list method has been defined
            if syssendmessage("precedence_list", "willrespondto", metafl) then
                syssendmessage("precedence_list", f_myinstance(flave)) -> plist;
                ;;; plist will be a list of instances.  Get the flavour records.
                maplist(plist, I_frecord) -> plist;
            endif;
        endunless;
        ;;; update the cache;
        plist -> f_precedencelist(flave);
    endunless;
enddefine;

;;; --- GET DEFAULT VALUE --------------------------------------------------

;;; get the default value associated with ivar -name- in flavour -flave-
define getdefaultvalue(name, flave);
lvars pair name flave fl;
    for fl in precedencelist(flave) do
        if (gettaggedpair(name, f_defaultvalues(fl)) ->> pair) then
            return(fast_front(pair));
        endif;
    endfor;
    undef;          ;;; default default value
enddefine;

;;; --- GETTING PREVIOUS DELCARATIONS --------------------------------------
define global flavour_of(fn);
lvars fn = fn <> "_flavour";
    unless identprops(fn) == undef do
        if isflavour_instance(valof(fn) ->> fn) then
            return(fn);
        endif;
    endunless;
    false;
enddefine;

define flavourrecord_of(fn);
lvars fn;
    (flavour_of(fn) ->> fn) and I_frecord(fn)
enddefine;

;;; this will do autoloading.
define getflavourrecord_of(fn);
lvars fn = fn <> "_flavour";
    if isflavour_instance(valof(fn) ->> fn) then
        I_frecord(fn);
    else
        false
    endif;
enddefine;

;;; --- INSTANCE CREATION --------------------------------------------------
define consinstance(flave);
lvars fvars i flave;
    consflavour_instance(flave,
        {%  fast_for i from 1 to datalength(f_divars(flave) ->> fvars) do
                getdefaultvalue(fast_subscrv(i, fvars), flave)
            endfast_for; %},
        {%  fast_for i from 1 to datalength(fast_f_livars(flave) ->> fvars) do
                getdefaultvalue(fast_subscrv(i, fvars), flave)
            endfast_for; %});
enddefine;

;;; --- CONSTRUCT CLASS SPECIFIC PROCEDURES --------------------------------

;;; a set of procedures for constructing, at flavour compile time, procedures
;;; for setting and saving the dynamic environment of a flavour and for
;;; handling messages to instances of the class

;;; plant daemons approprite for -mess- accessed by -apdr- from the -list- of
;;; flavour records.
define plantanydaemons(apdr, list, mess, okpdr, callpdr);
lvars rec m list mess procedure(apdr okpdr callpdr);
    fast_for rec in list do
        fast_for m in apdr(rec) do
            if fast_m_methodname(m) == mess and okpdr(fast_m_methodpdr(m)) then
                callpdr(fast_m_methodpdr(m));
                quitloop;   ;;; there should only be one in each record.
            endif;
        endfast_for;
    endfast_for;
enddefine;

constant procedure (
    plantdaemons = plantanydaemons(% notdummypdr, sysCALLQ %),
    plantupddaemons = plantanydaemons(% updater, sysUCALLQ %),
    );

/* -- PLANTINSTVARCHECK ---------------------------------------------------
 > procedure that checks against list of names with procedure for accessing
 > appropriate vector of values.  -vnames- is the name of the instance
 > variables, -apdr- the procedure for accessing the appropiate vector from an
 > instance (i_linstvals or i_dinstvals) -plist- the ordered precedence list of
 > components -rev_plist- a reversed -plist- (for planting after daemons),
 > -elab- is the end label that control should jump to if the message has been
 > dealt with.  -inst- is the instance, -mlist- the list of methods appropriate
 > to the instance and -updflag- says if the message is run in update mode.  It
 > can be false, true or "dynamic" meaning an update of a dynamic ivar. The
 > message is in the dynamic variable "message"
*/
define plantinstvarcheck(vnames, apdr, plist, rev_plist, elab, inst, mlist,
    updflag);
lvars i vnames procedure apdr plist rev_plist elab nextlabel inst
    mlist updflag;

    fast_for i from 1 to length(vnames) do
        /*  if theres a method by the same name then message does not mean an
            access of the instance variable */
        nextif(fast_lmember(vnames(i), mlist));
        sysPUSH("message");
        sysPUSHQ(vnames(i));
        sysCALLQ(nonop ==);
        sysIFNOT(sysNEW_LABEL() ->> nextlabel);
        if updflag then
            plantupddaemons(f_before, plist, vnames(i));
            if updflag == "dynamic" then
                sysPUSHS(false);    ;;; this should be the new value.
                sysPOP(vnames(i));  ;;; set the value of the dynamic variable
            endif
        else
            plantdaemons(f_before, plist, vnames(i));
        endif;
        sysPUSHQ(i);
        if apdr == i_linstvals then     ;;; optimise use the current_lexical_word
            sysPUSH(current_lexical_values_word);
        else
            sysPUSH(inst);
            sysCALLQ(apdr);
        endif;
        if updflag then
            sysUCALLQ(fast_subscrv);
            plantupddaemons(f_after, rev_plist, vnames(i));
        else
            sysCALLQ(fast_subscrv);
            plantdaemons(f_after, rev_plist, vnames(i));
        endif;
        sysGOTO(elab);
        sysLABEL(nextlabel);
    endfast_for;
enddefine;

;;; construct a procedure for setting the dynamic variable environment
define conssetpdr(dvlist, props);
lvars dvlist props i dvals = sysNEW_LVAR();
    sysPROCEDURE(props, 1);
        sysLVARS(dvals, 0);
        sysCALLQ(fast_i_dinstvals);     ;;; get divals for the instance
        sysPOP(dvals);
        fast_for i from 1 to length(dvlist) do
            sysPUSHQ(i);
            sysPUSH(dvals);
            sysCALLQ(fast_subscrv);
            sysPOP(dvlist(i));
        endfast_for;
    sysENDPROCEDURE();
enddefine;

;;; construct a procedure for saving the dynamic variable environment
define conssavepdr(dvlist, props);
lvars dvlist props i dvals = sysNEW_LVAR();
    sysPROCEDURE(props, 1);
        sysLVARS(dvals, 0);
        sysCALLQ(fast_i_dinstvals);     ;;; get divals for the instance
        sysPOP(dvals);
        fast_for i from 1 to length(dvlist) do
            sysPUSH(dvlist(i));
            sysPUSHQ(i);
            sysPUSH(dvals);
            sysUCALLQ(fast_subscrv);
        endfast_for;
    sysENDPROCEDURE();
enddefine;

;;; construct a procedure for finding and running a method.
define constant procedure consrunmethodpdr(record) -> methodrunner;
lvars meth plist rev_plist lexvars dynvars nextlabel endlab mlist
    methodrunner inst mname defmeth = false,
    ;
    fast_f_livars(record) -> lexvars;
    f_divars(record) -> dynvars;

    ;;; get the prcedence list
    precedencelist(record) -> plist;
    ;;; reverse copy for looking up after daemons
    rev(plist) -> rev_plist;

    ;;; construct a procedure for finding and running a method.
    sysNEW_LABEL() -> endlab;
    sysPROCEDURE(false, 2);
        sysNEW_LVAR() -> inst;
        sysLOCAL("message");
        sysPOP(inst);
        sysPOP("message");
        ;;; execute any before any_message daemons
        plantdaemons(f_before, plist, "any_message");
        ;;; check if the message is the name of a method.
        fast_for meth in (getallmethods(plist) ->> mlist) do
            sysPUSH("message");
            sysPUSHQ(fast_m_methodname(meth) ->> mname);
            sysCALLQ(nonop ==);
            sysIFNOT(sysNEW_LABEL() ->> nextlabel);
            plantdaemons(f_before, plist, mname);
            sysCALLQ(fast_m_methodpdr(meth));
            plantdaemons(f_after, rev_plist, mname);
            sysGOTO(endlab);
            sysLABEL(nextlabel);
            unless defmeth do
                if mname == "default_method" then meth -> defmeth endif;
            endunless;
        endfast_for;
        ;;; check if the message is a (lexical) inst variable
        plantinstvarcheck(lexvars, i_linstvals, plist, rev_plist, endlab,
            inst, mlist, false);
        ;;; check if the message is a (dynamic) inst variable
        plantinstvarcheck(dynvars, fast_i_dinstvals, plist, rev_plist, endlab,
            inst, mlist, false);
        ;;; stack the message for either default_method or mishap
        sysPUSH("message");
        ;;; check if there is a default method
        if defmeth then
            plantdaemons(f_before, plist, "default_method");
            sysCALLQ(fast_m_methodpdr(defmeth));
            plantdaemons(f_after, rev_plist, "default_method");
        else                        ;;; mishap if nothing to handle message;
            sysPUSH(inst);
            sysPUSHQ(2);
            sysPUSHQ('message not understood, no default method');
            sysCALLQ(mishap);
        endif;
    sysLABEL(endlab);
        plantdaemons(f_after, rev_plist, "any_message");
    sysENDPROCEDURE() -> methodrunner;

    sysNEW_LABEL() -> endlab;
    false -> defmeth;
    ;;; now the updater.
    sysPROCEDURE(false, 2);
        sysLOCAL("message");
        sysNEW_LVAR() -> inst;
        sysPOP(inst);
        sysPOP("message");
        plantupddaemons(f_before, plist, "any_message");
        ;;; check if the message is the name of a method.
        fast_for meth in (getallupdmethods(plist) ->> mlist) do
            sysPUSH("message");
            sysPUSHQ(fast_m_methodname(meth) ->> mname);
            sysCALLQ(nonop ==);
            sysIFNOT(sysNEW_LABEL() ->> nextlabel);
            plantupddaemons(f_before, plist, mname);
            sysUCALLQ(fast_m_methodpdr(meth));
            plantupddaemons(f_after, rev_plist, mname);
            sysGOTO(endlab);
            sysLABEL(nextlabel);
            unless defmeth do
                if mname == "default_method" then meth -> defmeth endif;
            endunless;
        endfast_for;
        ;;; check if the message is a (lexical) "inst" variable
        plantinstvarcheck(lexvars, i_linstvals, plist, rev_plist, endlab,
            inst, mlist, true);
        ;;; check if the message is a (dynamic) "inst" variable
        plantinstvarcheck(dynvars, fast_i_dinstvals, plist, rev_plist, endlab,
            inst, mlist, "dynamic");
        ;;; stack the message for either default_method or mishap
        sysPUSH("message");
        ;;; check if there is a default method
        if defmeth then
            plantupddaemons(f_before, plist, "default_method");
            sysUCALLQ(fast_m_methodpdr(defmeth));
            plantupddaemons(f_after, rev_plist, "default_method");
        else                        ;;; mishap if nothing to handle message;
            sysPUSH(inst);
            sysPUSHQ(2);
            sysPUSHQ('update message not understood, no default method');
            sysCALLQ(mishap);
        endif;
    sysLABEL(endlab);
        plantupddaemons(f_after, rev_plist, "any_message");
    sysENDPROCEDURE() -> updater(methodrunner);
enddefine;

define constant procedure cons_set_save_handle(record);
lvars dynvars name setter saver methodrunner handler inst;

    f_name(record) -> name;
    f_divars(record) -> dynvars;

    ;;; remake the set and save pdrs if necessary.
    if (f_setself(record) ->> setter) == set_needscompiling then
        if datalength(dynvars) == 0 then
            erase ->> setter ->> f_setself(record)
                ->> saver -> f_saveself(record)
        else
            conssetpdr(dynvars, "sysset_" <> name) ->> setter -> f_setself(record);
            conssavepdr(dynvars, "syssave_" <> name) ->> saver -> f_saveself(record);
        endif;
    else
        f_saveself(record) -> saver
    endif;

    ;;; remake the methodrunner
    consrunmethodpdr(record) ->> methodrunner -> f_runmethod(record);

    ;;; construct a procedure for handling messages.
    sysPROCEDURE("sysflmessagereceiver_" <> name, 2);
        sysNEW_LVAR() -> inst;
        sysPOP(inst);
        unless datalength(dynvars) == 0 do
            ;;; declare the dynamic variables as local
            appdata(dynvars, Declare);
            ;;; set the environment
            sysPUSH(inst);
            sysCALLQ(setter);
        endunless;
        ;;; run the methodrunner procedure
        sysPUSH(inst);
        sysCALLQ(methodrunner);
        ;;; save the environment
        unless datalength(dynvars) == 0 do
            sysPUSH(inst);
            sysCALLQ(saver);
        endunless;
    sysENDPROCEDURE() -> handler;

    ;;; now the updater.
    sysPROCEDURE("sysflmessagereceiver_" <> name, 2);
        sysNEW_LVAR() -> inst;
        sysPOP(inst);
        unless datalength(dynvars) == 0 do
            ;;; declare the dynamic variables as local
            appdata(dynvars, Declare);
            ;;; set the environment
            sysPUSH(inst);
            sysCALLQ(setter);
        endunless;
        ;;; run the methodrunner procedure
        sysPUSH(inst);
        sysUCALLQ(methodrunner);
        ;;; save the environment
        unless datalength(dynvars) == 0 do
            sysPUSH(inst);
            sysCALLQ(saver);
        endunless;
    sysENDPROCEDURE() -> updater(handler);
    ;;; make the handler the message receiver of the record;
    handler -> fast_f_messagereceiver(record);
enddefine;

;;; -- AUTOMATIC RECOMPILATION PROCEDURES ----------------------------------
;;; these procedures are put into the appropiate slots (message receiver, set
;;; self or save self) or a flavour record.  If they are ever needed they will
;;; compile the necessary procedure, cache it and run it.

define mr_needscompiling with_props sys_message_receiver_needscompiling;
lvars flave = i_frec(self);
    ;;; recompile the necssary procedures
    cons_set_save_handle(flave);
    ;;; now run the new procedure
    chain(fast_f_messagereceiver(flave));
enddefine;

define updaterof mr_needscompiling with_props sys_message_receiver_needscompiling;
lvars flave = i_frec(self);
    ;;; recompile the necessary procedures
    cons_set_save_handle(flave);
    ;;; now run the updater of the new procedure
    chain(updater(fast_f_messagereceiver(flave)));
enddefine;

define set_needscompiling(inst) with_props sys_set_self_needscompiling;
lvars inst, flave = fast_i_frec(inst), dynvars = f_divars(flave);
    if datalength(dynvars) == 0 then
        ;;; might as well update both bits
        erase ->> f_setself(flave) -> f_saveself(flave);
        ;;; no need to run it.
    else
        conssetpdr(dynvars, "sysset_" <> f_name(flave))
            ->> f_setself(flave) -> dynvars;
        ;;; now apply it.
        chain(inst, dynvars);
    endif;
enddefine;

define save_needscompiling(inst) with_props sys_save_self_needscompiling;
lvars inst, flave = fast_i_frec(inst), dynvars = f_divars(flave);
    if datalength(dynvars) == 0 then
        ;;; might as well update both bits
        erase ->> f_setself(flave) -> f_saveself(flave);
        ;;; no need to run it.
    else
        conssavepdr(dynvars, "syssave_" <> f_name(flave))
            ->> f_saveself(flave) -> dynvars;
        ;;; now apply it.
        chain(inst, dynvars);
    endif;
enddefine;

define mrun_needscompiling(inst) with_props sys_method_runner_needscompiling;
lvars inst pdr flave = fast_i_frec(inst);
    ;;; recompile the necssary procedure
    consrunmethodpdr(flave) ->> f_runmethod(flave) -> pdr;
    ;;; now run the new procedure
    chain(inst, pdr);
enddefine;

define updaterof mrun_needscompiling(inst) with_props sys_message_runner_needscompiling;
lvars inst pdr flave = fast_i_frec(inst);
    ;;; recompile the necssary procedure
    consrunmethodpdr(flave) ->> f_runmethod(flave) -> pdr;
    ;;; now run the updater of the new procedure
    chain(inst, updater(pdr));
enddefine;

;;; -- RECACHING PROCEDURES ------------------------------------------------
;;; this is called when a new instance variable is declared
define constant recache_ivarlist(iv, flave, apdr);
dlvars iv donelist = [], flave procedure(apdr);
    define lconstant dorecache_varlist(f);
    lvars f;
        unless fast_lmember(f, donelist) do
            unless getindex(iv, apdr(f)) do
                addwtov(apdr(f), iv) -> apdr(f);
                if apdr == f_divars then set_needscompiling -> f_setself(f) endif;
                mr_needscompiling -> fast_f_messagereceiver(f);
                mrun_needscompiling -> f_runmethod(f);
            endunless;
            conspair(f, donelist) -> donelist;
            applist(f_subclasses(f), dorecache_varlist)
        endunless
    enddefine;
    dorecache_varlist(flave);
enddefine;

constant procedure (
    recache_livarlist = recache_ivarlist(% fast_f_livars %),
    recache_divarlist = recache_ivarlist(% f_divars %),
    );

;;; this is called when a new method is defined.
define recompilepdrs() with_nargs 1;
dlvars donelist = nil;
    define lconstant dorecompilepdrs(f);
    lvars f;
        unless fast_lmember(f, donelist) do
            mr_needscompiling -> fast_f_messagereceiver(f);
            mrun_needscompiling -> f_runmethod(f);
            conspair(f, donelist) -> donelist;
            applist(f_subclasses(f), dorecompilepdrs);
        endunless;
    enddefine;
    dorecompilepdrs();
enddefine;

;;; note the cached preclist needs remaking.  Remake the standpreclist
define resetpreclist() with_nargs 1;
dlvars donelist = nil;
    define lconstant doresetpreclist(f);
    lvars f;
        unless fast_lmember(f, donelist) do
            standardpreclist(f) -> f_standardpreclist(f);
            false -> f_precedencelist(f);
            conspair(f, donelist) -> donelist;
            applist(f_subclasses(f), doresetpreclist);
        endunless;
    enddefine;
    doresetpreclist();
enddefine;

;;; --- FLAVOUR CREATION AND ALTERATION ------------------------------------
;;; the non syntax way of creating or altering a flavour.  This take a list
;;; of additions and then either alters the existing flavour record or
;;; creates a new one and an instance of the flavour flavour called
;;; <name>_flavour.
define global sysflavour(name, i_supers, i_livars, i_divars, i_methods,
    i_before, i_after, metaflavour, i_deflist) -> frecord;
lvars name i_supers i_livars i_divars, i_methods i_before i_after i_deflist
    frecord p_supers p_after p_before p_methods p_livars p_divars each temp
    save_super p_subclasses p_deflist metaflavour inform_message
    nonewsuper = true;

    ;;; adding a new lexical ivar to a list if necessary and warning
    define lconstant procedure newlivar(iv);
    lvars iv;
        unless getindex(iv, p_livars) or fast_lmember(iv, protectedivars) do
            if getindex(iv, p_divars) then
                newivartype(iv, "divars", name)
            else
                addwtov(p_livars, iv) -> p_livars;
                if frecord then recache_livarlist(iv, frecord) endif;
            endif;
        endunless
    enddefine;
    ;;; adding a new dynamic ivar to a list if necessary and warning
    define lconstant procedure newdivar(iv);
    lvars iv;
        unless getindex(iv, p_divars) do
            if getindex(iv, p_livars) do
                newivartype(iv, "ivars", name)
            elseunless fast_lmember(iv, protectedivars) do
                addwtov(p_divars, iv) -> p_divars;
                if frecord then recache_divarlist(iv, frecord) endif;
            endif
        endunless
    enddefine;

    ;;; get or create the p (previous values) variables
    if flavourrecord_of(name) ->> frecord then
        fast_f_livars(frecord) -> p_livars;
        f_divars(frecord) -> p_divars;
        f_methods(frecord) -> p_methods;
        f_before(frecord) -> p_before;
        f_after(frecord) -> p_after;
        f_supers(frecord) -> p_supers;
        f_defaultvalues(frecord) -> p_deflist;
        i_frec(f_myinstance(frecord)) -> metaflavour;
    else
        {} ->> p_livars -> p_divars;
        [] ->> p_methods ->> p_before ->> p_after ->> p_supers -> p_deflist;
    endif;

    ;;; make sure each component is fully declared insert new components in
    ;;; declared order.  This ensures that new ones come after old ones that
    ;;; are specified as being more specific.
    conspair(false, p_supers) ->> p_supers -> save_super;
    for each in i_supers do
        if (fast_lmember(each, p_supers) ->> temp) then
            temp -> p_supers;
        else
            false -> nonewsuper;
            conspair(each, back(p_supers)) ->> back(p_supers) -> p_supers;
        endif;
        appdata(f_livars(each), newlivar);
        appdata(f_divars(each), newdivar);
    endfor;
    back(save_super) -> p_supers;

    ;;; new declare the i (input values) variables methods and default values
    appdata(i_livars, newlivar);
    appdata(i_divars, newdivar);
    linkmethods(i_methods, p_methods) -> p_methods;
    linkmethods(i_before, p_before) -> p_before;
    linkmethods(i_after, p_after) -> p_after;
    linkdefaults(i_deflist, p_deflist) -> p_deflist;

    ;;; now create or alter the frecord
    if frecord then
        p_livars -> fast_f_livars(frecord);
        p_divars -> f_divars(frecord);
        p_methods -> f_methods(frecord);
        p_before -> f_before(frecord);
        p_after -> f_after(frecord);
        p_supers -> f_supers(frecord);
        p_deflist -> f_defaultvalues(frecord);
        "flavour_changed" -> inform_message;
    else
        ;;;               mesreceiver        livars    divars    name
        consflavourrecord(mr_needscompiling, p_livars, p_divars, name,
        ;;; methods    before    after    supers    subs myinst plist  standp
            p_methods, p_before, p_after, p_supers, nil, false, false, false,
        ;;; setter              saver                methodrunner
            set_needscompiling, save_needscompiling, mrun_needscompiling,
        ;;; defaults
            p_deflist) -> frecord;
        ;;; now make the metaflavour instance.
        consinstance(metaflavour) -> each;
        frecord -> I_frecord(each);
        name -> ivalof(each, "name");
        each -> f_myinstance(frecord);

        ;;; declare the <name>_flavour as global constant and assign
        name <> "_flavour" -> each;
        if identprops(each) == undef then
            sysSYNTAX(each, 0, true);
            sysGLOBAL(each);
        endif;
        f_myinstance(frecord) -> valof(each);
        "initialise" -> inform_message;
    endif;
    standardpreclist(frecord) -> f_standardpreclist(frecord);

    ;;; update the down pointer of each superclass
    fast_for each in p_supers do
        unless fast_lmember(frecord, f_subclasses(each)) do
            conspair(frecord, f_subclasses(each)) -> f_subclasses(each)
        endunless;
    endfast_for;

    ;;; note that this class and all subclasses needs recompiling
    unless datalength(i_livars) == 0 and datalength(i_divars) == 0
    and nonewsuper and i_methods == [] and i_before == [] and i_after == [] do
        recompilepdrs(frecord);
    endunless;
    unless nonewsuper do
        resetpreclist(frecord);
    endunless;
    ;;; send the message initialise if the flavour will respond
    if syssendmessage(inform_message, "willrespondto", f_myinstance(metaflavour)) then
        ;;; if necessary pass empty list to initliase;
        if inform_message == "initialise" then nil endif;
        syssendmessage(inform_message, f_myinstance(frecord));
    endif;
enddefine;

;;; --- MESSAGE SENDING AND METHOD EXECUTION -------------------------------
define global syssendmessage(message, inst);
lvars inst flave vals i lvnames cuself = false;
dlocal message myflavour self;
#_< sysVARS(current_lexical_values_word, 0);
    sysVARS(current_lexical_names_word, 0);
>_#
    unless isflavour_instance(inst) do
        mishap(inst, 1, 'ATTEMPT TO SEND MESSAGE TO NON INSTANCE');
    endunless;
    ;;; if we are currently in a method then save the environment
    if isflavour_instance(self) then
        ;;; apply the appropriate save self pdr to the vector of dynamic values
        self -> cuself;
        f_saveself(fast_i_frec(cuself))(cuself);
    endif;

    ;;; set the variables
    inst -> self;
    fast_i_frec(inst) -> flave;
    f_myinstance(flave) -> myflavour;

    ;;; check that the inst has correct amount of variable space
    if datalength(fast_i_dinstvals(inst) ->> vals) fi_< datalength(f_divars(flave)) do
        fast_for i from datalength(vals) fi_+ 1 to datalength(f_divars(flave)) do
            addwtov(vals, getdefaultvalue(
                fast_subscrv(i, f_divars(flave)), flave)) -> vals
        endfast_for;
        vals -> fast_i_dinstvals(inst);
    endif;
    fast_f_livars(flave) -> lvnames;
    if datalength(i_linstvals(inst) ->> vals) fi_< datalength(lvnames) do
        fast_for i from datalength(vals) fi_+ 1 to datalength(lvnames) do
            addwtov(vals, getdefaultvalue(fast_subscrv(i, lvnames), flave))
                -> vals;
        endfor;
        vals -> i_linstvals(inst);
    endif;

    ;;; set the value of the lexical values word to be the vector of values
    ;;; and the lexical names word to be the vector of names
    vals -> valof(current_lexical_values_word);
    lvnames -> valof(current_lexical_names_word);

    ;;; run the message receiver procedrue
    fast_f_messagereceiver(flave)(message, inst);

    ;;; if we are in a method then reset the environment.
    if cuself then
        ;;; apply the appropriate save self pdr to the vector of dynamic values
        f_setself(fast_i_frec(cuself))(cuself);
    endif;
enddefine;

define updaterof global constant procedure syssendmessage(message, inst);
lvars inst flave vals i lvnames cuself = false;
dlocal message myflavour self;
#_< sysVARS(current_lexical_values_word, 0);
    sysVARS(current_lexical_names_word, 0);
>_#
    unless isflavour_instance(inst) do
        mishap(inst, 1, 'ATTEMPT TO SEND (UPDATE) MESSAGE TO NON INSTANCE');
    endunless;
    ;;; if we are currently in a method then save the environment
    if isflavour_instance(self) then
        ;;; apply the appropriate save self pdr to the vector of dynamic values
        self -> cuself;
        f_saveself(fast_i_frec(cuself))(cuself);
    endif;

    ;;; set the variables
    inst -> self;
    fast_i_frec(inst) -> flave;
    f_myinstance(flave) -> myflavour;

    ;;; check that the inst has correct amount of variable space
    if datalength(fast_i_dinstvals(inst) ->> vals) fi_< datalength(f_divars(flave)) do
        fast_for i from datalength(vals) fi_+ 1 to datalength(f_divars(flave)) do
            addwtov(vals, getdefaultvalue(
                fast_subscrv(i, f_divars(flave)), flave)) -> vals
        endfast_for;
        vals -> fast_i_dinstvals(inst);
    endif;
    fast_f_livars(flave) -> lvnames;
    if datalength(i_linstvals(inst) ->> vals) fi_< datalength(lvnames) do
        fast_for i from datalength(vals) fi_+ 1 to datalength(lvnames) do
            addwtov(vals, getdefaultvalue(fast_subscrv(i, lvnames), flave))
                -> vals;
        endfor;
        vals -> i_linstvals(inst);
    endif;

    ;;; set the value of the lexical values word to be the vector of values
    ;;; and the lexical names word to be the vector of names
    vals -> valof(current_lexical_values_word);
    lvnames -> valof(current_lexical_names_word);

    ;;; run the updater of the message receiver procedrue
    -> fast_f_messagereceiver(flave)(message, inst);

    ;;; if we are in a method then reset the environment.
    if cuself then
        ;;; apply the appropriate save self pdr to the vector of dynamic values
        f_setself(fast_i_frec(cuself))(cuself);
    endif;
enddefine;

;;; --- ACCESSING AND PRINTING ROUTINES ------------------------------------
syssendmessage -> class_apply(flavour_instance_key);

procedure(flave);
lvars flave;
    printf('<flavour_record %p>', [% f_name(flave) %]);
endprocedure -> class_print(flavourrecord_key);

procedure(inst);
lvars inst f i;
    fast_i_frec(inst) -> f;
    if syssendmessage("printself", "willrespondto", f_myinstance(f)) then
        syssendmessage("printself", inst)
    else
        printf('<instance of %p>', [% f_name(f) %]);
    endif;
endprocedure -> class_print(flavour_instance_key);

nonop == -> class_=(flavourrecord_key);

;;; one vital core flavour
consflavourrecord(mr_needscompiling, {}, {name flavour_record}, "metaflavour",
    [% consmethodrecord(
    procedure(mess) with_props willrespondto;
    lvars mess rec myrec meth okpdr = notdummypdr;
        if mess == "updater" then
            -> mess;
            updater -> okpdr;
        endif;
        if getindex(mess, f_livars(I_frecord(self) ->> myrec))
        or getindex(mess, f_divars(myrec)) then
            return(true)
        else
            for rec in f_standardpreclist(myrec) do
                for meth in f_methods(rec) do
                    if m_methodname(meth) == mess
                    and okpdr(m_methodpdr(meth)) then
                        return(true)
                    endif;
                endfor;
            endfor;
            false;
        endif;
    endprocedure, "willrespondto") %], nil, nil, nil, nil, false,
    nil, nil, set_needscompiling, save_needscompiling, mrun_needscompiling,
    nil) -> metaflavour_flavour;
;;; set the precedence lists
[% metaflavour_flavour %]
    ->> f_standardpreclist(metaflavour_flavour)
    -> f_precedencelist(metaflavour_flavour);
;;; recompile the pdrs.
cons_set_save_handle(metaflavour_flavour);
;;; now construct the instance
consflavour_instance(metaflavour_flavour,
    {% "metaflavour", metaflavour_flavour %}, {})
    ->> f_myinstance(metaflavour_flavour) -> metaflavour_flavour;

;;; tidy up
applist([self myflavour], sysprotect);
false ->> popdefineprocedure -> popdefineconstant;

endsection;     ;;; $-flavour
endsection;

/* --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Jul  2 1986 - made the automatic-recompilation procedures
    chain to the compiled procedure instead of simply applying them.
--- Mark Rubinstein, Jun 24 1986 - corrected the order of after daemons for the
    method -default_method- and an error in -linkmethods- which meant that
    replacements of updater methods were not properly dealt with and old
    methods were also kept, creating excessive garbage.
--- Mark Rubinstein, Jun 20 1986 - altered the updater of -ivalof- so that if
    you are updating the value of a dyanamic instance variable of self then the
    -valof- of the word is also updated.
--- Mark Rubinstein, Jun 19 1986 - It is no longer to specify a metaflavour to
    -sysflavuor- if you are altering an existing flavour.  If the flavour is
    being changed (rather than created) then the message "flavour_changed" is
    sent instead of "initialise".
--- Mark Rubinstein, Jun  9 1986 - altered the construction of values word and
    names word (previously used -sysnvariable-) in order to prevent clash with
    other libraries (such as lib foreach).
--- Mark Rubinstein, May 29 1986 - made -message- a global exported variable
    bound to the current messgae at message send time.  Added the f_runmethod
    slot to flavourrecords allowing for efficient sending of message to -self-
    without need to reset the current dynamic environment.
--- Mark Rubinstein, May  7 1986 made sysflavour insert new components in
    order sensitive to existing components.
*/
