/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/metaflavour_flavour.p
 > Purpose:        The Metaflavour flavour for the flavours system
 > Author:         Mark Rubinstein, Apr 17 1986 (see revisions)
 > Documentation:  HELP * METAFLAVOUR, TEACH * FLAVOURS
 > Related Files:  LIB * FLAVOURS
 */

section $-flavour => metaflavour_flavour;

/* vanilla is mixed into metaflavour once vanilla has been defined */
flavour metaflavour a metaflavour novanilla;
divars name flavour_record;
lconstant canceled_ivar = sysnvariable();
    defmethod before updaterof flavour_record;
        mishap("flavour_record", 2, 'ATTEMPT TO ALTER PROTECTED INSTANCE VARIABLE');
    enddefmethod;
    defmethod before updaterof name;
        mishap("name", 2, 'ATTEMPT TO ALTER PROTECTED INSTANCE VARIABLE');
    enddefmethod;
    defmethod printself;
        printf('<%p %p>', [% myflavour<-name, name %]);
    enddefmethod;
    defmethod instance_variables;
         delete(canceled_ivar,
            datalist(f_livars(flavour_record)) <>
                datalist(f_divars(flavour_record)))
    enddefmethod;
    defmethod lexical_instance_variables;
        delete(canceled_ivar, datalist(f_livars(flavour_record)))
    enddefmethod;
    defmethod dynamic_instance_variables;
        delete(canceled_ivar, datalist(f_divars(flavour_record)))
    enddefmethod;
    defmethod methods;
        maplist(f_methods(flavour_record), m_methodname)
    enddefmethod;
    defmethod beforedaemons;
        maplist(f_before(flavour_record), m_methodname)
    enddefmethod;
    defmethod afterdaemons;
        maplist(f_after(flavour_record), m_methodname)
    enddefmethod;
    defmethod components;
        maplist(f_supers(flavour_record), f_myinstance)
    enddefmethod;
    defmethod subflavours;
        maplist(f_subclasses(flavour_record), f_myinstance);
    enddefmethod;
    defmethod methodfor(methodname) -> p;
    lvars methodname accesspdr p upd = false;
        if methodname == "before" then
            -> methodname; f_before;
        elseif methodname == "after" then
            -> methodname; f_after;
        else
            f_methods;
        endif -> accesspdr;
        if methodname == "updater" then
            true -> upd; -> methodname;
        endif;
        if (gettaggedpair(methodname, accesspdr(flavour_record)) ->> p) then
            m_methodpdr(p) -> p;
            if upd then
                updater(p) -> p
            elseunless notdummypdr(p) then  ;;; if is dummypdr
                false -> p
            endif;
        endif;
    enddefmethod;
    defmethod updaterof methodfor(methodname);
    lvars methodname pdr type = "method", upd = false;
        if lmember(methodname, [before after]) then
            methodname -> type; -> methodname;
        endif;
        if methodname == "updater" then
            true -> upd; -> methodname;
        endif;
        -> pdr;
        unless isprocedure(pdr) do mishap(pdr, 1, 'PROCEDURE NEEDED') endunless;
        consmethodrecord(pdr, methodname) -> pdr;
        if upd then consmethodrecord(pdr, "updater") -> pdr endif;
        [^pdr] -> pdr;
        sysflavour(name, [], {}, {},
            if type == "method" then
                pdr, [], []
            elseif type == "before" then
                [], pdr, []
            else
                [], [], pdr
            endif, flavour_record, []) ->;
    enddefmethod;
    defmethod cancelmethod(methodname);
    lvars each methodname accesspdr list;
        if methodname == "before" then
            -> methodname; f_before;
        elseif methodname == "after" then
            -> methodname; f_after;
        else
            f_methods;
        endif -> accesspdr;
        conspair(false, accesspdr(flavour_record)) -> list;
        for each on list do
            if ispair(back(each)) do
                if m_methodname(front(back(each))) == methodname then
                    back(back(each)) -> back(each);
                    back(list) -> accesspdr(flavour_record);
                    recompilepdrs(flavour_record);
                    return;
                endif;
            endif
        endfor;
        mishap(methodname, name, 2, 'method/daemon not found')
    enddefmethod;
    defmethod cancelivar(iv);
    lvars i lexvars = f_livars(flavour_record),
        dynvars = f_divars(flavour_record)
        ;
        if (getindex(iv, lexvars) ->> i) then
            canceled_ivar -> lexvars(i);
            recompilepdrs(flavour_record);
        elseif (getindex(iv, dynvars) ->> i) then
            canceled_ivar -> dynvars(i);
            recompilepdrs(flavour_record);
        else
            mishap(iv, name, 2, 'no such instance variable')
        endif;
    enddefmethod;
    defmethod cancelcomponent(icomp);
    lvars each icomp comp clist = conspair(false, f_supers(flavour_record));
        if isword(icomp) then
            flavourrecord_of(icomp) -> comp; 
        elseif isflavour_instance(icomp) then
            I_frecord(icomp) -> comp;
        else
            icomp -> comp;
        endif;
        unless isflavourrecord(comp) do
            mishap('FLAVOUR OR NAME OF FLAVOUR NEEDED', [^icomp]);
        endunless;
        for each on clist do
            if ispair(back(each)) do
                if front(back(each)) == comp then
                    back(back(each)) -> back(each);
                    back(clist) -> f_supers(flavour_record);
                    recompilepdrs(flavour_record);
                    return;
                endif;
            endif;
        endfor;
        mishap(icomp, clist, 2, 'no such component')
    enddefmethod;
    ;;; get the default value for an instance varable.  Does not climb
    ;;; the heirarchy.
    defmethod default_value_for(ivname);
    lvars ivname p;
        unless lmember(ivname, ^instance_variables) do
            mishap(ivname, 1, 'NOT AN INSTANCE VARIABLE NAME')
        endunless;
        if (gettaggedpair(ivname, f_defaultvalues(flavour_record)) ->> p) then
            front(p)
        else
            undef
        endif
    enddefmethod;
    ;;; set the default value for an instance varable.  UNDEF means delete
    ;;; any existing default value at this flavour.             
    defmethod updaterof default_value_for(newvalue, ivname);
    lvars ivname p newvalue;
        unless lmember(ivname, ^instance_variables) do
            mishap(ivname, 1, 'NOT AN INSTANCE VARIABLE NAME')
        endunless;
        if (gettaggedpair(ivname, f_defaultvalues(flavour_record)) ->> p) then
            if newvalue == undef then
                ncdelete(p, f_defaultvalues(flavour_record)) ->
                    f_defaultvalues(flavour_record);
            else
                newvalue -> front(p)
            endif
        elseunless newvalue == undef do
            conspair(conspair(newvalue, ivname),
                f_defaultvalues(flavour_record))
                    -> f_defaultvalues(flavour_record);
        endif;
    enddefmethod;
    defmethod flavour_changed;
    enddefmethod;
endflavour;

endsection;

/* --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Jun 19 1986 - added the dummy method -flavour_changed-.
--- Mark Rubinstein, Jun 11 1986 - fixed typo in cancelcomponent.
--- Mark Rubinstein, Jun  8 1986 - added the -default_value_for- method
    and its updater.
--- Mark Rubinstein, Jun  4 1986 - made both the instance variables divars.
--- Mark Rubinstein, Jun  3 1986 - added before updaterof flavour_record.
*/
