/*  --- Copyright University of Sussex 1991. All rights reserved. ----------
 *  File:           C.all/lib/flavours/knownmethods.p
 *  Purpose:        find all the known methods for a given flavour
 *  Author:         Mark Rubinstein, Sep 18 1985 (see revisions)
 *  Machines:       all
 *  Documentation:  TEACH * FLAVOUR /knownmethods
 *  Related Files:  LIB * FLAVOUR
 */

uses flavour;

section $-flavour => knownmethods;

;;; find all the known methods for the given flavour.  Can take optional 2nd
;;; argument the word "before" or "after" to get appropriate daemons.
define global knownmethods(record) -> methods;
lvars record each methods procedure accesspdr = f_methods;
    if record == "before" then
        -> record;
        f_before -> accesspdr;
    elseif record == "after" then
        -> record;
        f_after -> accesspdr
    elseif isprocedure(record) then
        record -> accesspdr;
        -> record;
    endif;
    if isflavour_instance(record) then
        if isinstance(record, "flavour") then
            chain(record<-flavour_record, accesspdr, knownmethods)
        else
            chain(i_frec(record), accesspdr, knownmethods)
        endif;
    endif;
    unless isflavourrecord(record) do
        mishap(record, 'flavour record expected')
    endunless;
    maplist(accesspdr(record), m_methodname) -> methods;
    for each in f_supers(record) do
        applist(knownmethods(each, accesspdr),
            procedure(w);
            lvars w;
                unless lmember(w, methods) do
                    conspair(w, methods) -> methods
                endunless;
            endprocedure);
    endfor;
enddefine;

endsection;

/*  --- Revision History ---------------------------------------------------
--- Andreas Schoter, Jun 27 1991 - made to work with the new format for
    flavour records (i.e. changed reference to -f_component- to a reference
    to -f_supers-
--- Mark Rubinstein, Oct  4 1985 - made to accept instances of the flavour
    flavour and to respond with methods recognised by its flavour record rather
    than that of the flavour flavour.
 */
