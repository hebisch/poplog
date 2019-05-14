/*  --- Copyright University of Sussex 1989.  All rights reserved. ---------
 >  File:           C.all/lib/lib/sets.p
 >  Purpose:        various set manipulation routines
 >  Author:         Jon Cunningham 1982 (see revisions)
 >  Documentation:  HELP * SETS
 >  Related Files:
 */
compile_mode:pop11 +defcon +defpdr;

vars sets; true -> sets;                    ;;; so that 'uses sets' is ok

vars iselementtype elementorder;            ;;; default set type (see helpfile)
    isword -> iselementtype;
    alphabefore -> elementorder;

uses recordclass;
recordclass constant set elements;

constant emptyset;
consset([]) -> emptyset;

define listtoset(list);
lvars item;
    if list == [] then return(consset([])) endif;
    unless list.islist then
        mishap('Trying to convert a non-list to a set',[^list])
    endunless;
    for item in list do
        unless item.iselementtype then
            mishap('Trying to convert a list with wrong element type to a set',list)
        endunless
    endfor;
    syssort(list,elementorder) -> list;
    consset([% while back(list) /== [] do           ;;; eliminate repetitions
                 if front(list) = front(back(list)) then back(list) -> list
                 else destpair(list) -> list
                 endif
             endwhile;
             front(list)
             %])
enddefine;

define intersect(s1,s2) -> s3;
lvars e1 e2 o;
    unless s1.isset and s2.isset then
        mishap('Intersection of non-sets',[^s1 ^s2])
    endunless;
    [% elements(s1) -> s1;
         elements(s2) -> s2;
         until s1 == [] or s2 == [] do
             front(s1) -> e1;
             front(s2) -> e2;
             elementorder(e1,e2) -> o;
             if o == 1 then
                 e1;
                 back(s1) -> s1;
                 back(s2) -> s2
             elseif o then
                 back(s1) -> s1
             else
                 back(s2) -> s2
             endif
         enduntil
         %] -> s3;
    consset(s3) -> s3
enddefine;

define union(s1,s2) -> s3;
    lvars e1 e2 o;
    [% elements(s1) -> s1;
         elements(s2) -> s2;
         until s1 == [] or s2 == [] do
             front(s1) -> e1;
             front(s2) -> e2;
             elementorder(e1,e2) -> o;
             if o == 1 then
                 e1;
                 back(s1) -> s1;
                 back(s2) -> s2
             elseif o then
                 e1,back(s1) -> s1
             else
                 e2,back(s2) -> s2
             endif
         enduntil;
         %] nc_<> if s1 /== [] then s1 else s2 endif -> s3;
    consset(s3) -> s3
enddefine;

define elementof(e,s);
    lvars o;
    elements(s) -> s;
    unless e.iselementtype then return(false) endunless;
    until s == [] or not(elementorder((destpair(s) -> s),e) ->> o) do
        if o == 1 then return(true) endif
    enduntil;
    return(false)
enddefine;

define makesubset(s,p);
lvars e;
    consset([% for e in elements(s) do if p(e) then e endif endfor %])
enddefine;

define singleton(s);
    if s.isset
            and (elements(s) ->> s) /== []
            and back(s) == []
    then front(s)
    else false
    endif
enddefine;

define setdiff(s1,s2) -> s3;
lvars e1 e2 o;
    unless s1.isset and s2.isset then
        mishap('Set difference of non-sets',[^s1 ^s2])
    endunless;
    elements(s1) -> s1;
    elements(s2) -> s2;
    [% until s1 == [] or s2 == [] do
             front(s1) -> e1;
             front(s2) -> e2;
             elementorder(e1,e2) -> o;
             if o == 1 then
                 back(s1) -> s1;
                 back(s2) -> s2
             elseif o then
                 e1,back(s1) -> s1
             else back(s2) -> s2
             endif
         enduntil
         %] nc_<> s1 -> s3;
    consset(s3) -> s3
enddefine;

define subset(s1,s2);
    setdiff(s1,s2) = emptyset
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Aug 13 1989
        Replaced nc join with nc_<>
 */
