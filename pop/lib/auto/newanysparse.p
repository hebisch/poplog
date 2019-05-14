/*  --- Copyright University of Sussex 1996.  All rights reserved. ---------
 >  File:           C.all/lib/auto/newanysparse.p
 >  Purpose:        Create sparse arrays, using a tree of properties
 >  Author:         Jonathan Laventhol, 1984 (see revisions)
 >  Documentation:  REF *PROPS, HELP *NEWANYSPARSE (See below also)
 >  Related Files:  NEWSPARSE
 */
compile_mode:pop11 +strict;

/*
Makes a new sparse array, out of a tree of properties
the sparse array is a procedure (with updater) with N arguments if
it has N dimensions.  the pdprops of it are [sparse_array ^default]
(although note that changing the pdprops has no effect on the
behaviour of the array)
the tree is fleshed out the minimal amount to contain all the entries,
but it never contracts.  you can control the size of the properties at
the different levels of the tree, and you can set the default for the
array.  you can also have "active defaults" -- where the value for an
unspecified entry is computed rather than stored.  the default size of the
properties is 20.  because the tree is made from properties, you can use
any pop item as a subscript, but remember that the subscript you access
with must be == with the subscript you stored with.  (usually this means
strings, list, vectors are no good -- use words, numbers, procedures, undef
variables).

Some examples:

newanysparse(3, [])
    makes a three-dimensional array, default is []
newanysparse(2, undef)
    two dimensions, default is undef
newanysparse([10 20], 0)
    two dimensions, default is 0
    the list specifies that for the first dimension, there are going to be
    about 10 entries.  for the second dimension, about 20.
    (this controls the size of the properties used at each level)
newanysparse(5, active_default, apply)
    five dimensions
    if active_default is a procedure (at the time of this call) then
    the default for each element will be computed when needed, by applying
    active_default to the subscripts of the entry.
newanysparse(3, consvector(% 3 %), apply)
    three dimensions.  the default for each entry is {I J K} where I, J and
    K are the subscripts of that entry
newanysparse([10], apply);
    one dimension, default is the procedure apply
    this is the same in effect as newproperty([], 10, apply, true)
*/


section;

uses $-lib$-newanysparse_base;

define newanysparse(Ndims, default) -> pdr;
    lvars Ndims, default, i, active_default = false, dimlist, pdr, upd;

    ;;; want pdr applied at runtime for default?
    if default == apply and isprocedure(Ndims) then
        (), Ndims -> (Ndims, active_default);
        consstring(0) -> default        ;;; make unique marker
    endif;

    ;;; check dimension specifier is okay
    if isinteger(Ndims) then
        [] -> dimlist;
        repeat Ndims times 20 :: dimlist -> dimlist endrepeat;
    elseif islist(Ndims) then
        Ndims -> dimlist;
        listlength(Ndims) -> Ndims;
        for i in dimlist do
            unless isinteger(i) and i > 0 then
                mishap(i, 1, 'DIMENSION SIZE MUST BE POSITIVE INTEGER')
            endunless;
        endfor;
    else
        mishap(Ndims, 1, 'DIMENSION SPECIFIER MUST BE INTEGER OR LIST')
    endif;
    unless Ndims > 0 then
        mishap(Ndims, 1, 'NUMBER OF DIMENSIONS MUST BE 1 OR MORE');
    endunless;

    ;;; check default procedure, if being used
/*  if active_default and pdnargs(active_default) /== Ndims then
        mishap(pdnargs(active_default), 1,
                    'DEFAULT PROCEDURE HAS WRONG NUMBER OF ARGUMENTS')
    endif;
*/

    $-lib$-newanysparse_base(% Ndims,
                            newproperty([], hd(dimlist), default, true),
                            default, active_default %) -> pdr;
    [sparse_array
    % if active_default then active_default, apply else default endif %]
            -> pdprops(pdr);
    Ndims -> pdnargs(pdr);
    updater(pdr) -> upd;
    tl(dimlist) -> frozval(4, upd);     ;;; less first dimension
    Ndims+1 -> pdnargs(upd)
enddefine;


endsection;


/* --- Revision History ---------------------------------------------------
--- John Gibson, May 22 1996
        Rewritten so it doesn't use the VM (which a runtime procedure
        like this shouldn't).
--- John Gibson, Aug 27 1990
        Changed n*ote to weak
--- John Gibson, Jun 30 1989
        Added +strict etc
--- John Gibson, Aug  5 1987
        sectioned
 */
