/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/make_instance.p
 > Purpose:        instantiate a flavour and initialise the instance
 > Author:         Mark Rubinstein, Apr 18 1986 (see revisions)
 > Documentation:  HELP * MAKE_INSTANCE, *FLAVOUR_LIBRARY, TEACH * FLAVOUR
 > Related Files:  LIB * FLAVOURS
 */

section $-flavour => make_instance;

;;; --- INSTANCE CREATION --------------------------------------------------
;;; Takes a list of form [flavour_name attribute value attribute value ...]
;;; makes an instance, sends the initialise message if there is a response
;;; to it and then sets the attributes to be the given values.

define global make_instance(list) -> inst;
lvars list name flave inst;
    dest(list) -> list -> name;
    if isflavour_instance(name) then
        name -> flave
    elseunless isflavour_instance(valof(name <> "_flavour") ->> flave) do
        mishap(name, 1, 'no such flavour');
    endif;
    (flave<-new) -> inst;
    if flave <- willrespondto("initialise") then
        inst <- initialise(list);
    endif;
enddefine;

endsection;

/* --- Revision History ---------------------------------------------------
--- Mark Rubinstein, Jun  9 1986 - made provision to allow first argument to
    be a flavour instead of the name of a flavour.
*/
