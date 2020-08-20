/* --- Copyright University of Sussex 1986.  All rights reserved. ---------
 > File:           $usepop/master/C.all/lib/flavours/subclass_responsibility_message.p
 > Purpose:        note that method should be provided by a subclass
 > Author:         Mark Rubinstein, Jun  2 1986
 > Documentation:  HELP * METAFLAVOUR_FLAVOUR /subclass_responsibility
 > Related Files:
 */

section;

define lconstant syssubclass_responsibility(message, here);
lvars message here;
    mishap('MESSAGE SHOULD HAVE BEEN DEFINED IN MY SUBCLASS',
            [^self <- ^message handled by ^here]);
enddefine;

flavour metaflavour;
    defmethod subclass_responsibility(method);
    lvars method m qualifier;
        if islist(method) then
            for m in method do ^subclass_responsibility(m) endfor;
            return;
        endif;
        unless isword(method) do
            mishap(method, 1, 'name of method expected');
        endunless;

        if lmember(method, [before after]) then
            method -> qualifier;
            -> method;
            syssubclass_responsibility(% method, name %)
                -> ^methodfor(method, qualifier);
        else
            syssubclass_responsibility(% method, name %)
                -> ^methodfor(method);
        endif;
    enddefmethod;
endflavour;

endsection;
