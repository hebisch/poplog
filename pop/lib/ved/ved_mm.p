/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_mm.p
 >  Purpose:        Move to Matching end. E.g. if at "if" move to after "endif"
 >  Author:         Aaron Sloman, July 1982 (see revisions)
 >  Documentation:  HELP * VEDCOMMS/ved_mm
 >  Related Files:
 */
compile_mode :pop11 +strict;

section;

define vars ved_mm();
    lvars item items count listdepth wordq pcentstack opener in_part_apply last;
    dlocal vvedmarkhi;
    define dlocal prmishap(mess, list);
        lvars mess list;
            vederror(mess)
        enddefine;
    vvedbuffersize -> vvedmarkhi;
    vedsetlinesize();
    incharitem(vedrepeater)-> items;
    items() -> opener;     ;;; move past next item
    if lmember(opener,vedopeners) or opener == "[" or opener == "{" then
        'FOUND "%" OUTSIDE LIST DOING VED_MM' -> pcentstack;
        0 -> wordq;
        if opener == "{" or opener == "[" then 1 else 0 endif -> listdepth;
        1 -> count;
        0 -> in_part_apply;
        opener -> item;
        until (item -> last; items() -> item; item) == termin do
            if wordq > 0 then
                wordq - 1 -> wordq
            elseif item == "[" or item == "{" then
                listdepth + 1 -> listdepth;
                count + 1 -> count;
            elseif item == "%" then
                if listdepth == 0 then
                    if last == "(" then
                        in_part_apply + 1 -> in_part_apply
                    elseif in_part_apply == 0 then
                        destpair(destpair(pcentstack) -> pcentstack) -> listdepth
                            -> in_part_apply
;;;                 else    ;;; next item should be ")"
                    endif
                else
                    conspair(
                        conspair(in_part_apply, listdepth ),
                        pcentstack) -> pcentstack;
                    0 ->> listdepth -> in_part_apply
                endif
            elseif item == "]" or item == "}" then
                listdepth - 1 -> listdepth;
                count - 1 -> count;
                if listdepth < 0 then
                    vederror('SURPLUS "' >< item >< '" IN VED_MM');
                endif;
            elseif listdepth /== 0 then   ;;; ignore quoted items
            elseif item == ")" then
                if last == "%" then in_part_apply - 1 -> in_part_apply endif;
                count - 1 -> count
            elseif item == """ then
                2 -> wordq
            elseif lmember(item, vedopeners) then
                count + 1 -> count
            elseif lmember(item, vedclosers) then
                count - 1 -> count
            endif;

            if count < 1 then
                if listdepth /== 0 then
                    vederror('UNMATCHED "{" or "["')
                elseif in_part_apply /== 0 then
                    vederror('UNMATCHED "%"')
                endif;
                goto found
            endif
        enduntil;
        vederror('NO  MATCHING CLOSER FOR "' >< opener >< """)
    elseif opener == "vars" then
        until (items() ->> item) == ";" or item == termin do enduntil;
    endif;

found:
    ;;; backspace as necessary to compensate for characters in buffer
    cont(frozval(1,items)) -> items;
    while ispair(items) then vedcharleft(); back(items) -> items endwhile;
enddefine;

endsection;
