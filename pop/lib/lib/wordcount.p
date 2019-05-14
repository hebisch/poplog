/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 >  File:           C.all/lib/lib/wordcount.p
 >  Purpose:        a simple word counter for text files.
 >  Author:         Roger Evans
 >  Documentation:  HELP * WORDCOUNT
 >  Related Files:  LIB * VEDWORDCOUNT_TEXT
 */

/* argument is one of:
    a procedure - assumed to be a repeater
    a string or word - use discin to get repeater

    it counts items (produced by the itemiser) based on the repeater obtained
    except that any char of chartype > 4 is turned into a space (ie an item
    separator) - a simple heuristic that gives a pretty good word count.
    See HELP ITEM_CHARTYPE for details of char types - basically punctuation
    is filtered out.
*/

section;

define global vars wordcount(rep);
    lvars c count procedure wcrep;
    unless rep.isprocedure then discin(rep) -> rep endunless;

    incharitem(rep) -> wcrep;
    for c from 0 to 127 do
        if item_chartype(c) > 4 then 6 -> item_chartype(c,wcrep) endif;
    endfor;

    0 -> count;
    until wcrep() == termin do
        count fi_+ 1 -> count
    enduntil;
    count;
enddefine;


endsection;
