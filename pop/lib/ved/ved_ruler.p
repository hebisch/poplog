/*  --- Copyright University of Sussex 1995.  All rights reserved. ---------
 >  File:           C.all/lib/ved/ved_ruler.p
 >  Purpose:        inserts a 'ruler' in the text showing margins and tabs
 >  Author:         Chris Slymon (from David Roberts), June 1983
 >  Documentation:  HELP * FORMAT/RULER
 >  Related Files:
 */

/*  Inserts, in a marked range above the current line, a 'ruler'
    indicating the limits of left and right margins and tab settings e.g

         1         2         3         4         5         6         7
123456789012345678901234567890123456789012345678901234567890123456789012345678
            <   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |   |>

*/


section;

define global ved_ruler();
dlocal vedchanged = false;
    vedpositionpush();

    /* tens line */
    vedlineabove();
    vedcheck();
    vedmarklo();
    for vedcolumn from 10 by 9 /* VEDCHARINSERT adds 1 */ to vedlinemax do
        vedcharinsert( (vedcolumn div 10) + `0` )
    endfor;

    /* units line */
    vedlinebelow(); 1 -> vedcolumn;
    while vedcolumn <= vedlinemax do
        vedcharinsert( (vedcolumn rem 10 ) + `0`);
    endwhile;

    /* margins and tab settings */
    vedlinebelow();
    vedmarkhi();
    vedcharinsert(`<`);
    for vedtabright() step vedtabright() till vedcolumn >= vedlinemax do
            vedcharinsert(`|`);
    endfor;
    vedlinemax -> vedcolumn;
    vedcharinsert(`>`);

    vedpositionpop()
enddefine;

endsection;
