/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/turtle/Markhere.p
 *  Purpose:        put paint in current location.
 *  Author:         Unknown, ???
 *  Documentation:
 *  Related Files:
 */

section $-turtle => Markhere;

define global procedure Markhere(_x,_y);
    paint -> picture(round(_x) ->> xposition, round(_y) ->> yposition)
enddefine;
false -> pdprops(Markhere);

endsection;
