/* --- Copyright University of Sussex 1997. All rights reserved. ----------
 > File:            C.all/lib/include/doc_index.ph
 > Purpose:         Header file for documentation indexing mechanism
 > Author:          John Williams, Apr 26 1989 (see revisions)
 > Documentation:
 > Related Files:   C.all/src/sys_search_doc_index.p
 */


/* Index entry format

Index lives in sub-directory 'doc_index' of associated documentation
directory. 'doc_index' comprises 26 files, [a-z], listing entries
alphabetically.

Each entry is a line with 5 space-separated fields:

    NAME    FILE                HDR_START_LINE  HDR_END_LINE    END_LINE
e.g.
    abs     $usepop/pop/ref/numbers     401         401             406


The procedure -sys_search_doc_index- looks up entries by NAME and returns
matching entries as vectors containing five elements.

*/

define lconstant Rsubscrv(vec, index);
    lvars index vec;
    fast_subscrv(index, vec)
enddefine;

define updaterof lconstant Rsubscrv(item, vec, index);
    lvars index item vec;
    item -> fast_subscrv(index, vec)
enddefine;

define lconstant Info_name = Rsubscrv(% 1 %) enddefine;

define lconstant Info_file = Rsubscrv(% 2 %) enddefine;

define lconstant Info_hdr_start = Rsubscrv(% 3 %) enddefine;

define lconstant Info_hdr_end = Rsubscrv(% 4 %) enddefine;

define lconstant Info_txt_end = Rsubscrv(% 5 %) enddefine;


/* Indexing algorithm */

weak constant procedure (isalphacode sysisdirectory);

define lconstant Index_dir(dir);
    lvars dir;
    dir dir_>< 'doc_index/'
enddefine;

define lconstant Has_index_dir(dir);
    lvars dir;
    sysisdirectory(Index_dir(dir));
enddefine;

define lconstant Index_file(string);
    lvars char string;
    uppertolower(fast_subscrs(1, string)) -> char;
    unless isalphacode(char) do
        `z` -> char
    endunless;
    consstring(char, 1)
enddefine;


lvars All_index_files_list = false;

define lconstant All_index_files();
    lvars i;
    unless All_index_files_list do
        [% fast_for i from `a` to `z` do
            Index_file(consstring(i, 1))
        endfast_for %] -> All_index_files_list
    endunless;
    All_index_files_list
enddefine;


/* Standard line buffer */

lconstant LLEN = 255;

lvars
    LINE    = false,
    LINE8   = false,
    LINE16  = false,
;

define lconstant setup_LINE();
    if pop_default_device_encoding then
        LINE16 or (inits16(LLEN) ->> LINE16)
    else
        LINE8 or (inits(LLEN) ->> LINE8)
    endif -> LINE
enddefine;



/* --- Revision History ---------------------------------------------------
--- John Gibson, Apr 10 1997
        Changed setup_LINE to init buffers on the basis of
        pop_default_device_encoding.
--- Robert Duncan, Mar 18 1996
        Changed Has_index_dir not to depend on sysdefs; now works for Win32
        also.
--- Robert John Duncan, Aug 20 1991
        Added -Has_index_dir- for testing the existence of an index
        directory (primarily for VMS).
--- Robert John Duncan, Aug  5 1991
        Made -Index_dir- work for VMS by adding a trailing '/' to the
        directory name.
--- Simon Nichols, Sep  3 1990
        Changed n*ote to weak.
 */
