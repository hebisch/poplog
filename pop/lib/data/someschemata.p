/*  --- Copyright University of Sussex 1986.  All rights reserved. ---------
 *  File:           $usepop/master/C.all/lib/data/someschemata.p
 *  Purpose:        Some sample schemata for use with SCHECK.
 *  Author:         Steven Hardy, May 1979 (see revisions)
 *  Documentation:  TEACH * SCHEMA
 *  Related Files:  LIB * SCHEMA
 */

section;

;;;     Ensure that LIB SCHEMA has been compiled

uses schema;

;;;     Define a few stories
vars story1;

[[john intends to meet mary]
 [john car wont start]
 [john phones mary]
] -> story1;

vars story2;
[[john likes mary]
 [john has not seen mary recently]
 [john phones mary]
] -> story2;

vars story3;
[[john intends to meet mary]
 [john car wont start]
 [john phones bill]
 [bill is a mechanic]
] -> story3;

vars story4;
[[john is going to holland]
 [john phones cooks]
 [cooks phones british airways]
 [cooks writes ticket to holland for john]
 [john is very excited]
] -> story4;

;;;     Define a few schemata

;;;     Declare variables used in schemata
vars w, x, y, z;
vars sorry;
[[??x intends to meet ??y]
 [??x car wont start]
 [??x phones ??y]
 [??x apologizes to ??y]
] -> sorry;

vars garage;
[[??x intends to meet ??y]
 [??x car wont start]
 [??x phones ??z]
 [??z is a mechanic]
 [??x asks ??z to fix car]
 [??x takes taxi to ??y]
] -> garage;

vars date;
[[??x likes ??y]
 [??x phones ??y]
 [??x arranges meeting with ??y]
 [??x meets ??y]
] -> date;

vars bookticket;
[[??x is going to ??y]
 [??x phones ??z]
 [??z is a travel agent]
 [??x asks for ticket to ??y]
 [??z phones ??w]
 [??w is an airline]
 [??z asks for reservation to ??y]
 [??z writes ticket to ??y for ??x]
] -> bookticket;


define identify(story);
;;; Given a story decide which of the above schemata it fits best
lvars story;
        story -> database;
        schoose([sorry garage date bookticket]) =>
enddefine;

endsection;
