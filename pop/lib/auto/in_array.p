/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.all/lib/auto/in_array.p
 > Purpose:         Iteration over data in arrays
 > Author:          David S Young, Jan 31 1994
 > Documentation:   HELP * IN_ARRAY
 > Related Files:   LIB * S-array-S-runtime
 */

/*

Implements for loops over arrays, or regions of arrays. Regions are
rectangular in 2-D and the equivalent in other dimensions.

         CONTENTS - (Use <ENTER> g to access required sections)

 -- Note on processing strategy
 -- Run-time procedures to check arrays etc.
 -- Code-planting utilities
 -- Loop setup, initial and final code - 1-D case
 -- Loop setup, initial and final code - N-D case, N known
 -- Loop setup, initial and final code - N-D case, N unknown
 -- Main loop code - dimension unknown at compile time
 -- Main loop code - dimension known at compile time
 -- Parse for statement and check arguments
 -- Interface to the for_form system

-- Note on processing strategy ----------------------------------------

The code is long because the situation is intrinsically more complex
than for iteration over vectors or lists;  it is necessary to allow for
iteration over arrays of any number of dimensions. The number of
dimensions may not be known at compile-time (e.g. in a library procedure
to find the maximum and mimimum values in an arbitrary region of an
arbitrary array.)

If the of_dimension keyword is included, the dimensions are known at
compile-time, and code that effectively implements nested numerical for
loops is planted. Access to the arrays is then always via the array
procedures. This is fairly straightforward.

Otherwise, we cannot infer the number of dimensions at compile-time
(even if the region is a compile-time list). In this case, we have to
plant code that dynamically adapts to the number of dimensions - which
may be arbitrarily large. Such code cannot avoid holding the array
indices in a structure, which entails accessing and updating the
structure on each iteration.

However, a general method is unnecessarily slow for 1-D and 2-D arrays,
where we can simply hold the indices in variables. We therefore plant
code to recognise and deal with three cases:

    1.   The 1-D case.  We simply iterate a single index, directly
        accessing the relevant part of the arrayvector. This is
        substantially faster than the 2-D or N-D methods.

        This method extends to any array in which the data to be
        processed occupy a contiguous section of the arrayvector. This
        will be the case if the region list is the same as the
        boundslist in all but its last 2 elements (or first 2 for an
        array arrayed by column).  In fact, the 1-D method is used in
        the following cases:

        i.  The arrays are 1-D.

        ii. The arrays are N-D with N > 1, all the arrays have
        contiguous data regions in their arrayvectors, all are arrayed
        by row, and the index variable (with_index) is not in use.

    2.  The 2-D case.  We iterate two variables (as if in two nested for
        loops) over the data region, and access the data through the
        array procedures. This is faster than the N-D method.

        This is applied to all 2-D arrays which cannot be treated by the
        1-D method.  The indices have to be stuffed into a vector if the
        index variable is in use, which increases the overhead.

    3.  The N-D case. We construct a vector at run-time to hold the
        current indices.  At least one of these has to be incremented on
        each iteration, and then tested to see whether we need to do the
        next dimension as well. The vector is exploded to get the
        indices to apply each array to.

        This is applied to all arrays of dimension 3 or greater which
        cannot be handled by the 1-D method. The vector itself is
        assigned to the index variable if there is one, so this has
        little overhead.  (I'd be inclined to copy it to protect the
        internal state, but other looping constructs allow the user to
        change the index variable.)

*/

compile_mode:pop11 +strict;

section;

/*
-- Run-time procedures to check arrays etc. ---------------------------
*/

uses-by_name from $-in_array$-runtime (
    $-in_array$-getcoords,
    $-in_array$-arrays_1d,
    $-in_array$-check_region,
    $-in_array$-min_region
);

/*
-- Code-planting utilities --------------------------------------------
*/

define lconstant plantINCR(V);
    lvars V;
    sysPUSH(V);
    sysPUSHQ(1);
    sysCALL("fi_+");
    sysPOP(V);
enddefine;

define lconstant plantDECR(V);
    lvars V;
    sysPUSH(V);
    sysPUSHQ(1);
    sysCALL("fi_-");
    sysPOP(V);
enddefine;

define lconstant lvarsLIST(n) /* -> LIST */;
    lvars n;
    [% repeat n times sysNEW_LVAR() endrepeat %]
enddefine;

define lconstant plantPROCLIST(LIST1, p, LIST2);
    lvars LIST1, p, LIST2;
    ;;; Plants code to apply p to value of each item in LIST1 and put
    ;;; the results in LIST2, where the lists contain variables.
    lvars V1, V2;
    for V1, V2 in LIST1, LIST2 do
        sysPUSH(V1);
        sysCALL(p);         ;;; expects a word
        sysPOP(V2);
    endfor
enddefine;

/*
-- Loop setup, initial and final code - 1-D case ----------------------
*/

/* This procedure (like the 2-D and N-D versions) plants three chunks of
code:

        1. Setup before loop starts
        2. Initial actions inside loop (before body)
        3. Final actions inside loop (before jumping back)

These have labels and transfer control as follows:

    (no label)
                SETUP code
                GO TO label STARTLOOP

    INIT:
                INITIAL ACTIONS code
                if no more data to process
                    GO TO label ENDLOOP
                else
                    GO TO label STARTBODY
                endif

    FINAL:
                FINAL ACTIONS code
                GO TO label STARTLOOP

The STARTLOOP, STARTBODY and ENDLOOP labels must be generated and
planted by the calling procedure. The INIT and FINAL labels are
generated, planted and returned by this procedure.
*/

define lconstant plantA1LOOP(
        VARLIST, nset, INDEX, ARRAYS, VECINDS, IND, NTODO,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP)
        -> (labelINIT, labelFINAL);
    lvars VARLIST, nset, INDEX, ARRAYS, VECINDS, IND, NTODO,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP, labelINIT, labelFINAL;

    ;;; Region continuous and arrays arrayed by row

    ;;; SETUP CODE

    ;;; Get the subscriptors
    lvars SUBS = lvarsLIST(length(ARRAYS));
    plantPROCLIST(ARRAYS, "array_subscrp", SUBS);
    ;;; Replace ARRAYS by their arrayvectors
    plantPROCLIST(ARRAYS, "arrayvector", ARRAYS);

    ;;; Set up INDEX variable - needs to be a vector
    if INDEX then
        sysPUSHQ(1);
        sysCALL("initv");
        sysPOP(INDEX);
    endif;
    sysGOTO(labelSTARTLOOP);

    ;;; INITIAL CODE IN LOOP

    sysLABEL(sysNEW_LABEL() ->> labelINIT);

    ;;; decrement the count and check whether more to do
    plantDECR(NTODO);
    sysPUSH(NTODO);
    sysPUSHQ(0);
    sysCALL("fi_<");
    sysIFSO(labelENDLOOP);       ;;; jump out - done

    ;;; Set up the user variables and increment the array indexes
    lvars VAR, ARRVEC, VECIND, SUB;
    repeat nset times
        fast_destpair(VARLIST) -> (VAR, VARLIST);
        fast_destpair(ARRAYS) -> (ARRVEC, ARRAYS);
        fast_destpair(VECINDS) -> (VECIND, VECINDS);
        fast_destpair(SUBS) -> (SUB, SUBS);
        sysPUSH(VECIND);
        sysPUSH(ARRVEC);
        sysCALL(SUB);
        sysPOP(VAR);        ;;; set user variable
        plantINCR(VECIND);  ;;; increment index for this array
    endrepeat;

    ;;; Copy the user index into the vector
    if INDEX then
        sysPUSH(IND);
        sysPUSHQ(1);
        sysPUSH(INDEX);
        sysUCALL("fast_subscrv");
        ;;; and increment it for next time
        plantINCR(IND);
    endif;
    sysGOTO(labelSTARTBODY);

    ;;; FINAL CODE IN LOOP

    sysLABEL(sysNEW_LABEL() ->> labelFINAL);
    for VAR, ARRVEC, VECIND, SUB in VARLIST, ARRAYS, VECINDS, SUBS do
        sysPUSH(VAR);
        sysPUSH(VECIND);
        sysPUSH(ARRVEC);
        sysUCALL(SUB);      ;;; update arrayvector
        plantINCR(VECIND);
    endfor;
    sysGOTO(labelSTARTLOOP);

enddefine;

/*
-- Loop setup, initial and final code - N-D case, N known -------------
*/

define lconstant plantAKLOOP(VARLIST, nset, INDEX, ARRAYS, REGION, ndim,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP)
        -> (labelINIT, labelFINAL);
    lvars VARLIST, nset, INDEX, ARRAYS, REGION, ndim,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP, labelINIT, labelFINAL;

    ;;; Loop over N-D ARRAYS, where N is known at compile-time.
    ;;; Essentially just N nested numerical for-loops.

    ;;; SETUP

    ;;; Assign region dimensions to variables
    lvars
        MINCOORDS = lvarsLIST(ndim),
        MAXCOORDS = lvarsLIST(ndim),
        COORDS = lvarsLIST(ndim),
        C0, C1, C;                          ;;; min, max, current coord
    sysPUSH(REGION);
    sysCALL("dl");          ;;; Region on stack
    for C0, C1, C in MINCOORDS, MAXCOORDS, COORDS do
        sysPOP(C1);     ;;; Top of stack is end of REGION - a max coord
        sysPUSHS(true); ;;; both min coord and initialisation of coord
        sysPOP(C0);
        sysPOP(C);
    endfor;
    ;;; Reverse lists as took data off backwards
    ncrev(MINCOORDS) -> MINCOORDS;
    ncrev(MAXCOORDS) -> MAXCOORDS;
    ncrev(COORDS) -> COORDS;
    ;;; Decrement first coord
    plantDECR(hd(COORDS));

    ;;; Set up user index
    if INDEX then
        sysPUSHQ(ndim);
        sysCALL("initv");
        sysPOP(INDEX)
    endif;
    sysGOTO(labelSTARTLOOP);

    ;;; INITIAL CODE IN LOOP

    sysLABEL(sysNEW_LABEL() ->> labelINIT);
    ;;; for y from y0 to y1 do for x from x0 to x1 do etc.
    lvars labelCOORDSREADY = sysNEW_LABEL();
    for C0, C1, C in MINCOORDS, MAXCOORDS, COORDS do
        plantINCR(C);
        sysPUSH(C);
        sysPUSH(C1);
        sysCALL("fi_>");
        sysIFNOT(labelCOORDSREADY);
        ;;; Gone off end, so reset, and carry on to next coord
        sysPUSH(C0);
        sysPOP(C);
    endfor;
    ;;; If end up here, then last coord has gone off end
    sysGOTO(labelENDLOOP);

    ;;; Coords vars all set up now - assign to user vars
    sysLABEL(labelCOORDSREADY);
    lvars VAR, ARR;
    repeat nset times
        fast_destpair(VARLIST) -> (VAR, VARLIST);
        fast_destpair(ARRAYS) -> (ARR, ARRAYS);
        for C in COORDS do
            sysPUSH(C);
        endfor;
        sysCALL(ARR);
        sysPOP(VAR);
    endrepeat;

    ;;; Update user index
    if INDEX then
        for C in COORDS do
            sysPUSH(C);
        endfor;
        sysPUSH(INDEX);
        sysCALL("fill");
        sysERASE(true);
    endif;
    sysGOTO(labelSTARTBODY);

    ;;; FINAL CODE IN LOOP

    sysLABEL(sysNEW_LABEL() ->> labelFINAL);
    for VAR, ARR in VARLIST, ARRAYS do
        sysPUSH(VAR);
        for C in COORDS do
            sysPUSH(C);
        endfor;
        sysUCALL(ARR);
    endfor;
    sysGOTO(labelSTARTLOOP);

enddefine;

/*
-- Loop setup, initial and final code - N-D case, N unknown -----------
*/

define lconstant plantANLOOP(
        VARLIST, nset, INDEX, ARRAYS, REGION, NDIM,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP)
        -> (labelINIT, labelFINAL);
    lvars VARLIST, nset, INDEX, ARRAYS, REGION, NDIM,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP, labelINIT, labelFINAL;

    ;;; Plant code to loop over N-D ARRAYS
    ;;; We can no longer assign loop variables at compile time
    ;;; - must adopt the slower method of keeping the indices in
    ;;; a vector, created at run-time.

    ;;; SETUP

    lvars
        COORDS,
        MINCOORDS = sysNEW_LVAR(),
        MAXCOORDS = sysNEW_LVAR();
    ;;; Use index variable if given - user must not change it,
    ;;; but this seems to be in spirit of e.g. in_vector.
    if INDEX then
        INDEX
    else
        sysNEW_LVAR()
    endif -> COORDS;

    ;;; Create vectors to hold the coords
    sysPUSH(NDIM);
    sysCALL("initv");
    sysPOP(COORDS);
    sysPUSH(NDIM);
    sysCALL("initv");
    sysPOP(MINCOORDS);
    sysPUSH(NDIM);
    sysCALL("initv");
    sysPOP(MAXCOORDS);

    ;;; Fill them from the region list
    sysPUSH(REGION);
    sysPUSH(COORDS);
    sysPUSH(MINCOORDS);
    sysPUSH(MAXCOORDS);
    sysPUSH(NDIM);
    sysCALL("ident $-in_array$-getcoords");
    sysGOTO(labelSTARTLOOP);

    ;;; INITIAL code in loop

    sysLABEL(sysNEW_LABEL() ->> labelINIT);
    ;;; Update coordinate vector, checking if more to do
    lvars
        DIM = sysNEW_LVAR(),
        coords_LOOP = sysNEW_LABEL(),
        coords_DONE = sysNEW_LABEL();
    sysPUSHQ(0);
    sysPOP(DIM);
    ;;;                             for DIM from 1 to NDIM do
    sysLABEL(coords_LOOP);
    plantINCR(DIM);
    sysPUSH(DIM);
    sysPUSH(NDIM);
    sysCALL("fi_>");
    sysIFSO(labelENDLOOP);      ;;; completely finished
    ;;;                             coords(DIM) + 1 ->> coords(DIM)
    sysPUSH(DIM);
    sysPUSH(COORDS);
    sysCALL("fast_subscrv");
    sysPUSHQ(1);
    sysCALL("fi_+");
    sysPUSHS(true);
    sysPUSH(DIM);
    sysPUSH(COORDS);
    sysUCALL("fast_subscrv");
    ;;;                             quitif coords(DIM) <= maxcoords(DIM)
    sysPUSH(DIM);
    sysPUSH(MAXCOORDS);
    sysCALL("fast_subscrv");
    sysCALL("fi_<=");
    sysIFSO(coords_DONE);
    ;;;                             mincoords(DIM) -> coords(DIM)
    sysPUSH(DIM);
    sysPUSH(MINCOORDS);
    sysCALL("fast_subscrv");
    sysPUSH(DIM);
    sysPUSH(COORDS);
    sysUCALL("fast_subscrv");
    ;;;                             endfor
    sysGOTO(coords_LOOP);

    ;;; Coords set up, so can set user variables.
    ;;; Turns out to be faster to keep exploding the vector than
    ;;; it is to copy the top NDIM items of the stack.
    sysLABEL(coords_DONE);
    lvars VAR, ARR;
    repeat nset times
        fast_destpair(VARLIST) -> (VAR, VARLIST);
        fast_destpair(ARRAYS) -> (ARR, ARRAYS);
        sysPUSH(COORDS);
        sysCALL("destvector");
        sysERASE(true);     ;;; delete no of items
        sysCALL(ARR);
        sysPOP(VAR);
    endrepeat;
    sysGOTO(labelSTARTBODY);

    ;;; FINAL CODE IN LOOP

    sysLABEL(sysNEW_LABEL() ->> labelFINAL);
    for VAR, ARR in VARLIST, ARRAYS do
        sysPUSH(VAR);
        sysPUSH(COORDS);
        sysCALL("destvector");
        sysERASE(true);     ;;; delete no of items
        sysUCALL(ARR);
    endfor;
    sysGOTO(labelSTARTLOOP);

enddefine;

/*
-- Main loop code - dimension unknown at compile time -----------------
*/

define lconstant plantVLOOPCODE(
        VARLIST, INDEX, ARRAYS, nset, REGION, NDIM);
    lvars
        VARLIST,        ;;; list of variables
        INDEX,          ;;; index variable or false
        ARRAYS,         ;;; list of variables holding arrays
        nset,           ;;; number of variables to set at start
        REGION,         ;;; region variable
        NDIM;           ;;; no. dimension variable

    ;;; Plant code to test which of the 3 kinds of processing to use;
    ;;; set up arguments and call all 3 main code-planting procedure;
    ;;; plant code for body of loop.

    lvars narrs = length(VARLIST);

    ;;; Flag for 3-way strategy
    ;;; 1 for indexing directly into arrayvector
    ;;; 2 for loop over 2-D array
    ;;; 3 for loop over discontinuous N-D array
    lvars METHOD = sysNEW_LVAR();

    ;;; Loop labels
    lvars
        labelSTARTLOOP = sysNEW_LABEL(),
        labelENDLOOP = sysNEW_LABEL(),
        labelSTARTBODY = sysNEW_LABEL();
    pop11_loop_start(labelSTARTLOOP);
    pop11_loop_end(labelENDLOOP);

    ;;; Jump labels for options
    lvars
        labelA1SETUP = sysNEW_LABEL(),
        labelA2SETUP = sysNEW_LABEL(),
        labelANSETUP = sysNEW_LABEL();

    ;;; Are the regions all continuous in their arrayvectors,
    ;;; and all arrayed by row, and either 1-D or not requiring
    ;;; indexing?
    ;;; The startpts vector must be finished with before any
    ;;; general code is executed (compile-time list used at run-time so
    ;;; recursive calls could mess it up).
    lvars
        startpts = initv(narrs),
        RSIZE = sysNEW_LVAR();
    applist(ARRAYS, sysPUSH);       ;;; Push arrays
    sysPUSHQ(narrs);
    sysPUSH(REGION);
    sysPUSHQ(startpts);
    sysCALL("ident $-in_array$-arrays_1d");    ;;; arrays_1d(narrs, region, startpts)
    sysPUSHS(true);
    sysPOP(RSIZE);              ;;; ->> RSIZE
    sysIFNOT(labelA2SETUP);
    ;;; Yes, continuous - if requiring indexing then must be 1-D
    if INDEX then
        sysPUSH(NDIM);
        sysPUSHQ(1);
        sysCALL("==");
        sysIFNOT(labelA2SETUP);
        ;;; Will use simple arrayvector access, so initialise user index
        lvars IND = sysNEW_LVAR();
        sysPUSH(REGION);
        sysCALL("hd");
        sysPOP(IND);
    endif;
    ;;; OK to do using arrayvector access
    ;;; Put start points into variables
    lvars VECINDS = lvarsLIST(narrs);
    sysPUSHQ(startpts);
    sysCALL("destvector");
    sysERASE(true);         ;;; remove length of vector
    applist(VECINDS, sysPOP);
    ncrev(VECINDS) -> VECINDS;
    ;;; Plant main 1-D code
    sysPUSHQ(1);
    sysPOP(METHOD);
    lvars (labelINIT1, labelFINAL1) =
        plantA1LOOP(VARLIST, nset, INDEX, ARRAYS, VECINDS, IND, RSIZE,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP);

    ;;; Are the arrays two-dimensional?
    sysLABEL(labelA2SETUP);
    sysPUSH(NDIM);
    sysPUSHQ(2);
    sysCALL("==");
    sysIFNOT(labelANSETUP);
    ;;; 2-D - (but regions discontinuous, or index needed)
    sysPUSHQ(2);
    sysPOP(METHOD);
    lvars (labelINIT2, labelFINAL2) =
        plantAKLOOP(VARLIST, nset, INDEX, ARRAYS, REGION, 2,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP);

    ;;; So they are N-dimensional - could plant specific code for
    ;;; more cases, but would make for excessive size.
    sysLABEL(labelANSETUP);
    sysPUSHQ(3);
    sysPOP(METHOD);
    lvars (labelINIT3, labelFINAL3) =
        plantANLOOP(VARLIST, nset, INDEX, ARRAYS, REGION, NDIM,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP);

    ;;; Start the loop
    sysLABEL(labelSTARTLOOP);
    sysPUSH(METHOD);       ;;; Switch increment code
    sysGO_ON([% labelINIT1, labelINIT2, labelINIT3 %], false);

    ;;; do the loop body
    sysLABEL(labelSTARTBODY);
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;

    ;;; do termination code
    sysPUSH(METHOD);       ;;; Switch increment code
    sysGO_ON([% labelFINAL1, labelFINAL2, labelFINAL3 %], false);

    sysLABEL(labelENDLOOP);
enddefine;

/*
-- Main loop code - dimension known at compile time -------------------
*/

define lconstant plantFLOOPCODE(
        VARLIST, INDEX, ARRAYS, nset, REGION, ndim);
    lvars
        VARLIST,        ;;; list of variables
        INDEX,          ;;; index variable or false
        ARRAYS,         ;;; list of variables holding arrays
        nset,           ;;; number of variables to set at start
        REGION,         ;;; region variable
        ndim;           ;;; number of dimensions

    ;;; Plant code for case where no. of dimensions is known at
    ;;; compile-time.

    ;;; Loop labels
    lvars
        labelSTARTLOOP = sysNEW_LABEL(),
        labelENDLOOP = sysNEW_LABEL(),
        labelSTARTBODY = sysNEW_LABEL();
    pop11_loop_start(labelSTARTLOOP);
    pop11_loop_end(labelENDLOOP);

    lvars (labelINIT, labelFINAL) =
        plantAKLOOP(VARLIST, nset, INDEX, ARRAYS, REGION, ndim,
        labelSTARTLOOP, labelSTARTBODY, labelENDLOOP);

    ;;; Start the loop
    sysLABEL(labelSTARTLOOP);
    sysGOTO(labelINIT);

    ;;; do the loop body
    sysLABEL(labelSTARTBODY);
    pop11_comp_stmnt_seq_to(popclosebracket) -> ;

    ;;; do termination code
    sysGOTO(labelFINAL);
    sysLABEL(labelENDLOOP);
enddefine;

/*
-- Parse for statement and check arguments ----------------------------
*/

define lconstant array_loop(VARLIST, is_fast, INDEX);
    lvars VARLIST, is_fast, INDEX;

    ;;; Read the for ... do section, and plant code to check the
    ;;; arguments. Set up calls to main code planting processes.

    dlocal pop_new_lvar_list;

    lvars narrs = length(VARLIST);

    ;;; Get the arrays and assign them to variables

    lvars wd;
    pop11_comp_stmnt_seq_to(
        [updating_last in_region of_dimension do then]
    ) -> wd;;

    lvars ARRAYS = lvarsLIST(narrs);
    applist(ARRAYS, sysPOP);
    ;;; Put the array variables into the same order as VARLIST
    ncrev(ARRAYS) -> ARRAYS;

    ;;; Get the updating number (default to 1 if keyword but no number)
    lvars nset = narrs;     ;;; no. vars to set at start of loop
    if wd == "updating_last" then
        if isinteger(nextitem()) then
            itemread() -> nset;         ;;; no. to update
            checkinteger(nset, 0, narrs);
            narrs - nset -> nset;       ;;; no. to set
        else
            narrs - 1 -> nset
        endif;
        pop11_need_nextitem([in_region of_dimension do then]) -> wd
    endif;

    ;;; Check the arrays and get the iteration region
    ;;; and runtime dimensions
    lvars
        REGION = sysNEW_LVAR(),
        NDIM = sysNEW_LVAR();
    if wd == "in_region" then
        ;;; Region specified - get and check it
        pop11_comp_stmnt_seq_to([of_dimension do then]) -> wd;
        sysPOP(REGION);
        unless is_fast then          ;;; do not check if fast
            applist(ARRAYS, sysPUSH);
            sysPUSHQ(narrs);
            sysPUSH(REGION);
            sysCALL("ident $-in_array$-check_region");
            sysPOP(NDIM);
        endunless
    else
        ;;; Figure out the region from array boundslists
        if is_fast then
            ;;; Just get boundslist of first array
            sysPUSH(hd(ARRAYS));
            sysCALL("boundslist");
            sysPOP(REGION);
        else
            ;;; Get intersection of arrays
            applist(ARRAYS, sysPUSH);
            sysPUSHQ(narrs);
            sysCALL("ident $-in_array$-min_region");
            sysPOP(REGION);
            sysPOP(NDIM);
        endif;
    endif;
    ;;; Still need NDIM if fast
    if is_fast then
        sysPUSH(hd(ARRAYS));
        sysCALL("pdnargs");
        sysPOP(NDIM);
    endif;

    ;;; Get the number of dimensions at compile-time
    lvars ndim = false;             ;;; compile-time dimension
    if wd == "of_dimension" then
        itemread() -> ndim;
        checkinteger(ndim, 1, false);
        pop11_need_nextitem([do then]) -> ;

        ;;; and test that arrays agree at run-time
        unless is_fast then
            lvars DIMSOK = sysNEW_LABEL();
            sysPUSHQ(ndim);
            sysPUSH(NDIM);
            sysCALL("==");
            sysIFSO(DIMSOK);
            sysPUSHQ(ndim);
            sysPUSH(NDIM);
            sysPUSHQ(2);
            sysPUSHQ('Array or region with wrong no. dimensions');
            sysCALL("mishap");
            sysLABEL(DIMSOK);
        endunless
    endif;

    ;;; Now split depending on whether have compile-time dimensions
    if ndim then
        plantFLOOPCODE(VARLIST, INDEX, ARRAYS, nset, REGION, ndim)
    else
        plantVLOOPCODE(VARLIST, INDEX, ARRAYS, nset, REGION, NDIM)
    endif;

enddefine;

/*
-- Interface to the for_form system -----------------------------------
*/

constant syntax (
    updating_last = pop_undef,
    of_dimension = pop_undef
    );

define :for_extension in_array(VARLIST, is_fast);
    lvars VARLIST, is_fast;
    array_loop(VARLIST, is_fast, false)
enddefine;

define :with_index_hook in_array(VARLIST, is_fast, INDEX);
    lvars VARLIST, is_fast, INDEX;
    array_loop(VARLIST, is_fast, INDEX)
enddefine;

define :for_extension in_region(VARLIST, is_fast);
    lvars VARLIST, is_fast;
    unless back(VARLIST) == [] then
        mishap(0, 'Only one variable allowed')
    endunless;
    ;;; Pretend have not read in_region
    "in_region" :: proglist -> proglist;
    ;;; fast_for form not used - ignore it
    array_loop([], false, front(VARLIST))
enddefine;

endsection;
