/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/src/plog_process.p
 > Purpose:
 > Author:          John Gibson, Feb 22 1988 (see revisions)
 */

;;; -------------------- PROLOG PROCESSES -----------------------------------

#_INCLUDE 'declare.ph'
#_INCLUDE 'process.ph'
#_INCLUDE 'gctypes.ph'

vars
        _plog_trail_sp, _plog_trail_lim, _plog_trail_barrier,
        _plog_contn_barrier
    ;

section $-Sys$-Plog;

constant
        procedure (Create_barrier, Set_barrier, Unset_barrier, Area_expand),
    ;

vars
        barrier_count
    ;

endsection;


;;; ---------------------------------------------------------------------

section $-Sys$-Plog;

    ;;; prolog process state key
lconstant
    plog_proc_state_key = struct KEY_GC =>> {%
        _NULL,                      ;;; K_GC_RELOC
        key_key,                    ;;; KEY
        _:M_K_WRITEABLE,            ;;; K_FLAGS
        _:GCTYPE_PLOG_PROC_STATE,   ;;; K_GC_TYPE
        Rawstruct_getsize           ;;; K_GET_SIZE
        %};


    ;;; restore a process prolog state
define Proc_restore(_plgstate);
    lvars   _plgstate, _trailsize, _contnsize, _diff;
    Set_barrier();
    _plgstate!PLGPS_TRAIL_SIZE -> _trailsize;
    _plgstate!PLGPS_CONTN_SIZE -> _contnsize;
    (_trailsize _add _contnsize)
            _sub @@(w){_plog_trail_lim, _plog_trail_barrier} -> _diff;
    if _diff _sgr _0 then
        ;;; need more space in prolog area
        Area_expand(##(w){_diff})
    endif;
    _plgstate!PLGPS_COUNT -> barrier_count;
    _plgstate@PLGPS_TRAIL -> _plgstate;
    _moveq(_trailsize, _plgstate, _plog_trail_barrier) -> _plog_trail_barrier;
    _plog_contn_barrier@(w)-{_contnsize} -> _plog_contn_barrier;
    _moveq(_contnsize, _plgstate@(w){_trailsize}, _plog_contn_barrier) ->;
    Unset_barrier()
enddefine;

    ;;; save a process prolog state
define Proc_save(proc, _barcount);
    lvars   proc, n, _plgstate, _contnsize, _trailsize, _size,
            _trail_barrier, _contn_barrier, _trail_end, _contn_end,
            _barcount, _save;
    ;;; find the trail barrier and contn barrier for the proc
    _plog_trail_barrier -> _trail_barrier;
    _plog_contn_barrier -> _contn_barrier;
    _barcount -> n;
    until n == 1 do
        _int(_trail_barrier--!(w) -> _trail_barrier) -> _size;
        _trail_barrier@(w)-{_size} -> _trail_barrier;
        (_contn_barrier@(w)++)!(w)++ -> _contn_barrier -> _size;
        _contn_barrier@(w){_size} -> _contn_barrier;
        n fi_- 1 -> n
    enduntil;

    ;;; create trail and contn barriers without changing the prolog globals
    ;;; (in particular we must not change _plog_trail_barrier, because if the
    ;;; Get_store below causes a gc, then the extra barrier will not have a
    ;;; corresponding call of prolog_barrier_apply)
    Create_barrier() -> _contn_end -> _trail_end;

    ;;; compute trail and contn sizes and see if new state needed
    @@(w){_trail_end, _trail_barrier} -> _trailsize;
    @@(w){_contn_barrier, _contn_end} -> _contnsize;
    @@PLGPS_TRAIL{_trailsize _add _contnsize} _sub @@POPBASE -> _size;
    proc!PS_PLOG_STATE -> _plgstate;
    if not(_plgstate) or _plgstate!RAW_SIZE _lt _size then
        ;;; need a new state
        ;;; -- zap the old one so it'll get garbaged if a gc occurs
        false -> proc!PS_PLOG_STATE;
        ;;; save this to test for whether it's the same after
        _plog_trail_sp -> _save;
        Get_store(_size) -> _plgstate;
        if _plog_trail_sp /== _save then
            ;;; a gc happened and the trail shrunk
            ;;; reclaim the state record and start over ...
            _plgstate -> Get_store();
            chain(proc, _barcount, Proc_save)
        endif;
        plog_proc_state_key -> _plgstate!KEY;
        _size -> _plgstate!RAW_SIZE;
        proc!PS_PERM_FLAGS -> _plgstate!PLGPS_PERM_FLAGS;
        _plgstate -> proc!PS_PLOG_STATE
    endif;

    ;;; set the sizes for the state
    _trailsize -> _plgstate!PLGPS_TRAIL_SIZE;
    _contnsize -> _plgstate!PLGPS_CONTN_SIZE;
    _barcount -> _plgstate!PLGPS_COUNT;
    ;;; move in the saved stack parts
    _moveq(_trailsize, _trail_barrier, _plgstate@PLGPS_TRAIL) -> _plgstate;
    _moveq(_contnsize, _contn_end, _plgstate) -> ;

    ;;; remove the saved parts from the stacks
    _trail_barrier -> _plog_trail_barrier;
    _contn_barrier -> _plog_contn_barrier;
    if (barrier_count fi_- _barcount ->> barrier_count) == 0 then
        false -> barrier_count
    endif;
    Unset_barrier()
enddefine;

endsection;     /* $-Sys$-Plog */



/* --- Revision History ---------------------------------------------------
--- John Gibson, Oct 24 1996
        Changed plog_proc_state_key to use Rawstruct_getsize.
--- John Gibson, Jul 12 1991
        Made -Proc_save- reduce -barrier_count- correctly
--- John Gibson, Dec 20 1989
        Changes for new pop pointers
--- John Gibson, Sep 15 1989
        Some changes to -Proc_save- as part of process overhaul.
--- John Gibson, Apr 10 1989
        Stuff in plogcore.p into Sys$-Plog
--- John Gibson, Feb 25 1988
        New file from routines previously in process.p
 */
