/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/plog/auto/time.pl
 *  Purpose:        timing goals in Prolog
 *  Author:         Roger Evans, July 1983
 *  Documentation:  HELP * TIME
 *  Related Files:
 */

time(P) :- GCstart is valof(popgctime),
           Start is apply(valof(systime)),
           call(P),
           End is apply(valof(systime)),
           GCend is valof(popgctime),
           Time is (End - Start)/100,
           GCtime is (GCend - GCstart)/100,
           functor(P,F,N),
           write('Time for '),write(F),nl,
           tab(4),write(Time),write(' secs (CPU)'),nl,
           tab(4),write(GCtime),write(' secs (GC)'),nl,!.

time(P) :- write('Timed goal failed - '),write(P),nl,!.

times(P) :- repeat, time(P), fail.
