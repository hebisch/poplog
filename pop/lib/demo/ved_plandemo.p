/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/demo/ved_plandemo.p
 *  Purpose:        some kind of continous demonstration of planning
 *  Author:         Chris Mellish, ???
 *  Documentation:
 *  Related Files:
 */

define pause(n);
    charout(0);
    rawcharout(0);
    syssleep(n * 500)
enddefine;

define ved_plandemo();
vars pr vedargument vederror;
    syspr -> pr;
repeat                
    vedscreenclear();
    rawcharout(0);
    pr('\n\^[H');
    pr('This demonstration illustrates two ways a robot might plan\n');
    pr('how to manipulate some blocks on a table. The robot is\n');
    pr('holding block 3 (b3), and wants to put block  1 on block 2\n');
    pr('The blocks are arranged as:\n\n');
    pr('                        |\n');
    pr('                        |\n');
    pr('                        H\n');
    pr('                     -------\n');
    pr('                    |       |\n');
    pr('                    |  b3   |\n');
    pr('                    |       |\n');
    pr('                     -------\n');
    pr('      ------\n');
    pr('     |      |\n');
    pr('     |  b2  |                                 ---------\n');
    pr('    ----------                               |         |\n');
    pr('   |          |                     ---------|         |\n');
    pr('   |    b1    |                    |   b4    |    b5   |\n');
    pr('   |          |                    |         |         |\n');
    pr('    ----------                      --------- ---------\n');
    pr('TABLETABLETABLETABLETABLETABLETABLETABLETABLETABLETABLETABLE\n');
    pause(3);
    'b1 on b2' -> vedargument;
    ved_strips();
    pause(2);
    vedscreenclear();
    rawcharout(0);
    pr('\n\^[H\nAnd now the same problem done by a different technique\n');
    pause(1);
    'b1 on b2' -> vedargument;
    ved_astar();
    pause(2);
    vedscreenclear();
    rawcharout(0);
    pr('\n\^[HFor more explanation of all this, try out\n');
    pr('demonstration program 10 on one of the VDUs.\n');
    pause(2)
endrepeat;
enddefine;
