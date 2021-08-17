/*  --- University of Sussex POPLOG file -----------------------------------
 *  File:           $usepop/master/C.all/lib/data/blockdata.p
 *  Purpose:        put a collection of blocks world data into the database
 *  Author:         Aaron Sloman, Sep 27 1976
 *  Documentation:  TEACH * BLOCKS
 *  Related Files:
 */

applist([
    [block b1]
    [block b2]
    [block b3]
    [block b4]
    [block b5]
    [colour b1 red]
    [colour b2 red]
    [colour b3 green]
    [colour b4 blue]
    [on b1 b2]
    [on b2 b3]
    [on b5 b4]
    [at b3 here]
    [at b4 there]
    [human harold]
    [human margaret]
    [human ted]
    [loves harold margaret]
    [hates ted margaret]
    [loves margaret ted]
    [owns harold b1]
    [owns margaret b3]
    [owns ted b5]
    ],add);
  'data ready'=>;
