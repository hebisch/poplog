/* --- Copyright University of Sussex 1996. All rights reserved. ----------
 > File:            C.all/lib/ved/ved_strip.p
 > Purpose:         Strip attributes, special chars and embedded data
 >                  from a Ved buffer
 > Author:          John Gibson, Aug  9 1996
 > Documentation:   REF * VEDCOMMS
 */
compile_mode :pop11 +strict;

section;

define ved_strip;
    lvars   linebuf = nullstring, pos, count = 0, inline = 1, string, dev,
            procedure (linecons, linerep);

    define read(dev, bsub, buf, nbytes) -> nbytes;
        if count == 0 then
            ;;; write next line into linebuf
            returnif(inline > vvedbuffersize) (0 -> nbytes);
            linecons(subscrv(inline,vedbuffer));
            inline + 1 -> inline
        endif;
        if count < nbytes then count -> nbytes endif;
        move_bytes(pos, linebuf, bsub, buf, nbytes);
        count - nbytes -> count;
        pos + count -> pos
    enddefine;

    ;;; only called once for each line
    define write(dev, bsub, buf, nbytes);
        unless datalength(linebuf) >= nbytes then
            inits(nbytes*2) -> linebuf
        endunless;
        move_bytes(bsub, buf, 1, linebuf, nbytes);
        1 -> pos;
        nbytes -> count
    enddefine;


    consdevice(nullstring, nullstring, true, 0,
                {{^read ^erase ^erase} {^write ^erase} ^false ^false})
                        -> dev;

    vedfile_line_consumer(dev, if vedargument = nullstring then 1
                               else strnumber(vedargument)
                               endif) -> linecons;
    vedfile_line_repeater(dev) -> linerep;

    vedpositionpush();
    vedtopfile();

    until (linerep() ->> string) == termin do
        string -> vedthisline();
        vednextline()
    enduntil;
    while vedline <= vvedbuffersize do
        nullstring -> vedthisline();
        vednextline()
    endwhile;

    vedpositionpop();
    vedrefresh();

    if vedchanged then vedchanged+1 else 1 endif -> vedchanged
enddefine;

endsection;
