/* --- Copyright University of Sussex 1994. All rights reserved. ----------
 > File:            C.unix/lib/auto/syssocket.p
 > Purpose:         Old Unix socket procedure name
 > Author:          John Gibson May  6 1994
 > Documentation:   REF * OBSOLETE
 */
compile_mode :pop11 +strict;

section;

uses unix_sockets;

define syssocket(af, type, protocol, org);
    lvars af, type, protocol, org;
    sys_socket(af, type, if protocol /== 0 then
                            consword(protocol sys_>< nullstring)
                         endif, org)
enddefine;

endsection;
