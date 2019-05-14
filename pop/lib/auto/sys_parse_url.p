/* --- Copyright University of Sussex 1998. All rights reserved. ----------
 > File:            C.all/lib/auto/sys_parse_url.p
 > Purpose:
 > Author:          John Gibson, Oct  8 1998
 > Documentation:
 > Related Files:
 */
compile_mode :pop11 +strict;

section;

define sys_parse_url(url) -> (scheme, url);
    lvars url, n, scheme = false, url_path, user, port;
    sys_split_url(`:`, url, 2:000) -> (scheme, url);
    returnunless(scheme);
    uppertolower(scheme) -> scheme;
    returnunless(isstartstring('//', url));

    ;;; Common IP scheme //<user>:<password>@<host>:<port>/<url-path>
    sys_split_url(`/`, allbutfirst(2, url), 2:100) -> (url, url_path);
    sys_split_url(`@`, url, 2:000) -> (user, url);
    sys_split_url(`:`, url, 2:110) -> (url, port);
    (port and (strnumber(port)->>port) and {^port tcp}) or scheme -> port;
    [%  (user and [% sys_split_url(`:`, user, 2:111) %]) :: [^url ^port],
        url_path or nullstring
    %] -> url
enddefine;

endsection;
