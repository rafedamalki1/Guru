DEFINE VARIABLE what-lib AS CHARACTER.
DEFINE VARIABLE location AS CHARACTER.
DEFINE VARIABLE myfile   AS CHARACTER FORMAT "x(16)" LABEL "R-code File".

SET myfile.
location = SEARCH(myfile).

IF location = ?
THEN DO:
   MESSAGE "Can't find" myfile.
   LEAVE.
END.

what-lib = LIBRARY(location).

IF what-lib <> ? 
THEN MESSAGE MEMBER(location) "can be found in library" what-lib.
ELSE MESSAGE myfile "is not in a library but is in" location.
