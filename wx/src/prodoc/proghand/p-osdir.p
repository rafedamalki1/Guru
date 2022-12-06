/* p-osdir.p */

DEFINE VARIABLE search-dir AS CHARACTER.
DEFINE VARIABLE file-name AS CHARACTER FORMAT "x(16)" LABEL "File". 
DEFINE VARIABLE attr-list AS CHARACTER FORMAT "x(4)" LABEL "Attributes".

search-dir = OS-GETENV("DLC").

INPUT FROM OS-DIR(search-dir).
 
REPEAT:
   SET file-name ^ attr-list
         WITH WIDTH 70 USE-TEXT TITLE "Contents of " + search-dir.
    
END.

INPUT CLOSE.
