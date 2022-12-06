/* r-dserv.p */

DEFINE VARIABLE db-types AS CHARACTER VIEW-AS SELECTION-LIST
                         INNER-CHARS 20 INNER-LINES 3 LABEL "DataServers".
FORM
   db-types.

db-types:LIST-ITEMS = DATASERVERS.

UPDATE db-types. 
