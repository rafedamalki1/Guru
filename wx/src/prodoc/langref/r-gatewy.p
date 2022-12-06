DEFINE VARIABLE db-types AS CHARACTER VIEW-AS SELECTION-LIST
                         INNER-CHARS 20 INNER-LINES 3 LABEL "Gateways". 

FORM
   db-types.

db-types:LIST-ITEMS = GATEWAYS.

UPDATE db-types.
