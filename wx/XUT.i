  /*XUT.I*/
 
 /*funkar inte i runtime*/ 
DEFINE VARIABLE lgh AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn1 AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE progvag AS CHARACTER FORMAT "X(20)" NO-UNDO.
OUTPUT TO VALUE(vart) APPEND.
PUT 
"OUTPUT TO " AT 1 "{2}" " convert target " '"iso8859-1"' " source " '"iso8859-1".' SKIP
  "FOR EACH " "{1}" ":" SKIP
   "EXPORT " "{1}" "." SKIP
"END." SKIP  
"OUTPUT CLOSE." SKIP. 
 
