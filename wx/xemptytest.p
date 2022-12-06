/*
     Filename: XEMPTYTEST.P
      Created: 2004.03.23 15:47ELPAO     
     Modified: 
*/

DEFINE VARIABLE i AS INTEGER NO-UNDO.

DEFINE TEMP-TABLE kalletemp
   FIELD PELLE AS INTEGER.

i = 0.
DO WHILE i < 100000:
   CREATE kalletemp.
   kalletemp.PELLE = i.
   i = i + 1.
END.

ETIME(YES).

FOR EACH kalletemp:
   kalletemp.PELLE = kalletemp.PELLE * 2.
   kalletemp.PELLE = kalletemp.PELLE + 2.
/*    DELETE kalletemp. */
END.
EMPTY TEMP-TABLE kalletemp NO-ERROR.


DISP ETIME.

