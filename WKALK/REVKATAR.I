/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: REVKATAR.I
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.06.26 11:02 ELPAO   
     Modified: 
*/


DEFINE VARIABLE kat1 AS INTEGER NO-UNDO.
DEFINE VARIABLE kat2 AS INTEGER NO-UNDO.
FUNCTION katar RETURNS INTEGER  (INPUT inkatar AS INTEGER):
   IF inkatar < 9999 THEN RETURN inkatar. 

   RETURN  INTEGER(SUBSTRING(STRING(inkatar),2,4)).

END FUNCTION.

FUNCTION revkatar RETURNS INTEGER  (INPUT inkatar AS INTEGER):
   IF inkatar < 9999 THEN RETURN 0. 

   RETURN  INTEGER(SUBSTRING(STRING(inkatar),1,1)).

END FUNCTION.
/*
kat1 = 2009.
kat2 = katar(kat1).
MESSAGE kat1 kat2 VIEW-AS ALERT-BOX.

kat2 = revkatar(kat1).
MESSAGE kat1 kat2 VIEW-AS ALERT-BOX.
*/
   



