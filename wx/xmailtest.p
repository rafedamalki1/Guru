/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: XMAILTEST.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.09.03 10:49 ELPAO   
     Modified: 
*/

{mail.i}


DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE c AS CHARACTER NO-UNDO.

i = 0.

   DO i = 1 TO 2:
      IF i = 1 THEN c = "os.txt".
      ELSE IF i = 2 THEN c = "test.txt".

      CREATE tempattach.
      tempattach.fil = SESSION:TEMP-DIRECTORY + c.
   END.

   RUN mailattach.p (INPUT TABLE tempattach).
