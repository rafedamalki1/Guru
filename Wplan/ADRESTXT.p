&Scoped-define NEW 
&Scoped-define SHARED 
{TELENAMN.I}
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE melvar AS INTEGER NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".
   
   
DEFINE INPUT PARAMETER infil AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER dlcvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER wtidvar AS CHARACTER NO-UNDO.       
    
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR tele_namn.
   EMPTY TEMP-TABLE tidin NO-ERROR.    
   sokfil = SEARCH("QUOTER.EXE").
   IF sokfil = ? THEN DO:
      RETURN. 
   END.
   ELSE DO:
      dlcvar = sokfil.
   END.
   ASSIGN
   wtidvar = SESSION:TEMP-DIRECTORY
   wtidvar = wtidvar + "sms_medd.q".
   OS-COMMAND SILENT VALUE(dlcvar)
   VALUE(infil) > VALUE(wtidvar).           
   INPUT FROM VALUE(wtidvar) NO-ECHO
   CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" NO-ECHO.
   /*iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME GGG WIDTH 80.
      CREATE tidin. 
      ASSIGN tidin.TIN = words.
   END.
   INPUT CLOSE.     
   OS-DELETE VALUE(wtidvar).     
         
   FIND FIRST tidin NO-LOCK NO-ERROR.  
   IF AVAILABLE tidin THEN DO TRANSACTION: 
      melvar = INDEX(tidin.TIN,CHR(9),1).                    
      FIND FIRST tele_namn WHERE tele_namn.TELNR = SUBSTRING(tidin.TIN,melvar + 1,12)
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tele_namn THEN DO:
         CREATE tele_namn.
         ASSIGN
         tele_namn.NAMN = SUBSTRING(tidin.TIN,1,melvar - 1)
         tele_namn.TELNR = SUBSTRING(tidin.TIN,melvar + 1,12).          
      END.   
   END. 
   REPEAT:  
      FIND NEXT tidin NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidin THEN LEAVE.
      ELSE DO TRANSACTION:          
         melvar = INDEX(tidin.TIN,CHR(9),1).                    
         FIND FIRST tele_namn WHERE tele_namn.TELNR = SUBSTRING(tidin.TIN,melvar + 1,12)
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tele_namn THEN DO:
            CREATE tele_namn.
            ASSIGN
            tele_namn.NAMN = SUBSTRING(tidin.TIN,1,melvar - 1)
            tele_namn.TELNR = SUBSTRING(tidin.TIN,melvar + 1,12).          
         END.   
      END.
   END.
