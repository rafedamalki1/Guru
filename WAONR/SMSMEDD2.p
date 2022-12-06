/*SMSMEDD2.P SKICKA SMS-MEDDELANDE VIA MEDDELANDE FUNKTIONEN*/
DEFINE VARIABLE sokfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE wtidvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE sparfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE valpers      
   FIELD NAMN AS CHARACTER FORMAT "X(40)"      
   FIELD TELNR AS CHARACTER FORMAT "X(12)".

DEFINE INPUT PARAMETER medvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR valpers.
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
  {AMERICANEUROPEAN.I} 
   IF globforetag = "ELPA" THEN DO:    
      ASSIGN                 
      sparfil = "\\GRANGURU\guru_ser\klient\pro9\" + "sms_spar2.txt"
      wtidvar = SESSION:TEMP-DIRECTORY
      wtidvar = wtidvar + "sms_medd.q".       
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      ASSIGN
      sparfil = {GRANSMS.I} + "Result\sms_spar2.txt"
      wtidvar = SESSION:TEMP-DIRECTORY
      wtidvar = wtidvar + "sms_medd.q".
   END.
      
   OUTPUT TO VALUE(wtidvar) APPEND CONVERT TARGET "ibm850" SOURCE "iso8859-1" 
   NO-ECHO.           
   FOR EACH valpers NO-LOCK:          
      PUT UNFORMATTED        
      SUBSTRING(valpers.TELNR,1,12) + CHR(9) + 
      medvar + CHR(13) AT 1.                
   END.   
   OUTPUT CLOSE.      
   OS-APPEND VALUE(wtidvar) VALUE(sparfil).
   OS-DELETE VALUE(wtidvar).

{EUROPEANAMERICAN.I}