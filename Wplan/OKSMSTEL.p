/*OKSMSTEL.p*/
&Scoped-define NEW 
&Scoped-define SHARED 
{TELENAMN.I}   
DEFINE INPUT PARAMETER infil AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sparfil AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER intele AS LOGICAL NO-UNDO.       
   {AMERICANEUROPEAN.I} 
DEFINE INPUT PARAMETER TABLE FOR tele_namn.
   
   IF intele = TRUE THEN DO:
      FOR EACH tele_namn:      
         OUTPUT TO VALUE(sparfil) APPEND CONVERT TARGET "ibm850" SOURCE "iso8859-1" 
         NO-ECHO.           
         PUT UNFORMATTED        
         SUBSTRING(tele_namn.TELNR,1,12) + CHR(9) + 
         tele_namn.NAMN + CHR(10) AT 1.                
         OUTPUT CLOSE.
      END.    
   END.
   ELSE DO:
      FOR EACH tele_namn:      
         OUTPUT TO VALUE(sparfil) APPEND CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
         NO-ECHO.           
         PUT UNFORMATTED        
         tele_namn.NAMN + CHR(9) + 
         SUBSTRING(tele_namn.TELNR,1,12) + CHR(10) AT 1.                
         OUTPUT CLOSE.
      END.
   END.   
   OS-DELETE VALUE(infil).
   OS-APPEND VALUE(sparfil) VALUE(infil).  
   OS-DELETE VALUE(sparfil).
{EUROPEANAMERICAN.I}