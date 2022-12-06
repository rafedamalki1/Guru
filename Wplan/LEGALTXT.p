/*LEGALTXT.P*/
&Scoped-define NEW 
&Scoped-define SHARED 
{TELENAMN.I}
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".
    
DEFINE INPUT PARAMETER infil AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER dlcvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER wtidvar AS CHARACTER NO-UNDO.       
    
DEFINE OUTPUT PARAMETER TABLE FOR tele_namn.
{AMERICANEUROPEAN.I}
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

/*    sokfil = SEARCH("infil").                                                              */
/*    OUTPUT TO "D:\delad\\PRO9\kalle.txt" APPEND CONVERT TARGET "ibm850" SOURCE "iso8859-1" */
/*    NO-ECHO.                                                                               */
/*    PUT UNFORMATTED                                                                        */
/*    dlcvar ";" wtidvar ";" infil ";" sokfil SKIP.                                          */
/*    OUTPUT CLOSE.                                                                          */

   OS-COMMAND SILENT VALUE(dlcvar)
   VALUE(infil) > VALUE(wtidvar).           
   INPUT FROM VALUE(wtidvar) NO-ECHO
   CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   /*iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME HHH WIDTH 80.
      CREATE tidin. 
      ASSIGN tidin.TIN = words.
   END.
   INPUT CLOSE.     
   OS-DELETE VALUE(wtidvar).     
         
   FIND FIRST tidin NO-LOCK NO-ERROR.  
   IF AVAILABLE tidin THEN DO TRANSACTION: 
      CREATE tele_namn.
      ASSIGN
      tele_namn.TELNR = SUBSTRING(tidin.TIN,1,12).
      tele_namn.NAMN = SUBSTRING(tidin.TIN,14,40).          
   END. 
   REPEAT:  
      FIND NEXT tidin NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidin THEN LEAVE.
      ELSE DO TRANSACTION:          
         CREATE tele_namn.
         ASSIGN
         tele_namn.TELNR = SUBSTRING(tidin.TIN,1,12).
         tele_namn.NAMN = SUBSTRING(tidin.TIN,14,40).   
      END.
   END.
{EUROPEANAMERICAN.I}