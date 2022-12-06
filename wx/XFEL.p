
                               
DEFINE VARIABLE infil AS CHARACTER NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(256)".

   ASSIGN 
   infil = "\\GRANGURU\guru_ser\server\SMS\WORK_SMS\address.txt"   
   dlcvar = "\\GRANGURU\guru_ser\klient\pro8\dlc\bin\QUOTER.EXE"
   wtidvar = SESSION:TEMP-DIRECTORY
   wtidvar = wtidvar + "sms_medd.q".  
   
   OS-COMMAND SILENT VALUE(dlcvar)                                           
   VALUE(infil) > VALUE(wtidvar).                       
   INPUT FROM VALUE(wtidvar) NO-ECHO
   CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" NO-ECHO.
   /*iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME HHH WIDTH 80.
      CREATE tidin. 
      ASSIGN tidin.TIN = words.
   END.      
   INPUT CLOSE.     
   
