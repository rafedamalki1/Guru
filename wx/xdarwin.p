   DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokfil2 AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER.
   def var a as integer.
   a = 0.
   /*OS-COMMAND SILENT "\\ntserver2\delad\pro8\dlc\bin\quoter.exe"
 *    "\\ntserver2\delad\elpnj\darwin\def.dbf" > "\\ntserver2\delad\elpnj\darwin\def.txt".*/           
   INPUT FROM "\\ntserver2\delad\elpnj\darwin\def.dbf" NO-ECHO
   CONVERT TARGET "iso8859-1" SOURCE "ibm850" NO-ECHO.
   /*iso8859-1 swedish-7-bit ibm850"*/
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME HHH WIDTH 80.
      CREATE tidin. 
      ASSIGN tidin.TIN = words.
      a = a + 1.
      display a substring(words,1,40).
   END.   
   INPUT CLOSE.
   OUTPUT TO "\\ntserver2\DELAD\elpnj\darwin\kalle.txt" APPEND CONVERT TARGET "iso8859-1" SOURCE "iso8859-1" 
         NO-ECHO.
   FOR EACH tidin:   
         put unformatted           
         tidin.TIN skip.         
         end.
   OUTPUT CLOSE.
