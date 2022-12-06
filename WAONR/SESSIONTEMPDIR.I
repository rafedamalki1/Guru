/*SESSIONTEMPDIR.I*/
DEFINE VARIABLE webclienttempdir AS CHARACTER NO-UNDO.

DEFINE VARIABLE osvarnum AS INTEGER NO-UNDO.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      IF OS-GETENV("SystemDrive") NE ? THEN webclienttempdir = OS-GETENV("SystemDrive") + "\temp\".
      ELSE webclienttempdir = "c:\temp\".
      OS-CREATE-DIR VALUE(webclienttempdir) . 
      OS-CREATE-DIR VALUE(webclienttempdir + Guru.Konstanter:globanv) .
      
      IF OS-ERROR NE 0  THEN DO:
         webclienttempdir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv.
         OS-CREATE-DIR VALUE(webclienttempdir).
         webclienttempdir = webclienttempdir + "\".  
              
      END.
      ELSE webclienttempdir = webclienttempdir + Guru.Konstanter:globanv + "\".
              
   END.
