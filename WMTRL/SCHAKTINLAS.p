/*SCHAKTINLAS.P*/
DEFINE VARIABLE rakn AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(68)" NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER  NO-UNDO.

{AMERICANEUROPEAN.I}
SESSION:NUMERIC-FORMAT = "AMERICAN".
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
 Guru.Konstanter:globforetag = FORETAG.FORETAG.

IF  Guru.Konstanter:globforetag = "FORS"  THEN DO:      
   prognamn = "C:\DELAD\PRO9\GURU\". 
END.
ELSE IF  Guru.Konstanter:globforetag = "PSNK"  THEN DO:      
   prognamn = "C:\delad\hamta\". 
END.

ELSE prognamn =  "C:\delad\hamta\".   
 
prognamn2 = prognamn + "hdhandel.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE HDHANDELSE.
      ASSIGN.
      IMPORT HDHANDELSE.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "ytbelagg.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE YTBELAGG.
      ASSIGN.
      IMPORT YTBELAGG.           
   END.
   INPUT CLOSE.
   
   prognamn2 = prognamn + "forlagg.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE FORLAGG.
      ASSIGN.
      IMPORT FORLAGG.           
   END.
   INPUT CLOSE.   
   
   prognamn2 = prognamn + "hdkopp.d".      
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
   
   REPEAT:
      CREATE HDKKOPP.
      ASSIGN.
      IMPORT HDKKOPP.           
   END.
   INPUT CLOSE.   