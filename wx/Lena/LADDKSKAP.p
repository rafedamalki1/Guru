/*LADDKSKAP.p PROGRAMMET LADDAR KABELSKÅP*/

DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.

   {muswait.i}
     
     {AMERICANEUROPEAN.I}
   SESSION:NUMERIC-FORMAT = "EUROPEAN".

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM e:\delad\pro9\guru\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM d:\elpool\delad\pro9\wrk\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LAPP" THEN DO:
      INPUT FROM G:\DELAD\PRO9\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\PRO10\berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM berskap.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE BERSKAP.
      ASSIGN.
      IMPORT BERSKAP.
      IF globforetag = "snat" THEN DO:
         IF BERSKAP.ENR NE "" AND SUBSTRING(BERSKAP.ENR,1,1) NE "E" THEN BERSKAP.ENR = "E" + BERSKAP.ENR.
      END.    
   END.
   INPUT CLOSE.

{EUROPEANAMERICAN.I}