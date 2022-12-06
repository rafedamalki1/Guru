/*P2KOM.p*/
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE arvar AS INTEGER NO-UNDO.
DEFINE VARIABLE startvar AS INTEGER NO-UNDO.
DEFINE VARIABLE slutvar AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD ARBKOD                 AS CHARACTER 
   FIELD BENAMNING              AS CHARACTER
   FIELD INGAREJ                AS CHARACTER.

DEFINE TEMP-TABLE tidin2
   FIELD ARBKOD                 AS CHARACTER 
   FIELD BENAMNING              AS CHARACTER
   FIELD INGAREJ                AS CHARACTER
   INDEX ARBKOD ARBKOD.

{AMERICANEUROPEAN.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "ELPA" THEN DO:
   ASSIGN
   /*arvar = 2008
   filnamn = "\\server04\d\elpool\elpnj\kalk\2007\P2KOM07.skv".
   arvar = 2009
   filnamn = "\\server04\d\elpool\elpnj\kalk\2008\P2KOM08.skv".
   arvar = 2010
   filnamn = "\\server04\d\elpool\elpnj\kalk\2009\P2KOM09.skv".
   arvar = 2011
   filnamn = "\\server04\d\elpool\elpnj\kalk\2010\skv\P2KOM10.skv".*/
   /*arvar = 2012
   filnamn = "X:\kalk\2011\skv\P2KOM11.skv".*/
   arvar = 2013
   filnamn = "\\server05\d\elpool\elplo\kalk\2012\skv\P2KOM12.skv".
END.
/*ELSE IF FORETAG.FORETAG = "VAST"  OR FORETAG.FORETAG = "VOST" OR FORETAG.FORETAG = "VSYD" OR
FORETAG.FORETAG = "VORD" THEN DO:
   ASSIGN
   arvar = 2008
   filnamn = "e:\delad\pro9\guru\p2kom04.skv".
END.
ELSE IF FORETAG.FORETAG = "GRAN" THEN DO:
   ASSIGN
   arvar = 2009
   filnamn = "d:\elpool\delad\pro9\wrk\P2KOM04.skv".
END.
ELSE IF FORETAG.FORETAG = "GKAL" THEN DO:
   ASSIGN
   arvar = 2008
   filnamn = "D:\DELAD\klient\PRO9\P2KOM04.skv".
END.
ELSE IF FORETAG.FORETAG = "SUND" OR FORETAG.FORETAG = "SNAT" THEN DO:
   ASSIGN
   arvar = 2008
   filnamn = "D:\DELAD\KLIENT\PRO9\P2KOM04.skv".
END.
ELSE DO:
   ASSIGN
   arvar = 2008
   filnamn = "C:\PRO9\GURU\WTID\P2KOM04.skv".
END. */
EMPTY TEMP-TABLE tidin NO-ERROR. 
EMPTY TEMP-TABLE tidin2 NO-ERROR. 

INPUT FROM VALUE(filnamn) NO-ECHO.
REPEAT:
   DO TRANSACTION: 
      CREATE tidin.
      ASSIGN.
      IMPORT DELIMITER ";" tidin   NO-ERROR.
   END.               
END.

FOR EACH tidin:
   FIND FIRST tidin2 WHERE tidin2.ARBKOD = tidin.ARBKOD USE-INDEX ARBKOD 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidin2 THEN DO:      
      CREATE tidin2.
      ASSIGN
      tidin2.ARBKOD = tidin.ARBKOD.      
      IF tidin.INGAREJ = "X" THEN tidin2.INGAREJ = tidin.BENAMNING.
      ELSE tidin2.BENAMNING = tidin.BENAMNING.
   END.
   ELSE DO:
      IF tidin.INGAREJ = "X" THEN tidin2.INGAREJ = tidin2.INGAREJ + chr(13) + tidin.BENAMNING.
      ELSE tidin2.BENAMNING = tidin2.BENAMNING + chr(13) + tidin.BENAMNING.      
   END.
END.
FOR EACH tidin2:
   IF SUBSTRING(tidin2.ARBKOD,6,1) = "-" THEN DO:
      ASSIGN
      startvar = INTEGER(SUBSTRING(tidin2.ARBKOD,4,2))
      slutvar = INTEGER(SUBSTRING(tidin2.ARBKOD,7,2)).
      DO WHILE startvar <= slutvar:
         DO TRANSACTION:         
            FIND FIRST LOP2 WHERE LOP2.ARBKOD = SUBSTRING(tidin2.ARBKOD,1,3) AND
            LOP2.LOPNR = startvar AND LOP2.KATAR = arvar
            EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE LOP2 THEN DO:
               SUBSTRING(LOP2.BENAMNING,60,1000) = " ".
               IF tidin2.INGAREJ NE "" THEN
               SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
               ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
            END.
            startvar = startvar + 1.
         END.
      END.
   END.
   ELSE IF SUBSTRING(tidin2.ARBKOD,6,1) = "," THEN DO:
      DO TRANSACTION:      
         FIND FIRST LOP2 WHERE LOP2.ARBKOD = SUBSTRING(tidin2.ARBKOD,1,3) AND
         LOP2.LOPNR = INTEGER(SUBSTRING(tidin2.ARBKOD,4,2)) AND LOP2.KATAR = arvar
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE LOP2 THEN DO:
            SUBSTRING(LOP2.BENAMNING,60,1000) = " ".
            IF tidin2.INGAREJ NE "" THEN
            SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
            ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
         END.
      END.
      DO TRANSACTION:      
         FIND FIRST LOP2 WHERE LOP2.ARBKOD = SUBSTRING(tidin2.ARBKOD,1,3) AND
         LOP2.LOPNR = INTEGER(SUBSTRING(tidin2.ARBKOD,7,2)) AND LOP2.KATAR = arvar
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE LOP2 THEN DO:
            SUBSTRING(LOP2.BENAMNING,60,1000) = " ".
            IF tidin2.INGAREJ NE "" THEN
            SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
            ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
         END.
      END.
   END.
   ELSE DO:
      IF SUBSTRING(tidin2.ARBKOD,4,2) = "00" THEN DO:
         OPEN QUERY pq FOR EACH LOP2 WHERE LOP2.KATAR = arvar AND
         LOP2.ARBKOD = SUBSTRING(tidin2.ARBKOD,1,3) NO-LOCK.
         DO TRANSACTION:
            GET FIRST pq EXCLUSIVE-LOCK.
            IF AVAILABLE lop2 THEN DO:
               SUBSTRING(LOP2.BENAMNING,60,1000) = " ".
               IF tidin2.INGAREJ NE "" THEN
               SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
               ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
            END.
         END.         
         REPEAT:
            DO TRANSACTION:
               GET NEXT pq EXCLUSIVE-LOCK.
               IF AVAILABLE LOP2 THEN DO:
                  SUBSTRING(LOP2.BENAMNING,60,1000) = " ".
                  IF tidin2.INGAREJ NE "" THEN
                  SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                  ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
               END.
               ELSE LEAVE.
            END.
         END.
       END.
      ELSE DO:
         DO TRANSACTION:         
            FIND FIRST LOP2 WHERE LOP2.ARBKOD = SUBSTRING(tidin2.ARBKOD,1,3) AND
            LOP2.LOPNR = INTEGER(SUBSTRING(tidin2.ARBKOD,4,2)) AND LOP2.KATAR = arvar
            EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE LOP2 THEN DO:
               SUBSTRING(LOP2.BENAMNING,60,1000) = " ".
               IF tidin2.INGAREJ NE "" THEN
               SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
               ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
            END.
         END.
      END.
   END.
END.
{EUROPEANAMERICAN.I}
