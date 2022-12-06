/*VP2KOMN.p*/
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE arvar AS INTEGER NO-UNDO.
DEFINE VARIABLE startvar AS INTEGER NO-UNDO.
DEFINE VARIABLE slutvar AS INTEGER NO-UNDO.
DEFINE VARIABLE spos AS INTEGER NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER  NO-UNDO.
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD HJKOD                 AS CHARACTER
  /* FIELD LOPNR                  AS CHARACTER*/    
   FIELD BENAMNING              AS CHARACTER
   FIELD INGAREJ                AS CHARACTER.
DEFINE TEMP-TABLE tidin3
   FIELD HJKOD                 AS CHARACTER
   FIELD ARBKOD                 AS CHARACTER
   FIELD LOPNR                  AS CHARACTER    
   FIELD BENAMNING              AS CHARACTER
   FIELD INGAREJ                AS CHARACTER.

DEFINE TEMP-TABLE tidin2
   FIELD ARBKOD                 AS CHARACTER
   FIELD LOPNR                  AS CHARACTER 
   FIELD BENAMNING              AS CHARACTER
   FIELD INGAREJ                AS CHARACTER
   INDEX ARBKOD ARBKOD LOPNR.

{AMERICANEUROPEAN.I}
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.

IF    globforetag = "VAST"  OR 
    globforetag = "VNAT" THEN DO:
      prognamn = "e:\delad\pro9\guru\".
   END.
   ELSE IF globforetag = "VELK"   OR globforetag = "GETB"  OR globforetag = "BHEL"   OR globforetag = "GREL" OR  globforetag = "KEWA"  THEN DO:
      prognamn = "C:\PRO10\GURU\WTID\".      
   END.
   ELSE IF  globforetag = "OVIK" OR globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"  THEN DO:
      prognamn = "C:\PRO10\GURU\WTID\".
   END.
   ELSE IF  globforetag = "GKAL" THEN DO:
      prognamn = "D:\DELAD\klient\PRO9\".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      prognamn = "d:\elpool\delad\pro9\wrk\".      
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:                 
      prognamn = "D:\DELAD\KLIENT\PRO10\GURU\WTID\".
   END.
   ELSE IF globforetag = "UMEA" THEN DO:
      prognamn = "D:\DELAD\PRO9\GURU\".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      prognamn = "D:\GURU\PRO9\GURU\WTID\".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      prognamn = "D:\ELPOOL\DELAD\PRO9\". 
   END.
   ELSE IF globforetag = "STRA" THEN DO:      
      prognamn = "E:\DELAD\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "ALTE" OR globforetag = "FORS"  THEN DO:      
      prognamn = "C:\DELAD\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "ELKB" OR globforetag = "ORBI" OR globforetag = "SKOK" OR globforetag = "HANA" OR globforetag = "ATS" 
   OR globforetag = "JSBF" OR globforetag = "KRAF" OR  globforetag = "PICA" OR globforetag = "SWEO" OR globforetag = "OXEL" OR globforetag = "NYLB" OR globforetag = "ELPC" OR globforetag = "POLA" OR globforetag = "KNOR" THEN DO:      
      prognamn = "C:\DELAD\PRO10\GURU\WTID\". 
   END.
   ELSE IF globforetag = "SKEK" THEN DO:
      prognamn = "C:\DELAD\PRO10\GURU\WTID\". 
   END.   
   ELSE IF globforetag = "tras" THEN DO:      
      prognamn = "C:\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "LAPP" OR globforetag = "REJI" THEN DO:
      prognamn = "C:\ELPOOL\DELAD\PRO10\GURU\WTID\".       
   END.
   ELSE IF globforetag = "TECT" THEN DO:      
      prognamn = "D:\SHARED\elpool\PRO9\GURU\WTID\". 
   END.
   ELSE IF globforetag = "BODE" THEN DO:
      prognamn = "C:\ELPOOL\DELAD\PRO9\GURU\WTID\". 
   END.   
   ELSE IF globforetag = "ELPA"  THEN DO:
      /*prognamn = "C:\Pro10\GURU\WTID\".*/ 
      /*prognamn = "\\server05\d\elpool\elplo\kalk\2014\KLG2\skv\".*/
      prognamn = "\\SERVER05\d\elpool\elplo\kalk\2015\KLG2\SKV\".
      
   END.
   ELSE DO:
      prognamn = "C:\Pro10\GURU\WTID\". 
   END.
   
   /*IF globforetag = "GRAN" THEN arvar = 2016.
   ELSE arvar = 2015.*/
   IF globforetag = "GRAN" THEN arvar = 2017.
   ELSE arvar = 2016.   
  
   /*filnamn = prognamn + "VP2KOM2014.skv".*/
   filnamn = prognamn + "VP2KOM2015.skv".      
   
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

EMPTY TEMP-TABLE tidin3 NO-ERROR. 
FOR EACH tidin:
   CREATE tidin3.
   BUFFER-COPY tidin TO tidin3.
   ASSIGN
   tidin3.ARBKOD = SUBSTRING(tidin3.HJKOD,1,3)
   tidin3.LOPNR = TRIM(SUBSTRING(tidin3.HJKOD,4)).
    
END.

FOR EACH tidin3:
   IF tidin3.BENAMNING = "" THEN .
   ELSE DO:
      FIND FIRST tidin2 WHERE tidin2.ARBKOD = tidin3.ARBKOD AND tidin2.LOPNR = tidin3.LOPNR USE-INDEX ARBKOD 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidin2 THEN DO:      
         CREATE tidin2.
         ASSIGN
         tidin2.ARBKOD = tidin3.ARBKOD.
         tidin2.LOPNR = tidin3.LOPNR.               
         IF tidin3.INGAREJ = "X" THEN tidin2.INGAREJ = tidin3.BENAMNING.
         ELSE tidin2.BENAMNING = tidin3.BENAMNING.
      END.
      ELSE DO:
         IF tidin3.INGAREJ = "X" THEN tidin2.INGAREJ = tidin2.INGAREJ + chr(13) + tidin3.BENAMNING.
         ELSE tidin2.BENAMNING = tidin2.BENAMNING + chr(13) + tidin3.BENAMNING.      
      END.
   END.   
END.

/*/*RENSA INNAN KÖRNING*/
FOR EACH LOP2 WHERE LOP2.KATAR = arvar  EXCLUSIVE-LOCK :         
   SUBSTRING(LOP2.BENAMNING,60,1000) = " ".            
END.*/

FOR EACH tidin2:  
   IF SUBSTRING(tidin2.LOPNR,1,1) = "." THEN spos = 1.
   ELSE spos = 0.
    /*IF SUBSTRING(tidin2.ARBKOD,6,1) = "-" THEN DO:*/    
   IF SUBSTRING(tidin2.LOPNR,3 + spos,1) = "-" THEN DO:
      ASSIGN
      startvar = INTEGER(SUBSTRING(tidin2.LOPNR,1 + spos,2))
      slutvar = INTEGER(SUBSTRING(tidin2.LOPNR,4 + spos,2)).
      DO WHILE startvar <= slutvar:
         DO TRANSACTION:         
            FIND FIRST LOP2 WHERE LOP2.ARBKOD = " " + SUBSTRING(tidin2.ARBKOD,1,2) AND
            LOP2.LOPNR = startvar AND LOP2.KATAR = arvar
            EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE LOP2 THEN DO:
               /*SUBSTRING(LOP2.BENAMNING,60,1000) = " ".*/
               IF SUBSTRING(LOP2.BENAMNING,60) = "" THEN DO:
                  IF tidin2.INGAREJ NE "" THEN
                  SUBSTRING(LOP2.BENAMNING,60) =  tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                  ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
               END.
               ELSE DO:
                  IF tidin2.INGAREJ NE "" THEN
                  SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                  ELSE SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING.               
               END.               
            END.
            startvar = startvar + 1.
         END.
      END.
   END.
   /*ELSE IF SUBSTRING(tidin2.ARBKOD,6,1) = "," THEN DO:*/
   ELSE IF SUBSTRING(tidin2.LOPNR,3 + spos,1) = "," THEN DO:
      DO TRANSACTION:      
         FIND FIRST LOP2 WHERE LOP2.ARBKOD = " " + SUBSTRING(tidin2.ARBKOD,1,2) AND
         LOP2.LOPNR = INTEGER(SUBSTRING(tidin2.LOPNR,1 + spos,2)) AND LOP2.KATAR = arvar
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE LOP2 THEN DO:
            /*koden kan finnas flera gånger tex 135 .21-22 135 .21
            SUBSTRING(LOP2.BENAMNING,60,1000) = " ".*/
            IF SUBSTRING(LOP2.BENAMNING,60) = "" THEN DO:
               IF tidin2.INGAREJ NE "" THEN
               SUBSTRING(LOP2.BENAMNING,60) =  tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
               ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
            END.
            ELSE DO:
               IF tidin2.INGAREJ NE "" THEN
               SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
               ELSE SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING.               
            END.      
         END.
      END.
      DO TRANSACTION:      
         FIND FIRST LOP2 WHERE LOP2.ARBKOD = " " + SUBSTRING(tidin2.ARBKOD,1,2) AND
         LOP2.LOPNR = INTEGER(SUBSTRING(tidin2.LOPNR,4 + spos,2)) AND LOP2.KATAR = arvar
         EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE LOP2 THEN DO:
            /*SUBSTRING(LOP2.BENAMNING,60,1000) = " ".*/
            IF SUBSTRING(LOP2.BENAMNING,60) = "" THEN DO:
               IF tidin2.INGAREJ NE "" THEN
               SUBSTRING(LOP2.BENAMNING,60) =  tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
               ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
            END.
            ELSE DO:
               IF tidin2.INGAREJ NE "" THEN
               SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
               ELSE SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING.               
            END.      
         END.
      END.
   END.
   ELSE DO:               
      /*IF SUBSTRING(tidin2.ARBKOD,4,2) = "00" THEN DO:*/
      IF tidin2.LOPNR = "" THEN DO:
         OPEN QUERY pq FOR EACH LOP2 WHERE LOP2.KATAR = arvar AND
         LOP2.ARBKOD = " " + SUBSTRING(tidin2.ARBKOD,1,2) NO-LOCK.
         DO TRANSACTION:
            GET FIRST pq EXCLUSIVE-LOCK.
            IF AVAILABLE lop2 THEN DO:
               /*SUBSTRING(LOP2.BENAMNING,60,1000) = " ".*/
               IF SUBSTRING(LOP2.BENAMNING,60) = "" THEN DO:
                  IF tidin2.INGAREJ NE "" THEN
                  SUBSTRING(LOP2.BENAMNING,60) =  tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                  ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
               END.
               ELSE DO:
                  IF tidin2.INGAREJ NE "" THEN
                  SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                  ELSE SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING.               
               END.   
            END.
         END.         
         REPEAT:
            DO TRANSACTION:
               GET NEXT pq EXCLUSIVE-LOCK.
               IF AVAILABLE LOP2 THEN DO:
                  /*SUBSTRING(LOP2.BENAMNING,60,1000) = " ".*/
                  IF SUBSTRING(LOP2.BENAMNING,60) = "" THEN DO:
                     IF tidin2.INGAREJ NE "" THEN
                     SUBSTRING(LOP2.BENAMNING,60) =  tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                     ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
                  END.
                  ELSE DO:
                     IF tidin2.INGAREJ NE "" THEN
                     SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                     ELSE SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING.               
                  END.   
               END.
               ELSE LEAVE.
            END.
         END.
       END.
      ELSE DO:
         DO TRANSACTION:         
            FIND FIRST LOP2 WHERE LOP2.ARBKOD = " " + SUBSTRING(tidin2.ARBKOD,1,2) AND
            LOP2.LOPNR = INTEGER(SUBSTRING(tidin2.LOPNR,1 + spos,2)) AND LOP2.KATAR = arvar
            EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE LOP2 THEN DO:
               /*SUBSTRING(LOP2.BENAMNING,60,1000) = " ".*/
               IF SUBSTRING(LOP2.BENAMNING,60) = "" THEN DO:
                  IF tidin2.INGAREJ NE "" THEN
                  SUBSTRING(LOP2.BENAMNING,60) =  tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                  ELSE SUBSTRING(LOP2.BENAMNING,60) = tidin2.BENAMNING.
               END.
               ELSE DO:
                  IF tidin2.INGAREJ NE "" THEN
                  SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING + chr(13) + chr(13) + "Ingår ej:" + tidin2.INGAREJ.
                  ELSE SUBSTRING(LOP2.BENAMNING,60) = SUBSTRING(LOP2.BENAMNING,60) + chr(13) + tidin2.BENAMNING.               
               END.
            END.               
         END.
      END.
   END.
END.
{EUROPEANAMERICAN.I}
