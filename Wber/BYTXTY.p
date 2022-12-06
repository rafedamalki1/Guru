/*BYTXTY.P BYT ARTIKEL X TILL ARTIKEL Y*/

{SMTRL.I} 
{BMTRL.I}

DEFINE TEMP-TABLE grupp_temp NO-UNDO
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.

DEFINE INPUT PARAMETER valgrupp LIKE KONSTGRUPP.KONSKOD NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR spec_mtrl.
DEFINE INPUT PARAMETER TABLE FOR byt_mtrl.
DEFINE INPUT PARAMETER TABLE FOR grupp_temp.
DEFINE INPUT PARAMETER varalla AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER erslev AS LOGICAL NO-UNDO.
DEFINE VARIABLE kabelskapup AS LOGICAL NO-UNDO.
DEFINE QUERY mtrlq FOR MTRLBER.
DEFINE QUERY kq FOR KONSTRUKTION.
DEFINE BUFFER mtrlberbuff FOR MTRLBER.
DEFINE BUFFER berstolpbuff FOR BERSTOLP.
DEFINE BUFFER berskapbuff FOR BERSKAP.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE Utbyttabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE ErsattEnr AS CHARACTER NO-UNDO.
DEFINE VARIABLE ErsattRow AS ROWID NO-UNDO.
DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
DEFINE VAR DynWp AS CHARACTER NO-UNDO.
DynWp = "DYNBYT" + STRING(TIME).
CREATE WIDGET-POOL STRING(DynWp) NO-ERROR.

CREATE BUFFER Utbyttabbuffh FOR TABLE "UTBYTESLISTA".

FIND FIRST byt_mtrl NO-LOCK NO-ERROR.        
FIND FIRST spec_mtrl NO-LOCK NO-ERROR.     
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.




IF valgrupp = ? THEN DO:
 
END.

ELSE DO:   
   FOR EACH grupp_temp:
      valgrupp = grupp_temp.KONSKOD.
      IF valgrupp = 0 THEN kabelskapup = TRUE.
      OPEN QUERY kq FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.
      GET FIRST kq NO-LOCK.
      DO WHILE AVAILABLE(KONSTRUKTION):
         IF Guru.Konstanter:globforetag = "ELPA" /*{GLOBVES.I}*/ THEN DO:
            IF spec_mtrl.LEVKOD = "1" OR spec_mtrl.LEVKOD = "11" THEN DO:
               OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD AND
               MTRLBER.ENR = spec_mtrl.ENR AND
               (MTRLBER.LEVKOD = "1" OR MTRLBER.LEVKOD = "11") NO-LOCK.
            END.
            ELSE DO:
               OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD AND
               MTRLBER.ENR = spec_mtrl.ENR AND
               MTRLBER.LEVKOD = spec_mtrl.LEVKOD NO-LOCK.
            END.
         END.
         ELSE DO:
            OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD AND
            MTRLBER.ENR = spec_mtrl.ENR AND
            MTRLBER.LEVKOD = spec_mtrl.LEVKOD NO-LOCK.
         END.               
         DO TRANSACTION:
            GET FIRST mtrlq EXCLUSIVE-LOCK.
            IF AVAILABLE MTRLBER THEN DO:
               FIND FIRST mtrlberbuff WHERE mtrlberbuff.ENR =  byt_mtrl.ENR AND mtrlberbuff.KTYPKOD  = MTRLBER.KTYPKOD  AND mtrlberbuff.F1  = MTRLBER.F1 AND mtrlberbuff.F2  = MTRLBER.F2 AND mtrlberbuff.F3  = MTRLBER.F3 
               AND mtrlberbuff.F4  = MTRLBER.F4 AND mtrlberbuff.F5  = MTRLBER.F5 AND mtrlberbuff.LEVKOD   = MTRLBER.LEVKOD NO-LOCK NO-ERROR.
               IF AVAILABLE mtrlberbuff THEN DO:
                  DELETE MTRLBER.     
               END.
               ELSE DO:
                  ASSIGN
                  MTRLBER.ENR = byt_mtrl.ENR
                  MTRLBER.BENAMNING = byt_mtrl.BENAMNING
                  MTRLBER.ENHET = byt_mtrl.ENHET
                  MTRLBER.PRIS = byt_mtrl.NPRIS
                  MTRLBER.SATS = byt_mtrl.SATS
                  MTRLBER.LEVKOD = byt_mtrl.LEVKOD.
               END.   
            END.
         END.         
         REPEAT:                                              
            DO TRANSACTION:
               GET NEXT mtrlq EXCLUSIVE-LOCK.
               IF AVAILABLE MTRLBER THEN DO:
                  FIND FIRST mtrlberbuff WHERE mtrlberbuff.ENR =  byt_mtrl.ENR AND mtrlberbuff.KTYPKOD  = MTRLBER.KTYPKOD  AND mtrlberbuff.F1  = MTRLBER.F1 AND mtrlberbuff.F2  = MTRLBER.F2 AND mtrlberbuff.F3  = MTRLBER.F3 
                  AND mtrlberbuff.F4  = MTRLBER.F4 AND mtrlberbuff.F5  = MTRLBER.F5 AND mtrlberbuff.LEVKOD   = MTRLBER.LEVKOD NO-LOCK NO-ERROR.
                  IF AVAILABLE mtrlberbuff THEN DO:
                     DELETE MTRLBER.     
                  END.
                  ELSE DO:
                     ASSIGN
                     MTRLBER.ENR = byt_mtrl.ENR
                     MTRLBER.BENAMNING = byt_mtrl.BENAMNING
                     MTRLBER.ENHET = byt_mtrl.ENHET
                     MTRLBER.PRIS = byt_mtrl.NPRIS
                     MTRLBER.SATS = byt_mtrl.SATS
                     MTRLBER.LEVKOD = byt_mtrl.LEVKOD.
                  END.   
               END.
               ELSE LEAVE.
            END.                       
         END.
         CLOSE QUERY mtrlq.
         GET NEXT kq NO-LOCK.
      END.
      CLOSE QUERY kq.
   END.
   /*skåpupp*/
   IF kabelskapup = TRUE THEN DO:
      IF Guru.Konstanter:globforetag = "ELPA" /*{GLOBVES.I}*/ THEN DO:
         IF spec_mtrl.LEVKOD = "1" OR spec_mtrl.LEVKOD = "11" THEN DO:
            OPEN QUERY mskapq FOR EACH BERSKAP WHERE BERSKAP.ENR = spec_mtrl.ENR AND
            (BERSKAP.LEVKOD = "1" OR BERSKAP.LEVKOD = "11") NO-LOCK.         
         END.
         ELSE DO:
            OPEN QUERY mskapq FOR EACH BERSKAP WHERE BERSKAP.ENR = spec_mtrl.ENR AND
            BERSKAP.LEVKOD = spec_mtrl.LEVKOD NO-LOCK.         
         END.
      END.
      ELSE DO:
         OPEN QUERY mskapq FOR EACH BERSKAP WHERE BERSKAP.ENR = spec_mtrl.ENR AND
         BERSKAP.LEVKOD = spec_mtrl.LEVKOD NO-LOCK.         
      END.         
      GET FIRST mskapq NO-LOCK.
      DO WHILE AVAILABLE(BERSKAP):
          DO TRANSACTION:
            GET CURRENT mskapq EXCLUSIVE-LOCK.            
            FIND FIRST berskapbuff WHERE berskapbuff.ENR = byt_mtrl.ENR AND berskapbuff.KTYPKOD = BERSKAP.KTYPKOD AND berskapbuff.KOD = BERSKAP.KOD AND berskapbuff.LEVKOD   = BERSKAP.LEVKOD NO-LOCK NO-ERROR.
            IF AVAILABLE berskapbuff THEN DO:
               DELETE BERSKAP.
            END.
            ELSE DO:
               ASSIGN
               BERSKAP.ENR = byt_mtrl.ENR
               BERSKAP.BENAMNING = byt_mtrl.BENAMNING
               BERSKAP.ENHET = byt_mtrl.ENHET
               BERSKAP.PRIS = byt_mtrl.NPRIS
               BERSKAP.LEVKOD = byt_mtrl.LEVKOD.
           END.    
            
         END.         
         GET NEXT mskapq NO-LOCK.
      END.
      CLOSE QUERY mskapq.
    
   END.
   /*STOLPAR OCH TRANSFORMATORER*/
   IF Guru.Konstanter:globforetag = "ELPA" /*{GLOBVES.I}*/ THEN DO:
      IF spec_mtrl.LEVKOD = "1" OR spec_mtrl.LEVKOD = "11" THEN DO:
         OPEN QUERY stolpq FOR EACH BERSTOLP WHERE BERSTOLP.ENR = spec_mtrl.ENR AND
         (BERSTOLP.LEVKOD = "1" OR BERSTOLP.LEVKOD = "11") NO-LOCK.      
      END.
      ELSE DO:
         OPEN QUERY stolpq FOR EACH BERSTOLP WHERE BERSTOLP.ENR = spec_mtrl.ENR AND
         BERSTOLP.LEVKOD = spec_mtrl.LEVKOD NO-LOCK.      
      END.
   END.
   ELSE DO:
      OPEN QUERY stolpq FOR EACH BERSTOLP WHERE BERSTOLP.ENR = spec_mtrl.ENR AND
      BERSTOLP.LEVKOD = spec_mtrl.LEVKOD NO-LOCK.      
   END.      
   DO TRANSACTION:
      GET FIRST stolpq EXCLUSIVE-LOCK.
      IF AVAILABLE BERSTOLP THEN DO:
         FIND FIRST berstolpbuff WHERE berstolpbuff.ENR = byt_mtrl.ENR AND berstolpbuff.LEVKOD   = BERSTOLP.LEVKOD NO-LOCK NO-ERROR.
         IF AVAILABLE berstolpbuff THEN DO:
            DELETE BERSTOLP.
         END.
         ELSE DO:
            ASSIGN
            BERSTOLP.ENR = byt_mtrl.ENR
            BERSTOLP.BENAMNING = byt_mtrl.BENAMNING
            BERSTOLP.ENHET = byt_mtrl.ENHET
            BERSTOLP.PRIS = byt_mtrl.NPRIS            
            BERSTOLP.LEVKOD = byt_mtrl.LEVKOD.
        END.    
      END.
   END.      
   REPEAT:
      DO TRANSACTION:
         GET NEXT stolpq EXCLUSIVE-LOCK. 
         IF AVAILABLE BERSTOLP THEN DO:
            FIND FIRST berstolpbuff WHERE berstolpbuff.ENR = byt_mtrl.ENR AND berstolpbuff.LEVKOD   = BERSTOLP.LEVKOD NO-LOCK NO-ERROR.
            IF AVAILABLE berstolpbuff THEN DO:
               DELETE BERSTOLP.
            END.
            ELSE DO:
               ASSIGN
               BERSTOLP.ENR = byt_mtrl.ENR
               BERSTOLP.BENAMNING = byt_mtrl.BENAMNING
               BERSTOLP.ENHET = byt_mtrl.ENHET
               BERSTOLP.PRIS = byt_mtrl.NPRIS               
               BERSTOLP.LEVKOD = byt_mtrl.LEVKOD.
            END.   
         END.
         ELSE LEAVE.
      END.         
   END.      
   CLOSE QUERY stolpq.  
   IF varalla = TRUE THEN DO:   
      /* Byt ersättningslistan från*/
      queryvar =  "FOR EACH " + Utbyttabbuffh:TABLE + " WHERE UID = 0 " + " AND ORGLEVKOD = " + QUOTER(spec_mtrl.LEVKOD) +
      " AND ORGENR = " + QUOTER(spec_mtrl.ENR).
      RUN CreateCustomQuery(INPUT Utbyttabbuffh,INPUT queryvar,OUTPUT qh).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qH:GET-FIRST().
         DO TRANSACTION:
            qH:GET-CURRENT(EXCLUSIVE-LOCK).
            Utbyttabbuffh:BUFFER-FIELD("ORGENR"):BUFFER-VALUE = byt_mtrl.ENR.
            Utbyttabbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE = byt_mtrl.LEVKOD.
            IF Utbyttabbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE = Utbyttabbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE THEN DO:
               Utbyttabbuffh:BUFFER-DELETE().  
            END.
         END.   
         qH:GET-NEXT(). 
      END.  
      queryvar =  "FOR EACH " + Utbyttabbuffh:TABLE + " WHERE UID = 0 " + " AND BYTTILLLEVKOD = " + QUOTER(spec_mtrl.LEVKOD) +
      " AND BYTTILLENR = " + QUOTER(spec_mtrl.ENR).
      RUN CreateCustomQuery(INPUT Utbyttabbuffh,INPUT queryvar,OUTPUT qh).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qH:GET-FIRST().
         DO TRANSACTION:
            qH:GET-CURRENT(EXCLUSIVE-LOCK).
            Utbyttabbuffh:BUFFER-FIELD("BYTTILLENR"):BUFFER-VALUE = byt_mtrl.ENR.
            Utbyttabbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE = byt_mtrl.LEVKOD.
            IF Utbyttabbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE = Utbyttabbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE THEN DO:
               Utbyttabbuffh:BUFFER-DELETE().  
            END.
         END.   
         qH:GET-NEXT(). 
      END. 
      queryvar =  "FOR EACH " + Utbyttabbuffh:TABLE + " WHERE UID NE 0 " + " AND ORGLEVKOD = " + QUOTER(spec_mtrl.LEVKOD) +
      " AND ORGENR = " + QUOTER(spec_mtrl.ENR).
      RUN CreateCustomQuery(INPUT Utbyttabbuffh,INPUT queryvar,OUTPUT qh).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         qH:GET-FIRST().
         DO TRANSACTION:
            qH:GET-CURRENT(EXCLUSIVE-LOCK).
            Utbyttabbuffh:BUFFER-FIELD("ORGENR"):BUFFER-VALUE = byt_mtrl.ENR.
            Utbyttabbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE = byt_mtrl.LEVKOD.
            IF Utbyttabbuffh:BUFFER-FIELD("ORGLEVKOD"):BUFFER-VALUE = Utbyttabbuffh:BUFFER-FIELD("BYTTILLLEVKOD"):BUFFER-VALUE THEN DO:
               Utbyttabbuffh:BUFFER-DELETE().  
            END.
         END.   
         qH:GET-NEXT(). 
      END.  
       
   END.
  
END.   
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL DynWp.
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
   
PROCEDURE CloseCustomQuery:
   DEFINE INPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CustomQueryh:QUERY-CLOSE()  NO-ERROR.
   CustomQueryh = ?.
END PROCEDURE.         
