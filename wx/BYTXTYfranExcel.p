/*BYTXTYfranExcel.P BYT ARTIKEL X TILL ARTIKEL Y*/
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
/*{SMTRL.I} 
{BMTRL.I}*/

/*DEFINE TEMP-TABLE grupp_temp NO-UNDO
   FIELD KONSKOD AS INTEGER
   FIELD BENAMNING AS CHARACTER.*/
DEFINE VARIABLE valgrupp AS INTEGER NO-UNDO.
DEFINE VARIABLE varalla AS LOGICAL NO-UNDO.
/*DEFINE INPUT PARAMETER valgrupp LIKE KONSTGRUPP.KONSKOD NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR spec_mtrl.
DEFINE INPUT PARAMETER TABLE FOR byt_mtrl.
DEFINE INPUT PARAMETER TABLE FOR grupp_temp.
DEFINE INPUT PARAMETER varalla AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER erslev AS LOGICAL NO-UNDO.*/
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
DEFINE TEMP-TABLE bytenr
   FIELD franlev        AS CHARACTER       
   FIELD franenr        AS CHARACTER      
   FIELD tilllev      AS CHARACTER
   FIELD tillenr      AS CHARACTER
   FIELD benamning      AS CHARACTER
   FIELD enhet      AS CHARACTER
   FIELD npris      AS DECIMAL
   FIELD sats      AS LOGICAL     
   FIELD ejlev  AS logical
   INDEX franenr IS PRIMARY franenr.
DEFINE VARIABLE filnamn  AS CHARACTER NO-UNDO.   
/*DEFINE VAR DynWp AS CHARACTER NO-UNDO.
DynWp = STRING(DynWp) + STRING(TIME).
CREATE WIDGET-POOL STRING(DynWp) NO-ERROR.
IF Guru.Konstanter:varforetypval[59] = 1 THEN DO: 
   CREATE BUFFER Utbyttabbuffh FOR TABLE "UTBYTESLISTA".
END.*/ 
varalla = TRUE.
     
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.

EMPTY TEMP-TABLE bytenr NO-ERROR.    
   
filnamn = "D:\elpool\OniErsattmtrlber.skv".
   
RUN inlas_UI.
PROCEDURE inlas_UI: 
   
   
   INPUT FROM VALUE(filnamn) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE bytenr.
         ASSIGN.
         IMPORT DELIMITER ";" bytenr   NO-ERROR.
      END.               
   END.
              
END PROCEDURE.


FOR EACH bytenr :
   BYTENR.ejlev = FALSE.
   IF BYTENR.franenr BEGINS "E" THEN BYTENR.franenr = substring(BYTENR.franenr,2).
   IF BYTENR.tillenr BEGINS "E" THEN BYTENR.tillenr = substring(BYTENR.tillenr,2). 
END.
OUTPUT TO D:\elpool\ejOni.txt.
FOR EACH bytenr :
   FIND FIRST MTRL WHERE MTRL.ENR = BYTENR.TILLENR AND MTRL.LEVKOD = BYTENR.TILLLEV NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MTRL THEN DO:
      PUT "från enr " BYTENR.franENR " till enr finns ej " BYTENR.tillenr " " BYTENR.TILLlev SKIP.
      DELETE BYTENR.
   END.
END.
OUTPUT CLOSE.
OUTPUT TO D:\elpool\FinnsOni.txt.
FOR EACH bytenr :   
   PUT "från enr " BYTENR.franENR " till enr " BYTENR.tillenr " " BYTENR.TILLlev SKIP.         
END.
OUTPUT CLOSE.

OUTPUT TO D:\elpool\FRANFINNSEJ.txt.
FOR EACH bytenr :
   FIND FIRST MTRLBER WHERE MTRLBER.ENR = BYTENR.FRANENR AND MTRLBER.LEVKOD = BYTENR.FRANLEV NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MTRLBER THEN DO:
      
      FIND FIRST BERSKAP WHERE BERSKAP.ENR = bytenr.FRANENR AND
      BERSKAP.LEVKOD = bytenr.FRANLEV NO-LOCK NO-ERROR .
      IF NOT AVAILABLE BERSKAP THEN DO:
         FIND FIRST BERSTOLP WHERE BERSTOLP.ENR = bytenr.FRANENR AND
         BERSTOLP.LEVKOD = bytenr.FRANLEV NO-LOCK  NO-ERROR.
         IF NOT AVAILABLE BERSTOLP THEN DO:
            /*FIND FIRST BETPLAN WHERE BETPLAN.ANVANDARE = bytenr.FRANENR AND
            BETPLAN.NAMN = bytenr.FRANLEV  NO-LOCK NO-ERROR.
            IF NOT AVAILABLE BETPLAN THEN DO:*/            
               PUT "från enr " BYTENR.franENR " " BYTENR.FRANLEV SKIP.
               BYTENR.ejlev = TRUE.
            /*END.   */
         END.
      END.
         
   END.
END.
OUTPUT CLOSE.
OUTPUT TO D:\elpool\FRANFINNSEJhosnågonlev.txt.
FOR EACH bytenr WHERE bytenr.ejlev = true:
   FIND FIRST MTRLBER WHERE MTRLBER.ENR = BYTENR.FRANENR  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE MTRLBER THEN DO:
      FIND FIRST BERSKAP WHERE BERSKAP.ENR = bytenr.FRANENR  NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BERSKAP THEN DO:
         FIND FIRST BERSTOLP WHERE BERSTOLP.ENR = bytenr.FRANENR NO-LOCK NO-ERROR.
         IF NOT AVAILABLE BERSTOLP THEN DO:
            /*FIND FIRST BETPLAN WHERE BETPLAN.ANVANDARE = bytenr.FRANENR   NO-LOCK NO-ERROR.
            IF NOT AVAILABLE BETPLAN THEN DO:*/
               PUT "från enr " BYTENR.franENR " finns ej hos någon lev "  SKIP.
               DELETE BYTENR.
            /*END.   
            ELSE BYTENR.franlev =  BETPLAN.NAMN.*/            
         END.
         ELSE BYTENR.franlev =  BERSTOLP.LEVKOD.
      END.
      ELSE BYTENR.franlev =  BERSKAP.LEVKOD.
   END.
   ELSE BYTENR.franlev =  MTRLBER.LEVKOD.  
    
END.
OUTPUT CLOSE.
OUTPUT TO D:\elpool\okbyt.txt.
FOR EACH bytenr :   
   PUT "från enr " BYTENR.franENR " " BYTENR.franlev " till enr " BYTENR.tillenr " " BYTENR.TILLlev SKIP.         
END.
OUTPUT CLOSE.

    
 
/*   FOR EACH KONSTGRUPP NO-LOCK ,
   EACH bytenr  NO-LOCK:
      valgrupp = KONSTGRUPP.KONSKOD.
      IF valgrupp = 0 THEN kabelskapup = TRUE.
      OPEN QUERY kq FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.
      GET FIRST kq NO-LOCK.
      DO WHILE AVAILABLE(KONSTRUKTION):
         
         OPEN QUERY mtrlq FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD AND
         MTRLBER.ENR = bytenr.FRANENR AND
         MTRLBER.LEVKOD = bytenr.FRANLEV NO-LOCK.
                     
         DO TRANSACTION:
            GET FIRST mtrlq EXCLUSIVE-LOCK.
            IF AVAILABLE MTRLBER THEN DO:
               FIND FIRST mtrlberbuff WHERE mtrlberbuff.ENR =  bytenr.TILLENR AND mtrlberbuff.KTYPKOD  = MTRLBER.KTYPKOD  AND mtrlberbuff.F1  = MTRLBER.F1 AND mtrlberbuff.F2  = MTRLBER.F2 AND mtrlberbuff.F3  = MTRLBER.F3 
               AND mtrlberbuff.F4  = MTRLBER.F4 AND mtrlberbuff.F5  = MTRLBER.F5 AND mtrlberbuff.LEVKOD   = MTRLBER.LEVKOD NO-LOCK NO-ERROR.
               IF AVAILABLE mtrlberbuff THEN DO:
                  DELETE MTRLBER.     
               END.
               ELSE DO:
                  
                  ASSIGN
                  MTRLBER.ENR = bytenr.TILLENR
                  MTRLBER.BENAMNING = bytenr.BENAMNING
                  MTRLBER.ENHET = bytenr.ENHET
                  MTRLBER.PRIS = bytenr.NPRIS
                  MTRLBER.SATS = bytenr.SATS
                  MTRLBER.LEVKOD = bytenr.TILLLEV.
               END.   
            END.
         END.         
         REPEAT:                                              
            DO TRANSACTION:
               GET NEXT mtrlq EXCLUSIVE-LOCK.
               IF AVAILABLE MTRLBER THEN DO:
                  FIND FIRST mtrlberbuff WHERE mtrlberbuff.ENR =  bytenr.TILLENR AND mtrlberbuff.KTYPKOD  = MTRLBER.KTYPKOD  AND mtrlberbuff.F1  = MTRLBER.F1 AND mtrlberbuff.F2  = MTRLBER.F2 AND mtrlberbuff.F3  = MTRLBER.F3 
                  AND mtrlberbuff.F4  = MTRLBER.F4 AND mtrlberbuff.F5  = MTRLBER.F5 AND mtrlberbuff.LEVKOD   = MTRLBER.LEVKOD NO-LOCK NO-ERROR.
                  IF AVAILABLE mtrlberbuff THEN DO:
                     DELETE MTRLBER.     
                  END.
                  ELSE DO:
                     ASSIGN
                     MTRLBER.ENR = bytenr.TILLENR
                     MTRLBER.BENAMNING = bytenr.BENAMNING
                     MTRLBER.ENHET = bytenr.ENHET
                     MTRLBER.PRIS = bytenr.NPRIS
                     MTRLBER.SATS = bytenr.SATS
                     MTRLBER.LEVKOD = bytenr.TILLLEV.
                  END.   
               END.
               ELSE LEAVE.
            END.                       
         END.
         CLOSE QUERY mtrlq.
         GET NEXT kq NO-LOCK.
      END.
      CLOSE QUERY kq.
      
   
      /*skåpupp*/
      IF kabelskapup = TRUE THEN DO:
         
         OPEN QUERY mskapq FOR EACH BERSKAP WHERE BERSKAP.ENR = bytenr.FRANENR AND
         BERSKAP.LEVKOD = bytenr.FRANLEV NO-LOCK.         
               
         GET FIRST mskapq NO-LOCK.
         DO WHILE AVAILABLE(BERSKAP):
             DO TRANSACTION:
               GET CURRENT mskapq EXCLUSIVE-LOCK.            
               FIND FIRST berskapbuff WHERE berskapbuff.ENR = bytenr.TILLENR AND berskapbuff.KTYPKOD = BERSKAP.KTYPKOD AND berskapbuff.KOD = BERSKAP.KOD AND berskapbuff.LEVKOD   = BERSKAP.LEVKOD NO-LOCK NO-ERROR.
               IF AVAILABLE berskapbuff THEN DO:
                  DELETE BERSKAP.
               END.
               ELSE DO:
                  ASSIGN
                  BERSKAP.ENR = bytenr.TILLENR
                  BERSKAP.BENAMNING = bytenr.BENAMNING
                  BERSKAP.ENHET = bytenr.ENHET
                  BERSKAP.PRIS = bytenr.NPRIS
                  BERSKAP.LEVKOD = bytenr.TILLLEV.
              END.    
               
            END.         
            GET NEXT mskapq NO-LOCK.
         END.
         CLOSE QUERY mskapq.
       
      END.
      /*STOLPAR OCH TRANSFORMATORER*/
      
      OPEN QUERY stolpq FOR EACH BERSTOLP WHERE BERSTOLP.ENR = bytenr.FRANENR AND
      BERSTOLP.LEVKOD = bytenr.FRANLEV NO-LOCK.      
            
      DO TRANSACTION:
         GET FIRST stolpq EXCLUSIVE-LOCK.
         IF AVAILABLE BERSTOLP THEN DO:
            FIND FIRST berstolpbuff WHERE berstolpbuff.ENR = bytenr.TILLENR AND berstolpbuff.LEVKOD   = BERSTOLP.LEVKOD NO-LOCK NO-ERROR.
            IF AVAILABLE berstolpbuff THEN DO:
               DELETE BERSTOLP.
            END.
            ELSE DO:
               ASSIGN
               BERSTOLP.ENR = bytenr.TILLENR
               BERSTOLP.BENAMNING = bytenr.BENAMNING
               BERSTOLP.ENHET = bytenr.ENHET
               BERSTOLP.PRIS = bytenr.NPRIS            
               BERSTOLP.LEVKOD = bytenr.TILLLEV.
           END.    
         END.
      END.      
      REPEAT:
         DO TRANSACTION:
            GET NEXT stolpq EXCLUSIVE-LOCK. 
            IF AVAILABLE BERSTOLP THEN DO:
               FIND FIRST berstolpbuff WHERE berstolpbuff.ENR = bytenr.TILLENR AND berstolpbuff.LEVKOD   = BERSTOLP.LEVKOD NO-LOCK NO-ERROR.
               IF AVAILABLE berstolpbuff THEN DO:
                  DELETE BERSTOLP.
               END.
               ELSE DO:
                  ASSIGN
                  BERSTOLP.ENR = bytenr.TILLENR
                  BERSTOLP.BENAMNING = bytenr.BENAMNING
                  BERSTOLP.ENHET = bytenr.ENHET
                  BERSTOLP.PRIS = bytenr.NPRIS               
                  BERSTOLP.LEVKOD = bytenr.TILLLEV.
               END.   
            END.
            ELSE LEAVE.
         END.         
      END.      
      CLOSE QUERY stolpq.  
      IF varalla = TRUE THEN DO:   
         /* Byt ersättningslistan från*/
         OPEN QUERY ersq FOR EACH BETPLAN WHERE BETPLAN.ANVANDARE = bytenr.FRANENR AND
         BETPLAN.NAMN = bytenr.FRANLEV NO-LOCK.      
         DO TRANSACTION:
            GET FIRST ersq EXCLUSIVE-LOCK.
            IF AVAILABLE BETPLAN THEN DO:
               ASSIGN
               BETPLAN.ANVANDARE = bytenr.TILLENR        
               BETPLAN.NAMN = bytenr.TILLLEV.
               IF BETPLAN.ANVANDARE = BETPLAN.OMRADE THEN DELETE BETPLAN.
            END.
         END.      
         REPEAT:
            DO TRANSACTION:
               GET NEXT ersq EXCLUSIVE-LOCK. 
               IF AVAILABLE BETPLAN THEN DO:
                  ASSIGN
                  BETPLAN.ANVANDARE = bytenr.TILLENR                  
                  BETPLAN.NAMN = bytenr.TILLLEV.
                  IF BETPLAN.ANVANDARE = BETPLAN.OMRADE THEN DELETE BETPLAN.
               END.
               ELSE LEAVE.
            END.         
         END.      
         CLOSE QUERY ersq.   
         /* Byt ersättningslistan till*/
         OPEN QUERY erstq FOR EACH BETPLAN WHERE BETPLAN.OMRADE = bytenr.FRANENR AND
         BETPLAN.BESTID = bytenr.FRANLEV NO-LOCK.      
         DO TRANSACTION:
            GET FIRST erstq EXCLUSIVE-LOCK.
            IF AVAILABLE BETPLAN THEN DO:
               ASSIGN
               BETPLAN.OMRADE = bytenr.TILLENR        
               BETPLAN.BESTID = bytenr.TILLLEV.
               IF BETPLAN.ANVANDARE = BETPLAN.OMRADE THEN DELETE BETPLAN.
            END.
         END.      
         REPEAT:
            DO TRANSACTION:
               GET NEXT erstq EXCLUSIVE-LOCK. 
               IF AVAILABLE BETPLAN THEN DO:
                  ASSIGN
                  BETPLAN.OMRADE = bytenr.TILLENR                  
                  BETPLAN.BESTID = bytenr.TILLLEV.
                  IF BETPLAN.ANVANDARE = BETPLAN.OMRADE THEN DELETE BETPLAN.
               END.
               ELSE LEAVE.
            END.         
         END.      
         CLOSE QUERY ersq.   
      END.
END.*/
   
   
/*PROCEDURE CreateCustomQuery:
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
*/