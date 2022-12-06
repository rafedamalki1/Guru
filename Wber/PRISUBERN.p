/*PRISUBERN.p*/
/*UPPDATERAR PRISER FÖR TABELLER SOM ANVÄNDS I BEREDNING*/


DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE BUFFER mtrlbuff FOR MTRL.
{INKKOLL3.I}
{DYNPOSTFINNS.I}
DEFINE VARIABLE musz AS LOGICAL     NO-UNDO.
DEFINE VARIABLE fragavar AS CHARACTER   NO-UNDO.
{AMERICANEUROPEAN.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
IF   Guru.Konstanter:globforetag = "VAST"   THEN DO:
   prognamn = "e:\delad\pro9\guru\koll.txt".   
END.
ELSE IF Guru.Konstanter:globforetag = "FORS" THEN DO:
   prognamn = "C:\DELAD\PRO9\GURU\koll.txt".   
END.
ELSE IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   prognamn = "D:\DELAD\klient\PRO9\koll.txt".   
END.
ELSE IF Guru.Konstanter:globforetag = "GRAN" THEN DO:
   prognamn = "d:\elpool\delad\pro9\wrk\koll.txt".   
END.   


OUTPUT TO VALUE(prognamn) APPEND.
PUT "PRISUBERN.P start. " leverant  substring(Guru.Konstanter:globforetag,1,10)  string(TODAY) SKIP.
OUTPUT CLOSE.
OPEN QUERY bq FOR EACH BEREDNING WHERE BEREDNING.AKTIV = TRUE NO-LOCK.
GET FIRST bq NO-LOCK.
DO WHILE AVAILABLE(BEREDNING):
   FIND FIRST BERMTRL WHERE BERMTRL.AONR = BEREDNING.BERAONR AND
   BERMTRL.OMRADE = BEREDNING.OMRADE AND BERMTRL.INKOP = TRUE
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE BERMTRL THEN DO:
      musz = FALSE.
      IF inkopkollvar = TRUE THEN DO:               
         fragavar = "INKMTRL.BERNR = " + STRING(BEREDNING.BERNR) + " AND INKMTRL.OMRADE = '" + BEREDNING.OMRADE + "'".  
         RUN finnspostdyn_UI (INPUT "INKMTRL",INPUT fragavar,OUTPUT musz).
         RUN queryclose_UI.   
      END.
      
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO:
         IF   Guru.Konstanter:globforetag = "VAST"   THEN DO:
            OUTPUT TO VALUE(prognamn) APPEND.            
            PUT "PRISUBERN.P BERMTRLiber" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR  string(TODAY) SKIP.
            OUTPUT CLOSE.
         END.
                 
         OPEN QUERY m2q FOR EACH BERMTRL WHERE BERMTRL.AONR = BEREDNING.BERAONR AND
         BERMTRL.OMRADE = BEREDNING.OMRADE AND BERMTRL.LEVKOD = leverant NO-LOCK.
         GET FIRST m2q NO-LOCK.
         DO WHILE AVAILABLE(BERMTRL):
            DO TRANSACTION:
               GET CURRENT m2q EXCLUSIVE-LOCK NO-WAIT.                        
               IF LOCKED(BERMTRL) = FALSE THEN DO:               
                  FIND FIRST MTRL WHERE MTRL.ENR = BERMTRL.ENR AND 
                  MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 USE-INDEX LEV
                  NO-LOCK NO-ERROR.
                  IF AVAILABLE MTRL THEN DO:
                     ASSIGN
                     BERMTRL.PRIS = MTRL.NPRIS.
                  END.      
                  IF Guru.Konstanter:globforetag = "ELPA" {GLOBVES.I} THEN DO:
                     /* om enr borttaget ur 12 uppdatera med leverantör 1 och pris från 1*/
                     IF leverant = "12" THEN DO: 
                        IF NOT AVAILABLE MTRL THEN DO:
                           FIND FIRST MTRL WHERE MTRL.ENR = BERMTRL.ENR AND 
                           MTRL.LEVKOD = "1" AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.
                           IF AVAILABLE MTRL THEN DO:
                              ASSIGN
                              BERMTRL.LEVKOD = "1"
                              BERMTRL.PRIS = MTRL.NPRIS.
                           END.                           
                        END.
                     END.
                  END.   
               END.
               
               ELSE IF LOCKED(BERMTRL) = TRUE THEN DO:                                 
                  OUTPUT TO VALUE(prognamn) APPEND.
                  PUT "PRISUBERN.P BERMTRLlåst" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR string(TODAY) SKIP.
                  OUTPUT CLOSE.                  
               END.
            END.   
            GET NEXT m2q NO-LOCK.   
         END.
         CLOSE QUERY m2q.
         RELEASE BERMTRL NO-ERROR.        
         
      END.
   END.                       
   GET NEXT bq NO-LOCK.
   
END.
CLOSE QUERY bq.         
       
         
OUTPUT TO VALUE(prognamn) APPEND.
PUT "PRISUBERN.P uppdatering bered. klart. " leverant  SUBSTRING(Guru.Konstanter:globforetag,1,10)  STRING(TODAY) SKIP.
OUTPUT CLOSE.

{EUROPEANAMERICAN.I}
