/*PRISUBER.p*/
/*UPPDATERAR PRISER FÖR TABELLER SOM ANVÄNDS I BEREDNING*/
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.

DEFINE INPUT PARAMETER leverant LIKE LEVERANTOR.LEVKOD NO-UNDO.
{INKKOLL3.I}
{DYNPOSTFINNS.I}
DEFINE VARIABLE musz AS LOGICAL     NO-UNDO.
DEFINE VARIABLE fragavar AS CHARACTER   NO-UNDO.
{AMERICANEUROPEAN.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
IF   globforetag = "VAST"   THEN DO:
   OUTPUT TO C:\PRO10\GURU\koll.txt APPEND.
   PUT "PRISUBERN.P start. " leverant  substring(globforetag,1,10)  string(TODAY) SKIP.
   OUTPUT CLOSE.
END.
ELSE IF globforetag = "FORS" THEN DO:
   OUTPUT TO C:\DELAD\PRO9\GURU\koll.txt APPEND.
   PUT "PRISUBERN.P start. " leverant  substring(globforetag,1,10)  string(TODAY) SKIP.
   OUTPUT CLOSE.
END.
ELSE IF globforetag = "GKAL" THEN DO:
   OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
   PUT "PRISUBERN.P start. " leverant  substring(globforetag,1,10)  string(TODAY) SKIP.
   OUTPUT CLOSE.
END.
ELSE IF globforetag = "GRAN" THEN DO:
   OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
   PUT "PRISUBERN.P start. " leverant  substring(globforetag,1,10)  string(TODAY) SKIP.
   OUTPUT CLOSE.
END.   
OPEN QUERY bq FOR EACH BEREDNING NO-LOCK.
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
         
      END.
      IF musz = TRUE THEN musz = FALSE.
      ELSE DO:
         IF   globforetag = "VAST"   THEN DO:
            OUTPUT TO C:\PRO10\GURU\koll.txt APPEND.
            PUT "PRISUBERN.P BERMTRLi" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR  string(TODAY) SKIP.
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
                  IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
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
                  IF   globforetag = "VAST"   THEN DO:
                     OUTPUT TO C:\PRO10\GURU\koll.txt APPEND.
                     PUT "PRISUBERN.P BERMTRL" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR string(TODAY) SKIP.
                     OUTPUT CLOSE.
                  END.
                  ELSE IF globforetag = "FORS" THEN DO:
                     OUTPUT TO C:\DELAD\PRO9\GURU\koll.txt APPEND.
                     PUT "PRISUBERN.P BERMTRL" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR string(TODAY) SKIP.
                     OUTPUT CLOSE.
                  END.
                  ELSE IF globforetag = "GKAL" THEN DO:
                     OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
                     PUT "PRISUBERN.P BERMTRL" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR  string(TODAY) SKIP.
                     OUTPUT CLOSE.
                  END.
                  ELSE IF globforetag = "GRAN" THEN DO:
                     OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
                     PUT "PRISUBERN.P BERMTRL" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR string(TODAY) SKIP.
                     OUTPUT CLOSE.
                  END.
               END.
            END.   
            GET NEXT m2q NO-LOCK.   
         END.
         CLOSE QUERY m2q.
                 
         OPEN QUERY m3q FOR EACH BERLINKAB WHERE BERLINKAB.AONR = BEREDNING.BERAONR AND
         BERLINKAB.OMRADE = BEREDNING.OMRADE AND BERLINKAB.LEVKOD = leverant NO-LOCK.
         GET FIRST m3q NO-LOCK.
         DO WHILE AVAILABLE(BERLINKAB):
            DO TRANSACTION:
               GET CURRENT m3q EXCLUSIVE-LOCK NO-WAIT.
               IF LOCKED(BERLINKAB) = FALSE THEN DO:                                       
                  FIND FIRST MTRL WHERE MTRL.ENR = BERLINKAB.ENR AND 
                  MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 USE-INDEX LEV
                  NO-LOCK NO-ERROR.
                  IF AVAILABLE MTRL THEN DO:
                     ASSIGN
                     BERLINKAB.PRIS = MTRL.NPRIS.
                  END.
                  IF globforetag = "ELPA" {GLOBVES.I} THEN DO:
                     /* om enr borttaget ur 12 uppdatera med leverantör 16 och pris från 16*/
                     IF leverant = "12" THEN DO: 
                        IF NOT AVAILABLE MTRL THEN DO:
                           FIND FIRST MTRL WHERE MTRL.ENR = BERLINKAB.ENR AND 
                           MTRL.LEVKOD = "1" AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK NO-ERROR.
                           IF AVAILABLE MTRL THEN DO:
                              ASSIGN
                              BERLINKAB.LEVKOD = "1"
                              BERLINKAB.PRIS = MTRL.NPRIS.
                           END.                      
                        END.
                     END.
                  END.                  
               END.
               ELSE IF LOCKED(BERLINKAB) = TRUE THEN DO:                                 
                  IF   globforetag = "VAST"   THEN DO:
                     OUTPUT TO C:\PRO10\GURU\koll.txt APPEND.
                     PUT "PRISUBERN.P BERLINKAB" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR  string(TODAY) SKIP.
                     OUTPUT CLOSE.
                  END.
                  ELSE IF globforetag = "FORS" THEN DO:
                     OUTPUT TO C:\DELAD\PRO9\GURU\koll.txt APPEND.
                     PUT "PRISUBERN.P BERLINKAB" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR string(TODAY) SKIP.
                     OUTPUT CLOSE.
                  END.
                  ELSE IF globforetag = "GKAL" THEN DO:
                     OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
                     PUT "PRISUBERN.P BERLINKAB" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR string(TODAY) SKIP.
                     OUTPUT CLOSE.
                  END.
                  ELSE IF globforetag = "GRAN" THEN DO:
                     OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
                     PUT "PRISUBERN.P BERLINKAB" leverant   BEREDNING.OMRADE BEREDNING.BERAONR BEREDNING.AONR string(TODAY) SKIP.
                     OUTPUT CLOSE.
                  END.
               END.         
            END.
            GET NEXT m3q NO-LOCK.
         END.
         CLOSE QUERY m3q.
      END.
   END.                       
   GET NEXT bq NO-LOCK.
   
END.
CLOSE QUERY bq.         
         /*OPEN QUERY berqskydd FOR EACH KSKYDD WHERE KSKYDD.AONR = BEREDNING.BERAONR AND
         KSKYDD.OMRADE = BEREDNING.OMRADE AND KSKYDD.BERED = TRUE AND 
         KSKYDD.LEVKOD = leverant NO-LOCK.
         DO TRANSACTION:
            GET FIRST berqskydd EXCLUSIVE-LOCK.
            IF AVAILABLE KSKYDD THEN DO:               
               FIND FIRST MTRL WHERE MTRL.ENR = KSKYDD.ENR AND 
               MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 USE-INDEX LEV
               NO-LOCK NO-ERROR.
               IF AVAILABLE MTRL THEN DO:
                  ASSIGN
                  KSKYDD.PRIS = MTRL.NPRIS.
               END.                                            
            END.
         END.         
         REPEAT:
            DO TRANSACTION:            
               GET NEXT berqskydd EXCLUSIVE-LOCK.
               IF AVAILABLE KSKYDD THEN DO:               
                  FIND FIRST MTRL WHERE MTRL.ENR = KSKYDD.ENR AND 
                  MTRL.LEVKOD = leverant AND MTRL.KALKNR = 0 USE-INDEX LEV
                  NO-LOCK NO-ERROR.
                  IF AVAILABLE MTRL THEN DO:
                     ASSIGN
                     KSKYDD.PRIS = MTRL.NPRIS.
                  END.                                               
               END.      
               ELSE LEAVE.
            END.
         END.
         CLOSE QUERY berqskydd.*/


IF   globforetag = "VAST"   THEN DO:
   OUTPUT TO C:\PRO10\GURU\koll.txt APPEND.
   PUT "PRISUBERN.P uppdatering bered. klart. " leverant  substring(globforetag,1,10)  string(TODAY) SKIP.
   OUTPUT CLOSE.
END.
ELSE IF globforetag = "FORS" THEN DO:
   OUTPUT TO C:\DELAD\PRO9\GURU\koll.txt APPEND.
   PUT "PRISUBERN.P uppdatering bered. klart. " leverant  substring(globforetag,1,10)  string(TODAY) SKIP.
   OUTPUT CLOSE.
END.
ELSE IF globforetag = "GKAL" THEN DO:
   OUTPUT TO D:\DELAD\klient\PRO9\koll.txt APPEND.
   PUT "PRISUBERN.P uppdatering bered. klart. " leverant  substring(globforetag,1,10)  string(TODAY) SKIP.
   OUTPUT CLOSE.
END.
ELSE IF globforetag = "GRAN" THEN DO:
   OUTPUT TO d:\elpool\delad\pro9\wrk\koll.txt APPEND.
   PUT "PRISUBERN.P uppdatering bered. klart. " leverant  substring(globforetag,1,10)  string(TODAY) SKIP.
   OUTPUT CLOSE.
END.

{EUROPEANAMERICAN.I}
