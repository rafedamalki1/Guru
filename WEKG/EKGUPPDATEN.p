/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: EKGUPPDATEN.p
     
      Comment: <comment>
   */


DEFINE VARIABLE rakres AS INTEGER NO-UNDO.
DEFINE VARIABLE siffra AS LOGICAL NO-UNDO.
DEFINE VARIABLE ekgklog  AS INTEGER NO-UNDO.
DEFINE VARIABLE klgklass AS CHARACTER NO-UNDO.
DEFINE VARIABLE niva  AS CHARACTER NO-UNDO.
DEFINE VARIABLE frekuppdat AS LOGICAL NO-UNDO.
DEFINE VARIABLE p5ak AS CHARACTER NO-UNDO.


/*DEFINE INPUT  PARAMETER ekgklog AS INTEGER NO-UNDO.
DEFINE INPUT  PARAMETER klgklass AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER niva AS CHARACTER NO-UNDO.*/


   

{EKGSKAPTEMP.I}
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
/*test att uppdatera 1 kod*/
assign
ekgklog = 1
klgklass = "KLG1"
niva = "5".
p5ak = "87000120".
RUN uppdat_UI (INPUT ekgklog, INPUT klgklass,input niva,INPUT p5ak).

/*assign
ekgklog = 1
klgklass = "KLG1"
niva = "4".
RUN uppdat_UI (INPUT ekgklog, INPUT klgklass,input niva).

assign
ekgklog = 1
klgklass = "KLG1"
niva = "3".
RUN uppdat_UI (INPUT ekgklog, INPUT klgklass,input niva).

assign
ekgklog = 1
klgklass = "KLG1"
niva = "2".
RUN uppdat_UI (INPUT ekgklog, INPUT klgklass,input niva).

assign
ekgklog = 1
klgklass = "KLG1"
niva = "1".
RUN uppdat_UI (INPUT ekgklog, INPUT klgklass,input niva).

assign
ekgklog = 2
klgklass = "KLG2"
niva = "3".
RUN uppdat_UI (INPUT ekgklog, INPUT klgklass,input niva).

assign
ekgklog = 2
klgklass = "KLG2"
niva = "2".
RUN uppdat_UI (INPUT ekgklog, INPUT klgklass,input niva).

assign
ekgklog = 2
klgklass = "KLG2"
niva = "1".
RUN uppdat_UI (INPUT ekgklog, INPUT klgklass,input niva).
*/

      
PROCEDURE uppdat_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER tempniva AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER p5ak AS CHARACTER NO-UNDO.
   IF tempniva = "5" THEN DO:
      
      OPEN QUERY p5q FOR EACH EKGP5 WHERE EKGP5.EKGSUBID = tempsubid AND EKGP5.EBRKAT = tempebr AND EKGP5.P5ARBKOD = p5ak NO-LOCK. 
      GET FIRST p5q NO-LOCK.
      DO WHILE AVAILABLE(EKGP5):
         DO TRANSACTION:         
            FIND CURRENT EKGP5 EXCLUSIVE-LOCK.
            ASSIGN
            EKGP5.MONTTIM = 0 EKGP5.BERTIM = 0 EKGP5.MASKTIM = 0 EKGP5.EAMASK = 0 EKGP5.EAOVRIGT = 0 EKGP5.EAUTRUST = 0 EKGP5.ARBETEKOST = 0 EKGP5.MATERIELKOST = 0 EKGP5.MASKINKOST = 0 EKGP5.OVRIGT = 0  EKGP5.EA = 0 EKGP5.SUMMA = 0.
            FOR EACH EKGP5RESURS WHERE EKGP5RESURS.EKGSUBID = tempsubid AND EKGP5RESURS.EBRKAT = tempebr AND
            EKGP5RESURS.P5ARBKOD = EKGP5.P5ARBKOD NO-LOCK:
               FIND FIRST EKGRESURS WHERE EKGRESURS.RESURSNR = EKGP5RESURS.RESURSNR NO-LOCK NO-ERROR.
               IF AVAILABLE EKGP5RESURS THEN DO:
                  FIND FIRST EKGRESURSPRIS WHERE EKGRESURSPRIS.EKGSUBID = tempsubid AND EKGRESURSPRIS.EBRKAT = tempebr AND EKGRESURSPRIS.RESURSNR = EKGP5RESURS.RESURSNR NO-LOCK NO-ERROR.
                  FIND FIRST EKGREGEL WHERE EKGREGEL.REGELID = EKGRESURSPRIS.REGELID NO-LOCK NO-ERROR.
                                       
                  IF AVAILABLE EKGRESURSPRIS THEN DO:
                     FIND FIRST EKGRESURSPASLAG WHERE EKGRESURSPASLAG.EKGSUBID = tempsubid AND EKGRESURSPASLAG.EBRKAT = tempebr AND EKGRESURSPASLAG.RESURSNR = EKGP5RESURS.RESURSNR NO-LOCK NO-ERROR.
                     IF AVAILABLE EKGRESURSPASLAG  THEN DO:
                        FIND FIRST EKGPASLAG WHERE EKGPASLAG.EKGSUBID = tempsubid AND EKGPASLAG.EBRKAT = tempebr AND EKGPASLAG.PASLAGNR = EKGRESURSPASLAG.PASLAGNR NO-LOCK NO-ERROR.
                     END.                                                         
                     IF EKGRESURSPRIS.REGELID = 1 THEN DO:
                        /*MONTÖR EA*/
                        EKGP5.MONTTIM = EKGP5.MONTTIM + EKGP5RESURS.ANTAL.                           
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                           EKGP5.ARBETEKOST =  EKGP5.ARBETEKOST + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                        END.
                        ELSE DO:
                           /*SKALL ALLTID VARA PÅSLAG*/
                           EKGP5.ARBETEKOST = EKGP5.ARBETEKOST +  (EKGP5RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                        END.                                      
                     END.
                     IF EKGRESURSPRIS.REGELID = 2 THEN DO:
                        /*MASKIN EA*/
                        EKGP5.MASKTIM = EKGP5.MASKTIM + EKGP5RESURS.ANTAL.                           
                        EKGP5.EAMASK = EKGP5.EAMASK + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                        EKGP5.MASKINKOST = EKGP5.MASKINKOST + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                   
                     END.
                     IF EKGRESURSPRIS.REGELID = 3 THEN DO:
                        /*ÖVRIGT EA*/                                               
                        EKGP5.EAOVRIGT = EKGP5.EAOVRIGT + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                        EKGP5.OVRIGT = EKGP5.OVRIGT + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.PRIS).                               
                     END.
                     IF EKGRESURSPRIS.REGELID = 4 THEN DO:
                        /*Utrust ÖVRIGT EA*/                                
                        IF tempebr = "KLG1" THEN DO:                                       
                           EKGP5.EAOVRIGT = EKGP5.EAOVRIGT + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                           EKGP5.OVRIGT = EKGP5.OVRIGT + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.PRIS).
                        END.
                        IF tempebr = "KLG2" THEN DO:      
                           /*P5 FINNS INTE I KLG2*/                                 
                           EKGP5.EAUTRUST = EKGP5.EAUTRUST + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                           EKGP5.OVRIGT = EKGP5.OVRIGT + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.PRIS).
                        END.
                                                          
                     END.
                     IF EKGRESURSPRIS.REGELID = 5 THEN DO:
                        /*EAFAKTOR*/
                     END.   
                     IF EKGRESURSPRIS.REGELID = 6 THEN DO:
                        /*BEREDARE FINNS EJ I P5*/
                        EKGP5.BERTIM = EKGP5.BERTIM + EKGP5RESURS.ANTAL.                           
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                           EKGP5.ARBETEKOST =  EKGP5.ARBETEKOST + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                        END.
                        ELSE DO:
                           /*SKALL ALLTID VARA PÅSLAG*/
                           EKGP5.ARBETEKOST = EKGP5.ARBETEKOST +  (EKGP5RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                        END.                                  
                     END.
                     IF EKGRESURSPRIS.REGELID = 7 THEN DO:
                        /*TILLK BEREDARE FINNS EJ I P5*/
                        EKGP5.BERTIM = EKGP5.BERTIM + EKGP5RESURS.ANTAL.                           
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                           EKGP5.ARBETEKOST =  EKGP5.ARBETEKOST + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                        END.
                        ELSE DO:
                           /*SKALL ALLTID VARA PÅSLAG*/
                           EKGP5.ARBETEKOST = EKGP5.ARBETEKOST +  (EKGP5RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                        END.                                  
                     END.
                     IF EKGRESURSPRIS.REGELID = 8 THEN DO:
                        /*ÖVRIG KOSTNAD*/
                        EKGP5.OVRIGT = EKGP5.OVRIGT + (EKGP5RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                                          
                     END.     
                  END.                        
               END.                     
            END.
            EKGP5.EA = EKGP5.MONTTIM + EKGP5.EAMASK +  EKGP5.EAOVRIGT + EKGP5.EAUTRUST.   
            EKGP5.SUMMA = EKGP5.ARBETEKOST + EKGP5.MASKINKOST + EKGP5.MATERIELKOST + EKGP5.OVRIGT + EKGP5.UTRUSTKOST.   
         END.
         RELEASE EKGP5 NO-ERROR.
         GET NEXT p5q NO-LOCK.
      END.
      CLOSE QUERY p5q.
      
      
      OPEN QUERY p4q FOR EACH EKGP4 WHERE EKGP4.EKGSUBID = tempsubid AND EKGP4.EBRKAT = tempebr NO-LOCK. 
      GET FIRST p4q NO-LOCK.
      DO WHILE AVAILABLE(EKGP4):
         DO TRANSACTION:         
            FIND CURRENT EKGP4 EXCLUSIVE-LOCK.
            ASSIGN
            EKGP4.MONTTIM = 0 EKGP4.BERTIM = 0 EKGP4.MASKTIM = 0 EKGP4.EAMASK = 0 EKGP4.EAOVRIGT = 0 EKGP4.EAUTRUST = 0 EKGP4.ARBETEKOST = 0 EKGP4.MATERIELKOST = 0 EKGP4.MASKINKOST = 0 EKGP4.OVRIGT = 0  EKGP4.EA = 0 EKGP4.SUMMA = 0.
            frekuppdat = FALSE.
            FIND FIRST EKGP4FREKVENS WHERE EKGP4FREKVENS.EKGSUBID = tempsubid AND EKGP4FREKVENS.EBRKAT = tempebr AND EKGP4FREKVENS.P4ARBKOD = EKGP4.P4ARBKOD
            AND EKGP4FREKVENS.P5ARBKOD = p5ak NO-LOCK NO-ERROR.
            IF AVAILABLE EKGP4FREKVENS THEN frekuppdat = TRUE.      
            IF EKGP4.FREKVENS = TRUE AND frekuppdat = TRUE THEN DO:
               /*på P4 nivå finns inga resurser om frekvens = true*/
               
               FOR EACH EKGP4FREKVENS WHERE EKGP4FREKVENS.EKGSUBID = tempsubid AND EKGP4FREKVENS.EBRKAT = tempebr AND EKGP4FREKVENS.P4ARBKOD = EKGP4.P4ARBKOD NO-LOCK:
                  FIND FIRST EKGP5 WHERE EKGP5.EKGSUBID = tempsubid AND EKGP5.EBRKAT = tempebr AND EKGP5.P5ARBKOD = EKGP4FREKVENS.P5ARBKOD NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP5  THEN DO:
                     EKGP4.MONTTIM = EKGP4.MONTTIM + (EKGP4FREKVENS.ANTAL * EKGP5.MONTTIM).
                     EKGP4.MASKTIM = EKGP4.MASKTIM + (EKGP4FREKVENS.ANTAL * EKGP5.MASKTIM). 
                     EKGP4.ARBETEKOST =  EKGP4.ARBETEKOST + (EKGP4FREKVENS.ANTAL * EKGP5.ARBETEKOST).                                                                                         
                     EKGP4.MASKINKOST = EKGP4.MASKINKOST + (EKGP4FREKVENS.ANTAL * EKGP5.MASKINKOST).
                     EKGP4.OVRIGT = EKGP4.OVRIGT + (EKGP4FREKVENS.ANTAL * EKGP5.OVRIGT).
                     
                     EKGP4.EAMASK = EKGP4.EAMASK + (EKGP4FREKVENS.ANTAL * EKGP5.EAMASK).
                     EKGP4.EAOVRIGT = EKGP4.EAOVRIGT + (EKGP4FREKVENS.ANTAL * EKGP5.EAOVRIGT).
                     EKGP4.EAUTRUST = EKGP4.EAUTRUST + (EKGP4FREKVENS.ANTAL * EKGP5.EAUTRUST).
                  end.   
               END.               
            END.
            EKGP4.EA = EKGP4.MONTTIM + EKGP4.EAMASK +  EKGP4.EAOVRIGT + EKGP4.EAUTRUST.
            EKGP4.SUMMA = EKGP4.ARBETEKOST + EKGP4.MASKINKOST + EKGP4.MATERIELKOST + EKGP4.OVRIGT + EKGP4.UTRUSTKOST.   
         END.
         RELEASE EKGP4 NO-ERROR.
         GET NEXT p4q NO-LOCK.
      END.
      CLOSE QUERY p4q.
      
      OPEN QUERY p3q FOR EACH EKGP3 WHERE EKGP3.EKGSUBID = tempsubid AND EKGP3.EBRKAT = tempebr NO-LOCK. 
      GET FIRST p3q NO-LOCK.
      DO WHILE AVAILABLE(EKGP3):
         DO TRANSACTION:         
            FIND CURRENT EKGP3 EXCLUSIVE-LOCK.
            ASSIGN
            EKGP3.MONTTIM = 0 EKGP3.BERTIM = 0 EKGP3.MASKTIM = 0 EKGP3.EAMASK = 0 EKGP3.EAOVRIGT = 0 EKGP3.EAUTRUST = 0 EKGP3.ARBETEKOST = 0 EKGP3.MATERIELKOST = 0 EKGP3.MASKINKOST = 0 EKGP3.OVRIGT = 0 EKGP3.EA = 0 EKGP3.SUMMA = 0.
            frekuppdat = FALSE.
            FIND FIRST EKGP3FREKVENS WHERE EKGP3FREKVENS.EKGSUBID = tempsubid AND EKGP3FREKVENS.EBRKAT = tempebr AND EKGP3FREKVENS.P3ARBKOD = EKGP3.P3ARBKOD AND EKGP3FREKVENS.P3LOPNR = EKGP3.P3LOPNR NO-LOCK NO-ERROR.
            IF AVAILABLE EKGP3FREKVENS THEN frekuppdat = TRUE.
            IF EKGP3.FREKVENS = TRUE AND frekuppdat = TRUE THEN DO:                               
               FOR EACH EKGP3FREKVENS WHERE EKGP3FREKVENS.EKGSUBID = tempsubid AND EKGP3FREKVENS.EBRKAT = tempebr AND EKGP3FREKVENS.P3ARBKOD = EKGP3.P3ARBKOD AND EKGP3FREKVENS.P3LOPNR = EKGP3.P3LOPNR NO-LOCK:
                  FIND FIRST EKGP4 WHERE EKGP4.EKGSUBID = tempsubid AND EKGP4.EBRKAT = tempebr AND EKGP4.P4ARBKOD = EKGP3FREKVENS.P4ARBKOD NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP4  THEN DO:
                     EKGP3.MONTTIM = EKGP3.MONTTIM + (EKGP3FREKVENS.ANTAL * EKGP4.MONTTIM).
                     EKGP3.MASKTIM = EKGP3.MASKTIM + (EKGP3FREKVENS.ANTAL * EKGP4.MASKTIM). 
                     EKGP3.ARBETEKOST =  EKGP3.ARBETEKOST + (EKGP3FREKVENS.ANTAL * EKGP4.ARBETEKOST).                                                                                         
                     EKGP3.MASKINKOST = EKGP3.MASKINKOST + (EKGP3FREKVENS.ANTAL * EKGP4.MASKINKOST).
                     EKGP3.OVRIGT = EKGP3.OVRIGT + (EKGP3FREKVENS.ANTAL * EKGP4.OVRIGT).
                     
                     EKGP3.EAMASK = EKGP3.EAMASK + (EKGP3FREKVENS.ANTAL * EKGP4.EAMASK).
                     EKGP3.EAOVRIGT = EKGP3.EAOVRIGT + (EKGP3FREKVENS.ANTAL * EKGP4.EAOVRIGT).
                     EKGP3.EAUTRUST = EKGP3.EAUTRUST + (EKGP3FREKVENS.ANTAL * EKGP4.EAUTRUST).                     
                  END.   
               END.
               /*För P3 kan det vara både frekvens och tillkommande resurs 101-tillkommande beredare*/
               FOR EACH EKGP3RESURS WHERE EKGP3RESURS.EKGSUBID = tempsubid AND EKGP3RESURS.EBRKAT = tempebr AND
               EKGP3RESURS.P3ARBKOD = EKGP3.P3ARBKOD AND EKGP3RESURS.P3LOPNR = EKGP3.P3LOPNR NO-LOCK:
                  FIND FIRST EKGRESURS WHERE EKGRESURS.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP3RESURS THEN DO:
                     FIND FIRST EKGRESURSPRIS WHERE EKGRESURSPRIS.EKGSUBID = tempsubid AND EKGRESURSPRIS.EBRKAT = tempebr AND EKGRESURSPRIS.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR. 
                     FIND FIRST EKGREGEL WHERE EKGREGEL.REGELID = EKGRESURSPRIS.REGELID NO-LOCK NO-ERROR.
                                         
                     IF AVAILABLE EKGRESURSPRIS THEN DO:
                        FIND FIRST EKGRESURSPASLAG WHERE EKGRESURSPASLAG.EKGSUBID = tempsubid AND EKGRESURSPASLAG.EBRKAT = tempebr AND EKGRESURSPASLAG.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR.
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:
                           FIND FIRST EKGPASLAG WHERE EKGPASLAG.EKGSUBID = tempsubid AND EKGPASLAG.EBRKAT = tempebr AND EKGPASLAG.PASLAGNR = EKGRESURSPASLAG.PASLAGNR NO-LOCK NO-ERROR.
                        END.                                                         
                        
                        IF EKGRESURSPRIS.REGELID = 7 THEN DO:
                           /*TILLK BEREDARE FINNS EJ I P4*/
                           EKGP3.BERTIM = EKGP3.BERTIM + EKGP3RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP3.ARBETEKOST =  EKGP3.ARBETEKOST + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP3.ARBETEKOST = EKGP3.ARBETEKOST +  (EKGP3RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                             
                     END.                        
                  END.                     
               END.
                           
            END.
              
            EKGP3.EA = EKGP3.MONTTIM + EKGP3.EAMASK +  EKGP3.EAOVRIGT + EKGP3.EAUTRUST.
            EKGP3.SUMMA = EKGP3.ARBETEKOST + EKGP3.MASKINKOST + EKGP3.MATERIELKOST + EKGP3.OVRIGT + EKGP3.UTRUSTKOST.   
         END.
         RELEASE EKGP3 NO-ERROR.
         GET NEXT p3Q NO-LOCK.
      END.
      CLOSE QUERY p3q.
      
         
      
   END. 
   IF tempniva = "4" THEN DO:
      
      OPEN QUERY p4q FOR EACH EKGP4 WHERE EKGP4.EKGSUBID = tempsubid AND EKGP4.EBRKAT = tempebr NO-LOCK. 
      GET FIRST p4q NO-LOCK.
      DO WHILE AVAILABLE(EKGP4):
         DO TRANSACTION:         
            FIND CURRENT EKGP4 EXCLUSIVE-LOCK.
            ASSIGN
            EKGP4.MONTTIM = 0 EKGP4.BERTIM = 0 EKGP4.MASKTIM = 0 EKGP4.EAMASK = 0 EKGP4.EAOVRIGT = 0 EKGP4.EAUTRUST = 0 EKGP4.ARBETEKOST = 0 EKGP4.MATERIELKOST = 0 EKGP4.MASKINKOST = 0 EKGP4.OVRIGT = 0  EKGP4.EA = 0 EKGP4.SUMMA = 0.
            frekuppdat = FALSE.
            FIND FIRST EKGP4FREKVENS WHERE EKGP4FREKVENS.EKGSUBID = tempsubid AND EKGP4FREKVENS.EBRKAT = tempebr AND EKGP4FREKVENS.P4ARBKOD = EKGP4.P4ARBKOD NO-LOCK NO-ERROR.
            IF AVAILABLE EKGP4FREKVENS THEN frekuppdat = TRUE.      
            IF EKGP4.FREKVENS = TRUE AND frekuppdat = TRUE THEN DO:
               /*på P4 nivå finns inga resurser om frekvens = true*/
               
               FOR EACH EKGP4FREKVENS WHERE EKGP4FREKVENS.EKGSUBID = tempsubid AND EKGP4FREKVENS.EBRKAT = tempebr AND EKGP4FREKVENS.P4ARBKOD = EKGP4.P4ARBKOD NO-LOCK:
                  FIND FIRST EKGP5 WHERE EKGP5.EKGSUBID = tempsubid AND EKGP5.EBRKAT = tempebr AND EKGP5.P5ARBKOD = EKGP4FREKVENS.P5ARBKOD NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP5  THEN DO:
                     EKGP4.MONTTIM = EKGP4.MONTTIM + (EKGP4FREKVENS.ANTAL * EKGP5.MONTTIM).
                     EKGP4.MASKTIM = EKGP4.MASKTIM + (EKGP4FREKVENS.ANTAL * EKGP5.MASKTIM). 
                     EKGP4.ARBETEKOST =  EKGP4.ARBETEKOST + (EKGP4FREKVENS.ANTAL * EKGP5.ARBETEKOST).                                                                                         
                     EKGP4.MASKINKOST = EKGP4.MASKINKOST + (EKGP4FREKVENS.ANTAL * EKGP5.MASKINKOST).
                     EKGP4.OVRIGT = EKGP4.OVRIGT + (EKGP4FREKVENS.ANTAL * EKGP5.OVRIGT).
                     
                     EKGP4.EAMASK = EKGP4.EAMASK + (EKGP4FREKVENS.ANTAL * EKGP5.EAMASK).
                     EKGP4.EAOVRIGT = EKGP4.EAOVRIGT + (EKGP4FREKVENS.ANTAL * EKGP5.EAOVRIGT).
                     EKGP4.EAUTRUST = EKGP4.EAUTRUST + (EKGP4FREKVENS.ANTAL * EKGP5.EAUTRUST).
                  end.   
               END.               
            END.
            ELSE DO:                 
               FOR EACH EKGP4RESURS WHERE EKGP4RESURS.EKGSUBID = tempsubid AND EKGP4RESURS.EBRKAT = tempebr AND
               EKGP4RESURS.P4ARBKOD = EKGP4.P4ARBKOD NO-LOCK:
                  FIND FIRST EKGRESURS WHERE EKGRESURS.RESURSNR = EKGP4RESURS.RESURSNR NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP4RESURS THEN DO:
                     FIND FIRST EKGRESURSPRIS WHERE EKGRESURSPRIS.EKGSUBID = tempsubid AND EKGRESURSPRIS.EBRKAT = tempebr AND EKGRESURSPRIS.RESURSNR = EKGP4RESURS.RESURSNR NO-LOCK NO-ERROR.
                     FIND FIRST EKGREGEL WHERE EKGREGEL.REGELID = EKGRESURSPRIS.REGELID NO-LOCK NO-ERROR.                                          
                     IF AVAILABLE EKGRESURSPRIS THEN DO:
                        FIND FIRST EKGRESURSPASLAG WHERE EKGRESURSPASLAG.EKGSUBID = tempsubid AND EKGRESURSPASLAG.EBRKAT = tempebr AND EKGRESURSPASLAG.RESURSNR = EKGP4RESURS.RESURSNR NO-LOCK NO-ERROR.
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:
                           FIND FIRST EKGPASLAG WHERE EKGPASLAG.EKGSUBID = tempsubid AND EKGPASLAG.EBRKAT = tempebr AND EKGPASLAG.PASLAGNR = EKGRESURSPASLAG.PASLAGNR NO-LOCK NO-ERROR.
                        END.                                                         
                        IF EKGRESURSPRIS.REGELID = 1 THEN DO:
                           /*MONTÖR EA*/
                           EKGP4.MONTTIM = EKGP4.MONTTIM + EKGP4RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP4.ARBETEKOST =  EKGP4.ARBETEKOST + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP4.ARBETEKOST = EKGP4.ARBETEKOST +  (EKGP4RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                      
                        END.
                        IF EKGRESURSPRIS.REGELID = 2 THEN DO:
                           /*MASKIN EA*/
                           EKGP4.MASKTIM = EKGP4.MASKTIM + EKGP4RESURS.ANTAL.                           
                           EKGP4.EAMASK = EKGP4.EAMASK + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                           EKGP4.MASKINKOST = EKGP4.MASKINKOST + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                   
                        END.
                        IF EKGRESURSPRIS.REGELID = 3 THEN DO:
                           /*ÖVRIGT EA*/                                               
                           EKGP4.EAOVRIGT = EKGP4.EAOVRIGT + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                           EKGP4.OVRIGT = EKGP4.OVRIGT + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.PRIS).                               
                        END.
                        IF EKGRESURSPRIS.REGELID = 4 THEN DO:
                           /*Utrust ÖVRIGT EA*/         
                           IF tempebr = "KLG1" THEN DO:                                       
                              EKGP4.EAOVRIGT = EKGP4.EAOVRIGT + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                              EKGP4.OVRIGT = EKGP4.OVRIGT + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.PRIS).
                           END.
                           IF tempebr = "KLG2" THEN DO:      
                              /*P4 FINNS INTE I KLG2*/                                 
                              EKGP4.EAUTRUST = EKGP4.EAUTRUST + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                              EKGP4.OVRIGT = EKGP4.OVRIGT + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.PRIS).
                           END.
                                                             
                        END.
                        IF EKGRESURSPRIS.REGELID = 4 THEN DO:
                           /*EAFAKTOR*/
                        END.   
                        IF EKGRESURSPRIS.REGELID = 6 THEN DO:
                           /*BEREDARE FINNS EJ I P4*/
                           EKGP4.BERTIM = EKGP4.BERTIM + EKGP4RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP4.ARBETEKOST =  EKGP4.ARBETEKOST + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP4.ARBETEKOST = EKGP4.ARBETEKOST +  (EKGP4RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                        IF EKGRESURSPRIS.REGELID = 7 THEN DO:
                           /*TILLK BEREDARE FINNS EJ I P4*/
                           EKGP4.BERTIM = EKGP4.BERTIM + EKGP4RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP4.ARBETEKOST =  EKGP4.ARBETEKOST + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP4.ARBETEKOST = EKGP4.ARBETEKOST +  (EKGP4RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                        IF EKGRESURSPRIS.REGELID = 8 THEN DO:
                           /*ÖVRIG KOSTNAD*/
                           EKGP4.OVRIGT = EKGP4.OVRIGT + (EKGP4RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                                          
                        END.     
                     END.                        
                  END.                     
               END.
            END.      
            EKGP4.EA = EKGP4.MONTTIM + EKGP4.EAMASK +  EKGP4.EAOVRIGT + EKGP4.EAUTRUST.
            EKGP4.SUMMA = EKGP4.ARBETEKOST + EKGP4.MASKINKOST + EKGP4.MATERIELKOST + EKGP4.OVRIGT + EKGP4.UTRUSTKOST.   
         END.
         RELEASE EKGP4 NO-ERROR.
         GET NEXT p4q NO-LOCK.
      END.
      CLOSE QUERY p4q.
      
         
      
      
   END.
   IF tempniva = "3" THEN DO:
      
      OPEN QUERY p3q FOR EACH EKGP3 WHERE EKGP3.EKGSUBID = tempsubid AND EKGP3.EBRKAT = tempebr NO-LOCK. 
      GET FIRST p3q NO-LOCK.
      DO WHILE AVAILABLE(EKGP3):
         DO TRANSACTION:         
            FIND CURRENT EKGP3 EXCLUSIVE-LOCK.
            ASSIGN
            EKGP3.MONTTIM = 0 EKGP3.BERTIM = 0 EKGP3.MASKTIM = 0 EKGP3.EAMASK = 0 EKGP3.EAOVRIGT = 0 EKGP3.EAUTRUST = 0 EKGP3.ARBETEKOST = 0 EKGP3.MATERIELKOST = 0 EKGP3.MASKINKOST = 0 EKGP3.OVRIGT = 0 EKGP3.EA = 0 EKGP3.SUMMA = 0.
            frekuppdat = FALSE.
            FIND FIRST EKGP3FREKVENS WHERE EKGP3FREKVENS.EKGSUBID = tempsubid AND EKGP3FREKVENS.EBRKAT = tempebr AND EKGP3FREKVENS.P3ARBKOD = EKGP3.P3ARBKOD AND EKGP3FREKVENS.P3LOPNR = EKGP3.P3LOPNR NO-LOCK NO-ERROR.
            IF AVAILABLE EKGP3FREKVENS THEN frekuppdat = TRUE.
            IF EKGP3.FREKVENS = TRUE AND frekuppdat = TRUE THEN DO:                               
               FOR EACH EKGP3FREKVENS WHERE EKGP3FREKVENS.EKGSUBID = tempsubid AND EKGP3FREKVENS.EBRKAT = tempebr AND EKGP3FREKVENS.P3ARBKOD = EKGP3.P3ARBKOD AND EKGP3FREKVENS.P3LOPNR = EKGP3.P3LOPNR NO-LOCK:
                  FIND FIRST EKGP4 WHERE EKGP4.EKGSUBID = tempsubid AND EKGP4.EBRKAT = tempebr AND EKGP4.P4ARBKOD = EKGP3FREKVENS.P4ARBKOD NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP4  THEN DO:
                     EKGP3.MONTTIM = EKGP3.MONTTIM + (EKGP3FREKVENS.ANTAL * EKGP4.MONTTIM).
                     EKGP3.MASKTIM = EKGP3.MASKTIM + (EKGP3FREKVENS.ANTAL * EKGP4.MASKTIM). 
                     EKGP3.ARBETEKOST =  EKGP3.ARBETEKOST + (EKGP3FREKVENS.ANTAL * EKGP4.ARBETEKOST).                                                                                         
                     EKGP3.MASKINKOST = EKGP3.MASKINKOST + (EKGP3FREKVENS.ANTAL * EKGP4.MASKINKOST).
                     EKGP3.OVRIGT = EKGP3.OVRIGT + (EKGP3FREKVENS.ANTAL * EKGP4.OVRIGT).
                     
                     EKGP3.EAMASK = EKGP3.EAMASK + (EKGP3FREKVENS.ANTAL * EKGP4.EAMASK).
                     EKGP3.EAOVRIGT = EKGP3.EAOVRIGT + (EKGP3FREKVENS.ANTAL * EKGP4.EAOVRIGT).
                     EKGP3.EAUTRUST = EKGP3.EAUTRUST + (EKGP3FREKVENS.ANTAL * EKGP4.EAUTRUST).                     
                  END.   
               END.
               /*För P3 kan det vara både frekvens och tillkommande resurs 101-tillkommande beredare*/
               FOR EACH EKGP3RESURS WHERE EKGP3RESURS.EKGSUBID = tempsubid AND EKGP3RESURS.EBRKAT = tempebr AND
               EKGP3RESURS.P3ARBKOD = EKGP3.P3ARBKOD AND EKGP3RESURS.P3LOPNR = EKGP3.P3LOPNR NO-LOCK:
                  FIND FIRST EKGRESURS WHERE EKGRESURS.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP3RESURS THEN DO:
                     FIND FIRST EKGRESURSPRIS WHERE EKGRESURSPRIS.EKGSUBID = tempsubid AND EKGRESURSPRIS.EBRKAT = tempebr AND EKGRESURSPRIS.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR. 
                     FIND FIRST EKGREGEL WHERE EKGREGEL.REGELID = EKGRESURSPRIS.REGELID NO-LOCK NO-ERROR.
                                         
                     IF AVAILABLE EKGRESURSPRIS THEN DO:
                        FIND FIRST EKGRESURSPASLAG WHERE EKGRESURSPASLAG.EKGSUBID = tempsubid AND EKGRESURSPASLAG.EBRKAT = tempebr AND EKGRESURSPASLAG.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR.
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:
                           FIND FIRST EKGPASLAG WHERE EKGPASLAG.EKGSUBID = tempsubid AND EKGPASLAG.EBRKAT = tempebr AND EKGPASLAG.PASLAGNR = EKGRESURSPASLAG.PASLAGNR NO-LOCK NO-ERROR.
                        END.                                                         
                        
                        IF EKGRESURSPRIS.REGELID = 7 THEN DO:
                           /*TILLK BEREDARE FINNS EJ I P4*/
                           EKGP3.BERTIM = EKGP3.BERTIM + EKGP3RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP3.ARBETEKOST =  EKGP3.ARBETEKOST + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP3.ARBETEKOST = EKGP3.ARBETEKOST +  (EKGP3RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                             
                     END.                        
                  END.                     
               END.
                           
            END.
            ELSE DO:               
               FOR EACH EKGP3RESURS WHERE EKGP3RESURS.EKGSUBID = tempsubid AND EKGP3RESURS.EBRKAT = tempebr AND
               EKGP3RESURS.P3ARBKOD = EKGP3.P3ARBKOD AND EKGP3RESURS.P3LOPNR = EKGP3.P3LOPNR  NO-LOCK:
                  FIND FIRST EKGRESURS WHERE EKGRESURS.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP3RESURS THEN DO:
                     FIND FIRST EKGRESURSPRIS WHERE EKGRESURSPRIS.EKGSUBID = tempsubid AND EKGRESURSPRIS.EBRKAT = tempebr AND EKGRESURSPRIS.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR.
                     FIND FIRST EKGREGEL WHERE EKGREGEL.REGELID = EKGRESURSPRIS.REGELID NO-LOCK NO-ERROR.
                                         
                     IF AVAILABLE EKGRESURSPRIS THEN DO:
                        FIND FIRST EKGRESURSPASLAG WHERE EKGRESURSPASLAG.EKGSUBID = tempsubid AND EKGRESURSPASLAG.EBRKAT = tempebr AND EKGRESURSPASLAG.RESURSNR = EKGP3RESURS.RESURSNR NO-LOCK NO-ERROR.
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:
                           FIND FIRST EKGPASLAG WHERE EKGPASLAG.EKGSUBID = tempsubid AND EKGPASLAG.EBRKAT = tempebr AND EKGPASLAG.PASLAGNR = EKGRESURSPASLAG.PASLAGNR NO-LOCK NO-ERROR.
                        END.                                                         
                        IF EKGRESURSPRIS.REGELID = 1 THEN DO:
                           /*MONTÖR EA*/
                           EKGP3.MONTTIM = EKGP3.MONTTIM + EKGP3RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP3.ARBETEKOST =  EKGP3.ARBETEKOST + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP3.ARBETEKOST = EKGP3.ARBETEKOST +  (EKGP3RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                      
                        END.
                        IF EKGRESURSPRIS.REGELID = 2 THEN DO:
                           /*MASKIN EA*/
                           EKGP3.MASKTIM = EKGP3.MASKTIM + EKGP3RESURS.ANTAL.                           
                           EKGP3.EAMASK = EKGP3.EAMASK + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                           EKGP3.MASKINKOST = EKGP3.MASKINKOST + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                   
                        END.
                        IF EKGRESURSPRIS.REGELID = 3 THEN DO:
                           /*ÖVRIGT EA*/                                               
                           EKGP3.EAOVRIGT = EKGP3.EAOVRIGT + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                           EKGP3.OVRIGT = EKGP3.OVRIGT + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS).                               
                        END.
                        IF EKGRESURSPRIS.REGELID = 4 THEN DO:
                           /*Utrust ÖVRIGT EA*/         
                           IF tempebr = "KLG1" THEN DO:                                       
                              EKGP3.EAOVRIGT = EKGP3.EAOVRIGT + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                              EKGP3.OVRIGT = EKGP3.OVRIGT + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS).
                           END.
                           IF tempebr = "KLG2" THEN DO:      
                              /*P4 FINNS INTE I KLG2*/                                 
                              EKGP3.EAUTRUST = EKGP3.EAUTRUST + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                              EKGP3.OVRIGT = EKGP3.OVRIGT + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS).
                           END.
                                                             
                        END.
                        IF EKGRESURSPRIS.REGELID = 4 THEN DO:
                           /*EAFAKTOR*/
                        END.   
                        IF EKGRESURSPRIS.REGELID = 6 THEN DO:
                           /*BEREDARE FINNS EJ I P4*/
                           EKGP3.BERTIM = EKGP3.BERTIM + EKGP3RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP3.ARBETEKOST =  EKGP3.ARBETEKOST + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP3.ARBETEKOST = EKGP3.ARBETEKOST +  (EKGP3RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                        IF EKGRESURSPRIS.REGELID = 7 THEN DO:
                           /*TILLK BEREDARE FINNS EJ I P4*/
                           EKGP3.BERTIM = EKGP3.BERTIM + EKGP3RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP3.ARBETEKOST =  EKGP3.ARBETEKOST + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP3.ARBETEKOST = EKGP3.ARBETEKOST +  (EKGP3RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                        IF EKGRESURSPRIS.REGELID = 8 THEN DO:
                           /*ÖVRIG KOSTNAD*/
                           EKGP3.OVRIGT = EKGP3.OVRIGT + (EKGP3RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                                          
                        END.     
                     END.                        
                  END.                     
               END.
            END.  
            EKGP3.EA = EKGP3.MONTTIM + EKGP3.EAMASK +  EKGP3.EAOVRIGT + EKGP3.EAUTRUST.
            EKGP3.SUMMA = EKGP3.ARBETEKOST + EKGP3.MASKINKOST + EKGP3.MATERIELKOST + EKGP3.OVRIGT + EKGP3.UTRUSTKOST.   
         END.
         RELEASE EKGP3 NO-ERROR.
         GET NEXT p3Q NO-LOCK.
      END.
      CLOSE QUERY p3q.   
      
   END.
   
   
   IF tempniva = "2" THEN DO:
      
      OPEN QUERY p2q FOR EACH EKGP2 WHERE EKGP2.EKGSUBID = tempsubid AND EKGP2.EBRKAT = tempebr NO-LOCK. 
      GET FIRST p2q NO-LOCK.
      DO WHILE AVAILABLE(EKGP2):
         DO TRANSACTION:         
            FIND CURRENT EKGP2 EXCLUSIVE-LOCK.
            ASSIGN
            EKGP2.MONTTIM = 0 EKGP2.BERTIM = 0 EKGP2.MASKTIM = 0 EKGP2.EAMASK = 0 EKGP2.EAOVRIGT = 0 EKGP2.EAUTRUST = 0 EKGP2.ARBETEKOST = 0 EKGP2.MATERIELKOST = 0 EKGP2.MASKINKOST = 0 EKGP2.OVRIGT = 0 EKGP2.EA = 0 EKGP2.SUMMA = 0.
            frekuppdat = FALSE.
            FIND FIRST EKGP2FREKVENS WHERE EKGP2FREKVENS.EKGSUBID = tempsubid AND EKGP2FREKVENS.EBRKAT = tempebr AND EKGP2FREKVENS.P2ARBKOD = EKGP2.P2ARBKOD AND EKGP2FREKVENS.P2LOPNR = EKGP2.P2LOPNR NO-LOCK NO-ERROR.
            IF AVAILABLE EKGP2FREKVENS THEN frekuppdat = TRUE. 
            IF EKGP2.FREKVENS = TRUE AND frekuppdat = TRUE THEN DO:              
               FOR EACH EKGP2FREKVENS WHERE EKGP2FREKVENS.EKGSUBID = tempsubid AND EKGP2FREKVENS.EBRKAT = tempebr AND EKGP2FREKVENS.P2ARBKOD = EKGP2.P2ARBKOD AND EKGP2FREKVENS.P2LOPNR = EKGP2.P2LOPNR NO-LOCK:
                  FIND FIRST EKGP3 WHERE EKGP3.EKGSUBID = tempsubid AND EKGP3.EBRKAT = tempebr AND EKGP3.P3ARBKOD = EKGP2FREKVENS.P3ARBKOD AND EKGP3.P3LOPNR = EKGP2FREKVENS.P3LOPNR NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP3  THEN DO:
                     EKGP2.MONTTIM = EKGP2.MONTTIM + (EKGP2FREKVENS.ANTAL * EKGP3.MONTTIM).
                     EKGP2.MASKTIM = EKGP2.MASKTIM + (EKGP2FREKVENS.ANTAL * EKGP3.MASKTIM).
                     EKGP2.BERTIM = EKGP2.BERTIM + (EKGP2FREKVENS.ANTAL * EKGP3.BERTIM). 
                     EKGP2.ARBETEKOST =  EKGP2.ARBETEKOST + (EKGP2FREKVENS.ANTAL * EKGP3.ARBETEKOST).                                                                                         
                     EKGP2.MASKINKOST = EKGP2.MASKINKOST + (EKGP2FREKVENS.ANTAL * EKGP3.MASKINKOST).
                     EKGP2.OVRIGT = EKGP2.OVRIGT + (EKGP2FREKVENS.ANTAL * EKGP3.OVRIGT).
                     
                     EKGP2.EAMASK = EKGP2.EAMASK + (EKGP2FREKVENS.ANTAL * EKGP3.EAMASK).
                     EKGP2.EAOVRIGT = EKGP2.EAOVRIGT + (EKGP2FREKVENS.ANTAL * EKGP3.EAOVRIGT).
                     EKGP2.EAUTRUST = EKGP2.EAUTRUST + (EKGP2FREKVENS.ANTAL * EKGP3.EAUTRUST).                     
                  END.   
               END.
               /*För P2 kan det vara både frekvens och tillkommande resurs 101-tillkommande beredare och övriga kostnader*/
               FOR EACH EKGP2RESURS WHERE EKGP2RESURS.EKGSUBID = tempsubid AND EKGP2RESURS.EBRKAT = tempebr AND
               EKGP2RESURS.P2ARBKOD = EKGP2.P2ARBKOD AND EKGP2RESURS.P2LOPNR = EKGP2.P2LOPNR NO-LOCK:
                  FIND FIRST EKGRESURS WHERE EKGRESURS.RESURSNR = EKGP2RESURS.RESURSNR NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP2RESURS THEN DO:
                     FIND FIRST EKGRESURSPRIS WHERE EKGRESURSPRIS.EKGSUBID = tempsubid AND EKGRESURSPRIS.EBRKAT = tempebr AND EKGRESURSPRIS.RESURSNR = EKGP2RESURS.RESURSNR NO-LOCK NO-ERROR.
                     FIND FIRST EKGREGEL WHERE EKGREGEL.REGELID = EKGRESURSPRIS.REGELID NO-LOCK NO-ERROR.
                                          
                     IF AVAILABLE EKGRESURSPRIS THEN DO:
                        FIND FIRST EKGRESURSPASLAG WHERE EKGRESURSPASLAG.EKGSUBID = tempsubid AND EKGRESURSPASLAG.EBRKAT = tempebr AND EKGRESURSPASLAG.RESURSNR = EKGP2RESURS.RESURSNR NO-LOCK NO-ERROR.
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:
                           FIND FIRST EKGPASLAG WHERE EKGPASLAG.EKGSUBID = tempsubid AND EKGPASLAG.EBRKAT = tempebr AND EKGPASLAG.PASLAGNR = EKGRESURSPASLAG.PASLAGNR NO-LOCK NO-ERROR.
                        END.                                                         
                        
                        IF EKGRESURSPRIS.REGELID = 7 THEN DO:
                           /*TILLK BEREDARE FINNS EJ I P4*/
                           EKGP2.BERTIM = EKGP2.BERTIM + EKGP2RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP2.ARBETEKOST =  EKGP2.ARBETEKOST + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP2.ARBETEKOST = EKGP2.ARBETEKOST +  (EKGP2RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                        IF EKGRESURSPRIS.REGELID = 8 THEN DO:
                           /*ÖVRIG KOSTNAD*/
                           EKGP2.OVRIGT = EKGP2.OVRIGT + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                                          
                        END.                              
                     END.                        
                  END.                     
               END.                           
            END.
            ELSE DO:               
               FOR EACH EKGP2RESURS WHERE EKGP2RESURS.EKGSUBID = tempsubid AND EKGP2RESURS.EBRKAT = tempebr AND
               EKGP2RESURS.P2ARBKOD = EKGP2.P2ARBKOD AND  EKGP2RESURS.P2LOPNR = EKGP2.P2LOPNR NO-LOCK:
                  FIND FIRST EKGRESURS WHERE EKGRESURS.RESURSNR = EKGP2RESURS.RESURSNR NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP2RESURS THEN DO:
                     FIND FIRST EKGRESURSPRIS WHERE EKGRESURSPRIS.EKGSUBID = tempsubid AND EKGRESURSPRIS.EBRKAT = tempebr AND EKGRESURSPRIS.RESURSNR = EKGP2RESURS.RESURSNR NO-LOCK NO-ERROR.
                     FIND FIRST EKGREGEL WHERE EKGREGEL.REGELID = EKGRESURSPRIS.REGELID NO-LOCK NO-ERROR.                                         
                     IF AVAILABLE EKGRESURSPRIS THEN DO:
                        FIND FIRST EKGRESURSPASLAG WHERE EKGRESURSPASLAG.EKGSUBID = tempsubid AND EKGRESURSPASLAG.EBRKAT = tempebr AND EKGRESURSPASLAG.RESURSNR = EKGP2RESURS.RESURSNR NO-LOCK NO-ERROR.
                        IF AVAILABLE EKGRESURSPASLAG  THEN DO:
                           FIND FIRST EKGPASLAG WHERE EKGPASLAG.EKGSUBID = tempsubid AND EKGPASLAG.EBRKAT = tempebr AND EKGPASLAG.PASLAGNR = EKGRESURSPASLAG.PASLAGNR NO-LOCK NO-ERROR.
                        END.                                                         
                        IF EKGRESURSPRIS.REGELID = 1 THEN DO:
                           /*MONTÖR EA*/
                           EKGP2.MONTTIM = EKGP2.MONTTIM + EKGP2RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP2.ARBETEKOST =  EKGP2.ARBETEKOST + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP2.ARBETEKOST = EKGP2.ARBETEKOST +  (EKGP2RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                      
                        END.
                        IF EKGRESURSPRIS.REGELID = 2 THEN DO:
                           /*MASKIN EA*/
                           EKGP2.MASKTIM = EKGP2.MASKTIM + EKGP2RESURS.ANTAL.                           
                           EKGP2.EAMASK = EKGP2.EAMASK + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                           EKGP2.MASKINKOST = EKGP2.MASKINKOST + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                   
                        END.
                        IF EKGRESURSPRIS.REGELID = 3 THEN DO:
                           /*ÖVRIGT EA*/                                               
                           EKGP2.EAOVRIGT = EKGP2.EAOVRIGT + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                           EKGP2.OVRIGT = EKGP2.OVRIGT + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS).                               
                        END.
                        IF EKGRESURSPRIS.REGELID = 4 THEN DO:
                           /*Utrust ÖVRIGT EA*/         
                           IF tempebr = "KLG1" THEN DO:                                       
                              EKGP2.EAOVRIGT = EKGP2.EAOVRIGT + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                              EKGP2.OVRIGT = EKGP2.OVRIGT + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS).
                           END.
                           IF tempebr = "KLG2" THEN DO:      
                              /*P4 FINNS INTE I KLG2*/                                 
                              EKGP2.EAUTRUST = EKGP2.EAUTRUST + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.EA).                           
                              EKGP2.OVRIGT = EKGP2.OVRIGT + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS).
                           END.
                                                             
                        END.
                        IF EKGRESURSPRIS.REGELID = 4 THEN DO:
                           /*EAFAKTOR*/
                        END.   
                        IF EKGRESURSPRIS.REGELID = 6 THEN DO:
                           /*BEREDARE FINNS EJ I P4*/
                           EKGP2.BERTIM = EKGP2.BERTIM + EKGP2RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP2.ARBETEKOST =  EKGP2.ARBETEKOST + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP2.ARBETEKOST = EKGP2.ARBETEKOST +  (EKGP2RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                        IF EKGRESURSPRIS.REGELID = 7 THEN DO:
                           /*TILLK BEREDARE FINNS EJ I P4*/
                           EKGP2.BERTIM = EKGP2.BERTIM + EKGP2RESURS.ANTAL.                           
                           IF AVAILABLE EKGRESURSPASLAG  THEN DO:                              
                              EKGP2.ARBETEKOST =  EKGP2.ARBETEKOST + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS * ( 1 + EKGPASLAG.PASLAG)).
                           END.
                           ELSE DO:
                              /*SKALL ALLTID VARA PÅSLAG*/
                              EKGP2.ARBETEKOST = EKGP2.ARBETEKOST +  (EKGP2RESURS.ANTAL  * EKGRESURSPRIS.PRIS).
                           END.                                  
                        END.
                        IF EKGRESURSPRIS.REGELID = 8 THEN DO:
                           /*ÖVRIG KOSTNAD*/
                           EKGP2.OVRIGT = EKGP2.OVRIGT + (EKGP2RESURS.ANTAL * EKGRESURSPRIS.PRIS).                                                          
                        END.     
                     END.                        
                  END.                     
               END.
            END.

            OPEN QUERY p2mtrlq FOR EACH EKGP2MTRL WHERE EKGP2MTRL.EKGSUBID = tempsubid AND EKGP2MTRL.EBRKAT = tempebr 
            AND EKGP2MTRL.P2ARBKOD = EKGP2.P2ARBKOD AND EKGP2MTRL.P2LOPNR = EKGP2.P2LOPNR NO-LOCK. 
            GET FIRST p2mtrlq NO-LOCK.
            DO WHILE AVAILABLE(EKGP2MTRL):
               FIND FIRST EKGMTRL WHERE EKGMTRL.EKGSUBID = tempsubid AND EKGMTRL.EBRKAT = tempebr AND EKGMTRL.ARTNR = EKGP2MTRL.ARTNR NO-LOCK NO-ERROR.
               IF AVAILABLE EKGMTRL THEN DO:
                  EKGP2.MATERIELKOST =  EKGP2.MATERIELKOST + (EKGP2MTRL.ANTAL * EKGMTRL.KALKYLPRIS).
               END.
               
               GET NEXT p2mtrlq NO-LOCK.
            END.
            CLOSE QUERY p2mtrlq.
            
            EKGP2.EA = EKGP2.MONTTIM + EKGP2.EAMASK +  EKGP2.EAOVRIGT + EKGP2.EAUTRUST.
            EKGP2.SUMMA = EKGP2.ARBETEKOST + EKGP2.MASKINKOST + EKGP2.MATERIELKOST + EKGP2.OVRIGT + EKGP2.UTRUSTKOST. 
         END.
         RELEASE EKGP2 NO-ERROR.
         GET NEXT p2Q NO-LOCK.
      END.
      CLOSE QUERY p2q.   
      
      
      
         
      

   END.
   
  
   IF tempniva = "1" THEN DO:
      
      OPEN QUERY p1q FOR EACH EKGP1 WHERE EKGP1.EKGSUBID = tempsubid AND EKGP1.EBRKAT = tempebr NO-LOCK. 
      GET FIRST p1q NO-LOCK.
      DO WHILE AVAILABLE(EKGP1):
         DO TRANSACTION:         
            FIND CURRENT EKGP1 EXCLUSIVE-LOCK.
            ASSIGN
            EKGP1.MONTTIM = 0 EKGP1.BERTIM = 0 EKGP1.MASKTIM = 0 EKGP1.EAMASK = 0 EKGP1.EAOVRIGT = 0 EKGP1.EAUTRUST = 0 EKGP1.ARBETEKOST = 0 EKGP1.MATERIELKOST = 0 EKGP1.MASKINKOST = 0 EKGP1.OVRIGT = 0  EKGP1.EA = 0 EKGP1.SUMMA = 0.
            IF EKGP1.FREKVENS = TRUE THEN DO:                
               FOR EACH EKGP1FREKVENS WHERE EKGP1FREKVENS.EKGSUBID = tempsubid AND EKGP1FREKVENS.EBRKAT = tempebr AND EKGP1FREKVENS.P1ARBKOD = EKGP1.P1ARBKOD AND EKGP1FREKVENS.P1LOPNR = EKGP1.P1LOPNR NO-LOCK:
                  FIND FIRST EKGP2 WHERE EKGP2.EKGSUBID = tempsubid AND EKGP2.EBRKAT = tempebr AND EKGP2.P2ARBKOD = EKGP1FREKVENS.P2ARBKOD AND EKGP2.P2LOPNR = EKGP1FREKVENS.P2LOPNR NO-LOCK NO-ERROR.
                  IF AVAILABLE EKGP2  THEN DO:
                     EKGP1.MONTTIM = EKGP1.MONTTIM + (EKGP1FREKVENS.ANTAL * EKGP2.MONTTIM).
                     EKGP1.MASKTIM = EKGP1.MASKTIM + (EKGP1FREKVENS.ANTAL * EKGP2.MASKTIM).
                     EKGP1.BERTIM = EKGP1.BERTIM + (EKGP1FREKVENS.ANTAL * EKGP2.BERTIM). 
                     EKGP1.ARBETEKOST =  EKGP1.ARBETEKOST + (EKGP1FREKVENS.ANTAL * EKGP2.ARBETEKOST).                                                                                         
                     EKGP1.MASKINKOST = EKGP1.MASKINKOST + (EKGP1FREKVENS.ANTAL * EKGP2.MASKINKOST).
                     EKGP1.OVRIGT = EKGP1.OVRIGT + (EKGP1FREKVENS.ANTAL * EKGP2.OVRIGT).
                     EKGP1.MATERIELKOST = EKGP1.MATERIELKOST + (EKGP1FREKVENS.ANTAL * EKGP2.MATERIELKOST).
                     
                     EKGP1.EAMASK = EKGP1.EAMASK + (EKGP1FREKVENS.ANTAL * EKGP2.EAMASK).
                     EKGP1.EAOVRIGT = EKGP1.EAOVRIGT + (EKGP1FREKVENS.ANTAL * EKGP2.EAOVRIGT).
                     EKGP1.EAUTRUST = EKGP1.EAUTRUST + (EKGP1FREKVENS.ANTAL * EKGP2.EAUTRUST).          
                                
                  END.   
               END.                                          
            END.
            EKGP1.EA = EKGP1.MONTTIM + EKGP1.EAMASK +  EKGP1.EAOVRIGT + EKGP1.EAUTRUST.
            EKGP1.SUMMA = EKGP1.ARBETEKOST + EKGP1.MASKINKOST + EKGP1.MATERIELKOST + EKGP1.OVRIGT + EKGP1.UTRUSTKOST.     
         END.
         RELEASE EKGP1 NO-ERROR.
         GET NEXT p1Q NO-LOCK.
      END.
      CLOSE QUERY p1q.   
      
   END.   
     
      
   
END PROCEDURE.

/*EKGP5
EKGP5RESURS
EKGRESURS
EKGRESURSPRIS
EKGRESURS

EKGPASLAG
EKGRESURSPASLAG
EKGREGEL

CREATE EKGP5RESURS.
ASSIGN 
ekgp5resurstemp.EKGSUBID  = tempsubid
ekgp5resurstemp.EBRKAT = tempebr
ekgp5resurstemp.P5ARBKOD = esqldat.DATAFALT[1]      
ekgp5resurstemp.P5LOPNR = 0
ekgp5resurstemp.RESURSNR = 1 
ekgp5resurstemp.ANTAL = ekgp5temp.MONTTIM.*/



PROCEDURE skapskarptabell_UI :
MESSAGE "skapskarptabell_UI".
   FOR EACH ekgresurstemp:
      CREATE ekgresurs.
      BUFFER-COPY ekgresurstemp  TO ekgresurs.      
   END.
   FOR EACH ekgresurspristemp:
      CREATE ekgresurspris.
      BUFFER-COPY ekgresurspristemp TO ekgresurspris.      
   END.
   FOR EACH ekgpaslagtemp :
      CREATE ekgpaslag.
      BUFFER-COPY ekgpaslagtemp TO ekgpaslag.      
   END.
   FOR EACH ekgnivatemp:
      CREATE ekgniva.
      BUFFER-COPY ekgnivatemp TO ekgniva.      
   END.
   FOR EACH ekgresursnivatemp:
      CREATE ekgresursniva.
      BUFFER-COPY ekgresursnivatemp TO ekgresursniva.      
   END.
   FOR EACH ekgresurspaslagtemp:
      CREATE ekgresurspaslag.
      BUFFER-COPY ekgresurspaslagtemp TO ekgresurspaslag.      
   END.
   FOR EACH ekgregeltemp:
      CREATE ekgregel.
      BUFFER-COPY ekgregeltemp  TO ekgregel.      
   END.
   FOR EACH ekgmtrltemp:
      CREATE ekgmtrl.
      BUFFER-COPY ekgmtrltemp TO ekgmtrl.      
   END.
   FOR EACH ekgp5temp:
      CREATE ekgp5.
      BUFFER-COPY ekgp5temp TO ekgp5.      
   END.
   FOR EACH ekgp5resurstemp:
      CREATE ekgp5resurs.
      BUFFER-COPY ekgp5resurstemp  TO ekgp5resurs.      
   END.
   FOR EACH ekgp4temp:
      CREATE ekgp4.
      BUFFER-COPY ekgp4temp TO ekgp4.      
   END.
   FOR EACH ekgp4resurstemp:
      CREATE ekgp4resurs.
      BUFFER-COPY ekgp4resurstemp  TO ekgp4resurs.      
   END.
   FOR EACH ekgp4frekvenstemp:
      CREATE ekgp4frekvens.
      BUFFER-COPY ekgp4frekvenstemp  TO ekgp4frekvens.      
   END.
   FOR EACH ekgp3arbkodtemp:
      CREATE ekgp3arbkod.
      BUFFER-COPY ekgp3arbkodtemp TO ekgp3arbkod.      
   END.
   FOR EACH ekgp3temp:
      CREATE ekgp3.
      BUFFER-COPY ekgp3temp TO ekgp3.      
   END.
   FOR EACH ekgp3resurstemp:
      CREATE ekgp3resurs.
      BUFFER-COPY ekgp3resurstemp  TO ekgp3resurs.      
   END.
   FOR EACH ekgp3frekvenstemp:
      CREATE ekgp3frekvens.
      BUFFER-COPY ekgp3frekvenstemp  TO ekgp3frekvens.      
   END.
   FOR EACH ekgp2arbkodtemp:
      CREATE ekgp2arbkod.
      BUFFER-COPY ekgp2arbkodtemp TO ekgp2arbkod.      
   END.
   FOR EACH ekgp2temp:
      CREATE ekgp2.
      BUFFER-COPY ekgp2temp TO ekgp2.      
   END.
   FOR EACH ekgp2resurstemp:
      CREATE ekgp2resurs.
      BUFFER-COPY ekgp2resurstemp  TO ekgp2resurs.      
   END.
   FOR EACH ekgp2frekvenstemp:
      CREATE ekgp2frekvens.
      BUFFER-COPY ekgp2frekvenstemp  TO ekgp2frekvens.      
   END.
   FOR EACH ekgp2mtrltemp:
      CREATE ekgp2mtrl.
      BUFFER-COPY ekgp2mtrltemp TO ekgp2mtrl.      
   END.
   FOR EACH ekgp1arbkodtemp:
      CREATE ekgp1arbkod.
      BUFFER-COPY ekgp1arbkodtemp TO ekgp1arbkod.      
   END.
   FOR EACH ekgp1temp:
      CREATE ekgp1.
      BUFFER-COPY ekgp1temp TO ekgp1.      
   END.
   
   FOR EACH ekgp1frekvenstemp:
      CREATE ekgp1frekvens.
      BUFFER-COPY ekgp1frekvenstemp  TO ekgp1frekvens.      
   END.
   
END PROCEDURE.

PROCEDURE rensskarptabell_UI:
   
   
   FOR EACH ekgregel :
      DELETE ekgregel.           
   END.
END PROCEDURE.

PROCEDURE rensskarptabellklg_UI :
   DEFINE INPUT  PARAMETER tempsubid AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER tempebr AS CHARACTER NO-UNDO.
   
   FOR EACH ekgresurs :  
      IF ekgresurs.RESURSNR GE 1 AND ekgresurs.RESURSNR LE 39 THEN DO:       
         DELETE ekgresurs.
      END.
      ELSE IF ekgresurs.RESURSNR = 101 THEN DO:       
         DELETE ekgresurs.
      END.                
   END.   
   FOR EACH ekgresurspris /*WHERE  ekgresurspris.EKGSUBID  = tempsubid AND  ekgresurspris.EBRKAT = tempebr*/ :
      /*Ta bort resurs 1-39 och 101 för både klg1 och klg2*/
      IF ekgresurspris.RESURSNR GE 1 AND ekgresurspris.RESURSNR LE 39 THEN DO:       
         DELETE ekgresurspris.
      END.
      ELSE IF ekgresurspris.RESURSNR = 101 THEN DO:       
         DELETE ekgresurspris.
      END.         
   END.
   FOR EACH ekgresursniva /*WHERE  ekgresurspris.EKGSUBID  = tempsubid AND  ekgresurspris.EBRKAT = tempebr*/ :
      /*Ta bort resurs 1-39 och 101 för både klg1 och klg2*/
      IF ekgresursniva.RESURSNR GE 1 AND ekgresursniva.RESURSNR LE 39 THEN DO:       
         DELETE ekgresursniva.
      END.
      ELSE IF ekgresursniva.RESURSNR = 101 THEN DO:       
         DELETE ekgresursniva.
      END.         
   END.
   FOR EACH ekgpaslag WHERE  ekgpaslag.EKGSUBID  = tempsubid AND  ekgpaslag.EBRKAT = tempebr:      
      DELETE ekgpaslag.      
   END.
   FOR EACH ekgniva WHERE  ekgniva.EKGSUBID  = tempsubid AND  ekgniva.EBRKAT = tempebr:   
      DELETE ekgniva.      
   END.
   
   FOR EACH ekgresurspaslag WHERE  ekgresurspaslag.EKGSUBID  = tempsubid AND  ekgresurspaslag.EBRKAT = tempebr:
      DELETE ekgresurspaslag.     
   END.
   
   FOR EACH ekgmtrl WHERE  ekgmtrl.EKGSUBID  = tempsubid AND  ekgmtrl.EBRKAT = tempebr:
      DELETE ekgmtrl.           
   END.
   FOR EACH ekgp5 WHERE  ekgp5.EKGSUBID  = tempsubid AND  ekgp5.EBRKAT = tempebr:
      DELETE ekgp5.     
   END.
   FOR EACH ekgp5resurs WHERE  ekgp5resurs.EKGSUBID  = tempsubid AND  ekgp5resurs.EBRKAT = tempebr:
      DELETE ekgp5resurs.           
   END.
   FOR EACH ekgp4 WHERE  ekgp4.EKGSUBID  = tempsubid AND  ekgp4.EBRKAT = tempebr:
      DELETE ekgp4.           
   END.
   FOR EACH ekgp4resurs WHERE  ekgp4resurs.EKGSUBID  = tempsubid AND  ekgp4resurs.EBRKAT = tempebr:
      DELETE ekgp4resurs.           
   END.
   FOR EACH ekgp4frekvens WHERE  ekgp4frekvens.EKGSUBID  = tempsubid AND  ekgp4frekvens.EBRKAT = tempebr:
      DELETE ekgp4frekvens.           
   END.
   FOR EACH ekgp3arbkod WHERE  ekgp3arbkod.EKGSUBID  = tempsubid AND  ekgp3arbkod.EBRKAT = tempebr:
      DELETE ekgp3arbkod.       
   END.
   FOR EACH ekgp3 WHERE  ekgp3.EKGSUBID  = tempsubid AND  ekgp3.EBRKAT = tempebr:
      DELETE ekgp3.       
   END.
   FOR EACH ekgp3resurs WHERE  ekgp3resurs.EKGSUBID  = tempsubid AND  ekgp3resurs.EBRKAT = tempebr:
      DELETE ekgp3resurs.           
   END.
   FOR EACH ekgp3frekvens WHERE ekgp3frekvens.EKGSUBID  = tempsubid AND  ekgp3frekvens.EBRKAT = tempebr:
      DELETE ekgp3frekvens.            
   END.
   FOR EACH ekgp2arbkod WHERE  ekgp2arbkod.EKGSUBID  = tempsubid AND  ekgp2arbkod.EBRKAT = tempebr:
      DELETE ekgp2arbkod.       
   END.
   FOR EACH ekgp2 WHERE  ekgp2.EKGSUBID  = tempsubid AND  ekgp2.EBRKAT = tempebr:
      DELETE ekgp2.            
   END.
   FOR EACH ekgp2resurs WHERE  ekgp2resurs.EKGSUBID  = tempsubid AND  ekgp2resurs.EBRKAT = tempebr:
      DELETE ekgp2resurs.           
   END.
   FOR EACH ekgp2frekvens WHERE  ekgp2frekvens.EKGSUBID  = tempsubid AND  ekgp2frekvens.EBRKAT = tempebr:
      DELETE ekgp2frekvens.            
   END.
   FOR EACH ekgp2mtrl WHERE  ekgp2mtrl.EKGSUBID  = tempsubid AND  ekgp2mtrl.EBRKAT = tempebr:
      DELETE ekgp2mtrl.           
   END.
   FOR EACH ekgp1arbkod WHERE  ekgp1arbkod.EKGSUBID  = tempsubid AND  ekgp1arbkod.EBRKAT = tempebr:
      DELETE ekgp1arbkod.       
   END.
   FOR EACH ekgp1 WHERE  ekgp1.EKGSUBID  = tempsubid AND  ekgp1.EBRKAT = tempebr:
      DELETE ekgp1.           
   END.
   
   FOR EACH ekgp1frekvens WHERE  ekgp1frekvens.EKGSUBID  = tempsubid AND  ekgp1frekvens.EBRKAT = tempebr:
      DELETE ekgp1frekvens.           
   END.
   
END PROCEDURE.



PROCEDURE isSiffra_UI :
   DEFINE INPUT PARAMETER varde AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER artecken AS LOGICAL NO-UNDO.
   DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
   
   ascivarde = ASC(varde).
   /*siffror*/
   IF ascivarde >= 48 AND ascivarde <= 57 THEN artecken = TRUE.
   
   /*/*stora bokstäver*/
   ELSE IF ascivarde >= 65 AND ascivarde <= 90 THEN artecken = TRUE.
   
   /*små bokstäver*/
   ELSE IF ascivarde >= 97 AND ascivarde <= 122 THEN artecken = TRUE.
   
   /*Å Ä Ö å ä ö*/
   ELSE IF ascivarde = 197 OR ascivarde = 196 OR ascivarde = 214 OR ascivarde = 229 OR 
      ascivarde = 228 OR ascivarde = 246 THEN artecken = TRUE.
   ELSE artecken = FALSE.*/
END PROCEDURE.



