/*PBERXTIY.P BYT ARTIKEL X TILL ARTIKEL Y  I EN VALD BEREDNING*/



{SMTRL.I}
&Scoped-define NEW         

DEFINE BUFFER berbuff FOR BERMTRL.
DEFINE BUFFER berfinnsEnrbuff FOR BERMTRL.
DEFINE BUFFER linbuff FOR BERLINKAB.
DEFINE BUFFER skyddbuff FOR KSKYDD.
    
DEFINE INPUT PARAMETER radval AS INTEGER NO-UNDO. 
DEFINE INPUT PARAMETER TABLE FOR spec_mtrl.
DEFINE INPUT PARAMETER varenr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER varlevkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER valomrade AS CHARACTER NO-UNDO. 
DEFINE QUERY mq FOR BERVAL, BERMTRL.
DEFINE VARIABLE inkberh AS HANDLE NO-UNDO.
DEFINE VARIABLE fragavar AS CHARACTER NO-UNDO. 
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.  
{INKSUMBERTEMP.I}  
{INKKOLL3.I}
{DYNPOSTFINNS.I}
IF inkopkollvar = TRUE THEN DO:               
   fragavar = "INKMTRL.BERNR = " + STRING(valaonr) + " AND INKMTRL.OMRADE = '" + valomrade + "'".  
   RUN finnspostdyn_UI (INPUT "INKMTRL",INPUT fragavar,OUTPUT musz).
   IF musz = FALSE THEN RUN gamminkop_UI.
   ELSE DO:
      musz = FALSE.
      RUN inkop_UI.
     
   END.                     
END.        
ELSE RUN gamminkop_UI.
PROCEDURE inkop_UI:
   DEFINE VARIABLE inkaonr AS CHARACTER NO-UNDO.
   DEFINE VARIABLE inkdelnr AS INTEGER NO-UNDO.
   RUN INKBERAPP.P PERSISTENT SET inkberh.
   RUN faltvarde_UI (INPUT "AONRAONR",OUTPUT fragavar).
   inkaonr = fragavar.
   RUN faltvarde_UI (INPUT "AONRDELNR",OUTPUT fragavar).
   inkdelnr = INTEGER(fragavar).
   RUN beredningskontroll_UI IN inkberh (INPUT inkaonr,INPUT inkdelnr,OUTPUT TABLE sumbernum_mtrl,OUTPUT TABLE sumberskapnum_mtrl).
   OPEN QUERY mq FOR EACH BERVAL WHERE BERVAL.AONR = valaonr AND BERVAL.OMRADE = valomrade AND BERVAL.KSKAP = FALSE NO-LOCK,
   EACH BERMTRL WHERE BERMTRL.AONR = BERVAL.AONR AND BERMTRL.OMRADE = BERVAL.OMRADE AND BERMTRL.NUM = BERVAL.NUM AND
   BERMTRL.ENR = varenr AND BERMTRL.LEVKOD = varlevkod NO-LOCK.   
   RUN uppdat_UI.               
   IF VALID-HANDLE(inkberh) THEN DO: 
      RUN InkAvs_UI IN inkberh.
      DELETE PROCEDURE inkberh NO-ERROR.
   END.   
END PROCEDURE.   
PROCEDURE gamminkop_UI :
        
   OPEN QUERY mq FOR EACH BERVAL WHERE BERVAL.AONR = valaonr AND BERVAL.OMRADE = valomrade
   /*AND BERVAL.ORT = ""*/ AND BERVAL.KSKAP = FALSE USE-INDEX OMR NO-LOCK,
   EACH BERMTRL WHERE BERMTRL.AONR = valaonr AND BERMTRL.OMRADE = valomrade AND BERMTRL.NUM = BERVAL.NUM AND
   BERMTRL.ENR = varenr AND BERMTRL.LEVKOD = varlevkod NO-LOCK.
   
   RUN uppdat_UI.
   
   
END PROCEDURE.
PROCEDURE uppdat_UI :
  
   GET FIRST mq NO-LOCK.
   DO WHILE AVAILABLE(BERMTRL):
      IF BERVAL.ORT = "" OR BERVAL.ORT = "STOPP" THEN DO TRANSACTION:
         FIND FIRST sumberskapnum_mtrl WHERE sumberskapnum_mtrl.NUM = BERMTRL.NUM AND 
         sumberskapnum_mtrl.SKAPNUM = BERMTRL.SKAPNUM AND
         sumberskapnum_mtrl.ENRORG = BERMTRL.ENR AND  
         sumberskapnum_mtrl.LEVKOD = BERMTRL.LEVKOD 
         NO-LOCK NO-ERROR.    
         /*inget inköp på enr och vald konst skapnum*/
         IF NOT AVAILABLE sumberskapnum_mtrl THEN DO:            
            GET CURRENT mq EXCLUSIVE-LOCK.
            IF radval =  1 OR radval =  2 OR radval =  4 THEN DO: 
           /*här    kollas det INTEGER om enr INTEGER finns redan på samma konst.*/        
               FOR EACH spec_mtrl:
                  FIND FIRST berfinnsEnrbuff WHERE
                  berfinnsEnrbuff.AONR = BERVAL.AONR AND berfinnsEnrbuff.OMRADE = BERVAL.OMRADE AND 
                  berfinnsEnrbuff.NUM = BERMTRL.NUM AND 
                  berfinnsEnrbuff.SKAPNUM = BERMTRL.SKAPNUM AND
                  berfinnsEnrbuff.ENR = spec_mtrl.ENR AND  
                  berfinnsEnrbuff.LEVKOD = spec_mtrl.LEVKOD AND 
                  ROWID(berfinnsEnrbuff) NE ROWID(BERMTRL) 
                  EXCLUSIVE-LOCK NO-ERROR.    
                  IF AVAILABLE berfinnsEnrbuff THEN DO:
                     IF radval = 1 THEN DO: 
                        berfinnsEnrbuff.ANTAL = berfinnsEnrbuff.ANTAL + BERMTRL.ANTAL.
                        DELETE BERMTRL.
                     END.
                     IF radval = 2 THEN DO: 
                        IF BERMTRL.ANTAL = 0 THEN.
                        ELSE berfinnsEnrbuff.ANTAL = berfinnsEnrbuff.ANTAL + spec_mtrl.ANTAL.
                     END.
                     IF radval = 4 THEN DO: 
                        
                        IF BERMTRL.ANTAL = 0 THEN.
                        ELSE berfinnsEnrbuff.ANTAL = berfinnsEnrbuff.ANTAL + (spec_mtrl.ANTAL * BERMTRL.ANTAL) .
                     END.
                       
                  END.
                  ELSE DO:
                     CREATE berbuff.
                     BUFFER-COPY BERMTRL TO berbuff.
                     ASSIGN
                     berbuff.ENR = spec_mtrl.ENR
                     berbuff.BENAMNING = spec_mtrl.BENAMNING
                     berbuff.ENHET = spec_mtrl.ENHET
                     berbuff.PRIS = spec_mtrl.NPRIS
                     berbuff.LEVKOD = spec_mtrl.LEVKOD.
                     IF radval = 2 THEN DO:                  
                        IF berbuff.ANTAL = 0 THEN.
                        ELSE berbuff.ANTAL = spec_mtrl.ANTAL.                  
                     END.
                     IF radval = 4 THEN DO:                 
                        /*lägg till multiplicerat med antal- om markerat enr har 3 i antal och valt antal på artikel som skall läggas till är 4 så blir antalet 3*4 = 12
                        ursprungliga lägg till -om markerat enr har 3 i antal och valt antal på artikel som skall läggas till är 4 så blir antalet 4*/ 
                        IF berbuff.ANTAL = 0 THEN.
                        ELSE berbuff.ANTAL = berbuff.ANTAL * spec_mtrl.ANTAL.                  
                     END.
                     IF radval = 1 THEN DO:            
                        DELETE BERMTRL.
                     END. 
                  END.   
               END.
            END.
            ELSE DO:
             /*  RADVAL = 3*/
               DELETE BERMTRL.
            END.
         END.   
      END.      
      GET NEXT mq NO-LOCK.
   END.
   /*
   IF radval NE 2 THEN DO:  
      OPEN QUERY lq FOR EACH BERUPP WHERE BERUPP.AONR = valaonr AND BERUPP.OMRADE = valomrade AND 
      BERUPP.UPPLAG NE ? AND BERUPP.ANTALRADER <= 100 AND BERUPP.DELNR = 0 NO-LOCK, 
      EACH BERLINKAB WHERE BERLINKAB.AONR = valaonr AND BERLINKAB.OMRADE = valomrade AND BERLINKAB.UPPLAG = BERUPP.UPPLAG AND
      BERLINKAB.ENR = varenr AND BERLINKAB.LEVKOD = varlevkod NO-LOCK.
      DO TRANSACTION:
         GET FIRST lq EXCLUSIVE-LOCK.
         IF AVAILABLE BERLINKAB THEN DO:
            IF radval = 1 THEN DO:
               FOR EACH spec_mtrl:
                  CREATE linbuff.
                  BUFFER-COPY BERLINKAB TO linbuff.
                  ASSIGN
                  linbuff.ENR = spec_mtrl.ENR
                  linbuff.BENAMNING = spec_mtrl.BENAMNING
                  linbuff.ENHET = spec_mtrl.ENHET
                  linbuff.PRIS = spec_mtrl.NPRIS
                  linbuff.LEVKOD = spec_mtrl.LEVKOD.                        
               END.
               DELETE BERLINKAB.
            END.
            ELSE DO:
               DELETE BERLINKAB.
            END.
         END.
      END.
      REPEAT:
         DO TRANSACTION:
            GET NEXT lq EXCLUSIVE-LOCK.
            IF AVAILABLE BERLINKAB THEN DO:
               IF radval = 1 THEN DO:
                  FOR EACH spec_mtrl:
                     CREATE linbuff.
                     BUFFER-COPY BERLINKAB TO linbuff.
                     ASSIGN
                     linbuff.ENR = spec_mtrl.ENR
                     linbuff.BENAMNING = spec_mtrl.BENAMNING
                     linbuff.ENHET = spec_mtrl.ENHET
                     linbuff.PRIS = spec_mtrl.NPRIS
                     linbuff.LEVKOD = spec_mtrl.LEVKOD.
                  END.
                  DELETE BERLINKAB.
               END.                   
               ELSE DO:
                  DELETE BERLINKAB.
               END.
            END.
            ELSE LEAVE.
         END.
      END.
      OPEN QUERY berqskydd FOR EACH KSKYDD WHERE KSKYDD.AONR = valaonr AND
      KSKYDD.OMRADE = valomrade AND KSKYDD.ENR = varenr AND 
      KSKYDD.LEVKOD = varlevkod AND KSKYDD.BERED = TRUE
      USE-INDEX OMR NO-LOCK.
      DO TRANSACTION:
         GET FIRST berqskydd EXCLUSIVE-LOCK.
         IF AVAILABLE KSKYDD THEN DO:
            IF radval = 1 THEN DO:
               FOR EACH spec_mtrl:
                  CREATE skyddbuff.
                  BUFFER-COPY KSKYDD TO skyddbuff.
                  ASSIGN
                  skyddbuff.ENR = spec_mtrl.ENR
                  skyddbuff.BENAMNING = spec_mtrl.BENAMNING
                  skyddbuff.ENHET = spec_mtrl.ENHET
                  skyddbuff.PRIS = spec_mtrl.NPRIS
                  skyddbuff.LEVKOD = spec_mtrl.LEVKOD.
               END.
               DELETE KSKYDD.
            END.  
            ELSE DO:
               DELETE KSKYDD.
            END.
         END.
      END.         
      REPEAT:
         DO TRANSACTION:
            GET NEXT berqskydd EXCLUSIVE-LOCK.
            IF AVAILABLE KSKYDD THEN DO:
               IF radval = 1 THEN DO:
                  FOR EACH spec_mtrl:
                     CREATE skyddbuff.
                     BUFFER-COPY KSKYDD TO skyddbuff.
                     ASSIGN
                     skyddbuff.ENR = spec_mtrl.ENR
                     skyddbuff.BENAMNING = spec_mtrl.BENAMNING
                     skyddbuff.ENHET = spec_mtrl.ENHET
                     skyddbuff.PRIS = spec_mtrl.NPRIS
                     skyddbuff.LEVKOD = spec_mtrl.LEVKOD.
                  END.
                  DELETE KSKYDD.
               END.            
               ELSE DO:
                  DELETE KSKYDD.
               END.
            END.
            ELSE LEAVE.
         END.
      END.         
      CLOSE QUERY berqskydd.
       
   END.
   */
END PROCEDURE.