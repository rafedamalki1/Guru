/*FAOBERU.I*/

&Scoped-define NEW NEW 
{REGVAR.I}
DEFINE VARIABLE ovkod LIKE TIDREGITAB.OVERTIDTILL NO-UNDO. 
DEFINE VARIABLE ovantal LIKE TIDREGITAB.OVERANTAL NO-UNDO.
DEFINE VARIABLE ovbelopp LIKE EKRAPPRESULT.EOVERBELOPP NO-UNDO. 
DEFINE VARIABLE multi AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE VARIABLE typover AS CHARACTER NO-UNDO.
PROCEDURE utan_UI:
   /*obs inga tidfelar */
   GET FIRST faktaonrq NO-LOCK.
   DO WHILE AVAILABLE(FAKTAONR):  
      OPEN QUERY fkq FOR EACH FAKTKOLL WHERE FAKTKOLL.FAKTNR = FAKTPLAN.FAKTNR AND      
      FAKTKOLL.AONR = FAKTAONR.AONR AND FAKTKOLL.DELNR = FAKTAONR.DELNR AND FAKTKOLL.SLUTFAKT = FALSE AND 
      FAKTKOLL.BESTID NE "" NO-LOCK.
      GET FIRST fkq NO-LOCK.
      DO WHILE AVAILABLE(FAKTKOLL): 
         IF FAKTKOLL.SENASTTID NE ? THEN DO:      
            IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
               IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                  TIDREGITAB.PERSONALKOD = FAKTKOLL.BESTID AND
                  TIDREGITAB.DATUM > FAKTKOLL.SENASTTID 
                  NO-LOCK.  
               END.
               ELSE DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
                  TIDREGITAB.PERSONALKOD = FAKTKOLL.BESTID AND
                  TIDREGITAB.DATUM > FAKTKOLL.SENASTTID 
                  NO-LOCK.  
               END.
               
            END.
            ELSE DO:
               IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                  TIDREGITAB.PERSONALKOD = FAKTKOLL.BESTID AND
                  TIDREGITAB.DATUM > FAKTKOLL.SENASTTID AND 
                  YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
                  NO-LOCK.  
               END.
               ELSE DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
                  TIDREGITAB.PERSONALKOD = FAKTKOLL.BESTID AND
                  TIDREGITAB.DATUM > FAKTKOLL.SENASTTID AND 
                  YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
                  NO-LOCK.
               END.
            END.   
         END.
         ELSE DO:
            IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
               IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                  TIDREGITAB.PERSONALKOD = FAKTKOLL.BESTID 
                  NO-LOCK. 
               END.
               ELSE DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  TIDREGITAB.DELNR = FAKTAONR.DELNR AND
                  TIDREGITAB.PERSONALKOD = FAKTKOLL.BESTID 
                  NO-LOCK. 
               END.
            END.
            ELSE DO:
               IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                  TIDREGITAB.PERSONALKOD = FAKTKOLL.BESTID AND
                  TIDREGITAB.TIDLOG = TRUE AND 
                  YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
                  NO-LOCK. 
               END.
               ELSE DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
                  TIDREGITAB.PERSONALKOD = FAKTKOLL.BESTID AND
                  TIDREGITAB.TIDLOG = TRUE AND 
                  YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
                  NO-LOCK. 
               END.
            END.   
         END.      
         GET FIRST tidq NO-LOCK. 
         DO WHILE AVAILABLE(TIDREGITAB):         
            IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN musz = musz.
            ELSE DO:
               CREATE tidertemp.
               BUFFER-COPY TIDREGITAB TO tidertemp.
               IF Guru.Konstanter:varforetypval[9] = 1 THEN tidertemp.DELNR = 0.
               tidertemp.DEBET = TRUE.
            END.
            GET NEXT tidq NO-LOCK. 
   
         END.
         RUN anst_UI.
         dnrhj = FAKTAONR.DELNR.
         IF Guru.Konstanter:varforetypval[9] = 1 THEN dnrhj = 0.
         FOR EACH tidertemp WHERE tidertemp.AONR = FAKTAONR.AONR AND tidertemp.DELNR = dnrhj:
            IF Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR 
            Guru.Konstanter:globforetag = "ETA"  OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:               
               IF tidertemp.DATUM < 01/01/2000 THEN musz = TRUE.
            END.                                              
            IF musz = TRUE THEN DO: 
               musz = FALSE.
            END.
            ELSE DO: 
               IF tidertemp.DATUM <= FILL-IN-TOMDAT THEN DO:
                  IF FAKTKOLL.SENASTTID = ? THEN RUN nytid_UI.
                  ELSE IF FAKTKOLL.SENASTTID < tidertemp.DATUM THEN RUN nytid_UI.                     
               END.              
            END.              
         END.                    
         GET NEXT fkq NO-LOCK.   
      END.
      GET NEXT faktaonrq NO-LOCK.   
   END.        
   RUN bef_UI.
END PROCEDURE.
PROCEDURE anst_UI :
   DEFINE VARIABLE pkod AS CHARACTER NO-UNDO.
   REPEAT:
      FIND FIRST tidertemp WHERE tidertemp.KOD = "" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidertemp THEN LEAVE.
      pkod = tidertemp.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.  
         IF NOT AVAILABLE ANSTFORMTAB THEN FIND FIRST ANSTFORMTAB WHERE NO-LOCK NO-ERROR.
      END.
      ELSE FIND FIRST ANSTFORMTAB WHERE NO-LOCK NO-ERROR.
      FOR EACH tidertemp WHERE tidertemp.KOD = "" AND tidertemp.PERSONALKOD = pkod:
         tidertemp.KOD = ANSTFORMTAB.KOD.
      END.
   END.
END PROCEDURE.
PROCEDURE overegnat_UI :
   GET FIRST faktaonrq NO-LOCK.
   DO WHILE AVAILABLE(FAKTAONR):   
      FIND FIRST FAKTKOLL WHERE FAKTKOLL.FAKTNR = FAKTPLAN.FAKTNR AND      
      FAKTKOLL.AONR = FAKTAONR.AONR AND 
      FAKTKOLL.DELNR = FAKTAONR.DELNR AND FAKTKOLL.SLUTFAKT = FALSE
      USE-INDEX FASTK NO-LOCK NO-ERROR.
      IF AVAILABLE FAKTKOLL THEN DO:
         IF FAKTKOLL.VECKOKORD NE "" THEN DO:      
            IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
               IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                   /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                   TIDREGITAB.VECKOKORD >= FAKTKOLL.VECKOKORD 
                   AND TIDREGITAB.TIDLOG = TRUE USE-INDEX AONR NO-LOCK.  
               END.
               ELSE DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
                  TIDREGITAB.VECKOKORD >= FAKTKOLL.VECKOKORD AND 
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX AONR NO-LOCK.  
               END.
               
            END.
            ELSE DO:
               IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                  TIDREGITAB.VECKOKORD >= FAKTKOLL.VECKOKORD 
                  AND TIDREGITAB.TIDLOG = TRUE AND 
                  YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
                  USE-INDEX AONR NO-LOCK.  
               END.
               ELSE DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
                  TIDREGITAB.VECKOKORD >= FAKTKOLL.VECKOKORD AND 
                  TIDREGITAB.TIDLOG = TRUE AND 
                  YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
                  USE-INDEX AONR NO-LOCK.
               END.
            END.   
         END.
         ELSE DO:
            IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
               IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                  TIDREGITAB.VECKOKORD NE "" AND 
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX AONR NO-LOCK. 
               END.
               ELSE DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  TIDREGITAB.DELNR = FAKTAONR.DELNR AND TIDREGITAB.VECKOKORD NE "" AND 
                  TIDREGITAB.TIDLOG = TRUE USE-INDEX AONR NO-LOCK. 
               END.
            END.
            ELSE DO:
               IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  /*TIDREGITAB.DELNR = FAKTAONR.DELNR AND */
                  TIDREGITAB.VECKOKORD NE ""  AND 
                  TIDREGITAB.TIDLOG = TRUE AND 
                  YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
                  USE-INDEX AONR NO-LOCK. 
               END.
               ELSE DO:
                  OPEN QUERY tidq FOR EACH TIDREGITAB WHERE TIDREGITAB.AONR = FAKTAONR.AONR AND 
                  TIDREGITAB.DELNR = FAKTAONR.DELNR AND 
                  TIDREGITAB.VECKOKORD NE ""  AND 
                  TIDREGITAB.TIDLOG = TRUE AND 
                  YEAR(TIDREGITAB.DATUM) = YEAR(FILL-IN-TOMDAT)
                  USE-INDEX AONR NO-LOCK. 
               END.
            END.   
         END.
         GET FIRST tidq NO-LOCK. 
         DO WHILE AVAILABLE(TIDREGITAB):         
            IF TIDREGITAB.PRISTYP = "FRÅNVARO." THEN musz = musz.
            ELSE DO:
               CREATE tidertemp.
               BUFFER-COPY TIDREGITAB TO tidertemp.
               IF Guru.Konstanter:varforetypval[9] = 1 THEN tidertemp.DELNR = 0.
               tidertemp.DEBET = TRUE.
            END.
            GET NEXT tidq NO-LOCK. 
         END.
         RUN anst_UI.
         RUN tidfel_UI.
         dnrhj = FAKTAONR.DELNR.
         IF Guru.Konstanter:varforetypval[9] = 1 THEN dnrhj = 0.
         FOR EACH tidertemp WHERE tidertemp.AONR = FAKTAONR.AONR AND tidertemp.DELNR = dnrhj:
            IF Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ESMA" OR 
            Guru.Konstanter:globforetag = "ETA"  OR Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" THEN DO:               
               IF tidertemp.DATUM < 01/01/2000 THEN musz = TRUE.
            END.                                              
            IF musz = TRUE THEN DO: 
               musz = FALSE.
            END.
            ELSE DO: 
               IF tidertemp.FELDATUM = ? THEN DO:
                  IF tidertemp.DATUM <= FILL-IN-TOMDAT THEN DO:
                     IF FAKTKOLL.SENASTTID = ? THEN RUN nytid_UI.
                     ELSE IF FAKTKOLL.VECKOKORD = "" THEN DO:
                        /*obs bara om VECKOKORD = "" =<*/
                        IF FAKTKOLL.SENASTTID <= tidertemp.DATUM THEN RUN nytid_UI.
                     END.   
                     ELSE IF tidertemp.VECKOKORD = FAKTKOLL.VECKOKORD AND FAKTKOLL.SENASTTID < tidertemp.DATUM THEN RUN nytid_UI.
                     ELSE IF tidertemp.VECKOKORD > FAKTKOLL.VECKOKORD THEN RUN nytid_UI.                                                                 
                  END.
               END.
               ELSE DO:
                  IF tidertemp.FELDATUM <= FILL-IN-TOMDAT THEN DO:
                     IF FAKTKOLL.SENASTTID = ? THEN RUN nytid_UI.
                     ELSE IF FAKTKOLL.VECKOKORD = "" THEN DO:
                        IF FAKTKOLL.SENASTTID <= tidertemp.FELDATUM THEN RUN nytid_UI.
                     END. 
                     ELSE IF tidertemp.FELKORD = FAKTKOLL.VECKOKORD AND FAKTKOLL.SENASTTID < tidertemp.FELDATUM THEN RUN nytid_UI.
                     ELSE IF tidertemp.FELKORD > FAKTKOLL.VECKOKORD THEN RUN nytid_UI.                                                                 
                  END.
               END.
            END.           
         END. 
      END.
      GET NEXT faktaonrq NO-LOCK.   
   END.        
   RUN bef_UI.
   
END PROCEDURE.
      
PROCEDURE tidfel_UI :
   IF FAKTKOLL.VECKOKORD NE "" THEN DO:      
      IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
         IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
             /*TIDFEL.DELNR = FAKTAONR.DELNR AND */
             TIDFEL.FELKORD >= FAKTKOLL.VECKOKORD 
             AND TIDFEL.TIDLOG = TRUE USE-INDEX AONR NO-LOCK.  
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            TIDFEL.DELNR = FAKTAONR.DELNR AND 
            TIDFEL.FELKORD >= FAKTKOLL.VECKOKORD AND 
            TIDFEL.TIDLOG = TRUE USE-INDEX AONR NO-LOCK.  
         END.
         
      END.
      ELSE DO:
         IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            /*TIDFEL.DELNR = FAKTAONR.DELNR AND */
            TIDFEL.FELKORD >= FAKTKOLL.VECKOKORD 
            AND TIDFEL.TIDLOG = TRUE AND 
            YEAR(TIDFEL.DATUM) = YEAR(FILL-IN-TOMDAT)
            USE-INDEX AONR NO-LOCK.  
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            TIDFEL.DELNR = FAKTAONR.DELNR AND 
            TIDFEL.FELKORD >= FAKTKOLL.VECKOKORD AND 
            TIDFEL.TIDLOG = TRUE AND 
            YEAR(TIDFEL.DATUM) = YEAR(FILL-IN-TOMDAT)
            USE-INDEX AONR NO-LOCK.
         END.
      END.   
   END.
   ELSE DO:
      IF FAKTREGLER.SUMALLAR = TRUE THEN DO:
         IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            /*TIDFEL.DELNR = FAKTAONR.DELNR AND */
            TIDFEL.FELKORD NE "" AND 
            TIDFEL.TIDLOG = TRUE USE-INDEX AONR NO-LOCK. 
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            TIDFEL.DELNR = FAKTAONR.DELNR AND TIDFEL.FELKORD NE "" AND 
            TIDFEL.TIDLOG = TRUE USE-INDEX AONR NO-LOCK. 
         END.
      END.
      ELSE DO:
         IF Guru.Konstanter:varforetypval[9] = 1 THEN DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            /*TIDFEL.DELNR = FAKTAONR.DELNR AND */
            TIDFEL.FELKORD NE ""  AND 
            TIDFEL.TIDLOG = TRUE AND 
            YEAR(TIDFEL.DATUM) = YEAR(FILL-IN-TOMDAT)
            USE-INDEX AONR NO-LOCK. 
         END.
         ELSE DO:
            OPEN QUERY tidq FOR EACH TIDFEL WHERE TIDFEL.AONR = FAKTAONR.AONR AND 
            TIDFEL.DELNR = FAKTAONR.DELNR AND 
            TIDFEL.FELKORD NE ""  AND 
            TIDFEL.TIDLOG = TRUE AND 
            YEAR(TIDFEL.DATUM) = YEAR(FILL-IN-TOMDAT)
            USE-INDEX AONR NO-LOCK. 
         END.
      END.   
   END.
   GET FIRST tidq NO-LOCK. 
   DO WHILE AVAILABLE(TIDFEL):         
      IF TIDFEL.PRISTYP = "FRÅNVARO." THEN musz = musz.
      ELSE DO:
         CREATE tidertemp.
         BUFFER-COPY TIDFEL TO tidertemp.                     
          IF Guru.Konstanter:varforetypval[9] = 1 THEN tidertemp.DELNR = 0.
      END.
      GET NEXT tidq NO-LOCK. 
   END.
   RUN anst_UI.
END PROCEDURE.

PROCEDURE bef_UI :
   
   FIND FIRST tidtemp WHERE tidtemp.NYBEF = FALSE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   REPEAT:      
      IF NOT AVAILABLE tidtemp THEN LEAVE.                 
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = tidtemp.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST ANSTFORMTAB WHERE 
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.
         IF NOT AVAILABLE ANSTFORMTAB THEN FIND FIRST ANSTFORMTAB NO-LOCK NO-ERROR.
         FOR EACH tidtemp WHERE tidtemp.PERSONALKOD = PERSONALTAB.PERSONALKOD 
         USE-INDEX PERSONALKOD:
            ASSIGN 
            tidtemp.NYBEF = TRUE
            tidtemp.KOD = ANSTFORMTAB.KOD.
            IF tidtemp.BEFATTNING = "" THEN DO:
               ASSIGN
               tidtemp.PERSMASK = PERSONALTAB.PERSMASK
               tidtemp.BEFATTNING = PERSONALTAB.BEFATTNING.                                    
            END.
            ELSE DO:
               FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = tidtemp.BEFATTNING NO-LOCK NO-ERROR.
               IF AVAILABLE BEFATTNINGSTAB THEN tidtemp.PERSMASK = BEFATTNINGSTAB.PERSMASK.
            END.
            ASSIGN
            tidtemp.NAMN = SUBSTRING(PERSONALTAB.FORNAMN,1,1) + "." + PERSONALTAB.EFTERNAMN.   
            tidtemp.TRAAVTAL =  PERSONALTAB.TRAAVTAL.
            Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
            /*FAKTFORE*/
            IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "elpa" THEN DO:              
               IF tidtemp.PERSMASK = FALSE THEN DELETE tidtemp.
            END.
            ELSE IF  Guru.Konstanter:globforetag = "SNAT" THEN DO:
               /*externa konsulter ska inte över till lön , men till fakturering Ingrig 20190409*/
               IF tidtemp.BEFATTNING = "Extern konsult" THEN.                 
               ELSE IF tidtemp.PERSMASK = FALSE THEN DELETE tidtemp.
            END.
            
         END.         
      END.             
      ELSE DO:
         FIND FIRST ANSTFORMTAB NO-LOCK NO-ERROR.
         ASSIGN
         tidtemp.NYBEF = TRUE
         tidtemp.KOD = ANSTFORMTAB.KOD.
         FIND FIRST BEFATTNINGSTAB NO-LOCK NO-ERROR.
         IF tidtemp.BEFATTNING = "" THEN DO:
            ASSIGN
            tidtemp.PERSMASK = TRUE
            tidtemp.BEFATTNING = BEFATTNINGSTAB.BEFATTNING.                                    
         END.
         ASSIGN
         tidtemp.NAMN = "Saknas".         
      END.
      FIND FIRST tidtemp WHERE tidtemp.NYBEF = FALSE USE-INDEX PERSONALKOD NO-ERROR.
   END.   
   IF FAKTREGLER.OVERTIDRGL = "EGNA REGLER" THEN DO:                   
      FOR EACH tidtemp:
         IF tidtemp.PRISTYP = "RESTID..." THEN NEXT. 
         ELSE DO:
            FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = tidtemp.DATUM AND 
            OVERAVTAB.KOD = tidtemp.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
            IF AVAILABLE OVERAVTAB THEN DO:
               ASSIGN tidtemp.EQDAG = OVERAVTAB.EQDAG.
            END. 
            ELSE DO:
               ASSIGN tidtemp.EQDAG = WEEKDAY(tidtemp.DATUM).
            END.  
            RUN overber_UI.         
            IF tidtemp.DEBET = FALSE THEN DO:
               RUN kreditpost_UI (INPUT 2).               
            END.
         END.
      END.       
   END.      
   REPEAT: 
      FIND FIRST extrasum NO-ERROR.
      IF NOT AVAILABLE extrasum THEN LEAVE.
      
      CREATE tidtemp.
      BUFFER-COPY extrasum TO tidtemp.
      ASSIGN
      tidtemp.NYBEF = TRUE
      tidtemp.PRIS = extrasum.PRISA  
      tidtemp.START = extrasum.START
      tidtemp.SLUT = extrasum.SLUT      
      tidtemp.OKOST = extrasum.OBELOPP
      tidtemp.OTIMMAR = extrasum.OTIMMAR    
      tidtemp.OANT1 = extrasum.OANT1.
      DELETE extrasum.
      /*
      IF tidtemp.DEBET = FALSE THEN DO:
         RUN kreditpost_UI (INPUT 1).
      END.
      */
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = tidtemp.DATUM AND 
      OVERAVTAB.KOD = tidtemp.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF AVAILABLE OVERAVTAB THEN DO:
         ASSIGN tidtemp.EQDAG = OVERAVTAB.EQDAG.
      END. 
      ELSE DO:
         ASSIGN tidtemp.EQDAG = WEEKDAY(tidtemp.DATUM).
      END.  
      RUN overber_UI.
      IF tidtemp.DEBET = FALSE THEN DO:
         RUN kreditpost_UI (INPUT 2).         
      END.
   END.    
   {GDPRLOGGCLIENT.I}  
END PROCEDURE.

PROCEDURE kreditpost_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      ASSIGN
      tidtemp.LUNCH    =  tidtemp.LUNCH   * -1
      tidtemp.TOTALT   =  tidtemp.TOTALT  * -1
      tidtemp.RESTIM   =  tidtemp.RESTIM  * -1
      tidtemp.OANT1    =  tidtemp.OANT1   * -1
      tidtemp.OTIMMAR  =  tidtemp.OTIMMAR * -1.
   END.
   IF vad = 2 THEN DO:
      ASSIGN
      tidtemp.LUNCH    =  tidtemp.LUNCH   * -1
      tidtemp.TOTALT   =  tidtemp.TOTALT  * -1
      tidtemp.RESTIM   =  tidtemp.RESTIM  * -1
      tidtemp.OANT1    =  tidtemp.OANT1   * -1
      tidtemp.OTIMMAR  =  tidtemp.OTIMMAR * -1
      tidtemp.RESKOSTDEC = tidtemp.RESKOSTDEC * -1 
      tidtemp.KOST       = tidtemp.KOST       * -1 
      tidtemp.OKOST      = tidtemp.OKOST      * -1 .
   END.
END PROCEDURE.

PROCEDURE overber_UI :
   IF tidtemp.EQDAG = 1 THEN DO:  
      FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 8 AND
      FAKTOVER.START LE tidtemp.START AND
      FAKTOVER.SLUT > tidtemp.START 
      USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      RUN overber2_UI.      
   END.   
   ELSE IF tidtemp.EQDAG = 7 THEN DO:
      FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 7 AND
      FAKTOVER.START LE tidtemp.START AND
      FAKTOVER.SLUT > tidtemp.START 
      USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      RUN overber2_UI.            
   END.   
   ELSE DO:            
      FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 2 AND
      FAKTOVER.START LE tidtemp.START AND
      FAKTOVER.SLUT > tidtemp.START 
      USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      RUN overber2_UI.         
   END.                       
END PROCEDURE.

PROCEDURE overber2_UI :
   IF AVAILABLE FAKTOVER THEN DO:
      tidtemp.OTEXTID = FAKTOVER.OTEXTID. 
      IF FAKTOVER.SLUT >= tidtemp.SLUT THEN DO:         
         ASSIGN
         tidtemp.TOTALT = 0
         tidtemp.KOST = 0                               
         tidtemp.OTIMMAR = klockan100(tidtemp.SLUT) - klockan100(tidtemp.START).                  
         IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ESAN" THEN DO:
            IF tidtemp.OANT1 = 0 THEN DO: 
               ASSIGN               
               tidtemp.OPRIS = FAKTOVER.PRISA
               tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.                                   
            END.           
            ELSE DO:
               ASSIGN
               tidtemp.OPRIS = FAKTOVER.PRISA
               tidtemp.OTIMMAR = tidtemp.OANT1
               tidtemp.OKOST = tidtemp.OKOST + tidtemp.OANT1 * FAKTOVER.PRISA.                                          
            END. 
         END. 
         ELSE DO:     
            ASSIGN            
            tidtemp.OPRIS = FAKTOVER.PRISA
            tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.                                                             
         END.         
      END. 
      ELSE DO:         
         CREATE extrasum.
         BUFFER-COPY tidtemp TO extrasum.
         ASSIGN
         extrasum.PRISA = tidtemp.PRIS  
         extrasum.TIMMAR = 0
         extrasum.BELOPP = 0         
         extrasum.START = FAKTOVER.SLUT
         extrasum.GSTART = FAKTOVER.SLUT
         extrasum.GSLUT = tidtemp.SLUT         
         extrasum.OBELOPP = 0
         extrasum.OTIMMAR = 0    
         extrasum.OANT1 = tidtemp.OANT1 - tidtemp.OTIMMAR.
         IF extrasum.OANT1 < 0 THEN extrasum.OANT1 = 0.
         ASSIGN
         tidtemp.TOTALT = 0
         tidtemp.KOST = 0             
         tidtemp.SLUT = FAKTOVER.SLUT         
         tidtemp.OTIMMAR = klockan100(FAKTOVER.SLUT) - klockan100(tidtemp.START).                                                                                
         ASSIGN
         tidtemp.OPRIS = FAKTOVER.PRISA
         tidtemp.OKOST = tidtemp.OTIMMAR * FAKTOVER.PRISA.               
         IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ESAN" THEN DO:
            IF tidtemp.OANT1 = 0 THEN DO:               
               musz = musz.
            END.           
            ELSE DO:                 
               tidtemp.OANT1 = tidtemp.OANT1 - tidtemp.OTIMMAR.                                                                           
            END.                       
         END.          
      END.  
   END.   
   ELSE DO:                   
      IF tidtemp.EQDAG = 1 THEN DO:
         FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
         FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 8 AND
         FAKTOVER.START < tidtemp.SLUT AND
         FAKTOVER.SLUT >= tidtemp.SLUT 
         USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      END.  
      ELSE IF tidtemp.EQDAG = 7 THEN DO:
         FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
         FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 7 AND
         FAKTOVER.START < tidtemp.SLUT AND
         FAKTOVER.SLUT >= tidtemp.SLUT 
         USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      END.
      ELSE DO:                
         FIND FIRST FAKTOVER WHERE FAKTOVER.FAKTNR = FAKTPLAN.FAKTNR AND
         FAKTOVER.BEFATTNING = tidtemp.BEFATTNING AND FAKTOVER.EQDAG = 2 AND
         FAKTOVER.START < tidtemp.SLUT AND FAKTOVER.SLUT >= tidtemp.SLUT 
         USE-INDEX FAKTOVER NO-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE FAKTOVER THEN DO: 
         ASSIGN                      
         tidtemp.TOTALT = (klockan100(tidtemp.SLUT) - klockan100(tidtemp.START)) - tidtemp.LUNCH.
         tidtemp.KOST = tidtemp.TOTALT * tidtemp.PRIS.         
      END.
      ELSE DO:
         tidtemp.OTEXTID = FAKTOVER.OTEXTID.
         IF FAKTOVER.START <= tidtemp.START THEN DO:            
            ASSIGN
            tidtemp.TOTALT = 0
            tidtemp.KOST = 0 
            tidtemp.OTIMMAR = klockan100(tidtemp.SLUT) - klockan100(tidtemp.START).                                                         
            IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ESAN" THEN DO:
               IF tidtemp.OANT1 = 0 THEN DO:  
                  ASSIGN      
                  tidtemp.OPRIS = FAKTOVER.PRISA            
                  tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.  
               END.           
               ELSE DO:  
                  ASSIGN
                  tidtemp.OTIMMAR = tidtemp.OANT1
                  tidtemp.OPRIS = FAKTOVER.PRISA
                  tidtemp.OKOST = tidtemp.OKOST + tidtemp.OANT1 * FAKTOVER.PRISA.                                          
               END. 
            END. 
            ELSE DO:
               ASSIGN                  
               tidtemp.OPRIS = FAKTOVER.PRISA
               tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.   
            END.                                
         END. 
         ELSE DO:   
            CREATE extrasum.
            BUFFER-COPY tidtemp TO extrasum.
            ASSIGN
            extrasum.PRISA = tidtemp.PRIS  
            extrasum.TIMMAR = 0
            extrasum.BELOPP = 0         
            extrasum.START = tidtemp.START
            extrasum.SLUT = FAKTOVER.START
            extrasum.GSTART = tidtemp.START
            extrasum.GSLUT = FAKTOVER.START         
            extrasum.OBELOPP = 0
            extrasum.OTIMMAR = 0    
            extrasum.OANT1 = tidtemp.OANT1 - tidtemp.OTIMMAR.
            IF extrasum.OANT1 < 0 THEN extrasum.OANT1 = 0.  
            ASSIGN              
            tidtemp.START = FAKTOVER.START
            tidtemp.TOTALT = 0
            tidtemp.KOST = 0             
            tidtemp.OTIMMAR = klockan100(tidtemp.SLUT) - klockan100(FAKTOVER.START).             
            ASSIGN
            tidtemp.OPRIS = FAKTOVER.PRISA
            tidtemp.OKOST = tidtemp.OKOST + tidtemp.OTIMMAR * FAKTOVER.PRISA.   
            IF Guru.Konstanter:globforetag = "NORD" OR Guru.Konstanter:globforetag = "ESAN" THEN DO:
               IF tidtemp.OANT1 = 0 THEN DO:               
                  musz = musz.
               END.           
               ELSE DO:                                              
                  ASSIGN                               
                  tidtemp.OANT1 = tidtemp.OANT1 - tidtemp.OTIMMAR.                                                            
               END. 
            END.                                 
         END.  
      END. 
   END.   
END PROCEDURE.

PROCEDURE nytid_UI :
   IF kollvecko = "UTAN@" THEN musz = musz.
   ELSE IF kollvecko < tidertemp.VECKOKORD THEN kollvecko = tidertemp.VECKOKORD. 
   IF tidertemp.DEBET = TRUE THEN debkredvar = 1.
   ELSE DO:
      debkredvar = -1.
   END.
   IF tidertemp.TIDLOG = TRUE THEN DO:
      IF tidertemp.PRISTYP = "RESTID..." THEN DO:
         CREATE tidtemp.
         ASSIGN
         tidtemp.DEBET = tidertemp.DEBET
         tidtemp.NYBEF = FALSE
         tidtemp.PERSONALKOD = tidertemp.PERSONALKOD
         tidtemp.PRISTYP = tidertemp.PRISTYP
         tidtemp.AONR = tidertemp.AONR
         tidtemp.DELNR = tidertemp.DELNR
         tidtemp.DATUM = tidertemp.DATUM
         tidtemp.RESPRIS = tidertemp.PRIS
         tidtemp.START = tidertemp.START      
         tidtemp.SLUT = tidertemp.SLUT
         tidtemp.BEFATTNING = tidertemp.OVERTIDTILL .
         tidtemp.TOTALT = klockan100(tidertemp.TOTALT).
         ASSIGN  
         tidtemp.RESTIM = klockan100(tidtemp.SLUT) - klockan100(tidtemp.START).
         tidtemp.LUNCH = (klockan100(tidtemp.SLUT) - klockan100(tidtemp.START)) - tidtemp.TOTALT.
         ASSIGN
         tidtemp.RESKOSTDEC = tidtemp.RESTIM * tidtemp.RESPRIS.
         tidtemp.TOTALT = 0.     
   
       /*  tidtemp.START = 0
               tidtemp.SLUT = 0.*/
      END.
      ELSE DO:
         CREATE tidtemp.
         ASSIGN
         tidtemp.DEBET = tidertemp.DEBET
         tidtemp.NYBEF = FALSE
         tidtemp.PERSONALKOD = tidertemp.PERSONALKOD
         tidtemp.PRISTYP = tidertemp.PRISTYP
         tidtemp.AONR = tidertemp.AONR
         tidtemp.DELNR = tidertemp.DELNR
         tidtemp.DATUM = tidertemp.DATUM
         tidtemp.START = tidertemp.START
         tidtemp.SLUT = tidertemp.SLUT
         tidtemp.BEFATTNING = tidertemp.OVERTIDTILL .
         tidtemp.TOTALT = klockan100(tidertemp.TOTALT).
         ASSIGN
         tidtemp.PRIS = tidertemp.PRIS      
         tidtemp.OBER = FALSE.
         ASSIGN 
         tidtemp.KOST = tidtemp.TOTALT * tidtemp.PRIS
         tidtemp.LUNCH = (klockan100(tidtemp.SLUT) - klockan100(tidtemp.START)) - tidtemp.TOTALT.       
         IF FAKTREGLER.OVERTIDRGL = "TIDREDOVISNING"  THEN DO:
            IF tidertemp.OKOD1 NE " " THEN DO: 
               ASSIGN            	      
               ovkod = tidertemp.OKOD1
               nytid = tidertemp.OANT1.
               RUN TIMSEK.P.
               ovantal = (sekunder / 3600).
               RUN over_UI.
               tidtemp.OKOST = tidtemp.OKOST + ovbelopp.
            END.	  
            RUN noll_UI.
            IF tidertemp.OKOD2 NE " " THEN DO: 
               ASSIGN            	      
               ovkod = tidertemp.OKOD2
               nytid = tidertemp.OANT2.
               RUN TIMSEK.P.
               ovantal = (sekunder / 3600).
               RUN over_UI.		                 
               tidtemp.OKOST = tidtemp.OKOST + ovbelopp.
            END.	  
            RUN noll_UI.
            IF tidertemp.OKOD3 NE " " THEN DO: 
               ASSIGN            	      
               ovkod = tidertemp.OKOD3
               nytid = tidertemp.OANT3.
               RUN TIMSEK.P.
               ovantal = (sekunder / 3600).
               RUN over_UI.		                 
               tidtemp.OKOST = tidtemp.OKOST + ovbelopp.
            END.	  
            RUN noll_UI.                 
         END.
         tidtemp.OANT1 = ((DECIMAL(SUBSTRING(STRING(tidertemp.OANT1,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT1,"99.99"),4,2)) / 60) +
                         (DECIMAL(SUBSTRING(STRING(tidertemp.OANT2,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT2,"99.99"),4,2)) / 60) +
                         (DECIMAL(SUBSTRING(STRING(tidertemp.OANT3,"99.99"),1,2)) + 
                          DECIMAL(SUBSTRING(STRING(tidertemp.OANT3,"99.99"),4,2)) / 60)).
         IF FAKTREGLER.OVERTIDRGL = "INGA" THEN DO:
            tidtemp.OANT1 = 0.
         END.        
         IF tidtemp.OANT1 NE 0 THEN DO: 
            ASSIGN
            tidtemp.OTIMMAR = tidtemp.OANT1
            tidtemp.KOST = 0
            tidtemp.TOTALT = 0
            tidtemp.OPRIS = tidtemp.OKOST /  tidtemp.OANT1.
         END.
         IF FAKTREGLER.OVERTIDRGL = "EGNA REGLER" THEN  tidtemp.OKOST = 0.
      END.
   END.
   IF tidertemp.LONTILLANTAL NE 0 THEN DO:
      IF kollvecko = "UTAN@" AND FAKTREGLER.LONRGL = "ENDAST MILERSÄTTNING" THEN DO:
         ASSIGN
         tidtemp.LONTILLAGG   = tidertemp.LONTILLAGG
         tidtemp.LONTILLANTAL = tidertemp.LONTILLANTAL.                 
      END.
   END.
   IF tidertemp.TRAKTANTAL NE 0 THEN DO:
      IF FAKTREGLER.TRAKTRGL = "INGA" THEN musz = musz.
      ELSE IF kollvecko = "UTAN@" THEN DO:
         ASSIGN
         tidtemp.TRAKTKOD     = tidertemp.TRAKTKOD
         tidtemp.TRAKTANTAL   = tidertemp.TRAKTANTAL.
      END.
   END.
   IF tidtemp.DEBET = FALSE THEN DO:
      RUN kreditpost_UI (INPUT 1).         
   END.
   IF tidtemp.DEBET = FALSE THEN DO:
      RUN kreditpost_UI (INPUT 2).         
   END.
   ASSIGN
   tidtemp.VECKOKORD = tidertemp.VECKOKORD
   tidtemp.FELKORD = tidertemp.FELKORD.
   
END PROCEDURE.
PROCEDURE over_UI:
   typover = "OVE".
   multi = 0.
   FIND FIRST OVERKOD WHERE OVERKOD.KOD = tidertemp.KOD AND
   OVERKOD.OVERTIDTILL = ovkod 
   USE-INDEX OVER NO-LOCK NO-ERROR.
   IF AVAILABLE OVERKOD THEN DO:
      multi = OVERKOD.MULTIP.      
   END.       
   ovbelopp = (tidtemp.PRIS + (tidtemp.PRIS * multi)) * ovantal.    
   
END.
PROCEDURE noll_UI:
   ASSIGN    
   ovbelopp = 0                    
   ovkod = " ".            
END PROCEDURE.      
