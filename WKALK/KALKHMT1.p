/*KALKHMT1.P*/
&Scoped-define NEW NEW
{KALKALLTEMP.I}  
{KALKSTART.I}
{HAMTAVDJUDEF.I}
{GLOBVAR2DEL1.I}
/* DEFINE INPUT PARAMETER TABLE FOR uppkalktemp.           */
/* DEFINE INPUT-OUTPUT PARAMETER TABLE FOR utvaldfasttemp. */
DEFINE VARIABLE valomr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE TEMP-TABLE kalkaonrtemp NO-UNDO LIKE KALKAONR.
DEFINE VARIABLE eqh AS HANDLE NO-UNDO.
{DYNHMT.I}
FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
/*Ladda*/ 
{BOLAGSEKSTART.I}
PROCEDURE anvglob_UI :
   DEFINE INPUT PARAMETER anvglob AS CHARACTER NO-UNDO.
   /*Guru.Konstanter:globanv = anvglob.*/
END PROCEDURE.
/* RUN skapvalda_UI. */

PROCEDURE koppplanaonr_UI:
   DEFINE INPUT PARAMETER planrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER artalvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR valdfasttemp. 
   EMPTY TEMP-TABLE valdfasttemp NO-ERROR. 
   FOR EACH KALKAONR WHERE  KALKAONR.PLANNR = planrvar AND KALKAONR.ARTAL = artalvar NO-LOCK:
      IF KALKAONR.TYP = 6 THEN DO:
         /*FRI*/
      END.
      ELSE DO:
         CREATE valdfasttemp.             
         FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = KALKAONR.KALKNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE FASTSPEC THEN DO:
            BUFFER-COPY FASTSPEC TO valdfasttemp.
            valdfasttemp.TYPCHAR = STRING(valdfasttemp.TYP).
            IF valdfasttemp.TYP = 5 THEN valdfasttemp.TYPCHAR = "Sam".
            ELSE IF valdfasttemp.TYP = 7 THEN valdfasttemp.TYPCHAR = "Nät".
         END.
         BUFFER-COPY KALKAONR TO valdfasttemp.
         valdfasttemp.AKTIV = TRUE.
      END.      
   END.
END PROCEDURE.


PROCEDURE skapvaldan_UI :
   DEFINE INPUT PARAMETER TABLE FOR uppkalktemp.
   DEFINE INPUT PARAMETER TABLE FOR uppavdjud.
   DEFINE OUTPUT PARAMETER TABLE FOR utvaldfasttemp.
   FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.   
   EMPTY TEMP-TABLE kalkaonrtemp NO-ERROR. 
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   EMPTY TEMP-TABLE utvaldfasttemp NO-ERROR. 
   RUN skap_UI.
   RUN aonrsekkoll_UI (INPUT 1).
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   
END PROCEDURE.
PROCEDURE skapvalda_UI:
   DEFINE INPUT PARAMETER TABLE FOR uppkalktemp.
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.   
   EMPTY TEMP-TABLE kalkaonrtemp NO-ERROR. 
   EMPTY TEMP-TABLE utvaldfasttemp NO-ERROR. 
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   RUN skap_UI.
   RUN aonrsekkoll_UI (INPUT 2).
END PROCEDURE.

PROCEDURE skap_UI :
    ASSIGN 
   valomr = "ALLA"
   kommandoquery = " ".
   FIND FIRST uppkalktemp NO-ERROR.
   IF uppkalktemp.OMRADE NE "ALLA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = uppkalktemp.OMRADE NO-LOCK NO-ERROR. 
      valomr = OMRADETAB.OMRADE.
   END.
   IF uppkalktemp.AKIN = 1 THEN DO:
      kommandoquery = "KALKAONR.AKTIV = TRUE ".      
   END.
   ELSE IF uppkalktemp.AKIN = 2 THEN DO:
      kommandoquery = "KALKAONR.AKTIV = FALSE ".      
   END.
   ELSE IF uppkalktemp.AKIN = 3 THEN DO:
      kommandoquery = " ".      
   END.
   IF valomr NE "ALLA" THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " KALKAONR.OMRADE = " +  "'" + valomr + "'".
   END.
   IF uppkalktemp.TYP NE 0 THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " KALKAONR.TYP = " + STRING(uppkalktemp.TYP).
   END.
   IF uppkalktemp.AONR = TRUE THEN DO: 
      RUN and_UI.
      kommandoquery = kommandoquery + " KALKAONR.AONR = ?".
   END.
   ASSIGN
   utvaltab   = "FOR EACH uppkalktemp"
   nytab      = "kalkaonrtemp"
   orginaltab = "KALKAONR".
   ASSIGN
   kommandoquery = "FOR EACH " +  orginaltab + " WHERE " + kommandoquery + " NO-LOCK".   
   /*BUGG 9.1c FIX*/
   ASSIGN extratemptabh = TEMP-TABLE kalkaonrtemp:DEFAULT-BUFFER-HANDLE.
   ASSIGN extratemptabh2 = TEMP-TABLE uppkalktemp:DEFAULT-BUFFER-HANDLE.
   RUN dynquery_UI (INPUT TRUE,INPUT FALSE).
   RUN egenkoll_UI.
   
   FIND FIRST uppavdjud NO-ERROR. 
   IF AVAILABLE uppavdjud THEN DO:
      IF uppkalktemp.OMRADE = "ALLA" THEN DO:
         IF uppavdjud.AVDNR = "ALLA" THEN DO:
            IF uppavdjud.JUDID = "ALLA" THEN DO:  
               FOR EACH eutvaldfasttemp:
                  CREATE utvaldfasttemp.
                  BUFFER-COPY Eutvaldfasttemp TO utvaldfasttemp.
               END.
            END.
            ELSE DO:                        
               FOR EACH AVDELNING WHERE AVDELNING.POSTANST = uppavdjud.JUDID NO-LOCK,
               EACH OMRADETAB WHERE OMRADETAB.AVDELNINGNR = AVDELNING.AVDELNINGNR NO-LOCK,
               EACH eutvaldfasttemp WHERE eutvaldfasttemp.OMRADE = OMRADETAB.OMRADE:
                  CREATE utvaldfasttemp.
                  BUFFER-COPY Eutvaldfasttemp TO utvaldfasttemp.
               END.
            END.
         END.
         ELSE DO:
            FOR EACH AVDELNING WHERE AVDELNING.AVDELNINGNR = INTEGER(uppavdjud.AVDNR) NO-LOCK,
            EACH OMRADETAB WHERE OMRADETAB.AVDELNINGNR = AVDELNING.AVDELNINGNR NO-LOCK,
            EACH eutvaldfasttemp WHERE eutvaldfasttemp.OMRADE = OMRADETAB.OMRADE:
               CREATE utvaldfasttemp.
               BUFFER-COPY Eutvaldfasttemp TO utvaldfasttemp.
            END.
         END.
      END.
      ELSE DO:
         FOR EACH eutvaldfasttemp:
            CREATE utvaldfasttemp.
            BUFFER-COPY Eutvaldfasttemp TO utvaldfasttemp.
         END.
      END.
   END.   
   ELSE DO:
      FOR EACH eutvaldfasttemp:
         CREATE utvaldfasttemp.
         BUFFER-COPY Eutvaldfasttemp TO utvaldfasttemp.
      END.
   END.
   /*kalkår*/
   FOR EACH utvaldfasttemp:
     
         IF Guru.Konstanter:globforetag = "GRAN" THEN utvaldfasttemp.VIKATAR = utvaldfasttemp.KATAR - 2.
         ELSE utvaldfasttemp.VIKATAR = utvaldfasttemp.KATAR - 1.
         IF utvaldfasttemp.VIKATAR < 1900 THEN utvaldfasttemp.VIKATAR = 0.      
     
   END.
   RUN objdelete_UI.
   
END PROCEDURE.
PROCEDURE aonrsekkoll_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF Guru.Konstanter:varforetypchar[4] = "" AND Guru.Konstanter:varforetypval[18] = 0 THEN DO:
      RETURN.
   END.
   IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)   THEN RETURN.
   IF Guru.Konstanter:varforetypchar[4] = "ja" THEN DO:
      IF vad = 1 THEN DO:
         FOR EACH utvaldfasttemp:
            FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = Guru.Konstanter:globanv AND 
            OFFERT.OMRADE = utvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE OFFERT THEN DO:
               FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = Guru.Konstanter:globanv AND
               OFFERT.AONR = utvaldfasttemp.AONR AND OFFERT.DELNR = utvaldfasttemp.DELNR
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE OFFERT THEN DO:
                  FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = utvaldfasttemp.KALKNR AND FASTSPEC.OMRADE = utvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
                  IF AVAILABLE FASTSPEC THEN DO:
                     IF FASTSPEC.ANVANDARE = Guru.Konstanter:globanv AND utvaldfasttemp.AONR = ? THEN.
                     ELSE DELETE utvaldfasttemp.  
                  END.   
                  ELSE DELETE utvaldfasttemp.
               END.
            END.
         END.
      END.
      IF vad = 2 THEN DO:
         FOR EACH eutvaldfasttemp:
            FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = Guru.Konstanter:globanv AND 
            OFFERT.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE OFFERT THEN DO:
               FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = Guru.Konstanter:globanv AND 
               OFFERT.AONR = eutvaldfasttemp.AONR AND OFFERT.DELNR = eutvaldfasttemp.DELNR
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE OFFERT THEN DO:
                  FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = eutvaldfasttemp.KALKNR AND FASTSPEC.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
                  IF AVAILABLE FASTSPEC THEN DO:
                     IF FASTSPEC.ANVANDARE = Guru.Konstanter:globanv AND eutvaldfasttemp.AONR = ? THEN.
                     ELSE DELETE eutvaldfasttemp.  
                  END.   
                  ELSE DELETE eutvaldfasttemp.
               END.   
            END.
         END.
      END.
   END.
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:
      IF vad = 1 THEN DO:
         FOR EACH utvaldfasttemp:
            FIND FIRST omvtemp WHERE omvtemp.OMRADE = utvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omvtemp THEN DO:
               FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR. 
               IF NOT AVAILABLE BOLAGSEK THEN DELETE utvaldfasttemp.               
            END.
            ELSE DO:
               IF utvaldfasttemp.OMRADE NE "" THEN DELETE utvaldfasttemp.  
            END.
         END.
      END.
      IF vad = 2 THEN DO:
         FOR EACH eutvaldfasttemp:
            FIND FIRST omvtemp WHERE omvtemp.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omvtemp THEN DO:
               FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR. 
               IF NOT AVAILABLE BOLAGSEK THEN DELETE eutvaldfasttemp.               
            END.
            ELSE DO:
               IF eutvaldfasttemp.OMRADE NE "" THEN DELETE eutvaldfasttemp.  
            END.
         END.
      END.
      RETURN.
   END.
END PROCEDURE.

PROCEDURE egenkoll_UI:  
  FOR EACH kalkaonrtemp WHERE kalkaonrtemp.KALKNR = ?:
     DELETE kalkaonrtemp.
  END.
  FOR EACH kalkaonrtemp: 
     
      IF kalkaonrtemp.TYP = 6 THEN DO:
         FIND FIRST KALKSPEC WHERE KALKSPEC.KALKNR = kalkaonrtemp.KALKNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE KALKSPEC THEN DO:
            CREATE eutvaldfasttemp.
            BUFFER-COPY KALKSPEC TO eutvaldfasttemp .
            BUFFER-COPY kalkaonrtemp TO eutvaldfasttemp.
            ASSIGN
            eutvaldfasttemp.BENAMNING = KALKSPEC.KALKTEXT  
            eutvaldfasttemp.TYPCHAR = "Fri"
            eutvaldfasttemp.TYP = 6.
            /*KALKÅR*/
            IF Guru.Konstanter:globforetag = "GRAN" THEN eutvaldfasttemp.VIKATAR = eutvaldfasttemp.KATAR - 2.
            ELSE eutvaldfasttemp.VIKATAR = eutvaldfasttemp.KATAR - 1.              
           IF eutvaldfasttemp.VIKATAR < 1900  THEN eutvaldfasttemp.VIKATAR = 0.
         END.
      END.
      ELSE DO:
         FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = kalkaonrtemp.KALKNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE FASTSPEC THEN DO:
            CREATE eutvaldfasttemp.
            BUFFER-COPY FASTSPEC TO eutvaldfasttemp.          
            BUFFER-COPY kalkaonrtemp TO eutvaldfasttemp .
            ASSIGN
            eutvaldfasttemp.TYPCHAR = STRING(eutvaldfasttemp.TYP).
            IF eutvaldfasttemp.TYP = 5 THEN eutvaldfasttemp.TYPCHAR = "Sam".
            ELSE IF eutvaldfasttemp.TYP = 7 THEN eutvaldfasttemp.TYPCHAR = "Nät".
             
               IF Guru.Konstanter:globforetag = "GRAN" THEN eutvaldfasttemp.VIKATAR = eutvaldfasttemp.KATAR - 2.
               ELSE eutvaldfasttemp.VIKATAR = eutvaldfasttemp.KATAR - 1.         
               IF eutvaldfasttemp.VIKATAR < 1900  THEN eutvaldfasttemp.VIKATAR = 0.
             
         END.
      END.
   END.
   CREATE WIDGET-POOL "dynTemp" NO-ERROR.
   utvaltab   = "FOR EACH uppkalktemp".
   nytab      = "eutvaldfasttemp".
   kommandoquery = "FOR EACH " +  nytab.
   /*BUG FIX 9.1C*/
   ASSIGN extratemptabh = TEMP-TABLE eutvaldfasttemp:DEFAULT-BUFFER-HANDLE.
   ASSIGN extratemptabh2 = TEMP-TABLE uppkalktemp:DEFAULT-BUFFER-HANDLE.
   CREATE BUFFER utvaltabh FOR TABLE extratemptabh2 IN WIDGET-POOL "dynTemp". 
   CREATE QUERY uqh IN WIDGET-POOL "dynTemp".
   uqh:SET-BUFFERS(utvaltabh).                                
   uqh:QUERY-PREPARE(utvaltab).   
   uqh:QUERY-OPEN().
   uqh:GET-FIRST(NO-LOCK).
   CREATE BUFFER nytabh FOR TABLE extratemptabh IN WIDGET-POOL "dynTemp".
   CREATE QUERY eqh IN WIDGET-POOL "dynTemp".
   eqh:SET-BUFFERS(nytabh).
   eqh:QUERY-PREPARE(kommandoquery).   
   eqh:QUERY-OPEN().
   eqh:GET-FIRST(NO-LOCK).   
   DO WHILE nytabh:AVAILABLE:     
      musz = FALSE.
      IF musz = FALSE THEN DO: 
         IF uppkalktemp.BESTID NE "ALLA" THEN DO:
            ASSIGN
            kommandonyfalt = "BESTID"
            kommandoorgfalt = "BESTID".
            RUN dynakoll_UI (OUTPUT musz).
         END.
      END.   
      IF musz = FALSE THEN DO: 
         IF uppkalktemp.KALKANSVARIG NE "ALLA" THEN DO:
            ASSIGN
            kommandonyfalt = "KALKANV"
            kommandoorgfalt = "KALKANSVARIG".
            RUN dynakoll_UI (OUTPUT musz).
         END.
      END.
     
      IF musz = FALSE THEN DO: 
         IF uppkalktemp.UTFARD NE "ALLA" THEN DO:
            ASSIGN
            kommandonyfalt = "ANVANDARE"
            kommandoorgfalt = "UTFARD".
            RUN dynakoll_UI (OUTPUT musz).
         END.
      END.
      eqh:GET-NEXT(NO-LOCK).        
   END.  
   eqh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.
   RUN objdelete_UI.
END PROCEDURE. 

PROCEDURE dynstartkoll_UI:  
   musz = FALSE.
END PROCEDURE. 
   
PROCEDURE laddatypben_UI:
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valdfasttemp.  
   FOR EACH valdfasttemp:
      FIND FIRST KALKSPEC WHERE KALKSPEC.KALKNR = valdfasttemp.KALKNR
      NO-LOCK NO-ERROR.
      IF AVAILABLE KALKSPEC THEN DO:
         BUFFER-COPY KALKSPEC TO valdfasttemp.
         ASSIGN
         valdfasttemp.BENAMNING = KALKSPEC.KALKTEXT
         valdfasttemp.TYPCHAR = "Fri"
         valdfasttemp.TYP = 6.
      END.
      ELSE DO:
         FIND FIRST FASTSPEC WHERE FASTSPEC.KALKNR = valdfasttemp.KALKNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE FASTSPEC THEN DO:
            BUFFER-COPY FASTSPEC TO valdfasttemp.
            
            valdfasttemp.TYPCHAR = STRING(valdfasttemp.TYP).
            IF valdfasttemp.TYP = 5 THEN valdfasttemp.TYPCHAR = "Sam".
            ELSE IF valdfasttemp.TYP = 7 THEN valdfasttemp.TYPCHAR = "Nät".
         END.
         ELSE DELETE valdfasttemp.
      END.
      IF AVAILABLE valdfasttemp THEN DO:
         FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = valdfasttemp.KALKNR NO-LOCK NO-ERROR.
         IF AVAILABLE KALKAONR THEN BUFFER-COPY KALKAONR TO valdfasttemp.
      END.
   END.
END PROCEDURE.
         
