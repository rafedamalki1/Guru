
/*BYTTOLKW.P*/
DEFINE TEMP-TABLE byttemp NO-UNDO
   FIELD VADGORA AS INTEGER
   FIELD BEFATTNING AS CHARACTER
   FIELD PRIS AS DECIMAL
   FIELD PRISTYP AS CHARACTER 
   FIELD UTRYCKNING AS LOGICAL
   FIELD AONR AS CHARACTER 
   FIELD DELNR AS INTEGER
   FIELD TRAKTAMENTE AS INTEGER 
   FIELD ANVANDARE AS CHARACTER
   FIELD RECTIDVIS AS RECID.
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

{REGVAR.I}
{SOKDEF.I}
DEFINE INPUT PARAMETER TABLE FOR byttemp.
DEFINE OUTPUT PARAMETER placerarec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE varfabef AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE varpris LIKE TIDREGITAB.PRIS NO-UNDO.
DEFINE NEW SHARED VARIABLE varpristyp LIKE TIDREGITAB.PRISTYP NO-UNDO.
DEFINE NEW SHARED VARIABLE varutryck LIKE TIDREGITAB.UTRYCK NO-UNDO.
DEFINE NEW SHARED VARIABLE varaonr LIKE TIDREGITAB.AONR NO-UNDO.
DEFINE NEW SHARED VARIABLE vardelnr LIKE TIDREGITAB.DELNR NO-UNDO. 
DEFINE NEW SHARED VARIABLE vartrakt LIKE TIDREGITAB.TRAKTAMENTE NO-UNDO.  
DEFINE NEW SHARED VARIABLE tidtabrec AS RECID NO-UNDO.
DEFINE NEW SHARED VARIABLE bustart3 LIKE TIDREGITAB.START NO-UNDO.
DEFINE NEW SHARED VARIABLE debitering AS INTEGER NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE tidtabrecspar AS RECID NO-UNDO.
DEFINE VARIABLE aosok AS CHARACTER FORMAT "X(8)" NO-UNDO.   
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE my1hand AS WIDGET-HANDL NO-UNDO.
DEFINE VARIABLE antal_valda AS INTEGER NO-UNDO.
DEFINE VARIABLE antal_raknare AS INTEGER NO-UNDO.
DEFINE VARIABLE overant AS INTEGER NO-UNDO.
DEFINE VARIABLE otim AS INTEGER NO-UNDO.
DEFINE VARIABLE otim2 AS INTEGER NO-UNDO.
DEFINE VARIABLE regdagspar AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE VARIABLE regdatumspar AS DATE NO-UNDO.
DEFINE VARIABLE ortssok AS CHARACTER NO-UNDO.
DEFINE VARIABLE kontrollstart AS DECIMAL NO-UNDO.
DEFINE VARIABLE hjslut LIKE TIDREGITAB.SLUT NO-UNDO.                   
DEFINE VARIABLE hjstart LIKE TIDREGITAB.START NO-UNDO.  
DEFINE VARIABLE ehjstart LIKE TIDREGITAB.START NO-UNDO.  
{TIDAPPDEF.I}
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
{FORESTYR.I}
FIND FIRST byttemp NO-ERROR.
ASSIGN 
debitering = byttemp.VADGORA 
varfabef   = byttemp.BEFATTNING  
varpris    = byttemp.PRIS 
varpristyp = byttemp.PRISTYP 
varutryck  = byttemp.UTRYCKNING 
varaonr    = byttemp.AONR 
vardelnr   = byttemp.DELNR 
vartrakt   = byttemp.TRAKTAMENTE 

tidtabrec  = byttemp.RECTIDVIS.   
placerarec = tidtabrec.
IF debitering = 1 THEN DO:       
   RUN byttolk_UI.
END.
ELSE IF debitering = 2 THEN DO:
   DO TRANSACTION:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      IF TIDREGITAB.PRISTYP = "RESTID..." THEN musz = musz.
      ELSE DO:
         ASSIGN          
         /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
         SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "ALLAAND" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
         TIDREGITAB.PRISTYP = varpristyp
         TIDREGITAB.PRIS = varpris.
      END.
   END.   
END.    
ELSE IF debitering = 3 THEN DO:
   DO TRANSACTION:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN 
      /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "ALLAAND" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv.      
      TIDREGITAB.OVERTIDTILL = varfabef.
      IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
         IF TIDREGITAB.PRISTYP = "FRÅNVARO." OR TIDREGITAB.PRISTYP = "RESTID..." THEN DO:
         END.
         ELSE IF Guru.Konstanter:globforetag = "elpa" AND TIDREGITAB.PRISTYP = "EJ.KOSTN."  THEN DO:
         END.
         ELSE IF (Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) AND TIDREGITAB.PRISTYP = "EJ.KOSTN."  THEN DO:
         END.
         ELSE IF Guru.Konstanter:globforetag = "elpa" AND TIDREGITAB.PRISTYP = "FASTPRIS1"  THEN DO:
         END.
         ELSE IF (Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) AND TIDREGITAB.PRISTYP = "FASTPRIS1"  THEN DO:
         END.

         ELSE DO:
            FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
            PERSONALPRIS.BEFATTNING = TIDREGITAB.OVERTIDTILL AND 
            PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
            AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
            NO-ERROR.
            IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.
            ELSE DO:
               {SOKSTART.I}
               ASSIGN
               soktemp.SOKVAL = 1
               soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
               soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
               soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
               soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
               soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
               {SOKANROP.I}
               TIDREGITAB.PRIS = soktemp.SOKDECI[1].
            END.
         END.
      END.
   END.   
END.


PROCEDURE byttolk_UI:
   FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrec NO-LOCK NO-ERROR.      
   FIND PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TIDREGITAB.PERSONALKOD NO-LOCK NO-ERROR.
   persrec = RECID(PERSONALTAB).
   FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
   USE-INDEX ANSTF NO-LOCK NO-ERROR.
   DO TRANSACTION:
      FIND CURRENT TIDREGITAB EXCLUSIVE-LOCK NO-ERROR.
      IF TIDREGITAB.PRISTYP = "RESTID..." THEN DO:
         musz = musz.      
      END.
      ELSE DO:         
         IF Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:            
            /*Om man byter projekt på en endagstjänsteresa ska inte traktzon försvinna lena 20100809*/
            IF TIDREGITAB.TRAKTAMENTE = 1 THEN.
            ELSE ASSIGN TIDREGITAB.TRAKTAMENTE = vartrakt.
         END.
         ELSE ASSIGN TIDREGITAB.TRAKTAMENTE = vartrakt.
         ASSIGN
         TIDREGITAB.PRISTYP = varpristyp
         TIDREGITAB.PRIS = varpris.
         RUN pris_UI.
      END.
      ASSIGN
      bustart3 = TIDREGITAB.START.
      ASSIGN 
      /*godöver persotb.GKANDVEMNAR = SUBSTRING(TIDREGITAB.PROGRAM,159) Lena 20200603 */
      SUBSTRING(TIDREGITAB.PROGRAM,1,158) = "BYTTOLK" + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv              
      TIDREGITAB.UTRYCKNING = FALSE  /*varutryck*/ 
      TIDREGITAB.NODF = FALSE.
      IF TIDREGITAB.AONR = varaonr AND TIDREGITAB.DELNR = vardelnr THEN DO:
         varaonr = varaonr.
      END.
      ELSE DO:          
         ASSIGN 
         sekunder = 0   
         regdatum = TIDREGITAB.DATUM.         
      END.
      ASSIGN 
      TIDREGITAB.AONR = varaonr 
      TIDREGITAB.DELNR = vardelnr.
      ASSIGN      
      nytid = TIDREGITAB.START.
      RUN TIMSEK.P. 
      regstartsek = sekunder.
      nytid = TIDREGITAB.SLUT.
      RUN TIMSEK.P.
      regslutsek = sekunder.
      regdatum = TIDREGITAB.DATUM.
      RUN TOTTID.P.
      ASSIGN TIDREGITAB.TOTALT = nytid.           
   END.
   RELEASE TIDREGITAB NO-ERROR.
   RUN nytolk_UI.
END PROCEDURE.
PROCEDURE nytolk_UI :
   tidtabrecspar = tidtabrec.   
   EMPTY TEMP-TABLE tidapptemp NO-ERROR.    
   CREATE tidapptemp.
   ASSIGN
   tidapptemp.FORETAG = Guru.Konstanter:globforetag
   tidapptemp.ANVANDARE = Guru.Konstanter:globanv
   tidapptemp.RECPERS = RECID(PERSONALTAB)
   tidapptemp.RECTID = tidtabrec
   tidapptemp.DATUM = regdatum.      
   {TIDUPPIN.I}
   musz = FALSE.   
   RUN REGVEC.P.
   RUN SLUTARB.P.
   FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING
   USE-INDEX ORDARB NO-LOCK NO-ERROR.    
   IF NOT AVAILABLE ORDARB THEN DO:
      MESSAGE "Kontakta Elpool! För nu är det något fel! Ange läge 4." VIEW-AS ALERT-BOX. 
   END. 
   ELSE DO:
      sekunder = ORDARB.START1.
      RUN SEKTIM.P.
      ASSIGN
      hjstart = nytid
      ehjstart = hjstart.
      IF ORDARB.OBKOD NE "" THEN DO:
         FIND FIRST FLEXREG WHERE FLEXREG.KOD = ANSTFORMTAB.KOD NO-LOCK NO-ERROR.
         IF NOT AVAILABLE FLEXREG THEN DO:
            FIND FIRST FLEXREG WHERE FLEXREG.KOD = "" NO-LOCK NO-ERROR.
         END.         
         IF AVAILABLE FLEXREG THEN DO:
            IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:
               ASSIGN hjslut =  DECIMAL(ORDARB.OBKOD).   
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "TU" THEN hjstart = ehjstart.
               
               IF Guru.Konstanter:globforetag = "SNAT" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.               
                              
               IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.               
               /*IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD = "KV" THEN hjstart = 7.30.*/               
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
               ASSIGN hjslut = DECIMAL(ORDARB.OBKOD).
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "TU" THEN hjstart = ehjstart.
               
               IF Guru.Konstanter:globforetag = "SNAT" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.               
                              
               IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.               
               /*IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD = "KV" THEN hjstart = 7.30.*/               
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
               ASSIGN hjslut = DECIMAL(ORDARB.OBKOD).
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.
               IF Guru.Konstanter:globforetag = "SUND" AND FLEXREG.KOD = "TU" THEN hjstart = ehjstart.
               
               IF Guru.Konstanter:globforetag = "SNAT" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.               
                              
               IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD BEGINS "T" THEN hjstart = 7.30.               
               /*IF Guru.Konstanter:globforetag = "MISV" AND FLEXREG.KOD = "KV" THEN hjstart = 7.30.*/               
            END.
            ELSE DO:
               sekunder = ORDARB.STOPP1.
               RUN SEKTIM.P.
               hjslut = nytid.    
            END.
         END. 
         ELSE DO:
            sekunder = ORDARB.STOPP1.
            RUN SEKTIM.P.
            hjslut = nytid.    
         END.  
      END. 
      ELSE DO:
         sekunder = ORDARB.STOPP1.
         RUN SEKTIM.P.
         hjslut = nytid.    
      END.  
   END.   
   
   IF bustart3 GE regslut OR  bustart3 < regstart THEN DO TRANSACTION:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrecspar EXCLUSIVE-LOCK NO-ERROR.
      nytid = bustart3.
      RUN TIMSEK.P.  
      FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = regdatum AND 
      OVERAVTAB.KOD = ANSTFORMTAB.KOD USE-INDEX ODATUM NO-LOCK NO-ERROR.
      IF AVAILABLE OVERAVTAB AND (OVERAVTAB.DAGEQ = "HAL" OR OVERAVTAB.DAGEQ = "VAL" OR OVERAVTAB.DAGEQ = "HAV" OR
      OVERAVTAB.EQDAG = 1 OR OVERAVTAB.EQDAG = 7)  THEN musz = musz.
      ELSE IF bustart3 GE hjslut OR bustart3 < hjstart OR
      WEEKDAY(regdatum) = 1 OR WEEKDAY(regdatum) = 7 THEN musz = musz.
      ELSE DO: 
         IF TIDREGITAB.OANT1 NE 0 OR TIDREGITAB.OANT2 NE 0 OR
         TIDREGITAB.OANT3 NE 0  THEN DO:
            ASSIGN TIDREGITAB.OANT1 = 0 TIDREGITAB.OKOD1 = ""
            TIDREGITAB.OANT2 = 0 TIDREGITAB.OKOD2 = ""
            TIDREGITAB.OANT3 = 0 TIDREGITAB.OKOD3 = "".
            
         END.
      END.
   END.
   ELSE DO TRANSACTION:
      FIND TIDREGITAB WHERE RECID(TIDREGITAB) = tidtabrecspar EXCLUSIVE-LOCK NO-ERROR.
      IF TIDREGITAB.OANT1 NE 0 OR TIDREGITAB.OANT2 NE 0 OR
      TIDREGITAB.OANT3 NE 0 THEN DO:
         ASSIGN TIDREGITAB.OANT1 = 0 TIDREGITAB.OKOD1 = ""
         TIDREGITAB.OANT2 = 0 TIDREGITAB.OKOD2 = ""
         TIDREGITAB.OANT3 = 0 TIDREGITAB.OKOD3 = "".
         
      END. 
   END.             
END PROCEDURE.      
PROCEDURE pris_UI :
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:         
      IF TIDREGITAB.PRISTYP = "FRÅNVARO." OR TIDREGITAB.PRISTYP = "RESTID..." THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF (Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) AND TIDREGITAB.PRISTYP = "EJ.KOSTN." THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" AND TIDREGITAB.PRISTYP = "EJ.KOSTN." THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF (Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) AND TIDREGITAB.PRISTYP = "FASTPRIS1" THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" AND TIDREGITAB.PRISTYP = "FASTPRIS1" THEN DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.PRISTYP AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE DO:
         FIND LAST PERSONALPRIS WHERE PERSONALPRIS.PERSONALKOD = TIDREGITAB.PERSONALKOD AND
         PERSONALPRIS.BEFATTNING = TIDREGITAB.OVERTIDTILL AND 
         PERSONALPRIS.STARTDATUM <= TIDREGITAB.DATUM 
         AND PERSONALPRIS.SLUTDATUM >= TIDREGITAB.DATUM 
         NO-LOCK NO-ERROR.
         IF AVAILABLE PERSONALPRIS THEN TIDREGITAB.PRIS = PERSONALPRIS.PRIS.  
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = TIDREGITAB.PERSONALKOD
            soktemp.SOKCHAR[3] = TIDREGITAB.PRISTYP
            soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
            soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
            {SOKANROP.I}
            TIDREGITAB.PRIS = soktemp.SOKDECI[1].
         END.
      END.
   END.
END PROCEDURE.
