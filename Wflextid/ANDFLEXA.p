/*ANDFLEXA.P*/
&Scoped-define NEW     
&Scoped-define SHARED 
{FLEXTAB.I}
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
{REGVAR.I}
{SOKDEF.I}
DEFINE INPUT PARAMETER vart AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER inregdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER CMB_KNAPP AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER varaonr AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER vardelnr AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER FILL-IN_TID AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER FILL-IN_ORSAK AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER flexkvst AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER flexkvsl AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER inglobanv AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sregstart AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER sregslut AS DECIMAL NO-UNDO.
DEFINE INPUT PARAMETER sregdagnamn AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER sregvnr AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR extratemp.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE flrec AS RECID NO-UNDO. 
DEFINE VARIABLE fldrec AS RECID NO-UNDO. 
DEFINE VARIABLE flexstart AS DECIMAL NO-UNDO. 
DEFINE VARIABLE status-ok AS LOGICAL NO-UNDO.
DEFINE VARIABLE sparomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE flexmost AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flexmosl AS DECIMAL NO-UNDO.     
DEFINE VARIABLE seku AS INTEGER NO-UNDO.
DEFINE VARIABLE fstart AS DECIMAL NO-UNDO.     
DEFINE VARIABLE fslut AS DECIMAL NO-UNDO.     
DEFINE VARIABLE flkoll AS DECIMAL NO-UNDO.     
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE sok1 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok2 AS INTEGER NO-UNDO.
DEFINE VARIABLE sok3 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok4 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sok5 AS DECIMAL NO-UNDO.
DEFINE VARIABLE kolllu AS INTEGER NO-UNDO.
DEFINE VARIABLE kollfv1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE kollfv2 AS DECIMAL NO-UNDO.
DEFINE BUFFER flexbuff FOR FLEXTID.
DEFINE BUFFER tidbuff FOR TIDREGITAB.
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.
FUNCTION klock100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

FUNCTION klock60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/

  RETURN  TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) * 60 / 100 ).

END FUNCTION.
ASSIGN
globanv = inglobanv
regdatum = inregdatum
regstart = sregstart
regslut = sregslut
regdagnamn = sregdagnamn
regvnr = sregvnr. 
FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
{FORESTYR.I}
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
persrec = RECID(PERSONALTAB).
FIND FIRST extratemp NO-ERROR.
FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.  
FIND FIRST UTRYCKNING WHERE  UTRYCKNING.KOD = ANSTFORMTAB.KOD
USE-INDEX UT NO-LOCK NO-ERROR.  
FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD USE-INDEX FLEX NO-LOCK 
NO-ERROR.  
FIND FIRST AONRTAB WHERE AONRTAB.AONR = varaonr AND AONRTAB.DELNR = vardelnr
NO-LOCK NO-ERROR.
RUN REGVEC.P.
RUN SLUTARB.P.         
FIND FIRST GODKOLL WHERE GODKOLL.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
GODKOLL.DATAR = YEAR(regdatum) AND GODKOLL.DATMAN = MONTH(regdatum) USE-INDEX PKODAR NO-LOCK NO-ERROR.                    
IF CMB_KNAPP = "Annat in" THEN DO TRANSACTION:             
   FIND LAST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD
   AND FLEXTID.DATUM = regdatum  USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE FLEXTID AND FLEXTID.KNAPP = "Annat ut" THEN DO :
      ASSIGN FLEXTID.GICK = FALSE.
   END.
END.
IF vart = "nya" THEN DO:   
   FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD AND 
   FLEXTID.DATUM = regdatum AND FLEXTID.KNAPP = CMB_KNAPP USE-INDEX FLEX
   NO-LOCK NO-ERROR.
   IF AVAILABLE FLEXTID THEN DO:
      IF CMB_KNAPP = "FLEX IN" OR CMB_KNAPP = "FLEX UT" THEN flrec = flrec.
      ELSE DO:
         CREATE felmeddtemp.
         felmeddtemp.FELMEDD = "Det finns redan en " + CMB_KNAPP + " registrering.". 
         RETURN.
      END.         
      IF CMB_KNAPP = "IN" THEN DO:     
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND flexbuff.DATUM = regdatum AND flexbuff.KOM = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:
            CREATE felmeddtemp.
            felmeddtemp.FELMEDD = "Det finns redan en in-registrering".
            RETURN.
         END.   
      END.   
      IF CMB_KNAPP = "UT" THEN DO:     
         FIND LAST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND flexbuff.DATUM = regdatum USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE flexbuff AND flexbuff.GICK = TRUE THEN DO:
            CREATE felmeddtemp.
            felmeddtemp.FELMEDD = "Det finns redan en ut-registrering.".
            RETURN.
         END.   
      END.   
   END.
   IF CMB_KNAPP = "FLEX IN" THEN DO:
      FIND LAST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
      AND flexbuff.DATUM = regdatum AND flexbuff.TID < FILL-IN_TID USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF AVAILABLE flexbuff THEN DO:
         IF flexbuff.KNAPP = "Flex ut" OR flexbuff.KNAPP = "Lunch ut" THEN flrec = flrec.
         ELSE DO:    
            IF flexbuff.KNAPP = "Annat ut" THEN DO:
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = "Föregående registrering är Annat ut , använd Annat in istället".
            END.
            ELSE DO:            
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = "Det finns redan en in-registrering".
            END.
            RETURN.
         END.
      END.   
   END.
   IF CMB_KNAPP = "FLEX IN" THEN DO:
      FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
      AND flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "In" AND flexbuff.KOM = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF AVAILABLE flexbuff THEN DO:
         IF flexbuff.TID GE FILL-IN_TID THEN DO:         
            CREATE felmeddtemp. 
            felmeddtemp.felmedd = "Det finns redan en In-registrering, det går inte att registrera flex in före denna tid ".
            RETURN.       
         END.
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "Flex ut" AND flexbuff.TID < FILL-IN_TID  USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF NOT AVAILABLE flexbuff THEN DO:
            CREATE felmeddtemp. 
            felmeddtemp.felmedd = "Det finns redan en In-registrering, då måste det finnas en flex ut innan en flex in-registrering".
            RETURN.
         END.
      END.   
   END.
   IF CMB_KNAPP = "ANNAT IN" THEN DO:
      FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
      AND flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "In" AND flexbuff.KOM = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
      IF AVAILABLE flexbuff THEN DO:
         IF flexbuff.TID GE FILL-IN_TID THEN DO:         
            CREATE felmeddtemp. 
            felmeddtemp.felmedd = "Det finns redan en In-registrering, det går inte att registrera annat in före denna tid ".
            RETURN.
         END.
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND flexbuff.DATUM = regdatum AND flexbuff.KNAPP = "Annat ut" AND flexbuff.TID < FILL-IN_TID  USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF NOT AVAILABLE flexbuff THEN DO:
            CREATE felmeddtemp. 
            felmeddtemp.felmedd = "Det finns redan en In-registrering, då måste det finnas en annat ut innan en annat in-registrering".
            RETURN.
         END.
      END.   
   END.
   
   DO TRANSACTION:
      CREATE FLEXTID.
      ASSIGN
      flrec = RECID(FLEXTID)
      FLEXTID.AUTO = "Korr"
      FLEXTID.DATUM = regdatum
      FLEXTID.PERSONALKOD = PERSONALTAB.PERSONALKOD.
   END.
END.
RUN btnreg_UI.
EMPTY TEMP-TABLE extratemp NO-ERROR. 
FIND FLEXTID WHERE RECID(FLEXTID) = flrec NO-LOCK NO-ERROR.
IF AVAILABLE FLEXTID THEN DO:
   CREATE extratemp.
   BUFFER-COPY FLEXTID TO extratemp.      
END.
IF AVAILABLE TIDREGITAB THEN RELEASE TIDREGITAB NO-ERROR.
IF AVAILABLE FLEXTID THEN RELEASE FLEXTID NO-ERROR.
IF AVAILABLE FLEXDAG THEN RELEASE FLEXDAG NO-ERROR.

PROCEDURE btnreg_UI :
   DO TRANSACTION:
      IF vart = "AND" THEN DO:
         FIND FIRST FLEXTID WHERE FLEXTID.PERSONALKOD = extratemp.PERSONALKOD AND 
         FLEXTID.DATUM = extratemp.DATUM AND FLEXTID.TID = extratemp.TID AND 
         FLEXTID.KNAPP = extratemp.KNAPP NO-LOCK NO-ERROR. 
         flrec = RECID(FLEXTID).
      END.
      FIND FLEXTID WHERE RECID(FLEXTID) = flrec EXCLUSIVE-LOCK NO-ERROR.       
      flrec = RECID(FLEXTID).
      IF vart = "and" THEN DO:
         flexstart = FLEXTID.TID. 
      END.   
      ASSIGN         
      FLEXTID.KNAPP = CMB_KNAPP  
      FLEXTID.TID = FILL-IN_TID
      FLEXTID.AONR = varaonr
      FLEXTID.DELNR = vardelnr
      FLEXTID.ORSAK = FILL-IN_ORSAK
      FLEXTID.AUTO = "Korr".   
      IF vart = "NYA" THEN DO:
         flexstart = FLEXTID.TID. 
      END.   
      IF FLEXTID.KNAPP = "In" THEN ASSIGN FLEXTID.KOM = TRUE FLEXTID.GICK = FALSE.
      IF FLEXTID.KNAPP = "Ut" THEN ASSIGN FLEXTID.KOM = FALSE FLEXTID.GICK = TRUE. 
      FIND FIRST FLEXDAG WHERE FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
      FLEXDAG.DATUM = FLEXTID.DATUM USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE FLEXDAG THEN DO:
         CREATE FLEXDAG.
         ASSIGN FLEXDAG.PERSONALKOD = PERSONALTAB.PERSONALKOD
         FLEXDAG.DATUM = FLEXTID.DATUM
         FLEXDAG.KONTROLL = "Ejkontroll"
         FLEXDAG.START = regstart
         FLEXDAG.SLUT = regslut
         FLEXDAG.FELOK = FALSE.
      END.     
      ASSIGN
      fldrec = RECID(FLEXDAG)
      regdatum = FLEXDAG.DATUM.
      IF FLEXDAG.KONTROLL = "Kontroll" THEN DO:
         ASSIGN FLEXDAG.KONTROLL = "Ejkontroll" FLEXDAG.FELOK = FALSE.
      END.   
      IF FLEXTID.KNAPP = "In" THEN DO:      
         IF AVAILABLE FLEXREG THEN DO:
            ASSIGN
            flexmost = FLEXREG.MOSTART
            flexmosl = FLEXREG.MOSLUT.
            
             /* sommartid*/
            /*k- morgonflex till 9 både sommar och vinter , övriga sommar 8.30 vinter 9 -ändras manuellt i FLEXREG*/         
            IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.               
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                                                 
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                                         
            END.
            ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE IF flexmosl = 9 THEN ASSIGN flexmosl = 8.30.                                         
            END.
            ELSE DO:
               IF FLEXAVT.FLEXKOD = "K" THEN regdatum = regdatum.
               ELSE  IF flexmosl = 8.30 THEN ASSIGN flexmosl = 9.00.                                       
            END.
            
            IF PERSONALTAB.DELTID = TRUE THEN DO:
               FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING NO-LOCK NO-ERROR.    
               IF AVAILABLE ORDARB THEN DO:                                   
                  sekunder = ORDARB.START1.
                  RUN SEKTIM.P.               
                  IF nytid < regstart THEN DO:                   
                     nytid = flexmosl.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regstart.
                     RUN TIMSEK.P.
                     sekunder = seku - ORDARB.START1 + sekunder.
                     RUN SEKTIM.P.
                     flexmosl = nytid.
                     nytid = flexmost.
                     RUN TIMSEK.P.
                     seku = sekunder.
                     nytid = regstart.
                     RUN TIMSEK.P.
                     sekunder = seku - ORDARB.START1 + sekunder.
                     RUN SEKTIM.P.
                     flexmost = nytid.
                  END.               
               END.
            END.
            IF flexmost > FLEXTID.TID THEN ASSIGN FLEXDAG.START = flexmost.            
            ELSE ASSIGN FLEXDAG.START = FLEXTID.TID.
         END.   
         IF vart = "AND" THEN DO:
            FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND TIDREGITAB.DATUM = regdatum AND TIDLOG = TRUE 
            AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.START = flexstart
            AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE TIDREGITAB THEN DO:         
               ASSIGN
               TIDREGITAB.START = FLEXDAG.START.
               RUN andtid_UI.               
               IF AVAILABLE AONRTAB THEN DO:                  
                  IF AONRTAB.AONR NE "" THEN DO:                                          
                     {SOKSTART.I}
                     ASSIGN
                     soktemp.SOKVAL = 1
                     soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
                     soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
                     soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
                     soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                     soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                     {SOKANROP.I}
                     ASSIGN
                     TIDREGITAB.AONR = AONRTAB.AONR
                     TIDREGITAB.DELNR = AONRTAB.DELNR
                     TIDREGITAB.PRISTYP = AONRTAB.PRISTYP   
                     TIDREGITAB.PRIS = soktemp.SOKDECI[1].                         
                  END.
               END.
            END.   
         END.   
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO: 
                   fstart = FLEXDAG.START.                   
                   fslut = FLEXDAG.SLUT.
                   RUN nytid_UI.               
               END.
               ELSE DO:                   
                  ASSIGN
                  TIDREGITAB.START = FLEXDAG.START.     
                  RUN andtid_UI.                  
                  IF AVAILABLE AONRTAB THEN DO:                  
                     IF AONRTAB.AONR NE "" THEN DO:                                             
                        {SOKSTART.I}
                        ASSIGN
                        soktemp.SOKVAL = 1
                        soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
                        soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
                        soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
                        soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                        soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                        {SOKANROP.I}
                        ASSIGN
                        TIDREGITAB.AONR = AONRTAB.AONR
                        TIDREGITAB.DELNR = AONRTAB.DELNR
                        TIDREGITAB.PRISTYP = AONRTAB.PRISTYP   
                        TIDREGITAB.PRIS = soktemp.SOKDECI[1].                         
                     END.
                  END.
               END.   
            END.
         END.   
      END.   
      ELSE IF FLEXTID.KNAPP = "Ut" THEN DO:      
         IF AVAILABLE FLEXREG THEN DO:
            /*egentligen borde kollektiv vatten ha en egen anstformtab med arbetstid 7-16 den alla på vatten har är 8-17
            8-17 jämfört med KV flexavtal6-17 gör att de inte har någorn flexmån på kvällen . de borde ha en timme*/
            IF FLEXAVT.FLEXKOD = "KV" AND PERSONALTAB.DELTID = TRUE THEN flexkvsl = flexkvsl + 1.
            IF flexkvsl < FLEXTID.TID THEN ASSIGN FLEXDAG.SLUT = flexkvsl.
            ELSE ASSIGN FLEXDAG.SLUT = FLEXTID.TID.
         END.   
         IF vart = "AND" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.SLUT = flexstart
               AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  ASSIGN
                  TIDREGITAB.SLUT = FLEXDAG.SLUT.
                  RUN andtid_UI.        
                  IF AVAILABLE AONRTAB THEN DO:                  
                     IF AONRTAB.AONR NE "" THEN DO:                                          
                        {SOKSTART.I}
                        ASSIGN
                        soktemp.SOKVAL = 1
                        soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
                        soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
                        soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
                        soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                        soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                        {SOKANROP.I}
                        ASSIGN
                        TIDREGITAB.AONR = AONRTAB.AONR
                        TIDREGITAB.DELNR = AONRTAB.DELNR
                        TIDREGITAB.PRISTYP = AONRTAB.PRISTYP   
                        TIDREGITAB.PRIS = soktemp.SOKDECI[1].                         
                     END.
                  END.
               END.   
            END.
         END.   
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:
                   fstart = FLEXDAG.START.
                   fslut = FLEXDAG.SLUT.                      
                   IF NOT AVAILABLE AONRTAB THEN DO:                                           
                      FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
                      AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.OVERTIDUTTAG = "F" AND 
                      TIDREGITAB.PRISTYP NE "FRÅNVARO." AND TIDREGITAB.PRISTYP NE "RESTID.." USE-INDEX PKOD NO-LOCK NO-ERROR.
                      IF AVAILABLE TIDREGITAB  THEN DO:
                         FIND FIRST AONRTAB WHERE AONRTAB.AONR = TIDREGITAB.AONR AND AONRTAB.DELNR = TIDREGITAB.DELNR NO-LOCK NO-ERROR.
                      END.
                   END.
                   RUN nytid_UI.                             
               END.
               ELSE DO:         
                  ASSIGN
                  TIDREGITAB.SLUT = FLEXDAG.SLUT.
                  RUN andtid_UI.  
                  IF AVAILABLE AONRTAB THEN DO:                  
                     IF AONRTAB.AONR NE "" THEN DO:                     
                        {SOKSTART.I}
                        ASSIGN
                        soktemp.SOKVAL = 1
                        soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
                        soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
                        soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
                        soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                        soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                        {SOKANROP.I}
                        ASSIGN
                        TIDREGITAB.AONR = AONRTAB.AONR
                        TIDREGITAB.DELNR = AONRTAB.DELNR
                        TIDREGITAB.PRISTYP = AONRTAB.PRISTYP   
                        TIDREGITAB.PRIS = soktemp.SOKDECI[1].                         
                     END.
                  END.
               END.   
            END.
         END.   
      END.   
      IF FLEXTID.KNAPP = "Flex in" THEN DO:
         FIND flexbuff WHERE RECID(flexbuff) = flrec NO-LOCK NO-ERROR.   
         FIND PREV flexbuff USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:
            IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Flex ut" THEN DO:
               ASSIGN flexbuff.GICK = FALSE
               nytid = flexbuff.TID.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
            ELSE IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Lunch ut" THEN DO:
               ASSIGN flexbuff.GICK = FALSE
               nytid = flexbuff.TID.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
            ELSE IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM NE FLEXTID.DATUM THEN DO:
               ASSIGN FLEXTID.KOM = TRUE
               nytid = regstart.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
            ELSE IF flexbuff.PERSONALKOD NE FLEXTID.PERSONALKOD THEN DO:
               ASSIGN FLEXTID.KOM = TRUE
               nytid = regstart.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
         END.  
         ELSE DO:
            ASSIGN FLEXTID.KOM = TRUE
            nytid = regstart.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = FLEXTID.TID.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.
            ASSIGN FLEXDAG.FLARB = 0 - nytid.
         END.
         IF vart = "AND" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.SLUT = flexstart
               AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE 
                  AND tidbuff.OVERTIDUTTAG = "F" AND tidbuff.START = flexstart 
                  AND tidbuff.GODKAND = ""
                  EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:        
                     ASSIGN
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN andtid_UI.                     
                     ASSIGN
                     tidbuff.START = FLEXTID.TID.
                     RUN andtid_UI.                     
                  END.                  
               END.   
            END.
         END.   
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.START LE FLEXTID.TID AND TIDREGITAB.SLUT GE FLEXTID.TID
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:  
                  FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
                  AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO:                  
                     ASSIGN
                     fstart = FLEXDAG.START
                     fslut = FLEXTID.TID.
                     RUN flnytid_UI. 
                     ASSIGN
                     fstart = FLEXTID.TID
                     fslut = FLEXDAG.SLUT.
                     RUN nytid_UI.                  
                  END.
               END.
               ELSE DO:         
                  ASSIGN
                  flkoll = TIDREGITAB.SLUT
                  TIDREGITAB.SLUT = FLEXTID.TID.                     
                  RUN andtid_UI.     
                  {SOKSTART.I}
                  ASSIGN
                  soktemp.SOKVAL = 1
                  soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
                  soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
                  soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
                  soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                  soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                  {SOKANROP.I}
                  CREATE tidbuff.
                  ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  tidbuff.START = FLEXTID.TID     
                  tidbuff.PROGRAM = 'andflex' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                  tidbuff.DATUM = TIDREGITAB.DATUM
                  tidbuff.DAG = TIDREGITAB.DAG 
                  tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                  tidbuff.TRAKTAMENTE = AONRTAB.TRAKTAMENTE 
                  tidbuff.OVERTIDUTTAG = 'F'
                  tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
                  tidbuff.AONR = AONRTAB.AONR 
                  tidbuff.DELNR = AONRTAB.DELNR 
                  tidbuff.PRISTYP = AONRTAB.PRISTYP
                  tidbuff.PRIS = soktemp.SOKDECI[1]                         
                  tidbuff.SLUT = flkoll.
                  IF tidbuff.OVERTIDTILL = "" THEN tidbuff.OVERTIDTILL = PERSONALTAB.BEFATTNING. 
                  ASSIGN                  
                  nytid = tidbuff.START.
                  RUN TIMSEK.P.
                  regstartsek = sekunder.
                  nytid = tidbuff.SLUT.
                  RUN TIMSEK.P.
                  regslutsek = sekunder.
                  regdatum = tidbuff.DATUM.                 
                  RUN TOTTID.P.
                  ASSIGN tidbuff.TOTALT = nytid.
                  VALIDATE tidbuff.
               END.   
            END.
         END.   
      END.      
      ELSE IF FLEXTID.KNAPP = "Flex ut" THEN DO:         
         FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
         AND flexbuff.DATUM = regdatum AND flexbuff.GICK = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
         IF AVAILABLE flexbuff AND RECID(flexbuff) NE RECID(FLEXTID) THEN DO:
            ASSIGN FLEXTID.GICK = FALSE.   
         END.
         ELSE DO:   
            ASSIGN FLEXTID.GICK = TRUE.
            nytid = FLEXTID.TID.
            RUN TIMSEK.P.
            regstartsek = sekunder.
            nytid = regslut.
            RUN TIMSEK.P.
            regslutsek = sekunder. 
            RUN TOTTID.P.
            ASSIGN FLEXDAG.FLARB = 0 - nytid.   
         END.     
         IF vart = "AND" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.SLUT = flexstart
               AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE 
                  AND tidbuff.OVERTIDUTTAG = "F" AND tidbuff.START = flexstart 
                  AND tidbuff.GODKAND = ""
                  EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:        
                     ASSIGN
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN andtid_UI.                     
                     ASSIGN
                     tidbuff.START = FLEXTID.TID     
                     tidbuff.PROGRAM = 'ANDFLEX' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv                  
                     nytid = tidbuff.START.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.
                     nytid = tidbuff.SLUT.
                     RUN TIMSEK.P.
                     regslutsek = sekunder.
                     regdatum = tidbuff.DATUM.                  
                     RUN TOTTID.P.
                     ASSIGN tidbuff.TOTALT = nytid.
                     VALIDATE tidbuff.
                  END.                  
               END.   
            END.
         END.   
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" AND TIDREGITAB.START = FLEXTID.TID EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:                                             
                  FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
                  AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO:         
                     ASSIGN
                     fstart = FLEXDAG.START
                     fslut = FLEXTID.TID.
                     RUN nytid_UI.                 
                     ASSIGN
                     fstart = FLEXTID.TID
                     fslut = regslut.
                     RUN flnytid_UI.                  
                  END.
                  ELSE DO:                           
                     ASSIGN
                     flkoll = TIDREGITAB.SLUT
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN andtid_UI.                  
                     CREATE tidbuff.
                     ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     tidbuff.START = FLEXTID.TID     
                     tidbuff.PROGRAM = 'ANDFLEX' + STRING(TODAY)  + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                     tidbuff.DATUM = TIDREGITAB.DATUM
                     tidbuff.DAG = TIDREGITAB.DAG tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                     tidbuff.TRAKTAMENTE = 0 tidbuff.OVERTIDUTTAG = 'F'
                     tidbuff.AONR = "155" tidbuff.DELNR = 0 tidbuff.PRISTYP = "FRÅNVARO."
                     tidbuff.PRIS = 0.                  
                     ASSIGN tidbuff.SLUT = flkoll.
                     ASSIGN                  
                     nytid = tidbuff.START.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.
                     nytid = tidbuff.SLUT.
                     RUN TIMSEK.P.
                     regslutsek = sekunder.
                     regdatum = tidbuff.DATUM.                 
                     RUN TOTTID.P.
                     ASSIGN tidbuff.TOTALT = nytid.
                     VALIDATE tidbuff.
                  END.  
               END.
               ELSE DO:
                  /*om redan uppdelning gjorts i Tid -byt projekt*/
                  ASSIGN                   
                  TIDREGITAB.PROGRAM = 'ANDFLEX' + STRING(TODAY)  + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv                     
                  TIDREGITAB.TRAKTAMENTE = 0
                  TIDREGITAB.AONR = "155" TIDREGITAB.DELNR = 0 TIDREGITAB.PRISTYP = "FRÅNVARO."
                  TIDREGITAB.PRIS = 0.               
                  
               END.
            END.
                  
         END.   
      END. 
      ELSE IF FLEXTID.KNAPP = "Annat in" THEN DO:
         FIND flexbuff WHERE RECID(flexbuff) = flrec NO-LOCK NO-ERROR.   
         FIND PREV flexbuff USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:
            IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Annat ut" THEN ASSIGN flexbuff.GICK = FALSE.                           
            ELSE IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Lunch ut" THEN ASSIGN flexbuff.GICK = FALSE.                          
            ELSE IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM NE FLEXTID.DATUM THEN ASSIGN FLEXTID.KOM = TRUE.                           
            ELSE IF flexbuff.PERSONALKOD NE FLEXTID.PERSONALKOD THEN ASSIGN FLEXTID.KOM = TRUE.                           
         END.  
         ELSE ASSIGN FLEXTID.KOM = TRUE.         
         IF vart = "AND" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.SLUT = flexstart
               AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE 
                  AND tidbuff.OVERTIDUTTAG = "F" AND tidbuff.START = flexstart 
                  AND tidbuff.GODKAND = ""
                  EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:        
                     ASSIGN
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN andtid_UI.                     
                     ASSIGN
                     tidbuff.START = FLEXTID.TID.
                     RUN andtid_UI.                     
                  END.                  
               END.   
            END.
         END.   
         IF vart = "NYA" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.START LE FLEXTID.TID AND TIDREGITAB.SLUT GE FLEXTID.TID
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
               IF NOT AVAILABLE TIDREGITAB THEN DO:         
                  ASSIGN
                  fstart = FLEXDAG.START
                  fslut = FLEXTID.TID.
                  RUN nytid_UI.                  
                  
                  ASSIGN
                  sok1 = "TID"
                  sok3 = PERSONALTAB.PERSONALKOD.
                  IF Guru.Konstanter:appcon THEN DO: 
                     RUN FLEXTIDH.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
                     (INPUT 1,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                     INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
                  END.
                  ELSE DO:
                     RUN FLEXTIDH.P 
                     (INPUT 1,INPUT-OUTPUT sok1,INPUT-OUTPUT sok2,INPUT-OUTPUT sok3,
                     INPUT-OUTPUT sok4,INPUT-OUTPUT sok5).            
                  END.                  
                  IF sok1 = "FINNS EJ" THEN DO:
                     FIND FIRST AONRTAB WHERE AONRTAB.AONR = varaonr AND AONRTAB.DELNR = vardelnr
                     NO-LOCK NO-ERROR.
                     IF AVAILABLE AONRTAB THEN DO:
                        FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                        TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP USE-INDEX PRISPERS NO-LOCK NO-ERROR.         
                     END.
                  END.
                  ELSE DO:
                     FIND FIRST AONRTAB WHERE AONRTAB.AONR = sok1 AND AONRTAB.DELNR = sok2 NO-LOCK NO-ERROR.    
                     IF AVAILABLE AONRTAB THEN DO:
                        FIND FIRST TIMKOSTNADSTAB WHERE TIMKOSTNADSTAB.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
                        TIMKOSTNADSTAB.PRISTYP = AONRTAB.PRISTYP USE-INDEX PRISPERS NO-LOCK NO-ERROR.         
                     END.
                  END.                 
                  ASSIGN
                  fstart = FLEXTID.TID
                  fslut = FLEXDAG.SLUT.
                  RUN nytid_UI.                  
               END.
               ELSE DO:         
                  ASSIGN
                  flkoll = TIDREGITAB.SLUT
                  TIDREGITAB.SLUT = FLEXTID.TID.                     
                  RUN andtid_UI.                     
                  {SOKSTART.I}
                  ASSIGN
                  soktemp.SOKVAL = 1
                  soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
                  soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
                  soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
                  soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                  soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                  {SOKANROP.I}
                  CREATE tidbuff.
                  ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  tidbuff.START = FLEXTID.TID     
                  tidbuff.PROGRAM = 'andflex' + STRING(TODAY)  + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                  tidbuff.DATUM = TIDREGITAB.DATUM
                  tidbuff.DAG = TIDREGITAB.DAG 
                  tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                  tidbuff.TRAKTAMENTE = AONRTAB.TRAKTAMENTE 
                  tidbuff.OVERTIDUTTAG = 'F'
                  tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
                  tidbuff.AONR = AONRTAB.AONR 
                  tidbuff.DELNR = AONRTAB.DELNR 
                  tidbuff.PRISTYP = AONRTAB.PRISTYP
                  tidbuff.PRIS = soktemp.SOKDECI[1]
                  tidbuff.SLUT = flkoll.
                  IF tidbuff.OVERTIDTILL = "" THEN tidbuff.OVERTIDTILL = PERSONALTAB.BEFATTNING. 
                  ASSIGN                  
                  nytid = tidbuff.START.
                  RUN TIMSEK.P.
                  regstartsek = sekunder.
                  nytid = tidbuff.SLUT.
                  RUN TIMSEK.P.
                  regslutsek = sekunder.
                  regdatum = tidbuff.DATUM.                 
                  RUN TOTTID.P.
                  ASSIGN tidbuff.TOTALT = nytid.
                  VALIDATE tidbuff.
               END.   
            END.
         END.   
      END.               
      ELSE IF FLEXTID.KNAPP = "Annat ut" THEN DO:         
         
         IF vart = "AND" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.SLUT = flexstart
               AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  FIND FIRST tidbuff WHERE tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND tidbuff.DATUM = regdatum AND tidbuff.TIDLOG = TRUE 
                  AND tidbuff.OVERTIDUTTAG = "F" AND tidbuff.START = flexstart 
                  AND tidbuff.GODKAND = ""
                  EXCLUSIVE-LOCK NO-ERROR.
                  IF AVAILABLE tidbuff THEN DO:        
                     ASSIGN
                     TIDREGITAB.SLUT = FLEXTID.TID.
                     RUN andtid_UI.                     
                     ASSIGN
                     tidbuff.START = FLEXTID.TID     
                     tidbuff.PROGRAM = 'ANDFLEX' + STRING(TODAY)  + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv             
                     nytid = tidbuff.START.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.
                     nytid = tidbuff.SLUT.
                     RUN TIMSEK.P.
                     regslutsek = sekunder.
                     regdatum = tidbuff.DATUM.                  
                     RUN TOTTID.P.
                     ASSIGN tidbuff.TOTALT = nytid.
                     VALIDATE tidbuff.
                  END.                  
               END.   
            END.
         END.   
         IF vart = "nya" THEN DO:
            FIND FIRST flexbuff WHERE flexbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND flexbuff.DATUM = regdatum AND flexbuff.GICK = TRUE USE-INDEX FLEX NO-LOCK NO-ERROR.
            IF AVAILABLE flexbuff THEN DO:
               ASSIGN FLEXTID.GICK = FALSE.   
            END.
            ELSE DO:   
               ASSIGN FLEXTID.GICK = TRUE.
            END.
            kolllu = 0.
            IF lunchstarten NE lunchslutet AND FLEXTID.TID GE lunchslutet THEN kolllu = 1.
            IF lunchstarten =  lunchslutet AND FLEXTID.TID > regstart  THEN kolllu =  1.
            IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
               kollfv1 = regslut - 1.
               kollfv2 = FLEXTID.TID + 1.
            END.
            IF Guru.Konstanter:globforetag = "MISV" THEN DO:            
               kollfv1 = klock60(klock100(regslut) - 1.25).
               kollfv2 = klock60(klock100(FLEXTID.TID) + 1.25).
            END.

            FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
            AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
            AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" USE-INDEX PSTART EXCLUSIVE-LOCK NO-ERROR.
            IF NOT AVAILABLE TIDREGITAB THEN DO:         
               ASSIGN
               fstart = FLEXDAG.START
               fslut = FLEXDAG.SLUT.
               RUN nytid_UI.               
            END.                                                               
            ELSE DO:         
               IF FLEXTID.TID > TIDREGITAB.START THEN DO:                  
                  IF TIDREGITAB.AONR = varaonr AND TIDREGITAB.DELNR = vardelnr THEN musz = musz.
                  ELSE DO:                       
                     ASSIGN
                     TIDREGITAB.SLUT = FLEXTID.TID.                     
                     RUN andtid_UI.                  
                     {SOKSTART.I}
                     ASSIGN
                     soktemp.SOKVAL = 1
                     soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
                     soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
                     soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
                     soktemp.SOKCHAR[4] = TIDREGITAB.OVERTIDTILL 
                     soktemp.SOKDATE[1] = TIDREGITAB.DATUM.
                     {SOKANROP.I}
                     CREATE tidbuff.
                     ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                     tidbuff.START = FLEXTID.TID     
                     tidbuff.PROGRAM = 'andflex' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                     tidbuff.DATUM = TIDREGITAB.DATUM
                     tidbuff.DAG = TIDREGITAB.DAG 
                     tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                     tidbuff.TRAKTAMENTE = AONRTAB.TRAKTAMENTE 
                     tidbuff.OVERTIDUTTAG = 'F'
                     tidbuff.OVERTIDTILL = TIDREGITAB.OVERTIDTILL
                     tidbuff.AONR = AONRTAB.AONR 
                     tidbuff.DELNR = AONRTAB.DELNR 
                     tidbuff.PRISTYP = AONRTAB.PRISTYP
                     tidbuff.PRIS = soktemp.SOKDECI[1]                         
                     tidbuff.SLUT = regslut.
                     IF tidbuff.OVERTIDTILL = "" THEN tidbuff.OVERTIDTILL = PERSONALTAB.BEFATTNING. 
                     ASSIGN                  
                     nytid = tidbuff.START.
                     RUN TIMSEK.P.
                     regstartsek = sekunder.
                     nytid = tidbuff.SLUT.
                     RUN TIMSEK.P.
                     regslutsek = sekunder.
                     regdatum = tidbuff.DATUM.                 
                     RUN TOTTID.P.
                     ASSIGN tidbuff.TOTALT = nytid.
                     VALIDATE tidbuff.
                     IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                           
                        /*IF onr = "140" AND tid < (regslut - 1) AND tid > (regslut - 3)   THEN DO:            */
                        kolllu = 0.
                        IF lunchstarten NE lunchslutet AND FLEXTID.TID GE lunchslutet THEN kolllu = 1.
                        IF lunchstarten =  lunchslutet AND FLEXTID.TID > regstart  THEN kolllu =  1.
                        IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
                           kollfv1 = regslut - 1.
                           kollfv2 = FLEXTID.TID + 1.
                        END.
                        IF Guru.Konstanter:globforetag = "MISV" THEN DO:            
                           kollfv1 = klock60(klock100(regslut) - 1.25).
                           kollfv2 = klock60(klock100(FLEXTID.TID) + 1.25).
                        END.

                        /*halvdagar har ingen lunch*/
                        IF AONRTAB.AONR = "140" AND FLEXTID.TID <  kollfv1 AND kolllu =  1   THEN DO:                             
                           /*special vid friskvård i kombination med flex*/
                           ASSIGN
                           tidbuff.SLUT = kollfv2                 
                           nytid = tidbuff.START.
                           RUN TIMSEK.P.
                           ASSIGN regstartsek = sekunder
                           nytid = tidbuff.SLUT.
                           RUN TIMSEK.P.
                           ASSIGN regslutsek = sekunder
                           regdatum = tidbuff.DATUM.
                           RUN TOTTID.P.
                           ASSIGN tidbuff.TOTALT = nytid.
                           VALIDATE tidbuff.                  
                           CREATE tidbuff.
                           ASSIGN tidbuff.PERSONALKOD = PERSONALTAB.PERSONALKOD
                           tidbuff.START = kollfv2     
                           tidbuff.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
                           tidbuff.DATUM = TIDREGITAB.DATUM
                           tidbuff.DAG = TIDREGITAB.DAG 
                           tidbuff.VECKONUMMER = TIDREGITAB.VECKONUMMER
                           tidbuff.TRAKTAMENTE = 0                           
                           tidbuff.OVERTIDUTTAG = 'F'
                           tidbuff.AONR = "155" 
                           tidbuff.DELNR = 0 
                           tidbuff.PRISTYP = "FRÅNVARO."
                           tidbuff.PRIS = 0             
                           tidbuff.SLUT = regslut.
                           
                           ASSIGN            
                           nytid = tidbuff.START.
                           RUN TIMSEK.P.
                           regstartsek = sekunder.
                           nytid = tidbuff.SLUT.
                           RUN TIMSEK.P.
                           regslutsek = sekunder.
                           regdatum = tidbuff.DATUM.            
                           RUN TOTTID.P.
                           ASSIGN tidbuff.TOTALT = nytid.
                           VALIDATE tidbuff.
                           /*tidtabrec = RECID(tidbuff).                           */
                        END.
                     END.            
                  END.
               END.
            END.                         
            IF AONRTAB.AONR = "140" AND FLEXTID.TID < kollfv1 AND kolllu =  1   THEN DO:               
               ASSIGN 
               FLEXTID.KNAPP = "Flex ut" 
               FLEXTID.TID =  kollfv2.
            END.            
         END.
      END.
      ELSE IF FLEXTID.KNAPP = "Lunch in" THEN DO:
         FIND flexbuff WHERE RECID(flexbuff) = flrec NO-LOCK NO-ERROR.   
         FIND PREV flexbuff USE-INDEX FLEX EXCLUSIVE-LOCK NO-ERROR.
         IF AVAILABLE flexbuff THEN DO:
            IF flexbuff.PERSONALKOD = FLEXTID.PERSONALKOD AND 
            flexbuff.DATUM = FLEXTID.DATUM AND flexbuff.KNAPP = "Flex ut" THEN DO:
               ASSIGN flexbuff.GICK = FALSE
               nytid = flexbuff.TID.
               RUN TIMSEK.P.
               regstartsek = sekunder.
               nytid = FLEXTID.TID.
               RUN TIMSEK.P.
               regslutsek = sekunder. 
               RUN TOTTID.P.
               ASSIGN FLEXDAG.FLARB = 0 - nytid.
            END.
         END.
         IF vart = "AND" OR vart = "nya" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.START < flexstart
               AND TIDREGITAB.SLUT > flexstart AND TIDREGITAB.GODKAND = ""
               EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                   RUN andtid_UI.                  
               END.   
            END.
         END.   
      END.    
      ELSE IF FLEXTID.KNAPP = "Lunch ut" THEN DO:
         IF vart = "AND" OR vart = "nya" THEN DO:
            IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
            ELSE DO:  
               FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
               AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
               AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.START < flexstart
               AND TIDREGITAB.SLUT > flexstart AND TIDREGITAB.GODKAND = ""
                EXCLUSIVE-LOCK NO-ERROR.
               IF AVAILABLE TIDREGITAB THEN DO:         
                  RUN andtid_UI.                  
               END.   
            END.
         END.   
      END.  
      ELSE IF FLEXTID.KNAPP = "Övertid in" THEN DO:         
         IF regstart = regslut OR FLEXTID.TID > regslut THEN DO:  /*HELG*/            
            IF vart = "NYA" THEN DO:
               IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
               ELSE DO:  
                  FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE  AND TIDREGITAB.START LE FLEXTID.TID
                  AND TIDREGITAB.SLUT GE FLEXTID.TID 
                  AND TIDREGITAB.OVERTIDUTTAG NE "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO: 
                     fstart = FLEXTID.TID.                   
                     fslut = FLEXTID.TID.
                     RUN nytid_UI.               
                  END.               
               END.
            END.
         END.   
         ELSE DO:            
            IF vart = "NYA" THEN DO:
               IF AVAILABLE GODKOLL AND GODKOLL.DATUM GE regdatum THEN regdatum = regdatum.
               ELSE DO:  
                  FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
                  AND TIDREGITAB.DATUM = regdatum AND TIDREGITAB.TIDLOG = TRUE 
                  AND TIDREGITAB.OVERTIDUTTAG = "F" AND TIDREGITAB.GODKAND = "" EXCLUSIVE-LOCK NO-ERROR.
                  IF NOT AVAILABLE TIDREGITAB THEN DO:                             
   
                      fstart = FLEXTID.TID.                   
                      fslut = FLEXDAG.START.
                      RUN nytid_UI.               
                      fstart = FLEXDAG.START.                   
                      fslut = FLEXDAG.SLUT.
                      RUN nytid_UI.               
                  END.
                  
               END.
            END.   
         END.
      END.
      RUN SLUTARB.P.
   END.   
   
END PROCEDURE.

PROCEDURE andtid_UI :
   ASSIGN
   TIDREGITAB.PROGRAM = 'ANDFLEX' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
   nytid = TIDREGITAB.START.
   RUN TIMSEK.P.
   regstartsek = sekunder.
   nytid = TIDREGITAB.SLUT.
   RUN TIMSEK.P.
   regslutsek = sekunder.
   regdatum = TIDREGITAB.DATUM.                  
   RUN TOTTID.P.
   ASSIGN TIDREGITAB.TOTALT = nytid.
   VALIDATE TIDREGITAB.
END PROCEDURE.

PROCEDURE nytid_UI :
   {SOKSTART.I}
   ASSIGN
   soktemp.SOKVAL = 1
   soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
   soktemp.SOKCHAR[2] = PERSONALTAB.PERSONALKOD
   soktemp.SOKCHAR[3] = AONRTAB.PRISTYP
   soktemp.SOKCHAR[4] = PERSONALTAB.BEFATTNING 
   soktemp.SOKDATE[1] = regdatum.
   {SOKANROP.I}
   CREATE TIDREGITAB.
   ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
   TIDREGITAB.START = fstart     
   TIDREGITAB.SLUT = fslut   
   TIDREGITAB.PROGRAM = 'ANDFLEX' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
   TIDREGITAB.DATUM = regdatum
   TIDREGITAB.DAG = regdagnamn
   TIDREGITAB.VECKONUMMER = regvnr
   TIDREGITAB.TRAKTAMENTE = AONRTAB.TRAKTAMENTE
   TIDREGITAB.OVERTIDUTTAG = 'F'
   TIDREGITAB.AONR = AONRTAB.AONR
   TIDREGITAB.DELNR = AONRTAB.DELNR
   TIDREGITAB.PRISTYP = AONRTAB.PRISTYP
   TIDREGITAB.OVERTIDTILL = PERSONALTAB.BEFATTNING.
   TIDREGITAB.PRIS = soktemp.SOKDECI[1].                         
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
   VALIDATE TIDREGITAB.
END PROCEDURE.

PROCEDURE flnytid_UI :
   CREATE TIDREGITAB.
   ASSIGN TIDREGITAB.PERSONALKOD = PERSONALTAB.PERSONALKOD
   TIDREGITAB.START = fstart     
   TIDREGITAB.SLUT = fslut   
   TIDREGITAB.PROGRAM = 'GLOMDF' + STRING(TODAY) + STRING(TIME,"HH:MM") + Guru.Konstanter:globanv
   TIDREGITAB.DATUM = regdatum
   TIDREGITAB.DAG = regdagnamn TIDREGITAB.VECKONUMMER = regvnr
   TIDREGITAB.TRAKTAMENTE = 0 TIDREGITAB.OVERTIDUTTAG = 'F'
   TIDREGITAB.AONR = "155" TIDREGITAB.DELNR = 0 TIDREGITAB.PRISTYP = "FRÅNVARO."
   TIDREGITAB.PRIS = 0.                         
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
   VALIDATE TIDREGITAB.
END PROCEDURE.


