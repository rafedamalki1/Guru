/*TIDINLULE.P*/
/*
GUDIM1 4 TEXT('Konto')
GUDIM2 3 TEXT('AO')
GUDIM3 4 TEXT('Rgr')
GUDIM4 9 TEXT('Projekt')
GUBDAT 10 0 TEXT('Bokf.datum') Format: ÅÅÅÅMMDD + två blanka
GUBELX 17 3 TEXT('Belopp i SEK') 17 inkl. 3 dec
GUTEXT 30 TEXT('Verifikattext')
GUPERI 8 0 TEXT('Period') Format: ÅÅÅÅMM + två blanka
GUDOTY 3 TEXT('Verifikattyp') 
GUIDNO 7 0 TEXT('Verifikatnummer')
GUTID1 6 2 TEXT('Ant tim normaltid') 
GUBEL1 17 3 TEXT('Belopp normaltid')
GUTID2 6 2 TEXT('Ant tim övertid') 
GUBEL2 17 3 TEXT('Belopp övertid')
GUTID3 6 2 TEXT('Ant tim kval. övertid') 
GUBEL3 17 3 TEXT('Belopp kval. övertid')
*/
{NAMNDB.I}
FUNCTION klockan100 RETURNS DECIMAL
  ( INPUT ber60 AS DECIMAL ):
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600.

END FUNCTION.

FUNCTION klockan60 RETURNS DECIMAL
  ( INPUT ber100 AS DECIMAL ):
  RETURN TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60 . 

END FUNCTION.

&Scoped-define NEW NEW
{REGVAR.I}
DEFINE TEMP-TABLE tidluletemp NO-UNDO LIKE TIDREGITAB
   FIELD DEBET AS LOGICAL.
DEFINE TEMP-TABLE ekoforst
   FIELD BEFATTNING AS CHARACTER 
   FIELD OMRADE LIKE PERSONALTAB.OMRADE 
   FIELD PERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD AONR LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD DATUM LIKE TIDREGITAB.DATUM
   FIELD ORG LIKE EKRAPPRESULT.EORG  
   FIELD PRIS LIKE TIDREGITAB.PRIS   
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP    
   FIELD BELOPP AS DECIMAL
   FIELD OBELOPP AS DECIMAL
   FIELD ANTAL AS DECIMAL
   FIELD OTIMMAR AS DECIMAL             
 /*  FIELD LONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD LONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    */
   FIELD LONBELOPP LIKE EKRAPPRESULT.ELONBELOPP  
   FIELD OANT1 LIKE TIDREGITAB.OANT1 
   FIELD OANT2 LIKE TIDREGITAB.OANT2 
   FIELD OANT3 LIKE TIDREGITAB.OANT3 
   FIELD OKOD1 LIKE TIDREGITAB.OKOD1 
   FIELD OKOD2 LIKE TIDREGITAB.OKOD2 
   FIELD OKOD3 LIKE TIDREGITAB.OKOD3   
   FIELD TRAKTKOD LIKE TIDREGITAB.TRAKTKOD 
   FIELD TRAKTANTAL LIKE TIDREGITAB.TRAKTANTAL 
   FIELD TRAAVTAL LIKE PERSONALTAB.TRAAVTAL      
   FIELD TBELOPP LIKE EKRAPPRESULT.EBELOPP
   INDEX PERSAO IS PRIMARY PERSONALKOD AONR DELNR ASCENDING
   INDEX ORGAONR ORG AONR DELNR ASCENDING.
DEFINE TEMP-TABLE slutsum               
   FIELD BEFATTNING AS CHARACTER
   FIELD OMRADE LIKE PERSONALTAB.OMRADE 
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD DATUM LIKE TIDREGITAB.DATUM 
   FIELD ARTAL AS INTEGER 
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ANTAL AS DECIMAL LABEL "TIMMAR"          
   FIELD OTIMMAR AS DECIMAL LABEL "OTIMMAR"
   FIELD BELOPP AS DECIMAL LABEL "ARBKOSTNAD"           
   FIELD OBELOPP AS DECIMAL LABEL "Ö-KOSTNAD"  
   FIELD OANTAL AS DECIMAL  LABEL "Ö-ANTAL"         
   FIELD TBELOPP AS DECIMAL LABEL "T-KOSTNAD"
   FIELD TANTAL AS DECIMAL  LABEL "T-ANTAL"         
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.  
DEFINE TEMP-TABLE oversum   
   FIELD BEFATTNING AS CHARACTER
   FIELD PERSONALKOD LIKE TIDREGITAB.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS     
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP 
   FIELD DATUM LIKE TIDREGITAB.DATUM
   FIELD AONR LIKE TIDREGITAB.AONR
   FIELD DELNR LIKE TIDREGITAB.DELNR 
   FIELD ANTAL LIKE EKRAPPRESULT.EANTAL                 
   FIELD BELOPP LIKE EKRAPPRESULT.EBELOPP              
   FIELD OVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL
   INDEX AONR IS PRIMARY AONR DELNR ASCENDING.     
DEFINE TEMP-TABLE overtidhj 
   FIELD BEFATTNING AS CHARACTER
   FIELD PERSONALKOD LIKE ekoforst.PERSONALKOD 
   FIELD PRIS LIKE TIDREGITAB.PRIS    
   FIELD PRISTYP LIKE TIDREGITAB.PRISTYP
   FIELD DATUM LIKE TIDREGITAB.DATUM   
   FIELD AONR LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE TIDREGITAB.DELNR     
   FIELD BELOPP LIKE TIDREGITAB.PRIS
   FIELD OVERTIDTILL LIKE TIDREGITAB.OVERTIDTILL   
   FIELD OVERANTAL LIKE TIDREGITAB.OVERANTAL.           
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE befvar LIKE  PERSONALTAB.BEFATTNING NO-UNDO.
DEFINE VARIABLE omrpers LIKE PERSONALTAB.OMRADE NO-UNDO.
DEFINE VARIABLE tidtim AS DECIMAL NO-UNDO. 
DEFINE VARIABLE otidtim AS DECIMAL NO-UNDO.
DEFINE VARIABLE tid100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim1 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim2 AS DECIMAL NO-UNDO.
DEFINE VARIABLE otidtim3 AS DECIMAL NO-UNDO.

DEFINE VARIABLE aonrtvar LIKE AONRTAB.AONR NO-UNDO.  
DEFINE VARIABLE delnrtvar LIKE AONRTAB.DELNR NO-UNDO.  
DEFINE VARIABLE pkod LIKE PERSONALTAB.PERSONALKOD NO-UNDO. 
DEFINE VARIABLE kollvecka LIKE VECKONATT.VECKOKORD NO-UNDO.
DEFINE VARIABLE nattaonr LIKE AONRTAB.AONR NO-UNDO.  
DEFINE VARIABLE nattdelnr LIKE AONRTAB.DELNR NO-UNDO.  
DEFINE VARIABLE nattaoomr LIKE AONRTAB.OMRADE NO-UNDO. 
    
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE prognamnvarhj AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE felvar AS INTEGER NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE aonrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE delnrvar AS INTEGER NO-UNDO.
DEFINE VARIABLE totnormal     AS DECIMAL NO-UNDO.
DEFINE VARIABLE totprisnormal AS DECIMAL NO-UNDO.
DEFINE VARIABLE totenk        AS DECIMAL NO-UNDO.
DEFINE VARIABLE totprisenk    AS DECIMAL NO-UNDO.
DEFINE VARIABLE totkval       AS DECIMAL NO-UNDO.
DEFINE VARIABLE totpriskval   AS DECIMAL NO-UNDO.
DEFINE VARIABLE totnormalhjkoll AS DECIMAL NO-UNDO.
DEFINE VARIABLE totnormalhj AS DECIMAL NO-UNDO.

DEFINE VARIABLE rad AS INTEGER NO-UNDO.
DEFINE STREAM eko.  
DEFINE STREAM ekospar.
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)" 
   INDEX PRO IS PRIMARY PROGNAMN. 
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(132)".   
IF namndb() = "UTBI" THEN RETURN.
regar = YEAR(TODAY).
IF MONTH(TODAY) = 1 THEN regmannamn = "jan".
ELSE IF MONTH(TODAY) = 2 THEN regmannamn = "feb".
ELSE IF MONTH(TODAY) = 3 THEN regmannamn = "mar". 
ELSE IF MONTH(TODAY) = 4 THEN regmannamn = "apr". 
ELSE IF MONTH(TODAY) = 5 THEN regmannamn = "maj". 
ELSE IF MONTH(TODAY) = 6 THEN regmannamn = "jun".
ELSE IF MONTH(TODAY) = 7 THEN regmannamn = "jul".
ELSE IF MONTH(TODAY) = 8 THEN regmannamn = "aug". 
ELSE IF MONTH(TODAY) = 9 THEN regmannamn = "sep".
ELSE IF MONTH(TODAY) = 10 THEN regmannamn = "okt". 
ELSE IF MONTH(TODAY) = 11 THEN regmannamn = "nov".
ELSE IF MONTH(TODAY) = 12 THEN regmannamn = "dec".
{AMERICANEUROPEAN.I}
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.   
IF Guru.Konstanter:globforetag = "ELPA" THEN DO:            
   prognamnvar = "D:\DELAD\PRO9s\".   
END.
ELSE DO:            
   prognamnvar = "D:\elpool\DELAD\PRO9s\".
END.
progkopia = prognamnvar + "imkopia\tidko" + STRING(regar) + regmannamn + ".txt".    
kommandoprog = prognamnvar + "import\guruin.txt".
kommando = "DIR/a:-d /b " + prognamnvar + "import\verif2guru*.* > " + prognamnvar + "import\GURUIN.TXT".   

FIND FIRST TIDREGITAB WHERE TIDREGITAB.VECKOKORD = "w" + STRING(TODAY,"99999999") NO-LOCK NO-ERROR.
IF AVAILABLE TIDREGITAB THEN RETURN.
RUN startin_UI.
FOR EACH TIDREGITAB WHERE TIDREGITAB.VECKOKORD = "w" + STRING(TODAY,"99999999") NO-LOCK:
   CREATE tidluletemp.
   BUFFER-COPY TIDREGITAB TO tidluletemp.
END.
RUN sumtid_UI.
EMPTY TEMP-TABLE tidluletemp NO-ERROR.   
EMPTY TEMP-TABLE ekoforst NO-ERROR.   
EMPTY TEMP-TABLE slutsum NO-ERROR.                 
EMPTY TEMP-TABLE oversum NO-ERROR.     
EMPTY TEMP-TABLE overtidhj NO-ERROR. 
FOR EACH TIDFEL WHERE TIDFEL.VECKOKORD = "w" + STRING(TODAY,"99999999") NO-LOCK:
   CREATE tidluletemp.
   BUFFER-COPY TIDFEL TO tidluletemp.
END.
RUN sumtid_UI.
RUN namn_UI.
EMPTY TEMP-TABLE tidluletemp NO-ERROR.   
EMPTY TEMP-TABLE ekoforst NO-ERROR.   
EMPTY TEMP-TABLE slutsum NO-ERROR.                 
EMPTY TEMP-TABLE oversum NO-ERROR.     
EMPTY TEMP-TABLE overtidhj NO-ERROR. 
prognamnvarhj = prognamnvar + "autotid.txt".
OUTPUT TO VALUE(prognamnvarhj) APPEND.
PUT "KLAR TIDINLULE " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
{EUROPEANAMERICAN.I}
PROCEDURE startin_UI:
   OS-DELETE VALUE(kommandoprog) NO-ERROR.
   OS-COMMAND SILENT VALUE(kommando).
   INPUT FROM VALUE(kommandoprog) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE infil.
         ASSIGN.
         IMPORT infil NO-ERROR.
      END.
   END.
   INPUT CLOSE.
   FOR EACH infil:   
      IF INDEX(infil.PROGNAMN,".txt") = 0 THEN DO:       
         DELETE infil.
         NEXT.
      END.
   END.
   FIND FIRST infil NO-ERROR.
   IF NOT AVAILABLE infil THEN DO:
      prognamnvarhj = prognamnvar + "autotid.txt".
      OUTPUT TO VALUE(prognamnvarhj) APPEND.
      PUT "DET FANNS INGEN TIDFIL FRÅN EKO " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      RETURN.
   END.
   FOR EACH infil:  
      EMPTY TEMP-TABLE tidin NO-ERROR. 
      prognamnvarhj = prognamnvar + "import\" + infil.PROGNAMN.
      RUN in_UI.
      IF felvar = 0 THEN DO:
         OUTPUT TO VALUE(progkopia) APPEND.
         PUT "ny fil  " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
         OUTPUT CLOSE.
         OS-APPEND VALUE(prognamnvarhj) VALUE(progkopia).
         OS-DELETE VALUE(prognamnvarhj).
      END.
      felvar = 0.
   END.
END PROCEDURE.

PROCEDURE in_UI:
   progque = prognamnvar + "import\tid.q".
   OS-DELETE SILENT VALUE(progque).       
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN DO:
      felvar = 1.
      RETURN.
   END.
   
   OS-COMMAND SILENT VALUE(kommando) VALUE(prognamnvarhj) > VALUE(progque).      
   INPUT FROM VALUE(progque) NO-ECHO.
   REPEAT:
      SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.   
      REPEAT:
         IF INDEX(words,'"',1) = 0 THEN LEAVE.
         words = REPLACE(words,'"',' ').
      END.
      CREATE tidin.   
      ASSIGN tidin.TIN = words.   
   END.
   INPUT CLOSE.  

   FOR EACH tidin:
      IF SUBSTRING(tidin.TIN,86,2) = "TI" THEN RUN tid_UI.
      ELSE RUN kostreg_UI.
      DELETE tidin.
   END.
END PROCEDURE.

PROCEDURE tid_UI:  
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = TRIM(SUBSTRING(tidin.TIN,48,4)) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PERSONALTAB THEN DO:
      ASSIGN
      befvar = "XXXX"
      pkod = "XXX".
      FIND FIRST BEFATTNINGSTAB NO-LOCK NO-ERROR.
      IF AVAILABLE BEFATTNINGSTAB THEN befvar = BEFATTNINGSTAB.BEFATTNING.
   END.
   ELSE DO:
      ASSIGN
      befvar = PERSONALTAB.BEFATTNING
      pkod = PERSONALTAB.PERSONALKOD.
      FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = befvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE BEFATTNINGSTAB THEN DO:
         FIND FIRST BEFATTNINGSTAB NO-LOCK NO-ERROR.
         IF AVAILABLE BEFATTNINGSTAB THEN befvar = BEFATTNINGSTAB.BEFATTNING.
      END.
   END.
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,12,6) AND AONRTAB.DELNR = INTEGER(SUBSTRING(tidin.TIN,18,3)) 
   NO-LOCK NO-ERROR.
   IF NOT AVAILABLE AONRTAB THEN aonrvar = "XXXXXX".
   ELSE DO:
      ASSIGN
      aonrvar = AONRTAB.AONR
      delnrvar = AONRTAB.DELNR.
   END.
   DO TRANSACTION:
      regdatum = DATE(INTEGER(SUBSTRING(tidin.TIN,25,2)),INTEGER(SUBSTRING(tidin.TIN,27,2)),INTEGER(SUBSTRING(tidin.TIN,21,4))).
      RUN REGVEC.P.
      RUN REGDAG.P.
      ASSIGN
      totnormal = DECIMAL(SUBSTRING(tidin.TIN,100,6))
      totprisnormal = DECIMAL(SUBSTRING(tidin.TIN,106,17))
      totenk = DECIMAL(SUBSTRING(tidin.TIN,123,6))
      totprisenk = DECIMAL(SUBSTRING(tidin.TIN,129,17))
      totkval = DECIMAL(SUBSTRING(tidin.TIN,146,6))
      totpriskval = DECIMAL(SUBSTRING(tidin.TIN,152,17)).
      IF totnormal < 0 OR totenk < 0 OR totkval < 0 THEN DO:
         /*kredit poster*/
         IF totnormal NE 0 THEN DO:
            ASSIGN
            totnormalhjkoll = totnormal
            totnormalhj = totnormal.
            REPEAT:
               IF totnormalhjkoll = 0 THEN LEAVE.
               RUN tidfelskap_UI.
               IF totnormalhjkoll > 8 THEN DO:               
                  totnormalhj = 8.
                  totnormalhjkoll = totnormalhjkoll - 8.
               END.
               ELSE DO:
                  totnormalhj = totnormalhjkoll.
                  totnormalhjkoll = 0.
               END.
               TIDFEL.SLUT = klockan60(totnormalhj + 8.00).
               TIDFEL.TOTALT = klockan60(totnormalhj).
               ASSIGN
               TIDFEL.PRIS = totprisnormal / totnormal            
               TIDFEL.START = 8.00.                
            END.
         END.
         IF totenk NE 0 THEN DO:
            ASSIGN
            totnormalhjkoll = totenk
            totnormalhj = totenk.
            REPEAT:
               IF totnormalhjkoll = 0 THEN LEAVE.
               RUN tidfelskap_UI.
               IF totnormalhjkoll > 8 THEN DO:               
                  totnormalhj = 8.
                  totnormalhjkoll = totnormalhjkoll - 8.
               END.
               ELSE DO:
                  totnormalhj = totnormalhjkoll.
                  totnormalhjkoll = 0.
               END.
               TIDFEL.SLUT = klockan60(totnormalhj + 16.00).
               TIDFEL.TOTALT = klockan60(totnormalhj).
               ASSIGN
               TIDFEL.OANT1 = TIDREGITAB.TOTALT
               TIDFEL.OKOD1 = "ENK" 
               TIDFEL.PRIS = totprisenk / totenk            
               TIDFEL.START = 16.00.                   
            END.
         END.
         IF totkval NE 0 THEN DO:
            ASSIGN
            totnormalhjkoll = totkval
            totnormalhj = totkval.
            REPEAT:
               IF totnormalhjkoll = 0 THEN LEAVE.
               RUN tidfelskap_UI.
               IF totnormalhjkoll > 8 THEN DO:               
                  totnormalhj = 8.
                  totnormalhjkoll = totnormalhjkoll - 8.
               END.
               ELSE DO:
                  totnormalhj = totnormalhjkoll.
                  totnormalhjkoll = 0.
               END.
               TIDFEL.SLUT = klockan60(totnormalhj + 16.00).
               TIDFEL.TOTALT = klockan60(totnormalhj).
               ASSIGN
               TIDFEL.OANT1 = TIDREGITAB.TOTALT
               TIDFEL.OKOD1 = "KVAL" 
               TIDFEL.PRIS = totpriskval / totkval            
               TIDFEL.START = 16.00.                 
            END.
         END.
      END.
      ELSE DO:
         /*debet poster*/
         IF totnormal NE 0 THEN DO:
            ASSIGN
            totnormalhjkoll = totnormal
            totnormalhj = totnormal.
            REPEAT:
               IF totnormalhjkoll = 0 THEN LEAVE.
               RUN tidskap_UI.
               IF totnormalhjkoll > 8 THEN DO:               
                  totnormalhj = 8.
                  totnormalhjkoll = totnormalhjkoll - 8.
               END.
               ELSE DO:
                  totnormalhj = totnormalhjkoll.
                  totnormalhjkoll = 0.
               END.
               TIDREGITAB.SLUT = klockan60(totnormalhj + 8.00).
               TIDREGITAB.TOTALT = klockan60(totnormalhj).
               ASSIGN
               TIDREGITAB.PRIS = totprisnormal / totnormal            
               TIDREGITAB.START = 8.00.                
            END.
         END.
         IF totenk NE 0 THEN DO:
            ASSIGN
            totnormalhjkoll = totenk
            totnormalhj = totenk.
            REPEAT:
               IF totnormalhjkoll = 0 THEN LEAVE.
               RUN tidskap_UI.
               IF totnormalhjkoll > 8 THEN DO:               
                  totnormalhj = 8.
                  totnormalhjkoll = totnormalhjkoll - 8.
               END.
               ELSE DO:
                  totnormalhj = totnormalhjkoll.
                  totnormalhjkoll = 0.
               END.
               TIDREGITAB.SLUT = klockan60(totnormalhj + 16.00).
               TIDREGITAB.TOTALT = klockan60(totnormalhj).
               ASSIGN
               TIDREGITAB.OANT1 = TIDREGITAB.TOTALT
               TIDREGITAB.OKOD1 = "ENK" 
               TIDREGITAB.PRIS = totprisenk / totenk            
               TIDREGITAB.START = 16.00.     
               
            END.
         END.
         IF totkval NE 0 THEN DO:
            ASSIGN
            totnormalhjkoll = totkval
            totnormalhj = totkval.
            REPEAT:
               IF totnormalhjkoll = 0 THEN LEAVE.
               RUN tidskap_UI.
               IF totnormalhjkoll > 8 THEN DO:               
                  totnormalhj = 8.
                  totnormalhjkoll = totnormalhjkoll - 8.
               END.
               ELSE DO:
                  totnormalhj = totnormalhjkoll.
                  totnormalhjkoll = 0.
               END.
               TIDREGITAB.SLUT = klockan60(totnormalhj + 16.00).
               TIDREGITAB.TOTALT = klockan60(totnormalhj).
               ASSIGN
               TIDREGITAB.OANT1 = TIDREGITAB.TOTALT
               TIDREGITAB.OKOD1 = "KVAL" 
               TIDREGITAB.PRIS = totpriskval / totkval            
               TIDREGITAB.START = 16.00.                 
            END.
         END.
      END.
   END.
END PROCEDURE.

PROCEDURE tidfelskap_UI:                       
   CREATE TIDFEL.
   ASSIGN
   TIDFEL.ANVANDARE = "ASWEKOKREDIT"
   TIDFEL.AONR = aonrvar
   TIDFEL.DAG = regdagnamn
   TIDFEL.DATUM = 03/31/2005 /*regdatum*/ 
   TIDFEL.DELNR = delnrvar
   TIDFEL.GODKAND = "G" + STRING(TODAY,"99999999")
   TIDFEL.OVERTIDTILL = befvar
   TIDFEL.PERSONALKOD = pkod
   TIDFEL.PRISTYP = "TOT.PRIS."
   TIDFEL.PROGRAM = "TIDINLULE" + STRING(TODAY,"99999999")
   TIDFEL.TIDLOG = TRUE
   TIDFEL.VECKOKORD = "w" + STRING(TODAY,"99999999")
   TIDFEL.VECKONUMMER = regvnr
   TIDFEL.DEBET = FALSE.

END PROCEDURE.
PROCEDURE tidskap_UI:

   CREATE TIDREGITAB.
   ASSIGN
   TIDREGITAB.ANVANDARE = "ASWEKO"
   TIDREGITAB.AONR = aonrvar
   TIDREGITAB.DAG = regdagnamn
   TIDREGITAB.DATUM = 03/31/2005 /*regdatum*/ 
   TIDREGITAB.DELNR = delnrvar
   TIDREGITAB.GODKAND = "G" + STRING(TODAY,"99999999")
   TIDREGITAB.OVERTIDTILL = befvar
   TIDREGITAB.PERSONALKOD = pkod
   TIDREGITAB.PRISTYP = "TOT.PRIS."
   TIDREGITAB.PROGRAM = "TIDINLULE" + STRING(TODAY,"99999999")
   TIDREGITAB.TIDLOG = TRUE
   TIDREGITAB.VECKOKORD = "w" + STRING(TODAY,"99999999")
   TIDREGITAB.VECKONUMMER = regvnr.

END PROCEDURE.

PROCEDURE kostreg_UI:
   
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUBSTRING(tidin.TIN,12,6) AND AONRTAB.DELNR = INTEGER(SUBSTRING(tidin.TIN,18,3)) 
   NO-LOCK NO-ERROR.
   
   IF NOT AVAILABLE AONRTAB THEN aonrvar = "XXXXXX".
   ELSE DO:
      ASSIGN
      aonrvar = AONRTAB.AONR
      delnrvar = AONRTAB.DELNR.
   END.
   
   regdatum = DATE(INTEGER(SUBSTRING(tidin.TIN,25,2)),INTEGER(SUBSTRING(tidin.TIN,27,2)),INTEGER(SUBSTRING(tidin.TIN,21,4))).
   IF SUBSTRING(tidin.TIN,1,1) = "x" OR SUBSTRING(tidin.TIN,1,1) = "x" THEN musz = musz.
   ELSE DO:
      FIND LAST KOSTREG WHERE KOSTREG.AONR = aonrvar AND KOSTREG.DELNR = delnrvar  NO-LOCK NO-ERROR.  
      rad = 1.                                                                      
      IF AVAILABLE KOSTREG THEN rad = KOSTREG.RADNR + 1.     
      
      DO TRANSACTION:
         CREATE KOSTREG.
         ASSIGN  
         KOSTREG.RADNR = rad
         KOSTREG.AONR = aonrvar
         KOSTREG.DELNR = delnrvar
         KOSTREG.REGDATUM = regdatum       
         KOSTREG.BETDATUM = TODAY
         KOSTREG.BENAMNING = TRIM(SUBSTRING(tidin.TIN,48,30))
         KOSTREG.BOKKONTO = SUBSTRING(tidin.TIN,1,4)
         KOSTREG.FAKTNR = SUBSTRING(tidin.TIN,89,11)
         KOSTREG.FAKTURERAD = ?
         KOSTREG.LEVKOD = ""
         SUBSTRING(KOSTREG.ANVANDARE,1,12) = "ASWEKO"
         KOSTREG.KOSTAUTO = TRUE. 
         IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "3" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,31,17)) * -1.
         END. 
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,1) = "7930" THEN DO:
            ASSIGN KOSTREG.INKOMST = DECIMAL(SUBSTRING(tidin.TIN,31,17)) * -1.
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) = "6510" THEN DO:
            ASSIGN KOSTREG.MASK = DECIMAL(SUBSTRING(tidin.TIN,31,17)).
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) = "6571" THEN DO:
            ASSIGN KOSTREG.MASK = DECIMAL(SUBSTRING(tidin.TIN,31,17)).
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) = "6572" THEN DO:
            ASSIGN KOSTREG.MASK = DECIMAL(SUBSTRING(tidin.TIN,31,17)).
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "7020" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "7390" THEN DO:
            ASSIGN KOSTREG.PERS = DECIMAL(SUBSTRING(tidin.TIN,31,17)).
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "9700" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "9712" THEN DO:
            ASSIGN KOSTREG.PERS = DECIMAL(SUBSTRING(tidin.TIN,31,17)).
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "9750" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "9759" THEN DO:
            ASSIGN KOSTREG.PERS = DECIMAL(SUBSTRING(tidin.TIN,31,17)).
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "4500" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "4899" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END. 
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "9040" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "9048" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "9460" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "9469" THEN DO:
            ASSIGN KOSTREG.MTRL = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "5000" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "5199" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "5300" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "6499" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "6520" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "6560" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "6580" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "6999" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) >= "7610" AND SUBSTRING(KOSTREG.BOKKONTO,1,4) <= "7981" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
         ELSE IF SUBSTRING(KOSTREG.BOKKONTO,1,4) = "9080" THEN DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
         ELSE DO:
            ASSIGN KOSTREG.OVRKR = DECIMAL(SUBSTRING(tidin.TIN,31,17)). 
         END.
                 
      END.
   END.
   
   
END PROCEDURE.
  
/* **********************  Internal Procedures  *********************** */

PROCEDURE noll_UI:
   ASSIGN    
   tidtim = 0
   otidtim = 0
   tid100 = 0
   otidtim1 = 0
   otidtim2 = 0
   otidtim3 = 0.   
END PROCEDURE.      


PROCEDURE sumtid_UI:
   
   OPEN QUERY tidq FOR EACH tidluletemp BY tidluletemp.AONR BY tidluletemp.DELNR BY tidluletemp.PERSONALKOD.   
   GET FIRST tidq NO-LOCK.        
   DO WHILE AVAILABLE(tidluletemp):
      ASSIGN
      regdatum = tidluletemp.DATUM
      kollvecka = tidluletemp.VECKOKORD.
      IF pkod NE tidluletemp.PERSONALKOD THEN DO:
         pkod = tidluletemp.PERSONALKOD.
         aonrtvar = "".
      END.
      IF aonrtvar = tidluletemp.AONR AND delnrtvar = tidluletemp.DELNR THEN musz = musz.
      ELSE DO:
         ASSIGN
         aonrtvar = tidluletemp.AONR 
         delnrtvar = tidluletemp.DELNR.
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = tidluletemp.AONR AND 
         AONRTAB.DELNR = tidluletemp.DELNR
         NO-LOCK NO-ERROR.         
      END.
      RUN noll_UI.       
      IF tidluletemp.OKOD1 = " " AND tidluletemp.OKOD2 = " " AND tidluletemp.OKOD3 = " " THEN DO:   
         tidtim = klockan100(tidluletemp.TOTALT).
      END.
      ELSE DO:
         otidtim = klockan100(tidluletemp.TOTALT).        
      END.   
      otidtim1 = klockan100(tidluletemp.OANT1).
      otidtim2 = klockan100(tidluletemp.OANT2).
      otidtim3 = klockan100(tidluletemp.OANT3).
      CREATE ekoforst.
      ASSIGN
      ekoforst.BEFATTNING = tidluletemp.OVERTIDTILL
      ekoforst.OMRADE = omrpers
      ekoforst.PERSONALKOD = tidluletemp.PERSONALKOD 
      ekoforst.AONR = tidluletemp.AONR
      ekoforst.DELNR = tidluletemp.DELNR                  
      ekoforst.PRIS = tidluletemp.PRIS 
      ekoforst.DATUM = tidluletemp.DATUM          
      ekoforst.PRISTYP = tidluletemp.PRISTYP  
      ekoforst.BELOPP = tidluletemp.PRIS
      ekoforst.OBELOPP = tidluletemp.PRIS 
      ekoforst.ANTAL = tidtim  
      ekoforst.OTIMMAR = otidtim      
      ekoforst.OANT1 = otidtim1    
      ekoforst.OANT2 = otidtim2    
      ekoforst.OANT3 = otidtim3    
      ekoforst.OKOD1 = tidluletemp.OKOD1 
      ekoforst.OKOD2 = tidluletemp.OKOD2 
      ekoforst.OKOD3 = tidluletemp.OKOD3                                
      ekoforst.TRAKTKOD = tidluletemp.TRAKTKOD 
      ekoforst.TRAKTANTAL = tidluletemp.TRAKTANTAL
      ekoforst.TRAAVTAL = traav.              
      IF tidluletemp.OKOD1 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
      IF tidluletemp.OKOD2 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
      IF tidluletemp.OKOD3 NE " " THEN ASSIGN ekoforst.BELOPP = 0.
      ASSIGN ekoforst.BELOPP = ekoforst.ANTAL * ekoforst.BELOPP. 
                                      
      
 
      GET NEXT tidq NO-LOCK.
 
   END. 
   
   RUN sumeraallt_UI.           
    
END PROCEDURE.

PROCEDURE sumeraallt_UI:
   ASSIGN   
   pkod = " ".     
   FOR EACH ekoforst BREAK BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.BEFATTNING BY ekoforst.AONR BY 
   ekoforst.DELNR BY ekoforst.PRISTYP:
      IF pkod NE ekoforst.PERSONALKOD THEN DO:
         pkod = ekoforst.PERSONALKOD.       
      END.
      IF ekoforst.OANT1 NE 0  THEN DO:      
         CREATE overtidhj.
         ASSIGN 
         overtidhj.BEFATTNING = ekoforst.BEFATTNING
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP
         overtidhj.DATUM = ekoforst.DATUM 
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT1 
         overtidhj.OVERANTAL = ekoforst.OANT1 
         overtidhj.OVERTIDTILL = ekoforst.OKOD1.                                  
      END. 
      IF ekoforst.OANT2 NE 0  THEN DO:      
         CREATE overtidhj.      
         ASSIGN
         overtidhj.BEFATTNING = ekoforst.BEFATTNING
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP 
         overtidhj.DATUM = ekoforst.DATUM
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT2   
         overtidhj.OVERANTAL = ekoforst.OANT2
         overtidhj.OVERTIDTILL = ekoforst.OKOD2.                                      
      END.                    
      IF ekoforst.OANT3 NE 0  THEN DO:      
         CREATE overtidhj.           
         ASSIGN 
         overtidhj.BEFATTNING = ekoforst.BEFATTNING
         overtidhj.PERSONALKOD = ekoforst.PERSONALKOD 
         overtidhj.PRIS = ekoforst.PRIS     
         overtidhj.PRISTYP = ekoforst.PRISTYP
         overtidhj.DATUM = ekoforst.DATUM 
         overtidhj.AONR = ekoforst.AONR 
         overtidhj.DELNR = ekoforst.DELNR 
         overtidhj.BELOPP = ekoforst.OBELOPP * ekoforst.OANT3
         overtidhj.OVERANTAL = ekoforst.OANT3
         overtidhj.OVERTIDTILL = ekoforst.OKOD3. 
         
      END.                                                             
      ACCUMULATE ekoforst.BELOPP 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.BEFATTNING BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP). 
      ACCUMULATE ekoforst.ANTAL 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.BEFATTNING BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP).  
      ACCUMULATE ekoforst.OTIMMAR 
      (TOTAL BY ekoforst.DATUM BY ekoforst.PERSONALKOD BY ekoforst.BEFATTNING BY ekoforst.AONR BY 
      ekoforst.DELNR BY ekoforst.PRISTYP).     
      IF LAST-OF(ekoforst.PRISTYP) THEN DO:
         CREATE slutsum.
         ASSIGN 
         slutsum.BEFATTNING = ekoforst.BEFATTNING
         slutsum.OMRADE = ekoforst.OMRADE 
         slutsum.PERSONALKOD = ekoforst.PERSONALKOD 
         slutsum.PRIS = ekoforst.PRIS     
         slutsum.PRISTYP = ekoforst.PRISTYP 
         slutsum.DATUM = ekoforst.DATUM
         slutsum.ARTAL = YEAR(ekoforst.DATUM) 
         slutsum.AONR = ekoforst.AONR
         slutsum.DELNR = ekoforst.DELNR         
         slutsum.BELOPP = (ACCUM TOTAL BY ekoforst.PRISTYP ekoforst.BELOPP)
         slutsum.ANTAL = (ACCUM TOTAL BY ekoforst.PRISTYP ekoforst.ANTAL)  
         slutsum.OTIMMAR = (ACCUM TOTAL BY ekoforst.PRISTYP ekoforst.OTIMMAR).           
      END. 
   END.
   FOR EACH overtidhj BREAK BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY overtidhj.BEFATTNING BY
   overtidhj.AONR BY overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP:   
      ACCUMULATE overtidhj.BELOPP 
      (TOTAL BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY overtidhj.BEFATTNING BY overtidhj.AONR BY 
      overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP). 
      ACCUMULATE overtidhj.OVERANTAL 
      (TOTAL BY overtidhj.DATUM BY overtidhj.PERSONALKOD BY overtidhj.BEFATTNING BY overtidhj.AONR BY
      overtidhj.DELNR BY overtidhj.OVERTIDTILL BY overtidhj.PRISTYP).
      IF LAST-OF(overtidhj.PRISTYP) THEN DO:
         CREATE oversum.
         ASSIGN         
         oversum.BEFATTNING = overtidhj.BEFATTNING
         oversum.PERSONALKOD = overtidhj.PERSONALKOD 
         oversum.PRIS = overtidhj.PRIS     
         oversum.PRISTYP = overtidhj.PRISTYP
         oversum.DATUM = overtidhj.DATUM 
         oversum.AONR = overtidhj.AONR
         oversum.DELNR = overtidhj.DELNR 
         oversum.OVERTIDTILL = overtidhj.OVERTIDTILL
         oversum.BELOPP = (ACCUM TOTAL BY overtidhj.PRISTYP overtidhj.BELOPP)
         oversum.ANTAL = (ACCUM TOTAL BY overtidhj.PRISTYP overtidhj.OVERANTAL).                                   
      END.   
   END.         
   FOR EACH oversum BREAK BY oversum.DATUM BY oversum.PERSONALKOD BY oversum.BEFATTNING BY oversum.AONR BY  
   oversum.DELNR BY oversum.PRISTYP:           
      ACCUMULATE oversum.BELOPP (TOTAL BY oversum.DATUM BY oversum.PERSONALKOD BY oversum.BEFATTNING BY
      oversum.AONR BY oversum.DELNR BY oversum.PRISTYP). 
      ACCUMULATE oversum.ANTAL (TOTAL BY oversum.DATUM BY oversum.PERSONALKOD  BY oversum.BEFATTNING BY 
      oversum.AONR BY oversum.DELNR BY oversum.PRISTYP).
      IF LAST-OF(oversum.PRISTYP) THEN DO:
         FIND FIRST slutsum WHERE slutsum.DATUM = oversum.DATUM AND slutsum.PERSONALKOD = oversum.PERSONALKOD 
         AND slutsum.AONR = oversum.AONR AND slutsum.DELNR = oversum.DELNR AND
         slutsum.PRISTYP = oversum.PRISTYP AND slutsum.BEFATTNING = oversum.BEFATTNING
         USE-INDEX AONR NO-ERROR.
         IF AVAILABLE slutsum THEN DO:
            ASSIGN slutsum.OBELOPP = (ACCUM TOTAL BY oversum.PRISTYP oversum.BELOPP )
            slutsum.OANTAL = (ACCUM TOTAL BY oversum.PRISTYP oversum.ANTAL).                 
         END.
      END.  
   END. 
            
   FOR EACH slutsum:     
      CREATE SUMTIDDAG.
      ASSIGN     
      SUMTIDDAG.BEFATTNING = slutsum.BEFATTNING
      SUMTIDDAG.OMRADE = slutsum.OMRADE
      SUMTIDDAG.VECKOKORD = kollvecka 
      SUMTIDDAG.PERSONALKOD = slutsum.PERSONALKOD 
      SUMTIDDAG.PRIS = slutsum.PRIS     
      SUMTIDDAG.DATUM = slutsum.DATUM
      SUMTIDDAG.AUTODATUM = TODAY
      SUMTIDDAG.AONR = slutsum.AONR   
      SUMTIDDAG.DELNR = slutsum.DELNR
      SUMTIDDAG.PRISTYP = slutsum.PRISTYP 
      SUMTIDDAG.TIMMAR = slutsum.ANTAL 
      SUMTIDDAG.OTIMMAR = slutsum.OTIMMAR  
      SUMTIDDAG.BELOPP = slutsum.BELOPP
      SUMTIDDAG.OBELOPP = slutsum.OBELOPP
      SUMTIDDAG.TBELOPP = slutsum.TBELOPP
      SUMTIDDAG.IKOSTNAD = slutsum.BELOPP.             
   END.                                  
      
END PROCEDURE.


PROCEDURE namn_UI: 
   RELEASE AONRTAB NO-ERROR.                  
   ASSIGN
   nattaonr = ""
   nattdelnr = 0.
   OPEN QUERY sumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.VECKOKORD = kollvecka NO-LOCK
   BY SUMTIDDAG.VECKOKORD BY SUMTIDDAG.AONR by SUMTIDDAG.DELNR.    
   GET FIRST sumq NO-LOCK.
   DO WHILE AVAILABLE(SUMTIDDAG):
      DO TRANSACTION:   
         GET CURRENT sumq EXCLUSIVE-LOCK.
         IF NOT AVAILABLE AONRTAB THEN DO:
            ASSIGN  
            nattaonr = SUMTIDDAG.AONR 
            nattdelnr = SUMTIDDAG.DELNR. 
            FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUMTIDDAG.AONR AND 
            AONRTAB.DELNR = SUMTIDDAG.DELNR
            USE-INDEX AONR NO-LOCK NO-ERROR. 
         END.            
         ELSE DO:                                                          
            IF nattaonr = SUMTIDDAG.AONR AND nattdelnr = SUMTIDDAG.DELNR THEN nattaonr = nattaonr.
            ELSE DO:
               ASSIGN  
               nattaonr = SUMTIDDAG.AONR 
               nattdelnr = SUMTIDDAG.DELNR. 
               FIND FIRST AONRTAB WHERE AONRTAB.AONR = SUMTIDDAG.AONR AND 
               AONRTAB.DELNR = SUMTIDDAG.DELNR
               USE-INDEX AONR NO-LOCK NO-ERROR. 
            END.
         END.    
         IF AVAILABLE AONRTAB THEN DO: 
            ASSIGN 
            SUMTIDDAG.FASTAAONR = AONRTAB.FASTAAONR          
            SUMTIDDAG.GEOMRADE = AONRTAB.OMRADE 
            SUMTIDDAG.ORT = AONRTAB.ORT.
            IF AONRTAB.OMRADE = "" THEN ASSIGN SUMTIDDAG.GEOMRADE = SUMTIDDAG.OMRADE.           
         END.        
      END.
      GET NEXT sumq NO-LOCK.
   END.         
   CLOSE QUERY sumq.                        
   pkod = " ".
   OPEN QUERY sumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.VECKOKORD = kollvecka NO-LOCK 
   BY SUMTIDDAG.VECKOKORD BY SUMTIDDAG.PERSONALKOD.  
   GET FIRST sumq NO-LOCK.
   DO WHILE AVAILABLE(SUMTIDDAG):
      DO TRANSACTION:   
         GET CURRENT sumq EXCLUSIVE-LOCK.
         RUN satt_UI.          
      END. 
      GET NEXT sumq NO-LOCK.
   END.          
END PROCEDURE.
PROCEDURE satt_UI:
   IF pkod NE SUMTIDDAG.PERSONALKOD THEN DO:
      pkod = SUMTIDDAG.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE 
      PERSONALTAB.PERSONALKOD = SUMTIDDAG.PERSONALKOD
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.   
      IF AVAILABLE PERSONALTAB THEN DO:                          
         FIND FIRST ANSTFORMTAB WHERE 
         ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
         USE-INDEX ANSTF NO-LOCK NO-ERROR.         
         IF NOT AVAILABLE ANSTFORMTAB THEN DO:
            FIND FIRST ANSTFORMTAB USE-INDEX ANSTF NO-LOCK NO-ERROR.           
         END.
      END.
      ELSE DO:
         FIND FIRST ANSTFORMTAB USE-INDEX ANSTF NO-LOCK NO-ERROR.         
      END.
   END.
   IF AVAILABLE PERSONALTAB THEN DO:                          
      IF SUMTIDDAG.GEOMRADE = "" THEN SUMTIDDAG.GEOMRADE = PERSONALTAB.OMRADE.
      ASSIGN 
      SUMTIDDAG.FORNAMN = PERSONALTAB.FORNAMN 
      SUMTIDDAG.EFTERNAMN = PERSONALTAB.EFTERNAMN
      SUMTIDDAG.OMRADE = PERSONALTAB.OMRADE.
      befvar = SUBSTRING(SUMTIDDAG.BEFATTNING,1,20).
      SUMTIDDAG.BEFATTNING = "".
      IF befvar = "" THEN SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = PERSONALTAB.BEFATTNING .
      ELSE SUBSTRING(SUMTIDDAG.BEFATTNING,1,20) = befvar.
      ASSIGN
      SUBSTRING(SUMTIDDAG.BEFATTNING,21) = ANSTFORMTAB.KOD         
      SUMTIDDAG.PERSMASK = PERSONALTAB.PERSMASK.       
      ASSIGN SUMTIDDAG.PRISI = SUMTIDDAG.PRIS.      
   END.
END PROCEDURE.   
				
