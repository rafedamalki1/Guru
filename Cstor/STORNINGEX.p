/*STORNINGEX.P UTLÄSNING AV MTRL KOPPLAT TILL KONSTRUKTIONER TILL EXCEL*/ 
DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
{GLOBVAR2DEL1.I}
&Scoped-define NEW 
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE chPageBreak        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE chCell             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE cActiveCell        AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.
DEFINE VARIABLE slutsumvar              AS DECIMAL.
DEFINE VARIABLE link AS CHARACTER NO-UNDO.
DEFINE VARIABLE kundtimtot             AS DECIMAL NO-UNDO.
DEFINE VARIABLE hsptot                 AS DECIMAL NO-UNDO.
DEFINE VARIABLE lsptot                 AS DECIMAL NO-UNDO.
DEFINE VARIABLE franfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE radnrS                  AS CHARACTER.
DEFINE VARIABLE cRangefont         AS CHARACTER  NO-UNDO.



DEFINE SHARED VARIABLE vallista AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE alla AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE alla2 AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE allaspann AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE forvar AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE SHARED VARIABLE regvnr AS INTEGER FORMAT "999" NO-UNDO.
DEFINE SHARED VARIABLE regdagnamn AS CHARACTER FORMAT "X(3)" NO-UNDO.        
DEFINE SHARED VARIABLE regdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE bdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE avdatum AS DATE NO-UNDO.
DEFINE SHARED VARIABLE uttyp AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE period AS INTEGER NO-UNDO.
DEFINE SHARED VARIABLE valfore AS LOGICAL NO-UNDO.
DEFINE VARIABLE antalomr AS INTEGER NO-UNDO.
DEFINE VARIABLE timmar100 AS DECIMAL NO-UNDO.
DEFINE VARIABLE timmar60 AS DECIMAL NO-UNDO.
DEFINE VARIABLE distvar AS INTEGER NO-UNDO.
DEFINE VARIABLE driftvar AS INTEGER NO-UNDO.
DEFINE VARIABLE spannvar AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(92)" NO-UNDO.
DEFINE VARIABLE endsum AS LOGICAL NO-UNDO.
DEFINE VARIABLE kant AS INTEGER NO-UNDO.
DEFINE VARIABLE tant AS INTEGER NO-UNDO.
DEFINE VARIABLE utrec AS RECID NO-UNDO.
DEFINE VARIABLE utrec2 AS RECID NO-UNDO.
DEFINE VARIABLE sidlangd AS INTEGER NO-UNDO.
DEFINE VARIABLE omrrec AS RECID NO-UNDO.
DEFINE VARIABLE formatvar AS DECIMAL NO-UNDO.

DEFINE TEMP-TABLE etut NO-UNDO
      FIELD A AS CHARACTER 
      FIELD B AS CHARACTER
      FIELD C AS CHARACTER
      FIELD D AS CHARACTER
      FIELD E AS CHARACTER
      FIELD F AS CHARACTER
      FIELD G AS CHARACTER
      FIELD H AS CHARACTER
      FIELD I AS CHARACTER
      FIELD J AS CHARACTER
      FIELD K AS CHARACTER
      FIELD L AS CHARACTER
      FIELD M AS CHARACTER
      FIELD N AS CHARACTER
      FIELD O AS CHARACTER.
&SCOPED-DEFINE NEW
&SCOPED-DEFINE SHARED SHARED 
{STORTEMP.I}
{AVDELNINGTEMP.I}
{STRTEMP.I}

DEFINE TEMP-TABLE extemp NO-UNDO LIKE urstorntemp.

{TIDUTTTNEW.I}
   
DEFINE SHARED TEMP-TABLE omr_temp
   FIELD AVDELNINGNR AS INTEGER
   FIELD DISTRIKTID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX OMR IS PRIMARY AVDELNINGNR DISTRIKTID.

DEFINE SHARED TEMP-TABLE avd_temp
   FIELD AVDELNINGNR AS INTEGER   
   FIELD NAMN AS CHARACTER
   INDEX AVD IS PRIMARY AVDELNINGNR.
   
DEFINE SHARED TEMP-TABLE spann_temp2    
   FIELD SPANID AS INTEGER
   FIELD NAMN AS CHARACTER
   INDEX SPAN SPANID. 
/*ger timmar och delar av timmar*/
FUNCTION klockan100 RETURNS DECIMAL
  (INPUT ber60 AS DECIMAL) :
  RETURN  (TRUNCATE(ber60,0) * 3600 + (ber60 - TRUNCATE(ber60,0)) * 100 * 60) / 3600. 
  
END FUNCTION.
/*ger timmar och minuter*/
FUNCTION klockan60 RETURNS DECIMAL
  (INPUT ber100 AS DECIMAL) :
  RETURN ROUND(TRUNCATE(ber100,0) + ((ber100 - TRUNCATE(ber100,0)) / 100) * 60,2).
  
END FUNCTION.
{AMERICANEUROPEAN.I}
FOR EACH extemp:
   DELETE extemp.
END.
IF valfore = TRUE THEN DO:
   FOR EACH spann_temp2 USE-INDEX SPAN:         
      ASSIGN
      spannvar = spann_temp2.SPANID.         
      RUN storning_UI.            
   END.
END.
ELSE DO:
   IF alla = TRUE THEN DO: 
      FOR EACH spann_temp2 USE-INDEX SPAN:         
         ASSIGN
         spannvar = spann_temp2.SPANID.         
         RUN storning_UI.            
      END.
   END.
   ELSE DO:
      IF alla2 = TRUE THEN DO:
         FOR EACH spann_temp2:               
            ASSIGN
            spannvar = spann_temp2.SPANID.               
            OPEN QUERY kq FOR EACH stordistemp WHERE stordistemp.AVDELNINGNR = forvar
            AND stordistemp.ARTAL = YEAR(bdatum) USE-INDEX AVDARTAL NO-LOCK.
            GET FIRST kq NO-LOCK.
            DO WHILE AVAILABLE(stordistemp):
               ASSIGN
               distvar = stordistemp.DISTRIKTID.       
               RUN storning_UI.                           
               GET NEXT kq NO-LOCK.
            END.
            CLOSE QUERY kq.
         END.
      END.
      ELSE DO:         
         FOR EACH spann_temp2:               
            ASSIGN
            spannvar = spann_temp2.SPANID.               
            FOR EACH omr_temp USE-INDEX OMR NO-LOCK:
               ASSIGN
               distvar = omr_temp.DISTRIKTID.       
               RUN storning_UI.                           
            END.
         END.
      END.
   END.
END.

IF vad = 1 THEN RUN eut_UI.
ELSE IF vad = 2 THEN RUN etut_UI.
{EUROPEANAMERICAN.I}
PROCEDURE excelproc_UI :
   DEFINE VARIABLE utrak AS INTEGER NO-UNDO.
   FIND FIRST etut NO-LOCK NO-ERROR.
   IF NOT AVAILABLE etut THEN DO:
      MESSAGE "Inga störningar att visa!" VIEW-AS ALERT-BOX.
      RETURN.
   END.
   REPEAT:     
      utrak = 0.
      FIND FIRST etut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE etut THEN do:
         OUTPUT CLOSE.
         RUN open_UI (INPUT franfil).      
         {EXCELFEL.I}
         RUN slutexcel_UI.
         {EXCELFEL.I}
         LEAVE.
      END.
      franfil = SESSION:TEMP-DIR + STRING(TIME) + ".txt". 
      OUTPUT TO VALUE(franfil).   
      START:
      FOR EACH etut:
         PUT UNFORMATTED
         etut.A CHR(9)
         etut.B CHR(9)
         etut.C CHR(9)
         etut.D CHR(9)
         etut.E CHR(9)
         etut.F CHR(9)
         etut.G CHR(9)
         etut.H CHR(9)
         etut.I CHR(9)
         etut.J CHR(9)
         etut.K CHR(9)
         etut.L CHR(9)
         etut.M CHR(9)
         etut.N CHR(9)
         etut.O CHR(9) SKIP.        
         utrak = utrak + 1.
         DELETE etut.
         /*
         IF utrak > 65000 THEN DO:
            OUTPUT CLOSE.
            RUN open_UI (INPUT franfil).      
            {EXCELFEL.I}
            RUN slutexcel_UI.
            {EXCELFEL.I}
            LEAVE START.
         END.
         */
      END.      
   END.
   
   
END PROCEDURE.
PROCEDURE etut_UI :
   
     
   CREATE etut.
   CREATE etut.
   ASSIGN
   etut.A = "Från"
   etut.B = STRING(bdatum,"9999/99/99").
   CREATE etut.
   ASSIGN
   etut.A = "Till"
   etut.B = STRING(avdatum,"9999/99/99").
   CREATE etut.
   ASSIGN
   etut.A = "Distrikt"
   etut.B = "Typ"
   etut.C = "Startdatum"
   etut.D = "Starttid".
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      
      /*
      chWorkSheet:Range("D:D"):NumberFormat = "@".
      */
   END.
   ELSE DO:
   END.
   ASSIGN
   etut.E = "Slutdatum"
   etut.F = "Sluttid".
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      /*
      chWorkSheet:Range("F:F"):NumberFormat = "@".
      */
   END.
   ELSE DO:      
   END.
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      etut.G = "Kundtim".   
   END.
   ELSE DO:
      etut.G = "Avbrottstid".   
   END.
   ASSIGN
   etut.H = "Hsp-ab"
   etut.I = "Lsp-ab".
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      ASSIGN
      etut.J = "Anl-del"
      etut.K = "Belägenhet" 
      etut.L = "Anl-del"
      etut.M = "Felorsak".       
   END.
   ELSE DO:
      ASSIGN
      etut.J = "Frånkopp.ställe"
      etut.K = "Felställe"
      etut.L = "Belägenhet" 
      etut.M = "Anl-del" 
      etut.N = "Felorsak". 
      IF Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "SEKG" THEN DO:
         etut.O = "Företag". 
      END.
   END.
   
   ASSIGN
   kundtimtot = 0
   hsptot = 0
   lsptot = 0.
   /*
   chWorkSheet:Range("A:A"):NumberFormat = "@".
   */
   DEFINE VARIABLE AA AS INTEGER NO-UNDO.
   FOR EACH extemp USE-INDEX STORNUMMERID:
      AA = AA + 1.      
      CREATE etut.
      FIND FIRST stordistemp WHERE stordistemp.DISTRIKTID = extemp.distriktid NO-LOCK NO-ERROR.
      IF AVAILABLE stordistemp THEN DO:
         IF Guru.Konstanter:globforetag = "MALU" THEN DO:
            etut.A = STRING(stordistemp.NAMN).
         END.
         ELSE DO:
            etut.A = STRING(stordistemp.VIDISTRIKT).
         END.
      END.
      ELSE etut.A = "".
      IF extemp.STORTYPID = 1 THEN etut.B = "Driftstörning".
      ELSE etut.B = "Planerat avbrott".
      ASSIGN
      etut.C = STRING(extemp.HDATUM,"9999/99/99")
      etut.D = STRING(extemp.HKLOCKAN,"99.99")
      etut.E = STRING(extemp.DATUM100%,"9999/99/99")
      etut.F = STRING(extemp.KLOCKAN100%,"99.99").
      etut.D = REPLACE(etut.D,".",",").
      etut.F= REPLACE(etut.F,".",",").
      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         etut.G = STRING(INTEGER(klockan60(extemp.AVBROTTSTID))).               
         kundtimtot = kundtimtot + DECIMAL(klockan60(extemp.AVBROTTSTID)).         
      END.
      ELSE DO:
         etut.G = REPLACE(STRING(DECIMAL(klockan60(extemp.AVBROTTSTID))),".",",").           
      END.

      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         etut.H = STRING(extemp.ANTALHSP).  
         hsptot = hsptot + DECIMAL(STRING(extemp.ANTALHSP)).
      END.
      ELSE DO:
         etut.H = STRING(extemp.ANTALHSP).               
      END.

      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         etut.I = STRING(extemp.ANTALLSP).               
         lsptot = lsptot + DECIMAL(STRING(extemp.ANTALLSP)).
      END.
      ELSE DO:
         etut.I = STRING(extemp.ANTALLSP).               
      END.

      FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = extemp.FRANSPANID
      NO-LOCK NO-ERROR.      
      IF AVAILABLE spanningsnivtemp THEN etut.J = STRING(spanningsnivtemp.NAMN).
      ELSE etut.J = "".
      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = extemp.STDRIFTID
         NO-LOCK NO-ERROR.
         IF AVAILABLE stordriftomrtemp THEN etut.K = STRING(stordriftomrtemp.NAMN).
         ELSE etut.K = "".
         FIND FIRST anlaggningsdeltemp WHERE anlaggningsdeltemp.ADELID = extemp.ADELID
         NO-LOCK NO-ERROR.
         IF AVAILABLE anlaggningsdeltemp THEN etut.L = STRING(anlaggningsdeltemp.NAMN).
         ELSE etut.L = "".
         IF extemp.STORTYPID = 1 THEN DO:
            FIND FIRST felorsaktemp WHERE felorsaktemp.FELOID = extemp.FELOID
            NO-LOCK NO-ERROR.
            IF AVAILABLE felorsaktemp THEN etut.M = STRING(felorsaktemp.NAMN).
            ELSE etut.M = "".
         END.
      END.
      ELSE DO:
         IF extemp.STORTYPID = 1 THEN DO:
            FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = extemp.FELSPANID
            NO-LOCK NO-ERROR.
            IF AVAILABLE spanningsnivtemp THEN etut.K = STRING(spanningsnivtemp.NAMN).
            ELSE etut.K = "".
         END.
         FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = extemp.STDRIFTID
         NO-LOCK NO-ERROR.
         IF AVAILABLE stordriftomrtemp THEN etut.L = STRING(stordriftomrtemp.NAMN).
         ELSE etut.L = "".
         FIND FIRST anlaggningsdeltemp WHERE anlaggningsdeltemp.ADELID = extemp.ADELID
         NO-LOCK NO-ERROR.
         IF AVAILABLE anlaggningsdeltemp THEN etut.M = STRING(anlaggningsdeltemp.NAMN).
         ELSE etut.M = "".
         IF extemp.STORTYPID = 1 THEN DO:
            FIND FIRST felorsaktemp WHERE felorsaktemp.FELOID = extemp.FELOID
            NO-LOCK NO-ERROR.
            IF AVAILABLE felorsaktemp THEN etut.N = STRING(felorsaktemp.NAMN).
            ELSE etut.N = "".
         END.
         IF Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "SEKG" THEN DO:
            IF AVAILABLE stordistemp THEN etut.O = STRING(stordistemp.AVDELNINGNR).
            ELSE etut.O = "".
         END.
      END.
   END.  
   
   /*END FOR EACH*/
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      CREATE etut.
      
      IF kundtimtot NE 0 THEN DO:
         etut.G = STRING(INTEGER(kundtimtot)).
      END.
      IF hsptot NE 0 THEN DO:
         etut.H = STRING(hsptot).
      END.
      IF lsptot NE 0 THEN DO:
         etut.I = STRING(lsptot).
      END.                                             
   END.
   RUN excelproc_UI.
   {EXCELFEL.I}
END PROCEDURE.

PROCEDURE eut_UI :
   
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE NO-ERROR.
   chWorkbook = chExcelApplication:Workbooks:Add() NO-ERROR.
   chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
   chWorkSheet:pagesetup:ORIENTATION = 2 NO-ERROR.  /*2 = landscapemode*/
   chWorkSheet:pagesetup:PrintGridlines = True NO-ERROR.


   chWorkSheet:Range("A:A"):Font:NAME = "Arial" NO-ERROR.   
   chWorkSheet:Range("A:A"):Font:SIZE = 8 NO-ERROR.  
   chWorkSheet:Range("B:B"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("B:B"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("C:C"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("C:C"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("D:D"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("D:D"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("E:E"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("E:E"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("F:F"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("F:F"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("G:G"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("G:G"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("H:H"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("H:H"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("I:I"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("I:I"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("J:J"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("J:J"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("K:K"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("K:K"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("L:L"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("L:L"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("M:M"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("M:M"):Font:SIZE = 8 NO-ERROR.
   chWorkSheet:Range("N:N"):Font:NAME = "Arial" NO-ERROR.
   chWorkSheet:Range("N:N"):Font:SIZE = 8 NO-ERROR.

   chWorkSheet:Columns("A"):ColumnWidth = 8 NO-ERROR.
   chWorkSheet:Columns("B"):ColumnWidth = 8 NO-ERROR.
   chWorkSheet:Columns("C"):ColumnWidth = 8.3 NO-ERROR.
   chWorkSheet:Columns("D"):ColumnWidth = 5.5 NO-ERROR.
   chWorkSheet:Columns("E"):ColumnWidth = 8 NO-ERROR.
   chWorkSheet:Columns("F"):ColumnWidth = 5 NO-ERROR.
   chWorkSheet:Columns("G"):ColumnWidth = 7 NO-ERROR.
   chWorkSheet:Columns("H"):ColumnWidth = 6 NO-ERROR.
   chWorkSheet:Columns("I"):ColumnWidth = 6 NO-ERROR.
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      chWorkSheet:Columns("J"):ColumnWidth = 14.5 NO-ERROR.
      chWorkSheet:Columns("K"):ColumnWidth = 10 NO-ERROR.
      chWorkSheet:Columns("L"):ColumnWidth = 17 NO-ERROR.      
      chWorkSheet:Columns("M"):ColumnWidth = 16 NO-ERROR.
   END.
   ELSE DO:
      chWorkSheet:Columns("J"):ColumnWidth = 14 NO-ERROR.
      chWorkSheet:Columns("K"):ColumnWidth = 10 NO-ERROR.
      chWorkSheet:Columns("L"):ColumnWidth = 13 NO-ERROR.
      chWorkSheet:Columns("M"):ColumnWidth = 13 NO-ERROR.
      chWorkSheet:Columns("N"):ColumnWidth = 11 NO-ERROR.    
      IF Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "SEKG" THEN DO:
         chWorkSheet:Columns("O"):ColumnWidth = 10 NO-ERROR.
      END.
   END.
      
   ASSIGN
   iColumn = iColumn + 2
   cColumn = STRING(iColumn)
   cRange = "A" + cColumn   
   chWorkSheet:Range(cRange):Value = "Från" NO-ERROR.
   cRange = "B" + cColumn.   
   chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
   chWorkSheet:Range(cRange):Value = STRING(bdatum,"9999/99/99") NO-ERROR.
   ASSIGN
   iColumn = iColumn + 1
   cColumn = STRING(iColumn)
   cRange = "A" + cColumn   
   chWorkSheet:Range(cRange):Value = "Till" NO-ERROR.
   cRange = "B" + cColumn.   
   chWorkSheet:Range(cRange):NumberFormat = "@" NO-ERROR.
   chWorkSheet:Range(cRange):Value = STRING(avdatum,"9999/99/99") NO-ERROR.   
   ASSIGN
   iColumn = iColumn + 1.
   cRange = "A" + STRING(iColumn).
   IF Guru.Konstanter:globforetag = "MALU" THEN
   radnrS = "M" + STRING(iColumn).
   ELSE radnrS = "N" + STRING(iColumn).
   cRangefont = cRange + ":" + radnrS.
   chWorkSheet:Range(cRangefont):Borders(4):Weight = 2 NO-ERROR.
   ASSIGN
   iColumn = iColumn + 1
   cColumn = STRING(iColumn)
   cRange = "A" + cColumn
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "Distrikt" NO-ERROR.
   cRange = "B" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "Typ" NO-ERROR.                                           
   cRange = "C" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "Startdatum" NO-ERROR.
   chWorkSheet:Range("C:C"):NumberFormat = "@" NO-ERROR.
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Starttid" NO-ERROR.
      chWorkSheet:Range("D:D"):NumberFormat = "@" NO-ERROR.
   END.
   ELSE DO:
      cRange = "D" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Starttid" NO-ERROR.
   END.
   cRange = "E" + cColumn.
   chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
   chWorkSheet:Range(cRange):Value = "Slutdatum" NO-ERROR.   
   chWorkSheet:Range("E:E"):NumberFormat = "@" NO-ERROR.
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      cRange = "F" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Sluttid" NO-ERROR.
      chWorkSheet:Range("F:F"):NumberFormat = "@" NO-ERROR.
   END.
   ELSE DO:
      cRange = "F" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Sluttid" NO-ERROR.
   END.
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      cRange = "G" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Kundtim" NO-ERROR.   
   END.
   ELSE DO:
      cRange = "G" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Avbrottstid" NO-ERROR.   
   END.   
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      cRange = "H" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Hsp-ab" NO-ERROR. 
      cRange = "I" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Lsp-ab " NO-ERROR.
      cRange = "J" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Frånkoppl-ställe" NO-ERROR.
      cRange = "K" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Belägenhet"  NO-ERROR. 
      cRange = "L" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Anl-del" NO-ERROR. 
      cRange = "M" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Felorsak" NO-ERROR.       
   END.
   ELSE DO:
      cRange = "H" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Antal HSP" NO-ERROR. 
      cRange = "I" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Antal LSP" NO-ERROR.
      cRange = "J" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Frånkopp.ställe" NO-ERROR.
      cRange = "K" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Felställe" NO-ERROR.
      cRange = "L" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Belägenhet" NO-ERROR. 
      cRange = "M" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Anläggning" NO-ERROR. 
      cRange = "N" + cColumn.
      chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
      chWorkSheet:Range(cRange):Value = "Felorsak" NO-ERROR. 
      IF Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "SEKG" THEN DO:
         cRange = "O" + cColumn.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = "Företag" NO-ERROR. 
      END.
   END.

   cRange = "A" + STRING(iColumn).
   IF Guru.Konstanter:globforetag = "MALU" THEN
   radnrS = "M" + STRING(iColumn).
   ELSE radnrS = "N" + STRING(iColumn).
   cRangefont = cRange + ":" + radnrS.
   chWorkSheet:Range(cRangefont):Borders(4):Weight = 2 NO-ERROR.

   ASSIGN
   kundtimtot = 0
   hsptot = 0
   lsptot = 0.
   chWorkSheet:Range("A:A"):NumberFormat = "@" NO-ERROR.
   {EXCELFEL.I}
   FOR EACH extemp USE-INDEX STORNUMMERID:      
      {EXCELFEL.I}
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      FIND FIRST stordistemp WHERE stordistemp.DISTRIKTID = extemp.distriktid NO-LOCK NO-ERROR.
      cRange = "A" + cColumn.  
      IF AVAILABLE stordistemp THEN DO:
         IF Guru.Konstanter:globforetag = "MALU" THEN DO:
            chWorkSheet:Range(cRange):Value = STRING(stordistemp.NAMN) NO-ERROR.
         END.
         ELSE DO:
            chWorkSheet:Range(cRange):Value = STRING(stordistemp.VIDISTRIKT) NO-ERROR.
         END.
      END.
      ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.
      cRange = "B" + cColumn.
      IF extemp.STORTYPID = 1 THEN               
      chWorkSheet:Range(cRange):Value = "Driftst." NO-ERROR.
      ELSE chWorkSheet:Range(cRange):Value = "Plan.avb" NO-ERROR.

      
      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         cRange = "C" + cColumn.              
         chWorkSheet:Range(cRange):Value = STRING(extemp.HDATUM,"99-99-99") NO-ERROR.
         cRange = "D" + cColumn.                                                 
         chWorkSheet:Range(cRange):HorizontalAlignment = -4108 NO-ERROR.
         chWorkSheet:Range(cRange):Value = STRING(extemp.HKLOCKAN,"99.99") NO-ERROR.   
      END.
      ELSE DO:
         cRange = "C" + cColumn.              
         chWorkSheet:Range(cRange):Value = STRING(extemp.HDATUM,"9999/99/99") NO-ERROR.
         cRange = "D" + cColumn.                                                 
         chWorkSheet:Range(cRange):Value = STRING(extemp.HKLOCKAN,"99.99") NO-ERROR.   
      END.

      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         cRange = "E" + cColumn.
         chWorkSheet:Range(cRange):Value = STRING(extemp.DATUM100%,"99-99-99") NO-ERROR.
         cRange = "F" + cColumn.                                                 
         chWorkSheet:Range(cRange):HorizontalAlignment = -4108 NO-ERROR.
         chWorkSheet:Range(cRange):Value = STRING(extemp.KLOCKAN100%,"99.99") NO-ERROR.
      END.
      ELSE DO:
         cRange = "E" + cColumn.
         chWorkSheet:Range(cRange):Value = STRING(extemp.DATUM100%,"9999/99/99") NO-ERROR.
         cRange = "F" + cColumn.
         chWorkSheet:Range(cRange):Value = STRING(extemp.KLOCKAN100%,"99.99") NO-ERROR.
      END.

      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         cRange = "G" + cColumn.    
         formatvar = INTEGER(klockan60(extemp.AVBROTTSTID)).
         chWorkSheet:Range(cRange):Value = STRING(formatvar,">>>>>>>>9") NO-ERROR.               
         kundtimtot = kundtimtot + DECIMAL(klockan60(extemp.AVBROTTSTID)).         
      END.
      ELSE DO:
         cRange = "G" + cColumn.      
         chWorkSheet:Range(cRange):Value = DECIMAL(klockan60(extemp.AVBROTTSTID)) NO-ERROR.
      END.

      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         cRange = "H" + cColumn.
         chWorkSheet:Range(cRange):Value = STRING(extemp.ANTALHSP,">>>>>>>>9") NO-ERROR. 
         hsptot = hsptot + DECIMAL(STRING(extemp.ANTALHSP)).
      END.
      ELSE DO:
         cRange = "H" + cColumn.
         chWorkSheet:Range(cRange):Value = STRING(extemp.ANTALHSP) NO-ERROR.               
      END.

      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         cRange = "I" + cColumn.
         chWorkSheet:Range(cRange):Value = STRING(extemp.ANTALLSP,">>>>>>>>9") NO-ERROR. 
         lsptot = lsptot + DECIMAL(STRING(extemp.ANTALLSP)).
      END.
      ELSE DO:
         cRange = "I" + cColumn.
         chWorkSheet:Range(cRange):Value = STRING(extemp.ANTALLSP) NO-ERROR.               
      END.
      FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = extemp.FRANSPANID
      NO-LOCK NO-ERROR.      
      cRange = "J" + cColumn.
      IF AVAILABLE spanningsnivtemp THEN chWorkSheet:Range(cRange):Value = STRING(spanningsnivtemp.NAMN) NO-ERROR.               
      ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.
      IF Guru.Konstanter:globforetag = "MALU" THEN DO:
         FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = extemp.STDRIFTID
         NO-LOCK NO-ERROR.  
         cRange = "K" + cColumn.
         IF AVAILABLE stordriftomrtemp THEN chWorkSheet:Range(cRange):Value = STRING(stordriftomrtemp.NAMN) NO-ERROR.
         ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.
         FIND FIRST anlaggningsdeltemp WHERE anlaggningsdeltemp.ADELID = extemp.ADELID
         NO-LOCK NO-ERROR.      
         cRange = "L" + cColumn.
         IF AVAILABLE anlaggningsdeltemp THEN chWorkSheet:Range(cRange):Value = STRING(anlaggningsdeltemp.NAMN) NO-ERROR.
         ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.
   
         IF extemp.STORTYPID = 1 THEN DO:                    
            FIND FIRST felorsaktemp WHERE felorsaktemp.FELOID = extemp.FELOID
            NO-LOCK NO-ERROR.            
            cRange = "M" + cColumn.
            IF AVAILABLE felorsaktemp THEN chWorkSheet:Range(cRange):Value = STRING(felorsaktemp.NAMN) NO-ERROR.               
            ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.               
         END.
      END.
      ELSE DO:         
         IF extemp.STORTYPID = 1 THEN DO:                    
            FIND FIRST spanningsnivtemp WHERE spanningsnivtemp.SPANID = extemp.FELSPANID
            NO-LOCK NO-ERROR.
            cRange = "K" + cColumn.
            IF AVAILABLE spanningsnivtemp THEN chWorkSheet:Range(cRange):Value = STRING(spanningsnivtemp.NAMN) NO-ERROR.
            ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.         
         END.  
         FIND FIRST stordriftomrtemp WHERE stordriftomrtemp.STDRIFTID = extemp.STDRIFTID
         NO-LOCK NO-ERROR.  
         cRange = "L" + cColumn.
         IF AVAILABLE stordriftomrtemp THEN chWorkSheet:Range(cRange):Value = STRING(stordriftomrtemp.NAMN) NO-ERROR.
         ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.
         FIND FIRST anlaggningsdeltemp WHERE anlaggningsdeltemp.ADELID = extemp.ADELID
         NO-LOCK NO-ERROR.      
         cRange = "M" + cColumn.
         IF AVAILABLE anlaggningsdeltemp THEN chWorkSheet:Range(cRange):Value = STRING(anlaggningsdeltemp.NAMN) NO-ERROR.
         ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.
   
         IF extemp.STORTYPID = 1 THEN DO:                    
            FIND FIRST felorsaktemp WHERE felorsaktemp.FELOID = extemp.FELOID
            NO-LOCK NO-ERROR.            
            cRange = "N" + cColumn.
            IF AVAILABLE felorsaktemp THEN chWorkSheet:Range(cRange):Value = STRING(felorsaktemp.NAMN) NO-ERROR.               
            ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.               
         END.
         IF Guru.Konstanter:globforetag = "SVEN" OR Guru.Konstanter:globforetag = "SEKG" THEN DO:
            cRange = "O" + cColumn.      
            IF AVAILABLE stordistemp THEN chWorkSheet:Range(cRange):Value = STRING(stordistemp.AVDELNINGNR) NO-ERROR.
            ELSE chWorkSheet:Range(cRange):Value = "" NO-ERROR.
         END.
      END.         
   END.  
   /*END FOR EACH*/
   IF Guru.Konstanter:globforetag = "MALU" THEN DO:
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      IF kundtimtot NE 0 THEN DO:
         cRange = "G" + cColumn. 
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):VALUE = STRING(kundtimtot,">>>>>>>>9") NO-ERROR.
      END.
      IF hsptot NE 0 THEN DO:
         cRange = "H" + cColumn.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = STRING(hsptot,">>>>>>>>9") NO-ERROR.
      END.
      IF lsptot NE 0 THEN DO:
         cRange = "I" + cColumn.
         chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
         chWorkSheet:Range(cRange):Value = STRING(lsptot,">>>>>>>>9") NO-ERROR.
      END.                                             
   END.

/*    DEFINE INPUT PARAMETER varstryk AS INTEGER NO-UNDO. /*3= över 4= under*/ */
/*    DEFINE INPUT PARAMETER vartjock AS INTEGER NO-UNDO. /*2= tunn 4= tjock*/ */
/*    DEFINE INPUT PARAMETER inrow AS INTEGER NO-UNDO.                         */
   


   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE storning_UI:   
   IF Guru.Konstanter:appcon THEN DO:                           
      RUN STOREXCEL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
      INPUT bdatum, INPUT avdatum, INPUT period,
      INPUT uttyp, INPUT alla, INPUT spannvar, INPUT-OUTPUT TABLE extemp).      
   END.
   ELSE DO:
      RUN STOREXCEL.P 
      (INPUT TABLE avd_temp, INPUT valfore, INPUT distvar, 
      INPUT bdatum, INPUT avdatum, INPUT period,
      INPUT uttyp, INPUT alla, INPUT spannvar, INPUT-OUTPUT TABLE extemp).
   END.  
END PROCEDURE.

PROCEDURE open_UI:
   DEFINE INPUT  PARAMETER kommando AS CHARACTER  NO-UNDO.
      
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:VISIBLE = FALSE.
   {OPENEXCEL.I}
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
   chWorksheet = chWorkbook:Worksheets:ITEM(1) NO-ERROR.
   chWorkSheet:Columns("a:ab"):EntireColumn:AutoFit NO-ERROR.
   {EXCELFEL.I}
END PROCEDURE.
PROCEDURE slutexcel_UI:
   chExcelApplication:VISIBLE = TRUE NO-ERROR.
   chExcelApplication:DisplayAlerts = TRUE NO-ERROR.   /*all prompts will be shutoff/on*/   
   RELEASE OBJECT chWorkbook NO-ERROR.                   
   RELEASE OBJECT chExcelApplication NO-ERROR.           
   RELEASE OBJECT chPageBreak NO-ERROR.                  
   RELEASE OBJECT chCell NO-ERROR.                       
   RELEASE OBJECT chWorksheet NO-ERROR.                  
   RELEASE OBJECT chChart NO-ERROR.                      
   RELEASE OBJECT chWorksheetRange NO-ERROR.             
   RELEASE OBJECT cActiveCell NO-ERROR.      
   ASSIGN
   chWorkbook = ?  
   chExcelApplication = ?
   chPageBreak = ?
   chCell = ?
   chWorksheet = ?
   chChart = ?
   chWorksheetRange = ?
   cActiveCell = ?.
   
   {EXCELFEL.I}
END PROCEDURE.

