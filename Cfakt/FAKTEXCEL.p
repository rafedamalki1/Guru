
/*------------------------------------------------------------------------
    File        : FAKTEXCEL.p
    Purpose     : Presenterar faktura i excel

    Syntax      :

    Description : 

    Author(s)   : rs
    Created     : Mon Jan 13 13:07:02 CET 2014
    Notes       :
  ----------------------------------------------------------------------*/
{ExcelDS.i}
{FAKTMAILTT.I}
{FAKTMAIL.I}
DEFINE INPUT  PARAMETER skrivut AS LOGICAL NO-UNDO.
DEFINE INPUT  PARAMETER kredfakt AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR fakturaexcelTT.
DEFINE INPUT PARAMETER TABLE FOR faktposterexcelTT.
DEFINE VARIABLE kreditchr AS CHARACTER  NO-UNDO.
IF kredfakt = TRUE THEN kreditchr = "-".
 
&Scoped-define NEW 
{GLOBVAR2DEL1.I}
DEFINE STREAM dirstrom.
DEFINE VARIABLE chCell             AS COM-HANDLE NO-UNDO.
DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
DEFINE VARIABLE chExcelApplication      AS COM-HANDLE.
DEFINE VARIABLE chWorkbook              AS COM-HANDLE.
DEFINE VARIABLE chWorksheet             AS COM-HANDLE.
DEFINE VARIABLE chChart                 AS COM-HANDLE.
DEFINE VARIABLE chWorksheetRange        AS COM-HANDLE.
DEFINE VARIABLE iCount                  AS INTEGER.
DEFINE VARIABLE iIndex                  AS INTEGER.
DEFINE VARIABLE iMonth                  AS INTEGER.
DEFINE VARIABLE dAnnualQuota            AS DECIMAL.
DEFINE VARIABLE dTotalSalesAmount       AS DECIMAL.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE cRange                  AS CHARACTER.   
DEFINE VARIABLE irad                 AS INTEGER INITIAL 15.
DEFINE VARIABLE ihrad                 AS INTEGER .
DEFINE VARIABLE varde                  AS CHARACTER.   
DEFINE VARIABLE raknare                 AS INTEGER INITIAL 50.
DEFINE VARIABLE path                  AS CHARACTER.   
DEFINE VARIABLE decichar AS CHARACTER NO-UNDO.
RUN huvud_UI.

PROCEDURE huvud_UI:
   DEFINE VARIABLE answ AS INTEGER NO-UNDO.
   DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
   kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
   OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
   IF kredfakt = TRUE THEN DO:
      kommando2 = kommando2 + "elpfaktmallkredit.xlsx".
      path = SEARCH("elpfaktmallkredit.xlsx").
   END.   
   ELSE DO:
      kommando2 = kommando2 + "elpfaktmall.xlsx".
      path = SEARCH("elpfaktmall.xlsx").
   END.
   OS-COPY VALUE(path) VALUE(kommando2).
   path = kommando2.
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE .
   chExcelApplication:ScreenUpdating = TRUE.
   
   ASSIGN chWorkbook = chExcelApplication:Workbooks:OPEN(path) NO-ERROR.
   chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
    
   FIND FIRST fakturaexcelTT.
   
   FIND FIRST Epeppol WHERE Epeppol.FAKTNR = fakturaexcelTT.FAKTNR NO-LOCK NO-ERROR.
   
       
  
   ihrad = 3.
   chWorkSheet:Range("A" + STRING(ihrad)):Value = fakturaexcelTT.BESTNAMN NO-ERROR.
   IF AVAILABLE Epeppol THEN DO:
      ihrad = ihrad + 1.
      IF Epeppol.GLN NE "" THEN chWorkSheet:Range("A" + STRING(ihrad)):VALUE = "GLN " + STRING(Epeppol.GLN) NO-ERROR. 
      ELSE IF Epeppol.PEPPOL NE "" THEN chWorkSheet:Range("A" + STRING(ihrad)):VALUE = "Peppol-ID " + STRING(Epeppol.PEPPOL) NO-ERROR. 
      ELSE chWorkSheet:Range("A" + STRING(ihrad)):Value = "Org.nr " + STRING(Epeppol.ORGNR) NO-ERROR.
  
   END.
   ihrad = ihrad + 1.
   chWorkSheet:Range("A" + STRING(ihrad)):Value = SUBSTRING(fakturaexcelTT.FAKTADRESS,1,25) NO-ERROR.
   ihrad = ihrad + 1.
   chWorkSheet:Range("A" + STRING(ihrad)):Value = SUBSTRING(fakturaexcelTT.FAKTADRESS,26,25) NO-ERROR.
   
   ihrad = ihrad + 1.
   IF fakturaexcelTT.FAKTPOSTNR = "" THEN chWorkSheet:Range("A" + STRING(ihrad)):Value = fakturaexcelTT.FAKTPOSTNR + " " + SUBSTRING(fakturaexcelTT.FAKTORT,1,25) NO-ERROR.
   ELSE chWorkSheet:Range("A" + STRING(ihrad)):Value = STRING(fakturaexcelTT.FAKTPOSTNR,"999 99")  + " " + SUBSTRING(fakturaexcelTT.FAKTORT,1,25) NO-ERROR.
   ihrad = ihrad + 1.
   chWorkSheet:Range("A" + STRING(ihrad)):Value = SUBSTRING(fakturaexcelTT.FAKTORT,26,25) NO-ERROR.
   ihrad = ihrad + 1.
   IF SUBSTRING(fakturaexcelTT.FAKTORT,52,25) NE "" THEN  chWorkSheet:Range("A" + STRING(ihrad)):Value = "VAT " + SUBSTRING(fakturaexcelTT.FAKTORT,52,25) NO-ERROR.  
   
   chWorkSheet:Range("D2"):Value =  fakturaexcelTT.FAKTDATUM NO-ERROR.
   chWorkSheet:Range("D5"):Value =  fakturaexcelTT.FAKTNUMMER NO-ERROR.
   chWorkSheet:Range("D9"):Value = fakturaexcelTT.VARREF NO-ERROR.
   chWorkSheet:Range("D12"):Value = fakturaexcelTT.ERREF NO-ERROR.
   
   
   /*Loopa poster*/
   OPEN QUERY poster FOR EACH faktposterexcelTT
   NO-LOCK.
   GET FIRST poster NO-LOCK.
   DO WHILE AVAILABLE(faktposterexcelTT):
      IF faktposterexcelTT.ANTAL NE "KOMMENTAR" THEN DO:
         chWorkSheet:Range("A" + STRING(irad)):Value = faktposterexcelTT.BESKIVNING NO-ERROR.
         chWorkSheet:Range("B" + STRING(irad)):Value = faktposterexcelTT.ANTAL NO-ERROR.
         chWorkSheet:Range("C" + STRING(irad)):Value = faktposterexcelTT.PRIS NO-ERROR.
         chWorkSheet:Range("D" + STRING(irad)):Value = faktposterexcelTT.SUMMA NO-ERROR.
         irad = irad + 1.
      END.
      GET NEXT poster NO-LOCK.
   END.   
   CLOSE QUERY poster.
   DEFINE VARIABLE ichr AS INTEGER NO-UNDO.
   DEFINE VARIABLE ichrsl AS INTEGER NO-UNDO.
   DEFINE VARIABLE kommut AS CHARACTER NO-UNDO.
   /*Lägg ut kommentar*/
   irad = irad + 1.
 
   ichr = 1.
   FIND FIRST faktposterexcelTT WHERE faktposterexcelTT.ANTAL = "KOMMENTAR" NO-ERROR.
   IF AVAILABLE faktposterexcelTT THEN DO:
      IF INDEX(SUBSTRING(faktposterexcelTT.BESKIVNING,ichr),CHR(10),ichr) = 0 THEN DO:
         IF TRIM (faktposterexcelTT.BESKIVNING) NE "" THEN DO:
            kommut = REPLACE(faktposterexcelTT.BESKIVNING,CHR(10),"").  
            IF TRIM (kommut) NE "" THEN DO:
               chWorkSheet:Range("A" + STRING(irad)):VALUE = kommut NO-ERROR.
               IF AVAILABLE Epeppol THEN DO:
                  chWorkSheet:Range("B" + STRING(irad)):VALUE = "1" NO-ERROR.
                  chWorkSheet:Range("C" + STRING(irad)):VALUE = "0" NO-ERROR.
                  chWorkSheet:Range("D" + STRING(irad)):VALUE = "0" NO-ERROR.
               END.
            END.   
         END.      
      END. 
      ELSE DO :
         REPEAT:
            IF INDEX(faktposterexcelTT.BESKIVNING,CHR(10),ichr + 1) = 0 THEN DO:
               kommut = SUBSTRING(faktposterexcelTT.BESKIVNING,ichr).
               kommut = REPLACE(kommut,CHR(10),"").   
               IF TRIM (kommut) NE "" THEN DO:
                  chWorkSheet:Range("A" + STRING(irad)):VALUE = kommut NO-ERROR.
                  IF AVAILABLE Epeppol THEN DO:
                     chWorkSheet:Range("B" + STRING(irad)):VALUE = "1" NO-ERROR.
                     chWorkSheet:Range("C" + STRING(irad)):VALUE = "0" NO-ERROR.
                     chWorkSheet:Range("D" + STRING(irad)):VALUE = "0" NO-ERROR.
                  END.
               END.   
               LEAVE.
            END.
            ELSE DO:
               ichrsl = INDEX(faktposterexcelTT.BESKIVNING,CHR(10),ichr + 1).
               kommut = SUBSTRING(faktposterexcelTT.BESKIVNING,ichr,ichrsl - ichr).
               kommut = REPLACE(kommut,CHR(10),""). 
               IF TRIM (kommut) NE "" THEN DO: 
                  chWorkSheet:Range("A" + STRING(irad)):VALUE = kommut NO-ERROR.
                  IF AVAILABLE Epeppol THEN DO:
                     chWorkSheet:Range("B" + STRING(irad)):VALUE = "1" NO-ERROR.
                     chWorkSheet:Range("C" + STRING(irad)):VALUE = "0" NO-ERROR.
                     chWorkSheet:Range("D" + STRING(irad)):VALUE = "0" NO-ERROR.
                  END.
               END.  
               ichr = ichrsl + 1.
               irad = irad + 1.
            END.
         END.
      END.
       
   END.
   irad = irad + 1.
   
 
 
   /*Lägg summarader*/
   
   chWorkSheet:Range("D44"):Value = kreditchr + fakturaexcelTT.SUMMA NO-ERROR.
   irad = irad + 1.
   chWorkSheet:Range("D45"):Value = kreditchr + fakturaexcelTT.MOMS NO-ERROR.
   irad = irad + 1.
   chWorkSheet:Range("D46"):Value = kreditchr + fakturaexcelTT.SLUTSUMMA NO-ERROR.
   chWorkSheet:Range("A46"):Value = "Förfallodatum " + fakturaexcelTT.FORFALLDATUM NO-ERROR.
  
   FIND FIRST faktmail WHERE faktmail.FAKTNR = fakturaexcelTT.FAKTNR NO-LOCK NO-ERROR.
   FIND FIRST Epeppol WHERE Epeppol.FAKTNR = fakturaexcelTT.FAKTNR NO-LOCK NO-ERROR.
   FIND FIRST faktkuvert WHERE faktkuvert.FAKTNR = fakturaexcelTT.FAKTNR NO-LOCK NO-ERROR.
   IF AVAILABLE faktmail THEN DO:
      
      RUN SaveAsPdf_UI (INPUT "D:\FAKTURORGURU\efakt\" + faktmail.EGETNAMN + STRING(fakturaexcelTT.VFAKTNR),FALSE).
        
   END.
   ELSE IF AVAILABLE Epeppol THEN DO:
      RUN SaveAsPdf_UI (INPUT "D:\FAKTURORGURU\FaktE\" + Epeppol.EGETNAMN + STRING(fakturaexcelTT.VFAKTNR),FALSE).
   END.  
   ELSE IF AVAILABLE faktkuvert THEN DO:
      DEFINE VARIABLE cRangefont AS CHARACTER NO-UNDO.
      cRangefont = "A41".
      chWorkSheet:Range(cRangefont):FONT:Bold = TRUE NO-ERROR.
      chCell = chWorkSheet:Range(cRangefont) NO-ERROR.
      chCell:Interior:ColorIndex = 36 NO-ERROR.
      chWorkSheet:Range(cRangefont):Value = "Vill du ha E-faktura eller pdf faktura på mail anmäl dig till anders@elpool.se".
      RELEASE OBJECT chCell NO-ERROR.
      RUN SaveAsPdf_UI (INPUT "D:\FAKTURORGURU\KUVERTPDF\" + faktkuvert.EGETNAMN + STRING(fakturaexcelTT.VFAKTNR),FALSE).
   END.
   IF skrivut = TRUE THEN chWorkSheet:PrintOut(,,,,STRING(SESSION:PRINTER-NAME),,,).  
   /*
   Application.WindowState = xlNormal
     
   chExcelApplication:WindowState = -4137. /*maximera*/
   
   chExcelApplication:WindowState = -4143. /*normal*/
   */
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   ASSIGN
   chExcelApplication = ?  
   chWorkbook = ?
   chWorksheet = ?.
   
   EMPTY TEMP-TABLE fakturaexcelTT NO-ERROR.
   EMPTY TEMP-TABLE faktposterexcelTT NO-ERROR.
END PROCEDURE.
PROCEDURE SWECORAKNA_UI :
   DEFINE OUTPUT PARAMETER antalsweco AS INTEGER NO-UNDO. 
    
   DEFINE VARIABLE dirnamn AS CHARACTER  NO-UNDO.
   DEFINE VARIABLE mvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE mvardat AS CHARACTER NO-UNDO.
   DEFINE VARIABLE tmpfilnamn AS CHARACTER FORMAT "X(77)" NO-UNDO.
   DEFINE VARIABLE tmpdirlist AS CHARACTER  NO-UNDO
   VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 3. 
   DEFINE VARIABLE tmpattrlist AS CHARACTER NO-UNDO.
   DEFINE VARIABLE iord AS INTEGER NO-UNDO.
   dirnamn = "D:\FAKTURORGURU\efakt\".
   mvar = "Sweco" + "*".
   mvardat = "*" + STRING(TODAY, "999999") + "*".
   ASSIGN
   tmpfilnamn = ""
   tmpattrlist = ""
   tmpdirlist = "".
   IF dirnamn = "" THEN RETURN.
   INPUT STREAM dirstrom FROM OS-DIR(dirnamn) NO-ECHO.
   REPEAT:
      SET STREAM dirstrom tmpfilnamn tmpdirlist tmpattrlist.
      IF tmpfilnamn MATCHES mvar THEN DO:
         IF tmpfilnamn MATCHES mvardat THEN DO:
            antalsweco = antalsweco + 1.
         END.   
      END.
   END.
   INPUT STREAM dirstrom CLOSE.    
   antalsweco = antalsweco + 1.  
END PROCEDURE.
PROCEDURE SaveAsPdf_UI:
   DEFINE INPUT  PARAMETER dirandfil AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER visa AS LOGICAL NO-UNDO.
   chworkbook:ExportAsFixedFormat(0,dirandfil,0,visa,,,,,) NO-ERROR. 

END PROCEDURE.
