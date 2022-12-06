/*AVROPEXC.P*/
DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
         
                              
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
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
DEFINE VARIABLE radnrS                  AS CHARACTER.
DEFINE VARIABLE pristext                AS CHARACTER.


DEFINE  SHARED TEMP-TABLE vis_temp
   FIELD AONR AS CHARACTER
   FIELD DELNR AS INTEGER
   FIELD BENAMNING AS CHARACTER
   FIELD LEVNAMN AS CHARACTER
   FIELD DATUM AS DATE
   FIELD KLAR AS DATE
   FIELD BKONTAKT AS CHARACTER
   FIELD BTEL AS CHARACTER
   FIELD BMOBIL AS CHARACTER
   FIELD EKONTAKT AS CHARACTER
   FIELD ETEL AS CHARACTER
   FIELD EMOBIL AS CHARACTER
   FIELD PRIS AS INTEGER.
   
   FIND FIRST vis_temp NO-LOCK NO-ERROR.
    
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.  
{GLOBVAR2DEL1.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
IF bloblog = TRUE THEN DO:
   {FINNSDYNBLOB.I}
   DEFINE VARIABLE resid AS INTEGER NO-UNDO.
   RUN blobfil_UI IN blobproch (INPUT "AVROP.XLS", OUTPUT resid).
   IF resid = ? THEN DO:
      kommando = SEARCH("AVROP.XLS").      
   END.
   ELSE DO:
      FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.
      RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT kommando).
      
   END.
   RUN deleteproc_UI IN blobproch.
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
END.
ELSE DO: 
   kommando = SEARCH("AVROP.XLS").      
END.
IF kommando = ? THEN DO:
   MESSAGE "Hittade inte AVROP.XLS" VIEW-AS ALERT-BOX.
   RETURN.       
END.  
kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
{SESSIONTEMPDIR.I}
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
OS-CREATE-DIR VALUE(kommando2) NO-ERROR.

IF  Guru.GlobalaVariabler:plusaonr = "" OR  Guru.GlobalaVariabler:plusaonr = ? THEN DO:
   kommando2 = kommando2 + STRING(TIME) + "AVROP.XLS".
END.
ELSE DO:
   kommando2 = kommando2 + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + "AVROP.XLS".
END.
OS-COPY VALUE(kommando) VALUE(kommando2).
kommando = kommando2.   
   {OPENEXCEL.I}
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.  
   chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.   
   /*HUVUD*/   
   iColumn = 6.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.   
   chWorkSheet:Range(cRange):Value = vis_temp.LEVNAMN NO-ERROR.
   iColumn = 10.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.   
   chWorkSheet:Range(cRange):Value = Guru.Konstanter:gaok + ":" + vis_temp.AONR + "   Delnr:" + 
   STRING(vis_temp.DELNR,Guru.Konstanter:varforetypchar[1]) + "   Benämning:" + SUBSTRING(vis_temp.BENAMNING,1,35) NO-ERROR.       
   iColumn = 13.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.   
   chWorkSheet:Range(cRange):Value = "datum " + STRING(vis_temp.DATUM,"9999/99/99") + " " + chWorkSheet:Range(cRange):VALUE NO-ERROR.   
   iColumn = 20.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.   
   chWorkSheet:Range(cRange):Value = vis_temp.LEVNAMN + " " + chWorkSheet:Range(cRange):VALUE NO-ERROR.   
   IF vis_temp.PRIS = 1 THEN pristext = "[X] Fastpris     [ ] Timpris     [ ] P2".
   ELSE IF vis_temp.PRIS = 2 THEN pristext = "[ ] Fastpris     [X] Timpris     [ ] P2".
   ELSE pristext = "[ ] Fastpris     [ ] Timpris     [X] P2".   
   iColumn = 26.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   
   chWorkSheet:Range(cRange):Value = pristext NO-ERROR.
   
   iColumn = 30.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   
   chWorkSheet:Range(cRange):Value = chWorkSheet:Range(cRange):Value + vis_temp.BKONTAKT NO-ERROR.
   
   iColumn = 31.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   
   chWorkSheet:Range(cRange):Value = "Telefon:" + vis_temp.BTEL + " Mobil:" + vis_temp.BMOBIL NO-ERROR.
   
   iColumn = 33.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   
   chWorkSheet:Range(cRange):Value = chWorkSheet:Range(cRange):Value + vis_temp.EKONTAKT NO-ERROR.
   
   iColumn = 34.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   
   chWorkSheet:Range(cRange):Value = "Telefon:" + vis_temp.ETEL + " Mobil:" + vis_temp.EMOBIL NO-ERROR.
   
   iColumn = 39.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   
   chWorkSheet:Range(cRange):Value = chWorkSheet:Range(cRange):Value + STRING(vis_temp.KLAR,"9999/99/99") NO-ERROR.
   
   iColumn = 47.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   
   chWorkSheet:Range(cRange):Value = chWorkSheet:Range(cRange):Value + vis_temp.LEVNAMN NO-ERROR.
   
   iColumn = 56.
   cColumn = STRING(iColumn).
   cRange = "A" + cColumn.
   
   chWorkSheet:Range(cRange):Value = chWorkSheet:Range(cRange):Value + vis_temp.LEVNAMN NO-ERROR.

   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   
{EXCELFEL.I}      
