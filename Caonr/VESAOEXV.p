/*VESAOEXV.P*/
&Scoped-define NEW 
DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
{REGVAR.I}
{GLOBVAR2DEL1.I}
{SOKDEF.I}
{FAKTTYPDEF.I}
{FAKTTYPSKAP.I}
&Scoped-define SHARED SHARED
{AONRTEMP.I}
DEFINE INPUT PARAMETER aoinvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnrinvar AS INTEGER NO-UNDO.
         

DEFINE VARIABLE maldatum AS DATE NO-UNDO.
DEFINE VARIABLE startdatum AS DATE NO-UNDO.
DEFINE VARIABLE radanmrak AS INTEGER NO-UNDO.
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


{ANMARKD.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
FIND FIRST aonrtemp WHERE aonrtemp.AONR = aoinvar AND aonrtemp.DELNR = delnrinvar NO-LOCK NO-ERROR.
IF NOT AVAILABLE aonrtemp THEN RETURN.   
IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
IF bloblog = TRUE THEN DO:
  {FINNSDYNBLOB.I}
   DEFINE VARIABLE resid AS INTEGER NO-UNDO.
   RUN blobfil_UI IN blobproch (INPUT "VESABAOV.XLS", OUTPUT resid).
   IF resid = ? THEN DO:
      kommando = SEARCH("VESABAOV.XLS").      
   END.
   ELSE DO:
      FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.
      RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT kommando).      
   END.
   RUN deleteproc_UI IN blobproch.
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
END.
ELSE kommando = SEARCH("VESABAOV.XLS").   
     
IF kommando = ? THEN DO:
   MESSAGE "Hittade inte VESABAOV.XLS" VIEW-AS ALERT-BOX.
   RETURN.       
END.
kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
{SESSIONTEMPDIR.I}
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.

OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
IF  Guru.GlobalaVariabler:plusaonr = "" OR  Guru.GlobalaVariabler:plusaonr = ? THEN DO:
   kommando2 = kommando2 + STRING(TIME) + "VESABAOV.XLS".
END.
ELSE DO:
   kommando2 = kommando2 + TRIM( Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + "VESABAOV.XLS".
END.
OS-COPY VALUE(kommando) VALUE(kommando2).
kommando = kommando2.


CREATE "Excel.Application" chExcelApplication.
chExcelApplication:Visible = TRUE.
{OPENEXCEL.I}
chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).
chWorkSheet = chExcelApplication:Sheets:Item(1).
chWorkSheet:Columns("A"):ColumnWidth = 15  NO-ERROR.
chWorkSheet:Columns("B"):ColumnWidth = 15  NO-ERROR.
chWorkSheet:Columns("C"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("D"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("E"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("F"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("G"):ColumnWidth = 16 NO-ERROR.
chWorkSheet:Columns("H"):ColumnWidth = 15 NO-ERROR.
chWorkSheet:Columns("I"):ColumnWidth = 15 NO-ERROR.
/*START RAD*/
iColumn = iColumn + 4.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "I" + cColumn.
cRange = cRange + ":" + radnrS.  


/*FÖRSTA RUBRIK*/

chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = CAPS(SUBSTRING(Guru.Konstanter:gutfardl,1,1)) + LC(SUBSTRING(Guru.Konstanter:gutfardl,2)) NO-ERROR.
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "Utskriftsdatum" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Faktureringstyp" NO-ERROR.

iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.  
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = SUBSTRING(aonrtemp.UTFARDAT,1,30) NO-ERROR.     
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = STRING(TODAY,"9999-99-99") NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = faktyp(aonrtemp.FAKTTYP) NO-ERROR.
{EXCELFEL.I}      

/*ANDRA RUBRIK*/
{SOKSTART.I}
ASSIGN
soktemp.SOKVAL = 10
soktemp.SOKCHAR[1] = aonrtemp.AONR
soktemp.SOKCHAR[2] = "MALDATUM"
soktemp.SOKINT[1] = aonrtemp.DELNR.
{SOKANROP.I}      
IF soktemp.SOKCHAR[1] = "FINNS EJ" THEN DO:
   ASSIGN maldatum = ?.
END.
ELSE DO:
   ASSIGN maldatum = soktemp.SOKDATE[1].
END.
{SOKSTART.I}
ASSIGN
soktemp.SOKVAL = 10
soktemp.SOKCHAR[1] = aonrtemp.AONR
soktemp.SOKCHAR[2] = "AOUPPLAGT"
soktemp.SOKINT[1] = aonrtemp.DELNR.
{SOKANROP.I}      
IF soktemp.SOKCHAR[1] = "FINNS EJ" THEN DO: 
   ASSIGN startdatum = ?.
END.
ELSE DO:
   ASSIGN startdatum = soktemp.SOKDATE[1].
END.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Projektledare" NO-ERROR. 
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "Beredare/Projektör" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Utförandeperiod" NO-ERROR.
{SOKSTART.I}
   
ASSIGN
soktemp.SOKVAL = 75
soktemp.SOKCHAR[1] = aonrtemp.ARBANSVARIG.
{SOKANROP.I}      
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.  
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = soktemp.SOKCHAR[2] NO-ERROR.
{EXCELFEL.I}      
{SOKSTART.I}
ASSIGN
soktemp.SOKVAL = 75
soktemp.SOKCHAR[1] = aonrtemp.BEREDARE.
{SOKANROP.I}      
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = soktemp.SOKCHAR[2] NO-ERROR.
cRange = "E" + cColumn.
IF startdatum = ? AND maldatum = ? THEN startdatum = startdatum.
ELSE IF startdatum = ? THEN chWorkSheet:Range(cRange):Value = "           - " + STRING(maldatum,"9999-99-99") NO-ERROR.
ELSE IF maldatum = ? THEN chWorkSheet:Range(cRange):Value = STRING(startdatum,"9999-99-99") + " - " NO-ERROR.
ELSE IF startdatum > maldatum THEN chWorkSheet:Range(cRange):Value = "           - " + STRING(maldatum,"9999-99-99") NO-ERROR.
ELSE chWorkSheet:Range(cRange):Value = STRING(startdatum,"9999-99-99") + " - " + STRING(maldatum,"9999-99-99") NO-ERROR.


/*TREDJE RUBRIK*/
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Elbehörighetsansvarig el. likn" NO-ERROR.
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "Ekonomisk ram" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Ekonomiskt utfall" NO-ERROR.

iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.  
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "" NO-ERROR.
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "" NO-ERROR.


/*FJÄRDE RUBRIK*/

iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = Guru.Konstanter:gaol NO-ERROR.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.  
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
cRange = "A" + cColumn.
IF aonrtemp.DELNR NE 0 THEN DO:
   chWorkSheet:Range(cRange):Value = "KK." + aonrtemp.AONR + " " + STRING(aonrtemp.DELNR,Guru.Konstanter:varforetypchar[1]) NO-ERROR.
END.
ELSE DO:
   chWorkSheet:Range(cRange):Value = "KK." + STRING(aonrtemp.AONR) NO-ERROR.
END.


/*FEMTE RUBRIK*/
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Kund, Kund ref." NO-ERROR.
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "Kontaktperson" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Telefon" NO-ERROR.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.  
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
{SOKSTART.I}
ASSIGN
soktemp.SOKVAL = 76
soktemp.SOKCHAR[1] = aonrtemp.BESTID.
{SOKANROP.I}      

IF soktemp.SOKINT[1] = 0 THEN DO:
   cRange = "A" + cColumn.
   chWorkSheet:Range(cRange):Value = soktemp.SOKCHAR[2] NO-ERROR.
   cRange = "C" + cColumn.
   chWorkSheet:Range(cRange):Value = soktemp.SOKCHAR[3] NO-ERROR.
   cRange = "E" + cColumn.
   chWorkSheet:Range(cRange):Value = soktemp.SOKCHAR[4] NO-ERROR.
END.


/*ÖVRIGA*/
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cRange = "A" + cColumn.
{EXCELFEL.I}      
chWorkSheet:Range(cRange):Value = "Bokade sign, kategorier" NO-ERROR.
iColumn = iColumn + 4.   
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Projektledaren har ansvaret för delegerade delar av arbetsmiljö, miljö och kvalitet enligt instruktion" NO-ERROR.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "TE-In-146, där bl.a. elarbetsansvaret ingår." NO-ERROR.


/*BENÄNNING OCH ARBETSUPPGIFT*/
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = aonrtemp.ORT NO-ERROR.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + STRING(iColumn + 4).
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
iColumn = iColumn - 1.
{EXCELFEL.I}      
IF LENGTH(aonrtemp.ARBUPPG[1]) > 0 THEN DO:      
   ASSIGN
   radanmrak = 0
   retvar = 1
   ednum = 1
   ednum3 = LENGTH(aonrtemp.ARBUPPG[1])
   retvar = INDEX(aonrtemp.ARBUPPG[1],CHR(10),ednum)
   edtecken = 38
   edtext = aonrtemp.ARBUPPG[1]
   tidtext = "".  
   {ANMARK2.I}                             
   IF radanmrak < 4 THEN DO:
      iColumn = iColumn + (4 - radanmrak).
      cColumn = STRING(iColumn).
   END.
END.
ELSE DO:
   iColumn = iColumn + 4.
   cColumn = STRING(iColumn).
END.
/*ÅTERRAPPORTERING*/
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.  
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = "Dagbok erfodras" NO-ERROR.
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = "Följesedel erfodras" NO-ERROR.   

iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Materiel" NO-ERROR.
iColumn = iColumn + 4.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Återrapportering" NO-ERROR.

iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + "40".
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
cColumn = STRING(iColumn).
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "Datum" NO-ERROR.
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = " Ja       Nej" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Sign" NO-ERROR.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Arbetet klart" NO-ERROR.
iColumn = iColumn + 2.
cColumn = STRING(iColumn) NO-ERROR.
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "Datum" NO-ERROR.
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = " Ja       Nej" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Sign" NO-ERROR.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Beredning klar" NO-ERROR.
iColumn = iColumn + 2.
cColumn = STRING(iColumn).
cRange = "B" + cColumn.
chWorkSheet:Range(cRange):Value = " Ja       Nej" NO-ERROR.
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = " Ja       Nej" NO-ERROR.
cRange = "F" + cColumn.
chWorkSheet:Range(cRange):Value = " Ja       Nej" NO-ERROR.
{EXCELFEL.I}      
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Dagbok finns" NO-ERROR.
cRange = "C" + cColumn.
chWorkSheet:Range(cRange):Value = "Tilläggsarbeten" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Följesedel finns" NO-ERROR.
iColumn = iColumn + 2.
cColumn = STRING(iColumn).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = " Ja       Nej" NO-ERROR.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Avrapportering i kundens system klar" NO-ERROR.
iColumn = iColumn + 2.
cColumn = STRING(iColumn).
cRange = "D" + cColumn.
chWorkSheet:Range(cRange):Value = " Ja       Nej" NO-ERROR.
iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Utestående leverantörsfakturor" NO-ERROR.
cRange = "E" + cColumn.
chWorkSheet:Range(cRange):Value = "Belopp" NO-ERROR.
iColumn = iColumn + 2.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Slutbesiktning, datum:" NO-ERROR.
iColumn = iColumn + 2.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + cColumn.
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = TRUE NO-ERROR.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
chWorkSheet:Range(cRange):Value = "Övrigt" NO-ERROR.

iColumn = iColumn + 1.
cColumn = STRING(iColumn).
cRange = "A" + cColumn.
radnrS = "E" + "50".
cRange = cRange + ":" + radnrS.
chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.
chWorkSheet:Range(cRange):Font:Bold = FALSE NO-ERROR.
iColumn = iColumn - 1.
cColumn = STRING(iColumn).

IF LENGTH(aonrtemp.ANM[1]) > 0 THEN DO:      
   ASSIGN
   radanmrak = 0
   retvar = 1
   ednum = 1
   ednum3 = LENGTH(aonrtemp.ANM[1])
   retvar = INDEX(aonrtemp.ANM[1],CHR(10),ednum)
   edtecken = 68
   edtext = aonrtemp.ANM[1]
   tidtext = "".  
   {ANMARK2.I}                             
   IF radanmrak < 7 THEN DO:
      iColumn = iColumn + (7 - radanmrak).
      cColumn = STRING(iColumn).
   END.
END.
ELSE DO:
   iColumn = iColumn + 4.
   cColumn = STRING(iColumn).
END.
iColumn = iColumn + 1.
RELEASE OBJECT chExcelApplication NO-ERROR.      
RELEASE OBJECT chWorkbook NO-ERROR.
RELEASE OBJECT chWorksheet NO-ERROR.

{EXCELFEL.I}      
PROCEDURE anmark_UI :
   DEFINE INPUT PARAMETER anmark AS INTEGER NO-UNDO.     
   IF radanmrak < 4 THEN DO:                 
      radanmrak = radanmrak + 1.
      iColumn = iColumn + 1.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      REPEAT:
         IF INDEX(tidtext,CHR(10),1) = 0 THEN LEAVE.
         REPLACE(tidtext,CHR(10),"").
      END.      
      chWorkSheet:Range(cRange):Value = tidtext NO-ERROR.
   END.     
   {EXCELFEL.I}      
END PROCEDURE.
