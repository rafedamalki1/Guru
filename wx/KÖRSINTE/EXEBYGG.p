/*EXEBYGG.P*/

/* DEFINE SHARED VARIABLE globforetag LIKE FORETAG.FORETAG NO-UNDO. */
         

DEFINE VARIABLE felexcel AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER FORMAT "X(20)" NO-UNDO.
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

DEFINE VARIABLE fnamn AS CHARACTER NO-UNDO.
/*{EGENBEN.I}*/
&Scoped-define SHARED SHARED  
{MARKVAL.I}                         
{GLOBVAR2DEL1.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
IF globforetag = "GRAN" OR globforetag = "VAST" OR globforetag = "VELD" OR globforetag = "LECA"  
OR globforetag = "ELPA"   THEN fnamn = "BYGGLOV.XLS".
ELSE IF globforetag = "UMEA" OR globforetag = "UMBR" THEN fnamn = "BYGGLOVUM.XLS".
ELSE fnamn = "BYGGLOVA.XLS".
IF Guru.Konstanter:appcon THEN RUN FINNSTABELL.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "BLOBINFO", OUTPUT bloblog).
ELSE RUN FINNSTABELL.P (INPUT "BLOBINFO", OUTPUT bloblog).
IF bloblog = TRUE THEN DO:
   {FINNSDYNBLOB.I}
   DEFINE VARIABLE resid AS INTEGER NO-UNDO.
   RUN blobfil_UI IN blobproch (INPUT fnamn, OUTPUT resid).
   IF resid = ? THEN DO:
      kommando = SEARCH(fnamn).      
   END.
   ELSE DO:
      FIND FIRST blobinfotemp WHERE blobinfotemp.ID = resid NO-LOCK NO-ERROR.
      RUN blobopen_UI IN blobproch (INPUT blobinfotemp.FILNAMN, OUTPUT kommando).
      
   END.
   RUN deleteproc_UI IN blobproch.
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
END.
ELSE kommando = SEARCH(fnamn).   
IF kommando = ? THEN DO:
   MESSAGE "Hittade inte " fnamn VIEW-AS ALERT-BOX.
   RETURN.       
END.   
kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
{SESSIONTEMPDIR.I}
IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
IF Guru.GlobalaVariabler:plusaonr = "" OR Guru.GlobalaVariabler:plusaonr = ? THEN DO:
   kommando2 = kommando2 + fnamn.
END.
ELSE DO:
   kommando2 = kommando2 + TRIM (Guru.GlobalaVariabler:plusaonr) + TRIM(STRING(Guru.GlobalaVariabler:plusdnr,Guru.Konstanter:varforetypchar[1])) + fnamn.
END.

OS-COPY VALUE(kommando) VALUE(kommando2).
kommando = kommando2.


   FIND FIRST markval NO-LOCK NO-ERROR.
    
   CREATE "Excel.Application" chExcelApplication.
   chExcelApplication:Visible = TRUE.
/*    kommando = SEARCH("bygglov.xls").                                                          */
/*    IF kommando = ? THEN DO:                                                                   */
/*        MESSAGE "Hittade inte bygglov.xls" VIEW-AS ALERT-BOX.                                  */
/*        RETURN.                                                                                */
/*    END.                                                                                       */
/*    /*IF globforetag = "ELPA" THEN kommando = "\\PC112\delad\pro9\guru\wmark\bygglov.xls".*/ */
/*    IF INDEX(kommando,"GURU") = 0 THEN DO:                                                     */

/*       kommando = wtidvar + "bygglov.xls".                                                     */
/*    END.                                          
                                                */
                                                
   {OPENEXCEL.I}
   chWorkbook = chExcelApplication:Workbooks:OPEN(kommando) NO-ERROR.
  
   chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.   
   /*chWorkSheet:Name = "Test Chart".*/
   
   /*FIND FIRST MARKAGARE WHERE MARKAGARE.MARKAGARE = markval.MARKAGARE
   AND MARKAGARE.PERSONNUMMER = markval.PERSONNUMMER NO-LOCK NO-ERROR.*/
   iColumn = 4.
   cColumn = STRING(iColumn).
   cRange = "K" + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.   
   chWorkSheet:Range(cRange):Value = STRING(TODAY,"9999-99-99") NO-ERROR.
   iColumn = 8.
   cColumn = STRING(iColumn).
   cRange = "K" + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = 8 NO-ERROR.    
   chWorkSheet:Range(cRange):Value = "Sökandes ref:" NO-ERROR.
   iColumn = 16.
   cColumn = STRING(iColumn).
   cRange = "B" + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.   
   chWorkSheet:Range(cRange):Value = markval.BETECKNING NO-ERROR.
   
   cColumn = STRING(iColumn).
   cRange = "J" + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.   
   chWorkSheet:Range(cRange):Value = markval.MARKAGARE NO-ERROR.  

   /*iColumn = 18.
   cColumn = STRING(iColumn).
   cRange = "O" + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL".
   chWorkSheet:Range(cRange):Font:SIZE = 10.   
   chWorkSheet:Range(cRange):Value = MARKAGARE.TELEFON2.  

   
   cRange = "O" + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL".
   chWorkSheet:Range(cRange):Font:SIZE = 10.   
   chWorkSheet:Range(cRange):Value = MARKAGARE.TELEFON.  */

   iColumn = 18.
   cColumn = STRING(iColumn).
   cRange = "B" + cColumn.
   chWorkSheet:Range(cRange):Font:NAME = "ARIAL" NO-ERROR.
   chWorkSheet:Range(cRange):Font:SIZE = 10 NO-ERROR.   
   chWorkSheet:Range(cRange):Value = markval.GATUADRESS + " " + markval.POSTNUMMER + " " + markval.POSTADRESS NO-ERROR.  
      
   RELEASE OBJECT chExcelApplication NO-ERROR.      
   RELEASE OBJECT chWorkbook NO-ERROR.
   RELEASE OBJECT chWorksheet NO-ERROR.
   
   {EXCELFEL.I}