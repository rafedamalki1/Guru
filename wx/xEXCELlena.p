DEFINE VARIABLE cDocPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDocName AS CHARACTER NO-UNDO.
DEFINE VARIABLE wdGoToBookmark AS INTEGER NO-UNDO INITIAL -1.

DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE chWord AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chDoc  AS COM-HANDLE     NO-UNDO. 
DEFINE VARIABLE chWord2 AS COM-HANDLE     NO-UNDO.
DEFINE VARIABLE chDoc2  AS COM-HANDLE     NO-UNDO.
/*
DEFINE VARIABLE SELECTION AS COM-HANDLE NO-UNDO.
*/
DEFINE VARIABLE bokvalue AS CHARACTER NO-UNDO.
DEFINE VARIABLE bokname AS CHARACTER NO-UNDO.
DEFINE VARIABLE bkRange AS COM-HANDLE NO-UNDO.
{GLOBVAR2DEL1.I}
{EXECLIN.I}

kommando = "C:\LENA\CMBTEST.doc".
/*kommando = "\\SERVER05\d\elpool\elplo\MUA Jord.doc".*/ 

CREATE "Excel.Application" chExcelApplication.
 chExcelApplication:Visible = TRUE.
{OPENEXCEL.I}
chWorkbook = chExcelApplication:Workbooks:OPEN(kommando).

chWorkSheet = chExcelApplication:Sheets:Item(1) NO-ERROR.
   


PROCEDURE inbokvalue_UI.
   DEFINE INPUT PARAMETER bokname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bokvalue AS CHARACTER NO-UNDO.
   DEFINE VARIABLE wdFieldAutoText AS INTEGER NO-UNDO. /*70 text 71 kryss 83 cmb-box*/
   DEFINE VARIABLE formfield AS COM-HANDLE NO-UNDO.
   IF chDoc:Bookmarks:EXISTS( bokname ) = TRUE THEN DO:      
      
      
      chDoc:FormFields:item(bokname):RESULT = bokvalue .
      
   END.
END PROCEDURE.
PROCEDURE inboklog_UI.
   DEFINE INPUT PARAMETER bokname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bokvalue AS LOGICAL NO-UNDO.
   IF chDoc:Bookmarks:EXISTS( bokname ) = TRUE THEN DO:
      chWord:SELECTION:GOTO (wdGoToBookmark BY-VARIANT-POINTER,,,bokname BY-VARIANT-POINTER).
      chDoc:FormFields:item(bokname):CheckBox:Value  = bokvalue .  

   END.
END PROCEDURE.

