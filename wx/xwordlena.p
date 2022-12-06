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


/*kommando = "C:\LENA\MUA Jord.doc".*/
kommando = "\\SERVER05\d\elpool\elplo\MUA Jord.doc". 

CREATE "Word.Application" chWord NO-ERROR.

IF ERROR-STATUS:ERROR = FALSE THEN DO:
   chDoc = chWord:Documents:Open(kommando,,,,,,,,,).
   chWord:VISIBLE = TRUE.
   
   chDoc:Protect(2) no-error.
   
   chDoc = chWord:ActiveDocument.
   RUN inbokvalue_UI (INPUT "KontaktNamn" ,INPUT "mika OLSSON").
   RUN inbokvalue_UI (INPUT "KontaktAdress" ,INPUT "seb OLSSON").   
   RUN inboklog_UI (INPUT "Kryssruta1" ,INPUT true).
   
  /*
   kommando = "c:\temp\nydok2.doc".
   chDoc:SaveAs(kommando,,,,,,,,,).
   
   NO-RETURN-VALUE chDoc:CLOSE().
   NO-RETURN-VALUE chWord:QUIT().
   RELEASE OBJECT chDoc NO-ERROR.
   RELEASE OBJECT chWord NO-ERROR.
   chDoc = ?.
   chWord = ?.
   CREATE "Word.Application" chWord NO-ERROR.
   chDoc = chWord:Documents:Open(kommando,,,,,,,,,).
   chWord:VISIBLE = TRUE.
   chDoc:Protect(2).
   */
   
   /*
   RUN inbokvalue_UI (INPUT "KontaktNamn" ,INPUT "sanders OLSSON").
   */
   /*chDoc:Protect(2).*/
/*   RUN inboklog_UI (INPUT "Kryss1" ,INPUT TRUE).*/
END.

/* funkar
      wdFieldAutoText = 70.
      chDoc:FormFields:ADD(bkRange BY-POINTER,wdFieldAutoText) no-error.
      chDoc:Bookmarks:ADD(bokname, bkRange BY-VARIANT-POINTER).
      chDoc:FormField:item(bokname):RESULT = bokvalue.
      chDoc:FormFields:ITEM(bokname):TextInput:Clear.
      
      chWord:SELECTION:GOTO (wdGoToBookmark BY-VARIANT-POINTER,,,bokname BY-VARIANT-POINTER).
      bkRange = chWord:SELECTION:Range.
      */


PROCEDURE inbokvalue_UI.
   DEFINE INPUT PARAMETER bokname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bokvalue AS CHARACTER NO-UNDO.
   DEFINE VARIABLE wdFieldAutoText AS INTEGER NO-UNDO. /*70 text 71 kryss 83 cmb-box*/
   DEFINE VARIABLE formfield AS COM-HANDLE NO-UNDO.
   IF chDoc:Bookmarks:EXISTS( bokname ) = TRUE THEN DO:      
      
      /**/

      /*MESSAGE  chDoc:FormFields:ITEM(bokname):type
      VIEW-AS ALERT-BOX.*/
      
      chDoc:FormFields:item(bokname):RESULT = bokvalue .
      /**/
      
      
      
      /*
      chDoc:FormFields:ITEM(bokname):RESULT = bokvalue.
      
      bkrange:text = bokvalue.
      
      chDoc:bkRange:Fields
      chDoc:FormField(4):RESULT = bokvalue.
      */
      /* funkar
      wdFieldAutoText = 70.
      chDoc:FormFields:ADD(bkRange BY-POINTER,wdFieldAutoText) no-error.
      chDoc:Bookmarks:ADD(bokname, bkRange BY-VARIANT-POINTER).
      chDoc:FormField:item(bokname):RESULT = bokvalue.
      */
      /*
      REPEAT:
         wdFieldAutoText = wdFieldAutoText + 1.
         IF wdFieldAutoText > 100 THEN LEAVE.
        
          chword:ActiveDocument:FormFields:ADD(bkRange BY-POINTER,wdFieldAutoText) no-error.
          IF NOT ERROR-STATUS:ERROR AND ERROR-STATUS:NUM-MESSAGES = 0 THEN MESSAGE wdFieldAutoText
                                                                           VIEW-AS ALERT-BOX.
      END.
      */
      /*
       chword:ActiveDocument:FormFields(1):TextInput:Clear.
      chword:ActiveDocument:FormFields(bokname):RESULT = bokvalue.
      
      MESSAGE chword:ActiveDocument:FormFields(bokname):type
      VIEW-AS ALERT-BOX.
      */
      
      
      
      
      /*chword:ActiveDocument:Bookmarks:ADD(bokname, bkRange BY-VARIANT-POINTER).*/
      /*
      chword:ActiveDocument:FormFields:ADD(bkRange BY-VARIANT-POINTER , bokvalue  ).
      */
      /*
      chword:ActiveDocument:FormFields:ADD(bkRange BY-POINTER,wdFieldAutoText).
       chword:ActiveDocument:FormFields:ADD(bkRange BY-POINTER,wdFieldAutoText).
       */
/*
    IF bokvalue <> ? THEN chWord:SELECTION:TypeText ( bokvalue )  NO-ERROR.*/
      
      /*   bkRange = chWord:ActiveDocument:Bookmarks(bokname):Range .*/
      
      /*
      chword:ActiveDocument:Bookmarks:ADD(bokname, bkRange BY-VARIANT-POINTER).*/
   END.
END PROCEDURE.
PROCEDURE inboklog_UI.
   DEFINE INPUT PARAMETER bokname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bokvalue AS LOGICAL NO-UNDO.
   IF chDoc:Bookmarks:EXISTS( bokname ) = TRUE THEN DO:
      chWord:SELECTION:GOTO (wdGoToBookmark BY-VARIANT-POINTER,,,bokname BY-VARIANT-POINTER).
      chDoc:FormFields:item(bokname):CheckBox:Value  = bokvalue .  
      /*chWord:VALUE(bokname) = TRUE. */
      /*chDoc:Bookmarks:ITEM(bokname)  = TRUE. */
      /*chDoc:FormFields:item(bokname):RESULT = bokvalue .*/
      /*chDoc:wdFieldFormCheckBox(bokname) = TRUE.*/
      
      /*chWord:Documents:Item(1):Range(0,chWord:Documents:Item(1):Characters:Count):VALUE(bokname) = TRUE. */
      /*chWord:Documents:Item(1):Range(0,chWord:Documents:Item(1):Characters:Count):font:NAME = "Arial".*/
      
      /*Selection.FormFields.Add Range:=Selection.Range, Type:=wdFieldFormCheckBox*/
      /*chWord:Selection:TypeText ( bokvalue ) NO-ERROR. */
      
      /*IF bokvalue <> ? THEN chDoc:FormFields(bokname) = bokvalue.*/
      /*ActiveDocument.FormFields("Text1").Result = "John Doe"*/


   END.
END PROCEDURE.

/*PROCEDURE inbokvalue_UI.
   DEFINE INPUT PARAMETER bokname AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER bokvalue AS CHARACTER NO-UNDO.
   IF chDoc:Bookmarks:EXISTS( bokname ) = TRUE THEN DO:
      chWord:SELECTION:GOTO (wdGoToBookmark BY-VARIANT-POINTER,,,bokname BY-VARIANT-POINTER).  
      IF bokvalue <> ? THEN chWord:SELECTION:TypeText ( bokvalue ) NO-ERROR. 
      /*IF bokvalue <> ? THEN chDoc:FormFields(bokname) = bokvalue.*/
      /*ActiveDocument.FormFields("Text1").Result = "John Doe"*/


   END.
END PROCEDURE.*/
