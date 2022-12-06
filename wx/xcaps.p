/*xcaps.p*/   
DEFINE VARIABLE kommandoprog AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE tider AS CHARACTER NO-UNDO. 
DEFINE VARIABLE indate AS DATE NO-UNDO. 
DEFINE VARIABLE prognamnvar AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE prognamnvarstor AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progkopia AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE progque AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnk AS CHARACTER NO-UNDO.
DEFINE VARIABLE importvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE importvark AS CHARACTER NO-UNDO.
DEFINE VARIABLE startnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnstart AS CHARACTER NO-UNDO.
DEFINE VARIABLE filnamnslut AS CHARACTER NO-UNDO.

DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(132)".  
DEFINE TEMP-TABLE tidinkonto
   FIELD TIN AS CHARACTER FORMAT "X(132)".  
DEFINE TEMP-TABLE infil
   FIELD PROGNAMN AS CHARACTER FORMAT "X(78)"    
   INDEX PRO IS PRIMARY PROGNAMN.

kommandoprog = "\\server3\D\DELAD\PRO9\guru\stora.TXT".
startnamn = "\\server3\D\DELAD\PRO9\guru\".
RUN kommando_UI (INPUT "waonr\").
RUN kommando_UI (INPUT "wavt\").
RUN kommando_UI (INPUT "wber\").
RUN kommando_UI (INPUT "wdep\").
RUN kommando_UI (INPUT "wextra\").
RUN kommando_UI (INPUT "wfakt\").
RUN kommando_UI (INPUT "wflextid\").
RUN kommando_UI (INPUT "wmark\").
RUN kommando_UI (INPUT "wmtrl\").
RUN kommando_UI (INPUT "wplan\").
RUN kommando_UI (INPUT "wstor\").
RUN kommando_UI (INPUT "wtid\").
RUN kommando_UI (INPUT "caonr\").
RUN kommando_UI (INPUT "cber\").
RUN kommando_UI (INPUT "cdep\").
RUN kommando_UI (INPUT "cextra\").
RUN kommando_UI (INPUT "cfakt\").
RUN kommando_UI (INPUT "cflextid\").
RUN kommando_UI (INPUT "cmark\").
RUN kommando_UI (INPUT "cmtrl\").
RUN kommando_UI (INPUT "cplan\").
RUN kommando_UI (INPUT "cstor\").
RUN kommando_UI (INPUT "ctid\").

PROCEDURE del_UI:
   OS-DELETE VALUE(kommandoprog) NO-ERROR.
END PROCEDURE.
PROCEDURE kommando_UI:
   DEFINE INPUT PARAMETER varbil AS CHARACTER NO-UNDO.
   RUN del_UI.
   kommando = "DIR/a:-d /b " + startnamn + varbil + "*.p > \\server3\D\DELAD\PRO9\guru\stora.TXT".   
   OS-COMMAND SILENT VALUE(kommando).
   RUN kopiera_UI (INPUT varbil).
END PROCEDURE.
PROCEDURE kopiera_UI:
   DEFINE INPUT PARAMETER varbil AS CHARACTER NO-UNDO.
   INPUT FROM VALUE(kommandoprog) NO-ECHO.
   REPEAT:
      DO TRANSACTION: 
         CREATE infil.
         ASSIGN.
         IMPORT infil   NO-ERROR.
      END.
   END.
   INPUT CLOSE.
   FOR EACH infil:   
      IF INDEX(infil.PROGNAMN,".p") = 0 THEN DO:       
         IF INDEX(infil.PROGNAMN,".i") = 0 THEN DO:
            DELETE infil.
            NEXT.
         END.
         ELSE DO:
            infil.PROGNAMN = CAPS(infil.PROGNAMN).
            infil.PROGNAMN = REPLACE(infil.PROGNAMN,".I",".i").
         END.
      END.
      ELSE DO:
         infil.PROGNAMN = CAPS(infil.PROGNAMN).
         infil.PROGNAMN = REPLACE(infil.PROGNAMN,".P",".p").
      END.
   END.
   FOR EACH infil: 
      prognamnvar = startnamn + varbil + infil.PROGNAMN.
      prognamnvarstor = startnamn + infil.PROGNAMN.
      OS-RENAME VALUE(prognamnvar) VALUE(prognamnvarstor).
      OS-COPY VALUE(prognamnvarstor) VALUE(prognamnvar) .
      OS-DELETE VALUE(prognamnvarstor).
   END.
   OS-DELETE VALUE(kommandoprog) NO-ERROR.
END PROCEDURE.
