/* AC.P ********************* Definitions  ***************************** */
DEFINE INPUT PARAMETER aonrrec AS RECID NO-UNDO.
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE BUTTON cmdOLE
     LABEL "&OLE"
     SIZE 15 BY 1.14.
DEFINE FRAME frmMain
    cmdOLE AT ROW 2 COL 5    WITH 1 DOWN NO-BOX KEEP-TAB-ORDER OVERLAY
         SIDE-LABELS NO-UNDERLINE THREE-D
         AT COL 1 ROW 1 SIZE 25 BY 4.
DEFINE TEMP-TABLE projektstatus 
  FIELD PROJEKTNUMMER AS CHARACTER
  FIELD KUND AS CHARACTER
  FIELD PROJEKTSTART AS DATE
  FIELD PROJEKTAVSLUT AS DATE
  FIELD FAKTKOST AS DECIMAL
  FIELD AFAKTKOST AS DECIMAL
  FIELD TAKTKOST AS DECIMAL.
/* ******************** Trigger *********************************** */
FIND AONRTAB WHERE RECID(AONRTAB) = aonrrec NO-LOCK NO-ERROR.
CREATE projektstatus.
ASSIGN
projektstatus.PROJEKTNUMMER = AONRTAB.AONR.
FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.BESTID NO-LOCK NO-ERROR.
IF AVAILABLE OMRADETAB THEN DO:
   projektstatus.KUND = OMRADETAB.NAMN.
END.
ELSE DO:
   FIND FIRST BESTTAB WHERE BESTTAB.BESTID = AONRTAB.BESTID NO-LOCK NO-ERROR.
   IF AVAILABLE BESTTAB THEN projektstatus.KUND = BESTTAB.BESTNAMN.
   ELSE projektstatus.KUND = "Okänd".
END.
FOR EACH AONRTIDLAGE WHERE
AONRTIDLAGE.AONR = AONRTAB.AONR AND
AONRTIDLAGE.DELNR = AONRTAB.DELNR NO-LOCK:
   FIND FIRST TIDSLAGEN WHERE TIDSLAGEN.IDTIDLAG = AONRTIDLAGE.IDTIDLAG
   NO-LOCK NO-ERROR.
   IF AVAILABLE TIDSLAGEN THEN DO:
      IF globforetag = "GRAN" OR globforetag = "GKAL" THEN DO:
         IF TIDSLAGEN.TIDLAGE = "UPPLAGT" THEN DO:
            ASSIGN
            projektstatus.PROJEKTSTART = AONRTIDLAGE.DATUM1.
         END.
         IF TIDSLAGEN.TIDLAGE = "AVSLUTAT" THEN DO:
            ASSIGN
            projektstatus.PROJEKTAVSLUT = AONRTIDLAGE.DATUM1.
         END.
      END.
      ELSE DO:
         IF TIDSLAGEN.TIDLAGE = "UPPLAGT" THEN DO:
            ASSIGN
            projektstatus.PROJEKTSTART = AONRTIDLAGE.DATUM1.
         END.
         IF TIDSLAGEN.TIDLAGE = "ARBETET KLART" THEN DO:
            ASSIGN
            projektstatus.PROJEKTAVSLUT = AONRTIDLAGE.DATUM1.
         END.
      END.
   END.
END.
ASSIGN
projektstatus.FAKTKOST = 1
projektstatus.AFAKTKOST = 2
projektstatus.TAKTKOST = 3.
/* CREATE projektstatus.                    */
/* ASSIGN                                   */
/* projektstatus.PROJEKTNUMMER = "123457"   */
/* projektstatus.KUND = "snab"              */
/* projektstatus.PROJEKTSTART = TODAY - 1   */
/* projektstatus.PROJEKTAVSLUT = TODAY + 35 */
/* projektstatus.FAKTKOST = 15              */
/* projektstatus.AFAKTKOST = 110            */
/* projektstatus.TAKTKOST = 130.            */
IF globforetag = "ELPA" THEN DO:
   filnamn = "\\pc112\delad\pro9\guru\export\".   
END.
ELSE IF globforetag = "GRAN" THEN DO:
   filnamn = "d:\elpool\delad\pro9s\export\".
END.
ELSE DO:
   filnamn = "d:\delad\server\pro9s\export\".
END.
{AMERICANEUROPEAN.I}
OUTPUT TO VALUE(filnamn + "data.txt").
PUT '"PROJEKTNUMMER";"KUND";"PROJEKTSTART";"PROJEKTAVSLUT";"FAKTURERAD KOSTNAD";"AKTIVERAD KOSTNAD";"TOTAL KOSTNAD"' SKIP.
FOR EACH projektstatus:
   EXPORT DELIMITER ";" projektstatus.PROJEKTNUMMER projektstatus.KUND projektstatus.PROJEKTSTART projektstatus.PROJEKTAVSLUT projektstatus.FAKTKOST 
   projektstatus.AFAKTKOST projektstatus.TAKTKOST.
END.
OUTPUT CLOSE.
{EUROPEANAMERICAN.I}
DEF VAR hAccess AS COM-HANDLE NO-UNDO.
IF globforetag = "ELPA" THEN DO:
   CREATE "Access.Application.9" hAccess 
   CONNECT TO filnamn + "guru.mdb".
   hAccess:Application:docmd:TransferText(0,,"projektstatus","\\pc112\delad\pro9\guru\export\data.txt",true,).  
   RELEASE OBJECT hAccess.
END.
IF globforetag = "GRAN" THEN DO:
   CREATE "Access.Application.9" hAccess 
   CONNECT TO filnamn + "guru.mdb".
   hAccess:Application:docmd:TransferText(0,,"projektstatus","d:\elpool\delad\pro9s\export\data.txt",true,).  
   RELEASE OBJECT hAccess.
END.
ELSE DO:
   CREATE "Access.Application.9" hAccess 
   CONNECT TO filnamn + "guru.mdb".
   hAccess:Application:docmd:TransferText(0,,"projektstatus","d:\delad\server\pro9s\export\data.txt",true,).  
   RELEASE OBJECT hAccess.
END.

  
 
