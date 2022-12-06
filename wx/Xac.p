/* ********************* Definitions  ***************************** */
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

CREATE projektstatus.
ASSIGN
projektstatus.PROJEKTNUMMER = "123456"
projektstatus.KUND = "snab1"
projektstatus.PROJEKTSTART = TODAY
projektstatus.PROJEKTAVSLUT = TODAY + 30
projektstatus.FAKTKOST = 5
projektstatus.AFAKTKOST = 10
projektstatus.TAKTKOST = 30.
CREATE projektstatus.
ASSIGN
projektstatus.PROJEKTNUMMER = "123457"
projektstatus.KUND = "snab2"
projektstatus.PROJEKTSTART = TODAY - 1
projektstatus.PROJEKTAVSLUT = TODAY + 35
projektstatus.FAKTKOST = 15
projektstatus.AFAKTKOST = 110
projektstatus.TAKTKOST = 130.
CREATE projektstatus.
ASSIGN
projektstatus.PROJEKTNUMMER = "123458"
projektstatus.KUND = "snab3"
projektstatus.PROJEKTSTART = TODAY - 5
projektstatus.PROJEKTAVSLUT = TODAY + 23
projektstatus.FAKTKOST = 25
projektstatus.AFAKTKOST = 210
projektstatus.TAKTKOST = 230.
CREATE projektstatus.
ASSIGN
projektstatus.PROJEKTNUMMER = "123459"
projektstatus.KUND = "snab4"
projektstatus.PROJEKTSTART = TODAY - 3
projektstatus.PROJEKTAVSLUT = TODAY + 45
projektstatus.FAKTKOST = 35
projektstatus.AFAKTKOST = 310
projektstatus.TAKTKOST = 330.
CREATE projektstatus.
ASSIGN
projektstatus.PROJEKTNUMMER = "123450"
projektstatus.KUND = "snab5"
projektstatus.PROJEKTSTART = TODAY - 3
projektstatus.PROJEKTAVSLUT = TODAY + 45
projektstatus.FAKTKOST = 35
projektstatus.AFAKTKOST = 310
projektstatus.TAKTKOST = 330.
OUTPUT TO "\\NTSERVER2\delad\pro9\guru\export\data.txt" APPEND.
FOR EACH projektstatus:
   EXPORT DELIMITER "," projektstatus.PROJEKTNUMMER projektstatus.KUND 
   projektstatus.PROJEKTSTART projektstatus.PROJEKTAVSLUT 
   projektstatus.FAKTKOST 
   projektstatus.AFAKTKOST projektstatus.TAKTKOST.
END.
OUTPUT CLOSE.

DEF VAR hAccess AS COM-HANDLE NO-UNDO.
CREATE "Access.Application.9" hAccess 
CONNECT TO "\\NTSERVER2\delad\pro9\guru\export\guru.mdb".
hAccess:Application:docmd:TransferText(0,,"projektstatus","\\NTSERVER2\delad\pro9\guru\export\data.txt",true,).  
RELEASE OBJECT hAccess.

  
 
