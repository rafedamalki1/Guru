/* AC2.P ********************* Definitions  ***************************** */
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
/* CREATE projektstatus.                                                   */
/* ASSIGN                                                                  */
/* projektstatus.PROJEKTNUMMER = "123456789"                               */
/* projektstatus.KUND = "kalle".                                           */
/*                                                                         */
/* CREATE projektstatus.                                                   */
/* ASSIGN                                                                  */
/* projektstatus.PROJEKTNUMMER = "987654321"                               */
/* projektstatus.KUND = "pelle".                                           */
/*                                                                         */
   filnamn = "\\pc112\delad\pro9\guru\export\".
/*                                                                         */

/* PUT '"Description","Language"' SKIP.                                    */
/* FOR EACH projektstatus:                                                 */
/*    EXPORT DELIMITER "," projektstatus.PROJEKTNUMMER projektstatus.KUND. */
/* END.                                                                    */
/* OUTPUT CLOSE.                                                           */

DEF VAR hAccess AS COM-HANDLE NO-UNDO.

   CREATE "Access.Application.9" hAccess 
   CONNECT TO filnamn + "db1.mdb".
   hAccess:Application:docmd:TransferText(0,,"myTable","\\pc112\delad\pro9\guru\export\data2.txt",true,).  
   RELEASE OBJECT hAccess.

  
 
