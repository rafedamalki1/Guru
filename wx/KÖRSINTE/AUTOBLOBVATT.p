/*AUTOBLOBVATT.P Körs ej
hämtar blobbar

*/
&Scoped-define NEW NEW 
{GLOBVAR2DEL1.I}
DEFINE VARIABLE prognamn AS CHARACTER FORMAT "X(50)" NO-UNDO.                
DEFINE VARIABLE filename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE skick  AS LOGICAL.        /* Email status  */
DEFINE VARIABLE efel AS CHARACTER.      /* Status txt  */
DEFINE VARIABLE utfil AS CHARACTER.
DEFINE VARIABLE sokfil AS CHARACTER.
DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO.
{VALDBDEF.I}
{VALDBVESABPRO.I}


   /*
kommando2 = "C:\ftrack\Bin\Appstart.exe Guru_UppdateGURU".
OS-COMMAND SILENT VALUE(kommando2).      
*/   
FIND FIRST valdbtemp WHERE valdbtemp.ORDNING = 11 NO-LOCK NO-ERROR.
IF NOT AVAILABLE valdbtemp THEN DO:
   FIND FIRST valdbtemp NO-LOCK NO-ERROR.
END.
IF AVAILABLE valdbtemp THEN DO: 
   dbfilename = valdbtemp.DBNAMN.
   {AppSprinSet.I}
   RUN val_UI.
   IF CONNECTED(LDBNAME(1)) THEN DO:         
      RUN ALIASSATT.P.
      RUN CONAPPS.P.
      RUN BLOBBACK.P.
      DISCONNECT VALUE(LDBNAME(1)) NO-ERROR. 
   END.           
END.
IF Guru.Konstanter:appcon THEN Guru.Konstanter:appcon = Guru.Konstanter:apphand:DISCONNECT().
DELETE OBJECT Guru.Konstanter:apphand NO-ERROR.

PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.       
END PROCEDURE.

  

