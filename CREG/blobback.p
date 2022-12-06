/*BLOBBACK.P*/






                              

DEFINE VARIABLE upptime AS INTEGER NO-UNDO.
DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.    
DEFINE VARIABLE sendat AS DATE NO-UNDO.
DEFINE VARIABLE sentid AS INTEGER NO-UNDO.
DEFINE VARIABLE dirlist AS CHARACTER NO-UNDO FORMAT "x(72)".
DEFINE VARIABLE attribut AS CHARACTER NO-UNDO .
DEFINE VARIABLE cdirlist AS CHARACTER NO-UNDO.
DEFINE VARIABLE cdir AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempi AS INTEGER NO-UNDO.
DEFINE VARIABLE clientpath AS CHARACTER NO-UNDO.
DEFINE VARIABLE numfil AS INTEGER NO-UNDO.
DEFINE VARIABLE antalupp AS CHARACTER NO-UNDO.
DEFINE VARIABLE logresult AS LOGICAL NO-UNDO.
DEFINE VARIABLE soklog AS LOGICAL NO-UNDO.
DEFINE VARIABLE tempquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
{FINNSDYNBLOB.I}
RUN huvud_UI.
RUN uppdatera_UI (INPUT 2).
RUN deleteproc_UI IN blobproch.
IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.

PROCEDURE huvud_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes
------------------------------------------------------------------------------*/   
   DEFINE VARIABLE lefti AS INTEGER NO-UNDO.
   DEFINE VARIABLE righti AS INTEGER NO-UNDO.
   DEFINE VARIABLE tempch AS CHARACTER NO-UNDO.
   RUN blobladda_UI IN blobproch (INPUT "").

END PROCEDURE.

PROCEDURE uppdatera_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes
------------------------------------------------------------------------------*/   
   DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
   upptime = TIME.
   /* detta körs innan autoblobvatt
   kommando = "C:\ftrack\Bin\Appstart.exe Guru_UppdateGURU".
   OS-COMMAND SILENT VALUE(kommando).         
   */
   RUN blobladdaen_UI IN blobproch (INPUT "vss.pf").
   FIND FIRST blobinfotemp NO-LOCK NO-ERROR.
   IF AVAILABLE blobinfotemp THEN RUN uppklient_UI.
   FIND FIRST blobinfotemp NO-LOCK NO-ERROR.
   IF NOT AVAILABLE blobinfotemp THEN RETURN.
   RUN senastuppdat_UI IN blobproch (OUTPUT sendat, OUTPUT sentid).
   /*SKAPAR BLOBCOMP FÖR SENAST UPPDATERINGEN*/
   IF sendat = ? THEN RUN skapauppdat_UI IN blobproch.
   IF sendat = ? OR vart = 1 THEN DO:
      FOR EACH blobinfotemp NO-LOCK:
         RUN uppklient_UI.         
      END.      
   END.
   ELSE DO:
      FOR EACH blobinfotemp WHERE blobinfotemp.SKAPDAT >= sendat NO-LOCK:
         IF blobinfotemp.SKAPDAT > sendat THEN DO:
            RUN uppklient_UI.
         END.
         ELSE IF blobinfotemp.SKAPDAT = sendat AND blobinfotemp.INTID > sentid THEN DO:
            RUN uppklient_UI.           
         END.         
      END.
   END.
   FOR EACH blobinfotemp NO-LOCK:
      ACCUMULATE blobinfotemp.SKAPDAT (MAXIMUM).      
   END.
   sendat = ACCUM MAXIMUM blobinfotemp.SKAPDAT.
   FOR EACH blobinfotemp WHERE blobinfotemp.SKAPDAT = sendat NO-LOCK:
      ACCUMULATE blobinfotemp.INTID (MAXIMUM).      
   END.
   sentid = ACCUM MAXIMUM blobinfotemp.INTID.
   RUN andrauppdat_UI IN blobproch (INPUT sendat, INPUT sentid).
   
END PROCEDURE.

PROCEDURE uppklient_UI :
/*------------------------------------------------------------------------------
  Purpose:     
  Parameters:  <none>
  Notes
------------------------------------------------------------------------------*/   
   
   DEFINE VARIABLE mappvar AS CHARACTER NO-UNDO.
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      mappvar = SUBSTRING(Guru.Konstanter:guruvar,1,LENGTH(Guru.Konstanter:guruvar) - 1) +  blobinfotemp.VARTWC + "\".
   END.
   ELSE DO:
      mappvar = SUBSTRING(Guru.Konstanter:guruvar,1,LENGTH(Guru.Konstanter:guruvar) - 1) + blobinfotemp.VART + "\".
      
   END.
   RUN klientprog_UI IN blobproch (INPUT blobinfotemp.ID, INPUT mappvar).

END PROCEDURE.


