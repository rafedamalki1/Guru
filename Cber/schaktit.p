/*
               KSV Editor
    Copyright: (C) 2000-2003 Serguey Klimoff (bulkl0DD)
     Filename: SCHAKTIT.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2008.08.18 11:50 ELPAO   
     Modified: 2008.09.18 08:20 ELPAO    
     Modified: 
*/

{SCHAKTAWID.I}
{ALLDEF.I}
&Scoped-define SHARED SHARED
{SCADMIN.I}
{GLOBVAR2DEL1.I}
&Scoped-define SHARED
&Scoped-define NEW
{EXTRADATA.I}
/*variabler osv*/
DEFINE SHARED VARIABLE valaonr AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE valomrade AS CHARACTER NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE temppunkth AS HANDLE NO-UNDO.
DEFINE VARIABLE tempokabh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempakabh AS HANDLE NO-UNDO.
DEFINE VARIABLE temppkabh AS HANDLE NO-UNDO.
DEFINE VARIABLE tempschakth AS HANDLE NO-UNDO.
DEFINE VARIABLE schaktapph AS HANDLE NO-UNDO.
DEFINE VARIABLE pidvalue AS INTEGER NO-UNDO.
DEFINE VARIABLE sidvalue AS INTEGER NO-UNDO.
DEFINE VARIABLE klidvalue AS INTEGER NO-UNDO.
DEFINE VARIABLE klidadd AS INTEGER NO-UNDO.
DEFINE VARIABLE valdkabel AS CHARACTER NO-UNDO.
ON 'CHOOSE' OF BTN_KABTILLSC PERSISTENT RUN btnner_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_KABFRSC PERSISTENT RUN btnupp_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_KABTILLP PERSISTENT RUN btnhoger_UI IN THIS-PROCEDURE.
ON 'CHOOSE' OF BTN_KABFRP PERSISTENT RUN btnvanster_UI IN THIS-PROCEDURE.
ON 'VALUE-CHANGED' OF BRW_PUNKT PERSISTENT RUN brwpunkt_UI IN THIS-PROCEDURE. 
ON 'VALUE-CHANGED' OF BRW_OKAB PERSISTENT RUN brwokab_UI IN THIS-PROCEDURE. 
ON 'VALUE-CHANGED' OF BRW_AKAB PERSISTENT RUN brwakab_UI IN THIS-PROCEDURE. 
ON 'VALUE-CHANGED' OF BRW_SCHAKT PERSISTENT RUN brwschakt_UI IN THIS-PROCEDURE. 
RUN main_UI.

PROCEDURE main_UI :   
   {ALLSTARTDYN.I}     
   FIND FIRST tempytbelagg WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tempytbelagg THEN DO:
      tthandle = ?.
      IF Guru.Konstanter:appcon THEN DO:
         RUN DYNLADDATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT
            (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "YTBELAGG", INPUT "").
      END.
      ELSE DO:
         RUN DYNLADDATEMP.P PERSISTENT SET laddaproch
            (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "YTBELAGG", INPUT "").
      END.
         
   END.
   /* fylla i schakt-lista */   
   tthandle = TEMP-TABLE hdschakttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDSCHAKT", INPUT " WHERE BERNR = '" + valaonr + "' AND OMRADE = '" + valomrade + "' ").
   /* förläggning */
   tthandle = TEMP-TABLE tempsamforlagg:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "SAMFORLAGG", INPUT "").
   tthandle = TEMP-TABLE tempforlagg:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "FORLAGG", INPUT "").
   tthandle = TEMP-TABLE tempytbelagg:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "YTBELAGG", INPUT "").   
   /*fylla i kabellinje tt att jämföra med*/
/*   RUN fyllakabellinje_UI.*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT.                  
   END.
   ELSE DO:
      RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
   END.   
   RUN extrakabel_UI.
   CMB_KAB:LIST-ITEMS = "".
   FOR EACH tempkabel USE-INDEX KABID NO-LOCK:
      status-ok = CMB_KAB:ADD-LAST(tempkabel.KABEL). 
      CMB_KAB:SCREEN-VALUE = tempkabel.KABEL. /*blir sista*/
   END.
   RUN openbdynspec_UI IN brwproc[6]. /* schakt */  
   RUN openbdynspec_UI IN brwproc[7]. /* for */  
   RUN openbdynspec_UI IN brwproc[8]. /* sam */  
   RUN openbdynspec_UI IN brwproc[9]. /* yt */    

END PROCEDURE.


PROCEDURE brwschakt_UI :
   RUN schaktval_UI.
END PROCEDURE.

PROCEDURE btnner_UI :
   RUN kabeltillschakt_UI.
END PROCEDURE.

PROCEDURE btnupp_UI :
   RUN kabelfranschakt_UI.
END PROCEDURE.

PROCEDURE btnhoger_UI :
   RUN kabeltillpunkt_UI.
END PROCEDURE.

PROCEDURE btnvanster_UI :
   RUN kabelfranpunkt_UI.
END PROCEDURE.

PROCEDURE brwokab_UI :
   RUN okabval_UI.
END PROCEDURE.


PROCEDURE brwakab_UI :
   RUN akabval_UI.
END PROCEDURE.

PROCEDURE brwpunkt_UI :
   RUN punktval_UI.   
END PROCEDURE.

PROCEDURE schaktval_UI :
   RUN selectfocschakt_UI.
   sidvalue = hdschakttemp.sid.
   /*FYLLA PUNKTER*/
   RUN fyllallakablar_UI.
   RUN fyllaok_UI.
   RUN fyllapunkt_UI.
   RUN fyllakabellinje_UI.
   
END PROCEDURE.

PROCEDURE okabval_UI :
   RUN selectfocokab_UI.
   klidvalue = okltemp.klid.
   RUN fyllamk_UI.
END PROCEDURE.


PROCEDURE akabval_UI :
   RUN selectfocakab_UI.
   klidvalue = akltemp.klid.
   RUN fyllamk_UI.
END PROCEDURE.


PROCEDURE punktval_UI :
   RUN selectfocpunkt_UI.
   pidvalue = hdpunkttemp.PID.   
   RUN fyllapk_UI.   
   
END PROCEDURE.


/* kolla om det finns punkter med samma klid före/efter nuvarande klid i kabellinje */
PROCEDURE forlkoll_UI :
   DEFINE INPUT PARAMETER klidd AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER pidz1 AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER pidz2 AS INTEGER NO-UNDO.
   DEFINE VARIABLE pidd1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE pidd2 AS INTEGER NO-UNDO.
   DEFINE VARIABLE fid AS INTEGER NO-UNDO.   
   FIND hdschaktfortemp WHERE hdschaktfortemp.pid1 = pidd1 AND hdschaktfortemp.pid2 = pidd2 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE hdschaktfortemp THEN DO:
      FIND LAST hdschaktfortemp USE-INDEX FORSATT NO-ERROR.
      IF AVAILABLE hdschaktfortemp THEN DO:
         fid = hdschaktfortemp.FID + 1.
      END.
      ELSE DO:
         fid = 1.
      END.
      CREATE hdschaktfortemp.
      ASSIGN
      hdschaktfortemp.BERNR = integer(valaonr)
      hdschaktfortemp.OMRADE = valomrade      
      hdschaktfortemp.PID1 = pidz1       
      hdschaktfortemp.PID2 = pidz2
      hdschaktfortemp.SID = sidvalue 
      hdschaktfortemp.FID = fid
      hdschaktfortemp.YTBELAGG = ?
      hdschaktfortemp.LAGID = ?
      hdschaktfortemp.DJUP  = ?
      hdschaktfortemp.BREDD = ?
      hdschaktfortemp.LANGD = ?.
      RUN addsf_UI IN schaktapph (INPUT INTEGER(hdschaktfortemp.BERNR), INPUT hdschaktfortemp.OMRADE, INPUT hdschaktfortemp.SID, INPUT fid, INPUT hdschaktfortemp.PID1, INPUT hdschaktfortemp.PID2, INPUT ?). /*längd är =? */      
      RUN fyllaforlaggning_UI.
   END.
END PROCEDURE.

/* lägga till kabel till schakt */
PROCEDURE kabeltillschakt_UI :
   DEFINE VARIABLE benamning AS CHARACTER NO-UNDO.   
   FIND FIRST tempkabel WHERE tempkabel.KABEL = CMB_KAB:SCREEN-VALUE NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE tempkabel THEN FIND FIRST tempkabel NO-LOCK NO-ERROR.  /* ?? */
   IF AVAILABLE tempkabel THEN DO:
      valdkabel = tempkabel.KABEL.   
   END.
   FIND LAST hdkabellinjetemp USE-INDEX KLID NO-ERROR.
      IF NOT AVAILABLE hdkabellinjetemp THEN DO:
         klidadd = 1.
      END.
      ELSE DO:
         klidadd = hdkabellinjetemp.KLID + 1.
      END.      
      CREATE hdkabellinjetemp.      
      benamning = "L".
      benamning = benamning + STRING(klidadd).
      benamning = benamning + " ".
      benamning = benamning + tempkabel.KABEL.      
      ASSIGN
      hdkabellinjetemp.BENAMNING = benamning
      hdkabellinjetemp.KABID = tempkabel.KABID
      hdkabellinjetemp.KLID = klidadd
      hdkabellinjetemp.BERNR = INTEGER(valaonr)
      hdkabellinjetemp.OMRADE = valomrade
      hdkabellinjetemp.SID = 1 /* fixa val-lista */
      hdkabellinjetemp.PID = hdpunkttemp.PID /*mer kontroll om valt*/
      hdkabellinjetemp.START = TRUE
      hdkabellinjetemp.AVSLUTAD = FALSE
      hdkabellinjetemp.ORDNING = 1
      hdkabellinjetemp.SLUT = FALSE.      
      RUN addkab_UI IN schaktapph (INPUT hdkabellinjetemp.BENAMNING, INPUT hdkabellinjetemp.KABID, INPUT hdkabellinjetemp.KLID, INPUT hdkabellinjetemp.BERNR, INPUT hdkabellinjetemp.OMRADE, INPUT hdkabellinjetemp.SID, INPUT hdkabellinjetemp.PID, INPUT hdkabellinjetemp.START, INPUT hdkabellinjetemp.AVSLUTAD, INPUT hdkabellinjetemp.ORDNING, INPUT hdkabellinjetemp.SLUT).
      /* skriva till DB */ 
      RUN fyllaok_UI.
      RUN fyllallakablar_UI.
      RUN fyllapk_UI.
      RUN fyllamk_UI.
END PROCEDURE.

/*ta bort kabelt helt från schakt (ordning 1 och över) */
PROCEDURE kabelfranschakt_UI :
   RUN selectfocokab_UI. /* ta bort från oavslutade.. */   
   RUN remkab_UI IN schaktapph (INPUT INTEGER(valaonr), INPUT valomrade, INPUT okltemp.SID,  INPUT okltemp.KLID, INPUT 1, INPUT ?).   
   RUN fyllaok_UI.
   RUN fyllallakablar_UI.
   RUN fyllapk_UI.
   RUN fyllamk_UI.
END PROCEDURE.

/* ta bort kabel från punkt och efterföljande punkter */
PROCEDURE kabelfranpunkt_UI :
   DEFINE VARIABLE ordz AS INTEGER NO-UNDO.
   RUN selectfocpkab_UI.
   FIND LAST pkltemp USE-INDEX ORDNING.
   ordz = pkltemp.ORDNING.   
   /*kontroll så att kabeln inte försvinner ur punkten start om man inte trycker på <bort från schakt> */
   IF pkltemp.ORDNING > 1 THEN DO:
      RUN remkab_UI IN schaktapph (INPUT INTEGER(valaonr), INPUT valomrade,INPUT pkltemp.SID, INPUT pkltemp.KLID, INPUT ordz, INPUT ?).
   END.
   ELSE DO:
      ordz = 2.
      RUN remkab_UI IN schaktapph (INPUT INTEGER(valaonr), INPUT valomrade,INPUT pkltemp.SID, INPUT pkltemp.KLID, INPUT ordz, INPUT ?).
   END.   
   RUN fyllallakablar_UI.
   RUN fyllapk_UI.
   RUN fyllamk_UI.
END PROCEDURE.

/* skickar kabel från listan över oavslutade kablar till vald punkt */
PROCEDURE kabeltillpunkt_UI :
   DEFINE VARIABLE pid1 AS INTEGER NO-UNDO.
   DEFINE VARIABLE pid2 AS INTEGER NO-UNDO.
   RUN selectfocokab_UI.
   RUN selectfocpunkt_UI.
   DEFINE VARIABLE inord AS INTEGER NO-UNDO.
   inord = okltemp.ordning + 1.
   FIND LAST mkltemp USE-INDEX ORDNING.
   inord = mkltemp.ordning + 1.   
   RUN addkab_UI IN schaktapph (INPUT okltemp.BENAMNING, INPUT okltemp.KABID, INPUT okltemp.KLID, INPUT okltemp.BERNR, INPUT okltemp.OMRADE, INPUT okltemp.SID, INPUT hdpunkttemp.PID, 
                                INPUT FALSE, INPUT FALSE, INPUT inord, INPUT FALSE).       
   RUN fyllapk_UI.
   RUN fyllamk_UI.
   FIND LAST mkltemp USE-INDEX ORDNING.
   pid1 = mkltemp.PID.
   FIND PREV mkltemp USE-INDEX ORDNING.
   pid2 = mkltemp.PID.
   RUN forlkoll_UI (INPUT okltemp.KLID, INPUT pid1, INPUT pid2).
END PROCEDURE.
 
PROCEDURE fyllakabellinje_UI :
   EMPTY TEMP-TABLE hdkabellinjetemp NO-ERROR.
   tthandle = TEMP-TABLE hdkabellinjetemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDKABELLINJE", INPUT " WHERE BERNR = '" + valaonr + "' AND OMRADE = '" + valomrade + "' AND SID = " + STRING(sidvalue) + " ").
END PROCEDURE.

PROCEDURE fyllapunkt_UI :
   EMPTY TEMP-TABLE hdpunkttemp NO-ERROR.
   tthandle = TEMP-TABLE hdpunkttemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDPUNKT", INPUT " WHERE BERNR = '" + valaonr + "' AND OMRADE = '" + valomrade + "' AND NUM = 0 ").
   RUN openbdynspec_UI IN brwproc[1]. /* punkt */
END PROCEDURE.

PROCEDURE fyllaok_UI :
   EMPTY TEMP-TABLE okltemp NO-ERROR.
   tthandle = TEMP-TABLE okltemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDKABELLINJE", INPUT " WHERE BERNR = '" + valaonr + "' AND OMRADE = '" + valomrade + "' AND AVSLUTAD = FALSE AND SID = " + STRING(sidvalue) + " AND START = TRUE").
   RUN openbdynspec_UI IN brwproc[2]. /* oavslutad kabellinje brw */            
END PROCEDURE.

PROCEDURE fyllapk_UI :
   EMPTY TEMP-TABLE pkltemp NO-ERROR.
   tthandle = TEMP-TABLE pkltemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDKABELLINJE", INPUT " WHERE BERNR = '" + valaonr + "' AND OMRADE = '" + valomrade + "' AND SID = " + STRING(sidvalue) + " AND PID = " + STRING(pidvalue) ).
   RUN openbdynspec_UI IN brwproc[3]. /* i punkt kabellinje brw */              
END PROCEDURE.

PROCEDURE fyllamk_UI :
   EMPTY TEMP-TABLE mkltemp NO-ERROR.
   tthandle = TEMP-TABLE mkltemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDKABELLINJE", INPUT " WHERE BERNR = '" + valaonr + "' AND OMRADE = '" + valomrade + "'AND KLID = " + STRING(klidvalue) + " ").
   RUN openbdynspec_UI IN brwproc[4]. /* markerad kabels väg kabellinje brw */  
END PROCEDURE.

PROCEDURE fyllallakablar_UI :
   EMPTY TEMP-TABLE akltemp NO-ERROR. 
   tthandle = TEMP-TABLE akltemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDKABELLINJE", INPUT " WHERE BERNR = '" + valaonr + "' AND OMRADE = '" + valomrade + "' AND START = TRUE AND SID = '" + STRING(sidvalue) + "' ").
   RUN openbdynspec_UI IN brwproc[5]. /* Alla kablar i schaktet brw */  
END PROCEDURE.


PROCEDURE fyllaforlaggning_UI :
   EMPTY TEMP-TABLE hdschaktfortemp NO-ERROR. 
   tthandle = TEMP-TABLE hdschaktfortemp:HANDLE.
   RUN laddatemp_UI IN laddaproch (INPUT-OUTPUT TABLE-HANDLE tthandle, INPUT "HDSCHAKTFOR", INPUT " WHERE BERNR = '" + valaonr + "' AND OMRADE = '" + valomrade + "' AND SID = '" + STRING(sidvalue) + "' ").
   RUN openbdynspec_UI IN brwproc[11]. /* Alla kablar i schaktet brw */  
END PROCEDURE.

PROCEDURE kl_UI :
   RUN fyllapk_UI.   
   RUN fyllamk_UI.   
END PROCEDURE.

/* punkt */
PROCEDURE selectfocpunkt_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(temppunkth) THEN DO:
      temppunkth = TEMP-TABLE hdpunkttemp:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[1] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[1](INPUT brwrowid). 
   IF brwrowid NE ? THEN
   FIND FIRST hdpunkttemp WHERE ROWID(hdpunkttemp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.

/* oavslutade kablar */
PROCEDURE selectfocokab_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempokabh) THEN DO:
      tempokabh = TEMP-TABLE okltemp:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[2] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[2](INPUT brwrowid). 
   IF brwrowid NE ? THEN
   FIND FIRST okltemp WHERE ROWID(okltemp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.

/* schakt */
PROCEDURE selectfocschakt_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempschakth) THEN DO:
      tempschakth = TEMP-TABLE hdschakttemp:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[6] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[6](INPUT brwrowid). 
   IF brwrowid NE ? THEN
   FIND FIRST hdschakttemp WHERE ROWID(hdschakttemp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.

/* kabel i <punkt> */
PROCEDURE selectfocpkab_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(temppkabh) THEN DO:
      temppkabh = TEMP-TABLE pkltemp:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[3] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[3](INPUT brwrowid). 
   IF brwrowid NE ? THEN
   FIND FIRST pkltemp WHERE ROWID(pkltemp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.

/* kabel i <alla kablar> */
PROCEDURE selectfocakab_UI :
   DEFINE VARIABLE brwrowid AS ROWID NO-UNDO.
   DEFINE VARIABLE brwrecid AS RECID NO-UNDO.
   IF NOT VALID-HANDLE(tempakabh) THEN DO:
      tempakabh = TEMP-TABLE akltemp:DEFAULT-BUFFER-HANDLE.
   END.
   RUN selectrowid_UI IN brwproc[5] (OUTPUT brwrowid).
   RUN selectbyrowid_UI IN brwproc[5](INPUT brwrowid). 
   IF brwrowid NE ? THEN
   FIND FIRST akltemp WHERE ROWID(akltemp) = brwrowid NO-LOCK NO-ERROR.
END PROCEDURE.

/* Lägger till kablar i comboboxen */
PROCEDURE extrakabel_UI :
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "KABLAR"                   
   inextradatatemp.HUVUDCH = ?.
   inextradatatemp.HUVUDINT = ?.
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp,OUTPUT TABLE extradatatemp).

   EMPTY TEMP-TABLE tempkabel NO-ERROR. 
   FOR EACH extradatatemp:
      CREATE tempkabel.
      ASSIGN 
      tempkabel.KABEL = extradatatemp.HUVUDCH.
      tempkabel.DIAMETER = extradatatemp.SOKINT[1].
      tempkabel.KABID = extradatatemp.HUVUDINT.
      tempkabel.BORTTAGEN = extradatatemp.SOKLOG[1].
   END.
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
END PROCEDURE.

PROCEDURE allstartbrw_UI :
   RUN DYNBRW.P PERSISTENT SET brwproc[1] (INPUT BRW_PUNKT).
   RUN DYNBRW.P PERSISTENT SET brwproc[2] (INPUT BRW_OKAB).
   RUN DYNBRW.P PERSISTENT SET brwproc[3] (INPUT BRW_PKAB).
   RUN DYNBRW.P PERSISTENT SET brwproc[4] (INPUT BRW_MKAB).  
   RUN DYNBRW.P PERSISTENT SET brwproc[5] (INPUT BRW_AKAB). 
   RUN DYNBRW.P PERSISTENT SET brwproc[6] (INPUT BRW_SCHAKT). 
   RUN DYNBRW.P PERSISTENT SET brwproc[7] (INPUT BRW_FOR). 
   RUN DYNBRW.P PERSISTENT SET brwproc[8] (INPUT BRW_SAM). 
   RUN DYNBRW.P PERSISTENT SET brwproc[9] (INPUT BRW_YT).
   RUN DYNBRW.P PERSISTENT SET brwproc[10] (INPUT BRW_SAMVAL).
   RUN DYNBRW.P PERSISTENT SET brwproc[11] (INPUT BRW_SF).
   IF Guru.Konstanter:appcon THEN DO:
      RUN SCHAKTADB.P PERSISTENT SET schaktapph ON Guru.Konstanter:apphand TRANSACTION DISTINCT. 
   END.
   ELSE DO:
      RUN SCHAKTADB.P PERSISTENT SET schaktapph.
   END.
END PROCEDURE.
