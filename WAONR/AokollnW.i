/*AOKOLLNW.I*/
/*kolla alltid i stanstid.w*/
&Scoped-define NEW NEW 
{GLOBVAR2DEL1.I}
/*{EGENBNS.I}*/    
{EXTRATAB.I}  
DEFINE INPUT PARAMETER kolladatumvar AS DATE NO-UNDO.
DEFINE INPUT PARAMETER tidtabrecvar AS RECID NO-UNDO.
DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER aonruvar AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER delnruvar AS INTEGER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonruvar AND AONRTAB.DELNR = delnruvar USE-INDEX AONR NO-LOCK NO-ERROR.  
FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
persrec = RECID(PERSONALTAB). 
RUN tidkoll_UI.
{AOKOLLNW2.I}
FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
IF AVAILABLE felmeddtemp THEN DO:
   RETURN.
END.
