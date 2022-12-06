/*asfaktapp.p*/
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
DEFINE VARIABLE restuserh AS HANDLE NO-UNDO.
{EXTRADATA.I}
{EPERSTEMP.I}
{KODERAVT.I}
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
DEFINE  TEMP-TABLE avtaltemp NO-UNDO
   FIELD AVTALNAMN AS CHARACTER
   FIELD AVTALTYP AS CHARACTER
   FIELD TYP AS CHARACTER
   FIELD ORDNING AS INTEGER
   INDEX TYP IS PRIMARY TYP ORDNING.
DEFINE TEMP-TABLE persextra NO-UNDO
   FIELD PERSONALKOD AS CHARACTER
   FIELD FORNAMN AS CHARACTER
   FIELD EFTERNAMN AS CHARACTER
   INDEX PKOD PERSONALKOD.
DEFINE TEMP-TABLE epersextra NO-UNDO LIKE persextra.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.   
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
PROCEDURE hfaktor:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER avkfaktor AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER sefaktor AS DECIMAL NO-UNDO.

   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "PFAKTOR"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      avkfaktor = extradatatemp.SOKDEC[1]   
      sefaktor = extradatatemp.SOKDEC[2].
   END.   
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE hdispens:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER dispens AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER dardatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER dargodk AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖ"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      dispens = extradatatemp.SOKLOG[1].      
      dardatum =  extradatatemp.SOKDATE[1]. 
      dargodk =   extradatatemp.SOKCHAR[1].
   END.   
   ELSE dispens = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE hdispensm:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER dispensm AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER dmandatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER dmangodk AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖM"                   
   inextradatatemp.HUVUDCH = pkod.   
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      dispensm = extradatatemp.SOKLOG[1].
      dmandatum =  extradatatemp.SOKDATE[1]. 
      dmangodk =   extradatatemp.SOKCHAR[1].         
   END.   
   ELSE dispensM = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE hdispens48:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER dispens48 AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER d48datum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER d48godk AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖ48"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      dispens48 = extradatatemp.SOKLOG[1].      
      d48datum =  extradatatemp.SOKDATE[1]. 
      d48godk =   extradatatemp.SOKCHAR[1].
   END.   
   ELSE dispens48 = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE spfrisk:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER spafrisk AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "SPFRISK"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      spafrisk = extradatatemp.SOKLOG[1].         
   END.   
   ELSE spafrisk = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE fortro:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ftro AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FORTRO"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      ftro = extradatatemp.SOKLOG[1].         
   END.   
   ELSE ftro = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE tillit:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER tillit AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "TILLIT"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      tillit = extradatatemp.SOKLOG[1].         
   END.   
   ELSE tillit = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE havafor:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER avarfor AS INTEGER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "AVAFOR"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      avarfor = extradatatemp.SOKINT[1].         
   END.   
   ELSE avarfor = 0.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE lakdispens:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lakdispens AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSLÄ"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      lakdispens = extradatatemp.SOKLOG[1].         
   END.   
   ELSE lakdispens = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE htillatnodf:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER nofall AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER nodatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER nogodk AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "NODFALL"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      nofall = extradatatemp.SOKLOG[1].
      nodatum =  extradatatemp.SOKDATE[1]. 
      nogodk =   extradatatemp.SOKCHAR[1].         
   END.   
   ELSE nofall = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.


PROCEDURE lonvaxatk:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lonvaxatk AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "LONVAXLINGATK"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      lonvaxatk = extradatatemp.SOKLOG[1].         
   END.   
   ELSE lonvaxatk = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE ater50_UI:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ater50 AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER a50tim AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER dardatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER dargodk AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "ATERFOR50"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      ater50 = extradatatemp.SOKLOG[1].
      a50tim = extradatatemp.SOKINT[1].
      dardatum =  extradatatemp.SOKDATE[1]. 
      dargodk =   extradatatemp.SOKCHAR[1].      
               
   END.   
   ELSE ater50 = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE sealltid_UI:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER sealltid AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER sedatum AS DATE NO-UNDO.
   DEFINE OUTPUT PARAMETER seanv AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "SEALLTID"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      sealltid = extradatatemp.SOKLOG[1].
      sedatum =  extradatatemp.SOKDATE[1]. 
      seanv =   extradatatemp.SOKCHAR[1].                     
  END.   
   ELSE sealltid = FALSE.         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE hmtpersligg:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER eorgnr AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER eforetag AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "PERSONALLIGGARE"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN
      eorgnr = extradatatemp.SOKCHAR[1].
      eforetag = extradatatemp.SOKCHAR[2].         
   END.   
            
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE sphfaktor:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER avkfaktor AS DECIMAL NO-UNDO.
   DEFINE INPUT PARAMETER sefaktor AS DECIMAL NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "PFAKTOR"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKDEC[1] = avkfaktor    
   inextradatatemp.SOKDEC[2] = sefaktor.    

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE spdispens:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER dispens AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER dardatum AS DATE NO-UNDO.
   DEFINE INPUT  PARAMETER dargodk AS CHARACTER NO-UNDO.
      
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖ"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = dispens.   
   inextradatatemp.SOKDATE[1] = dardatum. 
   inextradatatemp.SOKCHAR[1] = dargodk.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE spdispensm:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER dispensm AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER dmandatum AS DATE NO-UNDO.
   DEFINE INPUT  PARAMETER dmangodk AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖM"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = dispensm.       
   inextradatatemp.SOKDATE[1] = dmandatum. 
   inextradatatemp.SOKCHAR[1] = dmangodk.

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE spdispens48:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER dispens48 AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER d48datum AS DATE NO-UNDO.
   DEFINE INPUT  PARAMETER d48godk AS CHARACTER NO-UNDO.
      
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖ48"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = dispens48.   
   inextradatatemp.SOKDATE[1] = d48datum. 
   inextradatatemp.SOKCHAR[1] = d48godk.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE spspfr:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER spafr AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "SPFRISK"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = spafr.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE spfortro:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ftro AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FORTRO"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = ftro.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE sptillit:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER tillit AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "TILLIT"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = tillit.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.


PROCEDURE spater50:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER ater50 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER a50tim AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER dardatum AS DATE NO-UNDO.
   DEFINE INPUT  PARAMETER dargodk AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "ATERFOR50"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = ater50
   inextradatatemp.SOKINT[1] = a50tim
   inextradatatemp.SOKDATE[1] = dardatum. 
   inextradatatemp.SOKCHAR[1] = dargodk.    

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE spsealltid_UI:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER sealltid AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER sedatum AS DATE NO-UNDO.
   DEFINE INPUT  PARAMETER seanv AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "SEALLTID"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = sealltid
   inextradatatemp.SOKDATE[1] = sedatum. 
   inextradatatemp.SOKCHAR[1] = seanv.    

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.


PROCEDURE spavafor:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER avarfor AS INTEGER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "AVAFOR"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKINT[1] = avarfor.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.


PROCEDURE spdispenslak:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lakdispens AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSLÄ"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = lakdispens.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE spnodfall:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER nofall AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER nodatum AS DATE NO-UNDO.
   DEFINE INPUT  PARAMETER nogodk AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "NODFALL"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = nofall. 
   inextradatatemp.SOKDATE[1] = nodatum. 
   inextradatatemp.SOKCHAR[1] = nogodk.            

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE splonvatk:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lonvaxatk AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "LONVAXLINGATK"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKLOG[1] = lonvaxatk.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.

PROCEDURE flexstsl:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER flst AS DATE  NO-UNDO.
   DEFINE OUTPUT PARAMETER flsl AS DATE  NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FLEXSTARTSLUT"                   
   inextradatatemp.HUVUDCH = pkod.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.     
   IF AVAILABLE extradatatemp THEN DO:      
      ASSIGN      
      flst = extradatatemp.SOKDATE[1]  
      flsl = extradatatemp.SOKDATE[2].         
   END.   
         
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.


PROCEDURE spflexstsl:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER flstart AS DATE  NO-UNDO.
   DEFINE INPUT PARAMETER flslut AS DATE  NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FLEXSTARTSLUT"                   
   inextradatatemp.HUVUDCH = pkod
   inextradatatemp.SOKDATE[1] = flstart
   inextradatatemp.SOKDATE[2] = flslut.       

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.
PROCEDURE sppersligg:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER eorgnr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER eforetag AS CHARACTER NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).   
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "PERSONALLIGGARE"                   
   inextradatatemp.HUVUDCH = pkod
   
   inextradatatemp.SOKCHAR[1] = eorgnr.
   inextradatatemp.SOKCHAR[2] = eforetag.
          

   RUN extraspar_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END.


PROCEDURE bortfaktor:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "PFAKTOR"                   
   inextradatatemp.HUVUDCH = pkod.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PERSONALTAB THEN DO:
      RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).        
   END.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortdispens:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖ"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortdispensm:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖM"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortdispens48:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSÖ48"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.


PROCEDURE bortspfr:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "SPFRISK"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortftro:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FORTRO"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE borttillit:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "TILLIT"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortater50:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "ATERFOR50"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortsealltid:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "SEALLTID"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortavafor:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "AVAFOR"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortdispensla:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "DISPENSLÄ"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortnodfall:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "NODFALL"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortlonvatk:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "LONVAXLINGATK"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bortflexstsl:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "FLEXSTARTSLUT"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.



PROCEDURE bortpersligg:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   RUN FINNSTABELL.P (INPUT "EXTRADATA", OUTPUT bloblog).
   IF bloblog = FALSE THEN RETURN.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "PERSONALLIGGARE"                   
   inextradatatemp.HUVUDCH = pkod.   
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).           
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE bytbolag:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER nyomr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER spomr AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER judbyt AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER finnstid AS LOGICAL NO-UNDO.
   DEFINE VARIABLE spjud AS CHARACTER NO-UNDO.
   DEFINE VARIABLE nyjud AS CHARACTER NO-UNDO.
   judbyt = FALSE.
   finnstid = FALSE.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = nyomr NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   IF AVAILABLE JURPERS THEN DO:
      IF AVAILABLE AVDELNING THEN DO:
         nyjud = JURPERS.JUDID.
      END.
   END.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = spomr NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
   IF AVAILABLE JURPERS THEN DO:
      IF AVAILABLE AVDELNING THEN DO:
         spjud = JURPERS.JUDID.
      END.
   END.      
   IF spjud NE nyjud THEN DO:
      judbyt = TRUE.
   END.     
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD =  pkod AND TIDREGITAB.VECKOKORD = "" AND TIDREGITAB.TIDLOG = TRUE NO-LOCK NO-ERROR.
   IF AVAILABLE TIDREGITAB THEN DO:
      finnstid = TRUE.
   END.   
END PROCEDURE.


PROCEDURE pavt_UI:
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eperstemp.    
   FOR EACH eperstemp:
      FIND FIRST BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = eperstemp.BEFATTNING
      USE-INDEX BEF NO-LOCK NO-ERROR.
      FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = eperstemp.ANSTALLNING
      USE-INDEX ANSTF NO-LOCK NO-ERROR.
      FIND FIRST BERTAB WHERE BERTAB.BEREDSKAPSAVTAL = eperstemp.BEREDSKAPSAVTAL
      USE-INDEX BERTAB NO-LOCK NO-ERROR.
      FIND FIRST TRAAVTAB WHERE TRAAVTAB.TRAAVTAL = eperstemp.TRAAVTAL      USE-INDEX TRAAVTAB NO-LOCK NO-ERROR.
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = eperstemp.OMRADE   NO-LOCK NO-ERROR.
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = eperstemp.PERSONALKOD   NO-LOCK NO-ERROR.
      IF AVAILABLE BERTAB THEN eperstemp.BAVTAL = BERTAB.FORKL.
      IF AVAILABLE TRAAVTAB  THEN   eperstemp.TAVTAL = TRAAVTAB.FORKLARING.      
      eperstemp.ONAMN = OMRADETAB.NAMN.
      IF AVAILABLE FLEXAVT THEN DO:
         eperstemp.FLEXTID = FLEXAVT.FLEXTID.
         IF Guru.Konstanter:globforetag = "sund"  OR Guru.Konstanter:globforetag = "misv" THEN DO:    
            IF eperstemp.FLEXTID = TRUE THEN DO:      
               IF FLEXAVT.FLEXKOD = "TE" THEN ASSIGN eperstemp.FAVT = "Sundsvall Elnät".
               IF FLEXAVT.FLEXKOD = "T" THEN ASSIGN eperstemp.FAVT = "Sundsvall Övriga".
               IF FLEXAVT.FLEXKOD = "K" THEN ASSIGN eperstemp.FAVT = "Sundsvall kollektiv Elnät".
               IF FLEXAVT.FLEXKOD = "KV" THEN ASSIGN eperstemp.FAVT = "Sundsvall kollektiv Vatten".
               IF FLEXAVT.FLEXKOD = "TU" THEN ASSIGN eperstemp.FAVT = "Korsta".
               IF FLEXAVT.FLEXKOD = "" THEN ASSIGN eperstemp.FAVT = "Sundsvall Övriga".
            END.
            ELSE eperstemp.FAVT = "".                 
         END. 
         IF Guru.Konstanter:globforetag = "SNAT"  THEN DO:    
            IF eperstemp.FLEXTID = TRUE THEN DO:      
               IF FLEXAVT.FLEXKOD = "TE" THEN ASSIGN eperstemp.FAVT = "8 -17".
               /*IF FLEXAVT.FLEXKOD = "T" THEN ASSIGN eperstemp.FAVT = "Sundsvall Övriga".*/
               IF FLEXAVT.FLEXKOD = "K" THEN ASSIGN eperstemp.FAVT = "7 -16".
               /*IF FLEXAVT.FLEXKOD = "KV" THEN ASSIGN eperstemp.FAVT = "Sundsvall kollektiv Vatten".
               IF FLEXAVT.FLEXKOD = "TU" THEN ASSIGN eperstemp.FAVT = "Korsta".*/
               IF FLEXAVT.FLEXKOD = "" THEN ASSIGN eperstemp.FAVT = "Sundsvall Övriga".
            END.
            ELSE eperstemp.FAVT = "".                 
         END.   
      END.    
      ELSE eperstemp.FLEXTID = FALSE.
      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = eperstemp.TIDSGODK NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         eperstemp.TIDSGODK = eperstemp.TIDSGODK + " " + PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
      END.   
      FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD =  eperstemp.PERSONALKOD AND 
      TIDREGITAB.TIDLOG = TRUE  USE-INDEX  pstart NO-LOCK NO-ERROR.
      IF AVAILABLE tidregitab THEN DO:
          eperstemp.FTID = STRING(TIDREGITAB.DATUM).
      END.   
      FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = eperstemp.PERSONALKOD AND 
      TIDREGITAB.TIDLOG = TRUE  USE-INDEX  pstart NO-LOCK NO-ERROR.
      IF AVAILABLE tidregitab THEN DO:
         eperstemp.STID = STRING(TIDREGITAB.DATUM).
      END.    
      
      IF eperstemp.A50DATUM = ? THEN eperstemp.A50DATUM = "".
      IF  eperstemp.DARDATUM = ? THEN eperstemp.DARDATUM = "".
      IF eperstemp.D48DATUM = ? THEN eperstemp.D48DATUM = "".      
      IF eperstemp.DMANDATUM = ? THEN eperstemp.DMANDATUM = "".
      IF eperstemp.SEDATUM = ? THEN eperstemp.SEDATUM = "".
      IF eperstemp.FTID = ? THEN eperstemp.FTID = "".
      IF eperstemp.STID = ? THEN eperstemp.STID = "".
      IF eperstemp.NODDATUM = ? THEN eperstemp.NODDATUM = "".

      
              
   END.
END PROCEDURE.

PROCEDURE persGPL_UI:
   DEFINE OUTPUT PARAMETER TABLE FOR eperstemp.
   DEFINE VARIABLE plfanns  AS LOGICAL NO-UNDO.
   DEFINE VARIABLE eorgnr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE eforetag AS CHARACTER NO-UNDO.
   RUN RESTUSERS.P PERSISTENT SET restuserh.
  EMPTY TEMP-TABLE eperstemp  NO-ERROR.
  DEBUGGER:SET-BREAK(). 
   FOR EACH PERSONALTAB  WHERE PERSONALTAB.AKTIV = TRUE NO-LOCK:
      RUN fannsRESTUser_UI IN restuserh(INPUT PERSONALTAB.PERSONALKOD, OUTPUT plfanns).
      IF plfanns = TRUE THEN DO:
         CREATE eperstemp.
         BUFFER-COPY PERSONALTAB TO eperstemp.
         eperstemp.PLIGGARE = TRUE.           
         RUN hmtpersligg  (INPUT eperstemp.PERSONALKOD, OUTPUT eorgnr,output eforetag).     
         IF eorgnr = "" THEN  ASSIGN eperstemp.ORGNR = "0000000000".
         ELSE ASSIGN eperstemp.ORGNR = eorgnr.     
         eperstemp.FORETAG = eforetag.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
         FIND LAST GPLAKTIVITET WHERE GPLAKTIVITET.PERSONALKOD = PERSONALTAB.PERSONALKOD USE-INDEX LOGGTID NO-LOCK NO-ERROR.
         IF AVAILABLE GPLAKTIVITET THEN DO:
            eperstemp.SENASTREG = DATE(GPLAKTIVITET.LOGGTID). 
         END.   
      END.  
   END.
   {GDPRLOGGCLIENT.I}
   IF VALID-HANDLE(restuserh) THEN DELETE PROCEDURE restuserh. 
   restuserh = ?.
   
END PROCEDURE.

PROCEDURE persGPLsista_UI:
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eperstemp.
   DEFINE VARIABLE plfanns  AS LOGICAL NO-UNDO.
   DEFINE VARIABLE eorgnr  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE eforetag AS CHARACTER NO-UNDO.
   RUN RESTUSERS.P PERSISTENT SET restuserh.
  
  DEBUGGER:SET-BREAK().
   
   FOR EACH eperstemp :
      RUN fannsRESTUser_UI IN restuserh(INPUT eperstemp.PERSONALKOD, OUTPUT plfanns).
      IF plfanns = TRUE THEN DO:                  
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + eperstemp.PERSONALKOD.
         FIND LAST GPLAKTIVITET WHERE GPLAKTIVITET.PERSONALKOD = eperstemp.PERSONALKOD USE-INDEX LOGGTID NO-LOCK NO-ERROR.
         IF AVAILABLE GPLAKTIVITET THEN DO:
            eperstemp.SENASTREG = DATE(GPLAKTIVITET.LOGGTID). 
         END.   
      END.  
   END.
   {GDPRLOGGCLIENT.I}
   IF VALID-HANDLE(restuserh) THEN DELETE PROCEDURE restuserh. 
   restuserh = ?.
   
END PROCEDURE.


PROCEDURE hamtallalart_UI:
   DEFINE OUTPUT PARAMETER TABLE FOR koder.
   EMPTY TEMP-TABLE koder NO-ERROR. 
   EMPTY TEMP-TABLE avtaltemp NO-ERROR. 
    RUN AVTAPP.P (OUTPUT TABLE avtaltemp).
    OPEN QUERY till FOR EACH avtaltemp WHERE avtaltemp.TYP = "ANS",
    EACH ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = avtaltemp.AVTALTYP,
    EACH LONTILL WHERE LONTILL.KOD = ANSTFORMTAB.KOD AND LONTILL.VALBAR = TRUE.
    GET FIRST till NO-LOCK.   
    DO WHILE AVAILABLE(LONTILL): 
       CREATE koder.   
       ASSIGN
       koder.VILART = LONTILL.VILART
       koder.LONTILLAGG = LONTILL.LONTILLAGG
       koder.LONKODTEXT = LONTILL.LONKODTEXT 
       koder.ENHET = LONTILL.ENHET     
       koder.VALBAR = LONTILL.VALBAR
       koder.PRIS = LONTILL.ERSATTNING         
       koder.AVTALKOD = ANSTFORMTAB.KOD
       koder.AVTALNAMN = ANSTFORMTAB.ANSTALLNING
       koder.AVTALTYP = "Lönetillägg"
       koder.AVTALSFORM = "Anställningsform".         
     GET NEXT till NO-LOCK.      
   END.
   CLOSE QUERY till.
   
   OPEN QUERY overq FOR EACH avtaltemp WHERE avtaltemp.TYP = "ANS",
   EACH ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = avtaltemp.AVTALTYP,
   EACH OVERKOD WHERE OVERKOD.KOD = ANSTFORMTAB.KOD AND OVERKOD.VALBAR = TRUE BY OVERKOD.VILART.
   GET FIRST overq NO-LOCK.   
   DO WHILE AVAILABLE(OVERKOD):       
      CREATE koder.   
      ASSIGN
      koder.VILART = OVERKOD.VILART
      koder.LONTILLAGG = OVERKOD.OVERTIDTILL
      koder.LONKODTEXT = OVERKOD.LONKODTEXT 
      koder.ENHET = OVERKOD.ENHET   
      koder.VALBAR = OVERKOD.VALBAR       
      koder.AVTALKOD = ANSTFORMTAB.KOD
      koder.AVTALNAMN = ANSTFORMTAB.ANSTALLNING
      koder.AVTALTYP = "Övertidstillägg"
      koder.AVTALSFORM = "Anställningsform".                
      GET NEXT overq NO-LOCK.      
   END.
   CLOSE QUERY overq.

   OPEN QUERY berq FOR EACH avtaltemp WHERE avtaltemp.TYP = "BER",
   EACH BERKOD WHERE BERKOD.BEREDSKAPSAVTAL = avtaltemp.AVTALTYP and BERKOD.VALBAR = TRUE .
   GET FIRST berq NO-LOCK.   
   DO WHILE AVAILABLE(BERKOD):    
      CREATE koder.   
      ASSIGN
      koder.VILART = BERKOD.VILART
      koder.LONTILLAGG = BERKOD.BEREDSKAP
      koder.LONKODTEXT = BERKOD.LONKODTEXT 
      koder.ENHET = BERKOD.ENHET   
      koder.VALBAR = BERKOD.VALBAR
      koder.PRIS = BERKOD.ERSATTNING        
      koder.AVTALKOD = avtaltemp.AVTALTYP
      koder.AVTALNAMN = avtaltemp.AVTALNAMN
      koder.AVTALTYP = "Beredskap"
      koder.AVTALSFORM = "Beredskapsavtal".                  
      GET NEXT berq NO-LOCK.      
   END.
   CLOSE QUERY berq.   
   OPEN QUERY traq FOR EACH avtaltemp WHERE avtaltemp.TYP = "TRA",
   EACH TRAKTATAB WHERE  TRAKTATAB.TRAAVTAL = avtaltemp.AVTALTYP AND  TRAKTATAB.VALBAR = TRUE .
   GET FIRST traq NO-LOCK.   
   DO WHILE AVAILABLE(TRAKTATAB):    
      CREATE koder.   
      ASSIGN
      koder.VILART = TRAKTATAB.VILART
      koder.LONTILLAGG = TRAKTATAB.TRAKTKOD
      koder.LONKODTEXT = TRAKTATAB.FORKL 
      koder.ENHET = "ST"  
      koder.VALBAR = TRAKTATAB.VALBAR
      koder.PRIS = TRAKTATAB.ERSATTNING       
      koder.AVTALKOD = avtaltemp.AVTALTYP
      koder.AVTALNAMN = avtaltemp.AVTALNAMN
      koder.AVTALTYP = "Traktamente"
      koder.AVTALSFORM = "Traktamentsavtal".                  
      GET NEXT traq NO-LOCK.      
   END.
   CLOSE QUERY traq.   
END PROCEDURE.

PROCEDURE pbef_UI:
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.      
   DEFINE OUTPUT PARAMETER pkbef AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER pkanst AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ejmed AS LOGICAL NO-UNDO.
   DEFINE VARIABLE sokartal AS INTEGER NO-UNDO.
   FIND FIRST PERSONALTAB  WHERE PERSONALTAB.PERSONALKOD = pkod NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      pkbef = PERSONALTAB.BEFATTNING.
      pkanst = PERSONALTAB.ANSTALLNING.
  END.
  
  /*använd finnstid till att styra vilka befattningar som ej ska med*/
  ejmed = FALSE.
  /*se även APKTRL2.P*/
  
  IF  Guru.Konstanter:globforetag = "SUND"  THEN DO: 
     IF  pkanst = "ENTREP.AVTAL" THEN ejmed = TRUE.
     IF  pkbef = "Timanställd" THEN ejmed = TRUE.
  END.
  IF  Guru.Konstanter:globforetag = "SNAT"  THEN DO: 
     IF  pkanst = "ENTREP.AVTAL" THEN ejmed = TRUE.
     IF  pkbef = "Timanställd" THEN ejmed = TRUE.
  END.
  IF  Guru.Konstanter:globforetag = "misv"  THEN DO: 
     IF  pkanst = "ENTREP.AVTAL" THEN ejmed = TRUE.
     IF  pkbef = "Extern konsult" THEN ejmed = TRUE.
     IF  pkbef = "Ej tid" THEN ejmed = TRUE.
     IF  pkbef = "Timanställd" THEN ejmed = TRUE.
  END.   
  IF  Guru.Konstanter:globforetag = "GKAL"  THEN DO: 
     IF  pkbef = "INHYRD PERSONAL" THEN ejmed = TRUE.
     IF  pkanst = "ENTREP.AVTAL" THEN ejmed = TRUE.
     IF  pkanst = "Extern konsult" THEN ejmed = TRUE.     
  END.        
    
  /* Behövs ej , personalen väljer för nästa år och då har de alltid jobbat över årsskiftet  
  finnstid =  FALSE.
  IF MONTH(TODAY) = 10 OR MONTH(TODAY) = 11 OR MONTH(TODAY) = 12 THEN sokartal = YEAR(today) - 1 . 
  ELSE sokartal = YEAR(today) - 2.
  FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND TIDREGITAB.TIDLOG = TRUE AND TIDREGITAB.DATUM LE  DATE(12,31,sokartal) NO-LOCK NO-ERROR.
  IF AVAILABLE TIDREGITAB THEN DO:
     finnstid = TRUE.
  END.*/   
END PROCEDURE.   

PROCEDURE atkdat_UI :   
   DEFINE OUTPUT PARAMETER atkman  AS INTEGER NO-UNDO.
   /*finns även i phmtapp.P*/
   FIND FIRST FORETAG  NO-LOCK NO-ERROR.
   /*kalmar ska inte längre välja atk. De ska sättas till att alla får ta ut atk i tid vid årsskiftet 20230101 Lena 20220615*/
   IF FORETAG.FORETAG = "cgkal" THEN atkman = 10.
   IF FORETAG.FORETAG = "sund" THEN atkman = 10.
   IF FORETAG.FORETAG = "misv" THEN atkman = 11.
   IF FORETAG.FORETAG = "snat" THEN atkman = 11. 
   
END PROCEDURE.
PROCEDURE atkgrans :     
   DEFINE OUTPUT PARAMETER foremaxarbkort  AS INTEGER NO-UNDO.
   /*finns även i phmtapp.P*/
   FIND FIRST FORETAG  NO-LOCK NO-ERROR.
   IF FORETAG.FORETAG = "gkal" THEN foremaxarbkort = 63.
   ELSE IF FORETAG.FORETAG = "sund" THEN foremaxarbkort = 54.
   ELSE IF FORETAG.FORETAG = "misv" THEN foremaxarbkort = 27.
   ELSE IF FORETAG.FORETAG = "snat" THEN foremaxarbkort = 63.
   ELSE IF FORETAG.FORETAG = "LULE" THEN foremaxarbkort = 63.
   ELSE foremaxarbkort = 63.
      
END PROCEDURE.

PROCEDURE forstsisttid_UI :     
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.      
   DEFINE OUTPUT PARAMETER forstdat AS DATE  NO-UNDO.
   DEFINE OUTPUT PARAMETER sistdat AS DATE  NO-UNDO.
   FIND FIRST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND 
   TIDREGITAB.TIDLOG = TRUE  USE-INDEX  pstart NO-LOCK NO-ERROR.
   IF AVAILABLE tidregitab THEN DO:
      forstdat = TIDREGITAB.DATUM.
   END.   
   FIND LAST TIDREGITAB WHERE TIDREGITAB.PERSONALKOD = pkod AND 
   TIDREGITAB.TIDLOG = TRUE  USE-INDEX  pstart NO-LOCK NO-ERROR.
   IF AVAILABLE tidregitab THEN DO:
      sistdat = TIDREGITAB.DATUM.
   END.   
      
   
      
END PROCEDURE.

PROCEDURE persextra_UI :     
   DEFINE OUTPUT PARAMETER TABLE FOR persextra.      
   EMPTY TEMP-TABLE persextra NO-ERROR.       
   FOR EACH PERSONALTAB WHERE PERSONALTAB.AKTIV = TRUE NO-LOCK.
      CREATE persextra.
      BUFFER-COPY PERSONALTAB TO persextra.     
   END.
      
END PROCEDURE.

PROCEDURE godbortkoll_UI :     
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR epersextra.         
   EMPTY TEMP-TABLE epersextra NO-ERROR.
   FOR EACH  PERSONALTAB WHERE PERSONALTAB.TIDSGODK = pkod  NO-LOCK:         
      CREATE epersextra.
      BUFFER-COPY PERSONALTAB TO epersextra.     
   END.      
END PROCEDURE.
PROCEDURE godbort_UI :     
   DEFINE INPUT PARAMETER pkod AS CHARACTER NO-UNDO.
   DO TRANSACTION:
      FIND FIRST GODKANNARTAB WHERE GODKANNARTAB.PERSONALKOD = pkod EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE GODKANNARTAB THEN DO:
         DELETE GODKANNARTAB.        
      END.
   END.         
END PROCEDURE.

PROCEDURE ovgrans_UI :     
   DEFINE OUTPUT PARAMETER ovgrans AS INTEGER NO-UNDO.
   /*Nya regler allmän övertid 200 tim/kalkenderår , extra övertid + 150 timmar vid särskilda skäl
   Lena 20171114
   */
   IF Guru.Konstanter:globforetag = "MISV" OR Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "GKAL"
   THEN ovgrans = 200.
   ELSE ovgrans = 350.
         
END PROCEDURE.
