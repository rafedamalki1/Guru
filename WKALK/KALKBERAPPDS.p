
/*------------------------------------------------------------------------
    File        : KALKBERAPPDS.p
    Purpose     : 

    Syntax      :RUN XXX.P  (OUTPUT TABLE-HANDLE HdSchaktStrackatth).
    
                   DEFINE OUTPUT PARAMETER TABLE-HANDLE OutHdSchaktStrackatth.
    Description : SKICKA TEMP TABEL HANDLES �VER APPSERVER 
                   OUTPUT TTH
                   OUTPUT PARAMETER TABLE-HANDLE TTH
                    

    Author(s)   : elpao
    Created     : Thu Aug 30 08:30:08 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/


           
 
DEFINE VARIABLE kalknumanvegenlog AS LOGICAL NO-UNDO.
DEFINE VARIABLE berkopplalogg AS LOGICAL NO-UNDO.

/*f�r att skapa kalkyl i ber grunddata*/
DEFINE VARIABLE berkopptabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE berdningtabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE kalkbefbtabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE berkalktabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE hdkalktabkbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE kalknumtabbuffh AS HANDLE NO-UNDO.
DEFINE VARIABLE arbetsleadare AS CHARACTER NO-UNDO.
DEFINE VARIABLE utfardatvartel AS CHARACTER NO-UNDO.
DEFINE VARIABLE beredartel AS CHARACTER NO-UNDO.
DEFINE VARIABLE arbannsvtel AS CHARACTER NO-UNDO.
DEFINE VARIABLE arbetsleadaretel AS CHARACTER NO-UNDO.
DEFINE VARIABLE utfardatvarepost AS CHARACTER NO-UNDO.
DEFINE VARIABLE beredarepost AS CHARACTER NO-UNDO.
DEFINE VARIABLE arbannsvepost AS CHARACTER NO-UNDO.
DEFINE VARIABLE arbetsleadareepost AS CHARACTER NO-UNDO.
CREATE WIDGET-POOL "KalkDynTable" NO-ERROR.


DEFINE VARIABLE AppServerExtraHandle AS HANDLE NO-UNDO.
{BerKalktt.I}
{KALKYLKAT.I}
&Scoped-define PUBLIC
{KALKDYNTABLEH.I}

{KALKALLTEMPC.I}
{KALKUPPTT.I}
{LISTDEF.I}
{SOKDEF.I}
&SCOPED-DEFINE NEW NEW
&SCOPED-DEFINE SHARED SHARED
{BLOB.I}
{GLOBVAR2DEL1.I}
  
/* 
{MTRLTEMPc.I}
*/
{EXTRADATA.I}
{EXTRATAB.I}
{BOLAGSEKSTART.I}
{HAMTAVDJUDEF.I}
{GATILL.I}
{PTEMP.I}
{LOPTEMP.I}
{PTEMPLOPTEMP.I}
{PROVAG.I}
{EBRPRISTEMP.I}
{KALKBEFBTEMP.I}
 
DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE BUFFER KALKYLKATALOGSUBBUF FOR KALKYLKATALOGSUB.   
DEFINE BUFFER KALKYLKATALOGBUF FOR KALKYLKATALOG.
DEFINE BUFFER KALKHUVBUF FOR KALKHUV.
DEFINE BUFFER GURUDEFAULTSBUF FOR GURUDEFAULTS.
DEFINE BUFFER KALKFAKTORERBUF FOR KALKFAKTORER.
DEFINE BUFFER KALKEGNAPRISERBUF FOR KALKEGNAPRISER.
DEFINE BUFFER KALKMTRLBUF FOR KALKMTRL.
DEFINE BUFFER KALKNUMBUF FOR KALKNUM.
DEFINE BUFFER KALKNUMBUF2 FOR KALKNUM.
DEFINE BUFFER KALKNUMSUBBUF FOR KALKNUMSUB.
DEFINE BUFFER FREKVENSKATALOGBUF FOR FREKVENSKATALOG.
DEFINE BUFFER KALKYLARBKODERBUF FOR KALKYLARBKODER.
DEFINE BUFFER KALKYLLOPPOSTERBUF FOR KALKYLLOPPOSTER.
DEFINE BUFFER KALKYLLOPSUBBUF FOR KALKYLLOPSUB.
DEFINE BUFFER kalkaobuff FOR KALKAONR.

DEFINE INPUT  PARAMETER inglobanv AS CHARACTER NO-UNDO.

DEFINE VARIABLE aonrplan AS LOGICAL NO-UNDO. /* = true aonr = false planr*/


FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.

RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).

PROCEDURE Kontakt:
   RUN GuruAppserverKontakt.p.
END PROCEDURE.
{SparaDynDSstar.I}
{SparaDynDSstarMulti.I}




/*KalkylDS*/
{KALKYLPRODATA.i}


{SparaProDatasSet.i KalkylDS}



DEFINE QUERY KalkylQuery FOR KALKHUV.
DEFINE DATA-SOURCE KalkhuvSrc FOR QUERY KalkylQuery KALKHUV KEYS (KALKNR,OMRADE).   /*keys unika nycklar*/
DEFINE DATA-SOURCE NumSrc FOR KALKNUM KEYS (KALKNR,OMRADE,NUM).
DEFINE DATA-SOURCE NumSubSrc FOR KALKNUMSUB KEYS (KALKNR,OMRADE,NUM,NUMSUBID).
DEFINE DATA-SOURCE AonrSrc FOR KALKAONR KEYS (KALKNR,OMRADE,PLANNR,ARTAL).
 /*EXTRA KLOGSUBID*/
DEFINE DATA-SOURCE FaktSrc FOR KALKFAKTORER KEYS (KLOGSUBID,KALKNR,OMRADE,KPID).
 /*EXTRA KLOGSUBID*/
DEFINE DATA-SOURCE EgnaSrc FOR KALKEGNAPRISER KEYS (KLOGSUBID,KALKNR,OMRADE,KPID).
DEFINE DATA-SOURCE MtrlSrc FOR KALKMTRL KEYS (KALKNR,OMRADE,MID).
DEFINE DATA-SOURCE TidlSrc FOR KALKYLTIDLAGE KEYS (KALKNR,OMRADE,IDTIDLAG).

DEFINE VARIABLE hKalkylDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hKalkylDataSet = DATASET KalkylDS:HANDLE.      /*koppla handel till dataset*/

hKalkylDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylDS", THIS-PROCEDURE). 


/*KalkylDS*/

/*KalkylAnvEgenDS*/
PROCEDURE AnvEgenCreate_UI :
   
   CREATE TEMP-TABLE Kalknumanvegentth IN WIDGET-POOL "KalkDynTable".
   
   Kalknumanvegentth:CREATE-LIKE("KALKNUMANVEGEN").
   Kalknumanvegentth:ADD-NEW-FIELD("TTRECID","RECID").
   Kalknumanvegentth:ADD-NEW-FIELD("VIARBKOD","CHARACTER",0,"","SPARAD").
   Kalknumanvegentth:ADD-NEW-INDEX("MATRIS").
   Kalknumanvegentth:ADD-INDEX-FIELD("MATRIS","MATRIS").
   Kalknumanvegentth:ADD-INDEX-FIELD("MATRIS","ANVANDARE").
   Kalknumanvegentth:ADD-INDEX-FIELD("ANVEGEN","MATRIS").
   Kalknumanvegentth:ADD-INDEX-FIELD("ANVEGEN","MARKSUB").
   
   Kalknumanvegentth:ADD-NEW-INDEX("KLOGSUBID").
   Kalknumanvegentth:ADD-INDEX-FIELD("KLOGSUBID","KLOGSUBID").
   Kalknumanvegentth:ADD-NEW-INDEX("ARBKOD").
   Kalknumanvegentth:ADD-INDEX-FIELD("ARBKOD","ARBKOD").
   Kalknumanvegentth:ADD-INDEX-FIELD("ARBKOD","LOPNR").
   Kalknumanvegentth:ADD-INDEX-FIELD("ARBKOD","NUM").
   Kalknumanvegentth:ADD-NEW-INDEX("NUM").
   Kalknumanvegentth:ADD-INDEX-FIELD("NUM","NUM").

   Kalknumanvegentth:TEMP-TABLE-PREPARE("Kalknumegen").
   Kalknumanvegenbuffh = Kalknumanvegentth:DEFAULT-BUFFER-HANDLE.
   CREATE TEMP-TABLE Kalknumanvegensubtth IN WIDGET-POOL "KalkDynTable".
   Kalknumanvegensubtth:CREATE-LIKE("KALKNUMANVEGENSUB").
   Kalknumanvegensubtth:ADD-NEW-FIELD("TTRECID","RECID").
   Kalknumanvegensubtth:ADD-NEW-INDEX("BENAMNING").
   Kalknumanvegensubtth:ADD-INDEX-FIELD("BENAMNING","BENAMNING").
   
   Kalknumanvegensubtth:TEMP-TABLE-PREPARE("Kalknumegensub").
   Kalknumanvegensubbuffh = Kalknumanvegensubtth:DEFAULT-BUFFER-HANDLE. 
   
END PROCEDURE.

PROCEDURE laddaKalkylAnvEgenDS_UI :
   DEFINE INPUT  PARAMETER vad AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER DATASET-HANDLE KalkylAnvEgenDS BIND.
   RUN FINNSTABELL.P (INPUT "KALKNUMANVEGEN", OUTPUT kalknumanvegenlog).
   IF kalknumanvegenlog = TRUE THEN.
   ELSE RETURN. 
   IF VALID-HANDLE(KalkylAnvEgenDS) THEN.
   ELSE RUN AnvEgenCreate_UI.
   
   RUN GetDatasetDeftt_UI ("KalkylAnvEgenDS").  
   DatasetDeftt.antaltab = 2.
   DatasetDeftt.pcBuffers[1] = STRING(Kalknumanvegenbuffh).
   DatasetDeftt.pcBuffers[2] = STRING(Kalknumanvegensubbuffh). 
   DatasetDeftt.pcRelFields[1] = "ANVANDARE,ANVANDARE,NUM,NUM".
   DatasetDeftt.pcSources[1] = "KALKNUMANVEGEN".
   DatasetDeftt.pcSources[2] = "KALKNUMANVEGENSUB".
   DatasetDeftt.pcSourceKeys[1] = "ANVANDARE,NUM".
   DatasetDeftt.pcSourceKeys[2] = "ANVANDARE,NUM,NUMSUBID".
   DatasetDeftt.pcKeyValue[1] = "KALKNUMANVEGEN.ANVANDARE = '" + vad + "'".
   
   RUN DefAndLoadDs_UI IN dyndamicDSh
   ({DataSetInput.I} OUTPUT DATASET-HANDLE KalkylAnvEgenDS BIND).
 
END PROCEDURE.


/*BerKalkylDS*/
PROCEDURE BerKalkylCreate_UI :
   CREATE TEMP-TABLE BerKalkkopplatth IN WIDGET-POOL "KalkDynTable".
   BerKalkkopplatth:CREATE-LIKE("BERKALKOPPLA").
   BerKalkkopplatth:ADD-NEW-FIELD("TTRECID","RECID").
   BerKalkkopplatth:TEMP-TABLE-PREPARE("Berkalkkopptt").
   BerKalkkopplabuffh = BerKalkkopplatth:DEFAULT-BUFFER-HANDLE.
      
   CREATE TEMP-TABLE Bervalltth IN WIDGET-POOL "KalkDynTable".
   Bervalltth:CREATE-LIKE("BERVAL").
   Bervalltth:ADD-NEW-FIELD("KALKNR","INTEGER").
   Bervalltth:ADD-NEW-FIELD("TTRECID","RECID").
   Bervalltth:ADD-NEW-FIELD("ID2","CHARACTER").
   Bervalltth:ADD-NEW-FIELD("EXTRA1","CHARACTER").
   Bervalltth:ADD-NEW-FIELD("F1","CHARACTER").
   Bervalltth:ADD-NEW-FIELD("ORD","INTEGER").
   Bervalltth:ADD-NEW-FIELD("BERKALKAND","LOGICAL").
   Bervalltth:ADD-NEW-FIELD("NGRUPP","INTEGER").
   Bervalltth:ADD-NEW-INDEX("NUM").
   Bervalltth:ADD-INDEX-FIELD("NUM","NUM").   
   Bervalltth:ADD-NEW-INDEX("ID2").
   Bervalltth:ADD-INDEX-FIELD("ID2","ID2").
   Bervalltth:ADD-NEW-INDEX("ORD").
   Bervalltth:ADD-INDEX-FIELD("ORD","ORD").
   Bervalltth:TEMP-TABLE-PREPARE("Bervaltt").   
   Bervallbuffh = Bervalltth:DEFAULT-BUFFER-HANDLE.
   /*
   CREATE TEMP-TABLE NBervalltth IN WIDGET-POOL "KalkDynTable".
   NBervalltth:CREATE-LIKE(Bervallbuffh).
   NBervalltth:TEMP-TABLE-PREPARE("Bervaltt").
   NBervallbuffh = NBervalltth:DEFAULT-BUFFER-HANDLE.
   */
   
   CREATE TEMP-TABLE Bermtrltth IN WIDGET-POOL "KalkDynTable".
   Bermtrltth:CREATE-LIKE("BERMTRL").
   Bermtrltth:ADD-NEW-FIELD("TTRECID","RECID").
   Bermtrltth:ADD-NEW-FIELD("KUND","LOGICAL").
   Bermtrltth:TEMP-TABLE-PREPARE("Bermtrltt").
   Bermtrlbuffh = Bermtrltth:DEFAULT-BUFFER-HANDLE.   
 
   
   CREATE TEMP-TABLE HdSchakttth IN WIDGET-POOL "KalkDynTable".
   HdSchakttth:CREATE-LIKE("HDSCHAKT").
   HdSchakttth:ADD-NEW-FIELD("KALKNR","INTEGER").
   HdSchakttth:ADD-NEW-FIELD("TTRECID","RECID").
   HdSchakttth:TEMP-TABLE-PREPARE("Schakttt").
   HdSchaktbuffh = HdSchakttth:DEFAULT-BUFFER-HANDLE.
   
   CREATE TEMP-TABLE HdSchaktStrackatth IN WIDGET-POOL "KalkDynTable".
   HdSchaktStrackatth:CREATE-LIKE("HDSCHAKTPROT").
   HdSchaktStrackatth:ADD-NEW-FIELD("KALKNR","INTEGER").
   HdSchaktStrackatth:ADD-NEW-FIELD("TTRECID","RECID").
   HdSchaktStrackatth:TEMP-TABLE-PREPARE("SchaktStrackatt").
   HdSchaktStrackabuffh = HdSchaktStrackatth:DEFAULT-BUFFER-HANDLE.
  
   
END PROCEDURE.
PROCEDURE KalkBerVisa_UI :
   DEFINE INPUT  PARAMETER Bernummer AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER OmrVar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER felmed AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER Kalknr AS INTEGER NO-UNDO.
   CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "KalkDynTable".
   berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(Bernummer) + " AND OMRADE = '" + OmrVar + "'", NO-LOCK) NO-ERROR.
   IF berkopptabbuffh:AVAILABLE THEN.
   ELSE DO:
      felmed = "Det finns ingen Kalkyl".
      RETURN.
   END.   
   Kalknr = berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE. 
     
END PROCEDURE.
PROCEDURE NyBerKalkyl_UI :
    DEFINE INPUT  PARAMETER BernrVar AS INTEGER NO-UNDO.
    DEFINE INPUT  PARAMETER OmrVar AS CHARACTER NO-UNDO.
    DEFINE OUTPUT PARAMETER NyBerKalkyl AS LOGICAL NO-UNDO.
    CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "KalkDynTable".
    berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(BernrVar) + " AND OMRADE = '" + OmrVar + "'", NO-LOCK) NO-ERROR.
    IF berkopptabbuffh:AVAILABLE THEN DO:
       NyBerKalkyl = FALSE.
       
    END.   
    ELSE NyBerKalkyl = TRUE.
    berkopptabbuffh:BUFFER-RELEASE() NO-ERROR.
END PROCEDURE.

PROCEDURE KalkyleraiBeredning_UI :
   
   DEFINE INPUT  PARAMETER klogidvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER BernrVar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER OmrVar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER felmedd AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER ejstart AS LOGICAL NO-UNDO.
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
  
   RUN FINNSTABELL.P (INPUT "BERKALKOPPLA", OUTPUT berkopplalogg).
   IF berkopplalogg = TRUE THEN.
   ELSE DO:
      ejstart = TRUE. 
      RETURN.
   END.  
  
   CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "KalkDynTable".
   CREATE BUFFER berdningtabbuffh FOR TABLE "BEREDNING" IN WIDGET-POOL "KalkDynTable".
   
   berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(BernrVar) + " AND OMRADE = '" + OmrVar + "'", NO-LOCK) NO-ERROR.
   IF berkopptabbuffh:AVAILABLE THEN.
   ELSE DO:
      RUN omradekoll_UI  (INPUT OmrVar, OUTPUT KalkNrvar).  
      IF KalkNrvar = ? THEN DO:
         felmedd = "Det g�r inte att l�gga upp kalkyler p� detta " + LC(Guru.Konstanter:gomrk) + ". Nummerserie saknas eller �r fylld.".
         ejstart = TRUE.
         RETURN.
      END.         
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
      berdningtabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(BernrVar) + " AND OMRADE = '" + OmrVar + "'",NO-LOCK) NO-ERROR.
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = berdningtabbuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE NO-LOCK NO-ERROR.
      
      IF NOT AVAILABLE ANVANDARE THEN DO:
         FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
           
      END.
      IF ANVANDARE.PERSONALKOD = "" THEN DO:
         FIND FIRST ANVANDARE WHERE ANVANDARE.PERSONALKOD NE "" NO-LOCK NO-ERROR.
      END.   
      DO TRANSACTION:
         CREATE KALKHUV.
         ASSIGN
         KALKHUV.AKTIV = TRUE
         KALKHUV.BENAMNING = berdningtabbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE 
         KALKHUV.KALKANV = ANVANDARE.PERSONALKOD
         KALKHUV.ANVANDARE = berdningtabbuffh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE
         KALKHUV.KALKNR = KalkNrvar
         KALKHUV.OMRADE = OmrVar
         KALKHUV.BESTID = OmrVar
         KALKHUV.TYPKALK = Guru.Konstanter:varforetypval[53].
         IF klogidvar = 0 THEN KALKHUV.KLOGID = KALKYLKATALOG.KLOGID.
         ELSE KALKHUV.KLOGID = klogidvar.
         berkopptabbuffh:BUFFER-CREATE().
         berkopptabbuffh:BUFFER-FIELD("AONR"):BUFFER-VALUE = STRING(BernrVar).
         berkopptabbuffh:BUFFER-FIELD("BERNR"):BUFFER-VALUE = BernrVar.
         berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = OmrVar.
         berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = KALKHUV.KALKNR.
         berkopptabbuffh:BUFFER-FIELD("OMRADEKALK"):BUFFER-VALUE = KALKHUV.OMRADE.
      END.   
   END.
   RELEASE KALKHUV NO-ERROR.
   KalkNrvar = berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE .
   berkalktabbuffh:BUFFER-RELEASE () NO-ERROR.
   berdningtabbuffh:BUFFER-RELEASE () NO-ERROR.
   DELETE OBJECT berkopptabbuffh  NO-ERROR.
   berkopptabbuffh = ?.
   DELETE OBJECT berdningtabbuffh NO-ERROR.
   berdningtabbuffh = ?.
   
END PROCEDURE.

PROCEDURE KalkyleraiBeredningMulti_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR berkalktt.
   CREATE BUFFER berkopptabbuffh FOR TABLE "BERKALKOPPLA" IN WIDGET-POOL "KalkDynTable".
   FOR EACH berkalktt WHERE NO-LOCK:
      berkopptabbuffh:FIND-FIRST("WHERE BERNR = " + STRING(berkalktt.BERNR) + " AND OMRADE = '" + berkalktt.OMRADE + "'", NO-LOCK) NO-ERROR.
      IF berkopptabbuffh:AVAILABLE THEN DO:
         berkalktt.KALKNR = berkopptabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
         berkalktt.OMRADE = berkopptabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE.
      END.   
      ELSE DO:
         DELETE berkalktt. 
      END.   
   END.
   DELETE OBJECT berkopptabbuffh  NO-ERROR.
   berkopptabbuffh = ?.
   
END PROCEDURE.


/*k�rs inte d� hdkalk och berkalk inte ska anv�ndas*/
PROCEDURE egen_UI :
   DEFINE INPUT  PARAMETER BernrVar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER KalkNrvar AS INTEGER  NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER egenkod AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lopnrkod AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER numvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE lopnrkodorg AS INTEGER NO-UNDO.
   DEFINE VARIABLE benvar AS CHARACTER NO-UNDO.
   lopnrkodorg = lopnrkod.
   kalknumtabbuffh:BUFFER-CREATE().
   IF hdkalktabkbuffh:AVAILABLE THEN DO:
      kalknumtabbuffh:BUFFER-COPY(hdkalktabkbuffh).
   END.    
   IF berkalktabbuffh:AVAILABLE THEN DO:
      kalknumtabbuffh:BUFFER-COPY(berkalktabbuffh,"NUM").
   END.
   benvar   = kalknumtabbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE.
   kalknumtabbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = "".
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar AND KALKHUV.OMRADE = omrvar NO-LOCK NO-ERROR.
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
   END. 
   ASSIGN
   kalknumtabbuffh:BUFFER-FIELD("KLOGSUBID"):BUFFER-VALUE = KALKYLKATALOG.HKLOGSUBID
   kalknumtabbuffh:BUFFER-FIELD("BENAMNING"):BUFFER-VALUE = SUBSTRING(benvar,1,40)
   kalknumtabbuffh:BUFFER-FIELD("ANMARKNING"):BUFFER-VALUE = SUBSTRING(benvar,50)           
   kalknumtabbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = KalkNrvar
   kalknumtabbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = omrvar
   kalknumtabbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = 1  
   kalknumtabbuffh:BUFFER-FIELD("TYPKALK"):BUFFER-VALUE = 2.
   
   RUN sistanumhuv_UI (OUTPUT numvar).
   kalknumtabbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE = numvar. 
   IF NOT VALID-HANDLE(AppServerExtraHandle) THEN RUN KALKBERAPPDSEXTRA.p PERSISTENT SET AppServerExtraHandle (INPUT Guru.Konstanter:globanv).
   RUN egensub_UI IN AppServerExtraHandle (INPUT BernrVar,INPUT KalkNrvar, INPUT omrvar, INPUT egenkod, INPUT lopnrkod, INPUT numvar).
END PROCEDURE.

  
PROCEDURE ReladdaBerkalkDS_UI :
   
   DEFINE INPUT  PARAMETER berkalkvad AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER bervalvad AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER bermtrlvad AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER schaktvad AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER DATASET-HANDLE BerKalkDS BIND.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE BerValDS BIND.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE BerMtrlDS BIND.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE HdSchaktDS BIND.
   DEFINE OUTPUT PARAMETER TABLE-HANDLE OutHdSchaktStrackatth.
   OutHdSchaktStrackatth = HdSchaktStrackatth.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE q2h AS HANDLE NO-UNDO.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kommandoquery2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE beridbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE mtrlbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE berordbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE sokhelpstring AS CHARACTER NO-UNDO.
   IF NOT VALID-HANDLE(BerKalkkopplatth) THEN RUN BerKalkylCreate_UI.
   RUN GetDatasetDeftt_UI ("BerKalkDS").
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(BerKalkkopplabuffh).
   DatasetDeftt.pcSources[1] = "BERKALKOPPLA".
   DatasetDeftt.pcSourceKeys[1] = "BERNR,OMRADE,KALKNR".
   DatasetDeftt.pcKeyValue[1] = berkalkvad.

   RUN ReloadDs_UI IN dyndamicDSMh[1] 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE BerKalkDS BIND).
   
   RUN GetDatasetDeftt_UI ("BerValDS").
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(Bervallbuffh).
   DatasetDeftt.pcSources[1] = "BERVAL".
   DatasetDeftt.pcSourceKeys[1] = "AONR,OMRADE,NUM,SKAPNUM".
   DatasetDeftt.pcKeyValue[1] = bervalvad.
  
   RUN ReloadDs_UI IN dyndamicDSMh[2]
   ({DataSetInput.I} OUTPUT DATASET-HANDLE BerValDS BIND).
   RUN GetDatasetDeftt_UI ("BerMtrlDS").
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(Bermtrlbuffh).
   DatasetDeftt.pcSources[1] = "BERMTRL".
   DatasetDeftt.pcSourceKeys[1] = "AONR,OMRADE,NUM".  /*EJ UNIKT*/
   DatasetDeftt.pcKeyValue[1] = bermtrlvad.
   RUN ReloadDs_UI IN dyndamicDSMh[3] 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE BerMtrlDS BIND). 
   RUN GetDatasetDeftt_UI ("HdschaktDS").  
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(HdSchaktbuffh).
   
   DatasetDeftt.pcSources[1] = "HDSCHAKT".
   DatasetDeftt.pcSourceKeys[1] = "BERNR,OMRADE,SID".
   DatasetDeftt.pcKeyValue[1] = schaktvad.
   RUN ReloadDs_UI IN dyndamicDSMh[4] 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE HdSchaktDS BIND).
  
   CREATE BUFFER berordbuffh FOR TABLE "BERORD" IN WIDGET-POOL "KalkDynTable".
   CREATE BUFFER beridbuffh FOR TABLE "BERID" IN WIDGET-POOL "KalkDynTable".
   CREATE BUFFER mtrlbuffh FOR TABLE "MTRL" IN WIDGET-POOL "KalkDynTable".
   kommandoquery = "FOR EACH " + Bervallbuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT Bervallbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   
   DO WHILE qH:QUERY-OFF-END = FALSE:
      kommandoquery2 = "FOR EACH " + beridbuffh:TABLE + 
      " WHERE BERID.AONR = '" + Bervallbuffh:BUFFER-FIELD("AONR"):BUFFER-VALUE + "' AND BERID.OMRADE = '" + Bervallbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE +
      "' AND BERID.NUM = " + STRING(Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
      RUN CreateCustomQuery(INPUT beridbuffh,INPUT kommandoquery2,OUTPUT q2h).
      q2H:GET-FIRST().
      DO WHILE q2H:QUERY-OFF-END = FALSE: 
         Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE = beridbuffh:BUFFER-FIELD("FRI2"):BUFFER-VALUE.
         Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE = beridbuffh:BUFFER-FIELD("FRI3"):BUFFER-VALUE.
         q2H:GET-NEXT().  
      END.
     
      sokhelpstring = " where BERORD.AONR = '" + Bervallbuffh:BUFFER-FIELD("AONR"):BUFFER-VALUE + 
      "' AND BERORD.OMRADE = '" + Bervallbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE + 
      "' AND BERORD.NUM = " + STRING(Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
    
      berordbuffh:FIND-FIRST(sokhelpstring,NO-LOCK) NO-ERROR.
      IF berordbuffh:AVAILABLE THEN DO:
         Bervallbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE = berordbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE.
         
      END.
/*          
      FIND FIRST BERORD WHERE BERORD.AONR = Bervallbuffh:BUFFER-FIELD("AONR"):BUFFER-VALUE AND
      BERORD.OMRADE = Bervallbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE AND BERORD.NUM = Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE NO-LOCK NO-ERROR.
      IF AVAILABLE BERORD THEN DO:
         Bervallbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE = BERORD.ORD.
      END.
     
*/
      qH:GET-NEXT().     
   END.
   RUN CloseCustomQuery(INPUT qH).
   kommandoquery = "FOR EACH " + Bermtrlbuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT Bermtrlbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      kommandoquery2 = "WHERE MTRL.LEVKOD = '" + Bermtrlbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE + "' AND MTRL.ENR = '" + Bermtrlbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE + "'".
      mtrlbuffh:FIND-FIRST(kommandoquery2,NO-LOCK) NO-ERROR.
      IF mtrlbuffh:AVAILABLE THEN DO:
         
         Bermtrlbuffh:BUFFER-FIELD("KUND"):BUFFER-VALUE = mtrlbuffh:BUFFER-FIELD("KUND"):BUFFER-VALUE.
      END. 
      qH:GET-NEXT().
   END.
   RUN CloseCustomQuery(INPUT qH).
   DELETE OBJECT qH NO-ERROR.
   qH = ?.
   RUN BerKalkKalnr_UI.   
   OutHdSchaktStrackatth = HdSchaktStrackatth.
END PROCEDURE.
PROCEDURE KalkyAonrBerFinns_UI :
   DEFINE INPUT  PARAMETER knr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER finns AS LOGICAL NO-UNDO.
   IF knr = ? THEN RETURN.
   FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = knr NO-LOCK NO-ERROR.
   IF AVAILABLE KALKAONR THEN DO:
      IF KALKAONR.AONR = ? THEN RETURN.
      FIND FIRST BEREDNING WHERE BEREDNING.AONR = KALKAONR.AONR AND BEREDNING.DELNR = KALKAONR.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE BEREDNING THEN DO:
         finns = TRUE.
      END.
   END.   
END PROCEDURE.
PROCEDURE BerMtrlPris_UI :
   DEFINE INPUT  PARAMETER knr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER mpris AS DECIMAL NO-UNDO.
   FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = knr NO-LOCK NO-ERROR.
   IF AVAILABLE KALKAONR THEN DO:
      FIND FIRST BEREDNING WHERE BEREDNING.AONR = KALKAONR.AONR AND BEREDNING.DELNR = KALKAONR.DELNR NO-LOCK NO-ERROR.
      IF AVAILABLE BEREDNING THEN DO:
         FOR EACH BERMTRL WHERE BERMTRL.AONR = STRING(BEREDNING.BERNR) AND BERMTRL.OMRADE = BEREDNING.OMRADE AND BERMTRL.INKOP = FALSE NO-LOCK:
            mpris = mpris + BERMTRL.ANTAL * BERMTRL.PRIS. 
         END.
      END.
   END.   
END PROCEDURE.
PROCEDURE BerKalkKalnr_UI :
/*  
  DEFINE {&PUBLIC}  VARIABLE HdSchaktStrackatth AS HANDLE NO-UNDO.
DEFINE {&PUBLIC}  VARIABLE HdSchaktStrackabuffh AS HANDLE NO-UNDO.
HdSchaktStrackabuffh = HdSchaktStrackatth:DEFAULT-BUFFER-HANDLE.
BerKalkkopplabuffh
*/
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE q2h AS HANDLE NO-UNDO.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kommandoquery2 AS CHARACTER NO-UNDO.
   kommandoquery = "FOR EACH " + BerKalkkopplabuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT BerKalkkopplabuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      kommandoquery2 = "FOR EACH " + Bervallbuffh:TABLE + " WHERE AONR = " + QUOTER(BerKalkkopplabuffh:BUFFER-FIELD("BERNR"):BUFFER-VALUE) + " AND OMRADE = " + QUOTER(BerKalkkopplabuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE).
      RUN CreateCustomQuery(INPUT Bervallbuffh,INPUT kommandoquery2,OUTPUT q2h).
      q2h:GET-FIRST().
      DO WHILE q2h:QUERY-OFF-END = FALSE:
         Bervallbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = BerKalkkopplabuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
         q2h:GET-NEXT().
      END. 
      kommandoquery2 = "FOR EACH " + HdSchaktbuffh:TABLE + " WHERE BERNR = " + QUOTER(BerKalkkopplabuffh:BUFFER-FIELD("BERNR"):BUFFER-VALUE) + " AND OMRADE = " + QUOTER(BerKalkkopplabuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE).
      RUN CreateCustomQuery(INPUT HdSchaktbuffh,INPUT kommandoquery2,OUTPUT q2h).
      q2h:GET-FIRST().
      DO WHILE q2h:QUERY-OFF-END = FALSE:
         HdSchaktbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = BerKalkkopplabuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.
         q2h:GET-NEXT().
      END. 
      qH:GET-NEXT().
   END.
   RUN CloseCustomQuery(INPUT qH).
   RUN CloseCustomQuery(INPUT q2H).
   BerKalkkopplabuffh:FIND-FIRST("WHERE  ",NO-LOCK) NO-ERROR.
   IF BerKalkkopplabuffh:AVAILABLE THEN DO:
      FIND FIRST BERKALKOPPLA WHERE BERKALKOPPLA.KALKNR = BerKalkkopplabuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE NO-LOCK NO-ERROR.
      IF AVAILABLE BERKALKOPPLA THEN DO:
         FOR EACH HDSCHAKTPROT WHERE HDSCHAKTPROT.BERNR = BERKALKOPPLA.BERNR AND HDSCHAKTPROT.OMRADE = BERKALKOPPLA.OMRADE NO-LOCK:
             HdSchaktStrackabuffh:BUFFER-CREATE().
             HdSchaktStrackabuffh:BUFFER-COPY(BUFFER HDSCHAKTPROT:HANDLE).
             HdSchaktStrackabuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = BERKALKOPPLA.KALKNR.
             HdSchaktStrackabuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = HdSchaktStrackabuffh:RECID.
         END.
      END.    
   END.
   DELETE OBJECT qH NO-ERROR.
   qH = ?.
   DELETE OBJECT q2H NO-ERROR.
   q2H = ?.      
END PROCEDURE. 
PROCEDURE laddaBerkalkDS_UI :
   
   DEFINE INPUT  PARAMETER berkalkvad AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER bervalvad AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER bermtrlvad AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER schaktvad AS CHARACTER NO-UNDO. 
   DEFINE OUTPUT PARAMETER DATASET-HANDLE BerKalkDS BIND.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE BerValDS BIND.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE BerMtrlDS BIND.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE HdSchaktDS BIND.
   DEFINE OUTPUT PARAMETER TABLE-HANDLE OutHdSchaktStrackatth.
   
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE q2h AS HANDLE NO-UNDO.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kommandoquery2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE beridbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE mtrlbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE berordbuffh AS HANDLE NO-UNDO.
   DEFINE VARIABLE sokhelpstring AS CHARACTER NO-UNDO.
   IF NOT VALID-HANDLE(BerKalkkopplatth) THEN RUN BerKalkylCreate_UI.
   
 
   RUN GetDatasetDeftt_UI ("BerKalkDS").
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(BerKalkkopplabuffh).
   
   DatasetDeftt.pcSources[1] = "BERKALKOPPLA".
   
   DatasetDeftt.pcSourceKeys[1] = "BERNR,OMRADE,KALKNR".
   DatasetDeftt.pcKeyValue[1] = berkalkvad.
 
   RUN DefAndLoadDsRe_UI IN dyndamicDSMh[1] 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE BerKalkDS BIND).
  
   RUN GetDatasetDeftt_UI ("BerValDS").
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(Bervallbuffh).
   DatasetDeftt.pcSources[1] = "BERVAL".
   DatasetDeftt.pcSourceKeys[1] = "AONR,OMRADE,NUM,SKAPNUM".
   DatasetDeftt.pcKeyValue[1] = bervalvad.
   RUN DefAndLoadDsRe_UI IN dyndamicDSMh[2] 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE BerValDS BIND).
   
   RUN GetDatasetDeftt_UI ("BerMtrlDS").
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(Bermtrlbuffh).
   DatasetDeftt.pcSources[1] = "BERMTRL".
   DatasetDeftt.pcSourceKeys[1] = "AONR,OMRADE,NUM".  /*EJ UNIKT*/
   DatasetDeftt.pcKeyValue[1] = bermtrlvad.
   RUN DefAndLoadDsRe_UI IN dyndamicDSMh[3] 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE BerMtrlDS BIND). 
   RUN GetDatasetDeftt_UI ("HdschaktDS").  
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(HdSchaktbuffh).
   DatasetDeftt.pcSources[1] = "HDSCHAKT".
   DatasetDeftt.pcSourceKeys[1] = "BERNR,OMRADE,SID".
   DatasetDeftt.pcKeyValue[1] = schaktvad.
   RUN DefAndLoadDsRe_UI IN dyndamicDSMh[4] 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE HdSchaktDS BIND).
   
   CREATE BUFFER berordbuffh FOR TABLE "BERORD" IN WIDGET-POOL "KalkDynTable".
   CREATE BUFFER beridbuffh FOR TABLE "BERID" IN WIDGET-POOL "KalkDynTable".
   CREATE BUFFER mtrlbuffh FOR TABLE "MTRL" IN WIDGET-POOL "KalkDynTable".
   kommandoquery = "FOR EACH " + Bervallbuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT Bervallbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   
   DO WHILE qH:QUERY-OFF-END = FALSE:
      kommandoquery2 = "FOR EACH " + beridbuffh:TABLE + 
      " WHERE BERID.AONR = '" + Bervallbuffh:BUFFER-FIELD("AONR"):BUFFER-VALUE + "' AND BERID.OMRADE = '" + Bervallbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE +
      "' AND BERID.NUM = " + STRING(Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE) + " NO-LOCK".
      RUN CreateCustomQuery(INPUT beridbuffh,INPUT kommandoquery2,OUTPUT q2h).
      q2H:GET-FIRST().
      DO WHILE q2H:QUERY-OFF-END = FALSE: 
         Bervallbuffh:BUFFER-FIELD("ID2"):BUFFER-VALUE = beridbuffh:BUFFER-FIELD("FRI2"):BUFFER-VALUE.
         Bervallbuffh:BUFFER-FIELD("EXTRA1"):BUFFER-VALUE = beridbuffh:BUFFER-FIELD("FRI3"):BUFFER-VALUE.
         q2H:GET-NEXT().  
      END.
     
      sokhelpstring = " where BERORD.AONR = '" + Bervallbuffh:BUFFER-FIELD("AONR"):BUFFER-VALUE + 
      "' AND BERORD.OMRADE = '" + Bervallbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE + 
      "' AND BERORD.NUM = " + STRING(Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE).
    
      berordbuffh:FIND-FIRST(sokhelpstring,NO-LOCK) NO-ERROR.
      IF berordbuffh:AVAILABLE THEN DO:
         Bervallbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE = berordbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE.
         
      END.
/*          
      FIND FIRST BERORD WHERE BERORD.AONR = Bervallbuffh:BUFFER-FIELD("AONR"):BUFFER-VALUE AND
      BERORD.OMRADE = Bervallbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE AND BERORD.NUM = Bervallbuffh:BUFFER-FIELD("NUM"):BUFFER-VALUE NO-LOCK NO-ERROR.
      IF AVAILABLE BERORD THEN DO:
         Bervallbuffh:BUFFER-FIELD("ORD"):BUFFER-VALUE = BERORD.ORD.
      END.
     
*/
      qH:GET-NEXT().     
   END.
   RUN CloseCustomQuery(INPUT qH).
   kommandoquery = "FOR EACH " + Bermtrlbuffh:TABLE + " NO-LOCK". 
   RUN CreateCustomQuery(INPUT Bermtrlbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      kommandoquery2 = "WHERE MTRL.LEVKOD = '" + Bermtrlbuffh:BUFFER-FIELD("LEVKOD"):BUFFER-VALUE + "' AND MTRL.ENR = '" + Bermtrlbuffh:BUFFER-FIELD("ENR"):BUFFER-VALUE + "'".
      mtrlbuffh:FIND-FIRST(kommandoquery2,NO-LOCK) NO-ERROR.
      IF mtrlbuffh:AVAILABLE THEN DO:
         
         Bermtrlbuffh:BUFFER-FIELD("KUND"):BUFFER-VALUE = mtrlbuffh:BUFFER-FIELD("KUND"):BUFFER-VALUE.
      END. 
      qH:GET-NEXT().
   END.
   RUN CloseCustomQuery(INPUT qH).
   DELETE OBJECT qH NO-ERROR.
   qH = ?. 
  
   RUN BerKalkKalnr_UI.
   DEBUGGER:SET-BREAK().
   
   OutHdSchaktStrackatth = HdSchaktStrackatth.
   
END PROCEDURE.
      

/*KalkylAnvEgenDS*/
{KALKYLKATPRODATA.i}

{SparaProDatasSet.i KalkylKatDS}
{SparaProDatasSet.i KalkylKoderDS}
{SparaProDatasSet.i KalkylLoparDS}
{SparaProDatasSet.i KalkylFrekDS}
{SparaProDatasSet.i KalkylMallarDS}
/*KalkylKatDS*/
DEFINE QUERY KalkylKatQuery FOR KALKYLKATALOG.
DEFINE QUERY KalkylSubKatQuery FOR KALKYLKATALOGSUB.
DEFINE QUERY KalkylPrisKatQuery FOR KALKYLPRISER.
DEFINE QUERY KalkylKatVisningQuery FOR KALKVISNING.
DEFINE DATA-SOURCE KalkKatSrc FOR QUERY KalkylKatQuery KALKYLKATALOG KEYS (KLOGID).   /*keys unika nycklar*/
DEFINE DATA-SOURCE SubKatSrc FOR KALKYLKATALOGSUB KEYS (KLOGID,KLOGSUBID).
DEFINE DATA-SOURCE KatPrisSrc FOR KALKYLPRISER KEYS (KLOGSUBID,KPID).
DEFINE DATA-SOURCE VisaKatSrc FOR QUERY KalkylKatVisningQuery KALKVISNING KEYS (KVID).
DEFINE DATA-SOURCE AnvKatSrc FOR KALKYLKATALOGANV KEYS (KLOGID,ANVANDARE).


DEFINE VARIABLE hKalkylKatDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hKalkylKatDataSet = DATASET KalkylKatDS:HANDLE.      /*koppla handel till dataset*/
hKalkylKatDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylKatDS", THIS-PROCEDURE).
/*KalkylKatDS*/
/*KalkylKoderDS*/
DEFINE QUERY KalkylArbQuery FOR KALKYLARBKODER.
DEFINE DATA-SOURCE KalkKodSrc FOR QUERY KalkylArbQuery KALKYLARBKODER KEYS (KLOGSUBID,ARBKOD).   /*keys unika nycklar*/


/*KalkylKoderDS
DEFINE DATA-SOURCE LopSrc FOR KALKYLLOPPOSTER KEYS (KLOGSUBID,ARBKOD,LOPNR).
DEFINE DATA-SOURCE LopSubSrc FOR KALKYLLOPSUB KEYS (KLOGSUBID,ARBKOD,LOPNR,KPID).
*/

DEFINE VARIABLE hKalkylKoderDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hKalkylKoderDataSet = DATASET KalkylKoderDS:HANDLE.      /*koppla handel till dataset*/
hKalkylKoderDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylKoderDS", THIS-PROCEDURE). 

/*KalkylKoderDS*/

/*KalkylLoparDS*/
DEFINE QUERY KalkylLopQuery FOR KALKYLLOPPOSTER.
DEFINE DATA-SOURCE LopSrc FOR QUERY KalkylLopQuery KALKYLLOPPOSTER KEYS (KLOGSUBID,ARBKOD,LOPNR).
DEFINE DATA-SOURCE LopSubSrc FOR KALKYLLOPSUB KEYS (KLOGSUBID,ARBKOD,LOPNR,KPID).

DEFINE VARIABLE hKalkylLopDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hKalkylLopDataSet = DATASET KalkylLoparDS:HANDLE.      /*koppla handel till dataset*/
hKalkylLopDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylLoparDS", THIS-PROCEDURE). 

/*KalkylLoparDS*/


/*KalkylFrekDS*/
DEFINE QUERY KalkylFrekQuery FOR FREKVENSKATALOG.
DEFINE DATA-SOURCE KalkylFrekSrc FOR QUERY KalkylFrekQuery FREKVENSKATALOG KEYS (KLOGSUBID,ARBKOD,LOPNR,FREKOD,FREKNR).   /*keys unika nycklar*/

DEFINE VARIABLE hKalkylFrekDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hKalkylFrekDataSet = DATASET KalkylFrekDS:HANDLE.      /*koppla handel till dataset*/
hKalkylFrekDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylFrekDS", THIS-PROCEDURE). 

/*KalkylFrekDS*/
/*KalkylMallarDS*/
DEFINE QUERY KalkylMallQuery FOR KALKMALLHUVUD.
DEFINE DATA-SOURCE MallSrc FOR QUERY KalkylMallQuery KALKMALLHUVUD KEYS (MALLNR).
DEFINE DATA-SOURCE MallArbSrc FOR KALKMALLKODER KEYS (MALLNR,ARBKOD,LOPNR).

DEFINE VARIABLE hKalkylMallDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hKalkylMallDataSet = DATASET KalkylMallarDS:HANDLE.      /*koppla handel till dataset*/
hKalkylMallDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylMallarDS", THIS-PROCEDURE). 
/*KalkylMallarDS*/
PROCEDURE attachKalkylKatDS: /*kopplar ihop temptabell med skarptababell.      */
   hKalkylKatDataSet:GET-BUFFER-HANDLE("kalkylkatalogtt"):ATTACH-DATA-SOURCE(DATA-SOURCE KalkKatSrc:HANDLE).
   hKalkylKatDataSet:GET-BUFFER-HANDLE("kalkylkatalogsubtt"):ATTACH-DATA-SOURCE(DATA-SOURCE SubKatSrc:HANDLE).
   hKalkylKatDataSet:GET-BUFFER-HANDLE("kalkylprisertt"):ATTACH-DATA-SOURCE(DATA-SOURCE KatPrisSrc:HANDLE).
   hKalkylKatDataSet:GET-BUFFER-HANDLE("kalkvisningtt"):ATTACH-DATA-SOURCE(DATA-SOURCE VisaKatSrc:HANDLE).
   hKalkylKatDataSet:GET-BUFFER-HANDLE("kalkanvtt"):ATTACH-DATA-SOURCE(DATA-SOURCE AnvKatSrc:HANDLE).
END PROCEDURE.


PROCEDURE LaddaKataloger:
   DEFINE INPUT PARAMETER KatIdvar AS INTEGER.
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylKatDS.
   DEFINE OUTPUT PARAMETER TABLE FOR markfiltertt. 
   EMPTY TEMP-TABLE markfiltertt NO-ERROR. 
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DEFINE VARIABLE queryprepsub AS CHARACTER NO-UNDO.
   DEFINE VARIABLE querypreppris AS CHARACTER NO-UNDO.
   DATASET KalkylKatDS:EMPTY-DATASET().
   IF KatIdvar > 0 THEN DO:
      queryprep = "FOR EACH KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = " + STRING(KatIdvar) +  " NO-LOCK".
      queryprepsub = "FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = " + STRING(KatIdvar) +  " NO-LOCK". 
      querypreppris = "FOR EACH KALKYLPRISER NO-LOCK".
       
   END.
   ELSE DO:
      queryprep = "FOR EACH KALKYLKATALOG NO-LOCK".
      queryprepsub = "FOR EACH KALKYLKATALOGSUB NO-LOCK".
      querypreppris = "FOR EACH KALKYLPRISER NO-LOCK".
   END.
   QUERY KalkylKatQuery:QUERY-PREPARE(queryprep).
   QUERY KalkylSubKatQuery:QUERY-PREPARE(queryprepsub).
   QUERY KalkylPrisKatQuery:QUERY-PREPARE(querypreppris).
   QUERY KalkylKatVisningQuery:QUERY-PREPARE("FOR EACH KALKVISNING NO-LOCK").
   
   RUN attachKalkylKatDS.
   DATASET KalkylKatDS:FILL().
   detachDataSetKalkylKatDS(hKalkylKatDataSet).
   CREATE markfiltertt.
   markfiltertt.MARKNING = "Region".
   FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.MARKNING NE "" NO-LOCK:
      FIND FIRST markfiltertt WHERE markfiltertt.MARKNING = KALKYLARBKODER.MARKNING NO-LOCK NO-ERROR.
      IF NOT AVAILABLE markfiltertt THEN DO:
         CREATE markfiltertt.
         markfiltertt.MARKNING = CAPS(SUBSTRING(KALKYLARBKODER.MARKNING,1,1)) + LC(SUBSTRING(KALKYLARBKODER.MARKNING,2)).
      END.
   END.    
   
END PROCEDURE.
PROCEDURE LaddaDelKataloger:
   DEFINE INPUT PARAMETER SubKatIdvar AS INTEGER.
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylKatDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DEFINE VARIABLE queryprepsub AS CHARACTER NO-UNDO.
   DEFINE VARIABLE querypreppris AS CHARACTER NO-UNDO.
   DATASET KalkylKatDS:EMPTY-DATASET().
   /*
   queryprep = "FOR EACH KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = " + ? +  " NO-LOCK".
    */
   queryprepsub = "FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGSUBID = " + STRING(SubKatIdvar) +  " NO-LOCK". 
   querypreppris = "FOR EACH KALKYLPRISER NO-LOCK".
   /*    
   QUERY KalkylKatQuery:QUERY-PREPARE(queryprep).
   */
   QUERY KalkylSubKatQuery:QUERY-PREPARE(queryprepsub).
   QUERY KalkylPrisKatQuery:QUERY-PREPARE(querypreppris).
   QUERY KalkylKatVisningQuery:QUERY-PREPARE("FOR EACH KALKVISNING NO-LOCK").
   
   RUN attachKalkylKatDS.
   DATASET KalkylKatDS:FILL().
   detachDataSetKalkylKatDS(hKalkylKatDataSet).
  
END PROCEDURE.


PROCEDURE attachKalkylKoderDS: /*kopplar ihop temptabell med skarptababell.      */
   hKalkylKoderDataSet:GET-BUFFER-HANDLE("kalkylarbkodertt"):ATTACH-DATA-SOURCE(DATA-SOURCE KalkKodSrc:HANDLE).
   /*KalkylKoderDS
   hKalkylKoderDataSet:GET-BUFFER-HANDLE("kalkylloppostertt"):ATTACH-DATA-SOURCE(DATA-SOURCE LopSrc:HANDLE).
   hKalkylKoderDataSet:GET-BUFFER-HANDLE("kalkyllopsubtt"):ATTACH-DATA-SOURCE(DATA-SOURCE LopSubSrc:HANDLE).
   */
END PROCEDURE.

PROCEDURE LaddaKatalogerKoder:
   DEFINE INPUT PARAMETER KatSubIdvar AS INTEGER.
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylKoderDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DATASET KalkylKoderDS:EMPTY-DATASET().
   queryprep = "FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = " + STRING(KatSubIdvar) +  " NO-LOCK". 
   QUERY KalkylArbQuery:QUERY-PREPARE(queryprep).      
   RUN attachKalkylKoderDS.
   DATASET KalkylKoderDS:FILL().
   /*KalkylKoderDS
   FOR EACH KALKYLPRISER  WHERE KALKYLPRISER.KLOGSUBID = KatSubIdvar NO-LOCK:
      FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.KLOGSUBID = KatSubIdvar AND kalkyllopsubtt.KPID =  KALKYLPRISER.KPID NO-LOCK:
         ASSIGN
         kalkyllopsubtt.PRIS = KALKYLPRISER.PRIS
         kalkyllopsubtt.BENAMNING = KALKYLPRISER.BENAMNING.
      END.
   END.
   */
   detachDataSetKalkylKoderDS(hKalkylKoderDataSet).
   
END PROCEDURE.

PROCEDURE attachKalkylFrekDS: /*kopplar ihop temptabell med skarptababell.      */
   hKalkylFrekDataSet:GET-BUFFER-HANDLE("frekvenstemp"):ATTACH-DATA-SOURCE(DATA-SOURCE KalkylFrekSrc:HANDLE).
   
END PROCEDURE.

PROCEDURE LaddaFrekvensHmt :
   DEFINE INPUT PARAMETER katidvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kodvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER lopvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylFrekDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DATASET KalkylFrekDS:EMPTY-DATASET().
   IF kodvar NE ? THEN DO:
      queryprep = "FOR EACH FREKVENSKATALOG WHERE FREKVENSKATALOG.KLOGSUBID = " + STRING(katidvar) +  
                  " AND FREKVENSKATALOG.ARBKOD = '" + kodvar + "' AND FREKVENSKATALOG.LOPNR = " + STRING(lopvar) + 
                  " NO-LOCK".
   END.
   ELSE DO:
      queryprep = "FOR EACH FREKVENSKATALOG WHERE FREKVENSKATALOG.KLOGSUBID = " + STRING(katidvar) +  " NO-LOCK".
   END.    
   IF queryprep = "" THEN RETURN.
   IF queryprep = ? THEN RETURN.
   QUERY KalkylFrekQuery:QUERY-PREPARE(queryprep).      
   RUN attachKalkylFrekDS.
   DATASET KalkylFrekDS:FILL().
   detachDataSetKalkylFrekDS(hKalkylFrekDataSet).
   
END PROCEDURE.

PROCEDURE attachKalkylLoparDS: /*kopplar ihop temptabell med skarptababell.      */
   hKalkylLopDataSet:GET-BUFFER-HANDLE("kalkylloppostertt"):ATTACH-DATA-SOURCE(DATA-SOURCE LopSrc:HANDLE).
   hKalkylLopDataSet:GET-BUFFER-HANDLE("kalkyllopsubtt"):ATTACH-DATA-SOURCE(DATA-SOURCE LopSubSrc:HANDLE).
END PROCEDURE.


PROCEDURE LaddaKatalogerLopar:
   DEFINE INPUT PARAMETER KatSubIdvar AS INTEGER.
   DEFINE INPUT  PARAMETER ArbKodvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylLoparDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DATASET KalkylLoparDS:EMPTY-DATASET(). 
   IF ArbKodvar = ? THEN 
   queryprep = "FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = " + STRING(KatSubIdvar) + " NO-LOCK". 
   ELSE queryprep = "FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = " + STRING(KatSubIdvar) + " AND KALKYLLOPPOSTER.ARBKOD = '" + ArbKodvar + "' NO-LOCK".
   QUERY KalkylLopQuery:QUERY-PREPARE(queryprep).      
   RUN attachKalkylLoparDS.
   DATASET KalkylLoparDS:FILL().
   
   IF ArbKodvar = ? THEN.
   ELSE DO:
      RUN TotLoppris_UI (INPUT KatSubIdvar).
   END.   
   FIND FIRST kalkylloppostertt WHERE NO-LOCK NO-ERROR.
   detachDataSetKalkylLoparDS(hKalkylLopDataSet).
   
END PROCEDURE.
PROCEDURE TotLoppris_UI :
   DEFINE INPUT PARAMETER KatSubIdvar AS INTEGER.
   FOR EACH KALKYLPRISER  WHERE KALKYLPRISER.KLOGSUBID = KatSubIdvar NO-LOCK:
      FOR EACH kalkyllopsubtt WHERE kalkyllopsubtt.KLOGSUBID = KatSubIdvar AND kalkyllopsubtt.KPID =  KALKYLPRISER.KPID NO-LOCK:
         
         ASSIGN
         kalkyllopsubtt.PRIS = KALKYLPRISER.PRIS
         kalkyllopsubtt.BENAMNING = KALKYLPRISER.BENAMNING.
         kalkyllopsubtt.POSTKOST = kalkyllopsubtt.KOSTNAD + kalkyllopsubtt.PRIS * kalkyllopsubtt.TIMMAR.
         FIND FIRST kalkylloppostertt WHERE kalkylloppostertt.ARBKOD = kalkyllopsubtt.ARBKOD AND kalkylloppostertt.LOPNR = kalkyllopsubtt.LOPNR NO-LOCK NO-ERROR.
         
         IF AVAILABLE kalkylloppostertt THEN kalkylloppostertt.TKOST = kalkylloppostertt.TKOST + kalkyllopsubtt.POSTKOST. 
      END.
   END.
END PROCEDURE.

PROCEDURE attachKalkylDS: /*kopplar ihop temptabell med skarptababell.      */
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkhuvtt"):ATTACH-DATA-SOURCE(DATA-SOURCE KalkhuvSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalknumtt"):ATTACH-DATA-SOURCE(DATA-SOURCE NumSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalknumsubtt"):ATTACH-DATA-SOURCE(DATA-SOURCE NumsubSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkaonrTT"):ATTACH-DATA-SOURCE(DATA-SOURCE AonrSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkfaktorertt"):ATTACH-DATA-SOURCE(DATA-SOURCE FaktSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkegnaprisertt"):ATTACH-DATA-SOURCE(DATA-SOURCE EgnaSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalktmtrlTT"):ATTACH-DATA-SOURCE(DATA-SOURCE MtrlSrc:HANDLE).
   hKalkylDataSet:GET-BUFFER-HANDLE("kalkttidlageTT"):ATTACH-DATA-SOURCE(DATA-SOURCE TidlSrc:HANDLE).
   /*
   DATA-SOURCE NumSrc:HANDLE:PREFER-DATASET = TRUE.
   DATA-SOURCE NumsubSrc:HANDLE:PREFER-DATASET = TRUE.
   */
END PROCEDURE.
PROCEDURE LaddaKalkyl:
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER.
   DEFINE INPUT PARAMETER KalkOmr AS CHARACTER.  
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   
   DATASET KalkylDS:EMPTY-DATASET().
   IF KalkNrvar NE ? THEN DO:
      queryprep = "FOR EACH KALKHUV WHERE KALKHUV.KALKNR = " + STRING(KalkNrvar) + 
      " AND KALKHUV.OMRADE = " + "'" + KalkOmr + "'" + "  NO-LOCK". 
      QUERY KalkylQuery:QUERY-PREPARE(queryprep).
   END.   
   RUN attachKalkylDS.
   IF KalkNrvar NE ? THEN DATASET KalkylDS:FILL().
  
   RUN KalktidLageNamn.
   
   detachDataSetKalkylDS(hKalkylDataSet).
  
END PROCEDURE.

PROCEDURE attachKalkylMallarDS: /*kopplar ihop temptabell med skarptababell.      */
   hKalkylMallDataSet:GET-BUFFER-HANDLE("KalkmallHuvudtt"):ATTACH-DATA-SOURCE(DATA-SOURCE MallSrc:HANDLE).
   hKalkylMallDataSet:GET-BUFFER-HANDLE("KalkmallKodertt"):ATTACH-DATA-SOURCE(DATA-SOURCE MallArbSrc:HANDLE).
END PROCEDURE.

PROCEDURE LaddaMallar:
   DEFINE INPUT PARAMETER Mallnrvar AS INTEGER.
   DEFINE OUTPUT PARAMETER DATASET FOR KalkylMallarDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DATASET KalkylMallarDS:EMPTY-DATASET(). 
   IF Mallnrvar = 0 THEN 
   queryprep = "FOR EACH KALKMALLHUVUD NO-LOCK". 
   ELSE queryprep = "FOR EACH KALKMALLHUVUD WHERE KALKMALLHUVUD.MALLNR = " + STRING(Mallnrvar)  + "' NO-LOCK".
   QUERY KalkylMallQuery:QUERY-PREPARE(queryprep).      
   RUN attachKalkylMallarDS.
   DATASET KalkylMallarDS:FILL().
   detachDataSetKalkylMallarDS(hKalkylMallDataSet).
   
END PROCEDURE.

PROCEDURE KalkStopp_UI :
   /*l�sa UPP kalkyl*/
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kalknrvar AS CHARACTER NO-UNDO.  
   DEFINE INPUT PARAMETER omradevar AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER kalkanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lasavanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lasavnamn AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lastav AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE sokvar  AS CHARACTER NO-UNDO.
   /*�NDAR BORT KALKYL,KOPIERA, KONVERTERA NY xml-import*/
   sokvar = "K" + kalknrvar + "$" + omradevar.
    
   IF vadgora = 1 THEN DO:
      /*�NDRA KALKYL S�TTERL�SNING*/
      FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = sokvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE ANVPER THEN DO TRANSACTION:
         CREATE ANVPER.
         ASSIGN
         ANVPER.ANVANDARE = sokvar
         ANVPER.PERSONALKOD = kalkanv.
      END.
      ELSE DO:
         FIND FIRST ANVANDARE  WHERE ANVANDARE.ANVANDARE = ANVPER.PERSONALKOD NO-LOCK NO-ERROR.
         lastav = TRUE.
         lasavanv = ANVPER.PERSONALKOD.
         
         IF AVAILABLE ANVANDARE THEN lasavnamn = ANVANDARE.AV-NAMN.
         ELSE DO TRANSACTION:
            FIND CURRENT ANVPER EXCLUSIVE-LOCK.
            ANVPER.PERSONALKOD = kalkanv.
            lastav = FALSE.
         END.
      END.
   END.
   IF vadgora = 2 THEN DO:
      /*G�R UR KALKYL MODULEN TAR BORT L�SNING*/
      FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = sokvar AND ANVPER.PERSONALKOD = kalkanv NO-LOCK NO-ERROR.
      IF AVAILABLE ANVPER THEN DO TRANSACTION:
         FIND CURRENT ANVPER EXCLUSIVE-LOCK.
         DELETE ANVPER.
      END.
   END.
   IF vadgora = 3 THEN DO:
      /*TAR BORT KALKYLEN */
      FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = sokvar NO-LOCK NO-ERROR.
     
      IF NOT AVAILABLE ANVPER THEN DO TRANSACTION:
          lastav = FALSE.
      END.
      ELSE DO:
         FIND FIRST ANVANDARE  WHERE ANVANDARE.ANVANDARE = ANVPER.PERSONALKOD NO-LOCK NO-ERROR.
         lastav = TRUE.
         lasavanv = ANVPER.PERSONALKOD.
         IF AVAILABLE ANVANDARE THEN lasavnamn = ANVANDARE.AV-NAMN.
         ELSE DO TRANSACTION:
            FIND CURRENT ANVPER EXCLUSIVE-LOCK.
            ANVPER.PERSONALKOD = kalkanv.
            lastav = FALSE.
         END.
      END.
   END.
   RELEASE ANVPER NO-ERROR.

END PROCEDURE.

/*kontroll om kataloger g�r att ta bort*/
PROCEDURE BortkatOk_UI :
   DEFINE INPUT  PARAMETER katnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER subkatnr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER okbort AS LOGICAL NO-UNDO.
   IF katnr NE ? THEN DO:
      FIND FIRST KALKHUV WHERE KALKHUV.KLOGID = katnr NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN DO:
         okbort = FALSE.
         RETURN.
      END.   
   END.
   ELSE DO:
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGSUBID = subkatnr NO-LOCK:
         FIND FIRST KALKHUV WHERE KALKHUV.KLOGID = KALKYLKATALOGSUB.KLOGID NO-LOCK NO-ERROR.
         IF AVAILABLE KALKHUV THEN DO:
            okbort = FALSE.
            RETURN.
            
         END.
      END.    
   END.      
   okbort = TRUE.
   
END PROCEDURE.

/*kontroll om kataloger g�r att ta bort*/
PROCEDURE BortkatkollOk_UI :
   DEFINE INPUT  PARAMETER klogidnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER subkatnr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER okbort AS LOGICAL NO-UNDO.
   IF subkatnr = 0 THEN DO:
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = klogidnr NO-LOCK:
         FIND FIRST KALKYLKATALOGSUBBUF  WHERE KALKYLKATALOGSUBBUF.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLKATALOGSUBBUF.KLOGID NE klogidnr NO-LOCK NO-ERROR.
         IF AVAILABLE KALKYLKATALOGSUBBUF THEN DO:
            okbort = FALSE.
            RETURN.
         END.   
      END.
      okbort = TRUE.
      RETURN.
   END.
   ELSE DO:   
      FIND FIRST KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGSUBID = subkatnr AND KALKYLKATALOGSUB.KLOGID NE klogidnr NO-LOCK NO-ERROR.
      IF AVAILABLE KALKYLKATALOGSUB THEN DO:
         okbort = FALSE.
         RETURN.
      END.
      ELSE DO:
         okbort = TRUE.
         RETURN.
      END.
   END.      
END PROCEDURE.

/*ta bort subkat*/
PROCEDURE BortKatdelar_UI :
   DEFINE INPUT  PARAMETER subkatnr AS INTEGER NO-UNDO. 
   FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = subkatnr EXCLUSIVE-LOCK:
      DELETE KALKYLLOPPOSTER.
   END.
   
   FOR EACH KALKYLLOPSUB WHERE KALKYLLOPSUB.KLOGSUBID = subkatnr EXCLUSIVE-LOCK:
      DELETE KALKYLLOPSUB.
   END.
   FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = subkatnr EXCLUSIVE-LOCK:
      DELETE KALKYLARBKODER.
   END.    
    
END PROCEDURE.
/*ta bort frek*/
PROCEDURE BortKatFrek_UI :
   DEFINE INPUT  PARAMETER subkatnr AS INTEGER NO-UNDO. 
   FOR EACH FREKVENSKATALOG WHERE FREKVENSKATALOG.KLOGSUBID = subkatnr EXCLUSIVE-LOCK:
      DELETE FREKVENSKATALOG.
   END.
           
END PROCEDURE.
/*sista num*/
PROCEDURE sistanum_UI :
   DEFINE OUTPUT PARAMETER hjraknare AS INTEGER NO-UNDO.

   FIND LAST KALKNUMBUF WHERE KALKNUMBUF.KALKNR = KALKHUVBUF.KALKNR AND KALKNUMBUF.OMRADE = KALKHUVBUF.OMRADE USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE KALKNUMBUF THEN DO:
      hjraknare = KALKNUMBUF.NUM + 1.   
   END.  
   ELSE hjraknare = hjraknare + 1.
END PROCEDURE.
/*sista num*/
PROCEDURE sistanumhuv_UI :
   DEFINE OUTPUT PARAMETER hjraknare AS INTEGER NO-UNDO.

   FIND LAST KALKNUMBUF WHERE KALKNUMBUF.KALKNR = KALKHUV.KALKNR AND KALKNUMBUF.OMRADE = KALKHUV.OMRADE USE-INDEX NUM NO-LOCK NO-ERROR.
   IF AVAILABLE KALKNUMBUF THEN DO:
      hjraknare = KALKNUMBUF.NUM + 1.   
   END.  
   ELSE hjraknare = hjraknare + 1.
END PROCEDURE.
/*SISTA SUBKATALOGEN*/                 
PROCEDURE sistakatsubid_UI :
   DEFINE OUTPUT PARAMETER hjraknare AS INTEGER NO-UNDO.
   FIND LAST KALKYLKATALOGSUBBUF USE-INDEX KLOGSUBID NO-LOCK NO-ERROR.
   IF AVAILABLE KALKYLKATALOGSUBBUF THEN DO:
      hjraknare = KALKYLKATALOGSUBBUF.KLOGSUBID + 1.
   END.
   ELSE hjraknare = hjraknare + 1.
END PROCEDURE.

/*SISTA SUBKATALOGEN*/                 
PROCEDURE sistakatid_UI :
   DEFINE OUTPUT PARAMETER hjraknare AS INTEGER NO-UNDO.
   FIND LAST KALKYLKATALOGBUF USE-INDEX KLOGID NO-LOCK NO-ERROR.
   IF AVAILABLE KALKYLKATALOGBUF THEN DO:
      hjraknare = KALKYLKATALOGBUF.KLOGID + 1.
   END.
   ELSE hjraknare = hjraknare + 1.
END PROCEDURE.
/*SISTA NUMMRET*/
PROCEDURE sistaprisid_UI :
   DEFINE INPUT  PARAMETER KatSubIdvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER hjraknare AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER maskinvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE maskinnr AS INTEGER NO-UNDO.
   FIND LAST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KatSubIdvar USE-INDEX KPID NO-LOCK NO-ERROR.
   IF AVAILABLE KALKYLPRISER THEN DO: 
      hjraknare = KALKYLPRISER.KPID + 1.
   END.
   ELSE hjraknare = hjraknare + 1.
   FIND LAST KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KatSubIdvar AND 
   /*KALKYLPRISER.KVID = 3 AND*/  KALKYLPRISER.SOKBENAMNING BEGINS "PRIS" USE-INDEX KPID NO-LOCK NO-ERROR.
   IF AVAILABLE KALKYLPRISER THEN DO:
      maskinnr = INTEGER(SUBSTRING(KALKYLPRISER.SOKBENAMNING,5)) + 1.
   END.
   ELSE maskinnr = 1.   
   maskinvar = "PRIS" + STRING(maskinnr).
   
END PROCEDURE.  
/*SKAPARTIDL�GEN*/
PROCEDURE kalktidl_UI :
   DEFINE INPUT  PARAMETER kynr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR ekalkttidlageTT.
   EMPTY TEMP-TABLE ekalkttidlageTT NO-ERROR. 
   FOR EACH TIDSLAGEN WHERE TIDSLAGEN.AUTOMATISKT = TRUE NO-LOCK:
      CREATE ekalkttidlageTT.
      ASSIGN
      ekalkttidlageTT.KALKNR = kynr
      ekalkttidlageTT.OMRADE = omrvar
      ekalkttidlageTT.IDTIDLAG = TIDSLAGEN.IDTIDLAG
      ekalkttidlageTT.ORDNING = TIDSLAGEN.AKTIVITET2
      ekalkttidlageTT.TIDLAGE = TIDSLAGEN.TIDLAGE
      ekalkttidlageTT.TTRECID = RECID(ekalkttidlageTT).
   END.
END PROCEDURE.
PROCEDURE KalktidLageNamn :
   FOR EACH kalkttidlageTT WHERE NO-LOCK:
      FIND FIRST TIDSLAGEN WHERE TIDSLAGEN.IDTIDLAG = kalkttidlageTT.IDTIDLAG NO-LOCK NO-ERROR.
      IF AVAILABLE TIDSLAGEN THEN DO:
         kalkttidlageTT.TIDLAGE = TIDSLAGEN.TIDLAGE.
      END.
      ELSE kalkttidlageTT.TIDLAGE = "Borttaget ifr�n registret!". 
      IF kalkttidlageTT.ANVANDARE1 NE "" THEN DO:
         FIND FIRST ANVANDARE  WHERE ANVANDARE.ANVANDARE = kalkttidlageTT.ANVANDARE1 NO-LOCK NO-ERROR.
         IF AVAILABLE ANVANDARE THEN DO:
            kalkttidlageTT.NAMNANVANDARE1 = ANVANDARE.AV-NAMN.
         END.
      END.      
   END.
END PROCEDURE.


/*tar fram r�tt nummer*/
PROCEDURE omradekoll_UI:
   DEFINE INPUT PARAMETER omradevar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER tempvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
   DO TRANSACTION:
      IF Guru.Konstanter:varforetypchar[3] NE "" THEN DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = Guru.Konstanter:varforetypchar[3] EXCLUSIVE-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = omradevar EXCLUSIVE-LOCK NO-ERROR.
      END.
      IF NOT AVAILABLE OMRADETAB THEN FIND FIRST OMRADETAB USE-INDEX OMR EXCLUSIVE-LOCK NO-ERROR.
      IF OMRADETAB.KALKYLINT2 < OMRADETAB.KALKYLSIST OR OMRADETAB.KALKYLINT1 = OMRADETAB.KALKYLINT2 THEN DO:
         tempvar = ?.     
         RETURN.
      END.
      ELSE tempvar = OMRADETAB.KALKYLSIST.
     
      RUN kalksista_UI (INPUT-OUTPUT tempvar).
   END.  
END PROCEDURE.
PROCEDURE kalksista_UI :
   DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.  
   DEFINE INPUT-OUTPUT PARAMETER tempvar AS INTEGER NO-UNDO.
   DEFINE BUFFER KALKHUVSISTA FOR KALKHUV.
   DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
   DEFINE VARIABLE leavevar AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "FASTSPEC", OUTPUT bloblog).
   IF bloblog = TRUE THEN RUN KALKBERAPPDSEXTRA.p PERSISTENT SET AppServerExtraHandle (INPUT Guru.Konstanter:globanv).
   
   IF tempvar < OMRADETAB.KALKYLINT1 THEN tempvar = OMRADETAB.KALKYLINT1.
   REPEAT:
      FIND FIRST KALKHUVSISTA WHERE KALKHUVSISTA.KALKNR = tempvar NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKHUVSISTA THEN DO:
         IF bloblog = TRUE THEN DO:
            /*koll om fastkalknr finns*/
            RUN sistafastspec_UI IN AppServerExtraHandle (INPUT tempvar, OUTPUT leavevar).
            IF leavevar = TRUE THEN LEAVE.
         END.
         ELSE LEAVE. 
      END.   
      tempvar = tempvar + 1.
      IF tempvar > OMRADETAB.KALKYLINT2 THEN DO:
         felkoll = TRUE.
         LEAVE.
      END.    
   END.              
   OMRADETAB.KALKYLSIST = tempvar.
   IF felkoll = TRUE THEN tempvar = ?.  
   FIND CURRENT OMRADETAB NO-LOCK. 
   IF VALID-HANDLE(AppServerExtraHandle) THEN DELETE PROCEDURE AppServerExtraHandle NO-ERROR.
   AppServerExtraHandle = ?.
END PROCEDURE.
/*startproc h�mtar katalogernamn och nykalkyl*/
PROCEDURE startny_UI :
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER sekanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR ekalkhuvtt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylkatalogtt.
     
   /*SAVEDS*/
   EMPTY TEMP-TABLE ekalkhuvtt NO-ERROR.
   EMPTY TEMP-TABLE kalkylkatalogtt NO-ERROR. 
   /*
   FIND FIRST KALKYLKATALOG WHERE  KALKYLKATALOG.BENAMNING BEGINS "EBR" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      RETURN.
   END.
   */
   FOR EACH KALKYLKATALOG WHERE KALKYLKATALOG.AVSLUTAD  = FALSE NO-LOCK:
      CREATE kalkylkatalogtt.
      BUFFER-COPY KALKYLKATALOG TO kalkylkatalogtt.
      kalkylkatalogtt.TTRECID = RECID(kalkylkatalogtt).
   END.
   FOR EACH kalkylkatalogtt WHERE kalkylkatalogtt.SEKRETESS = TRUE NO-LOCK:
      FIND FIRST KALKYLKATALOGANV WHERE KALKYLKATALOGANV.KLOGID = kalkylkatalogtt.KLOGID AND KALKYLKATALOGANV.ANVANDARE = sekanv NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKYLKATALOGANV THEN DO:
         DELETE kalkylkatalogtt.
      END.
   END.
   FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kalkylkatalogtt THEN DO:
      FIND FIRST kalkylkatalogtt NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE kalkylkatalogtt THEN DO:
      RETURN.
   END.
   CREATE ekalkhuvtt.
   ASSIGN 
   ekalkhuvtt.OMRADE = omrvar
   ekalkhuvtt.KLOGID = kalkylkatalogtt.KLOGID
   ekalkhuvtt.TYPKALK = 2 
   ekalkhuvtt.EGETMTRL = FALSE  
   ekalkhuvtt.EGNAPRISER = FALSE
   ekalkhuvtt.FAKTORER = FALSE.
   ekalkhuvtt.TTRECID = RECID(ekalkhuvtt).

END PROCEDURE.

PROCEDURE sparakalkhuv_UI :
   DEFINE OUTPUT PARAMETER felvar AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER felmedd AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR ekalkhuvtt.
   DEFINE VARIABLE nyvar AS LOGICAL NO-UNDO.
   FIND FIRST ekalkhuvtt WHERE NO-LOCK NO-ERROR.
   IF ekalkhuvtt.KALKNR = 0 THEN DO:
      RUN omradekoll_UI (INPUT ekalkhuvtt.OMRADE, OUTPUT ekalkhuvtt.KALKNR).
      nyvar = TRUE.
   END.   
   ELSE nyvar = FALSE.
   IF ekalkhuvtt.KALKNR = ? THEN DO:
      ekalkhuvtt.KALKNR = 0.
      ASSIGN 
      felvar = TRUE
      felmedd =  "6667".      
      RETURN.
   END.    
   IF ekalkhuvtt.BENAMNING = "" THEN DO:
      ASSIGN 
      felvar = TRUE
      felmedd =  "68".      
      RETURN.
   END.    
   IF ekalkhuvtt.KALKANV = "" THEN DO:
      ASSIGN 
      felvar = TRUE
      felmedd =  "69".
      RETURN.
   END. 
   IF ekalkhuvtt.ANVANDARE = "" THEN DO:
      ASSIGN 
      felvar = TRUE
      felmedd =  "209".
      RETURN.
   END. 
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ekalkhuvtt.KALKANV USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PERSONALTAB THEN DO:
      ASSIGN 
      felvar = TRUE
      felmedd = "7071".
      RETURN.
   END.  
   IF ekalkhuvtt.OMRADE = ? THEN DO:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = ekalkhuvtt.KALKNR NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN
      ekalkhuvtt.OMRADE = KALKHUV.OMRADE.
   END.   
   IF ekalkhuvtt.BESTID = ? THEN DO:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = ekalkhuvtt.KALKNR NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN
      ekalkhuvtt.BESTID = KALKHUV.BESTID.
   END.  
END PROCEDURE.
PROCEDURE BortTagenPersonal_UI :
   DEFINE INPUT-OUTPUT PARAMETER pkodNamnvar AS CHARACTER NO-UNDO.
  
   FIND FIRST BORTPERS WHERE BORTPERS.PERSONALKOD = pkodNamnvar USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF AVAILABLE BORTPERS THEN DO:
      pkodNamnvar = BORTPERS.FORNAMN + " " + BORTPERS.EFTERNAMN.
   END.   
   
END PROCEDURE.
/*skapar kalkylnum*/ 
PROCEDURE skapanumsub_UI :
   DEFINE INPUT  PARAMETER subidvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER arbkodvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lopkodvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR ekalknumsubtt.
   EMPTY TEMP-TABLE ekalknumsubtt NO-ERROR. 
   DEFINE VARIABLE numvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
   FOR EACH KALKYLLOPSUB WHERE KALKYLLOPSUB.KLOGSUBID = subidvar AND KALKYLLOPSUB.ARBKOD = arbkodvar AND 
   KALKYLLOPSUB.LOPNR = lopkodvar NO-LOCK: 
      CREATE ekalknumsubtt.  
      BUFFER-COPY KALKYLLOPSUB TO ekalknumsubtt.      
      FIND FIRST KALKYLPRISER  WHERE KALKYLPRISER.KPID = ekalknumsubtt.KPID AND
      KALKYLPRISER.KLOGSUBID = KALKYLLOPSUB.KLOGSUBID NO-LOCK NO-ERROR.
      IF AVAILABLE KALKYLPRISER THEN DO:
         ASSIGN 
         ekalknumsubtt.PRIS = KALKYLPRISER.PRIS
         ekalknumsubtt.EGENPRISUPP = KALKYLPRISER.EGENPRISUPP 
         ekalknumsubtt.EGENKODUPP = KALKYLPRISER.EGENKODUPP  
         ekalknumsubtt.BENAMNING = KALKYLPRISER.BENAMNING.
      END.  
      ASSIGN         
      ekalknumsubtt.FRIBENAMNING = ekalknumsubtt.BENAMNING         
      ekalknumsubtt.FRIKOSTNAD = ekalknumsubtt.KOSTNAD
      ekalknumsubtt.FRIPRIS = ekalknumsubtt.PRIS
      ekalknumsubtt.FRITIMMAR = ekalknumsubtt.TIMMAR.
      
   END.           
END PROCEDURE.

/*h�mtar kalkymallar k�rs inte*/
PROCEDURE Mallarhmt:
   DEFINE OUTPUT PARAMETER TABLE FOR KalkmallHuvudtt.
   DEFINE OUTPUT PARAMETER TABLE FOR KalkmallKodertt.
   EMPTY TEMP-TABLE KalkmallHuvudtt NO-ERROR. 
   EMPTY TEMP-TABLE KalkmallKodertt NO-ERROR. 
   RUN procset_UI.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "KALKMALL"  
   inextradatatemp.HUVUDINT = ?                 
   inextradatatemp.HUVUDCH = "1".              
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).
   FOR EACH extradatatemp WHERE extradatatemp.SOKLOG[1] = TRUE NO-LOCK:
      CREATE KalkmallHuvudtt.
      ASSIGN
      KalkmallHuvudtt.BENAMNING = extradatatemp.SOKCHAR[2]
      KalkmallHuvudtt.MALLNR = extradatatemp.HUVUDINT
      KalkmallHuvudtt.TYP = extradatatemp.SOKINT[3].
      KalkmallHuvudtt.TTRECID = RECID(KalkmallHuvudtt). 
   END. 
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.  
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   FOR EACH KalkmallHuvudtt WHERE NO-LOCK:
       CREATE inextradatatemp.          
       ASSIGN
       inextradatatemp.PROGRAM = "KALKMALL"                   
       inextradatatemp.HUVUDINT = KalkmallHuvudtt.MALLNR
       inextradatatemp.HUVUDCH = "2".  
       RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp).                
      FOR EACH extradatatemp NO-LOCK:
         CREATE KalkmallKodertt.
         ASSIGN
         KalkmallKodertt.ANTAL = extradatatemp.SOKDEC[1]
         KalkmallKodertt.ARBKOD = extradatatemp.SOKCHAR[1] 
         KalkmallKodertt.BENAMNING = extradatatemp.SOKCHAR[3]
         KalkmallKodertt.ENHET = extradatatemp.SOKCHAR[4]
         KalkmallKodertt.LOPNR = extradatatemp.SOKINT[1]
         KalkmallKodertt.MALLNR = extradatatemp.HUVUDINT
         KalkmallKodertt.TYP = extradatatemp.SOKINT[3]. 
         
      END.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR.  
      EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   END.      
   RUN procreset_UI.
   
END PROCEDURE.
/*startproc h�mtar kataloger och kalkyl*/
PROCEDURE kathmt_UI :
   DEFINE INPUT  PARAMETER kynr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER sekanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER felmed AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylarbkodertt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylloppostertt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylkatalogtt.
   DEFINE OUTPUT PARAMETER TABLE FOR markfiltertt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkyldelkatalogtt.
   
   EMPTY TEMP-TABLE kalkylkatalogtt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylarbkodertt NO-ERROR.
   EMPTY TEMP-TABLE kalkylloppostertt NO-ERROR.
   EMPTY TEMP-TABLE markfiltertt NO-ERROR. 
   FIND LAST KALKYLKATALOG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      felmed = "Fel uppl�gg!".
      RETURN.
      
   END.
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = kynr AND KALKHUV.OMRADE = omrvar  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKHUV THEN DO:
      felmed = "Kalkyl finns inte!".
      RETURN.
   END.
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN FIND LAST KALKYLKATALOG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      felmed = "Fel uppl�gg!".
      RETURN.
   END. 
   FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKHUV.KLOGID NO-LOCK:
      FIND FIRST kalkyldelkatalogtt WHERE kalkyldelkatalogtt.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkyldelkatalogtt THEN DO:
         CREATE kalkyldelkatalogtt.
         BUFFER-COPY KALKYLKATALOGSUB TO kalkyldelkatalogtt.
         kalkyldelkatalogtt.TTRECID = RECID(kalkyldelkatalogtt).
      END.  
   END.
   
  
   IF KALKYLKATALOG.SEKRETESS = TRUE THEN DO:
      FIND FIRST KALKYLKATALOGANV WHERE KALKYLKATALOGANV.KLOGID = KALKYLKATALOG.KLOGID AND KALKYLKATALOGANV.ANVANDARE = sekanv NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKYLKATALOGANV THEN DO:
         felmed = "Du �r inte beh�rig att se denna kalkyl!".
         RETURN.
      END.
   END.   
   CREATE kalkylkatalogtt.
   BUFFER-COPY KALKYLKATALOG TO kalkylkatalogtt.
   kalkylkatalogtt.TTRECID = RECID(kalkylkatalogtt).
   FOR EACH KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK, 
   EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID AND KALKYLKATALOGSUB.AVSLUTAD  = FALSE NO-LOCK,
   EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID NO-LOCK:
      IF KALKYLARBKODER.MARKNING NE "" THEN DO:
         FIND FIRST markfiltertt WHERE markfiltertt.TYP = KALKYLARBKODER.TYPKALK AND markfiltertt.MARKNING = KALKYLARBKODER.MARKNING NO-LOCK NO-ERROR.
         IF NOT AVAILABLE markfiltertt THEN DO:
            CREATE markfiltertt.
            markfiltertt.TYP = KALKYLARBKODER.TYPKALK.
            markfiltertt.MARKNING = CAPS(SUBSTRING(KALKYLARBKODER.MARKNING,1,1)) + LC(SUBSTRING(KALKYLARBKODER.MARKNING,2)).
         END.
      END.
      CREATE kalkylarbkodertt.
      BUFFER-COPY KALKYLARBKODER TO kalkylarbkodertt.
      kalkylarbkodertt.TTRECID = RECID(kalkylarbkodertt).     
   END.    
   FOR EACH kalkylarbkodertt,
   EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = kalkylarbkodertt.KLOGSUBID AND KALKYLLOPPOSTER.ARBKOD = kalkylarbkodertt.ARBKOD NO-LOCK:
      CREATE kalkylloppostertt.
      BUFFER-COPY KALKYLLOPPOSTER TO kalkylloppostertt.
      kalkylloppostertt.TTRECID = RECID(kalkylloppostertt).
      
   END.   
   FOR EACH KALKYLKATALOG WHERE KALKYLKATALOG.AVSLUTAD  = FALSE NO-LOCK:
      FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kalkylkatalogtt THEN DO:
         CREATE kalkylkatalogtt.
         BUFFER-COPY KALKYLKATALOG TO kalkylkatalogtt.
         kalkylkatalogtt.TTRECID = RECID(kalkylkatalogtt).
      END.   
   END.
   FOR EACH kalkylkatalogtt WHERE kalkylkatalogtt.SEKRETESS = TRUE NO-LOCK:
      FIND FIRST KALKYLKATALOGANV WHERE KALKYLKATALOGANV.KLOGID = kalkylkatalogtt.KLOGID AND KALKYLKATALOGANV.ANVANDARE = sekanv NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKYLKATALOGANV THEN DO:
         DELETE kalkylkatalogtt.
      END.
   END.
 
   FOR EACH kalkyldelkatalogtt WHERE  NO-LOCK:
      FOR EACH KALKYLLOPSUB WHERE KALKYLLOPSUB.KLOGSUBID = kalkyldelkatalogtt.KLOGSUBID  NO-LOCK: 
         CREATE kalkyllopsubtt.  
         BUFFER-COPY KALKYLLOPSUB TO kalkyllopsubtt.
         kalkyllopsubtt.TTRECID = RECID(kalkyllopsubtt).
      END.   
      RUN TotLoppris_UI (INPUT kalkyldelkatalogtt.KLOGSUBID).
   END.
   
  
END PROCEDURE.
/* h�mta kalkylpriser*/
PROCEDURE kphmt : 
   /* vilken katalog den ska h�mta poster p� */
   DEFINE INPUT PARAMETER knr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omr AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylprisertt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkvisningtt.
   EMPTY TEMP-TABLE kalkvisningtt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylprisertt NO-ERROR.
   FIND FIRST KALKHUV WHERE  KALKHUV.KALKNR = knr AND KALKHUV.OMRADE = omr NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKHUV THEN RETURN.
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
     /*EXTRA KLOGSUBID*/
   FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID AND KALKYLKATALOGSUB.AVSLUTAD = FALSE NO-LOCK:
      FOR EACH KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID NO-LOCK:
         CREATE kalkylprisertt.
         BUFFER-COPY KALKYLPRISER TO kalkylprisertt.
         kalkylprisertt.TTRECID = RECID(kalkylprisertt).
         
         FOR EACH KALKVISNING WHERE KALKVISNING.KVID = KALKYLPRISER.KVID NO-LOCK:
            FIND FIRST kalkvisningtt WHERE kalkvisningtt.KVID = KALKYLPRISER.KVID NO-LOCK NO-ERROR.
            IF NOT AVAILABLE kalkvisningtt THEN DO:
               CREATE kalkvisningtt.
               BUFFER-COPY KALKVISNING TO kalkvisningtt.
               kalkvisningtt.TTRECID = RECID(kalkvisningtt).
            END.   
         END.
      END.         
   END. 
  
   /*faktorer MED klogsubid*/
   FIND FIRST KALKFAKTORER WHERE KALKFAKTORER.KALKNR = knr AND KALKFAKTORER.OMRADE = omr NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKFAKTORER THEN DO TRANSACTION:
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID AND KALKYLKATALOGSUB.AVSLUTAD = FALSE NO-LOCK,
      EACH kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND kalkylprisertt.EGENKODUPP = TRUE NO-LOCK:
         CREATE KALKFAKTORER.
         BUFFER-COPY kalkylprisertt TO KALKFAKTORER.
         ASSIGN
         KALKFAKTORER.KALKNR = knr
         KALKFAKTORER.OMRADE = omr
         KALKFAKTORER.FAKTOR = 1.0.
      END.
   END.   
   
   FIND FIRST KALKEGNAPRISER WHERE KALKEGNAPRISER.KALKNR = knr AND KALKEGNAPRISER.OMRADE = omr NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKEGNAPRISER THEN DO TRANSACTION:
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID AND KALKYLKATALOGSUB.AVSLUTAD = FALSE NO-LOCK,
      EACH kalkylprisertt WHERE kalkylprisertt.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND kalkylprisertt.EGENPRISUPP = TRUE NO-LOCK:
         CREATE KALKEGNAPRISER.
         BUFFER-COPY kalkylprisertt TO KALKEGNAPRISER.
         ASSIGN
         KALKEGNAPRISER.KALKNR = knr
         KALKEGNAPRISER.OMRADE = omr.
      END.         
   END.
   
END PROCEDURE.

PROCEDURE avtalhmt :
   DEFINE INPUT  PARAMETER katarvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR Avtalskalktt.
   DEFINE VARIABLE blobproch AS HANDLE NO-UNDO.
   DEFINE VARIABLE arraknare AS INTEGER NO-UNDO.
   DEFINE VARIABLE idvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE Avtalskalktt NO-ERROR. 
   {FINNSDYNBLOB.I}
   
   RUN blobladda_UI IN blobproch (INPUT 'blobinfo.FILNAMN BEGINS "20"').
   arraknare = katarvar - 1.
   FOR EACH blobinfotemp:
      CREATE Avtalskalktt.
            ASSIGN
            Avtalskalktt.KATAR = INTEGER(SUBSTRING(blobinfotemp.FILNAMN,1,4))
            Avtalskalktt.AVTALTXTHELA = blobinfotemp.FILNAMN 
            Avtalskalktt.AVTALAR = INTEGER(SUBSTRING(blobinfotemp.FILNAMN,5,4))
            Avtalskalktt.AVTALTXT = SUBSTRING(blobinfotemp.FILNAMN,9).
            Avtalskalktt.ID = idvar + 1.
            idvar = idvar + 1.
      /*
      REPEAT:      
         IF SUBSTRING(blobinfotemp.FILNAMN,1,4) = STRING(arraknare) THEN DO:
            CREATE Avtalskalktt.
            ASSIGN
            Avtalskalktt.KATAR = INTEGER(SUBSTRING(blobinfotemp.FILNAMN,1,4))
            Avtalskalktt.AVTALTXTHELA = blobinfotemp.FILNAMN 
            Avtalskalktt.AVTALAR = INTEGER(SUBSTRING(blobinfotemp.FILNAMN,5,4))
            Avtalskalktt.AVTALTXT = SUBSTRING(blobinfotemp.FILNAMN,9).
            Avtalskalktt.ID = idvar + 1.
            arraknare = katarvar.
            idvar = idvar + 1.
            LEAVE.
         END.
         arraknare = arraknare + 1.
         IF arraknare > YEAR(TODAY) THEN DO:
            arraknare = katarvar.
            LEAVE.
         END.     
      END.
      */            
   END.  
   FIND FIRST Avtalskalktt WHERE NO-LOCK NO-ERROR.
  
   IF VALID-HANDLE(blobproch) THEN DELETE PROCEDURE blobproch NO-ERROR.
   
END PROCEDURE.
PROCEDURE AonrPlanrInfo_UI :
   DEFINE INPUT  PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER valaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER beredarvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER utfardatvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER refvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ortnamn AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER kontakt AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER arbannsv AS CHARACTER NO-UNDO.   
   IF vad = 1 THEN DO:  
      RUN AvtalKalkAo_UI (INPUT valaonr,INPUT valdelnr,OUTPUT beredarvar,OUTPUT utfardatvar,OUTPUT refvar,OUTPUT ortnamn,OUTPUT kontakt,OUTPUT arbannsv).    
   END.  
   ELSE DO:
      FIND FIRST PLANNRTAB  WHERE PLANNRTAB.PLANNR = valaonr AND PLANNRTAB.ARTAL = valdelnr NO-LOCK NO-ERROR.
      IF AVAILABLE PLANNRTAB THEN DO:
         ortnamn = PLANNRTAB.ORT.
      END.   
   END.       
END PROCEDURE.

PROCEDURE ExtraPerUppgiter_UI :
   /*
   RUN ExtraPerUppgiter_UI IN AppServerHandle (OUTPUT arbetsleadare,OUTPUT utfardatvartel,OUTPUT beredartel,OUTPUT arbannsvtel,OUTPUT arbetsleadaretel,OUTPUT utfardatvarepost,OUTPUT beredarepost,OUTPUT arbannsvepost,OUTPUT arbetsleadareepost).
   */
   DEFINE OUTPUT PARAMETER arbetsleadareout AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER utfardatvartelout AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER beredartelout AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER arbannsvtelout AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER arbetsleadaretelout AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER utfardatvarepostout AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER beredarepostout AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER arbannsvepostout AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER arbetsleadareepostout AS CHARACTER NO-UNDO.
   
   ASSIGN
   arbetsleadareout        = arbetsleadare          
   utfardatvartelout       = utfardatvartel    
   beredartelout           = beredartel        
   arbannsvtelout          = arbannsvtel       
   arbetsleadaretelout     = arbetsleadaretel  
   utfardatvarepostout     = utfardatvarepost  
   beredarepostout         = beredarepost      
   arbannsvepostout        = arbannsvepost     
   arbetsleadareepostout   = arbetsleadareepost.
END PROCEDURE.

PROCEDURE avtalkalkao_UI :
   DEFINE INPUT PARAMETER valaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER beredarvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER utfardatvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER refvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER ortnamn AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER kontakt AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER arbannsv AS CHARACTER NO-UNDO.   
         
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = valaonr AND AONRTAB.DELNR = valdelnr NO-LOCK NO-ERROR.
   IF NOT AVAILABLE AONRTAB THEN RETURN.
   
   ASSIGN
   ortnamn = AONRTAB.ORT
   arbannsv = AONRTAB.ARBANSVARIG 
   utfardatvar = AONRTAB.UTFARDAT  
   beredarvar = AONRTAB.BEREDARE.
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = utfardatvar NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:
      utfardatvar = ANVANDARE.AV-NAMN.
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + ANVANDARE.ANVANDARE + "," + ANVANDARE.PERSONALKOD.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD NO-LOCK NO-ERROR.
      IF AVAILABLE PERSONALTAB THEN DO:
         
         utfardatvartel = PERSONALTAB.TELEFON. 
         IF utfardatvartel = "" THEN utfardatvartel = PERSONALTAB.MOBILTEL. 
         utfardatvarepost = PERSONALTAB.PERSONSOK.
         Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
      END.
   END. 
   
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = beredarvar NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      beredarvar = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
      beredartel = PERSONALTAB.TELEFON. 
      IF beredartel = "" THEN beredartel = PERSONALTAB.MOBILTEL. 
      beredarepost = PERSONALTAB.PERSONSOK.
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = arbannsv NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      arbannsv = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
      arbannsvtel = PERSONALTAB.TELEFON.
      IF arbannsvtel = "" THEN arbannsvtel = PERSONALTAB.MOBILTEL. 
      arbannsvepost = PERSONALTAB.PERSONSOK.
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.
   
   /*arbetsleadare*/
   
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = AONRTAB.STARTDAG NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      arbetsleadare = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
      arbetsleadaretel = PERSONALTAB.TELEFON.
      IF arbetsleadaretel = "" THEN arbetsleadaretel = PERSONALTAB.MOBILTEL. 
      arbetsleadareepost = PERSONALTAB.PERSONSOK.
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.
   
   RUN procset_UI.
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "AOREF"                   
   inextradatatemp.HUVUDCH = AONRTAB.AONR              
   inextradatatemp.HUVUDINT =  AONRTAB.DELNR.         
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO:
      ASSIGN
      refvar = extradatatemp.SOKCHAR[1]
      kontakt = extradatatemp.SOKCHAR[5].
   END.   
   RUN procreset_UI. 
   
   {GDPRLOGGCLIENT.I}
END PROCEDURE.

PROCEDURE RefUE_UI :   
   DEFINE INPUT PARAMETER valaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER valdelnr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER btimUE AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER mtimUE AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER kabtimUE AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER ledtimUE AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER mtrlUE AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER ovrUE AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER uenr AS INTEGER NO-UNDO.
   DEFINE VARIABLE uekal AS LOGICAL NO-UNDO.
   uekal = FALSE.         
   /*FIND FIRST AONRTAB WHERE AONRTAB.AONR = valaonr AND AONRTAB.DELNR = valdelnr NO-LOCK NO-ERROR.
   IF NOT AVAILABLE AONRTAB THEN RETURN.*/
   FOR EACH KALKAONR WHERE KALKAONR.AONR = valaonr AND KALKAONR.DELNR = valdelnr NO-LOCK:
      FIND FIRST KALKHUV  WHERE KALKHUV.KALKNR = KALKAONR.KALKNR NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN DO:
         IF KALKHUV.BENAMNING MATCHES "*UE kalkyl*" THEN DO:
            uekal = TRUE.
            uenr = KALKHUV.KALKNR.
         END.   
      END.
   END.         
   IF uekal = TRUE THEN DO:
      
      FIND FIRST KALKFAKTORER WHERE KALKFAKTORER.KALKNR = uenr AND  KALKFAKTORER.BENAMNING = "BEREDARE" NO-LOCK NO-ERROR.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = uenr AND  KALKNUMSUB.BENAMNING = "BEREDARE"  NO-LOCK:
         btimUE = btimUE + (KALKNUMSUB.FRITIMMAR * KALKFAKTORER.FAKTOR ).
      END.
      FIND FIRST KALKFAKTORER WHERE KALKFAKTORER.KALKNR = uenr AND  KALKFAKTORER.BENAMNING = "Mont�r" NO-LOCK NO-ERROR.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = uenr AND  KALKNUMSUB.BENAMNING = "Mont�r"  NO-LOCK:
         mtimUE = mtimUE + (KALKNUMSUB.FRITIMMAR * KALKFAKTORER.FAKTOR ).
      END.
      FIND FIRST KALKFAKTORER WHERE KALKFAKTORER.KALKNR = uenr AND  KALKFAKTORER.BENAMNING = "LEDNB.MASK" NO-LOCK NO-ERROR.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = uenr AND  KALKNUMSUB.BENAMNING = "LEDNB.MASK"  NO-LOCK:
         ledtimUE = ledtimUE + (KALKNUMSUB.FRITIMMAR * KALKFAKTORER.FAKTOR ).
      END.
      FIND FIRST KALKFAKTORER WHERE KALKFAKTORER.KALKNR = uenr AND  KALKFAKTORER.BENAMNING = "KAB SCH MASK" NO-LOCK NO-ERROR.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = uenr AND  KALKNUMSUB.BENAMNING = "KAB SCH MASK"  NO-LOCK:
         kabtimUE = kabtimUE + (KALKNUMSUB.FRITIMMAR * KALKFAKTORER.FAKTOR ).
      END.
      
      FIND FIRST KALKFAKTORER WHERE KALKFAKTORER.KALKNR = uenr AND  KALKFAKTORER.BENAMNING = "MATERIEL" NO-LOCK NO-ERROR.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = uenr AND  KALKNUMSUB.BENAMNING = "MATERIEL"  NO-LOCK:
         mtrlUE = mtrlUE + (KALKNUMSUB.FRIKOSTNAD * KALKFAKTORER.FAKTOR ).
      END.
      FIND FIRST KALKFAKTORER WHERE KALKFAKTORER.KALKNR = uenr AND  KALKFAKTORER.BENAMNING = "�VRIGKOSTNAD" NO-LOCK NO-ERROR.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = uenr AND  KALKNUMSUB.BENAMNING = "�VRIGKOSTNAD"  NO-LOCK:
         ovrUE = ovrUE + (KALKNUMSUB.FRIKOSTNAD * KALKFAKTORER.FAKTOR ).
      END.
      
   END.
   
      
END PROCEDURE.

PROCEDURE esmallhmt_UI:
   DEFINE INPUT  PARAMETER avtalvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik1 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik2 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER frist AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER frisl AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER oversatt137 AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER ExelKommando AS CHARACTER  NO-UNDO.
   oversatt137 = FALSE .
   IF avtalvar BEGINS "2012" or avtalvar BEGINS "2013" or avtalvar BEGINS "2014" THEN DO:
      ASSIGN 
      sterad = 12  
      slerad = 1064
      flik1 = 15
      flik2 = 16.
   END.
   ELSE IF  avtalvar BEGINS "20152015" THEN DO:
      ASSIGN 
      sterad = 12  
      slerad = 1081
      flik1 = 15
      flik2 = 16.
   END.
   ELSE IF  avtalvar BEGINS "2016" OR avtalvar BEGINS "20172017" THEN DO:
      /*samma flik from 20160101*/ 
      ASSIGN
      sterad = 20  
      slerad = 1055      
      /*FROM 20161018 bytt fr�n flik 13 till flik 1*/
      flik1 = 1
      flik2 = 1      
      frist = 1127
      frisl = 1161.      
   END.   
   ELSE IF  avtalvar BEGINS "20172018" THEN DO:     
      /*samma flik from 20160101*/ 
      ASSIGN
      sterad = 20  
      slerad = 1090
      /*sterad = 12  
      slerad = 1052*/
      /*FROM 20161018 bytt fr�n flik 13 till flik 1*/
      flik1 = 1
      flik2 = 1
      /*flik1 = 13
      flik2 = 13*/
      frist = 1127
      frisl = 1161.
      /*frist = 1077
      frisl = 1111.*/
      /*skrivkol = "c".*/
   END.
   ELSE IF  avtalvar BEGINS "2018" OR avtalvar BEGINS "20192019" THEN DO:  /*20182018*/
     /* oversatt137 = TRUE f�r att eonmallen 20182018 styr mot KLG2017 f�r 137 koder*/
      /*oversatt137 = TRUE .*/
      ASSIGN
      sterad = 18  
      slerad = 1152
      /*sterad = 12  
      slerad = 1052*/
      /*FROM 20161018 bytt fr�n flik 13 till flik 1*/
      flik1 = 5
      flik2 = 4
      /*flik1 = 13
      flik2 = 13*/
      frist = 18
      frisl = 52.
   END.
   ELSE IF avtalvar BEGINS  "2020"  THEN DO:
       ASSIGN
      sterad = 18  
      slerad = 1152
      /*sterad = 12  
      slerad = 1052*/
      /*FROM 20161018 bytt fr�n flik 13 till flik 1*/
      flik1 = 6
      flik2 = 4
      /*flik1 = 13
      flik2 = 13*/
      frist = 18
      frisl = 52.
   END.
   ELSE DO:  /*senaste*/
     /* oversatt137 = TRUE f�r att eonmallen 20182018 styr mot KLG2017 f�r 137 koder*/
      /*oversatt137 = TRUE .*/
      ASSIGN
      sterad = 18  
      slerad = 1152
      /*sterad = 12  
      slerad = 1052*/
      /*FROM 20161018 bytt fr�n flik 13 till flik 1*/
      flik1 = 7
      flik2 = 5
      /*flik1 = 13
      flik2 = 13*/
      frist = 18
      frisl = 55.
   END.       
   /**/
   /* 20122013
   sterad = 12  
   slerad = 1075
   flik1 = 13
   flik2 = 14.*/
   
END PROCEDURE.

PROCEDURE inframallhmt_UI:   
   DEFINE INPUT PARAMETER avtalvar AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad1 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad1 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad2 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad2 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad3 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad3 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER frist AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik1 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik2 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER oversatt137 AS LOGICAL NO-UNDO.   
   DEFINE INPUT PARAMETER ExelKommando AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER kalkstber AS LOGICAL NO-UNDO.
   
   oversatt137 = FALSE.
   
   IF kalkstber = TRUE THEN DO:
      IF ExelKommando MATCHES "*Schaktprotokoll*" THEN DO:
         assign
         flik1 = 1
         flik2 = 1.
      END.
      ELSE DO:   
         IF avtalvar BEGINS  "2019"  THEN DO:
            ASSIGN 
            sterad1 = 17
            slerad1 = 1049    /*1095  1081*/
            sterad2 = 18   /*20*/  
            slerad2 = 52   /*44*/
            sterad3 = 10    /*kraftringens mall f�r egna koder  ist�llet f�r sterad2*/
            slerad3 = 59
            frist = 1031 /* 1031 + 21 = 1052*/  
                         /* 1033 + 18 = 1051*/                      
            flik1 = 4
            flik2 = 6.
         END.
         ELSE IF avtalvar BEGINS  "20202020"  THEN DO:
             
            ASSIGN 
            sterad1 = 17
            slerad1 = 1058    /*1095  1081*/
            sterad2 = 18   /*20*/  
            slerad2 = 52   /*44*/
            sterad3 = 10    /*kraftringens mall f�r egna koder  ist�llet f�r sterad2*/
            slerad3 = 59
            frist = 1039 /* 1039 + 21 = 1060*/                                             
            flik1 = 4
            flik2 = 6.         
            IF ExelKommando MATCHES "*Mall-LN-EBR Kalkylmall KLG 1 20 Kraftringen ver 3.0*" THEN DO:
               /*EN RAD MINDRE I VERSION3*/
               frist = 1038. /* 1038 + 21 = 1059*/
            END.   
               
         END.             
         ELSE IF avtalvar BEGINS  "20202021"  THEN DO:
            ASSIGN 
            sterad1 = 17
            slerad1 = 1058    /*1095  1081*/
            sterad2 = 18   /*20*/  
            slerad2 = 52   /*44*/
            sterad3 = 10    /*kraftringens mall f�r egna koder  ist�llet f�r sterad2*/
            slerad3 = 59
            frist = 1038 /* 1038 + 21 = 1059*/                                             
            flik1 = 4
            flik2 = 6.
         END. 
         ELSE IF avtalvar BEGINS  "2021"  THEN DO:
            ASSIGN 
            sterad1 = 17
            slerad1 = 1017    /*1095  1081*/
            sterad2 = 21   /*20*/  
            slerad2 = 47   /*44*/
            sterad3 = 10    /*kraftringens mall f�r egna koder  ist�llet f�r sterad2*/
            slerad3 = 59
            frist = 997 /* 997 + 21 = 1018*/                                             
            flik1 = 5    /*m�ngdprotokoll*/
            flik2 = 7.
         END.  
         ELSE DO:
            ASSIGN 
            sterad1 = 17
            slerad1 = 1027    /*1095  1081*/
            sterad2 = 21   /*20*/  
            slerad2 = 47   /*44*/
            sterad3 = 10    /*kraftringens mall f�r egna koder  ist�llet f�r sterad2*/
            slerad3 = 59
            frist = 1007 /* 1007 + 21 = 1028*/                                             
            flik1 = 5    /*m�ngdprotokoll*/
            flik2 = 7.
         END.
      END.      
      
   END. 
   ELSE DO:     
      IF avtalvar BEGINS  "2017" OR avtalvar BEGINS  "2018" THEN DO:
         ASSIGN 
         sterad1 = 13  
         slerad1 = 1112    /*1095  1081*/
         sterad2 = 18   /*20*/  
         slerad2 = 52   /*44*/
         sterad3 = 6  
         slerad3 = 15
         frist = 1100   /* 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
         flik1 = 4
         flik2 = 5.
      END.
      ELSE IF ExelKommando MATCHES "*Kraftringen EBR kostnadskalkyl KLG*" THEN DO:
         IF avtalvar BEGINS  "2019"  THEN DO:
            ASSIGN 
            sterad1 = 13  
            slerad1 = 1048    /*1095  1081*/
            sterad2 = 10   /*20*/  
            slerad2 = 52   /*44*/
            sterad3 = 6  
            slerad3 = 15
            frist = 1040   /* 1040 + 10 = 1050   1037 + 18 = 1055 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
            flik1 = 4
            flik2 = 5.
         END.
         ELSE IF avtalvar BEGINS  "2020"  THEN DO:
            ASSIGN 
            sterad1 = 13  
            slerad1 = 1048    /*1095  1081*/
            sterad2 = 10   /*20*/  
            slerad2 = 52   /*44*/
            sterad3 = 6  
            slerad3 = 15
            frist = 1051   /* 1051 + 10 = 1061   1037 + 18 = 1055 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
            flik1 = 4
            flik2 = 5.
         END.
         ELSE DO:
            ASSIGN 
            sterad1 = 13  
            slerad1 = 1048    /*1095  1081*/
            sterad2 = 10   /*20*/  
            slerad2 = 52   /*44*/
            sterad3 = 6  
            slerad3 = 15
            frist = 1051   /* 1051 + 10 = 1061   1037 + 18 = 1055 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
            flik1 = 4
            flik2 = 5.
         END.   
      END.  
      ELSE IF avtalvar BEGINS  "2019" THEN DO:
          /*SENASTE*/
         ASSIGN 
         sterad1 = 13  
         slerad1 = 1046    /*1095  1081*/
         sterad2 = 18   /*20*/  
         slerad2 = 52   /*44*/
         sterad3 = 6  
         slerad3 = 15
         frist = 1035   /* 1035 + 21 = 1056   1037 + 18 = 1055 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
         flik1 = 4
         flik2 = 6.
      END.
      ELSE IF avtalvar BEGINS "20202020" THEN DO:
         
         ASSIGN 
         sterad1 = 13  
         slerad1 = 1054    /*1095  1081*/
         sterad2 = 18   /*20*/  
         slerad2 = 52   /*44*/
         sterad3 = 6  
         slerad3 = 15
         frist = 1043   /* 1043 + 21 = 1064   1037 + 18 = 1055 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
         flik1 = 4
         flik2 = 6.
      END. 
      ELSE IF  avtalvar BEGINS "20202021" THEN DO:
         
         ASSIGN 
         sterad1 = 13  
         slerad1 = 1054    /*1095  1081*/
         sterad2 = 18   /*20*/  
         slerad2 = 52   /*44*/
         sterad3 = 6  
         slerad3 = 15
         frist = 1042   /* 1042 + 21 = 1063   1037 + 18 = 1055 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
         flik1 = 4
         flik2 = 6.
      END.
      ELSE IF  avtalvar BEGINS "2021" THEN DO:
         ASSIGN 
         sterad1 = 13  
         slerad1 = 1017    /*1095  1081*/
         sterad2 = 21   /*20*/  
         slerad2 = 47   /*44*/
         sterad3 = 6  
         slerad3 = 15
         frist = 1001  /* 1001 + 21 = 1022   1037 + 18 = 1055 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
         flik1 = 5
         flik2 = 7.
      END.
      ELSE DO:
         /*SENASTE*/
         ASSIGN 
         sterad1 = 13  
         slerad1 = 1027    /*1095  1081*/
         sterad2 = 21   /*20*/  
         slerad2 = 47   /*44*/
         sterad3 = 6  
         slerad3 = 15
         frist = 1011  /* 1011 + 21 = 1032   1037 + 18 = 1055 1100 + 18 = 1118  1060 + 18 = 1078 f�r  2015   1023 + 20 = 1043   1017 + 20 =1037*/
         flik1 = 5
         flik2 = 7.
      END.   
   END.      
END PROCEDURE.

PROCEDURE vattmalleonhmt_UI:
   DEFINE OUTPUT PARAMETER sterad AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad2 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad2 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik1 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik2 AS INTEGER NO-UNDO.
   ASSIGN 
   sterad = 17 
   slerad = 1085
   sterad2 = 18  
   slerad2 = 52
   flik1 = 5
   flik2 = 4.
   
END PROCEDURE.



PROCEDURE C4mallhmt_UI:
   DEFINE INPUT PARAMETER avtalvar AS CHARACTER  NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad1 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad1 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad2 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad2 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad3 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad3 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER frist AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik1 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik2 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER oversatt137 AS LOGICAL NO-UNDO.   
   DEFINE INPUT PARAMETER ExelKommando AS CHARACTER  NO-UNDO.
   DEFINE INPUT PARAMETER kalkstber AS LOGICAL NO-UNDO.
   
   
   
   ASSIGN 
   sterad1 = 100 
   slerad1 = 920
   sterad2 = 1120  
   slerad2 = 1154
   flik1 = 1
   flik2 = 2.
   
END PROCEDURE.
PROCEDURE vattmallelleviohmt_UI:
   DEFINE OUTPUT PARAMETER sterad AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad2 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad2 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik1 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik2 AS INTEGER NO-UNDO.
   ASSIGN 
   sterad = 18 
   slerad = 125
   sterad2 = 0 
   slerad2 =0
   flik1 = 1
   flik2 = 3.
   
END PROCEDURE.
PROCEDURE vattmallhmt_UI:
   DEFINE OUTPUT PARAMETER sterad AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER sterad2 AS INTEGER  NO-UNDO.
   DEFINE OUTPUT PARAMETER slerad2 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik1 AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER flik2 AS INTEGER NO-UNDO.
   ASSIGN 
   sterad = 8 
   slerad = 1032
   /*sterad2 = 18  
   slerad2 = 52*/
   flik1 = 1
   flik2 = 7.
   
END PROCEDURE.



PROCEDURE kalkylbort_UI :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER felmed AS CHARACTER NO-UNDO.
   DEFINE VARIABLE berapph AS HANDLE NO-UNDO.
   DEFINE VARIABLE lasavanv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lasavnamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lastav AS LOGICAL NO-UNDO.
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKHUV THEN RETURN.
  
   RUN KalkStopp_UI (INPUT 3,INPUT KALKHUV.KALKNR,INPUT KALKHUV.OMRADE, INPUT Guru.Konstanter:globanv, OUTPUT lasavanv, OUTPUT lasavnamn, OUTPUT lastav).
   IF lastav = TRUE THEN DO:
      felmed = "Kalkylen �r l�st av " + lasavanv + " " + lasavnamn.
      RETURN.
   END.   
   RUN MENYBERAPP.P PERSISTENT SET berapph.
   RUN procsetkop_UI.
   /*KALKYL-BEREDNING SUNDSVALL*/
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "KALKBER"                   
   inextrakopptemp.KOPPLACHAR1 = KALKHUV.OMRADE
   inextrakopptemp.KOPPLAINT1 = KALKHUV.KALKNR
   inextrakopptemp.KOPPLACHAR2 = ?
   inextrakopptemp.KOPPLAINT2 = ?. 
   RUN etabhamt_UI IN fbestapph (INPUT TABLE inextrakopptemp,OUTPUT TABLE extrakopptemp).  
   
   FIND FIRST extrakopptemp WHERE NO-LOCK NO-ERROR.
   /*FINNS DET KOPPLING*/
   IF AVAILABLE extrakopptemp THEN DO:
      FIND FIRST valsoktemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE valsoktemp THEN CREATE valsoktemp.
      ASSIGN
      valsoktemp.SOKVAL = 3
      valsoktemp.SOKINT[1] = extrakopptemp.KOPPLAINT2
      valsoktemp.SOKINT[2] = Guru.Konstanter:globniv
      valsoktemp.SOKCHAR[1] = extrakopptemp.KOPPLACHAR2.                     
      RUN bort_UI IN berapph (INPUT-OUTPUT TABLE valsoktemp).   
      FIND FIRST valsoktemp NO-LOCK NO-ERROR.
      IF valsoktemp.SOKINT[10] = 1 THEN DO:
          /*BEREDNINGEN �R L�ST*/
         felmed = "Kalkylen �r kopplad till en beredning och " + valsoktemp.SOKCHAR[10].
         RETURN.                
      END.
      ELSE DO:
         /*BEREDNINGEN OCH KOPPLING TAS BORTG*/
         RUN DELBERE.P (INPUT STRING(extrakopptemp.KOPPLAINT2), INPUT extrakopptemp.KOPPLACHAR2).
      END.
   END.
      
   FOR EACH GURUDEFAULTS WHERE GURUDEFAULTS.PROGRAM = "KALKYL" AND GURUDEFAULTS.HUVUDINT = KALKHUV.KALKNR  AND GURUDEFAULTS.HUVUDCHAR = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE GURUDEFAULTS.
   END. 
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE KALKNUM.
   END.    
   FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKHUV.KALKNR  AND KALKNUMSUB.OMRADE = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE KALKNUMSUB.
   END.
   FOR EACH KALKAONR WHERE KALKAONR.KALKNR = KALKHUV.KALKNR  AND KALKAONR.OMRADE = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE KALKAONR.
   END.
   FOR EACH KALKFAKTORER WHERE KALKFAKTORER.KALKNR = KALKHUV.KALKNR  AND KALKFAKTORER.OMRADE = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE KALKFAKTORER.
   END.
   FOR EACH KALKEGNAPRISER WHERE KALKEGNAPRISER.KALKNR = KALKHUV.KALKNR  AND KALKEGNAPRISER.OMRADE = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE KALKEGNAPRISER.
   END.
   FOR EACH KALKMTRL WHERE KALKMTRL.KALKNR = KALKHUV.KALKNR  AND KALKMTRL.OMRADE = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE KALKMTRL.
   END.
    FOR EACH HDVOLYMBER WHERE HDVOLYMBER.KALKNR = KALKHUV.KALKNR  AND HDVOLYMBER.OMRADE = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE HDVOLYMBER.
   END.
 
   DO TRANSACTION: 
      FIND CURRENT KALKHUV EXCLUSIVE-LOCK NO-ERROR. 
      IF AVAILABLE KALKHUV THEN       DELETE KALKHUV.
   END.
   RUN procresetkop_UI.
   IF VALID-HANDLE(berapph) THEN DELETE PROCEDURE berapph NO-ERROR.
END PROCEDURE.
/*KOPIERA START */
PROCEDURE kopierakalkyl_UI :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER NyKatIdvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nyomrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER nykalknr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE kalkylarbkodertt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylloppostertt NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar AND KALKHUV.OMRADE = omrvar NO-LOCK NO-ERROR.
   IF KALKHUV.KLOGID = NyKatIdvar THEN.
   ELSE DO:
      FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
      FIND FIRST KALKYLKATALOGBUF WHERE KALKYLKATALOGBUF.KLOGID = NyKatIdvar NO-LOCK NO-ERROR.
      IF KALKYLKATALOG.VISARTAL = 1999 THEN DO:
         CREATE tidut.
         tidut.UT = "Kalkylen g�r ej att kopiera d� den �r gjord enligt " + STRING(KALKYLKATALOG.VISARTAL)  + " �rs kostnadskatalog. Formatet har �ndrats.".
         tidut.TTRECID = RECID(tidut).
         RETURN.   
      END.
      IF ABSOLUTE(KALKYLKATALOGBUF.VISARTAL - KALKYLKATALOG.VISARTAL) > 3 THEN DO:
         CREATE tidut.
         tidut.UT = "Kalkylen g�r ej att kopiera d� den �r bygger p� en f�r gammal kostnadskatalog.".
         tidut.TTRECID = RECID(tidut).
         RETURN.      
      END.
   END. 
   RUN omradekoll_UI (INPUT nyomrvar, OUTPUT nykalknr).  
   IF nykalknr = ? THEN DO:
      CREATE tidut.
      tidut.UT =  "Det g�r inte att l�gga upp kalkyler p� detta " + LC(Guru.Konstanter:gomrk) + ". Nummerserie saknas eller �r fylld.".
      tidut.TTRECID = RECID(tidut).      
      RETURN.
   END.
   RUN copycat_UI (INPUT KalkNRvar, INPUT omrvar,INPUT NyKatIdvar,INPUT nykalknr,INPUT nyomrvar, INPUT KALKHUV.TYPKALK, INPUT ?, INPUT ?).
   RUN copyrel_UI.   
END PROCEDURE.
PROCEDURE kopieraFRI_UI :
   /*endast mellan samma kataloger*/
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER NyKatIdvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nyomrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER nykalknr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE kalkylarbkodertt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylloppostertt NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar AND KALKHUV.OMRADE = omrvar NO-LOCK NO-ERROR.
   IF KALKHUV.KLOGID = NyKatIdvar THEN.
   ELSE DO:
      FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
      FIND FIRST KALKYLKATALOGBUF WHERE KALKYLKATALOGBUF.KLOGID = NyKatIdvar NO-LOCK NO-ERROR.
      IF KALKYLKATALOG.VISARTAL = 1999 THEN DO:
         CREATE tidut.
         tidut.UT = "Kalkylen g�r ej att kopiera d� den �r gjord enligt " + STRING(KALKYLKATALOG.VISARTAL)  + " �rs kostnadskatalog. Formatet har �ndrats.".
         tidut.TTRECID = RECID(tidut).
         RETURN.   
      END.
      IF ABSOLUTE(KALKYLKATALOGBUF.VISARTAL - KALKYLKATALOG.VISARTAL) > 3 THEN DO:
         CREATE tidut.
         tidut.UT = "Kalkylen g�r ej att kopiera d� den �r bygger p� en f�r gammal kostnadskatalog.".
         tidut.TTRECID = RECID(tidut).
         RETURN.      
      END.
   END. 
   RUN omradekoll_UI (INPUT nyomrvar, OUTPUT nykalknr).  
   IF nykalknr = ? THEN DO:
      CREATE tidut.
      tidut.UT =  "Det g�r inte att l�gga upp kalkyler p� detta " + LC(Guru.Konstanter:gomrk) + ". Nummerserie saknas eller �r fylld.".
      tidut.TTRECID = RECID(tidut).      
      RETURN.
   END.
   RUN copycatFri_UI (INPUT KalkNRvar, INPUT omrvar,INPUT NyKatIdvar,INPUT nykalknr,INPUT nyomrvar, INPUT KALKHUV.TYPKALK, INPUT ?, INPUT ?).
   RUN copyrel_UI.   
END PROCEDURE.
/*flyttakalkyl*/
PROCEDURE FlyttaKalkylNykat_UI :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER NyKatIdvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE kalkylarbkodertt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylloppostertt NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   DO TRANSACTION:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar AND KALKHUV.OMRADE = omrvar EXCLUSIVE-LOCK NO-ERROR.
      IF KALKHUV.KLOGID = NyKatIdvar THEN.
      ELSE DO:
         KALKHUV.KLOGID = NyKatIdvar.
         FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
         FIND FIRST KALKYLKATALOGBUF WHERE KALKYLKATALOGBUF.KLOGID = NyKatIdvar NO-LOCK NO-ERROR.
         IF KALKYLKATALOG.VISARTAL = 1999 THEN DO:
            CREATE tidut.
            tidut.UT = "Kalkylen g�r ej att flytta d� den �r gjord enligt " + STRING(KALKYLKATALOG.VISARTAL)  + " �rs kostnadskatalog. Formatet har �ndrats.".
            tidut.TTRECID = RECID(tidut).
            RETURN.   
         END.
         IF ABSOLUTE(KALKYLKATALOGBUF.VISARTAL - KALKYLKATALOG.VISARTAL) > 3 THEN DO:
            CREATE tidut.
            tidut.UT = "Kalkylen g�r ej att flytta d� den �r bygger p� en f�r gammal kostnadskatalog.".
            tidut.TTRECID = RECID(tidut).
            RETURN.      
         END.
      END.
      FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE EXCLUSIVE-LOCK: 
         IF KALKNUM.ARBKOD = "EGEN" THEN.
         ELSE DO: 
            FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKHUV.KALKNR  AND KALKNUMSUB.OMRADE = KALKHUV.OMRADE AND KALKNUMSUB.NUM = KALKNUM.NUM EXCLUSIVE-LOCK:
               DELETE KALKNUMSUB.
            END.
            DELETE KALKNUM.
         END.   
      END. 
  END.   
  RELEASE KALKHUV NO-ERROR.    
END PROCEDURE.


/*SLIHOP START */
PROCEDURE SlaIhopKalkyler :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER TABLE-HANDLE slaihophin.
   DEFINE OUTPUT PARAMETER TABLE FOR KalkylimportTT.
   DEFINE VARIABLE slaihoph AS HANDLE NO-UNDO.
   DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE matrisnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE statusvar AS CHARACTER NO-UNDO.
   EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
   slaihoph = slaihophin:DEFAULT-BUFFER-HANDLE.
   slaihoph:FIND-FIRST("WHERE AONR NE ?") NO-ERROR.
   IF slaihoph:AVAILABLE THEN DO:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = "'" + slaihoph:BUFFER-FIELD("AONR"):BUFFER-VALUE + "'"  AND AONRTAB.DELNR = 0 NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         DO TRANSACTION:
            FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = KalkNrvar AND KALKAONR.OMRADE = omrvar EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE KALKAONR THEN DO:
               ASSIGN 
               KALKAONR.AONR = KALKAONR.AONR
               KALKAONR.DELNR = KALKAONR.DELNR
               KALKAONR.PLANNR = ?
               KALKAONR.ARTAL = ?.
            END.   
            
            RUN statusnivkoll_UI ( OUTPUT statusvar).
            KALKAONR.STATUSNIV = statusvar.
         END.   
      END.
   END.
         
   kommandoquery = "FOR EACH " + slaihoph:TABLE + " NO-LOCK".
   RUN CreateCustomQuery(INPUT slaihoph,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      
      matrisnr = matrisnr + 1.
      RUN slaihopcat_UI (INPUT KalkNRvar, INPUT omrvar,INPUT  slaihoph:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT  slaihoph:BUFFER-FIELD("OMRADE"):BUFFER-VALUE,INPUT matrisnr).
      qH:GET-NEXT().
   END.
   RUN copyrel_UI.  
   DELETE OBJECT slaihoph NO-ERROR. 
   slaihoph = ?.
   DELETE OBJECT slaihophin NO-ERROR.
   slaihophin = ?.
   DELETE OBJECT qH NO-ERROR.
   qH = ?.
END PROCEDURE.

/*jmf kalk START */
PROCEDURE JmfKalkyler :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER TABLE-HANDLE jmftthin.
   DEFINE OUTPUT PARAMETER TABLE FOR KalkylimportTT.
   DEFINE VARIABLE jmfcath AS HANDLE NO-UNDO.
   DEFINE VARIABLE numsubvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE matrisnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE statusvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE minusvar AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE KalkylimportTT NO-ERROR. 
   
   
   jmfcath = jmftthin:DEFAULT-BUFFER-HANDLE.
   jmfcath:FIND-FIRST("WHERE AONR NE ?") NO-ERROR.
   IF jmfcath:AVAILABLE THEN DO:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = "'" + jmfcath:BUFFER-FIELD("AONR"):BUFFER-VALUE + "'"  AND AONRTAB.DELNR = 0 NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         DO TRANSACTION:
            FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = KalkNrvar AND KALKAONR.OMRADE = omrvar EXCLUSIVE-LOCK NO-ERROR.
            IF AVAILABLE KALKAONR THEN DO:
               ASSIGN 
               KALKAONR.AONR = KALKAONR.AONR
               KALKAONR.DELNR = KALKAONR.DELNR
               KALKAONR.PLANNR = ?
               KALKAONR.ARTAL = ?.
            END.   
            
            RUN statusnivkoll_UI ( OUTPUT statusvar).
            KALKAONR.STATUSNIV = statusvar.
         END.   
      END.
   END.
         
   kommandoquery = "FOR EACH " + jmfcath:TABLE + " NO-LOCK".
   RUN CreateCustomQuery(INPUT jmfcath,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   minusvar = 1.
   DO WHILE qH:QUERY-OFF-END = FALSE:
      matrisnr = 1.
      RUN jmfcat_UI (INPUT minusvar,INPUT KalkNRvar, INPUT omrvar,INPUT  jmfcath:BUFFER-FIELD("KALKNR"):BUFFER-VALUE,INPUT  jmfcath:BUFFER-FIELD("OMRADE"):BUFFER-VALUE,INPUT matrisnr).
      minusvar = -1.
      qH:GET-NEXT().
   END.
   RUN copyrel_UI.  
   DELETE OBJECT jmfcath NO-ERROR. 
   DELETE OBJECT jmftthin NO-ERROR.
   DELETE OBJECT qh NO-ERROR.
END PROCEDURE.

PROCEDURE KonverteraKalkyl_UI :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER nykattyp AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nyomrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER nykalknr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR kalknumtt.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE konverterainte AS LOGICAL NO-UNDO.
   EMPTY TEMP-TABLE kalknumtt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylarbkodertt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylloppostertt NO-ERROR. 
   EMPTY TEMP-TABLE tidut NO-ERROR. 
  
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar AND KALKHUV.OMRADE = omrvar NO-LOCK NO-ERROR.
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR AND KALKNUM.OMRADE = KALKHUV.OMRADE NO-LOCK: 
     IF KALKNUM.TYPKALK NE KALKHUV.TYPKALK THEN DO:
         konverterainte = TRUE.
      END.
   END.
   IF konverterainte = TRUE THEN DO:
      CREATE tidut.
      tidut.UT =  "Kalkylen �r gjord med koder fr�n olika typer, konvertering ej m�jlig.".
      tidut.TTRECID = RECID(tidut).
      RETURN.
   END.
   RUN omradekoll_UI (INPUT nyomrvar, OUTPUT nykalknr).  
   IF nykalknr = ? THEN DO:
      CREATE tidut.
      tidut.UT =  "Det g�r inte att l�gga upp kalkyler p� detta " + LC(Guru.Konstanter:gomrk) + ". Nummerserie saknas eller �r fylld.".
      tidut.TTRECID = RECID(tidut).      
      RETURN.
   END.
   RUN copycat_UI (INPUT KalkNRvar, INPUT omrvar,INPUT KALKHUV.KLOGID,INPUT nykalknr,INPUT nyomrvar, INPUT nykattyp,INPUT ?, INPUT ?).
   IF nykattyp = 2 OR nykattyp = 3 THEN RUN konvkalk_UI.
   IF nykattyp = 7 THEN RUN konvp1natkalk_UI.
   IF nykattyp = 1 THEN RUN konvnatkalk_UI.
   
   RUN copyrel_UI. 
END PROCEDURE.

PROCEDURE konvkalk_UI:
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR AND
   KALKNUM.OMRADE = KALKHUV.OMRADE NO-LOCK.
      IF KALKNUM.ARBKOD = "EGEN" THEN.
      ELSE DO:         
         FIND FIRST FREKVENSKATALOG WHERE FREKVENSKATALOG.ARBKOD = KALKNUM.ARBKOD AND FREKVENSKATALOG.LOPNR = KALKNUM.LOPNR AND 
         FREKVENSKATALOG.KLOGSUBID = KALKNUM.KLOGSUBID NO-LOCK NO-ERROR.
         IF AVAILABLE FREKVENSKATALOG THEN DO: 
            FOR EACH FREKVENSKATALOG WHERE FREKVENSKATALOG.ARBKOD = KALKNUM.ARBKOD AND FREKVENSKATALOG.LOPNR = KALKNUM.LOPNR AND 
            FREKVENSKATALOG.KLOGSUBID = KALKNUM.KLOGSUBID NO-LOCK:
               CREATE kalknumtt.
               BUFFER-COPY KALKNUM EXCEPT KALKNUM.ARBKOD TO kalknumtt.
               ASSIGN 
               kalknumtt.ARBKOD = FREKVENSKATALOG.FREKOD
               kalknumtt.LOPNR  = FREKVENSKATALOG.FREKNR
               kalknumtt.ANTAL  = KALKNUM.ANTAL * FREKVENSKATALOG.ANTAL
               kalknumtt.TYPKALK  = KALKHUVBUF.TYPKALK.
            END.
         END.      
         ELSE DO:
            CREATE tidut.
            tidut.UT =  "Frekvens tabell f�r arbetskod:" + KALKNUM.ARBKOD +
                  " med l�pnr:" + STRING(KALKNUM.LOPNR,Guru.Konstanter:varforetypchar[6]) + " saknas. L�gg till manuellt i kalkylen.".
            tidut.TTRECID = RECID(tidut).                                
         END.
      END.      
   END.     
END PROCEDURE.

PROCEDURE konvnatkalk_UI:
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR AND
   KALKNUM.OMRADE = KALKHUV.OMRADE NO-LOCK.
      IF KALKNUM.ARBKOD = "EGEN" THEN.
      IF SUBSTRING(KALKNUM.ARBKOD,1,1) = "N" THEN DO: 
         CREATE kalknumtt.
         BUFFER-COPY KALKNUM EXCEPT KALKNUM.ARBKOD TO kalknumtt.
         ASSIGN 
         kalknumtt.ARBKOD = "G" + SUBSTRING(KALKNUM.ARBKOD,2)
         kalknumtt.TYPKALK  = KALKNUMBUF.TYPKALK.                 
      END.      
      CREATE tidut.
      tidut.UT =  "Frekvens tabell f�r arbetskod:" + KALKNUM.ARBKOD +
      " med l�pnr:" + STRING(KALKNUM.LOPNR,Guru.Konstanter:varforetypchar[6]) + " saknas. L�gg till manuellt i kalkylen.".
      tidut.TTRECID = RECID(tidut).   
   END.
      
END PROCEDURE.

PROCEDURE konvp1natkalk_UI:
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR AND
   KALKNUM.OMRADE = KALKHUV.OMRADE NO-LOCK.
      IF KALKNUM.ARBKOD = "EGEN" THEN.
      IF SUBSTRING(KALKNUM.ARBKOD,1,1) = "G" THEN DO: 
         CREATE kalknumtt.
         BUFFER-COPY KALKNUM EXCEPT KALKNUM.ARBKOD TO kalknumtt.
         ASSIGN 
         kalknumtt.ARBKOD = "N" + SUBSTRING(KALKNUM.ARBKOD,2)
         kalknumtt.TYPKALK  = KALKNUMBUF.TYPKALK.                 
      END.      
      ELSE DO:
         CREATE tidut.
         tidut.UT = "Motsvarigheten till  Arbetskod:" + KALKNUM.ARBKOD + " med l�pnr:" + STRING(KALKNUM.LOPNR,Guru.Konstanter:varforetypchar[6]) + " �terfanns ej bland N�tkoderna och konverterades d�rf�r inte.".
         tidut.TTRECID = RECID(tidut).
      END.                                 
   END.      
END PROCEDURE.

/*SL�PP POSTER*/
PROCEDURE copyrel_UI :
   RELEASE KALKHUVBUF NO-ERROR.
   RELEASE KALKAONR NO-ERROR.
   RELEASE GURUDEFAULTSBUF NO-ERROR.
   RELEASE KALKFAKTORERBUF NO-ERROR.
   RELEASE KALKEGNAPRISERBUF NO-ERROR.
   RELEASE KALKMTRLBUF NO-ERROR.
   RELEASE KALKNUMBUF NO-ERROR.
   RELEASE KALKNUMSUBBUF NO-ERROR.
END PROCEDURE.
/*SJ�LVA KOPIERA*/

PROCEDURE copycatFRI_UI :
   /*endast mellan samma kataloger MED FRI KALK*/
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER NyKatIdvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nykalknr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nyomrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER nytyp AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER nyaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyadelnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE statusvar AS CHARACTER NO-UNDO.
   /*SE copycat_UI*/
   DO TRANSACTION:
      CREATE KALKHUVBUF.
      BUFFER-COPY KALKHUV EXCEPT KALKHUV.KALKNR TO KALKHUVBUF.
      ASSIGN
      KALKHUVBUF.KALKNR = nykalknr
      KALKHUVBUF.OMRADE = nyomrvar
      KALKHUVBUF.KLOGID = NyKatIdvar
      KALKHUVBUF.TYPKALK = nytyp.
      FIND FIRST kalkaobuff WHERE kalkaobuff.KALKNR = KalkNrvar AND kalkaobuff.OMRADE = omrvar NO-LOCK NO-ERROR.
      CREATE KALKAONR.
      ASSIGN
      KALKAONR.KALKNR = nykalknr
      KALKAONR.OMRADE = nyomrvar
      KALKAONR.AKTIV = TRUE 
      KALKAONR.TYP = KALKHUVBUF.TYPKALK.
      IF nyaonr = ? THEN DO:
         /*fr�n kalkdb.cls*/
         ASSIGN 
         KALKAONR.AONR = kalkaobuff.AONR
         KALKAONR.DELNR = kalkaobuff.DELNR
         KALKAONR.PLANNR = kalkaobuff.PLANNR
         KALKAONR.ARTAL = kalkaobuff.ARTAL.
         IF kalkaobuff.AONR = ? AND  kalkaobuff.PLANNR = ? THEN KALKHUVBUF.AKTIV = TRUE.
      END.   
      ELSE DO:
         /*KOPPLA OCH KOPIERA*/
         IF aonrplan = TRUE THEN DO:
            /*TILL AONRkalkyl*/
            ASSIGN    
            KALKAONR.AONR = nyaonr
            KALKAONR.DELNR = nyadelnr
            KALKAONR.PLANNR = kalkaobuff.PLANNR
            KALKAONR.ARTAL = kalkaobuff.ARTAL.
            KALKHUVBUF.AKTIV = TRUE.
         END.   
         ELSE IF aonrplan = FALSE THEN DO:
            /*TILL PLANKALKYL*/
            ASSIGN    
            KALKAONR.AONR = kalkaobuff.AONR
            KALKAONR.DELNR = kalkaobuff.DELNR
            KALKAONR.PLANNR = nyaonr
            KALKAONR.ARTAL = nyadelnr.
            KALKHUVBUF.AKTIV = TRUE.
         END.   
         ELSE DO:
            /*FR�N PLAN TILL AONRKALK ENDAST AKTIVA AONR*/
            ASSIGN    
            KALKAONR.AONR = nyaonr
            KALKAONR.DELNR = nyadelnr
            KALKAONR.PLANNR = ?
            KALKAONR.ARTAL = ?.
         END.   
      END.   
      
      RUN statusnivkoll_UI ( OUTPUT statusvar).
      KALKAONR.STATUSNIV = statusvar.
   END.   
   FOR EACH GURUDEFAULTS WHERE GURUDEFAULTS.PROGRAM = "KALKYL" AND GURUDEFAULTS.HUVUDINT = KALKHUV.KALKNR  AND GURUDEFAULTS.HUVUDCHAR = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      CREATE GURUDEFAULTSBUF.
      BUFFER-COPY GURUDEFAULTS TO GURUDEFAULTSBUF.
      ASSIGN
      GURUDEFAULTSBUF.HUVUDINT  = KALKHUVBUF.KALKNR  
      GURUDEFAULTSBUF.HUVUDCHAR  = KALKHUVBUF.OMRADE.         
   END.
   FOR EACH KALKFAKTORER WHERE KALKFAKTORER.KALKNR = KALKHUV.KALKNR  AND KALKFAKTORER.OMRADE = KALKHUV.OMRADE  NO-LOCK:
      CREATE KALKFAKTORERBUF.
      BUFFER-COPY KALKFAKTORER TO KALKFAKTORERBUF.
      ASSIGN
      KALKFAKTORERBUF.KALKNR  = KALKHUVBUF.KALKNR  
      KALKFAKTORERBUF.OMRADE  = KALKHUVBUF.OMRADE. 
   END.
   /*Anders Olsson Elpool i Ume� AB  15 maj 2017 14:58:47 
   det blir fel priser om man byter kat�r 
   */
   IF KALKHUV.KLOGID = KALKHUVBUF.KLOGID THEN DO:
      FOR EACH KALKEGNAPRISER WHERE KALKEGNAPRISER.KALKNR = KALKHUV.KALKNR  AND KALKEGNAPRISER.OMRADE = KALKHUV.OMRADE  NO-LOCK:
         CREATE KALKEGNAPRISERBUF.
         BUFFER-COPY KALKEGNAPRISER EXCEPT KALKEGNAPRISER.KALKNR TO KALKEGNAPRISERBUF.
         ASSIGN
         KALKEGNAPRISERBUF.KALKNR  = KALKHUVBUF.KALKNR  
         KALKEGNAPRISERBUF.OMRADE  = KALKHUVBUF.OMRADE. 
      END.
   END.
   ELSE DO:
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKHUVBUF.KLOGID AND KALKYLKATALOGSUB.AVSLUTAD = FALSE NO-LOCK:
         FOR EACH KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLPRISER.EGENPRISUPP = TRUE NO-LOCK:
            CREATE KALKEGNAPRISERBUF.
            BUFFER-COPY KALKYLPRISER TO KALKEGNAPRISERBUF.
            ASSIGN
            KALKEGNAPRISERBUF.KALKNR  = KALKHUVBUF.KALKNR  
            KALKEGNAPRISERBUF.OMRADE  = KALKHUVBUF.OMRADE.
         END.         
      END. 
   END.
   
   FOR EACH KALKMTRL WHERE KALKMTRL.KALKNR = KALKHUV.KALKNR  AND KALKMTRL.OMRADE = KALKHUV.OMRADE  NO-LOCK:
      CREATE KALKMTRLBUF.
      BUFFER-COPY KALKMTR EXCEPT KALKMTR.KALKNR TO KALKMTRLBUF.
      ASSIGN
      KALKMTRLBUF.KALKNR  = KALKHUVBUF.KALKNR  
      KALKMTRLBUF.OMRADE  = KALKHUVBUF.OMRADE. 
   END.
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE  NO-LOCK:
      FIND FIRST KALKNUMBUF  WHERE 
      KALKNUMBUF.KALKNR  = KALKHUVBUF.KALKNR AND  
      KALKNUMBUF.OMRADE  = KALKHUVBUF.OMRADE AND
      KALKNUMSUBBUF.NUM  = KALKNUM.NUM 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKNUMBUF THEN DO:
         CREATE KALKNUMBUF.
         BUFFER-COPY KALKNUM EXCEPT KALKNUM.KALKNR KALKNUM.OMRADE TO KALKNUMBUF .
         /*
         FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = NyKatIdvar NO-LOCK NO-ERROR.
         FIND FIRST KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID NO-LOCK NO-ERROR.
          */
         ASSIGN
       /*  KALKNUMBUF.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID*/
         KALKNUMBUF.KALKNR  = KALKHUVBUF.KALKNR  
         KALKNUMBUF.OMRADE  = KALKHUVBUF.OMRADE                
         KALKNUMBUF.TYPKALK = KALKHUVBUF.TYPKALK.
         FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKHUV.KALKNR  AND KALKNUMSUB.OMRADE = KALKHUV.OMRADE AND KALKNUMSUB.NUM = KALKNUM.NUM NO-LOCK:
            FIND FIRST KALKNUMSUBBUF WHERE 
            KALKNUMSUBBUF.KALKNR  = KALKHUVBUF.KALKNR AND 
            KALKNUMSUBBUF.OMRADE  = KALKHUVBUF.OMRADE AND
            KALKNUMSUBBUF.NUM  = KALKNUMSUB.NUM AND 
            KALKNUMSUBBUF.NUMSUBID  = KALKNUMSUB.NUMSUBID 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE KALKNUMSUBBUF THEN DO:
               CREATE KALKNUMSUBBUF.
               BUFFER-COPY KALKNUMSUB EXCEPT KALKNUMSUB.KALKNR TO KALKNUMSUBBUF.
               ASSIGN
               KALKNUMSUBBUF.KALKNR  = KALKHUVBUF.KALKNR  
               KALKNUMSUBBUF.OMRADE  = KALKHUVBUF.OMRADE.  
            END.
         END.
      END.   
   END.
  
END PROCEDURE.
PROCEDURE copycat_UI :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER NyKatIdvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nykalknr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nyomrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER nytyp AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER nyaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyadelnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE statusvar AS CHARACTER NO-UNDO.
   DO TRANSACTION:
      CREATE KALKHUVBUF.
      BUFFER-COPY KALKHUV EXCEPT KALKHUV.KALKNR TO KALKHUVBUF.
      ASSIGN
      KALKHUVBUF.KALKNR = nykalknr
      KALKHUVBUF.OMRADE = nyomrvar
      KALKHUVBUF.KLOGID = NyKatIdvar
      KALKHUVBUF.TYPKALK = nytyp.
      FIND FIRST kalkaobuff WHERE kalkaobuff.KALKNR = KalkNrvar AND kalkaobuff.OMRADE = omrvar NO-LOCK NO-ERROR.
      CREATE KALKAONR.
      ASSIGN
      KALKAONR.KALKNR = nykalknr
      KALKAONR.OMRADE = nyomrvar
      KALKAONR.AKTIV = TRUE 
      KALKAONR.TYP = KALKHUVBUF.TYPKALK.
      IF nyaonr = ? THEN DO:
         /*fr�n kalkdb.cls*/
         ASSIGN 
         KALKAONR.AONR = kalkaobuff.AONR
         KALKAONR.DELNR = kalkaobuff.DELNR
         KALKAONR.PLANNR = kalkaobuff.PLANNR
         KALKAONR.ARTAL = kalkaobuff.ARTAL.
         IF kalkaobuff.AONR = ? AND  kalkaobuff.PLANNR = ? THEN KALKHUVBUF.AKTIV = TRUE.
      END.   
      ELSE DO:
         /*KOPPLA OCH KOPIERA*/
         IF aonrplan = TRUE THEN DO:
            /*TILL AONRkalkyl*/
            ASSIGN    
            KALKAONR.AONR = nyaonr
            KALKAONR.DELNR = nyadelnr
            KALKAONR.PLANNR = kalkaobuff.PLANNR
            KALKAONR.ARTAL = kalkaobuff.ARTAL.
            KALKHUVBUF.AKTIV = TRUE.
         END.   
         ELSE IF aonrplan = FALSE THEN DO:
            /*TILL PLANKALKYL*/
            ASSIGN    
            KALKAONR.AONR = kalkaobuff.AONR
            KALKAONR.DELNR = kalkaobuff.DELNR
            KALKAONR.PLANNR = nyaonr
            KALKAONR.ARTAL = nyadelnr.
            KALKHUVBUF.AKTIV = TRUE.
         END.   
         ELSE DO:
            /*FR�N PLAN TILL AONRKALK ENDAST AKTIVA AONR*/
            ASSIGN    
            KALKAONR.AONR = nyaonr
            KALKAONR.DELNR = nyadelnr
            KALKAONR.ARTAL = ?.
         END.   
      END.   
      
      RUN statusnivkoll_UI ( OUTPUT statusvar).
      KALKAONR.STATUSNIV = statusvar.
   END.   
   FOR EACH GURUDEFAULTS WHERE GURUDEFAULTS.PROGRAM = "KALKYL" AND GURUDEFAULTS.HUVUDINT = KALKHUV.KALKNR  AND GURUDEFAULTS.HUVUDCHAR = KALKHUV.OMRADE  EXCLUSIVE-LOCK:
      CREATE GURUDEFAULTSBUF.
      BUFFER-COPY GURUDEFAULTS TO GURUDEFAULTSBUF.
      ASSIGN
      GURUDEFAULTSBUF.HUVUDINT  = KALKHUVBUF.KALKNR  
      GURUDEFAULTSBUF.HUVUDCHAR  = KALKHUVBUF.OMRADE.         
   END.
   FOR EACH KALKFAKTORER WHERE KALKFAKTORER.KALKNR = KALKHUV.KALKNR  AND KALKFAKTORER.OMRADE = KALKHUV.OMRADE  NO-LOCK:
      CREATE KALKFAKTORERBUF.
      BUFFER-COPY KALKFAKTORER TO KALKFAKTORERBUF.
      ASSIGN
      KALKFAKTORERBUF.KALKNR  = KALKHUVBUF.KALKNR  
      KALKFAKTORERBUF.OMRADE  = KALKHUVBUF.OMRADE. 
   END.
   /*Anders Olsson Elpool i Ume� AB  15 maj 2017 14:58:47 
   det blir fel priser om man byter kat�r 
   */
   IF KALKHUV.KLOGID = KALKHUVBUF.KLOGID THEN DO:
      FOR EACH KALKEGNAPRISER WHERE KALKEGNAPRISER.KALKNR = KALKHUV.KALKNR  AND KALKEGNAPRISER.OMRADE = KALKHUV.OMRADE  NO-LOCK:
         CREATE KALKEGNAPRISERBUF.
         BUFFER-COPY KALKEGNAPRISER EXCEPT KALKEGNAPRISER.KALKNR TO KALKEGNAPRISERBUF.
         ASSIGN
         KALKEGNAPRISERBUF.KALKNR  = KALKHUVBUF.KALKNR  
         KALKEGNAPRISERBUF.OMRADE  = KALKHUVBUF.OMRADE. 
      END.
   END.
   ELSE DO:
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKHUVBUF.KLOGID AND KALKYLKATALOGSUB.AVSLUTAD = FALSE NO-LOCK:
         FOR EACH KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLPRISER.EGENPRISUPP = TRUE NO-LOCK:
            CREATE KALKEGNAPRISERBUF.
            BUFFER-COPY KALKYLPRISER  TO KALKEGNAPRISERBUF.
            ASSIGN
            KALKEGNAPRISERBUF.KALKNR  = KALKHUVBUF.KALKNR  
            KALKEGNAPRISERBUF.OMRADE  = KALKHUVBUF.OMRADE.
         END.         
      END. 
   END.
   
   FOR EACH KALKMTRL WHERE KALKMTRL.KALKNR = KALKHUV.KALKNR  AND KALKMTRL.OMRADE = KALKHUV.OMRADE  NO-LOCK:
      CREATE KALKMTRLBUF.
      BUFFER-COPY KALKMTRL EXCEPT KALKMTRL.KALKNR TO KALKMTRLBUF.
      ASSIGN
      KALKMTRLBUF.KALKNR  = KALKHUVBUF.KALKNR  
      KALKMTRLBUF.OMRADE  = KALKHUVBUF.OMRADE. 
   END.
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE AND KALKNUM.ARBKOD = "EGEN" NO-LOCK:
      FIND FIRST KALKNUMBUF  WHERE 
      KALKNUMBUF.KALKNR  = KALKHUVBUF.KALKNR AND  
      KALKNUMBUF.OMRADE  = KALKHUVBUF.OMRADE AND
      KALKNUMSUBBUF.NUM  = KALKNUM.NUM 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKNUMBUF THEN DO:
         CREATE KALKNUMBUF.
         BUFFER-COPY KALKNUM EXCEPT KALKNUM.KALKNR KALKNUM.OMRADE TO KALKNUMBUF .
         FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = NyKatIdvar NO-LOCK NO-ERROR.
         FIND FIRST KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID NO-LOCK NO-ERROR.
         ASSIGN
         KALKNUMBUF.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID
         KALKNUMBUF.KALKNR  = KALKHUVBUF.KALKNR  
         KALKNUMBUF.OMRADE  = KALKHUVBUF.OMRADE
         KALKNUMBUF.TYPKALK = KALKHUVBUF.TYPKALK.
         
         FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKHUV.KALKNR  AND KALKNUMSUB.OMRADE = KALKHUV.OMRADE AND KALKNUMSUB.NUM = KALKNUM.NUM NO-LOCK:
            FIND FIRST KALKNUMSUBBUF WHERE 
            KALKNUMSUBBUF.KALKNR  = KALKHUVBUF.KALKNR AND 
            KALKNUMSUBBUF.OMRADE  = KALKHUVBUF.OMRADE AND
            KALKNUMSUBBUF.NUM  = KALKNUMSUB.NUM AND 
            KALKNUMSUBBUF.NUMSUBID  = KALKNUMSUB.NUMSUBID 
            NO-LOCK NO-ERROR.
            IF NOT AVAILABLE KALKNUMSUBBUF THEN DO:
               CREATE KALKNUMSUBBUF.
               BUFFER-COPY KALKNUMSUB EXCEPT KALKNUMSUB.KALKNR TO KALKNUMSUBBUF.
               ASSIGN
               KALKNUMSUBBUF.KALKNR  = KALKHUVBUF.KALKNR  
               KALKNUMSUBBUF.OMRADE  = KALKHUVBUF.OMRADE.  
            END.
         END.
      END.   
   END.
   
END PROCEDURE.
/*BERKALKSKAP*/
/*skapa kalkyl av berkalk*/
PROCEDURE bercopycat_UI :
   
   DEFINE INPUT PARAMETER nykalknr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER KatIdvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nytyp AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER nyaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER nyadelnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER kbename AS CHARACTER NO-UNDO.
   DEFINE VARIABLE statusvar AS CHARACTER NO-UNDO.
   DO TRANSACTION:
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = nyaonr AND AONRTAB.DELNR = nyadelnr NO-LOCK NO-ERROR.
      FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR.
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = ANVANDARE.PERSONALKOD USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      IF NOT AVAILABLE PERSONALTAB THEN DO:
         FIND FIRST PERSONALTAB WHERE USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
      END.
      CREATE KALKHUVBUF.
      ASSIGN
      KALKHUVBUF.AKTIV = TRUE
      /*
      KALKHUVBUF.BENAMNING = AONRTAB.ORT
      */
      KALKHUVBUF.BENAMNING = kbename
      KALKHUVBUF.KALKANV = PERSONALTAB.PERSONALKOD
      KALKHUVBUF.ANVANDARE = ANVANDARE.ANVANDARE
      KALKHUVBUF.KALKNR = nykalknr
      KALKHUVBUF.OMRADE = omrvar
      KALKHUVBUF.KLOGID = KatIdvar
      KALKHUVBUF.TYPKALK = nytyp.
      CREATE KALKAONR.
      ASSIGN
      KALKAONR.KALKNR = KALKHUVBUF.KALKNR
      KALKAONR.OMRADE = KALKHUVBUF.OMRADE 
      KALKAONR.TYP = KALKHUVBUF.TYPKALK
      KALKAONR.AONR = nyaonr
      KALKAONR.DELNR = nyadelnr
      KALKAONR.PLANNR = ?
      KALKAONR.ARTAL = ?
      KALKAONR.AKTIV = TRUE.       
      RUN statusnivkoll_UI ( OUTPUT statusvar).
      KALKAONR.STATUSNIV = statusvar. 
   END.   
   
END PROCEDURE.

PROCEDURE slaihopcat_UI :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER slaihopkalknr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER slaihopomrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER matrisnr AS INTEGER NO-UNDO.
   DO TRANSACTION:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = slaihopkalknr AND KALKHUV.OMRADE = slaihopomrvar EXCLUSIVE-LOCK NO-ERROR.
      FIND FIRST KALKHUVBUF  WHERE KALKHUVBUF.KALKNR = KalkNrvar AND KALKHUVBUF.OMRADE = omrvar EXCLUSIVE-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN KALKHUVBUF.ANMARKNING = KALKHUVBUF.ANMARKNING + " " + KALKHUV.ANMARKNING.
   END.  
   FOR EACH KALKMTRL WHERE KALKMTRL.KALKNR = KALKHUV.KALKNR  AND KALKMTRL.OMRADE = KALKHUV.OMRADE  NO-LOCK:
      CREATE KALKMTRLBUF.
      BUFFER-COPY KALKMTRL EXCEPT KALKMTRL.KALKNR TO KALKMTRLBUF.
      ASSIGN 
      KALKMTRLBUF.MATRIS = matrisnr
      KALKMTRLBUF.KALKNR  = KALKHUVBUF.KALKNR  
      KALKMTRLBUF.OMRADE  = KALKHUVBUF.OMRADE. 
   END.
   
   DEFINE VARIABLE numint AS INTEGER NO-UNDO.
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE AND KALKNUM.ARBKOD = "EGEN" NO-LOCK:
      RUN sistanum_UI (OUTPUT numint).
      CREATE KALKNUMBUF.
      BUFFER-COPY KALKNUM EXCEPT KALKNUM.KALKNR TO KALKNUMBUF.
      ASSIGN
      KALKNUMBUF.MATRIS = matrisnr
      KALKNUMBUF.NUM = numint
      KALKNUMBUF.LOPNR = matrisnr * 100 + KALKNUM.LOPNR
      KALKNUMBUF.KALKNR  = KALKHUVBUF.KALKNR  
      KALKNUMBUF.OMRADE  = KALKHUVBUF.OMRADE
      KALKNUMBUF.TYPKALK = KALKHUVBUF.TYPKALK.
      
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKHUV.KALKNR  AND KALKNUMSUB.OMRADE = KALKHUV.OMRADE AND KALKNUMSUB.NUM = KALKNUM.NUM NO-LOCK:
         CREATE KALKNUMSUBBUF.
         BUFFER-COPY KALKNUMSUB EXCEPT KALKNUMSUB.KALKNR TO KALKNUMSUBBUF.
         ASSIGN
         KALKNUMSUBBUF.NUM = numint
         KALKNUMSUBBUF.KALKNR  = KALKHUVBUF.KALKNR  
         KALKNUMSUBBUF.OMRADE  = KALKHUVBUF.OMRADE.   
      END.
   END.
   
   
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE NO-LOCK:
      IF KALKNUM.ARBKOD = "EGEN" THEN .
      ELSE DO:
         CREATE KalkylimportTT.
         BUFFER-COPY KALKNUM EXCEPT KALKNUM.KALKNR TO KalkylimportTT.
         ASSIGN
         KalkylimportTT.MATRIS = matrisnr
         KalkylimportTT.KALKNR  = KALKHUVBUF.KALKNR  
         KalkylimportTT.OMRADE  = KALKHUVBUF.OMRADE.
         KalkylimportTT.TTRECID = RECID(KalkylimportTT).
      END.
   END.
END PROCEDURE.

PROCEDURE jmfcat_UI :
   DEFINE INPUT  PARAMETER minusvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER jmfkalknr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER jmfomrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER matrisnr AS INTEGER NO-UNDO.
   DEFINE VARIABLE egenkodlopnr AS INTEGER NO-UNDO.
   IF minusvar = 1 THEN egenkodlopnr = 1.
   ELSE egenkodlopnr = 2.
   DO TRANSACTION:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = jmfkalknr AND KALKHUV.OMRADE = jmfomrvar EXCLUSIVE-LOCK NO-ERROR.
      FIND FIRST KALKHUVBUF  WHERE KALKHUVBUF.KALKNR = KalkNrvar AND KALKHUVBUF.OMRADE = omrvar EXCLUSIVE-LOCK NO-ERROR.
      KALKHUVBUF.ANMARKNING = KALKHUVBUF.ANMARKNING + " " + KALKHUV.ANMARKNING.
   END.  
   
   FOR EACH KALKMTRL WHERE KALKMTRL.KALKNR = KALKHUV.KALKNR  AND KALKMTRL.OMRADE = KALKHUV.OMRADE  NO-LOCK:
      
      FIND FIRST KALKMTRLBUF WHERE  KALKMTRLBUF.KALKNR = KALKHUVBUF.KALKNR  AND KALKMTRLBUF.OMRADE = KALKHUVBUF.OMRADE AND  
      KALKMTRLBUF.Enr = KALKMTRL.Enr EXCLUSIVE-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKMTRLBUF THEN DO:
         CREATE KALKMTRLBUF.
         BUFFER-COPY KALKMTRL EXCEPT KALKMTRL.KALKNR TO KALKMTRLBUF.
         ASSIGN 
         KALKMTRLBUF.MATRIS = matrisnr
         KALKMTRLBUF.KALKNR  = KALKHUVBUF.KALKNR  
         KALKMTRLBUF.OMRADE  = KALKHUVBUF.OMRADE.
         KALKMTRLBUF.BERKVANT =  KALKMTRL.BERKVANT * minusvar.
      END.
      ELSE KALKMTRLBUF.BERKVANT = KALKMTRLBUF.BERKVANT + KALKMTRL.BERKVANT * minusvar.
   END.
   
   DEFINE VARIABLE numint AS INTEGER NO-UNDO.
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE AND KALKNUM.ARBKOD = "EGEN" NO-LOCK:
      RUN sistanum_UI (OUTPUT numint).
      CREATE KALKNUMBUF.
      BUFFER-COPY KALKNUM EXCEPT KALKNUM.KALKNR TO KALKNUMBUF.
      ASSIGN
      KALKNUMBUF.MATRIS = matrisnr
      KALKNUMBUF.NUM = numint
      KALKNUMBUF.LOPNR = egenkodlopnr * 100 + KALKNUM.LOPNR
      KALKNUMBUF.KALKNR  = KALKHUVBUF.KALKNR  
      KALKNUMBUF.OMRADE  = KALKHUVBUF.OMRADE
      KALKNUMBUF.TYPKALK = KALKHUVBUF.TYPKALK.
      KALKNUMBUF.ANTAL = KALKNUM.ANTAL * minusvar.
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKHUV.KALKNR  AND KALKNUMSUB.OMRADE = KALKHUV.OMRADE AND KALKNUMSUB.NUM = KALKNUM.NUM NO-LOCK:
         CREATE KALKNUMSUBBUF.
         BUFFER-COPY KALKNUMSUB EXCEPT KALKNUMSUB.KALKNR TO KALKNUMSUBBUF.
         ASSIGN
         KALKNUMSUBBUF.NUM = numint
         KALKNUMSUBBUF.KALKNR  = KALKHUVBUF.KALKNR  
         KALKNUMSUBBUF.OMRADE  = KALKHUVBUF.OMRADE.   
      END.
   END.
   
   
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE NO-LOCK:
      IF KALKNUM.ARBKOD = "EGEN" THEN .
      ELSE DO:
         FIND FIRST KalkylimportTT WHERE KalkylimportTT.KALKNR  = KALKHUVBUF.KALKNR AND KalkylimportTT.OMRADE = KALKHUVBUF.OMRADE AND 
         /*KalkylimportTT.MATRIS = matrisnr AND*/ KalkylimportTT.ARBKOD = KALKNUM.ARBKOD AND KalkylimportTT.LOPNR = KALKNUM.LOPNR 
         
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE KalkylimportTT THEN DO:
            CREATE KalkylimportTT.
            BUFFER-COPY KALKNUM EXCEPT KALKNUM.KALKNR TO KalkylimportTT.
            ASSIGN
            KalkylimportTT.MATRIS = matrisnr
            KalkylimportTT.KALKNR  = KALKHUVBUF.KALKNR  
            KalkylimportTT.OMRADE  = KALKHUVBUF.OMRADE.
            KalkylimportTT.TTRECID = RECID(KalkylimportTT).
            KalkylimportTT.ANTAL = KALKNUM.ANTAL * minusvar.
         END.
         ELSE KalkylimportTT.ANTAL = KalkylimportTT.ANTAL + KALKNUM.ANTAL * minusvar.
         
      END.
   END.
END PROCEDURE.



/*kopiera frekvenskat*/
PROCEDURE KopieraFrekKataloger :
   DEFINE INPUT PARAMETER orgsubid AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER  nysubid AS INTEGER NO-UNDO.
   DEFINE VARIABLE AA AS INTEGER NO-UNDO.
   FOR EACH FREKVENSKATALOG WHERE FREKVENSKATALOG.KLOGSUBID = orgsubid NO-LOCK:
      DO TRANSACTION:
         CREATE FREKVENSKATALOGBUF.
         BUFFER-COPY FREKVENSKATALOG EXCEPT FREKVENSKATALOG.KLOGSUBID TO FREKVENSKATALOGBUF.
         ASSIGN  
         FREKVENSKATALOGBUF.KLOGSUBID  = nysubid.      
      END.   
   END.
    RELEASE FREKVENSKATALOGBUF NO-ERROR.
END PROCEDURE.
/*KOPIER KATARB + KATLOP*/
PROCEDURE KopieraSubKatalogerKoder :
   DEFINE INPUT PARAMETER orgsubid AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER  nysubid AS INTEGER NO-UNDO.
   FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = orgsubid NO-LOCK:
      DO TRANSACTION:
         CREATE KALKYLARBKODERBUF.
         BUFFER-COPY KALKYLARBKODER EXCEPT KALKYLARBKODER.KLOGSUBID TO KALKYLARBKODERBUF.
         ASSIGN 
         KALKYLARBKODERBUF.KLOGSUBID = nysubid.  
      END.       
   END.
   FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = orgsubid NO-LOCK:
      DO TRANSACTION:
         CREATE KALKYLLOPPOSTERBUF.
         BUFFER-COPY KALKYLLOPPOSTER EXCEPT KALKYLLOPPOSTER.KLOGSUBID TO KALKYLLOPPOSTERBUF.
         ASSIGN 
         KALKYLLOPPOSTERBUF.KLOGSUBID = nysubid.
      END. 
   END.
   /*L�PSUB*/
   FOR EACH KALKYLLOPSUB WHERE KALKYLLOPSUB.KLOGSUBID = orgsubid NO-LOCK:
      DO TRANSACTION:
         CREATE KALKYLLOPSUBBUF.
         BUFFER-COPY KALKYLLOPSUB EXCEPT KALKYLLOPSUB.KLOGSUBID TO KALKYLLOPSUBBUF.
         ASSIGN 
         KALKYLLOPSUBBUF.KLOGSUBID = nysubid.
      END.              
   END. 
   RELEASE KALKYLARBKODERBUF NO-ERROR.
   RELEASE KALKYLLOPPOSTERBUF NO-ERROR.
   RELEASE KALKYLLOPSUBBUF NO-ERROR.
END PROCEDURE.


/*F�R ATT ARBETAVIDARE SKA BLI R�TT*/
PROCEDURE LaddaTypBen_UI:
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valdfasttemp.  
   FOR EACH valdfasttemp:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = valdfasttemp.KALKNR NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN DO:
         BUFFER-COPY KALKHUV TO valdfasttemp.
         valdfasttemp.TYPCHAR = STRING(valdfasttemp.TYP).
         IF valdfasttemp.TYP = 5 THEN valdfasttemp.TYPCHAR = "Sam".
         ELSE IF valdfasttemp.TYP = 7 THEN valdfasttemp.TYPCHAR = "N�t".
         ELSE IF valdfasttemp.TYP = 6 THEN valdfasttemp.TYPCHAR = "Fri".
      END.
      ELSE DELETE valdfasttemp.
      
      IF AVAILABLE valdfasttemp THEN DO:
         FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = valdfasttemp.KALKNR NO-LOCK NO-ERROR.
         IF AVAILABLE KALKAONR THEN BUFFER-COPY KALKAONR TO valdfasttemp.
      END.
   END.
END PROCEDURE.

/*FR�N AONRHUV KOPPLA OCH KOPIERA*/
PROCEDURE kopierakoppla_UI :
   DEFINE INPUT  PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyomrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER nyadelnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER aonrplanin AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR tidut.
   DEFINE VARIABLE nykalknr AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE tidut NO-ERROR. 
   aonrplan = aonrplanin.
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar AND KALKHUV.OMRADE = omrvar NO-LOCK NO-ERROR.
   RUN omradekoll_UI (INPUT nyomrvar, OUTPUT nykalknr).  
   IF nykalknr = ? THEN DO:
      CREATE tidut.
      tidut.UT =  "Det g�r inte att l�gga upp kalkyler p� detta " + LC(Guru.Konstanter:gomrk) + ". Nummerserie saknas eller �r fylld.".
      tidut.TTRECID = RECID(tidut).      
      RETURN.
   END.
   RUN copycat_UI (INPUT KalkNRvar, INPUT omrvar,INPUT KALKHUV.KLOGID,INPUT nykalknr,INPUT nyomrvar, INPUT KALKHUV.TYPKALK,INPUT nyaonr,INPUT nyadelnr).
   FOR EACH KALKNUM WHERE KALKNUM.KALKNR = KALKHUV.KALKNR  AND KALKNUM.OMRADE = KALKHUV.OMRADE AND KALKNUM.ARBKOD NE "EGEN" NO-LOCK:
      CREATE KALKNUMBUF.
      BUFFER-COPY KALKNUM EXCEPT KALKNUM.KALKNR TO KALKNUMBUF.
      ASSIGN
      KALKNUMBUF.KALKNR  = KALKHUVBUF.KALKNR  
      KALKNUMBUF.OMRADE  = KALKHUVBUF.OMRADE
      KALKNUMBUF.TYPKALK = KALKHUVBUF.TYPKALK.
      
      FOR EACH KALKNUMSUB WHERE KALKNUMSUB.KALKNR = KALKHUV.KALKNR  AND KALKNUMSUB.OMRADE = KALKHUV.OMRADE AND KALKNUMSUB.NUM = KALKNUM.NUM NO-LOCK:
         CREATE KALKNUMSUBBUF.
         BUFFER-COPY KALKNUMSUB EXCEPT KALKNUMSUB.KALKNR TO KALKNUMSUBBUF.
         ASSIGN
         KALKNUMSUBBUF.KALKNR  = KALKHUVBUF.KALKNR  
         KALKNUMSUBBUF.OMRADE  = KALKHUVBUF.OMRADE.   
      END.
   END.
   RUN copyrel_UI.   
END PROCEDURE.


PROCEDURE aonrhmtkalk_UI :
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER aonrplanin AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR gatill.
   EMPTY TEMP-TABLE gatill NO-ERROR. 
   IF arendekalkin = "�RENDE" THEN DO:
      OPEN QUERY fq FOR EACH KALKAONR WHERE KALKAONR.AONR = aonrvar AND
      KALKAONR.DELNR = delnrvar AND KALKAONR.STATUSNIV = arendekalkin NO-LOCK.      
   END.
   ELSE DO:
      IF aonrplanin = TRUE THEN DO:
         OPEN QUERY fq FOR EACH KALKAONR WHERE KALKAONR.AONR = aonrvar AND
         KALKAONR.DELNR = delnrvar AND KALKAONR.STATUSNIV NE "�RENDE" NO-LOCK.
      END.
      ELSE DO:
         OPEN QUERY fq FOR EACH KALKAONR WHERE KALKAONR.PLANNR = aonrvar AND
         KALKAONR.ARTAL = delnrvar NO-LOCK.
      END. 
   END.         
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(KALKAONR):
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KALKAONR.KALKNR AND KALKHUV.OMRADE = KALKAONR.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN DO:
         IF arendekalkin = "�RENDE" THEN DO:
            CREATE gatill.
            ASSIGN
            gatill.TYP = KALKAONR.TYP
            gatill.TYPCHAR = arendekalkin
            gatill.F1 = "�rende"
            gatill.F2 = STRING(KALKAONR.KALKNR)         
            gatill.F3 = KALKHUV.BENAMNING
            gatill.STATUSNIV = KALKAONR.STATUSNIV.
         END.
         ELSE DO:
            CREATE gatill.
            ASSIGN
            gatill.TYPCHAR = "KAL"
            gatill.TYP = KALKAONR.TYP
            gatill.F1 = "Kalkyl typ " + STRING(KALKAONR.TYP) 
            gatill.F2 = STRING(KALKAONR.KALKNR)         
            gatill.F3 = KALKHUV.BENAMNING
            gatill.STATUSNIV = KALKAONR.STATUSNIV.        
            IF KALKHUV.TYP = 5 THEN gatill.F1 = "Kalkyl Sam B/F"  .
            IF KALKHUV.TYP = 6 THEN gatill.F1 = "Kalkyl typ fri"  .
            IF KALKHUV.TYP = 7 THEN gatill.F1 = "Kalkyl N�treg N1"  .
         END.   
      END.
      GET NEXT fq NO-LOCK.
   END. 
END PROCEDURE.

PROCEDURE urvalbort_UI :
   EMPTY TEMP-TABLE  eutvaldfasttemp NO-ERROR. 
END PROCEDURE.
PROCEDURE GetKalkUpp_UI:
   DEFINE INPUT PARAMETER kalknrvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER typvar AS INTEGER NO-UNDO.
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = kalknrvar NO-LOCK NO-ERROR.
   IF AVAILABLE KALKHUV THEN DO:
      typvar = KALKHUV.UTYP.   
   END.
END PROCEDURE.
/*aktiv/inaktiv*/
PROCEDURE kalkaktiv_UI :
   DEFINE INPUT  PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER aktivvar AS LOGICAL NO-UNDO.
   DO TRANSACTION:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar EXCLUSIVE-LOCK NO-ERROR.
      FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = KalkNrvar EXCLUSIVE-LOCK NO-ERROR.
      ASSIGN 
      KALKAONR.AKTIV = aktivvar.
      IF AVAILABLE KALKHUV THEN DO:
         KALKHUV.AKTIV = aktivvar.
      END.   
   END.
   RELEASE KALKHUV NO-ERROR.  
END PROCEDURE.
/*s�k p� nr + omr*/
PROCEDURE sokkalkylomr_UI :
   DEFINE INPUT  PARAMETER kalknrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.   
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = kalknrvar AND KALKAONR.OMRADE = omrvar NO-LOCK NO-ERROR.
   IF AVAILABLE KALKAONR THEN DO:
      CREATE eutvaldfasttemp. 
      RUN kalktyp_UI(INPUT 1).      
   END.
   RUN aonrsekkoll_UI (INPUT 1).
END PROCEDURE.


/*s�k p� nr */
PROCEDURE sokkalkyl_UI :
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER kalknrvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   IF arendekalkin = "�RENDE" THEN DO:
      FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = kalknrvar AND KALKAONR.STATUSNIV = arendekalkin NO-LOCK NO-ERROR.
   END.
   ELSE DO:
      FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = kalknrvar AND KALKAONR.STATUSNIV NE "�RENDE" NO-LOCK NO-ERROR.
   END.      
   IF AVAILABLE KALKAONR THEN DO: 
      CREATE eutvaldfasttemp.
      RUN kalktyp_UI(INPUT 1).      
   END.
   RUN aonrsekkoll_UI (INPUT 1).   
END PROCEDURE.
PROCEDURE StartSlaIihopUF_UI :
   DEFINE INPUT PARAMETER kalkaonrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   FOR EACH KALKAONR WHERE KALKAONR.AONR = kalkaonrvar AND KALKAONR.STATUSNIV = "UF":
      CREATE eutvaldfasttemp.
      BUFFER-COPY KALKAONR TO eutvaldfasttemp.
      eutvaldfasttemp.TYP = KALKAONR.TYP.
      eutvaldfasttemp.TYPCHAR = STRING(KALKAONR.TYP).
      IF eutvaldfasttemp.DELNR = 0 THEN DO:
         eutvaldfasttemp.KALKNR = ?.
      END.   
      FIND FIRST KALKHUV  WHERE KALKHUV.KALKNR = KALKAONR.KALKNR AND KALKHUV.OMRADE = KALKAONR.OMRADE  NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN DO:
         FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
         IF AVAILABLE KALKYLKATALOG THEN DO: 
            eutvaldfasttemp.VIKATAR = KALKYLKATALOG.VISARTAL.
            eutvaldfasttemp.KATAR  =  KALKYLKATALOG.VISARTAL. 
         END.
      END. 
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = kalkaonrvar AND AONRTAB.DELNR = eutvaldfasttemp.DELNR NO-LOCK NO-ERROR.
      eutvaldfasttemp.BENAMNING = AONRTAB.ORT.  
   END. 
    
   FIND FIRST eutvaldfasttemp WHERE eutvaldfasttemp.AONR = kalkaonrvar AND eutvaldfasttemp.DELNR = 0 NO-LOCK NO-ERROR.
   IF NOT AVAILABLE eutvaldfasttemp THEN DO:
      CREATE eutvaldfasttemp.
      eutvaldfasttemp.AONR = kalkaonrvar.
   END. 
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = kalkaonrvar AND AONRTAB.DELNR = 0 NO-LOCK NO-ERROR.
   eutvaldfasttemp.BENAMNING = AONRTAB.ORT.
END PROCEDURE.
PROCEDURE Kalkvisakoll_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   DEFINE OUTPUT PARAMETER felmedd AS CHARACTER NO-UNDO.
   DEFINE VARIABLE typvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE klogvar AS INTEGER NO-UNDO.
   FOR EACH eutvaldfasttemp WHERE NO-LOCK:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = eutvaldfasttemp.KALKNR AND KALKHUV.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN DO:
         IF typvar = 0 THEN DO:
            ASSIGN 
            typvar = KALKHUV.TYPKALK
            klogvar = KALKHUV.KLOGID.
         END.
         IF typvar = KALKHUV.TYPKALK THEN.
         ELSE DO:
            felmedd = "Du kan inte blanda olika kalkyltyper!".
            RETURN.
         END.       
         IF klogvar = KALKHUV.KLOGID THEN.
         ELSE DO:
            felmedd = "Kalkyler som har olika kataloger kan inte visas tillsammans!".
            RETURN.
         END.    
      END.   
   END.
   RUN aonrsekkoll_UI (INPUT 1).
END PROCEDURE.
/*FAVORITER*/
PROCEDURE Kalkfavo_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   FOR EACH eutvaldfasttemp NO-LOCK:
      FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = eutvaldfasttemp.KALKNR AND KALKAONR.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE KALKAONR THEN RUN kalktyp_UI(INPUT 1). 
      ELSE DELETE eutvaldfasttemp.
   END.
   RUN aonrsekkoll_UI (INPUT 1).
END PROCEDURE.
/*s�k p� aonr */
PROCEDURE sokaonrkalkyl_UI :
   
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER allavar AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER skapatom AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   IF arendekalkin = "�RENDE" THEN DO:
      IF allavar = FALSE THEN DO:
         FOR EACH KALKAONR WHERE KALKAONR.AONR = aonrvar AND
         KALKAONR.DELNR = delnrvar AND KALKAONR.STATUSNIV = arendekalkin NO-LOCK:
            CREATE eutvaldfasttemp.
            RUN kalktyp_UI(INPUT 1).
         END.
         IF skapatom = TRUE THEN DO:
            FIND FIRST eutvaldfasttemp WHERE eutvaldfasttemp.AONR = aonrvar AND eutvaldfasttemp.DELNR = delnrvar NO-LOCK NO-ERROR.
            IF NOT AVAILABLE eutvaldfasttemp THEN DO:
               RUN saknarkalk_UI (INPUT arendekalkin,INPUT aonrvar,INPUT delnrvar).          
            END.            
         END.   
      END.
      ELSE DO:
         FOR EACH KALKAONR WHERE KALKAONR.AONR = aonrvar AND KALKAONR.STATUSNIV = arendekalkin NO-LOCK:
            CREATE eutvaldfasttemp.
            RUN kalktyp_UI(INPUT 1).
         END.
      END.
   END.
   ELSE DO:
      IF allavar = FALSE THEN DO:
         FOR EACH KALKAONR WHERE KALKAONR.AONR = aonrvar AND
         KALKAONR.DELNR = delnrvar AND KALKAONR.STATUSNIV NE "�RENDE" NO-LOCK:
            CREATE eutvaldfasttemp.
            RUN kalktyp_UI(INPUT 1).
         END.
         IF skapatom = TRUE THEN DO:
            FIND FIRST eutvaldfasttemp WHERE eutvaldfasttemp.AONR = aonrvar AND eutvaldfasttemp.DELNR = delnrvar NO-LOCK NO-ERROR.
            IF NOT AVAILABLE eutvaldfasttemp THEN DO:
               RUN saknarkalk_UI (INPUT arendekalkin,INPUT aonrvar,INPUT delnrvar).          
            END.
         END.   
      END.
      ELSE DO:
         FOR EACH KALKAONR WHERE KALKAONR.AONR = aonrvar AND KALKAONR.STATUSNIV NE "�RENDE" NO-LOCK:
            CREATE eutvaldfasttemp.
            RUN kalktyp_UI(INPUT 1).
         END.
      END.
   END.      
   RUN aonrsekkoll_UI (INPUT 1).
   
END PROCEDURE.
PROCEDURE saknarkalk_UI :
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   /*ej �rende*/
   IF arendekalkin = "�RENDE" THEN RETURN.
   FIND FIRST AONRTAB WHERE AONRTAB.AONR = aonrvar AND AONRTAB.DELNR = delnrvar 
   NO-LOCK NO-ERROR.
   CREATE eutvaldfasttemp.
   ASSIGN
   eutvaldfasttemp.AONR = aonrvar 
   eutvaldfasttemp.DELNR = delnrvar
   eutvaldfasttemp.AKTIV = TRUE   
   eutvaldfasttemp.ANVANDARE = ?
   
   eutvaldfasttemp.BENAMNING = AONRTAB.ORT
   eutvaldfasttemp.BESTID   = AONRTAB.BESTID
   eutvaldfasttemp.KALKANV = ? 
   eutvaldfasttemp.KALKNR = ?  
   eutvaldfasttemp.KATAR  = ?  
   eutvaldfasttemp.PLANNR  = ? 
   eutvaldfasttemp.ARTAL = ?   
   eutvaldfasttemp.OMRADE  = AONRTAB.OMRADE 
   eutvaldfasttemp.TYPCHAR = ?
   eutvaldfasttemp.TYP   = ?.   
   /* KALK�R*/
   eutvaldfasttemp.VIKATAR = eutvaldfasttemp.KATAR.         
   IF eutvaldfasttemp.VIKATAR < 1900  THEN eutvaldfasttemp.VIKATAR = 0.
   IF arendekalkin = "�RENDE" THEN eutvaldfasttemp.STATUSNIV = arendekalkin.    
END PROCEDURE.

PROCEDURE saknarplankalk_UI :
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   FIND FIRST PLANNRTAB WHERE PLANNRTAB.PLANNR = aonrvar AND PLANNRTAB.ARTAL = delnrvar
   NO-LOCK NO-ERROR.
   CREATE eutvaldfasttemp.
   ASSIGN
   eutvaldfasttemp.AONR = ? 
   eutvaldfasttemp.DELNR = ?
   eutvaldfasttemp.AKTIV = TRUE   
   eutvaldfasttemp.ANVANDARE = ?
   eutvaldfasttemp.ARTAL = PLANNRTAB.ARTAL   
   eutvaldfasttemp.BENAMNING = PLANNRTAB.ORT
   eutvaldfasttemp.BESTID   = PLANNRTAB.BESTID
   eutvaldfasttemp.KALKANV = ? 
   eutvaldfasttemp.KALKNR = ?  
   eutvaldfasttemp.KATAR  = ?  
   eutvaldfasttemp.OMRADE  = PLANNRTAB.OMRADE 
   eutvaldfasttemp.PLANNR  = PLANNRTAB.PLANNR
   eutvaldfasttemp.TYPCHAR = ?
   eutvaldfasttemp.TYP   = ?.   
   /*
   KALK�R*/
   eutvaldfasttemp.VIKATAR = eutvaldfasttemp.KATAR.         
   IF eutvaldfasttemp.VIKATAR < 1900  THEN eutvaldfasttemp.VIKATAR = 0.
      
END PROCEDURE.
/*s�k p� PLANNR */
PROCEDURE sokplannrkalkyl_UI :
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER allavar AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER skapatom AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   IF allavar = FALSE THEN DO:
      FOR EACH KALKAONR WHERE KALKAONR.PLANNR = aonrvar AND
      KALKAONR.ARTAL = delnrvar NO-LOCK:
         CREATE eutvaldfasttemp.
         RUN kalktyp_UI(INPUT 1).
      END.
      IF skapatom = TRUE THEN DO:
         FIND FIRST eutvaldfasttemp WHERE eutvaldfasttemp.PLANNR = aonrvar AND eutvaldfasttemp.ARTAL = delnrvar NO-LOCK NO-ERROR.
         IF NOT AVAILABLE eutvaldfasttemp THEN DO:
            RUN saknarplankalk_UI (INPUT aonrvar,INPUT delnrvar).          
         END.
      END.  
   END.
   ELSE DO:
      FOR EACH KALKAONR WHERE KALKAONR.PLANNR = aonrvar NO-LOCK:
         CREATE eutvaldfasttemp.
         RUN kalktyp_UI(INPUT 1).
      END.
   END.
   RUN aonrsekkoll_UI (INPUT 1).
   
END PROCEDURE.
/*s�k p� urval */
PROCEDURE urvalkalkyl_UI :
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR uppkalktemp.
   DEFINE INPUT PARAMETER TABLE FOR uppavdjud.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   RUN skapurval_UI (INPUT arendekalkin).
   RUN aonrsekkoll_UI (INPUT 1).
END PROCEDURE.  

PROCEDURE skapurval_UI :
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qH AS HANDLE NO-UNDO.
   DEFINE VARIABLE orgtabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE tttabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE uppkalktemph AS HANDLE NO-UNDO.
   DEFINE VARIABLE uppavdjudh AS HANDLE NO-UNDO.
   ASSIGN tttabh = TEMP-TABLE eutvaldfasttemp:DEFAULT-BUFFER-HANDLE.
   CREATE BUFFER orgtabh FOR TABLE "KALKAONR" IN WIDGET-POOL "KalkDynTable".
     
   ASSIGN 
   kommandoquery = " ".
   FIND FIRST uppkalktemp NO-ERROR.
   IF uppkalktemp.AKIN = 1 THEN DO:
      kommandoquery = "KALKAONR.AKTIV = TRUE ".      
   END.
   ELSE IF uppkalktemp.AKIN = 2 THEN DO:
      kommandoquery = "KALKAONR.AKTIV = FALSE ".      
   END.
   ELSE IF uppkalktemp.AKIN = 3 THEN DO:
      kommandoquery = " ".      
   END.
   IF uppkalktemp.OMRADE = ? THEN.
   ELSE IF uppkalktemp.OMRADE NE "ALLA" THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " KALKAONR.OMRADE = " +  "'" + uppkalktemp.OMRADE + "'".
   END.
   IF uppkalktemp.TYP NE 0 THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " KALKAONR.TYP = " + STRING(uppkalktemp.TYP).
   END.
   IF uppkalktemp.AONR = TRUE THEN DO: 
      RUN and_UI.
      kommandoquery = kommandoquery + " KALKAONR.AONR = ?".
   END.
   IF arendekalkin = "�RENDE" THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " KALKAONR.STATUSNIV = '" + arendekalkin + "'".     
   END.   
   ELSE DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " KALKAONR.STATUSNIV NE '�RENDE'".
   END.   
   kommandoquery = "FOR EACH " + "KALKAONR" + " WHERE " + kommandoquery + " NO-LOCK".
   
   RUN CreateCustomQuery(INPUT orgtabh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      tttabh:BUFFER-CREATE().
      tttabh:BUFFER-COPY(orgtabh).         
      qH:GET-NEXT().
   END.
   RUN CloseCustomQuery(INPUT qH).
   
   FOR EACH eutvaldfasttemp WHERE NO-LOCK:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = eutvaldfasttemp.KALKNR AND KALKHUV.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE KALKHUV THEN RUN kalktyp_UI(INPUT 2).
   END.
   
   uppkalktemph = TEMP-TABLE uppkalktemp:DEFAULT-BUFFER-HANDLE.
   uppavdjudh = TEMP-TABLE uppavdjud:DEFAULT-BUFFER-HANDLE.
   uppkalktemph:FIND-FIRST() NO-ERROR.
   uppavdjudh:FIND-FIRST() NO-ERROR.
   kommandoquery = "FOR EACH " + "eutvaldfasttemp".
   RUN CreateCustomQuery(INPUT tttabh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      IF tttabh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = ? THEN tttabh:BUFFER-DELETE(). 
      IF uppkalktemph:AVAILABLE THEN DO:
         IF tttabh:AVAILABLE THEN DO:
            IF uppkalktemph:BUFFER-FIELD("BESTID"):BUFFER-VALUE  = "ALLA" THEN.
            ELSE DO:
               IF uppkalktemph:BUFFER-FIELD("BESTID"):BUFFER-VALUE NE tttabh:BUFFER-FIELD("BESTID"):BUFFER-VALUE THEN tttabh:BUFFER-DELETE().            
            END.
         END.   
         IF tttabh:AVAILABLE THEN DO: 
            IF uppkalktemph:BUFFER-FIELD("KALKANSVARIG"):BUFFER-VALUE  = "ALLA" THEN.
            ELSE DO:
               IF uppkalktemph:BUFFER-FIELD("KALKANSVARIG"):BUFFER-VALUE NE tttabh:BUFFER-FIELD("KALKANV"):BUFFER-VALUE THEN tttabh:BUFFER-DELETE().            
            END.
         END.
         IF tttabh:AVAILABLE THEN DO:
            
            IF uppkalktemph:BUFFER-FIELD("UTFARD"):BUFFER-VALUE  = "ALLA" THEN.
            ELSE DO:
               IF uppkalktemph:BUFFER-FIELD("UTFARD"):BUFFER-VALUE NE tttabh:BUFFER-FIELD("ANVANDARE"):BUFFER-VALUE THEN tttabh:BUFFER-DELETE().            
            END.            
         END.
      END.
      IF tttabh:AVAILABLE THEN DO:    
         IF uppavdjudh:AVAILABLE THEN DO:
            IF uppkalktemph:BUFFER-FIELD("OMRADE"):BUFFER-VALUE  = "ALLA" THEN DO:
               IF uppavdjudh:BUFFER-FIELD("AVDNR"):BUFFER-VALUE  = "ALLA" THEN DO:
                  IF uppavdjudh:BUFFER-FIELD("JUDID"):BUFFER-VALUE  = "ALLA" THEN DO:                               
                  END.
                  ELSE DO:      
                     FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = Eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
                     FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
                     IF AVAILABLE AVDELNING THEN DO:
                        IF AVDELNING.POSTANST = uppavdjud.JUDID THEN.
                        ELSE tttabh:BUFFER-DELETE().
                     END.                                           
                  END.   
               END.
               ELSE DO:
                  FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = Eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
                  IF OMRADETAB.AVDELNINGNR = INTEGER(uppavdjud.AVDNR) THEN.
                  ELSE tttabh:BUFFER-DELETE(). 
                  
               END.
            END.
         END.   
      END.   
      qH:GET-NEXT().
   END.
   RUN CloseCustomQuery(INPUT qH).
   DELETE OBJECT qH  NO-ERROR.
   DELETE OBJECT orgtabh NO-ERROR.
END PROCEDURE.

PROCEDURE kalktyp_UI :
   DEFINE INPUT  PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KALKAONR.KALKNR AND KALKHUV.OMRADE = KALKAONR.OMRADE NO-LOCK NO-ERROR.
      BUFFER-COPY KALKAONR TO eutvaldfasttemp.
   END.
       
   IF AVAILABLE KALKHUV THEN DO: 
      BUFFER-COPY KALKHUV TO eutvaldfasttemp.  
      IF KALKHUV.TYPKALK = 6 THEN eutvaldfasttemp.TYPCHAR = "Fri".
      ELSE IF KALKHUV.TYPKALK = 5 THEN eutvaldfasttemp.TYPCHAR = "Sam".   
      ELSE IF KALKHUV.TYPKALK = 7 THEN eutvaldfasttemp.TYPCHAR = "N�t".
      ELSE eutvaldfasttemp.TYPCHAR = STRING(eutvaldfasttemp.TYP).
   
      FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
      IF AVAILABLE KALKYLKATALOG THEN eutvaldfasttemp.VIKATAR = KALKYLKATALOG.VISARTAL.
      ELSE eutvaldfasttemp.VIKATAR = 0.
      /*
      IF SUBSTRING(KALKYLKATALOG.BENAMNING,1,4) = "EBR " THEN eutvaldfasttemp.VIKATAR = KALKYLKATALOG.VISARTAL.
      */  
      eutvaldfasttemp.KATAR = eutvaldfasttemp.VIKATAR.
   END.   
   IF eutvaldfasttemp.VIKATAR = ? OR eutvaldfasttemp.VIKATAR < 0 THEN DO:
      ASSIGN
      eutvaldfasttemp.VIKATAR = 0
      eutvaldfasttemp.KATAR = 0.
   END.     
END PROCEDURE.
/*F�R DU SE DENNA KALKYL*/
PROCEDURE aonrsekkoll_UI :
   DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
   IF Guru.Konstanter:varforetypchar[4] = "" AND Guru.Konstanter:varforetypval[18] = 0 THEN DO:
      RETURN.
   END.
   IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)   THEN RETURN.
   IF Guru.Konstanter:varforetypchar[4] = "ja" THEN DO:
      IF vad = 1 THEN DO:
         FOR EACH eutvaldfasttemp:
            FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = Guru.Konstanter:globanv AND 
            OFFERT.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
            IF NOT AVAILABLE OFFERT THEN DO:
               FIND FIRST OFFERT WHERE OFFERT.OFFNR = 1 AND OFFERT.OFFANV = Guru.Konstanter:globanv AND 
               OFFERT.AONR = eutvaldfasttemp.AONR AND OFFERT.DELNR = eutvaldfasttemp.DELNR
               NO-LOCK NO-ERROR.
               IF NOT AVAILABLE OFFERT THEN DO:
                  FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = eutvaldfasttemp.KALKNR AND KALKHUV.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
                  IF AVAILABLE KALKHUV THEN DO:
                     IF KALKHUV.ANVANDARE = Guru.Konstanter:globanv AND eutvaldfasttemp.AONR = ? THEN.
                     ELSE DELETE eutvaldfasttemp.  
                  END.   
                  ELSE DELETE eutvaldfasttemp.
               END. 
            END.
         END.
      END.
   END.
   IF Guru.Konstanter:varforetypval[18] = 1 THEN DO:
      IF vad = 1 THEN DO:
         FOR EACH eutvaldfasttemp:
            FIND FIRST omvtemp WHERE omvtemp.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
            IF AVAILABLE omvtemp THEN DO:
               FIND FIRST BOLAGSEK WHERE BOLAGSEK.OMRADE = omvtemp.JUDID AND BOLAGSEK.ANVANDARE = Guru.Konstanter:globanv NO-LOCK NO-ERROR. 
               IF NOT AVAILABLE BOLAGSEK THEN DELETE eutvaldfasttemp.               
            END.
            ELSE DO:
               IF eutvaldfasttemp.OMRADE NE "" THEN DELETE eutvaldfasttemp.  
            END.
         END.
      END.
   END.
END PROCEDURE.
PROCEDURE StatusImportKoll_UI :
   DEFINE INPUT PARAMETER kaonr AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER kdelnr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kplan AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER kartal AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER typvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER statusvar AS CHARACTER NO-UNDO.
   IF kaonr = ? AND  kplan = ? THEN DO:
      statusvar = "".
      RETURN.
   END.   
   EMPTY TEMP-TABLE kalkaonrTTalla NO-ERROR.
   IF kplan = ? THEN DO:
      OPEN QUERY fq FOR EACH KALKAONR WHERE KALKAONR.AONR = kaonr AND KALKAONR.DELNR = kdelnr NO-LOCK.      
   END.
   ELSE DO:
      OPEN QUERY fq FOR EACH KALKAONR WHERE KALKAONR.PLANNR = kplan AND KALKAONR.ARTAL = kartal NO-LOCK.
   END.         
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(KALKAONR):
      CREATE kalkaonrTTalla.
      BUFFER-COPY KALKAONR TO kalkaonrTTalla.
      GET NEXT fq NO-LOCK.
   END. 
   FIND FIRST kalkaonrTTalla WHERE kalkaonrTTalla.STATUSNIV = "UF" NO-LOCK NO-ERROR. 
   IF AVAILABLE kalkaonrTTalla THEN DO:
      IF kalkaonrTTalla.TYP NE typvar THEN DO:
         FIND FIRST kalkaonrTTalla WHERE kalkaobuff.STATUSNIV = "HUV" AND kalkaobuff.TYP = typvar NO-LOCK NO-ERROR. 
         IF AVAILABLE kalkaonrTTalla THEN DO:
            statusvar = "ALT".
            EMPTY TEMP-TABLE kalkaonrTTalla NO-ERROR.
         END.
         ELSE DO:
            statusvar= "HUV".
         END.
      END.
      ELSE statusvar = "ALT".
   END.
   ELSE DO:
      statusvar = "UF".
   END.
   EMPTY TEMP-TABLE kalkaonrTTalla NO-ERROR.
   
   
END PROCEDURE.
PROCEDURE startstatusnivkoll_UI :
   DEFINE INPUT  PARAMETER kalknrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER statusvar AS CHARACTER NO-UNDO.
   DO TRANSACTION:
      FIND FIRST KALKAONR WHERE KALKAONR.KALKNR = kalknrvar AND KALKAONR.OMRADE = omrvar NO-LOCK NO-ERROR.
      IF AVAILABLE KALKAONR THEN DO: 
         RUN statusnivkoll_UI (OUTPUT statusvar).
      END.  
      ELSE statusvar = "".
   END.
   RELEASE KALKAONR NO-ERROR.    
END PROCEDURE.
PROCEDURE statusnivkoll_UI:
   DEFINE OUTPUT PARAMETER statusvar AS CHARACTER NO-UNDO.
   IF KALKAONR.AONR = ?  AND KALKAONR.PLANNR = ? THEN RETURN.
   IF KALKAONR.STATUSNIV = "�RENDE" THEN RETURN.
   IF KALKAONR.AONR NE ? THEN DO:
      FIND FIRST kalkaobuff WHERE kalkaobuff.AONR = KALKAONR.AONR AND
      kalkaobuff.DELNR = KALKAONR.DELNR AND kalkaobuff.STATUSNIV = "UF"
      AND RECID(kalkaobuff) NE RECID(KALKAONR) NO-LOCK NO-ERROR. 
   END.
   ELSE IF KALKAONR.PLANNR NE ? THEN DO:
      FIND FIRST kalkaobuff WHERE kalkaobuff.PLANNR = KALKAONR.PLANNR AND
      kalkaobuff.ARTAL = KALKAONR.ARTAL AND kalkaobuff.STATUSNIV = "UF"
      AND RECID(kalkaobuff) NE RECID(KALKAONR) NO-LOCK NO-ERROR. 
   END.
   IF AVAILABLE kalkaobuff THEN DO:
      IF  kalkaobuff.TYP NE KALKAONR.TYP THEN DO:
         IF KALKAONR.AONR NE ? THEN DO:
            FIND FIRST kalkaobuff WHERE kalkaobuff.AONR = KALKAONR.AONR AND
            kalkaobuff.DELNR = KALKAONR.DELNR AND kalkaobuff.STATUSNIV = "HUV"
            AND kalkaobuff.TYP = KALKAONR.TYP AND RECID(kalkaobuff) NE RECID(KALKAONR)
            NO-LOCK NO-ERROR. 
         END.
         ELSE DO:
            FIND FIRST kalkaobuff WHERE kalkaobuff.PLANNR = KALKAONR.PLANNR AND
            kalkaobuff.ARTAL = KALKAONR.ARTAL AND kalkaobuff.STATUSNIV = "HUV"
            AND kalkaobuff.TYP = KALKAONR.TYP AND RECID(kalkaobuff) NE RECID(KALKAONR)
            NO-LOCK NO-ERROR. 
         END.
         IF AVAILABLE kalkaobuff THEN DO:
            statusvar = "ALT".
         END.
         ELSE DO:
            statusvar= "HUV".
         END.
      END.
      ELSE statusvar = "ALT".
   END.
   ELSE DO:
      statusvar = "UF".
   END.
END PROCEDURE.


/*============ALLA KALYLER DU JOBBAT MED I KALKYL.CLS==========*/

PROCEDURE anvkalkyl_UI :
   DEFINE INPUT  PARAMETER kalknrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   FIND FIRST anvkalkyltt WHERE anvkalkyltt.KALKNR = kalknrvar AND anvkalkyltt.OMRADE = omrvar NO-LOCK NO-ERROR.
   IF NOT AVAILABLE anvkalkyltt THEN DO:
      CREATE anvkalkyltt.
      ASSIGN
      anvkalkyltt.KALKNR = kalknrvar
      anvkalkyltt.OMRADE = omrvar.
   END.
END PROCEDURE.

PROCEDURE anvkalkylhmt_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR anvkalkyltt.
   
END PROCEDURE.

PROCEDURE anvkalkylbort_UI :
   EMPTY TEMP-TABLE  anvkalkyltt NO-ERROR. 
END PROCEDURE.

/*============H�MTA POSTER F�R UPPL�GG AV BERKALKYL===========*/
PROCEDURE KalkArtal_UI:
   DEFINE OUTPUT PARAMETER artalvar AS INTEGER NO-UNDO.
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   IF AVAILABLE KALKYLKATALOG THEN artalvar = KALKYLKATALOG.VISARTAL.
   ELSE artalvar = YEAR(TODAY).
   
END PROCEDURE.


PROCEDURE Ptemp_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.   
   DEFINE OUTPUT PARAMETER TABLE FOR ptemp.
   EMPTY TEMP-TABLE ptemp NO-ERROR. 
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
  
   FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
      IF rad_typ = 4 THEN DO:
         FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID NO-LOCK:
            IF KALKYLARBKODER.TYPKALK = 2 OR KALKYLARBKODER.TYPKALK = 3 THEN DO:
               CREATE ptemp.
               ASSIGN 
               ptemp.ARBKOD      = KALKYLARBKODER.ARBKOD
               ptemp.BENAMNING   = KALKYLARBKODER.BENAMNING
               ptemp.KATAR       = KALKYLKATALOG.VISARTAL
               ptemp.TYP         = KALKYLARBKODER.TYPKALK.
               {PKODREGION.I}
            END.           
         END.
      END.
      ELSE DO:
         FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLARBKODER.TYPKALK = rad_typ NO-LOCK:
            CREATE ptemp.
            ASSIGN 
            ptemp.ARBKOD      = KALKYLARBKODER.ARBKOD
            ptemp.BENAMNING   = KALKYLARBKODER.BENAMNING
            ptemp.KATAR       = KALKYLKATALOG.VISARTAL
            ptemp.TYP         = KALKYLARBKODER.TYPKALK.
            {PKODREGION.I}        
         END.
      END.   
   END.  
END PROCEDURE.

PROCEDURE Ptempklog_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER grundkloid AS INTEGER NO-UNDO.   
   DEFINE OUTPUT PARAMETER TABLE FOR ptemp.
   EMPTY TEMP-TABLE ptemp NO-ERROR. 
   IF grundkloid > 0 THEN FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = grundkloid USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   ELSE FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
  
   FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
      IF rad_typ = 4 THEN DO:
         FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID NO-LOCK:
            IF KALKYLARBKODER.TYPKALK = 2 OR KALKYLARBKODER.TYPKALK = 3 THEN DO:
               CREATE ptemp.
               ASSIGN 
               ptemp.ARBKOD      = KALKYLARBKODER.ARBKOD
               ptemp.BENAMNING   = KALKYLARBKODER.BENAMNING
               ptemp.KATAR       = KALKYLKATALOG.VISARTAL
               ptemp.TYP         = KALKYLARBKODER.TYPKALK.
               {PKODREGION.I}
            END.           
         END.
      END.
      ELSE DO:
         FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLARBKODER.TYPKALK = rad_typ NO-LOCK:
            CREATE ptemp.
            ASSIGN 
            ptemp.ARBKOD      = KALKYLARBKODER.ARBKOD
            ptemp.BENAMNING   = KALKYLARBKODER.BENAMNING
            ptemp.KATAR       = KALKYLKATALOG.VISARTAL
            ptemp.TYP         = KALKYLARBKODER.TYPKALK.
            {PKODREGION.I}        
         END.
      END.   
   END.  
END PROCEDURE.
PROCEDURE hmtebr_UI :
   DEFINE INPUT PARAMETER valaonr LIKE AONRTAB.AONR NO-UNDO.
   DEFINE INPUT PARAMETER valomrade LIKE AONRTAB.OMRADE NO-UNDO.  
   DEFINE OUTPUT PARAMETER TABLE FOR ebrpristemp.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkbefbtemp.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   EMPTY TEMP-TABLE kalkbefbtemp NO-ERROR.
   EMPTY TEMP-TABLE ebrpristemp NO-ERROR. 
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR KLG 1" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR KLG" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   END.
    IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   END.
   
   CREATE BUFFER kalkbefbtabbuffh FOR TABLE "KALKBEFB" IN WIDGET-POOL "KalkDynTable".
   kommandoquery = "FOR EACH " + kalkbefbtabbuffh:TABLE + " WHERE BERNR = " + STRING(valaonr) + " AND OMRADE = '" + valomrade + "'" + " NO-LOCK".
   RUN CreateCustomQuery(INPUT kalkbefbtabbuffh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST(NO-LOCK).
   DO WHILE qH:QUERY-OFF-END = FALSE:
       CREATE kalkbefbtemp.
        
       BUFFER kalkbefbtemp:HANDLE:BUFFER-COPY(kalkbefbtabbuffh).
      qH:GET-NEXT(NO-LOCK).
   END.   
   RUN CloseCustomQuery(INPUT qH).
   
   CREATE ebrpristemp.
   ebrpristemp.ARTAL = KALKYLKATALOG.VISARTAL.
   CREATE kalkbefbtemp.
   kalkbefbtemp.KATAR = KALKYLKATALOG.VISARTAL.
   
   
   FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
      FOR EACH KALKYLPRISER WHERE KALKYLPRISER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLPRISER.EGENPRISUPP = TRUE NO-LOCK:
          IF KALKYLPRISER.SOKBENAMNING = "MONT�R" THEN ebrpristemp.MONT = KALKYLPRISER.PRIS.
          IF KALKYLPRISER.SOKBENAMNING = "MASKIN1" THEN ebrpristemp.MASK1 = KALKYLPRISER.PRIS.
          IF KALKYLPRISER.SOKBENAMNING = "MASKIN2" THEN ebrpristemp.MASK2 = KALKYLPRISER.PRIS.
          IF KALKYLPRISER.SOKBENAMNING = "MASKIN3" THEN ebrpristemp.MASK3 = KALKYLPRISER.PRIS.
          
          IF KALKYLPRISER.SOKBENAMNING = "BEREDARE" THEN DO:
             kalkbefbtemp.PRIS1 = KALKYLPRISER.PRIS.
             kalkbefbtemp.BEF1 = KALKYLPRISER.BENAMNING.
             kalkbefbtemp.EBR1 = 1.
          END.   
          IF KALKYLPRISER.SOKBENAMNING = "MONT�R" THEN DO:
             kalkbefbtemp.PRIS2 = KALKYLPRISER.PRIS.
             kalkbefbtemp.BEF2 = KALKYLPRISER.BENAMNING.
             kalkbefbtemp.EBR2 = 1.
          END. 
          IF KALKYLPRISER.SOKBENAMNING = "MASKIN1" THEN DO:
             kalkbefbtemp.PRIS3 = KALKYLPRISER.PRIS.
             kalkbefbtemp.BEF3 = KALKYLPRISER.BENAMNING.
             kalkbefbtemp.EBR3 = 2.
          END. 
          IF KALKYLPRISER.SOKBENAMNING = "MASKIN2" THEN DO:
             kalkbefbtemp.PRIS4 = KALKYLPRISER.PRIS.
             kalkbefbtemp.BEF4 = KALKYLPRISER.BENAMNING.
             kalkbefbtemp.EBR4 = 3.
          END. 
          IF KALKYLPRISER.SOKBENAMNING = "MASKIN3" THEN DO:
             kalkbefbtemp.PRIS7 = KALKYLPRISER.PRIS.
             kalkbefbtemp.BEF7 = KALKYLPRISER.BENAMNING.
             kalkbefbtemp.EBR7 = 4.
          END. 
      END.
   END. 
   DELETE OBJECT qH  NO-ERROR.
   DELETE OBJECT kalkbefbtabbuffh  NO-ERROR.
   
END PROCEDURE.
/*P2 L�Pkoder f�r beredning*/
PROCEDURE Loptempn_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR loptemp.
   EMPTY TEMP-TABLE loptemp NO-ERROR. 
   DEBUGGER:SET-BREAK().
 
   IF rad_typ = 4 THEN DO:
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
         FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID NO-LOCK:
            IF KALKYLLOPPOSTER.TYPKALK = 2 OR KALKYLLOPPOSTER.TYPKALK = 3 THEN DO:
               CREATE loptemp.
               ASSIGN 
               loptemp.ARBKOD    = KALKYLLOPPOSTER.ARBKOD
               loptemp.LOPNR      = KALKYLLOPPOSTER.LOPNR
               loptemp.BENAMNING  = REPLACE(KALKYLLOPPOSTER.BENAMNING,CHR(10)," ")
               loptemp.ENHET      = KALKYLLOPPOSTER.ENHET
               loptemp.KATAR      = KALKYLKATALOG.VISARTAL
               loptemp.TYP        = KALKYLLOPPOSTER.TYPKALK.
               SUBSTRING(loptemp.BENAMNING,60) = KALKYLLOPPOSTER.KOMMENTAR.                   
            END.   
         END.
      END.
   END.
   ELSE DO:   
      FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
         FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLLOPPOSTER.TYPKALK = rad_typ NO-LOCK:
            CREATE loptemp.
            ASSIGN 
            loptemp.ARBKOD    = KALKYLLOPPOSTER.ARBKOD
            loptemp.LOPNR      = KALKYLLOPPOSTER.LOPNR            
            loptemp.BENAMNING  = REPLACE(KALKYLLOPPOSTER.BENAMNING,CHR(10)," ")
            loptemp.ENHET      = KALKYLLOPPOSTER.ENHET
            loptemp.KATAR      = KALKYLKATALOG.VISARTAL
            loptemp.TYP        = KALKYLLOPPOSTER.TYPKALK.
            IF loptemp.ARBKOD = "EAF" OR loptemp.ARBKOD = "EJK" OR loptemp.ARBKOD = "ELL" OR loptemp.ARBKOD = "EUH" THEN.
            ELSE SUBSTRING(loptemp.BENAMNING,60) = KALKYLLOPPOSTER.KOMMENTAR.
         END.
      END.
   END.      
END PROCEDURE.

PROCEDURE Loptempnklog_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER grundkloid AS INTEGER NO-UNDO.
   
   DEFINE OUTPUT PARAMETER TABLE FOR loptemp.
   EMPTY TEMP-TABLE loptemp NO-ERROR. 
   DEBUGGER:SET-BREAK().
   IF grundkloid > 0 THEN  FIND FIRST  KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = grundkloid NO-LOCK NO-ERROR.
   ELSE FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR. 
   IF rad_typ = 4 THEN DO:      
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
         FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID NO-LOCK:
            IF KALKYLLOPPOSTER.TYPKALK = 2 OR KALKYLLOPPOSTER.TYPKALK = 3 THEN DO:
               CREATE loptemp.
               ASSIGN 
               loptemp.ARBKOD    = KALKYLLOPPOSTER.ARBKOD
               loptemp.LOPNR      = KALKYLLOPPOSTER.LOPNR
               loptemp.BENAMNING  = REPLACE(KALKYLLOPPOSTER.BENAMNING,CHR(10)," ")
               loptemp.ENHET      = KALKYLLOPPOSTER.ENHET
               loptemp.KATAR      = KALKYLKATALOG.VISARTAL
               loptemp.TYP        = KALKYLLOPPOSTER.TYPKALK.
               SUBSTRING(loptemp.BENAMNING,60) = KALKYLLOPPOSTER.KOMMENTAR.                   
            END.   
         END.
      END.
   END.
   ELSE DO:         
      FOR EACH KALKYLKATALOGSUB WHERE KALKYLKATALOGSUB.KLOGID = KALKYLKATALOG.KLOGID NO-LOCK:
         FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOGSUB.KLOGSUBID AND KALKYLLOPPOSTER.TYPKALK = rad_typ NO-LOCK:
            CREATE loptemp.
            ASSIGN 
            loptemp.ARBKOD    = KALKYLLOPPOSTER.ARBKOD
            loptemp.LOPNR      = KALKYLLOPPOSTER.LOPNR            
            loptemp.BENAMNING  = REPLACE(KALKYLLOPPOSTER.BENAMNING,CHR(10)," ")
            loptemp.ENHET      = KALKYLLOPPOSTER.ENHET
            loptemp.KATAR      = KALKYLKATALOG.VISARTAL
            loptemp.TYP        = KALKYLLOPPOSTER.TYPKALK.
            IF loptemp.ARBKOD = "EAF" OR loptemp.ARBKOD = "EJK" OR loptemp.ARBKOD = "ELL" OR loptemp.ARBKOD = "EUH" THEN.
            ELSE SUBSTRING(loptemp.BENAMNING,60) = KALKYLLOPPOSTER.KOMMENTAR.
         END.
      END.
   END.      
END PROCEDURE.

PROCEDURE Ptemp3_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.   
   DEFINE OUTPUT PARAMETER TABLE FOR ptemp3.
   EMPTY TEMP-TABLE ptemp3 NO-ERROR. 
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   FOR EACH KALKYLARBKODER WHERE KALKYLARBKODER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID AND KALKYLARBKODER.TYPKALK = rad_typ NO-LOCK:
      CREATE ptemp3.
      ASSIGN 
      ptemp3.ARBKOD      = KALKYLARBKODER.ARBKOD
      ptemp3.BENAMNING   = KALKYLARBKODER.BENAMNING
      ptemp3.KATAR       = KALKYLKATALOG.VISARTAL.
   END.
END PROCEDURE.
/*P3 L�Pkoder f�r beredning*/

PROCEDURE Loptempn3_UI:
   DEFINE INPUT PARAMETER rad_typ AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR loptemp3.
   EMPTY TEMP-TABLE loptemp3 NO-ERROR. 
   FIND LAST KALKYLKATALOG WHERE KALKYLKATALOG.BENAMNING BEGINS "EBR" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   
   FOR EACH KALKYLLOPPOSTER WHERE KALKYLLOPPOSTER.KLOGSUBID = KALKYLKATALOG.HKLOGSUBID AND KALKYLLOPPOSTER.TYPKALK = rad_typ NO-LOCK:
      CREATE loptemp3.
      ASSIGN 
      loptemp3.ARBKOD    = KALKYLLOPPOSTER.ARBKOD
      loptemp3.LOPNR      = KALKYLLOPPOSTER.LOPNR
      loptemp3.BENAMNING  = KALKYLLOPPOSTER.BENAMNING
      loptemp3.ENHET      = KALKYLLOPPOSTER.ENHET
      loptemp3.KATAR      = KALKYLKATALOG.VISARTAL.
      SUBSTRING(loptemp3.BENAMNING,60) = KALKYLLOPPOSTER.KOMMENTAR.
   END.
      
END PROCEDURE.


/*============F�R ATT SKAPA QUERY===========*/
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh IN WIDGET-POOL "KalkDynTable".
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
   
PROCEDURE CloseCustomQuery:
   DEFINE INPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CustomQueryh:QUERY-CLOSE()  NO-ERROR.
   CustomQueryh = ?.
END PROCEDURE.
PROCEDURE and_UI:
   IF kommandoquery NE " " THEN kommandoquery = kommandoquery + " AND".
END PROCEDURE.

PROCEDURE procset_UI:
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.  
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.   
   IF NOT VALID-HANDLE(edataapph) THEN RUN EXTRADATAHMT.P PERSISTENT SET edataapph.            
END PROCEDURE .
PROCEDURE procreset_UI :
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR.  
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
   edataapph = ?.
END PROCEDURE.

PROCEDURE procsetkop_UI:  
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR.  
   IF NOT VALID-HANDLE(fbestapph) THEN RUN EXTRATABHMT.P PERSISTENT SET fbestapph.            
END PROCEDURE. 
PROCEDURE procresetkop_UI :
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
   IF VALID-HANDLE(fbestapph) THEN DELETE PROCEDURE fbestapph. 
   fbestapph = ?. 
END PROCEDURE.
PROCEDURE avsluta_UI :
   DEFINE VARIABLE dsi AS INTEGER NO-UNDO.
   RELEASE KALKNUMSUB NO-ERROR.
   RELEASE KALKNUM NO-ERROR.
   DELETE WIDGET-POOL "KalkDynTable" NO-ERROR.
   DELETE OBJECT Kalknumanvegentth NO-ERROR.
   Kalknumanvegentth = ?.
   DELETE OBJECT Kalknumanvegensubtth NO-ERROR.
   Kalknumanvegensubtth = ?.
   DELETE OBJECT Kalknumanvegensubbuffh NO-ERROR.
   Kalknumanvegensubbuffh = ?.
   DELETE OBJECT Kalknumanvegenbuffh NO-ERROR.
   Kalknumanvegenbuffh = ?.
   
   IF VALID-HANDLE(dyndamicDSh) THEN DO:
      RUN RelDatset_UI IN dyndamicDSh.
      DELETE PROCEDURE dyndamicDSh NO-ERROR.
      dyndamicDSh = ?.
   END.   
   DO dsi = 1 TO 10: 
      IF VALID-HANDLE(dyndamicDSMh[dsi]) THEN DO:
         RUN RelDatset_UI IN dyndamicDSMh[dsi].
         DELETE PROCEDURE dyndamicDSMh[dsi] NO-ERROR.
         dyndamicDSMh[dsi] = ?.
      END.   
   END.   
END PROCEDURE.

PROCEDURE oversatt_UI:
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER nykatidvar AS INTEGER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR KalkylimportTT.
   FIND FIRST KALKHUV WHERE KALKHUV.KALKNR = KalkNrvar AND KALKHUV.OMRADE = omrvar NO-LOCK NO-ERROR.
   IF KALKHUV.KLOGID = NyKatIdvar THEN.
   ELSE DO:      

      FIND FIRST KALKYLKATALOGBUF WHERE KALKYLKATALOGBUF.KLOGID = KALKHUV.KLOGID NO-LOCK NO-ERROR.
            
      FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = NyKatIdvar NO-LOCK NO-ERROR.
      IF KALKYLKATALOG.BENAMNING = "EBR 2018" OR KALKYLKATALOG.BENAMNING = "ANV�ND EJ EBR 2018" THEN DO:
         IF KALKYLKATALOGBUF.BENAMNING = "EBR 2017" OR KALKYLKATALOGBUF.BENAMNING = "EBR 2017/2018" THEN DO:
            FOR EACH KalkylimportTT WHERE KalkylimportTT.HJUPPDAT = 0 AND KalkylimportTT.ARBKOD = "137" NO-LOCK:
               IF       KalkylimportTT.LOPNR = 9 THEN KalkylimportTT.LOPNR = 64.
               ELSE IF KalkylimportTT.LOPNR = 11 THEN KalkylimportTT.LOPNR = 1.
               ELSE IF KalkylimportTT.LOPNR = 13 THEN KalkylimportTT.LOPNR = 3.
               ELSE IF KalkylimportTT.LOPNR = 14 THEN KalkylimportTT.LOPNR = 4.
               ELSE IF KalkylimportTT.LOPNR = 15 THEN KalkylimportTT.LOPNR = 5.
               ELSE IF KalkylimportTT.LOPNR = 16 THEN KalkylimportTT.LOPNR = 6.
                           
               ELSE IF KalkylimportTT.LOPNR = 20 THEN KalkylimportTT.LOPNR = 58.
               ELSE IF KalkylimportTT.LOPNR = 21 THEN KalkylimportTT.LOPNR = 54.
               ELSE IF KalkylimportTT.LOPNR = 22 THEN KalkylimportTT.LOPNR = 51.            
               ELSE IF KalkylimportTT.LOPNR = 23 THEN KalkylimportTT.LOPNR = 50.
               ELSE IF KalkylimportTT.LOPNR = 24 THEN KalkylimportTT.LOPNR = 53.
               ELSE IF KalkylimportTT.LOPNR = 25 THEN KalkylimportTT.LOPNR = 20.
               ELSE IF KalkylimportTT.LOPNR = 26 THEN KalkylimportTT.LOPNR = 59.
               ELSE IF KalkylimportTT.LOPNR = 27 THEN KalkylimportTT.LOPNR = 21.
               ELSE IF KalkylimportTT.LOPNR = 28 THEN KalkylimportTT.LOPNR = 23.
               ELSE IF KalkylimportTT.LOPNR = 29 THEN KalkylimportTT.LOPNR = 22.
               ELSE IF KalkylimportTT.LOPNR = 30 THEN KalkylimportTT.LOPNR = 26.
               ELSE IF KalkylimportTT.LOPNR = 32 THEN KalkylimportTT.LOPNR = 28.
               ELSE IF KalkylimportTT.LOPNR = 34 THEN KalkylimportTT.LOPNR = 30.
               ELSE IF KalkylimportTT.LOPNR = 35 THEN KalkylimportTT.LOPNR = 68.
               ELSE IF KalkylimportTT.LOPNR = 36 THEN KalkylimportTT.LOPNR = 65.
               ELSE IF KalkylimportTT.LOPNR = 37 THEN KalkylimportTT.LOPNR = 40.
               ELSE IF KalkylimportTT.LOPNR = 38 THEN KalkylimportTT.LOPNR = 41.
               ELSE IF KalkylimportTT.LOPNR = 39 THEN KalkylimportTT.LOPNR = 62.
               ELSE IF KalkylimportTT.LOPNR = 43 THEN KalkylimportTT.LOPNR = 44.
               ELSE IF KalkylimportTT.LOPNR = 44 THEN KalkylimportTT.LOPNR = 45.
               ELSE IF KalkylimportTT.LOPNR = 45 THEN KalkylimportTT.LOPNR = 72.
               ELSE IF KalkylimportTT.LOPNR = 46 THEN KalkylimportTT.LOPNR = 73.
               ELSE IF KalkylimportTT.LOPNR = 49 THEN KalkylimportTT.LOPNR = 76.
               ELSE IF KalkylimportTT.LOPNR = 50 THEN KalkylimportTT.LOPNR = 33.
               ELSE IF KalkylimportTT.LOPNR = 51 THEN KalkylimportTT.LOPNR = 34.
               ELSE IF KalkylimportTT.LOPNR = 52 THEN KalkylimportTT.LOPNR = 35.
                           
               ELSE IF KalkylimportTT.LOPNR = 53 THEN DO:
                  ASSIGN 
                  KalkylimportTT.ARBKOD = "138".
                  KalkylimportTT.LOPNR = 70.
               END.
               ELSE IF KalkylimportTT.LOPNR = 54 THEN DO:
                  ASSIGN 
                  KalkylimportTT.ARBKOD = "138".
                  KalkylimportTT.LOPNR = 71.
               END.
               ELSE IF KalkylimportTT.LOPNR = 55 THEN DO:
                  ASSIGN 
                  KalkylimportTT.ARBKOD = "138".
                  KalkylimportTT.LOPNR = 72.
               END.               
               ELSE IF KalkylimportTT.LOPNR = 56 THEN KalkylimportTT.LOPNR = 36.            
               ELSE IF KalkylimportTT.LOPNR = 87 THEN KalkylimportTT.LOPNR = 61.
               ELSE IF KalkylimportTT.LOPNR = 88 THEN KalkylimportTT.LOPNR = 80.
               ELSE IF KalkylimportTT.LOPNR = 89 THEN KalkylimportTT.LOPNR = 81.
               ELSE IF KalkylimportTT.LOPNR = 90 THEN KalkylimportTT.LOPNR = 84.
               ELSE IF KalkylimportTT.LOPNR = 91 THEN KalkylimportTT.LOPNR = 85.
               ELSE IF KalkylimportTT.LOPNR = 92 THEN KalkylimportTT.LOPNR = 87.
               ELSE IF KalkylimportTT.LOPNR = 93 THEN KalkylimportTT.LOPNR = 88.
               ELSE IF KalkylimportTT.LOPNR = 94 THEN KalkylimportTT.LOPNR = 89.
               ELSE IF KalkylimportTT.LOPNR = 95 THEN KalkylimportTT.LOPNR = 90.
               ELSE IF KalkylimportTT.LOPNR = 96 THEN KalkylimportTT.LOPNR = 91.
               ELSE IF KalkylimportTT.LOPNR = 97 THEN KalkylimportTT.LOPNR = 92.
               ELSE IF KalkylimportTT.LOPNR = 98 THEN KalkylimportTT.LOPNR = 70.
               ELSE IF KalkylimportTT.LOPNR = 99 THEN KalkylimportTT.LOPNR = 71.
               
                
               KalkylimportTT.HJUPPDAT = 1.
            END.   
         END.
      END.   
      
      IF KALKYLKATALOG.BENAMNING = "EBR 2017" OR KALKYLKATALOG.BENAMNING = "EBR 2017/2018" THEN DO:
         IF KALKYLKATALOGBUF.BENAMNING = "EBR 2018" OR KALKYLKATALOGBUF.BENAMNING = "ANV�ND EJ EBR 2018"   THEN DO:
            FOR EACH KalkylimportTT WHERE KalkylimportTT.HJUPPDAT = 0 AND KalkylimportTT.ARBKOD = "137" NO-LOCK:
               IF      KalkylimportTT.LOPNR = 1 THEN KalkylimportTT.LOPNR = 11.
               ELSE IF KalkylimportTT.LOPNR = 3 THEN KalkylimportTT.LOPNR = 13.
               ELSE IF KalkylimportTT.LOPNR = 4 THEN KalkylimportTT.LOPNR = 14.
               ELSE IF KalkylimportTT.LOPNR = 5 THEN KalkylimportTT.LOPNR = 15.
               ELSE IF KalkylimportTT.LOPNR = 6 THEN KalkylimportTT.LOPNR = 16.
               ELSE IF KalkylimportTT.LOPNR = 64 THEN KalkylimportTT.LOPNR = 9.
               ELSE IF KalkylimportTT.LOPNR = 58 THEN KalkylimportTT.LOPNR = 20.
               ELSE IF KalkylimportTT.LOPNR = 54 THEN KalkylimportTT.LOPNR = 21.
               ELSE IF KalkylimportTT.LOPNR = 51 THEN KalkylimportTT.LOPNR = 22.            
               ELSE IF KalkylimportTT.LOPNR = 50 THEN KalkylimportTT.LOPNR = 23.
               ELSE IF KalkylimportTT.LOPNR = 53 THEN KalkylimportTT.LOPNR = 24.
               ELSE IF KalkylimportTT.LOPNR = 20 THEN KalkylimportTT.LOPNR = 25.
               ELSE IF KalkylimportTT.LOPNR = 59 THEN KalkylimportTT.LOPNR = 26.
               ELSE IF KalkylimportTT.LOPNR = 21 THEN KalkylimportTT.LOPNR = 27.
               ELSE IF KalkylimportTT.LOPNR = 23 THEN KalkylimportTT.LOPNR = 28.
               ELSE IF KalkylimportTT.LOPNR = 22 THEN KalkylimportTT.LOPNR = 29.
               ELSE IF KalkylimportTT.LOPNR = 26 THEN KalkylimportTT.LOPNR = 30.
               ELSE IF KalkylimportTT.LOPNR = 28 THEN KalkylimportTT.LOPNR = 32.
               ELSE IF KalkylimportTT.LOPNR = 30 THEN KalkylimportTT.LOPNR = 34.
               ELSE IF KalkylimportTT.LOPNR = 68 THEN KalkylimportTT.LOPNR = 35.
               ELSE IF KalkylimportTT.LOPNR = 65 THEN KalkylimportTT.LOPNR = 36.
               ELSE IF KalkylimportTT.LOPNR = 40 THEN KalkylimportTT.LOPNR = 37.
               ELSE IF KalkylimportTT.LOPNR = 41 THEN KalkylimportTT.LOPNR = 38.
               ELSE IF KalkylimportTT.LOPNR = 62 THEN KalkylimportTT.LOPNR = 39.
               ELSE IF KalkylimportTT.LOPNR = 44 THEN KalkylimportTT.LOPNR = 43.
               ELSE IF KalkylimportTT.LOPNR = 45 THEN KalkylimportTT.LOPNR = 44.
               ELSE IF KalkylimportTT.LOPNR = 72 THEN KalkylimportTT.LOPNR = 45.
               ELSE IF KalkylimportTT.LOPNR = 73 THEN KalkylimportTT.LOPNR = 46.
               ELSE IF KalkylimportTT.LOPNR = 76 THEN KalkylimportTT.LOPNR = 49.
               ELSE IF KalkylimportTT.LOPNR = 33 THEN KalkylimportTT.LOPNR = 50.
               ELSE IF KalkylimportTT.LOPNR = 34 THEN KalkylimportTT.LOPNR = 51.
               ELSE IF KalkylimportTT.LOPNR = 35 THEN KalkylimportTT.LOPNR = 52.                                
               ELSE IF KalkylimportTT.LOPNR = 36 THEN KalkylimportTT.LOPNR = 56.
               
               ELSE IF KalkylimportTT.LOPNR = 61 THEN KalkylimportTT.LOPNR = 87.
               ELSE IF KalkylimportTT.LOPNR = 80 THEN KalkylimportTT.LOPNR = 88.
               ELSE IF KalkylimportTT.LOPNR = 81 THEN KalkylimportTT.LOPNR = 89.
               ELSE IF KalkylimportTT.LOPNR = 84 THEN KalkylimportTT.LOPNR = 90.
               ELSE IF KalkylimportTT.LOPNR = 85 THEN KalkylimportTT.LOPNR = 91.
               ELSE IF KalkylimportTT.LOPNR = 87 THEN KalkylimportTT.LOPNR = 92.
               ELSE IF KalkylimportTT.LOPNR = 88 THEN KalkylimportTT.LOPNR = 93.
               ELSE IF KalkylimportTT.LOPNR = 89 THEN KalkylimportTT.LOPNR = 94.
               ELSE IF KalkylimportTT.LOPNR = 90 THEN KalkylimportTT.LOPNR = 95.
               ELSE IF KalkylimportTT.LOPNR = 91 THEN KalkylimportTT.LOPNR = 96.
               ELSE IF KalkylimportTT.LOPNR = 92 THEN KalkylimportTT.LOPNR = 97.
               ELSE IF KalkylimportTT.LOPNR = 70 THEN KalkylimportTT.LOPNR = 98.
               ELSE IF KalkylimportTT.LOPNR = 71 THEN KalkylimportTT.LOPNR = 99.
               KalkylimportTT.HJUPPDAT = 1.
            END.
            FOR EACH KalkylimportTT WHERE KalkylimportTT.HJUPPDAT = 0 AND KalkylimportTT.ARBKOD = "138" NO-LOCK:               
               IF KalkylimportTT.LOPNR = 70 THEN DO:
                  ASSIGN 
                  KalkylimportTT.ARBKOD = "137".
                  KalkylimportTT.LOPNR = 53.
               END.
               ELSE IF KalkylimportTT.LOPNR = 71 THEN DO:
                  ASSIGN 
                  KalkylimportTT.ARBKOD = "137".
                  KalkylimportTT.LOPNR = 54.
               END.
               ELSE IF KalkylimportTT.LOPNR = 72 THEN DO:
                  ASSIGN 
                  KalkylimportTT.ARBKOD = "137".
                  KalkylimportTT.LOPNR = 55.
               END.
               KalkylimportTT.HJUPPDAT = 1.               
            END.             
         END.
      END.   
   END.   
      

END PROCEDURE.


/*VolymDS*/
PROCEDURE VolymCreate:
   CREATE TEMP-TABLE VolymberTTh IN WIDGET-POOL "KalkDynTable".
   VolymberTTh:CREATE-LIKE("VolymberTT").
   VolymberTTh:TEMP-TABLE-PREPARE("Volymtemp").
   VolymberTTbuffh = VolymberTTh:DEFAULT-BUFFER-HANDLE.
  
   
END PROCEDURE.
PROCEDURE laddaScHVolymDS_UI :
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER vad AS INTEGER NO-UNDO. 
   DEFINE OUTPUT PARAMETER DATASET-HANDLE VolymDS BIND.
   DEFINE VARIABLE volymlog AS LOGICAL NO-UNDO.
   
   RUN FINNSTABELL.P (INPUT "HDVOLYMBER", OUTPUT volymlog).  
   IF volymlog = TRUE THEN.
   ELSE RETURN. 
   IF VALID-HANDLE(VolymDS) THEN.
   ELSE RUN VolymCreate.
   
   RUN GetDatasetDeftt_UI ("VolymDS").  
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(VolymberTTbuffh).
   DatasetDeftt.pcRelFields[1] = "ANVANDARE,ANVANDARE,NUM,NUM".
   DatasetDeftt.pcSources[1] = "HDVOLYMBER".
   DatasetDeftt.pcSourceKeys[1] = "BERNR,OMRADE,SID,KLID".
   DatasetDeftt.pcKeyValue[1] = "HDVOLYMBER.BERNR = " + STRING(bernrvar) + " AND HDVOLYMBER.OMRADE = " + QUOTER(omrvar) + " AND HDVOLYMBER.SID = " + STRING(vad). 
  
   RUN DefAndLoadDs_UI IN dyndamicDSh
   ({DataSetInput.I} OUTPUT DATASET-HANDLE VolymDS BIND).

       
END PROCEDURE.

PROCEDURE laddaKalkVolymDS_UI :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER Omradevar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER Sidvar AS INTEGER NO-UNDO. 
   DEFINE INPUT  PARAMETER Matrisvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE VolymDS BIND.
   DEFINE VARIABLE klidvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE volymlog AS LOGICAL NO-UNDO.
   RUN FINNSTABELL.P (INPUT "HDVOLYMBER", OUTPUT volymlog).  
   IF volymlog = TRUE THEN.
   ELSE RETURN. 
   IF VALID-HANDLE(VolymDS) THEN.
   ELSE RUN VolymCreate.
   
   RUN GetDatasetDeftt_UI ("VolymDS").  
   DatasetDeftt.antaltab = 1.
   DatasetDeftt.pcBuffers[1] = STRING(VolymberTTbuffh).
   DatasetDeftt.pcRelFields[1] = "ANVANDARE,ANVANDARE,NUM,NUM".
   DatasetDeftt.pcSources[1] = "HDVOLYMBER".
   DatasetDeftt.pcSourceKeys[1] = "KALKNR,OMRADE,SID,MATRIS,KLID".
   DatasetDeftt.pcKeyValue[1] = "HDVOLYMBER.KALKNR = " + STRING(KalkNrvar) + " AND HDVOLYMBER.OMRADE = " + QUOTER(Omradevar) + 
   " AND HDVOLYMBER.SID = " + STRING(Sidvar) + " AND HDVOLYMBER.MATRIS = " + STRING(Matrisvar). 
  
   RUN DefAndLoadDs_UI IN dyndamicDSh
   ({DataSetInput.I} OUTPUT DATASET-HANDLE VolymDS BIND).
   /*
   REPEAT:     
      VolymberTTbuffh:BUFFER-CREATE().
      klidvar = klidvar + 1.
      VolymberTTbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = KalkNrvar.
      VolymberTTbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = Omradevar.
      VolymberTTbuffh:BUFFER-FIELD("SID"):BUFFER-VALUE = Sidvar.
      VolymberTTbuffh:BUFFER-FIELD("MATRIS"):BUFFER-VALUE = Matrisvar.
      VolymberTTbuffh:BUFFER-FIELD("KLID"):BUFFER-VALUE = klidvar.
      VolymberTTbuffh:BUFFER-FIELD("STRACKA"):BUFFER-VALUE = "Till�ggsstr�cka " + STRING(klidvar).
      VolymberTTbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = VolymberTTbuffh:RECID.
      IF klidvar = 3 THEN LEAVE.   
   END.
   */
   /*
   REPEAT:     
      VolymberTTbuffh:BUFFER-CREATE().
      klidvar = klidvar + 1.
      VolymberTTbuffh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = THIS-OBJECT:ControlShell:KalkNrvar.
      VolymberTTbuffh:BUFFER-FIELD("OMRADE"):BUFFER-VALUE = THIS-OBJECT:ControlShell:Omradevar.
      VolymberTTbuffh:BUFFER-FIELD("KLID"):BUFFER-VALUE = klidvar.
      VolymberTTbuffh:BUFFER-FIELD("STRACKA"):BUFFER-VALUE = "Till�ggsstr�cka " + STRING(klidvar).
      VolymberTTbuffh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = VolymberTTbuffh:RECID.
      IF klidvar = 3 THEN LEAVE.   
   END.
    */
    
         
       
END PROCEDURE.

PROCEDURE schaktberidlhmt_UI :
   DEFINE INPUT PARAMETER bernrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER avtschakt AS INTEGER NO-UNDO.
   /*hitta schaktet som beredningskonstruktionerna �r importerade till Lena 20200225*/
   FIND FIRST HDSCHAKTPROT    WHERE HDSCHAKTPROT.BERNR =  bernrvar AND HDSCHAKTPROT.OMRADE =  omrvar AND  HDSCHAKTPROT.BERID > 0 NO-LOCK NO-ERROR.
   IF AVAILABLE HDSCHAKTPROT THEN DO:
      avtschakt = HDSCHAKTPROT.SID.
   END.   
END PROCEDURE.