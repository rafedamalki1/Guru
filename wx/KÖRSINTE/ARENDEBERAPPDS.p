
/*------------------------------------------------------------------------
    File        : ARENDEBERAPPDS.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Thu Aug 30 08:30:08 CEST 2012
    Notes       :
  ----------------------------------------------------------------------*/
/*Anders Olsson Elpool i Umeå AB  26 nov 2013 10:01:52 
  
    {MobileArendeDS.i}
    
    {SparaProDatasSetClasser.i dsMobileArende}
    
    DEFINE QUERY ArendeQuery FOR ARENDEHUV.
    DEFINE DATA-SOURCE ArendehuvSrc FOR QUERY ArendeQuery ARENDEHUV KEYS (ARENDENR).   /*keys unika nycklar*/
    DEFINE VARIABLE hArendeDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
    METHOD PUBLIC VOID attachdsMobileArende():
       hArendeDataSet:GET-BUFFER-HANDLE("arendehuvtt"):ATTACH-DATA-SOURCE(DATA-SOURCE ArendehuvSrc:HANDLE).
    END METHOD.
     
    METHOD PUBLIC VOID ArendeDSstart():
       hArendeDataSet = DATASET dsMobileArende:HANDLE.      /*koppla handel till dataset*/
       hArendeDataSet:SET-CALLBACK("AFTER-FILL", "postDataSetFilldsMobileArende", THIS-OBJECT).
    END METHOD.
    
    /*------------------------------------------------------------------------------
            Purpose:  Get one or more records, based on a filter string                                                                     
            Notes:                                                                        
    ------------------------------------------------------------------------------*/
    @openapi.openedge.export(type="REST", useReturnValue="false", writeDataSetBeforeImage="false").
    @progress.service.resourceMapping(type="REST", operation="read", URI="?filter=~{filter~}", alias="", mediaType="application/json"). 
    METHOD PUBLIC VOID ReadMobileArende(
       INPUT filter AS CHARACTER, 
       OUTPUT DATASET dsMobileArende):      
       
       EMPTY TEMP-TABLE arendehuvtt.
       BUFFER arendehuvtt:ATTACH-DATA-SOURCE(DATA-SOURCE ArendehuvSrc:HANDLE).
       filter = "where ARENDENR = 100231".
       IF filter NE "" AND filter NE ? THEN
       DATA-SOURCE ArendehuvSrc:FILL-WHERE-STRING = filter.
       DATASET dsMobileArende:FILL().
       BUFFER arendehuvtt:DETACH-DATA-SOURCE().
       RETURN.   
        /* TODO: Add code to get a set of records and return the
           resulting records to the client. */      
    END METHOD. 
*/

{ARENDEKAT.I}
{ARENDEALLTEMPC.I}
{ARENDEUPPTT.I}
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

DEFINE VARIABLE dynqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
DEFINE BUFFER ARENDEHUVBUF FOR ARENDEHUV.
DEFINE BUFFER GURUDEFAULTSBUF FOR GURUDEFAULTS.
DEFINE BUFFER ARENDEMTRLBUF FOR ARENDEMTRL.
DEFINE BUFFER ARENDENUMBUF FOR ARENDENUM.
DEFINE BUFFER ARENDENUMSUBBUF FOR ARENDENUMSUB.


DEFINE INPUT  PARAMETER inglobanv AS CHARACTER NO-UNDO.



FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.

RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).

{ARENDEPRODATA.i}
/*ArendeDS*/
{SparaProDatasSet.i ArendeDS}

DEFINE QUERY ArendeQuery FOR ARENDEHUV.
DEFINE DATA-SOURCE KalkhuvSrc FOR QUERY ArendeQuery ARENDEHUV KEYS (ARENDENR,OMRADE).   /*keys unika nycklar*/
DEFINE DATA-SOURCE NumSrc FOR ARENDENUM KEYS (ARENDENR,OMRADE,NUM).
DEFINE DATA-SOURCE NumSubSrc FOR ARENDENUMSUB KEYS (ARENDENR,OMRADE,NUM,NUMSUBID).
DEFINE DATA-SOURCE AonrSrc FOR ARENDEAONR KEYS (ARENDENR,OMRADE,PLANNR,ARTAL).
DEFINE DATA-SOURCE MtrlSrc FOR ARENDEMTRL KEYS (ARENDENR,OMRADE,MID).
DEFINE DATA-SOURCE TidlSrc FOR ARENDETIDLAGE KEYS (ARENDENR,OMRADE,IDTIDLAG).

DEFINE VARIABLE hArendeDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hArendeDataSet = DATASET ArendeDS:HANDLE.      /*koppla handel till dataset*/
hArendeDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillArendeDS", THIS-PROCEDURE). 
/*ArendeDS*/
{ARENDEKATPRODATA.i}

{SparaProDatasSet.i KalkylMallarDS}



/*KalkylMallarDS*/
DEFINE QUERY KalkylMallQuery FOR KALKMALLHUVUD.
DEFINE DATA-SOURCE MallSrc FOR QUERY KalkylMallQuery KALKMALLHUVUD KEYS (MALLNR).
DEFINE DATA-SOURCE MallArbSrc FOR KALKMALLKODER KEYS (MALLNR,ARBKOD,LOPNR).

DEFINE VARIABLE hKalkylMallDataSet   AS HANDLE     NO-UNDO.         /*handl till dataset*/
hKalkylMallDataSet = DATASET KalkylMallarDS:HANDLE.      /*koppla handel till dataset*/
hKalkylMallDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillKalkylMallarDS", THIS-PROCEDURE). 
/*KalkylMallarDS*/



PROCEDURE attachArendeDS: /*kopplar ihop temptabell med skarptababell.      */
   hArendeDataSet:GET-BUFFER-HANDLE("arendehuvtt"):ATTACH-DATA-SOURCE(DATA-SOURCE KalkhuvSrc:HANDLE).
   hArendeDataSet:GET-BUFFER-HANDLE("arendenumtt"):ATTACH-DATA-SOURCE(DATA-SOURCE NumSrc:HANDLE).
   hArendeDataSet:GET-BUFFER-HANDLE("arendenumsubtt"):ATTACH-DATA-SOURCE(DATA-SOURCE NumsubSrc:HANDLE).
   hArendeDataSet:GET-BUFFER-HANDLE("arendeaonrTT"):ATTACH-DATA-SOURCE(DATA-SOURCE AonrSrc:HANDLE).
   hArendeDataSet:GET-BUFFER-HANDLE("arendemtrlTT"):ATTACH-DATA-SOURCE(DATA-SOURCE MtrlSrc:HANDLE).
   hArendeDataSet:GET-BUFFER-HANDLE("arendetidlageTT"):ATTACH-DATA-SOURCE(DATA-SOURCE TidlSrc:HANDLE).
   
END PROCEDURE.
PROCEDURE LaddaArende:
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER.
   DEFINE INPUT PARAMETER KalkOmr AS CHARACTER.  
   DEFINE OUTPUT PARAMETER DATASET FOR ArendeDS.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DATASET ArendeDS:EMPTY-DATASET().
   IF KalkNrvar NE ? THEN DO:
      queryprep = "FOR EACH ARENDEHUV WHERE ARENDEHUV.ARENDENR = " + STRING(KalkNrvar) + 
      " AND ARENDEHUV.OMRADE = " + "'" + KalkOmr + "'" + "  NO-LOCK". 
      QUERY ArendeQuery:QUERY-PREPARE(queryprep).
   END.   
   RUN attachArendeDS.
   IF KalkNrvar NE ? THEN DATASET ArendeDS:FILL().
   RUN KalktidLageNamn.
   
   detachDataSetArendeDS(hArendeDataSet).
  
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
   DEFINE INPUT PARAMETER vadgora AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER kalknrvar AS CHARACTER NO-UNDO.  
   DEFINE INPUT PARAMETER omradevar AS CHARACTER NO-UNDO. 
   DEFINE INPUT PARAMETER kalkanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lasavanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lasavnamn AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER lastav AS LOGICAL NO-UNDO. 
   DEFINE VARIABLE sokvar  AS CHARACTER NO-UNDO.
   /*ÄNDAR BORT KALKYL,KOPIERA, KONVERTERA NY xml-import*/
   sokvar = "A" + kalknrvar + "$" + omradevar.
    
   IF vadgora = 1 THEN DO:
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
      END.
   END.
   IF vadgora = 2 THEN DO:
      FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = sokvar AND ANVPER.PERSONALKOD = kalkanv NO-LOCK NO-ERROR.
      IF AVAILABLE ANVPER THEN DO TRANSACTION:
         FIND CURRENT ANVPER EXCLUSIVE-LOCK.
         DELETE ANVPER.
      END.
   END.
   IF vadgora = 3 THEN DO:
      FIND FIRST ANVPER WHERE ANVPER.ANVANDARE = sokvar NO-LOCK NO-ERROR.
     
      IF NOT AVAILABLE ANVPER THEN DO TRANSACTION:
          lastav = FALSE.
      END.
      ELSE DO:
         FIND FIRST ANVANDARE  WHERE ANVANDARE.ANVANDARE = ANVPER.PERSONALKOD NO-LOCK NO-ERROR.
         lastav = TRUE.
         lasavanv = ANVPER.PERSONALKOD.
         IF AVAILABLE ANVANDARE THEN lasavnamn = ANVANDARE.AV-NAMN.
      END.
   END.
   RELEASE ANVPER NO-ERROR.

END PROCEDURE.


PROCEDURE AonrInfo_UI :
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
   END.  
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = beredarvar NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      beredarvar = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.  
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = arbannsv NO-LOCK NO-ERROR.
   IF AVAILABLE PERSONALTAB THEN DO:
      arbannsv = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.
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




/*SKAPARTIDLÄGEN*/
PROCEDURE kalktidl_UI :
   DEFINE INPUT  PARAMETER kynr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR earendetidlageTT.
   EMPTY TEMP-TABLE earendetidlageTT NO-ERROR. 
   FOR EACH TIDSLAGEN WHERE TIDSLAGEN.AUTOMATISKT = TRUE NO-LOCK:
      CREATE earendetidlageTT.
      ASSIGN
      earendetidlageTT.ARENDENR = kynr
      earendetidlageTT.OMRADE = omrvar
      earendetidlageTT.IDTIDLAG = TIDSLAGEN.IDTIDLAG
      earendetidlageTT.ORDNING = TIDSLAGEN.AKTIVITET2
      earendetidlageTT.TIDLAGE = TIDSLAGEN.TIDLAGE
      earendetidlageTT.TTRECID = RECID(earendetidlageTT).
   END.
END PROCEDURE.
PROCEDURE KalktidLageNamn :
   FOR EACH arendetidlageTT WHERE NO-LOCK:
      FIND FIRST TIDSLAGEN WHERE TIDSLAGEN.IDTIDLAG = arendetidlageTT.IDTIDLAG NO-LOCK NO-ERROR.
      IF AVAILABLE TIDSLAGEN THEN DO:
         arendetidlageTT.TIDLAGE = TIDSLAGEN.TIDLAGE.
      END.
      ELSE arendetidlageTT.TIDLAGE = "Borttaget ifrån registret!". 
      IF arendetidlageTT.ANVANDARE1 NE "" THEN DO:
         FIND FIRST ANVANDARE  WHERE ANVANDARE.ANVANDARE = arendetidlageTT.ANVANDARE1 NO-LOCK NO-ERROR.
         IF AVAILABLE ANVANDARE THEN DO:
            arendetidlageTT.NAMNANVANDARE1 = ANVANDARE.AV-NAMN.
         END.
      END.      
   END.
END PROCEDURE.
PROCEDURE ArendeStatus_UI :
   DEFINE INPUT  PARAMETER kalkdatah AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER arendetidlageTTh AS HANDLE NO-UNDO.
   arendetidlageTTh = TEMP-TABLE arendetidlageTT:HANDLE:DEFAULT-BUFFER-HANDLE.
   arendetidlageTTh:EMPTY-TEMP-TABLE() NO-ERROR.
   CREATE QUERY dynqueh.
   dynqueh:SET-BUFFERS(kalkdatah).
   dynqueh:QUERY-PREPARE("FOR EACH " + kalkdatah:TABLE ).
   dynqueh:QUERY-OPEN() .
   dynqueh:GET-FIRST(NO-LOCK).
   DO WHILE kalkdatah:AVAILABLE: 
      arendetidlageTTh:BUFFER-CREATE().      
      arendetidlageTTh:BUFFER-COPY(kalkdatah).
      arendetidlageTTh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE = kalkdatah:BUFFER-FIELD("KALKNR"):BUFFER-VALUE.           
      dynqueh:GET-NEXT(NO-LOCK).        
   END.
   dynqueh:QUERY-CLOSE().
   
   FOR EACH arendetidlageTT:
      FIND LAST ARENDETIDLAGE WHERE ARENDETIDLAGE.ARENDENR = arendetidlageTT.ARENDENR AND ARENDETIDLAGE.OMRADE = arendetidlageTT.OMRADE USE-INDEX ARENDENR NO-LOCK NO-ERROR.
      BUFFER-COPY ARENDETIDLAGE TO arendetidlageTT.
   END.
   RUN KalktidLageNamn.
END PROCEDURE.

/*tar fram rätt nummer*/
PROCEDURE omradekoll_UI:
   DEFINE INPUT PARAMETER omradevar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER tempvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
   RUN kalksista_UI (INPUT-OUTPUT tempvar).

END PROCEDURE.
PROCEDURE kalksista_UI :  
   DEFINE INPUT-OUTPUT PARAMETER tempvar AS INTEGER NO-UNDO.
   DEFINE BUFFER ARENDEHUVSISTA FOR ARENDEHUV.
   DEFINE VARIABLE felkoll AS LOGICAL NO-UNDO.
   FIND LAST ARENDEHUVBUF NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ARENDEHUVBUF THEN DO:
      tempvar = 100000.
      RETURN.
   END.
   tempvar = ARENDEHUVBUF.ARENDENR + 1.
   RELEASE ARENDEHUVBUF NO-ERROR.
    
END PROCEDURE.
/*startproc hämtar katalogernamn och nykalkyl*/
PROCEDURE startny_UI :
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER sekanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR earendehuvtt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylkatalogtt.
    
   /*SAVEDS*/
   EMPTY TEMP-TABLE earendehuvtt NO-ERROR.
   EMPTY TEMP-TABLE kalkylkatalogtt NO-ERROR. 
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KATALOGTYP = "Ärende" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG  THEN DO:
      RETURN.
   END.
   
   FOR EACH KALKYLKATALOG WHERE KALKYLKATALOG.AVSLUTAD  = FALSE AND KALKYLKATALOG.KATALOGTYP = "Ärende" NO-LOCK:
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
   FIND FIRST kalkylkatalogtt WHERE kalkylkatalogtt.KATALOGTYP = "Ärende" USE-INDEX VISARTAL NO-LOCK NO-ERROR.
   IF NOT AVAILABLE kalkylkatalogtt THEN DO:
      FIND FIRST kalkylkatalogtt NO-LOCK NO-ERROR.
   END.
   IF NOT AVAILABLE kalkylkatalogtt THEN DO:
      RETURN.
   END.
   CREATE earendehuvtt.
   ASSIGN 
   earendehuvtt.OMRADE = omrvar
   earendehuvtt.KLOGID = kalkylkatalogtt.KLOGID
   earendehuvtt.TYPKALK = 2 
   earendehuvtt.EGETMTRL = FALSE  
   earendehuvtt.EGNAPRISER = FALSE
   earendehuvtt.FAKTORER = FALSE.
   earendehuvtt.TTRECID = RECID(earendehuvtt).

END PROCEDURE.

PROCEDURE sparakalkhuv_UI :
   DEFINE OUTPUT PARAMETER felvar AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER felmedd AS CHARACTER NO-UNDO.
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR earendehuvtt.
   DEFINE VARIABLE nyvar AS LOGICAL NO-UNDO.
   FIND FIRST earendehuvtt WHERE NO-LOCK NO-ERROR.
   IF earendehuvtt.ARENDENR = 0 THEN DO:
      RUN omradekoll_UI (INPUT earendehuvtt.OMRADE, OUTPUT earendehuvtt.ARENDENR).
      nyvar = TRUE.
   END.   
   ELSE nyvar = FALSE.
   IF earendehuvtt.ARENDENR = ? THEN DO:
      earendehuvtt.ARENDENR = 0.
      ASSIGN 
      felvar = TRUE
      felmedd =  "6667".      
      RETURN.
   END.    
   IF earendehuvtt.BENAMNING = "" THEN DO:
      ASSIGN 
      felvar = TRUE
      felmedd =  "68".      
      RETURN.
   END.    
   IF earendehuvtt.KALKANV = "" THEN DO:
      ASSIGN 
      felvar = TRUE
      felmedd =  "69".
      RETURN.
   END. 
   FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = earendehuvtt.KALKANV USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.
   IF NOT AVAILABLE PERSONALTAB THEN DO:
      ASSIGN 
      felvar = TRUE
      felmedd = "7071".
      RETURN.
   END.  
   
END PROCEDURE.
/*skapar kalkylnum*/ 
PROCEDURE skapanumsub_UI :
   DEFINE INPUT  PARAMETER subidvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER arbkodvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER lopkodvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR earendenumsubtt.
   EMPTY TEMP-TABLE earendenumsubtt NO-ERROR. 
   FOR EACH KALKYLLOPSUB WHERE KALKYLLOPSUB.KLOGSUBID = subidvar AND KALKYLLOPSUB.ARBKOD = arbkodvar AND 
   KALKYLLOPSUB.LOPNR = lopkodvar NO-LOCK: 
      CREATE earendenumsubtt.  
      BUFFER-COPY KALKYLLOPSUB TO earendenumsubtt.    
      FIND FIRST KALKYLPRISER  WHERE KALKYLPRISER.KPID = earendenumsubtt.KPID AND
      KALKYLPRISER.KLOGSUBID = KALKYLLOPSUB.KLOGSUBID NO-LOCK NO-ERROR.
      IF AVAILABLE KALKYLPRISER THEN DO:
         ASSIGN 
         earendenumsubtt.PRIS = KALKYLPRISER.PRIS
         earendenumsubtt.EGENPRISUPP = KALKYLPRISER.EGENPRISUPP 
         earendenumsubtt.EGENKODUPP = KALKYLPRISER.EGENKODUPP  
         earendenumsubtt.BENAMNING = KALKYLPRISER.BENAMNING.
      END.  
      ASSIGN         
      earendenumsubtt.FRIBENAMNING = earendenumsubtt.BENAMNING         
      earendenumsubtt.FRIKOSTNAD = earendenumsubtt.KOSTNAD
      earendenumsubtt.FRIPRIS = earendenumsubtt.PRIS
      earendenumsubtt.FRITIMMAR = earendenumsubtt.TIMMAR.
   END.           
END PROCEDURE.

/*hämtar kalkymallar körs inte*/
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
/*startproc hämtar kataloger och kalkyl*/
PROCEDURE kathmt_UI :
   DEFINE INPUT  PARAMETER kynr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER sekanv AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER felmed AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylarbkodertt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylloppostertt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylkatalogtt.
   DEFINE OUTPUT PARAMETER TABLE FOR markfiltertt.
   EMPTY TEMP-TABLE kalkylkatalogtt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylarbkodertt NO-ERROR.
   EMPTY TEMP-TABLE kalkylloppostertt NO-ERROR.
   EMPTY TEMP-TABLE markfiltertt NO-ERROR. 
   FIND LAST KALKYLKATALOG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      felmed = "Fel upplägg!".
      RETURN.
      
   END.
   FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = kynr AND ARENDEHUV.OMRADE = omrvar  NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ARENDEHUV THEN DO:
      felmed = "Ärende finns inte!".
      RETURN.
   END.
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = ARENDEHUV.KLOGID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN FIND LAST KALKYLKATALOG NO-LOCK NO-ERROR.
   IF NOT AVAILABLE KALKYLKATALOG THEN DO:
      felmed = "Fel upplägg!".
      RETURN.
   END.
   IF KALKYLKATALOG.SEKRETESS = TRUE THEN DO:
      FIND FIRST KALKYLKATALOGANV WHERE KALKYLKATALOGANV.KLOGID = KALKYLKATALOG.KLOGID AND KALKYLKATALOGANV.ANVANDARE = sekanv NO-LOCK NO-ERROR.
      IF NOT AVAILABLE KALKYLKATALOGANV THEN DO:
         felmed = "Du är inte behörig att se denna kalkyl!".
         RETURN.
      END.
   END.   
   CREATE kalkylkatalogtt.
   BUFFER-COPY KALKYLKATALOG TO kalkylkatalogtt.
   kalkylkatalogtt.TTRECID = RECID(kalkylkatalogtt).
   FOR EACH KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = ARENDEHUV.KLOGID NO-LOCK, 
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
   
END PROCEDURE.
/* hämta kalkylpriser*/
PROCEDURE kphmt : 
   /* vilken katalog den ska hämta poster på */
   DEFINE INPUT PARAMETER knr AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER omr AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkylprisertt.
   DEFINE OUTPUT PARAMETER TABLE FOR kalkvisningtt.
   EMPTY TEMP-TABLE kalkvisningtt NO-ERROR. 
   EMPTY TEMP-TABLE kalkylprisertt NO-ERROR.
   FIND FIRST ARENDEHUV WHERE  ARENDEHUV.ARENDENR = knr AND ARENDEHUV.OMRADE = omr NO-LOCK NO-ERROR.
   IF NOT AVAILABLE ARENDEHUV THEN RETURN.
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = ARENDEHUV.KLOGID NO-LOCK NO-ERROR.
  
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
  
   
END PROCEDURE.





PROCEDURE kalkylbort_UI :
   DEFINE INPUT PARAMETER KalkNrvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER felmed AS CHARACTER NO-UNDO.
   DEFINE VARIABLE berapph AS HANDLE NO-UNDO.
   DEFINE VARIABLE lasavanv AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lasavnamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lastav AS LOGICAL NO-UNDO.
   FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = KalkNrvar NO-LOCK NO-ERROR.
    RUN KalkStopp_UI (INPUT 3,INPUT ARENDEHUV.ARENDENR,INPUT ARENDEHUV.OMRADE, INPUT Guru.Konstanter:globanv, OUTPUT lasavanv, OUTPUT lasavnamn, OUTPUT lastav).
   IF lastav = TRUE THEN DO:
      felmed = "Ärendet är låst av " + lasavanv + " " + lasavnamn.
      RETURN.
   END.   
   RUN MENYBERAPP.P PERSISTENT SET berapph.
   RUN procsetkop_UI.
   FOR EACH GURUDEFAULTS WHERE GURUDEFAULTS.PROGRAM = "ARENDE" AND GURUDEFAULTS.HUVUDINT = ARENDEHUV.ARENDENR  AND GURUDEFAULTS.HUVUDCHAR = ARENDEHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE GURUDEFAULTS.
   END. 
   FOR EACH ARENDENUM WHERE ARENDENUM.ARENDENR = ARENDEHUV.ARENDENR  AND ARENDENUM.OMRADE = ARENDEHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE ARENDENUM.
   END.    
   FOR EACH ARENDENUMSUB WHERE ARENDENUMSUB.ARENDENR = ARENDEHUV.ARENDENR  AND ARENDENUMSUB.OMRADE = ARENDEHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE ARENDENUMSUB.
   END.
   FOR EACH ARENDEAONR WHERE ARENDEAONR.ARENDENR = ARENDEHUV.ARENDENR  AND ARENDEAONR.OMRADE = ARENDEHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE ARENDEAONR.
   END.
   FOR EACH ARENDEMTRL WHERE ARENDEMTRL.ARENDENR = ARENDEHUV.ARENDENR  AND ARENDEMTRL.OMRADE = ARENDEHUV.OMRADE  EXCLUSIVE-LOCK:
      DELETE ARENDEMTRL.
   END.
   
   DO TRANSACTION: 
      FIND CURRENT ARENDEHUV EXCLUSIVE-LOCK NO-ERROR.  
      DELETE ARENDEHUV.
   END.
   RUN procresetkop_UI.
   IF VALID-HANDLE(berapph) THEN DELETE PROCEDURE berapph NO-ERROR.
END PROCEDURE.


/*FÖR ATT ARBETAVIDARE SKA BLI RÄTT*/
PROCEDURE LaddaTypBen_UI:
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valdfasttemp.  
   FOR EACH valdfasttemp:
      FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = valdfasttemp.KALKNR NO-LOCK NO-ERROR.
      IF AVAILABLE ARENDEHUV THEN DO:
         BUFFER-COPY ARENDEHUV TO valdfasttemp.
         valdfasttemp.KALKNR = ARENDEHUV.ARENDENR.
         valdfasttemp.TYPCHAR = STRING(valdfasttemp.TYP).
         
      END.
      ELSE DELETE valdfasttemp.
      
      IF AVAILABLE valdfasttemp THEN DO:
         FIND FIRST ARENDEAONR WHERE ARENDEAONR.ARENDENR = valdfasttemp.KALKNR NO-LOCK NO-ERROR.
         IF AVAILABLE ARENDEAONR THEN DO:
            BUFFER-COPY ARENDEAONR TO valdfasttemp.
            valdfasttemp.KALKNR = ARENDEAONR.ARENDENR.
         END.   
         
      END.
   END.
END PROCEDURE.

PROCEDURE aonrhmtkalk_UI :
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER aonrplanin AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR gatill.
   EMPTY TEMP-TABLE gatill NO-ERROR. 
   IF arendekalkin = "ÄRENDE" THEN DO:
      OPEN QUERY fq FOR EACH ARENDEAONR WHERE ARENDEAONR.AONR = aonrvar AND
      ARENDEAONR.DELNR = delnrvar AND ARENDEAONR.STATUSNIV = arendekalkin NO-LOCK.      
   END.
         
   GET FIRST fq NO-LOCK.
   DO WHILE AVAILABLE(ARENDEAONR):
      FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = ARENDEAONR.ARENDENR AND ARENDEHUV.OMRADE = ARENDEAONR.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE ARENDEHUV THEN DO:
         IF arendekalkin = "ÄRENDE" THEN DO:
            CREATE gatill.
            ASSIGN
            gatill.TYP = ARENDEAONR.TYP
            gatill.TYPCHAR = arendekalkin
            gatill.F1 = "Ärende"
            gatill.F2 = STRING(ARENDEAONR.ARENDENR)         
            gatill.F3 = ARENDEHUV.BENAMNING
            gatill.STATUSNIV = ARENDEAONR.STATUSNIV.
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
   FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = kalknrvar NO-LOCK NO-ERROR.
   IF AVAILABLE ARENDEHUV THEN DO:
      typvar = ARENDEHUV.UTYP.   
   END.
END PROCEDURE.
/*sök på nr + omr*/
PROCEDURE sokkalkylomr_UI :
   DEFINE INPUT  PARAMETER kalknrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.   
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   FIND FIRST ARENDEAONR WHERE ARENDEAONR.ARENDENR = kalknrvar AND ARENDEAONR.OMRADE = omrvar NO-LOCK NO-ERROR.
   IF AVAILABLE ARENDEAONR THEN DO:
      CREATE eutvaldfasttemp. 
      RUN kalktyp_UI(INPUT 1).      
   END.
   RUN aonrsekkoll_UI (INPUT 1).
END PROCEDURE.


/*sök på nr */
PROCEDURE sokkalkyl_UI :
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER kalknrvar AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   IF arendekalkin = "ÄRENDE" THEN DO:
      FIND FIRST ARENDEAONR WHERE ARENDEAONR.ARENDENR = kalknrvar AND ARENDEAONR.STATUSNIV = arendekalkin NO-LOCK NO-ERROR.
   END.
        
   IF AVAILABLE ARENDEAONR THEN DO: 
      CREATE eutvaldfasttemp.
      RUN kalktyp_UI(INPUT 1).      
   END.
   RUN aonrsekkoll_UI (INPUT 1).   
END PROCEDURE.

PROCEDURE Arendevisakoll_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   DEFINE OUTPUT PARAMETER felmedd AS CHARACTER NO-UNDO.
   DEFINE VARIABLE typvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE klogvar AS INTEGER NO-UNDO.
   FOR EACH eutvaldfasttemp WHERE NO-LOCK:
      FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = eutvaldfasttemp.KALKNR AND ARENDEHUV.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
      IF typvar = 0 THEN DO:
         ASSIGN 
         typvar = ARENDEHUV.TYPKALK
         klogvar = ARENDEHUV.KLOGID.
      END.
      IF typvar = ARENDEHUV.TYPKALK THEN.
      ELSE DO:
         felmedd = "Du kan inte blanda olika typer!".
         RETURN.
      END.       
      IF klogvar = ARENDEHUV.KLOGID THEN.
      ELSE DO:
         felmedd = "Arenden som har olika kataloger kan inte visas tillsammans!".
         RETURN.
      END.    
   END.
   RUN aonrsekkoll_UI (INPUT 1).
END PROCEDURE.


/*FAVORITER*/
PROCEDURE Kalkfavo_UI :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   FOR EACH eutvaldfasttemp NO-LOCK:
      FIND FIRST ARENDEAONR WHERE ARENDEAONR.ARENDENR = eutvaldfasttemp.KALKNR AND ARENDEAONR.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE ARENDEAONR THEN RUN kalktyp_UI(INPUT 1). 
      ELSE DELETE eutvaldfasttemp.
   END.
   RUN aonrsekkoll_UI (INPUT 1).
END PROCEDURE.
/*sök på aonr */
PROCEDURE sokaonrkalkyl_UI :
   
   DEFINE INPUT  PARAMETER arendekalkin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER aonrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER delnrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER allavar AS LOGICAL NO-UNDO.
   DEFINE INPUT  PARAMETER skapatom AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR eutvaldfasttemp.
   EMPTY TEMP-TABLE eutvaldfasttemp NO-ERROR. 
   IF arendekalkin = "ÄRENDE" THEN DO:
      IF allavar = FALSE THEN DO:
         FOR EACH ARENDEAONR WHERE ARENDEAONR.AONR = aonrvar AND
         ARENDEAONR.DELNR = delnrvar AND ARENDEAONR.STATUSNIV = arendekalkin NO-LOCK:
            CREATE eutvaldfasttemp.
            RUN kalktyp_UI(INPUT 1).
         END.
         IF skapatom = TRUE THEN DO:
            FIND FIRST eutvaldfasttemp WHERE eutvaldfasttemp.AONR = aonrvar AND eutvaldfasttemp.DELNR = delnrvar NO-LOCK NO-ERROR.
                        
         END.   
      END.
      ELSE DO:
         FOR EACH ARENDEAONR WHERE ARENDEAONR.AONR = aonrvar AND ARENDEAONR.STATUSNIV = arendekalkin NO-LOCK:
            CREATE eutvaldfasttemp.
            RUN kalktyp_UI(INPUT 1).
         END.
      END.
   END.
         
   RUN aonrsekkoll_UI (INPUT 1).
   
END PROCEDURE.

/*sök på urval */
PROCEDURE UrvalArende_UI :
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
   CREATE BUFFER orgtabh FOR TABLE "ARENDEAONR".
     
   ASSIGN 
   kommandoquery = " ".
   FIND FIRST uppkalktemp NO-ERROR.
   IF uppkalktemp.AKIN = 1 THEN DO:
      kommandoquery = "ARENDEAONR.AKTIV = TRUE ".      
   END.
   ELSE IF uppkalktemp.AKIN = 2 THEN DO:
      kommandoquery = "ARENDEAONR.AKTIV = FALSE ".      
   END.
   ELSE IF uppkalktemp.AKIN = 3 THEN DO:
      kommandoquery = " ".      
   END.
   IF uppkalktemp.OMRADE = ? THEN.
   ELSE IF uppkalktemp.OMRADE NE "ALLA" THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " ARENDEAONR.OMRADE = " +  "'" + uppkalktemp.OMRADE + "'".
   END.
   IF uppkalktemp.TYP NE 0 THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " ARENDEAONR.TYP = " + STRING(uppkalktemp.TYP).
   END.
   IF uppkalktemp.AONR = TRUE THEN DO: 
      RUN and_UI.
      kommandoquery = kommandoquery + " ARENDEAONR.AONR = ?".
   END.
   IF arendekalkin = "ÄRENDE" THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " ARENDEAONR.STATUSNIV = '" + arendekalkin + "'".     
   END.   
   
   kommandoquery = "FOR EACH " + "ARENDEAONR" + " WHERE " + kommandoquery + " NO-LOCK".
   
   RUN CreateCustomQuery(INPUT orgtabh,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      tttabh:BUFFER-CREATE().
      tttabh:BUFFER-COPY(orgtabh). 
      tttabh:BUFFER-FIELD("KALKNR"):BUFFER-VALUE = orgtabh:BUFFER-FIELD("ARENDENR"):BUFFER-VALUE.    
      qH:GET-NEXT().
   END.
   RUN CloseCustomQuery(INPUT qH).
   
   FOR EACH eutvaldfasttemp WHERE NO-LOCK:
      FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = eutvaldfasttemp.KALKNR AND ARENDEHUV.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
      IF AVAILABLE ARENDEHUV THEN RUN kalktyp_UI(INPUT 2).
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
                     IF AVDELNING.POSTANST = uppavdjud.JUDID THEN.
                     ELSE tttabh:BUFFER-DELETE().                                        
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
   
   IF VALID-HANDLE(orgtabh) THEN DELETE OBJECT orgtabh NO-ERROR.
   orgtabh = ?.
END PROCEDURE.

PROCEDURE kalktyp_UI :
   DEFINE INPUT  PARAMETER vad AS INTEGER NO-UNDO.
   IF vad = 1 THEN DO:
      FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = ARENDEAONR.ARENDENR AND ARENDEHUV.OMRADE = ARENDEAONR.OMRADE NO-LOCK NO-ERROR.
      BUFFER-COPY ARENDEAONR TO eutvaldfasttemp.
      eutvaldfasttemp.KALKNR = ARENDEAONR.ARENDENR.
   END.
       
   IF AVAILABLE ARENDEHUV THEN DO: 
      BUFFER-COPY ARENDEHUV TO eutvaldfasttemp.
      eutvaldfasttemp.KALKNR = ARENDEHUV.ARENDENR.   
      eutvaldfasttemp.TYPCHAR = STRING(eutvaldfasttemp.TYP).
   END.
   FIND FIRST KALKYLKATALOG WHERE KALKYLKATALOG.KLOGID = ARENDEHUV.KLOGID NO-LOCK NO-ERROR.
   IF KALKYLKATALOG.KATALOGTYP = "Ärende" THEN eutvaldfasttemp.VIKATAR = KALKYLKATALOG.VISARTAL.  
   eutvaldfasttemp.KATAR = eutvaldfasttemp.VIKATAR.
   IF eutvaldfasttemp.VIKATAR = ? OR eutvaldfasttemp.VIKATAR < 0 THEN DO:
      ASSIGN
      eutvaldfasttemp.VIKATAR = 0
      eutvaldfasttemp.KATAR = 0.
   END.     
END PROCEDURE.
/*FÅR DU SE DENNA KALKYL*/
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
                  FIND FIRST ARENDEHUV WHERE ARENDEHUV.ARENDENR = eutvaldfasttemp.KALKNR AND ARENDEHUV.OMRADE = eutvaldfasttemp.OMRADE NO-LOCK NO-ERROR.
                  IF AVAILABLE ARENDEHUV THEN DO:
                     IF ARENDEHUV.ANVANDARE = Guru.Konstanter:globanv AND eutvaldfasttemp.AONR = ? THEN.
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



/*============ALLA KALYLER DU JOBBAT MED I KALKYL.CLS==========*/

PROCEDURE anvkalkyl_UI :
   DEFINE INPUT  PARAMETER kalknrvar AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrvar AS CHARACTER NO-UNDO.
   FIND FIRST anvkalkyltt WHERE anvkalkyltt.ARENDENR = kalknrvar AND anvkalkyltt.OMRADE = omrvar NO-LOCK NO-ERROR.
   IF NOT AVAILABLE anvkalkyltt THEN DO:
      CREATE anvkalkyltt.
      ASSIGN
      anvkalkyltt.ARENDENR = kalknrvar
      anvkalkyltt.OMRADE = omrvar.
   END.
END PROCEDURE.

PROCEDURE anvkalkylhmt_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR anvkalkyltt.
   
END PROCEDURE.

PROCEDURE anvkalkylbort_UI :
   EMPTY TEMP-TABLE  anvkalkyltt NO-ERROR. 
END PROCEDURE.


/*============FÖR ATT SKAPA QUERY===========*/
PROCEDURE CreateCustomQuery:
   DEFINE INPUT PARAMETER tth  AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER q AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CREATE QUERY CustomQueryh.
   CustomQueryh:SET-BUFFERS(tth).
   CustomQueryh:QUERY-PREPARE(q).
   CustomQueryh:QUERY-OPEN().
END PROCEDURE.
   
PROCEDURE CloseCustomQuery:
   DEFINE INPUT PARAMETER CustomQueryh AS HANDLE NO-UNDO.
   CustomQueryh:QUERY-CLOSE() NO-ERROR.
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