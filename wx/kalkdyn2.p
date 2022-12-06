
/*------------------------------------------------------------------------
    File        : kalkdyn2.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Dec 15 10:16:16 CET 2015
    Notes       :
  ----------------------------------------------------------------------*/
{KALKYLKAT.I}
DEFINE  VARIABLE HuvudTTh              AS HANDLE NO-UNDO.
   DEFINE  VARIABLE IngKatalogTTh              AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KatalogTTh            AS HANDLE NO-UNDO.
   DEFINE  VARIABLE Katalogsubtth         AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KatalogDeltth         AS HANDLE NO-UNDO.
   DEFINE  VARIABLE PriserTTh             AS HANDLE NO-UNDO.
   DEFINE  VARIABLE VisningTTh            AS HANDLE NO-UNDO.
   DEFINE  VARIABLE ValdaPriserTTh        AS HANDLE NO-UNDO.
   DEFINE  VARIABLE ArbetskoderTTh        AS HANDLE NO-UNDO.
   DEFINE  VARIABLE LopposterTTh          AS HANDLE NO-UNDO.
   DEFINE  VARIABLE LopsubTTh             AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KoderTTh              AS HANDLE NO-UNDO.
   DEFINE  VARIABLE MarkningTTh           AS HANDLE NO-UNDO.
   DEFINE  VARIABLE kalkantalTTh          AS HANDLE NO-UNDO.
   DEFINE  VARIABLE kalkkostnadTTh        AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KalkRubrikTTh         AS HANDLE NO-UNDO.
   DEFINE  VARIABLE EgnaPriserTTh         AS HANDLE NO-UNDO.
   DEFINE  VARIABLE FaktorerTTh           AS HANDLE NO-UNDO.
   DEFINE  VARIABLE kalkaonrTTh           AS HANDLE NO-UNDO.
   DEFINE  VARIABLE Avtalskalktth         AS HANDLE NO-UNDO.
   DEFINE  VARIABLE AvtalKodertth         AS HANDLE NO-UNDO.
   DEFINE  VARIABLE kalktmtrlTTh          AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KalkylimportTTh       AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KaladmimportTTh       AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KalkmallHuvudtth      AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KalkmallKodertth      AS HANDLE NO-UNDO.
   DEFINE  VARIABLE KalkmallValdtth       AS HANDLE NO-UNDO.
   DEFINE  VARIABLE VolymberTTh           AS HANDLE NO-UNDO.
   DEFINE  VARIABLE FrekvensTTh           AS HANDLE NO-UNDO.
   DEFINE  VARIABLE tiduth                AS HANDLE NO-UNDO.
   DEFINE  VARIABLE berkalkmtrltth        AS HANDLE NO-UNDO.
   DEFINE  VARIABLE kalkanvtth            AS HANDLE NO-UNDO.
   DEFINE  VARIABLE kalkttidlageTTh       AS HANDLE NO-UNDO.
   
   DEFINE  VARIABLE ArbKalkhuvTTh     AS HANDLE   NO-UNDO.
   DEFINE  VARIABLE ArbRubrikTTh      AS HANDLE   NO-UNDO.
   DEFINE  VARIABLE ArbKalkKoderTidTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbKalkKoderKostTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbKalkKoderTTh   AS HANDLE  NO-UNDO.
   
   DEFINE  VARIABLE ArbKalkKoderTTbuffh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbKalkKoderTTbuff2h   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbKalkAnmTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbEgnaPriserTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbFaktorerTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbMtrlTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbBerMtrlTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbFrekTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbKomTTh   AS HANDLE  NO-UNDO.
   DEFINE  VARIABLE ArbKalkTTh   AS HANDLE  NO-UNDO. 
   
      
 
DEFINE VARIABLE huvbuffh AS HANDLE NO-UNDO.
huvbuffh = TEMP-TABLE kalkhuvtt:HANDLE:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE huvTEMPH AS HANDLE NO-UNDO.
CREATE TEMP-TABLE huvTEMPH.
huvTEMPH:CREATE-LIKE(huvbuffh).
huvTEMPH:TEMP-TABLE-PREPARE("huvtt").
HuvudTTh = huvTEMPH:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE numbuffh AS HANDLE NO-UNDO.
numbuffh = TEMP-TABLE kalknumtt:HANDLE:DEFAULT-BUFFER-HANDLE.
DEFINE VARIABLE numTEMPH AS HANDLE NO-UNDO.
CREATE TEMP-TABLE numTEMPH.
numTEMPH:CREATE-LIKE(numbuffh).
numTEMPH:TEMP-TABLE-PREPARE("numtt").
KoderTTh = numTEMPH :DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE numsubbuffh AS HANDLE NO-UNDO.
numsubbuffh = TEMP-TABLE kalknumsubtt:HANDLE:DEFAULT-BUFFER-HANDLE.
DEFINE VARIABLE numsubTEMPH AS HANDLE NO-UNDO.
CREATE TEMP-TABLE numsubTEMPH.
numsubTEMPH:CREATE-LIKE(numsubbuffh).
numsubTEMPH:TEMP-TABLE-PREPARE("numsubtt").
ValdaPriserTTh = numsubTEMPH:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE aonrbuffh AS HANDLE NO-UNDO.
aonrbuffh = TEMP-TABLE kalkaonrTT:HANDLE:DEFAULT-BUFFER-HANDLE.
DEFINE VARIABLE aonrTEMPH AS HANDLE NO-UNDO.
CREATE TEMP-TABLE aonrTEMPH.
aonrTEMPH:CREATE-LIKE(aonrbuffh).
aonrTEMPH:TEMP-TABLE-PREPARE("aonrtt").
kalkaonrTTh = aonrTEMPH:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE faktbuffh AS HANDLE NO-UNDO.
faktbuffh = TEMP-TABLE kalkfaktorertt:HANDLE:DEFAULT-BUFFER-HANDLE.
DEFINE VARIABLE faktTEMPH AS HANDLE NO-UNDO.
CREATE TEMP-TABLE faktTEMPH.
faktTEMPH:CREATE-LIKE(faktbuffh).
faktTEMPH:TEMP-TABLE-PREPARE("fakttt").
FaktorerTTh = faktTEMPH:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE eprisbuffh AS HANDLE NO-UNDO.
eprisbuffh = TEMP-TABLE kalkegnaprisertt:HANDLE:DEFAULT-BUFFER-HANDLE.
DEFINE VARIABLE eprisTEMPH AS HANDLE NO-UNDO.
CREATE TEMP-TABLE eprisTEMPH.
eprisTEMPH:CREATE-LIKE(eprisbuffh).
eprisTEMPH:TEMP-TABLE-PREPARE("epristt").
EgnaPriserTTh = eprisTEMPH:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE mtrlbuffh AS HANDLE NO-UNDO.
mtrlbuffh = TEMP-TABLE kalktmtrlTT:HANDLE:DEFAULT-BUFFER-HANDLE.
DEFINE VARIABLE mtrlTEMPH AS HANDLE NO-UNDO.
CREATE TEMP-TABLE mtrlTEMPH.
mtrlTEMPH:CREATE-LIKE(mtrlbuffh).
mtrlTEMPH:TEMP-TABLE-PREPARE("mtrltt").
kalktmtrlTTh =  mtrlTEMPH:DEFAULT-BUFFER-HANDLE.

DEFINE VARIABLE tidbuffh AS HANDLE NO-UNDO.
tidbuffh = TEMP-TABLE kalkttidlageTT:HANDLE:DEFAULT-BUFFER-HANDLE.
DEFINE VARIABLE tidTEMPH AS HANDLE NO-UNDO.
CREATE TEMP-TABLE tidTEMPH.
tidTEMPH:CREATE-LIKE(tidbuffh).
tidTEMPH:TEMP-TABLE-PREPARE("tidtt").
kalkttidlageTTh = tidTEMPH:DEFAULT-BUFFER-HANDLE.   
    

/*KalkylDS*/

DEFINE VARIABLE dyndamicDSh AS HANDLE NO-UNDO.
DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE KalkylDS AS HANDLE NO-UNDO.
RUN DynamicDataSet.p PERSISTENT SET dyndamicDSh.
{DataSetVariable.I}

antaltab = 8.
pcBuffers[1] = STRING(HuvudTTh). 
pcBuffers[2] = STRING(KoderTTh).
pcBuffers[3] = STRING(ValdaPriserTTh).
pcBuffers[4] = STRING(kalkaonrTTh).
pcBuffers[5] = STRING(FaktorerTTh).
pcBuffers[6] = STRING(EgnaPriserTTh).
pcBuffers[7] = STRING(kalktmtrlTTh).
pcBuffers[8] = STRING(kalkttidlageTTh).
pcRelFields[1] = "KALKNR,KALKNR,OMRADE,OMRADE".
pcRelFields[2] = "KALKNR,KALKNR,OMRADE,OMRADE,NUM,NUM".
pcRelFields[3] = "KALKNR,KALKNR,OMRADE,OMRADE".
pcRelFields[4] = "KALKNR,KALKNR,OMRADE,OMRADE".
pcRelFields[5] = "KALKNR,KALKNR,OMRADE,OMRADE".
pcRelFields[6] = "KALKNR,KALKNR,OMRADE,OMRADE".
pcRelFields[7] = "KALKNR,KALKNR,OMRADE,OMRADE".
/*
pcRelTables[1] = "1,2".
pcRelTables[2] = "2,3".
pcRelTables[3] = "1,4".
pcRelTables[4] = "1,5".
pcRelTables[5] = "1,6".
pcRelTables[6] = "1,7".
pcRelTables[7] = "1,8".
*/
pcRelTables[2] = "2,3".
pcSources[1] = "KALKHUV".
pcSources[2] = "KALKNUM".
pcSources[3] = "KALKNUMSUB".
pcSources[4] = "KALKAONR".
pcSources[5] = "KALKFAKTORER".
pcSources[6] = "KALKEGNAPRISER".
pcSources[7] = "KALKMTRL".
pcSources[8] = "KALKYLTIDLAGE".
pcSourceKeys[1] = "KALKNR,OMRADE".
pcSourceKeys[2] = "KALKNR,OMRADE,NUM".
pcSourceKeys[3] = "KALKNR,OMRADE,NUM,NUMSUBID".
pcSourceKeys[4] = "KALKNR,OMRADE,PLANNR,ARTAL".
pcSourceKeys[5] = "KLOGSUBID,KALKNR,OMRADE,KPID".
pcSourceKeys[6] = "KLOGSUBID,KALKNR,OMRADE,KPID".
pcSourceKeys[7] = "KALKNR,OMRADE,MID".
pcSourceKeys[8] = "KALKNR,OMRADE,IDTIDLAG".
pcKeyValue[1] = "KALKHUV.KALKNR = 100848 AND KALKHUV.OMRADE = '0910'".

RUN DefAndLoadDs_UI IN dyndamicDSh 
     ({DataSetInput.I} OUTPUT DATASET-HANDLE KalkylDS BIND).

   queryvar = "FOR EACH " + KoderTTh:TABLE + " NO-LOCK".
   RUN CreateCustomQuery(INPUT KoderTTh,INPUT queryvar,OUTPUT qh).
   qH:GET-FIRST(NO-LOCK).
   DO WHILE qH:QUERY-OFF-END = FALSE:
      DO TRANSACTION:
         DISPLAY 
         KoderTTh:BUFFER-FIELD("ARBKOD"):BUFFER-VALUE
         KoderTTh:BUFFER-FIELD("lopnr"):BUFFER-VALUE WITH FRAME cc down. 
         DOWN 1 WITH FRAME cc.   
           
      END.
      qH:GET-NEXT(NO-LOCK).
   END.
   
   RUN CloseCustomQuery(INPUT qH).



/*
RUN ChDsSave_UI.

PROCEDURE ChDsSave_UI :
   DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
   kalknumanvegenTEMPH:TRACKING-CHANGES = FALSE.
   kalknumanvegensubTEMPH:TRACKING-CHANGES = FALSE.

   CREATE DATASET hDSChanges.
   hDSChanges:CREATE-LIKE (KalkylAnvEgenDS).
   hDSChanges:GET-CHANGES (KalkylAnvEgenDS).
   
   
   DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest.xml". 
      hDSChanges:WRITE-XML("FILE", SPARAXML). 
  
   
   
   DEFINE VARIABLE iBuff AS INTEGER NO-UNDO.
   RUN SPARADATSET.p (INPUT hDSChanges).
   DO iBuff = 1 TO KalkylAnvEgenDS:NUM-BUFFERS:
      hDSChanges:GET-BUFFER-HANDLE(iBuff):DETACH-DATA-SOURCE().
   END.

   
   
   hDSChanges:MERGE-CHANGES(KalkylAnvEgenDS).
    kalknumanvegenTEMPH:TRACKING-CHANGES = TRUE.
 kalknumanvegensubTEMPH:TRACKING-CHANGES = TRUE.

           
  
END PROCEDURE.
*/
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

PROCEDURE dstest_UI :
   DEFINE VARIABLE dsTEMPH AS HANDLE NO-UNDO.
   DEFINE VARIABLE dsTTh AS HANDLE NO-UNDO.
   CREATE TEMP-TABLE dsTEMPH.
   dsTEMPH:ADD-NEW-FIELD("enr","character").
   dsTEMPH:ADD-NEW-FIELD("antal","character").
   dsTEMPH:TEMP-TABLE-PREPARE("ekalknumanvegentt").
   dsTTh = dsTEMPH:DEFAULT-BUFFER-HANDLE.
   DEFINE VARIABLE DSChanges AS HANDLE NO-UNDO. 
   CREATE DATASET DSChanges.
   DSChanges:ADD-BUFFER(dsTTh).
   dsTEMPH:TRACKING-CHANGES = TRUE.
   dsTTh:BUFFER-CREATE().
   dsTTh:BUFFER-FIELD("enr"):BUFFER-VALUE = "33".
   DEFINE VARIABLE hDSChanges AS HANDLE NO-UNDO.
   dsTEMPH:TRACKING-CHANGES = FALSE.
   DEFINE VARIABLE hBeforeBuff AS HANDLE NO-UNDO.
   
    hBeforeBuff = dsTTh:BEFORE-BUFFER.
   MESSAGE hBeforeBuff:NAME 
   VIEW-AS ALERT-BOX.
   CREATE DATASET hDSChanges.
   hDSChanges:CREATE-LIKE (DSChanges).
   hDSChanges:GET-CHANGES (DSChanges).
   
   
   DEFINE VARIABLE SPARAXML AS CHARACTER NO-UNDO.
      SPARAXML = "C:\CTest.xml". 
      hDSChanges:WRITE-XML("FILE", SPARAXML). 
  
   
  
END PROCEDURE.