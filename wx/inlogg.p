
/*------------------------------------------------------------------------
    File        : inlogg.p
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Apr 25 16:04:00 CEST 2017
    Notes       :
  ----------------------------------------------------------------------*/

/* ***************************  Definitions  ************************** */
/*
DEFINE VARIABLE cTargetType     AS CHARACTER NO-UNDO.
DEFINE VARIABLE cFile           AS CHARACTER NO-UNDO.
DEFINE VARIABLE lFormatted      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE cEncoding       AS CHARACTER NO-UNDO.
DEFINE VARIABLE cSchemaLocation AS CHARACTER NO-UNDO.
DEFINE VARIABLE lWriteSchema    AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lMinSchema      AS LOGICAL   NO-UNDO.
DEFINE VARIABLE lRetOK          AS LOGICAL   NO-UNDO.

DEFINE TEMP-TABLE intttt NO-UNDO
  FIELD datum AS date
  FIELD fore AS CHARACTER
  FIELD  antal AS INTEGER.
  


DEFINE DATASET inttDS FOR intttt. 
  */
/*
WRITE-XMLSCHEMA
  
ASSIGN 
cTargetType    =  "FILE"
cFile           = "C:\PROTEMP11\inttDS.XSD"
lFormatted      = TRUE
cEncoding       = "iso8859-1"
lWriteSchema = TRUE 
lMinSchema = TRUE.
lRetOK = DATASET  inttDS:WRITE-XMLSCHEMA(cTargetType, cFile, lFormatted,  cEncoding, lMinSchema).

DATASET inttDS:READ-XML ("FILE", "Y:\Löpande\2016-03-02 GPL (Guru Personalliggare)\l2017.xml", "MERGE", ?,False).
DEFINE VARIABLE mest AS INTEGER NO-UNDO.

FOR EACH intttt BREAK BY intttt.datum:
         /*ACCUMULATE intttt.antal (TOTAL BY intttt.datum).*/
         ACCUMULATE intttt.datum (COUNT BY intttt.datum).
         IF LAST-OF(intttt.datum) THEN DO:
            DISPLAY (ACCUM COUNT BY intttt.datum intttt.datum) intttt.datum mest.
            IF (ACCUM COUNT BY intttt.datum intttt.datum) > mest THEN DO:
               mest = (ACCUM COUNT BY intttt.datum intttt.datum).
            END.
                    
         END.
      END.

DISPLAY mest.
*/
/*
kalkylds

DEFINE DATASET KalkylDS FOR kalkhuvtt,kalknumtt,kalknumsubtt,kalkaonrTT,kalkfaktorertt,kalkegnaprisertt,kalktmtrlTT,kalkttidlageTT
DATA-RELATION huvNumsDR FOR kalkhuvtt, kalknumtt RELATION-FIELDS (kalkhuvtt.KALKNR,kalknumtt.KALKNR,kalkhuvtt.OMRADE,kalknumtt.OMRADE)
DATA-RELATION numSubsDR FOR kalknumtt, kalknumsubtt RELATION-FIELDS (kalknumtt.KALKNR,kalknumsubtt.KALKNR,kalknumtt.OMRADE,kalknumsubtt.OMRADE,kalknumtt.NUM,kalknumsubtt.NUM)
DATA-RELATION huvAonrDR FOR kalkhuvtt, kalkaonrTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkaonrTT.KALKNR,kalkhuvtt.OMRADE,kalkaonrTT.OMRADE)
 /*EXTRA KLOGSUBID*/
DATA-RELATION huvFaktDR FOR kalkhuvtt, kalkfaktorertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkfaktorertt.KALKNR,kalkhuvtt.OMRADE,kalkfaktorertt.OMRADE)
 /*EXTRA KLOGSUBID*/
DATA-RELATION huvEgnaDR FOR kalkhuvtt, kalkegnaprisertt RELATION-FIELDS (kalkhuvtt.KALKNR,kalkegnaprisertt.KALKNR,kalkhuvtt.OMRADE,kalkegnaprisertt.OMRADE)
DATA-RELATION huvMtrlDR FOR kalkhuvtt, kalktmtrlTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalktmtrlTT.KALKNR,kalkhuvtt.OMRADE,kalktmtrlTT.OMRADE)
DATA-RELATION huvTidlDR FOR kalkhuvtt, kalkttidlageTT RELATION-FIELDS (kalkhuvtt.KALKNR,kalkttidlageTT.KALKNR,kalkhuvtt.OMRADE,kalkttidlageTT.OMRADE).


OUTPUT DATASET-HANDLE BerKalkDS BIND,
       OUTPUT DATASET-HANDLE BerValDS BIND,
       OUTPUT DATASET-HANDLE BerMtrlDS BIND,
       OUTPUT DATASET-HANDLE HdSchaktDS BIND).
*/

DEFINE INPUT  PARAMETER globunikdb AS CHARACTER NO-UNDO.
DEFINE INPUT  PARAMETER prisvar AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER DATASET-HANDLE dynDS BIND. 
&Scoped-define PUBLIC
CREATE WIDGET-POOL "DynTable" NO-ERROR.
{EXTRADATA.I}
{EXTRATAB.I}
{KALKYLKAT.I}
{KALKYLKATH.i}
{KALKYLKATTTH.i}
{SparaDynDSstar.I}
{TTEXPIMPHD.I}
{BEREDNINGTEMP.I}
DEFINE VARIABLE beredningbuffTTh AS HANDLE NO-UNDO.

beredningbuffTTh = TEMP-TABLE beredningtemp:HANDLE:DEFAULT-BUFFER-HANDLE.
DEFINE TEMP-TABLE tempnamn NO-UNDO
   FIELD NAMNH AS HANDLE
   FIELD TORGTH AS HANDLE
   FIELD NODTAB AS CHARACTER.

DEFINE VARIABLE dynxmlOE10 AS HANDLE NO-UNDO.
RUN READWRIXMLOE.P PERSISTENT SET dynxmlOE10.
DEFINE VARIABLE bernrv AS INTEGER NO-UNDO.
DEFINE VARIABLE omrv  AS CHARACTER NO-UNDO.
DEFINE VARIABLE knr AS INTEGER NO-UNDO.
DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
DEFINE VARIABLE globanv AS CHARACTER NO-UNDO.

FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
globforetag = FORETAG.FORETAG.
FUNCTION TempOldBer RETURNS HANDLE (INPUT Bertabtab AS CHARACTER):
   FIND FIRST tempnamn WHERE tempnamn.NODTAB = Bertabtab NO-LOCK NO-ERROR.
   RETURN tempnamn.NAMNH.
END FUNCTION. /* detachDataSet */
bernrv = 1237.
omrv = "0910".
RUN Tnamn_UI.
FIND FIRST BERKALKOPPLA WHERE BERKALKOPPLA.BERNR = bernrv AND BERKALKOPPLA.OMRADE = omrv NO-LOCK NO-ERROR.
IF AVAILABLE BERKALKOPPLA THEN knr = BERKALKOPPLA.KALKNR.
RUN laddaBerkalkDS_UI (INPUT bernrv, INPUT knr, INPUT omrv, OUTPUT DATASET-HANDLE dynDS BIND  ).

RUN skapaexport_UI (INPUT bernrv, INPUT knr, INPUT omrv).
DEFINE VARIABLE iEntry AS INTEGER NO-UNDO.
EMPTY TEMP-TABLE tempnamn NO-ERROR. 
REPEAT: 
   iEntry = iEntry + 1.
   IF dynDS:GET-BUFFER-HANDLE(iEntry) = ? THEN LEAVE.
   ELSE DO:
      CREATE tempnamn.
      tempnamn.NAMNH = dynDS:GET-BUFFER-HANDLE(iEntry).
      tempnamn.NODTAB = tempnamn.NAMNH:TABLE.
   END.
   
END.
FOR EACH tempnamn WHERE NO-LOCK:
   DISPLAY tempnamn.NODTAB. 
   END.

RUN utxml_UI.

PROCEDURE utxml_UI :
   /*Anders Olsson Elpool i Umeå AB  4 maj 2017 10:56:24 
   SKA KÖRAS PÅ KLINTEN 
   */
   DEFINE VARIABLE mvarhj AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fildir AS CHARACTER FORMAT "X(256)" NO-UNDO.
   DEFINE VARIABLE filnamn AS CHARACTER NO-UNDO.
   Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79).
   mvarhj = "$.xml".
   FIND FIRST beredningtemp WHERE NO-LOCK NO-ERROR.
   fildir = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
   {SESSIONTEMPDIR.I}
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN fildir = webclienttempdir.
   filnamn = fildir + beredningtemp.BERAONR + "!" + beredningtemp.OMRADE + "!" + beredningtemp.BENAMNING + mvarhj .
   RUN WritexmlDS_UI IN dynxmlOE10 (INPUT filnamn,INPUT DATASET-HANDLE dynDS BIND).
END PROCEDURE.
PROCEDURE laddaBerkalkDS_UI :
   DEFINE INPUT  PARAMETER bernrvad AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER kalknrvad AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrnrvad AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER DATASET-HANDLE BerKalkDS BIND. 
   DEFINE VARIABLE brtbnr AS INTEGER NO-UNDO.
   brtbnr = 0.
   RUN BerKalkylCreate_UI.
   
   RUN GetDatasetDeftt_UI ("BerKalkDS").
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "tempglobforetag".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).
   DatasetDeftt.pcSources[brtbnr] = "FORETAG".        
   DatasetDeftt.pcSourceKeys[brtbnr] = "FORETAG".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".FORETAG = " + QUOTER(globforetag).
   brtbnr =  brtbnr +  1.
   
   DatasetDeftt.pcSources[brtbnr] = "BEREDNING".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(beredningbuffTTh).
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "BERVAL".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.                                              
   DatasetDeftt.pcSources[brtbnr] = "BERORD".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "FRIKORT".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "BERID".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "BERMTRL".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "BERLINKAB".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "BERUPP".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "BERID2".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "BERPUNKT".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).
   DatasetDeftt.pcSourceKeys[brtbnr] = "AONR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".AONR = " + QUOTER(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).        
  
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDPUNKT".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDSCHAKTFOR".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDSCHAKT".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDKABELLINJE".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDFORLKAB".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDFORLSAM".   
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDPROTKOPPBER".                 
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDSCHAKTKALKSPEC".                 
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDSCHAKTPROT".                 
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).        
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "HDSCHAKTPROTHAND".                 
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   DatasetDeftt.pcSources[brtbnr] = "MARKSTN".   
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSources[brtbnr] = "EXTRAKOPPLINGAR". 
   DatasetDeftt.pcSourceKeys[brtbnr] = "PROGRAM,KOPPLACHAR1,KOPPLACHAR2".
   DatasetDeftt.pcKeyValue[brtbnr] = "EXTRAKOPPLINGAR.PROGRAM = " + QUOTER('MARKSTN') + " AND EXTRAKOPPLINGAR.KOPPLACHAR1 = " + QUOTER(bernrvad) +  
   " AND EXTRAKOPPLINGAR.KOPPLACHAR2 =  "  + QUOTER(omrnrvad).
   IF kalknrvad = 0 THEN.
   ELSE DO: 
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(BerKalkkopplabuffh).
      DatasetDeftt.pcSources[brtbnr] = "BERKALKOPPLA".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "BERKALKOPPLA.KALKNR = " + STRING(kalknrvad) + " AND BERKALKOPPLA.OMRADE = "  + QUOTER(omrnrvad).
      
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(kalkaonrTTh).
      DatasetDeftt.pcSources[brtbnr] = "KALKAONR".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "KALKAONR.KALKNR = " + STRING(kalknrvad) + " AND KALKAONR.OMRADE = "  + QUOTER(omrnrvad).
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(HuvudTTh).
      DatasetDeftt.pcSources[brtbnr] = "KALKHUV".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "KALKHUV.KALKNR = " + STRING(kalknrvad) + " AND KALKHUV.OMRADE = "  + QUOTER(omrnrvad).
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(KoderTTh).
      DatasetDeftt.pcSources[brtbnr] = "KALKNUM".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "KALKNUM.KALKNR = " + STRING(kalknrvad) + " AND KALKNUM.OMRADE = "  + QUOTER(omrnrvad).
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(ValdaPriserTTh).
      DatasetDeftt.pcSources[brtbnr] = "KALKNUMSUB".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "KALKNUMSUB.KALKNR = " + STRING(kalknrvad) + " AND KALKNUMSUB.OMRADE = "  + QUOTER(omrnrvad).
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(FaktorerTTh).
      DatasetDeftt.pcSources[brtbnr] = "KALKFAKTORER".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "KALKFAKTORER.KALKNR = " + STRING(kalknrvad) + " AND KALKFAKTORER.OMRADE = "  + QUOTER(omrnrvad).
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(EgnaPriserTTh).
      DatasetDeftt.pcSources[brtbnr] = "KALKEGNAPRISER".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "KALKEGNAPRISER.KALKNR = " + STRING(kalknrvad) + " AND KALKEGNAPRISER.OMRADE = "  + QUOTER(omrnrvad).
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(kalktmtrlTTh).
      DatasetDeftt.pcSources[brtbnr] = "KALKMTRL".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "KALKMTRL.KALKNR = " + STRING(kalknrvad) + " AND KALKMTRL.OMRADE = "  + QUOTER(omrnrvad).
      brtbnr =  brtbnr +  1.
      DatasetDeftt.pcBuffers[brtbnr] = STRING(kalkttidlageTTh).
      DatasetDeftt.pcSources[brtbnr] = "KALKYLTIDLAGE".
      DatasetDeftt.pcSourceKeys[brtbnr] = "KALKNR,OMRADE".
      DatasetDeftt.pcKeyValue[brtbnr] = "KALKYLTIDLAGE.KALKNR = " + STRING(kalknrvad) + " AND KALKYLTIDLAGE.OMRADE = "  + QUOTER(omrnrvad).
   END.   
   DatasetDeftt.antaltab = brtbnr.
   
    RUN DefAndLoadDs_UI IN dyndamicDSh 
   ({DataSetInput.I} OUTPUT DATASET-HANDLE BerKalkDS BIND).
   /*
   sundsvals kalkber går ej att exportera
   DatasetDeftt.pcSources[brtbnr] = "KALKBER".
   DatasetDeftt.pcBuffers[brtbnr] = STRING(TempOldBer(DatasetDeftt.pcSources[brtbnr])).        
   DatasetDeftt.pcSourceKeys[brtbnr] = "BERNR,OMRADE".
   DatasetDeftt.pcKeyValue[brtbnr] = DatasetDeftt.pcSources[brtbnr] + ".BERNR = " + STRING(bernrvad) + " AND " + DatasetDeftt.pcSources[brtbnr] + ".OMRADE = "  + QUOTER(omrnrvad).
   brtbnr =  brtbnr +  1.
   */
   
  
     
END PROCEDURE.
PROCEDURE skapaexport_UI :
   DEFINE INPUT  PARAMETER bernrvad AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER kalknrvad AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER omrnrvad AS CHARACTER NO-UNDO.
   DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
   DEFINE VARIABLE ekoppdataapph AS HANDLE NO-UNDO.
   DEFINE VARIABLE lnamnex AS CHARACTER NO-UNDO.
   RUN hamtlev_UI (INPUT omrnrvad,INPUT STRING(bernrvad),OUTPUT lnamnex).
   RUN EXTRATABHMT.P PERSISTENT SET ekoppdataapph.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "BERFORE"  
   inextradatatemp.HUVUDCH  = omrnrvad
   inextradatatemp.HUVUDINT = bernrvad.
   
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   FOR EACH tempglobforetag WHERE NO-LOCK:
      ASSIGN 
      tempglobforetag.OMRADE = omrnrvad       
      tempglobforetag.BERNR  = bernrvad
      tempglobforetag.GFORETAGUNIK = globunikdb
      tempglobforetag.GFORETAG = tempglobforetag.FORETAG.
      tempglobforetag.LEVNAMNEX = lnamnex. 
      IF AVAILABLE extradatatemp THEN DO:
         tempglobforetag.GFORETAG = extradatatemp.SOKCHAR[1]. 
      END.
   END.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   EMPTY TEMP-TABLE extradatatemp NO-ERROR.  
   
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "MARKSTN"                   
   inextrakopptemp.KOPPLACHAR1 = STRING(bernrvad)               
   inextrakopptemp.KOPPLAINT1 = ?
   inextrakopptemp.KOPPLACHAR2 = omrnrvad            
   inextrakopptemp.KOPPLAINT2 =  ?.
   RUN etabhamt_UI IN ekoppdataapph (INPUT TABLE inextrakopptemp, OUTPUT TABLE extrakopptemp). 
   FOR EACH extrakopptemp:
      FIND FIRST tempextramarkstn WHERE tempextramarkstn.SOKINT1  = 0 NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tempextramarkstn THEN CREATE tempextramarkstn.
      ASSIGN
      tempextramarkstn.KOPPLACHAR1      = extrakopptemp.KOPPLACHAR1
      tempextramarkstn.KOPPLACHAR2      = extrakopptemp.KOPPLACHAR2
      tempextramarkstn.SOKINT1          = extrakopptemp.SOKINT[1]
      tempextramarkstn.SOKINT2          = extrakopptemp.SOKINT[2]
      tempextramarkstn.SOKCHAR1         = extrakopptemp.SOKCHAR[1].
   END.
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR. 
   EMPTY TEMP-TABLE extrakopptemp NO-ERROR. 
   IF prisvar = TRUE THEN.
   ELSE DO:
      FOR EACH tempbermtrl  WHERE NO-LOCK:
         tempbermtrl.PRIS = 0. 
      END.
   END.
   IF VALID-HANDLE(ekoppdataapph) THEN DELETE PROCEDURE ekoppdataapph.
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.
END PROCEDURE.

PROCEDURE hamtlev_UI :
   DEFINE INPUT PARAMETER valomrade AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER valbernr AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER levnamnex AS CHARACTER NO-UNDO.
   DEFINE VARIABLE lkod AS CHARACTER NO-UNDO.   
   FIND FIRST BETFRIA WHERE BETFRIA.FAKTTEXT = valomrade AND
   BETFRIA.BETNR = valbernr NO-LOCK NO-ERROR. 
   IF AVAILABLE BETFRIA THEN lkod = BETFRIA.TYP.         
   ELSE lkod = "".
   
   IF lkod = "" THEN DO:
      FIND FIRST HUVUDLEV WHERE HUVUDLEV.DEP-NR = 999 NO-LOCK NO-ERROR.
      IF AVAILABLE HUVUDLEV THEN DO:
         ASSIGN  lkod = HUVUDLEV.LEVKOD.
      END.
      ELSE DO:
         FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD NE "0" AND LEVERANTOR.LEVKOD NE "99"
         AND LEVERANTOR.BORTTAG = FALSE NO-LOCK NO-ERROR.
         ASSIGN  lkod = LEVERANTOR.LEVKOD.
      END.                     
   END.      
   FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = lkod NO-LOCK NO-ERROR.
   levnamnex = LEVERANTOR.LEVNAMN.
END PROCEDURE.

PROCEDURE Tnamn_UI :
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER tempberval:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempberval:HANDLE.
      tempnamn.NODTAB = "BERVAL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberord:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempberord:HANDLE.
      tempnamn.NODTAB = "BERORD".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempfrikort:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempfrikort:HANDLE.
      tempnamn.NODTAB = "FRIKORT".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberid:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempberid:HANDLE.
      tempnamn.NODTAB = "BERID".
      /*
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberkalk:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempberkalk:HANDLE.
      tempnamn.NODTAB = "BERKALK".
      */
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempbermtrl:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempbermtrl:HANDLE.
      tempnamn.NODTAB = "BERMTRL".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberlinkab:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempberlinkab:HANDLE.
      tempnamn.NODTAB = "BERLINKAB".
      /*
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempkskydd:HANDLE.   
      tempnamn.TORGTH = TEMP-TABLE tempkskydd:HANDLE.
      tempnamn.NODTAB = "KSKYDD".
      */
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberupp:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempberupp:HANDLE.
      tempnamn.NODTAB = "BERUPP".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberid2:HANDLE.    
      tempnamn.TORGTH = TEMP-TABLE tempberid2:HANDLE.
      tempnamn.NODTAB = "BERID2".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempglobforetag:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempglobforetag:HANDLE.
      tempnamn.NODTAB = "tempglobforetag".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempberpunkt:HANDLE.  
      tempnamn.TORGTH = TEMP-TABLE tempberpunkt:HANDLE.
      tempnamn.NODTAB = "BERPUNKT".
      
      
      
      /*HD HÄR*/
     
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdpunkttemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdpunkttemp:HANDLE.
      tempnamn.NODTAB = "HDPUNKT".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschaktfortemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdschaktfortemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKTFOR".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschakttemp:HANDLE. 
      tempnamn.TORGTH = TEMP-TABLE hdschakttemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKT".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdkabellinjetemp:HANDLE. 
      tempnamn.TORGTH = TEMP-TABLE hdkabellinjetemp:HANDLE.
      tempnamn.NODTAB = "HDKABELLINJE".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdforlkabtemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdforlkabtemp:HANDLE.
      tempnamn.NODTAB = "HDFORLKAB".
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdforlsamtemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdforlsamtemp:HANDLE.
      tempnamn.NODTAB = "HDFORLSAM".   
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdprotkopbertemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdprotkopbertemp:HANDLE.
      tempnamn.NODTAB = "HDPROTKOPPBER".                 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschaktkalkspectemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdschaktkalkspectemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKTKALKSPEC".                 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschakprottemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdschakprottemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKTPROT".                 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdschakprothandtemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdschakprothandtemp:HANDLE.
      tempnamn.NODTAB = "HDSCHAKTPROTHAND".                 
      /*
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdkalkbefbtemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdkalkbefbtemp:HANDLE.
      tempnamn.NODTAB = "KALKBEFB".                 
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER hdkalktemp:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE hdkalktemp:HANDLE.
      tempnamn.NODTAB = "HDKALK".   
      */
      CREATE tempnamn.
      tempnamn.NAMNH = BUFFER tempextramarkstn:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempextramarkstn:HANDLE.
      tempnamn.NODTAB = "MARKSTN".   
      CREATE tempnamn.        
      tempnamn.NAMNH = BUFFER tempextrakalkber:HANDLE.
      tempnamn.TORGTH = TEMP-TABLE tempextrakalkber:HANDLE.
      tempnamn.NODTAB = "KALKBER".
END PROCEDURE.

/*BerKalkylDS*/
PROCEDURE BerKalkylCreate_UI :
   CREATE TEMP-TABLE BerKalkkopplatth IN WIDGET-POOL "DynTable".
   BerKalkkopplatth:CREATE-LIKE("BERKALKOPPLA").
   BerKalkkopplatth:ADD-NEW-FIELD("TTRECID","RECID").
   BerKalkkopplatth:TEMP-TABLE-PREPARE("Berkalkkopptt").
   BerKalkkopplabuffh = BerKalkkopplatth:DEFAULT-BUFFER-HANDLE.
      
   CREATE TEMP-TABLE Bervalltth IN WIDGET-POOL "DynTable".
   Bervalltth:CREATE-LIKE("BERVAL").
   Bervalltth:ADD-NEW-FIELD("TTRECID","RECID").
   Bervalltth:ADD-NEW-FIELD("ID2","CHARACTER").
   Bervalltth:ADD-NEW-FIELD("EXTRA1","CHARACTER").
   Bervalltth:ADD-NEW-FIELD("F1","CHARACTER").
   Bervalltth:ADD-NEW-FIELD("ORD","INTEGER").
   Bervalltth:TEMP-TABLE-PREPARE("Bervaltt").
   Bervallbuffh = Bervalltth:DEFAULT-BUFFER-HANDLE.
   
   
 
   
   CREATE TEMP-TABLE HdSchakttth IN WIDGET-POOL "DynTable".
   HdSchakttth:CREATE-LIKE("HDSCHAKT").
   HdSchakttth:ADD-NEW-FIELD("TTRECID","RECID").
   HdSchakttth:TEMP-TABLE-PREPARE("Schakttt").
   HdSchaktbuffh = HdSchakttth:DEFAULT-BUFFER-HANDLE.
   
END PROCEDURE.


