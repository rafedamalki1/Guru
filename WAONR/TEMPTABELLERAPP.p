/*TEMPTABELLERAPP.p*/
/*Anders Olsson Elpool i Umeå AB  13 okt 2022 11:33:46 
skapar språkposter. meddelanden för cls program, hämtar värden för globalakonstanter gaonr mm.  
*/
DEFINE INPUT PARAMETER globnystart AS LOGICAL NO-UNDO.
DEFINE BUFFER SPRAK_STRANGbuff FOR SPRAK_STRANG.

DEFINE VARIABLE sokstring AS CHARACTER FORMAT "x(40)" EXTENT 10 NO-UNDO.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR. 
Guru.Konstanter:globforetag = FORETAG.FORETAG.

&Scoped-define NEW NEW
{VARFORETYP.I}
&Scoped-define NEW
RUN STYREAPP.P 
(INPUT Guru.Konstanter:globforetag, INPUT-OUTPUT varforetypval, INPUT-OUTPUT varforetypchar, INPUT globnystart).  
{STYREAPPLADD.I}
                
&Scoped-define STATIC
&Scoped-define PUBLIC
{GLOBALTT.I}  
{SPRAKTEMP.I}
{LEVTEMPORDNINGFUNC.I}  

/*Defult värden för ribbon*/
{DEFVALUESNETTT.I}
DEFINE VARIABLE hDataSet AS HANDLE NO-UNDO.
DEFINE DATASET DefaultDS FOR GuruDefaultsTT.
OPEN QUERY gtq FOR EACH SPRAK_STRANG NO-LOCK.
GET FIRST gtq NO-LOCK.
DO WHILE AVAILABLE(SPRAK_STRANG):
   CREATE textsprakstrangtemp.
   BUFFER-COPY SPRAK_STRANG TO textsprakstrangtemp.
   GET NEXT gtq NO-LOCK.
END.

/*NYA SPRÅKSTRÄNGAR*/
/*Anders Olsson Elpool i Umeå AB  13 okt 2022 11:29:15 
   nya globala konstanter i TEMPTABELLERAPP.p
 
*/

FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SOKCHAR = "gdelnrk"  NO-LOCK NO-ERROR.
IF NOT AVAILABLE textsprakstrangtemp THEN DO:
   CREATE textsprakstrangtemp.
   ASSIGN 
   textsprakstrangtemp.SOKCHAR = "gdelnrk"
   textsprakstrangtemp.ID = ?
   textsprakstrangtemp.SOKID = ?
   textsprakstrangtemp.SPRAKID = Guru.Konstanter:globsprak
   textsprakstrangtemp.BENAMNING = "Delnr".
END.
FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SOKCHAR = "gutbytk"  NO-LOCK NO-ERROR.
IF NOT AVAILABLE textsprakstrangtemp THEN DO:
   CREATE textsprakstrangtemp.
   ASSIGN 
   textsprakstrangtemp.SOKCHAR = "gutbytk"
   textsprakstrangtemp.ID = ?
   textsprakstrangtemp.SOKID = ?
   textsprakstrangtemp.SPRAKID = Guru.Konstanter:globsprak
   textsprakstrangtemp.BENAMNING = "Ersättningslista".
END.
   



{SparaProDatasSet.i DefaultDS}
DEFINE QUERY DefaultQuery FOR GURUDEFAULTS .

DEFINE DATA-SOURCE DefaultSrc FOR QUERY DefaultQuery GURUDEFAULTS KEYS (PROGRAM,ANVANDARE,HUVUDINT,HUVUDCHAR,TOOLKEY).

hDataSet = DATASET DefaultDS:HANDLE.   
hDataSet:SET-CALLBACK-PROCEDURE ("AFTER-FILL", "postDataSetFillDefaultDS", THIS-PROCEDURE).

PROCEDURE attachDefaultDS :
   
   hDataSet:GET-BUFFER-HANDLE("GuruDefaultsTT"):ATTACH-DATA-SOURCE(DATA-SOURCE DefaultSrc:HANDLE).
END PROCEDURE.

PROCEDURE FetchDefaultValues:
   DEFINE OUTPUT PARAMETER DATASET FOR DefaultDS.
   DEFINE INPUT  PARAMETER cprog AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ganv AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER huvudintvar AS INTEGER.
   DEFINE INPUT PARAMETER huvudcharvar AS CHARACTER.
   DEFINE VARIABLE queryprep AS CHARACTER NO-UNDO.
   DATASET DefaultDS:EMPTY-DATASET().
   queryprep = "FOR EACH GURUDEFAULTS WHERE GURUDEFAULTS.PROGRAM = '" + cprog + "' AND GURUDEFAULTS.ANVANDARE = '" + ganv +     
   "' AND GURUDEFAULTS.HUVUDINT = " + STRING(huvudintvar) + 
   " AND GURUDEFAULTS.HUVUDCHAR = " + "'" + huvudcharvar + "'" + "  NO-LOCK". 
   QUERY DefaultQuery:QUERY-PREPARE(queryprep).
   RUN attachDefaultDS.
   DATASET DefaultDS:FILL().
   detachDataSetDefaultDS(hDataSet).

END PROCEDURE.

PROCEDURE foretagdefault :
   DEFINE INPUT-OUTPUT PARAMETER TABLE FOR GuruDefaultValuesTT.
   FIND FIRST GuruDefaultValuesTT WHERE NO-LOCK NO-ERROR.
   
   IF GuruDefaultValuesTT.PROGRAM = "Kalkyl" THEN DO:
      /*Koder*/ 
      IF GuruDefaultValuesTT.TOOLGROUP = "Koder" AND GuruDefaultValuesTT.TOOLKEY = "KalkylRibbonMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "KalkylRibbonVisaTyp" THEN GuruDefaultValuesTT.TOOLVALUE = "1". 
      IF GuruDefaultValuesTT.TOOLKEY = "FilterTyp" THEN GuruDefaultValuesTT.TOOLVALUE = "1". 
      IF GuruDefaultValuesTT.TOOLGROUP = "Koder" AND GuruDefaultValuesTT.TOOLKEY = "FriKalkActive" THEN GuruDefaultValuesTT.TOOLVALUE = "no". 
       /*Egna Koder*/ 
      IF GuruDefaultValuesTT.TOOLGROUP = "Egna Koder" AND GuruDefaultValuesTT.TOOLKEY = "KalkylRibbonMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLGROUP = "Egna Koder" AND GuruDefaultValuesTT.TOOLKEY = "FilterTyp" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      /*Inkludera i visning*/ 
      IF GuruDefaultValuesTT.TOOLKEY = "VisHuvud" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".   /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisAnmarkning" THEN GuruDefaultValuesTT.TOOLVALUE = "yes". /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisAnmUtf" THEN GuruDefaultValuesTT.TOOLVALUE = "no". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisFaktorer" THEN GuruDefaultValuesTT.TOOLVALUE = "no". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisEgnaPriser" THEN GuruDefaultValuesTT.TOOLVALUE = "no". 
      /*Visningsinställningar*/ 
      IF GuruDefaultValuesTT.TOOLKEY = "VisVisafor" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisSummera" THEN GuruDefaultValuesTT.TOOLVALUE = "yes". /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisGruppera" THEN GuruDefaultValuesTT.TOOLVALUE = "no". /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisPriser" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisMatSpec" THEN GuruDefaultValuesTT.TOOLVALUE = "no". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "0".  /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisGruppMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "no".  /*vis ok*/
      /*Uppföljning*/ 
      /*
      IF GuruDefaultValuesTT.TOOLKEY = "UppfPriser" THEN GuruDefaultValuesTT.TOOLVALUE = "0".
      */ 
      IF Guru.Konstanter:varforetypchar[12] = "1"  THEN DO: 
         IF GuruDefaultValuesTT.TOOLKEY = "VisFaktorer" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      END.
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      END.
   END.
   IF GuruDefaultValuesTT.PROGRAM = "Arende" THEN DO:
      /*Koder*/ 
      IF GuruDefaultValuesTT.TOOLGROUP = "Koder" AND GuruDefaultValuesTT.TOOLKEY = "KalkylRibbonMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "KalkylRibbonVisaTyp" THEN GuruDefaultValuesTT.TOOLVALUE = "1". 
      IF GuruDefaultValuesTT.TOOLKEY = "FilterTyp" THEN GuruDefaultValuesTT.TOOLVALUE = "1". 
       /*Egna Koder*/ 
      IF GuruDefaultValuesTT.TOOLGROUP = "Egna Koder" AND GuruDefaultValuesTT.TOOLKEY = "KalkylRibbonMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLGROUP = "Egna Koder" AND GuruDefaultValuesTT.TOOLKEY = "FilterTyp" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      /*Inkludera i visning*/ 
      IF GuruDefaultValuesTT.TOOLKEY = "VisHuvud" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".   /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisAnmarkning" THEN GuruDefaultValuesTT.TOOLVALUE = "yes". /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisAnmUtf" THEN GuruDefaultValuesTT.TOOLVALUE = "no". 
     
      /*Visningsinställningar*/ 
      IF GuruDefaultValuesTT.TOOLKEY = "VisVisafor" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisSummera" THEN GuruDefaultValuesTT.TOOLVALUE = "yes". /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisGruppera" THEN GuruDefaultValuesTT.TOOLVALUE = "no". /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisPriser" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisMatSpec" THEN GuruDefaultValuesTT.TOOLVALUE = "no". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "0".  /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisGruppMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "no".  /*vis ok*/
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      END.
   END.
   IF GuruDefaultValuesTT.PROGRAM = "Kalkyladm" THEN DO: 
           
      IF GuruDefaultValuesTT.TOOLGROUP = "Välj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmKod" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "Välj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "Välj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmDelKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "Välj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmPriser" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "Välj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmSek" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "Välj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmFrek" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "Välj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "Välj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      
      IF GuruDefaultValuesTT.TOOLGROUP = "Katalog" AND GuruDefaultValuesTT.TOOLKEY = "Katalog" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "Katalog" AND GuruDefaultValuesTT.TOOLKEY = "Visaartal" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "Katalog" AND GuruDefaultValuesTT.TOOLKEY = "Visasek" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "Katalog" AND GuruDefaultValuesTT.TOOLKEY = "Visadelk" THEN GuruDefaultValuesTT.TOOLVALUE = "NO".
      IF GuruDefaultValuesTT.TOOLGROUP = "Katalog" AND GuruDefaultValuesTT.TOOLKEY = "Visaskapad" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "Katalog" AND GuruDefaultValuesTT.TOOLKEY = "Visaavsl" THEN GuruDefaultValuesTT.TOOLVALUE = "NO".
      
      IF GuruDefaultValuesTT.TOOLGROUP = "DelKatalog" AND GuruDefaultValuesTT.TOOLKEY = "DelKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "DelKatalog" AND GuruDefaultValuesTT.TOOLKEY = "KopplaDelKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "DelKatalog" AND GuruDefaultValuesTT.TOOLKEY = "Visadelskapad" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "DelKatalog" AND GuruDefaultValuesTT.TOOLKEY = "Visdelavsl" THEN GuruDefaultValuesTT.TOOLVALUE = "NO".
     
      IF GuruDefaultValuesTT.TOOLGROUP = "PrisKatalog" AND GuruDefaultValuesTT.TOOLKEY = "PrisKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "PrisKatalog" AND GuruDefaultValuesTT.TOOLKEY = "SokBenPris" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "PrisKatalog" AND GuruDefaultValuesTT.TOOLKEY = "TaEgnaKoder" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "PrisKatalog" AND GuruDefaultValuesTT.TOOLKEY = "TaEgnaPriser" THEN GuruDefaultValuesTT.TOOLVALUE = "NO".
      
      IF GuruDefaultValuesTT.TOOLGROUP = "VisningsRegeler" AND GuruDefaultValuesTT.TOOLKEY = "VisRegel" THEN GuruDefaultValuesTT.TOOLVALUE = "NO".
      
      IF GuruDefaultValuesTT.TOOLKEY = "KalkylRibbonVisaTyp" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "FilterTyp" THEN GuruDefaultValuesTT.TOOLVALUE = "0".
      IF GuruDefaultValuesTT.TOOLKEY = "VisaLopnr" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLKEY = "FrekvensKom" THEN GuruDefaultValuesTT.TOOLVALUE = "NO".
      IF GuruDefaultValuesTT.TOOLKEY = "VisaTypKalk" THEN GuruDefaultValuesTT.TOOLVALUE = "NO".
      
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      END.      
   END.
   IF GuruDefaultValuesTT.PROGRAM = "EkgShell" THEN DO:
      
      
      IF Guru.Konstanter:globforetag = "elpa" THEN DO:
      END. 
   END.        
END PROCEDURE.
/*Defult värden för ribbon*/
/*hämtar kund*/

PROCEDURE bestkund_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR bestkundalltclass .
   EMPTY TEMP-TABLE bestkundalltclass NO-ERROR.       
   OPEN QUERY Bq FOR EACH BESTTAB NO-LOCK.
   GET FIRST Bq NO-LOCK.
   
   DO WHILE AVAILABLE(BESTTAB):
      CREATE bestkundalltclass.
      BUFFER-COPY BESTTAB TO bestkundalltclass.
      bestkundalltclass.TTRECID = RECID(bestkundalltclass).   
      FIND FIRST EXTRADATA WHERE EXTRADATA.PROGRAM = "BESTEPOST" AND EXTRADATA.HUVUDCH = BESTTAB.BESTID NO-LOCK NO-ERROR.
      IF AVAILABLE EXTRADATA  THEN DO:
         ASSIGN 
         bestkundalltclass.AVDELNING  =      EXTRADATA.SOKCHAR[2]
         bestkundalltclass.EPOST      =      EXTRADATA.SOKCHAR[1].
      END.
   
      GET NEXT Bq NO-LOCK.
   END.

END PROCEDURE.
/*hämtar områden*/
PROCEDURE omrade_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR omrtemp .
   EMPTY TEMP-TABLE omrtemp NO-ERROR.          
   OPEN QUERY oq FOR EACH OMRADETAB WHERE OMRADETAB.ELVOMRKOD = 0 USE-INDEX OMR NO-LOCK.
   GET FIRST oq NO-LOCK.
   DO WHILE AVAILABLE(OMRADETAB):
      CREATE omrtemp.
      BUFFER-COPY OMRADETAB TO omrtemp.
      omrtemp.TTRECID = RECID(omrtemp).   
      GET NEXT oq NO-LOCK.
   END.
END PROCEDURE.
PROCEDURE Bortagnaomrade_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR Borttagnaomrtemp .
   EMPTY TEMP-TABLE Borttagnaomrtemp NO-ERROR.       
   
   OPEN QUERY oq FOR EACH OMRADETAB WHERE OMRADETAB.ELVOMRKOD = 1 USE-INDEX OMR NO-LOCK.
   GET FIRST oq NO-LOCK.
   DO WHILE AVAILABLE(OMRADETAB):
      CREATE Borttagnaomrtemp.
      BUFFER-COPY OMRADETAB TO Borttagnaomrtemp.
      Borttagnaomrtemp.TTRECID = RECID(Borttagnaomrtemp).   
      GET NEXT oq NO-LOCK.
   END.
END PROCEDURE.
/*juridisk*/
PROCEDURE jurp_UI :
   DEFINE INPUT  PARAMETER vem AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER TABLE FOR jurperstemp.
   DEFINE OUTPUT PARAMETER TABLE FOR judavdtemp.
   EMPTY TEMP-TABLE  jurperstemp NO-ERROR. 
   EMPTY TEMP-TABLE judavdtemp NO-ERROR. 
      
   IF Guru.Konstanter:varforetypval[18] = 0 OR vem = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN DO:
      FOR EACH JURPERS NO-LOCK:
         CREATE  jurperstemp.
         BUFFER-COPY JURPERS TO jurperstemp.     
      END.          
      FOR EACH AVDELNING WHERE AVDELNING.KOSTMASK = 0 :
         CREATE judavdtemp.
         ASSIGN              
         judavdtemp.AVDELNINGNAMN = AVDELNING.AVDELNINGNAMN
         judavdtemp.JUDID = AVDELNING.POSTANST
         judavdtemp.AVDELNINGNR = AVDELNING.AVDELNINGNR.
      END.
   END. 
   ELSE IF Guru.Konstanter:varforetypval[18] = 1 THEN DO: 
      FOR EACH BOLAGSEK WHERE BOLAGSEK.ANVANDARE = vem NO-LOCK:
         FOR EACH JURPERS WHERE JURPERS.JUDID = BOLAGSEK.OMRADE NO-LOCK:
            CREATE jurperstemp.
            BUFFER-COPY JURPERS TO jurperstemp.     
         END. 
      END.  
      FOR EACH jurperstemp:  
         FOR EACH AVDELNING WHERE AVDELNING.POSTANST = jurperstemp.JUDID AND AVDELNING.KOSTMASK = 0 :
            CREATE judavdtemp.
            ASSIGN              
            judavdtemp.AVDELNINGNAMN = AVDELNING.AVDELNINGNAMN
            judavdtemp.JUDID = AVDELNING.POSTANST
            judavdtemp.AVDELNINGNR = AVDELNING.AVDELNINGNR.
         END.   
      END.
   END.
         
END PROCEDURE.

/*användare*/
PROCEDURE anvtemp_UI:
   DEFINE OUTPUT PARAMETER TABLE FOR anvandartemp.
   EMPTY TEMP-TABLE anvandartemp NO-ERROR. 
   FOR EACH ANVANDARE NO-LOCK:
      CREATE anvandartemp.
      BUFFER-COPY ANVANDARE TO anvandartemp.
      anvandartemp.TTRECID = RECID(anvandartemp).             
   END.
   FOR EACH anvandartemp,
   EACH PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = anvandartemp.PERSONALKOD NO-LOCK:
      anvandartemp.OMRADE = PERSONALTAB.OMRADE.       
   END.
   Guru.GlobalaVariabler:GDPRtyp = "AL".
   {GDPRLOGGCLIENT.I} 
END PROCEDURE.
/*personal*/
PROCEDURE perstemp_UI :
   DEFINE OUTPUT PARAMETER TABLE FOR personaltemp.
   EMPTY TEMP-TABLE personaltemp NO-ERROR. 
   
   FOR EACH PERSONALTAB NO-LOCK:
      CREATE personaltemp.
      BUFFER-COPY PERSONALTAB TO personaltemp.
      personaltemp.TTRECID = RECID(personaltemp).   
      Guru.GlobalaVariabler:GDPRvem = Guru.GlobalaVariabler:GDPRvem + "," + PERSONALTAB.PERSONALKOD.
   END.
   Guru.GlobalaVariabler:GDPRtyp = "PL".
   {GDPRLOGGCLIENT.I}   
END PROCEDURE.
/*materiel*/
DEFINE VARIABLE valdkommandoquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE felmedd AS CHARACTER NO-UNDO.
DEFINE VARIABLE tempbufmerah AS HANDLE NO-UNDO.
DEFINE VARIABLE tempbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE faltvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE begvar AS LOGICAL NO-UNDO.
DEFINE VARIABLE nyaquery AS CHARACTER NO-UNDO.
DEFINE VARIABLE appqueh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgbufh AS HANDLE NO-UNDO.
DEFINE VARIABLE ttcopyh AS HANDLE NO-UNDO.
PROCEDURE mtrltemptt_UI :
   DEFINE INPUT PARAMETER TABLE-HANDLE Etth.
   DEFINE OUTPUT PARAMETER TABLE-HANDLE Mtth.
   DEFINE INPUT PARAMETER levvar AS CHARACTER.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE EttHbuff AS HANDLE NO-UNDO.
   DEFINE VARIABLE MttHbuff AS HANDLE NO-UNDO.
   DEFINE VARIABLE qH AS HANDLE NO-UNDO.
   CREATE TEMP-TABLE Mtth.
   Mtth:CREATE-LIKE("mtrltemp").
   Mtth:TEMP-TABLE-PREPARE("mtrltt").
   EttHbuff = Etth:DEFAULT-BUFFER-HANDLE. 
   MttHbuff  = Mtth:DEFAULT-BUFFER-HANDLE. 
   MttHbuff:EMPTY-TEMP-TABLE() NO-ERROR.   
   
   kommandoquery = "FOR EACH " + Etth:NAME.
   RUN CreateCustomQuery(INPUT EttHbuff,INPUT kommandoquery,OUTPUT qh).
   qH:GET-FIRST().
   DO WHILE qH:QUERY-OFF-END = FALSE:
      IF levvar = "" THEN DO:
         FIND FIRST MTRL  WHERE MTRL.KALKNR = 0 AND MTRL.ENR = EttHbuff:BUFFER-FIELD("ENR"):BUFFER-VALUE NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST MTRL WHERE MTRL.KALKNR = 0 AND MTRL.ENR = EttHbuff:BUFFER-FIELD("ENR"):BUFFER-VALUE AND MTRL.LEVKOD = levvar NO-LOCK NO-ERROR.         
      END.      
      IF AVAILABLE MTRL THEN DO:
         MttHbuff:BUFFER-CREATE().
         MttHbuff:BUFFER-COPY(BUFFER MTRL:HANDLE).
         MttHbuff:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = MttHbuff:RECID.
      END.    
      qH:GET-NEXT().     
   END.
   RUN CloseCustomQuery(INPUT qH).
   
   /*
   FOR EACH emtrltemp WHERE NO-LOCK:
      IF levvar = "" THEN DO:
         FIND FIRST MTRL  WHERE MTRL.KALKNR = 0 AND MTRL.ENR = emtrltemp.ENR NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST MTRL WHERE MTRL.KALKNR = 0 AND MTRL.ENR = emtrltemp.ENR AND MTRL.LEVKOD = levvar NO-LOCK NO-ERROR.         
      END.      
      IF AVAILABLE MTRL THEN DO:
         CREATE mtrltemp.
         BUFFER-COPY MTRL TO mtrltemp.
         mtrltemp.TTRECID = RECID(mtrltemp).
      END.   
   END.
   */     
END PROCEDURE.
PROCEDURE mtrltemp_UI:
   DEFINE INPUT-OUTPUT PARAMETER TABLE-HANDLE apptthtemp.
   DEFINE INPUT PARAMETER lev AS CHARACTER.
   DEFINE INPUT PARAMETER enr AS CHARACTER.
   DEFINE INPUT PARAMETER ben AS CHARACTER.
   DEFINE INPUT PARAMETER kundvar AS LOGICAL NO-UNDO.
   DEFINE VARIABLE tth AS HANDLE NO-UNDO.
   DELETE WIDGET-POOL "dynpool" NO-ERROR.
   CREATE WIDGET-POOL "dynpool" PERSISTENT.
   sokstring = "".
   valdkommandoquery = "".
   tempbufh = apptthtemp:DEFAULT-BUFFER-HANDLE.
   CREATE TEMP-TABLE ttcopyh. 
   ttcopyh:CREATE-LIKE(tempbufh).
   ttcopyh:TEMP-TABLE-PREPARE("ttkopia").
   tempbufmerah = ttcopyh:DEFAULT-BUFFER-HANDLE.
   IF enr = "#" THEN enr = "".
   CREATE BUFFER orgbufh FOR TABLE "MTRL" IN WIDGET-POOL "dynpool".
   IF lev  = "" THEN nyaquery = "MTRL.KALKNR = 0".
   ELSE nyaquery = "MTRL.LEVKOD = '" + lev + "' AND MTRL.KALKNR = 0".
   IF kundvar = ? THEN.
   ELSE  nyaquery = nyaquery + " AND MTRL.KUND = " + STRING(kundvar). 
   
   
   EMPTY TEMP-TABLE mtrltemp NO-ERROR.
   
   IF ben NE "" THEN DO:
      sokstring[1] = ben.
      RUN sokoch_UI (sokstring[1]).
      /*
      sokstring[1] = "*" + sokstring[1] + "*" .
      */
      faltvar = "Benamning".
      RUN starwars_UI (sokstring[1]).
      
      /*
      begvar = TRUE.
      */
   END.   
   ELSE IF enr NE "" THEN DO:
      faltvar = "enr".
      sokstring[1] = enr.
      /*
      sokstring[1] = "*" + enr  /*+ "*"*/.
      */
      RUN sokoch_UI (sokstring[1]).
      RUN starwars_UI (sokstring[1]).
      /*
      begvar = TRUE.
      */
   END.  
   RUN kommandoskap_UI (INPUT sokstring[1]).
   /* Skapar query och kopplar queryn till den skarpa tabellens buffer */
   
   CREATE QUERY appqueh IN WIDGET-POOL "dynpool".
   appqueh:SET-BUFFERS(orgbufh).
   appqueh:QUERY-PREPARE(valdkommandoquery).
   appqueh:QUERY-OPEN().      
   appqueh:GET-FIRST(NO-LOCK).
   IF appqueh:QUERY-OFF-END = TRUE THEN DO:
      felmedd = "Det finns inget på sökbegreppet.".      
   END.
   ELSE DO:
      tempbufh:EMPTY-TEMP-TABLE  NO-ERROR.
      DO WHILE appqueh:QUERY-OFF-END = FALSE:
         tempbufh:BUFFER-CREATE().
         tempbufh:BUFFER-COPY(orgbufh) NO-ERROR.
         
         tempbufh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = orgbufh:RECID. 
         
         /*
         mtrltemp.TTRECID = RECID(mtrltemp).
         */
         appqueh:GET-NEXT(NO-LOCK).
      END.         
   END.
   /*
   IF TRIM(sokstring[1]) = ""  AND TRIM(enr) NE "" THEN DO:
      FOR EACH MTRL WHERE MTRL.ENR BEGINS enr AND MTRL.LEVKOD = lev AND MTRL.KALKNR = 0 USE-INDEX LEV NO-LOCK:
         CREATE mtrltemp.
         BUFFER-COPY MTRL TO mtrltemp.
         mtrltemp.TTRECID = RECID(mtrltemp).
      END.
      Found = TRUE.
   END.

   IF TRIM(sokstring[1]) NE ""  AND TRIM(enr) = "" THEN DO:
      FOR EACH MTRL WHERE MTRL.BENAMNING MATCHES "*" + sokstring[1] + "*" AND MTRL.LEVKOD = lev AND MTRL.KALKNR = 0 USE-INDEX BEN NO-LOCK:
         CREATE mtrltemp.
         BUFFER-COPY MTRL TO mtrltemp.
         mtrltemp.TTRECID = RECID(mtrltemp).
      END.
      Found = TRUE.
   END.
   
   IF TRIM(sokstring[1]) NE ""  AND TRIM(enr) NE "" THEN DO:
      
      /* FOR EACH MTRL WHERE MTRL.ENR BEGINS enr AND MTRL.BENAMNING BEGINS ben AND MTRL.LEVKOD = lev AND MTRL.KALKNR = 0 USE-INDEX BEN NO-LOCK:*/
      FOR EACH MTRL WHERE MTRL.ENR BEGINS enr AND MTRL.Benamning  MATCHES "*" + sokstring[1] + "*" AND MTRL.LEVKOD = lev AND MTRL.KALKNR = 0 USE-INDEX BEN NO-LOCK:
         CREATE mtrltemp.
         BUFFER-COPY MTRL TO mtrltemp.
         mtrltemp.TTRECID = RECID(mtrltemp).
      END.
      Found = TRUE.
   END.
   */
   
   tth = TEMP-TABLE mtrltemp:HANDLE.
   nyaquery = "".
   IF sokstring[2] = "" THEN .
   ELSE RUN sokmera_UI.
   DELETE WIDGET-POOL "dynpool" NO-ERROR.
   DELETE OBJECT appqueh NO-ERROR.
   appqueh = ?.
   DELETE OBJECT orgbufh NO-ERROR.
   orgbufh = ?.
END PROCEDURE.
PROCEDURE starwars_UI:
   DEFINE INPUT  PARAMETER stringkoll AS CHARACTER NO-UNDO.
   IF INDEX(stringkoll,"*",1) = 0 THEN DO:
      begvar = TRUE.
      RETURN.
   END.
   IF SUBSTRING(stringkoll,1,1) = "*" THEN DO:
      begvar = FALSE.
      RETURN.      
   END.  
   
END PROCEDURE.
PROCEDURE sokmera_UI :
   DEFINE VARIABLE rkollvar AS INTEGER NO-UNDO.
   rkollvar = 2.
   REPEAT: 
      orgbufh = tempbufh.
      IF sokstring[rkollvar] = "" THEN LEAVE.
      RUN starwars_UI (INPUT sokstring[rkollvar]).
      IF begvar = ? THEN LEAVE.
      RUN kommandoskap_UI (INPUT sokstring[rkollvar]).
      /* Skapar query och kopplar queryn till den skarpa tabellens buffer */
      CREATE QUERY appqueh.
      appqueh:SET-BUFFERS(orgbufh).
      appqueh:QUERY-PREPARE(valdkommandoquery).
      appqueh:QUERY-OPEN().      
      appqueh:GET-FIRST(NO-LOCK).
      IF appqueh:QUERY-OFF-END = TRUE THEN DO:
         orgbufh:EMPTY-TEMP-TABLE  NO-ERROR.
         felmedd = "Det finns inget på sökbegreppet.". 
         LEAVE.     
      END.
      ELSE DO:
         tempbufmerah:EMPTY-TEMP-TABLE  NO-ERROR.
         DO WHILE appqueh:QUERY-OFF-END = FALSE:
            tempbufmerah:BUFFER-CREATE().
            tempbufmerah:BUFFER-COPY(orgbufh) NO-ERROR.
            tempbufmerah:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = orgbufh:RECID.
            appqueh:GET-NEXT(NO-LOCK).
         END.         
      END.
      orgbufh:EMPTY-TEMP-TABLE.
      
      CREATE QUERY appqueh.
      appqueh:SET-BUFFERS(tempbufmerah).
      appqueh:QUERY-PREPARE("FOR EACH " + tempbufmerah:TABLE).
      appqueh:QUERY-OPEN().      
      appqueh:GET-FIRST(NO-LOCK).
      DO WHILE appqueh:QUERY-OFF-END = FALSE:
         orgbufh:BUFFER-CREATE().
         orgbufh:BUFFER-COPY(tempbufmerah) NO-ERROR.
         orgbufh:BUFFER-FIELD("TTRECID"):BUFFER-VALUE = tempbufmerah:RECID.
         appqueh:GET-NEXT(NO-LOCK).             
      END.      
      tempbufmerah:EMPTY-TEMP-TABLE  NO-ERROR.
      rkollvar = rkollvar + 1.
      IF rkollvar > 10 THEN LEAVE.
   END.  
   
END PROCEDURE.
PROCEDURE kommandoskap_UI :
   DEFINE INPUT  PARAMETER sokmera AS CHARACTER NO-UNDO.
   DEFINE VARIABLE extrafalth5 AS HANDLE NO-UNDO.
   valdkommandoquery = "".
   IF sokstring[1] NE "" THEN DO:
      extrafalth5 = tempbufh:BUFFER-FIELD(faltvar).
      IF begvar = FALSE THEN DO:
         IF extrafalth5:DATA-TYPE = "CHARACTER" THEN DO:
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES '" + sokmera + "'".
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES '" + sokmera + "' AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "INTEGER" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "LOGICAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.          
         ELSE IF extrafalth5:DATA-TYPE = "DECIMAL" THEN DO:  
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " MATCHES " + STRING(sokmera) + " AND " + nyaquery.
         END.          
      END.
      ELSE DO:
         IF extrafalth5:DATA-TYPE = "CHARACTER" THEN DO:
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS '" + sokmera + "'".
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS '" + sokmera + "' AND " +  nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "INTEGER" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.
         ELSE IF extrafalth5:DATA-TYPE = "LOGICAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.       
         ELSE IF extrafalth5:DATA-TYPE = "DECIMAL" THEN DO: 
            IF nyaquery = "" THEN valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera).
            ELSE valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + faltvar + " BEGINS " + STRING(sokmera) + " AND " + nyaquery.
         END.       
      END.
      DELETE OBJECT extrafalth5 NO-ERROR. 
      extrafalth5 = ?.
   END.
   ELSE DO:
      valdkommandoquery = " FOR EACH " + orgbufh:TABLE + " WHERE " + nyaquery.
   END.
END PROCEDURE.
PROCEDURE sokoch_UI :
   DEFINE INPUT  PARAMETER sokstringorg AS CHARACTER NO-UNDO.
   DEFINE VARIABLE forsta AS CHARACTER NO-UNDO.
   DEFINE VARIABLE sista AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kollvar AS INTEGER NO-UNDO.
   DEFINE VARIABLE rkollvar AS INTEGER NO-UNDO.
   IF INDEX(sokstringorg,"&&") = 0  THEN RETURN.
   IF SUBSTRING(sokstringorg,1,1) = "*" THEN  forsta = "*".
   IF SUBSTRING(sokstringorg,LENGTH(sokstringorg),1) = "*" THEN  sista = "*".
   sokstringorg = REPLACE(sokstringorg,"*","").
   rkollvar = 1.
   kollvar = 1.
   REPEAT: 
      IF kollvar >= LENGTH(sokstringorg) THEN LEAVE.
      IF rkollvar > 1 THEN DO:
         ASSIGN
         forsta = "*"
         sista = "*".
      END.   
      IF INDEX(sokstringorg,"&&",kollvar) = 0 THEN DO: 
         sokstring[rkollvar] = TRIM(forsta + SUBSTRING(sokstringorg,kollvar) + sista).
         LEAVE.
      END.
      sokstring[rkollvar] = TRIM(forsta + SUBSTRING(sokstringorg,kollvar,INDEX(sokstringorg,"&&",kollvar) - kollvar) + sista).
      kollvar = INDEX(sokstringorg,"&&",kollvar) + 2.
      rkollvar = rkollvar + 1.
      IF rkollvar > 10 THEN LEAVE.
   END.  
END PROCEDURE.

/*materiel*/
/*lev*/
PROCEDURE huvlevtemp_UI:
   DEFINE OUTPUT PARAMETER TABLE FOR huvlevtemp.
   EMPTY TEMP-TABLE huvlevtemp NO-ERROR.
   
   FOR EACH HUVUDLEV NO-LOCK:
      CREATE huvlevtemp.
      BUFFER-COPY HUVUDLEV TO huvlevtemp.
      huvlevtemp.TTRECID = RECID(huvlevtemp).
   END.
   FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
   IF AVAILABLE huvlevtemp THEN DO:
      ASSIGN
      Guru.Konstanter:HuvudLeverantor = huvlevtemp.LEVKOD.
   END.
   ELSE DO:
      FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD NE "0" AND LEVERANTOR.LEVKOD NE "99"
      AND LEVERANTOR.BORTTAG = FALSE NO-LOCK NO-ERROR.
      IF AVAILABLE LEVERANTOR THEN Guru.Konstanter:HuvudLeverantor = LEVERANTOR.LEVKOD.
   END.         
   FIND FIRST LEVERANTOR WHERE LEVERANTOR.LEVKOD = Guru.Konstanter:HuvudLeverantor
   USE-INDEX LEV NO-LOCK NO-ERROR.   
   IF AVAILABLE LEVERANTOR THEN Guru.Konstanter:HuvudLeverantorName = levtemp.LEVNAMN.
   
   
   
END PROCEDURE.  


PROCEDURE levtemp_UI:
   DEFINE OUTPUT PARAMETER TABLE FOR levtemp.
   
   EMPTY TEMP-TABLE levtemp NO-ERROR.
   FOR EACH LEVERANTOR WHERE LEVERANTOR.BORTTAG = FALSE NO-LOCK:
      CREATE levtemp.
      BUFFER-COPY LEVERANTOR TO levtemp.
      {LEVTEMPORDNING.I}
      levtemp.TTRECID = RECID(levtemp).
   END.
END PROCEDURE.  


/* Språk börjar här */

PROCEDURE hamtasprak:
   DEFINE OUTPUT PARAMETER TABLE FOR spraktemp.
   EMPTY TEMP-TABLE spraktemp NO-ERROR.
   FIND FIRST SPRAK WHERE NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SPRAK THEN DO:
      CREATE SPRAK.
      ASSIGN 
      SPRAK.ID = 0
      SPRAK.BENAMNING = "Svenska".
      
   END.
   FOR EACH SPRAK NO-LOCK:
      CREATE spraktemp.
      BUFFER-COPY SPRAK TO spraktemp.
      spraktemp.TTROWID = ROWID(spraktemp).
   END.
   
END PROCEDURE.

/*HÄMTAR MEDDELANDE TEXT*/
PROCEDURE hamtasprakstrang:
   DEFINE INPUT PARAMETER langid AS INTEGER.
   DEFINE INPUT PARAMETER sokidvar AS INTEGER.
   DEFINE OUTPUT PARAMETER TABLE FOR sprakstrangtemp.
   EMPTY TEMP-TABLE sprakstrangtemp NO-ERROR. 
   FIND FIRST SPRAK_STRANG WHERE SPRAK_STRANG.SPRAKID = langid AND SPRAK_STRANG.SOKID = sokidvar AND 
   SPRAK_STRANG.SOKCHAR = "" NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SPRAK_STRANG THEN DO:
      RUN sprakstrankkny_UI (INPUT langid,INPUT sokidvar, INPUT "").
      FIND FIRST SPRAK_STRANG WHERE SPRAK_STRANG.SPRAKID = langid AND SPRAK_STRANG.SOKID = sokidvar AND 
      SPRAK_STRANG.SOKCHAR = "" NO-LOCK NO-ERROR.
   END.
   IF AVAILABLE(SPRAK_STRANG) THEN DO:
      CREATE sprakstrangtemp.
      BUFFER-COPY SPRAK_STRANG TO sprakstrangtemp.
      sprakstrangtemp.TTROWID = ROWID(sprakstrangtemp).
   END.
END PROCEDURE.

/*HÄMTAR KONSTVÄRDEN*/
PROCEDURE hamtasprakstrangc:
   DEFINE INPUT PARAMETER langid AS INTEGER.
   DEFINE INPUT PARAMETER sokidcvar AS CHARACTER .
   DEFINE OUTPUT PARAMETER TABLE FOR sprakstrangtemp.
   EMPTY TEMP-TABLE sprakstrangtemp NO-ERROR. 
   FIND FIRST SPRAK_STRANG WHERE SPRAK_STRANG.SPRAKID = langid AND SPRAK_STRANG.SOKCHAR = sokidcvar NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SPRAK_STRANG THEN DO:
      RUN sprakstrankkny_UI (INPUT langid,INPUT 0, INPUT sokidcvar).
      FIND FIRST SPRAK_STRANG WHERE SPRAK_STRANG.SPRAKID = langid AND SPRAK_STRANG.SOKCHAR = sokidcvar NO-LOCK NO-ERROR.
   END.
   IF AVAILABLE(SPRAK_STRANG) THEN DO:
      CREATE sprakstrangtemp.
      BUFFER-COPY SPRAK_STRANG TO sprakstrangtemp.
      sprakstrangtemp.TTROWID = ROWID(sprakstrangtemp).
   END.
END PROCEDURE.

FUNCTION LastStrangId RETURNS INTEGER :
   DEFINE VARIABLE idd AS INTEGER.
   FIND LAST SPRAK_STRANGbuff USE-INDEX ID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SPRAK_STRANGbuff THEN idd = 0.
   ELSE idd = SPRAK_STRANGbuff.ID + 1.
   RETURN idd.
END FUNCTION.
FUNCTION LastStrangSokId RETURNS INTEGER (INPUT sprakid AS INTEGER):
   DEFINE VARIABLE idd AS INTEGER.
   FIND LAST SPRAK_STRANGbuff WHERE SPRAK_STRANGbuff.SPRAKID = sprakid USE-INDEX SOKID NO-LOCK NO-ERROR.
   IF NOT AVAILABLE SPRAK_STRANGbuff THEN idd = 0.
   ELSE idd = SPRAK_STRANGbuff.SOKID + 1.
   RETURN idd.
END FUNCTION.


PROCEDURE sprakstrankcreate_UI :
   DEFINE INPUT PARAMETER langid AS INTEGER.
   DEFINE INPUT PARAMETER sokidvar AS INTEGER.
   DEFINE INPUT  PARAMETER benvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER sokvar AS CHARACTER NO-UNDO.
   IF sokidvar = ? THEN sokidvar = LastStrangSokId(langid).
   DO TRANSACTION:
      CREATE SPRAK_STRANG.
      ASSIGN
      SPRAK_STRANG.SPRAKID = langid
      SPRAK_STRANG.ID = LastStrangId()
      SPRAK_STRANG.SOKCHAR = sokvar
      SPRAK_STRANG.SOKID = sokidvar
      SPRAK_STRANG.BENAMNING = benvar.
   END.  
   RELEASE SPRAK_STRANG NO-ERROR. 
END PROCEDURE.
PROCEDURE GuruvarSet_UI :
   DEFINE INPUT  PARAMETER sokvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER benvar AS CHARACTER NO-UNDO.
   
END PROCEDURE.
PROCEDURE longkort_UI :
   DEFINE INPUT PARAMETER langid AS INTEGER.
   DEFINE INPUT PARAMETER sokidvar AS INTEGER.
   DEFINE INPUT PARAMETER sokidcvar AS CHARACTER .
   DEFINE INPUT PARAMETER forstasprak AS LOGICAL NO-UNDO.
   /*Dessa måste vara så här*/
      IF sokidvar = 1  OR sokidcvar = "gavdl" OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,1 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 2  OR sokidcvar = "gavdk"    OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,2 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 3  OR sokidcvar = "gomrl"           OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,3 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 4  OR sokidcvar = "gomrk"           OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,4 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 5  OR sokidcvar = "gaol"            OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,5 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 6  OR sokidcvar = "gaok"            OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,6 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 7  OR sokidcvar = "gpll"            OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,7 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 8  OR sokidcvar = "gplk"            OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,8 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 9  OR sokidcvar = "genl"            OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,9 ,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 10 OR sokidcvar = "genk"            OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,10,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 11 OR sokidcvar = "gjul"            OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,11,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 12 OR sokidcvar = "gjuk"            OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,12,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 13 OR sokidcvar = "gfastl"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,13,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 14 OR sokidcvar = "gfastk"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,14,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 15 OR sokidcvar = "gtilll"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,15,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 16 OR sokidcvar = "gtillk"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,16,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 17 OR sokidcvar = "gberel"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,17,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 18 OR sokidcvar = "gberek"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,18,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 19 OR sokidcvar = "gprojl"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,19,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 20 OR sokidcvar = "gprojk"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,20,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 21 OR sokidcvar = "garbal"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,21,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 22 OR sokidcvar = "garbak"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,22,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 23 OR sokidcvar = "gutfk"           OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,23,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 24 OR sokidcvar = "gutfl"           OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,24,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 25 OR sokidcvar = "gbestk"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,25,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 26 OR sokidcvar = "gbestl"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,26,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 27 OR sokidcvar = "gdebk"           OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,27,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 28 OR sokidcvar = "gdebl"           OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,28,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 29 OR sokidcvar = "gtidlk"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,29,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 30 OR sokidcvar = "gtidll"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,30,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 31 OR sokidcvar = "gutfardk"        OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,31,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 32 OR sokidcvar = "gutfardl"        OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,32,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 33 OR sokidcvar = "grefbefk"        OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,33,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 34 OR sokidcvar = "grefbefl"        OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,34,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 35 OR sokidcvar = "gpriok"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,35,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 36 OR sokidcvar = "gpriol"          OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,36,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 37 OR sokidcvar = "gartk"           OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,37,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 38 OR sokidcvar = "gartl"           OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,38,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 39 OR sokidcvar = "gaonamnk"        OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,39,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.   
      IF sokidvar = 40 OR sokidcvar = "gaonamnl"        OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,40,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END. 
      IF sokidcvar = "gdelnrk" OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,?,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.
      IF sokidcvar = "gutbytk" OR forstasprak = TRUE THEN DO:
         FIND FIRST textsprakstrangtemp WHERE textsprakstrangtemp.SPRAKID = langid AND textsprakstrangtemp.SOKCHAR = sokidcvar  NO-LOCK NO-ERROR.
         RUN sprakstrankcreate_UI (langid ,?,textsprakstrangtemp.BENAMNING, textsprakstrangtemp.SOKCHAR).
      END.    
      
      /*Dessa måste vara så här*/
END PROCEDURE.   
PROCEDURE sprakstrankkny_UI :
   
   DEFINE INPUT PARAMETER langid AS INTEGER.
   DEFINE INPUT PARAMETER sokidvar AS INTEGER.
   DEFINE INPUT PARAMETER sokidcvar AS CHARACTER .
   DEFINE VARIABLE forstasprak AS LOGICAL NO-UNDO.
   IF langid = 0 THEN DO: 
      FIND FIRST SPRAK WHERE SPRAK.ID = langid NO-LOCK NO-ERROR.
      IF NOT AVAILABLE SPRAK THEN DO TRANSACTION:
         CREATE SPRAK.
         ASSIGN
         SPRAK.ID = langid
         SPRAK.BENAMNING = "Svenska".
         forstasprak = TRUE. 
      END.   
      FIND FIRST SPRAK WHERE SPRAK.ID = langid NO-LOCK NO-ERROR.
      RUN longkort_UI (INPUT langid, INPUT sokidvar,INPUT sokidcvar, INPUT forstasprak).
      /*Anders Olsson Elpool i Umeå AB  13 okt 2022 11:28:11 
      meddelande i cls program 
      */    
      IF sokidvar = 41 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Skapa egen","").
      IF sokidvar = 42 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är du säker på att du vill ta bort dessa rader?","").
      IF sokidvar = 43 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är du säker på att du vill ta bort ALLA rader?","").
      IF sokidvar = 44 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är du säker?","").
      IF sokidvar = 45 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är du säker på att du vill generera om din visning? Den gamla versionen kommer att försvinna!","").
      IF sokidvar = 46 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är du säker på att du vill återställa alla fria värden för hela kalkylen?","").
      IF sokidvar = 47 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du visar nu avtalskalkyl med denna mall:","").
      IF sokidvar = 48 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Hittar inte:","").
      IF sokidvar = 49 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan ej lägga upp denna post!","").
      IF sokidvar = 50 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ligger arbetskod i A, löpnr i B och antal i C  ? - Svara Ja","").
      IF sokidvar = 51 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ligger arbetskod och löpnr i A och antal i C  ? - Svara Nej","").
      IF sokidvar = 52 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inläsning av Excel-fil till kalkyl","").
      IF sokidvar = 53 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du är inte behörig att ändra denna kalkyl!","").
      IF sokidvar = 54 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Hittade inte modul -","").
      IF sokidvar = 55 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"-. Avslutar.","").
      IF sokidvar = 56 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kan inte skapa Kalkyl!","").
      IF sokidvar = 57 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kan inte ladda Kalkyl!","").
      IF sokidvar = 58 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inloggningen misslyckades!","").
      IF sokidvar = 59 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du är inte behörig att ta bort denna kalkyl!","").
      IF sokidvar = 60 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det går inte att ta bort Katalogen! Den används. ","").
      IF sokidvar = 61 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det går inte att koppla Katalogen. Det blir dubbletter!","").
      IF sokidvar = 62 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Felaktigt värde! Arbetskoden finns redan eller har ett ogiltigt värde.","").
      IF sokidvar = 63 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Funktionen går inte att nå för vald leverantör!","").
      IF sokidvar = 64 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod finns ej i vald katalog!","").   
      IF sokidvar = 65 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har skapat en ny kalkyl, vill du spara den?","").
      /*FRÅN KALKBERAPPDS.p*/
      IF sokidvar = 66 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det går inte att lägga upp kalkyler på detta","").
      IF sokidvar = 67 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Nummerserie saknas eller är fylld.","").
      IF sokidvar = 68 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Benämningen får inte vara blank!","").
      IF sokidvar = 69 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kalkylansavarig kan inte vara blank!","").
      IF sokidvar = 70 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ingen ansvarig med enhet ","").
      IF sokidvar = 71 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"finns i registret! ","").
      
      IF sokidvar = 72 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Spara kalkyl? ","").
      IF sokidvar = 73 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ingen rad är markerad ","").
      /*ändra*/
      IF sokidvar = 74 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du måste markera en rad","").
      
      IF sokidvar = 75 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ingen rad ","").
      IF sokidvar = 76 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns ingen rad att ta bort ","").
      /*Kopiering*/
      IF sokidvar = 77 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kopieringen genomförd ","").
      IF sokidvar = 78 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en kopierad kalkyl. Din gamla kalkyl finns fortfarande sparad. ","").
      /*Konvertering*/
      IF sokidvar = 79 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Konverteringen genomförd ","").
      IF sokidvar = 80 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar med en konverterad kalkyl. Din gamla kalkyl finns fortfarande sparad. ","").
      /*XML export*/
      IF sokidvar = 81 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"XML-import genomförd ","").
      IF sokidvar = 82 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en importerad kalkyl. Din gamla kalkyl finns fortfarande sparad. ","").
      /*XML export e-post*/
      IF sokidvar = 83 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Skicka via mail? ","").
      IF sokidvar = 84 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Din kalkyl är nu exporterad till ","").
      IF sokidvar = 85 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du skicka filen till en e-post adress? ","").
      /*EXCELIMPORT*/
      /*IF sokidvar = 86 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Innehåller Excel -filen förklaringsraden till kolumnerna? ","").*/
      IF sokidvar = 86 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är första inläsningsraden i filen rad 3? (annars antas första inläsningsraden vara rad 2) ","").
      IF sokidvar = 87 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du kopiera vald katalog? ","").
      IF sokidvar = 88 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du ta bort vald katalog? ","").
      
      IF sokidvar = 89 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du väljer att det ska vara frekvens på denna kod kommer befintliga resurer på koden att tas bort. Vill du detta? ","").
      IF sokidvar = 90 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du väljer att det inte skall vara frekvens på denna kod kommer befintliga frekvenser på koden att tas bort. Vill du detta? ","").
      IF sokidvar = 91 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du spara dina ändringar på denna nivå och sedan uppdatera upp till P1-nivå? Svarar du Nej kommer bara ändringarna sparas men inga beräkningar göras. ","").
      IF sokidvar = 92 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du spara dina ändringar på denna nivå? Svarar du Nej kommer ändringarna tas bort. ","").
      IF sokidvar = 93 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du väljer att det inte skall vara frekvens på denna kod kommer befintliga frekvenser och tillkommande beredning på koden att tas bort. Vill du detta? ","").
      IF sokidvar = 94 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna kod kan ej tas bort, den används i frekvenstabell för kod:  ","").
      IF sokidvar = 95 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Alla resurser, frekvenser och ev materiel kopplade till denna kod kommer också tas bort vid borttag. Är du säker på att du vill ta bort dessa koder?","").
      IF sokidvar = 96 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Alla kopplade löpnr, resurser, frekvenser och ev materiel kopplade till denna kod kommer också tas bort vid borttag. Är du säker på att du vill ta bort dessa koder?","").
      IF sokidvar = 97 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vid uppdatering av EA-faktorn räknas samtliga resurser EA-mängd om. Vill du detta?","").
      IF sokidvar = 98 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod finns i Katalogen och kommer att ersätta befintlig kod.","").
      IF sokidvar = 99 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny arbetskod! Läggs till i Katalogen.","").
      IF sokidvar = 100 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod har ogiltig resurs. Kommer att läsas in utan resurser.","").
      IF sokidvar = 101 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns redan inlästa resurser för denna kod. De redan inlästa resurserna kommer att ersättas med dessa nya.","").
      
      IF sokidvar = 102 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ta bort denna resurs ur denna katalog?","").
      IF sokidvar = 103 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna resurs kan inte tas bort från denna katalog. Den används på: ","").
      IF sokidvar = 104 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Detta är inte en giltig regel. Se under fliken Regel för giltiga regler","").
      IF sokidvar = 105 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen byta regel för denna resurs?","").
      
      IF sokidvar = 106 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koderna för denna frekvens finns inte i katalogen. Frekvens kommer inte att läsas in!","").
      /*IF sokidvar = 107 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har redan frekvens. Frekvens upplägget kommer att ersättas/kompletteras!","").*/
      IF sokidvar = 107 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna frekvenskod finns redan upplagd på koden. Görs valet Läs in Excel lista, så ersätts upplägget på koden. Görs valet Komplettera med Excel lista så kommer denna kod ej att läsas in.","").
      IF sokidvar = 108 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har sedan tidigare ett resursupplägg. Detta kommer att ersättas med denna frekvens.","").
      IF sokidvar = 109 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden kommer att få ett frekvens upplägg.","").
      IF sokidvar = 110 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Arbetskod finns inte! Posten kommer inte att läsas in.","").
      IF sokidvar = 111 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Byte mellan resurs och frekvens kommer att ske.","").
      IF sokidvar = 112 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod har resurs. Inget kommer att läsas in.","").
      IF sokidvar = 113 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vid uppdatering kommer alla ej låsta artiklar att räknas upp med Påslag för materielhanteringomkostnad. Vill du detta?","").
      IF sokidvar = 114 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vid uppdatering kommer alla artiklar (även låsta) att räknas upp med Påslag för materielhanteringomkostnad. Vill du detta?","").
      IF sokidvar = 115 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ta bort denna resurs på denna nivå?","").
      IF sokidvar = 116 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du uppdaterar Kalkylkostnad manuellt kommer detta fält låsas för uppdatering. Vill du detta?","").
      IF sokidvar = 117 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du uppdaterar påslaget kommer berörda poster att räknas upp. Vill du detta?","").
      IF sokidvar = 118 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ändra detta påslag till 0?","").
      IF sokidvar = 119 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ta bort denna resurs för alla kataloger?","").
      IF sokidvar = 120 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna resurs kan inte tas bort. Den används på: ","").
      IF sokidvar = 121 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du ta bort låsningen på denna post? Kalkylkostnad kommer i så fall att räknas om. Vill du detta?","").
      IF sokidvar = 122 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du låsa Kalkylkostnad från automatisk uppdatering?","").
      IF sokidvar = 123 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ta bort denna artikel ur denna katalog?","").
      IF sokidvar = 124 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna artikel kan inte tas bort ur denna katalog. Den används på:","").
      IF sokidvar = 125 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du skriver in en Faktor för uppräkning av övriga kostnader kommer antalet på alla P2-resurser 61-Övriga kostnader i denna katalog att räknas upp med denna faktor. 
      Sedan kommer Faktor att läggas tillbaka till 0. Vill du detta?","").
      IF sokidvar = 126 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Alla P2-resurser 61-Övriga kostnader i denna katalog räknas vid Spara upp med : ","").
      IF sokidvar = 127 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen uppdatera kostnad på denna resurs?","").
      IF sokidvar = 128 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen uppdatera kostnad på denna artikel?","").
      IF sokidvar = 129 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Artikeln finns i katalogen och kommer att uppdateras","").
      IF sokidvar = 130 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Artikeln finns inte i katalogen och kommer att läggas upp som nytt materiel","").
      IF sokidvar = 131 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Artikeln finns inte i katalogen, benämning saknas för att lägga upp nytt materiel","").
      IF sokidvar = 132 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har gjort ändringar i katalogen som ännu inte är sparade, vill du spara dessa innan du avslutar programmet?","").
      IF sokidvar = 133 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Artikeln som du vill uppdatera är låst i katalogen, kalkylkostnad kommer ej att uppdateras","").
      IF sokidvar = 134 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Katalogen är öppnad i låst läge","").
      IF sokidvar = 135 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Benämningen på resursen gäller för alla kataloger. Vill du verkligen byta benämning på denna resurs? ","").
      IF sokidvar = 136 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Enhet på resursen gäller för alla kataloger. Vill du verkligen byta enhet på denna resurs? ","").
      IF sokidvar = 137 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"När en resurs läggs inaktiv går den inte längre att lägga till en katalog. Vill du inaktivera markerad resurs? ","").
      IF sokidvar = 138 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"När en resurs läggs aktiv går den att lägga till en katalog. Vill du aktivera markerad resurs? ","").
      IF sokidvar = 139 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"De förändringar som du har gjort kommer att påverka samtliga kataloger, är du säker på att du vill göra detta?","").
      IF sokidvar = 140 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Valda kataloger kan inte jämföras!","").
      IF sokidvar = 141 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Din nuvarande katalog stängs. Vill du jobba med den igen välj den på nytt!","").  
      IF sokidvar = 142 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du exportera kostnad?","").
      IF sokidvar = 143 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du låsa vald katalog?","").
      IF sokidvar = 144 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du låsa upp vald katalog?","").
      IF sokidvar = 145 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Den katalog du arbetade med kommer nu att laddas om",""). 
      IF sokidvar = 146 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har redan frekvens. Frekvens upplägget kommer att raderas!","").
      IF sokidvar = 147 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny kod! Läggs till i Katalogen.",""). 
      IF sokidvar = 148 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är du säker på att du vill byta benämning?",""). 
      IF sokidvar = 149 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Katalogen är låst, du måste låsa upp den innan du kan ta bort den",""). 
      IF sokidvar = 150 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är du säker på att du uppdatera årtalet?","").
      IF sokidvar = 151 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Katalogen används och går därför inte att ta bort","").
      IF sokidvar = 152 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har gjort ändringar i katalogen som ännu inte är sparade, vill du spara dessa innan du stänger katalogen?","").
      IF sokidvar = 153 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny arbetskod!Saknar benämning. Arbetskoden kommer inte att läsas in!","").
      IF sokidvar = 154 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny arbetskod!Saknar enhet. Arbetskoden kommer inte att läsas in!","").
      IF sokidvar = 155 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Import-posterna för denna arbetskod har inte samma angivelse om det skall vara frekvens eller inte! Arbetskoden kommer inte att läsas in!","").
      IF sokidvar = 156 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Resurs 101- Tillkommande beredning kan bara användas när det inte är frekvens.Koden kommer att läsas in utan denna resurs!","").
      IF sokidvar = 157 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Filen finns sparad i mapp -","").
      IF sokidvar = 158 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns ingen kod med denna arbetskod och löpnummer, materielet kommer inte att läggas till -","").   
      IF sokidvar = 159 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns inget materiel med detta artikelnummer i katalogen -","").   
      IF sokidvar = 160 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns materiel på denna arbetskod/löpnummer, dessa materiel kommer att ersättas -","").  
      IF sokidvar = 161 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Materielet kommer att läggas till på arbetskoden -",""). 
      IF sokidvar = 162 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du ska använda ',' som decimalkomma ","").
      IF sokidvar = 163 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du ska använda '.' som decimalkomma ","").
      IF sokidvar = 164 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inläsning klar!",""). 
      IF sokidvar = 165 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om montörskostnaden uppdateras så uppdateras även Ea-faktorn och samtliga resursers EA-mängd räknas om. Vill du detta?","").
      IF sokidvar = 166 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Felaktigt värde! Löpnummret finns redan eller har ett ogiltigt värde.","").
      IF sokidvar = 167 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Din katalog är nu exporterad till ","").
      IF sokidvar = 168 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Låses av ","").
      IF sokidvar = 169 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ändring ej möjlig.","").
      IF sokidvar = 170 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du fortsätta ändå?","").
      IF sokidvar = 171 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kan inte skapa Ärende!","").
      IF sokidvar = 172 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kan inte ladda Ärende!","").
      IF sokidvar = 173 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Felaktig markering!","").
      IF sokidvar = 174 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Minst 2 poster för ett Svep!","").
      IF sokidvar = 175 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Alla poster i ett Svep måste ha samma arbetskod!","").
      IF sokidvar = 176 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inga Kommentarer valda!","").
      IF sokidvar = 177 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Svepet finns redan!","").
      IF sokidvar = 178 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ," använder Kommentaren!","").
      IF sokidvar = 179 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Går inte att ta bort!","").
      IF sokidvar = 180 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det går inte att ta bort Katalogen! Den används till vissa kalkyler. ","").
      IF sokidvar = 181 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det går inte att ta bort DelKatalogen! Den används som delkatalog. ","").
      IF sokidvar = 182 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det går inte att ta bort Katalogen! DelKatalogerna använs till flera Kataloger. ","").
      IF sokidvar = 183 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du fick några fel! Kontrollera under fliken för felmeddelande. ","").
      IF sokidvar = 184 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte flytta posten, för att det finns dubbletter. ","").      
      IF sokidvar = 185 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Använda årets timpriser-svara JA. Använda fjolårets timpriser -svara NEJ. ","").
      IF sokidvar = 186 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Skall timpriser vara avrundare utan decimaler? ","").
      IF sokidvar = 187 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Arbetskod och löpnr i A -Ja, eller Arbetskod i A och löpnr i B-Nej ","").
      IF sokidvar = 188 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en ihopslagen kalkyl. Dina gamla kalkyler finns fortfarande sparade. ","").
      IF sokidvar = 189 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Sammanslagningen genomförd ","").
      IF sokidvar = 190 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Sortering är inte tillåten!","").
      IF sokidvar = 191 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte ta bort denna konstruktionsdel. Den är inköpt via inköpsrutinen.","").
      IF sokidvar = 192 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte ta bort denna del av konstruktionen. Du måste ta bort hela den valda konstruktionen.","").
      IF sokidvar = 193 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte ändra ordning i denna konstruktion. Den är inköpt via inköpsrutinen.","").
      IF sokidvar = 194 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte ändra på denna del av konstruktionen. Den är inköpt via inköpsrutinen.","").
      IF sokidvar = 195 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte flytta på posten.","").
      IF sokidvar = 196 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en ny kalkyl där du jämför två kalkyler. Dina gamla kalkyler finns fortfarande sparade. ","").
      IF sokidvar = 197 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ingen av leverantörerna har någon av de valda ariklarna.","").
      IF sokidvar = 198 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inga arbetskoder funna!","").
      IF sokidvar = 199 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du får antigen lägga upp Beredare eller Beredare Region på varje kod!","").
      IF sokidvar = 200 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Fältet är inte ifyllt korrekt!","").
      IF sokidvar = 201 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kontrollen sker endast mot den senast sparade Katalogen! Vill du spara dina ändringar på denna nivå?","").
      IF sokidvar = 202 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns inget att klistra in!","").
      IF sokidvar = 203 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Använda årets timpriser-svara JA. Använda kopierade katalogens timpriser -svara NEJ. ","").
      IF sokidvar = 204 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Personalliggaren är aktiverad","").
      IF sokidvar = 205 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Personalliggaren är inaktiverad","").
      IF sokidvar = 206 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inga poster hittades på sökbegreppet","").
      IF sokidvar = 207 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Sökbegreppet är för kort!","").
      IF sokidvar = 208 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Är du säker på att du vill ta bort denna rad?","").
      IF sokidvar = 209 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Utfärdare kan inte vara blank!","").
      IF sokidvar = 210 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har redan frekvens. Välj om den ska ersättas eller kompletteras! Via knapparna Läs in Excel listan resp. Komplettera med Excel listan","").
      IF sokidvar = 211 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Posten är redan upplagd!","").
      IF sokidvar = 212 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inläsningen slutförd.","").
      IF sokidvar = 213 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Obs! Orginalkalkylen flyttas till ny katalog detta går inte att ångra!",""). 
      IF sokidvar = 214 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Flytten är genomförd. ","").
      IF sokidvar = 215 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en ny kalkyl.","").
      IF sokidvar = 216 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Fel filformat!","").
      IF sokidvar = 217 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inga fel i filen hittades. Den går bra att läsa in.","").
      IF sokidvar = 218 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Saknar företag filen går inte att läsa in.","").
      IF sokidvar = 218 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har inte tagit ställning till denna post!","").
      IF sokidvar = 219 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Lösen måste vara minst 5 tecken!","").
      IF sokidvar = 220 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du ändrar lösen för denna användre. Vill du göra detta?","").
      IF sokidvar = 221 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"User får inte vara blank!","").
      IF sokidvar = 222 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"User finns redan!","").
      IF sokidvar = 223 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny användare!","").
      IF sokidvar = 224 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Posten sparades inte för att du har fel värde i User! Alla poster med fel värde i User kommer att rensas bort vid nästa sparning!","").
      IF sokidvar = 225 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Nytt lösen!","").
      IF sokidvar = 226 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Posten sparades inte för att du har fel värde i Password! Alla poster med fel värde i Password kommer att rensas bort vid nästa sparning!","").
      IF sokidvar = 227 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har nått maximalt antal Excelfönster, stäng för att skapa nya!","").
      IF sokidvar = 228 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"För att hämta grundupplägg måste du radera dina koder först!","").
      IF sokidvar = 229 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ska alla arbetskoder kopieras?","").
      IF sokidvar = 230 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns dubbletter! ","").
      IF sokidvar = 231 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du även kopiera Frikalkylen? ","").
      IF sokidvar = 232 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns ingen beredning att kopiera!","").
      IF sokidvar = 233 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du kopiera denna beredning?","").
      IF sokidvar = 234 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ditt nya beredningsnummer är:","").
      IF sokidvar = 235 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kopiering avbruten!","").
      IF sokidvar = 236 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du ta bort Ersättningslistan? ","").
      IF sokidvar = 237 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna Ersättningslistan går inte att ta bort!","").
      IF sokidvar = 238 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna översättning finns redan upplagd!","").
      IF sokidvar = 239 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Angiven leverantör finns ej upplagd!","").
      IF sokidvar = 240 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ta bort sista posten på en konstruktion måste göras i beredningsmodulen!","").
      IF sokidvar = 241 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Angivet enr finns ej hos leverantör","").
      IF sokidvar = 242 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kalkylkoden är en dublett på detta byggnr!","").
      IF sokidvar = 243 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Byggnr finns ej i beredning!","").
      IF sokidvar = 244 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Använda årets timpriser-svara JA. Använda kopierade katalogens timpriser -svara NEJ. ","").
      IF sokidvar = 245 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Delsumma för markerade poster!","").
   END.
   
END PROCEDURE.

PROCEDURE LoggaIn :
   DEFINE INPUT  PARAMETER styrbild AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER gurubilder AS CHARACTER NO-UNDO.
   DEFINE OUTPUT PARAMETER link AS CHARACTER NO-UNDO.
   RUN LOGGORIN.P (INPUT styrbild, INPUT gurubilder,OUTPUT link).
END PROCEDURE.
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
