/*TEMPTABELLERAPP.p*/
/*Anders Olsson Elpool i Ume� AB  13 okt 2022 11:33:46 
skapar spr�kposter. meddelanden f�r cls program, h�mtar v�rden f�r globalakonstanter gaonr mm.  
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

/*Defult v�rden f�r ribbon*/
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

/*NYA SPR�KSTR�NGAR*/
/*Anders Olsson Elpool i Ume� AB  13 okt 2022 11:29:15 
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
   textsprakstrangtemp.BENAMNING = "Ers�ttningslista".
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
      /*Visningsinst�llningar*/ 
      IF GuruDefaultValuesTT.TOOLKEY = "VisVisafor" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisSummera" THEN GuruDefaultValuesTT.TOOLVALUE = "yes". /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisGruppera" THEN GuruDefaultValuesTT.TOOLVALUE = "no". /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisPriser" THEN GuruDefaultValuesTT.TOOLVALUE = "0". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisMatSpec" THEN GuruDefaultValuesTT.TOOLVALUE = "no". 
      IF GuruDefaultValuesTT.TOOLKEY = "VisMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "0".  /*vis ok*/
      IF GuruDefaultValuesTT.TOOLKEY = "VisGruppMatris" THEN GuruDefaultValuesTT.TOOLVALUE = "no".  /*vis ok*/
      /*Uppf�ljning*/ 
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
     
      /*Visningsinst�llningar*/ 
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
           
      IF GuruDefaultValuesTT.TOOLGROUP = "V�lj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmKod" THEN GuruDefaultValuesTT.TOOLVALUE = "yes".
      IF GuruDefaultValuesTT.TOOLGROUP = "V�lj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "V�lj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmDelKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "V�lj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmPriser" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "V�lj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmSek" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "V�lj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmFrek" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "V�lj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      IF GuruDefaultValuesTT.TOOLGROUP = "V�lj administration" AND GuruDefaultValuesTT.TOOLKEY = "AdmKatalog" THEN GuruDefaultValuesTT.TOOLVALUE = "no".
      
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
/*Defult v�rden f�r ribbon*/
/*h�mtar kund*/

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
/*h�mtar omr�den*/
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

/*anv�ndare*/
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
      felmedd = "Det finns inget p� s�kbegreppet.".      
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
         felmedd = "Det finns inget p� s�kbegreppet.". 
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


/* Spr�k b�rjar h�r */

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

/*H�MTAR MEDDELANDE TEXT*/
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

/*H�MTAR KONSTV�RDEN*/
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
   /*Dessa m�ste vara s� h�r*/
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
      
      /*Dessa m�ste vara s� h�r*/
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
      /*Anders Olsson Elpool i Ume� AB  13 okt 2022 11:28:11 
      meddelande i cls program 
      */    
      IF sokidvar = 41 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Skapa egen","").
      IF sokidvar = 42 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r du s�ker p� att du vill ta bort dessa rader?","").
      IF sokidvar = 43 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r du s�ker p� att du vill ta bort ALLA rader?","").
      IF sokidvar = 44 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r du s�ker?","").
      IF sokidvar = 45 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r du s�ker p� att du vill generera om din visning? Den gamla versionen kommer att f�rsvinna!","").
      IF sokidvar = 46 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r du s�ker p� att du vill �terst�lla alla fria v�rden f�r hela kalkylen?","").
      IF sokidvar = 47 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du visar nu avtalskalkyl med denna mall:","").
      IF sokidvar = 48 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Hittar inte:","").
      IF sokidvar = 49 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan ej l�gga upp denna post!","").
      IF sokidvar = 50 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ligger arbetskod i A, l�pnr i B och antal i C  ? - Svara Ja","").
      IF sokidvar = 51 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ligger arbetskod och l�pnr i A och antal i C  ? - Svara Nej","").
      IF sokidvar = 52 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inl�sning av Excel-fil till kalkyl","").
      IF sokidvar = 53 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du �r inte beh�rig att �ndra denna kalkyl!","").
      IF sokidvar = 54 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Hittade inte modul -","").
      IF sokidvar = 55 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"-. Avslutar.","").
      IF sokidvar = 56 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kan inte skapa Kalkyl!","").
      IF sokidvar = 57 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kan inte ladda Kalkyl!","").
      IF sokidvar = 58 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inloggningen misslyckades!","").
      IF sokidvar = 59 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du �r inte beh�rig att ta bort denna kalkyl!","").
      IF sokidvar = 60 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det g�r inte att ta bort Katalogen! Den anv�nds. ","").
      IF sokidvar = 61 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det g�r inte att koppla Katalogen. Det blir dubbletter!","").
      IF sokidvar = 62 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Felaktigt v�rde! Arbetskoden finns redan eller har ett ogiltigt v�rde.","").
      IF sokidvar = 63 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Funktionen g�r inte att n� f�r vald leverant�r!","").
      IF sokidvar = 64 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod finns ej i vald katalog!","").   
      IF sokidvar = 65 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har skapat en ny kalkyl, vill du spara den?","").
      /*FR�N KALKBERAPPDS.p*/
      IF sokidvar = 66 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det g�r inte att l�gga upp kalkyler p� detta","").
      IF sokidvar = 67 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Nummerserie saknas eller �r fylld.","").
      IF sokidvar = 68 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ben�mningen f�r inte vara blank!","").
      IF sokidvar = 69 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kalkylansavarig kan inte vara blank!","").
      IF sokidvar = 70 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ingen ansvarig med enhet ","").
      IF sokidvar = 71 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"finns i registret! ","").
      
      IF sokidvar = 72 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Spara kalkyl? ","").
      IF sokidvar = 73 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ingen rad �r markerad ","").
      /*�ndra*/
      IF sokidvar = 74 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du m�ste markera en rad","").
      
      IF sokidvar = 75 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ingen rad ","").
      IF sokidvar = 76 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns ingen rad att ta bort ","").
      /*Kopiering*/
      IF sokidvar = 77 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kopieringen genomf�rd ","").
      IF sokidvar = 78 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en kopierad kalkyl. Din gamla kalkyl finns fortfarande sparad. ","").
      /*Konvertering*/
      IF sokidvar = 79 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Konverteringen genomf�rd ","").
      IF sokidvar = 80 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar med en konverterad kalkyl. Din gamla kalkyl finns fortfarande sparad. ","").
      /*XML export*/
      IF sokidvar = 81 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"XML-import genomf�rd ","").
      IF sokidvar = 82 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en importerad kalkyl. Din gamla kalkyl finns fortfarande sparad. ","").
      /*XML export e-post*/
      IF sokidvar = 83 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Skicka via mail? ","").
      IF sokidvar = 84 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Din kalkyl �r nu exporterad till ","").
      IF sokidvar = 85 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du skicka filen till en e-post adress? ","").
      /*EXCELIMPORT*/
      /*IF sokidvar = 86 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inneh�ller Excel -filen f�rklaringsraden till kolumnerna? ","").*/
      IF sokidvar = 86 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r f�rsta inl�sningsraden i filen rad 3? (annars antas f�rsta inl�sningsraden vara rad 2) ","").
      IF sokidvar = 87 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du kopiera vald katalog? ","").
      IF sokidvar = 88 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du ta bort vald katalog? ","").
      
      IF sokidvar = 89 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du v�ljer att det ska vara frekvens p� denna kod kommer befintliga resurer p� koden att tas bort. Vill du detta? ","").
      IF sokidvar = 90 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du v�ljer att det inte skall vara frekvens p� denna kod kommer befintliga frekvenser p� koden att tas bort. Vill du detta? ","").
      IF sokidvar = 91 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du spara dina �ndringar p� denna niv� och sedan uppdatera upp till P1-niv�? Svarar du Nej kommer bara �ndringarna sparas men inga ber�kningar g�ras. ","").
      IF sokidvar = 92 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du spara dina �ndringar p� denna niv�? Svarar du Nej kommer �ndringarna tas bort. ","").
      IF sokidvar = 93 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du v�ljer att det inte skall vara frekvens p� denna kod kommer befintliga frekvenser och tillkommande beredning p� koden att tas bort. Vill du detta? ","").
      IF sokidvar = 94 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna kod kan ej tas bort, den anv�nds i frekvenstabell f�r kod:  ","").
      IF sokidvar = 95 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Alla resurser, frekvenser och ev materiel kopplade till denna kod kommer ocks� tas bort vid borttag. �r du s�ker p� att du vill ta bort dessa koder?","").
      IF sokidvar = 96 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Alla kopplade l�pnr, resurser, frekvenser och ev materiel kopplade till denna kod kommer ocks� tas bort vid borttag. �r du s�ker p� att du vill ta bort dessa koder?","").
      IF sokidvar = 97 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vid uppdatering av EA-faktorn r�knas samtliga resurser EA-m�ngd om. Vill du detta?","").
      IF sokidvar = 98 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod finns i Katalogen och kommer att ers�tta befintlig kod.","").
      IF sokidvar = 99 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny arbetskod! L�ggs till i Katalogen.","").
      IF sokidvar = 100 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod har ogiltig resurs. Kommer att l�sas in utan resurser.","").
      IF sokidvar = 101 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns redan inl�sta resurser f�r denna kod. De redan inl�sta resurserna kommer att ers�ttas med dessa nya.","").
      
      IF sokidvar = 102 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ta bort denna resurs ur denna katalog?","").
      IF sokidvar = 103 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna resurs kan inte tas bort fr�n denna katalog. Den anv�nds p�: ","").
      IF sokidvar = 104 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Detta �r inte en giltig regel. Se under fliken Regel f�r giltiga regler","").
      IF sokidvar = 105 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen byta regel f�r denna resurs?","").
      
      IF sokidvar = 106 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koderna f�r denna frekvens finns inte i katalogen. Frekvens kommer inte att l�sas in!","").
      /*IF sokidvar = 107 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har redan frekvens. Frekvens uppl�gget kommer att ers�ttas/kompletteras!","").*/
      IF sokidvar = 107 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna frekvenskod finns redan upplagd p� koden. G�rs valet L�s in Excel lista, s� ers�tts uppl�gget p� koden. G�rs valet Komplettera med Excel lista s� kommer denna kod ej att l�sas in.","").
      IF sokidvar = 108 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har sedan tidigare ett resursuppl�gg. Detta kommer att ers�ttas med denna frekvens.","").
      IF sokidvar = 109 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden kommer att f� ett frekvens uppl�gg.","").
      IF sokidvar = 110 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Arbetskod finns inte! Posten kommer inte att l�sas in.","").
      IF sokidvar = 111 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Byte mellan resurs och frekvens kommer att ske.","").
      IF sokidvar = 112 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kod har resurs. Inget kommer att l�sas in.","").
      IF sokidvar = 113 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vid uppdatering kommer alla ej l�sta artiklar att r�knas upp med P�slag f�r materielhanteringomkostnad. Vill du detta?","").
      IF sokidvar = 114 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vid uppdatering kommer alla artiklar (�ven l�sta) att r�knas upp med P�slag f�r materielhanteringomkostnad. Vill du detta?","").
      IF sokidvar = 115 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ta bort denna resurs p� denna niv�?","").
      IF sokidvar = 116 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du uppdaterar Kalkylkostnad manuellt kommer detta f�lt l�sas f�r uppdatering. Vill du detta?","").
      IF sokidvar = 117 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du uppdaterar p�slaget kommer ber�rda poster att r�knas upp. Vill du detta?","").
      IF sokidvar = 118 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen �ndra detta p�slag till 0?","").
      IF sokidvar = 119 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ta bort denna resurs f�r alla kataloger?","").
      IF sokidvar = 120 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna resurs kan inte tas bort. Den anv�nds p�: ","").
      IF sokidvar = 121 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du ta bort l�sningen p� denna post? Kalkylkostnad kommer i s� fall att r�knas om. Vill du detta?","").
      IF sokidvar = 122 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du l�sa Kalkylkostnad fr�n automatisk uppdatering?","").
      IF sokidvar = 123 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen ta bort denna artikel ur denna katalog?","").
      IF sokidvar = 124 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna artikel kan inte tas bort ur denna katalog. Den anv�nds p�:","").
      IF sokidvar = 125 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om du skriver in en Faktor f�r uppr�kning av �vriga kostnader kommer antalet p� alla P2-resurser 61-�vriga kostnader i denna katalog att r�knas upp med denna faktor. 
      Sedan kommer Faktor att l�ggas tillbaka till 0. Vill du detta?","").
      IF sokidvar = 126 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Alla P2-resurser 61-�vriga kostnader i denna katalog r�knas vid Spara upp med : ","").
      IF sokidvar = 127 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen uppdatera kostnad p� denna resurs?","").
      IF sokidvar = 128 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du verkligen uppdatera kostnad p� denna artikel?","").
      IF sokidvar = 129 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Artikeln finns i katalogen och kommer att uppdateras","").
      IF sokidvar = 130 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Artikeln finns inte i katalogen och kommer att l�ggas upp som nytt materiel","").
      IF sokidvar = 131 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Artikeln finns inte i katalogen, ben�mning saknas f�r att l�gga upp nytt materiel","").
      IF sokidvar = 132 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har gjort �ndringar i katalogen som �nnu inte �r sparade, vill du spara dessa innan du avslutar programmet?","").
      IF sokidvar = 133 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Artikeln som du vill uppdatera �r l�st i katalogen, kalkylkostnad kommer ej att uppdateras","").
      IF sokidvar = 134 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Katalogen �r �ppnad i l�st l�ge","").
      IF sokidvar = 135 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ben�mningen p� resursen g�ller f�r alla kataloger. Vill du verkligen byta ben�mning p� denna resurs? ","").
      IF sokidvar = 136 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Enhet p� resursen g�ller f�r alla kataloger. Vill du verkligen byta enhet p� denna resurs? ","").
      IF sokidvar = 137 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"N�r en resurs l�ggs inaktiv g�r den inte l�ngre att l�gga till en katalog. Vill du inaktivera markerad resurs? ","").
      IF sokidvar = 138 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"N�r en resurs l�ggs aktiv g�r den att l�gga till en katalog. Vill du aktivera markerad resurs? ","").
      IF sokidvar = 139 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"De f�r�ndringar som du har gjort kommer att p�verka samtliga kataloger, �r du s�ker p� att du vill g�ra detta?","").
      IF sokidvar = 140 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Valda kataloger kan inte j�mf�ras!","").
      IF sokidvar = 141 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Din nuvarande katalog st�ngs. Vill du jobba med den igen v�lj den p� nytt!","").  
      IF sokidvar = 142 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du exportera kostnad?","").
      IF sokidvar = 143 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du l�sa vald katalog?","").
      IF sokidvar = 144 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du l�sa upp vald katalog?","").
      IF sokidvar = 145 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Den katalog du arbetade med kommer nu att laddas om",""). 
      IF sokidvar = 146 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har redan frekvens. Frekvens uppl�gget kommer att raderas!","").
      IF sokidvar = 147 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny kod! L�ggs till i Katalogen.",""). 
      IF sokidvar = 148 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r du s�ker p� att du vill byta ben�mning?",""). 
      IF sokidvar = 149 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Katalogen �r l�st, du m�ste l�sa upp den innan du kan ta bort den",""). 
      IF sokidvar = 150 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r du s�ker p� att du uppdatera �rtalet?","").
      IF sokidvar = 151 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Katalogen anv�nds och g�r d�rf�r inte att ta bort","").
      IF sokidvar = 152 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har gjort �ndringar i katalogen som �nnu inte �r sparade, vill du spara dessa innan du st�nger katalogen?","").
      IF sokidvar = 153 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny arbetskod!Saknar ben�mning. Arbetskoden kommer inte att l�sas in!","").
      IF sokidvar = 154 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny arbetskod!Saknar enhet. Arbetskoden kommer inte att l�sas in!","").
      IF sokidvar = 155 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Import-posterna f�r denna arbetskod har inte samma angivelse om det skall vara frekvens eller inte! Arbetskoden kommer inte att l�sas in!","").
      IF sokidvar = 156 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Resurs 101- Tillkommande beredning kan bara anv�ndas n�r det inte �r frekvens.Koden kommer att l�sas in utan denna resurs!","").
      IF sokidvar = 157 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Filen finns sparad i mapp -","").
      IF sokidvar = 158 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns ingen kod med denna arbetskod och l�pnummer, materielet kommer inte att l�ggas till -","").   
      IF sokidvar = 159 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns inget materiel med detta artikelnummer i katalogen -","").   
      IF sokidvar = 160 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns materiel p� denna arbetskod/l�pnummer, dessa materiel kommer att ers�ttas -","").  
      IF sokidvar = 161 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Materielet kommer att l�ggas till p� arbetskoden -",""). 
      IF sokidvar = 162 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du ska anv�nda ',' som decimalkomma ","").
      IF sokidvar = 163 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du ska anv�nda '.' som decimalkomma ","").
      IF sokidvar = 164 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inl�sning klar!",""). 
      IF sokidvar = 165 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Om mont�rskostnaden uppdateras s� uppdateras �ven Ea-faktorn och samtliga resursers EA-m�ngd r�knas om. Vill du detta?","").
      IF sokidvar = 166 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Felaktigt v�rde! L�pnummret finns redan eller har ett ogiltigt v�rde.","").
      IF sokidvar = 167 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Din katalog �r nu exporterad till ","").
      IF sokidvar = 168 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"L�ses av ","").
      IF sokidvar = 169 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�ndring ej m�jlig.","").
      IF sokidvar = 170 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du forts�tta �nd�?","").
      IF sokidvar = 171 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kan inte skapa �rende!","").
      IF sokidvar = 172 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kan inte ladda �rende!","").
      IF sokidvar = 173 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Felaktig markering!","").
      IF sokidvar = 174 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Minst 2 poster f�r ett Svep!","").
      IF sokidvar = 175 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Alla poster i ett Svep m�ste ha samma arbetskod!","").
      IF sokidvar = 176 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inga Kommentarer valda!","").
      IF sokidvar = 177 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Svepet finns redan!","").
      IF sokidvar = 178 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ," anv�nder Kommentaren!","").
      IF sokidvar = 179 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"G�r inte att ta bort!","").
      IF sokidvar = 180 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det g�r inte att ta bort Katalogen! Den anv�nds till vissa kalkyler. ","").
      IF sokidvar = 181 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det g�r inte att ta bort DelKatalogen! Den anv�nds som delkatalog. ","").
      IF sokidvar = 182 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det g�r inte att ta bort Katalogen! DelKatalogerna anv�ns till flera Kataloger. ","").
      IF sokidvar = 183 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du fick n�gra fel! Kontrollera under fliken f�r felmeddelande. ","").
      IF sokidvar = 184 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte flytta posten, f�r att det finns dubbletter. ","").      
      IF sokidvar = 185 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Anv�nda �rets timpriser-svara JA. Anv�nda fjol�rets timpriser -svara NEJ. ","").
      IF sokidvar = 186 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Skall timpriser vara avrundare utan decimaler? ","").
      IF sokidvar = 187 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Arbetskod och l�pnr i A -Ja, eller Arbetskod i A och l�pnr i B-Nej ","").
      IF sokidvar = 188 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en ihopslagen kalkyl. Dina gamla kalkyler finns fortfarande sparade. ","").
      IF sokidvar = 189 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Sammanslagningen genomf�rd ","").
      IF sokidvar = 190 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Sortering �r inte till�ten!","").
      IF sokidvar = 191 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte ta bort denna konstruktionsdel. Den �r ink�pt via ink�psrutinen.","").
      IF sokidvar = 192 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte ta bort denna del av konstruktionen. Du m�ste ta bort hela den valda konstruktionen.","").
      IF sokidvar = 193 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte �ndra ordning i denna konstruktion. Den �r ink�pt via ink�psrutinen.","").
      IF sokidvar = 194 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte �ndra p� denna del av konstruktionen. Den �r ink�pt via ink�psrutinen.","").
      IF sokidvar = 195 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du kan inte flytta p� posten.","").
      IF sokidvar = 196 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en ny kalkyl d�r du j�mf�r tv� kalkyler. Dina gamla kalkyler finns fortfarande sparade. ","").
      IF sokidvar = 197 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ingen av leverant�rerna har n�gon av de valda ariklarna.","").
      IF sokidvar = 198 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inga arbetskoder funna!","").
      IF sokidvar = 199 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du f�r antigen l�gga upp Beredare eller Beredare Region p� varje kod!","").
      IF sokidvar = 200 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"F�ltet �r inte ifyllt korrekt!","").
      IF sokidvar = 201 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kontrollen sker endast mot den senast sparade Katalogen! Vill du spara dina �ndringar p� denna niv�?","").
      IF sokidvar = 202 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns inget att klistra in!","").
      IF sokidvar = 203 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Anv�nda �rets timpriser-svara JA. Anv�nda kopierade katalogens timpriser -svara NEJ. ","").
      IF sokidvar = 204 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Personalliggaren �r aktiverad","").
      IF sokidvar = 205 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Personalliggaren �r inaktiverad","").
      IF sokidvar = 206 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inga poster hittades p� s�kbegreppet","").
      IF sokidvar = 207 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"S�kbegreppet �r f�r kort!","").
      IF sokidvar = 208 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"�r du s�ker p� att du vill ta bort denna rad?","").
      IF sokidvar = 209 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Utf�rdare kan inte vara blank!","").
      IF sokidvar = 210 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Koden har redan frekvens. V�lj om den ska ers�ttas eller kompletteras! Via knapparna L�s in Excel listan resp. Komplettera med Excel listan","").
      IF sokidvar = 211 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Posten �r redan upplagd!","").
      IF sokidvar = 212 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inl�sningen slutf�rd.","").
      IF sokidvar = 213 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Obs! Orginalkalkylen flyttas till ny katalog detta g�r inte att �ngra!",""). 
      IF sokidvar = 214 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Flytten �r genomf�rd. ","").
      IF sokidvar = 215 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du arbetar nu med en ny kalkyl.","").
      IF sokidvar = 216 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Fel filformat!","").
      IF sokidvar = 217 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Inga fel i filen hittades. Den g�r bra att l�sa in.","").
      IF sokidvar = 218 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Saknar f�retag filen g�r inte att l�sa in.","").
      IF sokidvar = 218 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har inte tagit st�llning till denna post!","").
      IF sokidvar = 219 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"L�sen m�ste vara minst 5 tecken!","").
      IF sokidvar = 220 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du �ndrar l�sen f�r denna anv�ndre. Vill du g�ra detta?","").
      IF sokidvar = 221 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"User f�r inte vara blank!","").
      IF sokidvar = 222 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"User finns redan!","").
      IF sokidvar = 223 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ny anv�ndare!","").
      IF sokidvar = 224 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Posten sparades inte f�r att du har fel v�rde i User! Alla poster med fel v�rde i User kommer att rensas bort vid n�sta sparning!","").
      IF sokidvar = 225 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Nytt l�sen!","").
      IF sokidvar = 226 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Posten sparades inte f�r att du har fel v�rde i Password! Alla poster med fel v�rde i Password kommer att rensas bort vid n�sta sparning!","").
      IF sokidvar = 227 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Du har n�tt maximalt antal Excelf�nster, st�ng f�r att skapa nya!","").
      IF sokidvar = 228 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"F�r att h�mta grunduppl�gg m�ste du radera dina koder f�rst!","").
      IF sokidvar = 229 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ska alla arbetskoder kopieras?","").
      IF sokidvar = 230 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns dubbletter! ","").
      IF sokidvar = 231 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du �ven kopiera Frikalkylen? ","").
      IF sokidvar = 232 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Det finns ingen beredning att kopiera!","").
      IF sokidvar = 233 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du kopiera denna beredning?","").
      IF sokidvar = 234 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ditt nya beredningsnummer �r:","").
      IF sokidvar = 235 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kopiering avbruten!","").
      IF sokidvar = 236 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Vill du ta bort Ers�ttningslistan? ","").
      IF sokidvar = 237 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna Ers�ttningslistan g�r inte att ta bort!","").
      IF sokidvar = 238 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Denna �vers�ttning finns redan upplagd!","").
      IF sokidvar = 239 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Angiven leverant�r finns ej upplagd!","").
      IF sokidvar = 240 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Ta bort sista posten p� en konstruktion m�ste g�ras i beredningsmodulen!","").
      IF sokidvar = 241 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Angivet enr finns ej hos leverant�r","").
      IF sokidvar = 242 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Kalkylkoden �r en dublett p� detta byggnr!","").
      IF sokidvar = 243 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Byggnr finns ej i beredning!","").
      IF sokidvar = 244 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Anv�nda �rets timpriser-svara JA. Anv�nda kopierade katalogens timpriser -svara NEJ. ","").
      IF sokidvar = 245 THEN RUN sprakstrankcreate_UI (langid ,sokidvar ,"Delsumma f�r markerade poster!","").
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
