/*GKALEKO2.P*/
/*{EGENBEN.I}*/
DEFINE TEMP-TABLE ekoforst
   FIELD FTG AS CHARACTER
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD EORG LIKE EKRAPPRESULT.EORG     
   FIELD EGEO LIKE EKRAPPRESULT.EGEO  
   FIELD EVERDATUM LIKE EKRAPPRESULT.EVERDATUM
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD ERESULTENH LIKE EKRAPPRESULT.ERESULTENH 
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL LIKE EKRAPPRESULT.EANTAL 
   FIELD ETIMMAR LIKE EKRAPPRESULT.EANTAL                
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   FIELD PERSTYP AS CHARACTER         
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT DELNR ASCENDING.
DEFINE TEMP-TABLE eko
   FIELD FTG AS CHARACTER
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER
   FIELD EDEBKRED LIKE EKRAPPRESULT.EDEBKRED
   FIELD EVERDATUM LIKE EKRAPPRESULT.EVERDATUM  
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD EORG LIKE EKRAPPRESULT.EORG 
   FIELD EGEO LIKE EKRAPPRESULT.EGEO       
   FIELD EKOSTNADSSLAG LIKE EKRAPPRESULT.EKOSTNADSSLAG    
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL  LIKE EKRAPPRESULT.EANTAL 	       
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   INDEX ORG IS PRIMARY EVERDATUM EORG EPROJEKT EKOSTNADSSLAG ASCENDING.   
DEFINE TEMP-TABLE slutut
   FIELD FTG AS CHARACTER
   FIELD DEBKRED AS LOGICAL 
   FIELD ANTAL AS DECIMAL 
   FIELD BELOPP AS DECIMAL       
   FIELD K1 AS CHARACTER
   FIELD K2 AS CHARACTER
   FIELD K3 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD K5 AS CHARACTER
   FIELD BIL AS CHARACTER
   FIELD K2POS8 AS CHARACTER
   INDEX ORG IS PRIMARY FTG DEBKRED K1 K2 K3 K4 K5.
DEFINE TEMP-TABLE slututK LIKE slutut.

DEFINE TEMP-TABLE omrrgrtemp
   FIELD OMRADE AS CHARACTER
   FIELD SPECOMR AS CHARACTER
   FIELD RGR AS CHARACTER
   FIELD PERSTYP AS CHARACTER
   INDEX PERSTYP IS PRIMARY PERSTYP OMRADE.
DEFINE TEMP-TABLE omrkosttemp
   FIELD OMRADE AS CHARACTER
   FIELD KOSTNADSLAGDEB AS CHARACTER
   FIELD KOSTNADSLAGKRED AS CHARACTER
   FIELD KOSTNADSLAGDEB2 AS CHARACTER
   FIELD KOSTNADSLAGKRED2 AS CHARACTER
   FIELD PERSTYP AS CHARACTER
   INDEX PERSTYP IS PRIMARY PERSTYP OMRADE.
DEFINE BUFFER eko2 FOR eko.
DEFINE BUFFER persbuff FOR PERSONALTAB.
DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER skarpvar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ekoforst.

DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE nytid AS DECIMAL FORMAT "99.99" NO-UNDO.
DEFINE NEW SHARED VARIABLE sekunder AS INTEGER FORMAT "-9999999" NO-UNDO. 
DEFINE NEW SHARED VARIABLE persrec AS RECID NO-UNDO. 
DEFINE VARIABLE sattfore AS CHARACTER NO-UNDO.
DEFINE VARIABLE summmallab AS DECIMAL NO-UNDO.
DEFINE VARIABLE summmallabch AS CHARACTER NO-UNDO.
DEFINE VARIABLE summmallaant AS DECIMAL NO-UNDO.
DEFINE VARIABLE summmallaantch AS CHARACTER NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO. 
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE filnamn1 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE filnamn2 AS CHARACTER NO-UNDO. 
DEF VAR debvar AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
DEF VAR debvar2 AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
DEF VAR kredvar AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
DEF VAR diffvar AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
DEF VAR kredvar2 AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
DEF VAR diffvar2 AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
DEFINE VARIABLE nolldummy AS CHARACTER NO-UNDO.
nolldummy = "00000000000000000000000000000000000000000000000000000000000000000000000000000000000".
DEFINE VARIABLE pkoder LIKE PERSONALTAB.PERSONALKOD NO-UNDO.  
DEFINE VARIABLE kontokod LIKE EKRAPPRESULT.EKOSTNADSSLAG NO-UNDO. 
DEFINE VARIABLE kontokode LIKE EKRAPPRESULT.EKOSTNADSSLAG NO-UNDO. 
DEFINE VARIABLE kontokod2 LIKE EKRAPPRESULT.EKOSTNADSSLAG NO-UNDO. 
DEFINE VARIABLE kontokode2 LIKE EKRAPPRESULT.EKOSTNADSSLAG NO-UNDO. 

DEFINE VARIABLE i AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(250)" NO-UNDO.
DEFINE VARIABLE utnrin AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE breddin AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE nrcolin AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE breddantalin AS INTEGER NO-UNDO.

DEFINE VARIABLE utnr AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE bredd AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE nrcol AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE breddantal AS INTEGER NO-UNDO.

DEFINE VARIABLE utnrav AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE breddav AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE nrcolav AS INTEGER EXTENT 55 NO-UNDO.
DEFINE VARIABLE breddantalav AS INTEGER NO-UNDO.
FIND FIRST FORETAG NO-LOCK NO-ERROR.
IF FORETAG.FORETAG = "GKAL" THEN DO:   
   ASSIGN
   kommando2 = "\\GRANGURU\guru_ser\server\PRO9S\ekobac\"
   kommando = "\\GRANGURU\guru_ser\server\PRO9S\"
   filnamn1 = "GKEAB" + STRING(TODAY,"99999999")
   filnamn2 = "SEFAB" + STRING(TODAY,"99999999").
END.
ELSE DO:
   ASSIGN
   kommando2 = "D:\DELAD\PRO9S\DBBACKUP\"
   kommando = "D:\DELAD\PRO9S\EXPORT\"
   filnamn1 = "GKEAB" + STRING(TODAY,"99999999")
   filnamn2 = "SEFAB" + STRING(TODAY,"99999999").
END.
RUN ladda_UI.
/*DEBET OCH KREDIT KONTON*/
RUN omrk_UI (INPUT "110",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "310",INPUT "97210",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "430",INPUT "97220",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "610",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "620",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "640",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").

RUN omrk_UI (INPUT "S110",INPUT "97210",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "S210",INPUT "97220",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "S220",INPUT "97260",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "S310",INPUT "97230",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "S410",INPUT "97240",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").
RUN omrk_UI (INPUT "S510",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "PERS").

RUN omrk_UI (INPUT "110",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "310",INPUT "97210",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "430",INPUT "97221",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "610",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "620",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "640",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").

RUN omrk_UI (INPUT "S110",INPUT "97210",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "S210",INPUT "97220",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "S220",INPUT "97260",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "S310",INPUT "97230",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "S410",INPUT "97240",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").
RUN omrk_UI (INPUT "S510",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "ENKE").

RUN omrk_UI (INPUT "110",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "310",INPUT "97210",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "430",INPUT "97222",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "610",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "620",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "640",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").

RUN omrk_UI (INPUT "S110",INPUT "97210",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "S210",INPUT "97220",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "S220",INPUT "97260",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "S310",INPUT "97230",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "S410",INPUT "97240",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").
RUN omrk_UI (INPUT "S510",INPUT "97250",INPUT "97900",INPUT "",INPUT "",INPUT "KVAL").

RUN omrk_UI (INPUT "",INPUT "92720",INPUT "93720",INPUT "",INPUT "",INPUT "SKYL").
RUN omrk_UI (INPUT "",INPUT "94610",INPUT "99460",INPUT "90410",INPUT "90490",INPUT "MTRL").

RUN omrrgr_UI (INPUT "110",INPUT "",INPUT "81000",INPUT "PERS").
RUN omrrgr_UI (INPUT "310",INPUT "",INPUT "15190",INPUT "PERS").
RUN omrrgr_UI (INPUT "430",INPUT "",INPUT "21000",INPUT "PERS").
RUN omrrgr_UI (INPUT "610",INPUT "",INPUT "82100",INPUT "PERS").
RUN omrrgr_UI (INPUT "620",INPUT "",INPUT "82200",INPUT "PERS").
RUN omrrgr_UI (INPUT "640",INPUT "",INPUT "82400",INPUT "PERS"). 

RUN omrrgr_UI (INPUT "S110",INPUT "",INPUT "81000",INPUT "PERS").
RUN omrrgr_UI (INPUT "S210",INPUT "",INPUT "82000",INPUT "PERS").
RUN omrrgr_UI (INPUT "S220",INPUT "",INPUT "82100",INPUT "PERS").
RUN omrrgr_UI (INPUT "S310",INPUT "",INPUT "83000",INPUT "PERS").
RUN omrrgr_UI (INPUT "S410",INPUT "",INPUT "84000",INPUT "PERS").
RUN omrrgr_UI (INPUT "S510",INPUT "",INPUT "85000",INPUT "PERS").


RUN omrrgr_UI (INPUT "110",INPUT "",INPUT "81000",INPUT "ENKE").
RUN omrrgr_UI (INPUT "310",INPUT "",INPUT "15190",INPUT "ENKE").
RUN omrrgr_UI (INPUT "430",INPUT "",INPUT "21000",INPUT "ENKE").
RUN omrrgr_UI (INPUT "610",INPUT "",INPUT "82100",INPUT "ENKE").
RUN omrrgr_UI (INPUT "620",INPUT "",INPUT "82200",INPUT "ENKE").
RUN omrrgr_UI (INPUT "640",INPUT "",INPUT "82400",INPUT "ENKE"). 

RUN omrrgr_UI (INPUT "S110",INPUT "",INPUT "81000",INPUT "ENKE").
RUN omrrgr_UI (INPUT "S210",INPUT "",INPUT "82000",INPUT "ENKE").
RUN omrrgr_UI (INPUT "S220",INPUT "",INPUT "82100",INPUT "ENKE").
RUN omrrgr_UI (INPUT "S310",INPUT "",INPUT "83000",INPUT "ENKE").
RUN omrrgr_UI (INPUT "S410",INPUT "",INPUT "84000",INPUT "ENKE").
RUN omrrgr_UI (INPUT "S510",INPUT "",INPUT "85000",INPUT "ENKE").

RUN omrrgr_UI (INPUT "110",INPUT "",INPUT "81000",INPUT "KVAL").
RUN omrrgr_UI (INPUT "310",INPUT "",INPUT "15190",INPUT "KVAL").
RUN omrrgr_UI (INPUT "430",INPUT "",INPUT "21000",INPUT "KVAL").
RUN omrrgr_UI (INPUT "610",INPUT "",INPUT "82100",INPUT "KVAL").
RUN omrrgr_UI (INPUT "620",INPUT "",INPUT "82200",INPUT "KVAL").
RUN omrrgr_UI (INPUT "640",INPUT "",INPUT "82400",INPUT "KVAL"). 

RUN omrrgr_UI (INPUT "S110",INPUT "",INPUT "81000",INPUT "KVAL").
RUN omrrgr_UI (INPUT "S210",INPUT "",INPUT "82000",INPUT "KVAL").
RUN omrrgr_UI (INPUT "S220",INPUT "",INPUT "82100",INPUT "KVAL").
RUN omrrgr_UI (INPUT "S310",INPUT "",INPUT "83000",INPUT "KVAL").
RUN omrrgr_UI (INPUT "S410",INPUT "",INPUT "84000",INPUT "KVAL").
RUN omrrgr_UI (INPUT "S510",INPUT "",INPUT "85000",INPUT "KVAL").
 
RUN omrrgr_UI (INPUT "",INPUT "311",INPUT "72199",INPUT "SKYL").
RUN omrrgr_UI (INPUT "",INPUT "311",INPUT "71100",INPUT "MTRL").
/*POSTER FÅR RÄTT KONTO*/
RUN kontoposter_UI.      /*debet per aonr kredit per aonr och rätt område*/
/*MATERIAL*/
RUN mtrl_UI (INPUT "MTRL").
/*KONTOFÖRDELLNING*/
OUTPUT TO "\\GRANGURU\guru_ser\server\PRO9S\cc2.txt".
FOR EACH eko WHERE eko.FTG = "800" AND eko.eDEBKRED = FALSE:
    DISP eko.k1 eko.k2 eko.k3 eko.k4 eko.ebelopp eko.eorg eko.eproj.   
END.
OUTPUT CLOSE.
RUN kontofordel_UI.      /*fördelning av på konto och blankning av nej aonr*/
/*KREDITPOSTER*/


OUTPUT TO "\\GRANGURU\guru_ser\server\PRO9S\cck.txt".
FOR EACH slutut WHERE slutut.FTG = "800" AND slutut.DEBKRED = FALSE:
    DISP slutut.k1 slutut.k2 slutut.k3 slutut.k4 slutut.belopp.   
END.
OUTPUT CLOSE.
RUN kreditposter_UI.    /*summering av kredit*/

/*FILEN*/
FIND FIRST slutut WHERE slutut.FTG = "100" NO-ERROR.
IF AVAILABLE slutut THEN RUN filut_UI (INPUT slutut.FTG,INPUT kommando + filnamn1,INPUT kommando2 + filnamn1).
FIND FIRST slutut WHERE slutut.FTG = "800" NO-ERROR.
IF AVAILABLE slutut THEN RUN filut_UI (INPUT slutut.FTG,INPUT kommando + filnamn2,INPUT kommando2 + filnamn2).
FIND FIRST slutut WHERE slutut.FTG = "999" NO-ERROR.
IF AVAILABLE slutut THEN RUN filut_UI (INPUT slutut.FTG,INPUT kommando + filnamn1,INPUT kommando2 + filnamn1).

PROCEDURE omrrgr_UI:
   DEFINE INPUT PARAMETER omrvar AS CHARACTER.
   DEFINE INPUT PARAMETER eomrvar AS CHARACTER.
   DEFINE INPUT PARAMETER rgrvar AS CHARACTER.
   DEFINE INPUT PARAMETER ptypvar AS CHARACTER.
   CREATE omrrgrtemp.
   ASSIGN
   omrrgrtemp.SPECOMR = eomrvar
   omrrgrtemp.OMRADE  = omrvar 
   omrrgrtemp.RGR = rgrvar    
   omrrgrtemp.PERSTYP = ptypvar.
END PROCEDURE.

PROCEDURE omrk_UI:
   DEFINE INPUT PARAMETER omrvar AS CHARACTER.
   DEFINE INPUT PARAMETER debvar AS CHARACTER.
   DEFINE INPUT PARAMETER krevar AS CHARACTER.
   DEFINE INPUT PARAMETER debvar2 AS CHARACTER.
   DEFINE INPUT PARAMETER krevar2 AS CHARACTER.
   DEFINE INPUT PARAMETER ptypvar AS CHARACTER.
   CREATE omrkosttemp.
   ASSIGN
   omrkosttemp.OMRADE  = omrvar
   omrkosttemp.KOSTNADSLAGDEB  = debvar 
   omrkosttemp.KOSTNADSLAGKRED = krevar 
   omrkosttemp.KOSTNADSLAGDEB2 = debvar2
   omrkosttemp.KOSTNADSLAGKRED2 = krevar2
   omrkosttemp.PERSTYP         = ptypvar.
END PROCEDURE.
PROCEDURE kontoposter_UI:
   pkoder = "". 
   OPEN QUERY qeko FOR EACH ekoforst WHERE ekoforst.ENY = FALSE USE-INDEX PERSORG NO-LOCK.
   GET FIRST qeko.
   DO WHILE AVAILABLE(ekoforst):
      IF pkoder NE ekoforst.EPERSONALKOD THEN DO:      
         pkoder = ekoforst.EPERSONALKOD.      
         FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkoder 
         USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.        
         persrec = RECID(PERSONALTAB).     
      END.   
      FIND FIRST omrkosttemp WHERE omrkosttemp.OMRADE  = ekoforst.EORG AND omrkosttemp.PERSTYP = "PERS" 
      NO-LOCK NO-ERROR.
      ASSIGN
      kontokod =  omrkosttemp.KOSTNADSLAGDEB 
      kontokode = omrkosttemp.KOSTNADSLAGKRED.         
      /*TIMMAR OCH PENNGAR*/
      IF ekoforst.EBELOPP = 0 THEN DO:
         persrec = persrec.
      END.                                   
      ELSE DO:      
         RUN kost_UI (INPUT TRUE,INPUT "PERS").      
      END.      
      IF ekoforst.ELONTILLAGG = "" THEN DO:
         persrec = persrec.
      END.                                   
      ELSE DO:
         /*ÖVERTIDTILLÄGG*/ 
         IF ekoforst.ERESULTENH = "OVE" THEN DO:            
            FIND FIRST omrkosttemp WHERE omrkosttemp.OMRADE  = ekoforst.EORG AND omrkosttemp.PERSTYP = ekoforst.PERSTYP 
            NO-LOCK NO-ERROR.
            ASSIGN
            kontokod =  omrkosttemp.KOSTNADSLAGDEB 
            kontokode = omrkosttemp.KOSTNADSLAGKRED.         
            RUN kost_UI (INPUT FALSE,INPUT ekoforst.PERSTYP). 
         END. 
            /*BILAR*/
         IF ekoforst.ERESULTENH = "BIL" THEN DO:                        
            FIND FIRST omrkosttemp WHERE 
            omrkosttemp.PERSTYP = "SKYL"
            NO-LOCK NO-ERROR.
            IF AVAILABLE omrkosttemp THEN DO:
               ASSIGN
               kontokod =  omrkosttemp.KOSTNADSLAGDEB 
               kontokode = omrkosttemp.KOSTNADSLAGKRED.         
               RUN bilkost_UI (INPUT "SKYL").   
            END.
         END.                           
      END.
      DELETE ekoforst.
      GET NEXT qeko.   
   END.
END PROCEDURE.
PROCEDURE kost_UI:
   DEFINE INPUT PARAMETER timlon AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER typvar AS CHARACTER  NO-UNDO.
   FIND FIRST omrrgrtemp WHERE omrrgrtemp.OMRADE = ekoforst.EORG AND
   omrrgrtemp.PERSTYP = typvar NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrrgrtemp THEN DO:
      FIND FIRST omrrgrtemp WHERE 
      omrrgrtemp.PERSTYP = typvar NO-LOCK NO-ERROR.
   END.
   /*DEBET POST*/
   FIND FIRST eko WHERE
   eko.EDEBKRED = TRUE AND
   eko.FTG = ekoforst.FTG AND
   eko.EORG = ekoforst.EGEO AND 
   eko.EVERDATUM = ekoforst.EVERDATUM AND
   eko.EPROJEKT = ekoforst.EPROJEKT AND
   eko.DELNR = ekoforst.DELNR AND
   eko.EKOSTNADSSLAG = kontokod
   USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
   IF NOT AVAILABLE eko THEN DO:
      CREATE eko.
   END.    
   ASSIGN         
   eko.K1 = ""
   eko.K2 = ""
   eko.K3 = ""
   eko.K4 = ekoforst.EPROJEKT
   eko.K5 = kontokod
   eko.FTG = ekoforst.FTG
   eko.EDEBKRED = TRUE 
   eko.EORG = ekoforst.EGEO  
   eko.EVERDATUM = ekoforst.EVERDATUM
   eko.EPROJEKT = ekoforst.EPROJEKT 
   eko.DELNR = ekoforst.DELNR 
   eko.EKOSTNADSSLAG = kontokod.
   IF timlon = TRUE THEN DO:
      ASSIGN
      eko.EANTAL = eko.EANTAL + ekoforst.EANTAL  
      eko.EBELOPP = eko.EBELOPP + ekoforst.EBELOPP.     
   END.
   ELSE DO:
      eko.EBELOPP = eko.EBELOPP + ekoforst.ELONBELOPP.     
   END.
   /*KREDIT POST*/          
   FIND FIRST eko2 WHERE
   eko2.EDEBKRED = FALSE AND
   eko2.FTG = eko.FTG AND
   eko2.EORG = ekoforst.EORG AND 
   eko2.EVERDATUM = ekoforst.EVERDATUM AND
   eko2.EPROJEKT = ekoforst.EPROJEKT AND
   eko2.EKOSTNADSSLAG = kontokode
   USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
   IF NOT AVAILABLE eko2 THEN DO:
      CREATE eko2.
   END.    
   IF ekoforst.EORG BEGINS "S" THEN DO:
      eko2.K1 = SUBSTRING(omrrgrtemp.OMRADE,2).
   END.
   ELSE DO:
      eko2.K1 = omrrgrtemp.OMRADE.
   END.
   ASSIGN               
   eko2.K2 = omrrgrtemp.RGR
   eko2.K3 = ""
   eko2.K4 = ekoforst.EPROJEKT
   eko2.K5 = kontokode
   eko2.FTG = eko.FTG
   eko2.EDEBKRED = FALSE 
   eko2.EORG = ekoforst.EORG  
   eko2.EVERDATUM = ekoforst.EVERDATUM
   eko2.EPROJEKT = ekoforst.EPROJEKT
   eko2.DELNR = ekoforst.DELNR 
   eko2.EKOSTNADSSLAG = kontokode.
   IF timlon = TRUE THEN DO:
      ASSIGN
      eko2.EANTAL = eko2.EANTAL + ekoforst.EANTAL  
      eko2.EBELOPP = eko2.EBELOPP + ekoforst.EBELOPP.     
   END. 
   ELSE DO:
      eko2.EBELOPP = eko2.EBELOPP + ekoforst.ELONBELOPP.     
   END.
END PROCEDURE.
PROCEDURE mtrl_UI:
   DEFINE INPUT PARAMETER typvar AS CHARACTER  NO-UNDO.
   FIND FIRST omrrgrtemp WHERE omrrgrtemp.PERSTYP = typvar NO-LOCK NO-ERROR.
   FIND FIRST omrkosttemp WHERE omrkosttemp.PERSTYP = "MTRL" 
   NO-LOCK NO-ERROR.
   ASSIGN
   kontokod =  omrkosttemp.KOSTNADSLAGDEB 
   kontokode = omrkosttemp.KOSTNADSLAGKRED
   kontokod2 =  omrkosttemp.KOSTNADSLAGDEB2 
   kontokode2 = omrkosttemp.KOSTNADSLAGKRED2.
   FIND LAST INTERNFAKTKOLL USE-INDEX VDATUM NO-LOCK NO-ERROR.
   OPEN QUERY bq FOR EACH BERBEST WHERE BERBEST.Bestdatum <= vkdatum NO-LOCK.
   
   GET FIRST bq NO-LOCK.
   DO WHILE AVAILABLE(BERBEST):
      IF SUBSTRING(BERBEST.BESTALLARE,60,1) = "W" THEN DO:
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = BERBEST.AONR AND AONRTAB.DELNR = BERBEST.DELNR
         NO-LOCK NO-ERROR.
         IF AVAILABLE AONRTAB THEN DO:
            FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = AONRTAB.OMRADE NO-LOCK NO-ERROR.
            FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.
            FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.
            IF JURPERS.JUDID = "GKEAB" THEN sattfore = "100".
            ELSE IF JURPERS.JUDID = "GSEAB" THEN sattfore = "800".
            ELSE sattfore = "999".
            FIND FIRST eko WHERE
            eko.FTG = sattfore AND
            eko.EDEBKRED = TRUE AND
            eko.EORG = AONRTAB.OMRADE AND 
            eko.EVERDATUM = STRING(DATE(MONTH(BERBEST.Bestdatum),01,YEAR(BERBEST.Bestdatum)),"999999") AND
            eko.EPROJEKT = AONRTAB.AONR AND
            eko.DELNR = AONRTAB.DELNR AND
            eko.EKOSTNADSSLAG = kontokod
            USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
            IF NOT AVAILABLE eko THEN DO:
               CREATE eko.
            END.    
            ASSIGN         
            eko.K1 = ""
            eko.K2 = ""
            eko.K3 = ""
            eko.K4 = AONRTAB.AONR
            eko.K5 = kontokod
            eko.FTG = sattfore
            eko.EDEBKRED = TRUE 
            eko.EORG = AONRTAB.OMRADE  
            eko.EVERDATUM = STRING(DATE(MONTH(BERBEST.Bestdatum),01,YEAR(BERBEST.Bestdatum)),"999999")
            eko.EPROJEKT = AONRTAB.AONR 
            eko.DELNR = AONRTAB.DELNR 
            eko.EKOSTNADSSLAG = kontokod.   
            eko.EBELOPP = eko.EBELOPP + BERBEST.PRIS * BERBEST.ANTAL.     
            /*DEBET2*/
            FIND FIRST eko WHERE
            eko.FTG = sattfore AND
            eko.EDEBKRED = TRUE AND
            eko.EORG = AONRTAB.OMRADE AND 
            eko.EVERDATUM = STRING(DATE(MONTH(BERBEST.Bestdatum),01,YEAR(BERBEST.Bestdatum)),"999999") AND
            eko.EPROJEKT = AONRTAB.AONR AND
            eko.DELNR = AONRTAB.DELNR AND
            eko.EKOSTNADSSLAG = kontokod2
            USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
            IF NOT AVAILABLE eko THEN DO:
               CREATE eko.
            END.    
            ASSIGN         
            eko.K1 = ""
            eko.K2 = ""
            eko.K3 = ""
            eko.K4 = AONRTAB.AONR
            eko.K5 = kontokod2
            eko.FTG = sattfore
            eko.EDEBKRED = TRUE 
            eko.EORG = AONRTAB.OMRADE  
            eko.EVERDATUM = STRING(DATE(MONTH(BERBEST.Bestdatum),01,YEAR(BERBEST.Bestdatum)),"999999")
            eko.EPROJEKT = AONRTAB.AONR 
            eko.DELNR = AONRTAB.DELNR 
            eko.EKOSTNADSSLAG = kontokod2.   
            eko.EBELOPP = eko.EBELOPP + BERBEST.PRIS * BERBEST.ANTAL * 0.1.          
         /*KREDIT POST*/          
            FIND FIRST eko2 WHERE
            eko2.EDEBKRED = FALSE AND
            eko2.FTG = sattfore AND
            eko2.EORG = "" AND 
            eko2.EVERDATUM = STRING(DATE(MONTH(BERBEST.Bestdatum),01,YEAR(BERBEST.Bestdatum)),"999999") AND
            eko2.EPROJEKT = "" AND
            eko2.DELNR = 0     AND
            eko2.EKOSTNADSSLAG = kontokode
            USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
            IF NOT AVAILABLE eko2 THEN DO:
               CREATE eko2.
            END.             
            ASSIGN            
            eko2.K1 = ""
            eko2.K2 = ""
            eko2.K3 = ""
            eko2.K4 = ""
            eko2.K5 = kontokode
            eko2.FTG = sattfore
            eko2.EDEBKRED = FALSE 
            eko2.EORG = ""  
            eko2.EVERDATUM = STRING(DATE(MONTH(BERBEST.Bestdatum),01,YEAR(BERBEST.Bestdatum)),"999999")
            eko2.EPROJEKT = ""
            eko2.DELNR = 0 
            eko2.EKOSTNADSSLAG = kontokode.
            eko2.EBELOPP = eko2.EBELOPP + BERBEST.PRIS * BERBEST.ANTAL.              
            /*KREDIT2*/
            FIND FIRST eko2 WHERE
            eko2.EDEBKRED = FALSE AND
            eko2.FTG = sattfore AND
            eko2.EORG = omrrgrtemp.SPECOMR AND 
            eko2.EVERDATUM = STRING(DATE(MONTH(BERBEST.Bestdatum),01,YEAR(BERBEST.Bestdatum)),"999999") AND
            eko2.EPROJEKT = "" AND
            eko2.DELNR = 0 AND
            eko2.EKOSTNADSSLAG = kontokode2
            USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
            IF NOT AVAILABLE eko2 THEN DO:
               CREATE eko2.
            END.    
            ASSIGN             
            eko2.K1 = omrrgrtemp.SPECOMR
            eko2.K2 = omrrgrtemp.RGR
            eko2.K3 = ""
            eko2.K4 = ""
            eko2.K5 = kontokode2
            eko2.FTG = sattfore
            eko2.EDEBKRED = FALSE 
            eko2.EORG = omrrgrtemp.SPECOMR  
            eko2.EVERDATUM = STRING(DATE(MONTH(BERBEST.Bestdatum),01,YEAR(BERBEST.Bestdatum)),"999999")
            eko2.EPROJEKT = ""
            eko2.DELNR = 0 
            eko2.EKOSTNADSSLAG = kontokode2.
            eko2.EBELOPP = eko2.EBELOPP + BERBEST.PRIS * BERBEST.ANTAL * 0.1.              
         END.
         IF skarpvar = TRUE THEN DO TRANSACTION:
            GET CURRENT bq EXCLUSIVE-LOCK.
            SUBSTRING(BERBEST.BESTALLARE,60) = "w" + STRING(TODAY,"99999999").
         END.
      END.
      GET NEXT bq NO-LOCK.
   END.   
END PROCEDURE.
PROCEDURE bilkost_UI:
   DEFINE INPUT PARAMETER typvar AS CHARACTER  NO-UNDO.
   FIND FIRST omrrgrtemp WHERE 
   omrrgrtemp.PERSTYP = typvar NO-LOCK NO-ERROR.     
   /*DEBET POST*/
   FIND FIRST eko WHERE
   eko.FTG = ekoforst.FTG AND
   eko.EORG = ekoforst.EGEO AND 
   eko.EVERDATUM = ekoforst.EVERDATUM AND
   eko.EPROJEKT = ekoforst.EPROJEKT AND
   eko.DELNR = ekoforst.DELNR AND
   eko.EKOSTNADSSLAG = kontokod 
   USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
   IF NOT AVAILABLE eko THEN DO:
      CREATE eko.
   END.    
   ASSIGN         
   eko.K1 = ""
   eko.K2 = ""
   eko.K3 = ""
   eko.K4 = ekoforst.EPROJEKT
   eko.K5 = kontokod
   eko.FTG = ekoforst.FTG
   eko.EDEBKRED = TRUE 
   eko.EORG = ekoforst.EGEO 
   eko.EVERDATUM = ekoforst.EVERDATUM
   eko.EPROJEKT = ekoforst.EPROJEKT 
   eko.DELNR = ekoforst.DELNR 
   eko.EKOSTNADSSLAG = kontokod   
   eko.EBELOPP = eko.EBELOPP + ekoforst.ELONBELOPP.     
   /*KREDIT POST*/          
   FIND FIRST eko2 WHERE
   eko2.FTG = ekoforst.FTG AND
   eko2.EORG = omrrgrtemp.SPECOMR AND 
   eko2.EVERDATUM = ekoforst.EVERDATUM AND
   eko2.EPROJEKT = ekoforst.EPROJEKT AND
   eko2.DELNR = ekoforst.DELNR AND
   eko2.EKOSTNADSSLAG = kontokode  AND
   eko2.ELONTILLAGG = ekoforst.ELONTILLAGG
   USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
   IF NOT AVAILABLE eko2 THEN DO:
      CREATE eko2.
   END.    
   ASSIGN     
   eko2.K1 = omrrgrtemp.SPECOMR
   eko2.K2 = omrrgrtemp.RGR
   eko2.K3 = ""
   eko2.K4 = ekoforst.EPROJEKT
   eko2.K5 = kontokode
   eko2.FTG = ekoforst.FTG
   eko2.EDEBKRED = FALSE 
   eko2.EORG = omrrgrtemp.SPECOMR  
   eko2.EVERDATUM = ekoforst.EVERDATUM
   eko2.EPROJEKT = ekoforst.EPROJEKT 
   eko2.DELNR = ekoforst.DELNR 
   eko2.EKOSTNADSSLAG = kontokode
   eko2.ELONTILLAGG = ekoforst.ELONTILLAGG.
   eko2.EBELOPP = eko2.EBELOPP + ekoforst.ELONBELOPP.        
END PROCEDURE.
PROCEDURE kontofordel_UI:
   OPEN QUERY qe FOR EACH eko.
   GET FIRST qe.
   DO WHILE AVAILABLE(eko):  
      IF eko.EPROJEKT = "" THEN DO:         
         FIND FIRST slutut WHERE 
         slutut.FTG = eko.FTG AND
         slutut.DEBKRED = eko.EDEBKRED AND 
         slutut.K1 = eko.K1 AND 
         slutut.K2 = eko.K2 AND
         slutut.K3 = eko.K3 AND
         slutut.K4 = eko.K4 AND
         slutut.K5 = eko.K5
         NO-ERROR.  
         IF NOT AVAILABLE slutut THEN DO:
            CREATE slutut.
         END.       
         ASSIGN  
         slutut.FTG = eko.FTG
         slutut.DEBKRED = eko.EDEBKRED           
         slutut.K1 = eko.K1  
         slutut.K2 = eko.K2 
         slutut.K3 = eko.K3 
         slutut.K4 = eko.K4
         slutut.K5 = eko.K5
         slutut.BELOPP = slutut.BELOPP + eko.EBELOPP.
      END.
      ELSE DO:               
         OPEN QUERY qa FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = eko.EPROJEKT AND 
         AONRKONTKOD.DELNR = eko.DELNR NO-LOCK.
         GET FIRST qa NO-LOCK.
         DO WHILE AVAILABLE(AONRKONTKOD):
            IF AONRKONTKOD.K5 = "Ja" THEN musz = musz.
            ELSE DO:
               ASSIGN
               eko.K4 = ""
               eko.EPROJEKT = "".
            END.
            /*DEBET*/
            IF eko.EDEBKRED = TRUE THEN DO:              
               FIND FIRST slutut WHERE 
               slutut.FTG = eko.FTG AND
               slutut.DEBKRED = eko.EDEBKRED AND                
               slutut.K1 = AONRKONTKOD.K1 AND
               slutut.K2 = AONRKONTKOD.K2 AND
               slutut.K3 = AONRKONTKOD.K3 AND
               slutut.K4 = eko.K4 AND               
               slutut.K5 = eko.K5 AND
               slutut.BIL = eko.ELONTILLAGG
               NO-ERROR.  
               IF NOT AVAILABLE slutut THEN DO:
                  CREATE slutut.
               END.       
               ASSIGN  
               slutut.FTG = eko.FTG
               slutut.DEBKRED = eko.EDEBKRED           
               slutut.K1 = AONRKONTKOD.K1 
               slutut.K2 = AONRKONTKOD.K2 
               slutut.K3 = AONRKONTKOD.K3 
               slutut.K4 = eko.K4                
               slutut.K5 = eko.K5 
               slutut.BIL = eko.ELONTILLAGG.
            END.
            /*kredit*/
            IF eko.EDEBKRED = FALSE THEN DO: 
               FIND FIRST slutut WHERE 
               slutut.FTG = eko.FTG AND
               slutut.DEBKRED = eko.EDEBKRED AND                
               slutut.K1 = eko.K1 AND
               slutut.K2 = eko.K2 AND
               slutut.K3 = "" AND
               slutut.K4 = "" AND               
               slutut.K5 = eko.K5 AND
               slutut.BIL = eko.ELONTILLAGG
               NO-ERROR.  
               IF NOT AVAILABLE slutut THEN DO:
                  CREATE slutut.
               END.       
               ASSIGN  
               slutut.FTG = eko.FTG
               slutut.DEBKRED = eko.EDEBKRED           
               slutut.K1 = eko.K1
               slutut.K2 = eko.K2 
               slutut.K3 = "" 
               slutut.K4 = ""
               slutut.K5 = eko.K5
               slutut.BIL = eko.ELONTILLAGG.               
            END.
            ASSIGN
            slutut.ANTAL = slutut.ANTAL + (eko.EANTAL * AONRKONTKOD.SATS%) / 100  
            slutut.BELOPP = slutut.BELOPP + (eko.EBELOPP * AONRKONTKOD.SATS%) / 100.                           
            GET NEXT qa NO-LOCK.
         END.
      END.
      GET NEXT qe.
   END.
END PROCEDURE.
PROCEDURE kreditposter_UI:
   OPEN QUERY sq FOR EACH slutut WHERE slutut.DEBKRED = FALSE NO-LOCK.
   GET FIRST sq.
   DO WHILE AVAILABLE(slutut):
      FIND FIRST slututK WHERE 
      slututK.FTG = slutut.FTG AND
      slututK.DEBKRED = slutut.DEBKRED AND 
      slututK.K1 = slutut.K1 AND
      slututK.K2 = slutut.K2 AND
      slututK.K3 = slutut.K3 AND
      slututK.K4 = slutut.K4 AND               
      slututK.K5 = slutut.K5 AND
      slututK.BIL = slutut.BIL      
      NO-ERROR.  
      IF NOT AVAILABLE slututK THEN DO:
         CREATE slututK.
      END.         
      ASSIGN  
      slututK.FTG = slutut.FTG
      slututK.DEBKRED = slutut.DEBKRED           
      slututK.K1 = slutut.K1
      slututK.K2 = slutut.K2
      slututK.K3 = slutut.K3
      slututK.K4 = slutut.K4
      slututK.K5 = slutut.K5
      slututK.BIL = slutut.BIL.
      IF slutut.BIL = "" THEN DO:
         ASSIGN
         slututK.ANTAL = slututK.ANTAL + ROUND(slutut.ANTAL,2) 
         slututK.BELOPP = slututK.BELOPP + ROUND(slutut.BELOPP,2).
      END.
      ELSE DO:     
         ASSIGN
         slututK.ANTAL = slututK.ANTAL + slutut.ANTAL 
         slututK.BELOPP = slututK.BELOPP + slutut.BELOPP.
      END.
      DELETE slutut.
      GET NEXT sq.
   END.
   /*Nödlösning för öresavrundning*/
   FOR EACH slututk:
      kredvar = kredvar + ROUND(slututK.BELOPP,2).
   END.
   FOR EACH slutut:
      debvar = debvar + ROUND(slutut.BELOPP,2).
   END.
   IF (kredvar - debvar) NE 0 THEN DO:
      diffvar = debvar - kredvar. 
      FIND LAST slututk WHERE slututk.BIL NE "" NO-ERROR.
      IF AVAILABLE slututk THEN DO:
         slututK.BELOPP = slututK.BELOPP + diffvar.
      END.
   END.
   FOR EACH slututK:
      CREATE slutut.
      BUFFER-COPY slututK TO slutut.
      DELETE slututK.
   END.   
   FOR EACH slutut:
      IF slutut.DEBKRED = FALSE THEN slutut.BELOPP = slutut.BELOPP.
      slutut.K2POS8 = slutut.K2.
   END.
END PROCEDURE.

PROCEDURE filut_UI:    
   DEFINE INPUT PARAMETER utftg AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER filut AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER filutkopia AS CHARACTER NO-UNDO.
   
   DEFINE VARIABLE postantal AS INTEGER NO-UNDO.
   str = "".
   ASSIGN
   SUBSTRING(str,utnrin[nrcolin[1]]) = "10" 
   SUBSTRING(str,utnrin[nrcolin[2]]) = "430"
   SUBSTRING(str,utnrin[nrcolin[3]]) = "08808"  
   SUBSTRING(str,utnrin[nrcolin[4]]) = "GUR"    
   SUBSTRING(str,utnrin[nrcolin[5]]) = STRING(TODAY,"999999")
   SUBSTRING(str,utnrin[nrcolin[6]]) = REPLACE(STRING(TIME,"HH:MM:SS"),":","")                                   
   SUBSTRING(str,utnrin[nrcolin[7]]) = "EKO"   
   SUBSTRING(str,utnrin[nrcolin[8]]) = "".
   
   OUTPUT TO VALUE(filut) APPEND.   
   PUT str SKIP.
   OUTPUT CLOSE.
   FOR EACH slutut WHERE slutut.FTG = utftg:
      ACCUMULATE slutut.BELOPP (COUNT).
   END.
   postantal = ACCUM COUNT slutut.BELOPP.
   /*Nödlösning för öresavrundning*/
   ASSIGN
   diffvar = 0
   kredvar = 0
   debvar = 0
   diffvar2 = 0
   kredvar2 = 0
   debvar2 = 0.
   FOR EACH slutut WHERE slutut.FTG = utftg AND slutut.DEBKRED = TRUE:
      kredvar = kredvar + ROUND(slutut.BELOPP,2).
      debvar = debvar + slutut.BELOPP.
      kredvar2 = kredvar2 + ROUND(slutut.ANTAL,2).
      debvar2 = debvar2 + slutut.ANTAL.
   END.
   IF (kredvar - debvar) NE 0 THEN DO:
      diffvar = kredvar - debvar.          
      /*summmallaant = summmallaant + slutut.ANTAL.*/
   END. 
   IF (kredvar2 - debvar2) NE 0 THEN DO:
      diffvar2 = kredvar2 - debvar2.          
      /*summmallaant = summmallaant + slutut.ANTAL.*/
   END.
   OUTPUT TO VALUE(filut) APPEND.
   FOR EACH slutut WHERE slutut.FTG = utftg AND slutut.DEBKRED = TRUE:
      RUN ut_UI.    
   END.   
   FOR EACH slutut WHERE slutut.FTG = utftg AND slutut.DEBKRED = FALSE:
      str = "".
      RUN ut_UI.   
   END.
   OUTPUT CLOSE.
   summmallab = summmallab + diffvar.
   summmallaant = summmallaant + diffvar2.
   RUN cobolantal_UI (INPUT 18,INPUT summmallab,OUTPUT summmallabch).   
   RUN cobolantal_UI (INPUT 18,INPUT summmallaant,OUTPUT summmallaantch).
   str = "".
   ASSIGN
   SUBSTRING(str,utnrav[nrcolav[1]]) = "50" 
   SUBSTRING(str,utnrav[nrcolav[2]]) = STRING(postantal,"9999999999")
   SUBSTRING(str,utnrav[nrcolav[3]]) = STRING(0,"9999999999")
   SUBSTRING(str,utnrav[nrcolav[4]]) = STRING(0,"9999999999")
   SUBSTRING(str,utnrav[nrcolav[5]]) = STRING(0,"9999999999")
   SUBSTRING(str,utnrav[nrcolav[6]]) = summmallabch                                   
   SUBSTRING(str,utnrav[nrcolav[7]]) = summmallaantch
   SUBSTRING(str,utnrav[nrcolav[8]]) = STRING(0,"999999999999999999")
   SUBSTRING(str,utnrav[nrcolav[9]]) = STRING(0,"9999999999")
   SUBSTRING(str,utnrav[nrcolav[10]]) = STRING(0,"9999999999")
   SUBSTRING(str,utnrav[nrcolav[11]]) = STRING(0,"9999999999")
   SUBSTRING(str,utnrav[nrcolav[12]]) = STRING(0,"9999999999")
   SUBSTRING(str,utnrav[nrcolav[13]]) = STRING(0,"999999999999999999")
   SUBSTRING(str,utnrav[nrcolav[14]]) = STRING(0,"999999999999999999")
   SUBSTRING(str,utnrav[nrcolav[15]]) = "".


   OUTPUT TO VALUE(filut) APPEND.   
   PUT str SKIP.
   OUTPUT CLOSE.

   OS-COPY VALUE(filut) VALUE(filutkopia).
   
END PROCEDURE.



PROCEDURE ut_UI:
   DEFINE VARIABLE beloppchar AS CHARACTER NO-UNDO.
   
   IF slutut.DEBKRED = TRUE THEN DO:
      ASSIGN
      summmallab = summmallab + slutut.BELOPP.
      summmallaant = summmallaant + slutut.ANTAL.
   END.   
   
   str = "".
   ASSIGN
   SUBSTRING(str,utnr[nrcol[1]]) = "20" 
   SUBSTRING(str,utnr[nrcol[2]]) = "08808"
   SUBSTRING(str,utnr[nrcol[3]]) = slutut.FTG
   SUBSTRING(str,utnr[nrcol[4]]) = "000"    
   SUBSTRING(str,utnr[nrcol[5]]) = "000"
   SUBSTRING(str,utnr[nrcol[6]]) = "000000000"
   SUBSTRING(str,utnr[nrcol[7]]) = SUBSTRING(STRING(YEAR(vkdatum),"9999"),3,2)   
   SUBSTRING(str,utnr[nrcol[8]]) = STRING(MONTH(vkdatum),"99")
   SUBSTRING(str,utnr[nrcol[9]]) = STRING(0,"99")
   SUBSTRING(str,utnr[nrcol[10]]) = STRING(DAY(vkdatum),"99").
   IF slutut.K1 NE "" THEN DO:
      SUBSTRING(str,utnr[nrcol[11]]) =  slutut.K1 /*+ SUBSTRING(nolldummy,1,10 - LENGTH(slutut.K1))*/.
   END.
   ELSE SUBSTRING(str,utnr[nrcol[11]]) = "".
   IF slutut.K2 NE "" THEN DO:
      SUBSTRING(str,utnr[nrcol[12]]) =  slutut.K2 /*+ SUBSTRING(nolldummy,1,10 - LENGTH(slutut.K2))*/.
   END.
   ELSE SUBSTRING(str,utnr[nrcol[12]]) = "".
   IF slutut.K3 NE "" THEN DO:
      SUBSTRING(str,utnr[nrcol[13]]) = slutut.K3 /*+ SUBSTRING(nolldummy,1,10 - LENGTH(slutut.KOSTNADSSLAG))*/.
   END.
   ELSE SUBSTRING(str,utnr[nrcol[13]]) = "".
   IF slutut.K4 NE "" THEN DO:
      SUBSTRING(str,utnr[nrcol[14]]) = slutut.K4 /*+ SUBSTRING(nolldummy,1,10 - LENGTH(slutut.PROJEKT))*/.
   END.
   ELSE SUBSTRING(str,utnr[nrcol[14]]) = "".
   IF slutut.K5 NE "" THEN DO:
      SUBSTRING(str,utnr[nrcol[15]]) = slutut.K5 /*+ SUBSTRING(nolldummy,1,10 - LENGTH(slutut.PROJEKT))*/.
   END.
   ELSE SUBSTRING(str,utnr[nrcol[15]]) = "".
   ASSIGN
   SUBSTRING(str,utnr[nrcol[16]]) = ""
   SUBSTRING(str,utnr[nrcol[17]]) = ""
   SUBSTRING(str,utnr[nrcol[18]]) = ""
   SUBSTRING(str,utnr[nrcol[19]]) = ""
   SUBSTRING(str,utnr[nrcol[20]]) = "".
   RUN cobolantal_UI (INPUT 15,INPUT slutut.BELOPP,OUTPUT beloppchar).   
   SUBSTRING(str,utnr[nrcol[21]]) = beloppchar.
   RUN cobolantal_UI (INPUT 15,INPUT slutut.ANTAL,OUTPUT beloppchar).   
   SUBSTRING(str,utnr[nrcol[22]]) = beloppchar.
   ASSIGN
   SUBSTRING(str,utnr[nrcol[23]]) = STRING(0,"99999999999")
   SUBSTRING(str,utnr[nrcol[24]]) = ""
   SUBSTRING(str,utnr[nrcol[25]]) = ""
   SUBSTRING(str,utnr[nrcol[26]]) = "".   
   PUT UNFORMATTED str AT 1 SKIP.       
   DELETE slutut. 
END PROCEDURE.

PROCEDURE cobolantal_UI:
   DEFINE INPUT PARAMETER nollantal AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER intal AS DECIMAL NO-UNDO.
   DEFINE OUTPUT PARAMETER uttalch AS CHARACTER NO-UNDO.
   DEFINE VARIABLE intalhel AS INTEGER NO-UNDO.
   DEFINE VARIABLE slutch AS CHARACTER NO-UNDO.
   intalhel = intal * 100.
   IF intalhel >= 0 THEN DO:
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "0" THEN slutch = "0".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "1" THEN slutch = "1".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "2" THEN slutch = "2".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "3" THEN slutch = "3".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "4" THEN slutch = "4".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "5" THEN slutch = "5".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "6" THEN slutch = "6".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "7" THEN slutch = "7".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "8" THEN slutch = "8".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "9" THEN slutch = "9".
   END.                                                             
   IF intalhel < 0 THEN DO:
      intalhel = intalhel * -1.
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "0" THEN slutch = "p".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "1" THEN slutch = "q".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "2" THEN slutch = "r".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "3" THEN slutch = "s".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "4" THEN slutch = "t".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "5" THEN slutch = "u".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "6" THEN slutch = "v".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "7" THEN slutch = "w".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "8" THEN slutch = "x".
      IF SUBSTRING(STRING(intalhel),LENGTH(STRING(intalhel)),1) = "9" THEN slutch = "y".
   END.                            
   uttalch = STRING(intalhel).
   uttalch = SUBSTRING(uttalch,1,LENGTH(uttalch) - 1) + slutch.
   IF LENGTH(uttalch) = nollantal THEN RETURN.
   uttalch = SUBSTRING(nolldummy,1,nollantal - LENGTH(uttalch)) + uttalch.
END PROCEDURE.
/*UTFILER*/
PROCEDURE ladda_UI:
ASSIGN
   nrcolin[1] = 1
   nrcolin[2] = 2
   nrcolin[3] = 3
   nrcolin[4] = 4
   nrcolin[5] = 5
   nrcolin[6] = 6
   nrcolin[7] = 7
   nrcolin[8] = 8
   nrcol[1] = 1
   nrcol[2] = 2
   nrcol[3] = 3
   nrcol[4] = 4
   nrcol[5] = 5
   nrcol[6] = 6
   nrcol[7] = 7
   nrcol[8] = 8       
   nrcol[9] = 9
   nrcol[10] = 10
   nrcol[11] = 11
   nrcol[12] = 12
   nrcol[13] = 13
   nrcol[14] = 14
   nrcol[15] = 15
   nrcol[16] = 16
   nrcol[17] = 17
   nrcol[18] = 18
   nrcol[19] = 19
   nrcol[20] = 20
   nrcol[21] = 21
   nrcol[22] = 22
   nrcol[23] = 23
   nrcol[24] = 24
   nrcol[25] = 25
   nrcol[26] = 26     
   nrcolav[1] = 1
   nrcolav[2] = 2
   nrcolav[3] = 3
   nrcolav[4] = 4
   nrcolav[5] = 5
   nrcolav[6] = 6
   nrcolav[7] = 7
   nrcolav[8] = 8       
   nrcolav[9] = 9
   nrcolav[10] = 10
   nrcolav[11] = 11
   nrcolav[12] = 12
   nrcolav[13] = 13
   nrcolav[14] = 14
   nrcolav[15] = 15.    
   ASSIGN
   breddantalin = 8
   breddantal = 26   /*antal kolumner*/
   breddantalav = 15
   breddin[1] =  2
   breddin[2] =  3
   breddin[3] =  5
   breddin[4] =  3
   breddin[5] =  6
   breddin[6] =  6
   breddin[7] =  3
   breddin[8] =  222

   bredd[1] =  2
   bredd[2] =  5
   bredd[3] =  10
   bredd[4] =  3
   bredd[5] =  3
   bredd[6] =  9
   bredd[7] =  2
   bredd[8] =  2
   bredd[9] =  2
   bredd[10] = 2
   bredd[11] = 10
   bredd[12] = 10
   bredd[13] = 10
   bredd[14] = 10
   bredd[15] = 10
   bredd[16] = 10
   bredd[17] = 10
   bredd[18] = 10
   bredd[19] = 10
   bredd[20] = 10
   bredd[21] = 15
   bredd[22] = 15
   bredd[23] = 11
   bredd[24] = 12
   bredd[25] = 36
   bredd[26] = 21

   breddav[1] =  2
   breddav[2] =  10
   breddav[3] =  10
   breddav[4] =  10
   breddav[5] =  10
   breddav[6] =  18
   breddav[7] =  18
   breddav[8] =  18
   breddav[9] =  10
   breddav[10] = 10
   breddav[11] = 10
   breddav[12] = 10
   breddav[13] = 18
   breddav[14] = 18
   breddav[15] = 78.
   
   ASSIGN
   i = 2.     
   utnrin[nrcolin[1]] = 1.
   DO WHILE i <= breddantalin:             
      utnrin[i] = utnrin[i - 1] + breddin[i - 1].            
      i = i + 1.
   END.
   ASSIGN
   i = 2.     
   utnr[nrcol[1]] = 1.
   DO WHILE i <= breddantal:             
      utnr[i] = utnr[i - 1] + bredd[i - 1].            
      i = i + 1.
   END.
   ASSIGN
   i = 2.     
   utnrav[nrcolav[1]] = 1.
   DO WHILE i <= breddantalav:             
      utnrav[i] = utnrav[i - 1] + breddav[i - 1].            
      i = i + 1.
   END.
   
END PROCEDURE.
