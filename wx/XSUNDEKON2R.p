/*G:\PRO9S\WX\XSUNDEKON2R.P*/ 
DEFINE SHARED TEMP-TABLE foretemp NO-UNDO
   FIELD FTG AS CHARACTER
   INDEX FTG IS PRIMARY FTG.
DEFINE TEMP-TABLE ekoforst
   FIELD FTG AS CHARACTER
   FIELD ENY LIKE EKRAPPRESULT.ENY 
   FIELD EPERSONALKOD LIKE EKRAPPRESULT.EPERSONALKOD 
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD EORG LIKE EKRAPPRESULT.EORG    
   FIELD EGEO LIKE EKRAPPRESULT.EGEO  
   FIELD FELDEBKRED AS LOGICAL
   FIELD EVERDATUM LIKE EKRAPPRESULT.EVERDATUM
   FIELD EOVERJA LIKE EKRAPPRESULT.EOVERJA
   FIELD ERESULTENH LIKE EKRAPPRESULT.ERESULTENH 
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL LIKE EKRAPPRESULT.EANTAL 
   FIELD ETIMMAR LIKE EKRAPPRESULT.EANTAL                
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP    
   INDEX PERSORG IS PRIMARY EPERSONALKOD EORG EGEO EPROJEKT DELNR ASCENDING.
DEFINE TEMP-TABLE eko
   FIELD PERSONALKOD AS CHARACTER
   FIELD FTG AS CHARACTER
   FIELD EDEBKRED LIKE EKRAPPRESULT.EDEBKRED
   FIELD ENY LIKE EKRAPPRESULT.ENY       
   FIELD EVERDATUM LIKE EKRAPPRESULT.EVERDATUM  
   FIELD EPROJEKT LIKE EKRAPPRESULT.EPROJEKT 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD EORG LIKE EKRAPPRESULT.EORG 
/*   FIELD EGEO LIKE EKRAPPRESULT.EGEO       */
   FIELD EKOSTNADSSLAG LIKE EKRAPPRESULT.EKOSTNADSSLAG    
   FIELD EBELOPP LIKE  EKRAPPRESULT.EBELOPP 
   FIELD EANTAL  LIKE EKRAPPRESULT.EANTAL 	       
   FIELD ELONTILLAGG LIKE EKRAPPRESULT.ELONTILLAGG 
   FIELD ELONTILLANTAL LIKE EKRAPPRESULT.ELONTILLANTAL    
   FIELD ELONBELOPP LIKE EKRAPPRESULT.ELONBELOPP         
   FIELD FELDEBKRED AS LOGICAL
   INDEX ORG IS PRIMARY EVERDATUM EORG EPROJEKT EKOSTNADSSLAG ASCENDING.   
DEFINE TEMP-TABLE slutut
   FIELD PERSONALKOD AS CHARACTER
   FIELD NAMN AS CHARACTER
   FIELD FTG AS CHARACTER
   FIELD DEBKRED AS LOGICAL 
   FIELD OMRADE AS CHARACTER
   FIELD PROJEKT AS CHARACTER 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD VERDATUM AS CHARACTER
   FIELD KOSTNADSSLAG AS CHARACTER
   FIELD ANTAL AS DECIMAL 
   FIELD BELOPP AS DECIMAL       
   FIELD K1 AS CHARACTER 
   FIELD K2 AS CHARACTER
   FIELD K4 AS CHARACTER
   FIELD BIL AS CHARACTER
   FIELD K2POS8 AS CHARACTER
   FIELD FELDEBKRED AS LOGICAL
   INDEX ORG IS PRIMARY PERSONALKOD FTG DEBKRED OMRADE PROJEKT KOSTNADSSLAG K1 K2.
DEFINE TEMP-TABLE slututK
   FIELD FTG AS CHARACTER
   FIELD DEBKRED AS LOGICAL 
   FIELD OMRADE AS CHARACTER
   FIELD PROJEKT AS CHARACTER 
   FIELD DELNR LIKE AONRTAB.DELNR 
   FIELD VERDATUM AS CHARACTER
   FIELD KOSTNADSSLAG AS CHARACTER
   FIELD ANTAL AS DECIMAL 
   FIELD BELOPP AS DECIMAL       
   FIELD K1 AS CHARACTER 
   FIELD K2 AS CHARACTER
   FIELD BIL AS CHARACTER
   FIELD K2POS8 AS CHARACTER
   FIELD FELDEBKRED AS LOGICAL
   INDEX ORG IS PRIMARY DEBKRED OMRADE PROJEKT KOSTNADSSLAG K1 K2.
DEFINE TEMP-TABLE omrkosttemp
   FIELD OMRADE AS CHARACTER
   FIELD KOSTNADSLAGDEB AS CHARACTER
   FIELD KOSTNADSLAGKRED AS CHARACTER
   FIELD PERSTYP AS CHARACTER
   FIELD NRSERIE AS CHARACTER
   INDEX PERSTYP IS PRIMARY PERSTYP OMRADE.
DEFINE BUFFER eko2 FOR eko.
DEFINE BUFFER persbuff FOR PERSONALTAB.
{LESAMMAN.I}
DEFINE INPUT PARAMETER vkdatum AS DATE NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR ekoforst. 
RUN sammut_UI (INPUT 1).
DEFINE VARIABLE str AS CHARACTER FORMAT "X(140)" NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER NO-UNDO. 
DEFINE VARIABLE kommando2 AS CHARACTER NO-UNDO. 
DEFINE VARIABLE kontokod LIKE EKRAPPRESULT.EKOSTNADSSLAG NO-UNDO. 
DEFINE VARIABLE kontokode LIKE EKRAPPRESULT.EKOSTNADSSLAG NO-UNDO. 
DEFINE VARIABLE pkoder LIKE PERSONALTAB.PERSONALKOD NO-UNDO.  
DEFINE VARIABLE persrec AS RECID NO-UNDO. 
DEF VAR debvar AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
DEF VAR kredvar AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
DEF VAR diffvar AS DECIMAL FORMAT "->>>>>>>>>.99" NO-UNDO.
{SUKONTON2.I}
PROCEDURE omrk_UI:
   DEFINE INPUT PARAMETER omrvar AS CHARACTER.
   DEFINE INPUT PARAMETER debvar AS CHARACTER.
   DEFINE INPUT PARAMETER krevar AS CHARACTER.
   DEFINE INPUT PARAMETER ptypvar AS CHARACTER.
   DEFINE INPUT PARAMETER nserie AS CHARACTER.
   CREATE omrkosttemp.
   ASSIGN
   omrkosttemp.OMRADE          = omrvar 
   omrkosttemp.KOSTNADSLAGDEB  = debvar 
   omrkosttemp.KOSTNADSLAGKRED = krevar 
   omrkosttemp.PERSTYP         = ptypvar
   omrkosttemp.NRSERIE         = nserie.
END PROCEDURE.
pkoder = "". 
/*TIDREGITAB = PER PERSON OCH DAG OCH AONR
EKOFORST = PER PERSON OCH AONR
EKO = PER OMRÅDE OCH KONTO.
SLUTUT DEBET PER OMRÅDE AONR OCH KONTO
SLUTUT KREDIT PER KONTO.
*/
OPEN QUERY qeko FOR EACH ekoforst WHERE ekoforst.ENY = FALSE USE-INDEX PERSORG NO-LOCK.
GET FIRST qeko.
DO WHILE AVAILABLE(ekoforst):
   IF pkoder NE ekoforst.EPERSONALKOD THEN DO:      
      pkoder = ekoforst.EPERSONALKOD.      
      FIND FIRST PERSONALTAB WHERE PERSONALTAB.PERSONALKOD = pkoder 
      USE-INDEX PERSONALKOD NO-LOCK NO-ERROR.        
      persrec = RECID(PERSONALTAB).     
   END.   
   
   FIND FIRST omrkosttemp WHERE omrkosttemp.OMRADE = ekoforst.EORG AND
   omrkosttemp.PERSTYP = "PERS" AND omrkosttemp.NRSERIE = SUBSTRING(ekoforst.EPROJEKT,1,2) NO-LOCK NO-ERROR.
   IF NOT AVAILABLE omrkosttemp  THEN DO:   
      FIND FIRST omrkosttemp WHERE omrkosttemp.OMRADE = ekoforst.EORG AND
      omrkosttemp.PERSTYP = "PERS" AND omrkosttemp.NRSERIE = SUBSTRING(ekoforst.EPROJEKT,1,1) NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrkosttemp  THEN DO:   
         FIND FIRST omrkosttemp WHERE omrkosttemp.OMRADE = ekoforst.EORG AND
         omrkosttemp.PERSTYP = "PERS" AND omrkosttemp.NRSERIE = "" NO-LOCK NO-ERROR.
      END.
   END.
   IF NOT AVAILABLE omrkosttemp THEN DO:
      /*CCCC????*/
      RUN sammut_UI (INPUT 3).
      OUTPUT TO VALUE(samvarglb) APPEND.
      PUT UNFORMATTED "FEL KONTAKTA ELPOOL  PERS "   ekoforst.EPERSONALKOD " " ekoforst.EORG SKIP.
      OUTPUT CLOSE.
   END.
   ELSE DO:   
      ASSIGN
      kontokod =  omrkosttemp.KOSTNADSLAGDEB 
      kontokode = omrkosttemp.KOSTNADSLAGKRED.       
   END.
   /*TIMMAR OCH PENNGAR*/
   IF ekoforst.EBELOPP = 0 THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:      
      RUN kost_UI (INPUT TRUE).      
   END.      
   IF ekoforst.ELONTILLAGG = "" THEN DO:
      persrec = persrec.
   END.                                   
   ELSE DO:
      /*ÖVERTIDTILLÄGG*/ 
      IF ekoforst.ERESULTENH = "OVE" THEN DO:            
         RUN kost_UI (INPUT FALSE). 
      END.   
      /*BEREDSKAP*/             
      IF ekoforst.ERESULTENH = "BER" THEN DO:     
         /*körs ej*/
         FIND FIRST omrkosttemp WHERE /*"omrkosttemp.OMRADE = ekoforst.EORG AND*/
         omrkosttemp.PERSTYP = "BEREDSKAP" 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrkosttemp THEN DO:
            /*CCCC????*/
            RUN sammut_UI (INPUT 3).
            OUTPUT TO VALUE(samvarglb) APPEND.
            PUT UNFORMATTED "FEL KONTAKTA ELPOOL  BEREDSKAP"  ekoforst.EPERSONALKOD  ekoforst.EORG SKIP.            
            OUTPUT CLOSE.
         END.
         ELSE DO:   
            ASSIGN
            kontokod =  omrkosttemp.KOSTNADSLAGDEB 
            kontokode = omrkosttemp.KOSTNADSLAGKRED.                
            RUN kost_UI (INPUT FALSE).       
         END.
      END.            
      /*BILAR*/
      IF ekoforst.ERESULTENH = "BIL" THEN DO:             
         FIND FIRST persbuff WHERE persbuff.PERSONALKOD = ekoforst.ELONTILLAGG
         NO-LOCK NO-ERROR.

         FIND FIRST omrkosttemp WHERE omrkosttemp.OMRADE = persbuff.OMRADE AND
         omrkosttemp.PERSTYP = persbuff.BEFATTNING AND omrkosttemp.NRSERIE = SUBSTRING(ekoforst.EPROJEKT,1,2) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE omrkosttemp  THEN DO:   
            FIND FIRST omrkosttemp WHERE omrkosttemp.OMRADE = persbuff.OMRADE AND
            omrkosttemp.PERSTYP = persbuff.BEFATTNING AND omrkosttemp.NRSERIE = "" NO-LOCK NO-ERROR.
         END.
         IF AVAILABLE omrkosttemp THEN DO:

         /*FIND FIRST omrkosttemp WHERE omrkosttemp.OMRADE = persbuff.OMRADE AND
         omrkosttemp.PERSTYP = persbuff.BEFATTNING 
         NO-LOCK NO-ERROR.
         IF AVAILABLE omrkosttemp THEN DO:*/
            ASSIGN
            kontokod =  omrkosttemp.KOSTNADSLAGDEB 
            kontokode = omrkosttemp.KOSTNADSLAGKRED.         
            RUN bilkost_UI.   
         END.
         ELSE DO:
            RUN sammut_UI (INPUT 3).
            OUTPUT TO VALUE(samvarglb) APPEND.
            PUT UNFORMATTED "FEL KONTAKTA ELPOOL  BILAR"  ekoforst.EPERSONALKOD persbuff.BEFATTNING  persbuff.OMRADE ekoforst.ELONTILLAGG SKIP.
            OUTPUT CLOSE.
         END.
      END.                           
   END.
   DELETE ekoforst.
   GET NEXT qeko.   
END.
/*SLUTUT PER AONR KONTO*/
OPEN QUERY qe FOR EACH eko /*WHERE eko.EDEBKRED = TRUE*/ NO-LOCK.
GET FIRST qe.
DO WHILE AVAILABLE(eko):
   OPEN QUERY qa FOR EACH AONRKONTKOD WHERE AONRKONTKOD.AONR = eko.EPROJEKT AND 
   AONRKONTKOD.DELNR = eko.DELNR NO-LOCK.
   GET FIRST qa NO-LOCK.
   DO WHILE AVAILABLE(AONRKONTKOD):
      FIND FIRST slutut WHERE 
      slutut.PERSONALKOD = eko.PERSONALKOD AND
      slutut.FTG = eko.FTG AND
      slutut.DEBKRED = eko.EDEBKRED AND 
      slutut.OMRADE = eko.EORG AND 
      slutut.PROJEKT = eko.EPROJEKT AND
      slutut.DELNR = eko.DELNR AND
      slutut.KOSTNADSSLAG = eko.EKOSTNADSSLAG AND        
      slutut.K1 = AONRKONTKOD.K1 AND
      slutut.K2 = AONRKONTKOD.K2 AND
      slutut.K4 = AONRKONTKOD.K4 AND
      slutut.BIL = eko.ELONTILLAGG
      NO-ERROR.  
      IF NOT AVAILABLE slutut THEN DO:
         CREATE slutut.
      END.       
      ASSIGN  
      slutut.PERSONALKOD = eko.PERSONALKOD 
      slutut.FTG = eko.FTG 
      slutut.DEBKRED = eko.EDEBKRED           
      slutut.OMRADE = eko.EORG                   
      slutut.PROJEKT = eko.EPROJEKT           
      slutut.DELNR = eko.DELNR 
      slutut.KOSTNADSSLAG = eko.EKOSTNADSSLAG 
      slutut.K1 = AONRKONTKOD.K1               
      slutut.K2 = AONRKONTKOD.K2 
      slutut.K4 = AONRKONTKOD.K4 
      slutut.BIL = eko.ELONTILLAGG
      slutut.ANTAL = slutut.ANTAL + (eko.EANTAL * AONRKONTKOD.SATS%) / 100  
      slutut.BELOPP = slutut.BELOPP + (eko.EBELOPP * AONRKONTKOD.SATS%) / 100.                           
      GET NEXT qa NO-LOCK.
   END.
   GET NEXT qe.
END.  
/*SUMMERA ALLA KREDITPOSTER PÅ OMRÅDE OCH BIL*/
OPEN QUERY sq FOR EACH slutut WHERE slutut.DEBKRED = FALSE NO-LOCK.
GET FIRST sq.
DO WHILE AVAILABLE(slutut):
   /* PERSONENS OMRÅDE PÅ ALLA KREDITPOSTER*/
   IF slutut.OMRADE = "8100" THEN DO:
      FIND FIRST slututK WHERE 
      slututK.FTG = slutut.FTG AND
      slututK.DEBKRED = slutut.DEBKRED AND 
      slututK.OMRADE = slutut.OMRADE AND 
      slututK.PROJEKT = "81401" AND
      slututK.DELNR = 0 AND 
      slututK.BIL = slutut.BIL AND
      slututK.KOSTNADSSLAG = slutut.KOSTNADSSLAG 
      NO-ERROR.  
   END.
   ELSE DO:
      FIND FIRST slututK WHERE 
      slututK.FTG = slutut.FTG AND
      slututK.DEBKRED = slutut.DEBKRED AND 
      slututK.OMRADE = slutut.OMRADE AND 
      /*slututK.PROJEKT = slutut.PROJEKT AND
      slututK.DELNR = slutut.DELNR AND  */
      slututK.BIL = slutut.BIL AND
      slututK.KOSTNADSSLAG = slutut.KOSTNADSSLAG 
      NO-ERROR.  
   END.
   IF NOT AVAILABLE slututK THEN DO:
      CREATE slututK.
   END.       
   IF slutut.BIL = "" THEN DO:
      ASSIGN  
      slututK.FTG = slutut.FTG
      slututK.DEBKRED = slutut.DEBKRED           
      slututK.OMRADE = slutut.OMRADE                   
      /*slututK.PROJEKT = slutut.PROJEKT
      slututK.DELNR = slutut.DELNR      */
      slututK.KOSTNADSSLAG = slutut.KOSTNADSSLAG 
      slututK.BIL = slutut.BIL  
      slututK.K2 = slutut.BIL  
      slututK.ANTAL = slututK.ANTAL + ROUND(slutut.ANTAL,2) 
      slututK.BELOPP = slututK.BELOPP + ROUND(slutut.BELOPP,2).
   END.
   ELSE DO:   
      ASSIGN  
      slututK.FTG = slutut.FTG
      slututK.DEBKRED = slutut.DEBKRED           
      slututK.OMRADE = slutut.OMRADE                   
      /*slututK.PROJEKT = slutut.PROJEKT
      slututK.DELNR = slutut.DELNR      */
      slututK.KOSTNADSSLAG = slutut.KOSTNADSSLAG 
      slututK.BIL = slutut.BIL  
      slututK.K2 = slutut.BIL  
      slututK.ANTAL = slututK.ANTAL + slutut.ANTAL 
      slututK.BELOPP = slututK.BELOPP + slutut.BELOPP.
   END.
   IF slutut.OMRADE = "8100" THEN DO:
      ASSIGN
      slututK.PROJEKT = "81401"
      slututK.DELNR = 0.      
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
   ELSE DO:
      FIND LAST slututk NO-ERROR.
      slututK.BELOPP = slututK.BELOPP + diffvar.
   END.
END.
FOR EACH slututK:
   CREATE slutut.
   BUFFER-COPY slututK TO slutut.
   DELETE slututK.
END.

FOR EACH slutut:
    IF slutut.DEBKRED = FALSE THEN slutut.BELOPP = slutut.BELOPP * -1.
    slutut.K2POS8 = slutut.K2.
END.
/* Tidfel i egen fil som start*/
FOR EACH foretemp:
   FOR EACH PERSONALTAB NO-LOCK,
   EACH slutut WHERE slutut.PERSONALKOD = PERSONALTAB.PERSONALKOD:
       slutut.NAMN = PERSONALTAB.FORNAMN + " " + PERSONALTAB.EFTERNAMN.      
   END.
   IF foretemp.FTG = "ELNÄT" THEN DO:
      /*kommando2 = "C:\Pro10\WRK\ekoD" + STRING(TODAY,"99999999"). 
      kommando = "C:\Pro10\ekoD" + STRING(TODAY,"99999999"). */
      kommando2 = "D:\DELAD\SERVER\PRO10S\BACKEXPORT\ekoD" + STRING(TODAY,"99999999"). 
      kommando = "D:\DELAD\SERVER\PRO10S\EXPORT\EKONOMI\ekoD" + STRING(TODAY,"99999999"). 
   END.
   ELSE DO:
      /*kommando2 = "C:\Pro10\WRK\" + "ekoD" + foretemp.FTG + STRING(TODAY,"99999999"). 
      kommando = "C:\Pro10\" + "ekoD" + foretemp.FTG + STRING(TODAY,"99999999").*/
      kommando2 = "D:\DELAD\SERVER\PRO10S\BACKEXPORT\" + "ekoD" + foretemp.FTG + STRING(TODAY,"99999999"). 
      kommando = "D:\DELAD\SERVER\PRO10S\EXPORT\EKONOMI\" + "ekoD" + foretemp.FTG + STRING(TODAY,"99999999").
   END.
   OUTPUT TO VALUE(kommando) APPEND.
   PUT "Datum " vkdatum SKIP.
   FOR EACH slutut WHERE slutut.FTG = foretemp.FTG AND slutut.DEBKRED = TRUE:
      RUN ut_UI.   
   END.
   OUTPUT CLOSE.
   OUTPUT TO VALUE(kommando) APPEND.
   FOR EACH slutut WHERE slutut.FTG = foretemp.FTG AND slutut.DEBKRED = FALSE:
      RUN ut_UI.   
   END.
   OUTPUT CLOSE.
   OS-COPY VALUE(kommando) VALUE(kommando2).
END.
RUN sammut_UI (INPUT 2).
PROCEDURE ut_UI:
   IF SUBSTRING(slutut.PROJEKT,1,1) = "S" THEN DO:
      IF LENGTH(slutut.PROJEKT) > 1 THEN
      slutut.PROJEKT = SUBSTRING(slutut.PROJEKT,2,LENGTH(slutut.PROJEKT) - 1).
   END.
   IF LENGTH(slutut.PROJEKT) = 1 THEN slutut.PROJEKT = slutut.PROJEKT + "0000".
   ELSE IF LENGTH(slutut.PROJEKT) = 2 THEN slutut.PROJEKT = slutut.PROJEKT + "000".
   ELSE IF LENGTH(slutut.PROJEKT) = 3 THEN slutut.PROJEKT = slutut.PROJEKT + "00".
   ELSE IF LENGTH(slutut.PROJEKT) = 4 THEN slutut.PROJEKT = slutut.PROJEKT + "0".                                                       
   IF LENGTH(slutut.K2) = 1      THEN slutut.K2 = slutut.K2 + "0000000".
   ELSE IF LENGTH(slutut.K2) = 2 THEN slutut.K2 = slutut.K2 + "000000".
   ELSE IF LENGTH(slutut.K2) = 3 THEN slutut.K2 = slutut.K2 + "00000".
   ELSE IF LENGTH(slutut.K2) = 4 THEN slutut.K2 = slutut.K2 + "0000".
   ELSE IF LENGTH(slutut.K2) = 5 THEN slutut.K2 = slutut.K2 + "000".
   ELSE IF LENGTH(slutut.K2) = 6 THEN slutut.K2 = slutut.K2 + "00".
   ELSE IF LENGTH(slutut.K2) = 7 THEN slutut.K2 = slutut.K2 + "0".   
   str = "".
   /*
   a   a d  d o        o 1  1 2      2 k             k b             b
   12345678901234567890123456789012345678901234567890123456789012345678901234567890                   
   */
   IF slutut.DEBKRED = TRUE THEN DO:
      ASSIGN     
      SUBSTRING(str,1,5) = STRING(slutut.PROJEKT,"99999") 
      SUBSTRING(str,7,4) = STRING(slutut.DELNR,"9999"). 
   END.   
   ELSE DO:
      
      IF slutut.OMRADE = "8100" THEN DO:
         ASSIGN     
         SUBSTRING(str,1,5) = STRING(slutut.PROJEKT,"99999") 
         SUBSTRING(str,7,4) = STRING(slutut.DELNR,"9999").  
      END.
      
   END.
   ASSIGN
   SUBSTRING(str,12,10) = slutut.OMRADE
   SUBSTRING(str,23,4) = slutut.K1
   SUBSTRING(str,28,8) = slutut.K2
   SUBSTRING(str,37,15) = slutut.KOSTNADSSLAG  
   SUBSTRING(str,53,15) = STRING(slutut.BELOPP,"->>>>>>>>>>>.99")
   SUBSTRING(str,69) = TRIM(slutut.NAMN).
   IF slutut.DEBKRED = TRUE THEN SUBSTRING(str,100) = slutut.K4.
   IF str = ? THEN DO:
      OUTPUT TO VALUE(samvarglb) APPEND.
          PUT "OMR =" AT 1 slutut.OMRADE   SKIP.
          PUT "k1 =" AT 1 slutut.K1 AT 20 SKIP.                                 
          PUT "k2 ="  AT 1 slutut.K2 AT 20 SKIP.                                
          PUT "kostslag= " AT 1 slutut.KOSTNADSSLAG AT 20 SKIP.                      
          PUT "belopp = " AT 1 slutut.BELOPP AT 20 SKIP. 
          PUT "namn ="    AT 1 slutut.NAMN AT 20 SKIP.                          

      OUTPUT CLOSE.
       
   END.
   ELSE IF slutut.BELOPP NE 0 THEN DO:   
      PUT UNFORMATTED str AT 1 SKIP.    
   END.
   DELETE slutut. 
END PROCEDURE.
PROCEDURE kost_UI:
   DEFINE INPUT PARAMETER timlon AS LOGICAL NO-UNDO.
   /*DEBET POST*/
   IF ekoforst.FTG NE "SEAB" THEN ekoforst.EPERSONALKOD = "".
   FIND FIRST eko WHERE
   eko.PERSONALKOD = ekoforst.EPERSONALKOD AND
   eko.FTG = ekoforst.FTG AND
   eko.ENY = TRUE AND
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
   eko.PERSONALKOD = ekoforst.EPERSONALKOD 
   eko.FTG = ekoforst.FTG 
   eko.EDEBKRED = TRUE 
   eko.ENY = TRUE 
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
   eko2.PERSONALKOD = ekoforst.EPERSONALKOD AND
   eko2.FTG = ekoforst.FTG AND
   eko2.ENY = TRUE AND
   eko2.EORG = ekoforst.EORG AND 
   eko2.EVERDATUM = ekoforst.EVERDATUM AND
   eko2.EPROJEKT = ekoforst.EPROJEKT AND
   eko2.DELNR = ekoforst.DELNR AND
   eko2.EKOSTNADSSLAG = kontokode
   USE-INDEX ORG EXCLUSIVE-LOCK NO-ERROR.  
   IF NOT AVAILABLE eko2 THEN DO:
      CREATE eko2.
   END.    
   ASSIGN              
   eko2.PERSONALKOD = ekoforst.EPERSONALKOD 
   eko2.FTG = ekoforst.FTG 
   eko2.EDEBKRED = FALSE 
   eko2.ENY = TRUE 
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
PROCEDURE bilkost_UI:
   /*DEBET POST*/
   IF ekoforst.FTG NE "SEAB" THEN ekoforst.EPERSONALKOD = "".
   FIND FIRST eko WHERE
   eko.PERSONALKOD = ekoforst.EPERSONALKOD AND
   eko.FTG = ekoforst.FTG AND
   eko.ENY = TRUE AND
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
   eko.PERSONALKOD = ekoforst.EPERSONALKOD 
   eko.FTG = ekoforst.FTG 
   eko.EDEBKRED = TRUE 
   eko.ENY = TRUE 
   eko.EORG = ekoforst.EGEO 
   eko.EVERDATUM = ekoforst.EVERDATUM
   eko.EPROJEKT = ekoforst.EPROJEKT 
   eko.DELNR = ekoforst.DELNR 
   eko.EKOSTNADSSLAG = kontokod   
   eko.EBELOPP = eko.EBELOPP + ekoforst.ELONBELOPP.     
   /*KREDIT POST*/          
   FIND FIRST eko2 WHERE
   eko2.PERSONALKOD = ekoforst.EPERSONALKOD AND
   eko2.FTG = ekoforst.FTG AND
   eko2.ENY = TRUE AND
   eko2.EORG = persbuff.OMRADE AND 
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
   eko2.PERSONALKOD = ekoforst.EPERSONALKOD 
   eko2.FTG = ekoforst.FTG 
   eko2.EDEBKRED = FALSE 
   eko2.ENY = TRUE 
   eko2.EORG = persbuff.OMRADE  
   eko2.EVERDATUM = ekoforst.EVERDATUM
   eko2.EPROJEKT = ekoforst.EPROJEKT 
   eko2.DELNR = ekoforst.DELNR 
   eko2.EKOSTNADSSLAG = kontokode
   eko2.ELONTILLAGG = ekoforst.ELONTILLAGG
   eko2.EBELOPP = eko2.EBELOPP + ekoforst.ELONBELOPP.        
END PROCEDURE.

