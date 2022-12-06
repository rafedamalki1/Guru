/*SUNDFAKKONTO.I*/
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.
DEFINE TEMP-TABLE kopkosttemp NO-UNDO
   FIELD KOSTNADSLAG AS CHARACTER
   FIELD SVHT        AS CHARACTER
   INDEX KOST SVHT KOSTNADSLAG.

DEFINE TEMP-TABLE saljinttemp NO-UNDO
   FIELD KOSTNADSLAG AS CHARACTER
   FIELD OMRADE      AS CHARACTER
   FIELD AOSTART     AS CHARACTER
   INDEX KOST OMRADE AOSTART KOSTNADSLAG.
DEFINE TEMP-TABLE utfaktt NO-UNDO
   FIELD ORDNING AS INTEGER
   FIELD UT AS CHARACTER
   INDEX ORDNING ORDNING.

PROCEDURE sundstart_UI :
   /*konto        säljarens vht i aonrkont*/
   RUN kopkost_UI (INPUT "961611",INPUT "1611").
   RUN kopkost_UI (INPUT "961615",INPUT "1615"). 
   RUN kopkost_UI (INPUT "961621",INPUT "1621").
   RUN kopkost_UI (INPUT "961622",INPUT "1622"). 
   RUN kopkost_UI (INPUT "961623",INPUT "1623"). 
   RUN kopkost_UI (INPUT "961624",INPUT "1624").
   RUN kopkost_UI (INPUT "961626",INPUT "1626"). 
   RUN kopkost_UI (INPUT "961633",INPUT "1633"). 
   RUN kopkost_UI (INPUT "961653",INPUT "1653"). 
   RUN kopkost_UI (INPUT "961690",INPUT "1690").
   RUN kopkost_UI (INPUT "961691",INPUT "1691").
   RUN kopkost_UI (INPUT "961722",INPUT "1722"). 
   RUN kopkost_UI (INPUT "961730",INPUT "1730").
   RUN kopkost_UI (INPUT "961731",INPUT "1731").
   RUN kopkost_UI (INPUT "961750",INPUT "1750").
   RUN kopkost_UI (INPUT "961780",INPUT "1780").
   RUN kopkost_UI (INPUT "961790",INPUT "1790").
   RUN kopkost_UI (INPUT "961815",INPUT "1815").
   RUN kopkost_UI (INPUT "961822",INPUT "1822"). 
   RUN kopkost_UI (INPUT "961831",INPUT "1831").
   RUN kopkost_UI (INPUT "961832",INPUT "1832").
   RUN kopkost_UI (INPUT "961833",INPUT "1833").
   RUN kopkost_UI (INPUT "961850",INPUT "1850").
  
   RUN kopkost_UI (INPUT "961860",INPUT "1860").
   RUN kopkost_UI (INPUT "961871",INPUT "1871").
   RUN kopkost_UI (INPUT "961872",INPUT "1872").
   RUN kopkost_UI (INPUT "961890",INPUT "1890").
   RUN kopkost_UI (INPUT "961781",INPUT "1781").
   RUN kopkost_UI (INPUT "961625",INPUT "1625").
   RUN kopkost_UI (INPUT "961626",INPUT "1626").
   RUN kopkost_UI (INPUT "961627",INPUT "1627").
   RUN kopkost_UI (INPUT "961628",INPUT "1628").
   RUN kopkost_UI (INPUT "961629",INPUT "1629").
   RUN kopkost_UI (INPUT "961630",INPUT "1630").
   RUN kopkost_UI (INPUT "961730",INPUT "1730").
   RUN kopkost_UI (INPUT "961791",INPUT "1791").
   RUN kopkost_UI (INPUT "961915",INPUT "1915").
   RUN kopkost_UI (INPUT "961922",INPUT "1922").
   RUN kopkost_UI (INPUT "961931",INPUT "1931").
   RUN kopkost_UI (INPUT "961932",INPUT "1932").
   RUN kopkost_UI (INPUT "961933",INPUT "1933").
   RUN kopkost_UI (INPUT "961940",INPUT "1940").
   RUN kopkost_UI (INPUT "961950",INPUT "1950").
   RUN kopkost_UI (INPUT "961960",INPUT "1960").
   RUN kopkost_UI (INPUT "961971",INPUT "1971").
   RUN kopkost_UI (INPUT "961972",INPUT "1972").
   RUN kopkost_UI (INPUT "961990",INPUT "1990").
   
   /*20100205*/
   RUN kopkost_UI (INPUT "961852",INPUT "1852").
   RUN kopkost_UI (INPUT "961864",INPUT "1864").
   RUN kopkost_UI (INPUT "961881",INPUT "1881").
   RUN kopkost_UI (INPUT "961882",INPUT "1882").
   RUN kopkost_UI (INPUT "961885",INPUT "1885").
   RUN kopkost_UI (INPUT "961691",INPUT "1540").
   RUN kopkost_UI (INPUT "961329",INPUT "1329").   
   
   
                                         /*köpare vht*/
                                         /*
   RUN saljint_UI(INPUT "936820",INPUT "1380",INPUT "3").
   RUN saljint_UI(INPUT "936821",INPUT "1511",INPUT "3").
   RUN saljint_UI(INPUT "933300",INPUT "1380",INPUT "").
   RUN saljint_UI(INPUT "933500",INPUT "1511",INPUT "").
   */
   RUN saljint_UI(INPUT "933300",INPUT "",INPUT "").
   
END PROCEDURE.
PROCEDURE kopkost_UI:
   DEFINE INPUT PARAMETER konto AS CHARACTER.
   DEFINE INPUT PARAMETER saljvht AS CHARACTER.
   CREATE kopkosttemp.
   ASSIGN
   kopkosttemp.KOSTNADSLAG = konto 
   kopkosttemp.SVHT = saljvht.    
END PROCEDURE.

PROCEDURE saljint_UI:
   DEFINE INPUT PARAMETER konto AS CHARACTER.
   DEFINE INPUT PARAMETER omrvar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER aostartvar AS CHARACTER.
   CREATE saljinttemp.
   ASSIGN
   saljinttemp.KOSTNADSLAG = konto 
   saljinttemp.OMRADE = omrvar 
   saljinttemp.AOSTART = aostartvar.
   
END PROCEDURE.

PROCEDURE utsund_UI:
   /*faktfor*/
   DEFINE VARIABLE prognamnvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fakthuvfil  AS CHARACTER NO-UNDO.
   DEFINE VARIABLE fakthuvfilkopia AS CHARACTER NO-UNDO.
   DEFINE VARIABLE bokfil      AS CHARACTER NO-UNDO.
   DEFINE VARIABLE bokfilkopia AS CHARACTER NO-UNDO.
   DEFINE VARIABLE orevarhjs AS DECIMAL NO-UNDO.
    
   /*
   IF namndb() = "utbi" THEN RETURN.
   */
   IF namndb() = "sundn9" OR namndb() = "sundnat" OR namndb() = "CRT9"  THEN DO:   
   END.
   ELSE RETURN.
   
   IF FAKTREGLER.KUNDID = 3 THEN DO:
      RUN cgisund_UI.
      RETURN.
   END.   
       
   RUN sundstart_UI.
   regar = YEAR(TODAY).
   regdatum = TODAY.
   RUN REGVEC.P.
   IF MONTH(TODAY) = 1 THEN regmannamn = "jan".
   ELSE IF MONTH(TODAY) = 2 THEN regmannamn = "feb".
   ELSE IF MONTH(TODAY) = 3 THEN regmannamn = "mar". 
   ELSE IF MONTH(TODAY) = 4 THEN regmannamn = "apr". 
   ELSE IF MONTH(TODAY) = 5 THEN regmannamn = "maj". 
   ELSE IF MONTH(TODAY) = 6 THEN regmannamn = "jun".
   ELSE IF MONTH(TODAY) = 7 THEN regmannamn = "jul".
   ELSE IF MONTH(TODAY) = 8 THEN regmannamn = "aug". 
   ELSE IF MONTH(TODAY) = 9 THEN regmannamn = "sep".
   ELSE IF MONTH(TODAY) = 10 THEN regmannamn = "okt". 
   ELSE IF MONTH(TODAY) = 11 THEN regmannamn = "nov".
   ELSE IF MONTH(TODAY) = 12 THEN regmannamn = "dec".
   IF debkred = FALSE THEN DO:
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = FAKTURERAD.FAKTTYPID NO-LOCK NO-ERROR.
      RUN viskont_UI.      
   END.
   IF debkred = TRUE THEN DO:
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = FAKTKRED.FAKTTYPID NO-LOCK NO-ERROR.
      RUN viskontK_UI.      
   END.
   /*Bara ett aonr*/
   DEFINE VARIABLE sbelopp AS DECIMAL NO-UNDO.
   FOR EACH sumkont WHERE sumkont.AONRKOP = "":
      sbelopp = sumkont.BELOPP.
      FOR EACH EXTRAKOPPLINGAR WHERE EXTRAKOPPLINGAR.PROGRAM = "FBAONR" AND
      EXTRAKOPPLINGAR.KOPPLACHAR1 = sumkont.AONR AND       
      EXTRAKOPPLINGAR.KOPPLAINT1 = sumkont.DELNR NO-LOCK:
         IF sumkont.AONRKOP = ""  THEN DO:
            ASSIGN
            sumkont.AONRKOP = EXTRAKOPPLINGAR.KOPPLACHAR2
            sumkont.DELNRKOP = EXTRAKOPPLINGAR.KOPPLAINT2
            sumkont.BELOPP = sbelopp * EXTRAKOPPLINGAR.SOKINT[1] / 100.
         END.            
         ELSE DO:
            CREATE sumkontbuff.
            BUFFER-COPY sumkont TO sumkontbuff.
            ASSIGN
            sumkontbuff.AONRKOP = EXTRAKOPPLINGAR.KOPPLACHAR2
            sumkontbuff.DELNRKOP = EXTRAKOPPLINGAR.KOPPLAINT2
            sumkontbuff.BELOPP = sbelopp * EXTRAKOPPLINGAR.SOKINT[1] / 100. 
         END.
      END.
   END.

   REPEAT:
      FIND FIRST sumkont WHERE sumkont.OMRADESALJ  = "" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sumkont THEN LEAVE.   
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = sumkont.AONR AND AONRTAB.DELNR = sumkont.DELNR NO-LOCK NO-ERROR.
      FOR EACH sumkontbuff WHERE sumkontbuff.AONR = AONRTAB.AONR AND sumkontbuff.DELNR = AONRTAB.DELNR :
         sumkontbuff.OMRADESALJ = AONRTAB.OMRADE.
      END.
   END.
   REPEAT:
      FIND FIRST sumkont WHERE sumkont.OMRADEKOP = "" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sumkont THEN LEAVE.   
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = sumkont.AONRKOP AND AONRTAB.DELNR = sumkont.DELNRKOP NO-LOCK NO-ERROR.
      FOR EACH sumkontbuff WHERE sumkontbuff.AONRKOP = AONRTAB.AONR AND sumkontbuff.DELNRKOP = AONRTAB.DELNR :
         sumkontbuff.OMRADEKOP = AONRTAB.OMRADE.
      END.
   END.
   REPEAT:
      FIND FIRST sumkontkop WHERE sumkontkop.OMRADEKOP  = "" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sumkontkop THEN LEAVE.   
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = sumkontkop.AONRKOP AND AONRTAB.DELNR = sumkontkop.DELNRKOP NO-LOCK NO-ERROR.
      FOR EACH sumkontkopbuff WHERE sumkontkopbuff.AONRKOP = AONRTAB.AONR AND sumkontkopbuff.DELNRKOP = AONRTAB.DELNR :
         sumkontkopbuff.OMRADEKOP = AONRTAB.OMRADE.
      END.
   END.
   REPEAT:
      FIND FIRST sumkontkop WHERE sumkontkop.OMRADESALJ  = "" NO-LOCK NO-ERROR.
      IF NOT AVAILABLE sumkontkop THEN LEAVE.   
      FIND FIRST AONRTAB WHERE AONRTAB.AONR = sumkontkop.AONR AND AONRTAB.DELNR = sumkontkop.DELNR NO-LOCK NO-ERROR.
      FOR EACH sumkontkopbuff WHERE sumkontkopbuff.AONR = AONRTAB.AONR AND sumkontkopbuff.DELNR = AONRTAB.DELNR :
         sumkontkopbuff.OMRADESALJ = AONRTAB.OMRADE.
      END.
   END.
   
   FOR EACH sumkont:           
      FIND FIRST saljinttemp NO-LOCK NO-ERROR.
      IF NOT AVAILABLE saljinttemp THEN DO:
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = "Borde finnas i kontotabellen " +  Guru.Konstanter:gomrk + " " + sumkont.OMRADEKOP + " och " + Guru.Konstanter:gaok + " " + sumkont.AONR + " " + STRING(sumkont.DELNR,">99") + " saknar konto!".           
      END.         
      ELSE sumkont.KOSTNADSLAG = saljinttemp.KOSTNADSLAG.
   /*       
      IF TRIM(sumkont.AONRKOP) BEGINS "3" THEN DO:
         FIND FIRST saljinttemp WHERE saljinttemp.OMRADE = sumkont.OMRADEKOP AND saljinttemp.AOSTART = SUBSTRING(TRIM(sumkont.AONRKOP),1,1) NO-LOCK NO-ERROR.
         IF NOT AVAILABLE saljinttemp THEN DO:
            CREATE felmeddtemp.
            ASSIGN 
            felmeddtemp.FELMEDD = "Borde finnas i kontotabellen " + gomrk + " " + sumkont.OMRADEKOP + " " + sumkont.AONRKOP + " och " + Guru.Konstanter:gaok + " " + sumkont.AONR + " " + STRING(sumkont.DELNR,">99") + " saknar konto!".           
         END.
         ELSE sumkont.KOSTNADSLAG = saljinttemp.KOSTNADSLAG.
      END. 
      ELSE DO:
         FIND FIRST saljinttemp WHERE saljinttemp.OMRADE = sumkont.OMRADEKOP AND saljinttemp.AOSTART = "" NO-LOCK NO-ERROR.
         IF NOT AVAILABLE saljinttemp THEN DO:
            CREATE felmeddtemp.
            ASSIGN 
            felmeddtemp.FELMEDD = "Borde finnas i kontotabellen " + gomrk + " " + sumkont.OMRADEKOP + " och " + Guru.Konstanter:gaok + " " + sumkont.AONR + " " + STRING(sumkont.DELNR,">99") + " saknar konto!".           
         END.         
         ELSE sumkont.KOSTNADSLAG = saljinttemp.KOSTNADSLAG.
      END.
      */
   END.   
   FOR EACH sumkontkop WHERE sumkontkop.K1SALJ = "":
                               /*sök säljarens proj*/
      FOR EACH sumkont WHERE sumkont.AONR = sumkontkop.AONR AND sumkont.DELNR = sumkontkop.DELNR AND
      sumkont.AONRKOP = sumkontkop.AONRKOP AND sumkont.DELNRKOP = sumkontkop.DELNRKOP:
         CREATE sumkontkopbuff.
         BUFFER-COPY sumkontkop TO sumkontkopbuff.
         ASSIGN
         sumkontkopbuff.K1SALJ = sumkont.K1
         sumkontkopbuff.BELOPP = ROUND(sumkontkopbuff.BELOPP * sumkont.SATS% / 100,2).                                 
         /*fakturerat belopp * % från faktura bestproj * kontoförd hos köpare * kontoförd hos säljare*/
      END.
      DELETE sumkontkop.
   END.
   FOR EACH sumkontkop:
      FIND FIRST kopkosttemp WHERE kopkosttemp.SVHT = sumkontkop.K1SALJ NO-LOCK NO-ERROR.
      IF NOT AVAILABLE kopkosttemp THEN DO:
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = Guru.Konstanter:gomrk + " " + sumkontkop.K1SALJ + " och " + Guru.Konstanter:gaok + " " + sumkontkop.AONR + " " + STRING(sumkontkop.DELNR,">99") + " saknar konto för köparen!".           
      END.                  
      ELSE sumkontkop.KOSTNADSLAG = kopkosttemp.KOSTNADSLAG.         
   END.
   FOR EACH sumkont:      
      ACCUMULATE sumkont.BELOPP (TOTAL).
   END.     
   FOR EACH sumkontkop:      
      ACCUMULATE sumkontkop.BELOPP (TOTAL).
   END. 
   orevarhjs = (ACCUM TOTAL sumkont.BELOPP) - (ACCUM TOTAL sumkontkop.BELOPP).
   FIND FIRST sumkontkop NO-LOCK NO-ERROR.
   IF AVAILABLE sumkontkop THEN sumkontkop.BELOPP = sumkontkop.BELOPP + orevarhjs.
   IF MONTH(TODAY) = 1 THEN regmannamn = "jan".
   ELSE IF MONTH(TODAY) = 2 THEN regmannamn = "feb".
   ELSE IF MONTH(TODAY) = 3 THEN regmannamn = "mar". 
   ELSE IF MONTH(TODAY) = 4 THEN regmannamn = "apr". 
   ELSE IF MONTH(TODAY) = 5 THEN regmannamn = "maj". 
   ELSE IF MONTH(TODAY) = 6 THEN regmannamn = "jun".
   ELSE IF MONTH(TODAY) = 7 THEN regmannamn = "jul".
   ELSE IF MONTH(TODAY) = 8 THEN regmannamn = "aug". 
   ELSE IF MONTH(TODAY) = 9 THEN regmannamn = "sep".
   ELSE IF MONTH(TODAY) = 10 THEN regmannamn = "okt". 
   ELSE IF MONTH(TODAY) = 11 THEN regmannamn = "nov".
   ELSE IF MONTH(TODAY) = 12 THEN regmannamn = "dec".
   IF namndb() = "RT9" THEN DO:
      kommando2 = "D:\DELAD\PRO9S\EXKOPIA\faD" + STRING(YEAR(TODAY)) + regmannamn. 
      kommando = "D:\DELAD\PRO9S\EXPORT\faD" + STRING(TODAY,"99999999"). 
   END.
   ELSE IF namndb() = "utbi" THEN DO:
      kommando2 = "c:\DELAD\PRO9S\EXKOPIA\faD" + STRING(YEAR(TODAY)) + regmannamn. 
      kommando = "c:\DELAD\PRO9S\EXPORT\faD" + STRING(TODAY,"99999999"). 
   END.
   ELSE DO:
   
      kommando2 = "D:\DELAD\SERVER\PRO10S\BACKEXPORT\faD" + STRING(YEAR(TODAY)) + regmannamn. 
      kommando = "D:\DELAD\SERVER\PRO10S\EXPORT\EKONOMI\faD" + STRING(TODAY,"99999999").
      /*SNATBERGET*/
      kommando2 = REPLACE(kommando2,"D:\DELAD\SERVER\PRO10S\","D:\DELAD\PRO10S\").
      kommando = REPLACE(kommando,"D:\DELAD\SERVER\PRO10S\","D:\DELAD\PRO10S\").
           
     
   END.
    
   /*
   kommando2 = "C:\DELAD\PRO9S\EXKOPIA\faD" + STRING(YEAR(TODAY)) + regmannamn. 
      kommando = "C:\DELAD\PRO9S\EXPORT\faD" + STRING(TODAY,"99999999").
   */
   IF AVAILABLE FAKTURERAD THEN DO:
      IF SEARCH(kommando) = ? THEN musz = TRUE.
      OUTPUT TO VALUE(kommando) APPEND.
      IF musz = TRUE THEN  PUT "Datum " FAKTURERAD.BOKDATUM SKIP.
      OUTPUT CLOSE.
      OUTPUT TO VALUE(kommando2) APPEND.
      IF musz = TRUE THEN  PUT "Datum " FAKTURERAD.BOKDATUM SKIP.
      OUTPUT CLOSE.
   END.
   IF AVAILABLE FAKTKRED THEN DO:
      IF SEARCH(kommando) = ? THEN musz = TRUE.
      OUTPUT TO VALUE(kommando) APPEND.
      IF musz = TRUE THEN  PUT "Datum " FAKTKRED.BOKDATUM SKIP.
      OUTPUT CLOSE.
      OUTPUT TO VALUE(kommando2) APPEND.
      IF musz = TRUE THEN  PUT "Datum " FAKTKRED.BOKDATUM SKIP.
      OUTPUT CLOSE.
   END.

   musz = FALSE.
   RUN sundfilutsalj_UI.
   RUN sundfilutkop_UI.  
   utfil =  SESSION:TEMP-DIR + "faktfel.txt".
   FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
   IF AVAILABLE felmeddtemp THEN DO:
      RUN TIDSTOPP.P (INPUT 8,INPUT "FAKTURERING FEL" + STRING(fplanr),INPUT Guru.Konstanter:globanv, OUTPUT lasanv,INPUT-OUTPUT TABLE felmeddtemp).     
      OUTPUT TO VALUE(utfil) APPEND.
      PUT TODAY AT 1 SKIP.
      PUT STRING(TIME,"hh:mm:ss") AT 1 SKIP.
      FOR EACH felmeddtemp:
         PUT UNFORMATTED felmeddtemp.FELMEDD SKIP.
      END.
      OUTPUT CLOSE.
   END.    
 /*  OS-APPEND VALUE(kommando) VALUE(kommando2).*/
   
END PROCEDURE.

PROCEDURE sundfilutsalj_UI:   

   FOR EACH sumkont:
      IF SUBSTRING(sumkont.AONR,1,1) = "S" THEN DO:
         IF LENGTH(sumkont.AONR) > 1 THEN
         sumkont.AONR = SUBSTRING(sumkont.AONR,2,LENGTH(sumkont.AONR) - 1).
      END.
      IF LENGTH(sumkont.AONR) = 1 THEN sumkont.AONR = sumkont.AONR + "0000".
      ELSE IF LENGTH(sumkont.AONR) = 2 THEN sumkont.AONR = sumkont.AONR + "000".
      ELSE IF LENGTH(sumkont.AONR) = 3 THEN sumkont.AONR = sumkont.AONR + "00".
      ELSE IF LENGTH(sumkont.AONR) = 4 THEN sumkont.AONR = sumkont.AONR + "0".                                                       
      IF LENGTH(sumkont.K2) = 1      THEN sumkont.K2 = sumkont.K2 + "0000000".
      ELSE IF LENGTH(sumkont.K2) = 2 THEN sumkont.K2 = sumkont.K2 + "000000".
      ELSE IF LENGTH(sumkont.K2) = 3 THEN sumkont.K2 = sumkont.K2 + "00000".
      ELSE IF LENGTH(sumkont.K2) = 4 THEN sumkont.K2 = sumkont.K2 + "0000".
      ELSE IF LENGTH(sumkont.K2) = 5 THEN sumkont.K2 = sumkont.K2 + "000".
      ELSE IF LENGTH(sumkont.K2) = 6 THEN sumkont.K2 = sumkont.K2 + "00".
      ELSE IF LENGTH(sumkont.K2) = 7 THEN sumkont.K2 = sumkont.K2 + "0".   
      str = "".
      /*
      a   a d  d o        o 1  1 2      2 k             k b             b
      12345678901234567890123456789012345678901234567890123456789012345678901234567890                   
      */
      /*DEBET*/
      IF debkred = FALSE THEN DO:
         sumkont.BELOPP = sumkont.BELOPP * -1.
         ASSIGN     
         SUBSTRING(str,1,5) = STRING(sumkont.AONR,"99999") 
         SUBSTRING(str,7,4) = STRING(sumkont.DELNR,"9999"). 
      END.   
      ELSE DO:         
         ASSIGN     
         SUBSTRING(str,1,5) = STRING(sumkont.AONR,"99999") 
         SUBSTRING(str,7,4) = STRING(sumkont.DELNR,"9999").                   
      END.
      ASSIGN
      SUBSTRING(str,12,10) = sumkont.OMRADESALJ
      SUBSTRING(str,23,4) = sumkont.K1
      SUBSTRING(str,28,8) = sumkont.K2
      SUBSTRING(str,37,15) = sumkont.KOSTNADSLAG  
      SUBSTRING(str,53,15) = STRING(sumkont.BELOPP,"->>>>>>>>>>>.99").     
      IF str = ? THEN DO:
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = "OMR =" + sumkont.OMRADESALJ + "k1 =" + sumkont.K1 + "k2 =" + sumkont.K2 + "kostslag= " + sumkont.KOSTNADSLAG.         
      END.
      ELSE IF sumkont.BELOPP NE 0 THEN DO:   
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE felmeddtemp THEN DO:
            OUTPUT TO VALUE(kommando) APPEND.
            PUT UNFORMATTED str AT 1 SKIP.    
            OUTPUT CLOSE.
            OUTPUT TO VALUE(kommando2) APPEND.
            PUT UNFORMATTED str AT 1 SKIP.    
            OUTPUT CLOSE.            
         END.         
         ELSE DO:
            /*
            utfil =  SESSION:TEMP-DIR + "faktfel.txt".
            OUTPUT TO VALUE(utfil) APPEND.
            PUT TODAY AT 1 SKIP.
            PUT STRING(TIME,"hh:mm:ss") AT 1 SKIP.
            FOR EACH felmeddtemp:
               PUT UNFORMATTED felmeddtemp.FELMEDD SKIP.
            END.
            OUTPUT CLOSE.
            */            
         END.
      END.
      DELETE sumkont. 
   END.
END PROCEDURE.


PROCEDURE sundfilutkop_UI:
   FOR EACH sumkontkop:
      IF SUBSTRING(sumkontkop.AONRKOP,1,1) = "S" THEN DO:
         IF LENGTH(sumkontkop.AONRKOP) > 1 THEN
         sumkontkop.AONRKOP = SUBSTRING(sumkontkop.AONRKOP,2,LENGTH(sumkontkop.AONRKOP) - 1).
      END.
      IF LENGTH(sumkontkop.AONRKOP) = 1 THEN sumkontkop.AONRKOP = sumkontkop.AONRKOP + "0000".
      ELSE IF LENGTH(sumkontkop.AONRKOP) = 2 THEN sumkontkop.AONRKOP = sumkontkop.AONRKOP + "000".
      ELSE IF LENGTH(sumkontkop.AONRKOP) = 3 THEN sumkontkop.AONRKOP = sumkontkop.AONRKOP + "00".
      ELSE IF LENGTH(sumkontkop.AONRKOP) = 4 THEN sumkontkop.AONRKOP = sumkontkop.AONRKOP + "0".                                                       
      IF LENGTH(sumkontkop.K2) = 1      THEN sumkontkop.K2 = sumkontkop.K2 + "0000000".
      ELSE IF LENGTH(sumkontkop.K2) = 2 THEN sumkontkop.K2 = sumkontkop.K2 + "000000".
      ELSE IF LENGTH(sumkontkop.K2) = 3 THEN sumkontkop.K2 = sumkontkop.K2 + "00000".
      ELSE IF LENGTH(sumkontkop.K2) = 4 THEN sumkontkop.K2 = sumkontkop.K2 + "0000".
      ELSE IF LENGTH(sumkontkop.K2) = 5 THEN sumkontkop.K2 = sumkontkop.K2 + "000".
      ELSE IF LENGTH(sumkontkop.K2) = 6 THEN sumkontkop.K2 = sumkontkop.K2 + "00".
      ELSE IF LENGTH(sumkontkop.K2) = 7 THEN sumkontkop.K2 = sumkontkop.K2 + "0".   
      str = "".
      /*
      a   a d  d o        o 1  1 2      2 k             k b             b
      12345678901234567890123456789012345678901234567890123456789012345678901234567890                   
      */
      /*DEBET*/
      IF debkred = FALSE THEN DO:
         ASSIGN     
         SUBSTRING(str,1,5) = STRING(sumkontkop.AONRKOP,"99999") 
         SUBSTRING(str,7,4) = STRING(sumkontkop.DELNRKOP,"9999"). 
      END.   
      ELSE DO:
         sumkontkop.BELOPP = sumkontkop.BELOPP * -1.
         ASSIGN     
         SUBSTRING(str,1,5) = STRING(sumkontkop.AONRKOP,"99999") 
         SUBSTRING(str,7,4) = STRING(sumkontkop.DELNRKOP,"9999").                 
      END.
      ASSIGN
      SUBSTRING(str,12,10) = sumkontkop.OMRADEKOP
      SUBSTRING(str,23,4) = sumkontkop.K1
      SUBSTRING(str,28,8) = sumkontkop.K2.
      IF sumkontkop.K3 = "" THEN SUBSTRING(str,37,15) = sumkontkop.KOSTNADSLAG.
      ELSE SUBSTRING(str,37,15) = sumkontkop.K3.
      SUBSTRING(str,53,15) = STRING(sumkontkop.BELOPP,"->>>>>>>>>>>.99").     
      IF str = ? THEN DO:
         CREATE felmeddtemp.
         ASSIGN 
         felmeddtemp.FELMEDD = "OMR =" + sumkont.OMRADEKOP + "k1 =" + sumkont.K1 + "k2 =" + sumkont.K2 + "kostslag= " + sumkont.KOSTNADSLAG.                          
      END.
      ELSE IF sumkontkop.BELOPP NE 0 THEN DO: 
         FIND FIRST felmeddtemp NO-LOCK NO-ERROR.
         IF NOT AVAILABLE felmeddtemp THEN DO:
            OUTPUT TO VALUE(kommando) APPEND.
            PUT UNFORMATTED str AT 1 SKIP.    
            OUTPUT CLOSE.
            OUTPUT TO VALUE(kommando2) APPEND.
            PUT UNFORMATTED str AT 1 SKIP.    
            OUTPUT CLOSE.            
         END.
         ELSE DO:
            /*
            utfil =  SESSION:TEMP-DIR + "faktfel.txt".
            OUTPUT TO VALUE(utfil) APPEND.
            PUT TODAY AT 1 SKIP.
            PUT STRING(TIME,"hh:mm:ss") AT 1 SKIP.
            FOR EACH felmeddtemp:
               PUT UNFORMATTED felmeddtemp.FELMEDD SKIP.
            END.
            OUTPUT CLOSE.
            */            
         END.
      END.
      DELETE sumkontkop. 
   END.
END PROCEDURE.
PROCEDURE cgisund_UI :
   DEFINE VARIABLE ordvar AS INTEGER NO-UNDO.
   regar = YEAR(TODAY).
   regdatum = TODAY.
   RUN REGVEC.P.
   IF MONTH(TODAY) = 1 THEN regmannamn = "jan".
   ELSE IF MONTH(TODAY) = 2 THEN regmannamn = "feb".
   ELSE IF MONTH(TODAY) = 3 THEN regmannamn = "mar". 
   ELSE IF MONTH(TODAY) = 4 THEN regmannamn = "apr". 
   ELSE IF MONTH(TODAY) = 5 THEN regmannamn = "maj". 
   ELSE IF MONTH(TODAY) = 6 THEN regmannamn = "jun".
   ELSE IF MONTH(TODAY) = 7 THEN regmannamn = "jul".
   ELSE IF MONTH(TODAY) = 8 THEN regmannamn = "aug". 
   ELSE IF MONTH(TODAY) = 9 THEN regmannamn = "sep".
   ELSE IF MONTH(TODAY) = 10 THEN regmannamn = "okt". 
   ELSE IF MONTH(TODAY) = 11 THEN regmannamn = "nov".
   ELSE IF MONTH(TODAY) = 12 THEN regmannamn = "dec".
   
   IF debkred = FALSE THEN DO:
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = FAKTURERAD.FAKTTYPID NO-LOCK NO-ERROR.
      RUN viskont_UI.
      RUN summasunddeb_UI.      
   END.
   IF debkred = TRUE THEN DO:
      FIND FIRST FAKTURERINGSTYP WHERE 
      FAKTURERINGSTYP.FAKTTYPID = FAKTKRED.FAKTTYPID NO-LOCK NO-ERROR.
      RUN viskontK_UI.  
      RUN summasundkred_UI.    
   END.
 
   FOR EACH sumkont WHERE NO-LOCK:
      REPEAT:
         FIND FIRST sumpers WHERE sumpers.AONR = sumkont.AONR AND sumpers.DELNR = sumkont.DELNR AND sumpers.KLAR = FALSE NO-LOCK NO-ERROR.
         IF NOT AVAILABLE sumpers THEN LEAVE.
         FOR EACH sumpers WHERE sumpers.AONR = sumkont.AONR AND sumpers.DELNR = sumkont.DELNR AND sumpers.KLAR = FALSE NO-LOCK:
            ASSIGN
            sumpers.KLAR = TRUE
            sumpers.K1 = sumkont.K1
            sumpers.K2 = sumkont.K2
            sumpers.K3 = sumkont.K3
            sumpers.K4 = sumkont.K4
            sumpers.K5 = sumkont.K5.
            IF sumkont.SATS% NE 100 THEN DO:
               CREATE sumpers2.
               BUFFER-COPY sumpers TO sumpers2.
               sumpers2.KLAR = FALSE.
               sumpers2.BELOPP = sumpers.BELOPP.
               sumpers.BELOPP = sumpers.BELOPP * sumkont.SATS% / 100.            
            END.   
         END.
      END.   
      FOR EACH sumpers2 WHERE NO-LOCK:
         CREATE sumpers.
         BUFFER-COPY sumpers2 TO sumpers.
      END.
      EMPTY TEMP-TABLE sumpers2 NO-ERROR. 
        
   END. 
   FOR EACH sumpers WHERE sumpers.KLAR = FALSE NO-LOCK:
      DELETE sumpers.
   END.
   DEBUGGER:SET-BREAK().
   IF namndb() = "RT9" THEN DO:
      kommando2 = "D:\DELAD\PRO9S\EXKOPIA\faR" + STRING(YEAR(TODAY)) + regmannamn. 
      kommando = "D:\DELAD\PRO9S\EXPORT\faR" + STRING(TODAY,"99999999"). 
   END.
   ELSE DO:
      kommando2 = "D:\DELAD\SERVER\PRO10S\BACKEXPORT\faR" + STRING(YEAR(TODAY)) + regmannamn. 
      kommando = "D:\DELAD\SERVER\PRO10S\EXPORT\EKONOMI\faR" + STRING(TODAY,"99999999"). 
      /*SNATBERGET*/
      kommando2 = REPLACE(kommando2,"D:\DELAD\SERVER\PRO10S\","D:\DELAD\PRO10S\").
      kommando = REPLACE(kommando,"D:\DELAD\SERVER\PRO10S\","D:\DELAD\PRO10S\").
         
   END. 
   /*
   kommando2 = "D:\DELAD\PRO9S\EXKOPIA\faR" + STRING(YEAR(TODAY)) + regmannamn + ".txt". 
   kommando = "D:\DELAD\PRO9S\EXPORT\faR" + STRING(TODAY,"99999999") + ".txt".
   */
   ordvar = 0.
   CREATE utfaktt.
   ordvar = ordvar + 1.
   ASSIGN
   utfaktt.ORDNING = ordvar
   SUBSTRING(utfaktt.UT,1,2) = "HP"
   SUBSTRING(utfaktt.UT,3,2) = "GU"
   SUBSTRING(utfaktt.UT,5,8) = ""
   SUBSTRING(utfaktt.UT,13,12) = "2100002"
   SUBSTRING(utfaktt.UT,25,8) = ""
   SUBSTRING(utfaktt.UT,33,8) = ""
   SUBSTRING(utfaktt.UT,41,30) = ""
   SUBSTRING(utfaktt.UT,71,30) = "".
   FOR EACH sumpers WHERE NO-LOCK:
      IF LENGTH(sumpers.K2) = 1      THEN sumpers.K2 = sumpers.K2 + "0000000".
      ELSE IF LENGTH(sumpers.K2) = 2 THEN sumpers.K2 = sumpers.K2 + "000000".
      ELSE IF LENGTH(sumpers.K2) = 3 THEN sumpers.K2 = sumpers.K2 + "00000".
      ELSE IF LENGTH(sumpers.K2) = 4 THEN sumpers.K2 = sumpers.K2 + "0000".
      ELSE IF LENGTH(sumpers.K2) = 5 THEN sumpers.K2 = sumpers.K2 + "000".
      ELSE IF LENGTH(sumpers.K2) = 6 THEN sumpers.K2 = sumpers.K2 + "00".
      ELSE IF LENGTH(sumpers.K2) = 7 THEN sumpers.K2 = sumpers.K2 + "0". 
      CREATE utfaktt.
      ordvar = ordvar + 1.
      ASSIGN
      utfaktt.ORDNING = ordvar
      SUBSTRING(utfaktt.UT,1,2) = "RP"
      SUBSTRING(utfaktt.UT,3,4) = SUBSTRING(sumpers.K1,1,4)
      SUBSTRING(utfaktt.UT,7,6) = "369999"
      /*Anders Olsson Elpool i Umeå AB  9 okt 2014 17:24:34 
      EFTER NYÅR 
      */
      SUBSTRING(utfaktt.UT,13,5) = sumpers.AONR
      SUBSTRING(utfaktt.UT,18,3) = STRING(sumpers.DELNR,"999")
      SUBSTRING(utfaktt.UT,21,8) = SUBSTRING(sumpers.K2,1,8)
      
      /*
      SUBSTRING(utfaktt.UT,13,5) = "28800"
      SUBSTRING(utfaktt.UT,18,4) = STRING(sumpers.DELNR,"9999")
      SUBSTRING(utfaktt.UT,22,5) = sumpers.AONR
      */
      SUBSTRING(utfaktt.UT,27,8) = SUBSTRING(sumpers.K2,1,8)
      SUBSTRING(utfaktt.UT,35,45) = SUBSTRING(sumpers.VIBEFATTNING,1,45)
      SUBSTRING(utfaktt.UT,80,2) = "M1".  
      IF debkred = TRUE THEN DO:
         sumpers.BELOPP = sumpers.BELOPP * 100.
         IF sumpers.BELOPP >= 0 THEN SUBSTRING(utfaktt.UT,82,15) = STRING(sumpers.BELOPP * -1,"-99999999999999").
         ELSE SUBSTRING(utfaktt.UT,82,15) = STRING(sumpers.BELOPP * -1,"999999999999999"). 
      END.   
      ELSE DO:
         sumpers.BELOPP = sumpers.BELOPP * 100.
         IF sumpers.BELOPP >= 0 THEN SUBSTRING(utfaktt.UT,82,15) = STRING(sumpers.BELOPP,"999999999999999").
         ELSE  SUBSTRING(utfaktt.UT,82,15) = STRING(sumpers.BELOPP,"-99999999999999").                          
      END.
   END.
   CREATE utfaktt.
   ordvar = ordvar + 1.
   ASSIGN
   utfaktt.ORDNING = ordvar.
   SUBSTRING(utfaktt.UT,1,4) = "SLUT".
   OUTPUT TO VALUE(kommando) APPEND.
   FOR EACH utfaktt WHERE NO-LOCK:
      PUT UNFORMATTED utfaktt.UT SKIP.     
   END.
   OUTPUT CLOSE.
   OS-COPY VALUE(kommando) VALUE(kommando2). 
   

 END PROCEDURE.
 PROCEDURE summasunddeb_UI :
    DEFINE VARIABLE ohj AS INTEGER NO-UNDO.
    ohj = ohj + 1.
    FOR EACH FAKTTID WHERE FAKTTID.MED = TRUE AND FAKTTID.FAKTNR = fplanr AND FAKTTID.VFAKTNR = skarpvar BREAK 
      BY FAKTTID.PRISA BY FAKTTID.AONR BY FAKTTID.DELNR:         
         ACCUMULATE 
         FAKTTID.BELOPP (TOTAL BY FAKTTID.PRISA BY FAKTTID.AONR BY FAKTTID.DELNR).
         ACCUMULATE 
         FAKTTID.TIMMAR (TOTAL BY FAKTTID.PRISA BY FAKTTID.AONR BY FAKTTID.DELNR).
         IF LAST-OF(FAKTTID.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.TIMMAR) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Arbetskostnad"            
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTID.AONR
               sumpers.DELNR = FAKTTID.DELNR               
               sumpers.OTIMMAR = FAKTTID.PRISA 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.TIMMAR)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.BELOPP).                              
            END.
         END.
      END.
      ohj = ohj + 1.
      FOR EACH FAKTTID WHERE FAKTTID.MED = TRUE  AND FAKTTID.FAKTNR = fplanr AND FAKTTID.VFAKTNR = skarpvar BREAK 
      BY FAKTTID.OPRIS BY FAKTTID.AONR BY FAKTTID.DELNR:         
         ACCUMULATE 
         FAKTTID.OBELOPP (TOTAL BY FAKTTID.OPRIS BY FAKTTID.AONR BY FAKTTID.DELNR).
         ACCUMULATE 
         FAKTTID.OTIMMAR (TOTAL BY FAKTTID.OPRIS BY FAKTTID.AONR BY FAKTTID.DELNR).
         IF LAST-OF(FAKTTID.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.OBELOPP) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Övertidskostnad"            
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTID.AONR
               sumpers.DELNR = FAKTTID.DELNR               
               sumpers.OTIMMAR = FAKTTID.OPRIS 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.OTIMMAR)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.OBELOPP).                              
            END.
         END.
      END.
      ohj = ohj + 1.
      FOR EACH FAKTTID WHERE FAKTTID.MED = TRUE  AND FAKTTID.FAKTNR = fplanr AND FAKTTID.VFAKTNR = skarpvar BREAK 
      BY FAKTTID.RESPRIS BY FAKTTID.AONR BY FAKTTID.DELNR:         
         ACCUMULATE 
         FAKTTID.RESKOSTDEC (TOTAL BY FAKTTID.RESPRIS BY FAKTTID.AONR BY FAKTTID.DELNR).
         ACCUMULATE 
         FAKTTID.RESTIM (TOTAL BY FAKTTID.RESPRIS BY FAKTTID.AONR BY FAKTTID.DELNR).
         IF LAST-OF(FAKTTID.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.RESKOSTDEC) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Restidskostnad"            
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTID.AONR
               sumpers.DELNR = FAKTTID.DELNR               
               sumpers.OTIMMAR = FAKTTID.RESPRIS 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.RESTIM)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.RESKOSTDEC).                              
            END.
         END.
      END.
      ohj = ohj + 1.
      FOR EACH FAKTTID WHERE FAKTTID.MED = TRUE  AND FAKTTID.FAKTNR = fplanr AND FAKTTID.VFAKTNR = skarpvar BREAK 
      BY FAKTTID.TRAKTKOD BY FAKTTID.AONR BY FAKTTID.DELNR:         
         ACCUMULATE 
         FAKTTID.TBELOPP (TOTAL BY FAKTTID.TRAKTKOD BY FAKTTID.AONR BY FAKTTID.DELNR).
         ACCUMULATE 
         FAKTTID.TRAKTANTAL (TOTAL BY FAKTTID.TRAKTKOD BY FAKTTID.AONR BY FAKTTID.DELNR).
         IF LAST-OF(FAKTTID.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.TBELOPP) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Trakt.kostnad "            
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTID.AONR
               sumpers.DELNR = FAKTTID.DELNR                                  
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.TRAKTANTAL)
               sumpers.BELOPP =                
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.TBELOPP).
               IF sumpers.TIMMAR > 0 THEN 
               sumpers.OTIMMAR = sumpers.BELOPP / sumpers.TIMMAR.                  
            END.
         END.
      END.        
      ohj = ohj + 1.
      FOR EACH FAKTTID WHERE FAKTTID.MED = TRUE  AND FAKTTID.FAKTNR = fplanr AND FAKTTID.VFAKTNR = skarpvar BREAK 
      BY FAKTTID.LONTILLAGG BY FAKTTID.AONR BY FAKTTID.DELNR:         
         ACCUMULATE 
         FAKTTID.LONKOST (TOTAL BY FAKTTID.LONTILLAGG BY FAKTTID.AONR BY FAKTTID.DELNR).
         ACCUMULATE 
         FAKTTID.LONTILLANTAL (TOTAL BY FAKTTID.LONTILLAGG BY FAKTTID.AONR BY FAKTTID.DELNR).
         IF LAST-OF(FAKTTID.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.LONKOST) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Lart.kostnad "             
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTID.AONR
               sumpers.DELNR = FAKTTID.DELNR                                  
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.LONTILLANTAL)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY FAKTTID.DELNR FAKTTID.LONKOST).
               IF sumpers.TIMMAR > 0 THEN 
               sumpers.OTIMMAR = sumpers.BELOPP / sumpers.TIMMAR.                  
            END.
         END.
      END.
      
      ohj = ohj + 1.
      FOR EACH FAKTKOST WHERE FAKTKOST.MED = TRUE AND FAKTKOST.FAKTNR = fplanr AND FAKTKOST.VFAKTNR = skarpvar BREAK 
      BY FAKTKOST.AONR BY FAKTKOST.DELNR:         
         ACCUMULATE 
         FAKTKOST.MASKKOST (TOTAL BY FAKTKOST.AONR BY FAKTKOST.DELNR).
         ACCUMULATE 
         FAKTKOST.MTRL (TOTAL BY FAKTKOST.AONR BY FAKTKOST.DELNR). 
         ACCUMULATE 
         FAKTKOST.OVRKR (TOTAL BY FAKTKOST.AONR BY FAKTKOST.DELNR).
         ACCUMULATE 
         FAKTKOST.PERSKOST (TOTAL BY FAKTKOST.AONR BY FAKTKOST.DELNR).
         ACCUMULATE 
         FAKTKOST.TRAKTKOST (TOTAL BY FAKTKOST.AONR BY FAKTKOST.DELNR).         
         IF LAST-OF(FAKTKOST.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.PERSKOST) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Konterad personalkostnad."           
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTKOST.AONR
               sumpers.DELNR = FAKTKOST.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.PERSKOST).                 
            END.
            IF (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.TRAKTKOST) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Konterad trakt.kostnad."           
               sumpers.ORDNING = ohj + 1
               sumpers.AONR = FAKTKOST.AONR
               sumpers.DELNR = FAKTKOST.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.TRAKTKOST).                 
            END.
            IF (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.MASKKOST) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Främandetj."           
               sumpers.ORDNING = ohj + 2
               sumpers.AONR = FAKTKOST.AONR
               sumpers.DELNR = FAKTKOST.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.MASKKOST) +  (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.MASKKOST) * (FAKTKOST.FRTJPA / 100). 
                 
            END.
            IF (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.MTRL) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Materielkostnad."           
               sumpers.ORDNING = ohj + 3
               sumpers.AONR = FAKTKOST.AONR
               sumpers.DELNR = FAKTKOST.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.MTRL) + (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.MTRL) * (FAKTKOST.MTRLPA / 100). 
                
            END.
            IF (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.OVRKR) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Övrig kostnad."           
               sumpers.ORDNING = ohj + 4
               sumpers.AONR = FAKTKOST.AONR
               sumpers.DELNR = FAKTKOST.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOST.DELNR FAKTKOST.OVRKR).                   
            END.               
         END.
      END.
      /*FAKTFORE*/
      ohj = ohj + 5.
      IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
         FOR EACH FAKTFRIA WHERE FAKTFRIA.FAKTNR = fplanr AND FAKTFRIA.VFAKTNR = skarpvar NO-LOCK BREAK 
         BY FAKTFRIA.AONR BY FAKTFRIA.DELNR BY FAKTFRIA.FAKTTEXT:         
            ACCUMULATE 
            FAKTFRIA.TOTALT (TOTAL BY FAKTFRIA.AONR BY FAKTFRIA.DELNR BY FAKTFRIA.FAKTTEXT).            
            IF LAST-OF(FAKTFRIA.FAKTTEXT) THEN DO:
               IF (ACCUM TOTAL BY FAKTFRIA.FAKTTEXT FAKTFRIA.TOTALT) NE 0 THEN DO:              
                  CREATE sumpers.
                  ASSIGN                
                  sumpers.VIBEFATTNING = FAKTFRIA.FAKTTEXT           
                  sumpers.ORDNING = ohj
                  sumpers.AONR = FAKTFRIA.AONR
                  sumpers.DELNR = FAKTFRIA.DELNR                                  
                  sumpers.OTIMMAR = 0 
                  sumpers.TIMMAR = 1
                  sumpers.BELOPP = (ACCUM TOTAL BY FAKTFRIA.FAKTTEXT FAKTFRIA.TOTALT).                  
               END.                              
            END.
         END.
      END.
    
 END PROCEDURE.
 PROCEDURE summasundkred_UI :
    DEFINE VARIABLE ohj AS INTEGER NO-UNDO.
    ohj = ohj + 1.
    FOR EACH FAKTTIDKRED WHERE FAKTTIDKRED.MED = TRUE AND FAKTTIDKRED.FAKTNR = fplanr AND FAKTTIDKRED.VKREDIT = skarpvar BREAK 
      BY FAKTTIDKRED.PRISA BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR:         
         ACCUMULATE 
         FAKTTIDKRED.BELOPP (TOTAL BY FAKTTIDKRED.PRISA BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         ACCUMULATE 
         FAKTTIDKRED.TIMMAR (TOTAL BY FAKTTIDKRED.PRISA BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         IF LAST-OF(FAKTTIDKRED.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.TIMMAR) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Arbetskostnad"            
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTIDKRED.AONR
               sumpers.DELNR = FAKTTIDKRED.DELNR               
               sumpers.OTIMMAR = FAKTTIDKRED.PRISA 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.TIMMAR)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.BELOPP).                              
            END.
         END.
      END.
      ohj = ohj + 1.
      FOR EACH FAKTTIDKRED WHERE FAKTTIDKRED.MED = TRUE  AND FAKTTIDKRED.FAKTNR = fplanr AND FAKTTIDKRED.VKREDIT = skarpvar BREAK 
      BY FAKTTIDKRED.OPRIS BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR:         
         ACCUMULATE 
         FAKTTIDKRED.OBELOPP (TOTAL BY FAKTTIDKRED.OPRIS BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         ACCUMULATE 
         FAKTTIDKRED.OTIMMAR (TOTAL BY FAKTTIDKRED.OPRIS BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         IF LAST-OF(FAKTTIDKRED.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.OBELOPP) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Övertidskostnad"            
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTIDKRED.AONR
               sumpers.DELNR = FAKTTIDKRED.DELNR               
               sumpers.OTIMMAR = FAKTTIDKRED.OPRIS 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.OTIMMAR)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.OBELOPP).                              
            END.
         END.
      END.
      ohj = ohj + 1.
      FOR EACH FAKTTIDKRED WHERE FAKTTIDKRED.MED = TRUE  AND FAKTTIDKRED.FAKTNR = fplanr AND FAKTTIDKRED.VKREDIT = skarpvar BREAK 
      BY FAKTTIDKRED.RESPRIS BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR:         
         ACCUMULATE 
         FAKTTIDKRED.RESKOSTDEC (TOTAL BY FAKTTIDKRED.RESPRIS BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         ACCUMULATE 
         FAKTTIDKRED.RESTIM (TOTAL BY FAKTTIDKRED.RESPRIS BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         IF LAST-OF(FAKTTIDKRED.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.RESKOSTDEC) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Restidskostnad"            
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTIDKRED.AONR
               sumpers.DELNR = FAKTTIDKRED.DELNR               
               sumpers.OTIMMAR = FAKTTIDKRED.RESPRIS 
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.RESTIM)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.RESKOSTDEC).                              
            END.
         END.
      END.
      ohj = ohj + 1.
      FOR EACH FAKTTIDKRED WHERE FAKTTIDKRED.MED = TRUE  AND FAKTTIDKRED.FAKTNR = fplanr AND FAKTTIDKRED.VKREDIT = skarpvar BREAK 
      BY FAKTTIDKRED.TRAKTKOD BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR:         
         ACCUMULATE 
         FAKTTIDKRED.TBELOPP (TOTAL BY FAKTTIDKRED.TRAKTKOD BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         ACCUMULATE 
         FAKTTIDKRED.TRAKTANTAL (TOTAL BY FAKTTIDKRED.TRAKTKOD BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         IF LAST-OF(FAKTTIDKRED.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.TBELOPP) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Trakt.kostnad "            
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTIDKRED.AONR
               sumpers.DELNR = FAKTTIDKRED.DELNR                                  
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.TRAKTANTAL)
               sumpers.BELOPP =                
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.TBELOPP).
               IF sumpers.TIMMAR > 0 THEN 
               sumpers.OTIMMAR = sumpers.BELOPP / sumpers.TIMMAR.                  
            END.
         END.
      END.        
      ohj = ohj + 1.
      FOR EACH FAKTTIDKRED WHERE FAKTTIDKRED.MED = TRUE  AND FAKTTIDKRED.FAKTNR = fplanr AND FAKTTIDKRED.VKREDIT = skarpvar BREAK 
      BY FAKTTIDKRED.LONTILLAGG BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR:         
         ACCUMULATE 
         FAKTTIDKRED.LONKOST (TOTAL BY FAKTTIDKRED.LONTILLAGG BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         ACCUMULATE 
         FAKTTIDKRED.LONTILLANTAL (TOTAL BY FAKTTIDKRED.LONTILLAGG BY FAKTTIDKRED.AONR BY FAKTTIDKRED.DELNR).
         IF LAST-OF(FAKTTIDKRED.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.LONKOST) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Lart.kostnad "             
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTTIDKRED.AONR
               sumpers.DELNR = FAKTTIDKRED.DELNR                                  
               sumpers.TIMMAR = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.LONTILLANTAL)
               sumpers.BELOPP = 
               (ACCUM TOTAL BY FAKTTIDKRED.DELNR FAKTTIDKRED.LONKOST).
               IF sumpers.TIMMAR > 0 THEN 
               sumpers.OTIMMAR = sumpers.BELOPP / sumpers.TIMMAR.                  
            END.
         END.
      END.
      
      ohj = ohj + 1.
      FOR EACH FAKTKOSTKRED WHERE FAKTKOSTKRED.MED = TRUE AND FAKTKOSTKRED.FAKTNR = fplanr AND FAKTKOSTKRED.VKREDIT = skarpvar BREAK 
      BY FAKTKOSTKRED.AONR BY FAKTKOSTKRED.DELNR:         
         ACCUMULATE 
         FAKTKOSTKRED.MASKKOST (TOTAL BY FAKTKOSTKRED.AONR BY FAKTKOSTKRED.DELNR).
         ACCUMULATE 
         FAKTKOSTKRED.MTRL (TOTAL BY FAKTKOSTKRED.AONR BY FAKTKOSTKRED.DELNR). 
         ACCUMULATE 
         FAKTKOSTKRED.OVRKR (TOTAL BY FAKTKOSTKRED.AONR BY FAKTKOSTKRED.DELNR).
         ACCUMULATE 
         FAKTKOSTKRED.PERSKOST (TOTAL BY FAKTKOSTKRED.AONR BY FAKTKOSTKRED.DELNR).
         ACCUMULATE 
         FAKTKOSTKRED.TRAKTKOST (TOTAL BY FAKTKOSTKRED.AONR BY FAKTKOSTKRED.DELNR).         
         IF LAST-OF(FAKTKOSTKRED.DELNR) THEN DO:
            IF (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.PERSKOST) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Konterad personalkostnad."           
               sumpers.ORDNING = ohj
               sumpers.AONR = FAKTKOSTKRED.AONR
               sumpers.DELNR = FAKTKOSTKRED.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.PERSKOST).                 
            END.
            IF (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.TRAKTKOST) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Konterad trakt.kostnad."           
               sumpers.ORDNING = ohj + 1
               sumpers.AONR = FAKTKOSTKRED.AONR
               sumpers.DELNR = FAKTKOSTKRED.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.TRAKTKOST).                 
            END.
            IF (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.MASKKOST) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Främandetj."           
               sumpers.ORDNING = ohj + 2
               sumpers.AONR = FAKTKOSTKRED.AONR
               sumpers.DELNR = FAKTKOSTKRED.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.MASKKOST) +  (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.MASKKOST) * (FAKTKOSTKRED.FRTJPA / 100). 
                 
            END.
            IF (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.MTRL) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Materielkostnad."           
               sumpers.ORDNING = ohj + 3
               sumpers.AONR = FAKTKOSTKRED.AONR
               sumpers.DELNR = FAKTKOSTKRED.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.MTRL) + (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.MTRL) * (FAKTKOSTKRED.MTRLPA / 100). 
                
            END.
            IF (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.OVRKR) NE 0 THEN DO:
               CREATE sumpers.
               ASSIGN                
               sumpers.VIBEFATTNING = "Övrig kostnad."           
               sumpers.ORDNING = ohj + 4
               sumpers.AONR = FAKTKOSTKRED.AONR
               sumpers.DELNR = FAKTKOSTKRED.DELNR                                  
               sumpers.OTIMMAR = 0 
               sumpers.TIMMAR = 1
               sumpers.BELOPP = (ACCUM TOTAL BY FAKTKOSTKRED.DELNR FAKTKOSTKRED.OVRKR).                   
            END.               
         END.
      END.
      /*FAKTFORE*/
      ohj = ohj + 5.
      IF Guru.Konstanter:varforetypval[3] >= 1 THEN DO:
         FOR EACH FAKTFRIAKRED WHERE FAKTFRIAKRED.FAKTNR = fplanr AND FAKTFRIAKRED.VKREDIT = skarpvar NO-LOCK BREAK 
         BY FAKTFRIAKRED.AONR BY FAKTFRIAKRED.DELNR BY FAKTFRIAKRED.FAKTTEXT:         
            ACCUMULATE 
            FAKTFRIAKRED.TOTALT (TOTAL BY FAKTFRIAKRED.AONR BY FAKTFRIAKRED.DELNR BY FAKTFRIAKRED.FAKTTEXT).            
            IF LAST-OF(FAKTFRIAKRED.FAKTTEXT) THEN DO:
               IF (ACCUM TOTAL BY FAKTFRIAKRED.FAKTTEXT FAKTFRIAKRED.TOTALT) NE 0 THEN DO:              
                  CREATE sumpers.
                  ASSIGN                
                  sumpers.VIBEFATTNING = FAKTFRIAKRED.FAKTTEXT           
                  sumpers.ORDNING = ohj
                  sumpers.AONR = FAKTFRIAKRED.AONR
                  sumpers.DELNR = FAKTFRIAKRED.DELNR                                  
                  sumpers.OTIMMAR = 0 
                  sumpers.TIMMAR = 1
                  sumpers.BELOPP = (ACCUM TOTAL BY FAKTFRIAKRED.FAKTTEXT FAKTFRIAKRED.TOTALT).                  
               END.                              
            END.
         END.
      END.
    
 END PROCEDURE.
 
