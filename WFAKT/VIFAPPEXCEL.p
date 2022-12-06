/*Robin Sj�berg Elpool i Ume� AB  16 jan 2014 15:32:23 
   DETTA PROGRAM ANV�NDS ENBART AV ELPOOL OCH �R GJORT UTIFR�N EN KOPIA AV VIFAAPP.P.
   DETTA PROGRAM INNEH�LLER EN HEL DEL SKR�PKOD.
*/


/*VIFAPPEXCEL.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

FIND FIRST FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
RUN STYRFORE.P (INPUT Guru.Konstanter:globforetag).
&Scoped-define NEW NEW                          
{FAKTTEMP.I}
{FAKTTYPDEF.I}
&Scoped-define NEW 
{FAKTTYPSKAP.I}
{FAKTBILAG.I}
DEFINE TEMP-TABLE vilkaaonr
   FIELD AONR LIKE FAKTAONR.AONR
   FIELD DELNR LIKE FAKTAONR.DELNR
   INDEX AONR AONR DELNR.
{TIDUTTTNEW.I}

DEFINE INPUT PARAMETER infaktnr LIKE FAKTPLAN.FAKTNR NO-UNDO.
DEFINE INPUT PARAMETER vfknr LIKE FAKTURERAD.VFAKTNR NO-UNDO.
DEFINE INPUT PARAMETER skrivutalla AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER direkt AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER slutfaktvar AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR fakbilag.
DEFINE INPUT PARAMETER TABLE FOR sumtidtemp.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE betvar AS DECIMAL NO-UNDO.
DEFINE VARIABLE fnrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE vartyp AS INTEGER NO-UNDO.
DEFINE VARIABLE varcolon AS INTEGER NO-UNDO.
DEFINE VARIABLE str AS CHARACTER FORMAT "X(80)" NO-UNDO.   
DEFINE VARIABLE momsumma AS DECIMAL NO-UNDO.
DEFINE VARIABLE hbelopp AS DECIMAL NO-UNDO.
DEFINE VARIABLE dbelopp AS DECIMAL NO-UNDO.
DEFINE VARIABLE momshlpvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE varfakturd AS DATE NO-UNDO.
DEFINE VARIABLE varforfalld AS DATE NO-UNDO.
DEFINE VARIABLE skarpnr AS CHARACTER NO-UNDO.


{ANMARKD.I}
{ExcelDS.i} /*RS*/
DEFINE OUTPUT PARAMETER TABLE FOR fakturaexcelTT.
DEFINE OUTPUT PARAMETER TABLE FOR faktposterexcelTT.
FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.
Guru.Konstanter:globforetag = FORETAG.FORETAG.
FIND FIRST FAKTPLAN WHERE FAKTPLAN.FAKTNR = infaktnr NO-LOCK NO-ERROR.

IF skrivutalla = TRUE THEN DO:
   FIND FIRST FAKTURERAD WHERE FAKTURERAD.FAKTNR = FAKTPLAN.FAKTNR
   USE-INDEX FAKTNR NO-LOCK NO-ERROR.            
END.    
ELSE DO:
   IF direkt = TRUE THEN DO:
      IF vfknr = 0 THEN DO:
         FIND FIRST FAKTURERAD WHERE FAKTURERAD.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTURERAD.FDELNR = FAKTPLAN.FDELNR NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST FAKTURERAD WHERE FAKTURERAD.FAKTNR = FAKTPLAN.FAKTNR AND 
         FAKTURERAD.VFAKTNR = vfknr NO-LOCK NO-ERROR.
      END.
   END.
   ELSE DO:
      
      IF vfknr NE 0 THEN DO:
         FIND FIRST FAKTURERAD WHERE FAKTURERAD.FAKTNR = FAKTPLAN.FAKTNR AND
         FAKTURERAD.VFAKTNR = vfknr NO-LOCK NO-ERROR.
      END.
      ELSE DO:
         FIND FIRST FAKTURERAD WHERE FAKTURERAD.FAKTNR = FAKTPLAN.FAKTNR AND
         FAKTURERAD.FDELNR = FAKTPLAN.FDELNR NO-LOCK NO-ERROR.
         
      END.    
      RUN tidhamt_UI.
   END.      
END.
FIND faktyptemp WHERE faktyptemp.FAKTTYP = FAKTPLAN.FAKTTYP NO-ERROR.
vartyp = faktyptemp.TYP. 
IF vartyp = 5 THEN DO:
   IF FAKTPLAN.FAKTTYPUNDER = 2 OR FAKTPLAN.FAKTTYPUNDER = 4 THEN vartyp = 52.
END. 
FIND FIRST FAKTURERINGSTYP WHERE FAKTURERINGSTYP.FAKTTYPID = FAKTURERAD.FAKTTYPID NO-LOCK NO-ERROR.
IF FAKTURERINGSTYP.TIDIGAREACONTO = TRUE THEN vartyp = 3.
/*FAKTFOR*/
IF Guru.Konstanter:globforetag = "ESAN" or Guru.Konstanter:globforetag = "cELPA" THEN DO:
   RUN huvudesab_UI.
   RUN summaesab_UI.
   RUN slutesab_UI.
   FIND FIRST fakbilag NO-ERROR.   
   IF AVAILABLE fakbilag THEN RUN bilaga_UI.
END.  
ELSE IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" or Guru.Konstanter:globforetag = "ELPA" THEN DO: 
   RUN huvud_UI.
   RUN summagran_UI.
   RUN kund_UI. /*rs*/
   RUN moms_UI. /*rs*/
END.
ELSE DO:
   RUN huvud_UI.
   RUN summa_UI.
END.   
/* **********************  Internal Procedures  *********************** */
/*RS PROC*/
PROCEDURE kund_UI:     
   FIND FIRST FAKTREGLER WHERE FAKTREGLER.FAKTNR = FAKTPLAN.FAKTNR 
   NO-LOCK NO-ERROR.
   IF FAKTURERAD.VFAKTNR = 0 THEN DO:
      ASSIGN
      varfakturd = DATE(INTEGER(SUBSTRING(STRING(FAKTURERAD.FDELNR,"999999"),3,2)),
                        INTEGER(SUBSTRING(STRING(FAKTURERAD.FDELNR,"999999"),5,2)),
                        INTEGER("20" + SUBSTRING(STRING(FAKTURERAD.FDELNR,"999999"),1,2))).          
      varforfalld = TODAY + FAKTREGLER.FDAGAR.
      REPEAT:
         IF WEEKDAY(varforfalld) = 1 THEN varforfalld = varforfalld + 1.
         IF WEEKDAY(varforfalld) = 7 THEN varforfalld = varforfalld + 2.
         FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = varforfalld NO-LOCK NO-ERROR.
         IF NOT AVAILABLE OVERAVTAB THEN DO:
            LEAVE.
         END.
         ELSE IF OVERAVTAB.EQDAG = 1 THEN varforfalld = varforfalld + 1.
         ELSE IF OVERAVTAB.EQDAG = 7 THEN varforfalld = varforfalld + 1.
      END.               
   END.
   ELSE DO:      
      FIND FIRST FAKTKUNDKONTO WHERE FAKTKUNDKONTO.FAKTNR = FAKTPLAN.FAKTNR AND        
      FAKTKUNDKONTO.VFAKTNR = vfknr NO-LOCK NO-ERROR.
      IF AVAILABLE FAKTKUNDKONTO THEN DO:
         ASSIGN
         varfakturd = FAKTKUNDKONTO.FAKTDATUM
         varforfalld = FAKTKUNDKONTO.FDATUM.            
      END.
   END.
   
   IF FAKTURERAD.VFAKTNR = 0 THEN DO:   
      skarpnr = STRING(FAKTPLAN.FAKTNR) + ' ' + STRING(FAKTURERAD.FDELNR).
   END.
   ELSE DO:
      FIND FIRST FAKTSKARP WHERE FAKTSKARP.OMRADE = FAKTPLAN.OMRADE 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FAKTSKARP THEN DO:
         FIND FIRST FAKTSKARP NO-LOCK NO-ERROR. 
      END.
      IF YEAR(FAKTURERAD.BOKDATUM) = FAKTSKARP.ARTAL THEN 
      skarpnr = STRING(FAKTSKARP.ARTAL) + FAKTSKARP.ARKIVSTALLE + STRING(FAKTURERAD.VFAKTNR).      
      ELSE skarpnr = STRING(YEAR(FAKTURERAD.BOKDATUM)) + FAKTSKARP.ARKIVSTALLE + STRING(FAKTURERAD.VFAKTNR).      
   END.

   fakturaexcelTT.FAKTDATUM = STRING(varfakturd,"99-99-9999").
   fakturaexcelTT.EFAKTDATUM = varfakturd.
   fakturaexcelTT.FORFALLDATUM = STRING(varforfalld,"99-99-9999").
   fakturaexcelTT.EFORFALLDATUM = varforfalld.
   
END PROCEDURE.
/*RS PROC2*/
PROCEDURE moms_UI:
   momsumma = 0.
   FOR EACH FAKTMOMS WHERE FAKTMOMS.FAKTNR = FAKTPLAN.FAKTNR AND 
   FAKTMOMS.FDELNR = FAKTURERAD.FDELNR NO-LOCK BREAK BY FAKTMOMS.MOMSID: 
      ACCUMULATE FAKTMOMS.BELOPP (TOTAL BY FAKTMOMS.MOMSID).
      ACCUMULATE FAKTMOMS.MOMSBELOPP (TOTAL BY FAKTMOMS.MOMSID).
      IF LAST-OF(FAKTMOMS.MOMSID) THEN DO:                        
         IF (ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP) NE 0 THEN DO:
            CREATE tidut.
            momsumma = momsumma + ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP.
         END.
      END.                                      
   END.    
   fakturaexcelTT.MOMS = STRING(momsumma + FAKTURERAD.ORESUTJ,"->>>>>>>>9.99").
END PROCEDURE.
   
PROCEDURE anmark_UI :
   {muswait.i}
   DEFINE INPUT PARAMETER anmark AS INTEGER NO-UNDO.
   IF anmark = 1 THEN DO:                  
      CREATE tidut.
      ASSIGN            
      SUBSTRING(tidut.UT,21) = ":"   
      SUBSTRING(tidut.UT,23) = SUBSTRING(edtext,ednum,edtecken).
   END.  
   ELSE IF anmark = 2 THEN DO:           
      CREATE tidut.
      ASSIGN            
      SUBSTRING(tidut.UT,21) = ":"   
      SUBSTRING(tidut.UT,23) = tidtext.
   END.   
   ELSE IF anmark = 3 THEN DO:           
      CREATE tidut.
      ASSIGN           
      SUBSTRING(tidut.UT,21) = ":"    
      SUBSTRING(tidut.UT,23) = SUBSTRING(edtext,1 + ednum2 * edtecken,edtecken).
   END.                         
END PROCEDURE.

PROCEDURE bilaga_UI :
   {muswait.i}
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "BILAGA".   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      OPEN QUERY finq FOR EACH FAKTINTAKTKONT WHERE 
      FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR AND               
      FAKTINTAKTKONT.FDELNR = FAKTURERAD.FDELNR 
      NO-LOCK.                                        
      GET FIRST finq NO-LOCK.      
      IF AVAILABLE FAKTINTAKTKONT THEN DO:
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = "Ing�ende " + LC(Guru.Konstanter:gaok) + " :".      
      END.
      DO WHILE AVAILABLE(FAKTINTAKTKONT):         
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTINTAKTKONT.AONR AND 
         AONRTAB.DELNR = FAKTINTAKTKONT.DELNR
         NO-LOCK NO-ERROR. 
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = AONRTAB.AONR
         SUBSTRING(tidut.UT,8) = STRING(AONRTAB.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,12) = AONRTAB.ORT.
         GET NEXT finq NO-LOCK.
      END.
   END.
   ELSE DO:
      OPEN QUERY faktaonrq FOR EACH FAKTAONR WHERE FAKTAONR.FAKTNR = FAKTPLAN.FAKTNR NO-LOCK.
      GET FIRST faktaonrq NO-LOCK.
      IF AVAILABLE FAKTAONR THEN DO:
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = "Ing�ende " + LC(Guru.Konstanter:gaok) + " :".      
      END.
      DO WHILE AVAILABLE(FAKTAONR):
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTAONR.AONR AND 
         AONRTAB.DELNR = FAKTAONR.DELNR
         NO-LOCK NO-ERROR. 
         CREATE tidut.
         ASSIGN
         SUBSTRING(tidut.UT,1) = AONRTAB.AONR
         SUBSTRING(tidut.UT,8) = STRING(AONRTAB.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,12) = AONRTAB.ORT.
         GET NEXT faktaonrq NO-LOCK.
      END.
   END.
   
   IF fakbilag.TIDEJMED = TRUE OR fakbilag.TIDMED = TRUE OR fakbilag.TIDTOT THEN DO:    
      RUN tid_UI.          
   END.    
   IF fakbilag.KOST = TRUE THEN DO:
      OPEN QUERY faktkostq FOR EACH FAKTKOST WHERE FAKTKOST.FAKTNR = FAKTURERAD.FAKTNR AND          
      FAKTKOST.FDELNR = FAKTURERAD.FDELNR AND FAKTKOST.MED = TRUE 
      USE-INDEX FAKTKOST NO-LOCK.
      GET FIRST faktkostq NO-LOCK. 
      IF AVAILABLE FAKTKOST THEN DO:   
         CREATE tidut.
         CREATE tidut.
         CREATE tidut.            
         ASSIGN          
         tidut.UT = "Ing�ende externa fakturor :".
         CREATE tidut.
         CREATE tidut. 
         CREATE tidut. 
         SUBSTRING(tidut.UT,48) = "TOTAL".
         CREATE tidut.
         ASSIGN                        
         SUBSTRING(tidut.UT,1) = "VER-NR" 
         SUBSTRING(tidut.UT,17) = CAPS(Guru.Konstanter:gaok)  
         SUBSTRING(tidut.UT,28) = "BEN�MNING"
         SUBSTRING(tidut.UT,48) = "KOSTNAD".  
         CREATE tidut.
         ASSIGN                        
         tidut.UT = "===============.==========.===================.=========".
      END.
      DO WHILE AVAILABLE(FAKTKOST):
         CREATE tidut.
         ASSIGN                       
         SUBSTRING(tidut.UT,17) = FAKTKOST.AONR 
         SUBSTRING(tidut.UT,24) = STRING(FAKTKOST.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,1) = FAKTKOST.VERNR
         SUBSTRING(tidut.UT,28) = STRING(FAKTKOST.BENAMNING,"X(19)")
         SUBSTRING(tidut.UT,48) = STRING(FAKTKOST.MASKKOST + FAKTKOST.MTRL + 
         FAKTKOST.OVRKR + FAKTKOST.PERSKOST + FAKTKOST.TRAKTKOST + 
         (FAKTKOST.MASKKOST * FAKTKOST.FRTJPA / 100) +
         (FAKTKOST.MTRL * FAKTKOST.MTRLPA / 100),"->>>>>>>9").
         /*
         FIND FIRST KOSTREG WHERE KOSTREG.AONR = FAKTKOST.AONR AND 
         KOSTREG.DELNR = FAKTKOST.DELNR AND KOSTREG.RADNR = FAKTKOST.RADNR
         USE-INDEX KOST NO-LOCK NO-ERROR.                                 
         IF AVAILABLE KOSTREG THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = KOSTREG.FAKTNR
            SUBSTRING(tidut.UT,28) = STRING(KOSTREG.BENAMNING,"X(19)")
            SUBSTRING(tidut.UT,48) = STRING(KOSTREG.MASKKOST + KOSTREG.MTRL + 
            KOSTREG.OVRKR + KOSTREG.PERSKOST + KOSTREG.TRAKTKOST + 
            (KOSTREG.MASKKOST * FAKTKOST.FRTJPA / 100) +
            (KOSTREG.MTRL * FAKTKOST.MTRLPA / 100),"->>>>>>>9").
         END.
         */
         GET NEXT faktkostq NO-LOCK.   
      END.
   END.   
   IF fakbilag.FRI = TRUE THEN DO: 
      CREATE tidut. 
      OPEN QUERY faktfriq FOR EACH FAKTFRIA WHERE FAKTFRIA.FAKTNR = FAKTURERAD.FAKTNR AND          
      FAKTFRIA.FDELNR = FAKTURERAD.FDELNR USE-INDEX FAKTFRIA NO-LOCK.  
      GET FIRST faktfriq NO-LOCK.
      IF AVAILABLE FAKTFRIA THEN DO:   
         CREATE tidut.
         CREATE tidut.
         CREATE tidut.            
         ASSIGN          
         tidut.UT = "Fri komplettering :".
         CREATE tidut.         
         CREATE tidut.         
         ASSIGN    
         SUBSTRING(tidut.UT,69) = "PRIS/". 
         CREATE tidut.
         ASSIGN                        
         SUBSTRING(tidut.UT,1) = "TEXT" 
         SUBSTRING(tidut.UT,52) = "ENHET"           
         SUBSTRING(tidut.UT,59) = "ANTAL"  
         SUBSTRING(tidut.UT,69) = "ENHET"
         SUBSTRING(tidut.UT,79) = "TOTALT".  
         CREATE tidut.
         ASSIGN                        
         tidut.UT =                                 
"==================================================.======.=========.=========.=========".         
         DO WHILE AVAILABLE(FAKTFRIA):  
            IF FAKTFRIA.FAKTTEXT = "DUBBEL-KLICKA P� DENNA RAD F�R NYUPPL�GG" THEN musz = musz.
            ELSE DO:
               /*RS*/ 
               CREATE faktposterexcelTT.
               faktposterexcelTT.BESKIVNING = STRING(FAKTFRIA.FAKTTEXT).
               faktposterexcelTT.ANTAL = STRING(FAKTFRIA.ANTAL).
               faktposterexcelTT.PRIS = STRING(FAKTFRIA.PRIS_ENHET).
               faktposterexcelTT.SUMMA = STRING(FAKTFRIA.TOTALT).

               CREATE tidut.             
               ASSIGN              
               SUBSTRING(tidut.UT,1) = FAKTFRIA.AONR + " " + STRING(FAKTFRIA.DELNR,Guru.Konstanter:varforetypchar[1]) + " " + STRING(FAKTFRIA.FAKTTEXT,"X(38)") 
               SUBSTRING(tidut.UT,52) = STRING(FAKTFRIA.ENHET,"X(6)")
               SUBSTRING(tidut.UT,59) = STRING(FAKTFRIA.ANTAL,"->>>>>>>9")
               SUBSTRING(tidut.UT,69) = STRING(FAKTFRIA.PRIS_ENHET,"->>>>>>>9")
               SUBSTRING(tidut.UT,79) = STRING(FAKTFRIA.TOTALT,"->>>>>>>9").                     
            END.
            GET NEXT faktfriq NO-LOCK.   
         END.         
      END.   
   END.    
   
END PROCEDURE.

PROCEDURE fakbi1_UI :
   
   {muswait.i}
   
   {FAKBIL.I}
                                                 
   RUN fakbi2_UI.                           
   
END PROCEDURE.

PROCEDURE fakbi2_UI :
   {muswait.i}
   FIND FIRST sumpers NO-ERROR.
   IF AVAILABLE sumpers THEN DO:
      CREATE tidut.            
      CREATE tidut.
      IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Alla l�netill�gg och traktamenten :". 
      END. 
      ELSE IF fakbilag.TIDMED = TRUE THEN DO:  
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Ing�ende l�netill�gg och traktamenten :". 
      END.
      ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:  
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Ej ing�ende l�netill�gg och traktamenten :".
      END.
      CREATE tidut.
      CREATE tidut.
      ASSIGN                                              
      SUBSTRING(tidut.UT,58) = "TRAKT."                           
      SUBSTRING(tidut.UT,89) = "L-TILL.". 
      IF fakbilag.PRIS = FALSE THEN DO:  
         ASSIGN
         SUBSTRING(tidut.UT,58,7) = "       "          
         SUBSTRING(tidut.UT,89,7) = "       ".                                     
      END.  
      IF fakbilag.LON = FALSE THEN DO:  
         ASSIGN
         SUBSTRING(tidut.UT,65,40) = "".                                               
      END.                  
      CREATE tidut.
      ASSIGN                    
      SUBSTRING(tidut.UT,1) = "BEFATTNING" 
      SUBSTRING(tidut.UT,25) = CAPS(Guru.Konstanter:gaok)        
      SUBSTRING(tidut.UT,36) = "TRAKTAMENTE"    
      SUBSTRING(tidut.UT,54) = "ST"       
      SUBSTRING(tidut.UT,58) = "KOST."  
      SUBSTRING(tidut.UT,65) = "L�NETILL�GG"              
      SUBSTRING(tidut.UT,83) = "ST"        
      SUBSTRING(tidut.UT,89) = "KOSTNAD".                                    
      IF fakbilag.PRIS = FALSE THEN DO:  
         ASSIGN                      
         SUBSTRING(tidut.UT,58,7) = "       "          
         SUBSTRING(tidut.UT,89,7) = "       ".                                    
      END.  
      IF fakbilag.LON = FALSE THEN DO:  
         ASSIGN
         SUBSTRING(tidut.UT,65,40) = "".                                               
      END.                                                  
      IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:
         SUBSTRING(tidut.UT,97) = "MED".           
         CREATE tidut.                                       
         ASSIGN 
         tidut.UT =       
"=======================.==========.=================.===.======.=================.=====.=======.===".        
      END.    
      ELSE IF fakbilag.TIDMED = TRUE THEN DO:           
         CREATE tidut.                                       
         ASSIGN 
         tidut.UT =                          
"=======================.==========.=================.===.======.=================.=====.=======".                
      END.  
      ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:           
         CREATE tidut.                                       
         ASSIGN 
         tidut.UT =                                                       
"=======================.==========.=================.===.======.=================.=====.=======".                                                
      END.  
      IF fakbilag.PRIS = FALSE THEN DO:  
         ASSIGN
         SUBSTRING(tidut.UT,57,8) = "        "          
         SUBSTRING(tidut.UT,88,9) = "         ".                                    
      END.  
      IF fakbilag.LON = FALSE THEN DO:  
         ASSIGN
         SUBSTRING(tidut.UT,64,40) = "".                                               
      END. 
      FOR EACH sumpers:      
         CREATE tidut.                                       
         ASSIGN
         SUBSTRING(tidut.UT,1) = STRING(sumpers.VIBEFATTNING,"X(23)") 
         SUBSTRING(tidut.UT,25) = sumpers.AONR 
         SUBSTRING(tidut.UT,32) = STRING(sumpers.DELNR,Guru.Konstanter:varforetypchar[1]).            
         IF sumpers.TRAKTANTAL NE 0 THEN DO: 
            ASSIGN
            SUBSTRING(tidut.UT,54) = STRING(sumpers.TRAKTANTAL,">>9")       
            SUBSTRING(tidut.UT,58) = STRING(sumpers.TBELOPP,">>>>>9").                
         END.
         IF sumpers.LONTILLANTAL NE 0 THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,83) = STRING(sumpers.LONTILLANTAL,"->>>9")        
            SUBSTRING(tidut.UT,89) = STRING(sumpers.LONKOST,"->>>>>9").            
         END.         
         IF sumpers.LONTILLAGG NE "" THEN DO: 
            FIND FIRST LONTILL WHERE LONTILL.LONTILLAGG = sumpers.LONTILLAGG
            USE-INDEX LONTIL NO-LOCK NO-ERROR.                                       
            IF AVAILABLE LONTILL THEN DO:
               SUBSTRING(tidut.UT,65) = STRING(LONTILL.LONKODTEXT,"X(17)").
            END.
         END. 
         IF sumpers.TRAKTKOD NE "" THEN DO: 
            FIND FIRST TRAKTATAB WHERE TRAKTATAB.TRAKTKOD = sumpers.TRAKTKOD 
            USE-INDEX TRAKTKOD NO-LOCK NO-ERROR.                                       
            IF AVAILABLE TRAKTATAB THEN DO:
               SUBSTRING(tidut.UT,36) = STRING(TRAKTATAB.FORKL,"X(17)").
            END.
         END.     
         IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
            SUBSTRING(tidut.UT,97) = STRING(sumpers.MED,"Ja/Nej").  
         END.       
         IF fakbilag.PRIS = FALSE THEN DO:  
            ASSIGN
            SUBSTRING(tidut.UT,58,7) = "       "          
            SUBSTRING(tidut.UT,89,7) = "       ".                                    
         END.  
         IF fakbilag.LON = FALSE THEN DO:  
            ASSIGN
            SUBSTRING(tidut.UT,65,40) = "".                                               
         END.                                   
      END.      
   END.
   IF fakbilag.TIDKLOCK = TRUE THEN DO:      
      CREATE tidut.
      CREATE tidut.
      tidut.UT = "Detaljerad tidskrivning :".
      CREATE tidut.      
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "BEFATTNING"         
      SUBSTRING(tidut.UT,25) = CAPS(Guru.Konstanter:gaok)                    
      SUBSTRING(tidut.UT,36) = "DATUM" 
      SUBSTRING(tidut.UT,45) = "START" 
      SUBSTRING(tidut.UT,51) = "SLUT"
      SUBSTRING(tidut.UT,57) = "TIMMAR"
      SUBSTRING(tidut.UT,64) = "PRIS"
      SUBSTRING(tidut.UT,74) = "RESTID". 
      CREATE tidut.                                       
      ASSIGN 
      tidut.UT =                               
"=======================.==========.========.=====.=====.======.=========.=========".       
      IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp USE-INDEX BEF.                 
      END.
      ELSE IF fakbilag.TIDMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp WHERE sumtidtemp.MED USE-INDEX BEF.      
      END.
      ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp WHERE sumtidtemp.MED = FALSE USE-INDEX BEF.      
      END.
      GET FIRST suq NO-LOCK.
      DO WHILE AVAILABLE(sumtidtemp):                  
         IF sumtidtemp.START = sumtidtemp.SLUT THEN musz = musz.
         ELSE DO:
            CREATE tidut.         
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.VIBEFATTNING,"X(23)") 
            SUBSTRING(tidut.UT,25) = sumtidtemp.AONR 
            SUBSTRING(tidut.UT,32) = STRING(sumtidtemp.DELNR,Guru.Konstanter:varforetypchar[1]) 
            SUBSTRING(tidut.UT,36) = STRING(sumtidtemp.DATUM) 
            SUBSTRING(tidut.UT,45) = STRING(sumtidtemp.START,"99.99") 
            SUBSTRING(tidut.UT,51) = STRING(sumtidtemp.SLUT,"99.99")
            SUBSTRING(tidut.UT,64) = STRING(sumtidtemp.PRISA,">>>>>>>>9").
            IF sumtidtemp.TIMMAR NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,57) = STRING(sumtidtemp.TIMMAR,">>9.99") 
               SUBSTRING(tidut.UT,64) = STRING(sumtidtemp.BELOPP / sumtidtemp.TIMMAR,">>>>>>>>9").
            END.
            ELSE IF sumtidtemp.OTIMMAR NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,57) = STRING(sumtidtemp.OTIMMAR,">>9.99")
               SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.VIOBEFATTNING,"X(23)") 
               SUBSTRING(tidut.UT,64) = STRING(sumtidtemp.OBELOPP / sumtidtemp.OTIMMAR,">>>>>>>>9").
            END.
            ELSE IF sumtidtemp.RESTIM NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,57) = STRING(sumtidtemp.RESTIM,">>9.99")
               SUBSTRING(tidut.UT,64) = STRING(sumtidtemp.RESKOSTDEC / sumtidtemp.RESTIM,">>>>>>>>9")
               SUBSTRING(tidut.UT,74) = "JA".                                               
            END.
         END.
         GET NEXT suq NO-LOCK.           
      END.         
   END.
   IF fakbilag.TIDTOT = TRUE THEN DO:      
      CREATE tidut.
      CREATE tidut.
      tidut.UT = "Detaljerad tidskrivning :".
      CREATE tidut.      
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "BEFATTNING"         
      SUBSTRING(tidut.UT,25) = CAPS(Guru.Konstanter:gaok)                    
      SUBSTRING(tidut.UT,36) = "DATUM" 
      SUBSTRING(tidut.UT,45) = "TIMMAR"
      SUBSTRING(tidut.UT,52) = "PRIS"
      SUBSTRING(tidut.UT,62) = "RESTID". 
      CREATE tidut.                                       
      ASSIGN 
      tidut.UT =                               
"=======================.==========.========.======.=========.=========".       
      IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp USE-INDEX BEF.                 
      END.
      ELSE IF fakbilag.TIDMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp WHERE sumtidtemp.MED USE-INDEX BEF.      
      END.
      ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp WHERE sumtidtemp.MED = FALSE USE-INDEX BEF.      
      END.
      GET FIRST suq NO-LOCK.
      DO WHILE AVAILABLE(sumtidtemp):                  
         IF sumtidtemp.START = sumtidtemp.SLUT THEN musz = musz.
         ELSE DO:
            CREATE tidut.         
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.VIBEFATTNING,"X(23)") 
            SUBSTRING(tidut.UT,25) = sumtidtemp.AONR 
            SUBSTRING(tidut.UT,32) = STRING(sumtidtemp.DELNR,Guru.Konstanter:varforetypchar[1]) 
            SUBSTRING(tidut.UT,36) = STRING(sumtidtemp.DATUM) 
            SUBSTRING(tidut.UT,52) = STRING(sumtidtemp.PRISA,">>>>>>>>9").
            IF sumtidtemp.TIMMAR NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,45) = STRING(sumtidtemp.TIMMAR,">>9.99") 
               SUBSTRING(tidut.UT,52) = STRING(sumtidtemp.BELOPP / sumtidtemp.TIMMAR,">>>>>>>>9").
            END.
            ELSE IF sumtidtemp.OTIMMAR NE 0 THEN DO:                              
               ASSIGN
               SUBSTRING(tidut.UT,45) = STRING(sumtidtemp.OTIMMAR,">>9.99")
               SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.VIOBEFATTNING,"X(23)") 
               SUBSTRING(tidut.UT,52) = STRING(sumtidtemp.OBELOPP / sumtidtemp.OTIMMAR,">>>>>>>>9").
            END.
            ELSE IF sumtidtemp.RESTIM NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,45) = STRING(sumtidtemp.RESTIM,">>9.99")
               SUBSTRING(tidut.UT,52) = STRING(sumtidtemp.RESKOSTDEC / sumtidtemp.RESTIM,">>>>>>>>9")
               SUBSTRING(tidut.UT,62) = "JA".                                               
            END.
         END.
         GET NEXT suq NO-LOCK.           
      END.         
   END.
   IF fakbilag.KLOCKNAMN = TRUE THEN DO:      
      CREATE tidut.
      CREATE tidut.
      tidut.UT = "Detaljerad tidskrivning :".
      CREATE tidut.      
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "NAMN"         
      SUBSTRING(tidut.UT,25) = CAPS(Guru.Konstanter:gaok)                    
      SUBSTRING(tidut.UT,36) = "DATUM" 
      SUBSTRING(tidut.UT,45) = "START" 
      SUBSTRING(tidut.UT,51) = "SLUT"
      SUBSTRING(tidut.UT,57) = "TIMMAR"
      SUBSTRING(tidut.UT,64) = "PRIS"
      SUBSTRING(tidut.UT,74) = "RESTID". 
      CREATE tidut.                                       
      ASSIGN 
      tidut.UT =                               
"=======================.==========.========.=====.=====.======.=========.=========".       
      IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp USE-INDEX BEF 
         BY sumtidtemp.PERSONALKOD BY sumtidtemp.DATUM BY sumtidtemp.START BY sumtidtemp.SLUT.               
      END.
      ELSE IF fakbilag.TIDMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp WHERE sumtidtemp.MED USE-INDEX BEF 
         BY sumtidtemp.PERSONALKOD BY sumtidtemp.DATUM BY sumtidtemp.START BY sumtidtemp.SLUT.
      END.
      ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp WHERE sumtidtemp.MED = FALSE USE-INDEX BEF 
         BY sumtidtemp.PERSONALKOD BY sumtidtemp.DATUM BY sumtidtemp.START BY sumtidtemp.SLUT.
      END.
      GET FIRST suq NO-LOCK.
      DO WHILE AVAILABLE(sumtidtemp):                  
         IF sumtidtemp.START = sumtidtemp.SLUT THEN musz = musz.
         ELSE DO:
            CREATE tidut.         
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.NAMN,"X(23)") 
            SUBSTRING(tidut.UT,25) = sumtidtemp.AONR 
            SUBSTRING(tidut.UT,32) = STRING(sumtidtemp.DELNR,Guru.Konstanter:varforetypchar[1]) 
            SUBSTRING(tidut.UT,36) = STRING(sumtidtemp.DATUM) 
            SUBSTRING(tidut.UT,45) = STRING(sumtidtemp.START,"99.99") 
            SUBSTRING(tidut.UT,51) = STRING(sumtidtemp.SLUT,"99.99")
            SUBSTRING(tidut.UT,64) = STRING(sumtidtemp.PRISA,">>>>>>>>9").
            IF sumtidtemp.TIMMAR NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,57) = STRING(sumtidtemp.TIMMAR,">>9.99") 
               SUBSTRING(tidut.UT,64) = STRING(sumtidtemp.BELOPP / sumtidtemp.TIMMAR,">>>>>>>>9").
            END.
            ELSE IF sumtidtemp.OTIMMAR NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,57) = STRING(sumtidtemp.OTIMMAR,">>9.99")            
               SUBSTRING(tidut.UT,64) = STRING(sumtidtemp.OBELOPP / sumtidtemp.OTIMMAR,">>>>>>>>9").
            END.
            ELSE IF sumtidtemp.RESTIM NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,57) = STRING(sumtidtemp.RESTIM,">>9.99")
               SUBSTRING(tidut.UT,64) = STRING(sumtidtemp.RESKOSTDEC / sumtidtemp.RESTIM,">>>>>>>>9")
               SUBSTRING(tidut.UT,74) = "JA".                                               
            END.
         END.
         GET NEXT suq NO-LOCK.           
      END.         
   END.
   IF fakbilag.TOTNAMN = TRUE THEN DO:      
      CREATE tidut.
      CREATE tidut.
      tidut.UT = "Detaljerad tidskrivning :".
      CREATE tidut.      
      CREATE tidut.
      ASSIGN
      SUBSTRING(tidut.UT,1) = "NAMN"         
      SUBSTRING(tidut.UT,25) = CAPS(Guru.Konstanter:gaok)                    
      SUBSTRING(tidut.UT,36) = "DATUM" 
      SUBSTRING(tidut.UT,45) = "TIMMAR"
      SUBSTRING(tidut.UT,52) = "PRIS"
      SUBSTRING(tidut.UT,62) = "RESTID". 
      CREATE tidut.                                       
      ASSIGN 
      tidut.UT =                               
"=======================.==========.========.======.=========.=========".       
      IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp USE-INDEX BEF
         BY sumtidtemp.PERSONALKOD BY sumtidtemp.DATUM BY sumtidtemp.START BY sumtidtemp.SLUT.       
      END.
      ELSE IF fakbilag.TIDMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp WHERE sumtidtemp.MED USE-INDEX BEF
         BY sumtidtemp.PERSONALKOD BY sumtidtemp.DATUM BY sumtidtemp.START BY sumtidtemp.SLUT.      
      END.
      ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:                                        
         OPEN QUERY suq FOR EACH sumtidtemp WHERE sumtidtemp.MED = FALSE USE-INDEX BEF 
         BY sumtidtemp.PERSONALKOD BY sumtidtemp.DATUM BY sumtidtemp.START BY sumtidtemp.SLUT.      
      END.
      GET FIRST suq NO-LOCK.
      DO WHILE AVAILABLE(sumtidtemp):                  
         IF sumtidtemp.START = sumtidtemp.SLUT THEN musz = musz.
         ELSE DO:
            CREATE tidut.         
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.NAMN,"X(23)") 
            SUBSTRING(tidut.UT,25) = sumtidtemp.AONR 
            SUBSTRING(tidut.UT,32) = STRING(sumtidtemp.DELNR,Guru.Konstanter:varforetypchar[1]) 
            SUBSTRING(tidut.UT,36) = STRING(sumtidtemp.DATUM) 
            SUBSTRING(tidut.UT,52) = STRING(sumtidtemp.PRISA,">>>>>>>>9").
            IF sumtidtemp.TIMMAR NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,45) = STRING(sumtidtemp.TIMMAR,">>9.99") 
               SUBSTRING(tidut.UT,52) = STRING(sumtidtemp.BELOPP / sumtidtemp.TIMMAR,">>>>>>>>9").
            END.
            ELSE IF sumtidtemp.OTIMMAR NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,45) = STRING(sumtidtemp.OTIMMAR,">>9.99") 
               SUBSTRING(tidut.UT,52) = STRING(sumtidtemp.OBELOPP / sumtidtemp.OTIMMAR,">>>>>>>>9").
            END.
            ELSE IF sumtidtemp.RESTIM NE 0 THEN DO:
               ASSIGN
               SUBSTRING(tidut.UT,45) = STRING(sumtidtemp.RESTIM,">>9.99")
               SUBSTRING(tidut.UT,52) = STRING(sumtidtemp.RESKOSTDEC / sumtidtemp.RESTIM,">>>>>>>>9")
               SUBSTRING(tidut.UT,62) = "JA".                                               
            END.
         END.
         GET NEXT suq NO-LOCK.           
      END.         
   END.                         
END PROCEDURE.

PROCEDURE huvud_UI :
     /*HUVUD*/              
   {muswait.i}
   /*RS*/
   CREATE fakturaexcelTT.
   FIND FIRST FAKTNAMN WHERE FAKTNAMN.FAKTURNR = FAKTURERAD.FAKTNR AND 
   FAKTNAMN.FDELNR = FAKTURERAD.FDELNR NO-LOCK NO-ERROR.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.     
   IF NOT AVAILABLE AVDELNING THEN DO:
      FIND FIRST AVDELNING NO-LOCK NO-ERROR.
   END.
                                                                                          
   str =      
   "===============================================================================".      
   CREATE tidut.   
   SUBSTRING(tidut.UT,65) = STRING(TODAY). 
   CREATE tidut.       
   FIND FIRST FAKTNAMN WHERE FAKTNAMN.FAKTURNR = FAKTURERAD.FAKTNR AND 
   FAKTNAMN.FDELNR = FAKTURERAD.FDELNR NO-LOCK NO-ERROR.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.     
   IF NOT AVAILABLE AVDELNING THEN DO:
      FIND FIRST AVDELNING NO-LOCK NO-ERROR.
   END.
   CREATE tidut.
   /*RS*/
   fakturaexcelTT.AVDELNINGSNAMN =  AVDELNING.AVDELNINGNAMN.
   SUBSTRING(tidut.UT,1) = AVDELNING.AVDELNINGNAMN.  
   fakturaexcelTT.FAKTNUMMER = STRING(FAKTURERAD.VFAKTNR).   
   fakturaexcelTT.BOKDATUM = FAKTURERAD.BOKDATUM.
               
   IF FAKTURERAD.VFAKTNR = 0 THEN DO:
      /*RS*/
      fakturaexcelTT.FAKTNUMMER = "ARBETSKOPIA " + STRING(FAKTPLAN.FAKTNR) + " " +
      STRING(FAKTURERAD.FDELNR).
      ASSIGN                                        
      SUBSTRING(tidut.UT,38) = "Detta �r endast en arbetskopia TEST"
      SUBSTRING(tidut.UT,70) = STRING(TODAY).            
      CREATE tidut.
      CREATE tidut.
      ASSIGN                    
      SUBSTRING(tidut.UT,38) = "Faktura Nr :"                              
      SUBSTRING(tidut.UT,51) = STRING(FAKTPLAN.FAKTNR) + " " +
      STRING(FAKTURERAD.FDELNR).    
      fakturaexcelTT.FAKTNR = FAKTURERAD.FAKTNR.                          
   END.   
   ELSE DO:       
      FIND FIRST FAKTSKARP WHERE FAKTSKARP.OMRADE = FAKTPLAN.OMRADE 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FAKTSKARP THEN DO:
         FIND FIRST FAKTSKARP NO-LOCK NO-ERROR. 
      END.
      /*RS*/
      fakturaexcelTT.FAKTNUMMER = STRING(YEAR(FAKTURERAD.BOKDATUM)) + STRING(FAKTSKARP.ARKIVSTALLE) +
      STRING(FAKTURERAD.VFAKTNR).
      CREATE tidut.
      ASSIGN                    
      SUBSTRING(tidut.UT,38) = "Faktura Nr :"                              
      SUBSTRING(tidut.UT,51) = STRING(FAKTPLAN.FAKTNR) + " " +  
      STRING(YEAR(FAKTURERAD.BOKDATUM)) + FAKTSKARP.ARKIVSTALLE +
      STRING(FAKTURERAD.VFAKTNR). 
      fakturaexcelTT.FAKTNR = FAKTURERAD.FAKTNR.
      fakturaexcelTT.VFAKTNR = FAKTURERAD.VFAKTNR.                        
   END.
   /*
   CREATE tidut.  
   ASSIGN                   
   SUBSTRING(tidut.UT,10) = "Faktureringsadress".
   */
   CREATE tidut.
   /*RS*/
   fakturaexcelTT.BESTNAMN = FAKTNAMN.BESTNAMN.
   ASSIGN                   
   SUBSTRING(tidut.UT,10) = FAKTNAMN.BESTNAMN.   
   CREATE tidut.
   ASSIGN    
   /*RS*/
   fakturaexcelTT.BESTALLARE = FAKTNAMN.BESTALLARE.               
   SUBSTRING(tidut.UT,10) = FAKTNAMN.BESTALLARE.
   CREATE tidut.
   CREATE tidut.  
   /*RS*/
   fakturaexcelTT.FAKTADRESS = FAKTNAMN.FAKADRESS.  
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = SUBSTRING(FAKTNAMN.FAKADRESS,1,25).
   CREATE tidut.
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = SUBSTRING(FAKTNAMN.FAKADRESS,27,25).   
   CREATE tidut.
   /*RS*/
   fakturaexcelTT.FAKTPOSTNR = FAKTNAMN.FAKPNR. 
   /*RS*/
   fakturaexcelTT.FAKTORT = FAKTNAMN.FAKORT. 
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = STRING(FAKTNAMN.FAKPNR,"999 99")
   SUBSTRING(tidut.UT,18) = FAKTNAMN.FAKORT.  
   
   CREATE tidut.
   CREATE tidut.
   /*RS*/
   fakturaexcelTT.VARREF = FAKTNAMN.VARREF.
   /*RS*/
   fakturaexcelTT.ERREF = FAKTNAMN.KONTAKT.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "V�r referens :"
   SUBSTRING(tidut.UT,16) = FAKTNAMN.VARREF                               
   SUBSTRING(tidut.UT,40) = "Er referens :"
   SUBSTRING(tidut.UT,54) = FAKTNAMN.KONTAKT. 
   CREATE tidut.       
   CREATE tidut.       
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Pris enligt offert  :"  
   SUBSTRING(tidut.UT,23) = faktyp(FAKTPLAN.FAKTTYP). 
   CREATE tidut. 
   /*RS*/
   fakturaexcelTT.FAKTNAMN = FAKTPLAN.NAMN.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Arbete              :" 
   SUBSTRING(tidut.UT,23) = FAKTPLAN.NAMN.                   
   IF AVAILABLE FAKTURERAD THEN DO:
      CREATE faktposterexcelTT.
      faktposterexcelTT.BESKIVNING = FAKTURERAD.FAKTXT.
      faktposterexcelTT.ANTAL = "KOMMENTAR".
      ASSIGN
      retvar = 1
      ednum = 1
      ednum3 = LENGTH(FAKTURERAD.FAKTXT)
      retvar = INDEX(FAKTURERAD.FAKTXT,CHR(10),ednum)
      edtecken = 50
      edtext = FAKTURERAD.FAKTXT
      tidtext = "".  
      {ANMARK2.I}                             
   END.                   
END PROCEDURE.



PROCEDURE summa_UI :
   /*
   "Fastpris" "L�pande r�kning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   {muswait.i}
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut.             
   varcolon = 25.
   IF (slutfaktvar = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      OPEN QUERY faktstartq FOR EACH FAKTSTART WHERE FAKTSTART.FAKTNR = FAKTURERAD.FAKTNR AND
      FAKTSTART.VFAKTNR = FAKTURERAD.VFAKTNR AND FAKTSTART.FAKTURERAD = TRUE NO-LOCK.
      IF vartyp = 2 THEN DO:
         GET FIRST faktstartq NO-LOCK.
         DO WHILE AVAILABLE(FAKTSTART):      
            IF FAKTSTART.START NE "" THEN DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTART.FAKTNR AND FAKTAVTALAONR.START = FAKTSTART.START 
               NO-LOCK.      
            END.
            ELSE DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTART.FAKTNR AND FAKTAVTALAONR.PLANDATUM = FAKTSTART.PLANDATUM NO-LOCK.
            END.
            GET FIRST faktavtalq NO-LOCK.
            DO WHILE AVAILABLE(FAKTAVTALAONR):
               CREATE tidut.      
               ASSIGN
               SUBSTRING(tidut.UT,1,30) = FAKTAVTALAONR.FRITEXT
               SUBSTRING(tidut.UT,62) = STRING(FAKTAVTALAONR.BELOPP,"->>>>>>>>9.99").
               GET NEXT faktavtalq NO-LOCK.
            END. 
            GET NEXT faktstartq NO-LOCK.
         END.
      END.
      CREATE tidut.  
   END.
   IF (slutfaktvar = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      /*
      OPEN QUERY faktstartq FOR EACH FAKTSTART WHERE FAKTSTART.FAKTNR = FAKTURERAD.FAKTNR AND
      FAKTSTART.VFAKTNR = FAKTURERAD.VFAKTNR AND FAKTSTART.FAKTURERAD = TRUE NO-LOCK.
      */
      GET FIRST faktstartq NO-LOCK.
      IF AVAILABLE FAKTSTART THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ing�ende poster     ".
      END.
      DO WHILE AVAILABLE(FAKTSTART):
         IF FAKTSTART.START = "START" THEN DO:
            CREATE tidut.      
            ASSIGN                  
            SUBSTRING(tidut.UT,1) = "Vid arbetets start " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTART.START = "SLUT" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Vid arbetets slut  " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTART.START = "" THEN DO:
            CREATE tidut.      
            ASSIGN                                                      
            SUBSTRING(tidut.UT,1) = "Enligt plan        " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,27,30) = FAKTSTART.FRITEXT 
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         GET NEXT faktstartq NO-LOCK.
      END.          
   END.
   ELSE IF vartyp = 52 THEN DO:      
      OPEN QUERY faktuppq FOR EACH FAKTUPPARB WHERE FAKTUPPARB.FAKTNR = FAKTURERAD.FAKTNR AND
      FAKTUPPARB.VFAKTNR = FAKTURERAD.VFAKTNR AND FAKTUPPARB.FAKTURERAD = TRUE NO-LOCK.     
      GET FIRST faktuppq NO-LOCK.
      IF AVAILABLE FAKTUPPARB THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ing�ende poster     ".
      END.
      DO WHILE AVAILABLE(FAKTUPPARB):
         CREATE tidut.      
         ASSIGN                                                      
         SUBSTRING(tidut.UT,1) = "Enligt plan        " + STRING(FAKTUPPARB.FAKT%,">>9") + " %"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,27,30) = FAKTUPPARB.FRITEXT 
         SUBSTRING(tidut.UT,62) = STRING(FAKTUPPARB.FAKTBELOPP,"->>>>>>>>9.99").   
         GET NEXT faktuppq NO-LOCK.
      END.          
   END.
   ELSE DO:
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,30) = "Ing�ende poster     ".
      CREATE tidut. 
      IF FAKTURERAD.BELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Arbetskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.BELOPP,"->>>>>>>>9.99").
      END.  
      IF FAKTURERAD.OBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "�vertidskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.OBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.TBELOPP NE 0 THEN DO:               
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Traktamenteskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.TBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.LONKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "L�netill�ggskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.LONKOST,"->>>>>>>>9.99").
      END. 
      IF FAKTURERAD.RESKOSTDEC NE 0 THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Resers�ttning"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.RESKOSTDEC,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.MTRLKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Materielkostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.MTRLKOST,"->>>>>>>>9.99").  
      END.  
      IF FAKTURERAD.OVRKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "�vrigakostnader"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.OVRKOST,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.KOSTBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Externa fakturor"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.KOSTBELOPP,"->>>>>>>>9.99").
      END.                                        
   END.
   CREATE tidut.
   CREATE tidut. 
   /*RS*/
   fakturaexcelTT.SUMMA = STRING(FAKTPLAN.TOTPRIS).  
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Denna faktura"
   SUBSTRING(tidut.UT,varcolon) = ":"
   SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.TOTPRIS,"->>>>>>>>9.99").     
   CREATE tidut.                 
   IF slutfaktvar = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
         CREATE tidut.                                 
         ASSIGN                  
         SUBSTRING(tidut.UT,1) = "Avg�r tidigare prel.fakt."      
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.AVGAR,"->>>>>>>>9.99") + " skr".         
      END.      
      CREATE tidut.
   END.
   momsumma = 0.
   FOR EACH FAKTMOMS WHERE FAKTMOMS.FAKTNR = FAKTPLAN.FAKTNR AND 
   FAKTMOMS.FDELNR = FAKTURERAD.FDELNR NO-LOCK BREAK BY FAKTMOMS.MOMSID: 
      ACCUMULATE FAKTMOMS.BELOPP (TOTAL BY FAKTMOMS.MOMSID).
      ACCUMULATE FAKTMOMS.MOMSBELOPP (TOTAL BY FAKTMOMS.MOMSID).
      IF LAST-OF(FAKTMOMS.MOMSID) THEN DO:                        
         IF (ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP) NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
           /* SUBSTRING(tidut.UT,1) = SUBSTRING(FAKTMOMS.FRITEXT,1,30)*/
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,31) = 
            "Moms " + STRING(FAKTMOMS.MOMSEXTERNT,">>9.99") + " % p�"                                                                                                                    
            SUBSTRING(tidut.UT,49) = STRING((ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.BELOPP),"->>>>>>>9.99") 
            SUBSTRING(tidut.ut,62) = STRING((ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP),"->>>>>>>>9.99").             
            momsumma = momsumma + ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP.
            /*RS*/
            fakturaexcelTT.MOMS  = STRING((ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP),"->>>>>>>>9.99"). 
         END.
      END.                                      
   END.
   RUN oresmoms_UI.
   
   CREATE tidut.  
   CREATE tidut.                 
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Att betala"
   SUBSTRING(tidut.UT,varcolon) = ":".
   IF slutfaktvar = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
         /*RS*/
         fakturaexcelTT.SLUTSUMMA = STRING(FAKTURERAD.TOTPRIS + 
         FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ - FAKTURERAD.AVGAR). 
         SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.TOTPRIS + 
         FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ - FAKTURERAD.AVGAR) + " skr".    
      END.
      ELSE DO:
         /*RS*/
         fakturaexcelTT.SLUTSUMMA = 
         STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ).  
         SUBSTRING(tidut.ut,62) = 
         STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ,"->>>>>>>>9.99") + " skr".   
      END. 
   END.
   ELSE DO: 
      /*RS*/
      fakturaexcelTT.SLUTSUMMA =  STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ).
      SUBSTRING(tidut.ut,62) = 
      STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ,"->>>>>>>>9.99") + " skr".          
   END.
   CREATE tidut.
   CREATE tidut. 
      /*
   "Fastpris" "L�pande r�kning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   CREATE tidut.
   CREATE tidut.      
   SUBSTRING(tidut.UT,1) = str.           
   CREATE tidut.   
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Adress"
   SUBSTRING(tidut.UT,40) = "Telefon".       

   CREATE tidut.                  
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = AVDELNING.GATUADR
   SUBSTRING(tidut.UT,40) = AVDELNING.TELVXL.   
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = STRING(AVDELNING.POSTNR,"999 99")       
   SUBSTRING(tidut.UT,9) = AVDELNING.POSTADR.     
   CREATE tidut.
   CREATE tidut.
   CREATE tidut.      
   FIND FIRST fakbilag NO-ERROR.   
   IF AVAILABLE fakbilag THEN DO:
      RUN bilaga_UI.
      IF fakbilag.KONTO = TRUE THEN DO:      
         RUN VISKONT.P (INPUT FAKTPLAN.FAKTNR, INPUT vfknr).
      END.                                              
   END.
   /*RS*/
   /*
   FOR EACH fakturaexcelTT:
      MESSAGE fakturaexcelTT.FAKTNAMN
      VIEW-AS ALERT-BOX.
   END.
   FOR EACH faktposterexcelTT:
      MESSAGE faktposterexcelTT.BESKIVNING
      VIEW-AS ALERT-BOX.
   END.
   */
END PROCEDURE.

PROCEDURE summagran_UI :
   /*
   "Fastpris" "L�pande r�kning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   {muswait.i}
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut.   
   IF (slutfaktvar = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      OPEN QUERY faktstartq FOR EACH FAKTSTART WHERE FAKTSTART.FAKTNR = FAKTURERAD.FAKTNR AND
      FAKTSTART.VFAKTNR = FAKTURERAD.VFAKTNR AND FAKTSTART.FAKTURERAD = TRUE NO-LOCK.
      IF vartyp = 2 THEN DO:
         GET FIRST faktstartq NO-LOCK.
         DO WHILE AVAILABLE(FAKTSTART):      
            IF FAKTSTART.START NE "" THEN DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTART.FAKTNR AND FAKTAVTALAONR.START = FAKTSTART.START 
               NO-LOCK.      
            END.
            ELSE DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTART.FAKTNR AND FAKTAVTALAONR.PLANDATUM = FAKTSTART.PLANDATUM NO-LOCK.
            END.
            GET FIRST faktavtalq NO-LOCK.
            DO WHILE AVAILABLE(FAKTAVTALAONR):
               CREATE tidut.      
               ASSIGN
               SUBSTRING(tidut.UT,1,30) = FAKTAVTALAONR.FRITEXT
               SUBSTRING(tidut.UT,62) = STRING(FAKTAVTALAONR.BELOPP,"->>>>>>>>9.99").
               GET NEXT faktavtalq NO-LOCK.
            END. 
            GET NEXT faktstartq NO-LOCK.
         END.
      END.
      CREATE tidut.  
   END.
   varcolon = 25.
   IF vartyp = 5 OR vartyp = 52 THEN DO:
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,54) = "Denna" 
      SUBSTRING(tidut.UT,67) = "�terst�ende".
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,1) = Guru.Konstanter:gaok
      SUBSTRING(tidut.UT,10) = "Ben�mning"
      SUBSTRING(tidut.UT,28) = "Takpris kr"
      SUBSTRING(tidut.UT,41) = "Hittills kr"
      SUBSTRING(tidut.UT,54) = "faktura kr" 
      SUBSTRING(tidut.UT,67) = "kr".
      CREATE tidut.
      ASSIGN 
      tidut.UT = "========.=================.============.============.============.============".      
      RUN vilkaao_UI.      
      CREATE tidut.
   END.      
   IF (slutfaktvar = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      GET FIRST faktstartq NO-LOCK.
      IF AVAILABLE FAKTSTART THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ing�ende poster     ".
      END.
      DO WHILE AVAILABLE(FAKTSTART):
         IF FAKTSTART.START = "START" THEN DO:
            CREATE tidut.      
            ASSIGN                  
            SUBSTRING(tidut.UT,1) = "Vid arbetets start " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTART.START = "SLUT" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Vid arbetets slut  " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTART.START = "" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Enligt plan        " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,27,30) = FAKTSTART.FRITEXT
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         GET NEXT faktstartq NO-LOCK.
      END.          
   END.
   ELSE DO:      
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,30) = "Ing�ende poster     ".      
      CREATE tidut. 
      IF FAKTURERAD.BELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Arbetskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"                 
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.BELOPP,"->>>>>>>>9.99").
      END.  
      IF FAKTURERAD.OBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "�vertidskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.OBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.TBELOPP NE 0 THEN DO:               
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Traktamenteskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.TBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.LONKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "L�netill�ggskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.LONKOST,"->>>>>>>>9.99").
      END. 
      IF FAKTURERAD.RESKOSTDEC NE 0 THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Resers�ttning"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.RESKOSTDEC,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.MTRLKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Materielkostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.MTRLKOST,"->>>>>>>>9.99").  
      END.  
      IF FAKTURERAD.OVRKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "�vrigakostnade"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.OVRKOST,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.KOSTBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Externa fakturor"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.KOSTBELOPP,"->>>>>>>>9.99").
      END.                                        
   END.
   CREATE tidut.
   CREATE tidut.
   /*RS*/
   fakturaexcelTT.SUMMA = STRING(FAKTURERAD.TOTPRIS).
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Denna faktura"
   SUBSTRING(tidut.UT,varcolon) = ":"
   SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.TOTPRIS,"->>>>>>>>9.99").     
   CREATE tidut.                 
   IF slutfaktvar = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
         CREATE tidut.                                 
         ASSIGN                  
         SUBSTRING(tidut.UT,1) = "Avg�r tidigare prel.fakt."      
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.AVGAR,"->>>>>>>>9.99") + " skr".         
      END.      
      CREATE tidut.
   END.
   momsumma = 0.
   FOR EACH FAKTMOMS WHERE FAKTMOMS.FAKTNR = FAKTPLAN.FAKTNR AND 
   FAKTMOMS.FDELNR = FAKTURERAD.FDELNR NO-LOCK BREAK BY FAKTMOMS.MOMSID: 
      ACCUMULATE FAKTMOMS.BELOPP (TOTAL BY FAKTMOMS.MOMSID).
      ACCUMULATE FAKTMOMS.MOMSBELOPP (TOTAL BY FAKTMOMS.MOMSID).
      IF LAST-OF(FAKTMOMS.MOMSID) THEN DO:                        
         IF (ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP) NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
           /* SUBSTRING(tidut.UT,1) = SUBSTRING(FAKTMOMS.FRITEXT,1,30)*/
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,31) = "Moms " + STRING(FAKTMOMS.MOMSEXTERNT,">>9.99") + " % p�"                                                                                                                    
            SUBSTRING(tidut.UT,49) = STRING((ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.BELOPP),"->>>>>>>9.99") 
            /*RS*/
            fakturaexcelTT.MOMS = STRING((ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP),"->>>>>>>>9.99").             
            momsumma = momsumma + ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP.
            SUBSTRING(tidut.ut,62) = STRING((ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP),"->>>>>>>>9.99").             
            momsumma = momsumma + ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP.
         END.
      END.                                      
   END.  
   RUN oresmoms_UI.
   
   CREATE tidut.  
   CREATE tidut.                 
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Att betala"
   SUBSTRING(tidut.UT,varcolon) = ":".
   IF slutfaktvar = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
                  /*RS*/
         fakturaexcelTT.SLUTSUMMA = STRING(FAKTURERAD.TOTPRIS + 
         FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ - FAKTURERAD.AVGAR).    
         SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.TOTPRIS + 
         FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ - FAKTURERAD.AVGAR) + " skr".    
      END.
      
      ELSE DO:
      /*RS*/
      fakturaexcelTT.SLUTSUMMA = 
      STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ).
      SUBSTRING(tidut.ut,62) = 
      STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ,"->>>>>>>>9.99") + " skr".
      END.   
   END.
   ELSE DO:
       /*RS*/
      fakturaexcelTT.SLUTSUMMA =
      STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ). 
      SUBSTRING(tidut.ut,62) = 
      STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ,"->>>>>>>>9.99") + " skr".          
   END.
   CREATE tidut.
   CREATE tidut. 
   
   /*
   "Fastpris" "L�pande r�kning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   
   CREATE tidut.
   CREATE tidut.      
   SUBSTRING(tidut.UT,1) = str.           
   CREATE tidut.   
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Adress"
   SUBSTRING(tidut.UT,40) = "Telefon".       

   CREATE tidut.                  
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = AVDELNING.GATUADR
   SUBSTRING(tidut.UT,40) = AVDELNING.TELVXL.   
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = STRING(AVDELNING.POSTNR,"999 99")       
   SUBSTRING(tidut.UT,9) = AVDELNING.POSTADR.     
   CREATE tidut.
   CREATE tidut.
   CREATE tidut.      
   FIND FIRST fakbilag NO-ERROR.   
   IF AVAILABLE fakbilag THEN DO:
      RUN bilaga_UI.
      IF fakbilag.KONTO = TRUE THEN DO:      
         RUN VISKONT.P (INPUT FAKTPLAN.FAKTNR, INPUT vfknr).
      END.                       
   END.
END PROCEDURE.


PROCEDURE huvudesab_UI :
     /*HUVUD*/              
   {muswait.i}                                                                                       
   str =      
   "===============================================================================".      
   
   CREATE tidut.       
   FIND FIRST FAKTNAMN WHERE FAKTNAMN.FAKTURNR = FAKTURERAD.FAKTNR AND 
   FAKTNAMN.FDELNR = FAKTURERAD.FDELNR NO-LOCK NO-ERROR.      
   IF FAKTURERAD.VFAKTNR = 0 THEN DO:   
      ASSIGN                                        
      SUBSTRING(tidut.UT,38) = "Detta �r endast en arbetskopia"
      SUBSTRING(tidut.UT,70) = STRING(TODAY).            
      CREATE tidut.
      CREATE tidut.
      ASSIGN                    
      SUBSTRING(tidut.UT,38) = "Faktura Nr :"                              
      SUBSTRING(tidut.UT,51) = STRING(FAKTPLAN.FAKTNR) + " " +
      STRING(FAKTURERAD.FDELNR).                              
   END.   
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,10) = FAKTNAMN.BESTNAMN.   
   CREATE tidut.   
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = SUBSTRING(FAKTNAMN.FAKADRESS,1,25).
   IF SUBSTRING(FAKTNAMN.FAKADRESS,27,25) NE "" THEN DO:
      CREATE tidut.
      SUBSTRING(tidut.UT,10) =
      SUBSTRING(FAKTNAMN.FAKADRESS,27,25).
   END.   
   CREATE tidut.
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = STRING(FAKTNAMN.FAKPNR,"999 99")
   SUBSTRING(tidut.UT,18) = FAKTNAMN.FAKORT. 
   FIND FIRST ANVANDARE WHERE ANVANDARE.ANVANDARE = FAKTPLAN.ANVANDARE NO-LOCK NO-ERROR.
   IF AVAILABLE ANVANDARE THEN DO:
      FIND FIRST PERSONALPROJEKT WHERE PERSONALPROJEKT.PERSONALKOD = ANVANDARE.PERSONALKOD 
      NO-LOCK NO-ERROR.
   END.
   CREATE tidut.
   IF AVAILABLE PERSONALPROJEKT THEN DO:
      ASSIGN                      
      SUBSTRING(tidut.UT,30) = FAKTPLAN.VISAONR + "/" + PERSONALPROJEKT.SIGN.
   END.
   ELSE DO:
      ASSIGN                      
      SUBSTRING(tidut.UT,30) = FAKTPLAN.VISAONR + "/" + FAKTPLAN.ANVANDARE.
   END.   
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.   
   IF FAKTNAMN.KONTAKT NE "" THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = "Er referens  :"
      SUBSTRING(tidut.UT,16) = FAKTNAMN.KONTAKT. 
   END.
   CREATE tidut.
   IF FAKTNAMN.BESTALLARE NE "" THEN DO:
      ASSIGN       
      SUBSTRING(tidut.UT,1) = "Best av      :"
      SUBSTRING(tidut.UT,16) = FAKTNAMN.BESTALLARE. 
   END.
   CREATE tidut.
   IF FAKTNAMN.VARREF NE "" THEN DO:
      ASSIGN                   
      SUBSTRING(tidut.UT,1) = "V�r referens :"
      SUBSTRING(tidut.UT,16) = FAKTNAMN.VARREF.
   END.
   CREATE tidut.       
   CREATE tidut.       
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Pris enligt offert  :"  
   SUBSTRING(tidut.UT,23) = faktyp(FAKTPLAN.FAKTTYP). 
   CREATE tidut. 
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Arbete              :" 
   SUBSTRING(tidut.UT,23) = FAKTPLAN.NAMN.                   
   IF AVAILABLE FAKTURERAD THEN DO:
      ASSIGN
      retvar = 1
      ednum = 1
      ednum3 = LENGTH(FAKTURERAD.FAKTXT)
      retvar = INDEX(FAKTURERAD.FAKTXT,CHR(10),ednum)
      edtecken = 50
      edtext = FAKTURERAD.FAKTXT
      tidtext = "".  
      {ANMARK2.I}                             
   END.
                         
END PROCEDURE.



PROCEDURE summaesab_UI :
   /*
   "Fastpris" "L�pande r�kning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut.                                 
   IF (slutfaktvar = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      OPEN QUERY faktstartq FOR EACH FAKTSTART WHERE FAKTSTART.FAKTNR = FAKTURERAD.FAKTNR AND
      FAKTSTART.VFAKTNR = FAKTURERAD.VFAKTNR AND FAKTSTART.FAKTURERAD = TRUE NO-LOCK.
      IF vartyp = 2 THEN DO:
         GET FIRST faktstartq NO-LOCK.
         DO WHILE AVAILABLE(FAKTSTART):      
            IF FAKTSTART.START NE "" THEN DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTART.FAKTNR AND FAKTAVTALAONR.START = FAKTSTART.START 
               NO-LOCK.      
            END.
            ELSE DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTART.FAKTNR AND FAKTAVTALAONR.PLANDATUM = FAKTSTART.PLANDATUM NO-LOCK.
            END.
            GET FIRST faktavtalq NO-LOCK.
            DO WHILE AVAILABLE(FAKTAVTALAONR):
               CREATE tidut.      
               ASSIGN
               SUBSTRING(tidut.UT,1,30) = FAKTAVTALAONR.FRITEXT
               SUBSTRING(tidut.UT,62) = STRING(FAKTAVTALAONR.BELOPP,"->>>>>>>>9.99").
               GET NEXT faktavtalq NO-LOCK.
            END. 
            GET NEXT faktstartq NO-LOCK.
         END.
      END.  
      CREATE tidut.
   END.
   varcolon = 25.
   IF (slutfaktvar = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      GET FIRST faktstartq NO-LOCK.
      IF AVAILABLE FAKTSTART THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ing�ende poster     ".
      END. 
      DO WHILE AVAILABLE(FAKTSTART):
         IF FAKTSTART.START = "START" THEN DO:
            CREATE tidut.      
            ASSIGN                  
            SUBSTRING(tidut.UT,1) = "Vid arbetets start " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTART.START = "SLUT" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Vid arbetets slut  " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTART.START = "" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Enligt plan        " + STRING(FAKTSTART.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,27,30) = FAKTSTART.FRITEXT
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTART.BELOPP,"->>>>>>>>9.99").
         END.
         GET NEXT faktstartq NO-LOCK.
      END.          
   END.
   ELSE DO:      
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,30) = "Ing�ende poster     ".
     
      CREATE tidut. 
      IF FAKTURERAD.BELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Arbetskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.BELOPP,"->>>>>>>>9.99").
      END.  
      IF FAKTURERAD.OBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "�vertidskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.OBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.TBELOPP NE 0 THEN DO:               
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Traktamenteskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.TBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.LONKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "L�netill�ggskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.LONKOST,"->>>>>>>>9.99").
      END. 
      IF FAKTURERAD.RESKOSTDEC NE 0 THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Resers�ttning"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.RESKOSTDEC,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.MTRLKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Materielkostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.MTRLKOST,"->>>>>>>>9.99").  
      END.  
      IF FAKTURERAD.OVRKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "�vrigakostnader"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.OVRKOST,"->>>>>>>>9.99").
      END.
      IF FAKTURERAD.KOSTBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Externa fakturor"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTURERAD.KOSTBELOPP,"->>>>>>>>9.99").
      END.                                        
   END.
   CREATE tidut.
   CREATE tidut.
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Denna faktura"
   SUBSTRING(tidut.UT,varcolon) = ":"
   SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.TOTPRIS,"->>>>>>>>9.99").     
   CREATE tidut.                 
   IF slutfaktvar = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
         CREATE tidut.                                 
         ASSIGN                  
         SUBSTRING(tidut.UT,1) = "Avg�r tidigare prel.fakt."      
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.AVGAR,"->>>>>>>>9.99") + " skr".         
      END.      
      CREATE tidut.
   END.
   momsumma = 0.
   FOR EACH FAKTMOMS WHERE FAKTMOMS.FAKTNR = FAKTPLAN.FAKTNR AND 
   FAKTMOMS.FDELNR = FAKTURERAD.FDELNR NO-LOCK BREAK BY FAKTMOMS.MOMSID: 
      ACCUMULATE FAKTMOMS.BELOPP (TOTAL BY FAKTMOMS.MOMSID).
      ACCUMULATE FAKTMOMS.MOMSBELOPP (TOTAL BY FAKTMOMS.MOMSID).
      IF LAST-OF(FAKTMOMS.MOMSID) THEN DO:                        
         IF (ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP) NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
/*            SUBSTRING(tidut.UT,1) = SUBSTRING(FAKTMOMS.FRITEXT,1,30)*/
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,31) = "Moms " + STRING(FAKTMOMS.MOMSEXTERNT,">>9.99") + " % p�"                                                                                                                    
            SUBSTRING(tidut.UT,49) = STRING((ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.BELOPP),"->>>>>>>9.99") 
            SUBSTRING(tidut.ut,62) = STRING((ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP),"->>>>>>>>9.99").             
            momsumma = momsumma + ACCUM TOTAL BY FAKTMOMS.MOMSID FAKTMOMS.MOMSBELOPP.
        END.
      END.                                      
   END.  
   RUN oresmoms_UI.
   
   CREATE tidut.  
   CREATE tidut.                               
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Att betala"
   SUBSTRING(tidut.UT,varcolon) = ":".
   IF slutfaktvar = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
         SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.TOTPRIS + 
         FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ - FAKTURERAD.AVGAR) + " skr".    
         betvar = FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ - FAKTURERAD.AVGAR.
      END.
      ELSE DO:
         SUBSTRING(tidut.ut,62) = 
         STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ,"->>>>>>>>9.99") + " skr".    
         betvar = FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ.
      END.
   END.
   ELSE DO:
      SUBSTRING(tidut.ut,62) = 
      STRING(FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ,"->>>>>>>>9.99") + " skr".    
      betvar = FAKTURERAD.TOTPRIS + FAKTURERAD.MOMSBELOPP + FAKTURERAD.ORESUTJ.
      
   END.
   CREATE tidut.
   CREATE tidut. 
   /*
   "Fastpris" "L�pande r�kning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   
   IF fakbilag.KONTO = TRUE THEN DO:      
      RUN VISKONT.P (INPUT FAKTPLAN.FAKTNR, INPUT vfknr).
   END.                                                  
END PROCEDURE.

PROCEDURE tidhamt_UI :
   {muswait.i}
   OPEN QUERY faktidq FOR EACH FAKTTID WHERE FAKTTID.FAKTNR = FAKTURERAD.FAKTNR AND                  
   FAKTTID.FDELNR = FAKTURERAD.FDELNR NO-LOCK.
   GET FIRST faktidq NO-LOCK.
   DO WHILE AVAILABLE(FAKTTID):
      CREATE sumtidtemp.      
      ASSIGN                                                  
      sumtidtemp.PERSONALKOD = FAKTTID.PERSONALKOD
      sumtidtemp.NAMN = FAKTTID.NAMN 
      sumtidtemp.AONR = FAKTTID.AONR
      sumtidtemp.DELNR = FAKTTID.DELNR
      sumtidtemp.TIMMAR = FAKTTID.TIMMAR
      sumtidtemp.BELOPP = FAKTTID.BELOPP        
      sumtidtemp.OBELOPP = FAKTTID.OBELOPP 
      sumtidtemp.TBELOPP = FAKTTID.TBELOPP             
      sumtidtemp.OTIMMAR = FAKTTID.OTIMMAR 
      sumtidtemp.LONKOST = FAKTTID.LONKOST                  
      sumtidtemp.BEFATTNING = FAKTTID.BEFATTNING      
      sumtidtemp.PERSMASK = FAKTTID.PERSMASK
      sumtidtemp.TRAKTKOD = FAKTTID.TRAKTKOD
      sumtidtemp.TRAKTANTAL = FAKTTID.TRAKTANTAL  
      sumtidtemp.LONTILLAGG = FAKTTID.LONTILLAGG      
      sumtidtemp.LONTILLANTAL = FAKTTID.LONTILLANTAL 
      sumtidtemp.PRISA = FAKTTID.PRISA 
      sumtidtemp.ENDAGS = FAKTTID.ENDAGS       
      sumtidtemp.MED = FAKTTID.MED      
      sumtidtemp.PRISTYP = FAKTTID.PRISTYP
      sumtidtemp.RESTIM = FAKTTID.DECRESTID
      sumtidtemp.RESKOSTDEC = FAKTTID.RESKOSTDEC
      sumtidtemp.OTEXTID = FAKTTID.OTEXTID
      sumtidtemp.DATUM = FAKTTID.DATUM
      sumtidtemp.START = FAKTTID.START 
      sumtidtemp.SLUT = FAKTTID.SLUT
      sumtidtemp.GSTART = FAKTTID.GSTART 
      sumtidtemp.GSLUT = FAKTTID.GSLUT
      sumtidtemp.LUNCH = FAKTTID.LUNCH
      sumtidtemp.OANT1 = FAKTTID.OANT1.      
      GET NEXT faktidq NO-LOCK.
   END.               
   OPEN QUERY sbq FOR EACH sumtidtemp NO-LOCK,
   EACH BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = sumtidtemp.BEFATTNING NO-LOCK.
   GET FIRST sbq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE (sumtidtemp):
      ASSIGN
      sumtidtemp.PERSBEF = sumtidtemp.PERSONALKOD + " " + BEFATTNINGSTAB.NAMN
      sumtidtemp.VIBEFATTNING = BEFATTNINGSTAB.NAMN 
      sumtidtemp.VIOBEFATTNING = BEFATTNINGSTAB.NAMN.
      GET NEXT sbq EXCLUSIVE-LOCK.
   END.
   OPEN QUERY sq FOR EACH sumtidtemp NO-LOCK,
   EACH OVERTEXTFAKT WHERE OVERTEXTFAKT.OTEXTID = sumtidtemp.OTEXTID NO-LOCK.
   GET FIRST sq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE (sumtidtemp):
      ASSIGN sumtidtemp.VIOBEFATTNING = OVERTEXTFAKT.OTEXT. 
      GET NEXT sq EXCLUSIVE-LOCK.
   END.
END PROCEDURE.

PROCEDURE tid_UI :
   {muswait.i}
   CREATE tidut.
   IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,1) = "All tidskrivning p� ing�ende " + LC(Guru.Konstanter:gaok) + " :". 
   END. 
   ELSE IF fakbilag.TIDMED = TRUE THEN DO:  
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,1) = "Ing�ende tidskrivning :". 
   END.
   ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:  
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,1) = "Ej ing�ende tidskrivning :".
   END.      
   CREATE tidut.
   CREATE tidut. 
   CREATE tidut.
   ASSIGN                        
   SUBSTRING(tidut.UT,36) = "ARBETS" 
   SUBSTRING(tidut.UT,46) = "ARBETS" 
   SUBSTRING(tidut.UT,54) = "�VERTID" 
   SUBSTRING(tidut.UT,65) = "�VERTID"                   
   SUBSTRING(tidut.UT,73) = "RESTID" 
   SUBSTRING(tidut.UT,82) = "RESTID".
   IF fakbilag.PRIS = FALSE THEN DO:  
      ASSIGN
      SUBSTRING(tidut.UT,46,8) = "        "          
      SUBSTRING(tidut.UT,65,8) = "        "                            
      SUBSTRING(tidut.UT,82,8) = "        ".
   END.                                   
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,1) = "BEFATTNING"         
   SUBSTRING(tidut.UT,25) = CAPS(Guru.Konstanter:gaok)                    
   SUBSTRING(tidut.UT,36) = "TIMMAR" 
   SUBSTRING(tidut.UT,46) = "KOSTNAD" 
   SUBSTRING(tidut.UT,54) = "TIMMAR" 
   SUBSTRING(tidut.UT,65) = "KOSTNAD"                   
   SUBSTRING(tidut.UT,73) = "TIMMAR" 
   SUBSTRING(tidut.UT,82) = "KOSTNAD".                         
   IF fakbilag.PRIS = FALSE THEN DO:  
      ASSIGN
      SUBSTRING(tidut.UT,46,8) = "        "          
      SUBSTRING(tidut.UT,65,8) = "        "                            
      SUBSTRING(tidut.UT,82,8) = "        ".
   END.                                          
   IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
      SUBSTRING(tidut.UT,90) = "MED".    
      CREATE tidut.                                       
      ASSIGN 
      tidut.UT =       
"=======================.==========.=========.=======.==========.=======.========.=======.===". 
   END. 
   ELSE IF fakbilag.TIDMED = TRUE THEN DO:  
      CREATE tidut.   
      ASSIGN 
      tidut.UT =       
"=======================.==========.=========.=======.==========.=======.========.=======".  
   END.
   ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:  
      CREATE tidut.   
      ASSIGN 
      tidut.UT =                                                 
"=======================.==========.=========.=======.==========.=======.========.=======".  
   END.                                   
   IF fakbilag.PRIS = FALSE THEN DO:  
      ASSIGN
      SUBSTRING(tidut.UT,45,9) = "         "          
      SUBSTRING(tidut.UT,64,9) = "         "                            
      SUBSTRING(tidut.UT,81,9) = "         ".
   END.
   FOR EACH sumtidtemp:
      IF sumtidtemp.MED = ? THEN sumtidtemp.MED = FALSE.  
   END.
   IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:                                        
      FOR EACH sumtidtemp BREAK BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
         ACCUMULATE 
         sumtidtemp.BELOPP (TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.TIMMAR (TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.OBELOPP (TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.OTIMMAR (TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.LONKOST (TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR). 
         ACCUMULATE 
         sumtidtemp.TBELOPP (TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.RESTIM (TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.RESKOSTDEC (TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         IF LAST-OF(sumtidtemp.DELNR) THEN DO:
            CREATE tidut.         
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.VIBEFATTNING,"X(23)") 
            SUBSTRING(tidut.UT,25) = sumtidtemp.AONR 
            SUBSTRING(tidut.UT,32) = STRING(sumtidtemp.DELNR,Guru.Konstanter:varforetypchar[1])                      
            SUBSTRING(tidut.UT,36) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TIMMAR),"->>>>9.99")
            SUBSTRING(tidut.UT,46) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.BELOPP),"->>>>>9") 
            SUBSTRING(tidut.UT,65) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OBELOPP),"->>>>>9")
            SUBSTRING(tidut.UT,54) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OTIMMAR),"->>>>>9.99")
            SUBSTRING(tidut.UT,73) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESTIM),"->>>9.99")
            SUBSTRING(tidut.UT,82) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESKOSTDEC),"->>>>>9").                        
            IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
               SUBSTRING(tidut.UT,90) = STRING(sumtidtemp.MED,"Ja/Nej").  
            END.      
            IF fakbilag.PRIS = FALSE THEN DO:  
               ASSIGN
               SUBSTRING(tidut.UT,46,8) = "        "          
               SUBSTRING(tidut.UT,65,8) = "        "                            
               SUBSTRING(tidut.UT,82,8) = "        ".
            END.          
         END.            
      END.
   END.
   ELSE IF fakbilag.TIDMED = TRUE THEN DO:                                        
      FOR EACH sumtidtemp WHERE sumtidtemp.MED = TRUE BREAK 
      BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
         ACCUMULATE 
         sumtidtemp.BELOPP (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.TIMMAR (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.OBELOPP (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.OTIMMAR (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.LONKOST (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR). 
         ACCUMULATE 
         sumtidtemp.TBELOPP (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.RESTIM (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.RESKOSTDEC (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         IF LAST-OF(sumtidtemp.DELNR) THEN DO:
            CREATE tidut.         
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.VIBEFATTNING,"X(23)") 
            SUBSTRING(tidut.UT,25) = sumtidtemp.AONR 
            SUBSTRING(tidut.UT,32) = STRING(sumtidtemp.DELNR,Guru.Konstanter:varforetypchar[1])                      
            SUBSTRING(tidut.UT,36) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TIMMAR),"->>>>9.99")
            SUBSTRING(tidut.UT,46) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.BELOPP),"->>>>>9") 
            SUBSTRING(tidut.UT,65) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OBELOPP),"->>>>>9")
            SUBSTRING(tidut.UT,54) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OTIMMAR),"->>>>>9.99")
            SUBSTRING(tidut.UT,73) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESTIM),"->>>9.99")
            SUBSTRING(tidut.UT,82) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESKOSTDEC),"->>>>>9").                        
            IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
               SUBSTRING(tidut.UT,90) = STRING(sumtidtemp.MED,"Ja/Nej").  
            END.      
            IF fakbilag.PRIS = FALSE THEN DO:  
               ASSIGN
               SUBSTRING(tidut.UT,46,8) = "        "          
               SUBSTRING(tidut.UT,65,8) = "        "                            
               SUBSTRING(tidut.UT,82,8) = "        ".
            END.          
         END.            
      END.
   END.
   ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:                                        
      FOR EACH sumtidtemp WHERE sumtidtemp.MED = FALSE BREAK 
      BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR:         
         ACCUMULATE 
         sumtidtemp.BELOPP (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.TIMMAR (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.OBELOPP (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.OTIMMAR (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.LONKOST (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR). 
         ACCUMULATE 
         sumtidtemp.TBELOPP (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.RESTIM (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         ACCUMULATE 
         sumtidtemp.RESKOSTDEC (TOTAL BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR).
         IF LAST-OF(sumtidtemp.DELNR) THEN DO:
            CREATE tidut.         
            ASSIGN
            SUBSTRING(tidut.UT,1) = STRING(sumtidtemp.VIBEFATTNING,"X(23)") 
            SUBSTRING(tidut.UT,25) = sumtidtemp.AONR 
            SUBSTRING(tidut.UT,32) = STRING(sumtidtemp.DELNR,Guru.Konstanter:varforetypchar[1])                      
            SUBSTRING(tidut.UT,36) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.TIMMAR),"->>>>9.99")
            SUBSTRING(tidut.UT,46) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.BELOPP),"->>>>>9") 
            SUBSTRING(tidut.UT,65) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OBELOPP),"->>>>>9")
            SUBSTRING(tidut.UT,54) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.OTIMMAR),"->>>>>9.99")
            SUBSTRING(tidut.UT,73) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESTIM),"->>>9.99")
            SUBSTRING(tidut.UT,82) = 
            STRING((ACCUM TOTAL BY sumtidtemp.DELNR sumtidtemp.RESKOSTDEC),"->>>>>9").                        
            IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
               SUBSTRING(tidut.UT,90) = STRING(sumtidtemp.MED,"Ja/Nej").  
            END.      
            IF fakbilag.PRIS = FALSE THEN DO:  
               ASSIGN
               SUBSTRING(tidut.UT,46,8) = "        "          
               SUBSTRING(tidut.UT,65,8) = "        "                            
               SUBSTRING(tidut.UT,82,8) = "        ".
            END.          
         END.            
      END.
   END.                                             
   RUN fakbi1_UI.
END PROCEDURE.
PROCEDURE slutesab_UI :
   FIND FIRST BESTTAB WHERE BESTTAB.BESTID = FAKTPLAN.BESTID NO-LOCK NO-ERROR. 
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.     
   IF NOT AVAILABLE AVDELNING THEN DO:
      FIND FIRST AVDELNING NO-LOCK NO-ERROR.
   END.
   FIND FIRST AVDELNINGEXTRA WHERE AVDELNINGEXTRA.AVDELNINGNR = AVDELNING.AVDELNINGNR NO-LOCK NO-ERROR.      
   FIND FIRST JURPERS WHERE JURPERS.JUDID = AVDELNING.POSTANST NO-LOCK NO-ERROR.   
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,1) = str.
   CREATE tidut.         
   ASSIGN
   SUBSTRING(tidut.UT,1) = AVDELNINGEXTRA.DROJRANT + "            " + AVDELNINGEXTRA.F-SKATT.  
   
   IF FAKTURERAD.VFAKTNR = 0 THEN DO:
      FIND FIRST FAKTREGLER WHERE FAKTREGLER.FAKTNR = FAKTPLAN.FAKTNR 
      NO-LOCK NO-ERROR.
      ASSIGN
      varfakturd = DATE(INTEGER(SUBSTRING(STRING(FAKTURERAD.FDELNR,"999999"),3,2)),
                        INTEGER(SUBSTRING(STRING(FAKTURERAD.FDELNR,"999999"),5,2)),
                        INTEGER("20" + SUBSTRING(STRING(FAKTURERAD.FDELNR,"999999"),1,2))).          
      varforfalld = TODAY + FAKTREGLER.FDAGAR.
      REPEAT:
         IF WEEKDAY(varforfalld) = 1 THEN varforfalld = varforfalld + 1.
         IF WEEKDAY(varforfalld) = 7 THEN varforfalld = varforfalld + 2.
         FIND FIRST OVERAVTAB WHERE OVERAVTAB.DATUM = varforfalld NO-LOCK NO-ERROR.
         IF NOT AVAILABLE OVERAVTAB THEN DO:
            LEAVE.
         END.
         ELSE IF OVERAVTAB.EQDAG = 1 THEN varforfalld = varforfalld + 1.
         ELSE IF OVERAVTAB.EQDAG = 7 THEN varforfalld = varforfalld + 1.
      END.               
   END.
   ELSE DO:      
      FIND FIRST FAKTKUNDKONTO WHERE FAKTKUNDKONTO.FAKTNR = FAKTPLAN.FAKTNR AND        
      FAKTKUNDKONTO.VFAKTNR = vfknr NO-LOCK NO-ERROR.
      ASSIGN
      varfakturd = FAKTKUNDKONTO.FAKTDATUM
      varforfalld = FAKTKUNDKONTO.FDATUM.
            
   END.             
   IF FAKTURERAD.VFAKTNR = 0 THEN DO:   
      fnrvar = STRING(FAKTPLAN.FAKTNR) + " " + STRING(FAKTURERAD.FDELNR).                              
   END.   
   ELSE DO:       
      FIND FIRST FAKTSKARP WHERE FAKTSKARP.OMRADE = FAKTPLAN.OMRADE 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FAKTSKARP THEN DO:
         FIND FIRST FAKTSKARP NO-LOCK NO-ERROR. 
      END. 
      fnrvar = STRING(AVDELNING.ELVOMRKOD) + STRING(FAKTURERAD.VFAKTNR,"999999").                         
   END.
   
   CREATE tidut.  
   ASSIGN
   SUBSTRING(tidut.UT,20) = STRING(varforfalld,"99999999")
   SUBSTRING(tidut.UT,30) = BESTTAB.VIBESTID
   SUBSTRING(tidut.UT,40) = STRING(varfakturd,"99999999")
   SUBSTRING(tidut.UT,50) = fnrvar
   SUBSTRING(tidut.UT,60) = STRING(betvar,"->>>>>>>>9.99").
   CREATE tidut.
   CREATE tidut.
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,10) = "Orgnr:" + AVDELNINGEXTRA.ORGNR + "              VATNR:" + AVDELNINGEXTRA.VATNR.
   CREATE tidut.
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,10) = JURPERS.NAMN.
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,10) = AVDELNING.AVDELNINGNAMN.
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,10) = OMRADETAB.GATUADR.
   CREATE tidut.   
   ASSIGN
   SUBSTRING(tidut.UT,10) = OMRADETAB.POSTNR + " " + OMRADETAB.POSTANST + 
   "             " + OMRADETAB.TELVXL.   
   IF FAKTURERAD.VFAKTNR = 0 THEN DO:   
      fnrvar = STRING(FAKTPLAN.FAKTNR) + " " + STRING(FAKTURERAD.FDELNR).                              
   END.   
   ELSE DO:       
      fnrvar = STRING(FAKTURERAD.VFAKTNR).                         
   END.                                 
END PROCEDURE.
PROCEDURE vilkaao_UI:
   FIND FIRST FAKTINTAKTKONT WHERE FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR AND               
   FAKTINTAKTKONT.FDELNR = FAKTURERAD.FDELNR NO-LOCK NO-ERROR. 
   IF NOT AVAILABLE FAKTINTAKTKONT THEN DO:
      OPEN QUERY faktvaonrbq FOR EACH FAKTAONR WHERE FAKTAONR.FAKTNR = FAKTPLAN.FAKTNR NO-LOCK.      
      GET FIRST faktvaonrbq NO-LOCK.                   
      DO WHILE AVAILABLE(FAKTAONR):
         CREATE vilkaaonr.
         ASSIGN 
         vilkaaonr.AONR = FAKTAONR.AONR
         vilkaaonr.DELNR = FAKTAONR.DELNR.
         GET NEXT faktvaonrbq NO-LOCK.
      END.
   END.
   ELSE DO:
      OPEN QUERY finq FOR EACH FAKTINTAKTKONT WHERE 
      FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR AND               
      FAKTINTAKTKONT.FDELNR = FAKTURERAD.FDELNR NO-LOCK.                                        
      GET FIRST finq NO-LOCK.
      DO WHILE AVAILABLE(FAKTINTAKTKONT):
         CREATE vilkaaonr.
         ASSIGN 
         vilkaaonr.AONR = FAKTINTAKTKONT.AONR
         vilkaaonr.DELNR = FAKTINTAKTKONT.DELNR.
         GET NEXT finq NO-LOCK.
      END.
   END.
   OPEN QUERY faktaonrbq FOR EACH vilkaaonr NO-LOCK,
   EACH FAKTAONR WHERE FAKTAONR.FAKTNR = FAKTPLAN.FAKTNR AND  
   FAKTAONR.AONR = vilkaaonr.AONR AND FAKTAONR.DELNR = vilkaaonr.DELNR NO-LOCK,
   EACH AONRTAB WHERE AONRTAB.AONR = FAKTAONR.AONR AND AONRTAB.DELNR = FAKTAONR.DELNR NO-LOCK.       
   GET FIRST faktaonrbq NO-LOCK.                   
   DO WHILE AVAILABLE(FAKTAONR):
      ASSIGN
      hbelopp = 0
      dbelopp = 0.       
      IF FAKTURERAD.VFAKTNR = 0 THEN DO:
          FOR EACH FAKTINTAKTKONT WHERE FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR AND               
          FAKTINTAKTKONT.AONR = FAKTAONR.AONR AND
          FAKTINTAKTKONT.DELNR = FAKTAONR.DELNR AND 
          FAKTINTAKTKONT.VFAKTNR NE  0 NO-LOCK BREAK BY FAKTINTAKTKONT.DELNR: 
             ACCUMULATE FAKTINTAKTKONT.BELOPP (TOTAL BY FAKTINTAKTKONT.DELNR).         
             IF LAST-OF(FAKTINTAKTKONT.DELNR) THEN DO:                        
                hbelopp = (ACCUM TOTAL BY FAKTINTAKTKONT.DELNR FAKTINTAKTKONT.BELOPP).
             END.
          END.   
      END.
      ELSE DO:
         FOR EACH FAKTINTAKTKONT WHERE FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR AND               
         FAKTINTAKTKONT.AONR = FAKTAONR.AONR AND
         FAKTINTAKTKONT.DELNR = FAKTAONR.DELNR AND 
         FAKTINTAKTKONT.VFAKTNR <= FAKTURERAD.VFAKTNR NO-LOCK BREAK BY FAKTINTAKTKONT.DELNR: 
            ACCUMULATE FAKTINTAKTKONT.BELOPP (TOTAL BY FAKTINTAKTKONT.DELNR).         
            IF LAST-OF(FAKTINTAKTKONT.DELNR) THEN DO:                        
               hbelopp = (ACCUM TOTAL BY FAKTINTAKTKONT.DELNR FAKTINTAKTKONT.BELOPP).
            END.
         END.   
      END.
      FOR EACH FAKTINTAKTKONT WHERE FAKTINTAKTKONT.FAKTNR = FAKTPLAN.FAKTNR AND               
      FAKTINTAKTKONT.AONR = FAKTAONR.AONR AND
      FAKTINTAKTKONT.DELNR = FAKTAONR.DELNR AND 
      FAKTINTAKTKONT.VFAKTNR = FAKTURERAD.VFAKTNR NO-LOCK BREAK BY FAKTINTAKTKONT.DELNR: 
         ACCUMULATE FAKTINTAKTKONT.BELOPP (TOTAL BY FAKTINTAKTKONT.DELNR).         
         IF LAST-OF(FAKTINTAKTKONT.DELNR) THEN DO:                        
            dbelopp = (ACCUM TOTAL BY FAKTINTAKTKONT.DELNR FAKTINTAKTKONT.BELOPP).
         END.          
      END.                                                   
      IF FAKTURERAD.VFAKTNR NE 0 THEN hbelopp = hbelopp - dbelopp.   
      CREATE tidut.
      ASSIGN 
      SUBSTRING(tidut.UT,1) = FAKTAONR.AONR + STRING(FAKTAONR.DELNR,"99")
      SUBSTRING(tidut.UT,10) = SUBSTRING(AONRTAB.ORT,1,17)             
      SUBSTRING(tidut.UT,28) = STRING(FAKTAONR.OPRIS,"->>>>>>>9.99")
      SUBSTRING(tidut.UT,41) = STRING(hbelopp,"->>>>>>>9.99")
      SUBSTRING(tidut.UT,54) = STRING(dbelopp,"->>>>>>>9.99").
      IF FAKTAONR.OPRIS - (hbelopp + dbelopp) > 0 THEN
      SUBSTRING(tidut.UT,67) = STRING(FAKTAONR.OPRIS - (hbelopp + dbelopp),"->>>>>>>9.99").         
      GET NEXT faktaonrbq NO-LOCK.   
   END.
END PROCEDURE.

PROCEDURE oresmoms_UI:
   IF momsumma NE 0 THEN DO:
      IF Guru.Konstanter:varforetypval[15] = 0 THEN DO: 
         IF FAKTURERAD.ORESUTJ NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "�resutj�mning"
            SUBSTRING(tidut.UT,varcolon) = ":"      
            SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.ORESUTJ,"->>>>>>>>9.99").
         END.
         CREATE tidut.
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Summa moms"
         SUBSTRING(tidut.UT,varcolon) = ":"      
         SUBSTRING(tidut.ut,62) = STRING(momsumma + FAKTURERAD.ORESUTJ,"->>>>>>>>9.99"). 
      END.
      ELSE DO:
         CREATE tidut.
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Summa moms"
         SUBSTRING(tidut.UT,varcolon) = ":"      
         SUBSTRING(tidut.ut,62) = STRING(momsumma,"->>>>>>>>9.99"). 
         CREATE tidut.
         IF FAKTURERAD.ORESUTJ NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "�resutj�mning"
            SUBSTRING(tidut.UT,varcolon) = ":"      
            SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.ORESUTJ,"->>>>>>>>9.99").
         END.
      END.
   END.
   ELSE DO:
      FIND FIRST FAKTURERINGSTYP WHERE FAKTURERINGSTYP.FAKTTYPID = FAKTURERAD.FAKTTYPID NO-LOCK NO-ERROR.
      IF FAKTURERINGSTYP.OBLMOMS = FALSE THEN DO:
         CREATE tidut.
         ASSIGN                    
         SUBSTRING(tidut.UT,4) = "Merv�rdeskatt utages p� slutfakturan".
      END.
      IF Guru.Konstanter:varforetypval[15] = 0 THEN DO: 
         
      END.
      ELSE DO:
         IF FAKTURERAD.ORESUTJ NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "�resutj�mning"
            SUBSTRING(tidut.UT,varcolon) = ":"      
            SUBSTRING(tidut.ut,62) = STRING(FAKTURERAD.ORESUTJ,"->>>>>>>>9.99").
         END.
      END.
   END.
END PROCEDURE.