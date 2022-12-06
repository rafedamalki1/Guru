/*VIFAKAPPA.I*/

FIND faktyptemp WHERE faktyptemp.FAKTTYP = FAKTPLAN.FAKTTYP NO-ERROR.
vartyp = faktyptemp.TYP. 
IF vartyp = 5 THEN DO:
   IF FAKTPLAN.FAKTTYPUNDER = 2 OR FAKTPLAN.FAKTTYPUNDER = 4 THEN vartyp = 52.
END. 


FIND FIRST FAKTURERINGSTYP WHERE FAKTURERINGSTYP.FAKTTYPID = FAKTKRED.FAKTTYPID NO-LOCK NO-ERROR.
IF FAKTURERINGSTYP.TIDIGAREACONTO = TRUE THEN vartyp = 3.    
/*FAKTFOR*/
IF Guru.Konstanter:globforetag = "ESAN" THEN DO:
   RUN huvudesab_UI.
   RUN summaesab_UI.
   RUN slutesab_UI.
   FIND FIRST fakbilag NO-ERROR.   
   IF AVAILABLE fakbilag THEN RUN bilaga_UI.
END.   
ELSE IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO: 
   RUN huvud_UI.
   RUN summagran_UI.
END.
ELSE DO:
   RUN huvud_UI.
   RUN summa_UI.
END.  
/* **********************  Internal Procedures  *********************** */

PROCEDURE anmark_UI :
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
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "BILAGA".   
   IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
      OPEN QUERY finq FOR EACH FAKTINTAKTKONTKRED WHERE 
      FAKTINTAKTKONTKRED.FAKTNR = FAKTPLAN.FAKTNR AND               
      FAKTINTAKTKONTKRED.FDELNR = FAKTKRED.FDELNR 
      NO-LOCK.                                        
      GET FIRST finq NO-LOCK.      
      IF AVAILABLE FAKTINTAKTKONTKRED THEN DO:
         CREATE tidut.
         CREATE tidut.
         SUBSTRING(tidut.UT,1) = "Ingående " + LC(Guru.Konstanter:gaok) + " :".      
      END.
      DO WHILE AVAILABLE(FAKTINTAKTKONTKRED):         
         FIND FIRST AONRTAB WHERE AONRTAB.AONR = FAKTINTAKTKONTKRED.AONR AND 
         AONRTAB.DELNR = FAKTINTAKTKONTKRED.DELNR
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
         SUBSTRING(tidut.UT,1) = "Ingående " + LC(Guru.Konstanter:gaok) + " :".      
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
      OPEN QUERY faktkostq FOR EACH FAKTKOSTKRED WHERE FAKTKOSTKRED.FAKTNR = FAKTPLAN.FAKTNR AND          
      FAKTKOSTKRED.FDELNR = FAKTKRED.FDELNR AND FAKTKOSTKRED.MED = TRUE  USE-INDEX FAKTKOST NO-LOCK.
      GET FIRST faktkostq NO-LOCK. 
      IF AVAILABLE FAKTKOSTKRED THEN DO:   
         CREATE tidut.
         CREATE tidut.
         CREATE tidut.            
         ASSIGN          
         tidut.UT = "Ingående externa fakturor :".
         CREATE tidut.
         CREATE tidut. 
         CREATE tidut. 
         SUBSTRING(tidut.UT,48) = "TOTAL".
         CREATE tidut.
         ASSIGN                        
         SUBSTRING(tidut.UT,1) = "VER-NR" 
         SUBSTRING(tidut.UT,17) = CAPS(Guru.Konstanter:gaok)  
         SUBSTRING(tidut.UT,28) = "BENÄMNING"
         SUBSTRING(tidut.UT,48) = "KOSTNAD".  
         CREATE tidut.
         ASSIGN                        
         tidut.UT = "===============.==========.===================.=========".
      END.
      DO WHILE AVAILABLE(FAKTKOSTKRED):
         CREATE tidut.
         ASSIGN                       
         SUBSTRING(tidut.UT,17) = FAKTKOSTKRED.AONR 
         SUBSTRING(tidut.UT,24) = STRING(FAKTKOSTKRED.DELNR,Guru.Konstanter:varforetypchar[1])
         SUBSTRING(tidut.UT,1) = FAKTKOSTKRED.VERNR
         SUBSTRING(tidut.UT,28) = STRING(FAKTKOSTKRED.BENAMNING,"X(19)")
         SUBSTRING(tidut.UT,48) = STRING(FAKTKOSTKRED.MASKKOST + FAKTKOSTKRED.MTRL + 
         FAKTKOSTKRED.OVRKR + FAKTKOSTKRED.PERSKOST + FAKTKOSTKRED.TRAKTKOST + 
         (FAKTKOSTKRED.MASKKOST * FAKTKOSTKRED.FRTJPA / 100) +
         (FAKTKOSTKRED.MTRL * FAKTKOSTKRED.MTRLPA / 100),"->>>>>>>9").
         /*
         FIND FIRST KOSTREG WHERE KOSTREG.AONR = FAKTKOSTKRED.AONR AND 
         KOSTREG.DELNR = FAKTKOSTKRED.DELNR AND KOSTREG.RADNR = FAKTKOSTKRED.RADNR
         USE-INDEX KOST NO-LOCK NO-ERROR.                                 
         IF AVAILABLE KOSTREG THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = KOSTREG.FAKTNR
            SUBSTRING(tidut.UT,28) = STRING(KOSTREG.BENAMNING,"X(19)")
            SUBSTRING(tidut.UT,48) = STRING(KOSTREG.MASKKOST + KOSTREG.MTRL + 
            KOSTREG.OVRKR + KOSTREG.PERSKOST + KOSTREG.TRAKTKOST + 
            (KOSTREG.MASKKOST * FAKTKOSTKRED.FRTJPA / 100) +
            (KOSTREG.MTRL * FAKTKOSTKRED.MTRLPA / 100),"->>>>>>>9").
         END.
         */
         GET NEXT faktkostq NO-LOCK.   
      END.
   END.   
   IF fakbilag.FRI = TRUE THEN DO: 
      CREATE tidut. 
      OPEN QUERY faktfriq FOR EACH FAKTFRIAKRED WHERE FAKTFRIAKRED.FAKTNR = FAKTPLAN.FAKTNR AND          
      FAKTFRIAKRED.FDELNR = FAKTKRED.FDELNR USE-INDEX FAKTFRIA NO-LOCK.  
      GET FIRST faktfriq NO-LOCK.
      IF AVAILABLE FAKTFRIAKRED THEN DO:   
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
         DO WHILE AVAILABLE(FAKTFRIAKRED):  
            IF FAKTFRIAKRED.FAKTTEXT = "DUBBEL-KLICKA PÅ DENNA RAD FÖR NYUPPLÄGG" THEN musz = musz.
            ELSE DO:
               CREATE tidut.             
               ASSIGN              
               SUBSTRING(tidut.UT,1) = FAKTFRIAKRED.AONR + " " + STRING(FAKTFRIAKRED.DELNR,Guru.Konstanter:varforetypchar[1]) + " " + STRING(FAKTFRIAKRED.FAKTTEXT,"X(38)") 
               SUBSTRING(tidut.UT,52) = STRING(FAKTFRIAKRED.ENHET,"X(6)")
               SUBSTRING(tidut.UT,59) = STRING(FAKTFRIAKRED.ANTAL,"->>>>>>>9")
               SUBSTRING(tidut.UT,69) = STRING(FAKTFRIAKRED.PRIS_ENHET,"->>>>>>>9")
               SUBSTRING(tidut.UT,79) = STRING(FAKTFRIAKRED.TOTALT,"->>>>>>>9").                     
            END.
            GET NEXT faktfriq NO-LOCK.   
         END.         
      END.   
   END.
   IF Guru.Konstanter:globforetag = "ELPA" OR Guru.Konstanter:globforetag = "GRAN"  THEN DO:
      RUN VISKONTK.P (INPUT FAKTKRED.FAKTNR,INPUT FAKTKRED.FDELNR).
   END.                        
END PROCEDURE.

PROCEDURE fakbi1_UI :
   
   {muswait.i}
   
   {FAKBIL.I}
                                                 
   RUN fakbi2_UI.                           
   
END PROCEDURE.


PROCEDURE fakbi2_UI :
   FIND FIRST sumpers NO-ERROR.
   IF AVAILABLE sumpers THEN DO:
      CREATE tidut.            
      CREATE tidut.
      IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Alla lönetillägg och traktamenten :". 
      END. 
      ELSE IF fakbilag.TIDMED = TRUE THEN DO:  
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Ingående lönetillägg och traktamenten :". 
      END.
      ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:  
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Ej ingående lönetillägg och traktamenten :".
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
      SUBSTRING(tidut.UT,65) = "LÖNETILLÄGG"              
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
                                                                                          
   str =      
   "===============================================================================".      
   CREATE tidut.   
   SUBSTRING(tidut.UT,65) = STRING(TODAY). 
   CREATE tidut.       
   FIND FIRST FAKTNAMNKRED WHERE FAKTNAMNKRED.FAKTURNR = FAKTPLAN.FAKTNR AND 
   FAKTNAMNKRED.FDELNR = FAKTKRED.FDELNR NO-LOCK NO-ERROR.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.     
   IF NOT AVAILABLE AVDELNING THEN DO:
      FIND FIRST AVDELNING NO-LOCK NO-ERROR.
   END.
   CREATE tidut. 
   SUBSTRING(tidut.UT,1) = AVDELNING.AVDELNINGNAMN.                    
   /*FAKTFOR*/
   IF Guru.Konstanter:globforetag = "ESAN" THEN DO:
      SUBSTRING(tidut.UT,1) = "".
      SUBSTRING(tidut.UT,1) = "Elektrosandberg".                                
   END.
   IF FAKTKRED.VKREDIT = 0 THEN DO:   
      ASSIGN                                        
      SUBSTRING(tidut.UT,38) = "Detta är endast en arbetskopia"
      SUBSTRING(tidut.UT,70) = STRING(TODAY).            
      CREATE tidut.
      CREATE tidut.
      ASSIGN                    
      SUBSTRING(tidut.UT,38) = "Faktura Nr :"                              
      SUBSTRING(tidut.UT,51) = STRING(FAKTPLAN.FAKTNR) + " " +
      STRING(FAKTKRED.FDELNR).                              
   END.   
   ELSE DO:        
      IF Guru.Konstanter:globforetag = "ESAN" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:
         FIND FIRST FAKTSKARP WHERE FAKTSKARP.OMRADE = FAKTPLAN.OMRADE 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE FAKTSKARP THEN DO:
            FIND FIRST FAKTSKARP NO-LOCK NO-ERROR. 
         END.
         CREATE tidut.
         ASSIGN                    
         SUBSTRING(tidut.UT,38) = "Faktura Nr :"                              
         SUBSTRING(tidut.UT,51) =  
         STRING(AVDELNING.ELVOMRKOD,"99") + STRING(FAKTSKARP.ARKIVSTALLE,"XX") +
         STRING(FAKTKRED.VKREDIT,"99999").                   
      END.
      ELSE DO:
         FIND FIRST FAKTSKARP WHERE FAKTSKARP.OMRADE = FAKTPLAN.OMRADE 
         NO-LOCK NO-ERROR.
         IF NOT AVAILABLE FAKTSKARP THEN DO:
            FIND FIRST FAKTSKARP NO-LOCK NO-ERROR. 
         END.
         CREATE tidut.
         ASSIGN                    
         SUBSTRING(tidut.UT,38) = "Faktura Nr :"                              
         SUBSTRING(tidut.UT,51) = STRING(FAKTPLAN.FAKTNR) + " " +  
         STRING(YEAR(FAKTKRED.BOKDATUM)) + FAKTSKARP.ARKIVSTALLE +
         STRING(FAKTKRED.VKREDIT).                   
      END.
      
   END.
   /*
   CREATE tidut.  
   ASSIGN                   
   SUBSTRING(tidut.UT,10) = "Faktureringsadress".
   */
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,10) = FAKTNAMNKRED.BESTNAMN.   
   CREATE tidut.   
   ASSIGN                   
   SUBSTRING(tidut.UT,10) = FAKTNAMNKRED.BESTALLARE.
   CREATE tidut.
   CREATE tidut.
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = SUBSTRING(FAKTNAMNKRED.FAKADRESS,1,25).
   CREATE tidut.
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = SUBSTRING(FAKTNAMNKRED.FAKADRESS,27,25).   
   CREATE tidut.
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = STRING(FAKTNAMNKRED.FAKPNR,"999 99")
   SUBSTRING(tidut.UT,18) = FAKTNAMNKRED.FAKORT.  
   
   CREATE tidut.
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Vår referens :"
   SUBSTRING(tidut.UT,16) = FAKTNAMNKRED.VARREF                               
   SUBSTRING(tidut.UT,40) = "Er referens :"
   SUBSTRING(tidut.UT,54) = FAKTNAMNKRED.KONTAKT. 
   CREATE tidut.       
   CREATE tidut.       
   ASSIGN                                     
   SUBSTRING(tidut.UT,1) = "Pris enligt         :"  
   SUBSTRING(tidut.UT,23) = "Kreditfaktura". 
   CREATE tidut. 
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Arbete              :" 
   SUBSTRING(tidut.UT,23) = FAKTPLAN.NAMN.                   
   IF AVAILABLE FAKTKRED THEN DO:
      ASSIGN
      retvar = 1
      ednum = 1
      ednum3 = LENGTH(FAKTKRED.FAKTXT)
      retvar = INDEX(FAKTKRED.FAKTXT,CHR(10),ednum)
      edtecken = 50
      edtext = FAKTKRED.FAKTXT
      tidtext = "".  
      {ANMARK2.I}                             
   END.
                         
END PROCEDURE.
PROCEDURE huvudesab_UI :
     /*HUVUD*/                                                                                                     
   str =      
   "===============================================================================".      
   CREATE tidut.   
   SUBSTRING(tidut.UT,65) = STRING(TODAY). 
   CREATE tidut.
   CREATE tidut.       
   FIND FIRST FAKTNAMNKRED WHERE FAKTNAMNKRED.FAKTURNR = FAKTPLAN.FAKTNR AND 
   FAKTNAMNKRED.FDELNR = FAKTKRED.FDELNR NO-LOCK NO-ERROR.
   FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = FAKTPLAN.OMRADE NO-LOCK NO-ERROR.
   FIND FIRST AVDELNING WHERE AVDELNING.AVDELNINGNR = OMRADETAB.AVDELNINGNR NO-LOCK NO-ERROR.     
   IF NOT AVAILABLE AVDELNING THEN DO:
      FIND FIRST AVDELNING NO-LOCK NO-ERROR.
   END.      
   IF FAKTKRED.VKREDIT = 0 THEN DO:   
      ASSIGN                                        
      SUBSTRING(tidut.UT,38) = "Detta är endast en arbetskopia"
      SUBSTRING(tidut.UT,70) = STRING(TODAY).            
      CREATE tidut.
      CREATE tidut.
      ASSIGN                    
      SUBSTRING(tidut.UT,38) = "Faktura Nr :"                              
      SUBSTRING(tidut.UT,51) = STRING(FAKTPLAN.FAKTNR) + " " +
      STRING(FAKTKRED.FDELNR).                              
   END.   
   CREATE tidut.
   ASSIGN                   
   SUBSTRING(tidut.UT,10) = FAKTNAMNKRED.BESTNAMN.   
   CREATE tidut.   
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = SUBSTRING(FAKTNAMNKRED.FAKADRESS,1,25).
   IF SUBSTRING(FAKTNAMNKRED.FAKADRESS,27,25) NE "" THEN DO:
      CREATE tidut.
      SUBSTRING(tidut.UT,10) =
      SUBSTRING(FAKTNAMNKRED.FAKADRESS,27,25).
   END.   
   CREATE tidut.
   ASSIGN                      
   SUBSTRING(tidut.UT,10) = STRING(FAKTNAMNKRED.FAKPNR,"999 99")
   SUBSTRING(tidut.UT,18) = FAKTNAMNKRED.FAKORT. 
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
   IF FAKTNAMNKRED.KONTAKT NE "" THEN DO:      
      ASSIGN
      SUBSTRING(tidut.UT,4) = "Er referens  :"
      SUBSTRING(tidut.UT,16) = FAKTNAMNKRED.KONTAKT. 
   END.   
   CREATE tidut.
   IF FAKTNAMNKRED.BESTALLARE NE "" THEN DO:  
      ASSIGN       
      SUBSTRING(tidut.UT,4) = "Best av      :"
      SUBSTRING(tidut.UT,16) = FAKTNAMNKRED.BESTALLARE. 
   END.
   CREATE tidut.
   IF FAKTNAMNKRED.VARREF NE "" THEN DO:
      ASSIGN                   
      SUBSTRING(tidut.UT,4) = "Vår referens :"
      SUBSTRING(tidut.UT,16) = FAKTNAMNKRED.VARREF.
   END.
   CREATE tidut.       
   CREATE tidut.       
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Pris Kreditfaktura  :"  
   SUBSTRING(tidut.UT,23) = faktyp(FAKTPLAN.FAKTTYP). 
   CREATE tidut. 
   ASSIGN                   
   SUBSTRING(tidut.UT,1) = "Arbete              :" 
   SUBSTRING(tidut.UT,23) = FAKTPLAN.NAMN.                   
   IF AVAILABLE FAKTKRED THEN DO:
      ASSIGN
      retvar = 1
      ednum = 1
      ednum3 = LENGTH(FAKTKRED.FAKTXT)
      retvar = INDEX(FAKTKRED.FAKTXT,CHR(10),ednum)
      edtecken = 50
      edtext = FAKTKRED.FAKTXT
      tidtext = "".  
      {ANMARK2.I}                             
   END.
                         
END PROCEDURE.
PROCEDURE summaesab_UI :
   /*
   "Fastpris" "Löpande räkning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   {muswait.i}
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut.      
   varcolon = 25.
   IF (FAKTURERINGSTYP.SLUT = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      OPEN QUERY faktstartq FOR EACH FAKTSTARTKRED WHERE 
      FAKTSTARTKRED.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTSTARTKRED.FDELNR = FAKTKRED.FDELNR AND 
      FAKTSTARTKRED.FAKTURERAD = TRUE NO-LOCK.
      IF vartyp = 2 THEN DO:
         GET FIRST faktstartq NO-LOCK.
         DO WHILE AVAILABLE(FAKTSTARTKRED):      
            IF FAKTSTARTKRED.START NE "" THEN DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTARTKRED.FAKTNR AND FAKTAVTALAONR.START = FAKTSTARTKRED.START 
               NO-LOCK.      
            END.
            ELSE DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTARTKRED.FAKTNR AND FAKTAVTALAONR.PLANDATUM = FAKTSTARTKRED.PLANDATUM NO-LOCK.
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
   IF (FAKTURERINGSTYP.SLUT = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      GET FIRST faktstartq NO-LOCK.
      IF AVAILABLE FAKTSTARTKRED THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ingående poster     ". 
      END.
      DO WHILE AVAILABLE(FAKTSTARTKRED):      
         IF FAKTSTARTKRED.START = "START" THEN DO:
            CREATE tidut.      
            ASSIGN                  
            SUBSTRING(tidut.UT,1) = "Vid arbetets start  " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTARTKRED.START = "SLUT" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Vid arbetets slut   " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTARTKRED.START = "" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Enligt plan         " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,27,30) = FAKTSTARTKRED.FRITEXT
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         GET NEXT faktstartq NO-LOCK.
      END.          
   END.
   ELSE DO:      
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,30) = "Ingående poster     ".       
      CREATE tidut. 
      IF FAKTKRED.BELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Arbetskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.BELOPP,"->>>>>>>>9.99").
      END.  
      IF FAKTKRED.OBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Övertidskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.OBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.TBELOPP NE 0 THEN DO:               
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Traktamenteskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.TBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.LONKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Lönetilläggskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.LONKOST,"->>>>>>>>9.99").
      END. 
      IF FAKTKRED.RESKOSTDEC NE 0 THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Resersättning"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.RESKOSTDEC,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.MTRLKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Materielkostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.MTRLKOST,"->>>>>>>>9.99").  
      END.  
      IF FAKTKRED.OVRKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Övrigakostnader"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.OVRKOST,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.KOSTBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Externa fakturor"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.KOSTBELOPP,"->>>>>>>>9.99").
      END.                                        
   END.
   IF vartyp = 52 THEN DO:     
      OPEN QUERY faktuppq FOR EACH FAKTUPPARBKRED WHERE FAKTUPPARBKRED.FAKTNR = FAKTURERAD.FAKTNR AND
      FAKTUPPARBKRED.FDELNR = FAKTKRED.FDELNR AND FAKTUPPARBKRED.FAKTURERAD = TRUE NO-LOCK.     
      GET FIRST faktuppq NO-LOCK.
      IF AVAILABLE FAKTUPPARBKRED THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ingående poster     ".
      END.
      DO WHILE AVAILABLE(FAKTUPPARBKRED):
         CREATE tidut.      
         ASSIGN                                                      
         SUBSTRING(tidut.UT,1) = "Enligt plan        " + STRING(FAKTUPPARBKRED.FAKT%,">>9") + " %"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,27,30) = FAKTUPPARBKRED.FRITEXT 
         SUBSTRING(tidut.UT,62) = STRING(FAKTUPPARBKRED.FAKTBELOPP,"->>>>>>>>9.99").   
         GET NEXT faktuppq NO-LOCK.
      END.          
   END.
   CREATE tidut.
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Denna faktura"
   SUBSTRING(tidut.UT,varcolon) = ":"
   SUBSTRING(tidut.ut,62) = STRING(FAKTKRED.TOTPRIS,"->>>>>>>>9.99").     
   CREATE tidut.                 
   momsumma = 0.
   FOR EACH FAKTMOMSKRED WHERE FAKTMOMSKRED.FAKTNR = FAKTPLAN.FAKTNR AND 
   FAKTMOMSKRED.FDELNR = FAKTKRED.FDELNR NO-LOCK BREAK BY FAKTMOMSKRED.MOMSID: 
      ACCUMULATE FAKTMOMSKRED.BELOPP (TOTAL BY FAKTMOMSKRED.MOMSID).
      ACCUMULATE FAKTMOMSKRED.MOMSBELOPP (TOTAL BY FAKTMOMSKRED.MOMSID).
      IF LAST-OF(FAKTMOMSKRED.MOMSID) THEN DO:                        
         IF (ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP) NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
/*            SUBSTRING(tidut.UT,1) = SUBSTRING(FAKTMOMSKRED.FRITEXT,1,30)*/
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,31) = "Moms " + STRING(FAKTMOMSKRED.MOMSEXTERNT,">>9.99") + " % på"                                                                                                                    
            SUBSTRING(tidut.UT,49) = STRING((ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.BELOPP),"->>>>>>>9.99") 
            SUBSTRING(tidut.ut,62) = STRING((ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP),"->>>>>>>>9.99").             
            momsumma = momsumma + ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP.
        END.
      END.                                      
   END.
   RUN oresmoms_UI.
   CREATE tidut.  
   CREATE tidut.                               
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Att betala"
   SUBSTRING(tidut.UT,varcolon) = ":".
   IF FAKTURERINGSTYP.SLUT = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
         SUBSTRING(tidut.ut,62) = STRING(FAKTKRED.TOTPRIS + 
         FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ /*- FAKTKRED.AVGAR*/) + " skr".    
      END.
      ELSE SUBSTRING(tidut.ut,62) = 
      STRING(FAKTKRED.TOTPRIS + FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ,"->>>>>>>>9.99") + " skr".    
   END.
   ELSE SUBSTRING(tidut.ut,62) = 
   STRING(FAKTKRED.TOTPRIS + FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ,"->>>>>>>>9.99") + " skr".    
   CREATE tidut.
   CREATE tidut. 
   /*
   "Fastpris" "Löpande räkning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
                                    
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
   
   IF FAKTKRED.VKREDIT = 0 THEN DO:
      FIND FIRST FAKTREGLER WHERE FAKTREGLER.FAKTNR = FAKTPLAN.FAKTNR 
      NO-LOCK NO-ERROR.
      ASSIGN
      varfakturd = DATE(INTEGER(SUBSTRING(STRING(FAKTKRED.FDELNR,"999999"),3,2)),
                        INTEGER(SUBSTRING(STRING(FAKTKRED.FDELNR,"999999"),5,2)),
                        INTEGER("20" + SUBSTRING(STRING(FAKTKRED.FDELNR,"999999"),1,2))).          
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
      FIND FIRST FAKTKUNDKONTOKRED WHERE FAKTKUNDKONTOKRED.FAKTNR = FAKTPLAN.FAKTNR AND        
      FAKTKUNDKONTOKRED.VKREDIT = vfknr NO-LOCK NO-ERROR.
      ASSIGN
      varfakturd = FAKTKUNDKONTOKRED.FAKTDATUM
      varforfalld = FAKTKUNDKONTOKRED.FDATUM.
            
   END.             
   IF FAKTKRED.VKREDIT = 0 THEN DO:   
      fnrvar = STRING(FAKTPLAN.FAKTNR) + " " + STRING(FAKTKRED.FDELNR).                              
   END.   
   ELSE DO:       
      FIND FIRST FAKTSKARP WHERE FAKTSKARP.OMRADE = FAKTPLAN.OMRADE 
      NO-LOCK NO-ERROR.
      IF NOT AVAILABLE FAKTSKARP THEN DO:
         FIND FIRST FAKTSKARP NO-LOCK NO-ERROR. 
      END. 
      fnrvar = STRING(AVDELNING.ELVOMRKOD) + STRING(FAKTKRED.VKREDIT).                         
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
   IF FAKTKRED.VKREDIT = 0 THEN DO:   
      fnrvar = STRING(FAKTPLAN.FAKTNR) + " " + STRING(FAKTKRED.FDELNR).                              
   END.   
   ELSE DO:       
      fnrvar = STRING(FAKTKRED.VKREDIT).                         
   END. 
                                
END PROCEDURE.
PROCEDURE summa_UI :
   /*
   "Fastpris" "Löpande räkning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut. 
   varcolon = 25.
   IF (FAKTURERINGSTYP.SLUT = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      OPEN QUERY faktstartq FOR EACH FAKTSTARTKRED WHERE 
      FAKTSTARTKRED.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTSTARTKRED.FDELNR = FAKTKRED.FDELNR AND 
      FAKTSTARTKRED.FAKTURERAD = TRUE NO-LOCK.
      IF vartyp = 2 THEN DO:
         GET FIRST faktstartq NO-LOCK.
         DO WHILE AVAILABLE(FAKTSTARTKRED):      
            IF FAKTSTARTKRED.START NE "" THEN DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTARTKRED.FAKTNR AND FAKTAVTALAONR.START = FAKTSTARTKRED.START 
               NO-LOCK.      
            END.
            ELSE DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTARTKRED.FAKTNR AND FAKTAVTALAONR.PLANDATUM = FAKTSTARTKRED.PLANDATUM NO-LOCK.
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
   IF vartyp = 52 THEN DO:     
      OPEN QUERY faktuppq FOR EACH FAKTUPPARBKRED WHERE FAKTUPPARBKRED.FAKTNR = FAKTURERAD.FAKTNR AND
      FAKTUPPARBKRED.FDELNR = FAKTKRED.FDELNR AND FAKTUPPARBKRED.FAKTURERAD = TRUE NO-LOCK.     
      GET FIRST faktuppq NO-LOCK.
      IF AVAILABLE FAKTUPPARBKRED THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ingående poster     ".
      END.
      DO WHILE AVAILABLE(FAKTUPPARBKRED):
         CREATE tidut.      
         ASSIGN                                                      
         SUBSTRING(tidut.UT,1) = "Enligt plan        " + STRING(FAKTUPPARBKRED.FAKT%,">>9") + " %"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,27,30) = FAKTUPPARBKRED.FRITEXT 
         SUBSTRING(tidut.UT,62) = STRING(FAKTUPPARBKRED.FAKTBELOPP,"->>>>>>>>9.99").   
         GET NEXT faktuppq NO-LOCK.
      END.          
   END.
   IF (FAKTURERINGSTYP.SLUT = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      GET FIRST faktstartq NO-LOCK.
      IF AVAILABLE FAKTSTARTKRED THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ingående poster     ".
      END.
      DO WHILE AVAILABLE(FAKTSTARTKRED):      
         IF FAKTSTARTKRED.START = "START" THEN DO:
            CREATE tidut.      
            ASSIGN                  
            SUBSTRING(tidut.UT,1) = "Vid arbetets start  " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTARTKRED.START = "SLUT" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Vid arbetets slut   " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTARTKRED.START = "" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Enligt plan         " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,27,30) = FAKTSTARTKRED.FRITEXT
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         GET NEXT faktstartq NO-LOCK.
      END.          
   END.
   ELSE DO:      
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,30) = "Ingående poster     ".      
      CREATE tidut. 
      IF FAKTKRED.BELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Arbetskostnad"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.BELOPP,"->>>>>>>>9.99").
      END.  
      IF FAKTKRED.OBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Övertidskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.OBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.TBELOPP NE 0 THEN DO:               
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Traktamenteskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.TBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.LONKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Lönetilläggskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.LONKOST,"->>>>>>>>9.99").
      END. 
      IF FAKTKRED.RESKOSTDEC NE 0 THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Resersättning"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.RESKOSTDEC,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.MTRLKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Materielkostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.MTRLKOST,"->>>>>>>>9.99").  
      END.  
      IF FAKTKRED.OVRKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Övrigakostnader"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.OVRKOST,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.KOSTBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Externa fakturor"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.KOSTBELOPP,"->>>>>>>>9.99").
      END.                         
      
   END.
   CREATE tidut.
   CREATE tidut.
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Denna faktura"
   SUBSTRING(tidut.UT,varcolon) = ":"
   SUBSTRING(tidut.ut,62) = STRING(FAKTKRED.TOTPRIS,"->>>>>>>>9.99").     
   CREATE tidut.                 
   momsumma = 0.
  
   FOR EACH FAKTMOMSKRED WHERE FAKTMOMSKRED.FAKTNR = FAKTPLAN.FAKTNR AND 
   FAKTMOMSKRED.FDELNR = FAKTKRED.FDELNR NO-LOCK BREAK BY FAKTMOMSKRED.MOMSID: 
      ACCUMULATE FAKTMOMSKRED.BELOPP (TOTAL BY FAKTMOMSKRED.MOMSID).
      ACCUMULATE FAKTMOMSKRED.MOMSBELOPP (TOTAL BY FAKTMOMSKRED.MOMSID).
      IF LAST-OF(FAKTMOMSKRED.MOMSID) THEN DO:                        
         IF (ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP) NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
/*            SUBSTRING(tidut.UT,1) = SUBSTRING(FAKTMOMSKRED.FRITEXT,1,30)*/
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,31) = "Moms " + STRING(FAKTMOMSKRED.MOMSEXTERNT,">>9.99") + " % på"                                                                                                                    
            SUBSTRING(tidut.UT,49) = STRING((ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.BELOPP),"->>>>>>>9.99") 
            SUBSTRING(tidut.ut,62) = STRING((ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP),"->>>>>>>>9.99").             
            momsumma = momsumma + ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP.
        END.
      END.                                      
   END.  
   RUN oresmoms_UI.
   
   CREATE tidut.  
   CREATE tidut.                               
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Att betala"
   SUBSTRING(tidut.UT,varcolon) = ":".
   IF FAKTURERINGSTYP.SLUT = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
         SUBSTRING(tidut.ut,62) = STRING(FAKTKRED.TOTPRIS + 
         FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ /*- FAKTKRED.AVGAR*/) + " skr".    
      END.
      ELSE SUBSTRING(tidut.ut,62) = 
      STRING(FAKTKRED.TOTPRIS + FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ,"->>>>>>>>9.99") + " skr".    
   END.
   ELSE SUBSTRING(tidut.ut,62) = 
   STRING(FAKTKRED.TOTPRIS + FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ,"->>>>>>>>9.99") + " skr".    
   CREATE tidut.
   CREATE tidut. 
   /*
   "Fastpris" "Löpande räkning" "A-contofakt." "Takprisfakt." "Avtal"
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
   IF AVAILABLE fakbilag THEN RUN bilaga_UI.
                            
END PROCEDURE.

PROCEDURE summagran_UI :
   /*
   "Fastpris" "Löpande räkning" "A-contofakt." "Takprisfakt." "Avtal"
   */   
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut. 
   CREATE tidut. 
   varcolon = 25.
   IF (FAKTURERINGSTYP.SLUT = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      OPEN QUERY faktstartq FOR EACH FAKTSTARTKRED WHERE 
      FAKTSTARTKRED.FAKTNR = FAKTPLAN.FAKTNR AND
      FAKTSTARTKRED.FDELNR = FAKTKRED.FDELNR AND 
      FAKTSTARTKRED.FAKTURERAD = TRUE NO-LOCK.
      IF vartyp = 2 THEN DO:
         GET FIRST faktstartq NO-LOCK.
         DO WHILE AVAILABLE(FAKTSTARTKRED):      
            IF FAKTSTARTKRED.START NE "" THEN DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTARTKRED.FAKTNR AND FAKTAVTALAONR.START = FAKTSTARTKRED.START 
               NO-LOCK.      
            END.
            ELSE DO:
               OPEN QUERY faktavtalq FOR EACH FAKTAVTALAONR WHERE FAKTAVTALAONR.FAKTNR =
               FAKTSTARTKRED.FAKTNR AND FAKTAVTALAONR.PLANDATUM = FAKTSTARTKRED.PLANDATUM NO-LOCK.
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
   IF vartyp = 52 THEN DO:     
      OPEN QUERY faktuppq FOR EACH FAKTUPPARBKRED WHERE FAKTUPPARBKRED.FAKTNR = FAKTURERAD.FAKTNR AND
      FAKTUPPARBKRED.FDELNR = FAKTKRED.FDELNR AND FAKTUPPARBKRED.FAKTURERAD = TRUE NO-LOCK.     
      GET FIRST faktuppq NO-LOCK.
      IF AVAILABLE FAKTUPPARBKRED THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ingående poster     ".
      END.
      DO WHILE AVAILABLE(FAKTUPPARBKRED):
         CREATE tidut.      
         ASSIGN                                                      
         SUBSTRING(tidut.UT,1) = "Enligt plan        " + STRING(FAKTUPPARBKRED.FAKT%,">>9") + " %"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,27,30) = FAKTUPPARBKRED.FRITEXT 
         SUBSTRING(tidut.UT,62) = STRING(FAKTUPPARBKRED.FAKTBELOPP,"->>>>>>>>9.99").   
         GET NEXT faktuppq NO-LOCK.
      END.          
   END.
   IF (FAKTURERINGSTYP.SLUT = FALSE AND vartyp = 3) OR vartyp = 1 OR vartyp = 2 THEN DO:
      GET FIRST faktstartq NO-LOCK.
      IF AVAILABLE FAKTSTARTKRED THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,30) = "Ingående poster     ".
      END.
      DO WHILE AVAILABLE(FAKTSTARTKRED):      
         IF FAKTSTARTKRED.START = "START" THEN DO:
            CREATE tidut.      
            ASSIGN                  
            SUBSTRING(tidut.UT,1) = "Vid arbetets start  " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTARTKRED.START = "SLUT" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Vid arbetets slut   " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         ELSE IF FAKTSTARTKRED.START = "" THEN DO:
            CREATE tidut.      
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Enligt plan         " + STRING(FAKTSTARTKRED.PLAN%,">>9") + " %"
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,27,30) = FAKTSTARTKRED.FRITEXT
            SUBSTRING(tidut.UT,62) = STRING(FAKTSTARTKRED.BELOPP,"->>>>>>>>9.99").
         END.
         GET NEXT faktstartq NO-LOCK.
      END.          
   END.
   ELSE DO:      
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,30) = "Ingående poster     ".      
      CREATE tidut. 
      IF FAKTKRED.BELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Arbetskostnad"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.BELOPP,"->>>>>>>>9.99").
      END.  
      IF FAKTKRED.OBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Övertidskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.OBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.TBELOPP NE 0 THEN DO:               
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Traktamenteskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.TBELOPP,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.LONKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Lönetilläggskostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.LONKOST,"->>>>>>>>9.99").
      END. 
      IF FAKTKRED.RESKOSTDEC NE 0 THEN DO:
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Resersättning"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.RESKOSTDEC,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.MTRLKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Materielkostnad"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.MTRLKOST,"->>>>>>>>9.99").  
      END.  
      IF FAKTKRED.OVRKOST NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Övrigakostnader"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.OVRKOST,"->>>>>>>>9.99").
      END.
      IF FAKTKRED.KOSTBELOPP NE 0 THEN DO:      
         CREATE tidut.                               
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Externa fakturor"
         SUBSTRING(tidut.UT,varcolon) = ":"
         SUBSTRING(tidut.UT,62) = STRING(FAKTKRED.KOSTBELOPP,"->>>>>>>>9.99").
      END.                                        
   END.
   CREATE tidut.
   CREATE tidut.
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Denna faktura"
   SUBSTRING(tidut.UT,varcolon) = ":"
   SUBSTRING(tidut.ut,62) = STRING(FAKTKRED.TOTPRIS,"->>>>>>>>9.99").     
   CREATE tidut.                 
   momsumma = 0.
  
   FOR EACH FAKTMOMSKRED WHERE FAKTMOMSKRED.FAKTNR = FAKTPLAN.FAKTNR AND 
   FAKTMOMSKRED.FDELNR = FAKTKRED.FDELNR NO-LOCK BREAK BY FAKTMOMSKRED.MOMSID: 
      ACCUMULATE FAKTMOMSKRED.BELOPP (TOTAL BY FAKTMOMSKRED.MOMSID).
      ACCUMULATE FAKTMOMSKRED.MOMSBELOPP (TOTAL BY FAKTMOMSKRED.MOMSID).
      IF LAST-OF(FAKTMOMSKRED.MOMSID) THEN DO:                        
         IF (ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP) NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
/*            SUBSTRING(tidut.UT,1) = SUBSTRING(FAKTMOMSKRED.FRITEXT,1,30)*/
            SUBSTRING(tidut.UT,varcolon) = ":"
            SUBSTRING(tidut.UT,31) = "Moms " + STRING(FAKTMOMSKRED.MOMSEXTERNT,">>9.99") + " % på"                                                                                                                    
            SUBSTRING(tidut.UT,49) = STRING((ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.BELOPP),"->>>>>>>9.99") 
            SUBSTRING(tidut.ut,62) = STRING((ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP),"->>>>>>>>9.99").             
            momsumma = momsumma + ACCUM TOTAL BY FAKTMOMSKRED.MOMSID FAKTMOMSKRED.MOMSBELOPP.
        END.
      END.                                      
   END.  
   RUN oresmoms_UI.
   
   CREATE tidut.  
   CREATE tidut.                               
   ASSIGN                    
   SUBSTRING(tidut.UT,1) = "Att betala"
   SUBSTRING(tidut.UT,varcolon) = ":".
   IF FAKTURERINGSTYP.SLUT = TRUE THEN DO:                
      IF vartyp = 3 THEN DO:
         SUBSTRING(tidut.ut,62) = STRING(FAKTKRED.TOTPRIS + 
         FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ /*- FAKTKRED.AVGAR*/) + " skr".    
      END.
      ELSE SUBSTRING(tidut.ut,62) = 
      STRING(FAKTKRED.TOTPRIS + FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ,"->>>>>>>>9.99") + " skr".    
   END.
   ELSE SUBSTRING(tidut.ut,62) = 
   STRING(FAKTKRED.TOTPRIS + FAKTKRED.MOMSBELOPP + FAKTKRED.ORESUTJ,"->>>>>>>>9.99") + " skr".    
   CREATE tidut.
   CREATE tidut. 
   /*
   "Fastpris" "Löpande räkning" "A-contofakt." "Takprisfakt." "Avtal"
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
   IF AVAILABLE fakbilag THEN RUN bilaga_UI.
                            
END PROCEDURE.

PROCEDURE tidhamt_UI :
   OPEN QUERY faktidq FOR EACH FAKTTIDKRED WHERE FAKTTIDKRED.FAKTNR = FAKTPLAN.FAKTNR AND                  
   FAKTTIDKRED.FDELNR = FAKTKRED.FDELNR NO-LOCK.
   GET FIRST faktidq NO-LOCK.
   DO WHILE AVAILABLE(FAKTTIDKRED):
      CREATE sumtidtemp.      
      ASSIGN                                                  
      sumtidtemp.PERSONALKOD = FAKTTIDKRED.PERSONALKOD
      sumtidtemp.NAMN = FAKTTIDKRED.NAMN 
      sumtidtemp.AONR = FAKTTIDKRED.AONR
      sumtidtemp.DELNR = FAKTTIDKRED.DELNR
      sumtidtemp.TIMMAR = FAKTTIDKRED.TIMMAR
      sumtidtemp.BELOPP = FAKTTIDKRED.BELOPP        
      sumtidtemp.OBELOPP = FAKTTIDKRED.OBELOPP 
      sumtidtemp.TBELOPP = FAKTTIDKRED.TBELOPP             
      sumtidtemp.OTIMMAR = FAKTTIDKRED.OTIMMAR 
      sumtidtemp.LONKOST = FAKTTIDKRED.LONKOST                  
      sumtidtemp.BEFATTNING = FAKTTIDKRED.BEFATTNING      
      sumtidtemp.PERSMASK = FAKTTIDKRED.PERSMASK
      sumtidtemp.TRAKTKOD = FAKTTIDKRED.TRAKTKOD
      sumtidtemp.TRAKTANTAL = FAKTTIDKRED.TRAKTANTAL  
      sumtidtemp.LONTILLAGG = FAKTTIDKRED.LONTILLAGG      
      sumtidtemp.LONTILLANTAL = FAKTTIDKRED.LONTILLANTAL 
      sumtidtemp.PRISA = FAKTTIDKRED.PRISA 
      sumtidtemp.ENDAGS = FAKTTIDKRED.ENDAGS       
      sumtidtemp.MED = FAKTTIDKRED.MED      
      sumtidtemp.PRISTYP = FAKTTIDKRED.PRISTYP
      sumtidtemp.RESTIM = FAKTTIDKRED.DECRESTID
      sumtidtemp.RESKOSTDEC = FAKTTIDKRED.RESKOSTDEC
      sumtidtemp.OTEXTID = FAKTTIDKRED.OTEXTID
      sumtidtemp.DATUM = FAKTTIDKRED.DATUM
      sumtidtemp.START = FAKTTIDKRED.START 
      sumtidtemp.SLUT = FAKTTIDKRED.SLUT
      sumtidtemp.GSTART = FAKTTIDKRED.GSTART 
      sumtidtemp.GSLUT = FAKTTIDKRED.GSLUT
      sumtidtemp.LUNCH = FAKTTIDKRED.LUNCH
      sumtidtemp.OANT1 = FAKTTIDKRED.OANT1.      
      GET NEXT faktidq NO-LOCK.
   END.               
   OPEN QUERY sbq FOR EACH sumtidtemp NO-LOCK,
   EACH BEFATTNINGSTAB WHERE BEFATTNINGSTAB.BEFATTNING = sumtidtemp.BEFATTNING NO-LOCK.
   GET FIRST sbq EXCLUSIVE-LOCK.
   DO WHILE AVAILABLE (sumtidtemp):
      ASSIGN sumtidtemp.VIBEFATTNING = BEFATTNINGSTAB.NAMN
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
   CREATE tidut.
   IF fakbilag.TIDMED = TRUE AND fakbilag.TIDEJMED = TRUE THEN DO:        
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,1) = "All tidskrivning på ingående " + LC(Guru.Konstanter:gaok) + " :". 
   END. 
   ELSE IF fakbilag.TIDMED = TRUE THEN DO:  
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,1) = "Ingående tidskrivning :". 
   END.
   ELSE IF fakbilag.TIDEJMED = TRUE THEN DO:  
      CREATE tidut.                               
      ASSIGN                    
      SUBSTRING(tidut.UT,1) = "Ej ingående tidskrivning :".
   END.      
   CREATE tidut.
   CREATE tidut. 
   CREATE tidut.
   ASSIGN                        
   SUBSTRING(tidut.UT,36) = "ARBETS" 
   SUBSTRING(tidut.UT,46) = "ARBETS" 
   SUBSTRING(tidut.UT,54) = "ÖVERTID" 
   SUBSTRING(tidut.UT,65) = "ÖVERTID"                   
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
PROCEDURE oresmoms_UI:
   IF momsumma NE 0 THEN DO:
      IF Guru.Konstanter:varforetypval[15] = 0 THEN DO: 
         IF FAKTKRED.ORESUTJ NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Öresutjämning"
            SUBSTRING(tidut.UT,varcolon) = ":"      
            SUBSTRING(tidut.ut,62) = STRING(FAKTKRED.ORESUTJ,"->>>>>>>>9.99").
         END.
         CREATE tidut.
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Summa moms"
         SUBSTRING(tidut.UT,varcolon) = ":"      
         SUBSTRING(tidut.ut,62) = STRING(momsumma + FAKTKRED.ORESUTJ,"->>>>>>>>9.99"). 
      END.
      ELSE DO:
         CREATE tidut.
         ASSIGN                    
         SUBSTRING(tidut.UT,1) = "Summa moms"
         SUBSTRING(tidut.UT,varcolon) = ":"      
         SUBSTRING(tidut.ut,62) = STRING(momsumma,"->>>>>>>>9.99"). 
         CREATE tidut.
         IF FAKTKRED.ORESUTJ NE 0 THEN DO:
            CREATE tidut.
            ASSIGN                    
            SUBSTRING(tidut.UT,1) = "Öresutjämning"
            SUBSTRING(tidut.UT,varcolon) = ":"      
            SUBSTRING(tidut.ut,62) = STRING(FAKTKRED.ORESUTJ,"->>>>>>>>9.99").
         END.
      END.
   END.
   
END PROCEDURE.
