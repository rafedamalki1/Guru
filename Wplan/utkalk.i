/*UTKALK.I*/
          
   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = kalk_temp3.ARBKOD                                                                                   
   SUBSTRING(tidut.UT,5) = STRING(kalk_temp3.LOPNR,">99")
   SUBSTRING(tidut.UT,9) = SUBSTRING(kalk_temp3.BENAMNING,1,20)
   SUBSTRING(tidut.UT,30) = STRING(kalk_temp3.ANTAL,">>>9.999")
   SUBSTRING(tidut.UT,39) = kalk_temp3.ENHET.
   summa = 0.
   IF uttyp <= 3 THEN DO:                
      IF uttyp = 1 THEN DO:      
         IF kalk_temp3.F1 > 0 THEN SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9").
         ELSE SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,"->>>9").
         IF kalk_temp3.MASKINTIMMAR > 0 THEN SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9").
         ELSE SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.MASKINTIMMAR,"->>>9").
         IF kalk_temp3.UTRUST > 0 THEN SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.UTRUST,">>>>9").
         ELSE  SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.UTRUST,"->>>9").
         IF kalk_temp3.EA > 0 THEN SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.EA,">>>>9").
         ELSE SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.EA,"->>>9").
         IF kalk_temp3.ARBETE > 0 THEN SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.ARBETE,">>>>>>>9").
         ELSE SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.ARBETE,"->>>>>>9").
         IF kalk_temp3.MATERIEL > 0 THEN SUBSTRING(tidut.UT,76) = STRING(kalk_temp3.MATERIEL,">>>>>>>>9").
         ELSE SUBSTRING(tidut.UT,76) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9").
         IF kalk_temp3.MASKINKOST > 0 THEN SUBSTRING(tidut.UT,86) = STRING(kalk_temp3.MASKINKOST,">>>>>>9").
         ELSE SUBSTRING(tidut.UT,86) = STRING(kalk_temp3.MASKINKOST,"->>>>>9").
         IF kalk_temp3.UTRUSTKOST > 0 THEN SUBSTRING(tidut.UT,94) = STRING(kalk_temp3.UTRUSTKOST,">>>>>>9").
         ELSE SUBSTRING(tidut.UT,94) = STRING(kalk_temp3.UTRUSTKOST,"->>>>>9").
         IF kalk_temp3.OVRIGT + kalk_temp3.ENTRP > 0 THEN SUBSTRING(tidut.UT,102) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9").
         ELSE SUBSTRING(tidut.UT,102) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,"->>>>>9").
         summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
         kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST.
         IF summa > 0 THEN SUBSTRING(tidut.UT,110) = STRING(summa,">>>>>>>>9").               
         ELSE SUBSTRING(tidut.UT,110) = STRING(summa,"->>>>>>>9").               
      END.
      ELSE IF uttyp = 2 THEN DO:  
         IF ejutr = FALSE THEN DO:
            IF kalk_temp3.F1 > 0 THEN SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9").
            ELSE SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,"->>>9").
            IF kalk_temp3.F2 > 0 THEN SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,">>>>9").
            ELSE SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,"->>>9").
            IF kalk_temp3.MASKINTIMMAR > 0 THEN SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9").
            ELSE SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.MASKINTIMMAR,"->>>9").
            IF kalk_temp3.UTRUST > 0 THEN SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.UTRUST,">>>>9").
            ELSE  SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.UTRUST,"->>>9").
            IF kalk_temp3.EA > 0 THEN SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.EA,">>>>9").
            ELSE SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.EA,"->>>9").
            IF kalk_temp3.ARBETE > 0 THEN SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.ARBETE,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.ARBETE,"->>>>>>9").
            IF kalk_temp3.MATERIEL > 0 THEN SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MATERIEL,">>>>>>>>9").
            ELSE SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9").
            IF kalk_temp3.MASKINKOST > 0 THEN SUBSTRING(tidut.UT,92) = STRING(kalk_temp3.MASKINKOST,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,92) = STRING(kalk_temp3.MASKINKOST,"->>>>>9").
            IF kalk_temp3.UTRUSTKOST > 0 THEN SUBSTRING(tidut.UT,100) = STRING(kalk_temp3.UTRUSTKOST,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,100) = STRING(kalk_temp3.UTRUSTKOST,"->>>>>9").
            IF kalk_temp3.OVRIGT + kalk_temp3.ENTRP > 0 THEN SUBSTRING(tidut.UT,108) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,108) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,"->>>>>9").
            summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
            kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST.
            IF summa > 0 THEN SUBSTRING(tidut.UT,116) = STRING(summa,">>>>>>>>9").               
            ELSE SUBSTRING(tidut.UT,116) = STRING(summa,"->>>>>>>9").               
            /*
            ASSIGN              
            SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9")
            SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,">>>>9")
            SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9")
            SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.UTRUST,">>>>9")                                  
            SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.EA,">>>>9")
            SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.ARBETE,">>>>>>>9")
            SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9")
            SUBSTRING(tidut.UT,92) = STRING(kalk_temp3.MASKINKOST,">>>>>>9")
            SUBSTRING(tidut.UT,100) = STRING(kalk_temp3.UTRUSTKOST,">>>>>>9")
            SUBSTRING(tidut.UT,108) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9")
            summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
            kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST
            SUBSTRING(tidut.UT,116) = STRING(summa,"->>>>>>>9"). 
            */
         END.
         ELSE DO:
            IF kalk_temp3.F1 > 0 THEN SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9").
            ELSE SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,"->>>9").
            IF kalk_temp3.F2 > 0 THEN SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,">>>>9").
            ELSE SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,"->>>9").
            IF kalk_temp3.MASKINTIMMAR > 0 THEN SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9").
            ELSE SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.MASKINTIMMAR,"->>>9").
            IF kalk_temp3.EA > 0 THEN SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.EA,">>>>9").
            ELSE SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.EA,"->>>9").
            IF kalk_temp3.ARBETE > 0 THEN SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.ARBETE,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.ARBETE,"->>>>>>9").
            IF kalk_temp3.MATERIEL > 0 THEN SUBSTRING(tidut.UT,76) = STRING(kalk_temp3.MATERIEL,">>>>>>>>9").
            ELSE SUBSTRING(tidut.UT,76) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9").
            IF kalk_temp3.MASKINKOST > 0 THEN SUBSTRING(tidut.UT,86) = STRING(kalk_temp3.MASKINKOST,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,86) = STRING(kalk_temp3.MASKINKOST,"->>>>>9").
            IF kalk_temp3.OVRIGT + kalk_temp3.ENTRP > 0 THEN SUBSTRING(tidut.UT,94) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,94) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,"->>>>>9").
            summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
            kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST.
            IF summa > 0 THEN SUBSTRING(tidut.UT,102) = STRING(summa,">>>>>>>>9").               
            ELSE SUBSTRING(tidut.UT,102) = STRING(summa,"->>>>>>>9").               
            /*
            ASSIGN              
            SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9")
            SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,">>>>9")
            SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9")                                       
            SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.EA,">>>>9")
            SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.ARBETE,">>>>>>>9")
            SUBSTRING(tidut.UT,76) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9")
            SUBSTRING(tidut.UT,86) = STRING(kalk_temp3.MASKINKOST,">>>>>>9")            
            SUBSTRING(tidut.UT,94) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9")
            summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
            kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST
            SUBSTRING(tidut.UT,102) = STRING(summa,"->>>>>>>9"). 
            */
         END.
      END.
      ELSE DO: 
         IF ejutr = FALSE THEN DO:         
            IF kalk_temp3.F1 > 0 THEN SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9").
            ELSE SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,"->>>9").
            IF kalk_temp3.F2 > 0 THEN SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,">>>>9").
            ELSE SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,"->>>9").
            IF kalk_temp3.F3 > 0 THEN SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.F3,">>>>9").
            ELSE SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.F3,"->>>9").
            IF kalk_temp3.MASKINTIMMAR > 0 THEN SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9").
            ELSE SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.MASKINTIMMAR,"->>>9").
            IF kalk_temp3.UTRUST > 0 THEN SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.UTRUST,">>>>9").
            ELSE  SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.UTRUST,"->>>9").
            IF kalk_temp3.EA > 0 THEN SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.EA,">>>>9").
            ELSE SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.EA,"->>>9").
            IF kalk_temp3.ARBETE > 0 THEN SUBSTRING(tidut.UT,79) = STRING(kalk_temp3.ARBETE,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,79) = STRING(kalk_temp3.ARBETE,"->>>>>>9").
            IF kalk_temp3.MATERIEL > 0 THEN SUBSTRING(tidut.UT,88) = STRING(kalk_temp3.MATERIEL,">>>>>>>>9").
            ELSE SUBSTRING(tidut.UT,88) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9").
            IF kalk_temp3.MASKINKOST > 0 THEN SUBSTRING(tidut.UT,98) = STRING(kalk_temp3.MASKINKOST,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,98) = STRING(kalk_temp3.MASKINKOST,"->>>>>9").
            IF kalk_temp3.UTRUSTKOST > 0 THEN SUBSTRING(tidut.UT,106) = STRING(kalk_temp3.UTRUSTKOST,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,106) = STRING(kalk_temp3.UTRUSTKOST,"->>>>>9").
            IF kalk_temp3.OVRIGT + kalk_temp3.ENTRP > 0 THEN SUBSTRING(tidut.UT,114) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,114) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,"->>>>>9").
            summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
            kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST.
            IF summa > 0 THEN SUBSTRING(tidut.UT,122) = STRING(summa,">>>>>>>>9").               
            ELSE SUBSTRING(tidut.UT,122) = STRING(summa,"->>>>>>>9").               
            /*
            ASSIGN              
            SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9")
            SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,">>>>9")                         
            SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.F3,">>>>9")
            SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9")
            SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.UTRUST,">>>>9")   
            SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.EA,">>>>9")
            SUBSTRING(tidut.UT,79) = STRING(kalk_temp3.ARBETE,">>>>>>>9")
            SUBSTRING(tidut.UT,88) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9")
            SUBSTRING(tidut.UT,98) = STRING(kalk_temp3.MASKINKOST,">>>>>>9")
            SUBSTRING(tidut.UT,106) = STRING(kalk_temp3.UTRUSTKOST,">>>>>>9")
            SUBSTRING(tidut.UT,114) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9")
            summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
            kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST
            SUBSTRING(tidut.UT,122) = STRING(summa,"->>>>>>>9").  
            */
         END.
         ELSE DO:
            IF kalk_temp3.F1 > 0 THEN SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9").
            ELSE SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,"->>>9").
            IF kalk_temp3.F2 > 0 THEN SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,">>>>9").
            ELSE SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,"->>>9").
            IF kalk_temp3.F3 > 0 THEN SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.F3,">>>>9").
            ELSE SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.F3,"->>>9").
            IF kalk_temp3.MASKINTIMMAR > 0 THEN SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9").
            ELSE SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.MASKINTIMMAR,"->>>9").
            IF kalk_temp3.EA > 0 THEN SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.EA,">>>>9").
            ELSE SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.EA,"->>>9").
            IF kalk_temp3.ARBETE > 0 THEN SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.ARBETE,">>>>>>>9").
            ELSE SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.ARBETE,"->>>>>>9").
            IF kalk_temp3.MATERIEL > 0 THEN SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MATERIEL,">>>>>>>>9").
            ELSE SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9").
            IF kalk_temp3.MASKINKOST > 0 THEN SUBSTRING(tidut.UT,92) = STRING(kalk_temp3.MASKINKOST,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,92) = STRING(kalk_temp3.MASKINKOST,"->>>>>9").
            IF kalk_temp3.OVRIGT + kalk_temp3.ENTRP > 0 THEN SUBSTRING(tidut.UT,100) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9").
            ELSE SUBSTRING(tidut.UT,100) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,"->>>>>9").
            summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
            kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST.
            IF summa > 0 THEN SUBSTRING(tidut.UT,108) = STRING(summa,">>>>>>>>9").               
            ELSE SUBSTRING(tidut.UT,108) = STRING(summa,"->>>>>>>9").               
                                       /*
            ASSIGN              
            SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9")
            SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.F2,">>>>9")                         
            SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.F3,">>>>9")
            SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9")            
            SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.EA,">>>>9")
            SUBSTRING(tidut.UT,73) = STRING(kalk_temp3.ARBETE,">>>>>>>9")
            SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9")
            SUBSTRING(tidut.UT,92) = STRING(kalk_temp3.MASKINKOST,">>>>>>9")            
            SUBSTRING(tidut.UT,100) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9")
            summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
            kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST
            SUBSTRING(tidut.UT,108) = STRING(summa,"->>>>>>>9").  
            */
         END.
      END.         
   END.
   ELSE DO:
      IF KALKBEF.BEF1 NE "" THEN DO: 
         IF KALKBEF.PERSMASK1 = TRUE THEN           
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF1,1,7).
         IF kalk_temp3.F1 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F1,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F1,"->>>9").         
      END.    
      IF KALKBEF.BEF2 NE "" THEN DO:
         IF KALKBEF.PERSMASK2 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF2,1,7).
         IF kalk_temp3.F2 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F2,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F2,"->>>9").                  
      END.           
      IF KALKBEF.BEF3 NE "" THEN DO:
         IF KALKBEF.PERSMASK3 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF3,1,7).
         IF kalk_temp3.F3 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F3,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F3,"->>>9").         
      END.        
      IF KALKBEF.BEF4 NE "" THEN DO:
         IF KALKBEF.PERSMASK4 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF4,1,7).           
         IF kalk_temp3.F4 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F4,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F4,"->>>9").         
      END.              
      IF KALKBEF.BEF5 NE "" THEN DO:
         IF KALKBEF.PERSMASK5 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF5,1,7).
         IF kalk_temp3.F5 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F5,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F5,"->>>9").                  
      END.
      IF KALKBEF.BEF6 NE "" THEN DO:
         IF KALKBEF.PERSMASK6 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF6,1,7).
         IF kalk_temp3.F6 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F6,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F6,"->>>9").                  
      END.
      IF KALKBEF.BEF7 NE "" THEN DO:
         IF KALKBEF.PERSMASK7 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF7,1,7).
         IF kalk_temp3.F7 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F7,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F7,"->>>9").                  
      END.
      IF KALKBEF.BEF8 NE "" THEN DO:
         IF KALKBEF.PERSMASK8 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF8,1,7).
         IF kalk_temp3.F8 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F8,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F8,"->>>9").                  
      END.
      IF KALKBEF.BEF9 NE "" THEN DO:
         IF KALKBEF.PERSMASK9 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF9,1,7).
         IF kalk_temp3.F9 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F9,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F9,"->>>9").                  
      END.
      /*KALKmatris
      IF KALKBEF.BEF10 NE "" THEN DO:
         IF KALKBEF.PERSMASK10 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF10,1,7).
         IF kalk_temp3.F10 > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F10,">>>>9").
         ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F10,"->>>9").                  
      END.
      */
      CREATE tidut.
      ASSIGN   
      SUBSTRING(tidut.UT,43) = "MASKIN".           
      SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9").
      IF kalk_temp3.MASKINTIMMAR > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9").
      ELSE SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.MASKINTIMMAR,"->>>9").
      CREATE tidut.
      ASSIGN   
      SUBSTRING(tidut.UT,43) = "UTRUSTN".
      IF kalk_temp3.UTRUST > 0 THEN SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.UTRUST,">>>>9").
      ELSE  SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.UTRUST,"->>>9").
      IF kalk_temp3.EA > 0 THEN SUBSTRING(tidut.UT,57) = STRING(kalk_temp3.EA,">>>>9").
      ELSE SUBSTRING(tidut.UT,57) = STRING(kalk_temp3.EA,"->>>9").
      IF kalk_temp3.ARBETE > 0 THEN SUBSTRING(tidut.UT,63) = STRING(kalk_temp3.ARBETE,">>>>>>>9").
      ELSE SUBSTRING(tidut.UT,63) = STRING(kalk_temp3.ARBETE,"->>>>>>9").
      IF kalk_temp3.MATERIEL > 0 THEN SUBSTRING(tidut.UT,72) = STRING(kalk_temp3.MATERIEL,">>>>>>>>9").
      ELSE SUBSTRING(tidut.UT,72) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9").
      IF kalk_temp3.MASKINKOST > 0 THEN SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MASKINKOST,">>>>>>9").
      ELSE SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MASKINKOST,"->>>>>9").
      IF kalk_temp3.UTRUSTKOST > 0 THEN SUBSTRING(tidut.UT,90) = STRING(kalk_temp3.UTRUSTKOST,">>>>>>9").
      ELSE SUBSTRING(tidut.UT,90) = STRING(kalk_temp3.UTRUSTKOST,"->>>>>9").
      IF kalk_temp3.OVRIGT + kalk_temp3.ENTRP > 0 THEN SUBSTRING(tidut.UT,98) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9").
      ELSE SUBSTRING(tidut.UT,98) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,"->>>>>9").           
      summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
      kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST.
      IF summa > 0 THEN SUBSTRING(tidut.UT,106) = STRING(summa,">>>>>>>>9").               
      ELSE SUBSTRING(tidut.UT,106) = STRING(summa,"->>>>>>>9").               
      CREATE tidut.
   END.
