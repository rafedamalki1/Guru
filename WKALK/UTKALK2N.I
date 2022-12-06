/*UTKALK2N.I*/

   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = kalk_temp3.ARBKOD                                                                                   
   SUBSTRING(tidut.UT,6) = STRING(kalk_temp3.LOPNR,"99")
   SUBSTRING(tidut.UT,9) = SUBSTRING(kalk_temp3.BENAMNING,1,20)
   SUBSTRING(tidut.UT,30) = STRING(kalk_temp3.ANTAL,">>>9.999")
   SUBSTRING(tidut.UT,39) = kalk_temp3.ENHET.
   summa = 0.
   IF uttyp <= 3 THEN DO:                
      IF uttyp = 1 THEN DO:      
         ASSIGN      
         SUBSTRING(tidut.UT,43) = STRING(kalk_temp3.F1,">>>>9")         
         SUBSTRING(tidut.UT,49) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9")
         SUBSTRING(tidut.UT,55) = STRING(kalk_temp3.UTRUST,">>>>9")
         SUBSTRING(tidut.UT,61) = STRING(kalk_temp3.EA,">>>>9")
         SUBSTRING(tidut.UT,67) = STRING(kalk_temp3.ARBETE,">>>>>>>9")
         SUBSTRING(tidut.UT,76) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9")
         SUBSTRING(tidut.UT,86) = STRING(kalk_temp3.MASKINKOST,">>>>>>9")
         SUBSTRING(tidut.UT,94) = STRING(kalk_temp3.UTRUSTKOST,">>>>>>9")
         SUBSTRING(tidut.UT,102) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9")
         summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
         kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST
         SUBSTRING(tidut.UT,110) = STRING(summa,"->>>>>>>9").               
      END.
      ELSE IF uttyp = 2 THEN DO:  
         IF ejutr = FALSE THEN DO:
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
         END.
         ELSE DO:
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
         END.
      END.
      ELSE DO: 
         IF ejutr = FALSE THEN DO:         
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
         END.
         ELSE DO:
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
         END.
      END.         
   END.
   ELSE DO:
      IF kalkbeftemp.BEF1 NE "" THEN DO: 
         IF kalkbeftemp.PERSMASK1 = TRUE THEN           
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF1,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F1,">>>>9").            
      END.    
      IF kalkbeftemp.BEF2 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK2 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF2,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F2,">>>>9").            
      END.           
      IF kalkbeftemp.BEF3 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK3 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF3,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F3,">>>>9").            
      END.        
      IF kalkbeftemp.BEF4 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK4 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF4,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F4,">>>>9").            
      END.              
      IF kalkbeftemp.BEF5 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK5 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF5,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F5,">>>>9").            
      END.
      IF kalkbeftemp.BEF6 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK6 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF6,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F6,">>>>9").            
      END.
      IF kalkbeftemp.BEF7 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK7 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF7,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F7,">>>>9").            
      END.
      IF kalkbeftemp.BEF8 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK8 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF8,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F8,">>>>9").            
      END.
      IF kalkbeftemp.BEF9 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK9 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF9,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F9,">>>>9").            
      END.
      /*KALKmatris
      IF kalkbeftemp.BEF10 NE "" THEN DO:
         IF kalkbeftemp.PERSMASK10 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(kalkbeftemp.BEF10,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.F10,">>>>9").            
      END.
      */
      CREATE tidut.
      ASSIGN   
      SUBSTRING(tidut.UT,43) = "MASKIN"           
      SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.MASKINTIMMAR,">>>>9").
      CREATE tidut.
      ASSIGN   
      SUBSTRING(tidut.UT,43) = "UTRUSTN"           
      SUBSTRING(tidut.UT,51) = STRING(kalk_temp3.UTRUST,">>>>9").
      ASSIGN
      SUBSTRING(tidut.UT,57) = STRING(kalk_temp3.EA,">>>>9")
      SUBSTRING(tidut.UT,63) = STRING(kalk_temp3.ARBETE,">>>>>>>9")
      SUBSTRING(tidut.UT,72) = STRING(kalk_temp3.MATERIEL,"->>>>>>>9")
      SUBSTRING(tidut.UT,82) = STRING(kalk_temp3.MASKINKOST,">>>>>>9")
      SUBSTRING(tidut.UT,90) = STRING(kalk_temp3.UTRUSTKOST,">>>>>>9")
      SUBSTRING(tidut.UT,98) = STRING(kalk_temp3.OVRIGT + kalk_temp3.ENTRP,">>>>>>9")
      summa = kalk_temp3.ARBETE + kalk_temp3.MATERIEL + kalk_temp3.MASKINKOST + kalk_temp3.OVRIGT + 
      kalk_temp3.ENTRP + kalk_temp3.UTRUSTKOST
      SUBSTRING(tidut.UT,106) = STRING(summa,"->>>>>>>9").
      CREATE tidut.
   END.
