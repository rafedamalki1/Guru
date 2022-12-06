/*UTKALK2.I*/

   CREATE tidut.      
   ASSIGN  
   SUBSTRING(tidut.UT,1) = kalktemp3.ARBKOD                                                                                   
   SUBSTRING(tidut.UT,6) = STRING(kalktemp3.LOPNR,"99")
   SUBSTRING(tidut.UT,9) = SUBSTRING(kalktemp3.BENAMNING,1,20)
   SUBSTRING(tidut.UT,30) = STRING(kalktemp3.ANTAL,">>>9.999")
   SUBSTRING(tidut.UT,39) = kalktemp3.ENHET.
   summa = 0.
   IF uttyp <= 3 THEN DO:                
      IF uttyp = 1 THEN DO:      
         ASSIGN      
         SUBSTRING(tidut.UT,43) = STRING(kalktemp3.F1,">>>>9")         
         SUBSTRING(tidut.UT,49) = STRING(kalktemp3.MASKINTIMMAR,">>>>9")
         SUBSTRING(tidut.UT,55) = STRING(kalktemp3.UTRUST,">>>>9")
         SUBSTRING(tidut.UT,61) = STRING(kalktemp3.EA,">>>>9")
         SUBSTRING(tidut.UT,67) = STRING(kalktemp3.ARBETE,">>>>>>>9")
         SUBSTRING(tidut.UT,76) = STRING(kalktemp3.MATERIEL,"->>>>>>>9")
         SUBSTRING(tidut.UT,86) = STRING(kalktemp3.MASKINKOST,">>>>>>9")
         SUBSTRING(tidut.UT,94) = STRING(kalktemp3.UTRUSTKOST,">>>>>>9")
         SUBSTRING(tidut.UT,102) = STRING(kalktemp3.OVRIGT + kalktemp3.ENTRP,">>>>>>9")
         summa = kalktemp3.ARBETE + kalktemp3.MATERIEL + kalktemp3.MASKINKOST + kalktemp3.OVRIGT + 
         kalktemp3.ENTRP + kalktemp3.UTRUSTKOST
         SUBSTRING(tidut.UT,110) = STRING(summa,"->>>>>>>9").               
      END.
      ELSE IF uttyp = 2 THEN DO:  
         IF ejutr = FALSE THEN DO:
            ASSIGN              
            SUBSTRING(tidut.UT,43) = STRING(kalktemp3.F1,">>>>9")
            SUBSTRING(tidut.UT,49) = STRING(kalktemp3.F2,">>>>9")
            SUBSTRING(tidut.UT,55) = STRING(kalktemp3.MASKINTIMMAR,">>>>9")
            SUBSTRING(tidut.UT,61) = STRING(kalktemp3.UTRUST,">>>>9")                                  
            SUBSTRING(tidut.UT,67) = STRING(kalktemp3.EA,">>>>9")
            SUBSTRING(tidut.UT,73) = STRING(kalktemp3.ARBETE,">>>>>>>9")
            SUBSTRING(tidut.UT,82) = STRING(kalktemp3.MATERIEL,"->>>>>>>9")
            SUBSTRING(tidut.UT,92) = STRING(kalktemp3.MASKINKOST,">>>>>>9")
            SUBSTRING(tidut.UT,100) = STRING(kalktemp3.UTRUSTKOST,">>>>>>9")
            SUBSTRING(tidut.UT,108) = STRING(kalktemp3.OVRIGT + kalktemp3.ENTRP,">>>>>>9")
            summa = kalktemp3.ARBETE + kalktemp3.MATERIEL + kalktemp3.MASKINKOST + kalktemp3.OVRIGT + 
            kalktemp3.ENTRP + kalktemp3.UTRUSTKOST
            SUBSTRING(tidut.UT,116) = STRING(summa,"->>>>>>>9"). 
         END.
         ELSE DO:
            ASSIGN              
            SUBSTRING(tidut.UT,43) = STRING(kalktemp3.F1,">>>>9")
            SUBSTRING(tidut.UT,49) = STRING(kalktemp3.F2,">>>>9")
            SUBSTRING(tidut.UT,55) = STRING(kalktemp3.MASKINTIMMAR,">>>>9")                                       
            SUBSTRING(tidut.UT,61) = STRING(kalktemp3.EA,">>>>9")
            SUBSTRING(tidut.UT,67) = STRING(kalktemp3.ARBETE,">>>>>>>9")
            SUBSTRING(tidut.UT,76) = STRING(kalktemp3.MATERIEL,"->>>>>>>9")
            SUBSTRING(tidut.UT,86) = STRING(kalktemp3.MASKINKOST,">>>>>>9")            
            SUBSTRING(tidut.UT,94) = STRING(kalktemp3.OVRIGT + kalktemp3.ENTRP,">>>>>>9")
            summa = kalktemp3.ARBETE + kalktemp3.MATERIEL + kalktemp3.MASKINKOST + kalktemp3.OVRIGT + 
            kalktemp3.ENTRP + kalktemp3.UTRUSTKOST
            SUBSTRING(tidut.UT,102) = STRING(summa,"->>>>>>>9"). 
         END.
      END.
      ELSE DO: 
         IF ejutr = FALSE THEN DO:         
            ASSIGN              
            SUBSTRING(tidut.UT,43) = STRING(kalktemp3.F1,">>>>9")
            SUBSTRING(tidut.UT,49) = STRING(kalktemp3.F2,">>>>9")                         
            SUBSTRING(tidut.UT,55) = STRING(kalktemp3.F3,">>>>9")
            SUBSTRING(tidut.UT,61) = STRING(kalktemp3.MASKINTIMMAR,">>>>9")
            SUBSTRING(tidut.UT,67) = STRING(kalktemp3.UTRUST,">>>>9")   
            SUBSTRING(tidut.UT,73) = STRING(kalktemp3.EA,">>>>9")
            SUBSTRING(tidut.UT,79) = STRING(kalktemp3.ARBETE,">>>>>>>9")
            SUBSTRING(tidut.UT,88) = STRING(kalktemp3.MATERIEL,"->>>>>>>9")
            SUBSTRING(tidut.UT,98) = STRING(kalktemp3.MASKINKOST,">>>>>>9")
            SUBSTRING(tidut.UT,106) = STRING(kalktemp3.UTRUSTKOST,">>>>>>9")
            SUBSTRING(tidut.UT,114) = STRING(kalktemp3.OVRIGT + kalktemp3.ENTRP,">>>>>>9")
            summa = kalktemp3.ARBETE + kalktemp3.MATERIEL + kalktemp3.MASKINKOST + kalktemp3.OVRIGT + 
            kalktemp3.ENTRP + kalktemp3.UTRUSTKOST
            SUBSTRING(tidut.UT,122) = STRING(summa,"->>>>>>>9").  
         END.
         ELSE DO:
            ASSIGN              
            SUBSTRING(tidut.UT,43) = STRING(kalktemp3.F1,">>>>9")
            SUBSTRING(tidut.UT,49) = STRING(kalktemp3.F2,">>>>9")                         
            SUBSTRING(tidut.UT,55) = STRING(kalktemp3.F3,">>>>9")
            SUBSTRING(tidut.UT,61) = STRING(kalktemp3.MASKINTIMMAR,">>>>9")            
            SUBSTRING(tidut.UT,67) = STRING(kalktemp3.EA,">>>>9")
            SUBSTRING(tidut.UT,73) = STRING(kalktemp3.ARBETE,">>>>>>>9")
            SUBSTRING(tidut.UT,82) = STRING(kalktemp3.MATERIEL,"->>>>>>>9")
            SUBSTRING(tidut.UT,92) = STRING(kalktemp3.MASKINKOST,">>>>>>9")            
            SUBSTRING(tidut.UT,100) = STRING(kalktemp3.OVRIGT + kalktemp3.ENTRP,">>>>>>9")
            summa = kalktemp3.ARBETE + kalktemp3.MATERIEL + kalktemp3.MASKINKOST + kalktemp3.OVRIGT + 
            kalktemp3.ENTRP + kalktemp3.UTRUSTKOST
            SUBSTRING(tidut.UT,108) = STRING(summa,"->>>>>>>9").  
         END.
      END.         
   END.
   ELSE DO:
      IF KALKBEF.BEF1 NE "" THEN DO: 
         IF KALKBEF.PERSMASK1 = TRUE THEN           
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF1,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F1,">>>>9").            
      END.    
      IF KALKBEF.BEF2 NE "" THEN DO:
         IF KALKBEF.PERSMASK2 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF2,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F2,">>>>9").            
      END.           
      IF KALKBEF.BEF3 NE "" THEN DO:
         IF KALKBEF.PERSMASK3 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF3,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F3,">>>>9").            
      END.        
      IF KALKBEF.BEF4 NE "" THEN DO:
         IF KALKBEF.PERSMASK4 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF4,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F4,">>>>9").            
      END.              
      IF KALKBEF.BEF5 NE "" THEN DO:
         IF KALKBEF.PERSMASK5 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF5,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F5,">>>>9").            
      END.
      IF KALKBEF.BEF6 NE "" THEN DO:
         IF KALKBEF.PERSMASK6 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF6,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F6,">>>>9").            
      END.
      IF KALKBEF.BEF7 NE "" THEN DO:
         IF KALKBEF.PERSMASK7 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF7,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F7,">>>>9").            
      END.
      IF KALKBEF.BEF8 NE "" THEN DO:
         IF KALKBEF.PERSMASK8 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF8,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F8,">>>>9").            
      END.
      IF KALKBEF.BEF9 NE "" THEN DO:
         IF KALKBEF.PERSMASK9 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF9,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F9,">>>>9").            
      END.
      /*KALKmatris
      IF KALKBEF.BEF10 NE "" THEN DO:
         IF KALKBEF.PERSMASK10 = TRUE THEN
         CREATE tidut.
         ASSIGN   
         SUBSTRING(tidut.UT,43) = SUBSTRING(KALKBEF.BEF10,1,7)           
         SUBSTRING(tidut.UT,51) = STRING(kalktemp3.F10,">>>>9").            
      END.
      */
      CREATE tidut.
      ASSIGN   
      SUBSTRING(tidut.UT,43) = "MASKIN"           
      SUBSTRING(tidut.UT,51) = STRING(kalktemp3.MASKINTIMMAR,">>>>9").
      CREATE tidut.
      ASSIGN   
      SUBSTRING(tidut.UT,43) = "UTRUSTN"           
      SUBSTRING(tidut.UT,51) = STRING(kalktemp3.UTRUST,">>>>9").
      ASSIGN
      SUBSTRING(tidut.UT,57) = STRING(kalktemp3.EA,">>>>9")
      SUBSTRING(tidut.UT,63) = STRING(kalktemp3.ARBETE,">>>>>>>9")
      SUBSTRING(tidut.UT,72) = STRING(kalktemp3.MATERIEL,"->>>>>>>9")
      SUBSTRING(tidut.UT,82) = STRING(kalktemp3.MASKINKOST,">>>>>>9")
      SUBSTRING(tidut.UT,90) = STRING(kalktemp3.UTRUSTKOST,">>>>>>9")
      SUBSTRING(tidut.UT,98) = STRING(kalktemp3.OVRIGT + kalktemp3.ENTRP,">>>>>>9")
      summa = kalktemp3.ARBETE + kalktemp3.MATERIEL + kalktemp3.MASKINKOST + kalktemp3.OVRIGT + 
      kalktemp3.ENTRP + kalktemp3.UTRUSTKOST
      SUBSTRING(tidut.UT,106) = STRING(summa,"->>>>>>>9").
      CREATE tidut.
   END.
