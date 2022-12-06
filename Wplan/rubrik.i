   uttyp = 0.
   IF KALKBEF.BEF1 NE "" THEN DO: 
      IF KALKBEF.PERSMASK1 = TRUE THEN       
      uttyp = uttyp + 1.
   END.   
   IF KALKBEF.BEF2 NE "" THEN DO:
      IF KALKBEF.PERSMASK2 = TRUE THEN      
      uttyp = uttyp + 1.
   END.   
   IF KALKBEF.BEF3 NE "" THEN DO:
      IF KALKBEF.PERSMASK3 = TRUE THEN      
      uttyp = uttyp + 1.
   END.   
   IF KALKBEF.BEF4 NE "" THEN DO:
      IF KALKBEF.PERSMASK4 = TRUE THEN      
      uttyp = uttyp + 1.
   END.   
   IF KALKBEF.BEF5 NE "" THEN DO:      
      IF KALKBEF.PERSMASK5 = TRUE THEN
      uttyp = uttyp + 1.
   END.   
   IF KALKBEF.BEF6 NE "" THEN DO:     
      IF KALKBEF.PERSMASK6 = TRUE THEN
      uttyp = uttyp + 1.
   END.   
   IF KALKBEF.BEF7 NE "" THEN DO:      
      IF KALKBEF.PERSMASK7 = TRUE THEN
      uttyp = uttyp + 1.
   END. 
   IF KALKBEF.BEF8 NE "" THEN DO:      
      IF KALKBEF.PERSMASK8 = TRUE THEN
      uttyp = uttyp + 1.
   END.
   IF KALKBEF.BEF9 NE "" THEN DO:
      IF KALKBEF.PERSMASK9 = TRUE THEN      
      uttyp = uttyp + 1.
   END.   
   /*KALKmatris
   IF KALKBEF.BEF10 NE "" THEN DO:      
      IF KALKBEF.PERSMASK10 = TRUE THEN
      uttyp = uttyp + 1.
   END.        
   */
   IF uttyp <= 3 THEN DO:
      IF uttyp = 1 THEN DO:
         ASSIGN
         str=                                                                    
"====.==.====================.========.===.=====.=====.=====.=====.========.=========.=======.=======.=======.=========". 
         CREATE tidut.    
         CREATE tidut.
         SUBSTRING(tidut.UT,43) = "TIMMAR:".
         SUBSTRING(tidut.UT,67) = "KRONOR:".         
         CREATE tidut.       
         SUBSTRING(tidut.UT,1) = str.                     
         CREATE tidut.      
         SUBSTRING(tidut.UT,102) = "ÖVRIGT/".
         CREATE tidut.
         {RUBRIK2.I}
         ASSIGN           
         SUBSTRING(tidut.UT,43) = CAPS(SUBSTRING(KALKBEF.BEF1,1,5))
         SUBSTRING(tidut.UT,49) = "MASKI" 
         SUBSTRING(tidut.UT,55) = "UTRUS"                                          
         SUBSTRING(tidut.UT,61) = "EA"
         SUBSTRING(tidut.UT,67) = "ARBETE"
         SUBSTRING(tidut.UT,76) = "MATERIEL"
         SUBSTRING(tidut.UT,86) = "MASKIN"
         SUBSTRING(tidut.UT,94) = "UTRUSTN"
         SUBSTRING(tidut.UT,102) = "ENTREPR"
         SUBSTRING(tidut.UT,110) = "SUMMA".                                                                            
         CREATE tidut.                   
         SUBSTRING(tidut.UT,1) = str.           
      END.
      ELSE IF uttyp = 2 THEN DO:   
         IF ejutr = FALSE THEN DO:
            ASSIGN
            str=                                                                    
   "====.==.====================.========.===.=====.=====.=====.=====.=====.========.=========.=======.=======.=======.=========". 
            CREATE tidut.    
            CREATE tidut.
            SUBSTRING(tidut.UT,43) = "TIMMAR:".
            SUBSTRING(tidut.UT,73) = "KRONOR:".         
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.                     
            CREATE tidut.         
            SUBSTRING(tidut.UT,108) = "ÖVRIGT/".
            CREATE tidut.
            {RUBRIK2.I}
            ASSIGN           
            SUBSTRING(tidut.UT,43) = CAPS(SUBSTRING(KALKBEF.BEF1,1,5))                                                                          
            SUBSTRING(tidut.UT,49) = CAPS(SUBSTRING(KALKBEF.BEF2,1,5))                         
            SUBSTRING(tidut.UT,55) = "MASKI"        
            SUBSTRING(tidut.UT,61) = "UTRUS"
            SUBSTRING(tidut.UT,67) = "EA"
            SUBSTRING(tidut.UT,73) = "ARBETE"
            SUBSTRING(tidut.UT,82) = "MATERIEL"
            SUBSTRING(tidut.UT,92) = "MASKIN"
            SUBSTRING(tidut.UT,100) = "UTRUSTN"
            SUBSTRING(tidut.UT,108) = "ENTREPR"
            SUBSTRING(tidut.UT,116) = "SUMMA".            
            CREATE tidut.                   
            SUBSTRING(tidut.UT,1) = str.  
         END.
         ELSE DO:
            ASSIGN
            str=                                                                    
   "====.==.====================.========.===.=====.=====.=====.=====.========.=========.=======.=======.=========". 
            CREATE tidut.    
            CREATE tidut.
            SUBSTRING(tidut.UT,43) = "TIMMAR:".
            SUBSTRING(tidut.UT,67) = "KRONOR:".         
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.                     
            CREATE tidut.         
            SUBSTRING(tidut.UT,94) = "ÖVRIGT/".
            CREATE tidut.
            {RUBRIK2.I}
            ASSIGN           
            SUBSTRING(tidut.UT,43) = CAPS(SUBSTRING(KALKBEF.BEF1,1,5))                                                                          
            SUBSTRING(tidut.UT,49) = CAPS(SUBSTRING(KALKBEF.BEF2,1,5))                         
            SUBSTRING(tidut.UT,55) = "MASKI"                    
            SUBSTRING(tidut.UT,61) = "EA"
            SUBSTRING(tidut.UT,67) = "ARBETE"
            SUBSTRING(tidut.UT,76) = "MATERIEL"
            SUBSTRING(tidut.UT,86) = "MASKIN"            
            SUBSTRING(tidut.UT,94) = "ENTREPR"
            SUBSTRING(tidut.UT,102) = "SUMMA".            
            CREATE tidut.                   
            SUBSTRING(tidut.UT,1) = str.
         END.
      END.
      /*ELSE IF Guru.Konstanter:globforetag = "elpa" AND uttyp = 3 THEN DO: 
         IF ejutr = FALSE THEN DO:         
            ASSIGN
            str=                                                                    
   "====.==.====================.========.===.=====.=====.=====.=====.=====.=====.========.=========.=======.=======.=======.=========". 
            CREATE tidut.    
            CREATE tidut.
            SUBSTRING(tidut.UT,43) = "TIMMAR:".
            SUBSTRING(tidut.UT,79) = "KRONOR:".         
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.                     
            CREATE tidut.  
            SUBSTRING(tidut.UT,114) = "ÖVRIGT/".
            CREATE tidut.
            {RUBRIK2.I}
            ASSIGN           
            SUBSTRING(tidut.UT,43) = CAPS(SUBSTRING(KALKBEF.BEF1,1,5))                                                                          
            SUBSTRING(tidut.UT,49) = CAPS(SUBSTRING(KALKBEF.BEF2,1,5))                         
            SUBSTRING(tidut.UT,55) = CAPS(SUBSTRING(KALKBEF.BEF5,1,5))
            SUBSTRING(tidut.UT,61) = "MASKI"
            SUBSTRING(tidut.UT,67) = "UTRUS"
            SUBSTRING(tidut.UT,73) = "EA"
            SUBSTRING(tidut.UT,79) = "ARBETE"
            SUBSTRING(tidut.UT,88) = "MATERIEL"
            SUBSTRING(tidut.UT,98) = "MASKIN"
            SUBSTRING(tidut.UT,106) = "UTRUSTN"
            SUBSTRING(tidut.UT,114) = "ENTREPR"
            SUBSTRING(tidut.UT,122) = "SUMMA".                                                                       
            CREATE tidut.                   
            SUBSTRING(tidut.UT,1) = str.               
         END.
         ELSE DO:
            ASSIGN
            str=                                                                    
   "====.==.====================.========.===.=====.=====.=====.=====.=====.========.=========.=======.=======.=========". 
            CREATE tidut.    
            CREATE tidut.
            SUBSTRING(tidut.UT,43) = "TIMMAR:".
            SUBSTRING(tidut.UT,73) = "KRONOR:".         
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.                     
            CREATE tidut.  
            SUBSTRING(tidut.UT,114) = "ÖVRIGT/".
            CREATE tidut.
            {RUBRIK2.I}
            ASSIGN           
            SUBSTRING(tidut.UT,43) = CAPS(SUBSTRING(KALKBEF.BEF1,1,5))                                                                          
            SUBSTRING(tidut.UT,49) = CAPS(SUBSTRING(KALKBEF.BEF2,1,5))                         
            SUBSTRING(tidut.UT,55) = CAPS(SUBSTRING(KALKBEF.BEF5,1,5))
            SUBSTRING(tidut.UT,61) = "MASKI"            
            SUBSTRING(tidut.UT,67) = "EA"
            SUBSTRING(tidut.UT,73) = "ARBETE"
            SUBSTRING(tidut.UT,82) = "MATERIEL"
            SUBSTRING(tidut.UT,92) = "MASKIN"            
            SUBSTRING(tidut.UT,100) = "ENTREPR"
            SUBSTRING(tidut.UT,108) = "SUMMA".                                                                       
            CREATE tidut.                   
            SUBSTRING(tidut.UT,1) = str.               
         END.
      END.*/
      ELSE DO: 
         IF ejutr = FALSE THEN DO:         
            ASSIGN
            str=                                                                    
   "====.==.====================.========.===.=====.=====.=====.=====.=====.=====.========.=========.=======.=======.=======.=========". 
            CREATE tidut.    
            CREATE tidut.
            SUBSTRING(tidut.UT,43) = "TIMMAR:".
            SUBSTRING(tidut.UT,79) = "KRONOR:".         
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.                     
            CREATE tidut.  
            SUBSTRING(tidut.UT,114) = "ÖVRIGT/".
            CREATE tidut.
            {RUBRIK2.I}
            ASSIGN           
            SUBSTRING(tidut.UT,43) = CAPS(SUBSTRING(KALKBEF.BEF1,1,5))                                                                          
            SUBSTRING(tidut.UT,49) = CAPS(SUBSTRING(KALKBEF.BEF2,1,5))                         
            SUBSTRING(tidut.UT,55) = CAPS(SUBSTRING(KALKBEF.BEF3,1,5))
            SUBSTRING(tidut.UT,61) = "MASKI"
            SUBSTRING(tidut.UT,67) = "UTRUS"
            SUBSTRING(tidut.UT,73) = "EA"
            SUBSTRING(tidut.UT,79) = "ARBETE"
            SUBSTRING(tidut.UT,88) = "MATERIEL"
            SUBSTRING(tidut.UT,98) = "MASKIN"
            SUBSTRING(tidut.UT,106) = "UTRUSTN"
            SUBSTRING(tidut.UT,114) = "ENTREPR"
            SUBSTRING(tidut.UT,122) = "SUMMA".                                                                       
            CREATE tidut.                   
            SUBSTRING(tidut.UT,1) = str.               
         END.
         ELSE DO:
            ASSIGN
            str=                                                                    
   "====.==.====================.========.===.=====.=====.=====.=====.=====.========.=========.=======.=======.=========". 
            CREATE tidut.    
            CREATE tidut.
            SUBSTRING(tidut.UT,43) = "TIMMAR:".
            SUBSTRING(tidut.UT,73) = "KRONOR:".         
            CREATE tidut.       
            SUBSTRING(tidut.UT,1) = str.                     
            CREATE tidut.  
            SUBSTRING(tidut.UT,114) = "ÖVRIGT/".
            CREATE tidut.
            {RUBRIK2.I}
            ASSIGN           
            SUBSTRING(tidut.UT,43) = CAPS(SUBSTRING(KALKBEF.BEF1,1,5))                                                                          
            SUBSTRING(tidut.UT,49) = CAPS(SUBSTRING(KALKBEF.BEF2,1,5))                         
            SUBSTRING(tidut.UT,55) = CAPS(SUBSTRING(KALKBEF.BEF3,1,5))
            SUBSTRING(tidut.UT,61) = "MASKI"            
            SUBSTRING(tidut.UT,67) = "EA"
            SUBSTRING(tidut.UT,73) = "ARBETE"
            SUBSTRING(tidut.UT,82) = "MATERIEL"
            SUBSTRING(tidut.UT,92) = "MASKIN"            
            SUBSTRING(tidut.UT,100) = "ENTREPR"
            SUBSTRING(tidut.UT,108) = "SUMMA".                                                                       
            CREATE tidut.                   
            SUBSTRING(tidut.UT,1) = str.               
         END.
      END.
   END.
   ELSE DO:
      ASSIGN
      str=                                                                    
"====.==.====================.========.===.=======.=====.=====.========.=========.=======.=======.=======.=========". 
      CREATE tidut.    
      CREATE tidut.
      SUBSTRING(tidut.UT,43) = "TIMMAR:".
      SUBSTRING(tidut.UT,63) = "KRONOR:".      
      CREATE tidut.       
      SUBSTRING(tidut.UT,1) = str.                     
      CREATE tidut.      
      SUBSTRING(tidut.UT,98) = "ÖVRIGT/".
      CREATE tidut.
      {RUBRIK2.I}
      ASSIGN        
      SUBSTRING(tidut.UT,43) = "BEFATT."                                                                          
      SUBSTRING(tidut.UT,51) = "TIMMAR"                                  
      SUBSTRING(tidut.UT,57) = "EA"
      SUBSTRING(tidut.UT,63) = "ARBETE"
      SUBSTRING(tidut.UT,72) = "MATERIEL"
      SUBSTRING(tidut.UT,82) = "MASKIN"
      SUBSTRING(tidut.UT,90) = "UTRUSTN"
      SUBSTRING(tidut.UT,98) = "ENTREPR"
      SUBSTRING(tidut.UT,106) = "SUMMA".       
      CREATE tidut.                
      SUBSTRING(tidut.UT,1) = str.
   END.
