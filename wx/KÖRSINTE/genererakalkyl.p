/*genererakalkyl.p
skapa 3 kalkylhuvuden. 1 för varje kalkyltyp ange kalkylnummren i detta program och kör. Du får tre kalkyler*/
/*med alla koder i GURU så att du kan jämföra med EBR:s kalkyler att vi får samma värden*/
DEFINE VARIABLE valar AS INTEGER NO-UNDO.
DEFINE VARIABLE vitkat AS LOGICAL NO-UNDO.
DEFINE VARIABLE utkod AS LOGICAL NO-UNDO.
valar = 2018.
vitkat = false.

utkod = FALSE.
FOR EACH lop3 WHERE lop3.katar = valar:   
   utkod = FALSE.
   IF vitkat = TRUE THEN DO:
      IF SUBSTRING(lop3.ARBKOD,1,1) <= "9" THEN DO:
         IF INTEGER(lop3.ARBKOD) < 800 THEN utkod = TRUE.            
      END.
      IF SUBSTRING(lop3.ARBKOD,1,1) = "R" THEN utkod = TRUE.      
   END.       
   ELSE IF vitkat = FALSE THEN DO:
      utkod = true.
      IF SUBSTRING(lop3.ARBKOD,1,1) <= "9" THEN DO:
         IF INTEGER(lop3.ARBKOD) < 800 THEN utkod = FALSE.            
      END.
      IF SUBSTRING(lop3.ARBKOD,1,1) = "R" THEN utkod = FALSE.
       utkod = true.
   END.    
   IF utkod = TRUE THEN DO:
      CREATE fastkalk.
      ASSIGN
      fastkalk.omrade = "110"
      fastkalk.kalknr = 90004
      FASTKALK.UTRUSTKOST = lop3.UTRUSTKOST
      FASTKALK.UTRUST     = lop3.UTRUST    
      FASTKALK.TYP        = 3       
      FASTKALK.OVRIGT     = lop3.OVRIGT       
      FASTKALK.MATERIEL   = lop3.MATERIEL  
      FASTKALK.MASKINKOST = lop3.MASKINKOST
      FASTKALK.LOPNR      = lop3.LOPNR        
      FASTKALK.FAST       = lop3.FAST      
      FASTKALK.F9         = lop3.F9        
      FASTKALK.F8         = lop3.F8        
      FASTKALK.F7         = lop3.F7        
      FASTKALK.F6         = lop3.F6        
      FASTKALK.F5         = lop3.F5        
      FASTKALK.F4         = lop3.F4        
      FASTKALK.F3         = lop3.F3        
      FASTKALK.F2         = lop3.F2        
     /*KALKmatris
      FASTKALK.F10        = lop3.F10       
      */
      FASTKALK.F1         = lop3.F1           
      FASTKALK.ENHET      = lop3.ENHET     
      FASTKALK.EA         = lop3.EA        
      FASTKALK.BENAMNING  = lop3.BENAMNING 
      FASTKALK.ARBKOD     = lop3.ARBKOD    
      FASTKALK.ARBETE     = lop3.ARBETE    
      FASTKALK.ANTAL      = 1.
   END.                    
END.      
utkod = FALSE.
FOR EACH lop2 WHERE lop2.katar = valar:
   utkod = FALSE.
   IF vitkat = TRUE AND LENGTH(TRIM(lop2.arbkod)) = 2 THEN utkod = TRUE.
   ELSE IF vitkat = FALSE THEN DO:      
       utkod = true.
       IF LENGTH(TRIM(lop2.arbkod)) = 2 THEN utkod = FALSE.
   END.    
   IF utkod = TRUE THEN DO:
      CREATE fastkalk.
      ASSIGN
      fastkalk.omrade = "110"
      fastkalk.kalknr = 90003
      FASTKALK.UTRUSTKOST = lop2.UTRUSTKOST
      FASTKALK.UTRUST     = lop2.UTRUST    
      FASTKALK.TYP        = 2       
      FASTKALK.OVRIGT     = lop2.OVRIGT       
      FASTKALK.MATERIEL   = lop2.MATERIEL  
      FASTKALK.MASKINKOST = lop2.MASKINKOST
      FASTKALK.LOPNR      = lop2.LOPNR        
      FASTKALK.FAST       = lop2.FAST      
      FASTKALK.F9         = lop2.F9        
      FASTKALK.F8         = lop2.F8        
      FASTKALK.F7         = lop2.F7        
      FASTKALK.F6         = lop2.F6        
      FASTKALK.F5         = lop2.F5        
      FASTKALK.F4         = lop2.F4        
      FASTKALK.F3         = lop2.F3        
      FASTKALK.F2         = lop2.F2        
         /*KALKmatris
      FASTKALK.F10        = lop2.F10       
      */
      FASTKALK.F1         = lop2.F1           
      FASTKALK.ENHET      = lop2.ENHET     
      FASTKALK.EA         = lop2.EA        
      FASTKALK.BENAMNING  = lop2.BENAMNING 
      FASTKALK.ARBKOD     = lop2.ARBKOD    
      FASTKALK.ARBETE     = lop2.ARBETE    
      FASTKALK.ANTAL      = 1.
   END.                       
END.             

FOR EACH lop1 WHERE lop1.katar = valar:
   utkod = FALSE.
   IF vitkat = TRUE AND LOP1.ARBKOD BEGINS "G " THEN utkod = TRUE.
   ELSE IF vitkat = FALSE THEN DO:
       utkod = true.
       IF LOP1.ARBKOD BEGINS "G " THEN utkod = FALSE.
   END.    
   IF utkod = TRUE THEN DO:
      CREATE fastkalk.
      ASSIGN
      fastkalk.omrade = "110"
      fastkalk.kalknr = 90002
      FASTKALK.UTRUSTKOST = lop1.UTRUSTKOST
      FASTKALK.UTRUST     = lop1.UTRUST    
      FASTKALK.TYP        = 1       
      FASTKALK.OVRIGT     = lop1.OVRIGT       
      FASTKALK.MATERIEL   = lop1.MATERIEL  
      FASTKALK.MASKINKOST = lop1.MASKINKOST
      FASTKALK.LOPNR      = lop1.LOPNR        
      FASTKALK.FAST       = lop1.FAST      
      FASTKALK.F9         = lop1.F9        
      FASTKALK.F8         = lop1.F8        
      FASTKALK.F7         = lop1.F7        
      FASTKALK.F6         = lop1.F6        
      FASTKALK.F5         = lop1.F5        
      FASTKALK.F4         = lop1.F4        
      FASTKALK.F3         = lop1.F3        
      FASTKALK.F2         = lop1.F2        
         /*KALKmatris
      FASTKALK.F10        = lop1.F10       
      */
      FASTKALK.F1         = lop1.F1           
      FASTKALK.ENHET      = lop1.ENHET     
      FASTKALK.EA         = lop1.EA        
      FASTKALK.BENAMNING  = lop1.BENAMNING 
      FASTKALK.ARBKOD     = lop1.ARBKOD    
      FASTKALK.ARBETE     = lop1.ARBETE    
      FASTKALK.ANTAL      = 1.
   END.                    
END.                   
                       

