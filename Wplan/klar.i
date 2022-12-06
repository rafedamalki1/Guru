/*KLAR.I*/   
   
   ASSIGN
   totea = 0 
   totarb = 0
   totmask = 0
   totmtrl = 0
   totovr = 0
   totalt = 0
   arbtim = 0
   msktim = 0
   bertim = 0
   totutr = 0
   totutrtim = 0.     
   FOR EACH kalk_temp: 
      IF AVAILABLE KALKBEF THEN DO:
         IF KALKBEF.KATAR NE 1998 THEN DO: 
            bertim = bertim + kalk_temp.F1.
         END.   
         IF KALKBEF.EBR1 = 1 THEN arbtim = arbtim + kalk_temp.F1. 
         ELSE msktim = msktim + kalk_temp.F1.    
         IF KALKBEF.EBR2 = 1 THEN arbtim = arbtim + kalk_temp.F2. 
         ELSE msktim = msktim + kalk_temp.F2.
         IF KALKBEF.EBR3 = 1 THEN arbtim = arbtim + kalk_temp.F3. 
         ELSE msktim = msktim + kalk_temp.F3.
         IF KALKBEF.EBR4 = 1 THEN arbtim = arbtim + kalk_temp.F4. 
         ELSE msktim = msktim + kalk_temp.F4.
         IF KALKBEF.EBR5 = 1 THEN arbtim = arbtim + kalk_temp.F5. 
         ELSE msktim = msktim + kalk_temp.F5.
         IF KALKBEF.EBR6 = 1 THEN arbtim = arbtim + kalk_temp.F6. 
         ELSE msktim = msktim + kalk_temp.F6.
         IF KALKBEF.EBR7 = 1 THEN arbtim = arbtim + kalk_temp.F7. 
         ELSE msktim = msktim + kalk_temp.F7.
         IF KALKBEF.EBR8 = 1 THEN arbtim = arbtim + kalk_temp.F8. 
         ELSE msktim = msktim + kalk_temp.F8.
         IF KALKBEF.EBR9 = 1 THEN arbtim = arbtim + kalk_temp.F9. 
         ELSE msktim = msktim + kalk_temp.F9.
         /*KALKmatris
         IF KALKBEF.EBR10 = 1 THEN arbtim = arbtim + kalk_temp.F10. 
         ELSE msktim = msktim + kalk_temp.F10.
         */
      END.
      ASSIGN
      totea = totea + kalk_temp.EA
      totarb = totarb + kalk_temp.ARBETE
      totmask = totmask + kalk_temp.MASKINKOST
      totmtrl = totmtrl + kalk_temp.MATERIEL
      totovr = totovr + kalk_temp.OVRIGT + kalk_temp.ENTRP
      totutr = totutr + kalk_temp.UTRUSTKOST
      totutrtim = totutrtim + kalk_temp.UTRUST.
   END.
      
