/*FAKBILLO.I*/
ACCUMULATE 
sumtidtemp.LONKOST 
(TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.LONTILLAGG).
ACCUMULATE 
sumtidtemp.LONTILLANTAL 
(TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.LONTILLAGG). 
IF LAST-OF(sumtidtemp.LONTILLAGG) THEN DO:
   IF sumtidtemp.LONTILLAGG = "" THEN musz = musz.
   ELSE DO:   
      FIND FIRST sumpers WHERE 
      sumpers.VIBEFATTNING = sumtidtemp.VIBEFATTNING AND             
      sumpers.AONR = sumtidtemp.AONR AND
      sumpers.DELNR = sumtidtemp.DELNR AND                          
      sumpers.MED = sumtidtemp.MED AND 
      sumpers.LONTILLAGG = sumtidtemp.LONTILLAGG NO-ERROR.
      IF NOT AVAILABLE sumpers THEN DO: 
         FIND FIRST sumpers WHERE 
         sumpers.VIBEFATTNING = sumtidtemp.VIBEFATTNING AND             
         sumpers.AONR = sumtidtemp.AONR AND
         sumpers.DELNR = sumtidtemp.DELNR AND                          
         sumpers.MED = sumtidtemp.MED AND 
         sumpers.LONTILLAGG = "" NO-ERROR.
         IF NOT AVAILABLE sumpers THEN CREATE sumpers.               
      END.   
      ASSIGN               
      sumpers.VIBEFATTNING = sumtidtemp.VIBEFATTNING              
      sumpers.AONR = sumtidtemp.AONR
      sumpers.DELNR = sumtidtemp.DELNR                           
      sumpers.MED = sumtidtemp.MED
      sumpers.LONTILLAGG = sumtidtemp.LONTILLAGG
      sumpers.LONKOST = 
      (ACCUM TOTAL BY sumtidtemp.LONTILLAGG sumtidtemp.LONKOST).
      sumpers.LONTILLANTAL = 
      (ACCUM TOTAL BY sumtidtemp.LONTILLAGG sumtidtemp.LONTILLANTAL).
   END.
END.
