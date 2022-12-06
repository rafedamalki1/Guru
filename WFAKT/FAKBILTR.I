/*FAKBILTR.I*/
ACCUMULATE 
sumtidtemp.TBELOPP 
(TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.TRAKTKOD). 
ACCUMULATE 
sumtidtemp.TRAKTANTAL 
(TOTAL BY sumtidtemp.MED BY sumtidtemp.VIBEFATTNING BY sumtidtemp.AONR BY sumtidtemp.DELNR BY sumtidtemp.TRAKTKOD).
IF LAST-OF(sumtidtemp.TRAKTKOD) THEN DO:
   IF sumtidtemp.TRAKTKOD = "" THEN musz = musz.
   ELSE DO:   
      CREATE sumpers.               
      ASSIGN               
      sumpers.VIBEFATTNING = sumtidtemp.VIBEFATTNING              
      sumpers.AONR = sumtidtemp.AONR
      sumpers.DELNR = sumtidtemp.DELNR                           
      sumpers.MED = sumtidtemp.MED
      sumpers.TRAKTKOD = sumtidtemp.TRAKTKOD
      sumpers.TBELOPP = 
      (ACCUM TOTAL BY sumtidtemp.TRAKTKOD sumtidtemp.TBELOPP).
      sumpers.TRAKTANTAL = 
      (ACCUM TOTAL BY sumtidtemp.TRAKTKOD sumtidtemp.TRAKTANTAL).
   END.                             
END.
