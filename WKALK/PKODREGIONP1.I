/*PKODREGIONP1.I */
IF SUBSTRING(ptemp.ARBKOD,2,1) = " " THEN DO:
   IF INTEGER(SUBSTRING(ptemp.ARBKOD,2,2)) < 99 THEN ptemp.REGION = TRUE.
END.
IF Guru.Konstanter:varforetypval[41] = 1 THEN DO:
   IF ptemp.TYP = 1 THEN DO:
      IF SUBSTRING(ptemp.ARBKOD,1,1) = "G" THEN .
      ELSE IF SUBSTRING(ptemp.ARBKOD,1,1) = "U" THEN.
      ELSE DO:         
         DELETE ptemp.
      END.
   END.
   ELSE IF ptemp.TYP = 7 THEN DO:
      IF SUBSTRING(ptemp.ARBKOD,1,1) = "G" THEN DELETE ptemp.
      ELSE IF SUBSTRING(ptemp.ARBKOD,1,1) = "U" THEN DELETE ptemp.
      ELSE DO: 
         ptemp.REGION = ?.      
      END.
   END.
END.
