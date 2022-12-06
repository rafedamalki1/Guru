   /*UNSETUSE.I*/
   IF uservar = "" THEN SETUSERID("","").
   ELSE DO:
      IF uservar = CHR(75) + CHR(85) + CHR(78) + CHR(68) THEN SETUSERID(CHR(75) + CHR(85) + CHR(78) + CHR(68),CHR(75) + CHR(85) + CHR(78) + CHR(68)).      
   END.
