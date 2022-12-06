   /*KUURV.I*/
   CREATE tidut.
   ASSIGN
   SUBSTRING(tidut.UT,3) = "DITT URVAL".
/*    SUBSTRING(tidut.UT,1) = "DITT URVAL" */
/*    SUBSTRING(tidut.UT,12) = ":". */
   CREATE tidut.
   IF uppvaltemp.BESTID = "ALLA" THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = SUBSTRING(CAPS(Guru.Konstanter:gbestk),1,11)
      SUBSTRING(tidut.UT,12) = ": " + CAPS(uppvaltemp.BESTID).
   END.
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = SUBSTRING(CAPS(Guru.Konstanter:gbestk),1,11)
      SUBSTRING(tidut.UT,12) = ": " + CAPS(uppvaltemp.BESTID) + " " + CAPS(uppvaltemp.BESTNAMN).
   END.
   CREATE tidut.
   IF uppvaltemp.OMRADE = "ALLA" THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk)
      SUBSTRING(tidut.UT,12)= ": " + CAPS(uppvaltemp.OMRADE).
   END.   
   ELSE DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = CAPS(Guru.Konstanter:gomrk)
      SUBSTRING(tidut.UT,12)= ": " + CAPS(uppvaltemp.OMRNAMN).
   END.
   CREATE tidut.   
   tidut.UT = "FAKTURATYP : " + CAPS(faktyp(uppvaltemp.FAKTTYP)).
   CREATE tidut.
   IF uppvaltemp.TILLFALLFAST = 1 THEN tidut.UT = CAPS(Guru.Konstanter:gtillk) + " " + CAPS(Guru.Konstanter:gaok).
   IF uppvaltemp.TILLFALLFAST = 2 THEN tidut.UT = CAPS(Guru.Konstanter:gfastl) + " " + CAPS(Guru.Konstanter:gaok).
   IF uppvaltemp.TILLFALLFAST = 3 THEN tidut.UT = "BÅDE " + CAPS(Guru.Konstanter:gtillk) + " OCH " + CAPS(Guru.Konstanter:gfastl) + " " + CAPS(Guru.Konstanter:gaok).
   CREATE tidut.
   IF uppvaltemp.PAAV = 1 THEN tidut.UT = "PÅGÅENDE " + CAPS(Guru.Konstanter:gaok).
   IF uppvaltemp.PAAV = 2 THEN tidut.UT = "AVSLUTANDE " + CAPS(Guru.Konstanter:gaok) + " MELLAN " 
   + STRING(uppvaltemp.AVSLUTSTART) + " OCH " + STRING(uppvaltemp.AVSLUTSLUT).
   IF uppvaltemp.PAAV = 3 THEN tidut.UT = "BÅDE PÅGÅENDE OCH AVSLUTANDE " + CAPS(Guru.Konstanter:gaok) + " MELLAN " 
   + STRING(uppvaltemp.AVSLUTSTART) + " OCH " + STRING(uppvaltemp.AVSLUTSLUT).
   IF uppvaltemp.ENDBES = ? THEN DO:
      CREATE tidut.
      tidut.UT = CAPS("Både beställda och ej beställda ") + CAPS(Guru.Konstanter:gaok). 
   END.
   IF uppvaltemp.ENDBES = TRUE THEN DO:
      CREATE tidut.
      tidut.UT = CAPS("Endast beställda ") + CAPS(Guru.Konstanter:gaok). 
   END.
   IF uppvaltemp.ENDBES = FALSE THEN DO:
      CREATE tidut.
      tidut.UT = CAPS("Endast ej beställda ") + CAPS(Guru.Konstanter:gaok). 
   END.
   IF uppvaltemp.VISGODKANDA = TRUE THEN DO:
      CREATE tidut.
      tidut.UT = CAPS("Endast ekonomi och lönekörda uppgifter").
   END.
