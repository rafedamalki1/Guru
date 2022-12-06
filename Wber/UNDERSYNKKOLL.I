
/*     : UNDERSYNKKOLL.I */
       
      IF  Guru.Konstanter:globforetag = "Celpa" OR Guru.Konstanter:globforetag = "gran"  OR Guru.Konstanter:globforetag = "NAEK" OR 
      Guru.Konstanter:globforetag = "ELKB"  THEN RETURN synk.
      ELSE RETURN FALSE.