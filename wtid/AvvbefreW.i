/*AVVBEFREW.I */
   IF Guru.Konstanter:varforetypval[4] = 1 THEN DO:
      tidallt.PRISTYP = FILL-IN-PRISTYP.
      FILL-IN_VIBEFATTNING = INPUT FRAME {&FRAME-NAME} FILL-IN_VIBEFATTNING.
      IF FILL-IN_VIBEFATTNING NE "" THEN DO:
         FIND FIRST befvaltemp WHERE befvaltemp.VIBEFATTNING = FILL-IN_VIBEFATTNING
         NO-LOCK NO-ERROR.
         
         tidallt.OVERTIDTILL = befvaltemp.BEFATTNING.
      END.
      ELSE tidallt.OVERTIDTILL = FILL-IN_VIBEFATTNING.    
      tidallt.VIBEFATTNING = FILL-IN_VIBEFATTNING.    

      IF tidallt.PRISTYP = "FRÅNVARO." OR tidallt.PRISTYP = "RESTID..." THEN DO:
         FIND FIRST perspristemp WHERE perspristemp.PERSONALKOD = pkod AND
         perspristemp.BEFATTNING = tidallt.PRISTYP AND 
         perspristemp.STARTDATUM <= regdatum AND perspristemp.SLUTDATUM >= regdatum 
         NO-ERROR.
         IF AVAILABLE perspristemp THEN FILL-IN-PRIS = perspristemp.PRIS.
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = pkod
            soktemp.SOKCHAR[3] = tidallt.PRISTYP
            soktemp.SOKCHAR[4] = tidallt.OVERTIDTILL 
            soktemp.SOKDATE[1] = regdatum.
            {SOKANROP.I}
            FILL-IN-PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) AND tidallt.PRISTYP = "EJ.KOSTN." THEN DO:
         FIND FIRST perspristemp WHERE perspristemp.PERSONALKOD = pkod AND
         perspristemp.BEFATTNING = tidallt.PRISTYP AND 
         perspristemp.STARTDATUM <= regdatum AND perspristemp.SLUTDATUM >= regdatum 
         NO-ERROR.
         IF AVAILABLE perspristemp THEN FILL-IN-PRIS = perspristemp.PRIS.
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = pkod
            soktemp.SOKCHAR[3] = tidallt.PRISTYP
            soktemp.SOKCHAR[4] = tidallt.OVERTIDTILL 
            soktemp.SOKDATE[1] = regdatum.
            {SOKANROP.I}
            FILL-IN-PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" AND tidallt.PRISTYP = "EJ.KOSTN." THEN DO:
         FIND FIRST perspristemp WHERE perspristemp.PERSONALKOD = pkod AND
         perspristemp.BEFATTNING = tidallt.PRISTYP AND 
         perspristemp.STARTDATUM <= regdatum AND perspristemp.SLUTDATUM >= regdatum 
         NO-ERROR.
         IF AVAILABLE perspristemp THEN FILL-IN-PRIS = perspristemp.PRIS.
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = pkod
            soktemp.SOKCHAR[3] = tidallt.PRISTYP
            soktemp.SOKCHAR[4] = tidallt.OVERTIDTILL 
            soktemp.SOKDATE[1] = regdatum.
            {SOKANROP.I}
            FILL-IN-PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF (Guru.Konstanter:globforetag = "sund" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" ) AND tidallt.PRISTYP = "FASTPRIS1" THEN DO:
         FIND FIRST perspristemp WHERE perspristemp.PERSONALKOD = pkod AND
         perspristemp.BEFATTNING = tidallt.PRISTYP AND 
         perspristemp.STARTDATUM <= regdatum AND perspristemp.SLUTDATUM >= regdatum 
         NO-ERROR.
         IF AVAILABLE perspristemp THEN FILL-IN-PRIS = perspristemp.PRIS.
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = pkod
            soktemp.SOKCHAR[3] = tidallt.PRISTYP
            soktemp.SOKCHAR[4] = tidallt.OVERTIDTILL 
            soktemp.SOKDATE[1] = regdatum.
            {SOKANROP.I}
            FILL-IN-PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE IF Guru.Konstanter:globforetag = "ELPA" AND tidallt.PRISTYP = "FASTPRIS1" THEN DO:
         FIND FIRST perspristemp WHERE perspristemp.PERSONALKOD = pkod AND
         perspristemp.BEFATTNING = tidallt.PRISTYP AND 
         perspristemp.STARTDATUM <= regdatum AND perspristemp.SLUTDATUM >= regdatum 
         NO-ERROR.
         IF AVAILABLE perspristemp THEN FILL-IN-PRIS = perspristemp.PRIS.
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = pkod
            soktemp.SOKCHAR[3] = tidallt.PRISTYP
            soktemp.SOKCHAR[4] = tidallt.OVERTIDTILL 
            soktemp.SOKDATE[1] = regdatum.
            {SOKANROP.I}
            FILL-IN-PRIS = soktemp.SOKDECI[1].
         END.
      END.
      ELSE DO:
         FIND FIRST perspristemp WHERE perspristemp.PERSONALKOD = pkod AND
         perspristemp.BEFATTNING = tidallt.OVERTIDTILL AND 
         perspristemp.STARTDATUM <= regdatum AND perspristemp.SLUTDATUM >= regdatum 
         NO-ERROR.
         IF AVAILABLE perspristemp THEN FILL-IN-PRIS = perspristemp.PRIS.
         ELSE DO:
            {SOKSTART.I}
            ASSIGN
            soktemp.SOKVAL = 1
            soktemp.SOKINT[1] = Guru.Konstanter:varforetypval[4]
            soktemp.SOKCHAR[2] = pkod
            soktemp.SOKCHAR[3] = tidallt.PRISTYP
            soktemp.SOKCHAR[4] = tidallt.OVERTIDTILL 
            soktemp.SOKDATE[1] = regdatum.
            {SOKANROP.I}
            FILL-IN-PRIS = soktemp.SOKDECI[1].
         END.
      END.
      tidallt.PRIS = FILL-IN-PRIS.
   END.

