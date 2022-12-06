/*KMBER.I*/
      FILL-IN-START = klockan100(FILL-IN-START).
      FILL-IN-SLUT = klockan100(FILL-IN-SLUT).
      IF FILL-IN-SLUT LE klockan100(regstart) THEN DO:
         FILL-IN-START = FILL-IN-SLUT - FILL-IN-KM / 54.
      END.
      ELSE IF FILL-IN-START GE klockan100(regslut) THEN DO:
         FILL-IN-SLUT = FILL-IN-START + FILL-IN-KM / 54.
      END.
      FILL-IN-SLUT = klockan60(FILL-IN-SLUT).
      FILL-IN-START = klockan60(FILL-IN-START).   
      IF FILL-IN-SLUT > 24 THEN FILL-IN-SLUT = 24.
      IF FILL-IN-START < 0 THEN FILL-IN-START = 0.
