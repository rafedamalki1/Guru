/*OPENEXCEL.I*/
   IF INDEX(kommando,".\") NE 0 THEN DO:
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
         kommando = TRIM(Guru.Konstanter:guruvar) + REPLACE(kommando,".\","").
      END.
      ELSE kommando = TRIM(Guru.Konstanter:wtidvar) + REPLACE(kommando,".\","").
   END.
