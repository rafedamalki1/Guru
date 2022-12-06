/*OPENEXCELC.I*/
   IF INDEX(excelkommando,".\") NE 0 THEN DO:
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
         excelkommando = TRIM(Guru.Konstanter:guruvar) + REPLACE(excelkommando,".\","").
      END.
      ELSE excelkommando = TRIM(Guru.Konstanter:wtidvar) + REPLACE(excelkommando,".\","").
   END.
