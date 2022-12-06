/*GURUTEXTLADD.I*/

FOR EACH tesxttemp:
   IF tesxttemp.PROGRAM = "AVD" THEN DO:
      ASSIGN
      Guru.Konstanter:gavdl = tesxttemp.EGENTEXTFULL
       Guru.Konstanter:gavdk = tesxttemp.EGENTEXT. 
   END.   
   ELSE IF tesxttemp.PROGRAM = "OMR" THEN DO:
      ASSIGN
       Guru.Konstanter:gomrl = tesxttemp.EGENTEXTFULL
       Guru.Konstanter:gomrk = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "AONR" THEN DO:
      ASSIGN
      Guru.Konstanter:gaol = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:gaok = tesxttemp.EGENTEXT.
   END.   
   ELSE IF tesxttemp.PROGRAM = "FAST" THEN DO:
      ASSIGN
      Guru.Konstanter:gfastl = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:gfastk = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "TILLF" THEN DO:
      ASSIGN
      Guru.Konstanter:gtilll = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:gtillk = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "PLAN" THEN DO:
      ASSIGN
      Guru.Konstanter:gpll = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:gplk = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "ENR" THEN DO:
      ASSIGN
      Guru.Konstanter:genl = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:genk = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "JUR" THEN DO:
      ASSIGN
      Guru.Konstanter:gjul = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:gjuk = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "BERE" THEN DO:
      ASSIGN
      Guru.Konstanter:gberel = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:gberek = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "PROJ" THEN DO:
      ASSIGN
      Guru.Konstanter:gprojl = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:gprojk = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "ARBA" THEN DO:
      ASSIGN
      Guru.Konstanter:garbal = tesxttemp.EGENTEXTFULL
      Guru.Konstanter:garbak = tesxttemp.EGENTEXT. 
   END.
   ELSE IF tesxttemp.PROGRAM = "UTFO" THEN DO:
      ASSIGN
      Guru.Konstanter:gutfk = tesxttemp.EGENTEXT
      Guru.Konstanter:gutfl = tesxttemp.EGENTEXTFULL. 
   END.
   ELSE IF tesxttemp.PROGRAM = "BEST" THEN DO:
      ASSIGN
      Guru.Konstanter:gbestk = tesxttemp.EGENTEXT
      Guru.Konstanter:gbestl = tesxttemp.EGENTEXTFULL. 
   END. 
   ELSE IF tesxttemp.PROGRAM = "DEB" THEN DO:
      ASSIGN
      Guru.Konstanter:gdebk = tesxttemp.EGENTEXT
      Guru.Konstanter:gdebl = tesxttemp.EGENTEXTFULL. 
   END.
   ELSE IF tesxttemp.PROGRAM = "TIDL" THEN DO:
      ASSIGN
      Guru.Konstanter:gtidlk = tesxttemp.EGENTEXT
      Guru.Konstanter:gtidll = tesxttemp.EGENTEXTFULL. 
   END.   
   ELSE IF tesxttemp.PROGRAM = "UTFA" THEN DO:
      ASSIGN
      Guru.Konstanter:gutfardk = tesxttemp.EGENTEXT
      Guru.Konstanter:gutfardl = tesxttemp.EGENTEXTFULL. 
   END.
   ELSE IF tesxttemp.PROGRAM = "REFBEF" THEN DO:
      ASSIGN
      Guru.Konstanter:grefbefk = tesxttemp.EGENTEXT
      Guru.Konstanter:grefbefl = tesxttemp.EGENTEXTFULL. 
   END.
    ELSE IF tesxttemp.PROGRAM = "Prio" THEN DO:
      ASSIGN
      Guru.Konstanter:gpriok = tesxttemp.EGENTEXT
      Guru.Konstanter:gpriol = tesxttemp.EGENTEXTFULL. 
   END.
   ELSE IF tesxttemp.PROGRAM = "Arbart" THEN DO:
      ASSIGN
      Guru.Konstanter:gartk = tesxttemp.EGENTEXT
      Guru.Konstanter:gartl = tesxttemp.EGENTEXTFULL. 
   END.
   ELSE IF tesxttemp.PROGRAM = "AONAMN" THEN DO:
      ASSIGN
      Guru.Konstanter:gaonamnk = tesxttemp.EGENTEXT
      Guru.Konstanter:gaonamnl = tesxttemp.EGENTEXTFULL. 
   END.
END.

