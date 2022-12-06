/*BYTPROPATH.I körs inte*/

IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO: 
   /*
   PROPATH = REPLACE(PROPATH,".",SUBSTRING(Guru.Konstanter:guruvar,1,LENGTH(Guru.Konstanter:guruvar) - 1) ).
    */
END.
