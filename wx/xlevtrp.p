DEFINE VARIABLE bernrvar AS CHARACTER NO-UNDO.
DEFINE VARIABLE omrvar AS CHARACTER NO-UNDO.
bernrvar = "2201".
omrvar = "tev8".
FOR EACH levtrp WHERE levtrp.bernr = INTEGER(bernrvar) AND levtrp.omrade = omrvar ,
EACH levtrp2 WHERE 
   LEVTRP2.BERNR = LEVTRP.BERNR AND LEVTRP2.omrade = LEVTRP.OMRADE AND LEVTRP2.levkod = LEVTRP.levkod AND
   levtrp2.bestnr = levtrp.bestnr
   BY levtrp.levkod BY levtrp.bestnr:
      DISP LEVTRP2.datum LEVTRP2.bestnr LEVTRP.bestnr date(integer(substring(LEVTRP.leverans,1,15))) substring(LEVTRP.leverans,20,5) 
        LEVTRP.BESTALLD FORMAT "x(5)" LEvtrp.levkod 
        WITH FRAME cc DOWN.

END.

