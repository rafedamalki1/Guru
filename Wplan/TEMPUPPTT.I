/*TEMPUPPTT.I*/
PROCEDURE tempuppsum_UI :
    IF AVAILABLE tempupp THEN DO:
      IF AVAILABLE kalksumsum THEN DO: 
         ASSIGN
         tempupp.bertim = kalksumsum.BTIMMAR
         tempupp.arbtim = kalksumsum.TIMMAR 
         tempupp.msktim = kalksumsum.MASKTIMMAR           
         tempupp.totea = kalksumsum.EAMANGD
         tempupp.totarb = kalksumsum.BELOPP
         tempupp.totmask = kalksumsum.MASKGBELOPP
         tempupp.totmtrl = kalksumsum.MTRL 
         tempupp.totovr = kalksumsum.OVRKR 
         tempupp.totutr = kalksumsum.UKOST
         tempupp.totutrtim = kalksumsum.UTIMMAR.
      
         tempupp.totalt = 0.
         tempupp.totalt = tempupp.totalt + tempupp.totarb + tempupp.totmask + 
                          tempupp.totovr + tempupp.totmtrl + tempupp.totutr.
      END.                    
   END.   
END PROCEDURE.
