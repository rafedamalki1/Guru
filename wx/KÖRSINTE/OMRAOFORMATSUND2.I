/*OMRAOFORMATSUND2.I*/
    /*aonrsund k?rs inte*/  
   IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "elpa" THEN DO:      
      /*  2 pos p? omr?de F?R 1611 */    
      IF AVAILABLE {&FORMATNAMNTAB} THEN DO:
         IF {&FORMATNAMNOMR} BEGINS "16" THEN DO:
            {&FORMATNAMNOMR}:FORMAT = "X(2)" .
         END.      
      END.
   END.


        
