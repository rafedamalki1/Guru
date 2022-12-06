/*OMRAOFORMATSUND2.I*/
    /*aonrsund körs inte*/  
   IF globforetag = "SUND" OR globforetag = "SNAT" OR globforetag = "elpa" THEN DO:      
      /*  2 pos på område FÖR 1611 */    
      IF AVAILABLE {&FORMATNAMNTAB} THEN DO:
         IF {&FORMATNAMNOMR} BEGINS "16" THEN DO:
            {&FORMATNAMNOMR}:FORMAT = "X(2)" .
         END.      
      END.
   END.


        
