/*OMRAOFORMATSUND.I*/
      /*aonrsund anv?nds inte*/
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "elpa" THEN DO:      
      /*  2 pos p? omr?de F?R 1611 */    
      IF AVAILABLE {&FORMATNAMNTAB} THEN DO:
         IF {&FORMATNAMNOMR} BEGINS "16" THEN DO:
            {&FORMATNAMNOMR}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(2)" .
         END.      
      END.
   END.


        
