/*OMRAOFORMAT.I*/
IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:      
   /* på elnät vill alla utom 13 och 18 bara se 2 pos på område*/      
   IF (personaltemp.OMRADE BEGINS "11" OR personaltemp.OMRADE BEGINS "15"
   OR personaltemp.OMRADE BEGINS "16" OR personaltemp.OMRADE BEGINS "17" ) THEN DO:
      {&FORMATNAMNOMR}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(2)" .
   END.
   ELSE DO:
      {&FORMATNAMNOMR}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(6)" .
   END.
END.

