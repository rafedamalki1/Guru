/*AOFORMAT1.I*/
/*finns i dynbrwproc
ON ROW-DISPLAY OF {&BROWSE-NAME} IN FRAME {&FRAME-NAME}
DO:
   IF Guru.Konstanter:varforetypval[8] = 7 THEN DO:
      {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(7)" .
   END.
   ELSE IF Guru.Konstanter:varforetypval[8] = 5 THEN DO:
      {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(5)" .
   END.
   ELSE IF Guru.Konstanter:varforetypval[8] = 8 THEN DO:
      {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(8)" .
   END.
   ELSE IF Guru.Konstanter:varforetypval[8] = 9 THEN DO:
      {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(9)" .
   END.
   ELSE IF Guru.Konstanter:varforetypval[8] = 10 THEN DO:
      {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(10)" .
   END.
   ELSE DO:
      {&FORMATNAMN}:FORMAT IN BROWSE {&BROWSE-NAME} = "X(6)" .
   END.
END.
IF Guru.Konstanter:varforetypval[8] >= 7 THEN DO:
   {&FORMATNAMN}:WIDTH-CHARS IN BROWSE {&BROWSE-NAME} = Guru.Konstanter:varforetypval[8].
END.
ELSE DO:
   {&FORMATNAMN}:WIDTH-CHARS IN BROWSE {&BROWSE-NAME} = 6.
END.
*/
