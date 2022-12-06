/*AOFORMAT3.I*/
IF Guru.Konstanter:varforetypval[8] >= 7 THEN DO:
   {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(" + STRING(Guru.Konstanter:varforetypval[8]) + ")".
   /*
   IF Guru.Konstanter:varforetypval[8] = 7 THEN DO:
      {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(7)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 8 THEN DO:
     {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(8)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 9 THEN DO:
     {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(9)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 10 THEN DO:
     {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(10)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 11 THEN DO:
     {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(11)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 12 THEN DO:
     {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(12)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 13 THEN DO:
     {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(13)".
   END.
   */
   IF {&FORMATNAMN}:WIDTH-CHARS IN FRAME {&FRAME-NAME} <= Guru.Konstanter:varforetypval[8] THEN 
      {&FORMATNAMN}:WIDTH-CHARS IN FRAME {&FRAME-NAME} = Guru.Konstanter:varforetypval[8] + 1.
END.
ELSE DO:
   IF Guru.Konstanter:varforetypval[8] = 5 THEN DO:
      {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(5)" .
   END.
   ELSE {&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = "X(6)".
   IF {&FORMATNAMN}:WIDTH-CHARS IN FRAME {&FRAME-NAME} <= 6 THEN {&FORMATNAMN}:WIDTH-CHARS IN FRAME {&FRAME-NAME} = 6.
END.

   




