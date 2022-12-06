/*AOFORMAT4.I*/
   
IF Guru.Konstanter:varforetypval[8] >= 7 THEN DO:
   {&FORMATNAMN}:FORMAT = "X(" + STRING(Guru.Konstanter:varforetypval[8]) + ")".
   /*
   IF Guru.Konstanter:varforetypval[8] = 7 THEN DO:
      {&FORMATNAMN}:FORMAT = "X(7)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 8 THEN DO:
     {&FORMATNAMN}:FORMAT  = "X(8)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 9 THEN DO:
     {&FORMATNAMN}:FORMAT  = "X(9)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 10 THEN DO:
     {&FORMATNAMN}:FORMAT  = "X(10)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 11 THEN DO:
     {&FORMATNAMN}:FORMAT  = "X(11)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 12 THEN DO:
     {&FORMATNAMN}:FORMAT  = "X(12)".
   END.
   IF Guru.Konstanter:varforetypval[8] = 13 THEN DO:
     {&FORMATNAMN}:FORMAT  = "X(13)".
   END.
   */
   IF {&FORMATNAMN}:WIDTH-CHARS <= Guru.Konstanter:varforetypval[8] THEN 
      {&FORMATNAMN}:WIDTH-CHARS = Guru.Konstanter:varforetypval[8] + 1.
END.
ELSE DO:
   IF Guru.Konstanter:varforetypval[8] = 5 THEN DO:
      {&FORMATNAMN}:FORMAT = "X(5)" .
   END.
   ELSE    {&FORMATNAMN}:FORMAT = "X(6)".
   IF {&FORMATNAMN}:WIDTH-CHARS <= 6 THEN {&FORMATNAMN}:WIDTH-CHARS = 6.
END.

   




