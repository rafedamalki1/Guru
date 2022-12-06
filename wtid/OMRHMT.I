/*OMRHMT.I*/   
   FIND FIRST omrtemp NO-ERROR.
   IF NOT AVAILABLE omrtemp THEN DO:
      IF Guru.Konstanter:appcon THEN DO:   
         IF Guru.Konstanter:AppSpringSet[1] = "ONEWWW" OR  Guru.Konstanter:AppSpringSet[1] = "VSWWW" THEN RUN OMRSKAPW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv , OUTPUT TABLE omrtemp).
         ELSE RUN OMRSKAPW.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT "", OUTPUT TABLE omrtemp).
      END.
      ELSE DO:
         IF Guru.Konstanter:AppSpringSet[1] = "ONEWWW" OR  Guru.Konstanter:AppSpringSet[1] = "VSWWW" THEN  RUN OMRSKAPW.P  (INPUT Guru.Konstanter:globanv, OUTPUT TABLE omrtemp).
         ELSE RUN OMRSKAPW.P  (INPUT "", OUTPUT TABLE omrtemp).                   
      END.
   END.
     
 