/*BESTHMT.I*/ 
  
   FIND FIRST bestkundallt NO-ERROR.
   IF NOT AVAILABLE bestkundallt THEN DO:
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN BESTALLTSKAPU.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
         (INPUT bestvad, OUTPUT TABLE bestkundallt,OUTPUT TABLE bestkundextra).
      END.
      ELSE DO:
         RUN BESTALLTSKAPU.P  
         (INPUT bestvad, OUTPUT TABLE bestkundallt,OUTPUT TABLE bestkundextra).
      END.
   END.
     
