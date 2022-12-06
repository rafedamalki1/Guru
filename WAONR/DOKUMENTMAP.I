   /*DOKUMENTMAP.I*/
   IF Guru.Konstanter:appcon THEN DO:
      RUN DOKUMENTMAP.p ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,INPUT datornamn,INPUT soktemp.SOKCHAR[1],INPUT Guru.Konstanter:guruvar,OUTPUT grundmappvar). 
   END.
   ELSE DO:
      RUN DOKUMENTMAP.p (INPUT Guru.Konstanter:globanv,INPUT Guru.Konstanter:globforetag,INPUT datornamn,INPUT soktemp.SOKCHAR[1],INPUT Guru.Konstanter:guruvar,OUTPUT grundmappvar). 
   END.
  
