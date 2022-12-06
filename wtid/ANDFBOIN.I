/*ANDFBOIN.I*/   
   IF Guru.Konstanter:appcon THEN DO:                                                    
      RUN APANDFBO.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE appmarkpers, INPUT gvisatidpermanad, INPUT regar, INPUT regmnr).
   END.
   ELSE DO:
      RUN APANDFBO.P 
      (INPUT TABLE appmarkpers, INPUT gvisatidpermanad, INPUT regar, INPUT regmnr).   
   END.
   
