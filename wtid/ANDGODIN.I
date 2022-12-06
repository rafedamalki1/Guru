/*ANDGODIN.I*/   
   IF Guru.Konstanter:appcon THEN DO:                                                    
      RUN APANDGOD.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
      (INPUT TABLE appmarkpers, INPUT gvisatidpermanad, INPUT regar, INPUT regmnr).
   END.
   ELSE DO:
      RUN APANDGOD.P 
      (INPUT TABLE appmarkpers, INPUT gvisatidpermanad, INPUT regar, INPUT regmnr).   
   END.
   
