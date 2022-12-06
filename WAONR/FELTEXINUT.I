/*FELTEXINUT.I*/

IF Guru.Konstanter:appcon THEN DO:                           
   RUN FELTEXTW2.P ON Guru.Konstanter:apphand TRANSACTION DISTINCT 
   (INPUT Guru.Konstanter:globanv,OUTPUT musz, OUTPUT TABLE felinut).
END.
ELSE DO:
   RUN FELTEXTW2.P  
   (INPUT Guru.Konstanter:globanv,OUTPUT musz, OUTPUT TABLE felinut).
END.
FOR EACH felinut:
   musz = TRUE.
   MESSAGE felinut.FELTEXT VIEW-AS ALERT-BOX.
   DELETE felinut.
END.
