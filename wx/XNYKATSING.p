/*NYKATSING.P*/
DEFINE INPUT PARAMETER valev AS CHARACTER NO-UNDO.

{muswait.i}   
   MESSAGE "Nu börjar borttag av materiel" VIEW-AS ALERT-BOX.
   IF valev = "Elektroskandia" OR valev = "Onninen och Elektroskandia och Ahlsell" THEN DO:      
      OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND
      MTRL.LEVKOD = "1" NO-LOCK.
      DO TRANSACTION:
         GET FIRST mtrlq EXCLUSIVE-LOCK.
         IF AVAILABLE MTRL THEN DO:
            DELETE MTRL.
            GET NEXT mtrlq EXCLUSIVE-LOCK.
         END.
      END.
      DO WHILE AVAILABLE(MTRL):
         DO TRANSACTION:
            DELETE MTRL.
            GET NEXT mtrlq EXCLUSIVE-LOCK.
         END.
      END.
      CLOSE QUERY mtrlq.

      MESSAGE "Borttag av Elektroskandia klart" VIEW-AS ALERT-BOX.  
   END.
   IF valev = "Ahlsell" OR valev = "Onninen och Elektroskandia och Ahlsell" THEN DO:      
      OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "2" NO-LOCK.
      DO TRANSACTION:
         GET FIRST mtrlq EXCLUSIVE-LOCK.
         IF AVAILABLE MTRL THEN DO:
            DELETE MTRL.
            GET NEXT mtrlq EXCLUSIVE-LOCK.
         END.
      END.
      DO WHILE AVAILABLE(MTRL):
         DO TRANSACTION:
            DELETE MTRL.
            GET NEXT mtrlq EXCLUSIVE-LOCK.
         END.
      END.
      CLOSE QUERY mtrlq.
   
      MESSAGE "Borttag av Ahlsell klart" VIEW-AS ALERT-BOX.        
   END.
  
   IF valev = "Onninen" OR valev = "Onninen och Elektroskandia och Ahlsell" THEN DO:   

      OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND MTRL.LEVKOD = "5" NO-LOCK.
      DO TRANSACTION:
         GET FIRST mtrlq EXCLUSIVE-LOCK.
         IF AVAILABLE MTRL THEN DO:
            DELETE MTRL.
            GET NEXT mtrlq EXCLUSIVE-LOCK.
         END.
      END.
      DO WHILE AVAILABLE(MTRL):
         DO TRANSACTION:
            DELETE MTRL.
            GET NEXT mtrlq EXCLUSIVE-LOCK.
         END.
      END.
      CLOSE QUERY mtrlq.
   
      MESSAGE "Borttag av Onninen klart" VIEW-AS ALERT-BOX. 
   END.
  

   MESSAGE "Nu börjar inläsning av materiel" VIEW-AS ALERT-BOX. 

   IF valev = "Elektroskandia" OR valev = "Onninen och Elektroskandia och Ahlsell" THEN DO:      
      INPUT FROM C:\tempguru\Elektroskandia.d convert target "iso8859-1" source "iso8859-1".       
      REPEAT:
         CREATE MTRL.
         ASSIGN.
         IMPORT MTRL.
      END.
   END.
   IF valev = "Ahlsell" OR valev = "Onninen och Elektroskandia och Ahlsell" THEN DO:      
      INPUT FROM C:\tempguru\Ahlsell.d convert target "iso8859-1" source "iso8859-1".       
      REPEAT:
         CREATE MTRL.
         ASSIGN.
         IMPORT MTRL.
      END.
   END.
   IF valev = "Onninen" OR valev = "Onninen och Elektroskandia och Ahlsell" THEN DO:      
      INPUT FROM C:\tempguru\Onninen.d convert target "iso8859-1" source "iso8859-1".       
      REPEAT:
         CREATE MTRL.
         ASSIGN.
         IMPORT MTRL.
      END.
   END.

   
   INPUT CLOSE.     

 MESSAGE "Klart med inläsning av materiel" VIEW-AS ALERT-BOX.  
 


