/*SPECPROG.p KÖRS VIA MTRLHANTERING OCH RUBRIKEN UPPDATERA DATABAS*/
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
{AMERICANEUROPEAN.I}
{muswait.i}
RUN P2KOMN.P.  
/*RUN KOPKONTIKON.P.*/
/*RUN xdumpovavtal.p.*/
/*RUN LKALKEXTRA.P.*/
/*RUN delkalk.p. /*  */*/

/*RUN SATSKOPSU.P.*/
    /*RUN LKALKEXTRAP5.P.*/
   /*RUN LKALKEXTRA2.P.
      */
/*   RUN XPRODAKER0603.P.*/
 /*  RUN ordkonstval.p. */
   


   /*  
   MESSAGE "Nu börjar borttag av materiel" VIEW-AS ALERT-BOX.

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

   OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND
   MTRL.LEVKOD = "2" NO-LOCK.
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

   MESSAGE "Borttag av Ahlsell klart" VIEW-AS ALERT-BOX.        */
  
   /*OPEN QUERY mtrlq FOR EACH MTRL WHERE MTRL.KALKNR = 0 AND
   MTRL.LEVKOD = "5" NO-LOCK.
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
  


   MESSAGE "Nu börjar inläsning av materiel" VIEW-AS ALERT-BOX. 

   IF  
   globforetag = "VAST"   THEN DO:
      INPUT FROM /guru/mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN"  OR globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\GURU\WTID\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "STRA" THEN DO:
      INPUT FROM E:\DELAD\PRO9\GURU\WTID\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "BORL" THEN DO:
      INPUT FROM D:\GURU\PRO9\GURU\WTID\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" THEN DO:
      INPUT FROM \\pc112\DELAD\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   
   ELSE IF globforetag = "ATS"   THEN DO:
      INPUT FROM C:\PRO10\GURU\wtid\mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE DO:
      INPUT FROM mtrl.d convert target "iso8859-1" source "iso8859-1".
   END.
   REPEAT:
      CREATE MTRL.
      ASSIGN.
      IMPORT MTRL.
   END.
   INPUT CLOSE.     

 MESSAGE "Klart med inläsning av materiel" VIEW-AS ALERT-BOX.*/  
  


{EUROPEANAMERICAN.I}
