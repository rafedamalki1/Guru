/*LADDAR BEREDNING FRÅN .D FILER*/
/*DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER valgrupp LIKE KONSTGRUPP.KONSKOD NO-UNDO.*/
   DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
   DEFINE VARIABLE valgrupp AS INTEGER NO-UNDO.
   FIND FIRST FORETAG WHERE NO-LOCK NO-ERROR.
   globforetag = foretag.foretag.
/*   valgrupp = 31.   */
/*   valgrupp = 42. OPTO  ELPA*/
   /*valgrupp = 19.    /*OPTO SUNDSVALL*/*/

  
   /*IF valgrupp NE 777 THEN DO:   
      /*En befintlig konstruktionsgrupp skall skrivas över*/
      OPEN QUERY kq FOR EACH KONSTGRUPP WHERE KONSTGRUPP.KONSKOD = valgrupp
      NO-LOCK.
      DO TRANSACTION:
         GET FIRST kq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTGRUPP THEN DELETE KONSTGRUPP.
      END.   
      REPEAT:
         DO TRANSACTION:
            GET NEXT kq EXCLUSIVE-LOCK.
            IF AVAILABLE KONSTGRUPP THEN DELETE KONSTGRUPP.
            ELSE LEAVE.
         END.       
      END.   
               
      OPEN QUERY ordq FOR EACH BBENAMNING WHERE BBENAMNING.KONSKOD = valgrupp NO-LOCK.  
      DO TRANSACTION:
         GET FIRST ordq EXCLUSIVE-LOCK.
         IF AVAILABLE BBENAMNING THEN DELETE BBENAMNING.
      END.   
      REPEAT:
         DO TRANSACTION:
            GET NEXT ordq EXCLUSIVE-LOCK.
            IF AVAILABLE BBENAMNING THEN DELETE BBENAMNING.
            ELSE LEAVE.
         END.       
      END. 
   
      OPEN QUERY ediq FOR EACH EDIGRUPP WHERE EDIGRUPP.KONSKOD = valgrupp NO-LOCK.  
      DO TRANSACTION:
         GET FIRST ediq EXCLUSIVE-LOCK.
         IF AVAILABLE EDIGRUPP THEN DELETE EDIGRUPP.
      END.   
      REPEAT:
         DO TRANSACTION:
            GET NEXT ediq EXCLUSIVE-LOCK.
            IF AVAILABLE EDIGRUPP THEN DELETE EDIGRUPP.
            ELSE LEAVE.
         END.       
      END.
         
      OPEN QUERY berqid FOR EACH KONSTVAL WHERE KONSTVAL.KONSKOD = valgrupp NO-LOCK.      
      DO TRANSACTION:
         GET FIRST berqid EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTVAL THEN DELETE KONSTVAL.
      END.   
      REPEAT:
         DO TRANSACTION:
            GET NEXT berqid EXCLUSIVE-LOCK.
            IF AVAILABLE KONSTVAL THEN DELETE KONSTVAL.
            ELSE LEAVE.
         END.       
      END.   
         
      OPEN QUERY mq FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.     
      GET FIRST mq NO-LOCK.
      DO WHILE AVAILABLE(KONSTRUKTION):
         OPEN QUERY mq2 FOR EACH MTRLBER WHERE MTRLBER.KTYPKOD = KONSTRUKTION.KTYPKOD NO-LOCK.
         DO TRANSACTION:
            GET FIRST mq2 EXCLUSIVE-LOCK.
            IF AVAILABLE MTRLBER THEN DELETE MTRLBER.
         END.   
         REPEAT:
            DO TRANSACTION:
               GET NEXT mq2 EXCLUSIVE-LOCK.
               IF AVAILABLE MTRLBER THEN DELETE MTRLBER.
               ELSE LEAVE.
            END.       
         END.      
         CLOSE QUERY mq2.          
         GET NEXT mq NO-LOCK.
      END.
      CLOSE QUERY mq.   
         
      OPEN QUERY mq3 FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.     
      GET FIRST mq3 NO-LOCK.
      DO WHILE AVAILABLE(KONSTRUKTION):
         OPEN QUERY kalkq FOR EACH KALKBER WHERE KALKBER.KTYPKOD = KONSTRUKTION.KTYPKOD 
         NO-LOCK.
         DO TRANSACTION:
            GET FIRST kalkq EXCLUSIVE-LOCK.
            IF AVAILABLE KALKBER THEN DELETE KALKBER.
         END.   
         REPEAT:
            DO TRANSACTION:
               GET NEXT kalkq EXCLUSIVE-LOCK.
               IF AVAILABLE KALKBER THEN DELETE KALKBER.
               ELSE LEAVE.
            END.       
         END.                             
         CLOSE QUERY kalkq.
         GET NEXT mq3 NO-LOCK.
      END.
      CLOSE QUERY mq3.   
   
      OPEN QUERY friq FOR EACH KONSTRUKTION WHERE KONSTRUKTION.KONSKOD = valgrupp NO-LOCK.        
      DO TRANSACTION:
         GET FIRST friq EXCLUSIVE-LOCK.
         IF AVAILABLE KONSTRUKTION THEN DELETE KONSTRUKTION.
      END.   
      REPEAT:
         DO TRANSACTION:
            GET NEXT friq EXCLUSIVE-LOCK.
            IF AVAILABLE KONSTRUKTION THEN DELETE KONSTRUKTION.
            ELSE LEAVE.
         END.       
      END.
   END.*/

   IF  
   globforetag = "VAST" OR globforetag = "VSAB" OR globforetag = "VSAB"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\konstgr.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM /u01/guru/wrk/konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
     
     INPUT FROM D:\ELPOOL\DELAD\PRO9\konstgr.d  convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" OR globforetag = "ESKO"  THEN DO:
      INPUT FROM  C:\PRO9\GURU\WTID\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GETB"  THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\konstgr.d  convert target "iso8859-1" source "iso8859-1".    
   END.   

   ELSE DO:
      INPUT FROM C:\konstgr.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE KONSTGRUPP.  
      ASSIGN.     
      IMPORT KONSTGRUPP.       
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST" OR globforetag = "VSAB"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\bbenamn.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM /u01/guru/wrk/bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\bbenamn.d  convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" OR globforetag = "ESKO"  THEN DO:
      INPUT FROM  C:\PRO9\GURU\WTID\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GETB"  THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\bbenamn.d  convert target "iso8859-1" source "iso8859-1".    
   END. 
   ELSE DO:
      INPUT FROM C:\bbenamn.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE BBENAMNING.  
      ASSIGN.     
      IMPORT BBENAMNING.        
   END.
   INPUT CLOSE.

   IF  
   globforetag = "VAST" OR globforetag = "VSAB"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\edigrupp.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM /u01/guru/wrk/edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\edigrupp.d  convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" OR globforetag = "ESKO"  THEN DO:
      INPUT FROM  C:\PRO9\GURU\WTID\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GETB"  THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\edigrupp.d  convert target "iso8859-1" source "iso8859-1".    
   END. 
   ELSE DO:
      INPUT FROM C:\edigrupp.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE EDIGRUPP.  
      ASSIGN.     
      IMPORT EDIGRUPP.      
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST" OR globforetag = "VSAB"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\konstru.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM /u01/guru/wrk/konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\konstru.d  convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" OR globforetag = "ESKO"  THEN DO:
      INPUT FROM  C:\PRO9\GURU\WTID\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GETB"  THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\konstru.d  convert target "iso8859-1" source "iso8859-1".    
   END. 
   ELSE DO:
      INPUT FROM C:\konstru.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE KONSTRUKTION.  
      ASSIGN.     
      IMPORT KONSTRUKTION.        
   END.
   INPUT CLOSE.
   
   IF  
   globforetag = "VAST" OR globforetag = "VSAB"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\konstval.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM /u01/guru/wrk/konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\konstval.d  convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" OR globforetag = "ESKO"  THEN DO:
      INPUT FROM  C:\PRO9\GURU\WTID\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GETB"  THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\konstval.d  convert target "iso8859-1" source "iso8859-1".    
   END. 
   ELSE DO:
      INPUT FROM C:\konstval.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE KONSTVAL.  
      ASSIGN.     
      IMPORT KONSTVAL.      
   END.
   INPUT CLOSE.                 

   IF  
   globforetag = "VAST" OR globforetag = "VSAB"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\mtrlber.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN"  THEN DO:
      INPUT FROM /u01/guru/wrk/mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\mtrlber.d  convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" OR globforetag = "ESKO"  THEN DO:
      INPUT FROM  C:\PRO9\GURU\WTID\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GETB"  THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\mtrlber.d  convert target "iso8859-1" source "iso8859-1".    
   END. 
   ELSE DO:
      INPUT FROM C:\mtrlber.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE MTRLBER.  
      ASSIGN.     
      IMPORT MTRLBER.  
   END.
   INPUT CLOSE. 

   IF  
   globforetag = "VAST" OR globforetag = "VSAB"  THEN DO:
      INPUT FROM e:\delad\pro9\guru\kalkber.d convert target "iso8859-1" source "iso8859-1". 
   END.
   ELSE IF globforetag = "GKAL" THEN DO:
      INPUT FROM D:\DELAD\klient\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:
      INPUT FROM /u01/guru/wrk/kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "LULE" THEN DO:
      INPUT FROM D:\ELPOOL\DELAD\PRO9\kalkber.d  convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "SUND" THEN DO:
      INPUT FROM D:\DELAD\KLIENT\PRO9\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "ELPA" OR globforetag = "ESKO"  THEN DO:
      INPUT FROM  C:\PRO9\GURU\WTID\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.
   ELSE IF globforetag = "GETB"  THEN DO:
      INPUT FROM C:\ELPOOL\DELAD\PRO10\GURU\kalkber.d  convert target "iso8859-1" source "iso8859-1".    
   END. 
   ELSE DO:
      INPUT FROM C:\kalkber.d convert target "iso8859-1" source "iso8859-1".
   END.   
   REPEAT: 
      CREATE KALKBER.  
      ASSIGN.     
      IMPORT KALKBER.  
   END.
   INPUT CLOSE.
