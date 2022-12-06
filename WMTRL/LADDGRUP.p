/*LADDGRUP.p LADDAR konstruktionsgrupp FRÅN .D FILER*/
DEFINE VARIABLE prognamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamn2 AS CHARACTER NO-UNDO.

&Scoped-define NEW  NEW 
&Scoped-define SHARED SHARED
{FELMEDTEMP.I}
{KONSTRMTRL.I}
DEFINE INPUT PARAMETER globforetag LIKE FORETAG.FORETAG NO-UNDO.
DEFINE INPUT PARAMETER valgrupp LIKE KONSTGRUPP.KONSKOD NO-UNDO.
DEFINE INPUT PARAMETER valord AS INTEGER  NO-UNDO.
DEFINE INPUT  PARAMETER skrivov AS LOGICAL NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR felmeddtemp.
{AMERICANEUROPEAN.I}
   {muswait.i}
   
   IF  globforetag = "VAST"  THEN DO:
      /*prognamn = "e:\delad\pro9\guru\".*/
       prognamn = "C:\PRO10\GURU\".
   END.      
   ELSE IF globforetag = "SKOG" OR globforetag = "ESKO"   OR globforetag = "OPPU"
   OR globforetag = "NKON" OR globforetag = "ETSA" OR globforetag = "LECA"  THEN DO:
      prognamn = "C:\PRO10\GURU\".
   END.
   ELSE IF  globforetag = "GKAL" THEN DO:
      prognamn = "D:\DELAD\klient\PRO9\".
   END.
   ELSE IF globforetag = "GRAN" THEN DO:      
      prognamn = "d:\elpool\delad\pro9\wrk\".
            
      /*prognamn = "C:\Pro10\Guru\14 eld lsp\".
      prognamn = "C:\Pro10\Guru\25 HSP 12 KV\".
      prognamn = "C:\Pro10\Guru\27 HSP 24KV\".
      prognamn = "C:\Pro10\Guru\0 kabelskåp\".
      prognamn = "C:\Pro10\Guru\12 KL\".
      prognamn = "C:\Pro10\Guru\30 holtab\".
      prognamn = "C:\Pro10\Guru\33 Norrmontage\".
      prognamn = "C:\Pro10\Guru\37 abb\".
      prognamn = "C:\Pro10\Guru\38 TRANSFIX\".*/     
       
   END.
   ELSE IF globforetag = "SUND" OR globforetag = "SNAT" THEN DO:                 
      prognamn = "D:\DELAD\PRO10\".
   END.
           
   ELSE IF globforetag = "FORS"  THEN DO:      
      prognamn = "C:\DELAD\PRO9\". 
   END.
   ELSE IF globforetag = "TECT"  THEN DO:      
      prognamn = "C:\DELAD\PRO10\". 
   END.
   ELSE IF globforetag = "POFO"  THEN DO:      
      prognamn = "C:\delad\pro10\". 
   END.
   ELSE IF globforetag = "POWE"  THEN DO:      
      prognamn = "C:\delad\pro10\". 
   END.   
   ELSE IF globforetag = "ELPA"  THEN DO:
      prognamn = "C:\Pro10\GURU\WTID\". 
   END.
   
   EMPTY TEMP-TABLE konsttemp NO-ERROR.
   prognamn2 = prognamn + "konstru.d". 
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1". 
      
   REPEAT: 
      CREATE konsttemp.  
      ASSIGN.     
      IMPORT konsttemp.        
      konsttemp.KONSKOD = valgrupp.
   END.
   INPUT CLOSE.
   EMPTY TEMP-TABLE felmeddtemp NO-ERROR. 
   IF skrivov = FALSE THEN DO:
      FOR EACH konsttemp NO-LOCK:
         FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD =  konsttemp.KTYPKOD NO-LOCK NO-ERROR.
         IF AVAILABLE KONSTRUKTION THEN DO:            
            FIND FIRST KONSTGRUPP WHERE KONSTGRUPP.KONSKOD = KONSTRUKTION.KONSKOD NO-LOCK NO-ERROR.
            IF AVAILABLE KONSTGRUPP THEN DO:
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = KONSTRUKTION.KTYPKOD + " " + KONSTRUKTION.BENAMNING + "  finns redan upplagd med samma namn i denna databas på konstruktionsgrupp: "
                + STRING(KONSTGRUPP.KONSKOD) + " " + KONSTGRUPP.BENAMNING.
                RETURN.
            END.
            ELSE DO:
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = KONSTRUKTION.KTYPKOD + " " + KONSTRUKTION.BENAMNING + "  finns redan upplagd med samma namn i denna databas på konstruktionsgrupp: " + STRING(KONSTRUKTION.KONSKOD).
               RETURN.
            END.
          END.        
      END.
   END.
   ELSE DO:
      FOR EACH konsttemp NO-LOCK:
         FIND FIRST KONSTRUKTION WHERE KONSTRUKTION.KTYPKOD =  konsttemp.KTYPKOD AND KONSTRUKTION.KONSKOD NE konsttemp.KONSKOD NO-LOCK NO-ERROR.
         IF AVAILABLE KONSTRUKTION THEN DO:            
            FIND FIRST KONSTGRUPP WHERE KONSTGRUPP.KONSKOD = KONSTRUKTION.KONSKOD NO-LOCK NO-ERROR.
            IF AVAILABLE KONSTGRUPP THEN DO:
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = KONSTRUKTION.KTYPKOD + " " + KONSTRUKTION.BENAMNING + "  finns redan upplagd med samma namn i denna databas på konstruktionsgrupp: "
                + STRING(KONSTGRUPP.KONSKOD) + " " + KONSTGRUPP.BENAMNING.
                RETURN.
            END.
            ELSE DO:
               CREATE felmeddtemp.
               felmeddtemp.FELMEDD = KONSTRUKTION.KTYPKOD + " " + KONSTRUKTION.BENAMNING + "  finns redan upplagd med samma namn i denna databas på konstruktionsgrupp: " + STRING(KONSTRUKTION.KONSKOD).
               RETURN.
            END.
          END.        
      END.
              
      
   END.   
      
   
   
    
    
   /* vill ej riskera överläsning Lena 20171018*/
   IF skrivov = TRUE THEN DO:   
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
   END.
   prognamn2 = prognamn + "konstgr.d". 
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".   
   REPEAT: 
      CREATE KONSTGRUPP.  
      ASSIGN.     
      IMPORT KONSTGRUPP.       
      assign
      KONSTGRUPP.KONSKOD = valgrupp
      KONSTGRUPP.ORDNING = valord.
   END.
   INPUT CLOSE.
   prognamn2 = prognamn + "bbenamn.d". 
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
      
   REPEAT: 
      CREATE BBENAMNING.  
      ASSIGN.     
      IMPORT BBENAMNING.
      BBENAMNING.KONSKOD = valgrupp.        
      
   END.
   INPUT CLOSE.
   prognamn2 = prognamn + "edigrupp.d". 
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
      
   REPEAT: 
      CREATE EDIGRUPP. 
      ASSIGN.     
      IMPORT EDIGRUPP.
      EDIGRUPP.KONSKOD = valgrupp.      
   END.
   INPUT CLOSE.
   prognamn2 = prognamn + "konstru.d". 
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
      
   REPEAT: 
      CREATE KONSTRUKTION.  
      ASSIGN.     
      IMPORT KONSTRUKTION.        
      KONSTRUKTION.KONSKOD = valgrupp.
   END.
   INPUT CLOSE.
   prognamn2 = prognamn + "konstval.d". 
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
      
   REPEAT: 
      CREATE KONSTVAL.  
      ASSIGN.     
      IMPORT KONSTVAL.      
      KONSTVAL.KONSKOD = valgrupp.
   END.
   INPUT CLOSE.                 
   prognamn2 = prognamn + "mtrlber.d". 
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
      
   REPEAT: 
      CREATE MTRLBER. 
      ASSIGN.     
      IMPORT MTRLBER.
      IF globforetag = "snat" AND SUBSTRING(MTRLBER.ENR,1,1) NE "E" THEN MTRLBER.ENR = "E" + MTRLBER.ENR. 
        
   END.
   INPUT CLOSE. 
   prognamn2 = prognamn + "kalkber.d". 
   INPUT FROM VALUE(prognamn2) convert target "iso8859-1" source "iso8859-1".
      
   REPEAT: 
      CREATE KALKBER.  
      ASSIGN.     
      IMPORT KALKBER.  
   END.
   INPUT CLOSE.
{EUROPEANAMERICAN.I}   
