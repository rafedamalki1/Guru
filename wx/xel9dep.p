DEFINE NEW SHARED VARIABLE filnamn AS CHARACTER NO-UNDO.
DEFINE NEW SHARED VARIABLE skrivut AS LOGICAL NO-UNDO.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.         
DEFINE VARIABLE str AS CHARACTER FORMAT "X(86)" NO-UNDO. 
DEFINE VARIABLE ar AS INTEGER NO-UNDO. 
DEFINE VARIABLE fackvar LIKE MTRLDEP.FACKID NO-UNDO.  
DEFINE VARIABLE var1 AS CHARACTER FORMAT "X(6)" NO-UNDO.
DEFINE VARIABLE var2 AS CHARACTER FORMAT "X(2)" NO-UNDO.
DEFINE VARIABLE langd AS INTEGER NO-UNDO.     
DEFINE VARIABLE fackid AS CHARACTER FORMAT "X(8)" NO-UNDO. 
DEFINE VARIABLE words AS CHARACTER FORMAT "X(132)" NO-UNDO.
DEFINE TEMP-TABLE tidin
   FIELD TIN AS CHARACTER FORMAT "X(132)".  

DEFINE NEW SHARED TEMP-TABLE temp_dep 
   FIELD DEPNR LIKE MTRLDEP.DEPNR 
   FIELD BOK AS CHARACTER FORMAT "X(2)"
   FIELD SIFF AS CHARACTER FORMAT "X(6)"   
   FIELD FACKID LIKE MTRLDEP.FACKID
   FIELD BENAMNING LIKE MTRLDEP.BENAMNING
   FIELD ENR LIKE MTRLDEP.ENR
   FIELD ENHET LIKE MTRLDEP.ENHET
   FIELD BESTKVANT LIKE MTRLDEP.BESTKVANT
   FIELD BESTPUNKT LIKE MTRLDEP.BESTPUNKT 
   FIELD LEVKOD LIKE MTRLDEP.LEVKOD 
   FIELD OMSATT LIKE MTRLDEP.OMSATT
   FIELD INVANT LIKE MTRLDEP.INVANT
   FIELD IB LIKE MTRLDEP.IB
   FIELD IBDATUM LIKE MTRLDEP.IBDATUM
   FIELD INVDATUM LIKE MTRLDEP.INVDATUM
   FIELD NPRIS LIKE MTRLDEP.NPRIS
   FIELD BPRIS LIKE MTRLDEP.BPRIS.

DOS SILENT quoter F:\PRO8\IMPORT\P1 > F:\PRO8\IMPORT\P1.Q.      
   
INPUT FROM A:\DEP.Q NO-ECHO
CONVERT TARGET "iso8859-1" SOURCE "swedish-7-bit".
REPEAT:
   SET words VIEW-AS EDITOR INNER-CHARS 50 INNER-LINES 3 WITH FRAME DDD WIDTH 80.   
   CREATE TIDIN.   
   ASSIGN TIDIN.TIN = words.   
END.
INPUT CLOSE.

FOR EACH TIDIN:
   CREATE temp_dep.
   ASSIGN
   temp_dep.DEPNR = INTEGER(SUBSTRING(TIDIN.TIN,1,3))
   temp_dep.BOK = SUBSTRING(TIDIN.TIN,4,1)
   temp_dep.SIFF = SUBSTRING(TIDIN.TIN,5,4)   
   temp_dep.BENAMNING = SUBSTRING(TIDIN.TIN,20,22)
   temp_dep.ENHET = SUBSTRING(TIDIN.TIN,42,4)
   temp_dep.ENR = SUBSTRING(TIDIN.TIN,46,11)
   temp_dep.BESTPUNKT = INTEGER(SUBSTRING(TIDIN.TIN,57,5))
   temp_dep.BESTKVANT = INTEGER(SUBSTRING(TIDIN.TIN,63,5)). 
   RUN fack_UI.
   IF temp_dep.BESTKVANT = 0 THEN temp_dep.BESTKVANT = 1.
   IF temp_dep.DEPNR = 71 OR temp_dep.DEPNR = 711 THEN DO:
      IF SUBSTRING(temp_dep.ENR,1,1) = "E" THEN DO:
         temp_dep.ENR = SUBSTRING(temp_dep.ENR,2,10).
      END.
   END.             
END.    /*HUVUDPROGRAM ÖVERFÖRING DEPÅ FRÅN EL90 TILL GURU*/  

FOR EACH temp_dep:
   IF temp_dep.ENR = " " THEN DO:
      temp_dep.ENR = temp_dep.ENR.
   END.
   ELSE IF SUBSTRING(temp_dep.ENR,1,3) = "Skr" THEN DO:
      FIND FIRST MTRL WHERE MTRL.ENR = temp_dep.FACKID AND MTRL.LEVKOD = "4"
      NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO: 
         CREATE MTRLDEP.
         ASSIGN
         MTRLDEP.DEPNR = temp_dep.DEPNR         
         MTRLDEP.ENR = temp_dep.FACKID 
         MTRLDEP.BENAMNING = temp_dep.BENAMNING   
         MTRLDEP.ENHET = temp_dep.ENHET
         MTRLDEP.BESTKVANT = temp_dep.BESTKVANT 
         MTRLDEP.BESTPUNKT = temp_dep.BESTPUNKT    
         MTRLDEP.INVDATUM = TODAY
         MTRLDEP.INVANT = 0
         MTRLDEP.FACKID = temp_dep.FACKID.          
         MTRLDEP.LEVKOD = "4".
      END.
      ELSE DO:
         CREATE MTRL.
         ASSIGN
         MTRL.ENR = temp_dep.FACKID
         MTRL.BENAMNING = temp_dep.BENAMNING
         MTRL.ENHET = temp_dep.ENHET
         MTRL.LEVKOD = "4".
         
         CREATE MTRLDEP.   
         ASSIGN
         MTRLDEP.DEPNR = temp_dep.DEPNR         
         MTRLDEP.ENR = temp_dep.FACKID 
         MTRLDEP.BENAMNING = temp_dep.BENAMNING   
         MTRLDEP.ENHET = temp_dep.ENHET
         MTRLDEP.BESTKVANT = temp_dep.BESTKVANT 
         MTRLDEP.BESTPUNKT = temp_dep.BESTPUNKT    
         MTRLDEP.INVDATUM = TODAY
         MTRLDEP.INVANT = 0
         MTRLDEP.FACKID = temp_dep.FACKID.          
         MTRLDEP.LEVKOD = "4".
      END.   
   END.
   ELSE IF SUBSTRING(temp_dep.ENR,1,3) = "Jär" THEN DO:
      FIND FIRST MTRL WHERE MTRL.ENR = temp_dep.FACKID AND MTRL.LEVKOD = "3"
      NO-LOCK NO-ERROR.
      IF AVAILABLE MTRL THEN DO: 
         CREATE MTRLDEP.
         ASSIGN
         MTRLDEP.DEPNR = temp_dep.DEPNR         
         MTRLDEP.ENR = temp_dep.FACKID 
         MTRLDEP.BENAMNING = temp_dep.BENAMNING   
         MTRLDEP.ENHET = temp_dep.ENHET
         MTRLDEP.BESTKVANT = temp_dep.BESTKVANT 
         MTRLDEP.BESTPUNKT = temp_dep.BESTPUNKT    
         MTRLDEP.INVDATUM = TODAY
         MTRLDEP.INVANT = 0
         MTRLDEP.FACKID = temp_dep.FACKID.          
         MTRLDEP.LEVKOD = "3".
      END.
      ELSE DO:
         CREATE MTRL.
         ASSIGN
         MTRL.ENR = temp_dep.FACKID
         MTRL.BENAMNING = temp_dep.BENAMNING
         MTRL.ENHET = temp_dep.ENHET
         MTRL.LEVKOD = "3".
         
         CREATE MTRLDEP.   
         ASSIGN
         MTRLDEP.DEPNR = temp_dep.DEPNR         
         MTRLDEP.ENR = temp_dep.FACKID 
         MTRLDEP.BENAMNING = temp_dep.BENAMNING   
         MTRLDEP.ENHET = temp_dep.ENHET
         MTRLDEP.BESTKVANT = temp_dep.BESTKVANT 
         MTRLDEP.BESTPUNKT = temp_dep.BESTPUNKT    
         MTRLDEP.INVDATUM = TODAY
         MTRLDEP.INVANT = 0
         MTRLDEP.FACKID = temp_dep.FACKID.          
         MTRLDEP.LEVKOD = "3".
      END.   
   END.
   ELSE DO:   
      CREATE MTRLDEP.
      ASSIGN
      MTRLDEP.DEPNR = temp_dep.DEPNR         
      MTRLDEP.ENR = temp_dep.ENR 
      MTRLDEP.BENAMNING = temp_dep.BENAMNING   
      MTRLDEP.ENHET = temp_dep.ENHET
      MTRLDEP.BESTKVANT = temp_dep.BESTKVANT 
      MTRLDEP.BESTPUNKT = temp_dep.BESTPUNKT    
      MTRLDEP.INVDATUM = TODAY
      MTRLDEP.INVANT = 0
      MTRLDEP.FACKID = temp_dep.FACKID. 
      IF temp_dep.DEPNR = 71 OR temp_dep.DEPNR = 711 THEN
      MTRLDEP.LEVKOD = "2".
      ELSE MTRLDEP.LEVKOD  = "1".
   END.   
END.  
   
PROCEDURE fack_UI.
   /*SIFFROR*/
   ASSIGN
   var2 = temp_dep.SIFF  
   var2 = TRIM(var2)
   langd = LENGTH(var2).  
   IF langd = 6 THEN DO:
      temp_dep.SIFF = var2.      
   END.
   ELSE DO:
      DO WHILE langd < 6:
         ASSIGN
         var2 = " " + var2
         langd = langd + 1.
      END.  
      temp_dep.SIFF = var2.     
   END. 
      
   /*BOKSTÄVER*/
   ASSIGN
   var1 = temp_dep.BOK  
   var1 = TRIM(var1)
   langd = LENGTH(var1).  
   IF langd = 2 THEN DO:
      temp_dep.BOK = var1.      
   END.
   ELSE DO:
      DO WHILE langd < 2:
         ASSIGN
         var1 = var1 + " " 
         langd = langd + 1.
      END.  
      temp_dep.BOK = var1.     
   END.         
   temp_dep.FACKID = var1 + var2. 
END PROCEDURE. /* fack_UI */                 
