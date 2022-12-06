/*IDTRP.I*/   
   CREATE tidut.
   ASSIGN
   utrec = RECID(tidut).
   CREATE tidut. 
   ASSIGN
   SUBSTRING(tidut.UT,1) = str1 
   utrec2 = RECID(tidut)
   kant = 2.
   DEFINE VARIABLE lage AS INTEGER.
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      musz = musz.
   END.
   ELSE DO: 
      IF AVAILABLE id_temp THEN DO:
         IF id_temp.FORNR NE "" THEN DO:  
            CREATE tidut.           
            ASSIGN    
            kant = kant + 1
            lage = 2                                                                                                   
            SUBSTRING(tidut.UT,1) = SUBSTRING(ANNNAMN.TAB1,1,20)
            SUBSTRING(tidut.UT,21) = ":"      
            SUBSTRING(tidut.UT,22) = id_temp.FORNR.
         END.
         ELSE DO:
            lage = 1.   
         END.
         IF id_temp.LINNR NE "" THEN DO:
            IF lage = 1 THEN DO:
               CREATE tidut.           
               ASSIGN    
               kant = kant + 1
               lage = 2
               SUBSTRING(tidut.UT,1) = SUBSTRING(ANNNAMN.TAB2,1,20)
               SUBSTRING(tidut.UT,21) = ":"            
               SUBSTRING(tidut.UT,22) = id_temp.LINNR.
            END.   
            ELSE DO:
               ASSIGN
               SUBSTRING(tidut.UT,40) = SUBSTRING(ANNNAMN.TAB2,1,20)
               SUBSTRING(tidut.UT,61) = ":"
               SUBSTRING(tidut.UT,62) = id_temp.LINNR
               lage = 1.
            END. 
         END.
         IF id_temp.NATNR NE "" THEN DO:
            IF lage = 1 THEN DO:              
               CREATE tidut.
               ASSIGN
               kant = kant + 1
               SUBSTRING(tidut.UT,1) = SUBSTRING(ANNNAMN.TAB3,1,20)
               SUBSTRING(tidut.UT,21) = ":"            
               SUBSTRING(tidut.UT,22) = id_temp.NATNR.
            END.
            ELSE DO:
               ASSIGN
               SUBSTRING(tidut.UT,40) = SUBSTRING(ANNNAMN.TAB3,1,20)
               SUBSTRING(tidut.UT,61) = ":"            
               SUBSTRING(tidut.UT,62) = id_temp.NATNR.
            END.   
         END.   
      END.  
      /*CREATE tidut.           
 *       ASSIGN      
 *       kant = kant + 1                                                                                     
 *       SUBSTRING(tidut.UT,1) = SUBSTRING(ANNNAMN.TAB1,1,20)
 *       SUBSTRING(tidut.UT,21) = ":".
 *       IF AVAILABLE id_temp THEN
 *       SUBSTRING(tidut.UT,22) = id_temp.FORNR.
 *       ASSIGN
 *       SUBSTRING(tidut.UT,40) = SUBSTRING(ANNNAMN.TAB2,1,20)
 *       SUBSTRING(tidut.UT,61) = ":".
 *       IF AVAILABLE id_temp THEN
 *       SUBSTRING(tidut.UT,62) = id_temp.LINNR.
 *       CREATE tidut.
 *       ASSIGN
 *       kant = kant + 1
 *       SUBSTRING(tidut.UT,1) = SUBSTRING(ANNNAMN.TAB3,1,20)
 *       SUBSTRING(tidut.UT,21) = ":".
 *       IF AVAILABLE id_temp THEN
 *       SUBSTRING(tidut.UT,22) = id_temp.NATNR.*/
   END.   
   CREATE tidut.
   kant = kant + 1.
   FIND FIRST BBENAMNING WHERE BBENAMNING.KONSKOD = mtrl_temp2.GRUPP 
   USE-INDEX KOD NO-LOCK NO-ERROR.
   IF BBENAMNING.ID1 NE "" THEN DO:
      ASSIGN
      SUBSTRING(tidut.UT,1) = BBENAMNING.ID1
      SUBSTRING(tidut.UT,21) = ":".
      IF AVAILABLE id_temp THEN DO:
         IF id_temp.FRI1 NE ? THEN
         SUBSTRING(tidut.UT,22) = STRING(id_temp.FRI1).
      END.   
   END.   
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      IF BBENAMNING.ID2 NE "" THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,1) = BBENAMNING.ID2
         SUBSTRING(tidut.UT,21) = ":".
         IF AVAILABLE id_temp THEN DO:
            IF id_temp.FRI2 NE ? THEN
            SUBSTRING(tidut.UT,22) = STRING(id_temp.FRI2).                                                      
         END.   
      END.
   END.
   ELSE DO:
      IF BBENAMNING.ID2 NE "" THEN DO:
         ASSIGN
         SUBSTRING(tidut.UT,40) = BBENAMNING.ID2
         SUBSTRING(tidut.UT,61) = ":".
         IF AVAILABLE id_temp THEN DO:
            IF id_temp.FRI2 NE ? THEN
            SUBSTRING(tidut.UT,62) = STRING(id_temp.FRI2).                                                      
         END.   
      END. 
   END.      
   IF AVAILABLE id_temp THEN DO:
      IF id_temp.FRI3 NE "" THEN DO:
         CREATE tidut.
         ASSIGN
         kant = kant + 1.
         IF Guru.Konstanter:globforetag = "GRAN" OR Guru.Konstanter:globforetag = "GKAL" OR Guru.Konstanter:globforetag = "NAEK" OR Guru.Konstanter:globforetag = "CPOMA" THEN DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = "Littera"
            SUBSTRING(tidut.UT,21) = ":".
         END.
         ELSE DO:
            ASSIGN
            SUBSTRING(tidut.UT,1) = "Fri ID"
            SUBSTRING(tidut.UT,21) = ":".
         END.         
         IF id_temp.FRI3 NE ? THEN
         SUBSTRING(tidut.UT,22) = STRING(id_temp.FRI3).
      END.   
   END.
   
   
