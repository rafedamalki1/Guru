/*INKOLLFINNS.I*/
   DEFINE INPUT  PARAMETER vad AS INTEGER NO-UNDO.
   DEFINE OUTPUT PARAMETER postfinns AS LOGICAL NO-UNDO.
   DEFINE OUTPUT PARAMETER postantal AS DECIMAL NO-UNDO.
   IF vad = 1 THEN DO:
      /*är något inköpt på konst*/
      FIND FIRST sumbernum_mtrl WHERE sumbernum_mtrl.NUM = kon_val.NUM        
      NO-LOCK NO-ERROR.    
      IF AVAILABLE sumbernum_mtrl THEN postfinns = TRUE.
      ELSE postfinns = FALSE.
   END. 
   ELSE IF vad = 2 THEN DO:
      /* ändra komp*/
      FIND FIRST sumberskapnum_mtrl WHERE sumberskapnum_mtrl.NUM = andrakon_val.NUM AND 
      sumberskapnum_mtrl.SKAPNUM = andrakon_val.SKAPNUM             
      NO-LOCK NO-ERROR.    
      IF AVAILABLE sumberskapnum_mtrl THEN postfinns = TRUE.
      ELSE postfinns = FALSE.
   END.  
   ELSE IF vad = 3 THEN DO:
      /*är något inköpt på skapnum*/
      FIND FIRST sumberskapnum_mtrl WHERE sumberskapnum_mtrl.NUM = kon_val.NUM AND 
      sumberskapnum_mtrl.SKAPNUM = kon_val.SKAPNUM             
      NO-LOCK NO-ERROR.    
      IF AVAILABLE sumberskapnum_mtrl THEN postfinns = TRUE.
      ELSE postfinns = FALSE.
   END.
   ELSE IF vad = 4 THEN DO:
      /*är något inköpt kabelskåp*/
      FIND FIRST sumberskapnum_mtrl WHERE sumberskapnum_mtrl.NUM = kon_val.NUM AND 
      sumberskapnum_mtrl.SKAPNUM = 99             
      NO-LOCK NO-ERROR.    
      IF AVAILABLE sumberskapnum_mtrl THEN postfinns = TRUE.
      ELSE postfinns = FALSE.
   END.
   ELSE IF vad = 5 THEN DO:
      /*är något inköpt MATERIEL*/
      FIND FIRST sumberskapnum_mtrl WHERE sumberskapnum_mtrl.NUM = list_mtrl.NUM AND 
      sumberskapnum_mtrl.SKAPNUM = list_mtrl.SKAPNUM AND sumberskapnum_mtrl.ENR = list_mtrl.ENR AND
      sumberskapnum_mtrl.LEVKOD = list_mtrl.LEVKOD           
      NO-LOCK NO-ERROR.    
      IF AVAILABLE sumberskapnum_mtrl THEN postfinns = TRUE.
      ELSE postfinns = FALSE.
   END.
   ELSE IF vad = 6 THEN DO:
      /*är något inköpt MATERIEL ANTAL*/
      FIND FIRST sumberskapnum_mtrl WHERE sumberskapnum_mtrl.NUM = list_mtrl.NUM AND 
      sumberskapnum_mtrl.SKAPNUM = list_mtrl.SKAPNUM AND sumberskapnum_mtrl.ENR = list_mtrl.ENR AND
      sumberskapnum_mtrl.LEVKOD = list_mtrl.LEVKOD           
      NO-LOCK NO-ERROR.    
      IF AVAILABLE sumberskapnum_mtrl THEN DO:
         ASSIGN
         postantal = sumberskapnum_mtrl.INKANTAL
         postfinns = TRUE.
      END.   
      ELSE postfinns = FALSE.
   END.
   ELSE IF vad = 7 THEN DO:
      /*är något inköpt MATERIEL*/
      FIND FIRST sumberskapnum_mtrl WHERE sumberskapnum_mtrl.NUM = elist_mtrl.NUM AND 
      sumberskapnum_mtrl.SKAPNUM = elist_mtrl.SKAPNUM AND sumberskapnum_mtrl.ENR = elist_mtrl.ENR AND
      sumberskapnum_mtrl.LEVKOD = elist_mtrl.LEVKOD           
      NO-LOCK NO-ERROR.    
      IF AVAILABLE sumberskapnum_mtrl THEN DO:
         ASSIGN
         postantal = sumberskapnum_mtrl.INKANTAL
         postfinns = TRUE.
      END.   
      ELSE postfinns = FALSE.
   END.
   
