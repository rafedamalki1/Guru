DEFINE VARIABLE  pkod LIKE SUMTIDDAG.PERSONALKOD NO-UNDO.
DEFINE VARIABLE varprisi LIKE TIDREGITAB.PRIS NO-UNDO.
DEFINE VARIABLE varprisiproj LIKE TIDREGITAB.PRIS NO-UNDO.   

   OPEN QUERY sumq FOR EACH SUMTIDDAG WHERE SUMTIDDAG.VECKOKORD = "V920" NO-LOCK 
   BY SUMTIDDAG.VECKOKORD BY SUMTIDDAG.PERSONALKOD.  
   DO TRANSACTION:   
      GET FIRST sumq EXCLUSIVE-LOCK.
      IF AVAILABLE SUMTIDDAG THEN DO:
         IF pkod NE SUMTIDDAG.PERSONALKOD THEN DO:
            pkod = SUMTIDDAG.PERSONALKOD.            
            /*PROJ2 990517*/
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.                          
            varprisi = TIMKOSTNADSTAB.PRISA.
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INT.PROJ." 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.
            varprisiproj = TIMKOSTNADSTAB.PRISA.                            
         END.
         IF AVAILABLE TIMKOSTNADSTAB THEN DO:
            ASSIGN SUMTIDDAG.PRISI = varprisi.
            /*PROJ2 990517*/
            IF SUMTIDDAG.OMRADE NE SUMTIDDAG.GEOMRADE THEN DO:
               IF SUBSTRING(SUMTIDDAG.OMRADE,1,1) = "2" AND 
                  SUBSTRING(SUMTIDDAG.GEOMRADE,1,1) = "2" THEN DO:
                  ASSIGN SUMTIDDAG.PRISI = varprisiproj.  
               END.
            END.                             
         END.                     
         ELSE DO: 
            ASSIGN SUMTIDDAG.PRISI = SUMTIDDAG.PRIS.
         END.
      END. 
   END.             
   NAMN2:
   REPEAT TRANSACTION:                    
      GET NEXT sumq EXCLUSIVE-LOCK.          
      IF NOT AVAILABLE SUMTIDDAG THEN LEAVE NAMN2.
      ELSE DO :
         IF pkod NE SUMTIDDAG.PERSONALKOD THEN DO:
            pkod = SUMTIDDAG.PERSONALKOD.
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.                          
            varprisi = TIMKOSTNADSTAB.PRISA.
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = SUMTIDDAG.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INT.PROJ." 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.
            varprisiproj = TIMKOSTNADSTAB.PRISA. 
         END. 
      END.
      IF AVAILABLE TIMKOSTNADSTAB THEN DO:
         ASSIGN SUMTIDDAG.PRISI = varprisi.
         /*PROJ2 990517*/
         IF SUMTIDDAG.OMRADE NE SUMTIDDAG.GEOMRADE THEN DO:
            IF SUBSTRING(SUMTIDDAG.OMRADE,1,1) = "2" AND 
            SUBSTRING(SUMTIDDAG.GEOMRADE,1,1) = "2" THEN DO:
               ASSIGN SUMTIDDAG.PRISI = varprisiproj.  
            END.
         END.
      END.                                
      ELSE DO: 
         ASSIGN SUMTIDDAG.PRISI =  SUMTIDDAG.PRIS.
      END.      
      DISPLAY SUMTIDDAG.OMRADE SUMTIDDAG.GEOMRADE SUMTIDDAG.PRISI varprisi varprisiproj SUMTIDDAG.PERSONALKOD SUMTIDDAG.DATUM WITH FRAME CC DOWN.
      DOWN 1 WITH FRAME CC.
   END.      
   OPEN QUERY sumq2 FOR EACH sumtid WHERE sumtid.VECKOKORD = "V920" NO-LOCK 
   BY sumtid.VECKOKORD BY sumtid.PERSONALKOD.  
   DO TRANSACTION:   
      GET FIRST sumq2 EXCLUSIVE-LOCK.
      IF AVAILABLE sumtid THEN DO:
         IF pkod NE sumtid.PERSONALKOD THEN DO:
            pkod = sumtid.PERSONALKOD.            
            /*PROJ2 990517*/
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = sumtid.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.                          
            varprisi = TIMKOSTNADSTAB.PRISA.
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = sumtid.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INT.PROJ." 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.
            varprisiproj = TIMKOSTNADSTAB.PRISA.                            
         END.
         IF AVAILABLE TIMKOSTNADSTAB THEN DO:
            ASSIGN sumtid.PRISI = varprisi.
            /*PROJ2 990517*/
            IF sumtid.OMRADE NE sumtid.GEOMRADE THEN DO:
               IF SUBSTRING(sumtid.OMRADE,1,1) = "2" AND 
                  SUBSTRING(sumtid.GEOMRADE,1,1) = "2" THEN DO:
                  ASSIGN sumtid.PRISI = varprisiproj.  
               END.
            END.                             
         END.                     
         ELSE DO: 
            ASSIGN sumtid.PRISI = sumtid.PRIS.
         END.
      END. 
   END.             
   NAMN2:
   REPEAT TRANSACTION:                    
      GET NEXT sumq2 EXCLUSIVE-LOCK.          
      IF NOT AVAILABLE sumtid THEN LEAVE NAMN2.
      ELSE DO :
         IF pkod NE sumtid.PERSONALKOD THEN DO:
            pkod = sumtid.PERSONALKOD.
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = sumtid.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INTERNPRI" 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.                          
            varprisi = TIMKOSTNADSTAB.PRISA.
            FIND FIRST TIMKOSTNADSTAB WHERE 
            TIMKOSTNADSTAB.PERSONALKOD = sumtid.PERSONALKOD AND 
            TIMKOSTNADSTAB.PRISTYP = "INT.PROJ." 
            USE-INDEX PRISPERS NO-LOCK NO-ERROR.
            varprisiproj = TIMKOSTNADSTAB.PRISA. 
         END. 
      END.
      IF AVAILABLE TIMKOSTNADSTAB THEN DO:
         ASSIGN sumtid.PRISI = varprisi.
         /*PROJ2 990517*/
         IF sumtid.OMRADE NE sumtid.GEOMRADE THEN DO:
            IF SUBSTRING(sumtid.OMRADE,1,1) = "2" AND 
            SUBSTRING(sumtid.GEOMRADE,1,1) = "2" THEN DO:
               ASSIGN sumtid.PRISI = varprisiproj.  
            END.
         END.
      END.                                
      ELSE DO: 
         ASSIGN sumtid.PRISI =  sumtid.PRIS.
      END.      
      DISPLAY sumtid.OMRADE sumtid.GEOMRADE sumtid.PRISI varprisi varprisiproj sumtid.PERSONALKOD sumtid.DATUM WITH FRAME CC2 DOWN.
      DOWN 1 WITH FRAME CC2.
   END.
