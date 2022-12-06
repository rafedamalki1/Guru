/*ANMARK2.I*/     
   edtext = REPLACE(edtext,CHR(9)," ").   
   IF retvar = 0 THEN DO:                 
      IF SUBSTRING(edtext,edtecken,1) = " " THEN DO:  
         tidtext = SUBSTRING(edtext,ednum,edtecken).
         ednum = ednum + edtecken.
      END.
      ELSE DO:                  
         ednum2 = 1. 
         retvar = INDEX(edtext," ",ednum2).
         IF retvar > edtecken THEN DO:
            tidtext = SUBSTRING(edtext,ednum,edtecken).
            ednum = ednum + edtecken.
         END.
         ELSE IF retvar = 0 THEN DO:
            tidtext = SUBSTRING(edtext,ednum,edtecken).
            ednum = ednum + edtecken.
         END.
         ELSE DO:     
            DO WHILE ednum2 < edtecken:
               IF ednum2 > 0 THEN DO:
                  retvar = INDEX(edtext," ",ednum2).
                  ednum2 = INDEX(edtext," ",retvar + 1).                   
               END.
               ELSE LEAVE.   
            END.             
            tidtext = SUBSTRING(edtext,ednum,retvar).
            ednum = ednum + retvar.
         END.   
      END.
      RUN anmark_UI (INPUT 2).             
      DO WHILE ednum < ednum3:
         IF SUBSTRING(edtext,ednum + edtecken,1) = " " THEN DO:
            tidtext = SUBSTRING(edtext,ednum,edtecken).
            ednum = ednum + edtecken /*+ 1*/.
         END.
         ELSE DO:           
            ednum2 = ednum.
            retvar = INDEX(edtext," ",ednum2).
            IF retvar > edtecken + ednum THEN DO:
               tidtext = SUBSTRING(edtext,ednum,edtecken).
               ednum = ednum + edtecken.
            END.
            ELSE IF retvar = 0 THEN DO:
               tidtext = SUBSTRING(edtext,ednum,edtecken).
               ednum = ednum + edtecken.
            END.
            ELSE DO:               
               DO WHILE ednum2 < edtecken + ednum:
                  IF ednum2 > 0 THEN DO:
                     retvar = INDEX(edtext," ",ednum2).
                     ednum2 = INDEX(edtext," ",retvar + 1).                   
                  END.
                  ELSE LEAVE.   
               END. 
               tidtext = SUBSTRING(edtext,ednum,retvar - ednum).
               ednum = retvar + 1.
            END.   
         END.   
         RUN anmark_UI (INPUT 2).
      END.
   END.
   ELSE DO:
      IF retvar <= edtecken THEN DO:         
         tidtext = SUBSTRING(edtext,ednum,retvar - 1).
         ednum = retvar + 1.
      END.
      ELSE DO:
         IF SUBSTRING(edtext,edtecken,1) = " " THEN DO:
            tidtext = SUBSTRING(edtext,ednum,edtecken).
            ednum = ednum + edtecken /*+ 1*/.
         END.
         ELSE DO:    
            ednum2 = 1.  
            retvar = INDEX(edtext," ",ednum2).
            IF retvar > edtecken THEN DO:
               tidtext = SUBSTRING(edtext,ednum,edtecken).
               ednum = ednum + edtecken.
            END.
            ELSE IF retvar = 0 THEN DO:
               tidtext = SUBSTRING(edtext,ednum,edtecken).
               ednum = ednum + edtecken.
            END.
            ELSE DO:    
               DO WHILE ednum2 < edtecken:
                  IF ednum2 > 0 THEN DO:
                     retvar = INDEX(edtext," ",ednum2).
                     ednum2 = INDEX(edtext," ",retvar + 1).
                  END.
                  ELSE LEAVE.   
               END. 
               tidtext = SUBSTRING(edtext,ednum,retvar).
               ednum = ednum + retvar.  
            END.   
         END.
      END.
      retvar = INDEX(edtext,CHR(10),ednum).
      RUN anmark_UI (INPUT 2).
      DO WHILE retvar > 0:
         IF retvar <= edtecken + ednum THEN DO:         
            tidtext = SUBSTRING(edtext,ednum,retvar - ednum).
            ednum = retvar + 1.
         END.
         ELSE DO:
            IF SUBSTRING(edtext,edtecken + ednum,1) = " " THEN DO:
               tidtext = SUBSTRING(edtext,ednum,edtecken).
               ednum = ednum + edtecken /*+ 1*/.
            END.
            ELSE DO:    
               ednum2 = ednum.      
               retvar = INDEX(edtext," ",ednum2).
               IF retvar > edtecken + ednum THEN DO:
                  tidtext = SUBSTRING(edtext,ednum,edtecken).
                  ednum = ednum + edtecken.
               END.
               ELSE IF retvar = 0 THEN DO:
                  tidtext = SUBSTRING(edtext,ednum,edtecken).
                  ednum = ednum + edtecken.
               END.
               ELSE DO:
                  DO WHILE ednum2 < edtecken + ednum:
                     IF ednum2 > 0 THEN DO:
                        retvar = INDEX(edtext," ",ednum2).
                        ednum2 = INDEX(edtext," ",retvar + 1).
                     END.
                     ELSE LEAVE.   
                  END. 
                  tidtext = SUBSTRING(edtext,ednum,retvar - ednum).
                  ednum = retvar + 1.  
               END.   
            END.
         END.
         retvar = INDEX(edtext,CHR(10),ednum).
         RUN anmark_UI (INPUT 2).
      END.   
      DO WHILE ednum < ednum3:
         IF SUBSTRING(edtext,ednum + edtecken,1) = " " THEN DO:
            tidtext = SUBSTRING(edtext,ednum,edtecken).
            ednum = ednum + edtecken /*+ 1*/.
         END.
         ELSE DO:           
            ednum2 = ednum.
            retvar = INDEX(edtext," ",ednum2).
            IF retvar > edtecken + ednum THEN DO:
               tidtext = SUBSTRING(edtext,ednum,edtecken).
               ednum = ednum + edtecken.
            END.
            ELSE IF retvar = 0 THEN DO:
               tidtext = SUBSTRING(edtext,ednum,edtecken).
               ednum = ednum + edtecken.
            END.
            ELSE DO:               
               DO WHILE ednum2 < edtecken + ednum:
                  IF ednum2 > 0 THEN DO:
                     retvar = INDEX(edtext," ",ednum2).
                     ednum2 = INDEX(edtext," ",retvar + 1).                   
                  END.
                  ELSE LEAVE.   
               END. 
               tidtext = SUBSTRING(edtext,ednum,retvar - ednum).
               ednum = retvar + 1.
            END.   
         END.   
         RUN anmark_UI (INPUT 2).
      END.
   END.
