/*ANMARK.I*/      
   edtext = REPLACE(edtext,CHR(9)," ").   
   IF retvar NE 0 THEN DO:
      DO WHILE retvar NE 0:              
         RUN SUBMARK.P           
         (INPUT-OUTPUT ednum, INPUT ednum3, INPUT edtext, INPUT edtecken, INPUT-OUTPUT retvar, OUTPUT tidtext).         
         IF retvar = 0 THEN DO:                                      
            DO WHILE ednum <= ednum3:
               RUN anmark_UI (INPUT 1).                  
               ednum = ednum + edtecken.            
            END.  
         END.
         ELSE DO:      
            RUN anmark_UI (INPUT 2).                          
         END.   
      END.
   END.
   ELSE DO:
      ASSIGN
      ednum = 0
      ednum2 = 0.         
      DO WHILE ednum <= ednum3:  
         RUN anmark_UI (INPUT 3).                
         ASSIGN
         ednum = ednum + edtecken
         ednum2 = ednum2 + 1.            
      END.
   END.
