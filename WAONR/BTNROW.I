   /*BTNROW.I*/
   DEFINE INPUT PARAMETER blefth AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER brighth AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER outblefth AS HANDLE NO-UNDO.

   outblefth =  brighth.
   Guru.GlobalaVariabler:collefth = outblefth.
   IF brighth:TYPE = "BUTTON" THEN DO:
      
      IF blefth = ? THEN DO:
         IF startradvar = 0 THEN startradvar = 8.
         brighth:ROW = startradvar.
         pixvar = startradvar.
         IF Guru.Konstanter:alltidmax = TRUE THEN RUN rowplus_UI (INPUT blefth, INPUT brighth). 
         RETURN.
      END.    
      ELSE DO:
         /*
         IF blefth:HEIGHT-CHARS < 1.2 THEN pixvar = pixvar + 1 +  0.01.
         ELSE pixvar = pixvar + blefth:HEIGHT-CHARS + 0.01.
         */
         /*
         pixvar = pixvar + blefth:HEIGHT-CHARS + 0.01.
         */
         IF Guru.SharedVariable:globmaxforand = 0 THEN Guru.SharedVariable:globmaxforand = 1.
         IF Guru.Konstanter:alltidmax = TRUE THEN DO:
             
            pixvar = blefth:ROW +  (blefth:HEIGHT-CHARS + 0.01) * Guru.SharedVariable:globmaxforand.
            brighth:ROW = pixvar.
         END.
         ELSE DO:   
            pixvar = blefth:ROW +  (blefth:HEIGHT-CHARS + 0.01) * Guru.SharedVariable:globmaxforand.
            brighth:ROW = pixvar.
             
         END.
         
      END.
           
      IF blefth:IMAGE NE "" THEN DO: 
         IF blefth:HEIGHT-CHARS < 1.2 THEN pixvar = pixvar.
         ELSE DO:
            ASSIGN
            pixvar = pixvar + 0.1 
            brighth:ROW = brighth:ROW + 0.1.   
         END.
         
      END.
             
      IF brighth:IMAGE NE "" THEN DO: 
         IF blefth:HEIGHT-CHARS < 1.2 THEN pixvar = pixvar.
         ELSE pixvar = pixvar + 0.1. 
            /*
         brighth:HEIGHT-CHARS =  brighth:HEIGHT-CHARS - 0.1.   
         */
      END.
   END.
   ELSE DO:
      IF blefth = ? THEN DO:
         brighth:ROW = 8.
         RETURN.
      END.                                   
      ELSE brighth:ROW = blefth:ROW + blefth:HEIGHT-CHARS + 0.1.
           /*
           
      IF blefth:IMAGE NE "" THEN brighth:ROW = brighth:ROW - 0.1.   
      */
          
   END.
  
   
