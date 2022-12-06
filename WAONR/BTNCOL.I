   /*BTNCOL.I*/
   DEFINE INPUT PARAMETER blefth AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER brighth AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER outblefth AS HANDLE NO-UNDO.
   outblefth =  brighth.
   Guru.GlobalaVariabler:collefth = outblefth.
   IF blefth = ? THEN DO:
      brighth:COLUMN = 16. 
      RETURN.
   END.
   ELSE brighth:COLUMN = (blefth:COLUMN + blefth:WIDTH-CHARS).   
   IF blefth:IMAGE NE "" THEN brighth:COLUMN = (brighth:COLUMN - 0.25).   
   
