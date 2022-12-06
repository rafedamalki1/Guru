   /*BTNCOLH.I*/
   DEFINE INPUT PARAMETER blefth AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER brighth AS HANDLE NO-UNDO.
   DEFINE OUTPUT PARAMETER outblefth AS HANDLE NO-UNDO.
   DEFINE VARIABLE parenth AS HANDLE NO-UNDO.
   outblefth =  brighth.
   Guru.GlobalaVariabler:collefth = outblefth.
   parenth = brighth:PARENT.
   IF blefth = ? THEN DO:
      IF parenth:WIDTH = 0 THEN.
      ELSE brighth:COLUMN = parenth:WIDTH - 35.5 .
      RETURN.
   END.
   ELSE brighth:COLUMN = blefth:COLUMN - brighth:WIDTH-CHARS.
       /*
   IF blefth:IMAGE NE "" THEN brighth:COLUMN = brighth:COLUMN - 0.25.   
          */
   