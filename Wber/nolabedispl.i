/*nolabedispl.i*/ 
   ASSIGN      
   kon_display.F1:LABEL IN BROWSE BRW_VAL = "" 
   kon_display.F2:LABEL IN BROWSE BRW_VAL = ""  
   kon_display.F3:LABEL IN BROWSE BRW_VAL = ""
   kon_display.F4:LABEL IN BROWSE BRW_VAL = ""      
   kon_display.F5:LABEL IN BROWSE BRW_VAL = ""      
   kon_display.F6:LABEL IN BROWSE BRW_VAL = "".
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_display.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
   END.
   ELSE DO:
      kon_display.ID2:LABEL IN BROWSE BRW_VAL = "Id".
   END.   
