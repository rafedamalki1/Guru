/*NOLABEL.I*/ 
   ASSIGN      
   kon_val.F1:LABEL IN BROWSE BRW_VAL = "" 
   kon_val.F2:LABEL IN BROWSE BRW_VAL = ""  
   kon_val.F3:LABEL IN BROWSE BRW_VAL = ""
   kon_val.F4:LABEL IN BROWSE BRW_VAL = ""      
   kon_val.F5:LABEL IN BROWSE BRW_VAL = ""      
   kon_val.F6:LABEL IN BROWSE BRW_VAL = "".
   IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" THEN DO:
      kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Objektnr".
   END.
   ELSE DO:
      kon_val.ID2:LABEL IN BROWSE BRW_VAL = "Id".
   END.   
