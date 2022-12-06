/*DEPHMT.I*/
   ASSIGN
   nytab      = "depatemp"
   orginaltab = "DEPA". 
   kommandoquery = "".
   kommandoquery = "FOR EACH " +  orginaltab + " NO-LOCK".       
   /*BUGG 9.1c FIX*/
   ASSIGN extratemptabh = TEMP-TABLE depatemp:DEFAULT-BUFFER-HANDLE.
   RUN dynquery_UI (INPUT FALSE,INPUT FALSE).
   FOR EACH depatemp:
      ASSIGN
      depatemp.FakPnr = REPLACE(depatemp.FakPnr," ","")
      depatemp.LevPnr = REPLACE(depatemp.LevPnr," ","")
      depatemp.Pnr    = REPLACE(depatemp.Pnr   ," ","").
   END.

   ASSIGN
   nytab      = "levtemp"
   orginaltab = "LEVERANTOR". 
   kommandoquery = "LEVERANTOR.BORTTAG = FALSE".
   kommandoquery = "FOR EACH " +  orginaltab + " WHERE " + kommandoquery + " NO-LOCK".       
   /*BUGG 9.1c FIX*/
   ASSIGN extratemptabh = TEMP-TABLE levtemp:DEFAULT-BUFFER-HANDLE.
   RUN dynquery_UI (INPUT FALSE,INPUT FALSE).   
   RUN objdelete_UI. 
