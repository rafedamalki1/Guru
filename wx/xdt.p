

/*KALKHMT.P*/
&Scoped-define NEW NEW
{KALKALLTEMP.I}  
{KALKSTART.I}
DEFINE VARIABLE valomr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE musz AS LOGICAL NO-UNDO.
CREATE uppkalktemp.
ASSIGN
uppkalktemp.OMRADE = "1"
uppkalktemp.AKIN = 1
uppkalktemp.BESTID = "1".
{DYNHMT.I}

RUN skapvalda_UI.
FOR EACH utvaldfasttemp:
   DISP utvaldfasttemp.bestid.
END.
PROCEDURE skapvalda_UI:
   FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR.   
   FOR EACH utvaldfasttemp:
      DELETE utvaldfasttemp.
   END.
   ASSIGN 
   valomr = "ALLA".
   FIND FIRST uppkalktemp NO-ERROR.
   IF uppkalktemp.OMRADE NE "ALLA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = uppkalktemp.OMRADE NO-LOCK NO-ERROR. 
      valomr = OMRADETAB.OMRADE.
   END.
   IF uppkalktemp.AKIN = 1 THEN DO:
      kommandoquery = "FASTSPEC.AKTIV = TRUE ".      
   END.
   ELSE IF uppkalktemp.AKIN = 2 THEN DO:
      kommandoquery = "FASTSPEC.AKTIV = FALSE ".      
   END.
   ELSE IF uppkalktemp.AKIN = 3 THEN DO:
      kommandoquery = " ".      
   END.
   IF valomr NE "ALLA" THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " FASTSPEC.OMRADE = " +  "'" + valomr + "'".
   END.
   IF uppkalktemp.TYP NE 0 THEN DO:
      RUN and_UI.
      kommandoquery = kommandoquery + " FASTSPEC.TYP = " + STRING(uppkalktemp.TYP).
   END.
   ASSIGN
   utvaltab   = "FOR EACH uppkalktemp"
   nytab      = "utvaldfasttemp"
   orginaltab = "FASTSPEC".
   ASSIGN
   kommandoquery = "FOR EACH " +  orginaltab + " WHERE " + kommandoquery + " NO-LOCK".   
   /*BUGG 9.1c FIX*/
   ASSIGN extratemptabh = TEMP-TABLE utvaldfasttemp:DEFAULT-BUFFER-HANDLE.
   ASSIGN extratemptabh2 = TEMP-TABLE uppkalktemp:DEFAULT-BUFFER-HANDLE.
   RUN dynquery_UI (INPUT TRUE,INPUT FALSE).
END PROCEDURE.
PROCEDURE dynstartkoll_UI:  
   musz = FALSE.
   IF uppkalktemp.BESTID NE "ALLA" THEN DO:
      kommandonyfalt = "BESTID".
      kommandoorgfalt = "BESTID".
      RUN dynakoll_UI (OUTPUT musz).
      IF musz = TRUE THEN RETURN.
   END.
  
END PROCEDURE. 
   

