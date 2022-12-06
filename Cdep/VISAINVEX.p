/*VISAINVEX.P*/
{TIDUTTTNEW.I}
DEFINE INPUT PARAMETER TABLE FOR tidut.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE SHARED VARIABLE colnamevar AS CHARACTER NO-UNDO.
{GLOBVAR2DEL1.I}

{EXECLIN.I}

RUN lista_UI.
RUN slutexcel_UI. 

PROCEDURE lista_UI:
   IF colnamevar = "FACKID" THEN DO:
      /*Vilka kolumner*/
      ASSIGN
      slutbredd = 7
      utnr[1] = 1
      utnr[2] = 10
      utnr[3] = 22
      utnr[4] = 61
      utnr[5] = 72
      utnr[6] = 78
      utnr[7] = 88.
   END.
   ELSE IF colnamevar = "ENR" THEN DO:
      /*Vilka kolumner*/
      ASSIGN
      slutbredd = 7
      utnr[1] = 1
      utnr[2] = 12
      utnr[3] = 22
      utnr[4] = 61
      utnr[5] = 72
      utnr[6] = 78
      utnr[7] = 88.
   END.
   ELSE IF colnamevar = "BENAMNING" THEN DO:
      /*Vilka kolumner*/
      ASSIGN
      slutbredd = 10
      utnr[1] = 1
      utnr[2] = 12
      utnr[3] = 20
      utnr[4] = 32
      utnr[5] = 42
      utnr[6] = 51
      utnr[7] = 62
      utnr[8] = 72
      utnr[9] = 78
      utnr[10] = 88.
   END.   
   RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
   RUN startexcel_UI.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   /*Poster*/
   raknare = 1.
   FIND FIRST tidut NO-ERROR.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE IF SUBSTRING(tidut.UT,88,8) = "========" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE IF SUBSTRING(tidut.UT,1,11) = "INVENTERADE" THEN DO:      
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
      ELSE IF SUBSTRING(tidut.UT,1,8) = "SORTERAT" THEN DO:      
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      END.
      ELSE DO:
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.
END PROCEDURE.
