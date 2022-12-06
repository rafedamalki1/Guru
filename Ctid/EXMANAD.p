/*EXMANAD.P*/
{TIDUTTTNEW.I}
DEFINE INPUT PARAMETER excellista AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER globforetagIN AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidut.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
DEFINE VARIABLE kommando AS CHARACTER FORMAT "X(20)" NO-UNDO.
DEFINE VARIABLE bladvar2 AS INTEGER NO-UNDO.
DEFINE VARIABLE protvar AS INTEGER NO-UNDO.
{GLOBVAR2DEL1.I}

{EXECLIN.I}
/*Vilka kolumner*/
ASSIGN
startc = "A"
slutc = "J"
slutbredd = 20
utnr[1] = 1
utnr[2] = 12
utnr[3] = 33
utnr[4] = 41
utnr[5] = 49
utnr[6] = 57        
utnr[7] = 65
utnr[8] = 72
utnr[9] = 80.
RUN satestat_UI. 
RUN startexcel_UI.
bladvar = 1.   
chWorkSheet:Name = "Månadssammanställning" NO-ERROR.   
FIND FIRST tidut NO-LOCK NO-ERROR.
/*Kolumnbredd*/
raknare = 1.

/*Rubriker*/
FIND FIRST tidut NO-LOCK NO-ERROR.
REPEAT:
   RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
   {EXCELFEL.I}
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidut THEN DO:
      LEAVE.
   END.
   IF SUBSTRING(tidut.UT,utnr[3],6) = "NORMAL" THEN DO:    /*Rubrikslut*/
      LEAVE.
   END.
END.
raknare = 1.
/*Kolumnbredd*/
RUN kolumnexcel_UI.
/*Poster*/
raknare = 1.
REPEAT:
   IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
      musz = musz.
   END.
   ELSE DO:
      RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
   END.   
   {EXCELFEL.I}
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidut THEN DO:
      LEAVE.
   END.
   
   IF SUBSTRING(tidut.UT,1,13) = "Förtroendetid" OR  SUBSTRING(tidut.UT,12,9) = "SUMMERING" OR SUBSTRING(tidut.UT,4,17) = "Intjänad komp och"  THEN LEAVE.
END.

IF AVAILABLE tidut  THEN DO:
   
   IF SUBSTRING(tidut.UT,4,17) = "Intjänad komp och" THEN DO:
      raknare = 1.
      iRad = 1.
      ASSIGN
      startc = "A"
      slutc = "P"
      slutbredd = 15
      utnr[1] = 1
      utnr[2] = 9
      utnr[3] = 16
      utnr[4] = 22
      utnr[5] = 29
      utnr[6] = 36
      utnr[7] = 43
      utnr[8] = 50
      utnr[9] = 57
      utnr[10] = 64
      utnr[11] = 71
      utnr[12] = 78
      utnr[13] = 85
      utnr[14] = 93
      utnr[15] = 100
      utnr[16] = 107.
              
      estartnr[1] = 0.
      estartnr[2] = 0.
      estartnr[3] = 0.
      estartnr[4] = 0.
      estartnr[5] = 0.
      estartnr[6] = 0.
      estartnr[7] = 0.
      estartnr[8] = 0.
      estartnr[9] = 0.
      estartnr[10] = 0.
      estartnr[11] = 0.
      estartnr[12] = 0.
      estartnr[13] = 0.
      estartnr[14] = 0.
   
      RUN kompinut_UI.      
      {EXCELFEL.I}
   END.
   IF SUBSTRING(tidut.UT,1,13) = "Förtroendetid" THEN DO:
      raknare = 1.
      iRad = 1.
      ASSIGN
      startc = "A"
      slutc = "E"
      slutbredd = 15
      utnr[1] = 1
      utnr[2] = 11
      utnr[3] = 23
      utnr[4] = 32
      utnr[5] = 43
      
      /*nollställning från föregående blad*/
      utnr[6] = 0
      utnr[7] = 0
      utnr[8] = 0
      utnr[9] = 0.        
      estartnr[1] = 0.
      estartnr[2] = 0.
      estartnr[3] = 0.
      estartnr[4] = 0.
      estartnr[5] = 0.
      estartnr[6] = 0.
      estartnr[7] = 0.
      estartnr[8] = 0.
      estartnr[9] = 0.   
      RUN ftro_UI.      
      {EXCELFEL.I}
   END. 
   IF SUBSTRING(tidut.UT,12,9) = "SUMMERING" THEN DO:
      raknare = 1.
      iRad = 1.
      ASSIGN
      startc = "A"
      slutc = "G"
      slutbredd = 7
      utnr[1] = 1
      utnr[2] = 11
      utnr[3] = 37
      utnr[4] = 45
      utnr[5] = 51
      utnr[6] = 62
      utnr[7] = 69
      /*nollställning från föregående blad*/
      utnr[8] = 0
      utnr[9] = 0.        
      estartnr[1] = 0.
      estartnr[2] = 0.
      estartnr[3] = 0.
      estartnr[4] = 0.
      estartnr[5] = 0.
      estartnr[6] = 0.
      estartnr[7] = 0.
      estartnr[8] = 0.
      estartnr[9] = 0.   
      RUN summerlon_UI.      
      {EXCELFEL.I}
   END.
END.
RUN slutexcel_UI.
{EXCELFEL.I}
   

PROCEDURE summerlon_UI:
   RUN nyttbladexcel_UI.      
   {EXCELFEL.I}
   chWorkSheet:Name = "Summering av tillägg"  NO-ERROR.
   RUN satestat_UI. 
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,12,7) = "TILLÄGG" THEN DO:    /*Rubrikslut*/
         LEAVE.
      END.
   END.      
   /*Kolumnbredd*/
   raknare = 1.
   RUN kolumnexcel_UI.
   /*Poster*/
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,12,9) = "=========" THEN DO:
         musz = musz.
      END.
      ELSE DO:
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,2,9) = "RIKTIGHET" THEN LEAVE.
   END.
END PROCEDURE.    
PROCEDURE ftro_UI:
   RUN nyttbladexcel_UI.      
   {EXCELFEL.I}
   chWorkSheet:Name = "Förtroendetid"  NO-ERROR.
   DEBUGGER:SET-BREAK().
   RUN satestat_UI. 
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
      {EXCELFEL.I}      
      IF SUBSTRING(tidut.UT,1,6) = "Förtro" THEN DO:    /*Rubrikslut*/
         LEAVE.
      END.
   END.   
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidut THEN DO:
      LEAVE.
   END.   
   /*Kolumnbredd*/
   raknare = 1.
   RUN kolumnexcel_UI.
   /*Poster*/
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,6) = "======" THEN DO:
         musz = musz.
      END.
      ELSE DO:
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,4,8) = "Intjänad" OR  SUBSTRING(tidut.UT,12,9) = "SUMMERING" THEN LEAVE.      
   END.
END PROCEDURE.    

PROCEDURE kompinut_UI:
   RUN nyttbladexcel_UI.      
   {EXCELFEL.I}
   chWorkSheet:Name = "Intjänad uttagen komp"  NO-ERROR.
   DEBUGGER:SET-BREAK().
   RUN satestat_UI. 
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,4,7) = "Uttagen" THEN DO:    /*Rubrikslut*/
         RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
         LEAVE.
      END.
   END.   
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF NOT AVAILABLE tidut THEN DO:
      LEAVE.
   END.   
   /*Kolumnbredd*/
   raknare = 1.
   RUN kolumnexcel_UI.
   /*Poster*/
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,4,6) = "======" OR  SUBSTRING(tidut.UT,1,6) = "======" THEN DO:
         musz = musz.
      END.
      ELSE DO:
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      
      IF  SUBSTRING(tidut.UT,12,9) = "SUMMERING" OR SUBSTRING(tidut.UT,1,13) = "Förtroendetid"  THEN LEAVE.
   
   END.
END PROCEDURE.    
   
   
