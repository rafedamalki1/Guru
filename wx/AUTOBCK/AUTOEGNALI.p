/*AUTOEGNALI.p*/

DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
   prognamnque = "C:\DELAD\PRO10S\autotid.txt".
   prognamnque2 = "C:\DELAD\PRO10S\autotidkop.txt".
    

IF DAY(TODAY) = 28 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.



RUN ut_UI("EGNASTART").
 

PROCEDURE ut_UI :
   DEFINE INPUT  PARAMETER instart AS CHARACTER NO-UNDO.
   
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED instart " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
  
   RUN ut_UI("nu ska ELKRAFTBEREDNING starta").
   RUN AUTOELKB.p.
   RUN ut_UI("nu ska skogskonsult starta").
   RUN AUTOSKOGSK.p.
   RUN ut_UI("nu ska picab STARTA").
   
   RUN AUTOPICAB.P.
   RUN ut_UI("nu ska OXEL STARTA").
   RUN AUTOOXEL.P.
   RUN ut_UI("nu ska övik STARTA").
   RUN AUTOOVIK.P.
   RUN ut_UI("nu ska el och skog STARTA").
   RUN AUTOESKOG.P.
   RUN ut_UI("nu ska Oppunda STARTA").
   RUN AUTOOPPU.p.
   RUN ut_UI("nu ska Nätstruktur STARTA").
   RUN AUTONSKOG.p.
   RUN ut_UI("nu ska Tectel STARTA").
   RUN AUTOTECT.P.
   RUN ut_UI("nu ska sekg STARTA").
   RUN AUTOSEKG.P.
   RUN ut_UI("nu ska GREL STARTA").
   RUN AUTOGREL.P.
   
   RUN ut_UI("nu ska ALTE STARTA").
   RUN AUTOALTE.P.
   
   RUN ut_UI("nu ska ALLT VARA KLART").
   
   
   QUIT.
  /* 
  RUN AUTOORBI.p.
   RUN ut_UI("nu ska picab STARTA").
   OUTPUT TO VALUE(prognamnque) APPEND.
   RUN ut_UI("nu ska ALTE STARTA").
   OUTPUT CLOSE.
   RUN AUTOALTE.P.
   */
  /*-----------------------------*/ 
   
   
   
   
   
   
   