/*AUTOWWW2.p*/
  
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


RUN ut_UI("www2START").

PROCEDURE ut_UI :
   DEFINE INPUT  PARAMETER instart AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED instart " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
   RUN ut_UI("nu ska ATS STARTA").
   RUN AUTOATS.p.
   RUN ut_UI("nu ska ETSA STARTA").
   RUN AUTOETS.p.
   RUN ut_UI("nu ska SWECO STARTA").
   RUN AUTOSWECO.p.
   RUN ut_UI("nu ska  STARTA").
   RUN AUTOLIMO.p.
   RUN ut_UI("nu ska JUKKAS STARTA").
   RUN AUTOJSBF.p.
   RUN ut_UI("nu ska SSEL STARTA").
   RUN AUTOSSEL.p.
   RUN ut_UI("nu ska LAKL STARTA").
   RUN AUTOLAKL.p.
   RUN ut_UI("nu ska åfcon STARTA").
   RUN AUTOAFCO.P.
   RUN ut_UI("nu ska ELPC STARTA").
   RUN AUTOELPC.P.
   RUN ut_UI("nu ska KEWAB STARTA").
   RUN AUTOKEWA.P.
   RUN ut_UI("nu ska HJEL STARTA").
   RUN AUTOHJORT.p.
   RUN ut_UI("nu ska POLA STARTA").
   RUN AUTOPOLA.p.
   RUN ut_UI("nu ska KRAF STARTA").
   RUN AUTOKRAF.p.
   
   RUN ut_UI("nu ska SKEK STARTA").
   RUN AUTOSKEK.P.
  
   
   RUN ut_UI("nu ska MOLN STARTA").
   RUN AUTOMOLN.p.
   RUN ut_UI("nu ska TOSE STARTA").
   RUN AUTOTOSE.p.
   RUN ut_UI("nu ska KRIN STARTA").
   RUN AUTOKRIN.p.
   RUN ut_UI("nu ska WIGA STARTA").
   RUN AUTOWIGA.p.
   RUN ut_UI("nu ska onewww STARTA").
   RUN AUTOONEWWW.p.
   /* körs separast kl 00.15
   RUN ut_UI("RENSAR GURU ZIP"). 
   RUN Rensawww2.p.
   RUN ut_UI("nu ska ALLT VARA KLART").
*/
QUIT.
 /*
    /*
    RUN ut_UI("nu ska RAMB STARTA").
   RUN AUTORAMB.p.
   RUN ut_UI("nu ska NAEK STARTA").
   RUN AUTONAEK.p.
   */
    RUN ut_UI("nu ska YSEN STARTA").
   RUN AUTOYSEN.p.
   /*
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT "nu ska KNOR STARTA" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   RUN AUTOKNOR.p.
   */
   /*
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT "nu ska KTEAM STARTA" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   RUN AUTOKTEAM.p.
   */
   
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT "nu ska POMA STARTA" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   RUN AUTOPOMA.p.
   
   IF TODAY < 11/30/2014 THEN DO: 
      prognamnque = "C:\DELAD\PRO10S\autotid.txt".
      OUTPUT TO VALUE(prognamnque) APPEND.
      PUT "nu ska HANA STARTA" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      RUN AUTOHANA.p. 
   END.    
   IF TODAY < 06/02/2013 THEN DO: 
      OUTPUT TO VALUE(prognamnque) APPEND.
      PUT "nu ska PPKO STARTA" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      RUN AUTOPPKO.P.
   END.   
   IF TODAY < 12/02/2012 THEN DO:
      OUTPUT TO VALUE(prognamnque) APPEND.
      PUT "nu ska NYLB STARTA" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.
      RUN AUTONYLB.P.
   END.
   */
