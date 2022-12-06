/*AUTOSU9BERGET.p*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}
DEFINE INPUT  PARAMETER gfin AS CHARACTER NO-UNDO.

DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER.

DEFINE TEMP-TABLE felmex   
   FIELD ENR                AS CHARACTER 
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER
   FIELD BPRIS              AS DECIMAL
   FIELD NPRIS              AS DECIMAL
   INDEX ENR IS PRIMARY ENR.
Guru.Konstanter:globforetag = gfin.
/*SNATBERGET*/
progflytt = "D:\DELAD\PRO10S\".
prognamnque = progflytt + "autotidSNAT.txt". 
prognamnque2 = progflytt + "autotidSNATkop.txt".
   
OUTPUT TO VALUE(prognamnque) APPEND.
PUT "START AUTOK�RNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
IF DAY(TODAY) = 28 THEN DO:
   OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
   OUTPUT TO  VALUE(prognamnque).
   PUT "T�MD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.     
END.
RUN AUTOFLEX.P.
RUN textut_UI (INPUT "flex KLAR").
/*FAKTFOR*/
RUN textut_UI (INPUT "NYA PRISLISTOR").  
RUN BESTTABPRIS.P.
RUN textut_UI (INPUT "NYA PRISLISTOR KLAR").  
RUN AUTOST.P. 
RUN textut_UI (INPUT "ALLATIDERKLAR").  
RUN FORSTTID.P.
RUN textut_UI (INPUT "TIDL�GEN").  
RUN textut_UI (INPUT "L�PAND UTAN").  
RUN FAKTKOLL.P.
RUN textut_UI (INPUT "L�PAND UTAN KLAR").
RUN textut_UI (INPUT "NYA FAKTMEDD"). 
/*Anders Olsson Elpool i Ume� AB  1 okt 2018 13:52:20 
snat ska inte ha fler fakt med. 
*/
/* 
RUN FAKMEDF.P.
*/
RUN textut_UI (INPUT "NYA FAKTMEDD KLAR").
IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
   IF WEEKDAY(TODAY) = 6 THEN DO:
      RUN textut_UI (INPUT "FLEXKOLL").            
      RUN FLAVVAPPNA.P.
      RUN textut_UI (INPUT "FLEXKOLL KLAR").            
   END.
END.      
IF Guru.Konstanter:globforetag = "SNAT" THEN DO:  
   IF WEEKDAY(TODAY) = 2 THEN DO:
      RUN textut_UI (INPUT "F�RDIG KOLL").            
      RUN EJFEPOST.P.
      RUN textut_UI (INPUT "F�RDIGKOLL KLAR").
      
      IF TODAY GE 10/01/2020 THEN DO:
         RUN textut_UI (INPUT "�VERTIDSBEORDRARE").                 
         RUN OBEORDLISTEJGOD.P.
         RUN textut_UI (INPUT "�VERTIDSBEORDRARE KLAR"). 
      END.            
   END.
END.
IF Guru.Konstanter:globforetag = "SNAT" THEN DO:   
   IF WEEKDAY(TODAY) = 3  THEN DO:
      RUN textut_UI (INPUT "GODK�ND KOLL").            
      RUN EJGEPOST.P.
      RUN textut_UI (INPUT "GODK�NDKOLL KLAR").            
   END.
END.
IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
   IF DAY(TODAY) = 1 THEN DO:
      RUN textut_UI (INPUT "AOAVSLUTKOLL").            
      RUN AOAVSLEPOST.P.
      RUN textut_UI (INPUT "AOAVSLUTKOLL KLAR").                                   
   END.
END.
IF DAY(TODAY) = 1 THEN DO:
   RUN textut_UI (INPUT "DISPENS �VERTID KOLL").            
   RUN DISPOVEPOST.P.
   RUN textut_UI (INPUT "DISPENS �VERTID KLAR").                                   
END.   

IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
   IF WEEKDAY(TODAY) = 4 THEN DO:
      RUN textut_UI (INPUT "BESTPUNKT DEP 9 START").
      RUN BESTPUNKTSNAT.P.
      RUN textut_UI (INPUT "BESTPUNKT DEP 9 KLAR").
   END.   
END.

IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
   RUN textut_UI (INPUT "NAPRBER.P").
   RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
   RUN textut_UI (INPUT "NAPRBER.P KLAR").
END.             

OUTPUT TO VALUE(prognamnque) APPEND.
PUT "SLUT AUTOK�RNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
