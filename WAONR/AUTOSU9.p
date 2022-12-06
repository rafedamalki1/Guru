/*AUTOsu9.P*/
&Scoped-define NEW NEW
{GLOBVAR2DEL1.I}

DEFINE VARIABLE dbfilename AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque AS CHARACTER NO-UNDO. 
DEFINE VARIABLE progflytt AS CHARACTER NO-UNDO.
DEFINE VARIABLE prognamnque2 AS CHARACTER NO-UNDO.
DEFINE VARIABLE sokfil AS CHARACTER.
{VALDBDEF.I}
{VALDBSUND9.I}

DEFINE TEMP-TABLE felmex   
   FIELD ENR                AS CHARACTER 
   FIELD BENAMNING          AS CHARACTER 
   FIELD ENHET              AS CHARACTER
   FIELD BPRIS              AS DECIMAL
   FIELD NPRIS              AS DECIMAL
   INDEX ENR IS PRIMARY ENR.
/*DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
{EXTRADATA.I}*/
OPEN QUERY vq FOR EACH valdbtemp WHERE valdbtemp.DBNAMN NE "UTBI" NO-LOCK.
GET FIRST vq NO-LOCK.
DO WHILE AVAILABLE(valdbtemp): 
   Guru.Konstanter:globforetag = valdbtemp.FORETAG.
   {AppSprinSet.I}
   IF Guru.Konstanter:globforetag = "sund" THEN DO:   
      
      IF AVAILABLE valdbtemp THEN DO:
         progflytt = SUBSTRING(valdbtemp.DBPLATS,1,INDEX(valdbtemp.DBPLATS,"DB\") - 1).
         prognamnque = progflytt + "autotid.txt". 
         prognamnque2 = progflytt + "autotidkop.txt".
      END.
      ELSE DO:
         prognamnque = "autotid.txt".
         prognamnque2 = "autotidkop.txt".
      END.
   END.
   IF Guru.Konstanter:globforetag = "snat" THEN DO:         
      IF AVAILABLE valdbtemp THEN DO:
         progflytt = SUBSTRING(valdbtemp.DBPLATS,1,INDEX(valdbtemp.DBPLATS,"DB\") - 1).
         prognamnque = progflytt + "autotidSNAT.txt". 
         prognamnque2 = progflytt + "autotidSNATkop.txt".
      END.
      ELSE DO:
         prognamnque = "autotidSNAT.txt".
         prognamnque2 = "autotidSNATkop.txt".
      END.
   END.
   
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT "START AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
   IF DAY(TODAY) = 28 THEN DO:
      OS-COPY VALUE(prognamnque) VALUE(prognamnque2).
      OUTPUT TO  VALUE(prognamnque).
      PUT "TÖMD" TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
      OUTPUT CLOSE.     
   END.
   dbfilename = valdbtemp.DBNAMN.
    
   RUN val_UI.
   
   IF CONNECTED(LDBNAME(1)) THEN DO:       
      RUN ALIASSATT.P.
      /*
      SAVE CACHE COMPLETE VALUE(LDBNAME(1)) TO VALUE(prognamn).
      */
      IF Guru.Konstanter:globforetag = "Xsund" THEN DO: 
         RUN textut_UI (INPUT "AO INSIKT").
         RUN SUNDINSIKTAO.p.
         RUN textut_UI (INPUT "AO INSIKT KLAR").
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
      RUN textut_UI (INPUT "TIDLÄGEN").  
      RUN textut_UI (INPUT "LÖPAND UTAN").  
      RUN FAKTKOLL.P.
      RUN textut_UI (INPUT "LÖPAND UTAN KLAR").
      RUN textut_UI (INPUT "NYA FAKTMEDD").
      /*  snat ska inte ha fler fakt med. 
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
            RUN textut_UI (INPUT "FÄRDIG KOLL").            
            RUN EJFEPOST.P.
            RUN textut_UI (INPUT "FÄRDIGKOLL KLAR").            
         END.
      END.
      IF Guru.Konstanter:globforetag = "SNAT" THEN DO:   
         IF WEEKDAY(TODAY) = 3 THEN DO:
            RUN textut_UI (INPUT "GODKÄND KOLL").            
            RUN EJGEPOST.P.
            RUN textut_UI (INPUT "GODKÄNDKOLL KLAR").            
         END.
      END.
      IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
         IF DAY(TODAY) = 1 THEN DO:
            RUN textut_UI (INPUT "AOAVSLUTKOLL").            
            RUN AOAVSLEPOST.P.
            RUN textut_UI (INPUT "AOAVSLUTKOLL KLAR").                                   
         END.
      END.
      IF Guru.Konstanter:globforetag = "XSUND" THEN DO:
         /*sista tisdagen i månaden 20120126 C Bergman*/    
         IF WEEKDAY(TODAY) = 3 THEN DO:
            IF DAY(TODAY + 7) < DAY(TODAY) THEN DO:
               RUN textut_UI (INPUT "FÄRDIG KOLL").            
               RUN EJFEPOST.P.
               RUN textut_UI (INPUT "FÄRDIGKOLL KLAR").
            END.               
         END.
      END.         
      IF Guru.Konstanter:globforetag = "XSUND" THEN DO:
         /*två dagar innan den sista i månaden 20120126 C Bergman*/   
         IF DAY(TODAY + 3) = 1 THEN DO:
            RUN textut_UI (INPUT "GODKÄND skick").            
            RUN GEPOST.P.
            RUN textut_UI (INPUT "GODKÄND skick KLAR").            
         END.
      END.
      IF Guru.Konstanter:globforetag = "XSUND" THEN DO:   
         IF WEEKDAY(TODAY) = 3 THEN DO:
            RUN textut_UI (INPUT "LARM 35 TIM ÖVERTID MÅNAD").            
            RUN LARM35TMAN.P.
            RUN textut_UI (INPUT "LARM 35 TIM ÖVERTID MÅNAD").            
         END.
        
         
      END.   
      IF Guru.Konstanter:globforetag = "SNAT" THEN DO:      
         IF DAY(TODAY) = 1 THEN DO:
            RUN textut_UI (INPUT "DISPENS ÖVERTID KOLL").            
            RUN DISPOVEPOST.P.
            RUN textut_UI (INPUT "DISPENS ÖVERTID KLAR").                                   
         END.
      END.      
         
      /*IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
         RUN EXTRADATAHMT.P PERSISTENT SET edataapph.      
         
         prognamnque = "D:\delad\klient\pro10\guru\" + "snatahlsell.skv".            
         sokfil = SEARCH(prognamnque).
         IF sokfil = ? THEN sokfil = sokfil.
         ELSE DO:         
            RUN textut_UI (INPUT "MTRL UPP START").            
            EMPTY TEMP-TABLE felmex NO-ERROR. 
            RUN IMPMTRLSKVN.P (INPUT prognamnque, INPUT "12",INPUT true,INPUT 0,OUTPUT TABLE felmex,INPUT false).           
            OS-DELETE VALUE(prognamnque).
             EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "INLKAT"                   
            inextradatatemp.HUVUDCH = "12".                    
                                
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.
            IF NOT AVAILABLE extradatatemp THEN DO:
               CREATE extradatatemp.
               ASSIGN
               extradatatemp.PROGRAM = "INLKAT"                   
               extradatatemp.HUVUDCH = "12"
               extradatatemp.SOKLOG[1] = FALSE.
            END.
            ASSIGN
            extradatatemp.SOKDATE[1] = TODAY 
            extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR.  
            RUN textut_UI (INPUT "MTRL UPP KLART").
         END.            
         prognamnque = "D:\delad\klient\pro10\guru\" + "snatonninen.skv".
         sokfil = SEARCH(prognamnque).
         IF sokfil = ? THEN sokfil = sokfil.
         ELSE DO:         
            RUN textut_UI (INPUT "MTRL UPP START").
            RUN IMPMTRLSKVN.P (INPUT prognamnque, INPUT "1",INPUT true,INPUT 0,OUTPUT TABLE felmex,INPUT true).
            OS-DELETE VALUE(prognamnque).
            
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
            CREATE inextradatatemp.          
            ASSIGN
            inextradatatemp.PROGRAM = "INLKAT"                   
            inextradatatemp.HUVUDCH = "1".                    
                                
            RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
            FIND FIRST extradatatemp NO-LOCK NO-ERROR.
            IF NOT AVAILABLE extradatatemp THEN DO:
               CREATE extradatatemp.
               ASSIGN
               extradatatemp.PROGRAM = "INLKAT"                   
               extradatatemp.HUVUDCH = "1"
               extradatatemp.SOKLOG[1] = FALSE.
            END.
            ASSIGN
            extradatatemp.SOKDATE[1] = TODAY 
            extradatatemp.SOKCHAR[1] = STRING(TIME,"hh:mm") 
            extradatatemp.SOKCHAR[2] =  globanv
            extradatatemp.SOKLOG[1] =  TRUE.
            RUN extraspar_UI IN edataapph (INPUT TABLE extradatatemp). 
            EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
            EMPTY TEMP-TABLE extradatatemp NO-ERROR.  
            RUN textut_UI (INPUT "MTRL UPP KLART").
         END.                
      END.
      EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
      EMPTY TEMP-TABLE extradatatemp NO-ERROR. 
      IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph. 
      edataapph = ?.*/
 
      IF Guru.Konstanter:globforetag = "SNAT" THEN DO:
         RUN textut_UI (INPUT "NAPRBER.P").
         RUN NAPRBER.P.  /* uppdatera pris i beredning om katalog uppdaterad*/
         RUN textut_UI (INPUT "NAPRBER.P KLAR").
      END.             
              
   END.  
    
   {DBBACKAI.I}
   GET NEXT vq NO-LOCK.
END.

OUTPUT TO VALUE(prognamnque) APPEND.
PUT "SLUT AUTOKÖRNING " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
OUTPUT CLOSE.
QUIT.
PROCEDURE val_UI :
   CONNECT VALUE(valdbtemp.DBCON) NO-ERROR.         
END PROCEDURE.

PROCEDURE textut_UI:
   DEFINE INPUT PARAMETER meddvar AS CHARACTER NO-UNDO.
   OUTPUT TO VALUE(prognamnque) APPEND.
   PUT UNFORMATTED meddvar " " Guru.Konstanter:globforetag " " TODAY " " STRING(TIME,"HH:MM:SS") SKIP.
   OUTPUT CLOSE.
END PROCEDURE.
