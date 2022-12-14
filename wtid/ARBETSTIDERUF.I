/*ARBETSTIDERUF.I*/
{KLOCKBER.I}
DEFINE BUFFER atab FOR ARBETSTIDTAB.
/*special MISV uppf?ljning arbetstider helt ?r heltid f?r vinterschema 5 och sommarschema 15*/
FIND FIRST ANSTFORMTAB WHERE ANSTFORMTAB.ANSTALLNING = PERSONALTAB.ANSTALLNING
USE-INDEX ANSTF NO-LOCK NO-ERROR.   
IF YEAR(regdatum) < 2001 THEN DO:
   arregvnr = regvnr.
END.
ELSE DO:
   IF INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) = 9 THEN DO:
      IF INTEGER(SUBSTRING(STRING(regvnr,"999"),2,2)) >= 52 THEN DO:
         IF INTEGER(SUBSTRING(STRING(YEAR(regdatum),"9999"),4,1)) NE INTEGER(SUBSTRING(STRING(regvnr,"999"),1,1)) THEN DO:
            arregvnr = INTEGER(SUBSTRING(STRING(YEAR(regdatum) - 1,"9999"),1,3) + STRING(regvnr,"999")).
         END.
         ELSE arregvnr = INTEGER(SUBSTRING(STRING(YEAR(regdatum),"9999"),1,3) + STRING(regvnr,"999")).
      END.
      ELSE arregvnr = INTEGER(SUBSTRING(STRING(YEAR(regdatum),"9999"),1,3) + STRING(regvnr,"999")).
   END.
   ELSE arregvnr = INTEGER(SUBSTRING(STRING(YEAR(regdatum),"9999"),1,3) + STRING(regvnr,"999")).
END.
   


IF Guru.Konstanter:globforetag = "misv" THEN DO:
   vschemat = 15.         
   FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
   USE-INDEX ORDARB NO-LOCK NO-ERROR.
   IF ORDARB.OBKOD NE "" THEN DO:
      FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
      FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD NO-LOCK NO-ERROR.            
      IF NOT AVAILABLE FLEXREG THEN DO:
         FIND FIRST FLEXREG WHERE FLEXREG.KOD = "" NO-LOCK NO-ERROR.
      END.      
      IF AVAILABLE FLEXREG THEN DO:
         IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:               
            vschemat = 15.         
         END.
         ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
            vschemat = 15.         
         END.
         ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
            vschemat = 15.         
         END.      
         ELSE vschemat = 5.               
      END.
      ELSE vschemat = 5.      
   END. 
END.
ELSE DO:
   FIND FIRST VECKOARBAV WHERE VECKOARBAV.PERSONALKOD = PERSONALTAB.PERSONALKOD AND
   VECKOARBAV.VECKONUMMER = arregvnr USE-INDEX PVNUMMER NO-LOCK NO-ERROR.
   IF NOT AVAILABLE VECKOARBAV THEN DO TRANSACTION:       
      {RULLVECKO.I}     
      /*CREATE VECKOARBAV.
      ASSIGN VECKOARBAV.PERSONALKOD = PERSONALTAB.PERSONALKOD
      VECKOARBAV.VECKONUMMER = arregvnr
      VECKOARBAV.VECKOSCHEMA = rull-veckovar.*/
      vschemat = rull-veckovar.
   END.
   ELSE DO:
      vschemat = VECKOARBAV.VECKOSCHEMA.
   END. 
END.   


FIND FIRST flexreg WHERE NO-LOCK NO-ERROR.
{AVIKDAGTID.I}
/* Omf?rhandlades 2006 . Nu har Nybro ALDRIG r?da dagar
IF Guru.Konstanter:globforetag = "GKAL" THEN DO:
   /*Inget av Nybros skiftscheman har r?da dagar , det har bara de som jobbar 7-16 veckoschema 8*/   
   IF ANSTFORMTAB.KOD = "K3" OR ANSTFORMTAB.KOD = "T3"  THEN DO:
      IF AVAILABLE OVERAVTAB THEN DO:
         IF dagnr = 1 OR dagnr = 7 THEN DO:
            IF WEEKDAY(regdatum) NE dagnr THEN DO:
               IF vschemat = 8 THEN.
               ELSE IF vschemat = 1 THEN.
               /*ELSE IF vschemat = 37 THEN.*/
               ELSE dagnr = WEEKDAY(regdatum).
            END.
         END.
      END.
   END.   
END.                 */
IF dagnr = 2 THEN DO:
   FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = VECKOARBETID.ARBTIDMAN
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.   

END.
ELSE IF dagnr = 3 THEN DO:
   FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = VECKOARBETID.ARBTIDTIS
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.   
END.
ELSE IF dagnr = 4 THEN DO:
   FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = VECKOARBETID.ARBTIDONS
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.   
END.
ELSE IF dagnr = 5 THEN DO:
   FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = VECKOARBETID.ARBTIDTOR
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.  
END.
ELSE IF dagnr = 6 THEN DO:
   FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = VECKOARBETID.ARBTIDFRE
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.   
END.
ELSE IF dagnr = 7 THEN DO:
   FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = VECKOARBETID.ARBTIDLOR
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.   
END.
ELSE IF dagnr = 1 THEN DO:
   FIND FIRST ARBETSTIDTAB WHERE ARBETSTIDTAB.ARBTIDKOD = VECKOARBETID.ARBTIDSON
   USE-INDEX ARBTIDKOD NO-LOCK NO-ERROR.   
END.
IF AVAILABLE ARBETSTIDTAB THEN DO:
   ASSIGN
   regstart = ARBETSTIDTAB.START
   regslut = ARBETSTIDTAB.SLUT
   lunchstarten = ARBETSTIDTAB.LUNCHSTART
   lunchslutet = ARBETSTIDTAB.LUNCHSLUT
   frustarten = ARBETSTIDTAB.FRUKOSTSTART
   fruslutet = ARBETSTIDTAB.FRUKOSTSLUT
   kaffestart = ARBETSTIDTAB.KAFESTART
   kaffeslut = ARBETSTIDTAB.KAFESLUT
   regtotalt = ARBETSTIDTAB.TOTDAGTID.   
END.

IF AVAILABLE OVERAVTAB AND OVERAVTAB.DAGEQ = "HAL"  THEN DO:
   IF regstart = regslut THEN regstart = regstart.
   ELSE DO:
      IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV"  OR Guru.Konstanter:globforetag = "CELPA" THEN DO:     
         IF PERSONALTAB.DELTID = TRUE AND regtotalt < 7.45 THEN DO:            
            FIND FIRST ORDARB WHERE ORDARB.ANSTALLNING = PERSONALTAB.ANSTALLNING 
            USE-INDEX ORDARB NO-LOCK NO-ERROR.
            IF ORDARB.OBKOD NE "" THEN DO:
               FIND FIRST FLEXAVT WHERE FLEXAVT.PERSONALKOD = PERSONALTAB.PERSONALKOD NO-LOCK NO-ERROR.
               FIND FIRST FLEXREG WHERE FLEXREG.KOD = FLEXAVT.FLEXKOD NO-LOCK NO-ERROR.            
               IF NOT AVAILABLE FLEXREG THEN DO:
                  FIND FIRST FLEXREG WHERE FLEXREG.KOD = "" NO-LOCK NO-ERROR.
               END.      
               IF AVAILABLE FLEXREG THEN DO:
                  IF MONTH(regdatum) > MONTH(FLEXREG.SOMMARST) AND MONTH(regdatum) < MONTH(FLEXREG.SOMMARSL) THEN DO:               
                     regtotalt = klockan60(klockan100(regtotalt) * 5 / 7.75).   
                  END.
                  ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARSL) AND DAY(regdatum) <= DAY(FLEXREG.SOMMARSL) THEN DO:
                     regtotalt = klockan60(klockan100(regtotalt) * 5 / 7.75).      
                  END.
                  ELSE IF MONTH(regdatum) = MONTH(FLEXREG.SOMMARST) AND DAY(regdatum) >= DAY(FLEXREG.SOMMARST) THEN DO: 
                     regtotalt = klockan60(klockan100(regtotalt) * 5 / 7.75).   
                  END.
                  ELSE regtotalt = klockan60(klockan100(regtotalt) * 5 / 8.25).
               END.
               ELSE regtotalt = klockan60(klockan100(regtotalt) * 5 / 8.25).
            END. 
            ELSE regtotalt = klockan60(klockan100(regtotalt) * 5 / 8.25).
            regslut = klockan60(klockan100(regstart) +  klockan100(regtotalt)) .
         END.
         ELSE IF regstart = 7 THEN DO:
            ASSIGN regslut = 12 regtotalt = 5.
         END.   
         ELSE IF regstart = 8 THEN DO:
            ASSIGN regslut = 13 regtotalt = 5.
         END.   
         ELSE IF regstart = 7.3 THEN DO:
            ASSIGN regslut = 12.3 regtotalt = 5.
         END.            
         ASSIGN
         lunchstarten = lunchslutet
         frustarten = fruslutet 
         kaffestart =  kaffeslut.
      END.                
      IF Guru.Konstanter:globforetag = "GRAN" THEN DO: 
         IF regstart = 14  THEN DO: 
            ASSIGN regslut = 18.00
            regtotalt = 4  
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.
         ELSE IF regstart = 14.30  THEN DO: 
            ASSIGN 
            regstart = 11.30
            regslut = 19.30
            regtotalt = 8  
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.
         ELSE IF ANSTFORMTAB.KOD = "T" THEN DO:  /* 7.30-16.15*/
            ASSIGN regslut = 12.00
            regtotalt = 4.30  
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END. 
         ELSE IF ANSTFORMTAB.KOD = "T2" THEN DO:        /*7.15-16*/
            ASSIGN regslut = 12.00
            regtotalt = 4.45  
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END. 
         ELSE IF ANSTFORMTAB.KOD BEGINS "K" THEN DO:     /*7-16*/
            ASSIGN regslut = 11.00
            regtotalt = 4           
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.
         ELSE IF ANSTFORMTAB.KOD = "T1" THEN DO:   /*7-16 ledarna 1 tim lunch kaffe*/
            ASSIGN regslut = 13.00
            regtotalt = 5           
            kaffestart =  kaffeslut.
         END.
         ELSE IF ANSTFORMTAB.KOD = "TS" THEN DO:  /*7-16 sif inga raster*/
            ASSIGN regslut = 12.00
            regtotalt =  5        
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.
         ELSE DO: 
            IF regstart = 7.00  THEN lunchslutet = lunchstarten + 1.
            ELSE lunchstarten = lunchslutet.                    
            ASSIGN
            regslut = 13.00
            regtotalt = 5.00     /* ?NDRAT EFTER F?RHANDLING*/         
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.               
      END.   
      IF Guru.Konstanter:globforetag = "GKRVA" THEN DO:       
         IF ANSTFORMTAB.KOD = "K" THEN DO:     /*7-16*/
            ASSIGN regslut = 11.00
            regtotalt = 4           
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.
         ELSE IF ANSTFORMTAB.KOD = "K2" THEN DO:     /*7-16*/
            ASSIGN regslut = 13.00
            regtotalt = 5                    
            kaffestart =  kaffeslut.
         END.
         ELSE IF ANSTFORMTAB.KOD = "T" THEN DO:  /* 7.45-16.3 sif*/
            ASSIGN regslut = 12.00
            regtotalt = 4.15  
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.       
         ELSE IF ANSTFORMTAB.KOD = "T3"  THEN DO:  /* 7-16 sif NORD*/
            ASSIGN regslut = 12.00
            regtotalt = 5  
            lunchstarten = lunchslutet
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.  
         ELSE IF ANSTFORMTAB.KOD = "T4" OR ANSTFORMTAB.KOD = "T5" THEN DO:   /*7-16 sif ledarna*/
            ASSIGN regslut = 13.00
            regtotalt = 5           
            kaffestart =  kaffeslut.
         END.      
         ELSE DO: 
            IF regstart = 7.00  THEN lunchslutet = lunchstarten + 1.
            ELSE lunchstarten = lunchslutet.                    
            ASSIGN
            regslut = 13.00
            regtotalt = 5.00     /* ?NDRAT EFTER F?RHANDLING*/         
            frustarten = fruslutet
            kaffestart =  kaffeslut.
         END.               
      END.   
      IF Guru.Konstanter:globforetag = "GADM"  THEN DO:  
         IF ANSTFORMTAB.KOD = "T" OR ANSTFORMTAB.KOD = "T3"   THEN DO:
            ASSIGN regslut = 12.00.
            IF regstart = 7.45 THEN regtotalt = 4.15.  
            ELSE regtotalt = 4.30.  
         END.  
         IF ANSTFORMTAB.KOD = "T2" OR ANSTFORMTAB.KOD = "T1"  THEN DO:
            ASSIGN regslut = 13.00
            regtotalt = 5.00.  
         END.                  
         IF regstart = 9 AND regslut = 13.00 THEN DO:
           regtotalt = 4.00.
         END.   
         ASSIGN
         lunchstarten = lunchslutet
         frustarten = fruslutet
         kaffestart =  kaffeslut.
      END.       
      IF Guru.Konstanter:globforetag = "LULE" OR Guru.Konstanter:globforetag = "ELPA" THEN DO:   
         IF ANSTFORMTAB.KOD = "T3" OR ANSTFORMTAB.KOD = "T4" OR ANSTFORMTAB.KOD = "T5"  THEN DO:
            /*Bioenergis personal jobbar 7-16 halvad 7-11*/
            ASSIGN regslut = 11
            regtotalt = 4.00.                  
         END.
         ELSE IF regstart = 7.3 AND regslut = 11.00 THEN DO:  
            /*Sven ? byter bara dagschema p? dessa*/
            ASSIGN regtotalt = 3.30.                  
         END.
         ELSE DO:         
            ASSIGN regslut = 11.30
            regtotalt = 4.00.                  
         END.
         ASSIGN
         lunchstarten = lunchslutet
         frustarten = fruslutet
         kaffestart =  kaffeslut.
      END.       
   END.
END.   
IF AVAILABLE OVERAVTAB AND OVERAVTAB.DAGEQ = "VAL"  THEN DO:
   /*3/4 dag valborg */
   IF Guru.Konstanter:globforetag = "LULE"  THEN DO:   
      IF regstart = 7.3 AND regslut = 16.3 THEN DO:
         ASSIGN regslut = 14.3.
         IF regtotalt = 8.05 THEN regtotalt = 6.05.
         IF regtotalt = 8.25 THEN regtotalt = 6.25.
      END.   
      IF regstart = 7.3 AND regslut = 16.15 THEN DO:
         ASSIGN regslut = 14.3.
         regtotalt = 6.15.
      END.   
   END.
END.
