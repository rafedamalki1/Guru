/*AKIVINEX.P*/
{TIDUTTTNEW.I}
DEFINE VARIABLE stdat AS DATE NO-UNDO.
DEFINE VARIABLE sldat AS DATE NO-UNDO.  
DEFINE VARIABLE slrader AS LOGICAL NO-UNDO.
DEFINE VARIABLE fetunder AS LOGICAL NO-UNDO.
DEFINE VARIABLE fetstart AS LOGICAL NO-UNDO.
DEFINE VARIABLE ftrorader AS LOGICAL NO-UNDO.
DEFINE INPUT PARAMETER excellista AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR tidut.
DEFINE BUFFER tidutbuff FOR tidut.
DEFINE NEW SHARED VARIABLE musz AS LOGICAL NO-UNDO.
{GLOBVAR2DEL1.I}
{EXECLIN.I}
IF excellista = 1 THEN DO:     
   RUN dir_UI.
END.
IF excellista = 2 THEN DO:  
   RUN kalkao_UI.
END.
IF excellista = 3 THEN DO:  
   RUN aktiv_UI.
END.
IF excellista = 4 THEN DO:
   RUN aokost_UI.
END.
IF excellista = 5 THEN DO:
   RUN mobe_UI.
END.
IF excellista = 51 THEN DO:
   RUN mobesund_UI.
END.
IF excellista = 6 THEN DO:
   RUN proje_UI.
END.
IF excellista = 7 THEN DO:
   RUN personal_UI.
END.
IF excellista = 8 THEN DO:
   RUN kontrolltid_UI.
END.
IF excellista = 9 THEN DO:
   RUN listtid_UI.
END.
IF excellista = 10 THEN DO:
   RUN kontrolltillagg_UI.
END.
IF excellista = 11 THEN DO:
   RUN personalarb_UI.
END.
IF excellista = 12 THEN DO:
   RUN personalovber_UI.
END.
IF excellista = 13 THEN DO:
   RUN personalkompetens_UI.
END.
IF excellista = 14 THEN DO:
   RUN prognos_UI.
END.

IF excellista = 15 THEN DO:
   RUN personalarbdeb_UI.
END.
IF excellista = 16 THEN DO:
   RUN gjbest_UI.
END.
IF excellista = 17 THEN DO:
   RUN gjbestmtrl_UI.
END.
IF excellista = 18 THEN DO:
   RUN obsjuk_UI.
END.
IF excellista = 19 THEN DO:
   RUN miproje_UI.
END.
IF excellista = 20 THEN DO:
   RUN personalarb2_UI.
END.
IF excellista = 21 THEN DO:
   RUN kontrollovertid_UI.
END.
IF excellista = 22 THEN DO:
   RUN kontrollfriskv_UI.
END.
IF excellista = 23 THEN DO:
   RUN personalfranvaro_UI.
END.
IF excellista = 24 THEN DO:
   RUN personalovsu_UI.
END.
IF excellista = 25 THEN DO:
   RUN kontrollflex_UI.
END.
IF excellista = 26 THEN DO:
   RUN nymobesund_UI.
END.
IF excellista = 27 THEN DO:
   RUN fortroendetid_UI.
END.
IF excellista = 28 THEN DO:
   RUN tidsedel_UI.
END.
IF excellista = 30 THEN DO:
   RUN mobelule_UI.
END.
IF excellista = 31 THEN DO:
   RUN tidlagelule_UI.
END.
IF excellista = 32 THEN DO:
   RUN kontrollberover_UI.
END.
IF excellista = 33 THEN DO:
   RUN kompinut_UI.
   
END.
IF excellista = 34 THEN DO:
   RUN kontrollejnoll_UI.
END.
IF excellista = 35 THEN DO:
   RUN otbeord_UI.   
END.
IF excellista = 36 THEN DO:
   RUN glomdflex_UI.   
END.
IF excellista = 37 THEN DO:
   RUN kontrolldepa_UI.
END.
IF excellista = 38 THEN DO:
   RUN kontrollkompsaldo_UI.
END.
IF excellista = 39 THEN DO:
   
   RUN personalarbsam_UI.
END.
RUN slutexcel_UI. 
PROCEDURE aktiv_UI:
   /*Vilka kolumner*/
   ASSIGN
   slutbredd = 5
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 30
   utnr[4] = 39
   utnr[5] = 48
   utnr[6] = 57
   utnr[7] = 66
   utnr[8] = 75
   utnr[9] = 84
   utnr[10] = 93
   utnr[11] = 102
   utnr[12] = 111
   utnr[13] = 120
   utnr[14] = 126.
   RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,utnr[11],4) = "9340" THEN DO:    /*Rubrikslut*/
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
         RUN understryk_UI (INPUT 4,INPUT 2).       
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
PROCEDURE dir_UI:
   /*Vilka kolumner*/
   ASSIGN
   slutbredd = 11
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 30
   utnr[4] = 39
   utnr[5] = 48
   utnr[6] = 64
   utnr[7] = 75
   utnr[8] = 85
   utnr[9] = 95
   utnr[10] = 105
   utnr[11] = 116
   helaben = "B"
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE.
   RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,utnr[3],8) = "TIDSKRI." THEN DO:  /*Rubrikslut*/
         radanmrak = iRad + 2.
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
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         IF utnr[raknare] = 30 OR utnr[raknare] = 39 THEN  DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT TRUE,INPUT FALSE,INPUT 0,INPUT 0).        
         END.
         ELSE DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).         
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.
END PROCEDURE.
PROCEDURE kalkao_UI:
   /*Vilka kolumner*/
   ASSIGN
   slutbredd = 10
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 30
   utnr[4] = 40
   utnr[5] = 50
   utnr[6] = 60
   utnr[7] = 70
   utnr[8] = 81     
   utnr[9] = 92   
   utnr[10] = 103
   helaben = "B". /*LÄGGER UT HELA BENÄMNINGEN I KOLUMN B BENÄMNINGEN FINNS I POS 150*/
   RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,utnr[5],7) = "PROGNOS" THEN DO:    /*Rubrikslut*/
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
         RUN understryk_UI (INPUT 4,INPUT 2).       
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
PROCEDURE aokost_UI:
   /*Vilka kolumner*/
   ASSIGN
   slutbredd = 9
   utnr[1] = 1
   utnr[2] = 12
   utnr[3] = 15
   utnr[4] = 22
   utnr[5] = 30
   utnr[6] = 37
   utnr[7] = 45
   utnr[8] = 52
   utnr[9] = 60
   utnr[10] = 68
   utnr[11] = 76
   utnr[12] = 85
   utnr[13] = 93
   utnr[14] = 102.
   RUN satestat_UI. /*BARA FÖR DE SOM HAR UTNR ANARS COLBREDD_ui*/
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,utnr[3],4) = "ARB." THEN DO:    /*Rubrikslut*/
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
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING" THEN DO:         
         ASSIGN
         helaben = ""
         iRad = 1
         estartnr = 0
         bredd = 0.
         ASSIGN
         slutbredd = 11
         bredd[1] = 10
         bredd[2] = 5
         bredd[3] = 15
         bredd[4] = 8
         bredd[5] = 42
         bredd[6] = 11.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            ASSIGN
            bredd[1] = 9
            bredd[2] = 6.
         END.
         allachar[4] = TRUE.
         RUN exelkost_UI.
         LEAVE.       
      END.
   END.
END PROCEDURE.
PROCEDURE exelkost_UI:   
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("KOSTNADSREGISTRERING")).
   /*Vilka kolumner*/     
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).      
   /*Poster*/
   FIND NEXT tidut NO-LOCK NO-ERROR.
   raknare = 1.
    REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).                     
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
   END.
END PROCEDURE.
PROCEDURE exeltidd_UI:      
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("TID-DETALJERAT")).
   /*Vilka kolumner*/        
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   DEBUGGER:SET-BREAK().
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[9],7) = "EKO-LÖN" THEN DO:    /*Rubrikslut FÖRSTA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
         LEAVE.
      END.
   END.
   FIND NEXT tidut NO-LOCK NO-ERROR.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.
      ELSE IF SUBSTRING(tidut.UT,1,5) = "-----" THEN DO:
         rubrikvar = TRUE.
         RUN understryk_UI (INPUT 4,INPUT 2). 
         musz = musz.
      END.
      ELSE IF SUBSTRING(tidut.UT,estartnr[1],9) = "SIGN NAMN" THEN DO:    /*Rubrikslut ANDRA */         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).
      END.
      ELSE DO:
         IF rubrikvar = TRUE THEN do:
            RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0).
         END.
         ELSE DO:  
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).        
         END.
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING" THEN LEAVE.
   END.
   
END PROCEDURE.

PROCEDURE flexrapp_UI:      
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("Flexrapport")).
   /*Vilka kolumner*/     
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   
   REPEAT:
      IF tidut.UT BEGINS  "Ingående" THEN DO:
         RUN fontexcel_UI (INPUT "Calibri",INPUT 12,INPUT true,INPUT 12,INPUT 11).
         cRange = "A" + string(irad).
         chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[1],20) NO-ERROR.
         cRange = "G" + string(irad).
         chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[7],7) NO-ERROR.               
      END.
      ELSE RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.               
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],6) = " Datum" THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
     
      
   /*Poster*/ 
   raknare = 1.
   slrader = FALSE .
   frapp:
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" AND slrader = FALSE THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 4).       
      END.
      ELSE IF SUBSTRING(tidut.UT,1,5) = "=====" AND slrader = true THEN DO:
         
      END.
      ELSE DO:
         IF tidut.UT BEGINS  "Periodens"  THEN slrader = TRUE.
         IF slrader = TRUE THEN DO:            
            RUN fontexcel_UI (INPUT "Calibri",INPUT 12,INPUT true,INPUT 12,INPUT 11).
            IF tidut.UT BEGINS  "Periodens" OR tidut.UT BEGINS  "Totalt" THEN DO:              
               IF SUBSTRING(tidut.UT,estartnr[1],20) = "Periodens flexsaldo:" THEN DO:
                  cRange = "A" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[1],20) NO-ERROR.
                  cRange = "G" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[7],7) NO-ERROR.               
               END.
               IF  SUBSTRING(tidut.UT,estartnr[1],20) = "Totalt flexsaldo:   "  THEN DO:
                  cRange = "A" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[1],20) NO-ERROR.
                  cRange = "G" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[7],7) NO-ERROR.               
               END.
            END.   
            ELSE RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
         END.    
         ELSE DO:         
            IF SUBSTRING(tidut.UT,estartnr[1],6) = " Datum" THEN RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).
            ELSE IF SUBSTRING(tidut.UT,estartnr[11],3) = " Ut" THEN RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).               
            ELSE  RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT false,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).
         END.                                 
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      DEBUGGER:SET-BREAK().
      IF AVAILABLE tidut THEN DO:
         IF tidut.UT BEGINS "Personalliggare" OR tidut.UT = "Gjorda rättningar" THEN LEAVE frapp.
      END.            
   END.   
END PROCEDURE.

PROCEDURE flexrappLUKA_UI:      
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("Månadens flexrapport")).
   /*Vilka kolumner*/     
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   
   REPEAT:
      
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.
         RUN rubrikerexcel_UI (INPUT "",INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).              
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],3) = "Dag" THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.      
   /*Poster*/ 
   raknare = 1.
   slrader = FALSE .
   
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" AND slrader = FALSE THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 4).       
      END.
      ELSE IF SUBSTRING(tidut.UT,1,5) = "=====" AND slrader = true THEN DO:       
      END.
      ELSE DO:
         IF tidut.UT BEGINS  "Månadens"  THEN slrader = TRUE.
         IF slrader = TRUE THEN DO:            
            RUN fontexcel_UI (INPUT "Calibri",INPUT 12,INPUT true,INPUT 12,INPUT 11).
            IF tidut.UT BEGINS  "Månadens" or tidut.UT BEGINS  "Flexsaldo" OR tidut.UT BEGINS  "Totalt" THEN DO:
               IF SUBSTRING(tidut.UT,estartnr[1],15) = "Månadens saldo:" THEN DO:
                  cRange = "A" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[1],15) NO-ERROR.
                  cRange = "F" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[6],12) NO-ERROR.               
               END.              
               IF SUBSTRING(tidut.UT,estartnr[1],26) = "Totalt flexsaldo körd tid:" THEN DO:
                  cRange = "A" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[1],26) NO-ERROR.
                  cRange = "F" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[6],12) NO-ERROR.               
               END.
               IF  SUBSTRING(tidut.UT,estartnr[1],46) = "Flexsaldo EJ körd tid: (uppdaterad igår kväll)"  THEN DO:
                  cRange = "A" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[1],46) NO-ERROR.
                  cRange = "F" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[6],12) NO-ERROR.               
               END.
               IF  SUBSTRING(tidut.UT,estartnr[1],20) = "Totalt flexsaldo:   "  THEN DO:
                  cRange = "A" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[1],20) NO-ERROR.
                  cRange = "F" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,estartnr[6],12) NO-ERROR.               
               END.
               
            END.   
            ELSE IF SUBSTRING(tidut.UT,46,7) = " ------" THEN.
            ELSE  RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
         END.    
         ELSE DO:         
            IF SUBSTRING(tidut.UT,estartnr[1],6) = "Dag" THEN RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).               
            ELSE  RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT false,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).
         END.                                 
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF AVAILABLE tidut THEN DO:
         IF tidut.UT = "Bilaga med kommentarer / resmål" OR tidut.UT = "Gjorda rättningar" THEN LEAVE.
      END.      
   END.
END PROCEDURE.


PROCEDURE bilagakomm_UI:   
   
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("Kommentarer resmål")).
   /*Vilka kolumner*/     
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   
   RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
   
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:        
   END.
   RUN rubrikerexcel_UI (INPUT "",INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
   FIND NEXT tidut NO-LOCK NO-ERROR.
   
   /*Poster*/ 
   raknare = 1.
   fetunder = FALSE.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         IF fetunder = TRUE THEN DO:
            fetunder = FALSE.
            RUN understryk_UI (INPUT 4,INPUT 4).
         END.     
         ELSE  RUN understryk_UI (INPUT 4,INPUT 2).        
      END.
      ELSE DO:
         IF SUBSTRING(tidut.UT,estartnr[1],3) = "Dag" THEN DO:
             RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).
             fetunder = TRUE.
         END.    
         ELSE RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT FALSE ,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).                     
        
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF AVAILABLE tidut THEN DO:
         IF tidut.UT = "Månadens flex" OR tidut.UT = "Flexrapport ej körda registreringar" OR tidut.UT = "Gjorda rättningar" OR  tidut.UT BEGINS  "Förtroendetid tom" OR 
         tidut.UT BEGINS  "Personalliggare" THEN LEAVE.
      END.         
   END.
END PROCEDURE.

PROCEDURE bilagaPL_UI:   
   
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("Personalliggare")).
   /*Vilka kolumner*/     
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   
   RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
   
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:        
   END.
   RUN rubrikerexcel_UI (INPUT "",INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
   FIND NEXT tidut NO-LOCK NO-ERROR.   
   /*Poster*/ 
   raknare = 1.
   fetunder = FALSE.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         IF fetunder = TRUE THEN DO:
            fetunder = FALSE.
            RUN understryk_UI (INPUT 4,INPUT 4).
         END.     
         ELSE  RUN understryk_UI (INPUT 4,INPUT 2).        
      END.
      ELSE DO:
         IF SUBSTRING(tidut.UT,estartnr[1],7) = "Loggtid" THEN DO:
             RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).
             fetunder = TRUE.
         END.    
         ELSE RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT FALSE ,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).                     
        
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
               
   END.
END PROCEDURE.
PROCEDURE rattningar_UI:   
   
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("Gjorda rättningar")).
   /*Vilka kolumner*/     
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   
   RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
   
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF SUBSTRING(tidut.UT,1,3) = "===" THEN DO:
        
   END.
   RUN rubrikerexcel_UI (INPUT "",INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
   FIND NEXT tidut NO-LOCK NO-ERROR.
   
   /*Poster*/ 
   raknare = 1.
   fetunder = FALSE.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,3) = "===" THEN DO:
         IF fetunder = TRUE THEN DO:
            fetunder = FALSE.
            RUN understryk_UI (INPUT 4,INPUT 4).
         END.     
         ELSE  RUN understryk_UI (INPUT 4,INPUT 2).        
      END.
      ELSE DO:
         IF SUBSTRING(tidut.UT,estartnr[1],3) = "Dag" THEN DO:
             RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).
             fetunder = TRUE.
         END.    
         ELSE RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT FALSE ,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).                     
        
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF AVAILABLE tidut THEN DO:
         IF tidut.UT = "Månadens flex" OR tidut.UT = "Flexrapport ej körda registreringar" OR tidut.UT BEGINS  "Personalliggare" THEN LEAVE.
      END.         
   END.
END PROCEDURE.

PROCEDURE ftro_UI:   
   
   RUN nyttbladexcel_UI.
   RUN namnbladexcel_UI (INPUT ("Förtroendetid")).
   /*Vilka kolumner*/     
   RUN colbredd_UI.  
   /*Rubriker*/
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   
   RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
   
   FIND NEXT tidut NO-LOCK NO-ERROR.
   IF SUBSTRING(tidut.UT,1,3) = "===" THEN DO:        
   END.
   RUN rubrikerexcel_UI (INPUT "",INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 11).
   FIND NEXT tidut NO-LOCK NO-ERROR.
   
   /*Poster*/ 
   raknare = 1.
   fetunder = FALSE.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,3) = "===" THEN DO:
         IF fetunder = TRUE THEN DO:
            fetunder = FALSE.
            RUN understryk_UI (INPUT 4,INPUT 4).
         END.     
         ELSE  RUN understryk_UI (INPUT 4,INPUT 2).        
      END.
      ELSE DO:
         IF SUBSTRING(tidut.UT,estartnr[1],3) = "Ord" THEN DO:
             RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).
             fetunder = TRUE.
         END.    
         ELSE RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT FALSE ,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 0,INPUT 0).                     
        
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF AVAILABLE tidut THEN DO:
    
         IF tidut.UT = "Bilaga med kommentarer / resmål" OR  tidut.UT = "Månadens flex" OR tidut.UT = "Flexrapport ej körda registreringar" OR tidut.UT BEGINS "Personalliggare" THEN LEAVE.
      END.         
   END.
END PROCEDURE.

PROCEDURE mobe_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 10   
   bredd[1] = 9
   bredd[2] = 15
   bredd[3] = 2
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 8  
   bredd[8] = 7
   bredd[9] = 7
   bredd[10] = 8
   bredd[11] = 7
   bredd[12] = 8
   bredd[13] = 8
   bredd[14] = 9
   bredd[15] = 8.
   helaben = "B".   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   chWorkSheet:COLUMNS(allac[16]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[17]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[18]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[19]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[20]):ColumnWidth = 20 NO-ERROR.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[4],6) = "MONTÖR" OR SUBSTRING(tidut.UT,estartnr[4],6) = "BERED." OR
         SUBSTRING(tidut.UT,estartnr[4],6) = "UTRUST" THEN DO:
            
         LEAVE.
      END.
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).    
         IF Guru.Konstanter:varforetypval[47] = 1 THEN   RUN speunderstryk_UI (INPUT 1,INPUT 4,INPUT 2,INPUT "P",INPUT "T").
         ELSE RUN speunderstryk_UI (INPUT 1,INPUT 4,INPUT 2,INPUT "P",INPUT "S").       
         {EXCELFEL.I}
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            IF SUBSTRING(tidut.UT,estartnr[3],2) = "UF" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE IF SUBSTRING(tidut.UT,estartnr[3],2) = "KA" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).        
            END.           
            IF SUBSTRING(tidut.UT,200,56) NE "" THEN DO:
               RUN speposter_UI (INPUT 1,INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
            END.
            IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                          
               IF SUBSTRING(tidut.UT,260,20) NE "" OR SUBSTRING(tidut.UT,280,20) NE "" THEN DO:
                  RUN speposter3_UI (INPUT 1,INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
               END.               
            END.
         END.
         {EXCELFEL.I}
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING" THEN DO:
         ASSIGN
         helaben = ""
         iRad = 1
         estartnr = 0
         bredd = 0.
         ASSIGN
         slutbredd = 11
         bredd[1] = 10
         bredd[2] = 5
         bredd[3] = 15
         bredd[4] = 8
         bredd[5] = 42
         bredd[6] = 11.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            ASSIGN
            bredd[1] = 9
            bredd[2] = 6.
         END.
         allachar[4] = TRUE.
         RUN exelkost_UI.
         LEAVE.
      END.
   END.   
   
END PROCEDURE.
PROCEDURE mobelule_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 10
   bredd[1] = 9
   bredd[2] = 15
   bredd[3] = 2
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 9  
   bredd[8] = 7
   bredd[9] = 8
   bredd[10] = 8
   bredd[11] = 7
   bredd[12] = 8
   bredd[13] = 8
   bredd[14] = 9
   bredd[15] = 8.   
   helaben = "B".   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   chWorkSheet:COLUMNS(allac[16]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[17]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[18]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[19]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[20]):ColumnWidth = 20 NO-ERROR.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[4],6) = "MONTÖR" OR SUBSTRING(tidut.UT,estartnr[4],6) = "BERED." OR
         SUBSTRING(tidut.UT,estartnr[4],6) = "UTRUST" THEN DO:            
         LEAVE.
      END.
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).    
         IF Guru.Konstanter:varforetypval[47] = 1 THEN   RUN speunderstryk_UI (INPUT 1,INPUT 4,INPUT 2,INPUT "P",INPUT "T").
         ELSE RUN speunderstryk_UI (INPUT 1,INPUT 4,INPUT 2,INPUT "P",INPUT "S").       
         {EXCELFEL.I}
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            IF SUBSTRING(tidut.UT,estartnr[3],2) = "UF" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE IF SUBSTRING(tidut.UT,estartnr[3],2) = "KA" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).        
            END.           
            IF SUBSTRING(tidut.UT,200,56) NE "" THEN DO:
               RUN speposter_UI (INPUT 1,INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
            END.
            IF Guru.Konstanter:globforetag = "gkal" OR Guru.Konstanter:globforetag = "elpa" THEN DO:                          
               IF SUBSTRING(tidut.UT,260,20) NE "" OR SUBSTRING(tidut.UT,280,20) NE "" THEN DO:
                  RUN speposter3_UI (INPUT 1,INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
               END.               
            END.
         END.
         {EXCELFEL.I}
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING" THEN DO:
         ASSIGN
         helaben = ""
         iRad = 1
         estartnr = 0
         bredd = 0.
         ASSIGN
         slutbredd = 11
         bredd[1] = 10
         bredd[2] = 5
         bredd[3] = 15
         bredd[4] = 8
         bredd[5] = 42
         bredd[6] = 11.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            ASSIGN
            bredd[1] = 9
            bredd[2] = 6.
         END.
         allachar[4] = TRUE.
         RUN exelkost_UI.
         LEAVE.
      END.
   END.      
END PROCEDURE.

PROCEDURE mobesund_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 10
   bredd[1] = 9
   bredd[2] = 15
   bredd[3] = 2
   bredd[4] = 6
   bredd[5] = 8
   bredd[6] = 7
   bredd[7] = 7
   bredd[8] =  8
   bredd[9] =  7
   bredd[10] = 8
   bredd[11] = 8
   bredd[12] = 8
   bredd[13] = 10.
   
   helaben = "B".
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[4],6) = "ARBETS" THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            IF SUBSTRING(tidut.UT,estartnr[3],2) = "UF" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE IF SUBSTRING(tidut.UT,estartnr[3],2) = "KA" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).        
            END.           
         END.
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING" THEN DO:
         ASSIGN
         helaben = ""
         iRad = 1
         estartnr = 0
         bredd = 0.
         ASSIGN
         slutbredd = 11
         bredd[1] = 10
         bredd[2] = 5
         bredd[3] = 15
         bredd[4] = 8
         bredd[5] = 42
         bredd[6] = 11.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            ASSIGN
            bredd[1] = 9
            bredd[2] = 6.
         END.
         allachar[4] = TRUE.
         RUN exelkost_UI.
         {EXCELFEL.I}
         LEAVE.
      END.
   END.
END PROCEDURE.

PROCEDURE nymobesund_UI:
   
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 9
   bredd[1] = 40
   bredd[2] = 6
   bredd[3] = 8
   bredd[4] = 6
   bredd[5] = 8
   bredd[6] = 6
   bredd[7] = 8
   bredd[8] =  5
   bredd[9] =  5
   bredd[10] =  8
   bredd[11] = 6
   bredd[12] = 9.
   
   allachar[10] = TRUE.
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
     
      IF SUBSTRING(tidut.UT,1) BEGINS "Utfall för" THEN RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22).
      ELSE IF SUBSTRING(tidut.UT,1) BEGINS "Prognos för" THEN RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22).          
      ELSE RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.            
      IF SUBSTRING(tidut.UT,estartnr[2],2) = "P1" THEN DO:   
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:              
         IF SUBSTRING(tidut.UT,1) BEGINS Guru.Konstanter:gaok THEN DO:         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                    
            RUN bgcell_UI (INPUT 36).
         END.
         ELSE IF SUBSTRING(tidut.UT,1) BEGINS Guru.Konstanter:garbal THEN DO:         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                    
            RUN bgcell_UI (INPUT 36).
         END.    
         ELSE IF SUBSTRING(tidut.UT,1) BEGINS "Summa" THEN DO:         
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                    
            RUN bgcell_UI (INPUT 37).
         END.
         ELSE RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                 
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF tidut.UT BEGINS "PROJEKTNUMMER-TID-DETALJERAT" THEN DO:                           
         ASSIGN
         helaben = ""
         iRad = 1
         estartnr = 0
         bredd = 0.
         ASSIGN
         slutbredd = 6
         bredd[1] = 13
         bredd[2] = 10
         bredd[3] = 8
         bredd[4] = 5
         bredd[5] = 5
         bredd[6] = 6
         bredd[7] = 10
         bredd[8] = 5
         bredd[9] = 9
         bredd[10] = 17
         bredd[11] = 11
         bredd[12] = 9.                  
         allachar[3] = TRUE.
         RUN exeltidd_UI.
         {EXCELFEL.I}         
      END.
      IF tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING" THEN DO:
         ASSIGN
         helaben = ""
         iRad = 1
         estartnr = 0
         bredd = 0.
         ASSIGN
         slutbredd = 11
         bredd[1] = 10
         bredd[2] = 5
         bredd[3] = 15
         bredd[4] = 8
         bredd[5] = 42
         bredd[6] = 11.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            ASSIGN
            bredd[1] = 9
            bredd[2] = 6.
         END.
         allachar[4] = TRUE.
         RUN exelkost_UI.
         {EXCELFEL.I}
         LEAVE.
      END.
   END.
END PROCEDURE.

PROCEDURE proje_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 6   
   bredd[1] = 9
   bredd[2] = 40
   bredd[3] = 6
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 7
   bredd[8] = 7
   bredd[9] = 8
   bredd[10] = 8
   bredd[11] = 7
   bredd[12] = 8
   bredd[13] = 8
   bredd[14] = 8
   bredd[15] = 8
   bredd[16] = 8
   bredd[17] = 8
   bredd[18] = 8
   bredd[19] = 8
   bredd[20] = 8
   bredd[21] = 8
   bredd[22] = 8
   bredd[23] = 8
   bredd[24] = 8
   bredd[25] = 8
   bredd[26] = 8
   bredd[27] = 8
   bredd[28] = 8.   
   helaben = "".
   ASSIGN
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE 
   allachar[7] = TRUE. 
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   chWorkSheet:COLUMNS(allac[26]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[27]):ColumnWidth = 20 NO-ERROR.
   chWorkSheet:COLUMNS(allac[28]):ColumnWidth = 20 NO-ERROR.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[9],6) = "MONTÖR" OR 
         SUBSTRING(tidut.UT,estartnr[9],6) = "UTRUST" THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            IF SUBSTRING(tidut.UT,estartnr[3],2) = "UF" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE IF SUBSTRING(tidut.UT,estartnr[3],2) = "KA" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).        
            END.
            IF SUBSTRING(tidut.UT,300,56) NE "" THEN DO:
               RUN speposter2_UI (INPUT 1,INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
            END.
         END.
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF tidut.UT = "VERIFIKAT FRÅN KOSTNADSREGISTRERING" THEN DO:
         ASSIGN
         helaben = ""
         iRad = 1
         estartnr = 0
         bredd = 0.
         ASSIGN
         slutbredd = 11
         bredd[1] = 10
         bredd[2] = 5
         bredd[3] = 15
         bredd[4] = 8
         bredd[5] = 42
         bredd[6] = 11.
         IF Guru.Konstanter:globforetag = "SUND" OR Guru.Konstanter:globforetag = "SNAT" OR Guru.Konstanter:globforetag = "MISV" THEN DO:
            ASSIGN
            bredd[1] = 9
            bredd[2] = 6.
         END.
         allachar[4] = TRUE.
         RUN exelkost_UI.
         {EXCELFEL.I}
         LEAVE.
      END.
   END.
END PROCEDURE.

PROCEDURE miproje_UI:
   
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 13
   bredd[1] = 9
   bredd[2] = 40
   bredd[3] = 13
   bredd[4] = 13
   bredd[5] = 13
   bredd[6] = 13
   bredd[7] = 13
   bredd[8] = 13
   bredd[9] = 13
   bredd[10] = 13.
      
   helaben = "".
   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   chWorkSheet:COLUMNS(allac[10]):ColumnWidth = 20 NO-ERROR.
   
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[3],3) = "TJM"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            IF SUBSTRING(tidut.UT,estartnr[3],2) = "UF" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            
            ELSE DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).        
            END.
            
            IF SUBSTRING(tidut.UT,260,20) NE "" THEN DO:
               RUN speposter4_UI (INPUT 1,INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
            END.
         END.
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.      
END PROCEDURE.

PROCEDURE tidlagelule_UI:
   ASSIGN
   slutbredd = 7   /*antal kolumner*/
   bredd[1] = 8
   bredd[2] = 3
   bredd[3] = 30
   bredd[4] = 7
   bredd[5] = 7
   bredd[6] = 7
   bredd[7] = 25
   bredd[8] = 10
   bredd[9] = 6
   bredd[10] = 7.     
   helaben = "".   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   chWorkSheet:COLUMNS(allac[10]):ColumnWidth = 20 NO-ERROR.
   
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[3],3) = "ORT"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.   
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).            
         END.
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
   END.      
END PROCEDURE.


PROCEDURE personal_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 10   
   bredd[1] = 20
   bredd[2] = 9
   bredd[3] = 40
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 6
   bredd[8] = 9
   bredd[9] = 9
   bredd[10] = 10
   bredd[11] = 10.   
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE 
   allachar[7] = TRUE. 
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[8],9) = "Normaltid"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE prognos_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 13   
   bredd[1] = 9
   bredd[2] = 40
   bredd[3] = 13
   bredd[4] = 13.   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      IF SUBSTRING(tidut.UT,estartnr[2],13) = CAPS(Guru.Konstanter:gaonamnk)  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.



PROCEDURE personalarb_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 20   
   bredd[1] = 30
   bredd[2] = 12
   bredd[3] = 12
   bredd[4] = 12
   bredd[5] = 12
   bredd[6] = 12.
   bredd[7] = 12.
   bredd[8] = 12.
   bredd[9] = 20.   
   helaben = "".
   ASSIGN
   allachar[1] = TRUE.   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                        
      IF SUBSTRING(tidut.UT,1,10) = "enhet/sign"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.


PROCEDURE personalarbsam_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 15   
   bredd[1] = 12
   bredd[2] = 12
   bredd[3] = 6
   bredd[4] = 30
   bredd[5] = 6
   bredd[6] = 25
   bredd[7] = 15   
   bredd[8] = 15
   bredd[9] = 15
   bredd[10] = 15
   bredd[11] = 15
   bredd[12] = 15
   bredd[13] = 15
   bredd[14] = 15
   bredd[15] = 15
   bredd[16] = 15
   bredd[17] = 15
   bredd[18] = 15
   bredd[19] = 15
   bredd[20] = 15
   bredd[21] = 15
   bredd[22] = 15
   bredd[23] = 15
   bredd[24] = 15
   bredd[25] = 15.   
   helaben = "".
   ASSIGN
   allachar[1] = TRUE.   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.   
   /*Poster*/ 
   raknare = 1.
   REPEAT:        
      RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE personalovsu_UI:   
   FIND FIRST tidutbuff WHERE SUBSTRING(tidutbuff.UT,64,6) = "PERIOD" NO-ERROR.
   IF AVAILABLE tidutbuff THEN DO:
      stdat = date(SUBSTRING(tidutbuff.UT,71,8)).
      sldat = date(SUBSTRING(tidutbuff.UT,82,8)).
   END.
   ELSE DO:
      stdat = ?.
      sldat = ?.
   END.            
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 12   
   bredd[1] = 30
   bredd[2] = 6
   bredd[3] = 20
   bredd[4] = 6
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 6
   bredd[8] = 6
   bredd[9] = 6
   bredd[10] = 6
   bredd[11] = 6
   bredd[12] = 6
   bredd[13] = 6
   bredd[14] = 6 
   bredd[15] = 6 
   bredd[16] = 12    .
   helaben = "".
   ASSIGN
   allachar[1] = TRUE.   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                        
      IF SUBSTRING(tidut.UT,1,10) = "enhet/sign"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.      
   IF sldat NE ? THEN DO:
      IF MONTH(sldat) < 12 THEN RUN delCol_UI (INPUT 15).
      IF MONTH(sldat) < 11 THEN RUN delCol_UI (INPUT 14).
      IF MONTH(sldat) < 10 THEN RUN delCol_UI (INPUT 13).
      IF MONTH(sldat) < 9 THEN RUN delCol_UI (INPUT 12).
      IF MONTH(sldat) < 8 THEN RUN delCol_UI (INPUT 11).
      IF MONTH(sldat) < 7 THEN RUN delCol_UI (INPUT 10).
      IF MONTH(sldat) < 6 THEN RUN delCol_UI (INPUT 9).
      IF MONTH(sldat) < 5 THEN RUN delCol_UI (INPUT 8).
      IF MONTH(sldat) < 4 THEN RUN delCol_UI (INPUT 7).
      IF MONTH(sldat) < 3 THEN RUN delCol_UI (INPUT 6).
      IF MONTH(sldat) < 2 THEN RUN delCol_UI (INPUT 5).
   END.   
   IF stdat NE ? THEN DO:
      IF MONTH(stdat) > 11 THEN RUN delCol_UI (INPUT 14).
      IF MONTH(stdat) > 10 THEN RUN delCol_UI (INPUT 13).
      IF MONTH(stdat) > 9 THEN RUN delCol_UI (INPUT 12).
      IF MONTH(stdat) > 8 THEN RUN delCol_UI (INPUT 11).
      IF MONTH(stdat) > 7 THEN RUN delCol_UI (INPUT 10).
      IF MONTH(stdat) > 6 THEN RUN delCol_UI (INPUT 9).
      IF MONTH(stdat) > 5 THEN RUN delCol_UI (INPUT 8).
      IF MONTH(stdat) > 4 THEN RUN delCol_UI (INPUT 7).
      IF MONTH(stdat) > 3 THEN RUN delCol_UI (INPUT 6).
      IF MONTH(stdat) > 2 THEN RUN delCol_UI (INPUT 5).
      IF MONTH(stdat) > 1 THEN RUN delCol_UI (INPUT 4).
   END.                   
END PROCEDURE.

PROCEDURE personalarb2_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 10   
   bredd[1] = 60
   bredd[2] = 15
   bredd[3] = 30
   bredd[4] = 10
   bredd[5] = 30
   bredd[6] = 15.
   
   helaben = "".
   ASSIGN
   allachar[1] = TRUE.
   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                        
      IF SUBSTRING(tidut.UT,1,12) = "Årsarbetstid"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE personalarbdeb_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 10   
   bredd[1] = 30
   bredd[2] = 20
   bredd[3] = 8
   bredd[4] = 13
   bredd[5] = 13
   bredd[6] = 22
   bredd[7] = 8   
   helaben = "".
   ASSIGN
   allachar[1] = TRUE.   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                        
      IF SUBSTRING(tidut.UT,1,10) = "enhet/sign"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.


PROCEDURE personalkompetens_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 50   
   bredd[1] = 30
   bredd[2] = 12
   bredd[3] = 10
   bredd[4] = 10
   bredd[5] = 12
   bredd[6] = 50.   
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE.   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[5],9) = "Timmar"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
   END.
END PROCEDURE.

PROCEDURE personalfranvaro_UI:
   /*Vilka kolumner*/
   ASSIGN
   slutbredd = 30   
   bredd[1] = 7
   bredd[2] = 40
   bredd[3] = 15   
   bredd[4] = 10
   bredd[5] = 15   
   bredd[6] = 12
   bredd[7] = 10
   bredd[8] = 10
   bredd[9] = 12
   bredd[10] = 12.
   bredd[11] = 30.        
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE.   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[9],9) = "Timmar"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
   END.
END PROCEDURE.


PROCEDURE personalovber_UI:
   /*Vilka kolumner*/
   IF Guru.Konstanter:globforetag = "snat" THEN DO:
      ASSIGN
      slutbredd = 35   
      bredd[1] = 30   
      bredd[2] = 12
      bredd[3] = 10
      bredd[4] = 10
      bredd[5] = 12
      bredd[6] = 25
      bredd[7] = 25
      bredd[8] = 7
      bredd[9] = 6
      bredd[10] = 50
      bredd[11] = 60.
      helaben = "".
      ASSIGN
      allachar[1] = TRUE
      allachar[2] = TRUE
      allachar[8] = TRUE.
   END.
   ELSE DO:         
      ASSIGN
      slutbredd = 35   
      bredd[1] = 30   
      bredd[2] = 12
      bredd[3] = 10
      bredd[4] = 10
      bredd[5] = 12
      bredd[6] = 25
      bredd[7] = 7
      bredd[8] = 6
      bredd[9] = 50
      bredd[10] = 60.
      helaben = "".
      ASSIGN
      allachar[1] = TRUE
      allachar[2] = TRUE
      allachar[7] = TRUE.
   END.      
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[5],9) = "Övertid"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE glomdflex_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 70   
   bredd[1] = 7
   bredd[2] = 9
   bredd[3] = 15
   bredd[4] = 8
   bredd[5] = 10
   bredd[6] = 5
   bredd[7] = 6
   bredd[8] = 19
   bredd[9] = 70.     
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE
   allachar[7] = TRUE
   allachar[8] = TRUE
   allachar[9] = TRUE.    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE otbeord_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 25   
   bredd[1] = 5
   bredd[2] = 5
   bredd[3] = 8
   bredd[4] = 5
   bredd[5] = 5
   bredd[6] = 7
   bredd[7] = 10
   bredd[8] = 6
   bredd[9] = 6.
   bredd[10] = 25.
      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE
   allachar[7] = TRUE
   allachar[8] = TRUE
   allachar[9] = TRUE
   allachar[10] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE kontrollberover_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 5   
   bredd[1] = 5
   bredd[2] = 36
   bredd[3] = 4
   bredd[4] = 6
   bredd[5] = 20
   bredd[6] = 11.
   bredd[7] = 5.
      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE
   allachar[7] = TRUE. 
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.
PROCEDURE kontrolltid_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 20   
   bredd[1] = 5
   bredd[2] = 25
   bredd[3] = 17
   bredd[4] = 20
   bredd[5] = 20
   bredd[6] = 20.      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE.     
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.


PROCEDURE obsjuk_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 6   
   bredd[1] = 5
   bredd[2] = 36
   bredd[3] = 11
   bredd[4] = 4
   bredd[5] = 14
   bredd[6] = 6
   bredd[7] = 6.
      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE
   allachar[7] = TRUE. 
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE listtid_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 8   
   bredd[1] = 7
   bredd[2] = 15
   bredd[3] = 25
   bredd[4] = 6
   bredd[5] = 9
   bredd[6] = 8   
   bredd[7] = 8.      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE
   allachar[7] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE kontrolltillagg_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 25   
   bredd[1] = 7
   bredd[2] = 15
   bredd[3] = 24    /*31*/
   bredd[4] = 8
   bredd[5] = 5
   bredd[6] = 5   
   bredd[7] = 25.      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE
   allachar[7] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE gjbest_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 25   
   bredd[1] = 7
   bredd[2] = 15
   bredd[3] = 31
   bredd[4] = 8
   bredd[5] = 5
   bredd[6] = 5   
   bredd[7] = 25.
      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE
   allachar[7] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.
PROCEDURE gjbestmtrl_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 25   
   bredd[1] = 7
   bredd[2] = 15
   bredd[3] = 31
   bredd[4] = 8
   bredd[5] = 5
   bredd[6] = 5   
   bredd[7] = 25.
      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE 
   allachar[6] = TRUE
   allachar[7] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "Enhet"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         /*IF tidut.UT NE "" THEN DO:           */
            RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                    
         /*END.*/
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE kontrollovertid_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 25   
   bredd[1] = 6
   bredd[2] = 30
   bredd[3] = 19    
   bredd[4] = 25.     
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],4) = "SIGN"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.


PROCEDURE kontrolldepa_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 25   
   bredd[1] = 11
   bredd[2] = 40
   bredd[3] = 8    
   bredd[4] = 5
   bredd[5] = 25.     
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],3) = "ENR"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE fortroendetid_UI:
   /*Vilka kolumner*/        
   ASSIGN
   slutbredd = 9   
   bredd[1] = 6
   bredd[2] = 23
   bredd[3] = 9
   bredd[4] = 9    
   bredd[5] = 9
   bredd[6] = 9
   bredd[7] = 7
   bredd[8] = 11    
   bredd[9] = 11
   bredd[10] = 11    
   bredd[11] = 9
   bredd[12] = 9.
      
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE   
   allachar[4] = TRUE
   allachar[5] = TRUE
   allachar[6] = TRUE
   allachar[7] = TRUE   
   allachar[8] = TRUE
   allachar[9] = TRUE   
   allachar[10] = TRUE.    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],4) = "SIGN"  THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "ENHET"  THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[5],5) = "Varav"  THEN DO:
         LEAVE.
      END.
      
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.


PROCEDURE kontrollfriskv_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 6   
   bredd[1] = 5
   bredd[2] = 7
   bredd[3] = 8    
   bredd[4] = 13    
   bredd[5] = 6    
   bredd[6] = 6.         
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE
   allachar[6] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "ENHET"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.

PROCEDURE kontrollejnoll_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 15   
   bredd[1] = 5
   bredd[2] = 7
   bredd[3] = 8    
   bredd[4] = 13    
   bredd[5] = 6    
   bredd[6] = 6
   bredd[7] = 3
   bredd[8] = 12
   bredd[9] = 5
   bredd[10] = 15.         
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE
   allachar[6] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],5) = "ENHET"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                            
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.


PROCEDURE kontrollflex_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 25   
   bredd[1] = 5
   bredd[2] = 9
   bredd[3] = 15
   bredd[4] = 9
   bredd[5] = 9
   bredd[6] = 9    
   bredd[7] = 25.     
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE
   allachar[6] = TRUE
   allachar[7] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],4) = "SIGN"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.
PROCEDURE kontrollkompsaldo_UI:
      
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 25   
   bredd[1] = 5
   bredd[2] = 9
   bredd[3] = 15
   bredd[4] = 9
   bredd[5] = 23
   bredd[6] = 25.     
   helaben = "".
   ASSIGN
   allachar[1] = TRUE
   allachar[2] = TRUE
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE
   allachar[5] = TRUE
   allachar[6] = TRUE
   allachar[7] = TRUE.
    
   RUN colbredd_UI.  
   RUN startexcel_UI.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[1],4) = "SIGN"  THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:         
         RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).                             
      END.   
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      {EXCELFEL.I}
   END.
END PROCEDURE.


PROCEDURE tidsedel_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 6   
   bredd[1] = 3
   bredd[2] = 2
   bredd[3] = 5
   bredd[4] = 5
   bredd[5] = 5
   bredd[6] = 9
   bredd[7] = 20
   bredd[8] = 4
   bredd[9] = 5
   bredd[10] = 5
   bredd[11] = 5
   bredd[12] = 5
   bredd[13] = 4
   bredd[14] = 4
   bredd[15] = 3
   bredd[16] = 4
   bredd[17] = 4
   bredd[18] = 5
   bredd[19] = 5      
   helaben = "".
   ASSIGN
   allachar[3] = TRUE    /*vilka kolumner skall vara character*/
   allachar[4] = TRUE.   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   RUN namnbladexcel_UI (INPUT ("Tidsedel")).
   ftrorader = FALsE.
   slrader = FALSE.
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 14,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,estartnr[3],5) = "Start" THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   slrader = FALSE.
   fetstart = FALSE.
   REPEAT:
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      IF SUBSTRING(tidut.UT,1,3) = "===" THEN DO:
         IF ftrorader = TRUE THEN DO:         
            chWorkSheet:Range("A" + string(irad) + ":" + "F" +  string(irad)):Borders(4):Weight = 4 NO-ERROR.
         END.
         ELSE RUN understryk_UI (INPUT 4,INPUT 4).       
      END.
      ELSE DO:         
         IF SUBSTRING(tidut.UT,1,3) = "---" THEN DO:
            RUN understryk_UI (INPUT 4,INPUT 2).       
         END.
         ELSE DO:
            IF slrader = TRUE THEN DO:
               IF ftrorader = TRUE THEN DO:                                    
                  RUN fontexcel_UI (INPUT "Calibri",INPUT 12,INPUT true,INPUT 14,INPUT 0).
                  cRange = "C" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,1,9) NO-ERROR.
                  cRange = "D" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,11,9) NO-ERROR.
                  cRange = "E" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,21,9) NO-ERROR.
                  cRange = "F" + string(irad).
                  chWorkSheet:Range(cRange):Value = SUBSTRING(tidut.UT,31,9) NO-ERROR.                  
                  chWorkSheet:COLUMNS("E"):ColumnWidth = 9 NO-ERROR.                                 
               END.   
               ELSE RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 14,INPUT 11).
               IF tidut.UT BEGINS  "Förtroendetid" THEN DO:                  
                  RUN posterexcel_UI (INPUT "",INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 13,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
                  ftrorader = TRUE.
               END.   
            END.
            ELSE DO:   
               IF tidut.UT BEGINS  "Reg" THEN fetstart = TRUE.
               IF SUBSTRING(tidut.UT,estartnr[3],5) = "Start" THEN DO:
                  RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
                  iRad = iRad - 1.
                  RUN fontexcel_UI (INPUT "Calibri",INPUT 12,INPUT true,INPUT 12,INPUT 0).                    
                  chWorkSheet:Range(allac[11] + cRad):VALUE = "" NO-ERROR.
                  chWorkSheet:Range(allac[12] + cRad):VALUE = "" NO-ERROR.
                  chWorkSheet:Range(allac[13] + cRad):VALUE = "" NO-ERROR.
                  chWorkSheet:Range(allac[14] + cRad):VALUE = "" NO-ERROR.
                  chWorkSheet:Range(allac[15] + cRad):VALUE = "" NO-ERROR.
                  chWorkSheet:Range(allac[16] + cRad):VALUE = "" NO-ERROR.
                  chWorkSheet:Range(allac[17] + cRad):VALUE = "" NO-ERROR.
                  chWorkSheet:Range(allac[18] + cRad):VALUE = "" NO-ERROR.
                  chWorkSheet:Range(allac[19] + cRad):VALUE = "" NO-ERROR.
                  cRange = allac[11] + cRad  + ":" + allac[13] + cRad.                        
                  chWorksheetRange = chWorkSheet:Range(cRange) NO-ERROR.            
                  chWorksheetRange:WrapText = TRUE NO-ERROR.
                  chWorksheetRange:Orientation = 0 NO-ERROR.
                  chWorksheetRange:AddIndent = FALSE NO-ERROR.
                  chWorksheetRange:ShrinkToFit = FALSE NO-ERROR.
                  chWorksheetRange:MergeCells = TRUE NO-ERROR.             
                  chWorkSheet:Range(cRange):VALUE = "Lönetillägg" NO-ERROR.
                  cRange = allac[14] + cRad  + ":" + allac[16] + cRad.                        
                  chWorksheetRange = chWorkSheet:Range(cRange) NO-ERROR.            
                  chWorksheetRange:WrapText = TRUE NO-ERROR.
                  chWorksheetRange:Orientation = 0 NO-ERROR.
                  chWorksheetRange:AddIndent = FALSE NO-ERROR.
                  chWorksheetRange:ShrinkToFit = FALSE NO-ERROR.
                  chWorksheetRange:MergeCells = TRUE NO-ERROR.             
                  chWorkSheet:Range(cRange):VALUE = "Traktamente" NO-ERROR.
                  cRange = allac[17] + cRad  + ":" + allac[19] + cRad.                        
                  chWorksheetRange = chWorkSheet:Range(cRange) NO-ERROR.            
                  chWorksheetRange:WrapText = TRUE NO-ERROR.
                  chWorksheetRange:Orientation = 0 NO-ERROR.
                  chWorksheetRange:AddIndent = FALSE NO-ERROR.
                  chWorksheetRange:ShrinkToFit = FALSE NO-ERROR.
                  chWorksheetRange:MergeCells = TRUE NO-ERROR.             
                  chWorkSheet:Range(cRange):VALUE = "Beredskap" NO-ERROR.      
   
                END.   
               ELSE IF SUBSTRING(tidut.UT,estartnr[1],3) = "dag" THEN  RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT TRUE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
               ELSE DO:
                  RUN posterexcel_UI (INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT fetstart,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
                  IF tidut.UT BEGINS  "Reg" OR tidut.UT BEGINS  "Varav" OR  tidut.UT BEGINS  "Ord" or tidut.UT BEGINS  "Arbetad" THEN DO:
                     fetstart = TRUE.
                     RUN speposter5_UI (INPUT 1,INPUT tidut.UT,INPUT "Calibri",INPUT 12,INPUT true,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).
                  END.
                  IF tidut.UT BEGINS  "Ord" THEN slrader = TRUE. 
               END.
            END.                  
         END.            
      END. 
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      IF AVAILABLE tidut THEN DO:    
         IF tidut.UT = "Månadens flex"  THEN DO:
            ASSIGN
            helaben = ""
            iRad = 1
            estartnr = 0
            bredd = 0.
            ASSIGN
            slutbredd = 12
            bredd[1] = 3
            bredd[2] = 3
            bredd[3] = 7
            bredd[4] = 12
            bredd[5] = 16
            bredd[6] = 12.
            IF Guru.Konstanter:globforetag = "gkal" THEN DO:
               assign
               bredd[6] = 6
               bredd[7] = 11
               bredd[8] = 25.
               slutbredd = 25.
            END.             
            RUN flexrappLUKA_UI.
            {EXCELFEL.I}            
         END.
      END.
      IF AVAILABLE tidut THEN DO:
         IF tidut.UT BEGINS "Förtroendetid tom"  THEN DO:
            ASSIGN
            helaben = ""
            iRad = 1
            estartnr = 0
            bredd = 0.
            
            ASSIGN
            slutbredd = 12   
            bredd[1] = 9
            bredd[2] = 10
            bredd[3] = 9
            bredd[4] = 9
            bredd[5] = 9.                                    
            RUN ftro_UI.
            {EXCELFEL.I}            
         END.
      END.
      IF AVAILABLE tidut THEN DO:
         IF tidut.UT = "Bilaga med kommentarer / resmål"  THEN DO:
            ASSIGN
            helaben = ""
            iRad = 1
            estartnr = 0
            bredd = 0.
            ASSIGN
            slutbredd = 12
            bredd[1] = 3
            bredd[2] = 2
            bredd[3] = 5
            bredd[4] = 5
            bredd[5] = 5         
            bredd[6] = 6
            bredd[7] = 3
            bredd[8] = 79
            bredd[9] = 12.                        
            RUN bilagakomm_UI.
            {EXCELFEL.I}            
         END.
      END.
      IF AVAILABLE tidut THEN DO:
         IF tidut.UT = "Gjorda rättningar"  THEN DO:
            ASSIGN
            helaben = ""
            iRad = 1
            estartnr = 0
            bredd = 0.
            
            ASSIGN
            slutbredd = 15   
            bredd[1] = 3
            bredd[2] = 3
            bredd[3] = 7
            bredd[4] = 7
            bredd[5] = 6
            bredd[6] = 6
            bredd[7] = 3
            bredd[8] = 3
            bredd[9] = 6
            bredd[10] = 15.
            
            ASSIGN
            allachar[3] = TRUE    /*vilka kolumner skall vara character*/
            allachar[4] = TRUE.            
            RUN rattningar_UI.
            {EXCELFEL.I}            
         END.
      END.
      
      IF AVAILABLE tidut THEN DO:    
         IF tidut.UT = "Flexrapport ej körda registreringar" THEN DO:            
            ASSIGN
            helaben = ""
            iRad = 1
            estartnr = 0
            bredd = 0.
            ASSIGN
            slutbredd = 6
            bredd[1] = 7
            bredd[2] = 3
            bredd[3] = 10
            bredd[4] = 5
            bredd[5] = 5
            bredd[6] = 10
            bredd[7] = 5
            bredd[8] = 3
            bredd[9] = 20
            bredd[10] = 29
            bredd[11] = 5
            bredd[12] = 5
            bredd[13] = 5
            bredd[14] = 5.
            
            ASSIGN
            allachar[4] = TRUE
            allachar[5] = TRUE
            allachar[11] = TRUE
            allachar[12] = TRUE
            allachar[13] = TRUE
            allachar[14] = TRUE.
            RUN flexrapp_UI.
            {EXCELFEL.I}            
         END.
      END.
      IF AVAILABLE tidut THEN DO:    
         IF tidut.UT BEGINS "Personalliggare" THEN DO:
           
            ASSIGN
            helaben = ""
            iRad = 1
            estartnr = 0
            bredd = 0.
            ASSIGN
            slutbredd = 6
            bredd[1] = 23
            bredd[2] = 9
            bredd[3] = 29
            bredd[4] = 3
            bredd[5] = 19
            bredd[6] = 19
            bredd[7] = 5.
            ASSIGN
            allachar[1] = TRUE
            allachar[2] = TRUE
            allachar[3] = TRUE
            allachar[4] = TRUE
            allachar[5] = TRUE
            allachar[6] = TRUE
            allachar[7] = TRUE.        
            RUN bilagaPL_UI.
            {EXCELFEL.I}
            LEAVE.
         END.
      END.   
   END.
END PROCEDURE.



PROCEDURE kompinut_UI:
   /*Vilka kolumner*/     
   ASSIGN
   slutbredd = 15
   bredd[1] = 6
   bredd[2] = 6
   bredd[3] = 6
   bredd[4] = 6     
   bredd[5] = 6
   bredd[6] = 6
   bredd[7] = 6
   bredd[8] = 6
   bredd[9] = 6
   bredd[10] = 6
   bredd[11] = 6
   bredd[12] = 6
   bredd[13] = 6
   bredd[14] = 6
   bredd[15] = 6
   bredd[16] = 6
   
   bredd[17] = 6
   bredd[18] = 15.     
   helaben = "".   
   RUN colbredd_UI.  
   RUN startexcel_UI.
   {EXCELFEL.I}
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   
   {EXCELFEL.I}
   REPEAT:
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.      
      IF SUBSTRING(tidut.UT,estartnr[3],4) = "Hela" THEN DO:
         LEAVE.
      END.
      {EXCELFEL.I}
   END.
   /*Poster*/ 
   raknare = 1.
   REPEAT:
      IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).       
      END.
      ELSE DO:
         IF tidut.UT NE "" THEN DO:
            IF SUBSTRING(tidut.UT,estartnr[18],9) = " Intjänad" OR SUBSTRING(tidut.UT,estartnr[18],8) = "Intjänad" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE IF SUBSTRING(tidut.UT,estartnr[18],8) = " Uttagen" OR SUBSTRING(tidut.UT,estartnr[18],7) = "Uttagen" THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 22,INPUT FALSE,INPUT TRUE,INPUT 25,INPUT 143).
            END.
            ELSE RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 25,INPUT 143).            
         END.
      END.   
      {EXCELFEL.I}
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.
      
   END.
END PROCEDURE.
