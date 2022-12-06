/*manepostexce.i*/

DEFINE INPUT PARAMETER vad AS INTEGER NO-UNDO.
/*vad = 1, epost vad = 2 på skärm*/

DEFINE VARIABLE filformat AS CHARACTER NO-UNDO.
DEFINE VARIABLE iColumn                 AS INTEGER INITIAL 0.
DEFINE VARIABLE cColumn                 AS CHARACTER.
DEFINE VARIABLE ivar AS INTEGER NO-UNDO.  
DEFINE VARIABLE postvar AS LOGICAL NO-UNDO.
felexcel = FALSE.
   ASSIGN
   allac[1] = "A"          
   allac[2] = "B"          
   allac[3] = "C"          
   allac[4] = "D"          
   allac[5] = "E"          
   allac[6] = "F".          
   IF Guru.Konstanter:mtrlsekvar[6] = TRUE THEN DO:
      ASSIGN      
      bredd[1] = 10
      bredd[2] = 35 
      bredd[3] = 6
      bredd[4] = 5.    
      slutbredd = 8.
      breddantal = 4.
      ivar = 4.
   END.
   ELSE DO:
      ASSIGN           
      bredd[1] = 10
      bredd[2] = 35 
      bredd[3] = 6
      bredd[4] = 5
      bredd[5] = 9
      bredd[6] = 10
      slutbredd = 10.
      breddantal = 6.
      ivar = 6.
   END.      
   ASSIGN
   allachar[1] = TRUE
   bladvar = 0.
   iRad = 1.   
   RUN colbredd_UI.  
   IF vad = 1 THEN RUN startexceld_UI.
   ELSE IF vad = 2 THEN RUN startexcel_UI.
   IF vad = 1 THEN RUN visaexel_UI (FALSE).
   ELSE IF vad = 2 THEN RUN visaexel_UI (TRUE).
   ASSIGN
   iColumn = 1
   cColumn = STRING(iColumn)
   cRange = "A" + cColumn   
   link = ?.
   /*FOREBILDER*/
   {LOGGOR.I}
   IF link NE ? THEN DO:
      IF raknare = 0 THEN RUN imageexcel_UI (INPUT link,INPUT "A",INPUT 1).
      ELSE RUN imageexcel_UI (INPUT link,INPUT "A",INPUT raknare).
      iColumn = iColumn + 5.
      cColumn = STRING(iColumn).
      cRange = "A" + cColumn.
      irad = iColumn.
   END.   
   IF vad = 1 THEN chExcelApplication:VISIBLE = FALSE.
   /*Rubriker*/
   FIND FIRST tidut NO-LOCK NO-ERROR.
   raknare = 1.
   /*Kolumnbredd*/
   RUN kolumnexcel_UI.
   REPEAT:
      /*
      RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
      */
      RUN rubrikerexceltwocol_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11, INPUT 40, INPUT "c").
      FIND NEXT tidut NO-LOCK NO-ERROR.
      IF NOT AVAILABLE tidut THEN DO:
         LEAVE.
      END.                                    
      IF SUBSTRING(tidut.UT,estartnr[1],3) = "ENR" THEN DO:         
         LEAVE.         
      END.           
      IF SUBSTRING(tidut.UT,estartnr[1],12) = "============" OR SUBSTRING(tidut.UT,estartnr[1],12) = "************" OR SUBSTRING(tidut.UT,estartnr[1],12) = "------------" THEN DO:
         RUN understryk_UI (INPUT 4,INPUT 2).  
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END. 
      END.              
   END.
   
   /*Poster*/ 
   IF NOT AVAILABLE tidut THEN DO:
      FIND NEXT tidut NO-ERROR.
   END.
   IF AVAILABLE tidut THEN DO:
      raknare = 1.
      REPEAT:
         IF SUBSTRING(tidut.UT,1,5) = "=====" THEN DO:
            RUN understryk_UI (INPUT 4,INPUT 2).                
         END.              
         ELSE IF SUBSTRING(tidut.UT,58,13) = "Summa totalt:" THEN DO:                     
            LEAVE.
         END.                                  
         ELSE IF SUBSTRING(tidut.UT,58,7) = "Summa :" THEN DO:                     
            LEAVE.
         END.
         ELSE IF SUBSTRING(tidut.UT,1,12) = "============" OR SUBSTRING(tidut.UT,1,12) = "************" OR SUBSTRING(tidut.UT,1,12) = "------------" THEN DO:         
            LEAVE.
         END.
         ELSE DO:
            /*IF tidut.UT NE "" THEN DO:*/
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
            /*END.*/
         END.   
         FIND NEXT tidut NO-LOCK NO-ERROR.
         IF NOT AVAILABLE tidut THEN DO:
            LEAVE.
         END.      
      END.   
      IF NOT AVAILABLE tidut THEN DO:
         FIND NEXT tidut NO-ERROR.
      END.
      IF AVAILABLE tidut THEN DO:
         /*REPEAT:
            IF SUBSTRING(tidut.UT,1,12) = "============" OR SUBSTRING(tidut.UT,1,12) = "************" OR SUBSTRING(tidut.UT,1,12) = "------------" THEN DO:
               RUN understryk_UI (INPUT 4,INPUT 2).  
               FIND NEXT tidut NO-LOCK NO-ERROR.
               IF NOT AVAILABLE tidut THEN DO:
                  LEAVE.
               END. 
            END.              
            RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
            FIND NEXT tidut NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidut THEN DO:
               LEAVE.
            END.                                    
         END.   */
         postvar = FALSE.
         REPEAT:            
            IF SUBSTRING(tidut.UT,1,12) = "============" OR SUBSTRING(tidut.UT,1,12) = "************" OR SUBSTRING(tidut.UT,1,12) = "------------" THEN DO:
               postvar = FALSE.
               RUN understryk_UI (INPUT 4,INPUT 2).  
               FIND NEXT tidut NO-LOCK NO-ERROR.
               IF NOT AVAILABLE tidut THEN DO:
                  LEAVE.
               END. 
            END.
            ELSE IF SUBSTRING(tidut.UT,58,13) = "Summa totalt:" THEN DO:
               postvar = FALSE.                     
            END.              
            ELSE IF SUBSTRING(tidut.UT,1,3) = "ENR" THEN DO:
               postvar = TRUE.               
            END.              
            IF postvar = TRUE THEN DO:
               RUN posterexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 0,INPUT FALSE,INPUT FALSE,INPUT 0,INPUT 0).           
            END.
            ELSE DO:            
               RUN rubrikerexcel_UI (INPUT tidut.UT,INPUT "COURIER",INPUT 10,INPUT FALSE,INPUT 12,INPUT 11).
            END.
            FIND NEXT tidut NO-LOCK NO-ERROR.
            IF NOT AVAILABLE tidut THEN DO:
               LEAVE.
            END.                                    
         END.   
      END.
   END.
   /* test bifoga skyltmall i beställning*/
   /*IF visaskylt = TRUE THEN DO:
      RUN EXKSKMAES.P (INPUT 1 ,INPUT 3).
   END.*/   
   DEFINE VARIABLE upplagnr AS INTEGER NO-UNDO.
   ASSIGN
   iColumn = 1
   cColumn = STRING(iColumn)
   cRange = "A" + cColumn.
   iRad = 1.
   ASSIGN      
      bredd[1] = 10
      bredd[2] = 35
      bredd[3] = 6
      bredd[4] = 5.    
      slutbredd = 8.
      breddantal = 4.
      ivar = 4.
   
   
   raknare = 1. 
   FIND FIRST stolpink_mtrl WHERE NO-LOCK NO-ERROR.
   IF AVAILABLE stolpink_mtrl THEN DO:
      RUN namnbladexcel_UI ("Beställning").
      RUN valjblad_UI  (2).
      RUN colbredd_UI.    
      RUN kolumnexcel_UI.
      RUN namnbladexcel_UI ("Stolpprotokoll").
      FOR EACH stolpink_mtrl WHERE stolpink_mtrl.UPPLAG NE ? NO-LOCK BY stolpink_mtrl.UPPLAG BY stolpink_mtrl.ENR:
         IF upplagnr NE stolpink_mtrl.UPPLAG THEN DO:
            upplagnr = stolpink_mtrl.UPPLAG.
            RUN put_UI (INPUT "A", "UPPLAG " + STRING(upplagnr)).
            iRad = iRad + 1.
            RUN put_UI (INPUT "A","ENR").
            RUN put_UI (INPUT "B","BENÄMNING").
            RUN put_UI (INPUT "C","ANTAL").            
         END. 
         iRad = iRad + 1.   
         
         RUN put_UI (INPUT "A",stolpink_mtrl.ENR).
         RUN put_UI (INPUT "B",stolpink_mtrl.BENAMNING).
         RUN put_UI (INPUT "C",STRING(stolpink_mtrl.INKANTAL)).    
      
      END.
      iRad = iRad + 1. 
      FOR EACH stolpink_mtrl WHERE stolpink_mtrl.UPPLAG = ? NO-LOCK BY stolpink_mtrl.UPPLAG BY stolpink_mtrl.ENR:
         IF upplagnr NE stolpink_mtrl.UPPLAG THEN DO:
            upplagnr = stolpink_mtrl.UPPLAG.
            RUN put_UI (INPUT "A","Ej kopplat till upplag").
            iRad = iRad + 1.
            RUN put_UI (INPUT "A","ENR").
            RUN put_UI (INPUT "B","BENÄMNING").
            RUN put_UI (INPUT "C","ANTAL").                     
         END. 
         iRad = iRad + 1.   
         
         RUN put_UI (INPUT "A",stolpink_mtrl.ENR).
         RUN put_UI (INPUT "B",stolpink_mtrl.BENAMNING).
         RUN put_UI (INPUT "C",STRING(stolpink_mtrl.INKANTAL)). 
        
      END.
      
   END.   
   IF vad = 1 THEN DO:   
      kommando2 = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
      {SESSIONTEMPDIR.I}
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN kommando2 = webclienttempdir.
      OS-CREATE-DIR VALUE(kommando2) NO-ERROR.
      DEBUGGER:SET-BREAK().
      IF excelnamn NE "" THEN DO:
         IF excelnamn = ".xls" THEN kommando3 = Guru.Konstanter:globanv  + STRING(TIME) + ".xls".
         ELSE kommando3 = excelnamn.
        
      END.   
      ELSE kommando3 = Guru.Konstanter:globanv  + STRING(TIME) + ".xls".   
      kommando2 = kommando2 +  kommando3.
      chExcelApplication:VISIBLE = true.
      chExcelApplication:displayalerts = true.
      RUN sidbrytbredd_UI  (INPUT 1).
      chExcelApplication:VISIBLE = FALSE.
      chExcelApplication:displayalerts = FALSE.
      chWorkbook:SaveAs(kommando2,56,,,,,,,,). 
    /*  RUN sidbrytbredd_UI  (INPUT 1).*/
       RUN slutexceld_UI.
   END.
   ELSE IF vad = 2 THEN DO:
      RUN sidbrytbredd_UI  (INPUT 1).
      RUN slutexcel_UI.  
   END.