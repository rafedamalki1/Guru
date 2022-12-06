/*   Filename: FILINFO2.I*/
  DEFINE VARIABLE globforetag AS CHARACTER NO-UNDO.
  
   tempfilinfo = "Filnamn: " + SUBSTRING(FILE-INFO:FILE-NAME,(R-INDEX(FILE-INFO:FILE-NAME,"\") + 1)) + CHR(10).             
   tempfilinfo = tempfilinfo + "Storlek: " + STRING((FILE-INFO:FILE-SIZE) / 1000) + " kB" + CHR(10).
   tempfilinfo = tempfilinfo + "Senast ändrad: " + STRING(FILE-INFO:FILE-MOD-DATE) + " " +
   STRING(FILE-INFO:FILE-MOD-TIME,"HH:MM:SS") + CHR(10).
   tempfilinfo = tempfilinfo + "Datornamn: " + TRIM(outdatornamn) + CHR(10).
   tempfilinfo = tempfilinfo + "PC-användare: " + TRIM(outanvanv) + CHR(10).
   IF vc = ? THEN tempfilinfo = tempfilinfo + "Progress: " + PROVERSION + CHR(10). 
   ELSE tempfilinfo = tempfilinfo + "Progress: " + vc + CHR(10).    
   IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
      vc = "".
      /*
      RUN WEBVERSION.P (INPUT 1,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT globforetag,OUTPUT Urlsite).
      */
      RUN WEBVERSION.P (INPUT 1,INPUT-OUTPUT vc,INPUT-OUTPUT vcnr,OUTPUT globforetag).
      
      RUN WEBPLACE.P (OUTPUT Ekgnr, OUTPUT Urlsite).
      IF vc = "" THEN.
      ELSE IF vc = ? THEN.
      ELSE DO:
         IF SESSION:TEMP-DIRECTORY MATCHES "*EKGONWEB*" THEN  tempfilinfo = tempfilinfo + "EKG version: " + Ekgnr + CHR(10).
         ELSE tempfilinfo = tempfilinfo + "Guru version: " + vc + CHR(10).
         tempfilinfo = tempfilinfo + "Url: " + Urlsite + CHR(10).
      END. 
   END.
  
   tempfilinfo = tempfilinfo + "Temp: " + SESSION:TEMP-DIRECTORY + CHR(10).
  
   IF Guru.Konstanter:AppSpringSet[6] = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)  THEN DO:
      tempfilinfo = tempfilinfo + "Db:" + Guru.Konstanter:AppSpringSet[12]  + CHR(10).
      tempfilinfo = tempfilinfo + "Appserver:" + Guru.Konstanter:AppSpringSet[13]  + CHR(10).
      
   END.   
   
   
   
   MESSAGE tempfilinfo VIEW-AS ALERT-BOX TITLE "Fil information".  
