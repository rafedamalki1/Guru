/*UTXMLPROC.I*/

hurmanga = 1.
RUN creatnod_UI.

PROCEDURE sammaniv_UI :
   DEFINE INPUT  PARAMETER insammaniv AS CHARACTER NO-UNDO.
   sammaniv = insammaniv.
END PROCEDURE.
PROCEDURE ejblank_UI :
   DEFINE INPUT PARAMETER inejblank AS LOGICAL NO-UNDO.
   ejblank = inejblank.
END PROCEDURE.
PROCEDURE intest_UI :
   DEFINE INPUT PARAMETER xmltestin AS LOGICAL NO-UNDO.
   xmltest = xmltestin.
END PROCEDURE.
/*tar bort cr ur filen*/ 
PROCEDURE linebort_UI :
   DEFINE INPUT PARAMETER utfilin AS CHARACTER NO-UNDO.
   DEFINE VARIABLE rrakn AS INTEGER NO-UNDO.
   INPUT FROM VALUE(utfilin) NO-ECHO.
   rrakn = 0.
   REPEAT:
      CREATE tidin.   
      rrakn = rrakn + 1.
      tidin.RAD = rrakn.
      /*CHR(11)*/      
      IMPORT DELIMITER "" tidin.TIN.
   END.
   INPUT CLOSE.  
   OUTPUT TO VALUE(utfilin) NO-ECHO.
   FOR EACH tidin:
      PUT UNFORMATTED tidin.TIN.
   END.
   OUTPUT CLOSE.
   EMPTY TEMP-TABLE tidin NO-ERROR. 
END PROCEDURE.

/*byter något i färdig fil*/
PROCEDURE byttxt_UI :
   DEFINE INPUT PARAMETER utfilin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER intxt AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER utxtx AS CHARACTER NO-UNDO.
   DEFINE VARIABLE utfilinq AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.   
   DEFINE VARIABLE words AS CHARACTER NO-UNDO.
   DEFINE VARIABLE rrakn AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE tidin NO-ERROR.
   utfilinq = utfilin + "q".    
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN RETURN.
   ELSE OS-COMMAND SILENT VALUE(kommando) VALUE(utfilin) > VALUE(utfilinq).      
   INPUT FROM VALUE(utfilinq) NO-ECHO.
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850".*/
   rrakn = 0.
   REPEAT:
      IMPORT WORDS.
      /*
      SET words VIEW-AS EDITOR INNER-CHARS 78 INNER-LINES 320 WITH FRAME DDD WIDTH 80.   
      REPEAT:
         IF INDEX(words,'"',1) = 0 THEN LEAVE.
         words = REPLACE(words,'"',' ').
      END.
      */
      rrakn = rrakn + 1.
      CREATE tidin.   
      ASSIGN tidin.TIN = words
      tidin.RAD = rrakn.
   END.
   INPUT CLOSE. 
     
   FOR EACH tidin WHERE NO-LOCK:
      tidin.TIN = REPLACE(tidin.TIN,intxt,utxtx).
   END.
   OUTPUT TO VALUE(utfilin) NO-ECHO.
   FOR EACH tidin:
      IF radbryt = TRUE THEN PUT UNFORMATTED tidin.TIN SKIP.
      ELSE PUT UNFORMATTED tidin.TIN.
      
   END.
   OUTPUT CLOSE.
   OS-DELETE VALUE(utfilinq).
   EMPTY TEMP-TABLE tidin NO-ERROR. 

END PROCEDURE.

/*byer något i färdig fil*/
PROCEDURE byttxttab_UI :
   DEFINE INPUT PARAMETER utfilin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER TABLE FOR bytxmltemp.
   
   DEFINE VARIABLE utfilinq AS CHARACTER NO-UNDO.
   DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.   
   DEFINE VARIABLE words AS CHARACTER NO-UNDO.
   DEFINE VARIABLE rrakn AS INTEGER NO-UNDO.
   EMPTY TEMP-TABLE tidin NO-ERROR.
   utfilinq = utfilin + "q".    
   kommando = SEARCH("quoter.exe").
   IF kommando = ? THEN RETURN.
   ELSE OS-COMMAND SILENT VALUE(kommando) VALUE(utfilin) > VALUE(utfilinq).      
   INPUT FROM VALUE(utfilinq) NO-ECHO.
   /*CONVERT TARGET "iso8859-1" SOURCE "ibm850".*/
   rrakn = 0.
   REPEAT:
      IMPORT WORDS.
      rrakn = rrakn + 1.
      CREATE tidin.   
      ASSIGN tidin.TIN = words
      tidin.RAD = rrakn.
   END.
   INPUT CLOSE. 
   FOR EACH bytxmltemp USE-INDEX ordning WHERE NO-LOCK:    
      FOR EACH tidin WHERE NO-LOCK:
         tidin.TIN = REPLACE(tidin.TIN,bytxmltemp.FRAN,bytxmltemp.TILL).
      END.
   END.   
   OUTPUT TO VALUE(utfilin) NO-ECHO.
   FOR EACH tidin:
      IF radbryt = TRUE THEN PUT UNFORMATTED tidin.TIN SKIP.
      ELSE PUT UNFORMATTED tidin.TIN.
      
   END.
   OUTPUT CLOSE.
   OS-DELETE VALUE(utfilinq).
   EMPTY TEMP-TABLE tidin NO-ERROR. 

END PROCEDURE.

/*start inläsning*/
PROCEDURE xmlinstart_UI :
   DEFINE INPUT PARAMETER TABLE FOR intemp.
   DEFINE INPUT PARAMETER utfilin AS CHARACTER NO-UNDO.
   IF NOT VALID-HANDLE(hDoc) THEN RUN creatnod_UI.
   IF utfilin NE "" THEN DO:
      utfil = utfilin.
      hDoc:LOAD("file", utfil, FALSE).
      hDoc:GET-DOCUMENT-ELEMENT(hRoot).
   END.
   RUN xmlin_UI.
END PROCEDURE.

/*start inläsning*/
PROCEDURE xmlinnibodystart_UI :
   DEFINE INPUT PARAMETER TABLE FOR intemp.
   DEFINE INPUT PARAMETER utfilin AS CHARACTER NO-UNDO.
   IF NOT VALID-HANDLE(hDoc) THEN RUN creatnod_UI.
   IF utfilin NE "" THEN DO:
      utfil = utfilin.
      hDoc:LOAD("file", utfil, FALSE).
      hDoc:GET-DOCUMENT-ELEMENT(hRoot).
   END.
   RUN xmlinnobody_UI.
END PROCEDURE.
/*läser in värden*/
PROCEDURE xmlin_UI :
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE j AS INTEGER NO-UNDO.
   DEFINE VARIABLE faltkoll AS LOGICAL NO-UNDO.
   DEFINE VARIABLE postraknare AS INTEGER NO-UNDO.
     
   i = i.
   REPEAT i = 1 TO hRoot:NUM-CHILDREN:
      hRoot:GET-CHILD(hTable,i).
      IF hTable:NUM-CHILDREN < 1 THEN NEXT.
      FOR EACH intemp WHERE intemp.NODTAB = hTable:NAME NO-LOCK:
         IF hTable:NAME = intemp.NODTAB THEN DO:
            intemp.ORGTH:BUFFER-CREATE().      
            postraknare = postraknare + 1.
            REPEAT j = 1 TO hTable:NUM-CHILDREN:
               hTable:GET-CHILD(hField,j).
               IF hField:NUM-CHILDREN < 1 THEN NEXT.
               /*SKIP ANY NULL VALUE*/
               dbfalt = intemp.ORGTH:BUFFER-FIELD(hField:NAME) NO-ERROR.
               IF dbfalt NE ? THEN DO:
                  hField:GET-CHILD(hText,1).
                  /*GET THE TEXT VALUE OF THE FIELD*/
                  dbfalt:BUFFER-VALUE = hText:NODE-VALUE.      
               END.
            END.
            IF xmltest = TRUE AND postraknare > 19 THEN DO:
               xmltest = FALSE.
               RETURN.
            END.
         END.
      END.
   END.     
END PROCEDURE.
/*läser in värden*/
PROCEDURE xmlinnobody_UI :
   DEFINE VARIABLE i AS INTEGER NO-UNDO.
   DEFINE VARIABLE j AS INTEGER NO-UNDO.
   DEFINE VARIABLE faltkoll AS LOGICAL NO-UNDO.
   DEFINE VARIABLE postraknare AS INTEGER NO-UNDO.     
      FOR EACH intemp WHERE intemp.NODTAB = hRoot:NAME NO-LOCK:
         IF hRoot:NAME = intemp.NODTAB THEN DO:
            intemp.ORGTH:BUFFER-CREATE().      
            postraknare = postraknare + 1.
            REPEAT j = 1 TO hRoot:NUM-CHILDREN:
               hRoot:GET-CHILD(hField,j).
               IF hField:NUM-CHILDREN < 1 THEN NEXT.
               /*SKIP ANY NULL VALUE*/
               IF hField:NAME = "MESSAGE" THEN dbfalt = intemp.ORGTH:BUFFER-FIELD("INMESSAGE") NO-ERROR.
               ELSE dbfalt = intemp.ORGTH:BUFFER-FIELD(hField:NAME) NO-ERROR.
               IF dbfalt NE ? THEN DO:
                  hField:GET-CHILD(hText,1).
                  /*GET THE TEXT VALUE OF THE FIELD*/
                  dbfalt:BUFFER-VALUE = hText:NODE-VALUE.      
               END.
            END.           
         END.
      END. 
END PROCEDURE.
/*sparar poster dynamiskt*/
PROCEDURE spara_UI :
   IF Guru.Konstanter:appcon THEN DO:
      RUN DYNSPARATEMP.P PERSISTENT SET laddaproch ON Guru.Konstanter:apphand TRANSACTION DISTINCT.      
   END.
   ELSE DO:
      RUN DYNSPARATEMP.P PERSISTENT SET laddaproch.      
   END.
   FOR EACH intemp:
      RUN sparatemp_UI IN laddaproch (INPUT TABLE-HANDLE intemp.TORGTH, INPUT intemp.NODTAB).  
   END.
   IF VALID-HANDLE(laddaproch) THEN DELETE PROCEDURE laddaproch NO-ERROR.   
END PROCEDURE.

/*om du vill styra antalet nivåer själv (ange en mer än vad du vill ha)*/
PROCEDURE xmlutstartniv_UI :
   DEFINE INPUT PARAMETER rootnamnin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER utfilin AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER textut AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER hrotnamn AS CHARACTER NO-UNDO.
   IF rootnamnin NE "" THEN do:
      rootnamn = rootnamnin.
      /*SET UP A ROOT NOD*/
      hDoc:CREATE-NODE(hRoot,rootnamn,"ELEMENT").
      hDoc:APPEND-CHILD(hRoot).
      IF textut NE "" THEN hRoot:SET-ATTRIBUTE(hrotnamn, textut).
      /*
      hRoot:SET-ATTRIBUTE("xmlns:ns0", "http://Elektroskandia.ESXML.ProjectOrder").
      */
   END.
   utfil = utfilin.
END PROCEDURE.

/*normal start*/
PROCEDURE xmlutstart_UI :
   DEFINE INPUT PARAMETER orgtabhin AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER temptabin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER rootnamnin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER nodervar AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER kommandoquerystartin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER utfilin AS CHARACTER NO-UNDO.
   IF rootnamnin NE "" THEN do:
      rootnamn = rootnamnin.
      /*SET UP A ROOT NOD*/
      hDoc:CREATE-NODE(hRoot,rootnamn,"ELEMENT").
      hDoc:APPEND-CHILD(hRoot).
   END.
   IF utfilin NE "" THEN utfil = utfilin.
   ASSIGN
   orgtabh = orgtabhin
   temptab = temptabin
   kommandoquerystart = kommandoquerystartin + " " + temptabin.
   RUN xmlut_UI (INPUT nodervar,INPUT kommandoquerystart).
END PROCEDURE.

/*en tabell åt gången kommer ut.*/
PROCEDURE xmlut_UI:
   DEFINE INPUT PARAMETER tabvar AS CHARACTER NO-UNDO.   
   DEFINE INPUT PARAMETER kommandoquery AS CHARACTER NO-UNDO.
   CREATE WIDGET-POOL "dynTemp" NO-ERROR.   
   /*
   CREATE BUFFER orgtabh FOR TABLE tabvar IN WIDGET-POOL "dynTemp".
   */
   CREATE QUERY qh IN WIDGET-POOL "dynTemp".
   qh:SET-BUFFERS(orgtabh).
   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:              
      hDoc:CREATE-NODE(hRow[1], tabvar, "ELEMENT"). /*CREATE A ROW NODE*/
      hRoot:APPEND-CHILD(hRow[1]).                   /*PUT THE ROW IN THE TREE*/
      /*
      hRow[1]:SET-ATTRIBUTE(intemp.UNIK1,"").
      */
      /*ADD THE OTHER FIELDS AS TAGS IN THE XML*/
      REPEAT antal = 1 TO orgtabh:NUM-FIELDS:
         dbfalt = orgtabh:BUFFER-FIELD(antal).
         /*
         IF dbfalt:NAME = SUBSTRING(intemp.UNIK2,INDEX(intemp.UNIK2,".") + 1) THEN NEXT.
         IF dbfalt:NAME = "AONR" OR 
         */
         IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
         ELSE DO: 
            /*CREATE A TAG WITH THE FIELD NAME*/
            hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
            /*PUT THE NEW FIELD AS NEXT CHILD OF ROW*/
            hRow[1]:APPEND-CHILD(hField).
            /*ADD A NODE TO HOLD FIELD VALUE*/
            hDoc:CREATE-NODE(hText, "", "TEXT").
            /*ATTACH THE TEXT TO THE FIELD*/
            hField:APPEND-CHILD(hText).     
            RUN vardut_UI.
         END.   
         
      END.
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.   
END PROCEDURE. 

/*  första posten ut huvnivut2_UI alla joinadec poster sedan slut första posten max 3 join*/
PROCEDURE inladd_UI :
   DEFINE INPUT PARAMETER inkommandoquery2     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inkommandoquery3     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inkommandoquery11     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inkommandoquery12     AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inkommandoqueryhelp2 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER inkommandoqueryhelp3 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER jointabh2 AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER jointabh3 AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER jointabh11 AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER jointabh12 AS HANDLE NO-UNDO.
   ASSIGN
   kommandoquery2       = inkommandoquery2    
   kommandoquery3       = inkommandoquery3    
   kommandoquery11       = inkommandoquery11    
   kommandoquery12       = inkommandoquery12    
   kommandoqueryhelp2   = inkommandoqueryhelp2
   kommandoqueryhelp3   = inkommandoqueryhelp3
   tabniv2 = jointabh2
   tabniv3 = jointabh3
   tabniv11 = jointabh11
   tabniv12 = jointabh12.
END PROCEDURE.

/*läser ut join 1 */
PROCEDURE huvnivut_UI:
   DEFINE INPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER orgtabh AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   CREATE WIDGET-POOL "dynTemp" NO-ERROR.   
   CREATE QUERY qh IN WIDGET-POOL "dynTemp".
   qh:SET-BUFFERS(orgtabh).
   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:        
      kommandoqueryhelp11 = "".
      kommandoqueryhelp12 = "".
      hDoc:CREATE-NODE(hRow[nivar], orgtabh:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
      IF nivar = 1 THEN hRoot:APPEND-CHILD(hRow[nivar]).                   /*PUT THE ROW IN THE TREE*/
      ELSE hRow[nivar - 1]:APPEND-CHILD(hRow[nivar]).  
      REPEAT antal = 1 TO orgtabh:NUM-FIELDS:
         dbfalt = orgtabh:BUFFER-FIELD(antal).
         IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
         ELSE DO:
            hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
            hRow[nivar]:APPEND-CHILD(hField).
            hDoc:CREATE-NODE(hText, "", "TEXT").
            hField:APPEND-CHILD(hText).     
            RUN vardut_UI.
            IF dbfalt:NAME = kommandoqueryhelp2 THEN DO: 
               kommandoqueryhelp[nivar + 1] = kommandoquery2 + STRING(dbfalt:BUFFER-VALUE) + "'" + " NO-LOCK".
               kommandoqueryhelp11 = kommandoquery11 + STRING(dbfalt:BUFFER-VALUE) + "'" + " NO-LOCK".
               kommandoqueryhelp12 = kommandoquery12 + STRING(dbfalt:BUFFER-VALUE) + "'" + " NO-LOCK".
            END.
         END.   
      END.      
      IF VALID-HANDLE(tabniv11) THEN RUN nivute_UI  (INPUT( nivar + 1),INPUT tabniv11,INPUT kommandoqueryhelp11).                     
      IF VALID-HANDLE(tabniv12) THEN RUN nivute_UI  (INPUT( nivar + 1),INPUT tabniv12,INPUT kommandoqueryhelp12).                     
      RUN huvnivut2_UI (INPUT nivar + 1).
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   IF nivar = 1 THEN DELETE WIDGET-POOL "dynTemp" NO-ERROR.   
END PROCEDURE.
/*läser ut join 2 */
PROCEDURE huvnivut2_UI:
   DEFINE INPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   IF kommandoqueryhelp[nivar] = "" THEN RETURN.
   CREATE WIDGET-POOL "dynTemp2" NO-ERROR.   
   /*
   CREATE BUFFER orgtabh FOR TABLE tabvar IN WIDGET-POOL "dynTemp".
   */
   kommandoqueryhelp[nivar + 1] = "".
   CREATE QUERY qh IN WIDGET-POOL "dynTemp2".
   qh:SET-BUFFERS(tabniv2).
   qh:QUERY-PREPARE(kommandoqueryhelp[nivar]).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE tabniv2:AVAILABLE:        
      hDoc:CREATE-NODE(hRow[nivar], tabniv2:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
      hRow[nivar - 1]:APPEND-CHILD(hRow[nivar]).                   /*PUT THE ROW IN THE TREE*/
     
      REPEAT antal = 1 TO tabniv2:NUM-FIELDS:
         dbfalt = tabniv2:BUFFER-FIELD(antal).
         IF dbfalt:NAME = kommandoqueryhelp2 THEN DO: 
            kommandoqueryhelp[nivar + 1] = kommandoquery3 + STRING(dbfalt:BUFFER-VALUE) + "'".
         END.
         ELSE IF dbfalt:NAME = kommandoqueryhelp3 THEN DO: 
            kommandoqueryhelp[nivar + 1] = kommandoqueryhelp[nivar + 1] + " AND " + kommandoqueryhelp3 + " = '" + STRING(dbfalt:BUFFER-VALUE) + "'" + " NO-LOCK ".
            IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
            ELSE DO:
               hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
               hRow[nivar]:APPEND-CHILD(hField).
               hDoc:CREATE-NODE(hText, "", "TEXT").
               hField:APPEND-CHILD(hText).     
               RUN vardut_UI.
            END.    
         END.
         ELSE DO:
            IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
            ELSE DO:
               hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
               hRow[nivar]:APPEND-CHILD(hField).
               hDoc:CREATE-NODE(hText, "", "TEXT").
               hField:APPEND-CHILD(hText).     
               RUN vardut_UI.
            END.    
         END.
      END.
      RUN huvnivut3_UI (INPUT nivar + 1).         
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp2" NO-ERROR.   
END PROCEDURE.
/*läser ut join 3 */
PROCEDURE huvnivut3_UI:
   DEFINE INPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   IF kommandoqueryhelp[nivar] = "" THEN RETURN.
   CREATE WIDGET-POOL "dynTemp3" NO-ERROR.   
   /*
   CREATE BUFFER orgtabh FOR TABLE tabvar IN WIDGET-POOL "dynTemp".
   */
   
   CREATE QUERY qh IN WIDGET-POOL "dynTemp3".
   qh:SET-BUFFERS(tabniv3).
   qh:QUERY-PREPARE(kommandoqueryhelp[nivar]).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE tabniv3:AVAILABLE:        
      hDoc:CREATE-NODE(hRow[nivar], tabniv3:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
      hRow[nivar - 1]:APPEND-CHILD(hRow[nivar]).                   /*PUT THE ROW IN THE TREE*/
     
      REPEAT antal = 1 TO tabniv3:NUM-FIELDS:
         dbfalt = tabniv3:BUFFER-FIELD(antal).
         IF dbfalt:NAME = kommandoqueryhelp2 THEN DO: 
         END.
         ELSE IF dbfalt:NAME = kommandoqueryhelp3 THEN DO: 
         END.
         ELSE IF dbfalt:NAME = "Comment" THEN DO:
            IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
            ELSE DO:
               RUN comment_UI (INPUT-OUTPUT nivar).              
            END.     
         END.    
         ELSE DO:
            IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
            ELSE DO:
               hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
               hRow[nivar]:APPEND-CHILD(hField).
               hDoc:CREATE-NODE(hText, "", "TEXT").
               hField:APPEND-CHILD(hText).     
               RUN vardut_UI.
            END. 
         END.
      END.
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp3" NO-ERROR.   
END PROCEDURE.
/*rätt nivå kommer ut*/
 PROCEDURE subnivkom_UI :
    DEFINE INPUT  PARAMETER subnivkomin AS LOGICAL NO-UNDO.
    subnivkom = subnivkomin.
 END PROCEDURE.


PROCEDURE nivut_UI:
   DEFINE INPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER orgtabh AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER kommandoquery AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER hjnamn AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER ejnivtab AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER extraradut AS CHARACTER NO-UNDO.
   DEFINE VARIABLE postlg AS LOGICAL NO-UNDO.
   DEFINE VARIABLE postraknare AS INTEGER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   CREATE WIDGET-POOL "dynTemp" NO-ERROR.   
   IF hjnamn = "POSTRAKNARE" THEN postlg = TRUE. 
   CREATE QUERY qh IN WIDGET-POOL "dynTemp".
   qh:SET-BUFFERS(orgtabh).
   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:  
      IF postlg = TRUE THEN DO:
         postraknare = postraknare + 1.
         hjnamn = "Postnumber" + STRING(postraknare).
      END.           
      hDoc:CREATE-NODE(hRow[nivar], hjnamn, "ELEMENT"). /*CREATE A ROW NODE dvs tabel namnet oftast*/       
      IF nivar = 1  THEN hRoot:APPEND-CHILD(hRow[nivar]).                   /*PUT THE ROW IN THE TREE*/
      ELSE hRow[nivar - 1]:APPEND-CHILD(hRow[nivar]).
      RUN setattr_UI (INPUT hjnamn,INPUT hRow[nivar]).
            
      REPEAT antal = 1 TO orgtabh:NUM-FIELDS:
         dbfalt = orgtabh:BUFFER-FIELD(antal).
         /*
         IF orgtabh:NAME = "tempheader" THEN .
         ELSE IF orgtabh:NAME = "ProjectOrder" THEN .
         */
         /*dessa tabeller ska bara ha en nivå inga värden*/
         IF LOOKUP(orgtabh:NAME,ejnivtab) > 0 THEN DO:
            
         END.    
         ELSE DO:   
            
            /*
            IF dbfalt:NAME =  "ProjectName" THEN exdbfalt = STRING(dbfalt:BUFFER-VALUE).
            */
            IF  LOOKUP(dbfalt:NAME,extraradut) > 0 THEN DO:
               /*fält vars värde ska ut vid ett senare tillfälle*/
               exdbfalt = STRING(dbfalt:BUFFER-VALUE).
            END.   
            ELSE DO:
               /*värdet läggs ut*/
               IF subnivkom = TRUE AND (dbfalt:NAME = "Comment" OR dbfalt:NAME = "OrderText") THEN DO:
                  IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
                  ELSE DO:                   
                     RUN comment_UI (INPUT-OUTPUT nivar).                       
                  END.     
               END.
               ELSE DO:   
                  IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
                  ELSE DO:
                     hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
                     hRow[nivar]:APPEND-CHILD(hField).
                     hDoc:CREATE-NODE(hText, "", "TEXT").
                     hField:APPEND-CHILD(hText).
                     RUN setattr_UI (INPUT dbfalt:NAME,INPUT hField).     
                     RUN vardut_UI.
                  END.
               END.      
            END.
         END.
         
      END.
      
      qh:GET-NEXT(NO-LOCK).        
   END.  
   totpostraknare = postraknare.
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.   
END PROCEDURE.

PROCEDURE PostRaknareUt_UI :
   DEFINE OUTPUT PARAMETER pr AS INTEGER NO-UNDO.
   pr = totpostraknare.
END PROCEDURE.

PROCEDURE comment_UI :
   DEFINE INPUT-OUTPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE VARIABLE postut AS CHARACTER NO-UNDO.
   DEFINE VARIABLE varchrsok AS INTEGER NO-UNDO.
   DEFINE VARIABLE vartraff AS INTEGER NO-UNDO.
   DEFINE VARIABLE maxtraff AS INTEGER NO-UNDO.
   DEFINE VARIABLE varchr AS INTEGER NO-UNDO EXTENT 9.
    /*elektroskandia order har comment rader ordertext projorder har det inte                      */                  
   
   
   IF dbfalt:NAME = "Comment" THEN DO:
     
      postut =  dbfalt:BUFFER-VALUE.
      IF LENGTH(dbfalt:BUFFER-VALUE) > 0 THEN DO: 
         IF SUBSTRING(dbfalt:BUFFER-VALUE,LENGTH(dbfalt:BUFFER-VALUE),1) = CHR(10) THEN DO:
            dbfalt:BUFFER-VALUE = "".
            dbfalt:BUFFER-VALUE = SUBSTRING(postut,1,LENGTH(postut)- 1).
         END.     
      END.
      postut = "".
      hDoc:CREATE-NODE(hField,"RowText", "ELEMENT").
      hRow[nivar]:APPEND-CHILD(hField).
      REPEAT:
         vartraff = vartraff + 1.
         varchrsok = varchrsok + 1.
         varchrsok = INDEX(STRING(dbfalt:BUFFER-VALUE),CHR(10),varchrsok).
         IF varchrsok = 0 THEN LEAVE.
         varchr[vartraff] = varchrsok.
         IF vartraff = 9 THEN LEAVE.
      END.  
      maxtraff = vartraff.
      vartraff = 1.
      IF varchr[vartraff] = 0 THEN DO:
         hDoc:CREATE-NODE(hRow[nivar + 1], dbfalt:NAME, "ELEMENT").
         hField:APPEND-CHILD(hRow[nivar + 1]).
         hDoc:CREATE-NODE(hText, "", "TEXT").
         hRow[nivar + 1]:APPEND-CHILD(hText).     
         RUN vardut_UI.
      END.
      ELSE DO:    
         vartraff = 0.
         REPEAT:
            vartraff = vartraff + 1.
            IF vartraff = 1 THEN postut = SUBSTRING(STRING(dbfalt:BUFFER-VALUE),1,varchr[vartraff]).
            ELSE IF vartraff = 9 THEN postut = SUBSTRING(STRING(dbfalt:BUFFER-VALUE),varchr[vartraff - 1]).
            ELSE IF vartraff = maxtraff THEN postut = SUBSTRING(STRING(dbfalt:BUFFER-VALUE),varchr[vartraff - 1]).
            ELSE DO: 
               postut = SUBSTRING(STRING(dbfalt:BUFFER-VALUE),varchr[vartraff - 1],varchr[vartraff] - varchr[vartraff - 1]).
            END.
           /*
            ELSE IF varchr[vartraff + 1] = 0 THEN DO:
               postut = SUBSTRING(STRING(dbfalt:BUFFER-VALUE),varchr[vartraff - 1]).
            END.
            ELSE DO: 
               postut = SUBSTRING(STRING(dbfalt:BUFFER-VALUE),varchr[vartraff - 1],varchr[vartraff] - varchr[vartraff - 1]).
            END.
            */
            IF ejblank = TRUE AND postut = ""  OR postut = CHR(10) THEN.
            ELSE DO: 
                hDoc:CREATE-NODE(hRow[nivar + 1], dbfalt:NAME, "ELEMENT").
                hField:APPEND-CHILD(hRow[nivar + 1]).
                hDoc:CREATE-NODE(hText, "", "TEXT").
                hRow[nivar + 1]:APPEND-CHILD(hText).
                IF radbryt = FALSE THEN hText:NODE-VALUE = STRING(postut).
                ELSE hText:NODE-VALUE = STRING(postut + CHR(10)).
            END.     
            IF vartraff = 9 THEN LEAVE.
            IF varchr[vartraff + 1] = 0 THEN DO:
               postut = SUBSTRING(STRING(dbfalt:BUFFER-VALUE),varchr[vartraff] + 1).
               IF postut = "" THEN LEAVE.
               hDoc:CREATE-NODE(hRow[nivar + 1], dbfalt:NAME, "ELEMENT").
               hField:APPEND-CHILD(hRow[nivar + 1]).
               hDoc:CREATE-NODE(hText, "", "TEXT").
               hRow[nivar + 1]:APPEND-CHILD(hText).
               IF radbryt = FALSE THEN hText:NODE-VALUE = STRING(postut).
               ELSE hText:NODE-VALUE = STRING(postut + CHR(10)).
               LEAVE.
            END.    
         END.
      END.   
   END.
   ELSE IF dbfalt:NAME = "OrderText" THEN DO:
      hDoc:CREATE-NODE(hField,dbfalt:NAME, "ELEMENT").
      hRow[nivar]:APPEND-CHILD(hField).
      hDoc:CREATE-NODE(hRow[nivar + 1], "Comment", "ELEMENT").
      hField:APPEND-CHILD(hRow[nivar + 1]).
      hDoc:CREATE-NODE(hText, "", "TEXT").
      hRow[nivar + 1]:APPEND-CHILD(hText).     
      RUN vardut_UI.
   END. 
END PROCEDURE.

PROCEDURE setattr_UI :
   DEFINE INPUT PARAMETER hjnamn AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER rnamn AS HANDLE NO-UNDO. 
   FOR EACH attrtemp WHERE attrtemp.TABNAMN = hjnamn NO-LOCK BY attrtemp.ORDNING:       
      rnamn:SET-ATTRIBUTE(attrtemp.NAMN,attrtemp.TTEXT).         
   END.
END PROCEDURE.
PROCEDURE createattr_UI :
   DEFINE INPUT PARAMETER hjnamn AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER namnvar AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER txtvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE raknare AS INTEGER NO-UNDO.
   FIND LAST attrtemp WHERE attrtemp.TABNAMN = hjnamn NO-LOCK NO-ERROR.
   IF AVAILABLE attrtemp THEN raknare = attrtemp.ORDNING + 1.
   ELSE raknare = 1.
   
   CREATE attrtemp. 
   ASSIGN 
   attrtemp.ORDNING = raknare 
   attrtemp.TABNAMN = hjnamn
   attrtemp.NAMN = namnvar
   attrtemp.TTEXT = txtvar.   
END PROCEDURE.
/*rätt nivå kommer ut bara tidutar*/
PROCEDURE nivtidut_UI:
   DEFINE INPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER orgtabh AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER uttabh AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER kommandoquery AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER hjnamn AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qth AS HANDLE NO-UNDO.
   CREATE WIDGET-POOL "dynTemp" NO-ERROR.   
   CREATE QUERY qh IN WIDGET-POOL "dynTemp".
   CREATE QUERY qth IN WIDGET-POOL "dynTemp".
   qth:SET-BUFFERS(uttabh).
   qth:QUERY-PREPARE("FOR EACH tidut").   
   qth:QUERY-OPEN().
   
   qh:SET-BUFFERS(orgtabh).
   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:           
      hDoc:CREATE-NODE(hRow[nivar], hjnamn, "ELEMENT"). /*CREATE A ROW NODE dvs tabel namnet oftast*/
      IF nivar = 1  THEN hRoot:APPEND-CHILD(hRow[nivar]).                   /*PUT THE ROW IN THE TREE*/
      ELSE hRow[nivar - 1]:APPEND-CHILD(hRow[nivar]).  
      qth:GET-FIRST(NO-LOCK).      
      DO WHILE uttabh:AVAILABLE:  
         dbfalt = uttabh:BUFFER-FIELD(1).
         hDoc:CREATE-NODE(hText, "", "TEXT").
         hRow[nivar]:APPEND-CHILD(hText).   
         RUN vardut_UI.  
         qth:GET-NEXT(NO-LOCK).         
      END.
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().
   qth:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.   
END PROCEDURE.


PROCEDURE exdbfaltsatt_UI :
   DEFINE INPUT  PARAMETER exdbfaltin AS CHARACTER NO-UNDO.
   exdbfalt = exdbfaltin.
END PROCEDURE.
/*bara en extra rad*/ 
PROCEDURE extraut_UI :
   DEFINE INPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varut AS CHARACTER NO-UNDO.
   IF exdbfalt NE "" THEN DO:
      hDoc:CREATE-NODE(hField, varut, "ELEMENT").
      hRow[nivar]:APPEND-CHILD(hField).
      hDoc:CREATE-NODE(hText, "", "TEXT").
      hField:APPEND-CHILD(hText).     
      hText:NODE-VALUE = exdbfalt.
   END.
END PROCEDURE.

/*PROCEDURE extrastniv_UI :
   DEFINE INPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER varut AS CHARACTER NO-UNDO.   
   hDoc:CREATE-NODE(hField, varut, "ELEMENT").
   hRow[nivar]:APPEND-CHILD(hField).      
   
END PROCEDURE.*/
/*under nivå data*/
PROCEDURE nivute_UI:
   DEFINE INPUT PARAMETER nivar AS INTEGER NO-UNDO.
   DEFINE INPUT PARAMETER orgtabh AS HANDLE NO-UNDO. 
   DEFINE INPUT PARAMETER kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   CREATE WIDGET-POOL "dynTempe" NO-ERROR.   
   
   CREATE QUERY qh IN WIDGET-POOL "dynTempe".
   qh:SET-BUFFERS(orgtabh).
   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:              
      hDoc:CREATE-NODE(hRow[nivar], orgtabh:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
      IF nivar = 1 THEN hRoot:APPEND-CHILD(hRow[nivar]).                   /*PUT THE ROW IN THE TREE*/
      ELSE hRow[nivar - 1]:APPEND-CHILD(hRow[nivar]).  
      REPEAT antal = 1 TO orgtabh:NUM-FIELDS:         
         dbfalt = orgtabh:BUFFER-FIELD(antal).
         IF dbfalt:NAME = kommandoqueryhelp2 THEN.
         ELSE DO:  
            IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
            ELSE DO:                   
               hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
               hRow[nivar]:APPEND-CHILD(hField).
               hDoc:CREATE-NODE(hText, "", "TEXT").
               hField:APPEND-CHILD(hText).     
               RUN vardut_UI.
            END.   
         END.
      END.      
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTempe" NO-ERROR.   
END PROCEDURE.
PROCEDURE radbryt_UI :
   DEFINE INPUT  PARAMETER radbrytin AS LOGICAL NO-UNDO.
   radbryt = radbrytin.
END PROCEDURE.
/*själva datat ut*/
PROCEDURE vardut_UI :
   DEFINE VARIABLE blobvar AS MEMPTR NO-UNDO.
   IF STRING(dbfalt:BUFFER-VALUE) NE ? THEN DO:
      IF dbfalt:DATA-TYPE = "BLOB" THEN DO:
         blobvar = dbfalt:BUFFER-VALUE.
         IF radbryt = FALSE THEN hText:NODE-VALUE = STRING(blobvar).
         ELSE hText:NODE-VALUE = STRING(blobvar) + CHR(10).
         
      END.
      ELSE DO:   
         IF radbryt = FALSE THEN hText:NODE-VALUE = STRING(dbfalt:BUFFER-VALUE).
         ELSE hText:NODE-VALUE = STRING(dbfalt:BUFFER-VALUE + CHR(10)).
      END.   
   END.      
   ELSE DO: 
      /*SKA OKÄNDA VÄRDEN VISA  I FILEN*/      
      IF okand = TRUE THEN DO:
         IF radbryt = FALSE THEN  hText:NODE-VALUE = "?".
         ELSE hText:NODE-VALUE = "?" + STRING(CHR(10)).
      END.    
   END.
END PROCEDURE.
/*spara fil*/
PROCEDURE xmlutslut_UI :
   hDoc:SAVE("file",utfil).     
END PROCEDURE.

/*rensa*/
PROCEDURE xmlslut_UI :
   DELETE OBJECT hDoc NO-ERROR.
  
   DELETE OBJECT hRoot NO-ERROR.
   DELETE OBJECT hRow[1] NO-ERROR.
   DELETE OBJECT hField NO-ERROR.
   DELETE OBJECT hText NO-ERROR.
   DELETE OBJECT hTable NO-ERROR.
   
END PROCEDURE.
/*roootnod start nod = 1*/
PROCEDURE creatnod_UI :
   CREATE X-DOCUMENT hDoc.    
   CREATE X-NODEREF hRoot.
   CREATE X-NODEREF hRow[1].
   CREATE X-NODEREF hField.
   CREATE X-NODEREF hText.
   CREATE X-NODEREF hTable.
   
END PROCEDURE.
PROCEDURE encod_UI :
   DEFINE INPUT  PARAMETER enc AS CHARACTER NO-UNDO.
   hDoc:ENCODING = enc.
END PROCEDURE.

/*roootnod start + ett antal nivåer*/
PROCEDURE creatnodniv_UI :
   DEFINE INPUT PARAMETER hurmangain AS INTEGER NO-UNDO.
   RUN xmlslut_UI.
   hurmanga = hurmangain.
   CREATE X-DOCUMENT hDoc.
   CREATE X-NODEREF hRoot.
   IF hurmanga > 0 THEN DO:
      REPEAT antal = 1 TO hurmanga:
         CREATE X-NODEREF hRow[antal].
      END.
   END.
   CREATE X-NODEREF hField.
   CREATE X-NODEREF hText.
   CREATE X-NODEREF hTable.
END PROCEDURE.
/*rensa alla nivåer*/
PROCEDURE xmlslutniv_UI :
   DELETE OBJECT hDoc NO-ERROR.
   DELETE OBJECT hRoot NO-ERROR.
   IF hurmanga > 0 THEN DO:
      REPEAT antal = 1 TO hurmanga:
         DELETE OBJECT hRow[antal].
      END.
   END.
   DELETE OBJECT hField NO-ERROR.
   DELETE OBJECT hText NO-ERROR.
   DELETE OBJECT hTable NO-ERROR.
END PROCEDURE.

PROCEDURE handspecial_UI :
   DEFINE INPUT PARAMETER valaonr2 AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER valomrade2 AS CHARACTER NO-UNDO.

END PROCEDURE.

/*join tab  första tab i for each orgtab är andra tab
  andra tab finns bara engång bör ej användas.

*/
/*skapar en undernivå med data*/
PROCEDURE tvanivut_UI :
   DEFINE INPUT PARAMETER orgtabh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER jointabh AS HANDLE NO-UNDO.
   DEFINE INPUT PARAMETER kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE dbfalt AS HANDLE NO-UNDO.
   CREATE WIDGET-POOL "dynTemp" NO-ERROR. 
   /*SET UP A ROOT NOD*/
   CREATE QUERY qh IN WIDGET-POOL "dynTemp".
   IF VALID-HANDLE(jointabh) THEN DO:
      qh:SET-BUFFERS(jointabh,orgtabh).
   END.
   ELSE DO:
      qh:SET-BUFFERS(orgtabh).
   END.

   qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:   
      hDoc:CREATE-NODE(hRow[1], jointabh:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
      hRoot:APPEND-CHILD(hRow[1]).
      hDoc:CREATE-NODE(hText, "", "TEXT").
      hRow[1]:APPEND-CHILD(hText). 
      /*hText:NODE-VALUE = "EE". */

      IF VALID-HANDLE(jointabh) THEN DO:         
         REPEAT antal = 1 TO jointabh:NUM-FIELDS:  
            IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
            ELSE DO:                                                
               dbfalt = jointabh:BUFFER-FIELD(antal).
               hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
               hRow[1]:APPEND-CHILD(hField).               
               hDoc:CREATE-NODE(hText, "", "TEXT").               
               hField:APPEND-CHILD(hText). 
               RUN vardut_UI.
            END.   
         END.         
      END.

      hDoc:CREATE-NODE(hRow[2], orgtabh:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
      hRow[1]:APPEND-CHILD(hRow[2]). 
      hDoc:CREATE-NODE(hText, "", "TEXT").
      hRow[2]:APPEND-CHILD(hText). 
      /*hText:NODE-VALUE = "RRR".*/

      REPEAT antal = 1 TO orgtabh:NUM-FIELDS:
         IF ejblank = TRUE AND STRING(dbfalt:BUFFER-VALUE) = "" THEN.
         ELSE DO:
            dbfalt = orgtabh:BUFFER-FIELD(antal).         
            hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
            hRow[2]:APPEND-CHILD(hField).         
            hDoc:CREATE-NODE(hText, "", "TEXT").     
            hField:APPEND-CHILD(hText). 
            RUN vardut_UI.
         END.   
      END.          
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.   
END PROCEDURE.
