/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: C:\DELAD\PRO9S\WX\T1.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.06.08 08:53 ELPAO   
     Modified: 2009.06.08 12:53 ELPAO    
     Modified: 
*/


/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename: C:\DELAD\PRO9\WRK\T.P
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
      Created: 2009.06.05 17:07 ELPAO   
     Modified: 2009.06.08 08:44 ELPAO    
     Modified: 2009.06.08 12:53 ELPAO    
     Modified: 
*/


/*XMLUT.P*/
DEFINE VARIABLE hDoc AS HANDLE.
DEFINE VARIABLE hRoot AS HANDLE.
DEFINE VARIABLE hRow AS HANDLE EXTENT 10.
DEFINE VARIABLE hField AS HANDLE.
DEFINE VARIABLE hText AS HANDLE.
DEFINE VARIABLE hBuf AS HANDLE.
DEFINE VARIABLE hDBFld AS HANDLE.
DEFINE VARIABLE antal AS INTEGER.
DEFINE VARIABLE dynxml AS HANDLE NO-UNDO.
DEFINE VARIABLE okand AS LOGICAL NO-UNDO.
DEFINE VARIABLE hTable AS HANDLE.
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE rootnamn AS CHARACTER NO-UNDO.
DEFINE VARIABLE hurmanga AS INTEGER NO-UNDO.
DEFINE TEMP-TABLE aa NO-UNDO LIKE AONRTAB.
DEFINE TEMP-TABLE PP NO-UNDO LIKE PERSONALTAB.

FOR EACH AONRTAB WHERE AONRTAB.OMRADE = "2" AND 
   AONRTAB.AONRAVDATUM = 01/01/91 NO-LOCK:
   CREATE AA.
   BUFFER-COPY AONRTAB TO AA.
   AA.ANM = "".
     
   FOR EACH PERSONALTAB WHERE PERSONALTAB.personalkod = AONRTAB.ARBANSVARIG NO-LOCK:
      FIND FIRST PP WHERE PP.personalkod = PERSONALTAB.personalkod NO-LOCK NO-ERROR.
      IF NOT AVAILABLE PP THEN DO:
         CREATE PP.
         BUFFER-COPY PERSONALTAB TO PP.   
      END.
      
      
   END.
END.
RUN creatnod_UI (INPUT 2).
RUN xmlutstart_UI (INPUT "start",INPUT "C:\PROTEMP9\test.xml").
RUN pro_UI.
RUN xmlutslut_UI.   
RUN xmlslut_UI.

PROCEDURE creatnod_UI :
   DEFINE INPUT PARAMETER hurmangain AS INTEGER NO-UNDO.
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
PROCEDURE xmlslut_UI :
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

PROCEDURE xmlutstart_UI :
   DEFINE INPUT PARAMETER rootnamnin AS CHARACTER NO-UNDO.
   DEFINE INPUT PARAMETER utfilin AS CHARACTER NO-UNDO.
   IF rootnamnin NE "" THEN do:
      rootnamn = rootnamnin.
      /*SET UP A ROOT NOD*/
      hDoc:CREATE-NODE(hRoot,rootnamn,"ELEMENT").
      hDoc:APPEND-CHILD(hRoot).
   END.
   utfil = utfilin.
END PROCEDURE.

PROCEDURE xmlutslut_UI :
   hDoc:SAVE("file",utfil).     
END PROCEDURE.

PROCEDURE pro_UI :
   DEFINE VARIABLE orgtabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE qh AS HANDLE NO-UNDO.
   DEFINE VARIABLE kommandoquery AS CHARACTER NO-UNDO.
   DEFINE VARIABLE jointabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE dbfalt AS HANDLE NO-UNDO.
   DEFINE VARIABLE VV AS DATE NO-UNDO.
   VV = 01/01/91.
   CREATE WIDGET-POOL "dynTemp" NO-ERROR. 

   ASSIGN
   kommandoquery = "FOR EACH AA  WHERE AA.OMRADE = '2' AND AA.AONRAVDATUM = " + STRING(VV) + " NO-LOCK,".
   kommandoquery = kommandoquery + " EACH PP WHERE " + "PP.personalkod = AA.ARBANSVARIG".
   
   jointabh = TEMP-TABLE AA:DEFAULT-BUFFER-HANDLE.    
   orgtabh = TEMP-TABLE PP:DEFAULT-BUFFER-HANDLE.    
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
      hText:NODE-VALUE = "EE".

      hDoc:CREATE-NODE(hRow[2], orgtabh:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
      hRow[1]:APPEND-CHILD(hRow[2]). 
      hDoc:CREATE-NODE(hText, "", "TEXT").
      hRow[2]:APPEND-CHILD(hText). 
      hText:NODE-VALUE = "RRR".


      
       
      qh:GET-NEXT(NO-LOCK).        
   END.  
   qh:QUERY-CLOSE().   
   DELETE WIDGET-POOL "dynTemp" NO-ERROR.   
END PROCEDURE.


        /*
        qh:QUERY-PREPARE(kommandoquery).   
   qh:QUERY-OPEN().
   qh:GET-FIRST(NO-LOCK).
   DO WHILE orgtabh:AVAILABLE:   
      IF VALID-HANDLE(jointabh) THEN hDoc:CREATE-NODE(hRow, jointabh:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
      ELSE hDoc:CREATE-NODE(hRow, orgtabh:NAME, "ELEMENT").
      hRoot:APPEND-CHILD(hRow).
      hDoc:CREATE-NODE(hText, "", "TEXT").
      hRow:APPEND-CHILD(hText). 
      hText:NODE-VALUE = "EE".

      IF VALID-HANDLE(jointabh) THEN DO:
         /*
         REPEAT antal = 1 TO jointabh:NUM-FIELDS:
                                                  
            dbfalt = jointabh:BUFFER-FIELD(antal).
            IF dbfalt:NAME = "ANM" OR dbfalt:NAME = "ARBUPPG" THEN.
            ELSE DO:
               hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
               hRow:APPEND-CHILD(hField).
               
               hDoc:CREATE-NODE(hText, "", "TEXT").
               
               hField:APPEND-CHILD(hText). 
               hText:NODE-VALUE = STRING(dbfalt:BUFFER-VALUE).
            END.
            
         END.
         */
      END.
      IF VALID-HANDLE(jointabh) THEN DO:
         hRoot:CREATE-NODE(hRow2, orgtabh:NAME, "ELEMENT"). /*CREATE A ROW NODE*/
         hRoot:APPEND-CHILD(hRow2). 
         hRoot:CREATE-NODE(hText, "", "TEXT").
         hRoot:APPEND-CHILD(hText). 
         hText:NODE-VALUE = "RRR".


      END.
      /*
      REPEAT antal = 1 TO orgtabh:NUM-FIELDS:
         dbfalt = orgtabh:BUFFER-FIELD(antal).
         
         hDoc:CREATE-NODE(hField, dbfalt:NAME, "ELEMENT").
         hRow:APPEND-CHILD(hField).
         
         hDoc:CREATE-NODE(hText, "", "TEXT").
         
         hField:APPEND-CHILD(hText).     
         hText:NODE-VALUE = STRING(dbfalt:BUFFER-VALUE).
      END.
      */
      qh:GET-NEXT(NO-LOCK).        
   END. */
