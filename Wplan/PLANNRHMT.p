/*PLANNRHMT.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{OMRTEMPW.I}
{PLANNRTEMP.I}
{DIRDEF.I}
DEFINE INPUT PARAMETER vart AS INTEGER NO-UNDO.
DEFINE INPUT PARAMETER TABLE FOR uppvaltemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR valplantemp.
DEFINE INPUT-OUTPUT PARAMETER TABLE FOR plannrtemp.

DEFINE VARIABLE musz AS LOGICAL NO-UNDO. 
DEFINE VARIABLE valbestomr AS CHARACTER NO-UNDO. 
DEFINE VARIABLE valdatum AS DATE NO-UNDO. 
DEFINE BUFFER ormbuff FOR OMRADETAB.

{DYNHMT.I}
    
valdatum = 01/01/1991.
IF vart = 1 THEN DO:
   FIND FIRST FORETAG USE-INDEX FORETAG NO-LOCK NO-ERROR. 
   EMPTY TEMP-TABLE plannrtemp NO-ERROR.    
   FIND FIRST uppvaltemp NO-ERROR.
   IF uppvaltemp.OMRADE NE "ALLA" THEN DO:
      FIND FIRST OMRADETAB WHERE OMRADETAB.OMRADE = uppvaltemp.OMRADE NO-LOCK NO-ERROR. 
   END.
   IF uppvaltemp.BESTID NE "ALLA" THEN DO:
      FIND FIRST BESTTAB WHERE BESTTAB.BESTID = uppvaltemp.BESTID NO-LOCK NO-ERROR.    
      IF NOT AVAILABLE BESTTAB THEN DO:
         FIND FIRST ormbuff WHERE ormbuff.OMRADE = uppvaltemp.BESTID NO-LOCK NO-ERROR.
         IF AVAILABLE ormbuff THEN DO:
            ASSIGN valbestomr = ormbuff.OMRADE.
         END.
      END.
      ELSE valbestomr = BESTTAB.BESTID.
   END.
   IF uppvaltemp.TILLFALLFAST = 1 THEN DO: 
      /*TOG TILLFÄLIGA*/
      IF uppvaltemp.PAAV = 1 THEN DO: 
         /*TOG PÅGÅENDE*/ 
         RUN allpa_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
         RUN allpav_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
      END.
      ELSE IF uppvaltemp.PAAV = 2 THEN DO:      
         /*TOG AVSLUTADE*/
         RUN allav_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
      END.   
      ELSE IF uppvaltemp.PAAV = 3 THEN DO:
         /*ALLA*/
         RUN allpa_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
         RUN allav_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).      
      END.
   END.
   ELSE IF uppvaltemp.TILLFALLFAST = 2 THEN DO:      
      /*TOG FASTA*/
      IF uppvaltemp.PAAV = 1 THEN DO:
         /*TOG PÅGÅENDE*/
         RUN allpa_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).
         RUN allpav_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).       
      END.
      ELSE IF uppvaltemp.PAAV = 2 THEN DO:      
         /*TOG AVSLUTADE*/
         RUN allav_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).
      END.   
      ELSE IF uppvaltemp.PAAV = 3 THEN DO:
         /*TOG PÅGÅENDE OCH AVSLUTADE*/
        RUN allpa_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).
        RUN allav_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).      
      END.
   END.
   ELSE IF uppvaltemp.TILLFALLFAST = 3 THEN DO:      
      /*FASTA OCH TILLFÄLLIGA*/
      IF uppvaltemp.PAAV = 1 THEN DO: 
         /*TOG PÅGÅENDE*/
         RUN allpa_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
         RUN allpa_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).
         RUN allpav_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
         RUN allpav_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR). 
      END.
      ELSE IF uppvaltemp.PAAV = 2 THEN DO:      
         /*TOG AVSLUTADE*/
         RUN allav_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
         RUN allav_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).        
      END.   
      ELSE IF uppvaltemp.PAAV = 3 THEN DO:
         /*TOG PÅGÅENDE OCH AVSLUTADE*/
         RUN allpa_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
         RUN allav_UI (INPUT FALSE,INPUT uppvaltemp.VISPERAR).
         RUN allpa_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).
         RUN allav_UI (INPUT TRUE,INPUT uppvaltemp.VISPERAR).        
      END.
   END.
   FOR EACH plannrtemp WHERE plannrtemp.KOPPAO = TRUE :
      FIND FIRST AONRTAB WHERE AONRTAB.PLANNR = plannrtemp.PLANNR AND
      AONRTAB.ARTAL = plannrtemp.ARTAL NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         ASSIGN
         plannrtemp.AONR = AONRTAB.AONR 
         plannrtemp.DELNR = AONRTAB.DELNR.
      END.
   END.
END.

IF vart = 2 THEN DO:
   FOR EACH valplantemp WHERE valplantemp.KOPPAO = TRUE :
      FIND FIRST AONRTAB WHERE AONRTAB.PLANNR = valplantemp.PLANNR AND 
      AONRTAB.ARTAL = valplantemp.ARTAL NO-LOCK NO-ERROR.
      IF AVAILABLE AONRTAB THEN DO:
         ASSIGN 
         valplantemp.AONR = AONRTAB.AONR 
         valplantemp.DELNR = AONRTAB.DELNR.
      END.
   END.
END.

PROCEDURE dynstartkoll_UI:  
   musz = FALSE.
   IF uppvaltemp.BESTID NE "ALLA" THEN DO:
      ASSIGN
      kommandonyfalt = "BESTID"
      kommandoorgfalt = "BESTID".
      RUN dynakoll_UI (OUTPUT musz).
      IF musz = TRUE THEN RETURN.
   END.
   IF uppvaltemp.OMRADE NE "ALLA" THEN DO:
      ASSIGN
      kommandonyfalt = "OMRADE"
      kommandoorgfalt = "OMRADE".
      RUN dynakoll_UI (OUTPUT musz).
      IF musz = TRUE THEN RETURN.
   END.
   IF uppvaltemp.ARBANSVARIG NE "ALLA" THEN DO:
      ASSIGN
      kommandonyfalt = "ARBANSVARIG"
      kommandoorgfalt = "ARBANSVARIG".
      RUN dynakoll_UI (OUTPUT musz).
      IF musz = TRUE THEN RETURN.
   END.
END PROCEDURE. 

PROCEDURE allpa_UI.
   DEFINE INPUT PARAMETER fastplan AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER visaarperiod AS LOGICAL NO-UNDO.
   kommandoquery = "".
   IF uppvaltemp.BESTID = "ALLA" THEN DO:
      IF visaarperiod = FALSE THEN DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM = " + STRING(valdatum).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL = " + STRING(YEAR(uppvaltemp.STARTDATUM)).         
      END.
      ELSE DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM = " + STRING(valdatum).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL >= " + STRING(YEAR(uppvaltemp.STARTDATUM)).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL <= " + STRING(YEAR(uppvaltemp.SLUTDATUM)).         
      END.
      
   END.
   ELSE DO: 
      IF visaarperiod = FALSE THEN DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM = " + STRING(valdatum).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.BESTID = """  + STRING(valbestomr) + """".
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL = " + STRING(YEAR(uppvaltemp.STARTDATUM)).         
      END.
      ELSE DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM = " + STRING(valdatum).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.BESTID = """ + STRING(valbestomr) + """".
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL >= " + STRING(YEAR(uppvaltemp.STARTDATUM)).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL <= " + STRING(YEAR(uppvaltemp.SLUTDATUM)).        
      END.
   END. 
   RUN skapaq_UI.
END PROCEDURE.

PROCEDURE allpav_UI.
   DEFINE INPUT PARAMETER fastplan AS LOGICAL NO-UNDO.
   DEFINE INPUT PARAMETER visaarperiod AS LOGICAL NO-UNDO.
   kommandoquery = "".
   IF uppvaltemp.BESTID = "ALLA" THEN DO:
      IF visaarperiod = FALSE THEN DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM > " + STRING(TODAY).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL = " + STRING(YEAR(uppvaltemp.STARTDATUM)).        
      END.
      ELSE DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM > " + STRING(TODAY).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL >= " + STRING(YEAR(uppvaltemp.STARTDATUM)).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL <= " + STRING(YEAR(uppvaltemp.SLUTDATUM)).         
      END.
   END.
   ELSE DO:
      IF visaarperiod = FALSE THEN DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM > " + STRING(TODAY).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.BESTID = """ + STRING(valbestomr) + """".
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL = " + STRING(YEAR(uppvaltemp.STARTDATUM)).        
      END.
      ELSE DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM > " + STRING(TODAY).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.BESTID = """ + STRING(valbestomr) + """".
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL >= " + STRING(YEAR(uppvaltemp.STARTDATUM)).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL <= " + STRING(YEAR(uppvaltemp.SLUTDATUM)).         
      END.
   END.
   RUN skapaq_UI.
END PROCEDURE.

PROCEDURE allav_UI.
   DEFINE INPUT PARAMETER fastplan AS LOGICAL NO-UNDO. 
   DEFINE INPUT PARAMETER visaarperiod AS LOGICAL NO-UNDO.
   kommandoquery = "".
   IF uppvaltemp.BESTID = "ALLA" THEN DO:
      IF visaarperiod = FALSE THEN DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM >= " + STRING(uppvaltemp.AVSLUTSTART).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM <= " + STRING(uppvaltemp.AVSLUTSLUT).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL = " + STRING(YEAR(uppvaltemp.STARTDATUM)).         
      END.
      ELSE DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM >= " + STRING(uppvaltemp.AVSLUTSTART).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM <= " + STRING(uppvaltemp.AVSLUTSLUT).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL >= " + STRING(YEAR(uppvaltemp.STARTDATUM)).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL <= " + STRING(YEAR(uppvaltemp.SLUTDATUM)).         
      END.
   END.
   ELSE DO:
      IF visaarperiod = FALSE THEN DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM >= " + STRING(uppvaltemp.AVSLUTSTART).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM <= " + STRING(uppvaltemp.AVSLUTSLUT).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.BESTID = """ + STRING(valbestomr) + """".
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL = " + STRING(YEAR(uppvaltemp.STARTDATUM)).         
      END.
      ELSE DO:
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.FASTAPLANNR = " + STRING(fastplan).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM >= " + STRING(uppvaltemp.AVSLUTSTART).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.PLANNRAVDATUM <= " + STRING(uppvaltemp.AVSLUTSLUT).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.BESTID = """ + STRING(valbestomr) + """".
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL >= " + STRING(YEAR(uppvaltemp.STARTDATUM)).
         RUN and_UI.
         kommandoquery = kommandoquery + " PLANNRTAB.ARTAL <= " + STRING(YEAR(uppvaltemp.SLUTDATUM)).         
      END.
   END.
   RUN skapaq_UI.
   RUN objdelete_UI.
END PROCEDURE.

PROCEDURE skapaq_UI.
   ASSIGN
   utvaltab   = "FOR EACH uppvaltemp"
   nytab      = "plannrtemp"
   orginaltab = "PLANNRTAB".
   ASSIGN
   kommandoquery = "FOR EACH " +  orginaltab + " WHERE " + kommandoquery + " NO-LOCK".   
   /*BUGG 9.1c FIX*/
   ASSIGN extratemptabh = TEMP-TABLE plannrtemp:DEFAULT-BUFFER-HANDLE.
   ASSIGN extratemptabh2 = TEMP-TABLE uppvaltemp:DEFAULT-BUFFER-HANDLE.
   RUN dynquery_UI (INPUT TRUE,INPUT FALSE).

END PROCEDURE.


