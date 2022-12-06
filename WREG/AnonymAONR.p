/*
               KSV Editor
    Copyright: (C) 2000-2001 Serguey Klimoff (bulkl0DD)
     Filename:    AnonymAONR.p
      Comment: <comment>
   Parameters:
         Uses:
      Used by:
         
      Created: 2011.05.19 15:16 ELPAO   
     Modified: 2011.05.19 18:14 ELPAO    
     Modified: 
Byter namn och pkod på vald person     
*/
DEFINE TEMP-TABLE bytatb NO-UNDO
   FIELD pg AS CHARACTER
   FIELD PERSONALKOD AS CHARACTER.
{EXTRATAB.I}
{EXTRADATA.I}
DEFINE VARIABLE fbestapph AS HANDLE NO-UNDO.
RUN EXTRATABHMT.P PERSISTENT SET fbestapph.

DEFINE INPUT PARAMETER GamVarde AS CHARACTER NO-UNDO.
DEFINE INPUT PARAMETER Nyvarde AS CHARACTER NO-UNDO.   /* nya värden*/
   
DEFINE VARIABLE gfalt AS CHARACTER NO-UNDO.
DEFINE VARIABLE styrtab AS CHARACTER NO-UNDO.

DEFINE VARIABLE NyvardeI AS INTEGER NO-UNDO.


DEFINE VARIABLE orgtabh AS HANDLE NO-UNDO.
DEFINE VARIABLE orgtabqh AS HANDLE NO-UNDO.
DEFINE VARIABLE gfalth AS HANDLE NO-UNDO.
  


gfalt = "AONR".  
RUN allmanbyt_UI.

PROCEDURE allmanbyt_UI :
   FOR EACH _File:
      IF SUBSTRING(_File._File-name,1,1) = "_" THEN.
      ELSE DO:
         FIND FIRST _Field OF _File WHERE _Field._Field-name = gfalt NO-ERROR.  
         IF AVAILABLE _Field THEN DO:
          /*  DISPLAY  _File._File-name  GamVarde.*/
            IF _File._File-name = "BEREDNING" OR _File._File-name = "VARDERING" THEN DO:
               RUN BLANKA_UI.
            END.   
            ELSE IF _File._File-name = "BERID" OR 
                    _File._File-name = "BERID2" OR 
                    _File._File-name = "BERKALK" OR 
                    _File._File-name = "BERKALKOPPLA" OR 
                    _File._File-name = "BERLINKAB" OR 
                    _File._File-name = "BERMTRL" OR 
                    _File._File-name = "BERORD" OR 
                    _File._File-name = "BERPUNKT" OR 
                    _File._File-name = "BERSCHAKT" OR 
                    _File._File-name = "BERUPP" OR 
                    _File._File-name = "BERVAL" OR 
                    _File._File-name = "BETAONR" OR 
                    _File._File-name = "FRIKORT" OR 
                    _File._File-name = "KSKYDD" OR 
                    _File._File-name = "SCHAKTKAB" THEN DO:
               
            END.
            ELSE IF Nyvarde = "" THEN DO: 
               RUN del_UI.
               RUN spec_UI.
            END.
            ELSE DO:
               RUN byt_ui.
               RUN spec_UI.
            END.   
         END.   
      END.
   END. 

END PROCEDURE.
PROCEDURE BLANKA_UI :
   DEFINE VARIABLE borgtabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE borgtabqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE AOfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE DOfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE bstyrkommando AS CHARACTER NO-UNDO.
   bstyrkommando = "FOR EACH " + _File._File-name + " WHERE " + _File-name + "." + gfalt + " = " + QUOTER(GamVarde).  
   CREATE BUFFER Borgtabh FOR TABLE _File._File-name NO-ERROR.
   AOfalth = borgtabh:BUFFER-FIELD(gfalt).
   DOfalth = borgtabh:BUFFER-FIELD("DELNR").
   CREATE QUERY borgtabqh.   
   borgtabqh:SET-BUFFERS(borgtabh).
   borgtabqh:QUERY-PREPARE(bstyrkommando).
   borgtabqh:QUERY-OPEN.
   borgtabqh:GET-FIRST(NO-LOCK).
   DO WHILE borgtabqh:QUERY-OFF-END = FALSE TRANSACTION:  
      borgtabqh:GET-CURRENT(EXCLUSIVE-LOCK).
      IF _File._File-name = "VARDERING" THEN DO:
         RUN Aovard_UI (INPUT borgtabqh:BUFFER-FIELD("VARDNR"):BUFFER-VALUE).
      END.   
      AOfalth:BUFFER-VALUE = ?.  
      DOfalth:BUFFER-VALUE = ?.  
      borgtabqh:GET-NEXT(NO-LOCK).
   END.
     
END PROCEDURE.
PROCEDURE Aovard_UI :
   DEFINE INPUT  PARAMETER NR AS INTEGER NO-UNDO.
   FIND FIRST AOVARD WHERE AOVARD.VARDNR = nr EXCLUSIVE-LOCK NO-ERROR.
   IF AVAILABLE AOVARD THEN DELETE AOVARD.
END PROCEDURE.
PROCEDURE del_UI :
   DEFINE VARIABLE borgtabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE borgtabqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE bfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE bstyrkommando AS CHARACTER NO-UNDO.
   bstyrkommando = "FOR EACH " + _File._File-name + " WHERE " + _File-name + "." + gfalt + " = " + QUOTER(GamVarde).  
   CREATE BUFFER Borgtabh FOR TABLE _File._File-name NO-ERROR.
   
   CREATE QUERY borgtabqh.   
   borgtabqh:SET-BUFFERS(borgtabh).
   borgtabqh:QUERY-PREPARE(bstyrkommando).
   borgtabqh:QUERY-OPEN.
   borgtabqh:GET-FIRST(NO-LOCK).
   DO WHILE borgtabqh:QUERY-OFF-END = FALSE TRANSACTION:  
      borgtabqh:GET-CURRENT(EXCLUSIVE-LOCK).
      borgtabh:BUFFER-DELETE().  
      borgtabqh:GET-NEXT(NO-LOCK).
   END.
END PROCEDURE.
PROCEDURE byt_UI :
   DEFINE VARIABLE borgtabh AS HANDLE NO-UNDO.
   DEFINE VARIABLE borgtabqh AS HANDLE NO-UNDO.
   DEFINE VARIABLE bfalth AS HANDLE NO-UNDO.
   DEFINE VARIABLE bstyrkommando AS CHARACTER NO-UNDO.
   MESSAGE "obs ej klar "
   VIEW-AS ALERT-BOX.
   RETURN.
   /*
   bstyrkommando = "FOR EACH " + _File._File-name + " WHERE " + _File-name + "." + gfalt + " = " + QUOTER(GamVarde).  
   CREATE BUFFER Borgtabh FOR TABLE _File._File-name NO-ERROR.
   bfalth = borgtabh:BUFFER-FIELD(gfalt).
   CREATE QUERY borgtabqh.   
   borgtabqh:SET-BUFFERS(borgtabh).
   borgtabqh:QUERY-PREPARE(bstyrkommando).
   borgtabqh:QUERY-OPEN.
   borgtabqh:GET-FIRST(NO-LOCK).
   DO WHILE borgtabqh:QUERY-OFF-END = FALSE TRANSACTION:  
      borgtabqh:GET-CURRENT(EXCLUSIVE-LOCK).
      bfalth:BUFFER-VALUE = Nyvarde.  
      borgtabqh:GET-NEXT(NO-LOCK).
   END.
*/
  
   
END PROCEDURE.

PROCEDURE spec_UI :
   IF Nyvarde = "" THEN DO: 
      RUN exbort_UI.
      
   END.
   ELSE DO:
      RUN exbyt_ui.
      
   END.  
   
END PROCEDURE.

PROCEDURE exbort_UI :
   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.      
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "FBAONR"                   
   inextrakopptemp.KOPPLACHAR1 = GamVarde
   inextrakopptemp.KOPPLAINT1 =  ?      
   inextrakopptemp.KOPPLACHAR2 = ?            
   inextrakopptemp.KOPPLAINT2 =  ?.
   RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp).
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "INTAONR"                   
   inextrakopptemp.KOPPLACHAR1 = GamVarde
   inextrakopptemp.KOPPLAINT1 =  ?      
   inextrakopptemp.KOPPLACHAR2 = ?            
   inextrakopptemp.KOPPLAINT2 =  ?.
   RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp).

   EMPTY TEMP-TABLE inextrakopptemp NO-ERROR.
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "AOTIDPERS"
   inextrakopptemp.KOPPLACHAR1 = GamVarde    
   inextrakopptemp.KOPPLAINT1 = ? 
   inextrakopptemp.KOPPLACHAR2 = ?
   inextrakopptemp.KOPPLAINT2 =  ?.
   RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp).   
   CREATE inextrakopptemp.          
   ASSIGN
   inextrakopptemp.PROGRAM = "AODAGBOK"                   
   inextrakopptemp.KOPPLACHAR1 = GamVarde               
   inextrakopptemp.KOPPLAINT1 = ?
   inextrakopptemp.KOPPLACHAR2 = ?            
   inextrakopptemp.KOPPLAINT2 =  ?.   
   RUN exbort_UI IN fbestapph (INPUT TABLE inextrakopptemp). 

   RUN refbort. 
   
END PROCEDURE.
PROCEDURE refbort.   
   
   DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.
   
   DEFINE VARIABLE bloblog AS LOGICAL NO-UNDO.
   RUN EXTRADATAHMT.P PERSISTENT SET edataapph.
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "AOREF"                   
   inextradatatemp.HUVUDCH = GamVarde              
   inextradatatemp.HUVUDINT =  ?.         
   RUN exbort_UI IN edataapph (INPUT TABLE inextradatatemp).
   
   IF VALID-HANDLE(edataapph) THEN DELETE PROCEDURE edataapph.      
   
END PROCEDURE.
/*
   
run AnonymAONR.p (input "32032",INPUT "").   
run AnonymAONR.p (input "32033",INPUT "").   
run AnonymAONR.p (input "32034",INPUT "").   
run AnonymAONR.p (input "32035",INPUT "").   
run AnonymAONR.p (input "32036",INPUT "").   
run AnonymAONR.p (input "32037",INPUT "").   
run AnonymAONR.p (input "32038",INPUT "").   
run AnonymAONR.p (input "32039",INPUT "").   
run AnonymAONR.p (input "32040",INPUT "").   
run AnonymAONR.p (input "32041",INPUT "").   
run AnonymAONR.p (input "32042",INPUT "").   
run AnonymAONR.p (input "32043",INPUT "").   
run AnonymAONR.p (input "32044",INPUT "").   
run AnonymAONR.p (input "32045",INPUT "").   
run AnonymAONR.p (input "32046",INPUT "").   
run AnonymAONR.p (input "32047",INPUT "").   
run AnonymAONR.p (input "32048",INPUT "").   
run AnonymAONR.p (input "32049",INPUT "").   
run AnonymAONR.p (input "32050",INPUT "").   
run AnonymAONR.p (input "32051",INPUT "").   
run AnonymAONR.p (input "32052",INPUT "").   
run AnonymAONR.p (input "32053",INPUT "").   
run AnonymAONR.p (input "32054",INPUT "").   
run AnonymAONR.p (input "32055",INPUT "").   
run AnonymAONR.p (input "32056",INPUT "").   
run AnonymAONR.p (input "32057",INPUT "").   
run AnonymAONR.p (input "32058",INPUT "").   
run AnonymAONR.p (input "32059",INPUT "").   
run AnonymAONR.p (input "32060",INPUT "").   
run AnonymAONR.p (input "32061",INPUT "").   
run AnonymAONR.p (input "32062",INPUT "").   
run AnonymAONR.p (input "32063",INPUT "").   
run AnonymAONR.p (input "32064",INPUT "").   
run AnonymAONR.p (input "32065",INPUT "").   
run AnonymAONR.p (input "32067",INPUT "").   
run AnonymAONR.p (input "32068",INPUT "").   
run AnonymAONR.p (input "32069",INPUT "").   
run AnonymAONR.p (input "32070",INPUT "").   
run AnonymAONR.p (input "32071",INPUT "").   
run AnonymAONR.p (input "32072",INPUT "").   
run AnonymAONR.p (input "32073",INPUT "").   
run AnonymAONR.p (input "32074",INPUT "").   
run AnonymAONR.p (input "32075",INPUT "").   
run AnonymAONR.p (input "32076",INPUT "").   
run AnonymAONR.p (input "32077",INPUT "").   
run AnonymAONR.p (input "32078",INPUT "").   
run AnonymAONR.p (input "32079",INPUT "").   
run AnonymAONR.p (input "32080",INPUT "").   
run AnonymAONR.p (input "32081",INPUT "").   
run AnonymAONR.p (input "32082",INPUT "").   
run AnonymAONR.p (input "32083",INPUT "").   
run AnonymAONR.p (input "32084",INPUT "").   
run AnonymAONR.p (input "32085",INPUT "").   
run AnonymAONR.p (input "32086",INPUT "").   
run AnonymAONR.p (input "32087",INPUT "").   
run AnonymAONR.p (input "32088",INPUT "").   
run AnonymAONR.p (input "32089",INPUT "").   
run AnonymAONR.p (input "32090",INPUT "").   
run AnonymAONR.p (input "32091",INPUT "").   
run AnonymAONR.p (input "32092",INPUT "").   
run AnonymAONR.p (input "32093",INPUT "").   
run AnonymAONR.p (input "32094",INPUT "").   
run AnonymAONR.p (input "32095",INPUT "").   
run AnonymAONR.p (input "32096",INPUT "").   
run AnonymAONR.p (input "32097",INPUT "").   
run AnonymAONR.p (input "32098",INPUT "").   
run AnonymAONR.p (input "32099",INPUT "").   
run AnonymAONR.p (input "32100",INPUT "").   
run AnonymAONR.p (input "32101",INPUT "").   
run AnonymAONR.p (input "32102",INPUT "").   
run AnonymAONR.p (input "32103",INPUT "").   
run AnonymAONR.p (input "32104",INPUT "").   
run AnonymAONR.p (input "32105",INPUT "").   
run AnonymAONR.p (input "32106",INPUT "").   
run AnonymAONR.p (input "32107",INPUT "").   
run AnonymAONR.p (input "32108",INPUT "").   
run AnonymAONR.p (input "32109",INPUT "").   
run AnonymAONR.p (input "32110",INPUT "").   
run AnonymAONR.p (input "32111",INPUT "").   
run AnonymAONR.p (input "32112",INPUT "").   
run AnonymAONR.p (input "32113",INPUT "").   
run AnonymAONR.p (input "32114",INPUT "").   
run AnonymAONR.p (input "32115",INPUT "").   
run AnonymAONR.p (input "32116",INPUT "").   
run AnonymAONR.p (input "32117",INPUT "").   
run AnonymAONR.p (input "32118",INPUT "").   
run AnonymAONR.p (input "32119",INPUT "").   
run AnonymAONR.p (input "32120",INPUT "").   
run AnonymAONR.p (input "32121",INPUT "").   
run AnonymAONR.p (input "32122",INPUT "").   
run AnonymAONR.p (input "32123",INPUT "").   
run AnonymAONR.p (input "32124",INPUT "").   
run AnonymAONR.p (input "32125",INPUT "").   
run AnonymAONR.p (input "32126",INPUT "").   
run AnonymAONR.p (input "32127",INPUT "").   
run AnonymAONR.p (input "32128",INPUT "").   
run AnonymAONR.p (input "32129",INPUT "").   
run AnonymAONR.p (input "32130",INPUT "").   
run AnonymAONR.p (input "32131",INPUT "").   
run AnonymAONR.p (input "33999",INPUT "").   


*/
