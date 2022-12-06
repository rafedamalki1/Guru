/*ARBARTS.P*/
DEFINE TEMP-TABLE aarttemp
   FIELD ARBARTKOD AS INTEGER
   FIELD ARBBENAMNING AS CHARACTER
   INDEX ARBARTKOD ARBARTKOD ASCENDING.
DEFINE OUTPUT PARAMETER TABLE FOR aarttemp .
{EXTRADATA.I}
DEFINE VARIABLE edataapph AS HANDLE NO-UNDO.

RUN EXTRADATAHMT.P PERSISTENT SET edataapph.         
OPEN QUERY oq FOR EACH ARBETSART NO-LOCK.
GET FIRST oq NO-LOCK.
DO WHILE AVAILABLE(ARBETSART):
   EMPTY TEMP-TABLE inextradatatemp NO-ERROR. 
   CREATE inextradatatemp.          
   ASSIGN
   inextradatatemp.PROGRAM = "ARBARTAKTIV"                   
   inextradatatemp.HUVUDINT = ARBETSART.ARBARTKOD.                    
   RUN etabhamt_UI IN edataapph (INPUT TABLE inextradatatemp, OUTPUT TABLE extradatatemp). 
   FIND FIRST extradatatemp NO-LOCK NO-ERROR.
   IF AVAILABLE extradatatemp THEN DO: 
      IF extradatatemp.SOKLOG[1] = TRUE THEN DO:      
         CREATE aarttemp.
         BUFFER-COPY ARBETSART TO aarttemp.            
      END.
   END.                   
   ELSE DO:
      CREATE aarttemp.
      BUFFER-COPY ARBETSART TO aarttemp.         
   END.


/*   CREATE aarttemp.
   ASSIGN
   aarttemp.ARBARTKOD = ARBETSART.ARBARTKOD 
   aarttemp.ARBBENAMNING = ARBETSART.ARBBENAMNING.     */
   GET NEXT oq NO-LOCK.
END.

   
