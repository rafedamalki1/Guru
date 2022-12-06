 /*KONTVALAPP.P*/
&Scoped-define NEW NEW
&Scoped-define SHARED SHARED
{GLOBVAR2DEL1.I}
{REGVAR.I}

{AVTPLANTEMP.I}

DEFINE NEW SHARED TEMP-TABLE kontkod   
   FIELD KONTO LIKE KONTO.KONTO
   FIELD KONTONR LIKE KONTO.KONTONR 
   FIELD BENAMNING LIKE KONTO.BENAMNING
   INDEX KNR IS PRIMARY KONTONR ASCENDING. 

DEFINE INPUT PARAMETER kto AS CHARACTER NO-UNDO.
DEFINE OUTPUT PARAMETER TABLE FOR kontkod.
DEFINE QUERY kontq FOR KONTO. 
EMPTY TEMP-TABLE kontkod NO-ERROR. 

OPEN QUERY kontq FOR EACH KONTO WHERE KONTO.KONTO = kto USE-INDEX KONTO NO-LOCK.  
GET FIRST kontq NO-LOCK.   
DO WHILE AVAILABLE(KONTO):
   /*CREATE kontkod.   
*       ASSIGN
*       kontkod.KONTO = kto
*       kontkod.KONTONR = KONTO.KONTONR
*       kontkod.BENAMNING = KONTO.BENAMNING.*/
   
   IF kto = "K1" THEN DO:        
      CREATE kontkod.   
      ASSIGN
      kontkod.KONTO = "K1"
      kontkod.KONTONR = KONTO.KONTONR
      kontkod.BENAMNING = KONTO.BENAMNING.         
   END.   
   ELSE IF kto = "K2" THEN DO:  
      CREATE kontkod.   
      ASSIGN
      kontkod.KONTO = "K2"
      kontkod.KONTONR = KONTO.KONTONR
      kontkod.BENAMNING = KONTO.BENAMNING.      
   END.   
   ELSE IF kto = "K3" THEN DO:   
      CREATE kontkod.   
      ASSIGN              
      kontkod.KONTO = "K3"
      kontkod.KONTONR = KONTO.KONTONR
      kontkod.BENAMNING = KONTO.BENAMNING.           
   END.   
   ELSE IF kto = "K4" THEN DO:       
      CREATE kontkod.   
      ASSIGN              
      kontkod.KONTO = "K4"
      kontkod.KONTONR = KONTO.KONTONR
      kontkod.BENAMNING = KONTO.BENAMNING.                                                                                              
   END.   
   ELSE IF kto = "K5" THEN DO:  
      CREATE kontkod.   
      ASSIGN              
      kontkod.KONTO = "K5"
      kontkod.KONTONR = KONTO.KONTONR
      kontkod.BENAMNING = KONTO.BENAMNING.      
   END.     
   GET NEXT kontq NO-LOCK.      
END.
CLOSE QUERY kontq.
