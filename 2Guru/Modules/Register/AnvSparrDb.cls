 /*------------------------------------------------------------------------
    File        : KalkylAdmDb
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Mon Jan 07 14:13:22 CET 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Modules.Register.AnvSparrDb: 
   DEFINE PUBLIC  PROPERTY AppServerHandle AS HANDLE NO-UNDO 
   PUBLIC GET. PUBLIC SET.
   DEFINE PUBLIC VARIABLE Root                  AS Guru.Root NO-UNDO. 
   DEFINE PUBLIC VARIABLE ControllAnvSparr                  AS Modules.Register.AnvSparr NO-UNDO. 
 
   DEFINE PUBLIC VARIABLE mjprish AS HANDLE NO-UNDO.
   DEFINE VARIABLE qH       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE queryvar AS CHARACTER NO-UNDO.
   DEFINE VARIABLE qH2       AS HANDLE    NO-UNDO.
   DEFINE VARIABLE queryvar2 AS CHARACTER NO-UNDO.
   DEFINE VARIABLE SparrDS  AS HANDLE NO-UNDO.
   DEFINE VARIABLE LogguserDS AS HANDLE NO-UNDO.
   DEFINE VARIABLE snabbspara AS LOGICAL NO-UNDO.
   
   
   &Scoped-define PUBLIC PUBLIC
   {USERSPARRTTH.I}

   CONSTRUCTOR PUBLIC AnvSparrDb(INPUT r AS Guru.Root):
      THIS-OBJECT:Root = r.
      THIS-OBJECT:Connect().
     
   END CONSTRUCTOR.
   METHOD PUBLIC VOID Connect():
       /*THIS-OBJECT:AnvSparr:Connect(THIS-PROCEDURE).*/
      IF Guru.Konstanter:appcon THEN DO:
         RUN SPARRUSERIP.P PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globanv).        
      END.
      ELSE DO:
         RUN SPARRUSERIP.P PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globanv).         
      END.   
      
      THIS-OBJECT:AnvSparr().
   END METHOD.
   
   METHOD PUBLIC VOID AnvSparr():
      DEFINE VARIABLE startdatetime AS DATETIME NO-UNDO.
      IF  VALID-HANDLE(usersparrbuffh) THEN usersparrbuffh:EMPTY-TEMP-TABLE().
      RUN SparrAnv_UI IN AppServerHandle (OUTPUT DATASET-HANDLE SparrDS APPEND BIND).
      IF VALID-HANDLE(SparrDS) THEN DO:
         usersparrbuffh = SparrDS:GET-BUFFER-HANDLE(1).         
         THIS-OBJECT:AnvSparrTracking(TRUE).
      END.
      startdatetime = DATETIME(TODAY,0).
      
       /*Anders Olsson Elpool i Umeå AB  3 nov 2022 11:06:07 
         visa inte   ELPAO OCH GURUSUPPORT  
         */
       queryvar =  "FOR EACH " + usersparrbuffh:TABLE.
      qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:usersparrbuffh,queryvar).
      qH:GET-FIRST().
      DO WHILE qH:QUERY-OFF-END = FALSE:
         usersparrbuffh:BUFFER-FIELD("SOKCHAR2"):BUFFER-VALUE = usersparrbuffh:BUFFER-FIELD("SOKCHAR"):BUFFER-VALUE(2).
         usersparrbuffh:BUFFER-FIELD("SOKCHAR3"):BUFFER-VALUE = usersparrbuffh:BUFFER-FIELD("SOKCHAR"):BUFFER-VALUE(3).
         usersparrbuffh:BUFFER-FIELD("SOKCHAR5"):BUFFER-VALUE = usersparrbuffh:BUFFER-FIELD("SOKCHAR"):BUFFER-VALUE(5).
         qH:GET-NEXT().
      END.  
       
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN.    
      ELSE DO:
         queryvar =  "FOR EACH " + usersparrbuffh:TABLE + " WHERE SOKCHAR5 = " +  QUOTER(CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) ).
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:usersparrbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            usersparrbuffh:BUFFER-DELETE().   
            qH:GET-NEXT().
         END.
         queryvar =  "FOR EACH " + usersparrbuffh:TABLE + " WHERE SOKCHAR5 = " +  QUOTER(CHR(71) + CHR(85) + CHR(82) + CHR(85) + CHR(83) + CHR(85) + CHR(80) + CHR(80) + CHR(79) + CHR(82) + CHR(84)).
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:usersparrbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            usersparrbuffh:BUFFER-DELETE().
            qH:GET-NEXT().
         END.
      END.
     
      
   
      
      
      
   END METHOD.

   METHOD PUBLIC VOID LoggHmt(INPUT vadsokes AS CHARACTER):
      IF VALID-HANDLE(userloggbuffh) THEN userloggbuffh:EMPTY-TEMP-TABLE().
      RUN LoggAnv_UI IN AppServerHandle (INPUT vadsokes,OUTPUT DATASET-HANDLE LogguserDS APPEND BIND).
      IF VALID-HANDLE(LogguserDS) THEN DO:
         IF  VALID-HANDLE(userloggbuffh) THEN.
         ELSE userloggbuffh = LogguserDS:GET-BUFFER-HANDLE(1).         
      END.
      IF Guru.Konstanter:globanv = CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79) THEN.    
      ELSE DO:
         /*Anders Olsson Elpool i Umeå AB  3 nov 2022 11:06:07 
         visa inte   ELPAO OCH GURUSUPPORT  
         */
         queryvar =  "FOR EACH " + userloggbuffh:TABLE + " WHERE Loggusertt.ANVANDARE = " +  QUOTER(CHR(69) + CHR(76) + CHR(80) + CHR(65) + CHR(79)).
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:userloggbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            userloggbuffh:BUFFER-DELETE().
            qH:GET-NEXT().
         END.
         queryvar =  "FOR EACH " + userloggbuffh:TABLE + " WHERE Loggusertt.ANVANDARE = " +  QUOTER(CHR(71) + CHR(85) + CHR(82) + CHR(85) + CHR(83) + CHR(85) + CHR(80) + CHR(80) + CHR(79) + CHR(82) + CHR(84)).
         qH = THIS-OBJECT:Root:DatabaseManager:Global:CreateCustomQuery(THIS-OBJECT:userloggbuffh,queryvar).
         qH:GET-FIRST().
         DO WHILE qH:QUERY-OFF-END = FALSE:
            userloggbuffh:BUFFER-DELETE().
            qH:GET-NEXT().
         END.
         
      END.      
   END METHOD.
   METHOD PUBLIC VOID Lasuppspara():
       DEFINE VARIABLE chDS AS HANDLE NO-UNDO.
       THIS-OBJECT:AnvSparrTracking(FALSE).
       CREATE DATASET chDS.
       chDS:CREATE-LIKE(SparrDS).
       chDS:GET-CHANGES(SparrDS).
       RUN SparaDynDSstart_UI IN AppServerHandle (INPUT "SparrDS", INPUT DATASET-HANDLE chDS).
     
       chDS:MERGE-CHANGES(SparrDS).
       THIS-OBJECT:AnvSparrTracking(TRUE).
   END METHOD.
   
   METHOD PUBLIC VOID AnvSparrTracking(INPUT onoff AS LOGICAL):
      IF onoff = TRUE THEN snabbspara = FALSE.
      ELSE snabbspara = TRUE.
      IF NOT VALID-HANDLE(usersparrtth) THEN usersparrtth = usersparrbuffh:TABLE-HANDLE.            
      usersparrtth:TRACKING-CHANGES = onoff.            
   END METHOD.
   
   METHOD PUBLIC VOID InitializeTTs():
      
   END METHOD.
   
   
   
   DESTRUCTOR AnvSparrDb():
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DO:
         RUN AvsSparrUser_UI IN AppServerHandle.
         DELETE PROCEDURE THIS-OBJECT:AppServerHandle NO-ERROR.
         AppServerHandle = ?.
      END.   
   END DESTRUCTOR.
   
END CLASS.
