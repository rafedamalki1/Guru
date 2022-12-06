 
 /*------------------------------------------------------------------------
    File        : Functions
    Purpose     : laddar ribbon med default har datum time siffra bokstav
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue Jul 10 11:03:38 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS  Helpers.Functions: 
   /*anders
   METHOD PUBLIC STATIC VOID CreateDefaults(r AS Guru.Root, p AS CHARACTER, toolgroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup):
      DEFINE VARIABLE iterator     AS INTEGER                                    NO-UNDO.
      DEFINE VARIABLE ToolType     AS CHARACTER                                  NO-UNDO.
      DEFINE VARIABLE ToolValue    AS CHARACTER                                  NO-UNDO.
      DEFINE VARIABLE ToolKey      AS CHARACTER                                  NO-UNDO.
      DEFINE VARIABLE DasTool      AS Infragistics.Win.UltraWinToolbars.ToolBase NO-UNDO.
      DEFINE VARIABLE cToolGroup AS CHARACTER NO-UNDO.
      
      DEFINE VARIABLE DebugMessage AS CHARACTER                                  NO-UNDO.
     
      cToolGroup = toolgroup:KEY.
      
      REPEAT iterator = 0 TO toolgroup:Tools:Count - 1 :
         DasTool = toolgroup:Tools[iterator].
         ToolKey  = DasTool:KEY.
         ToolType = DasTool:GetType():FullName.
         ToolValue = "???".
         IF ToolType EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN 
         DO:
            ToolValue = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED ).
         END.
        
         IF ToolType EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN 
         DO:
            ToolValue = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex ).
         END.
        
         r:DatabaseManager:Global:CreateActualDefault(p, Toolkey, Toolvalue, cToolgroup, Tooltype).
         
         
      END.
   END METHOD.
   */
   /*ANDERS*/
   METHOD PUBLIC STATIC VOID CreateDefaults(r AS Guru.Root, p AS CHARACTER, huvint AS INTEGER,huvchar AS CHARACTER, toolgroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup):
      DEFINE VARIABLE iterator     AS INTEGER                                    NO-UNDO.
      DEFINE VARIABLE ToolType     AS CHARACTER                                  NO-UNDO.
      DEFINE VARIABLE ToolValue    AS CHARACTER                                  NO-UNDO.
      DEFINE VARIABLE defToolValue    AS CHARACTER                                  NO-UNDO.
      DEFINE VARIABLE ToolKey      AS CHARACTER                                  NO-UNDO.
      DEFINE VARIABLE DasTool      AS Infragistics.Win.UltraWinToolbars.ToolBase NO-UNDO.
      DEFINE VARIABLE cToolGroup AS CHARACTER NO-UNDO.
      
      DEFINE VARIABLE DebugMessage AS CHARACTER                                  NO-UNDO.
      cToolGroup = toolgroup:KEY.
      
      REPEAT iterator = 0 TO toolgroup:Tools:Count - 1 :
         DasTool = toolgroup:Tools[iterator].
         ToolKey  = DasTool:KEY.
         ToolType = DasTool:GetType():FullName.
         ToolValue = "".
         IF ToolType EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN DO:
            ToolValue = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED ).
         END.
        
         IF ToolType EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN DO:
            ToolValue = STRING ( CAST(DasTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex ).
         END.
         ELSE IF ToolType EQ "Infragistics.Win.UltraWinToolbars.ButtonTool" THEN DO:
            NEXT.             
         END.   
                  
         /*SKAPAR NY POST OCH GER DEF VÄRDE*/
         r:DatabaseManager:Global:CreateActualDefaultTT(p,Guru.GlobalaVariabler:GuruDefaultAnv,huvint,huvchar,Toolkey, Toolvalue, cToolgroup, Tooltype,FALSE, OUTPUT defToolValue).
         IF ToolType EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN DO:
             CAST(DasTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED  = LOGICAL(defToolValue).
         END.
       
         IF ToolType EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN DO:
            CAST(DasTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex = INTEGER(defToolValue).
         END.
       
      END.
      
   END METHOD. 
   METHOD PUBLIC STATIC VOID GetDateTime(INPUT dattid AS DATETIME, OUTPUT datvar AS DATE, OUTPUT tidvar AS INTEGER ):
       tidvar = INTEGER( TRUNCATE( MTIME( dattid ) / 1000, 0 ) ).
       datvar = DATE(dattid).
       
   END METHOD.
   METHOD PUBLIC STATIC VOID GetDateTimeFormat(INPUT  datvar AS DATE, INPUT tidvardec AS DECIMAL, OUTPUT dattid AS DATETIME ):         
      dattid = DATETIME(MONTH(datvar) , DAY(datvar) , YEAR(datvar) , INTEGER(SUBSTRING(STRING(tidvardec),1,2)) , INTEGER(SUBSTRING(STRING(tidvardec),4,2))).
   END METHOD. 
   
   METHOD PUBLIC STATIC LOGICAL IsSiffraBokstav(INPUT varde AS CHARACTER):
       
      DEFINE VARIABLE ascivarde AS INTEGER NO-UNDO.
      
      ascivarde = ASC(varde).
      /*siffror*/
      IF ascivarde >= 48 AND ascivarde <= 57 THEN RETURN TRUE.
      
      /*stora bokstäver*/
      ELSE IF ascivarde >= 65 AND ascivarde <= 90 THEN RETURN TRUE.
      
      /*små bokstäver*/
      ELSE IF ascivarde >= 97 AND ascivarde <= 122 THEN RETURN TRUE.
      
      /*Å Ä Ö å ä ö*/
      ELSE IF ascivarde = 197 OR ascivarde = 196 OR ascivarde = 214 OR ascivarde = 229 OR 
              ascivarde = 228 OR ascivarde = 246 THEN RETURN TRUE.
      /*! " # % & / ( ) = ? + ' * - _ , ; :. < >  @ £ $ €[] \ § ½*/
      ELSE IF ascivarde = 33 OR ascivarde = 34 OR ascivarde = 35 OR ascivarde = 37 OR ascivarde = 38 OR ascivarde = 47 OR
              ascivarde = 40 OR ascivarde = 41 OR ascivarde = 61 OR ascivarde = 63 OR ascivarde = 43 OR ascivarde = 180 OR 
              ascivarde = 39 OR ascivarde = 42 OR ascivarde = 45 OR ascivarde = 95 OR ascivarde = 44 OR ascivarde = 59 OR 
              ascivarde = 58 OR ascivarde = 46 OR ascivarde = 60 OR ascivarde = 60 OR ascivarde = 32 OR ascivarde = 41 OR 
              ascivarde = 64 OR ascivarde = 163 OR ascivarde = 36 OR ascivarde = 128 OR ascivarde = 41 OR ascivarde = 91 OR 
              ascivarde = 93 OR ascivarde = 92 OR ascivarde = 167 OR ascivarde = 189 THEN RETURN TRUE.               
      
      RETURN FALSE.
   END METHOD.   

   METHOD PUBLIC STATIC CHARACTER SkapaMultiSok(INPUT sokstring AS CHARACTER, INPUT falt AS CHARACTER):
      DEFINE VARIABLE WorkString AS CHARACTER INITIAL "" NO-UNDO.
      DEFINE VARIABLE ix      AS INTEGER   NO-UNDO.
      DEFINE VARIABLE curr AS CHARACTER INITIAL "" NO-UNDO.
      DEFINE VARIABLE found AS LOGICAL INITIAL FALSE NO-UNDO.
      
      IF TRIM(sokstring) NE "" THEN DO: 
         REPEAT ix = 1 TO NUM-ENTRIES(sokstring,"&&"):
            curr = TRIM(ENTRY(ix, sokstring, "&&")).
            IF curr NE "" THEN DO: 
               IF ix EQ 1 THEN DO:
                  found = TRUE.
                  WorkString = falt + " MATCHES ~"*" + curr + "*~"".
               END.
               ELSE DO:
                  WorkString = WorkString + " AND " + falt + " MATCHES ~"*" + curr + "*~"".
               END.
            END.
         END.
      END.
      IF found NE TRUE THEN DO:
         WorkString = falt + " MATCHES ~"*" + sokstring + "*~"".
      END.
      RETURN WorkString.
   END METHOD.
END CLASS.