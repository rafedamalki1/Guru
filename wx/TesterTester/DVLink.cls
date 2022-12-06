 
 /*------------------------------------------------------------------------
    File        : DVLink
    Purpose     : var till för att ladda defaulta värden i ribbon 
    används inte . görs ist via Helpers.Functions:CreateDefaults
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Tue Jul 03 14:32:29 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Helpers.DVLink: 
   
   {DEFVALUESNETTT.I}
   
   DEFINE PUBLIC VARIABLE ToolGroups AS System.Collections.ArrayList NO-UNDO.
   DEFINE PUBLIC VARIABLE DefaultsTT AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE DefaultValuesTT AS HANDLE NO-UNDO.
   
   DEFINE PUBLIC VARIABLE defProgram AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE defHuvudChar AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE defHuvudInt AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE defExtras AS CHARACTER NO-UNDO.
   
   CONSTRUCTOR PUBLIC DVLink(TableHandle AS HANDLE, TableHandleDef AS HANDLE, dProgram AS CHARACTER, dHuvudInt AS INTEGER, dHuvudChar AS CHARACTER, dExtras AS CHARACTER):
      
      THIS-OBJECT:DefaultsTT = TableHandle.
      THIS-OBJECT:DefaultValuesTT = TableHandleDef.
      THIS-OBJECT:ToolGroups = NEW System.Collections.ArrayList().
      
      THIS-OBJECT:defProgram = dProgram.
      THIS-OBJECT:defHuvudChar = dHuvudChar.
      THIS-OBJECT:defHuvudInt = dHuvudInt.
      THIS-OBJECT:defExtras = dExtras.
      
   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID AddToolGroup(ToolGroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup):
      THIS-OBJECT:ToolGroups:Add(ToolGroup).
   END METHOD.
   
   METHOD PUBLIC VOID LoadIteration():
      DEFINE VARIABLE CurrGroup AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      DEFINE VARIABLE CurrGroupName AS CHARACTER NO-UNDO.
      
      DEFINE VARIABLE CurrTool AS Infragistics.Win.UltraWinToolbars.ToolBase NO-UNDO.
      DEFINE VARIABLE CurrToolName AS CHARACTER NO-UNDO.
      DEFINE VARIABLE CurrToolType AS CHARACTER NO-UNDO.
      DEFINE VARIABLE CurrToolValue AS CHARACTER NO-UNDO.
      
      {WALLMAN\foreach.i System.Object obj in THIS-OBJECT:ToolGroups a}
         CurrGroup = CAST(obja, Infragistics.Win.UltraWinToolbars.RibbonGroup).
         CurrGroupName = CurrGroup:KEY.
         {WALLMAN\foreach.i System.Object obj in CurrGroup:Tools b}
            CurrToolType = "?".
            CurrToolValue = "?".
            CurrTool = CAST(objb, Infragistics.Win.UltraWinToolbars.ToolBase).
            CurrToolName = CurrTool:KEY.
            CurrToolType = CurrTool:GetType():FullName.
            
            IF CurrToolType EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN DO:
               CurrToolValue = STRING ( CAST(CurrTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED ).
            END.
           
            IF CurrToolType EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN DO:
               CurrToolValue = STRING ( CAST(CurrTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex ).
            END.
            
            
            THIS-OBJECT:DefaultsTT:FIND-FIRST("WHERE TOOLGROUP = ~"" + CurrGroupName + "~" AND TOOLKEY = ~"" + CurrToolName + "~" AND TOOLTYPE = ~"" + CurrToolType + "~" USE-INDEX TOOLKEY" , NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:DefaultsTT:AVAILABLE EQ FALSE THEN DO:
               THIS-OBJECT:DefaultsTT:BUFFER-CREATE().
               ASSIGN
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLKEY"):BUFFER-VALUE = CurrToolName
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLGROUP"):BUFFER-VALUE = CurrGroupName
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLTYPE"):BUFFER-VALUE = CurrToolType
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("HUVUDINT"):BUFFER-VALUE = THIS-OBJECT:defHuvudInt
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("HUVUDCHAR"):BUFFER-VALUE = THIS-OBJECT:defHuvudChar
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("EXTRADATA"):BUFFER-VALUE = THIS-OBJECT:defExtras
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = THIS-OBJECT:defProgram.
                  
               THIS-OBJECT:DefaultValuesTT:FIND-FIRST("WHERE TOOLGROUP = ~"" + CurrGroupName + "~" AND TOOLKEY = ~"" + CurrToolName + "-default~" AND TOOLTYPE = ~"" + CurrToolType + "~" USE-INDEX TOOLKEY" , NO-LOCK) NO-ERROR.
               IF THIS-OBJECT:DefaultValuesTT:AVAILABLE EQ TRUE THEN THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLVALUE"):BUFFER-VALUE = THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("TOOLVALUE"):BUFFER-VALUE.
               ELSE DO:
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLVALUE"):BUFFER-VALUE = CurrToolValue.
                  
                  THIS-OBJECT:DefaultValuesTT:BUFFER-CREATE().
                  ASSIGN 
                     THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("TOOLVALUE"):BUFFER-VALUE = CurrToolValue
                     THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("TOOLKEY"):BUFFER-VALUE = CurrToolName
                     THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("TOOLGROUP"):BUFFER-VALUE = CurrGroupName
                     THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("TOOLTYPE"):BUFFER-VALUE = CurrToolType
                     THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("HUVUDINT"):BUFFER-VALUE = THIS-OBJECT:defHuvudInt
                     THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("HUVUDCHAR"):BUFFER-VALUE = THIS-OBJECT:defHuvudChar
                     THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("EXTRADATA"):BUFFER-VALUE = THIS-OBJECT:defExtras
                     THIS-OBJECT:DefaultValuesTT:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE = THIS-OBJECT:defProgram.
               END.
            END.
            
            IF CurrToolType EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN DO:
               CAST(CurrTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED = THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLVALUE"):BUFFER-VALUE.
            END.
           
            IF CurrToolType EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN DO:
               CAST(CurrTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex  = THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLVALUE"):BUFFER-VALUE.
            END.
            
            
         END.
      END.
   END METHOD.


   METHOD PUBLIC VOID SaveIteration():
      DEFINE VARIABLE CurrGroup     AS Infragistics.Win.UltraWinToolbars.RibbonGroup NO-UNDO.
      DEFINE VARIABLE CurrGroupName AS CHARACTER                                     NO-UNDO.
      
      DEFINE VARIABLE CurrTool      AS Infragistics.Win.UltraWinToolbars.ToolBase    NO-UNDO.
      DEFINE VARIABLE CurrToolName  AS CHARACTER                                     NO-UNDO.
      DEFINE VARIABLE CurrToolType  AS CHARACTER                                     NO-UNDO.
      DEFINE VARIABLE CurrToolValue AS CHARACTER                                     NO-UNDO.
      
      {WALLMAN\foreach.i System.Object obj in THIS-OBJECT:ToolGroups a}
         CurrGroup = CAST(obja, Infragistics.Win.UltraWinToolbars.RibbonGroup).
         CurrGroupName = CurrGroup:KEY.
         {WALLMAN\foreach.i System.Object obj in CurrGroup:Tools b}
            CurrToolType = "?".
            CurrToolValue = "?".
            CurrTool = CAST(objb, Infragistics.Win.UltraWinToolbars.ToolBase).
            CurrToolName = CurrTool:KEY.
            CurrToolType = CurrTool:GetType():FullName.
            
            IF CurrToolType EQ "Infragistics.Win.UltraWinToolbars.StateButtonTool" THEN DO:
               CurrToolValue = STRING ( CAST(CurrTool, Infragistics.Win.UltraWinToolbars.StateButtonTool):CHECKED ).
            END.
           
            IF CurrToolType EQ "Infragistics.Win.UltraWinToolbars.ComboBoxTool" THEN DO:
               CurrToolValue = STRING ( CAST(CurrTool, Infragistics.Win.UltraWinToolbars.ComboBoxTool):SelectedIndex ).
            END.
            
            
            THIS-OBJECT:DefaultsTT:FIND-FIRST("WHERE TOOLGROUP = ~"" + CurrGroupName + "~" AND TOOLKEY = ~"" + CurrToolName + "~" AND TOOLTYPE = ~"" + CurrToolType + "~" USE-INDEX TOOLKEY" , NO-LOCK) NO-ERROR.
            IF THIS-OBJECT:DefaultsTT:AVAILABLE EQ FALSE THEN DO:
               THIS-OBJECT:DefaultsTT:BUFFER-CREATE().
               ASSIGN
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLKEY"):BUFFER-VALUE   = CurrToolName
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLGROUP"):BUFFER-VALUE = CurrGroupName
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLTYPE"):BUFFER-VALUE  = CurrToolType
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("HUVUDINT"):BUFFER-VALUE  = THIS-OBJECT:defHuvudInt
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("HUVUDCHAR"):BUFFER-VALUE = THIS-OBJECT:defHuvudChar
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("EXTRADATA"):BUFFER-VALUE = THIS-OBJECT:defExtras
                  THIS-OBJECT:DefaultsTT:BUFFER-FIELD("PROGRAM"):BUFFER-VALUE   = THIS-OBJECT:defProgram.

            END.
            THIS-OBJECT:DefaultsTT:BUFFER-FIELD("TOOLVALUE"):BUFFER-VALUE = CurrToolValue. 
            
            
         END.
      END.
   END METHOD.



END CLASS.