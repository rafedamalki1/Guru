 
 /*------------------------------------------------------------------------
    File        : PDSTracker
    Purpose     : skulle används till att ha koll på ändringar i ds finns i Sprakdb
    men gör nog inget där.
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Mon Jul 02 15:20:13 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS Helpers.PDSTracker: 
   DEFINE PUBLIC VARIABLE TableHandles AS System.Collections.ArrayList NO-UNDO.
   
   CONSTRUCTOR PUBLIC PDSTracker():
      THIS-OBJECT:TableHandles = NEW System.Collections.ArrayList().
   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID AddTableHandle(INPUT harr AS HANDLE):
      DEFINE VARIABLE boxer AS Helpers.ABLData NO-UNDO.
      boxer = NEW Helpers.ABLData(harr:TABLE, harr).
      THIS-OBJECT:TableHandles:Add(boxer).
   END METHOD.
   
   
   METHOD PUBLIC VOID On():
      {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:TableHandles}
         CAST(oObject, Helpers.ABLData):hnData:TRACKING-CHANGES = TRUE.
      END.
   END METHOD.
   
   METHOD PUBLIC VOID Off():
      {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:TableHandles}
         CAST(oObject, Helpers.ABLData):hnData:TRACKING-CHANGES = FALSE.
      END.
   END METHOD.
    
END CLASS.