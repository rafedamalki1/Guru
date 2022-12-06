 
 /*------------------------------------------------------------------------
    File        : ABLDataContainer
    Purpose     : du skickar in ett värde från txt cmb_box får tillbaka ett värde som sen är sökbart mot databsen
    eller för att fylla cmb_boxar mm sök på FOR EACH typtt 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Wed Mar 07 10:04:56 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Helpers.ABLDataContainer INHERITS System.Object: 
   DEFINE PUBLIC VARIABLE DataItems AS System.Collections.ArrayList NO-UNDO.
   
   CONSTRUCTOR PUBLIC ABLDataContainer():
      THIS-OBJECT:DataItems = NEW System.Collections.ArrayList().
   END CONSTRUCTOR.
   
   METHOD PUBLIC LOGICAL CreateData(INPUT n AS CHARACTER, INPUT d AS CHARACTER):
      IF THIS-OBJECT:Exists(n) = TRUE THEN RETURN FALSE.
      THIS-OBJECT:DataItems:Add(NEW Helpers.ABLData(n, d)).
      RETURN TRUE.
   END METHOD.   
   
   METHOD PUBLIC LOGICAL CreateData(INPUT n AS CHARACTER, INPUT d AS HANDLE):
      IF THIS-OBJECT:Exists(n) = TRUE THEN RETURN FALSE.
      THIS-OBJECT:DataItems:Add(NEW Helpers.ABLData(n, d)).
      RETURN TRUE.
   END METHOD.
   
   METHOD PUBLIC LOGICAL CreateData(INPUT n AS CHARACTER, INPUT d AS INTEGER):
      IF THIS-OBJECT:Exists(n) = TRUE THEN RETURN FALSE.
      THIS-OBJECT:DataItems:Add(NEW Helpers.ABLData(n, d)).
      RETURN TRUE.
   END METHOD.
   
   METHOD PUBLIC LOGICAL CreateData(INPUT n AS CHARACTER, INPUT d AS LOGICAL):
      IF THIS-OBJECT:Exists(n) = TRUE THEN RETURN FALSE.
      THIS-OBJECT:DataItems:Add(NEW Helpers.ABLData(n, d)).
      RETURN TRUE.
   END METHOD.
   
   METHOD PUBLIC HANDLE GetHandle(INPUT n AS CHARACTER):
      {WALLMAN\Foreach.i System.Object oObject in THIS-OBJECT:DataItems}
         IF CAST(oObject, Helpers.ABLData):NAME = n THEN DO:
            RETURN CAST(oObject, Helpers.ABLData):hnData.
         END.
      END.
      RETURN ?.
   END METHOD.
   
   METHOD PUBLIC INTEGER GetInteger(INPUT n AS CHARACTER):
      {WALLMAN\Foreach.i System.Object oObject in THIS-OBJECT:DataItems}
         IF CAST(oObject, Helpers.ABLData):NAME = n THEN DO:
            RETURN CAST(oObject, Helpers.ABLData):intData.
         END.
      END.
      RETURN ?.
   END METHOD.
   
   METHOD PUBLIC CHARACTER GetCharacter(INPUT n AS CHARACTER):
      {WALLMAN\Foreach.i System.Object oObject in THIS-OBJECT:DataItems}
         IF CAST(oObject, Helpers.ABLData):NAME = n THEN DO:
            RETURN CAST(oObject, Helpers.ABLData):chData.
         END.
      END.
      RETURN ?.
   END METHOD.
   
   METHOD PUBLIC LOGICAL GetLogical(INPUT n AS CHARACTER):
      {WALLMAN\Foreach.i System.Object oObject in THIS-OBJECT:DataItems}
         IF CAST(oObject, Helpers.ABLData):NAME = n THEN DO:
            RETURN CAST(oObject, Helpers.ABLData):logData.
         END.
      END.
      RETURN ?.
   END METHOD.
   
   METHOD PUBLIC LOGICAL Exists(INPUT n AS CHARACTER):
      {WALLMAN\Foreach.i System.Object oObject in THIS-OBJECT:DataItems}
         IF CAST(oObject, Helpers.ABLData):NAME = n THEN DO:
            RETURN TRUE.
         END.
      END.
      RETURN FALSE.
   END METHOD.
   
END CLASS.