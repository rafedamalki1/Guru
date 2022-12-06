 
 /*------------------------------------------------------------------------
    File        : ABLData
    Purpose     : användsav ABLDataContainer och PDSTracker
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Wed Mar 07 10:04:37 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Helpers.ABLData INHERITS System.Object: 
   DEFINE PUBLIC VARIABLE Name AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE chData AS CHARACTER NO-UNDO.
   DEFINE PUBLIC VARIABLE hnData AS HANDLE NO-UNDO.
   DEFINE PUBLIC VARIABLE intData AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE logData AS LOGICAL NO-UNDO.
   
   CONSTRUCTOR PUBLIC ABLData(n AS CHARACTER, d AS CHARACTER):
     
      THIS-OBJECT:NAME = n.
      THIS-OBJECT:chData = d.
   END.
   
   CONSTRUCTOR PUBLIC ABLData(n AS CHARACTER, d AS HANDLE):
      
      THIS-OBJECT:NAME = n.
      THIS-OBJECT:hnData = d.
   END.
   
   CONSTRUCTOR PUBLIC ABLData(n AS CHARACTER, d AS INTEGER):
     
      THIS-OBJECT:NAME = n.
      THIS-OBJECT:intData = d.
   END.
   
   CONSTRUCTOR PUBLIC ABLData(n AS CHARACTER, d AS LOGICAL):
     
      THIS-OBJECT:NAME = n.
      THIS-OBJECT:logData = d.
   END.
   
END CLASS.