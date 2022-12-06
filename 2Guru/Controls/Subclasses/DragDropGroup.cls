 
 /*------------------------------------------------------------------------
    File        : DragDropGroup
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Fri Mar 02 14:46:52 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Controls.Subclasses.DragDropGroup INHERITS System.Object:
    
   DEFINE PUBLIC PROPERTY GroupName AS CHARACTER NO-UNDO
      PUBLIC GET.
      PUBLIC SET.
      
   DEFINE PUBLIC VARIABLE Dragger AS Controls.Grid NO-UNDO.
   DEFINE PUBLIC VARIABLE DraggerJoin AS Controls.GridJoin NO-UNDO.
      
   DEFINE PUBLIC VARIABLE Children AS System.Collections.ArrayList NO-UNDO.
   
   CONSTRUCTOR PUBLIC DragDropGroup(INPUT gn AS CHARACTER):
      THIS-OBJECT:GroupName = gn.
      THIS-OBJECT:Children = NEW System.Collections.ArrayList().
      THIS-OBJECT:Dragger = ?.
      THIS-OBJECT:DraggerJoin = ?.
   END CONSTRUCTOR.
   
   DESTRUCTOR PUBLIC DragDropGroup():
      /*{WALLMAN\foreach.i System.Object oObject IN THIS-OBJECT:Children}
         CAST(oObject, Controls.Grid):RemoveDragDropGroup(THIS-OBJECT).
      END.*/
   END DESTRUCTOR.
   
   METHOD PUBLIC VOID AddChild(INPUT obj AS system.object):
      THIS-OBJECT:Children:Add(obj).
      
      CAST(obj, Controls.Grid):AddDragDropGroup(THIS-OBJECT).
   END METHOD.
   
   METHOD PUBLIC VOID RemoveChild(INPUT obj AS System.Object):
     THIS-OBJECT:Children:Remove(obj).
   END METHOD.
   
   METHOD PUBLIC LOGICAL IsChild(INPUT o AS system.object):
      {WALLMAN\foreach.i System.Object oObject in THIS-OBJECT:Children }
         IF o = oObject THEN DO:
            RETURN TRUE.
         END.
      END.
      RETURN FALSE.
   END METHOD.
   
END CLASS.