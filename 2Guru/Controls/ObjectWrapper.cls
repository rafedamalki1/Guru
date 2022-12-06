 
 /*------------------------------------------------------------------------
    File        : ObjectWrapper
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Tue May 24 15:48:44 CEST 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Controls.ObjectWrapper INHERITS System.Object IMPLEMENTS System.IComparable : 
   DEFINE PUBLIC PROPERTY ProgressObject AS Progress.Lang.Object NO-UNDO
   GET.      SET.  
    
   CONSTRUCTOR ObjectWrapper (obj AS Progress.Lang.Object):
      ProgressObject = obj.
   END.  
   
   CONSTRUCTOR ObjectWrapper ():

   END.

   METHOD PUBLIC INTEGER compareTo (obj AS System.Object):
      DEFINE VARIABLE resultat AS INTEGER                NO-UNDO.
      DEFINE VARIABLE comp     AS Controls.GridRubrik    NO-UNDO.
      DEFINE VARIABLE objwr    AS Controls.ObjectWrapper NO-UNDO.
      DEFINE VARIABLE thisobj AS Controls.GridRubrik NO-UNDO.
      /*thisobj*/
      thisobj = CAST(THIS-OBJECT:ProgressObject, Controls.GridRubrik).
      /*comp = inkommet*/
      objwr = CAST(obj, Controls.ObjectWrapper).
      comp = CAST(objwr:ProgressObject, Controls.GridRubrik).
      IF thisobj:Ordning = comp:Ordning THEN  RETURN 0.
      ELSE IF thisobj:Ordning < comp:Ordning THEN RETURN -1.
      ELSE  RETURN 1.
   
   END METHOD.

END CLASS.