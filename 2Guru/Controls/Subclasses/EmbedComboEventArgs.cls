 
 /*------------------------------------------------------------------------
    File        : asd
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Mon Apr 23 14:03:15 CEST 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Controls.Subclasses.EmbedComboEventArgs INHERITS System.EventArgs:

   CONSTRUCTOR PUBLIC EmbedComboEventArgs(wut AS INTEGER):
      THIS-OBJECT:Wuut = wut.
   END CONSTRUCTOR.

   DEFINE PUBLIC VARIABLE Wuut AS INTEGER NO-UNDO.

END CLASS.