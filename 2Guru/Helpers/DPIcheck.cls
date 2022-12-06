 
 /*------------------------------------------------------------------------
    File        : DPIcheck
    Purpose     : 
    Syntax      : KOLLAR VILKEN TECKENSTORLEK finns i KalkylControl men ser inte ut att göra något
    Description : 
    Author(s)   : elprs
    Created     : Thu Sep 10 15:21:22 CEST 2015
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

USING Progress.Windows.UserControl.

CLASS Helpers.DPIcheck INHERITS UserControl: 
   DEFINE PRIVATE VARIABLE g AS System.Drawing.Graphics NO-UNDO.
   DEFINE PUBLIC VARIABLE yDPI AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE xDPI AS INTEGER NO-UNDO.
   
   CONSTRUCTOR PUBLIC DPIcheck():
      THIS-OBJECT:g = THIS-OBJECT:CreateGraphics().

      THIS-OBJECT:yDPI = g:DpiY.
      THIS-OBJECT:xDPI = g:DpiX.
   END.
   
   
END CLASS.