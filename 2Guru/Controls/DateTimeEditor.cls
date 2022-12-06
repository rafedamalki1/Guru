 
 /*------------------------------------------------------------------------
    File        : DateTimeEditor
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Thu Jul 07 11:33:20 CEST 2016
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinEditors.UltraDateTimeEditor.

CLASS Controls.DateTimeEditor INHERITS UltraDateTimeEditor: 
      
  
   CONSTRUCTOR PUBLIC DateTimeEditor():
      SUPER().
      THIS-OBJECT:ValjMask("99-99-9999 HH:MM:SS").
   END CONSTRUCTOR.
   
   METHOD PUBLIC VOID ValjMask(mask AS CHARACTER):
      THIS-OBJECT:MaskInput = mask.
   END METHOD. 
   
   
   DESTRUCTOR PUBLIC DateTimeEditor():
      
   END DESTRUCTOR.
   
END CLASS.