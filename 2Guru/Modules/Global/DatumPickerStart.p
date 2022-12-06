
/*------------------------------------------------------------------------
    File        : DatumPickerStart.p
    Purpose     : 

    Syntax      :run Modules\Global\DatumPickerStart.p ().

    Description : 

    Author(s)   : 
    Created     : 
    Notes       :
  ----------------------------------------------------------------------*/

PROCEDURE DatumPicker_UI :  
   
   DEFINE VARIABLE DatumPicker   AS Modules.Global.DatumPicker NO-UNDO.
   DEFINE VARIABLE cCancel AS CHARACTER NO-UNDO.
  
   DatumPicker = NEW Modules.Global.DatumPicker().
   /* Få in pickdatum */
   WAIT-FOR DatumPicker:ShowDialog().  
   
   cCancel = DatumPicker:DialogResult:ToString().
   
   IF cCancel = "Cancel" THEN Guru.GlobalaVariabler:avbryt_musz = TRUE.   
   
   DELETE OBJECT DatumPicker NO-ERROR.
   DatumPicker = ?.
END PROCEDURE.

