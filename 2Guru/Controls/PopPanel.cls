 
 /*------------------------------------------------------------------------
    File        : PopPanel
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elprs
    Created     : Mon Sep 09 10:08:59 CEST 2013
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.


CLASS Controls.PopPanel INHERITS Infragistics.Win.Misc.UltraPanel: 
   
CONSTRUCTOR PUBLIC PopPanel():
      SUPER().
      THIS-OBJECT:InitializeComponent().
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.    
END CONSTRUCTOR.


   METHOD PRIVATE VOID InitializeComponent ( ):
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* PopPanel */
      /*  */
      THIS-OBJECT:BorderStyle = Infragistics.Win.UIElementBorderStyle:Rounded4.
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
   END METHOD.




DESTRUCTOR PUBLIC PopPanel():

END DESTRUCTOR.  

END CLASS.