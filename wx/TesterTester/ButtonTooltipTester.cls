 
 /*------------------------------------------------------------------------
    File        : ButtonTooltipTester
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Thu Nov 13 12:55:45 CET 2014
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS TesterTester.ButtonTooltipTester INHERITS Form: 
	
   DEFINE PUBLIC VARIABLE button1 AS Controls.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.

		
	CONSTRUCTOR PUBLIC ButtonTooltipTester (  ):
		
		
        SUPER().
        InitializeComponent().
        THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
        CATCH e AS Progress.Lang.Error:
            UNDO, THROW e.
        END CATCH.

	END CONSTRUCTOR.

	METHOD PRIVATE VOID InitializeComponent(  ):
		
        /* NOTE: The following method is automatically generated.
        
        We strongly suggest that the contents of this method only be modified using the
        Visual Designer to avoid any incompatible modifications.
        
        Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:button1 = NEW Controls.Button().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* button1 */
      /*  */
      THIS-OBJECT:button1:GuruTooltip = "erter".
      THIS-OBJECT:button1:Location = NEW System.Drawing.Point(12, 12).
      THIS-OBJECT:button1:Name = "button1".
      THIS-OBJECT:button1:Size = NEW System.Drawing.Size(172, 144).
      THIS-OBJECT:button1:TabIndex = 0.
      THIS-OBJECT:button1:Text = "button1".
      /*  */
      /* ButtonTooltipTester */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:button1).
      THIS-OBJECT:Name = "ButtonTooltipTester".
      THIS-OBJECT:Text = "ButtonTooltipTester".
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC ButtonTooltipTester ( ):

	END DESTRUCTOR.

END CLASS.