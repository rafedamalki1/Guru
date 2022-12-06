 
 /*------------------------------------------------------------------------
    File        : FillEnter
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpao
    Created     : Thu Aug 27 13:39:02 CEST 2020
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Progress.Windows.Form.

BLOCK-LEVEL ON ERROR UNDO, THROW.

CLASS Modules.Beredning.FillEnter INHERITS Form: 
	
   DEFINE PRIVATE VARIABLE buttonBf AS Controls.Button NO-UNDO.
	DEFINE PRIVATE VARIABLE components AS System.ComponentModel.IContainer NO-UNDO.
   DEFINE PRIVATE VARIABLE fillinCharEf AS Controls.FillinChar NO-UNDO.

		
	CONSTRUCTOR PUBLIC FillEnter (  ):
		
		
      SUPER().
      InitializeComponent().
      THIS-OBJECT:ComponentsCollection:ADD(THIS-OBJECT:components).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.

	END CONSTRUCTOR.

	/*------------------------------------------------------------------------------
	 Purpose:
	 Notes:
	------------------------------------------------------------------------------*/
	@VisualDesigner.
	METHOD PRIVATE VOID buttonBf_Click( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
		MESSAGE " Fine working"
      VIEW-AS ALERT-BOX.
		RETURN.

	END METHOD.
   METHOD PUBLIC VOID handleClickStatic():
      THIS-OBJECT:buttonBf_Click(?,?).
   END METHOD.
	METHOD PRIVATE VOID InitializeComponent(  ):
		
      /* NOTE: The following method is automatically generated.
      
      We strongly suggest that the contents of this method only be modified using the
      Visual Designer to avoid any incompatible modifications.
      
      Modifying the contents of this method using a code editor will invalidate any support for this file. */
      THIS-OBJECT:buttonBf = NEW Controls.Button().
      THIS-OBJECT:fillinCharEf = NEW Controls.FillinChar().
      THIS-OBJECT:SuspendLayout().
      /*  */
      /* buttonBf */
      /*  */
      THIS-OBJECT:buttonBf:Location = NEW System.Drawing.Point(205, 77).
      THIS-OBJECT:buttonBf:Name = "buttonBf".
      THIS-OBJECT:buttonBf:Size = NEW System.Drawing.Size(75, 23).
      THIS-OBJECT:buttonBf:TabIndex = 10.
      THIS-OBJECT:buttonBf:Text = "Pusch".
      THIS-OBJECT:buttonBf:Click:Subscribe(THIS-OBJECT:buttonBf_Click).
      /*  */
      /* fillinCharEf */
      /*  */
      THIS-OBJECT:fillinCharEf:Location = NEW System.Drawing.Point(12, 145).
      THIS-OBJECT:fillinCharEf:Name = "fillinCharEf".
      THIS-OBJECT:fillinCharEf:Rubrik = "GuruLabel".
      THIS-OBJECT:fillinCharEf:Size = NEW System.Drawing.Size(240, 28).
      THIS-OBJECT:fillinCharEf:TabIndex = 9.
      /*  */
      /* FillEnter */
      /*  */
      THIS-OBJECT:ClientSize = NEW System.Drawing.Size(292, 266).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:buttonBf).
      THIS-OBJECT:Controls:Add(THIS-OBJECT:fillinCharEf).
      THIS-OBJECT:Name = "FillEnter".
      THIS-OBJECT:Text = "FillEnter".
      THIS-OBJECT:ResumeLayout(FALSE).
      CATCH e AS Progress.Lang.Error:
         UNDO, THROW e.
      END CATCH.
	END METHOD.

	DESTRUCTOR PUBLIC FillEnter ( ):

	END DESTRUCTOR.

END CLASS.