 
 /*------------------------------------------------------------------------
    File        : TextDate
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Wed Nov 30 14:15:01 CET 2011
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.
USING Infragistics.Win.UltraWinSchedule.UltraCalendarCombo.



CLASS Controls.TextDate INHERITS UltraCalendarCombo: 
	/*------------------------------------------------------------------------------
			Purpose: Datumväljare!															  
			Notes: Ingen textbox, heter bara som den gör pga att den används ofta
			       tillsammans med de andra TextClasserna										  
	------------------------------------------------------------------------------*/
	DEFINE PUBLIC PROPERTY Datum AS DATE NO-UNDO
	GET.
	SET.
	
	
	CONSTRUCTOR PUBLIC TextDate (  ):
		SUPER ().
      THIS-OBJECT:ValueChanged:Subscribe(THIS-OBJECT:ValueCh).
      
	END CONSTRUCTOR.

   /* 
   this-object:Value är av .net typen DateTime vilken har formatet ungefär 2011-11-30 00:00:00 
   så det blir besvärligt att använda den variabeln tillsamans med progress-datatyper.   
   */
   METHOD PRIVATE VOID ValueCh( INPUT sender AS System.Object, INPUT e AS System.EventArgs ):
      THIS-OBJECT:Datum = THIS-OBJECT:Value.            
      RETURN.

   END METHOD.

	DESTRUCTOR PUBLIC TextDate ( ):

	END DESTRUCTOR.

END CLASS.