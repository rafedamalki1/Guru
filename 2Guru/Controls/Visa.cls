 
 /*------------------------------------------------------------------------
    File        : Visa
    Purpose     : Visa PDFer/Bilder/Textfiler
    Syntax      : 
    Description : 
    Author(s)   : elpkl
    Created     : Thu Dec 15 14:14:15 CET 2011
    Notes       : Navigation genom klick p? hyperl?nkar till?ts ej
                  Adobe Reader kr?vs f?r PDF-visning, antagligen fungerar
                  andra PDF-readers ocks?. Allt som kan visas MHA en vanlig
                  web-browser som t.ex. Internet Explorer b?r fungera att ?ppna.
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

CLASS Controls.Visa INHERITS System.Windows.Forms.WebBrowser:
	/*------------------------------------------------------------------------------
			Purpose:  																	  
			Notes:  																	  
	------------------------------------------------------------------------------*/
   DEFINE VARIABLE navigera AS LOGICAL INITIAL FALSE NO-UNDO.
   
	CONSTRUCTOR PUBLIC Visa (  ):
		SUPER ().
		
      THIS-OBJECT:Navigating:Subscribe(THIS-OBJECT:Navigerar).
      THIS-OBJECT:Navigated:Subscribe(THIS-OBJECT:Navigerat).
	END CONSTRUCTOR.
	
	/* Visar t.ex. "c:\test.pdf" eller "c:\apa.gif" eller "c:\readme.txt" */
	METHOD PUBLIC VOID GuruVisa(sokvag AS CHARACTER):
	   navigera = TRUE.
	   THIS-OBJECT:Navigate(sokvag, FALSE).
	END METHOD.
	
	/* F?rbjuder navigation efter att det som visats ?r f?rdigladdat*/
	METHOD PRIVATE VOID Navigerat( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.WebBrowserNavigatedEventArgs ):
      navigera = FALSE. 
      RETURN.

   END METHOD.
	
	/*till?t ej navigering f?rutom n?r metoden GuruVisa anropas. Annars kan man klicka p? l?nkar i PDF:er */
	METHOD PRIVATE VOID Navigerar( INPUT sender AS System.Object, INPUT e AS System.Windows.Forms.WebBrowserNavigatingEventArgs ):
      IF navigera = FALSE THEN
         e:Cancel = TRUE.     
      RETURN.
            
   END METHOD.
	

	DESTRUCTOR PUBLIC Visa ( ):

	END DESTRUCTOR.

END CLASS.