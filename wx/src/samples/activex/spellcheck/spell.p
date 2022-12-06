/* 
 * This sample demonstrates using the Office 95 WinWord or Office 97 WinWord
 * ActiveX Automation Objects. In either case, use the WinWord
 * Automation Object to check the spelling of text entered into a 
 * Progress fill-in field.
 */

DEFINE BUTTON bExit LABEL "Done" SIZE 18 BY 1.25 AUTO-GO.
DEFINE BUTTON bWord LABEL "Spell Check" SIZE 18 BY 1.25.
DEFINE VAR myText AS CHAR 
    VIEW-AS EDITOR SIZE 60 BY 5 
    LABEL "Please enter new text:" FONT 2.

/* 
 * Create an edit control that the user can enter any text into
 * Once the text is complete, they can choose to perform the Spell Check
 */

FORM myText SKIP(1) 
    SPACE(10) bWord SPACE bExit 
    WITH FRAME a VIEW-AS DIALOG-BOX THREE-D FONT 6.
    
FRAME a:TITLE = "Spelling and Grammar Check: English".

/*
 * Trigger executed when Spell Check button is pressed
 */

ON CHOOSE OF bWord IN FRAME a
DO:
    DEFINE VAR wordAppl AS COM-HANDLE.
    DEFINE VAR i AS INTEGER.

    /*
     * First we need to determine if we are running on a system with Word for 
     * Office 95 or Office 97 since the Automation Objects are dramatically 
     * different. We do this by looking for a particular Object and if it fails
     * then we try the other version. This is done by accessing the system 
     * registry
     */

    LOAD "Word.Application" BASE-KEY "HKEY_CLASSES_ROOT" NO-ERROR. /* Open Registry key */
    IF error-status:error THEN DO:
    
        /* Office 95 Word 
         *
      	  * When the Spell Check button is pressed, create an Automation Object 
      	  * for Word.Basic
	  * 
	  * Copy the text from myText and put into a new file created in Word.
	  * The spelling check is done on that Word file and all changes are put
	  * into that unnamed file. This is all done by the ToolsSpelling() method.
	  *
	  * Once that completes, we take the updated contents of the unnamed file 
	  * and copy it back into the edit control on the Progress frame. We then 
	  * close the Automation Object without saving the file.
	  */

        CREATE "Word.Basic" wordAppl.
        NO-RETURN-VALUE wordAppl:FileNew.
        ASSIGN myText.
        NO-RETURN-VALUE wordAppl:Insert(myText).
        ASSIGN i = wordAppl:ToolsSpelling NO-ERROR.
        NO-RETURN-VALUE wordAppl:AppHide("Microsoft Word").
        NO-RETURN-VALUE wordAppl:EditSelectAll.
        myText:SCREEN-VALUE = wordAppl:Selection().
        NO-RETURN-VALUE wordAppl:FileClose(2).
        NO-RETURN-VALUE wordAppl:AppClose("Microsoft Word").
    END. 

    ELSE DO: 
        UNLOAD "Word.Application". /* Close Registry key */
        /* Office 97 Word
         *
         * When the Spell Check button is pressed,create an Automation Object for 
         * Word.Application
         *
         * Copy the text from myText and put into a new document in an OLE 
         * Collection.  The spelling check is done on that document and all 
         * changes are put into that document. This is all done by the 
         * CheckGrammar() method.
         *
         * Once that completes, we take the updated contents of the document 
         * and copy it back into the edit control on the Progress frame. We then 
         * quit the Automation Object.
         */

        CREATE "Word.Application" wordAppl.
        wordAppl:Documents:Add().
        ASSIGN myText.
        wordAppl:Documents:Item(1):Range(0,0):InsertAfter(myText).
        wordAppl:Options:CheckGrammarWithSpelling = TRUE.
        wordAppl:Documents:Item(1):CheckGrammar().
        wordAppl:Visible = FALSE.
        myText:SCREEN-VALUE=wordAppl:Documents:Item(1):Range(0,wordAppl:Documents:Item(1):Characters:Count):Text.
    
        /* 
         * The following two lines demonstrate a different way to do the 
         * selection of the text.
         *       wordAppl:Documents:Item(1):Select. 
         *       myText:SCREEN-VALUE=wordAppl:Selection:Text.
         */

        wordAppl:Quit(0).
    END.
 
    RELEASE OBJECT wordAppl.
END.

ENABLE ALL WITH FRAME a.
WAIT-FOR GO OF FRAME a.
