DEFINE VAR myText AS CHAR. 
DEFINE VARIABLE wordAppl AS COM-HANDLE NO-UNDO.
      DEFINE VARIABLE strang AS CHARACTER NO-UNDO.
      DEFINE VARIABLE kommando AS CHARACTER NO-UNDO.
       
    CREATE "Word.Application" wordAppl.
    wordAppl:Documents:Add().
    FIND FIRST personaltab WHERE personaltab.personalkod = "ao" NO-LOCK NO-ERROR.
    ASSIGN myText = "anders Olsson".
    wordAppl:Documents:Item(1):Range(0,0):InsertAfter(myText).
    wordAppl:Documents:Item(1):Range(0,wordAppl:Documents:Item(1):Characters:Count):font:NAME = "Humanst521 Cn BT".
    wordAppl:Visible = true.
    
    /*myText:SCREEN-VALUE=wordAppl:Documents:Item(1):Range(0,wordAppl:Documents:Item(1):Characters:Count):Text.
    
    pause.*/

/*        wordAppl:Quit(0).*/
    
        RELEASE OBJECT wordAppl.
