/*xwordkomp.p*/

DEFINE VARIABLE cDocPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDocName AS CHARACTER NO-UNDO.
{automation.i}

OpenWORD().
ASSIGN 
cDocPath = "c:\ksvedit\Markuppl�telseavtal.doc"
cDocName = "c:\ksvedit\Markuppl�telseavtal2.doc".
OS-COPY VALUE(cDocPath) VALUE(cDocName).
OpenDOC(cDocName).

pageView().

If existBookMark( "Fastighettj�nande" )  THEN DO:
   GotoBookMark( "Fastighettj�nande" ).
   insertText( "Fastighet nr 2").
END.
If existBookMark( "Kommuntj�nande" )  THEN DO:
   GotoBookMark( "Kommuntj�nande" ).
   insertText( "Ume�").
END.
If existBookMark( "Littera" )  THEN DO:
   GotoBookMark( "Littera" ).
   insertText( "234234").
END.

FileClose(INPUT TRUE).
