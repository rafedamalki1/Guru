/*xwordkomp.p*/

DEFINE VARIABLE cDocPath AS CHARACTER NO-UNDO.
DEFINE VARIABLE cDocName AS CHARACTER NO-UNDO.
{automation.i}

OpenWORD().
ASSIGN 
cDocPath = "c:\ksvedit\Markupplåtelseavtal.doc"
cDocName = "c:\ksvedit\Markupplåtelseavtal2.doc".
OS-COPY VALUE(cDocPath) VALUE(cDocName).
OpenDOC(cDocName).

pageView().

If existBookMark( "Fastighettjänande" )  THEN DO:
   GotoBookMark( "Fastighettjänande" ).
   insertText( "Fastighet nr 2").
END.
If existBookMark( "Kommuntjänande" )  THEN DO:
   GotoBookMark( "Kommuntjänande" ).
   insertText( "Umeå").
END.
If existBookMark( "Littera" )  THEN DO:
   GotoBookMark( "Littera" ).
   insertText( "234234").
END.

FileClose(INPUT TRUE).
