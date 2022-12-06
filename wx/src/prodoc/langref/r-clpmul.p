/* r-clpmul.p */

DEFINE VARIABLE i AS INTEGER.
DEFINE VARIABLE ClipBuffer AS CHARACTER VIEW-AS EDITOR SIZE 60 BY 5.
DEFINE VARIABLE ClipItem AS CHARACTER.

/* Copy rows of integer items to the clipboard */
/* and display the clipboard value.            */
CLIPBOARD:MULTIPLE=TRUE.
CLIPBOARD:ITEMS-PER-ROW=5.
REPEAT i = 1 TO 20: 
   CLIPBOARD:VALUE=STRING(i).
END.
CLIPBOARD:MULTIPLE=FALSE.
ClipBuffer = CLIPBOARD:VALUE.
ENABLE ClipBuffer WITH FRAME A.
DISPLAY SPACE(1) ClipBuffer LABEL "Clipboard Data" WITH FRAME A.
PAUSE.
	
/* Display each item of the clipboard value. */
CLIPBOARD:MULTIPLE=TRUE.
ClipItem="".
REPEAT WHILE ClipItem <> ?:
   ClipItem=CLIPBOARD:VALUE.
   IF ClipItem <> ? THEN
      DISPLAY SPACE(1) ClipItem 
         FORMAT "x(16)" LABEL "Clipboard Item" WITH DOWN FRAME B.
END.
CLIPBOARD:MULTIPLE=FALSE.