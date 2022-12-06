DEFINE VARIABLE infile     AS CHARACTER FORMAT "x(60)" LABEL "Input File".
DEFINE VARIABLE intext     AS CHARACTER.
DEFINE VARIABLE next-space AS INTEGER.
DEFINE VARIABLE word       AS CHARACTER FORMAT "x(32)" LABEL "Words".
 
/* Get the name of a text file. */
SET infile.

/* Set input to the text file. */
INPUT FROM VALUE(infile).

DO WHILE TRUE:
   /* Read the next line from the file. */
   IMPORT UNFORMATTED intext.
   intext = TRIM(intext).
   
   DO WHILE TRUE:
      /* Find the next space character. If none 
         found, find the end of string.          */
      next-space = INDEX(intext, " ").
      IF next-space = 0
      THEN next-space = LENGTH(intext) + 1.
      
      /* If the string contains no (more) words,
         then read the next line.                */
      IF next-space = 1
      THEN LEAVE.
      
      /* Pull the first word off the string.
         Remove any punctuation characters around it. */  
      word = SUBSTRING(intext, 1, next-space - 1).
      word = TRIM(word, ",.;:!?~"~'[]()").
      intext = TRIM(SUBSTRING(intext, next-space + 1)).
      
      /* Display the word. */
      DISPLAY word WITH DOWN FRAME x.
      DOWN WITH FRAME x.

   END.
END.
