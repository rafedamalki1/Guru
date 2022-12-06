/*MANNAMN.P.*/
DEFINE SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
IF regmnr = 1 THEN regmannamn = "januari".
ELSE IF regmnr = 2 THEN regmannamn = "februari".
ELSE IF regmnr = 3 THEN regmannamn = "mars". 
ELSE IF regmnr = 4 THEN regmannamn = "april". 
ELSE IF regmnr = 5 THEN regmannamn = "maj". 
ELSE IF regmnr = 6 THEN regmannamn = "juni".
ELSE IF regmnr = 7 THEN regmannamn = "juli".
ELSE IF regmnr = 8 THEN regmannamn = "augusti". 
ELSE IF regmnr = 9 THEN regmannamn = "september".
ELSE IF regmnr = 10 THEN regmannamn = "oktober". 
ELSE IF regmnr = 11 THEN regmannamn = "november".
ELSE IF regmnr = 12 THEN regmannamn = "december".
