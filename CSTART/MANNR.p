/*MANNR.P.*/
DEFINE SHARED VARIABLE regmannamn AS CHARACTER NO-UNDO.
DEFINE SHARED VARIABLE regmnr AS INTEGER FORMAT "99" NO-UNDO.
DEFINE SHARED VARIABLE regar AS INTEGER FORMAT "99" NO-UNDO.
IF regmannamn = "januari" THEN regmnr = 1.
ELSE IF regmannamn = "februari" THEN regmnr = 2.
ELSE IF regmannamn = "mars" THEN regmnr = 3.
ELSE IF regmannamn = "april" THEN regmnr = 4.
ELSE IF regmannamn = "maj" THEN regmnr = 5.
ELSE IF regmannamn = "juni" THEN regmnr = 6.
ELSE IF regmannamn = "juli" THEN regmnr = 7.
ELSE IF regmannamn = "augusti" THEN regmnr = 8.
ELSE IF regmannamn = "september" THEN regmnr = 9.
ELSE IF regmannamn = "oktober" THEN regmnr = 10.
ELSE IF regmannamn = "november" THEN regmnr = 11.
ELSE IF regmannamn = "december" THEN regmnr = 12.
