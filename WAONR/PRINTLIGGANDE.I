
/*PRINTLIGGANDE.I */

IF System.Environment:OSVersion:Version:Major <= 5 THEN OUTPUT TO PRINTER NUM-COPIES VALUE(Guru.SharedVariable:iCopies) LANDSCAPE PAGE-SIZE VALUE(Guru.GlobalaVariabler:globsidl) CONVERT TARGET "iso8859-1". 
ELSE OUTPUT TO PRINTER  NUM-COPIES VALUE(Guru.SharedVariable:iCopies) LANDSCAPE PAGE-SIZE VALUE(Guru.GlobalaVariabler:globsidl) CONVERT TARGET "iso8859-1". 
Guru.SharedVariable:iCopies = 1.
