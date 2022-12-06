
/*PRINTSTAENDE.I 
{PRINTSTAENDE.I}
{PRINTLIGGANDE.I} 
*/


IF System.Environment:OSVersion:Version:Major <= 5 THEN OUTPUT TO PRINTER NUM-COPIES VALUE(Guru.SharedVariable:iCopies) PORTRAIT PAGE-SIZE VALUE(Guru.GlobalaVariabler:globsids) CONVERT TARGET "iso8859-1".
ELSE OUTPUT TO PRINTER  NUM-COPIES VALUE(Guru.SharedVariable:iCopies) PORTRAIT PAGE-SIZE VALUE(Guru.GlobalaVariabler:globsids) CONVERT TARGET "iso8859-1".
Guru.SharedVariable:iCopies = 1.