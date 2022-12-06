   DEFINE VARIABLE korfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE GLOBANV AS CHARACTER NO-UNDO.
DEFINE VARIABLE utfil AS CHARACTER NO-UNDO.
DEFINE VARIABLE objnamn AS CHARACTER NO-UNDO.
GLOBANV = "cewe".
OBJNAMN = "bodil".
korfil = '"C:\program\ABBCewe\Z3\Z3.EXE"'.
IF objnamn NE "" THEN DO:
   objnamn = '''C:\program\ABBCewe\Z3\OBJEKT\' + objnamn + '.zta'''.
   korfil = korfil + ' /SOURCE=guru /OBJECT=' + objnamn + ' /FILE=' + globanv + '.txt'. 
END.      
ELSE DO:
   korfil = korfil + ' /SOURCE=guru /OBJECT='''' /FILE=' + globanv + '.txt'. 
END.
MESSAGE KORFIL VIEW-AS ALERT-BOX.
OS-COMMAND  VALUE(korfil).

/*
"C:\PROGRAM\ABBCEWE\Z3\Z3.EXE" /SOURCE=GURU /OBJECT="C:\PROGRAM\ABBCEWE\Z3\OBJEKT\BODIL.ZTA" /FILE=ELPAO.TXT
*/
