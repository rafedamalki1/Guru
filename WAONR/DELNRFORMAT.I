/*
     Filename: DELNRFORMAT.I
      Created: 2004.05.26 14:52ELPAO     
     Modified: 
*/

{&FORMATNAMN}:FORMAT IN FRAME {&FRAME-NAME} = Guru.Konstanter:varforetypchar[1].
IF {&FORMATNAMN}:WIDTH  IN FRAME {&FRAME-NAME} < LENGTH(Guru.Konstanter:varforetypchar[1]) THEN {&FORMATNAMN}:WIDTH  IN FRAME {&FRAME-NAME} = LENGTH(Guru.Konstanter:varforetypchar[1]).
{&FORMATNAMN}:LABEL IN FRAME {&FRAME-NAME} = Guru.Konstanter:gdelnrk.
