/*
     Filename: DELNRFORMAT2.I
      Created: 2004.05.26 14:52ELPAO     
     Modified: 
*/

{&FORMATNAMN}:FORMAT = Guru.Konstanter:varforetypchar[1].
IF {&FORMATNAMN}:WIDTH  < LENGTH(Guru.Konstanter:varforetypchar[1]) THEN {&FORMATNAMN}:WIDTH  = LENGTH(Guru.Konstanter:varforetypchar[1]).
