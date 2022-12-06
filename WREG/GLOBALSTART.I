/*GLOBALSTART.I*/
    
   Guru.Konstanter:AnvandareTTh = TEMP-TABLE anvandartemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   Guru.Konstanter:PersonalTTh = TEMP-TABLE personaltemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   Guru.Konstanter:OmradeTTh = TEMP-TABLE omrtemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   Guru.Konstanter:BorttagnaOmradeTTh = TEMP-TABLE Borttagnaomrtemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   Guru.Konstanter:BestKundTTh = TEMP-TABLE bestkundalltclass:HANDLE:DEFAULT-BUFFER-HANDLE.
   Guru.Konstanter:JurPersTTh = TEMP-TABLE jurperstemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   Guru.Konstanter:JurAvdTTh =  TEMP-TABLE judavdtemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   Guru.Konstanter:LeverantorTTh = TEMP-TABLE levtemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   
   Guru.GlobalaVariabler:MaterielTTh = TEMP-TABLE mtrltemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   Guru.GlobalaVariabler:EMaterielTTh = TEMP-TABLE emtrltemp:HANDLE:DEFAULT-BUFFER-HANDLE.
   /*DE ENR SOM PRISJMF OCH SALDO GÄLLER*/
  
   Guru.GlobalaVariabler:jmfPrisEnrTTh =  TEMP-TABLE enrjmf:HANDLE:DEFAULT-BUFFER-HANDLE.
   CREATE BUFFER Guru.GlobalaVariabler:jmfPrisEnrBuffTTh FOR TABLE Guru.GlobalaVariabler:jmfPrisEnrTTh.