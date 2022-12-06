
/*------------------------------------------------------------------------
    File        : KALKYLKATTTH.i
    Purpose     : 


    Syntax      :
Temp-table object handle
A handle to a temp-table object. A temp-table object handle corresponds to an underlying ABL temp-table, 
which can be static or dynamic. A static temp-table is one you define at compile time with the 
DEFINE TEMP-TABLE statement. A dynamic temp-table is one you create at run time with the CREATE TEMP-TABLE statement.
DEFAULT-BUFFER-HANDLE attribute
Like static temp-tables, every dynamic temp-table is created with at least one buffer. 
This buffer’s object handle is returned by this attribute. 
DEFAULT-BUFFER-HANDLE cannot be called until the TEMP-TABLE-PREPARE( ) method has been called, 
since the default buffer is not created until then.    
    
    Description : 

    Author(s)   : elpao
    Created     : Tue Jan 08 09:10:55 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
      ArbetskoderTTh = TEMP-TABLE kalkylarbkodertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      AvtalKodertth = TEMP-TABLE AvtalKodertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      BerAvtalKodertth = TEMP-TABLE BerAvtalKodertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      BerAvtalKodersumtth = TEMP-TABLE BerAvtalKodersumtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      SchAvtalKodertth = TEMP-TABLE SchAvtalKodertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      Avtalskalktth = TEMP-TABLE Avtalskalktt:HANDLE:DEFAULT-BUFFER-HANDLE.
      FrekvensTTh = TEMP-TABLE frekvenstemp:HANDLE:DEFAULT-BUFFER-HANDLE.  
      KalkmallHuvudtth = TEMP-TABLE KalkmallHuvudtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      KalkmallKodertth = TEMP-TABLE KalkmallKodertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      KalkmallValdtth = TEMP-TABLE KalkmallValdtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      KalkylimportTTh = TEMP-TABLE KalkylimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      KaladmimportTTh = TEMP-TABLE KaladmimportTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      KatalogTTh = TEMP-TABLE kalkylkatalogtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      IngKatalogTTh = TEMP-TABLE ingkalkylkatalogtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      Katalogsubtth = TEMP-TABLE kalkylkatalogsubtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      KatalogDeltth = TEMP-TABLE kalkyldelkatalogtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      
      HuvudTTh = TEMP-TABLE kalkhuvtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      kalkaonrTTh = TEMP-TABLE kalkaonrTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      KoderTTh = TEMP-TABLE kalknumtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      ValdaPriserTTh = TEMP-TABLE kalknumsubtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      FaktorerTTh = TEMP-TABLE kalkfaktorertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      EgnaPriserTTh = TEMP-TABLE kalkegnaprisertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      kalktmtrlTTh =  TEMP-TABLE kalktmtrlTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      kalkttidlageTTh = TEMP-TABLE kalkttidlageTT:HANDLE:DEFAULT-BUFFER-HANDLE.
      
      
      
      LopposterTTh = TEMP-TABLE kalkylloppostertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      LopsubTTh = TEMP-TABLE kalkyllopsubtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      MarkningTTh = TEMP-TABLE markfiltertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      PriserTTh = TEMP-TABLE kalkylprisertt:HANDLE:DEFAULT-BUFFER-HANDLE.
      
      VisningTTh = TEMP-TABLE kalkvisningtt:HANDLE:DEFAULT-BUFFER-HANDLE.
     
      tiduth = TEMP-TABLE tidut:HANDLE:DEFAULT-BUFFER-HANDLE.
      berkalkmtrltth = TEMP-TABLE berkalkmtrltt:HANDLE:DEFAULT-BUFFER-HANDLE.
      kalkanvtth = TEMP-TABLE kalkanvtt:HANDLE:DEFAULT-BUFFER-HANDLE.
      
    