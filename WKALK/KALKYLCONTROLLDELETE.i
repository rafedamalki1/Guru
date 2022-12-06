
/*------------------------------------------------------------------------
    File        : KALKYLCONTROLLDELETE.i
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : elpao
    Created     : Tue Jan 08 09:20:52 CET 2013
    Notes       :
  ----------------------------------------------------------------------*/
   DELETE OBJECT THIS-OBJECT:ControlShell NO-ERROR. 
      ControlShell = ?.
      DELETE OBJECT THIS-OBJECT:HuvudTTh              NO-ERROR.
   DELETE OBJECT THIS-OBJECT:IngKatalogTTh         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:KatalogTTh            NO-ERROR.
   DELETE OBJECT THIS-OBJECT:Katalogsubtth         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:KatalogDeltth         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:PriserTTh             NO-ERROR.
   DELETE OBJECT THIS-OBJECT:VisningTTh            NO-ERROR.
   DELETE OBJECT THIS-OBJECT:ValdaPriserTTh        NO-ERROR.
   DELETE OBJECT THIS-OBJECT:ArbetskoderTTh        NO-ERROR.
   DELETE OBJECT THIS-OBJECT:LopposterTTh          NO-ERROR.
   DELETE OBJECT THIS-OBJECT:LopsubTTh             NO-ERROR.
   DELETE OBJECT THIS-OBJECT:KoderTTh              NO-ERROR.
   DELETE OBJECT THIS-OBJECT:MarkningTTh           NO-ERROR.
   DELETE OBJECT THIS-OBJECT:kalkantalTTh          NO-ERROR.
   DELETE OBJECT THIS-OBJECT:kalkkostnadTTh        NO-ERROR.
   DELETE OBJECT THIS-OBJECT:KalkRubrikTTh         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:EgnaPriserTTh         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:FaktorerTTh           NO-ERROR.
   DELETE OBJECT THIS-OBJECT:kalkaonrTTh           NO-ERROR.
   DELETE OBJECT THIS-OBJECT:Avtalskalktth         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:AvtalKodertth         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:BerAvtalKodertth         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:BerAvtalKodersumtth         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:SchAvtalKodertth         NO-ERROR.
   DELETE OBJECT THIS-OBJECT:kalktmtrlTTh          NO-ERROR.
   /*
   DELETE OBJECT THIS-OBJECT:KalkylimportTTh       NO-ERROR.
   */
   DELETE OBJECT THIS-OBJECT:KaladmimportTTh       NO-ERROR.
   DELETE OBJECT THIS-OBJECT:KalkmallHuvudtth      NO-ERROR.
   DELETE OBJECT THIS-OBJECT:KalkmallKodertth      NO-ERROR.
   DELETE OBJECT THIS-OBJECT:KalkmallValdtth       NO-ERROR.
   DELETE OBJECT THIS-OBJECT:VolymberTTh           NO-ERROR.
   DELETE OBJECT THIS-OBJECT:VolymberTTbuffh           NO-ERROR.
   
   DELETE OBJECT THIS-OBJECT:FrekvensTTh           NO-ERROR.
   DELETE OBJECT THIS-OBJECT:tiduth                NO-ERROR.
   DELETE OBJECT THIS-OBJECT:berkalkmtrltth        NO-ERROR.
   DELETE OBJECT THIS-OBJECT:kalkanvtth            NO-ERROR.
   DELETE OBJECT THIS-OBJECT:kalkttidlageTTh       NO-ERROR.

HuvudTTh              = ?.  
IngKatalogTTh         = ?.
KatalogTTh            = ?.    
Katalogsubtth         = ?.    
KatalogDeltth         = ?.    
PriserTTh             = ?.    
VisningTTh            = ?.    
ValdaPriserTTh        = ?.    
ArbetskoderTTh        = ?.    
LopposterTTh          = ?.    
LopsubTTh             = ?.    
KoderTTh              = ?.    
MarkningTTh           = ?.    
kalkantalTTh          = ?.    
kalkkostnadTTh        = ?.    
KalkRubrikTTh         = ?.    
EgnaPriserTTh         = ?.    
FaktorerTTh           = ?.    
kalkaonrTTh           = ?.    
Avtalskalktth         = ?.    
AvtalKodertth         = ?.
BerAvtalKodertth         = ?.
SchAvtalKodertth         = ?.    
kalktmtrlTTh          = ?.
/*    
KalkylimportTTh       = ?.    
*/
KaladmimportTTh       = ?.    
KalkmallHuvudtth      = ?.    
KalkmallKodertth      = ?.    
KalkmallValdtth       = ?.    
VolymberTTh           = ?.  
VolymberTTbuffh       = ?.  
FrekvensTTh           = ?.    
tiduth                = ?.    
berkalkmtrltth        = ?.    
kalkanvtth            = ?.    
kalkttidlageTTh       = ?.    
   
      
      