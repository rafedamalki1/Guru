AO-kund 	  GURU-kund
--------------------------
KUNDNR          BESTID        char        Unikt id for varje kund
NAMN            BESTNAMN      char        Kundens namn i klartext 
ADRESS          ADRESS        char        Kundens adress, gata eller dylikt

POSTNUMMER      PNR           char        Kundens Postnummer
POSTADRESS      ORT           char        Kundens Postnummer ort
KUNDTYP         KUNDTYP       char        Till exempel intern eller externkund 
                                          Vilka typer finns ? Om typ ej finns l�ggs kundtypen upp i Guru.                                           
                                          0 = extern, 
                                          1 = str�kund 
                                          2 = intern sydkraft kund 
MOMSKOD         MOMSID        inte        Momskod.  Kod 1-8.
 
F�r dessa f�lt i AO har vi inte hittat n�gon koppling till GURU:

LEVERANSPLATS  Not: leveransplats �r tillsammans med kundnr ID-begrepp i AO,
                    hur ska vi g�ra med flera leveransplatser per kund
                    vid koppling mot GURU ??
COADRESS      FAKfakadress = coadress + adress.            
MSEGM         ej aktuella 
DSEGM         ej aktuella 
HKUND         ej aktuella 
UKUND         ej aktuella 
ENHET         BOLAG 401 402, STYR DATABASEN. 
BENAMNING     ej aktuella 
RABATTBREV    �r detta samma som GROSSISTPRISLISTA  p� ao s� �r det samma som PRISNUM
PRISREGEL     ej aktuella 
KUNDKATEGORI  ej aktuella 
FAKT_AVG      ej aktuella 
KONCERNKUND   MOTPART  Om MOTPART ej finns l�ggs den upp i Guru.
VAT_NUMMER    ej aktuella   
              Not: Momsregnr, �r v�r koppling momskod <-> momsid korrekt
              eller ska det vara vat_nummer <-> momsid ist�llet? Koppling momskod <-> momsid �r korrekt.
LEVPRIS       ej aktuella 

OBS
PRELAN        Tillf�lligt stopp av fakturering av denna kund. 0 = ok eller 1 = stopp. 

F�r dessa f�lt i GURU har vi inte hittat n�gon k�lla i AO

 DATABAS       char        F�lltet utg�r se f�ltert enhet.
                           N�gon typ av styrnig f�r vilken databas kunden
                           h�r till
                           Not: Anv�nder ES flera databaser i GURU eller kan vi s�tta
                                ett fast v�rde? Fler databaser.
                                Om det finns fler databaser, hur vet vi
                                vilken databas som g�ller? Bra fr�ga ?????
 TEL           char        Telefon
 KONTAKT       char        Kontaktperson
 KUNDPRISF     deci        Kundprisfaktor Beh�vs ej          
 LEVADRESS     char        Leverans adress, gata eller dylikt se adress
 LEVPNR        char        Leverans Postnummer se adress
 LEVORT        char        Kundens Postnummer ort se adress
 FAKADRESS     char        Faktura adress, gata eller dylikt se coadress
 FAKPNR        char        Faktura Postnummer
 FAKORT        char        Faktura Postnummer ort
 FAXNR         char        Faxnummer
 AOKUND        logi        Alltid true (Beh�vs ej)
                           Not: �r true enligt spec, hur markeras detta (1/0, TRUE/FALSE eller p� annat s�tt) ?
                           Svar TRUE/FALSE.

 START%        inte        Fastpris fakturering start procent (Beh�vs ej)       
 SLUT%         inte        Fastpris fakturering slut procent (Beh�vs ej)
 FAKINT        inte        Antal dagar mellan fakturering  (Beh�vs ej)          
 Vi flyttar f�ljande uppgifter till Gurus prislister f�r normaltid.
 MIL           deci        Ers�ttning f�r mil kr/mil (Beh�vs ej)
 ENTRAK        deci        Ers�ttning f�r endagsf�r�tning i kr (Beh�vs ej)
 FLERTRAK      deci        Ers�ttning f�r flerdyngsf�r�tning i kr (Beh�vs ej)
 MTRLPA        deci        Materialkostnadsp�slag i procent. Kan vara negativt (Beh�vs ej)     
 FRTJPA        deci        P�slag f�r fr�mmande tj�nst i procent. Kan vara negativt (Beh�vs ej) 
 
 FDAGAR        inte        Antal f�rfallodagar defult 30 dagar.
 MOTPARTID     inte        Motpart se ovan.
 PRISNUM       inte        Prislista se ovan.

             Not: N�r det g�ller f�lt som inte kopplas kan vi g�ra p� tre s�tt:
                  1. l�mna f�ltet helt tomt och l�ta GURU s�tta ett standardv�rde,
                  2. Vi anger ett fast v�rde
                  3. vi h�rleder ett v�rde utifr�n andra f�lt i AO-basen
                  Vilken metod ska vi v�lja f�r de olika f�lten?

�vriga fr�gor och noteringar: 
1. Avgr�nsande tecken, $ (dollar) �r f�reslaget, vi kommer att rensa alla
   f�lt fr�n detta tecken innan vi skickar till GURU s� att vi inte f� konflikt.
   �r $ acceptabelt elller f�rekommer det i namn och adressf�lt? Ok.
2. CR/LF: eftersom detta hamnar i en ASCII-fil d�r CR/LF �r postavgr�nsare m�ste
   vi plocka bort dessa fr�n ing�ende data. Vad jag ser �r detta inget problem.
3. Code page: vi kan v�lja den codepage som �nskas, data �r ursprungligen i
   ISO8859-1. Ok.
   Ska info om codepage finnas med i filen eller �r det fastst�llt f�r hela
   systemet? Nej
4. Decimalpunkt/komma: vilken decimalavskiljare ska anv�ndas, punkt eller
   komma? Punkt.
5. Vi antar att bara nyuppl�gg och �ndringar f�rs �ver, om borttag
   f�rekommer i AO f�r vi inte �ver dessa till GURU, OK? 
   Ok. Om bortag b�r end� posten finnas kvar i Guru. 
   Annars f�r man problem med gamla fakturor.
   Vi g�r inte heller n�gon skillnad p� nyuppl�gg och �ndring utan antar att
   GURU fixar detta. OK.
   Om kundnr �teranv�ns m�ste info om ny kund. 
 


Order Field Name    Data Type   AO-KUND
----- ------------- ----------- -------------------------------
    0 ENHET         char        ENHET 
   10 BESTID        char        KUNDNR
   20 BESTNAMN      char        NAMN 
   30 ADRESS        char        ADRESS   
   40 PNR           char        POSTNUMMER
   50 TEL           char        Beh�vs ej
   60 ORT           char        POSTADRESS
   70 KONTAKT       char        Beh�vs ej
   80 KUNDPRISF     deci        Beh�vs ej          
   90 LEVADRESS     char        Beh�vs ej
  100 LEVPNR        char        Beh�vs ej
  110 LEVORT        char        Beh�vs ej
  120 FAKADRESS     char        COADRESS + ADRESS
  130 FAKPNR        char        POSTNUMMER
  140 FAKORT        char        POSTNUMMER
  150 FAXNR         char        Beh�vs ej
  160 AOKUND        logi        Beh�vs ej 
  170 START%        inte        Beh�vs ej        
  180 SLUT%         inte        Beh�vs ej
  190 FAKINT        inte        Grundv�rde 30            
  200 MIL           deci        Beh�vs ej
  210 ENTRAK        deci        Beh�vs ej
  220 FLERTRAK      deci        Beh�vs ej
  230 MTRLPA        deci        Beh�vs ej     
  240 FRTJPA        deci        Beh�vs ej
  250 FDAGAR        inte        Grundv�rde 30
  260 MOTPARTID     inte        KONCERNKUND
  270 MOMSID        inte        Momskod
  280 KUNDTYP       char        KUNDTYP
  290 PRISNUM       inte        RABATTBREV   
  300 PRELAN        INTEGER     PRELAN  O eller 1.
  
  DET SOM BLIR KVAR AV FILEN:       
 Field Name    Data Type   AO-KUND
 ------------- ----------- -------------------------------
 ENHET         char        ENHET 
 BESTID        char        KUNDNR
 BESTNAMN      char        NAMN 
 ADRESS        char        ADRESS   
 PNR           char        POSTNUMMER
 ORT           char        POSTADRESS
 FAKADRESS     char        COADRESS + ADRESS
 FAKPNR        char        POSTNUMMER
 FAKORT        char        POSTNUMMER
 FAKINT        inte        Grundv�rde 30            
 FDAGAR        inte        Grundv�rde 30
 MOTPARTID     inte        KONCERNKUND
 MOMSID        inte        Momskod
 KUNDTYP       char        KUNDTYP
 PRISNUM       inte        RABATTBREV   
 PRELAN        INTEGER     PRELAN  O eller 1.
 
 //Anders 
