AO-kund 	  GURU-kund
--------------------------
KUNDNR          BESTID        char        Unikt id for varje kund
NAMN            BESTNAMN      char        Kundens namn i klartext 
ADRESS          ADRESS        char        Kundens adress, gata eller dylikt

POSTNUMMER      PNR           char        Kundens Postnummer
POSTADRESS      ORT           char        Kundens Postnummer ort
KUNDTYP         KUNDTYP       char        Till exempel intern eller externkund 
                                          Vilka typer finns ? Om typ ej finns läggs kundtypen upp i Guru.                                           
                                          0 = extern, 
                                          1 = strökund 
                                          2 = intern sydkraft kund 
MOMSKOD         MOMSID        inte        Momskod.  Kod 1-8.
 
För dessa fält i AO har vi inte hittat någon koppling till GURU:

LEVERANSPLATS  Not: leveransplats är tillsammans med kundnr ID-begrepp i AO,
                    hur ska vi göra med flera leveransplatser per kund
                    vid koppling mot GURU ??
COADRESS      FAKfakadress = coadress + adress.            
MSEGM         ej aktuella 
DSEGM         ej aktuella 
HKUND         ej aktuella 
UKUND         ej aktuella 
ENHET         BOLAG 401 402, STYR DATABASEN. 
BENAMNING     ej aktuella 
RABATTBREV    Är detta samma som GROSSISTPRISLISTA  på ao så är det samma som PRISNUM
PRISREGEL     ej aktuella 
KUNDKATEGORI  ej aktuella 
FAKT_AVG      ej aktuella 
KONCERNKUND   MOTPART  Om MOTPART ej finns läggs den upp i Guru.
VAT_NUMMER    ej aktuella   
              Not: Momsregnr, är vår koppling momskod <-> momsid korrekt
              eller ska det vara vat_nummer <-> momsid istället? Koppling momskod <-> momsid är korrekt.
LEVPRIS       ej aktuella 

OBS
PRELAN        Tillfälligt stopp av fakturering av denna kund. 0 = ok eller 1 = stopp. 

För dessa fält i GURU har vi inte hittat någon källa i AO

 DATABAS       char        Fälltet utgår se fältert enhet.
                           Någon typ av styrnig för vilken databas kunden
                           hör till
                           Not: Använder ES flera databaser i GURU eller kan vi sätta
                                ett fast värde? Fler databaser.
                                Om det finns fler databaser, hur vet vi
                                vilken databas som gäller? Bra fråga ?????
 TEL           char        Telefon
 KONTAKT       char        Kontaktperson
 KUNDPRISF     deci        Kundprisfaktor Behövs ej          
 LEVADRESS     char        Leverans adress, gata eller dylikt se adress
 LEVPNR        char        Leverans Postnummer se adress
 LEVORT        char        Kundens Postnummer ort se adress
 FAKADRESS     char        Faktura adress, gata eller dylikt se coadress
 FAKPNR        char        Faktura Postnummer
 FAKORT        char        Faktura Postnummer ort
 FAXNR         char        Faxnummer
 AOKUND        logi        Alltid true (Behövs ej)
                           Not: Är true enligt spec, hur markeras detta (1/0, TRUE/FALSE eller på annat sätt) ?
                           Svar TRUE/FALSE.

 START%        inte        Fastpris fakturering start procent (Behövs ej)       
 SLUT%         inte        Fastpris fakturering slut procent (Behövs ej)
 FAKINT        inte        Antal dagar mellan fakturering  (Behövs ej)          
 Vi flyttar följande uppgifter till Gurus prislister för normaltid.
 MIL           deci        Ersättning för mil kr/mil (Behövs ej)
 ENTRAK        deci        Ersättning för endagsförätning i kr (Behövs ej)
 FLERTRAK      deci        Ersättning för flerdyngsförätning i kr (Behövs ej)
 MTRLPA        deci        Materialkostnadspåslag i procent. Kan vara negativt (Behövs ej)     
 FRTJPA        deci        Påslag för främmande tjänst i procent. Kan vara negativt (Behövs ej) 
 
 FDAGAR        inte        Antal förfallodagar defult 30 dagar.
 MOTPARTID     inte        Motpart se ovan.
 PRISNUM       inte        Prislista se ovan.

             Not: När det gäller fält som inte kopplas kan vi göra på tre sätt:
                  1. lämna fältet helt tomt och låta GURU sätta ett standardvärde,
                  2. Vi anger ett fast värde
                  3. vi härleder ett värde utifrån andra fält i AO-basen
                  Vilken metod ska vi välja för de olika fälten?

Övriga frågor och noteringar: 
1. Avgränsande tecken, $ (dollar) är föreslaget, vi kommer att rensa alla
   fält från detta tecken innan vi skickar till GURU så att vi inte få konflikt.
   Är $ acceptabelt elller förekommer det i namn och adressfält? Ok.
2. CR/LF: eftersom detta hamnar i en ASCII-fil där CR/LF är postavgränsare måste
   vi plocka bort dessa från ingående data. Vad jag ser är detta inget problem.
3. Code page: vi kan välja den codepage som önskas, data är ursprungligen i
   ISO8859-1. Ok.
   Ska info om codepage finnas med i filen eller är det fastställt för hela
   systemet? Nej
4. Decimalpunkt/komma: vilken decimalavskiljare ska användas, punkt eller
   komma? Punkt.
5. Vi antar att bara nyupplägg och ändringar förs över, om borttag
   förekommer i AO för vi inte över dessa till GURU, OK? 
   Ok. Om bortag bör endå posten finnas kvar i Guru. 
   Annars får man problem med gamla fakturor.
   Vi gör inte heller någon skillnad på nyupplägg och ändring utan antar att
   GURU fixar detta. OK.
   Om kundnr återanväns måste info om ny kund. 
 


Order Field Name    Data Type   AO-KUND
----- ------------- ----------- -------------------------------
    0 ENHET         char        ENHET 
   10 BESTID        char        KUNDNR
   20 BESTNAMN      char        NAMN 
   30 ADRESS        char        ADRESS   
   40 PNR           char        POSTNUMMER
   50 TEL           char        Behövs ej
   60 ORT           char        POSTADRESS
   70 KONTAKT       char        Behövs ej
   80 KUNDPRISF     deci        Behövs ej          
   90 LEVADRESS     char        Behövs ej
  100 LEVPNR        char        Behövs ej
  110 LEVORT        char        Behövs ej
  120 FAKADRESS     char        COADRESS + ADRESS
  130 FAKPNR        char        POSTNUMMER
  140 FAKORT        char        POSTNUMMER
  150 FAXNR         char        Behövs ej
  160 AOKUND        logi        Behövs ej 
  170 START%        inte        Behövs ej        
  180 SLUT%         inte        Behövs ej
  190 FAKINT        inte        Grundvärde 30            
  200 MIL           deci        Behövs ej
  210 ENTRAK        deci        Behövs ej
  220 FLERTRAK      deci        Behövs ej
  230 MTRLPA        deci        Behövs ej     
  240 FRTJPA        deci        Behövs ej
  250 FDAGAR        inte        Grundvärde 30
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
 FAKINT        inte        Grundvärde 30            
 FDAGAR        inte        Grundvärde 30
 MOTPARTID     inte        KONCERNKUND
 MOMSID        inte        Momskod
 KUNDTYP       char        KUNDTYP
 PRISNUM       inte        RABATTBREV   
 PRELAN        INTEGER     PRELAN  O eller 1.
 
 //Anders 
