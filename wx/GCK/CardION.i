
/*------------------------------------------------------------------------
    File        : CardION.i
    Purpose     : 

    Syntax      :

    Description : 
    Author(s)   : 
    Created     : Thu Mar 14 16:31:00 CET 2019
    Notes       :
  ----------------------------------------------------------------------*/

DEFINE TEMP-TABLE IdrottonLine NO-UNDO 
   FIELD PR AS CHARACTER LABEL "Prova-på" 
   FIELD FN AS CHARACTER LABEL "Förnamn"
   FIELD AFN AS CHARACTER LABEL "Alt. förnamn"
   FIELD EN AS CHARACTER LABEL "Efternamn"
   FIELD K AS CHARACTER LABEL "Kön"
   FIELD NAT AS CHARACTER LABEL "Nationalitet"
   FIELD IID AS CHARACTER LABEL "IdrottsID"
   FIELD FOD AS CHARACTER   LABEL "Födelsedat./Personnr"
   FIELD TM AS CHARACTER LABEL "Telefon mobil"
   FIELD EPK AS CHARACTER LABEL "E-post kontakt"
   FIELD KAC AS CHARACTER LABEL "Kontaktadress - c/o adress"
   FIELD KG AS CHARACTER LABEL "Kontaktadress - Gatuadress"
   FIELD KPNR AS CHARACTER LABEL "Kontaktadress - Postnummer"
   FIELD KPO AS CHARACTER LABEL "Kontaktadress - Postort"
   FIELD KL AS CHARACTER LABEL "Kontaktadress - Land"
   FIELD AACO AS CHARACTER LABEL "Arbetsadress - c/o"
   FIELD AGA AS CHARACTER LABEL "Arbetsadress - Gatuadress"
   FIELD APNR AS INTEGER   LABEL "Arbetsadress - Postnummer"
   FIELD APO AS CHARACTER LABEL "Arbetsadress - Postort"
   FIELD ALA AS CHARACTER LABEL "Arbetsadress - Land"
   FIELD TB AS CHARACTER LABEL "Telefon bostad"
   FIELD TA AS CHARACTER LABEL "Telefon arbete"
   FIELD EPP AS CHARACTER LABEL "E-post privat"
   FIELD EPA AS CHARACTER LABEL "E-post arbete"
   FIELD MNR AS CHARACTER LABEL "Medlemsnr."
   FIELD MDT AS DATE  LABEL "Medlem sedan"
   FIELD MDTOM AS DATE LABEL "Medlem t.o.m."
   FIELD INF AS CHARACTER LABEL "Info"
   FIELD FAM AS CHARACTER LABEL "Familj"
   FIELD FAMD AS CHARACTER LABEL "Fam.Admin"
   FIELD LGID AS CHARACTER LABEL "Lägg till GruppID"
   FIELD TGID AS CHARACTER LABEL "Ta bort GruppID"
   FIELD SKRP AS CHARACTER LABEL "Skridskonätet pin".
   DEFINE VARIABLE IdrottonLinebuff  AS HANDLE NO-UNDO.
    
   
   DEFINE TEMP-TABLE Cardskipper NO-UNDO 
   FIELD FOD AS CHARACTER   LABEL "Födelsedat B"
   FIELD FODNR AS CHARACTER   LABEL "FödelseNummer C"
   FIELD K AS CHARACTER LABEL "Kön D"
   FIELD FN AS CHARACTER LABEL "Förnamn F"
   FIELD EN AS CHARACTER LABEL "Efternamn G"
   FIELD KG AS CHARACTER LABEL "Kontaktadress - Gatuadress H"
   FIELD KPNR AS CHARACTER LABEL "Kontaktadress - Postnummer J"
   FIELD KPO AS CHARACTER LABEL "Kontaktadress - Postort K"
    
   FIELD TM AS CHARACTER LABEL "Telefon mobil S"
   FIELD EPK AS CHARACTER LABEL "E-post kontakt T"
   FIELD KAC AS CHARACTER LABEL "Kontaktadress - c/o adress".
   
   
   