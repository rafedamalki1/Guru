
/*------------------------------------------------------------------------
    File        : FAKTMAIL.I
    Purpose     : 

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Tue Aug 11 17:01:55 CEST 2015
    Notes       :
  ----------------------------------------------------------------------*/

EMPTY TEMP-TABLE faktmail NO-ERROR. 
EMPTY TEMP-TABLE Epeppol NO-ERROR.
/*
 DEFINE INPUT  PARAMETER Pnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER Besk AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER eget  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER vatnr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER glnnr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER org AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER buy AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER pep AS CHARACTER NO-UNDO.
   */
         
                /*FAKTNR         BESTNAMN          EGETNAMN                 VAT                    GLN                    ORGNR             BUYREF            PEPPOL*/
RUN Peppol_UI (INPUT 42,INPUT "MittSverigeVatten",INPUT "MittSverigeVatten",INPUT "",INPUT "",INPUT  "556661-8756",INPUT  "",INPUT "").
RUN Peppol_UI (INPUT 39,INPUT "Oxelö Energi AB",INPUT "Oxelö Energi AB",INPUT "",INPUT "7340109100694",INPUT  "556070-9429",INPUT  "",INPUT "").
RUN Peppol_UI (INPUT 82,INPUT "Kraftringen",INPUT "Kraftringen",INPUT "SE556100985201",INPUT "7350060170544",INPUT  "556100-9852",INPUT  "100496-10",INPUT "5561009852").

RUN Peppol_UI (INPUT 14,INPUT "Umeå Energi Elnät AB",INPUT "Umeå Energi Elnät AB",INPUT "SE556086822501",INPUT    "7365560868228",INPUT  "",INPUT  "",INPUT "0007:5560868225").
RUN Peppol_UI (INPUT 99,INPUT "Umeå Energi Umenet AB",INPUT "Umeå Energi Umenet AB",INPUT "SE556619305701",INPUT  "7365566193058",INPUT  "",INPUT  "",INPUT "0007:5566193057").
RUN Peppol_UI (INPUT 125,INPUT "Akeab",INPUT "Akeab",INPUT "SE556764349801",INPUT  "7365567643491",INPUT  "556764-3498",INPUT  "",INPUT "").

RUN Peppol_UI (INPUT 40,INPUT "Bodens Energi Nät AB",INPUT "Bodens Energi Nät AB",INPUT "",INPUT  "7365565268566",INPUT  "",INPUT  "",INPUT "").


/* DE KAN INTE LÄSA 
RUN Peppol_UI (INPUT 36,INPUT "Infratek Sverige AB",INPUT "Infratek Sverige AB",INPUT "",INPUT  "007556702693401",INPUT  "556086-8225",INPUT  "",INPUT "").

*/
/*
RUN pdffakt_UI (INPUT 14, INPUT "Umeå Energi Elnät AB",INPUT "Umeå Energi Elnät AB", INPUT "Brevlada.fakturor@umeaenergi.se" ).
RUN pdffakt_UI (INPUT 99, INPUT "Umeå Energi Umenet AB",INPUT "Umeå Energi Umenet AB", INPUT "Brevlada.fakturor@umeaenergi.se" ).
*/

  /*    
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 5
faktmail.EPOST = "invoice.5565277539@kollektor.no"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Luleå Energi Elnät AB"
faktmail.EGETNAMN =  "LuleåEnergiElnät"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
*/

RUN pdffakt_UI (INPUT 5, INPUT "Luleå Energi Elnät AB",INPUT "LuleåEnergiElnät", INPUT "invoice.5565277539@kollektor.no" ).
RUN pdffakt_UI (INPUT 139, INPUT "Luleå Energi Elnät AB",INPUT "LuleåEnergiElnät", INPUT "invoice.5565277539@kollektor.no" ).                                                                                    
RUN pdffakt_UI (INPUT 8, INPUT "Oppunda Kraftkonsult AB",INPUT "Oppunda Kraftkonsult AB", INPUT "faktura@oppundakraft.se" ).
RUN pdffakt_UI (INPUT 26, INPUT "Lapplands Elnät",INPUT "Lapplands Elnät", INPUT "hvd3430@pdf.scancloud.se" ).
                                                                                          
RUN pdffakt_UI (INPUT 36, INPUT "Infratek Sverige AB",INPUT "Infratek Sverige AB", INPUT "faktura.infratek@omexom.com" ).
RUN pdffakt_UI (INPUT 45, INPUT "Elkraftberedning i Blekinge AB" ,INPUT "Elkraftberedning i Blekinge AB", INPUT "joakim@elkraftberedning.se" ).

RUN pdffakt_UI (INPUT 127, INPUT "Rolf Johansson",INPUT "Rolf Johansson", INPUT "rojoh55@outlook.com" ).
RUN pdffakt_UI (INPUT 128, INPUT "Power Forze AB",INPUT "Power Forze AB", INPUT "inbox.lev.1001473@arkivplats.se" ).
RUN pdffakt_UI (INPUT 129, INPUT "Rejlerssverige invoice 4181",INPUT "Rejlersinvoice", INPUT "invoice.rejlerssverige@rejlers.se").
RUN pdffakt_UI (INPUT 130, INPUT "Nelkon AB",INPUT "Nelkon AB", INPUT "kontakt@nelkon.se" ).
RUN pdffakt_UI (INPUT 131, INPUT "Gridtec AB",INPUT "Gridtec AB", INPUT "info@gridtec.se" ).
RUN pdffakt_UI (INPUT 132, INPUT "PSN Kraftkonsult",INPUT "PSN Kraftkonsult", INPUT "per.snygg@psnkraftkonsult.se" ).
RUN pdffakt_UI (INPUT 1, INPUT "ATS KRAFTSERVICE AB",INPUT "ATS KRAFTSERVICE AB", INPUT "Alexandra.Hemert.Sjoberg@peab.se" ).
RUN pdffakt_UI (INPUT 133, INPUT "ATS KRAFTSERVICE AB-Torio",INPUT "ATS KRAFTSERVICE AB-Torio", INPUT "Alexandra.Hemert.Sjoberg@peab.se" ).

RUN pdffakt_UI (INPUT 135, INPUT "Sveakraft Syd AB" ,INPUT "Sveakraft Syd AB", INPUT "johan.krondahl@telia.com" ).
RUN pdffakt_UI (INPUT 136, INPUT "Power Consulting 2u Borås AB" ,INPUT "Power Consulting 2u Borås AB", INPUT "John.johansson@pc-2u.se" ).
RUN pdffakt_UI (INPUT 137, INPUT "VTG Entreprenad AB" ,INPUT "VTG Entreprenad AB", INPUT "faktura@vtg.se" ).
RUN pdffakt_UI (INPUT 138, INPUT "Norberg Elkonsult" ,INPUT "Norberg Elkonsult", INPUT "goran.norberg@vilhelmina.ac" ).

RUN pdffakt_UI (INPUT 19, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT  "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 23, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT  "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 64, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT  "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 77, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT  "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 79, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT  "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 83, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT  "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 86, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT  "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 105, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 112, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 114, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 134, INPUT "Sweco" ,INPUT "Sweco" +  "-", INPUT "invoice.5567679849@kollektor.no" ).
RUN pdffakt_UI (INPUT 140, INPUT "T Karlsson konsult" ,INPUT "T Karlsson konsult" +  "-", INPUT "tk.karlsson58@gmail.com" ).
RUN pdffakt_UI (INPUT 3, INPUT "Kraftkonsult i Skandinavien AB" ,INPUT "Kraftkonsult i Skandinavien AB" +  "-", INPUT "faktura@kraftkonsult.nu" ).
RUN pdffakt_UI (INPUT 141, INPUT "Landsbro BioEnergi HB" ,INPUT "Landsbro BioEnergi HB" +  "-", INPUT "andersx51@gmail.com" ).                                                                                                                     

 
RUN faktkuvert_UI (INPUT 4, INPUT "Kalmar Energi" ,INPUT "Kalmar Energi" +  "-", INPUT "").
RUN faktkuvert_UI (INPUT 17, INPUT "Vattenfall Service Nordic AB" ,INPUT "Vattenfall Service Nordic AB" +  "-", INPUT "").
RUN faktkuvert_UI (INPUT 38, INPUT "El och Skog AB" ,INPUT "El och Skog AB" +  "-", INPUT "").
RUN faktkuvert_UI (INPUT 50, INPUT "Hjorts Entreprenad och Linjebyggnad" ,INPUT "Hjorts Entreprenad och Linjebyggnad" +  "-", INPUT "").
RUN faktkuvert_UI (INPUT 119, INPUT "Alakuijala Oy" ,INPUT "Alakuijala Oy" +  "-", INPUT "").


/* old */
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 55
faktmail.EPOST = "accounts.payable.industry@afconsult.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "ÅFConsult"
faktmail.EGETNAMN =  "ÅFConsult"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 2
faktmail.EPOST = "accounts.payable.industry@afconsult.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "ÅFConsult"
faktmail.EGETNAMN =  "ÅFConsult"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 72
faktmail.EPOST = "accounts.payable.industry@afconsult.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "ÅFConsult"
faktmail.EGETNAMN =  "ÅFConsult"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 80
faktmail.EPOST = "fakturaviaepost@molndalenergi.se"
faktmail.BESTNAMN = ""
faktmail.EGETNAMN = "MölndalEnergi"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 41
faktmail.EPOST = "inbox.lev.237295@arkivplats.se"
faktmail.BESTNAMN = ""
faktmail.EGETNAMN = "GRElkonsult"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.





CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 28
faktmail.EPOST = "faktura@tectel.se"
faktmail.BESTNAMN = ""
faktmail.EGETNAMN = "Tectel"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0. 


 
  


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 20
faktmail.EPOST = "one-nordic.mailinvoice@postenscanning.se"
faktmail.BESTNAMN = ""
faktmail.EGETNAMN = "OneNordic"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0. 
  
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 66
faktmail.EPOST = "inbox.lev.206851@arkivplats.se"
faktmail.BESTNAMN = ""
faktmail.EGETNAMN = "KraftteknikiStockholm"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0. 

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 33
faktmail.EPOST = "Infranet-37-invoice@eltelnetworks.se"
faktmail.BESTNAMN = ""
faktmail.EGETNAMN = "Eltelnetworksinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0. 
   
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 71
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2251"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 70
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2211"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.   

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 69
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2162"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 68
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2141"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 67
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2121"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 10
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2002"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.  

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 84
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2111"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 101
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2162"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 116
faktmail.EPOST = "invoice.rejlerssverige@rejlers.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Rejlerssverige invoice 2101"
faktmail.EGETNAMN = "Rejlersinvoice"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 43
/*
faktmail.EPOST = "faktura_skogs&kraft@erip.se"
*/
faktmail.EPOST = "faktura@skkab.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Skogs och Kraftkonsult AB"
faktmail.EGETNAMN = "Skogs och Kraftkonsult AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0. 




CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 115
faktmail.EPOST = "lars@dutab.nu"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Dutab"
faktmail.EGETNAMN =  "Dutab"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.     


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 53
faktmail.EPOST = "faktura@bygglet.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "JSBF"
faktmail.EGETNAMN =  "JSBF"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.




CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 13
faktmail.EPOST = "efakt.energiforetagen@cogidocs.net"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Energiföretagen Sverige-Swedenergy-AB"
faktmail.EGETNAMN =  "EnergiföretagenSverigeSwedenergyAB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 65
faktmail.EPOST = "efakt.energiforetagen@cogidocs.net"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Energiföretagen Sverige-Swedenergy-AB"
faktmail.EGETNAMN =  "EnergiföretagenSverigeSwedenergyAB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 27
faktmail.EPOST = "invoice.5561398255@kollektor.no"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Luleå Energi AB"
faktmail.EGETNAMN =  "LuleåEnergiAB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 87
faktmail.EPOST = "pdffaktura.att@atstjanster.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "ATS Tjänster AB"
faktmail.EGETNAMN =  "ATS Tjänster AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 73
faktmail.EPOST =    "faktura@skandinaviskelkraft.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Skandinavisk Elkraft AB"
faktmail.EGETNAMN =  "Skandinavisk Elkraft AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.




 

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 11
faktmail.EPOST = "fakturor@sundsvallelnat.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "SundsvallElnät"
faktmail.EGETNAMN =  "Sundsvall Elnät AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 12
faktmail.EPOST = "sundsvallenergi@recall.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Sundsvall Energi AB"
faktmail.EGETNAMN =  "Sundsvall Energi AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 44
faktmail.EPOST = "fakturor@servanet.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "ServaNet"
faktmail.EGETNAMN =  "ServaNet"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 59
faktmail.EPOST = "info@skog-elkraft.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Smålandska Skog & Elkraft AB"
faktmail.EGETNAMN =  "Smålandska Skog & Elkraft AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 61
faktmail.EPOST = "peder@elpedropower.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Elpedro Power Consultant AB"
faktmail.EGETNAMN =  "Elpedro Power Consultant AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
/*
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 62
/* påminnelser
faktmail.EPOST = "kewab@kewab.se"
*/
faktmail.EPOST = "faktura@kewab.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "KEWAB"
faktmail.EGETNAMN =  "KEWAB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
*/

 CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 62

faktmail.EPOST = "faktura@axeda.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Axeda"
faktmail.EGETNAMN =  "Axeda"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.  



CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 90
faktmail.EPOST = "levreskontra@wsp.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "WSP"
faktmail.EGETNAMN =  "WSP"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

/*
faktura¬_abpiteenergi@piteenergi.se
*/
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 92
faktmail.EPOST = "faktura_abpiteenergi@piteenergi.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Piteenergi AB"
faktmail.EGETNAMN =  "Piteenergi AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 93
faktmail.EPOST = "faktura@kasab.nu"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "KASAB"
faktmail.EGETNAMN =  "KASAB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 94
faktmail.EPOST = "FakturaEntreprenad@elverket.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Elverket Entreprenad AB"
faktmail.EGETNAMN =  "Elverket Entreprenad AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 61
faktmail.EPOST = "peder@elpedropower.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Elpedro Power Consultant AB"
faktmail.EGETNAMN =  "Elpedro Power Consultant AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 51
faktmail.EPOST = "levfaktura@linjemontage.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Linjemontage i Grästorp AB"
faktmail.EGETNAMN =  "Linjemontage i Grästorp AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 102
faktmail.EPOST = "ekonomi@miniab.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Mini Entreprenad AB"
faktmail.EGETNAMN =  "Mini Entreprenad AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 103
faktmail.EPOST = "lars@larsberonius.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Lars Beronius AB"
faktmail.EGETNAMN =  "Lars Beronius AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 104
faktmail.EPOST = "per.i.norenius@gmail.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Peno AB"
faktmail.EGETNAMN =  "Peno AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 63
faktmail.EPOST = "lars-ake.jacobsson@polaconsulting.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Pola Consulting AB"
faktmail.EGETNAMN =  "Pola Consulting AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
 



CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 108
faktmail.EPOST = "Kenneth.eriksson@kekraft.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "KE Kraft AB"
faktmail.EGETNAMN =  "KE Kraft AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 109
faktmail.EPOST = "Carl@terrangen.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Terrängen Konsult"
faktmail.EGETNAMN =  "Terrängen Konsult"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 98
faktmail.EPOST = "faktura@framtiden.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Framtiden AB"
faktmail.EGETNAMN =  "Framtiden AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 110
faktmail.EPOST = "stefan@liraab.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Lira el och Comp AB"
faktmail.EGETNAMN =  "Lira el och Comp AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 111
                  
faktmail.EPOST = "inbox.lev.856126@arkivplats.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Power Of Now AB"
faktmail.EGETNAMN =  "Power Of Now AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.



CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 113
faktmail.EPOST = "inbox.lev.241492@arkivplats.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "J. Pounu Elinstallationer AB"
faktmail.EGETNAMN =  "J. Pounu Elinstallationer AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 54
faktmail.EPOST = "ekonomi@kraftochljusteknik.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Landers Kraft och Ljusteknik AB"
faktmail.EGETNAMN =  "Landers Kraft och Ljusteknik AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 85
faktmail.EPOST = "ekonomi@wisegate.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "WiseGate Consulting AB "
faktmail.EGETNAMN =  "WiseGate Consulting AB "
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 120
faktmail.EPOST = "ted.jadesten@jkonsultab.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Jadesten Konsult AB "
faktmail.EGETNAMN =  "Jadesten Konsult AB "
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 100
faktmail.EPOST = "Lennart.sandqvist@enkab.nu"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "ENKAB "
faktmail.EGETNAMN =  "ENKAB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 121
faktmail.EPOST = "kajsaterkeab@gmail.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Kajsäter Elkraftkonsult AB "
faktmail.EGETNAMN =  "Kajsäter Elkraftkonsult AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 122
faktmail.EPOST = "leif_vikman@hotmail.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Leif Vikman "
faktmail.EGETNAMN =  "Leif Vikman"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.



CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 7
faktmail.EPOST = "ulf.andsenius@natstruktur.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Nätsruktur Skogaholm AB"
faktmail.EGETNAMN =  "Nätsruktur Skogaholm AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.



CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 46
faktmail.EPOST = "sara.hansson@infraservicenord.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "ETS"
faktmail.EGETNAMN =  "ETS"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 95
faktmail.EPOST = "kundtjanst@nossebroenergi.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Nossebroortens Energi Ekonomisk Förening"
faktmail.EGETNAMN =  "Nossebroortens Energi Ekonomisk Förening"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 126
faktmail.EPOST = "inbox.lev.888068@arkivplats.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Beredningskonsult AB"
faktmail.EGETNAMN =  "PF Beredningskonsult AB"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.


CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 123
faktmail.EPOST = "faktura@asbro.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "EFLA"
faktmail.EGETNAMN =  "Åsbro kursgård"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.



PROCEDURE Peppol_UI :
   DEFINE INPUT  PARAMETER Pnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER Besk AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER eget  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER vatnr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER glnnr AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER org AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER buy AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER pep AS CHARACTER NO-UNDO. 
   CREATE Epeppol.
   ASSIGN
   Epeppol.FAKTNR = Pnr
   Epeppol.BESKRIVNING = Besk
   Epeppol.EGETNAMN =  eget
   Epeppol.DATUM = TODAY 
   Epeppol.VFAKTNR = 0
   Epeppol.VAT = vatnr
   Epeppol.GLN = glnnr
   Epeppol.ORGNR = org
   Epeppol.BUYREF = buy
   Epeppol.PEPPOL = pep.
END PROCEDURE.

PROCEDURE pdffakt_UI :
   DEFINE INPUT  PARAMETER Pnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER Besk AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER eget  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER email AS CHARACTER NO-UNDO.
   
   CREATE faktmail.
   ASSIGN
   faktmail.FAKTNR = Pnr
   faktmail.EPOST = email
   faktmail.BESTNAMN = ""
   faktmail.BESKRIVNING = Besk
   faktmail.EGETNAMN =  eget
   faktmail.DATUM = TODAY
   faktmail.VFAKTNR = 0.
  
END PROCEDURE.

PROCEDURE faktkuvert_UI :
   DEFINE INPUT  PARAMETER Pnr AS INTEGER NO-UNDO.
   DEFINE INPUT  PARAMETER Besk AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER eget  AS CHARACTER NO-UNDO.
   DEFINE INPUT  PARAMETER email AS CHARACTER NO-UNDO.
   
   CREATE faktkuvert.
   ASSIGN
   faktkuvert.FAKTNR = Pnr
   faktkuvert.EPOST = email
   faktkuvert.BESTNAMN = ""
   faktkuvert.BESKRIVNING = Besk
   faktkuvert.EGETNAMN =  eget
   faktkuvert.DATUM = TODAY
   faktkuvert.VFAKTNR = 0.
  
END PROCEDURE.
/*
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 36
faktmail.EPOST = "763@invoicecenter.net"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Infratek"
faktmail.EGETNAMN =  "Infratek"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 82
faktmail.EPOST = "faktura42@kraftringen.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Kraftringen"
faktmail.EGETNAMN =  "Kraftringen"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

*/
/*
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 42
faktmail.EPOST = "faktura@msva.se"
faktmail.BESTNAMN = ""
faktmail.EGETNAMN = "MittsverigeVatten"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.   
*/

/*
GUMMISSON
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 88
faktmail.EPOST = "accounts.payable.industry@afconsult.com"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "ÅFConsult"
faktmail.EGETNAMN =  "ÅFConsult"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

*/
/*
CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 125
faktmail.EPOST = "levreskontra@akeab.se"
faktmail.BESTNAMN = ""
faktmail.BESKRIVNING = "Akeab"
faktmail.EGETNAMN =  "Akeab"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.

CREATE faktmail.
ASSIGN
faktmail.FAKTNR = 40
faktmail.EPOST = "faktura@bodensenergi.se"
faktmail.BESTNAMN = ""
faktmail.EGETNAMN = "BodensEnergi"
faktmail.DATUM = TODAY
faktmail.VFAKTNR = 0.
*/


