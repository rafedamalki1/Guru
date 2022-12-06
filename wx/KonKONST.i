
/*------------------------------------------------------------------------
    File        : KonKONST
    .i
    Purpose     : Guru.Konstanter:

    Syntax      :

    Description : 

    Author(s)   : 
    Created     : Fri May 21 15:19:53 CEST 2021
    Notes       :
  ----------------------------------------------------------------------*/
/*Tables*/
DEFINE PUBLIC STATIC PROPERTY AnvandareTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY PersonalTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY OmradeTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
      
DEFINE PUBLIC STATIC PROPERTY BorttagnaOmradeTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY BestKundTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.   
      
DEFINE PUBLIC STATIC PROPERTY JurPersTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY JurAvdTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY LeverantorTTh AS HANDLE NO-UNDO
      PUBLIC GET.    PUBLIC SET.

/*Tables*/
   
/*START*/
   /*f�r att man ska kunna komma �t tabeller �ver allt.*/
DEFINE PUBLIC STATIC PROPERTY startGlobalFakeRoot      AS Guru.FakeRoot   NO-UNDO      
      PUBLIC GET.    PUBLIC SET.
    /*ska s�ttas om anv�ndare ska s�ttas  f�r defaultv�rden*/ 
   /*globalroot f�r dom som inte har Root som input*/
DEFINE PUBLIC STATIC PROPERTY globalroot      AS Guru.Root   NO-UNDO      
      PUBLIC GET.    PUBLIC SET.
 


D

DEFINE PUBLIC STATIC PROPERTY globallm AS LOGICAL 
   PUBLIC GET. PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY globanvpkod AS CHARACTER 
   PUBLIC GET. PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY globallpers AS LOGICAL 
   PUBLIC GET. PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY globallao AS LOGICAL 
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY globavd AS INTEGER 
   PUBLIC GET. PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY globomr AS CHARACTER 
   PUBLIC GET. PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY globjid AS CHARACTER 
   PUBLIC GET. PUBLIC SET.  
     
DEFINE PUBLIC STATIC PROPERTY globpersnamn AS CHARACTER 
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY globstorb AS INTEGER   /*maxf�nster samma som globsidl ???  globsidl = ANVANDARE.SIDL  globstorb = ANVANDARE.SIDL  */
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY globstorh AS INTEGER 
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY globDefaultstorb AS INTEGER  /*default normalf�nster*/
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY globDefaultstorh AS INTEGER 
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY globsidl AS INTEGER      /*default maxf�nster*/ 
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY globsids AS INTEGER 
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY globradbrytch AS CHARACTER  
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:globnetprissortvar AS INTEGER 
   PUBLIC GET. PUBLIC SET.   /*"kund DESCENDING by enr"*/
DEFINE PUBLIC STATIC PROPERTY alltidmax AS LOGICAL
      PUBLIC GET.     PUBLIC SET.    

DEFINE PUBLIC STATIC PROPERTY conappvar  AS CHARACTER
      PUBLIC GET.     PUBLIC SET. 
      
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:appcon  AS LOGICAL
      PUBLIC GET.     PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:apphand AS HANDLE
      PUBLIC GET.     PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY appfel  AS LOGICAL
      PUBLIC GET.     PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY MultiStarth AS HANDLE
      PUBLIC GET.     PUBLIC SET. 
   
DEFINE PUBLIC STATIC PROPERTY hpApi AS HANDLE
      PUBLIC GET.     PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY hpWinFunc AS HANDLE
      PUBLIC GET.     PUBLIC SET.       
 

/*KLIENT-KONS SERVER-KONS*/ 

DEFINE PUBLIC STATIC PROPERTY globanvavdnr AS INTEGER 
   PUBLIC GET. PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY globforetag AS CHARACTER 
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY GForetag AS CHARACTER 
   PUBLIC GET. PUBLIC SET.  
   
DEFINE PUBLIC STATIC PROPERTY gvisatidpermanad AS LOGICAL 
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY AppSpringSet AS CHARACTER EXTENT 15
      PUBLIC GET.     PUBLIC SET. 
   /*      f�retag = 1 
           Computer_LanIP = 2
           MacAddDb = 3
           datoruser = 4
           GuruAnvandare = 5
           l�sen = 6

*/
DEFINE PUBLIC STATIC PROPERTY varforetypval AS INTEGER EXTENT 100
      PUBLIC GET.     PUBLIC SET.       
DEFINE PUBLIC STATIC PROPERTY varforetypchar AS CHARACTER EXTENT 100
      PUBLIC GET.     PUBLIC SET.      
DEFINE PUBLIC STATIC PROPERTY globnystart AS LOGICAL
      PUBLIC GET.     PUBLIC SET. 


DEFINE PUBLIC STATIC PROPERTY gavdl AS CHARACTER /*AVDELNING L�NG*/
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gavdk AS CHARACTER /*AVDELNING KORT*/ 
   PUBLIC GET. PUBLIC SET.     
DEFINE PUBLIC STATIC PROPERTY gomrl AS CHARACTER /*OMR�DE L�NG*/ 
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY gomrk AS CHARACTER     /*OMR�DE KORT*/ 
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gaol AS CHARACTER /*AONR L�NG*/ 
   PUBLIC GET. PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY gaok AS CHARACTER  /*AONR KORT*/  
   PUBLIC GET. PUBLIC SET.       
   
DEFINE PUBLIC STATIC PROPERTY gdelnrl AS CHARACTER  INITIAL "Delnr" /*delnr L�NG*/ 
   PUBLIC GET. PUBLIC SET.   
DEFINE PUBLIC STATIC PROPERTY gdelnrk AS CHARACTER INITIAL "Delnr" /*delnr KORT*/  
   PUBLIC GET. PUBLIC SET.
   
DEFINE PUBLIC STATIC PROPERTY gutbytk AS CHARACTER INITIAL "Ers�ttningslista" /* KORT*/  
   PUBLIC GET. PUBLIC SET.
   
DEFINE PUBLIC STATIC PROPERTY gpll AS CHARACTER /*PLANNR L�NG*/
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY gplk AS CHARACTER /*PLANNR KORT*/
   PUBLIC GET. PUBLIC SET.     
DEFINE PUBLIC STATIC PROPERTY genl AS CHARACTER /*E-NUMMER L�NG*/ 
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY genk AS CHARACTER /*E-NUMMER KORT*/
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY gjul AS CHARACTER /*JURIDISK L�NG*/ 
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gjuk AS CHARACTER /*JURIDISK KORT*/
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY gfastl AS CHARACTER /*FASTA L�NG*/ 
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gfastk AS CHARACTER  /*FASTA KORT*/
   PUBLIC GET. PUBLIC SET.    
DEFINE PUBLIC STATIC PROPERTY gtilll AS CHARACTER /*TILLF�LIGA L�NG*/
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY gtillk AS CHARACTER  /*TILLF�LIGA KORT*/
   PUBLIC GET. PUBLIC SET.    
DEFINE PUBLIC STATIC PROPERTY gberel AS CHARACTER /*BEREDARE L�NG*/ 
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gberek AS CHARACTER /*BEREDARE KORT*/
   PUBLIC GET. PUBLIC SET.     
DEFINE PUBLIC STATIC PROPERTY gprojl AS CHARACTER  /*PROJEKT�R L�NG*/ 
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY gprojk AS CHARACTER /*PROJEKT�R KORT*/
   PUBLIC GET. PUBLIC SET.     
DEFINE PUBLIC STATIC PROPERTY garbal AS CHARACTER /*ARBETSANSVARIG L�NG*/
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY garbak AS CHARACTER  /*ARBETSANSVARIG KORT*/
   PUBLIC GET. PUBLIC SET.    
DEFINE PUBLIC STATIC PROPERTY gutfk AS CHARACTER /*Utf�rande KORT*/
   PUBLIC GET. PUBLIC SET.     
DEFINE PUBLIC STATIC PROPERTY gutfl AS CHARACTER /*Utf�rande L�NG*/ 
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gbestk AS CHARACTER  /*Best�llare/Kund KORT*/
   PUBLIC GET. PUBLIC SET.    
DEFINE PUBLIC STATIC PROPERTY gbestl AS CHARACTER /*Best�llare/Kund L�NG*/ 
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gdebk AS CHARACTER  /*Debiterigstyp KORT*/
   PUBLIC GET. PUBLIC SET.    
DEFINE PUBLIC STATIC PROPERTY gdebl AS CHARACTER /*Debiterigstyp L�NG*/ 
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY gtidlk AS CHARACTER /*tidl�ge KORT*/
   PUBLIC GET. PUBLIC SET.     
DEFINE PUBLIC STATIC PROPERTY gtidll AS CHARACTER /*tidl�ge L�NG*/ 
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY gutfardk AS CHARACTER  /*utf�rdat av KORT*/
   PUBLIC GET. PUBLIC SET.    
DEFINE PUBLIC STATIC PROPERTY gutfardl AS CHARACTER /*utf�rdat av L�NG*/ 
   PUBLIC GET. PUBLIC SET.  
DEFINE PUBLIC STATIC PROPERTY grefbefk AS CHARACTER /*Ref.nr best�llare KORT*/
   PUBLIC GET. PUBLIC SET.     
DEFINE PUBLIC STATIC PROPERTY grefbefl AS CHARACTER /*Ref.nr best�llare  L�NG*/ 
   PUBLIC GET. PUBLIC SET.
   
DEFINE PUBLIC STATIC PROPERTY gpriok AS CHARACTER NO-UNDO     /*Prioritet KORT*/
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY gpriol AS CHARACTER NO-UNDO  /*Prioritet  L�NG*/
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gartk AS CHARACTER NO-UNDO     /*Arbetsart KORT*/
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY gartl AS CHARACTER NO-UNDO  /*Arbetsart  L�NG*/
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY gaonamnk AS CHARACTER NO-UNDO     /*Ort/Ben�mning aonr KORT*/
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY gaonamnl AS CHARACTER NO-UNDO  /*Ort/Ben�mning aonr  L�NG*/
   PUBLIC GET. PUBLIC SET. 
DEFINE PUBLIC STATIC PROPERTY globsprak AS INTEGER NO-UNDO  /*Ort/Ben�mning aonr  L�NG*/
   PUBLIC GET. PUBLIC SET.
      /*alla sekvar laddas i REGLERFORLOSEN.P*/
 DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:hoppsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
 DEFINE PUBLIC STATIC PROPERTY aonrsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
 DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:bulasekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
 DEFINE PUBLIC STATIC PROPERTY beresekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:faktsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
   
DEFINE PUBLIC STATIC PROPERTY kalk2sekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY arendesek AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY mtrlsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:persekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:plansekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:regsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY tadmsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY tidasekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:tidbsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY tidosekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY tidrsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:tidsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY tidssekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY tidtsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY Guru.Konstanter:storsekvar AS LOGICAL EXTENT 50 NO-UNDO
   PUBLIC GET. PUBLIC SET.
 

/*KLIENT-KONS SERVER-KONS*/
/*KLIENT-KONS OBS ANNAN  P� SERVER-KONS*/   
DEFINE PUBLIC STATIC PROPERTY globalhkeyvar AS CHARACTER 
      PUBLIC GET.    PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY dlcvar AS CHARACTER 
      PUBLIC GET.    PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY wtidvar AS CHARACTER 
      PUBLIC GET.    PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY guruvar AS CHARACTER 
      PUBLIC GET.    PUBLIC SET.    
DEFINE PUBLIC STATIC PROPERTY datornamn AS CHARACTER 
   PUBLIC GET. PUBLIC SET.
DEFINE PUBLIC STATIC PROPERTY gurubilder AS CHARACTER 
      PUBLIC GET.    PUBLIC SET.      
/*KLIENT-KONS OBS ANNAN  P� SERVER-KONS*/     
   
 
   
      

 