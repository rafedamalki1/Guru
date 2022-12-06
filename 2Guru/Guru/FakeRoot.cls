 
 /*------------------------------------------------------------------------
    File        : FakeRoot
    Purpose     : vid inloggning och byte av användare
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Wed May 02 15:32:13 CEST 2012
    Notes       : För att göra den nya språkmodulen bakåtkompatibel med gamla guru så körs en instans av denna "fakeroot" som enbart laddar konstanter
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.



CLASS Guru.FakeRoot: 
   &Scoped-define STATIC STATIC
   &Scoped-define NEW STATIC
   &Scoped-define PUBLIC PUBLIC
   {GLOBALTT.I}
   {SPRAKTEMP.i}
   DEFINE PUBLIC VARIABLE LanguageId AS INTEGER NO-UNDO.
   DEFINE PUBLIC VARIABLE AppServerHandle AS HANDLE NO-UNDO.
  
   CONSTRUCTOR PUBLIC FakeRoot():
      {GLOBALSTART.I}
      THIS-OBJECT:Connect().
      THIS-OBJECT:RensaTT(). 
      THIS-OBJECT:LoadLanguage(Guru.Konstanter:globsprak).
      THIS-OBJECT:Translateconstants().
      
   END CONSTRUCTOR.
   
   DESTRUCTOR PUBLIC FakeRoot():
      IF VALID-HANDLE(THIS-OBJECT:AppServerHandle) THEN DELETE PROCEDURE THIS-OBJECT:AppServerHandle NO-ERROR.
      THIS-OBJECT:AppServerHandle = ?.
   END DESTRUCTOR.
   
   METHOD PUBLIC VOID Connect (  ):  
      IF Guru.Konstanter:appcon THEN DO:                           
         RUN TEMPTABELLERAPP.P PERSISTENT SET AppServerHandle ON Guru.Konstanter:apphand TRANSACTION DISTINCT (INPUT Guru.Konstanter:globnystart).
      END.
      ELSE DO:
         RUN TEMPTABELLERAPP.P PERSISTENT SET AppServerHandle (INPUT Guru.Konstanter:globnystart).                  
      END.   
   END METHOD.
   
   {TranslateConstants.I}
   METHOD PUBLIC VOID RensaTT():
      EMPTY TEMP-TABLE sprakstrangtemp.
      THIS-OBJECT:EmptyTT(Guru.Konstanter:OmradeTTh). 
      THIS-OBJECT:EmptyTT(Guru.Konstanter:JurPersTTh).
      THIS-OBJECT:EmptyTT(Guru.Konstanter:JurAvdTTh).
      THIS-OBJECT:EmptyTT(Guru.Konstanter:AnvandareTTh).
      THIS-OBJECT:EmptyTT(Guru.Konstanter:PersonalTTh).
      THIS-OBJECT:EmptyTT(Guru.Konstanter:LeverantorTTh).
      THIS-OBJECT:EmptyTT(Guru.GlobalaVariabler:MaterielTTh).
      THIS-OBJECT:EmptyTT(Guru.Konstanter:BestKundTTh).                            
   END METHOD.
   /* Tömmer temptable */
   METHOD PUBLIC VOID EmptyTT(tempNamnh AS HANDLE ):
      tempNamnh:EMPTY-TEMP-TABLE() NO-ERROR.                                   
   END METHOD.
  METHOD PUBLIC VOID StartLaddaTT():
      THIS-OBJECT:FetchOmrade().
      THIS-OBJECT:FetchAnvandare().
      THIS-OBJECT:FetchPersonal().
      THIS-OBJECT:FetchLeverantor().
      THIS-OBJECT:FetchHuvudLeverantor().
  END METHOD.
  /* Hämtar områden ifrån databas*/
   METHOD PUBLIC VOID FetchOmrade():     
      
      FIND FIRST omrtemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE omrtemp THEN DO:
         RUN omrade_UI IN AppServerHandle (OUTPUT TABLE omrtemp).
         RUN Bortagnaomrade_UI IN AppServerHandle (OUTPUT TABLE Borttagnaomrtemp).
         RUN jurp_UI IN AppServerHandle (INPUT Guru.Konstanter:globanv,OUTPUT TABLE jurperstemp,OUTPUT TABLE judavdtemp).
         RUN bestkund_UI IN AppServerHandle (OUTPUT TABLE bestkundalltclass).
      END.                 
       
   END METHOD.
   METHOD PUBLIC VOID FetchOmrade(INPUT rensavar AS LOGICAL):
      IF rensavar = TRUE THEN DO:         
         THIS-OBJECT:EmptyTT(Guru.Konstanter:OmradeTTh). 
         THIS-OBJECT:EmptyTT(Guru.Konstanter:JurPersTTh).
         THIS-OBJECT:EmptyTT(Guru.Konstanter:JurAvdTTh).
         THIS-OBJECT:EmptyTT(Guru.Konstanter:BestKundTTh).
      END.   
      THIS-OBJECT:FetchOmrade().           
   END METHOD.
   /* Hämtar användare ifrån databas*/
   METHOD PUBLIC VOID FetchAnvandare():
      DEFINE VARIABLE ivar AS INTEGER NO-UNDO.
      FIND FIRST anvandartemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE anvandartemp THEN DO:
         RUN anvtemp_UI IN AppServerHandle (OUTPUT TABLE anvandartemp) NO-ERROR.
         IF ERROR-STATUS:ERROR = TRUE THEN DO:
            MESSAGE 
            "Versions problem!" SKIP 
            "Det går ej att ansluta Guru" SKIP
            "Kontakta Elpool tel 090/184540." SKIP
            SKIP
            "Vill du se fler felmeddelanden?" 
            VIEW-AS ALERT-BOX QUESTION BUTTONS YES-NO TITLE "Fel!"
            UPDATE view-errs AS LOGICAL .       
            IF view-errs THEN DO ivar = 1 TO ERROR-STATUS:NUM-MESSAGES:
               MESSAGE ERROR-STATUS:GET-NUMBER(ivar)
               ERROR-STATUS:GET-MESSAGE(ivar)
               VIEW-AS ALERT-BOX.
            END.
            QUIT.
         END.
         
         Guru.Konstanter:StoppaOmrSek(TEMP-TABLE anvandartemp:HANDLE).
      END.
   END METHOD.
   
   /* Hämtar personal ifrån databas*/
   METHOD PUBLIC VOID FetchPersonal(): 
      FIND FIRST personaltemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE personaltemp THEN DO:
         RUN perstemp_UI IN AppServerHandle (OUTPUT TABLE personaltemp).
         Guru.Konstanter:StoppaOmrSek(TEMP-TABLE personaltemp:HANDLE).        
      END. 
   END METHOD.
   /* Hämtar leverantörer */
   METHOD PUBLIC VOID FetchLeverantor():
      FIND FIRST levtemp WHERE NO-LOCK NO-ERROR.
      IF NOT AVAILABLE levtemp THEN DO:
         RUN levtemp_UI IN AppServerHandle (OUTPUT TABLE levtemp).
         RUN huvlevtemp_UI IN AppServerHandle (OUTPUT TABLE huvlevtemp).
      END.                       
   END METHOD.
   METHOD PUBLIC VOID FetchHuvudLeverantor():
      FIND FIRST huvlevtemp WHERE huvlevtemp.DEP-NR = 999 NO-LOCK NO-ERROR.
      IF AVAILABLE huvlevtemp THEN DO:
         ASSIGN
         Guru.Konstanter:HuvudLeverantor = huvlevtemp.LEVKOD.
      END.
      ELSE DO:
         FIND FIRST levtemp WHERE levtemp.LEVKOD NE "0" AND levtemp.LEVKOD NE "99"
         AND levtemp.BORTTAG = FALSE NO-LOCK NO-ERROR.
         IF AVAILABLE levtemp THEN Guru.Konstanter:HuvudLeverantor = levtemp.LEVKOD.
      END.         
      FIND FIRST levtemp WHERE levtemp.LEVKOD = Guru.Konstanter:HuvudLeverantor
      USE-INDEX LEV NO-LOCK NO-ERROR.   
      IF AVAILABLE levtemp THEN Guru.Konstanter:HuvudLeverantorName = levtemp.LEVNAMN.
   END METHOD.
     
END CLASS.