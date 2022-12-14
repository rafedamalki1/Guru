 
 /*------------------------------------------------------------------------
    File        : Log
    Purpose     : 
    Syntax      : 
    Description : 
    Author(s)   : elpfh
    Created     : Fri Feb 10 10:27:37 CET 2012
    Notes       : 
  ----------------------------------------------------------------------*/

USING Progress.Lang.*.

/* 
   Simpel loggerklass!

   Anv?nd WriteLine("Text") f?r att skriva en rad till loggfilen med texten "Text"
   

*/

CLASS Guru.Log: 
   DEFINE PUBLIC VARIABLE Root AS Guru.Root NO-UNDO.
   DEFINE PRIVATE VARIABLE FileStream AS System.IO.FileStream NO-UNDO.
   DEFINE PRIVATE VARIABLE Writer AS System.IO.BinaryWriter NO-UNDO.
   DEFINE PRIVATE VARIABLE DoLog AS LOGICAL NO-UNDO.
   DEFINE VARIABLE LastTime AS DATETIME.
   DEFINE VARIABLE StartTime AS DATETIME.   
   DEFINE VARIABLE LogPath AS CHARACTER NO-UNDO.
   DEFINE VARIABLE StringBuilder AS System.Text.StringBuilder NO-UNDO.
    
   /* Konstruktor */
   CONSTRUCTOR PUBLIC Log(INPUT r AS Guru.Root):
      THIS-OBJECT:StartTime = NOW.
      THIS-OBJECT:StringBuilder = NEW System.Text.StringBuilder().
      DEFINE VARIABLE LogFolder AS CHARACTER NO-UNDO.
      DEFINE VARIABLE LogFilename AS CHARACTER NO-UNDO.
      DEFINE VARIABLE naow AS DATETIME NO-UNDO.
      THIS-OBJECT:DoLog = FALSE.
      naow = NOW.
      {SESSIONTEMPDIR.I}
      LogFilename = Guru.Konstanter:globanv + "-" + STRING(System.Threading.Thread:CurrentThread:ManagedThreadId) + "." + STRING(naow)  + ".log".
      LogFilename = REPLACE(LogFilename, "/", "").
      LogFilename = REPLACE(LogFilename, "\", "").
      LogFilename = REPLACE(LogFilename, ":", "").
      

      LogFolder = SESSION:TEMP-DIRECTORY + Guru.Konstanter:globanv + "\".
      
      IF SESSION:CLIENT-TYPE = "WEBCLIENT" THEN DO:
         LogFolder = webclienttempdir.
      END.

      LogPath = LogFolder + LogFilename.
      
      IF System.IO.Directory:Exists(LogFolder) NE TRUE THEN System.IO.Directory:CreateDirectory(LogFolder).
      
      /* ?ppna en fil-str?m till guru.log */
      THIS-OBJECT:FileStream = NEW System.IO.FileStream(LogPath, System.IO.FileMode:OpenOrCreate).
      
      CATCH e AS Progress.Lang.Error :
         THIS-OBJECT:FileStream:Close().
         System.Windows.Forms.MessageBox:Show("Guru kommer inte ?t " + LogPath  +  ", loggning kommer inaktiveras.~nFel: " + e:GetMessage(0)).
         THIS-OBJECT:DoLog = FALSE.
      END CATCH.
      FINALLY.
         /* Om vi inte har skrivr?ttigheter, st?ng str?mmen, skicka felmeddelande-popup och st?ng av loggningsfunktionen */
         IF THIS-OBJECT:FileStream:CanWrite NE TRUE THEN DO:
            THIS-OBJECT:FileStream:Close().
            System.Windows.Forms.MessageBox:Show("Guru har inte skriv-r?ttigheter p? " + LogPath  +  ", loggning kommer inaktiveras.").
            THIS-OBJECT:DoLog = FALSE.
            
         END. 
         ELSE DO:
            /* Annars, aktivera loggningsfunktionen*/
            THIS-OBJECT:WriteLine("Starting session at " + STRING ( THIS-OBJECT:StartTime ) ).              
         END.
         IF THIS-OBJECT:DoLog = FALSE THEN DO:
            THIS-OBJECT:FileStream:Close(). 
            OS-DELETE VALUE(LogPath) NO-ERROR.
         END.   
      END.

   END CONSTRUCTOR.

   DESTRUCTOR Log():
      /* Destruktor, skriv, st?ng allt om det ?r ?ppet och rensa rubbet. */
      IF THIS-OBJECT:DoLog = TRUE THEN DO:
         THIS-OBJECT:Writer = NEW System.IO.BinaryWriter(THIS-OBJECT:FileStream).
         THIS-OBJECT:Writer:Write( THIS-OBJECT:StringBuilder:ToString() ).
         THIS-OBJECT:Writer:Close().
         THIS-OBJECT:FileStream:Close().         
      END.
      /*
      OS-DELETE VALUE(LogPath) NO-ERROR.
      */
      DELETE OBJECT THIS-OBJECT:FileStream NO-ERROR.
      DELETE OBJECT THIS-OBJECT:Writer NO-ERROR.
   END DESTRUCTOR.

   DEFINE VARIABLE TimeSinceLast AS INTEGER.
   DEFINE VARIABLE TimeSinceStart AS DATETIME.
   DEFINE VARIABLE NowS AS DATETIME.

   /* Denna metod vill du anv?nda, Logger:WriteLine("Jag heter olle") skriver en rad i loggfilen, om loggningsfunktionen ?r aktiverad */
   METHOD PUBLIC VOID WriteLine(INPUT linein AS CHARACTER):
      DEFINE VARIABLE lineor AS CHARACTER NO-UNDO.
      lineor = linein.
         
      IF THIS-OBJECT:DoLog = FALSE THEN RETURN.
      IF THIS-OBJECT:DoLog = TRUE THEN DO:
         THIS-OBJECT:NowS = NOW.
         THIS-OBJECT:TimeSinceLast = INTERVAL(THIS-OBJECT:NowS, THIS-OBJECT:LastTime, "milliseconds").
        
         linein = linein + "~t~t@ time: " + STRING(NowS) + "~t~tTime since last:~t" + STRING(TimeSinceLast) .
         IF linein = ? THEN linein = lineor.
         THIS-OBJECT:LastTime = NowS.
         /*"HH:MM:SS.SSS"*/
         THIS-OBJECT:StringBuilder:AppendLine(linein).
         
      END.
   END METHOD.

   
END CLASS.