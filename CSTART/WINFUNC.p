/* ===========================================================================
   file    : WINFUNC.P
   by      : Jurjen Dijkstra, 1997
             mailto:jurjen@global-shared.com
             http://www.global-shared.com
   purpose : implementation of functions that call windows API procedures
   =========================================================================== */

&GLOB DONTRUN-WINFUNC
{windows.i}


/* this CreateProcess function is a simplified version of the 
   CreateProcess API definition.
   Parameters:
   1. Commandline, for example "notepad.exe c:\config.sys"
   2. CurrentDir,  is default directory for new process
   3. wShowWindow, 0=hidden, 1=normal, 2=minimized, 3=maximized
   r. return       if <>0 then handle of new process
                   if  =0 then failed, check GetLastError  */
                   
FUNCTION CreateProcess RETURNS INTEGER 
         (input CommandLine as CHAR,
          input CurrentDir  as CHAR,
          input wShowWindow as INTEGER) :    

   def var lpStartupInfo as memptr.
   set-size(lpStartupInfo)     = 68.
   put-long(lpStartupInfo,1)   = 68.
   put-long (lpStartupInfo,45) = 1. /* = STARTF_USESHOWWINDOW */
   put-short(lpStartupInfo,49) = wShowWindow.

   def var lpProcessInformation as memptr.
   set-size(lpProcessInformation)   = 16.

   def var lpCurrentDirectory as memptr.
   if CurrentDir<>"" then do:
      set-size(lpCurrentDirectory)     = 256.
      put-string(lpCurrentDirectory,1) = CurrentDir.
   end.   

   def var bResult as integer.
   
   run CreateProcess{&A} IN Guru.Konstanter:hpApi
     ( 0,
       CommandLine,
       0,
       0,
       0,
       0,
       0,
       if CurrentDir="" 
          then 0 
          else get-pointer-value(lpCurrentDirectory),
       get-pointer-value(lpStartupInfo),
       get-pointer-value(lpProcessInformation),
       output bResult
     ).

  def var hProcess as integer no-undo.
  def var hThread  as integer no-undo.
  hProcess = get-long(lpProcessInformation,1).
  hThread  = get-long(lpProcessInformation,5).

  /* I am pretty sure you are not interested in hThread 
     so let's invalidate the handle right now. 
     This does not mean the thread is terminated, it just 
     means that Kernel doesn't need to keep the object for US. */
  DEF VAR ReturnValue as INTEGER NO-UNDO.
  RUN CloseHandle IN Guru.Konstanter:hpApi(hThread, output ReturnValue).
  
  set-size(lpStartupInfo)        = 0.
  set-size(lpProcessInformation) = 0.
  set-size(lpCurrentDirectory)   = 0.

  return ( hProcess ).

END FUNCTION.


/* GetLastError returns the Error code, set by the most recently 
   failed api-call. */

/* PROBLEM : GetLastError will always return 127. The reason is that 
   Progress will have called some api function AFTER the one you have
   called. (29 januari 1998) */
   
FUNCTION GetLastError RETURNS INTEGER :
  def var dwMessageID as integer no-undo.
  run GetLastError IN Guru.Konstanter:hpApi (output dwMessageID).
  RETURN (dwMessageID). 
END FUNCTION.    


/* GetParent returns the hWnd of the parent window */
   
FUNCTION GetParent RETURNS INTEGER 
         (input hWnd as INTEGER) :
  def var hParent as integer no-undo.
  run GetParent IN Guru.Konstanter:hpApi (hWnd, output hParent).
  RETURN (hParent). 
END FUNCTION.    


/* ShowLastError calls GetLastError and shows the message text in a 
   alert-box. The Message text is simply only searched in the system
   module, using the default language and does not insert any 
   arguments (like in the P4GL 'substitute' function) */

/* Note: can't work because GetLastError doesn't work with Progress */
FUNCTION ShowLastError RETURNS INTEGER :
  def var ErrorId as integer no-undo.
  def var txt as char no-undo.
  def var TxtLength as integer no-undo.

  ErrorId = GetLastError().
  txt = fill(" ",300).
  run FormatMessage{&A} IN Guru.Konstanter:hpApi (512 + 4096,  /* = FORMAT_MESSAGE_IGNORE_INSERTS  
                                                  + FORMAT_MESSAGE_FROM_SYSTEM */
                        0,
                        ErrorId,
                        0,
                        output txt,
                        length(txt),
                        0,
                        output TxtLength).
   message  txt view-as alert-box error.
   RETURN ( ErrorId ).
   
END FUNCTION.

FUNCTION DisableWindowClose RETURNS LOGICAL
  (INPUT winhandle AS HANDLE /* parameter-definitions */ ) :
/*------------------------------------------------------------------------------
  Purpose:  
    Notes:  
------------------------------------------------------------------------------*/
  define var hSysMenu   as  int no-undo.
  define var hParent    as  int no-undo.
  define var hInstance  as  int no-undo.
  define var iRetCode   as  int no-undo.
  define var iCnt       as  int no-undo.
  /*
  run GetParent IN Guru.Konstanter:hpApi (hWnd, output hParent).
  
  run GetParent (input {&window-name}:hWnd,
                output hParent).
   */
  hParent = GetParent(winhandle:HWND).
  /* Get handle to our the window's system menu (Restore, Maximize, Move, close etc.) */
  run GetSystemMenu(input  hParent, 
                    input  0,
                    output hSysMenu).

  if hSysMenu <> 0 then
  do:
    /* Get System menu's menu count */
    run GetMenuItemCount(input hSysMenu,
                         output iCnt).
      
    if iCnt <> 0 then
    do:
      /* Menu count is based on 0 (0, 1, 2, 3...) */

      /* remove the "close option" */
      run RemoveMenu(input hSysMenu, 
                     input iCnt - 1, 
                     input {&MF_BYPOSITION} + {&MF_REMOVE},
                     output iRetCode).

      /* remove the seperator */
      run RemoveMenu(input hSysMenu, 
                     input iCnt - 2, 
                     input {&MF_BYPOSITION} + {&MF_REMOVE},
                     output iRetCode).
            
      /* Force caption bar's refresh which will disable the window close ("X") button */
      run DrawMenuBar(input  hParent,
                      output iRetCode).
      /*{&window-name}:title = "Try to close me!".*/
    end. /* if iCnt <> 0... */
  end. /* if hSysMenu <> 0... */  

  RETURN FALSE.   /* Function return value. */

END FUNCTION.
