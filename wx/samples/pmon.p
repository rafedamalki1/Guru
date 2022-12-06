define variable my-title as character format "x(80)".
define variable perMinute as integer.

define variable page-length     as integer initial 24.
define variable sample-interval as integer initial 10.
define variable sleep-time      as integer initial 10.
define variable screen-pause    as integer initial 5.
define variable auto-repeats    as integer initial 10.
define variable auto-repeat-cnt as integer initial 0.

define variable tmp-uptime    like _Summary-UpTime   no-undo.
define variable tmp-commits   like _Summary-Commits   no-undo.
define variable tmp-dbreads   like _Summary-DbReads   no-undo.
define variable tmp-undos     like _Summary-Undos   no-undo.
define variable tmp-dbwrites  like _Summary-DbWrites   no-undo.
define variable tmp-recreads  like _Summary-RecReads   no-undo.
define variable tmp-bireads   like _Summary-BiReads    no-undo.
define variable tmp-recupd    like _Summary-RecUpd    no-undo.
define variable tmp-biwrites  like _Summary-BiWrites   no-undo.
define variable tmp-reccreat  like _Summary-RecCreat   no-undo.
define variable tmp-aiwrites  like _Summary-AiWrites   no-undo.
define variable tmp-recdel    like _Summary-RecDel    no-undo.
define variable tmp-chkpts    like _Summary-Chkpts   no-undo.
define variable tmp-reclock   like _Summary-RecLock   no-undo.
define variable tmp-flushed   like _Summary-Flushed    no-undo.
define variable tmp-recwait   like _Summary-RecWait    no-undo.


define variable i as integer.
define variable x as integer.

/*** Frames ***/

define frame UserLock-frame
       _UserLock-Id         COLUMN-LABEL "Lock Id" FORMAT ">>>>9"
       _Userlock-Usr        COLUMN-LABEL "Usr"
       _UserLock-Name       COLUMN-LABEL "Name"
       _UserLock-Type[i]    COLUMN-LABEL "Type"
       _UserLock-Recid[i]   COLUMN-LABEL "RECID"
       _UserLock-Chain[i]   COLUMN-LABEL "Chain"
       _UserLock-Flags[i]   COLUMN-LABEL "Flags"
       with down.


/*** Sub Menus ***/

DEFINE SUB-MENU sm-Files
    MENU-ITEM mi-Continue  LABEL "&Continue"
    MENU-ITEM mi-Choose-Db LABEL "&Choose Database"
    MENU-ITEM mi-Exit      LABEL "E&xit".
    
DEFINE SUB-MENU stat-Clients
    MENU-ITEM cl-All        LABEL "&All Processes"
    MENU-ITEM cl-Blocked    LABEL "&Blocked Clients"
    MENU-ITEM cl-Active     LABEL "Active &Transactions"
    MENU-ITEM cl-Interactive LABEL "Local &Interactive Clients"
    MENU-ITEM cl-Batch      LABEL "Local Batch Clients"
    MENU-ITEM cl-Remote     LABEL "&Remote Clients"
    MENU-ITEM cl-Background LABEL "&Background".

DEFINE SUB-MENU stat-Locks
    MENU-ITEM locks-All       LABEL "All Locks"
    MENU-ITEM locks-User      LABEL "Locks by User (1st 512)".
      
DEFINE SUB-MENU stat-Status
    MENU-ITEM stat-Database   LABEL "&Database"
    MENU-ITEM stat-Backup     LABEL "&Backup"
    MENU-ITEM stat-Servers    LABEL "&Servers"
    SUB-MENU  stat-Clients    LABEL "&Clients"
    MENU-ITEM stat-Files      LABEL "&Files"
    SUB-MENU  stat-Locks      LABEL "&Lock Table"
    MENU-ITEM stat-Buffers    LABEL "&Buffers"
    MENU-ITEM stat-Logging    LABEL "&Logging Summary"
    MENU-ITEM stat-BI-Log     LABEL "&BI Log"
    MENU-ITEM stat-AI-Log     LABEL "&AI Log"
    MENU-ITEM stat-2PC        LABEL "&Two Phase Commit"
    MENU-ITEM stat-Startup    LABEL "&Startup Parameters"
    MENU-ITEM stat-Resources  LABEL "Shared &Resources"
    MENU-ITEM stat-Mem-Seg    LABEL "Shared &Memory Segments" .
    
DEFINE SUB-MENU act-Activity
    MENU-ITEM act-Summary    LABEL "&Summary"
    MENU-ITEM act-Servers    LABEL "&Servers"
    MENU-ITEM act-Buffers    LABEL "&Buffer Cache"
    MENU-ITEM act-Page-Ws    LABEL "&Page Writers"
    MENU-ITEM act-BI-Log     LABEL "&BI Log"
    MENU-ITEM act-AI-Log     LABEL "&AI Log"
    MENU-ITEM act-Locks      LABEL "&Lock Table"
    MENU-ITEM act-IO-Type    LABEL "I/O Operations by &Type"
    MENU-ITEM act-IO-File    LABEL "I/O Operations by &File"
    MENU-ITEM act-Space      LABEL "&Space Allocation"
    MENU-ITEM act-Index      LABEL "&Index"
    MENU-ITEM act-Record     LABEL "&Record"
    MENU-ITEM act-Other      LABEL "&Other".

    
DEFINE SUB-MENU sm-Other
    MENU-ITEM ot-Performance LABEL "&Performance Indicators"
    MENU-ITEM ot-IO          LABEL "&I/O Operations"
    MENU-ITEM ot-Lock        LABEL "&Lock Requests"
    MENU-ITEM ot-Checkpoints LABEL "&Checkpoints".
    MENU-ITEM ot-Blocks      LABEL "&Blocks".
    
DEFINE SUB-MENU sm-Admin
    MENU-ITEM mi-Active-Trans  LABEL "&Active Trans"
    MENU-ITEM mi-2PC-TRans     LABEL "&2PC Trans"
    MENU-ITEM mi-Resolve-Limbo LABEL "&Resolve Limbo Trans"
    MENU-ITEM mi-Adj-Latch     LABEL "Adjust &Latch Options"
    MENU-ITEM mi-Adj-PWs       LABEL "Adjust &Page Writer Options".
    
/*** Main Menu ***/

DEFINE MENU mbar MENUBAR
    SUB-MENU sm-Files      LABEL "&Files"
    SUB-MENU stat-Status   LABEL "&Status"
    SUB-MENU act-Activity  LABEL "&Activity"
    SUB-MENU sm-Other      LABEL "&Other"
    SUB-MENU sm-Admin      LABEL "A&dmin"
    MENU-ITEM sm-Adjust     LABEL "Ad&just".
   

ASSIGN DEFAULT-WINDOW:MENUBAR = MENU mbar:HANDLE.

/****  DEFINE FRAMES  ****/

DEFINE FRAME Frame1.


/****  DEFINE TRIGGERS  ****/

ON CHOOSE OF MENU-ITEM mi-Exit
    DO:
       APPLY "CLOSE-WINDOW" TO DEFAULT-WINDOW.
       QUIT.
    END.


/**** STATUS ****/

ON CHOOSE OF MENU-ITEM stat-Database
do:
    hide all.
    my-title = string(TODAY) + 
               "            Status: Database             " +
               string(TIME, "HH:MM:SS").
    for each _DbStatus NO-LOCK:
        display SKIP(1)
            _DbStatus-starttime  LABEL "Database was started at"
                                 COLON 32
            _DbStatus-state      LABEL "Database state"
                                 COLON 32
            _DbStatus-tainted    LABEL "Database damaged flags"
                                 COLON 32
            _DbStatus-IntFlags   LABEL "Integrity flags"
                                 COLON 32
            _DbStatus-LastOpen   LABEL "Most recent database open"
                                 COLON 32
            _DbStatus-PrevOpen   LABEL "Previous database open"
                                 COLON 32
            _DbStatus-CacheStamp LABEL "Local cache file time stamp"
                                 COLON 32
            _DbStatus-DbBlkSize  LABEL "Database block size"
                                 COLON 32
            _DbStatus-TotalBlks  LABEL "Number of blocks allocated"
                                 COLON 32
            _DbStatus-EmptyBlks  LABEL "Empty blocks"
                                 COLON 32
            _DbStatus-FreeBlks   LABEL "Free blocks"
                                 COLON 32
            _DbStatus-RMFreeBlks LABEL "RM blocks with free space"
                                 COLON 32
            _DbStatus-LastTran   LABEL "Last transaction id"
                                 COLON 32
            _DbStatus-LastTable  LABEL "Highest table number defined"
                                 COLON 32
            _DbStatus-DbVers     LABEL "Database version number"
                                 COLON 32
            _DbStatus-ShmVers    LABEL "Shared memory version number"
                                 COLON 32
                     SKIP(1)
                     with frame DbStatus-frame
                          down
                          scrollable
                          use-text
                          side-labels
                          TITLE my-title.
                          
    pause screen-pause.
    hide all.
            
            display skip(1)
            _DbStatus-Integrity  LABEL "Integrity flags"
                                 COLON 32
            _DbStatus-HiWater    LABEL "Database blocks high water mark"
                                 COLON 32
            _DbStatus-BiBlkSize  LABEL "Before image block size (bytes)"
                                 COLON 32
            _DbStatus-BiClSize   LABEL "Before image cluster size (kb)"
                                 COLON 32
            _DbStatus-AiBlkSize  LABEL "After image block size (bytes)"
                                 COLON 32
            _DbStatus-CreateDate LABEL "Database created (multi-volume)"
                                 COLON 32
            _DbStatus-BiOpen     LABEL "Most recent .bi file open"
                                 COLON 32
            _DbStatus-BiSize     LABEL "Logical BI Size"
                                 COLON 32
            _DbStatus-BiTrunc    LABEL "Time since last truncate bi"
                                 COLON 32
            _DbStatus-Codepage   LABEL "Database Character Set"
                                 COLON 32
            _DbStatus-Collation  LABEL "Database Collation Name"
                                 COLON 32
            _DbStatus-NumAreas   LABEL "Number of areas"
                                 COLON 32
            _DbStatus-NumLocks   LABEL "Lock table entries in use"
                                 COLON 32
            _DbStatus-MostLocks  LABEL "Lock table high water mark"
                                 COLON 32
            _DbStatus-SharedMemVer  LABEL "Shared Memory Version #"
                                 COLON 32
            _DbStatus-NumSems    LABEL "Number of semaphores"
                                 COLON 32
            SKIP(1) 
            with frame DbStatus-frame2
                 down
                 scrollable
                 use-text
                 side-labels 
                 TITLE my-title.
        pause screen-pause.
        hide all.
    end.
end.

ON CHOOSE OF MENU-ITEM stat-Backup
do:
    hide all.
    my-title = string(TODAY) + 
               "             Status: Backup              " +
               string(TIME, "HH:MM:SS").
    for each _DbStatus NO-LOCK:
        display SKIP(1) 
                _DbStatus-fbDate  LABEL "Most recent full backup" 
                                  COLON 32
                _DbStatus-ibDate  LABEL "Most recent incremental backup"
                                  COLON 32
                _DbStatus-Changed LABEL "Database changed since backup" 
                                  COLON 32
                _DbStatus-ibSeq   LABEL "Sequence of last incremental"
                                  COLON 32
                skip(16)
                with frame Backup-frame
                     side-labels
                     TITLE my-title.
        pause screen-pause.
        hide all.
    end.
end.

ON CHOOSE OF MENU-ITEM cl-All
do:
    hide all.
    my-title = string(TODAY) + 
               "          Status: All Processes          " +
               string(TIME, "HH:MM:SS").
    for each _Connect where _Connect._Connect-name <> ? NO-LOCK:
        display SKIP(1) 
                _Connect-Id       LABEL "Usr"    FORMAT ">>>>9"
                _Connect-Name     LABEL "Name"
                _Connect-Type     LABEL "Type"
                _Connect-Wait     LABEL "Wait"   FORMAT "x(5)"
                _Connect-Wait1    LABEL "Wait1"  FORMAT ">>>>>>9"
                _Connect-TransId  LABEL "Trans id"  FORMAT ">>>>>>9"
                _Connect-Time     LABEL "Login time"
                _Connect-Pid      LABEL "Pid"    FORMAT ">>>>9"
                with frame Connect-frame
                     down 
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.


ON CHOOSE OF MENU-ITEM cl-Blocked
do:
    hide all.
    my-title = string(TODAY) + 
               "         Status: Blocked Clients         " +
               string(TIME, "HH:MM:SS").
    for each _Connect where _Connect-Wait NE " --" and
                            _Connect-Name NE ? NO-LOCK:
        display SKIP(1)
                _Connect-Id       LABEL "Usr"    FORMAT ">>>>9"
                _Connect-Name     LABEL "Name"
                _Connect-Type     LABEL "Type"
                _Connect-Wait     LABEL "Wait"   FORMAT "x(5)"
                _Connect-Wait1    LABEL "Wait1"  FORMAT ">>>>>>9"
                _Connect-TransId  LABEL "Trans id"
                _Connect-Time     LABEL "Login time"
                _Connect-Pid      LABEL "Pid"    FORMAT ">>>>9"
                with frame Connect-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM cl-Active
do:
    hide all.
    my-title = string(TODAY) + 
               "       Status: Active Transactions       " +
               string(TIME, "HH:MM:SS").
    for each _Connect where _Connect-TransId > 0 NO-LOCK:
        display SKIP(1)
                _Connect-Id       LABEL "Usr"    FORMAT ">>>>9"
                _Connect-Name     LABEL "Name"
                _Connect-Type     LABEL "Type"
                _Connect-Wait     LABEL "Wait"   FORMAT "x(5)"
                _Connect-Wait1    LABEL "Wait1"  FORMAT ">>>>>>9"
                _Connect-TransId  LABEL "Trans id"
                _Connect-Pid      LABEL "Pid"    FORMAT ">>>>9"
                with frame Connect-frame
                     down
                     TITLE my-title.
/* Tx start time
 * Trans State
 */
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM cl-Interactive
do:
    hide all.
    my-title = string(TODAY) + 
               "     Status: Local Interactive Client    " +
               string(TIME, "HH:MM:SS").
    for each _Connect where _Connect-Type = "SELF" NO-LOCK:
        display SKIP(1)
                _Connect-Id       LABEL "Usr"         FORMAT ">>>>9"
                _Connect-Name     LABEL "Name"
                _Connect-Type     LABEL "Type"
                _Connect-Wait     LABEL "Wait"   FORMAT "x(5)"
                _Connect-Wait1    LABEL "Wait1"  FORMAT ">>>>>>9"
                _Connect-TransId  LABEL "Trans id"
                _Connect-Time     LABEL "Login time"
                _Connect-Pid      LABEL "Pid"    FORMAT ">>>>9"
                with frame Connect-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

/* Need to complete Restriction */
ON CHOOSE OF MENU-ITEM cl-Batch     
do:
    hide all.
    my-title = string(TODAY) + 
               "       Status: Local Batch Clients       " +
               string(TIME, "HH:MM:SS").
    /* BATCHUSER and (!SELF or !REMC) */
    for each _Connect where 
                      _Connect-Batch = "Yes"  AND
                      _Connect-Type <> "SELF"  AND
                      _Connect-Type <> "REMC"
                      NO-LOCK:
        display SKIP(1)
                _Connect-Id       LABEL "Usr"    FORMAT ">>>>9"
                _Connect-Name     LABEL "Name"
                _Connect-Type     LABEL "Type"
                _Connect-Wait     LABEL "Wait"   FORMAT "x(5)"
                _Connect-Wait1    LABEL "Wait1"  FORMAT ">>>>>>9"
                _Connect-TransId  LABEL "Trans id"
                _Connect-Time     LABEL "Login time"
                _Connect-Server   LABEL "Serv"   FORMAT ">>>>9"
                with frame Connect-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM cl-Remote  
do:
    hide all.
    my-title = string(TODAY) + 
               "          Status: Remote Clients         " +
               string(TIME, "HH:MM:SS").
    for each _Connect where 
                      _Connect-Type = "REMC" 
                      NO-LOCK:
        display SKIP(1)
                _Connect-Id       LABEL "Usr"    FORMAT ">>>>9"
                _Connect-Name     LABEL "Name"
                _Connect-Type     LABEL "Type"
                _Connect-Wait     LABEL "Wait"   FORMAT "x(5)"
                _Connect-Wait1    LABEL "Wait1"  FORMAT ">>>>>>9"
                _Connect-TransId  LABEL "Trans id"
                _Connect-Time     LABEL "Login time"
                _Connect-Server   LABEL "Serv"   FORMAT ">>>>9"
                with frame Connect-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM cl-Background
do:
    hide all.
    my-title = string(TODAY) + 
               "       Status: Background Processes      " +
               string(TIME, "HH:MM:SS").
    /* Why is !REMC and !SELF considered Background?! */
    for each _Connect where 
                      _Connect-Type <> "REMC" AND
                      _Connect-Type <> "SELF" AND
                      _Connect-name <> ?
                      NO-LOCK:
        display SKIP(1)
                _Connect-Id       LABEL "Usr"    FORMAT ">>>>9"
                _Connect-Name     LABEL "Name"
                _Connect-Type     LABEL "Type"
                _Connect-Time     LABEL "Login time"
                _Connect-Pid      LABEL "Pid"    FORMAT ">>>>9"
                with frame Connect-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.


ON CHOOSE OF MENU-ITEM locks-All
do:
    hide all.
    my-title = string(TODAY) + 
               "           Status: Lock Table            " +
               string(TIME, "HH:MM:SS").
    for each _Lock where _Lock-Usr <> ? NO-LOCK:
        display SKIP(1)
                _Lock-Id         COLUMN-LABEL "Lock Id"     FORMAT ">>>>9"
                _Lock-Usr        COLUMN-LABEL "Usr"
                _Lock-Name       COLUMN-LABEL "Name" 
                _Lock-Type       COLUMN-LABEL "Type"
                _Lock-Recid      COLUMN-LABEL "RECID" 
                _Lock-Chain      COLUMN-LABEL "Chain"
                _Lock-Flags      COLUMN-LABEL "Flags"
                with frame Lock-All-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.


ON CHOOSE OF MENU-ITEM locks-User
do:
    hide all.
    my-title = string(TODAY) + 
               "         Status: Lock Table by User      " +
               string(TIME, "HH:MM:SS").
    for each _UserLock where _UserLock-Usr <> ? NO-LOCK
             with frame UserLock-frame:

        display SKIP(1)
                _UserLock-Id
                _UserLock-Usr
                _UserLock-Name.
        do i = 1 to 34:
            if (_UserLock-Recid[i] <> ?)   then
            do:
                display
                    _UserLock-Type[i]
                    _UserLock-Recid[i]
                    _UserLock-Chain[i]
                    _UserLock-Flags[i].
                down with frame UserLock-frame.
            end.
         end.
         down with frame UserLock-frame.
    end.
    pause screen-pause.
    hide all.
end.


ON CHOOSE OF MENU-ITEM stat-Files
do:
    hide all.
    my-title = string(TODAY) + 
               "             Status: Files               " +
               string(TIME, "HH:MM:SS").
    for each _FileList NO-LOCK:
        display SKIP(1)
                _FileList-Id        COLUMN-LABEL "File!Id"      FORMAT ">>>>9"
                _FileList-Size      COLUMN-LABEL "Size!(KB)"    FORMAT ">>>>9"
                _FileList-Extend    COLUMN-LABEL "Extend!(KB)"  FORMAT ">>>>9"
                _FileList-LogicalSz COLUMN-LABEL "Logical!Size" FORMAT ">>>>9"
                _FileList-BlkSize   COLUMN-LABEL "Block!Size"   FORMAT ">>>>9"
                _FileList-Openmode  COLUMN-LABEL "Open!Mode"    FORMAT "x(4)"
                _FileList-Name      COLUMN-LABEL "File Name"    FORMAT "x(40)"
                with frame FileList-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM stat-Buffers
do:
    hide all.
    my-title = string(TODAY) + 
               "            Status: Buffers              " +
               string(TIME, "HH:MM:SS").
    for each _BuffStatus NO-LOCK:
        display SKIP(1)
                _BfStatus-Id          LABEL "Buffer id"
                                      COLON 25
                _BfStatus-TotBufs     LABEL "Total buffers"
                                      COLON 25
                _BfStatus-HashSize    LABEL "Hash table size"
                                      COLON 25
                _BfStatus-UsedBuffs   LABEL "Used buffers"
                                      COLON 25
                _BfStatus-TotBufs - _BfStatus-UsedBuffs
                                      LABEL "Empty buffers"
                                      COLON 25
                _BfStatus-LRU         LABEL "On lru chain"
                                      COLON 25
                _BfStatus-APWQ        LABEL "On apw queue"
                                      COLON 25
                _BfStatus-CKPQ        LABEL "On ckp queue"
                                      COLON 25
                _BfStatus-ModBuffs    LABEL "Modified buffers"
                                      COLON 25
                _BfStatus-CKPMarked   LABEL "Marked for ckp"
                                      COLON 25
                _BfStatus-LastCkpNum  LABEL "Last checkpoint number"
                                      COLON 25
                skip(16)
                with frame BfStatus-frame
                     side-labels 
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM stat-Logging
do:
    hide all.
    my-title = string(TODAY) + 
               "         Status: Logging Summaary        " +
               string(TIME, "HH:MM:SS").
    for each _Logging NO-LOCK:
        display SKIP(1)
                _Logging-CrashProt   LABEL "Crash protection"
                                     COLON 32
                _Logging-CommitDelay LABEL "Delayed Commit"
                                     COLON 32
                _Logging-BiIO        LABEL "Before-image I/O"
                                     COLON 32
                _Logging-BiClAge     LABEL "Before-image cluster age time"
                                     COLON 32
/*
                                           "BI Writer status"
*/
                _Logging-2PC         LABEL "Two Phase Commit"
                                     COLON 32
                _Logging-AiJournal   LABEL "After-image journalling"
                                     COLON 32
                _Logging-AiIO        LABEL "After-image I/O"
                                     COLON 32
/*
                                           "AI Writer status"
*/
                skip(16)
                with frame Logging-Frame 
                     side-labels
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM stat-BI-Log
do:
    hide all.
    my-title = string(TODAY) + 
               "             Status: BI Log              " +
               string(TIME, "HH:MM:SS").
    for each _Logging NO-LOCK:
        display SKIP(1)
                _Logging-BiClAge     LABEL "Before-image cluster age time"
                                     COLON 32
                _Logging-BiBlkSize   LABEL "Before-image block size"
                                     COLON 32
                _Logging-BiClSize    LABEL "Before-image cluster size"
                                     COLON 32
                _Logging-BiExtents   LABEL "Number of before-image extents"
                                     COLON 32
                _Logging-BiLogSize   LABEL "Before-image log size (kb)"
                                     COLON 32
                _Logging-BiBytesFree LABEL "Bytes free in current cluster"
                                     COLON 32
                _Logging-LastCkp     LABEL "Last checkpoint was at"
                                     COLON 32
                _Logging-BiBuffs     LABEL "Number of BI buffers"
                                     COLON 32
                _Logging-BiFullBuffs LABEL "Full buffers"
                                     COLON 32
                skip(16)
                with frame Bi-Logging-Frame 
                     side-labels
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM stat-AI-Log
do:
    hide all.
    my-title = string(TODAY) + 
               "             Status: AI Log              " +
               string(TIME, "HH:MM:SS").
    for each _Logging NO-LOCK:
        display SKIP(1)
                _Logging-AiBegin     LABEL "After-image begin date"
                                     COLON 32
                _Logging-AiNew       LABEL "After-image new date"
                                     COLON 32
                _Logging-AiOpen      LABEL "After-image open date"
                                     COLON 32
                _Logging-AiGenNum    LABEL "After-image generation number"
                                     COLON 32
                _Logging-AiExtents   LABEL "Number of after-image extents"
                                     COLON 32
                _Logging-AiBuffs     LABEL "Number of AI buffers"
                                     COLON 32
                _Logging-AiBlkSize   LABEL "After-image block size"
                                     COLON 32
                _Logging-AiLogSize   LABEL "After-image log size"
                                     COLON 32
                skip(16)
                with frame AI-Logging-Frame 
                     side-labels
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM stat-2PC
do:
    hide all.
    my-title = string(TODAY) + 
               "         Status: Two-Phase Commit        " +
               string(TIME, "HH:MM:SS").
    for each _Logging NO-LOCK:
        display SKIP(1)
                _Logging-2PCNickName LABEL "Coordinator nickname"
                                     COLON 32
                _Logging-2PCPriority LABEL "Coordinator priority"
                                     COLON 32
                _Logging-CommitDelay LABEL "Delayed commit"
                                     COLON 32
                _Logging-AiJournal   LABEL "After-image Journalling"
                                     COLON 32
                skip(16)
                with frame 2PC-Logging-Frame 
                     side-labels
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.


ON CHOOSE OF MENU-ITEM stat-Startup
do:
    hide all.
    my-title = string(TODAY) + 
               "        Status: Startup Parameters       " + 
               string(TIME, "HH:MM:SS").
    for each _Startup NO-LOCK:
        display
        _Startup-AiName       LABEL "After-image file name (-a)"
                              COLON 44
        _Startup-Buffs        LABEL "Number of database buffers (-B)"
                              COLON 44
        _Startup-AiBuffs      LABEL "Number of after image buffers (-aibufs)"
                              COLON 44
        _Startup-BiBuffs      LABEL "Number of before image buffers (-bibufs)"
                              COLON 44
        _Startup-BiName       LABEL "Before-image file name (-g)"
                              COLON 44
        _Startup-BiTrunc      LABEL "Before-image truncate interval (-G)"
                              COLON 44
        _Startup-CrashProt    LABEL "No crash protection (-i)"
                              COLON 44
        _Startup-Directio     LABEL "Directio startup option enabled (-dirctio)"
                              COLON 44
        _Startup-LockTable    LABEL "Current size of locking table (-L)"
                              COLON 44
        _Startup-MaxClients   LABEL "Maximum number of clients per server (-Ma)"
                              COLON 44
        _Startup-BiDelay      LABEL "Delay of before-image flush (-Mf)"
                              COLON 44
        _Startup-MaxServers   LABEL "Maximum number of servers (-Mn)"
                              COLON 44
        _Startup-MaxUsers     LABEL "Maximum number of users (-n)"
                              COLON 44
        _Startup-BiIO         LABEL "Before-image file I/O (-r -R)"
                              COLON 44
        _Startup-APWQTime     LABEL "APW queue check time"
                              COLON 44
        _Startup-APWSTime     LABEL "APW scan time"
                              COLON 44
        _Startup-APWBuffs     LABEL "APW buffers to scan"
                              COLON 44
        _Startup-APWMaxWrites LABEL "APW max writes / scan"
                              COLON 44
                skip(16)
                with frame Startup-Frame 
                     side-labels
                     TITLE my-title.
        pause screen-pause.
        hide all.
    end.
end.


ON CHOOSE OF MENU-ITEM stat-Servers
do:
    hide all.
    my-title = string(TODAY) + 
               "             Status: Servers             " + 
               string(TIME, "HH:MM:SS").
    for each _Servers NO-LOCK:
        display SKIP(1)
                _Server-Id          COLUMN-LABEL "Srv!id"      FORMAT ">>>9"
                _Server-Num         COLUMN-LABEL "Srv!Num"     FORMAT ">>>>9"
                _Server-Pid         COLUMN-LABEL "Pid"         FORMAT ">>>>9"
                _Server-Type        COLUMN-LABEL "Type"
                _Server-Protocol    COLUMN-LABEL "Protocol"    FORMAT "x(8)"
                _Server-Logins      COLUMN-LABEL "Logins"      FORMAT ">>>>9"
                _Server-CurrUsers   COLUMN-LABEL "Curr!Users"  FORMAT ">>>>9"
                _Server-MaxUsers    COLUMN-LABEL "Max!Users"   FORMAT ">>>>9"
                _Server-PortNum     COLUMN-LABEL "Port!Num"    FORMAT ">>>>9"
                with frame Server-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.


ON CHOOSE OF MENU-ITEM stat-Resources
do:
    hide all.
    my-title = string(TODAY) + 
               "         Status: Shared Resources        " +
               string(TIME, "HH:MM:SS").
    display SKIP(1)
            "Sorry.  This feature is not yet implemented."
            with frame Recources-frame
                 down
                 TITLE my-title.
/*
    for each _Lock NO-LOCK:
        display SKIP(1)
                _Resource-Id    COLUMN-LABEL "Resource Id"     FORMAT ">>>>9"
                with frame Recources-frame
                     down
                     TITLE my-title.
    end.
 */
pause screen-pause.
hide all.
end.

ON CHOOSE OF MENU-ITEM stat-Mem-Seg
do:
    hide all.
    my-title = string(TODAY) + 
               "      Status: Shared Memory Segments     " +
               string(TIME, "HH:MM:SS").
    for each _Segments NO-LOCK:
        display SKIP(1)
                _Segment-Id        LABEL "Segment #"    FORMAT ">>>9"
                _Segment-SegId     LABEL "Segment Id"
                _Segment-SegSize   LABEL "Segment Size"
                _Segment-BytesUsed LABEL "Used"
                _Segment-ByteFree  LABEL "Free"
                skip(16)
                with frame Segment-frame
                     down
                     TITLE my-title.
    end.
        pause screen-pause.
        hide all.
end.

/**** ACTIVITY ****/

ON CHOOSE OF MENU-ITEM act-Summary
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "            Activity: Summary            " +
               string(TIME, "HH:MM:SS").
    find first _ActSummary.
        if tmp-uptime < 1 then do:
           tmp-uptime = _Summary-UpTime.
           tmp-commits = _Summary-Commits.
           tmp-dbreads = _Summary-DbReads.
           tmp-undos = _Summary-Undos.
           tmp-dbwrites = _Summary-DbWrites.
           tmp-recreads = _Summary-RecReads.
           tmp-bireads = _Summary-BiReads.
           tmp-recupd = _Summary-RecUpd.
           tmp-biwrites = _Summary-BiWrites.
           tmp-reccreat = _Summary-RecCreat.
           tmp-aiwrites = _Summary-AiWrites.
           tmp-recdel = _Summary-RecDel.
           tmp-chkpts = _Summary-Chkpts.
           tmp-reclock = _Summary-RecLock.
           tmp-flushed = _Summary-Flushed.
           tmp-recwait = _Summary-RecWait.
        end.


        display SKIP(1)
                _Summary-UpTime    LABEL "DB Up Time"
                _Summary-Commits   LABEL "Commits"
                _Summary-DbReads   LABEL "DB Reads"
                _Summary-Undos     LABEL "Undos"
                _Summary-DbWrites  LABEL "DB Writes"
                _Summary-RecReads  LABEL "Record Reads"
                _Summary-BiReads   LABEL "BI Reads"
                _Summary-RecUpd    LABEL "Record Updates"
                _Summary-BiWrites  LABEL "BI Writes"
                _Summary-RecCreat  LABEL "Record Creates"
                _Summary-AiWrites  LABEL "AI Writes"
                _Summary-RecDel    LABEL "Record Deletes"
                _Summary-Chkpts    LABEL "Checkpoints"
                _Summary-RecLock   LABEL "Record Locks"
                _Summary-Flushed   LABEL "Flushed at chkpt"
                _Summary-RecWait   LABEL "Record Waits"
                 SKIP(1)
                (_Summary-UpTime - tmp-uptime)   LABEL "Interval Time"
                (_Summary-Commits - tmp-commits)   LABEL "Int. Commits"
                (_Summary-DbReads - tmp-dbreads)  LABEL "Int. DB Reads"
                (_Summary-Undos - tmp-undos)    LABEL "Int. Undos"
                (_Summary-DbWrites - tmp-dbwrites) LABEL "Int. DB Writes"
                (_Summary-RecReads - tmp-recreads) LABEL "Int. Rec. Reads"
                (_Summary-BiReads - tmp-bireads)  LABEL "Int. BI Reads"
                (_Summary-RecUpd - tmp-recupd)   LABEL "Int. Rec. Updates"
                (_Summary-BiWrites - tmp-biwrites) LABEL "Int. BI Writes"
                (_Summary-RecCreat - tmp-reccreat) LABEL "Int. Rec. Creates"
                (_Summary-AiWrites - tmp-aiwrites) LABEL "Int. AI Writes"
                (_Summary-RecDel - tmp-recdel)   LABEL "Int. Rec. Deletes"
                (_Summary-Chkpts - tmp-chkpts)   LABEL "Int. Checkpoints"
                (_Summary-RecLock - tmp-reclock)  LABEL "Int. Rec. Locks"
                (_Summary-Flushed - tmp-flushed)  LABEL "Int. Flushed"
                (_Summary-RecWait - tmp-recwait)  LABEL "Int. Rec. Waits"

            SKIP(1)
            with frame Summary-frame
                 2 COLUMNS
                 TITLE my-title.
  /*  end. */

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.
           tmp-uptime = _Summary-UpTime.
           tmp-commits = _Summary-Commits.
           tmp-dbreads = _Summary-DbReads.
           tmp-undos = _Summary-Undos.
           tmp-dbwrites = _Summary-DbWrites.
           tmp-recreads = _Summary-RecReads.
           tmp-bireads = _Summary-BiReads.
           tmp-recupd = _Summary-RecUpd.
           tmp-biwrites = _Summary-BiWrites.
           tmp-reccreat = _Summary-RecCreat.
           tmp-aiwrites = _Summary-AiWrites.
           tmp-recdel = _Summary-RecDel.
           tmp-chkpts = _Summary-Chkpts.
           tmp-reclock = _Summary-RecLock.
           tmp-flushed = _Summary-Flushed.
           tmp-recwait = _Summary-RecWait.


    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-Servers
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "            Activity: Servers            " +
               string(TIME, "HH:MM:SS").
  for each _ActServer NO-LOCK:
        perMinute = _Server-UpTime / 60.
        DISPLAY SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _Server-Id          LABEL "Server Number"          COLON 23
            _Server-MsgRec      LABEL "Messages received"      COLON 23
            _Server-MsgRec / perMinute      NO-LABEL           AT 37
            _Server-MsgRec / _Server-Uptime NO-LABEL           AT 47
            _Server-MsgRec / _Server-Trans  NO-LABEL           AT 57
            _Server-MsgSent     LABEL "Messages sent"          COLON 23
            _Server-MsgSent / perMinute      NO-LABEL          AT 37
            _Server-MsgSent / _Server-Uptime NO-LABEL          AT 47
            _Server-MsgSent / _Server-Trans  NO-LABEL          AT 57
            _Server-ByteRec     LABEL "Bytes received"         COLON 23
            _Server-ByteRec / perMinute      NO-LABEL          AT 37
            _Server-ByteRec / _Server-Uptime NO-LABEL          AT 47
            _Server-ByteRec / _Server-Trans  NO-LABEL          AT 57
            _Server-ByteSent    LABEL "Bytes sent"             COLON 23
            _Server-ByteSent / perMinute      NO-LABEL         AT 37
            _Server-ByteSent / _Server-Uptime NO-LABEL         AT 47
            _Server-ByteSent / _Server-Trans  NO-LABEL         AT 57
            _Server-RecRec      LABEL "Records received"       COLON 23
            _Server-RecRec / perMinute      NO-LABEL           AT 37
            _Server-RecRec / _Server-Uptime NO-LABEL           AT 47
            _Server-RecRec / _Server-Trans  NO-LABEL           AT 57
            _Server-RecSent     LABEL "Records sent"           COLON 23
            _Server-RecSent / perMinute      NO-LABEL          AT 37
            _Server-RecSent / _Server-Uptime NO-LABEL          AT 47
            _Server-RecSent / _Server-Trans  NO-LABEL          AT 57
            _Server-QryRec      LABEL "Queries received"       COLON 23
            _Server-QryRec / perMinute      NO-LABEL           AT 37
            _Server-QryRec / _Server-Uptime NO-LABEL           AT 47
            _Server-QryRec / _Server-Trans  NO-LABEL           AT 57
            _Server-TimeSlice   LABEL "Time slices"            COLON 23
            _Server-TimeSlice / perMinute      NO-LABEL        AT 37
            _Server-TimeSlice / _Server-Uptime NO-LABEL        AT 47
            _Server-TimeSlice / _Server-Trans  NO-LABEL        AT 57
            skip(16)
            with frame Act-Server-frame
                 Side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-Buffers
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "          Activity: Buffer Cache         " +
               string(TIME, "HH:MM:SS").
  for each _ActBuffer NO-LOCK:
    perMinute = _Buffer-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _Buffer-UpTime     LABEL "Database Up Time"       COLON 23
            _Buffer-LogicRds   LABEL "Logical reads"          COLON 23
            _Buffer-LogicRds / perMinute      NO-LABEL        AT 37
            _Buffer-LogicRds / _Buffer-UpTime NO-LABEL        AT 47
            _Buffer-LogicRds / _Buffer-Trans  NO-LABEL        AT 57
            _Buffer-LogicWrts  LABEL "Logical writes"         COLON 23
            _Buffer-LogicWrts / perMinute      NO-LABEL       AT 37
            _Buffer-LogicWrts / _Buffer-UpTime NO-LABEL       AT 47
            _Buffer-LogicWrts / _Buffer-Trans  NO-LABEL       AT 57
            _Buffer-OSRds      LABEL "O/S reads"              COLON 23
            _Buffer-OSRds / perMinute      NO-LABEL           AT 37
            _Buffer-OSRds / _Buffer-UpTime NO-LABEL           AT 47
            _Buffer-OSRds / _Buffer-Trans  NO-LABEL           AT 57
            _Buffer-OSWrts     LABEL "O/S writes"             COLON 23
            _Buffer-OSWrts / perMinute      NO-LABEL          AT 37
            _Buffer-OSWrts / _Buffer-UpTime NO-LABEL          AT 47
            _Buffer-OSWrts / _Buffer-Trans  NO-LABEL          AT 57
            _Buffer-Chkpts     LABEL "Checkpoints"            COLON 23
            _Buffer-Chkpts / perMinute      NO-LABEL          AT 37
            _Buffer-Chkpts / _Buffer-UpTime NO-LABEL          AT 47
            _Buffer-Chkpts / _Buffer-Trans  NO-LABEL          AT 57
            _Buffer-Marked     LABEL "Marked to checkpoint"   COLON 23
            _Buffer-Marked / perMinute      NO-LABEL          AT 37
            _Buffer-Marked / _Buffer-UpTime NO-LABEL          AT 47
            _Buffer-Marked / _Buffer-Trans  NO-LABEL          AT 57
            _Buffer-Flushed    LABEL "Flushed at checkpoint"  COLON 23
            _Buffer-Flushed / perMinute      NO-LABEL         AT 37
            _Buffer-Flushed / _Buffer-UpTime NO-LABEL         AT 47
            _Buffer-Flushed / _Buffer-Trans  NO-LABEL         AT 57
            _Buffer-Deferred   LABEL "Writes deferred"        COLON 23
            _Buffer-Deferred / perMinute      NO-LABEL        AT 37
            _Buffer-Deferred / _Buffer-UpTime NO-LABEL        AT 47
            _Buffer-Deferred / _Buffer-Trans  NO-LABEL        AT 57
            _Buffer-LRUSkips   LABEL "LRU skips"              COLON 23
            _Buffer-LRUSkips / perMinute      NO-LABEL        AT 37
            _Buffer-LRUSkips / _Buffer-UpTime NO-LABEL        AT 47
            _Buffer-LRUSkips / _Buffer-Trans  NO-LABEL        AT 57
            _Buffer-LRUWrts    LABEL "LRU writes"             COLON 23
            _Buffer-LRUWrts / perMinute      NO-LABEL         AT 37
            _Buffer-LRUWrts / _Buffer-UpTime NO-LABEL         AT 47
            _Buffer-LRUWrts / _Buffer-Trans  NO-LABEL         AT 57
            _Buffer-APWEnq     LABEL "APW enqueues"           COLON 23
            _Buffer-APWEnq / perMinute      NO-LABEL          AT 37
            _Buffer-APWEnq / _Buffer-UpTime NO-LABEL          AT 47
            _Buffer-APWEnq / _Buffer-Trans  NO-LABEL          AT 57

            skip(16)
            with frame Act-Buffer-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-Page-Ws
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "          Activity: Page Writers         " +
               string(TIME, "HH:MM:SS").
  for each _ActPWs NO-LOCK:
    perMinute = _PW-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _PW-UpTime       LABEL "Database Up Time"       COLON 23
            _PW-TotDBWrites  LABEL "Total DB writes"        COLON 23
            _PW-TotDBWrites / perMinute  NO-LABEL           AT 37
            _PW-TotDBWrites / _PW-UpTime NO-LABEL           AT 47
            _PW-TotDBWrites / _PW-Trans  NO-LABEL           AT 57
            _PW-DBWrites     LABEL "APW DB writes"          COLON 23
            _PW-DBWrites / perMinute  NO-LABEL              AT 37
            _PW-DBWrites / _PW-UpTime NO-LABEL              AT 47
            _PW-DBWrites / _PW-Trans  NO-LABEL              AT 57
            _PW-ScanWrites   LABEL "    scan writes"        COLON 23
            _PW-ScanWrites / perMinute  NO-LABEL            AT 37
            _PW-ScanWrites / _PW-UpTime NO-LABEL            AT 47
            _PW-ScanWrites / _PW-Trans  NO-LABEL            AT 57
            _PW-ApwQWrites   LABEL "    apw queue writes"   COLON 23
            _PW-ApwQWrites / perMinute  NO-LABEL            AT 37
            _PW-ApwQWrites / _PW-UpTime NO-LABEL            AT 47
            _PW-ApwQWrites / _PW-Trans  NO-LABEL            AT 57
            _PW-CkpQWrites   LABEL "    ckp queue writes"   COLON 23
            _PW-CkpQWrites / perMinute  NO-LABEL            AT 37
            _PW-CkpQWrites / _PW-UpTime NO-LABEL            AT 47
            _PW-CkpQWrites / _PW-Trans  NO-LABEL            AT 57
            _PW-ScanCycles   LABEL "    scan cycles"        COLON 23
            _PW-ScanCycles / perMinute  NO-LABEL            AT 37
            _PW-ScanCycles / _PW-UpTime NO-LABEL            AT 47
            _PW-ScanCycles / _PW-Trans  NO-LABEL            AT 57
            _PW-BuffsScaned  LABEL "    buffers scanned"    COLON 23
            _PW-BuffsScaned / perMinute  NO-LABEL           AT 37
            _PW-BuffsScaned / _PW-UpTime NO-LABEL           AT 47
            _PW-BuffsScaned / _PW-Trans  NO-LABEL           AT 57
            _PW-BufsCkp      LABEL "    bufs checkpointed"  COLON 23
            _PW-BufsCkp / perMinute  NO-LABEL               AT 37
            _PW-BufsCkp / _PW-UpTime NO-LABEL               AT 47
            _PW-BufsCkp / _PW-Trans  NO-LABEL               AT 57
            _PW-Checkpoints  LABEL "Checkpoints"            COLON 23
            _PW-Checkpoints / perMinute  NO-LABEL           AT 37
            _PW-Checkpoints / _PW-UpTime NO-LABEL           AT 47
            _PW-Checkpoints / _PW-Trans  NO-LABEL           AT 57
            _PW-Marked       LABEL "Marked at checkpoint"   COLON 23
            _PW-Marked / perMinute  NO-LABEL                AT 37
            _PW-Marked / _PW-UpTime NO-LABEL                AT 47
            _PW-Marked / _PW-Trans  NO-LABEL                AT 57
            _PW-Flushed      LABEL "Flushed at checkpoint"  COLON 23
            _PW-Flushed / perMinute  NO-LABEL               AT 37
            _PW-Flushed / _PW-UpTime NO-LABEL               AT 47
            _PW-Flushed / _PW-Trans  NO-LABEL               AT 57
            skip(16)
            with frame Act-PW-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-BI-Log
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "            Activity: BI Log             " +
               string(TIME, "HH:MM:SS").
  for each _ActBiLog NO-LOCK:
    perMinute = _BiLog-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _BiLog-UpTime      LABEL "Database Up Time"       COLON 23
            _BiLog-TotalWrts   LABEL "Total BI writes"        COLON 23
            _BiLog-TotalWrts / perMinute     NO-LABEL         AT 37
            _BiLog-TotalWrts / _BiLog-UpTime NO-LABEL         AT 47
            _BiLog-TotalWrts / _BiLog-Trans  NO-LABEL         AT 57
            _BiLog-BIWWrites   LABEL "BIW BI writes"          COLON 23
            _BiLog-BIWWrites / perMinute     NO-LABEL         AT 37
            _BiLog-BIWWrites / _BiLog-UpTime NO-LABEL         AT 47
            _BiLog-BIWWrites / _BiLog-Trans  NO-LABEL         AT 57
            _BiLog-RecWriten   LABEL "Records written"        COLON 23
            _BiLog-RecWriten / perMinute     NO-LABEL         AT 37
            _BiLog-RecWriten / _BiLog-UpTime NO-LABEL         AT 47
            _BiLog-RecWriten / _BiLog-Trans  NO-LABEL         AT 57
            _BiLog-BytesWrtn   LABEL "Bytes written"          COLON 23
            _BiLog-BytesWrtn / perMinute     NO-LABEL         AT 37
            _BiLog-BytesWrtn / _BiLog-UpTime NO-LABEL         AT 47
            _BiLog-BytesWrtn / _BiLog-Trans  NO-LABEL         AT 57
            _BiLog-TotReads    LABEL "Total BI Reads"         COLON 23
            _BiLog-TotReads / perMinute     NO-LABEL          AT 37
            _BiLog-TotReads / _BiLog-UpTime NO-LABEL          AT 47
            _BiLog-TotReads / _BiLog-Trans  NO-LABEL          AT 57
            _BiLog-RecRead     LABEL "Records read"           COLON 23
            _BiLog-RecRead / perMinute     NO-LABEL           AT 37
            _BiLog-RecRead / _BiLog-UpTime NO-LABEL           AT 47
            _BiLog-RecRead / _BiLog-Trans  NO-LABEL           AT 57
            _BiLog-BytesRead   LABEL "Bytes read"             COLON 23
            _BiLog-BytesRead / perMinute     NO-LABEL         AT 37
            _BiLog-BytesRead / _BiLog-UpTime NO-LABEL         AT 47
            _BiLog-BytesRead / _BiLog-Trans  NO-LABEL         AT 57
            _BiLog-ClstrClose  LABEL "Clusters closed"        COLON 23
            _BiLog-ClstrClose / perMinute     NO-LABEL        AT 37
            _BiLog-ClstrClose / _BiLog-UpTime NO-LABEL        AT 47
            _BiLog-ClstrClose / _BiLog-Trans  NO-LABEL        AT 57
            _BiLog-BBuffWaits  LABEL "Busy buffer waits"      COLON 23
            _BiLog-BBuffWaits / perMinute     NO-LABEL        AT 37
            _BiLog-BBuffWaits / _BiLog-UpTime NO-LABEL        AT 47
            _BiLog-BBuffWaits / _BiLog-Trans  NO-LABEL        AT 57
            _BiLog-EBuffWaits  LABEL "Empty buffer waits"     COLON 23
            _BiLog-EBuffWaits / perMinute     NO-LABEL        AT 37
            _BiLog-EBuffWaits / _BiLog-UpTime NO-LABEL        AT 47
            _BiLog-EBuffWaits / _BiLog-Trans  NO-LABEL        AT 57
            _BiLog-ForceWaits  LABEL "Log force waits"        COLON 23
            _BiLog-ForceWaits / perMinute     NO-LABEL        AT 37
            _BiLog-ForceWaits / _BiLog-UpTime NO-LABEL        AT 47
            _BiLog-ForceWaits / _BiLog-Trans  NO-LABEL        AT 57
            _BiLog-ForceWrts   LABEL "Log force writes"       COLON 23
            _BiLog-ForceWrts / perMinute     NO-LABEL         AT 37
            _BiLog-ForceWrts / _BiLog-UpTime NO-LABEL         AT 47
            _BiLog-ForceWrts / _BiLog-Trans  NO-LABEL         AT 57
            _BiLog-PartialWrts LABEL "Partial writes"         COLON 23
            _BiLog-PartialWrts / perMinute     NO-LABEL       AT 37
            _BiLog-PartialWrts / _BiLog-UpTime NO-LABEL       AT 47
            _BiLog-PartialWrts / _BiLog-Trans  NO-LABEL       AT 57

            skip(16)
            with frame Act-BiLog-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-AI-Log
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "           Activity: AI Log              " +
               string(TIME, "HH:MM:SS").
  for each _ActAiLog NO-LOCK:
    perMinute = _AiLog-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _AiLog-UpTime      LABEL "Database Up Time"       COLON 23
            _AiLog-TotWrites   LABEL "Total AI writes"        COLON 23
            _AiLog-TotWrites / perMinute     NO-LABEL         AT 37
            _AiLog-TotWrites / _AiLog-UpTime NO-LABEL         AT 47
            _AiLog-TotWrites / _AiLog-Trans  NO-LABEL         AT 57
            _AiLog-AIWWrites   LABEL "AIW AI writes"          COLON 23
            _AiLog-AIWWrites / perMinute     NO-LABEL         AT 37
            _AiLog-AIWWrites / _AiLog-UpTime NO-LABEL         AT 47
            _AiLog-AIWWrites / _AiLog-Trans  NO-LABEL         AT 57
            _AiLog-RecWriten   LABEL "Records written"        COLON 23
            _AiLog-RecWriten / perMinute     NO-LABEL         AT 37
            _AiLog-RecWriten / _AiLog-UpTime NO-LABEL         AT 47
            _AiLog-RecWriten / _AiLog-Trans  NO-LABEL         AT 57
            _AiLog-BytesWritn  LABEL "Bytes written"          COLON 23
            _AiLog-BytesWritn / perMinute     NO-LABEL        AT 37
            _AiLog-BytesWritn / _AiLog-UpTime NO-LABEL        AT 47
            _AiLog-BytesWritn / _AiLog-Trans  NO-LABEL        AT 57
            _AiLog-BBuffWaits  LABEL "Busy buffer waits"      COLON 23
            _AiLog-BBuffWaits / perMinute     NO-LABEL        AT 37
            _AiLog-BBuffWaits / _AiLog-UpTime NO-LABEL        AT 47
            _AiLog-BBuffWaits / _AiLog-Trans  NO-LABEL        AT 57
            _AiLog-NoBufAvail  LABEL "Buffer not avail"       COLON 23
            _AiLog-NoBufAvail / perMinute     NO-LABEL        AT 37
            _AiLog-NoBufAvail / _AiLog-UpTime NO-LABEL        AT 47
            _AiLog-NoBufAvail / _AiLog-Trans  NO-LABEL        AT 57
            _AiLog-PartialWrt  LABEL "Partial writes"         COLON 23
            _AiLog-PartialWrt / perMinute     NO-LABEL        AT 37
            _AiLog-PartialWrt / _AiLog-UpTime NO-LABEL        AT 47
            _AiLog-PartialWrt / _AiLog-Trans  NO-LABEL        AT 57
            _AiLog-ForceWaits  LABEL "Log force waits"        COLON 23
            _AiLog-ForceWaits / perMinute     NO-LABEL        AT 37
            _AiLog-ForceWaits / _AiLog-UpTime NO-LABEL        AT 47
            _AiLog-ForceWaits / _AiLog-Trans  NO-LABEL        AT 57

            skip(16)
            with frame Act-AiLog-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.


ON CHOOSE OF MENU-ITEM act-Locks
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "           Activity: Lock Table          " +
               string(TIME, "HH:MM:SS").
  for each _ActLock NO-LOCK:
    hide all.
    perMinute = _Lock-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            _Lock-UpTime     LABEL "Database Up Time"       COLON 20
            _Lock-ShrReq     LABEL "Share requests"         COLON 20
            _Lock-ShrReq / perMinute    NO-LABEL            AT 37
            _Lock-ShrReq / _Lock-UpTime NO-LABEL            AT 47
            _Lock-ShrReq / _Lock-Trans  NO-LABEL            AT 57
            _Lock-ExclReq    LABEL "Exclusive requests"     COLON 20
            _Lock-ExclReq / perMinute    NO-LABEL           AT 37
            _Lock-ExclReq / _Lock-UpTime NO-LABEL           AT 47
            _Lock-ExclReq / _Lock-Trans  NO-LABEL           AT 57
            _Lock-UpgReq     LABEL "Upgrade requests"       COLON 20
            _Lock-UpgReq / perMinute    NO-LABEL            AT 37
            _Lock-UpgReq / _Lock-UpTime NO-LABEL            AT 47
            _Lock-UpgReq / _Lock-Trans  NO-LABEL            AT 57
            _Lock-RecGetReq  LABEL "Rec Get requests"       COLON 20
            _Lock-RecGetReq / perMinute    NO-LABEL         AT 37
            _Lock-RecGetReq / _Lock-UpTime NO-LABEL         AT 47
            _Lock-RecGetReq / _Lock-Trans  NO-LABEL         AT 57
            _Lock-ShrLock    LABEL "Share grants"           COLON 20
            _Lock-ShrLock / perMinute    NO-LABEL           AT 37
            _Lock-ShrLock / _Lock-UpTime NO-LABEL           AT 47
            _Lock-ShrLock / _Lock-Trans  NO-LABEL           AT 57
            _Lock-ExclLock   LABEL "Exclusive grants"       COLON 20
            _Lock-ExclLock / perMinute    NO-LABEL          AT 37
            _Lock-ExclLock / _Lock-UpTime NO-LABEL          AT 47
            _Lock-ExclLock / _Lock-Trans  NO-LABEL          AT 57
            _Lock-UpgLock    LABEL "Upgrade grants"         COLON 20
            _Lock-UpgLock / perMinute    NO-LABEL           AT 37
            _Lock-UpgLock / _Lock-UpTime NO-LABEL           AT 47
            _Lock-UpgLock / _Lock-Trans  NO-LABEL           AT 57
            _Lock-RecGetLock LABEL "Rec Get grants"         COLON 20
            _Lock-RecGetLock / perMinute    NO-LABEL        AT 37
            _Lock-RecGetLock / _Lock-UpTime NO-LABEL        AT 47
            _Lock-RecGetLock / _Lock-Trans  NO-LABEL        AT 57
            _Lock-ShrWait    LABEL "Share waits"            COLON 20 
            _Lock-ShrWait / perMinute    NO-LABEL           AT 37
            _Lock-ShrWait / _Lock-UpTime NO-LABEL           AT 47
            _Lock-ShrWait / _Lock-Trans  NO-LABEL           AT 57
            _Lock-ExclWait   LABEL "Exclusive waits"        COLON 20
            _Lock-ExclWait / perMinute    NO-LABEL          AT 37
            _Lock-ExclWait / _Lock-UpTime NO-LABEL          AT 47
            _Lock-ExclWait / _Lock-Trans  NO-LABEL          AT 57
            _Lock-UpgWait    LABEL "Upgrade waits"          COLON 20
            _Lock-UpgWait / perMinute    NO-LABEL           AT 37
            _Lock-UpgWait / _Lock-UpTime NO-LABEL           AT 47
            _Lock-UpgWait / _Lock-Trans  NO-LABEL           AT 57
            _Lock-RecGetWait LABEL "Rec Get waits"          COLON 20
            _Lock-RecGetWait / perMinute    NO-LABEL        AT 37
            _Lock-RecGetWait / _Lock-UpTime NO-LABEL        AT 47
            _Lock-RecGetWait / _Lock-Trans  NO-LABEL        AT 57
            _Lock-CanclReq   LABEL "Requests cancelled"     COLON 20
            _Lock-CanclReq / perMinute    NO-LABEL          AT 37
            _Lock-CanclReq / _Lock-UpTime NO-LABEL          AT 47
            _Lock-CanclReq / _Lock-Trans  NO-LABEL          AT 57
            _Lock-Downgrade  LABEL "Downgrades"             COLON 20
            _Lock-Downgrade / perMinute    NO-LABEL         AT 37
            _Lock-Downgrade / _Lock-UpTime NO-LABEL         AT 47
            _Lock-Downgrade / _Lock-Trans  NO-LABEL         AT 57
            _Lock-RedReq     LABEL "Redundant requests"     COLON 20
            _Lock-RedReq / perMinute    NO-LABEL            AT 37
            _Lock-RedReq / _Lock-UpTime NO-LABEL            AT 47
            _Lock-RedReq / _Lock-Trans  NO-LABEL            AT 57

            skip(16)
            with frame Act-Lock-frame2
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-IO-Type
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "     Activity: I/O Operations by Type    " +
               string(TIME, "HH:MM:SS").
  for each _ActIOType NO-LOCK:
    perMinute = _IOType-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _IOType-UpTime    LABEL "Database Up Time"       COLON 23
            _IOType-IdxRds + _IOType-DataReads
                              LABEL "Database reads"         COLON 23
            (_IOType-IdxRds + _IOType-DataReads) / perMinute 
                              NO-LABEL                       AT 37
            (_IOType-IdxRds + _IOType-DataReads) / _IOType-UpTime
                              NO-LABEL                       AT 47
            (_IOType-IdxRds + _IOType-DataReads) / _IOType-Trans 
                               NO-LABEL                      AT 57
            _IOType-IdxRds    LABEL "DB Index block Reads"   COLON 23
            _IOType-IdxRds / perMinute      NO-LABEL         AT 37
            _IOType-IdxRds / _IOType-UpTime NO-LABEL         AT 47
            _IOType-IdxRds / _IOType-Trans  NO-LABEL         AT 57
            _IOType-DataReads LABEL "DB Data block Reads"    COLON 23
            _IOType-DataReads / perMinute      NO-LABEL      AT 37
            _IOType-DataReads / _IOType-UpTime NO-LABEL      AT 47
            _IOType-DataReads / _IOType-Trans  NO-LABEL      AT 57
            _IOType-BiRds     LABEL "BI reads"               COLON 23
            _IOType-BiRds / perMinute      NO-LABEL          AT 37
            _IOType-BiRds / _IOType-UpTime NO-LABEL          AT 47
            _IOType-BiRds / _IOType-Trans  NO-LABEL          AT 57
            _IOType-AiRds     LABEL "AI reads"               COLON 23
            _IOType-AiRds / perMinute      NO-LABEL          AT 37
            _IOType-AiRds / _IOType-UpTime NO-LABEL          AT 47
            _IOType-AiRds / _IOType-Trans  NO-LABEL          AT 57
            _IOType-IdxRds + _IOType-DataReads + _IOType-BiRds + _IOType-AiRds 
                              LABEL "Total reads"            COLON 23
            (_IOType-IdxRds + _IOType-DataReads + _IOType-BiRds + _IOType-AiRds)
              / perMinute     NO-LABEL                       AT 37
            (_IOType-IdxRds + _IOType-DataReads + _IOType-BiRds + _IOType-AiRds)
              / _IOType-UpTime NO-LABEL                      AT 47
            (_IOType-IdxRds + _IOType-DataReads + 
             _IOType-BiRds + _IOType-AiRds) / _IOType-Trans 
                               NO-LABEL                      AT 57
            SKIP(1)
            _IOType-IdxWrts + _IOType-DataWrts
                              LABEL "Database Writes"        COLON 23
            (_IOType-IdxWrts + _IOType-DataWrts) / perMinute  
                              NO-LABEL                       AT 37
            (_IOType-IdxWrts + _IOType-DataWrts) / _IOType-UpTime 
                              NO-LABEL                       AT 47
            (_IOType-IdxWrts + _IOType-DataWrts) / _IOType-Trans 
                               NO-LABEL                      AT 57
            _IOType-IdxWrts   LABEL "DB Index block Writes"  COLON 23
            _IOType-IdxWrts / perMinute      NO-LABEL        AT 37
            _IOType-IdxWrts / _IOType-UpTime NO-LABEL        AT 47
            _IOType-IdxWrts / _IOType-Trans  NO-LABEL        AT 57
            _IOType-DataWrts  LABEL "DB Data block Writes"   COLON 23
            _IOType-DataWrts / perMinute      NO-LABEL       AT 37
            _IOType-DataWrts / _IOType-UpTime NO-LABEL       AT 47
            _IOType-DataWrts / _IOType-Trans  NO-LABEL       AT 57
            _IOType-BiWrts    LABEL "BI writes"              COLON 23
            _IOType-BiWrts / perMinute      NO-LABEL         AT 37
            _IOType-BiWrts / _IOType-UpTime NO-LABEL         AT 47
            _IOType-BiWrts / _IOType-Trans  NO-LABEL         AT 57
            _IOType-AiWrts    LABEL "AI writes"              COLON 23
            _IOType-AiWrts / perMinute      NO-LABEL         AT 37
            _IOType-AiWrts / _IOType-UpTime NO-LABEL         AT 47
            _IOType-AiWrts / _IOType-Trans  NO-LABEL         AT 57
            _IOType-IdxWrts + _IOType-DataWrts + 
                              _IOType-BiWrts + _IOType-AiWrts 
                              LABEL "Total writes"           COLON 23
            (_IOType-IdxWrts + _IOType-DataWrts + _IOType-BiWrts +
                              _IOType-AiWrts) / perMinute  
                              NO-LABEL                        AT 37
            (_IOType-IdxWrts + _IOType-DataWrts + _IOType-BiWrts +
                              _IOType-AiWrts) / _IOType-UpTime
                              NO-LABEL                        AT 47
            (_IOType-IdxWrts + _IOType-DataWrts + 
                              _IOType-BiWrts + _IOType-AiWrts) / _IOType-Trans 
                              NO-LABEL                       AT 57
            skip(16)
            with frame Act-IOType-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-IO-File
do: 
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "     Activity: I/O Operations by File    " +
               string(TIME, "HH:MM:SS").
  for each _ActIOFile NO-LOCK:
    perMinute = _IOFile-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _IOFile-UpTime    NO-LABEL
            _IOFile-FileName  NO-LABEL FORMAT "x(60)"
            SKIP(1)
            _IOFile-Reads     LABEL "Reads"                    COLON 23
            _IOFile-Reads / perMinute    NO-LABEL              AT 37
            _IOFile-Reads / _IOFile-UpTime NO-LABEL            AT 47
            _IOFile-Reads / _IOFile-Trans  NO-LABEL            AT 57
            _IOFile-BufReads     LABEL "Buffered Reads"        COLON 23
            _IOFile-BufReads / perMinute    NO-LABEL           AT 37
            _IOFile-BufReads / _IOFile-UpTime NO-LABEL         AT 47
            _IOFile-BufReads / _IOFile-Trans  NO-LABEL         AT 57
            _IOFile-UbufReads     LABEL "Unbuffered Reads"     COLON 23
            _IOFile-UbufReads / perMinute    NO-LABEL          AT 37
            _IOFile-UbufReads / _IOFile-UpTime NO-LABEL        AT 47
            _IOFile-UbufReads / _IOFile-Trans  NO-LABEL        AT 57

            _IOFile-Writes    LABEL "Writes"                   COLON 23
            _IOFile-Writes / perMinute    NO-LABEL             AT 37
            _IOFile-Writes / _IOFile-UpTime NO-LABEL           AT 47
            _IOFile-Writes / _IOFile-Trans  NO-LABEL           AT 57
            _IOFile-BufWrites    LABEL "Buffered Writes"       COLON 23
            _IOFile-BufWrites / perMinute    NO-LABEL          AT 37
            _IOFile-BufWrites / _IOFile-UpTime NO-LABEL        AT 47
            _IOFile-BufWrites / _IOFile-Trans  NO-LABEL        AT 57
            _IOFile-UbufWrites    LABEL "Unbuffered Writes"    COLON 23
            _IOFile-UbufWrites / perMinute    NO-LABEL         AT 37
            _IOFile-UbufWrites / _IOFile-UpTime NO-LABEL       AT 47
            _IOFile-UbufWrites / _IOFile-Trans  NO-LABEL       AT 57

            _IOFile-Extends   LABEL "Extends"                  COLON 23
            _IOFile-Extends / perMinute    NO-LABEL            AT 37
            _IOFile-Extends / _IOFile-UpTime NO-LABEL          AT 47
            _IOFile-Extends / _IOFile-Trans  NO-LABEL          AT 57
            skip(16)
            with frame Act-IOFile-frame
                 side-labels
                 TITLE my-title.
       pause screen-pause.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-Space
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "        Activity: Space Allocation       " +
               string(TIME, "HH:MM:SS").
  for each _ActSpace NO-LOCK:
    perMinute = _Space-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _Space-UpTime      LABEL "Database Up Time"       COLON 24
            _Space-DbExd       LABEL "Database extends"       COLON 24
            _Space-DbExd / perMinute     NO-LABEL             AT 37
            _Space-DbExd / _Space-UpTime NO-LABEL             AT 47
            _Space-DbExd / _Space-Trans  NO-LABEL             AT 57
            _Space-TakeFree    LABEL "Take free block"        COLON 24
            _Space-TakeFree / perMinute     NO-LABEL          AT 37
            _Space-TakeFree / _Space-UpTime NO-LABEL          AT 47
            _Space-TakeFree / _Space-Trans  NO-LABEL          AT 57
            _Space-RetFree     LABEL "Return free block"      COLON 24
            _Space-RetFree / perMinute     NO-LABEL           AT 37
            _Space-RetFree / _Space-UpTime NO-LABEL           AT 47
            _Space-RetFree / _Space-Trans  NO-LABEL           AT 57
            _Space-AllocNewRm  LABEL "Alloc rm space"         COLON 24
            _Space-AllocNewRm / perMinute     NO-LABEL        AT 37
            _Space-AllocNewRm / _Space-UpTime NO-LABEL        AT 47
            _Space-AllocNewRm / _Space-Trans  NO-LABEL        AT 57
            _Space-FromRm      LABEL "Alloc from rm"          COLON 24
            _Space-FromRm / perMinute     NO-LABEL            AT 37
            _Space-FromRm / _Space-UpTime NO-LABEL            AT 47
            _Space-FromRm / _Space-Trans  NO-LABEL            AT 57
            _Space-FromFree    LABEL "Alloc from free"        COLON 24
            _Space-FromFree / perMinute     NO-LABEL          AT 37
            _Space-FromFree / _Space-UpTime NO-LABEL          AT 47
            _Space-FromFree / _Space-Trans  NO-LABEL          AT 57
            _Space-BytesAlloc  LABEL "Bytes allocated"        COLON 24
            _Space-BytesAlloc / perMinute     NO-LABEL        AT 37
            _Space-BytesAlloc / _Space-UpTime NO-LABEL        AT 47
            _Space-BytesAlloc / _Space-Trans  NO-LABEL        AT 57
            _Space-Examined    LABEL "rm blocks examined"     COLON 24
            _Space-Examined / perMinute     NO-LABEL          AT 37
            _Space-Examined / _Space-UpTime NO-LABEL          AT 47
            _Space-Examined / _Space-Trans  NO-LABEL          AT 57
            _Space-Removed     LABEL "Remove from rm"         COLON 24
            _Space-Removed / perMinute     NO-LABEL           AT 37
            _Space-Removed / _Space-UpTime NO-LABEL           AT 47
            _Space-Removed / _Space-Trans  NO-LABEL           AT 57
            _Space-FrontAdd    LABEL "Add to rm, front"       COLON 24
            _Space-FrontAdd / perMinute     NO-LABEL          AT 37
            _Space-FrontAdd / _Space-UpTime NO-LABEL          AT 47
            _Space-FrontAdd / _Space-Trans  NO-LABEL          AT 57
            _Space-BackAdd     LABEL "Add to rm, back"        COLON 24
            _Space-BackAdd / perMinute     NO-LABEL           AT 37
            _Space-BackAdd / _Space-UpTime NO-LABEL           AT 47
            _Space-BackAdd / _Space-Trans  NO-LABEL           AT 57
            _Space-Front2Back  LABEL "Move rm front to back"  COLON 24
            _Space-Front2Back / perMinute     NO-LABEL        AT 37
            _Space-Front2Back / _Space-UpTime NO-LABEL        AT 47
            _Space-Front2Back / _Space-Trans  NO-LABEL        AT 57
            _Space-Locked      LABEL "Remove locked rm entry" COLON 24
            _Space-Locked / perMinute     NO-LABEL            AT 37
            _Space-Locked / _Space-UpTime NO-LABEL            AT 47
            _Space-Locked / _Space-Trans  NO-LABEL            AT 57

            skip(16)
            with frame Act-Space-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-Index
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "             Activity: Index             " +
               string(TIME, "HH:MM:SS").
  for each _ActIndex NO-LOCK:
    perMinute = _Index-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _Index-UpTime  LABEL "Database Up Time"       COLON 23
            _Index-Find    LABEL "Find index entry"       COLON 23
            _Index-Find / perMinute     NO-LABEL          AT 37
            _Index-Find / _Index-UpTime NO-LABEL          AT 47
            _Index-Find / _Index-Trans  NO-LABEL          AT 57
            _Index-Create  LABEL "Create index entry"     COLON 23
            _Index-Create / perMinute     NO-LABEL        AT 37
            _Index-Create / _Index-UpTime NO-LABEL        AT 47
            _Index-Create / _Index-Trans  NO-LABEL        AT 57
            _Index-Delete  LABEL "Delete index entry"     COLON 23
            _Index-Delete / perMinute     NO-LABEL        AT 37
            _Index-Delete / _Index-UpTime NO-LABEL        AT 47
            _Index-Delete / _Index-Trans  NO-LABEL        AT 57
            _Index-Remove  LABEL "Remove locked entry"    COLON 23
            _Index-Remove / perMinute     NO-LABEL        AT 37
            _Index-Remove / _Index-UpTime NO-LABEL        AT 47
            _Index-Remove / _Index-Trans  NO-LABEL        AT 57
            _Index-Splits  LABEL "Split block"            COLON 23
            _Index-Splits / perMinute     NO-LABEL        AT 37
            _Index-Splits / _Index-UpTime NO-LABEL        AT 47
            _Index-Splits / _Index-Trans  NO-LABEL        AT 57
            _Index-Free    LABEL "Free block"             COLON 23
            _Index-Free / perMinute     NO-LABEL          AT 37
            _Index-Free / _Index-UpTime NO-LABEL          AT 47
            _Index-Free / _Index-Trans  NO-LABEL          AT 57
            skip(16)
            with frame Act-Index-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-Record
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "            Activity: Record             " +
               string(TIME, "HH:MM:SS").
  for each _ActRecord NO-LOCK:
    perMinute = _Record-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _Record-UpTime      LABEL "Database Up Time"       COLON 23
            _Record-RecRead     LABEL "Read record"            COLON 23
            _Record-RecRead / perMinute      NO-LABEL          AT 37
            _Record-RecRead / _Record-UpTime NO-LABEL          AT 47
            _Record-RecRead / _Record-Trans  NO-LABEL          AT 57
            _Record-RecUpd      LABEL "Update record"          COLON 23
            _Record-RecUpd / perMinute      NO-LABEL           AT 37
            _Record-RecUpd / _Record-UpTime NO-LABEL           AT 47
            _Record-RecUpd / _Record-Trans  NO-LABEL           AT 57
            _Record-RecCreat    LABEL "Create record"          COLON 23
            _Record-RecCreat / perMinute      NO-LABEL         AT 37
            _Record-RecCreat / _Record-UpTime NO-LABEL         AT 47
            _Record-RecCreat / _Record-Trans  NO-LABEL         AT 57
            _Record-RecDel      LABEL "Delete record"          COLON 23
            _Record-RecDel / perMinute      NO-LABEL           AT 37
            _Record-RecDel / _Record-UpTime NO-LABEL           AT 47
            _Record-RecDel / _Record-Trans  NO-LABEL           AT 57
            _Record-FragRead    LABEL "Fragments read"         COLON 23
            _Record-FragRead / perMinute      NO-LABEL         AT 37
            _Record-FragRead / _Record-UpTime NO-LABEL         AT 47
            _Record-FragRead / _Record-Trans  NO-LABEL         AT 57
            _Record-FragCreat   LABEL "Fragments created"      COLON 23
            _Record-FragCreat / perMinute      NO-LABEL        AT 37
            _Record-FragCreat / _Record-UpTime NO-LABEL        AT 47
            _Record-FragCreat / _Record-Trans  NO-LABEL        AT 57
            _Record-FragDel     LABEL "Fragments deleted"      COLON 23
            _Record-FragDel / perMinute      NO-LABEL          AT 37
            _Record-FragDel / _Record-UpTime NO-LABEL          AT 47
            _Record-FragDel / _Record-Trans  NO-LABEL          AT 57
            _Record-FragUpd     LABEL "Fragments updated"      COLON 23
            _Record-FragUpd / perMinute      NO-LABEL          AT 37
            _Record-FragUpd / _Record-UpTime NO-LABEL          AT 47
            _Record-FragUpd / _Record-Trans  NO-LABEL          AT 57
            _Record-BytesRead   LABEL "Bytes read"             COLON 23
            _Record-BytesRead / perMinute      NO-LABEL        AT 37
            _Record-BytesRead / _Record-UpTime NO-LABEL        AT 47
            _Record-BytesRead / _Record-Trans  NO-LABEL        AT 57
            _Record-BytesCreat  LABEL "Bytes created"          COLON 23
            _Record-BytesCreat / perMinute      NO-LABEL       AT 37
            _Record-BytesCreat / _Record-UpTime NO-LABEL       AT 47
            _Record-BytesCreat / _Record-Trans  NO-LABEL       AT 57
            _Record-BytesDel    LABEL "Bytes deleted"          COLON 23
            _Record-BytesDel / perMinute      NO-LABEL         AT 37
            _Record-BytesDel / _Record-UpTime NO-LABEL         AT 47
            _Record-BytesDel / _Record-Trans  NO-LABEL         AT 57
            _Record-BytesUpd    LABEL "Bytes updated"          COLON 23
            _Record-BytesUpd / perMinute      NO-LABEL         AT 37
            _Record-BytesUpd / _Record-UpTime NO-LABEL         AT 47
            _Record-BytesUpd / _Record-Trans  NO-LABEL         AT 57

            skip(16)
            with frame Act-Record-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM act-Other
do:
  auto-repeat-cnt = 0.
  repeat on end-key undo, leave:
    hide all.
    my-title = string(TODAY) + 
               "             Activity: Other             " +
               string(TIME, "HH:MM:SS").
  for each _ActOther NO-LOCK:
    perMinute = _Other-UpTime / 60.
    display SKIP(1)
                                      "Total"                  AT 25
                                      "Per Min"                AT 37
                                      "Per Sec"                AT 47
                                      "Per Tx"                 AT 57
            SKIP(1)
            _Other-UpTime     LABEL "Database Up Time"       COLON 23
            _Other-Commit     LABEL "Commit"                 COLON 23
            _Other-Commit / perMinute     NO-LABEL           AT 37
            _Other-Commit / _Other-UpTime NO-LABEL           AT 47
            _Other-Commit / _Other-Trans  NO-LABEL           AT 57
            _Other-Undo       LABEL "Undo"                   COLON 23
            _Other-Undo / perMinute     NO-LABEL             AT 37
            _Other-Undo / _Other-UpTime NO-LABEL             AT 47
            _Other-Undo / _Other-Trans  NO-LABEL             AT 57
            _Other-Wait       LABEL "Wait on semaphore"      COLON 23
            _Other-Wait / perMinute     NO-LABEL             AT 37
            _Other-Wait / _Other-UpTime NO-LABEL             AT 47
            _Other-Wait / _Other-Trans  NO-LABEL             AT 57
            _Other-FlushMblk  LABEL "Flush master block"     COLON 23
            _Other-FlushMblk / perMinute     NO-LABEL        AT 37
            _Other-FlushMblk / _Other-UpTime NO-LABEL        AT 47
            _Other-FlushMblk / _Other-Trans  NO-LABEL        AT 57

            skip(16)
            with frame Act-Other-frame
                 side-labels
                 TITLE my-title.
  end.

    if ((sleep-time <= 0) OR (auto-repeat-cnt >= auto-repeats) ) then
        leave.

    pause sleep-time.
    auto-repeat-cnt = auto-repeat-cnt + 1.
  end.
  hide all.
end.

/**** Other Displays ... ****/
ON CHOOSE OF MENU-ITEM ot-Performance
do:
    hide all.
    my-title = string(TODAY) + 
               "          Performance Indicators         " +
               string(TIME, "HH:MM:SS").
    display SKIP(1)
            "Sorry.  This feature is not yet implemented."
            skip(1)
            with frame Performance-frame
                 down
                 TITLE my-title.
        pause screen-pause.
        hide all.
end.
ON CHOOSE OF MENU-ITEM ot-IO
do:
    hide all.
    my-title = string(TODAY) + 
               "        I/O Operations by Process        " +
               string(TIME, "HH:MM:SS").
  for each _UserIO where _UserIO-Name <> ? NO-LOCK: 
    display 
            _UserIO-Usr      COLUMN-LABEL "Usr"             FORMAT ">>>9"
            _UserIO-Name     COLUMN-LABEL "Name"            FORMAT "x(10)"
            _UserIO-DbAccess COLUMN-LABEL "Database!Access" FORMAT ">>>>>>>9"
            _UserIO-DbRead   COLUMN-LABEL "Database!Read"   FORMAT ">>>>>>>9"
            _UserIO-DbWrite  COLUMN-LABEL "Database!Write"  FORMAT ">>>>>>>9"
            _UserIO-BiRead   COLUMN-LABEL "BI Read"         FORMAT ">>>>>>>9"
            _UserIO-BiWrite  COLUMN-LABEL "BI Write"        FORMAT ">>>>>>>9"
            _UserIO-AiRead   COLUMN-LABEL "AI Read"         FORMAT ">>>>>>>9"
            _UserIO-AiWrite  COLUMN-LABEL "AI Write"        FORMAT ">>>>>>>9"
            with frame UserIO-frame
                 down
                 TITLE my-title.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM ot-Lock
do:
    hide all.
    my-title = string(TODAY) + 
               "          Lock Requests By User          " +
               string(TIME, "HH:MM:SS").
  for each _LockReq where _LockReq-name <> ? NO-LOCK:
    display SKIP(1)
            _LockReq-Num      COLUMN-LABEL "User!Number"  FORMAT ">>>>9"
            _LockReq-Name     COLUMN-LABEL "User!Name"    FORMAT "x(8)"
            _LockReq-RecLock  COLUMN-LABEL "Record!Locks" FORMAT ">>>>>>>9"
            _LockReq-RecWait  COLUMN-LABEL "Record!Waits" FORMAT ">>>>>>>9"
            _LockReq-TrnLock  COLUMN-LABEL "Trans!Locks"  FORMAT ">>>>>>>9"
            _LockReq-TrnWait  COLUMN-LABEL "Trans!Waits"  FORMAT ">>>>>>>9"
            _LockReq-SchLock  COLUMN-LABEL "Schema!Locks" FORMAT ">>>>>>>9"
            _LockReq-SchWait  COLUMN-LABEL "Schema!Waits" FORMAT ">>>>>>>9"

            skip(1)
            with frame LockReq-frame
                 down
                 TITLE my-title.
  end.
  hide all.
end.

ON CHOOSE OF MENU-ITEM ot-Checkpoints
do:
    hide all.
    my-title = string(TODAY) + 
               "              Checkpoints                " +
               string(TIME, "HH:MM:SS").
  for each _Checkpoint NO-LOCK:
    display SKIP(1)
            _Checkpoint-Id     LABEL "Id"            FORMAT ">>>9"
            _Checkpoint-Time   LABEL "Time"
            _Checkpoint-Len    LABEL "End Time"
            _Checkpoint-Dirty  LABEL "Dirty"         FORMAT ">>>9"
            _Checkpoint-CptQ   LABEL "CPT Q"         FORMAT ">>>9"
            _Checkpoint-Scan   LABEL "Scan"          FORMAT ">>>9"
            _Checkpoint-ApwQ   LABEL "APW Q"         FORMAT ">>>9"
            _Checkpoint-Flush  LABEL "Flushes"       FORMAT ">>>9"
            skip(1)
            with frame Checkpoint-frame
                 down
                 TITLE my-title.
  end.
  hide all.
end.


ON CHOOSE OF MENU-ITEM ot-Blocks
do:
    hide all.
    my-title = string(TODAY) + 
               "                Blocks                   " +
               string(TIME, "HH:MM:SS").
Block-block:
  for each _Block NO-LOCK ON ENDKEY UNDO, LEAVE Block-block:
    display 

            _Block-Id          LABEL "Id"         FORMAT ">>>>>9"
            _Block-Dbkey       LABEL "DBKEY"      FORMAT ">>>>>9"
            _Block-Type        LABEL "Type"       FORMAT "x(10)"
            _Block-ChainType   LABEL "Chain Type" FORMAT "x(10)"
            _Block-BkupCtr     LABEL "Bkup Ctr"   FORMAT ">>>>>>>9"
            _Block-NextDbkey   LABEL "Next Dbkey" FORMAT ">>>>>>>9"
            _Block-Update      LABEL "Update Ctr" FORMAT ">>>>>>>9"
            _Block-Area        LABEL "Area"       FORMAT ">>>>>>>9"
            with frame Block-frame1 
                 1 down
                 TITLE my-title.
     x = 0.
     repeat i = 1 to 32 ON ENDKEY UNDO, LEAVE Block-block:
         display
                  x NO-LABEL FORMAT "9999"
                  substr(_Block-Block, (x +  1), 8) FORMAT "x(8)"
                  substr(_Block-Block, (x +  9), 8) FORMAT "x(8)"
                  substr(_Block-Block, (x + 17), 8) FORMAT "x(8)"
                  substr(_Block-Block, (x + 25), 8) FORMAT "x(8)"
                  substr(_Block-Block, (x + 33), 8) FORMAT "x(8)"
                  substr(_Block-Block, (x + 41), 8) FORMAT "x(8)"
                  substr(_Block-Block, (x + 49), 8) FORMAT "x(8)"
                  substr(_Block-Block, (x + 57), 8) FORMAT "x(8)"
                  with frame Block-frame
                       down.

         down with frame Block-frame.

         x = (x + 64).
    end.

  end. /* Block-block */
  hide all.
end.


ON CHOOSE OF MENU-ITEM mi-Active-Trans
do:
    hide all.
    my-title = string(TODAY) + 
               "          Active Transaction Status          " +
               string(TIME, "HH:MM:SS").
  for each _Trans where _Trans-Usrnum <> ? NO-LOCK:
    display SKIP(1)
            _Trans-Usrnum    COLUMN-LABEL "Usr"          FORMAT ">>>>9"    
            _Trans-Num       COLUMN-LABEL "Tx id"        FORMAT ">>>>9"
            _Trans-State     COLUMN-LABEL "State"        FORMAT "x(6)"
            _Trans-Flags     COLUMN-LABEL "Flags"        FORMAT "x(4)"
            _Trans-counter   COLUMN-LABEL "Counter"      FORMAT ">>>>9"
            _Trans-Coord     COLUMN-LABEL "Coord Name"   FORMAT "x(4)"
            _Trans-CoordTx   COLUMN-LABEL "Coord Tx"     FORMAT ">>>>9"
            _Trans-txtime    COLUMN-LABEL "Start Time" 
            skip(1)
            with frame Transaction-frame
                 down
                 TITLE my-title.
  end.
  hide all.
end.

ON CHOOSE OF    MENU-ITEM mi-2PC-TRans
do:
    hide all.
    my-title = string(TODAY) + 
               "            Admin: 2PC-TRans            " +
               string(TIME, "HH:MM:SS").
    display SKIP(1)
            "Sorry.  This feature is not yet implemented."
            with frame 2PC-TRans-frame
                 down
                 TITLE my-title.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF MENU-ITEM mi-Resolve-Limbo
do:
    hide all.
    my-title = string(TODAY) + 
               "           Admin: Resolve-Limbo          " +
               string(TIME, "HH:MM:SS").
    display SKIP(1)
            "Sorry.  This feature is not yet implemented."
            with frame Resolve-Limbo-frame
                 down
                 TITLE my-title.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF    MENU-ITEM mi-Adj-Latch
do:
    hide all.
    my-title = string(TODAY) + 
               "           Admin: Adjust Latches         " +
               string(TIME, "HH:MM:SS").
    display SKIP(1)
            "Sorry.  This feature is not yet implemented."
            with frame Adj-Latch-frame
                 down
                 TITLE my-title.
        pause screen-pause.
        hide all.
end.

ON CHOOSE OF    MENU-ITEM mi-Adj-PWs
do:
    hide all.
    my-title = string(TODAY) + 
               "             Admin: Adjust PWs           " +
               string(TIME, "HH:MM:SS").
    display SKIP(1)
            "Sorry.  This feature is not yet implemented."
            with frame Adj-PWs-frame
                 down
                 TITLE my-title.
end.


ON CHOOSE OF MENU-ITEM sm-Adjust
do:
    hide all.
    my-title = string(TODAY) + 
               "            Adjust Monitor Options           " +
               string(TIME, "HH:MM:SS").
    update skip(1)
           page-length     LABEL "Display page length (lines)"      COLON 33
/* Is there a Monitor sampling interval??? don't we always sample 
 * everything and let the application determine the difference???
 */
           sample-interval LABEL "Monitor sampling interval (secs)" COLON 33
           sleep-time      LABEL "Pause between displays (secs)"    COLON 33
           screen-pause    LABEL "Pause between screens (secs)"     COLON 33
           auto-repeats    LABEL "Number of auto repeats"           COLON 33
           skip(1)
           with frame Adjust-frame
                side-labels
                TITLE my-title.
end.

/****  MAIN LOGIC  ****/

ENABLE ALL.

WAIT-FOR CHOOSE OF MENU-ITEM mi-Exit.


/**** INTERNAL PROCEDURES  ****/

