unit TrTexts;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

RESOURCESTRING

// main
  rsAnotherInstanceRunning = 'Another instance of tomboy-reborn appears to be running. Will exit.';
  rsFailedToIndex = 'Failed to index one or more notes.';
  rsCannotDismiss1 = 'Sadly, on this OS or because of a Bad Note,';
  rsCannotDismiss2 = 'I cannot let you dismiss this window';
  rsCannotDismiss3 = 'Are you trying to shut me down ? Dave ?';
  rsAbout = 'This is Tomboy Reborn, a rewrite of Tomboy Notes using Lazarus and FPC.';
  rsAboutVer = 'Version ';
  rsAboutBDate = 'Build date';
  rsAboutCPU = 'TargetCPU';
  rsAboutOperatingSystem = 'OS';

// notebook.pas

  rsMultipleNoteBooks = 'Settings allow multiple Notebooks';
  rsOneNoteBook = 'Settings allow only one Notebook';
  rsSetTheNotebooks = 'Set the notebooks this note is a member of';
  rsChangeNameofNotebook = 'Change the name of this Notebook';
  rsNumbNotesAffected = 'This will affect %d notes';                 // %d replaced by integer, 0 to big number
  rsEnterNewNotebook = 'Enter a new notebook name please';


// SearchForm
  // these are main menu items and string grid headings -
  rsMenuNewNote = 'New Note';
  rsMenuSearch = 'Search';
  rsMenuAbout = 'About';
  rsMenuSync = 'Synchronise';
  rsMenuSettings = 'Settings';
  rsMenuHelp = 'Help';
  rsMenuQuit = 'Quit';
  rsNotebooks = 'Notebooks';
  rsName = 'Name';
  rsLastChange = 'Last Change';

  rsSetupNotesDirFirst = 'Please setup a notes directory first';
  rsSetupSyncFirst = 'Please config sync system first';
  rsCannotFindNote = 'Cannot find ';                    // is followed by a filename
  rsSearchHint = 'Exact matches for terms between " "';


// Sync
  rsTestingSync = 'Testing Sync';
  rsUnableToSync = 'Unable to sync because ';
  rsRunningSync = 'Running Sync';
  rsAllDone = 'All Done';
  rsPressClose = 'Press Close';
  rsTestingRepo = 'Testing Repo ....';
  rsCreateNewRepo = 'Create a new Repo ?';
  rsUnableToProceed = 'Unable to proceed because';
  rsLookingatNotes = 'Looking at notes ....';
  rsSaveAndSync = 'Press Save and Sync if this looks OK';
  rsSyncError = 'A Sync Error occured';
  rsLastSync = 'Last Sync';     // Followed by a date and simplified sync report
  rsSyncNCDefault = 'https://YOURSERVER/index.php/apps/grauphel';

  rsNewUploads      = 'New Uploads    ';
  rsEditUploads     = 'Edit Uploads   ';
  rsDownloads       = 'Downloads      ';
  rsSynCopies       = 'Copied notes   ';
  rsLocalDeletes    = 'Local Deletes  ';
  rsRemoteDeletes   = 'Remote Deletes ';
  rsDoNothing       = 'Do Nothing     ';
  rsUndecided       = 'Unresolved     ';

  rsNoNotesNeededSync = 'No notes needed syncing. You need to write more.';
  rsNotesWereDealt = ' notes were dealt with.';
  rsChangeExistingSync = 'Change existing sync connection ?';
  rsNotRecommend = 'Generally not recommended.';
  rsNextBitSlow = 'Next bit can be a bit slow, please wait';


// Settings
  rsErrorCreateDir = 'Unable to Create Directory';
  rsErrorCannotWrite = 'Cannot write into';
  rsSyncNotConfig = 'Not configured';               // means that the file of net sync is not configured yet.
  rsSetUp = 'Setup';                               // means configure something, eg, one of the Sync modules.
  rsDoubleclickNote = 'double click a note ...';

  rsSelectLibrary = 'Select your hunspell library';
  rsSelectDictionary = 'Select the dictionary you want to use';
  rsCanNotHideSearch = 'As there is no Tray, Default window can not be hidden';
  rsDictionaryLoaded = 'Dictionary Loaded OK';
  rsDictionaryFailed = 'Library Not Loaded';
  rsDictionaryNotFound = 'No Dictionary Found';

  // Try Menu
  rsTrayNewNote = 'New note';
  rsTrayNotebooks = 'Notebooks';
  rsTraySearchNote = 'Search all notes';
  rsTraySync = 'Synchronize';
  rsTrayApplication = 'Application';
  rsTraySettings = 'Settings';
  rsTrayAbout = 'About';
  rsTrayQuit = 'Quit';


// Spelling
  rsCheckingFull = 'Checking full document';
  rsCheckingSelection = 'Checking selection';
  rsSpellNotConfig = 'Spelling not configured';
  rsSpellNoLib = 'Spelling library not configured';
  rsSpellNoDic = 'Select dic to use';

  rsHelpDelay = 'Delay startup 2 sec to allow OS to settle';
    rsHelpLang = 'Force Language, supported es, nl';
    rsHelpDebug = 'Direct debug output to SOME.LOG.';
    rsHelpHelp = 'Show this help message and exit.';
    rsHelpVersion = 'Print version and exit';
    rsHelpRedHat = 'Run in RedHatGnome mode, no TrayIcon';
    rsHelpNoSplash = 'Dont show small status/splash window';
    rsHelpDebugSync = 'Show debug messages during Sync';
    rsHelpDebugIndex = 'Show debug msgs while indexing notes';
    rsHelpDebugSpell = 'Show debug messages while spell setup';
    rsHelpConfig = 'Create or use an alternative config';
    rsHelpSingleNote = 'Open indicated note, switch is optional';
    rsHelpSaveExit = 'After import single note, save & exit';

implementation

end.

