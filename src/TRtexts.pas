unit TrTexts;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

RESOURCESTRING

// main
  rsAnotherInstanceRunning = 'Another instance of tomboy-reborn appears to be running. Will exit.';
  rsAbout = 'This is Tomboy Reborn, a rewrite of Tomboy Notes using Lazarus and FPC.';
  rsAboutVer = 'Version ';
  rsAboutBDate = 'Build date';
  rsAboutCPU = 'TargetCPU';
  rsAboutOperatingSystem = 'OS';
  rsCannotFind = 'Can not find ';
  rsAreYouSure = 'Are you sure ?';

// notebook.pas

  rsMultipleNoteBooks = 'Settings allow multiple Notebooks';
  rsOneNoteBook = 'Settings allow only one Notebook';
  rsSetTheNotebooks = 'Set the notebooks this note is a member of';
  rsChangeNameofNotebook = 'Change the name of this Notebook';
  rsNumbNotesAffected = 'This will affect %d notes';                 // %d replaced by integer, 0 to big number
  rsEnterNewNotebook = 'Enter a new notebook name please';

  rsConfirmDeleteNote = 'Do you really want'+sLineBreak+'to detele Note ';
  rsConfirmDeleteNotebook1 = 'Do you really want'+sLineBreak+'to detele Notebook ';
  rsConfirmDeleteNotebook2 = 'This will update all Notes accordingly';

// SearchForm
  // these are main menu items and string grid headings -
  rsMenuNotes = 'File';
  rsMenuEdit = 'Edit';
  rsMenuFormat = 'Format';
  rsMenuTools = 'Tools';
  rsMenuHelp = 'Help';
  rsMenuDeleteNote = 'Delete Note';
  rsMenuDeleteNotebook = 'Delete Notebook';

  rsMenuNewNote = 'New Note';
  rsMenuFind = 'Find';
  rsMenuSave = 'Save';
  rsMenuDelete = 'Delete note';
  rsMenuNewNotebook = 'New notebook';
  rsMenuSearch = 'Search';
  rsMenuAbout = 'About';
  rsMenuDuplicate = 'Duplicate';
  rsMenuSync = 'Synchronise';
  rsMenuExport = 'Export';
  rsMenuSettings = 'Settings';
  rsMenuQuit = 'Quit';
  rsMenuUndo = 'Undo';
  rsMenuRedo = 'Redo';
  rsMenuSelectAll = 'Select all';
  rsMenuCut = 'Cut';
  rsMenuCopy = 'Copy';
  rsMenuPaste = 'Paste';
  rsMenuBold = 'Bold';
  rsMenuItalic = 'Italic';
  rsMenuStrikeout = 'Strikethrough';
  rsMenuUnderlined = 'Underlined';
  rsMenuHighlight = 'Highlight';
  rsMenuFixed = 'FixedFont';
  rsAddNoteLink = 'Insert Link to Note';
  rsAddURLLink = 'Insert Link to URL';
  rsRemoveLink = 'Remove link';
  rsLinks = 'Link';
  rsNotebooks = 'Notebooks';
  rsMenuFontPlus = 'Increase font size';
  rsMenuFontMinus = 'Decrease font size';
  rsMenuBullet = 'Bullet';
  rsMenuBulletInc = 'Increase indent';
  rsMenuBulletDec = 'Decrease indent';
  rsAnyNotebook = 'Any notebook';
  rsNoNotebook = 'Without notebook';
  rsName = 'Name';
  rsLastChange = 'Last Change';

  rsMenuExportRTF = 'as RTF';
  rsMenuExportMarkdown = 'Markdown format';
  rsMenuExportPlain = 'Plain text';

  rsMenuPrint = 'Print';

  // Errors
  rsInvalidCreateDate = 'Invalid create date';
  rsInvalidLastChangeDate = 'Invalid last change date';
  rsInvalidLastMetaChangeDate = 'Invalid last meta change date';
  rsInvalidTitle = 'Invalid Note title';
  rsInvalidCursorPosition = 'Invalid cusor position';
  rsInvalidBoundPosition = 'Invalid bound position';
  rsInvalidWidth = 'Invalid width';
  rsInvalidHeight = 'Invalid height';
  rsInvalidPosX = 'Invalid horizontal position';
  rsInvalidPosY = 'Invalid vertical position';
  rsInvalidOpenOnStartup = 'Invalid open-on-startup flag';
  rsInvalidPinned = 'Invalid pinned flag';
  rsInvalidNoteContent = 'Invalid note-content';
  rsInvalidTags = 'Invalid tags';

  // Tray Menu
  rsTrayNewNote = 'New note';
  rsTrayNotebooks = 'Notebooks';
  rsTraySearchNote = 'Search all notes';
  rsTraySync = 'Synchronize';
  rsTraySettings = 'Settings';
  rsTrayAbout = 'About';
  rsTrayQuit = 'Quit';

  rsInsertDate = 'Insert current date';


  rsSetupNotesDirFirst = 'Please setup a notes directory first';
  rsSetupSyncFirst = 'Please config sync system first';
  rsSearchHint = 'Search all terms (space delimited)';
  rsNotebookHint = 'Select a notebook to see the Notes attached to it';

// Sync
  rsOtherSyncProcess = 'Sync not possible for now (other process running)';
  rsNoSync = 'You shall setup sync first in "settings"';
  rsScaleIncorrect = 'Incorrect scale';
  rsTestingSync = 'Testing Sync';
  rsUnableToSync = 'Unable to sync because ';
  rsRunningSync = 'Running Sync';
  rsAllDone = 'All Done';
  rsPressClose = 'Press Close';
  rsTestingRepo = 'Testing Repo ....';
  rsCreateNewRepo = 'Create a new Repo ?';
  rsUnableToProceed = 'Unable to proceed because';
  rsLookingatLocalNotes = 'Looking at local notes ....';
  rsLookingatRemoteNotes = 'Looking at remote notes ....';
  rsSaveAndSync = 'Press Save and Sync if this looks OK';
  rsSyncError = 'A Sync Error occured';
  rsErrorLoadingRemote = 'Loading remote notes has raised an error';
  rsErrorLoadingLocal = 'Loading local notes has raised an error';
  rsFindDeletedServerNotes = 'Finding deleted notes on remote';
  rsFindDeletedLocalNotes = 'Finding deleted notes locally';
  rsFindNewLocalNotes = 'Find new notes locally';
  rsFindNewRemoteNotes = 'Find new notes remotelly';
  rsSetSystemicActions = 'Applying systemic rules';
  rsProcessClashes = 'Processing clashes';
  rsStatistics = 'Doing statistics';
  rsSyncReport = 'Synchronisation summary';
  rsSyncDoDownloads = 'Downloading from remote';
  rsDoDeleteLocal = 'Deleting expired local notes';
  rsDoManifest = 'Writing manifest';
  rsSyncCanceledByUser = 'Synchronisation canceled by user';
  rsLastSync = 'Last Sync';     // Followed by a date and simplified sync report
  rsSyncNCDefault = 'https://YOURSERVER/index.php/apps/grauphel';

  rsSyncClashCanceled = 'User canceled clash resolution';
  rsSyncCoreError = 'Synchonization process core error';

  rsSyncRevisionError = 'Revision error : Old sync ?';
  rsSyncNeverSynced = 'Synced data error';
  rsSyncChangeError = 'Changes on both sides';

  rsNewUploads      = 'New upload       ';
  rsEditUploads     = 'Edited upload    ';
  rsNewDownloads    = 'New download     ';
  rseditDownloads   = 'Edited download  ';
  rsSynCopies       = 'To be Duplicated ';
  rsLocalDeletes    = 'Local obsolete   ';
  rsRemoteDeletes   = 'Remote obosolete ';
  rsDoNothing       = 'Unchanged        ';
  rsUndecided       = 'Unresolved       ';

  rsNoNotesNeededSync = 'No notes needed syncing. You need to write more.';
  rsNotesWereDealt = ' notes were dealt with.';
  rsChangeExistingSync = 'Change existing sync connection ?';
  rsNotRecommend = 'Generally not recommended.';
  //rsNextBitSlow = 'Next bit can be a bit slow, please wait';

  // Find
  rsSearchNotFound = 'Text not found';


// Settings
  rsErrorCreateDir = 'Unable to Create Directory';
  rsErrorCannotWrite = 'Cannot write file';
  rsSyncNotConfig = 'Not configured';               // means that the file of net sync is not configured yet.
  rsSetUp = 'Setup';                               // means configure something, eg, one of the Sync modules.
  rsDoubleclickNote = 'double click a note ...';

  rsDictNotSetup = 'Spelling libray not setup';
  rsDictNotFound = 'Spelling dictionnary file incorrect';
  rsWordInvalid = 'can not be processed';
  rsSelectLibrary = 'Select your hunspell library';
  rsSelectDictionary = 'Select the dictionary you want to use';
  rsCanNotHideSearch = 'As there is no Tray, Default window can not be hidden';
  rsDictionaryLoaded = 'Dictionary Loaded OK';
  rsDictionaryFailed = 'Library Not Loaded';
  rsDictionaryNotFound = 'No Dictionary Found';


// Spelling
  rsCheckingFull = 'Checking full document';
  rsCheckingSelection = 'Checking selection';
  rsSpellNotConfig = 'Spelling not configured';
  rsSpellNoLib = 'Spelling library not configured';
  rsSpellNoDic = 'Select dic to use';

  rsCheckSel = 'Spelling';
  rsCheckSelNop = 'Select text to check spelling';

  rsHelpDelay = 'Delay startup 2 sec to allow OS to settle';
  rsHelpLang = 'Force Language, supported es, nl';
  rsHelpDebug = 'Show debug on stdout';
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

  rsVersion = '1.0.0';

  rsLineBreak = {$IFDEF LINUX} AnsiChar(#10) {$ENDIF}
               {$IFDEF MSWINDOWS} AnsiUTF8String(#13#10) {$ENDIF};
implementation

end.

