unit TrTexts;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils;

RESOURCESTRING

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
  rsCannotFindNote = 'ERROR, cannot find ';                    // is followed by a filename
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
  rsSyncNotConfig = 'not configured';               // means that the file of net sync is not configured yet.
  rsSetUp = 'Setup';                               // means configure something, eg, one of the Sync modules.
  rsDoubleclickNote = 'double click a note ...';

  rsSelectLibrary = 'Select your hunspell library';
  rsSelectDictionary = 'Select the dictionary you want to use';
  rsDictionaryLoaded = 'Dictionary Loaded OK';
  rsDictionaryFailed = 'Library Not Loaded';
  rsDictionaryNotFound = 'No Dictionary Found';

implementation

end.

