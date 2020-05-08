unit TRsearchUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ActnList,
    Grids, ComCtrls, StdCtrls, ExtCtrls, Menus, Buttons, Note_Lister, lazLogger,
    TRcommon, TRtexts;

// These are choices for main popup menus.
type TMenuTarget = (mtSep=1, mtNewNote, mtSearch, mtAbout=10, mtSync, mtTomdroid, mtSettings, mtMainHelp, mtHelp, mtQuit, mtRecent);

// These are the possible kinds of main menu items
type TMenuKind = (mkFileMenu, mkRecentMenu, mkHelpMenu, mkAllMenu);


type        { TSearchForm }
    TSearchForm = class(TForm)
        ButtonNotebookOptions: TButton;
	ButtonClearFilters: TButton;
	ButtonRefresh: TButton;
        CheckCaseSensitive: TCheckBox;
        Edit1: TEdit;
	MenuEditNotebookTemplate: TMenuItem;
	MenuDeleteNotebook: TMenuItem;
        MenuRenameNoteBook: TMenuItem;
	MenuNewNoteFromTemplate: TMenuItem;
	Panel1: TPanel;
	PopupMenuNotebook: TPopupMenu;
        ButtonMenu: TSpeedButton;
	Splitter1: TSplitter;
        StatusBar1: TStatusBar;
        StringGrid1: TStringGrid;
	StringGridNotebooks: TStringGrid;
        SelectDirectoryDialog1: TSelectDirectoryDialog;
        procedure ButtonMenuClick(Sender: TObject);
	procedure ButtonNotebookOptionsClick(Sender: TObject);
  	procedure ButtonRefreshClick(Sender: TObject);
	procedure ButtonClearFiltersClick(Sender: TObject);
        procedure CheckCaseSensitiveChange(Sender: TObject);
        procedure Edit1Enter(Sender: TObject);
	procedure Edit1Exit(Sender: TObject);
        procedure Edit1KeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
	procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
        procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	procedure FormShow(Sender: TObject);
	procedure MenuDeleteNotebookClick(Sender: TObject);
	procedure MenuEditNotebookTemplateClick(Sender: TObject);
        procedure MenuRenameNoteBookClick(Sender: TObject);
	procedure MenuNewNoteFromTemplateClick(Sender: TObject);
        procedure SpeedButton1Click(Sender: TObject);
        procedure StringGrid1Resize(Sender: TObject);
	procedure StringGridNotebooksClick(Sender: TObject);
        procedure StringGrid1DblClick(Sender: TObject);
        // Recieves 2 lists from Sync subsystem, one listing deleted notes ID, the
        // other downloded note ID. Adjusts Note_Lister according and marks any
        // note that is currently open as read only.
        procedure ProcessSyncUpdates(const DeletedList, DownList: TStringList);
        procedure StringGridNotebooksPrepareCanvas(sender: TObject; aCol,
            aRow: Integer; aState: TGridDrawState);
        procedure StringGridNotebooksResize(Sender: TObject);
    private
        HelpNotes : TNoteLister;
        procedure AddItemMenu(TheMenu: TPopupMenu; Item: string;
            mtTag: TMenuTarget; OC: TNotifyEvent; MenuKind: TMenuKind);
        procedure CreateMenus();
        procedure DoSearch();
        procedure FileMenuClicked(Sender: TObject);
        procedure InitialiseHelpFiles();
                                // clears then Inserts file items in all main menus, note also removes help items ....
        procedure MenuFileItems(AMenu: TPopupMenu);
        procedure MenuHelpItems(AMenu: TPopupMenu);
        procedure MenuListBuilder(MList: TList);
        procedure RecentMenuClicked(Sender: TObject);
        procedure RefreshStrGrids();
        { Copies note data from internal list to StringGrid, sorts it and updates the
          TrayIconMenu recently used list.  Does not 'refresh list from disk'.  }
		procedure UseList();
    public

        PopupTBMainMenu : TPopupMenu;
        SelectedNotebook : integer;         // Position in Notebook grid use has clicked, 0 means none.
        //AllowClose : boolean;
        NoteLister : TNoteLister;
        NoteDirectory : string;
        procedure UpdateSyncStatus(SyncSt : string);
        {Just a service provided to NoteBook.pas, refresh the list of notebooks after adding or removing one}
        procedure RefreshNotebooks();
        // Fills in the Main TB popup menus. If AMenu is provided does an mkAllMenu on
        // that Menu, else applies WhichSection to all know Main TB Menus.
        procedure RefreshMenus(WhichSection: TMenuKind; AMenu: TPopupMenu=nil);
        function MoveWindowHere(WTitle: string): boolean;
        { If there is an open note from the passed filename, it will be marked read Only,
          If deleted, remove entry from NoteLister, will accept a GUID, Filename or FullFileName inc path }
        procedure MarkNoteReadOnly(const FullFileName: string; const WasDeleted : boolean);
         	{ Puts the names of recently used notes in the indicated menu, removes esisting ones first. }
        procedure MenuRecentItems(AMenu : TPopupMenu);
       	{ Call this NoteLister no longer thinks of this as a Open note }
        procedure NoteClosing(const ID: AnsiString);
        { Updates the List with passed data. Either updates existing data or inserts new }
        procedure UpdateList(const Title, LastChange, FullFileName: ANSIString; TheForm : TForm);
        { Reads header in each note in notes directory, updating Search List and
          the recently used list under the TrayIcon. Downside is time it takes
          to index. use UpdateList() if you just have updates. }
        function IndexNotes() : integer;
        { Returns true when passed string is the title of an existing note }
        function IsThisaTitle(const Term: ANSIString): boolean;
        { Gets called with a title and filename (clicking grid), with just a title
          (clicked a note link or recent menu item or Link Button) or nothing
          (new note). If its just Title but Title does not exist, its Link
          Button. }
        procedure OpenNote(NoteTitle : String = ''; FullFileName : string = ''; TemplateIs : AnsiString = '');
        { Returns True if it put next Note Title into SearchTerm }
        function NextNoteTitle(out SearchTerm : string) : boolean;
        { Initialises search of note titles, prior to calling NextNoteTitle() }
        procedure StartSearch();
        { Deletes the actual file then removes the indicated note from the internal
        data about notes, refreshes Grid }
        procedure DeleteNote(const FullFileName : ANSIString);


const
	MenuEmpty = '(empty)';

    end;

var
    SearchForm: TSearchForm;

implementation

{$R *.lfm}



uses MainUnit,      // Opening form, manages startup and Menus
    EditBox,
    TRsettings,		// Manages settings.
    LCLType,		// For the MessageBox
    LazFileUtils,   // LazFileUtils needed for TrimFileName(), cross platform stuff
    TRsync,           // because we need it to manhandle local manifest when a file is deleted
    process,        // Linux, we call wmctrl to move note to current workspace
    NoteBook;



{ TSearchForm }



{ -------------   FUNCTIONS  THAT  PROVIDE  SERVICES  TO  OTHER   UNITS  ------------ }


procedure TSearchForm.ProcessSyncUpdates(const DeletedList, DownList : TStringList);
// The lists arrive here with just the 36 char ID, some following functions are OK with that ????
var
    Index : integer;
begin
    if NoteLister <> nil then begin
        for Index := 0 to DeletedList.Count -1 do begin
            MarkNoteReadOnly(DeletedList.Strings[Index], True);
            //debugln('We have tried to mark read only on ' + DeletedList.Strings[Index]);
        end;
        for Index := 0 to DownList.Count -1 do begin
            MarkNoteReadOnly(DownList.Strings[Index], False);
            if NoteLister.IsIDPresent(DownList.Strings[Index]) then begin
                NoteLister.DeleteNote(DownList.Strings[Index]);
                //debugln('We have tried to delete ' + DownList.Strings[Index]);
            end;
            NoteLister.IndexThisNote(DownList.Strings[Index]);
            //debugln('We have tried to reindex ' + DownList.Strings[Index]);
        end;
        UseList();
    end;
end;

procedure TSearchForm.StringGridNotebooksPrepareCanvas(sender: TObject; aCol,
    aRow: Integer; aState: TGridDrawState);
begin
    if (SelectedNoteBook > 0) and (aRow = SelectedNotebook) then
        stringgridnotebooks.canvas.brush.color := clLtGray; {StringGrid1.FixedColor; }
end;

procedure TSearchForm.StringGridNotebooksResize(Sender: TObject);
begin
    StringGridNotebooks.Columns[0].Width := StringGridNotebooks.width;
end;

procedure TSearchForm.NoteClosing(const ID : AnsiString);
begin
    if NoteLister <> nil then         // else we are quitting the app !
    	NoteLister.ThisNoteIsOpen(ID, nil);
end;

procedure TSearchForm.StartSearch(); // Call before using NextNoteTitle() to list Titles.
begin
	NoteLister.StartSearch();
  // TitleIndex := 1;
end;


procedure TSearchForm.DeleteNote(const FullFileName: ANSIString);
var
    NewName, ShortFileName : ANSIString;
    // LocalMan : TTomboyLocalManifest;
    LocalMan : TSync;
begin
    ShortFileName := ExtractFileNameOnly(FullFileName);
    if NoteLister.IsATemplate(ShortFileName) then begin
        NoteLister.DeleteNoteBookwithID(ShortFileName);
      	DeleteFileUTF8(FullFileName);
        ButtonClearFiltersClick(self);
    end else begin
        NoteLister.DeleteNote(ShortFileName);
     	NewName := GetLocalNoteFile(ShortFileName,GetLocalBackupPath());

        if not RenameFileUTF8(FullFileName, NewName)
    		then DebugLn('Failed to move ' + FullFileName + ' to ' + NewName);
    end;
    UseList();
end;

function TSearchForm.NextNoteTitle(out SearchTerm: string): boolean;
begin
	Result := NoteLister.NextNoteTitle(SearchTerm);
end;

function TSearchForm.IsThisaTitle(const Term : ANSIString) : boolean;
begin
	Result := NoteLister.IsThisATitle(Term);
end;

procedure TSearchForm.RefreshNotebooks();
begin
    NoteLister.LoadStGridNotebooks(StringGridNotebooks, ButtonClearFilters.Enabled);
end;

    { As we no longer use the String Grid to provide a date sorted list of recent notes,
      it only needs to be refreshed when we are looking at it. I think. }
procedure TSearchForm.RefreshStrGrids();
{var
    T1, T2, T3 : qword;  }
begin
    //T1 := gettickcount64();
    NoteLister.LoadStGrid(StringGrid1, 2);         // 4 to 8mS on Dell
    //T2 := gettickcount64();
    NoteLister.LoadStGridNotebooks(StringGridNotebooks, ButtonClearFilters.Enabled); // 0mS on Dell
    //T3 := gettickcount64();
    //debugln('SearchUnit - UseList Timing ' + inttostr(T2 - T1) + ' ' + inttostr(T3 - T2));
end;

{ Sorts List and updates the recently used list under trayicon }
procedure TSearchForm.UseList();
{var
    NB : string; }
begin
    RefreshMenus(mkRecentMenu);
    if not Visible then exit;
    ButtonRefresh.Enabled := True;
    {
    if ButtonNotebookOptions.Enabled then begin
        NB := StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
        if NB <> '' then
            NoteLister.LoadNotebookGrid(StringGrid1, NB);
    end else RefreshStrGrids();  }
end;

procedure TSearchForm.UpdateSyncStatus(SyncSt: string);
begin
    //StatusBar1.Panels[0].Text:= SyncSt;
    StatusBar1.SimpleText:= SyncSt;
end;

procedure TSearchForm.UpdateList(const Title, LastChange, FullFileName : ANSIString; TheForm : TForm );
{ var
    T1, T2, T3, T4 : dword;    }
begin
    if NoteLister = Nil then exit;				// we are quitting the app !
  	// Can we find line with passed file name ? If so, apply new data.
    // T1 := gettickcount64();
	if not NoteLister.AlterNote(ExtractFileNameOnly(FullFileName), LastChange, Title) then begin
        // DebugLn('Assuming a new note ', FullFileName, ' [', Title, ']');
        NoteLister.AddNote(ExtractFileNameOnly(FullFileName)+'.note', Title, LastChange);
	end;
    // T2 := gettickcount64();
    NoteLister.ThisNoteIsOpen(FullFileName, TheForm);
    // T3 := gettickcount64();
    UseList();          // 13mS ?
    // T4 := gettickcount64();
    // debugln('SearchUnit.UpdateList ' + inttostr(T2 - T1) + ' ' + inttostr(T3 - T2) + ' ' + inttostr(T4 - T3));
end;


// ---------------------------------------------------------------------------
// -------------  M E N U    F U N C T I O N S -------------------------------
// ---------------------------------------------------------------------------

{ Menus are built and populated at end of CreateForm. }

procedure TSearchForm.InitialiseHelpFiles();
    // Todo : this uses about 300K, 3% of extra memory, better to code up a simpler model ?
begin
    if HelpNotes <> nil then
        freeandnil(HelpNotes);
    HelpNotes := TNoteLister.Create;     // freed in OnClose event.
    HelpNotes.GetNotes('', true);
end;

procedure TSearchForm.CreateMenus();
begin
    InitialiseHelpFiles();
    PopupTBMainMenu := TPopupMenu.Create(self);      // LCL will dispose because of 'self'
    ButtonMenu.PopupMenu := PopupTBMainMenu;
    MainForm.MainTBMenu := TPopupMenu.Create(self);
    MainForm.ButtMenu.PopupMenu := MainForm.MainTBMenu;
    // Add any other 'fixed' menu here.
end;

    // Builds a list of all the Menus we have floating around at the moment.
procedure TSearchForm.MenuListBuilder(MList : TList);
var
    AForm : TForm;
begin
    if assigned(NoteLister) then begin
      AForm := NoteLister.FindFirstOpenNote();
      while AForm <> Nil do begin
          MList.Add(TEditBoxForm(AForm).PopupMainTBMenu);
          AForm := SearchForm.NoteLister.FindNextOpenNote();
      end;
    end;
    if assigned(PopupTBMainMenu) then
        MList.Add(PopupTBMainMenu);
    if assigned(MainForm.MainTBMenu) then
        MList.Add(MainForm.MainTBMenu);
    if (MainForm.UseTrayMenu) and assigned(MainForm.PopupMenuTray) then
        MList.Add(MainForm.PopupMenuTray);
    //if assigned(Sett.PMenuMain) then
    //    MList.Add(Sett.PMenuMain);
end;

procedure TSearchForm.RefreshMenus(WhichSection : TMenuKind; AMenu : TPopupMenu = nil);
var
    MList : TList;
    I : integer;
begin
    if (WhichSection = mkRecentMenu) and (PopupTBMainMenu.Items.Count = 0)
        then exit;      // This is a call during startup, File and Help are not there yet, messes with Qt5
    if AMenu <> Nil then begin
          MenuFileItems(AMenu);
          MenuHelpItems(AMenu);
          MenuRecentItems(AMenu);
          exit();
    end;
    MList := TList.Create;
    MenuListBuilder(MList);
    case WhichSection of
        mkAllMenu : for I := 0 to MList.Count - 1 do begin
                            MenuFileItems(TPopupMenu(MList[i]));
                            MenuHelpItems(TPopupMenu(MList[i]));
                            MenuRecentItems(TPopupMenu(MList[i]));
                        end;
        mkFileMenu : for I := 0 to MList.Count - 1 do
                            MenuFileItems(TPopupMenu(MList[i]));
        mkRecentMenu : for I := 0 to MList.Count - 1 do
                            MenuRecentItems(TPopupMenu(MList[i]));
        mkHelpMenu : for I := 0 to MList.Count - 1 do begin
                            InitialiseHelpFiles();
                            MenuHelpItems(TPopupMenu(MList[i]));
                        end;
    end;
    MList.Free;
end;

procedure TSearchForm.AddItemMenu(TheMenu : TPopupMenu; Item : string; mtTag : TMenuTarget; OC : TNotifyEvent; MenuKind : TMenuKind);
var
    MenuItem : TMenuItem;

            procedure AddHelpItem();
            var
                X : Integer = 0;
            begin
                while X < TheMenu.Items.Count do begin
                    if TheMenu.Items[X].Tag = ord(mtMainHelp) then begin
                        TheMenu.Items[X].Add(MenuItem);
                        exit;
                    end;
                    inc(X);
                end;
            end;
begin
    if Item = '-' then begin
        TheMenu.Items.AddSeparator;
        TheMenu.Items.AddSeparator;
        exit();
    end;
    MenuItem := TMenuItem.Create(Self);
    if mtTag = mtQuit then
        {$ifdef DARWIN}
        MenuItem.ShortCut:= KeyToShortCut(VK_Q, [ssMeta]);
        {$else}
        MenuItem.ShortCut:= KeyToShortCut(VK_Q, [ssCtrl]);
        {$endif}
    MenuItem.Tag := ord(mtTag);             // for 'File' entries, this identifies the function to perform.
    MenuItem.Caption := Item;
    MenuItem.OnClick := OC;
    case MenuKind of
        mkFileMenu   : TheMenu.Items.Insert(0, MenuItem);
        mkRecentMenu : TheMenu.Items.Add(MenuItem);
        mkHelpMenu   : AddHelpItem();
    end;
end;

procedure TSearchForm.MenuFileItems(AMenu : TPopupMenu);
var
    i : integer = 0;
begin
    while i < AMenu.Items.Count do begin              // Find the seperator
        if (AMenu.Items[i]).Caption = '-' then break;
        inc(i);
    end;
    dec(i);                                         // cos we want to leave the '-'
    while (i >= 0) do begin                         // Remove File Type entries
        AMenu.Items.Delete(i);                      // Because it removes Help, removes all the individual help items too.
        dec(i);
    end;
    if AMenu.Items.Count = 0 then                   // If menu empty, put in seperator
        AddItemMenu(AMenu, '-', mtSep, nil, mkFileMenu);
    AddItemMenu(AMenu, rsMenuQuit, mtQuit,  @FileMenuClicked, mkFileMenu);

    AddItemMenu(AMenu, rsMenuHelp, mtMainHelp,  nil, mkFileMenu);
    AddItemMenu(AMenu, rsMenuSettings, mtSettings, @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuSync, mtSync,  @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuAbout, mtAbout, @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuSearch, mtSearch,  @FileMenuClicked, mkFileMenu);
    AddItemMenu(AMenu, rsMenuNewNote, mtNewNote, @FileMenuClicked, mkFileMenu);
    // Note items are in reverse order because we Insert at the top.
end;

procedure TSearchForm.MenuRecentItems(AMenu : TPopupMenu);
var
    i : integer = 1;
    j : integer;
begin
    i := AMenu.Items.Count;
    while i > 0 do begin            // Remove any existing entries first
        dec(i);
        if TMenuItem(AMenu.Items[i]).Tag = ord(mtRecent) then
            AMenu.Items.Delete(i);
    end;

    i := NoteLister.Count;
    j := i -10;
    if j < 0 then j := 0;
    while i > j do begin
        dec(i);
        AddItemMenu(AMenu, NoteLister.GetTitle(i), mtRecent,  @RecentMenuClicked, mkRecentMenu)
    end;

{   This model gets its sorted recent list from string grid, delete it at some stage.
    i := 1;
    while (i <= 10) do begin
       if i < StringGrid1.RowCount then
           AddItemMenu(AMenu, StringGrid1.Cells[0, i], mtRecent,  @RecentMenuClicked, mkRecentMenu)
       else break;
       inc(i);
    end;               }
end;

procedure TSearchForm.MenuHelpItems(AMenu : TPopupMenu);
var
  NoteTitle : string;
  Count : integer;

begin
    Count := AMenu.Items.Count;
    while Count > 0 do begin            // Remove any existing entries first
        dec(Count);
        if TMenuItem(AMenu.Items[Count]).Tag = ord(mtMainHelp) then begin
            AMenu.Items[Count].Clear;
            break;
        end;
    end;
    HelpNotes.StartSearch();
    while HelpNotes.NextNoteTitle(NoteTitle) do
        AddItemMenu(AMenu, NoteTitle, mtHelp,  @FileMenuClicked, mkHelpMenu);
end;

procedure TSearchForm.FileMenuClicked(Sender : TObject);
var
    FileName : string;
    syncok : boolean;
begin

    case TMenuTarget(TMenuItem(Sender).Tag) of
        mtSep, mtRecent : showmessage('Oh, thats bad, should not happen');
        mtNewNote : if (NotesDir = '') then ShowMessage(rsSetupNotesDirFirst)
                    else OpenNote();
        mtSearch :  if NotesDir = '' then showmessage(rsSetupNotesDirFirst)
                    else begin
                         MoveWindowHere(Caption);
                         EnsureVisible(true);
                         Show;
                    end;
        mtAbout :   MainForm.ShowAbout();
        //mtSync :    if(Sett.getSyncConfigured()) then Sett.Synchronise()
        //            else showmessage(rsSetupSyncFirst);
        mtSettings : begin
                     MoveWindowHere(FormSettings.Caption);
                     FormSettings.EnsureVisible(true);
                     FormSettings.Show;
                    end;
        mtQuit :      MainForm.close;
    end;
end;

procedure TSearchForm.RecentMenuClicked(Sender: TObject);
begin
 	if TMenuItem(Sender).Caption <> SearchForm.MenuEmpty then
 		SearchForm.OpenNote(TMenuItem(Sender).Caption);
end;

procedure TSearchForm.ButtonRefreshClick(Sender: TObject);
var
    NB : string;
begin
    // see https://forum.lazarus.freepascal.org/index.php/topic,48568.msg350984/topicseen.html
    // for info about hiding the sort indicators after changing note data. We don't need to but ....
    if (Edit1.Text <> rsMenuSearch) and (Edit1.Text <> '') then
        DoSearch()
    else begin
        if ButtonNotebookOptions.Enabled then begin
            NB := StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
            if NB <> '' then
                NoteLister.LoadNotebookGrid(StringGrid1, NB);
        end else RefreshStrGrids();
        SelectedNotebook := 0;      // ie off
    end;
    ButtonRefresh.Enabled := false;
end;

procedure TSearchForm.DoSearch();
begin
    if (Edit1.Text = '') then
        ButtonClearFiltersClick(self);
    if (Edit1.Text <> rsMenuSearch) and (Edit1.Text <> '') then begin
        ButtonClearFilters.Enabled := True;
        // TS1:=gettickcount64();
        NoteLister.GetNotes(Edit1.Text);   // observes sett.checkAnyCombo and sett.checkCaseSensitive
        // TS2:=gettickcount64();
        //NoteLister.LoadSearchGrid(StringGrid1);
        NoteLister.LoadStGrid(StringGrid1, 2, True);
        NoteLister.LoadStGridNotebooks(StringGridNotebooks, True);
        // TS3:=gettickcount64();
        // showmessage('Search Speed from SearchUnit ' + inttostr(TS2 - TS1) + 'mS ' + inttostr(TS3-TS2) + 'mS');
        // debugln('Search Speed from SearchUnit ' + inttostr(TS2 - TS1) + 'mS ' + inttostr(TS3-TS2) + 'mS');
        // releasemode, 50mS-70mS, 4-40mS on my linux laptop, longer on common search terms eg "and"
        // windows box, i5 with SSD, 1800 notes, 330mS + 30mS, 'blar'
    end;
end;

procedure TSearchForm.Edit1Exit(Sender: TObject);
begin
	if Edit1.Text = '' then begin
        Edit1.Hint:=rsSearchHint;
        Edit1.Text := rsMenuSearch;
        Edit1.SelStart := 1;
        Edit1.SelLength := length(Edit1.Text);
    end;
end;

procedure TSearchForm.Edit1KeyUp(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
    // Must do this here to stop LCL from selecting the text on VK_RETURN
    if Key = VK_RETURN then begin
      Key := 0;
      DoSearch();
    end;

end;

procedure TSearchForm.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
    CanClose := False;
    hide();
end;

function TSearchForm.IndexNotes() : integer;
// var
	// TS1, TS2 : TTimeStamp;
begin
    // TS1 := DateTimeToTimeStamp(Now);
    //if not Sett.HaveConfig then exit(0);
    if NoteLister <> Nil then
       freeandnil(NoteLister);
    NoteLister := TNoteLister.Create;
    //NoteLister.DebugMode := Application.HasOption('debug-index');
    Result := NoteLister.GetNotes();
    UseList();
    // TS2 := DateTimeToTimeStamp(Now);
	// debugln('That took (mS) ' + inttostr(TS2.Time - TS1.Time));
    MainForm.UpdateNotesFound(Result);      // Says how many notes found and runs over checklist.
    //Sett.CheckAutoSync();
end;

procedure TSearchForm.FormCreate(Sender: TObject);
begin
    NoteLister := nil;
    if MainForm.closeASAP or (MainForm.SingleNoteFileName <> '') then exit;
    StringGrid1.Clear;          // We'll setup the grid columns in Lazarus style, not Delphi
    StringGrid1.FixedCols := 0;
    StringGrid1.Columns.Add;
    StringGrid1.Columns[0].Title.Caption := rsName;
    StringGrid1.Columns.Add;
    StringGrid1.Columns[1].Title.Caption := rsLastChange;
    StringGrid1.FixedRows:=1;
    StringGrid1.Columns[1].Width := self.Canvas.GetTextWidth(' 2020-01-31 14:36:00 ');
    StringGridNotebooks.Clear;
    StringGridNotebooks.FixedCols := 0;
    StringGridNotebooks.Columns.Add;
    StringGridNotebooks.Columns[0].Title.Caption := rsNotebooks;
    StringGridNotebooks.FixedRows:=1;


    Edit1.Hint:=rsSearchHint;
    Edit1.Text := rsMenuSearch;
    Edit1.SelStart := 1;
    Edit1.SelLength := length(Edit1.Text);

    CreateMenus();

    IndexNotes();           // This could be a slow process, maybe a new thread ?
    RefreshMenus(mkAllMenu);    // IndexNotes->UseList has already called RefreshMenus(mkRecentMenu) and Qt5 does not like it.
    ButtonRefreshClick(self);
    //RefreshMenus(mkFileMenu);
    //RefreshMenus(mkHelpMenu);
    //RefreshMenus(mkRecentMenu);
end;

procedure TSearchForm.FormDestroy(Sender: TObject);
begin
  NoteLister.Free;
  NoteLister := Nil;
  HelpNotes.Free;
  HelpNotes := Nil;
end;

procedure TSearchForm.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
     if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then begin
       if key = ord('N') then begin OpenNote(); Key := 0; exit(); end;
       if key = VK_Q then MainForm.Close();
     end;
end;

procedure TSearchForm.FormShow(Sender: TObject);
begin
    // if MainForm.closeASAP or (MainForm.SingleNoteFileName <> '') then exit;
    Left := Placement + random(Placement*2);
    Top := Placement + random(Placement * 2);
    // Edit1.Text:= 'Search';
    CheckCaseSensitive.checked := SearchCaseSensitive;
    StringGridNotebooks.Options := StringGridNotebooks.Options - [goRowHighlight];
    {$ifdef windows}
    StringGrid1.Color := clWhite;   // err ? once changed from clDefault, there is no going back ?                                            // linux apps know how to do this themselves
    if Sett.DarkTheme then begin
         color := Sett.HiColour;
         font.color := Sett.TextColour;
         ButtonNoteBookOptions.Color := Sett.HiColour;
         ButtonClearFilters.Color := Sett.HiColour;
         ButtonMenu.color := Sett.HiColour;
         StringGrid1.Color := Sett.BackGndColour;
         StringGrid1.Font.color := Sett.TextColour;
         stringGrid1.GridLineColor:= clnavy; //Sett.HiColour;
         stringgridnotebooks.GridLineColor:= clnavy;
         StringGrid1.FixedColor := Sett.HiColour;
         StringGridNotebooks.FixedColor := Sett.HiColour;
         ButtonRefresh.Color := Sett.HiColour;
         splitter1.Color:= clnavy;
    end;
    {$endif}
    {
    stringgrid has -
    AltColor - color of alternating rows
    Fixedcolor - color of fixed cells
    color - color of 'control'.
    focuscolor - hollow rectangle around selected cell

    To change selected cell colour we have to use OnPrepareCanvas but use
    SelecteNoteBook to prevent it showing before user has clicked a cell.

    }
    //stringgrid1.FocusColor:= clblue;
    //stringgrid1.Color := clwhite;

    stringgridnotebooks.Font := stringgrid1.font;
    stringgridnotebooks.FocusColor := stringgrid1.FocusColor;
    stringgridnotebooks.color := stringgrid1.color;
    stringgridnotebooks.Font.Color:= stringgrid1.Font.Color;
    stringGridNotebooks.SelectedColor:= clGray;
    {$ifdef DARWIN}ButtonMenu.Refresh;{$endif}      // Cocoa issue
    RefreshStrGrids();
end;

procedure TSearchForm.CheckCaseSensitiveChange(Sender: TObject);
begin
    SearchCaseSensitive := CheckCaseSensitive.Checked;
end;

procedure TSearchForm.Edit1Enter(Sender: TObject);
// ToDo : this should select the word, 'Search' if user clicks in field but does not ??
begin
    if Edit1.Text = rsMenuSearch then begin
        //Edit1.SelStart:=0;
        //Edit1.SelLength:= length(rsMenuSearch);
        Edit1.SelectAll;
    end;
end;

procedure TSearchForm.MarkNoteReadOnly(const FullFileName : string; const WasDeleted : boolean);
var
    TheForm : TForm;
begin
    if NoteLister = nil then exit;
    if NoteLister.IsThisNoteOpen(FullFileName, TheForm) then begin
       // if user opened and then closed, we won't know we cannot access
        try
       	    TEditBoxForm(TheForm).SetReadOnly();
            exit();
        except on  EAccessViolation do
       	    DebugLn('Tried to mark a closed note as readOnly, thats OK');
   	    end;
    end;
    if WasDeleted then
        NoteLister.DeleteNote(FullFileName);
end;

function TSearchForm.MoveWindowHere(WTitle: string): boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
    Result := False;
    {$IFDEF LINUX}      // ToDo : Apparently, Windows now has something like Workspaces, implement .....
    //debugln('In MoveWindowHere with ', WTitle);
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'wmctrl';
    AProcess.Parameters.Add('-R' + WTitle);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        AProcess.Execute;
        Result := (AProcess.ExitStatus = 0);        // says at least one packet got back
    except on
        E: EProcess do debugln('Is wmctrl available ? Cannot move ' + WTitle);
    end;
    {if not Result then
        debugln('wmctrl exit error trying to move ' + WTitle); }  // wmctrl always appears to return something !
    List := TStringList.Create;
    List.LoadFromStream(AProcess.Output);       // just to clear it away.
    //debugln('Process List ' + List.Text);
    List.Free;
    AProcess.Free;
    {$endif}
end;

procedure TSearchForm.OpenNote(NoteTitle: String; FullFileName: string;
		TemplateIs: AnsiString);
// Might be called with no Title (NewNote) or a Title with or without a Filename
var
    EBox : TEditBoxForm;
    NoteFileName : string;
    TheForm : TForm;
begin
    NoteFileName := FullFileName;
    if (NoteTitle <> '') then begin
        if FullFileName = '' then Begin
        	if NoteLister.FileNameForTitle(NoteTitle, NoteFileName) then
            	NoteFileName := NotesDir+ NoteFileName
            else NoteFileName := '';
		end else NoteFileName := FullFileName;
        // if we have a Title and a Filename, it might be open aleady
        if NoteLister.IsThisNoteOpen(NoteFileName, TheForm) then begin
            // if user opened and then closed, we won't know we cannot re-show
            try
            	TheForm.Show;
                MoveWindowHere(TheForm.Caption);
                TheForm.EnsureVisible(true);
                exit();
			except on  EAccessViolation do
            	DebugLn('Tried to re show a closed note, thats OK');
			end;
            // We catch the exception and proceed .... but it should never happen.
        end;
    end;
    // if to here, we need open a new window. If Filename blank, its a new note
    if (NoteFileName = '') and (NoteTitle ='') and ButtonNoteBookOptions.Enabled then  // a new note with notebook selected.
       TemplateIs := StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
	EBox := TEditBoxForm.Create(Application);
    if (NoteFileName <> '') and (NoteTitle <> '') and (Edit1.Text <> '') and (Edit1.Text <> 'Search') then
        // Looks like we have a search in progress, lets take user there when note opens.
        EBox.SearchedTerm := Edit1.Text
    else
        EBox.SearchedTerm := '';
    EBox.NoteTitle:= NoteTitle;
    EBox.NoteFileName := NoteFileName;
    Ebox.TemplateIs := TemplateIs;
    //EBox.Top := Placement + random(Placement*2);
    //EBox.Left := Placement + random(Placement*2);
    EBox.Show;
    EBox.Dirty := False;
    NoteLister.ThisNoteIsOpen(NoteFileName, EBox);
end;

procedure TSearchForm.StringGrid1DblClick(Sender: TObject);
var
    NoteTitle : ANSIstring;
    FullFileName : string;
begin
    { TODO : If user double clicks title bar, we dont detect that and open some other note.  }
    // debugln('Clicked on row ' + inttostr(StringGrid1.Row));
    NoteTitle := StringGrid1.Cells[0, StringGrid1.Row];
    if NoteLister.FileNameForTitle(NoteTitle, FullFileName) then begin
        FullFileName := NotesDir + FullFileName;
  	    if not FileExistsUTF8(FullFileName) then begin
      	    showmessage('Cannot open ' + FullFileName);
      	    exit();
  	    end;
    end;
  	if length(NoteTitle) > 0 then
        OpenNote(NoteTitle, FullFileName);
end;


{ ----------------- NOTEBOOK STUFF -------------------- }

    // This button clears both search term (if any) and restores all notebooks and
    // displays all available notes.
procedure TSearchForm.ButtonClearFiltersClick(Sender: TObject);
begin
        ButtonNotebookOptions.Enabled := False;
        ButtonClearFilters.Enabled := False;
        // ButtonClearFilters.color := clblack;
        StringGridNotebooks.Options := StringGridNotebooks.Options - [goRowHighlight];
        // UseList();

        //self.ButtonRefresh.enabled := False;
        StringGridNoteBooks.Hint := '';
        //StringGrid1.AutoSizeColumns;
        Edit1.Hint:=rsSearchHint;
        Edit1.Text := rsMenuSearch;
        ButtonRefreshClick(self);
        Edit1.SetFocus;
        Edit1.SelStart := 0;
        Edit1.SelLength := length(Edit1.Text);
end;


procedure TSearchForm.StringGridNotebooksClick(Sender: TObject);
begin

    ButtonNotebookOptions.Enabled := True;
    ButtonClearFilters.Enabled := True;
    //StringGridNotebooks.SelectedColor:= clRed;        // does not work !
    // https://forum.lazarus.freepascal.org/index.php/topic,45009.msg317102.html#msg317102
    //StringGridNotebooks.Options := StringGridNotebooks.Options + [goRowHighlight];
    //StringGridNotebooks.repaint;
    ButtonRefreshClick(self);
    SelectedNoteBook := StringGridNotebooks.Row;
    StringGridNotebooks.Hint := 'Options for ' + StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
end;

procedure TSearchForm.ButtonMenuClick(Sender: TObject);
begin
    PopupTBMainMenu.popup;
end;

procedure TSearchForm.ButtonNotebookOptionsClick(Sender: TObject);
begin
    PopupMenuNotebook.Popup;
end;

procedure TSearchForm.MenuEditNotebookTemplateClick(Sender: TObject);
var
    NotebookID : ANSIString;
begin
    NotebookID := NoteLister.NotebookTemplateID(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]);
    if NotebookID = '' then
    	showmessage('Error, cannot open template for ' + StringGridNotebooks.Cells[0, StringGridNotebooks.Row])
    else
    	OpenNote(StringGridNotebooks.Cells[0, StringGridNotebooks.Row] + ' Template',
        		NotesDir + NotebookID);
end;

procedure TSearchForm.MenuRenameNoteBookClick(Sender: TObject);
var
    NotebookPick : TNotebookPick;
begin
        NotebookPick := TNotebookPick.Create(Application);
        //NotebookPick.FullFileName := StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
        try
            NotebookPick.Title := StringGridNotebooks.Cells[0, StringGridNotebooks.Row];
            NotebookPick.ChangeMode := True;
            NotebookPick.Top := Top;
            NotebookPick.Left := Left;
            if mrOK = NotebookPick.ShowModal then
                ButtonClearFilters.Click;
         finally
            NotebookPick.Free;
        end;
end;

procedure TSearchForm.MenuDeleteNotebookClick(Sender: TObject);
begin
    if IDYES = Application.MessageBox('Delete this Notebook',
    			PChar(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]),
       			MB_ICONQUESTION + MB_YESNO) then
		DeleteNote(NotesDir + NoteLister.NotebookTemplateID(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]));
end;

procedure TSearchForm.MenuNewNoteFromTemplateClick(Sender: TObject);
begin
    OpenNote('', NotesDir
    		+ NoteLister.NotebookTemplateID(StringGridNotebooks.Cells[0, StringGridNotebooks.Row]),
            StringGridNotebooks.Cells[0, StringGridNotebooks.Row]);
end;

procedure TSearchForm.SpeedButton1Click(Sender: TObject);
begin
    // note - image is 24x24 tpopupmenu.png from lazarus source
    MainForm.PopupMenuSearch.PopUp;
end;

procedure TSearchForm.StringGrid1Resize(Sender: TObject);
begin
    StringGrid1.Columns[0].Width := StringGrid1.Width - StringGrid1.Columns[1].Width -15;
end;

end.

