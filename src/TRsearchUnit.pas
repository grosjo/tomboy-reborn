unit TRsearchUnit;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, Menus, ComCtrls, ExtCtrls, FileUtil, ActnList,
    Grids, lazLogger,
    TRcommon, TRtexts;


type TTrayTags = (ttNewNote, ttNewNotebook, ttSearch, ttAbout, ttSync, ttSettings, ttQuit);

// These are choices for main popup menus.
type TMenuTags = (mtNewNote, mtNewNotebook, mtQuit, mtSync, mtExport1, mtSettings, mtAbout);

// These are the possible kinds of main menu items
type TMenuKind = (mkFileMenu, mkRecentMenu, mkHelpMenu, mkAllMenu);


type

{ TFormSearch }

 TFormSearch = class(TForm)

        CheckCaseSensitive: TCheckBox;
        MainMenu: TMainMenu;
        SearchBox: TEdit;
        Label1: TLabel;
	Panel1: TPanel;
	Splitter1: TSplitter;
        StatusBar1: TStatusBar;
        SGNotes: TStringGrid;
	SGNotebooks: TStringGrid;
	SGImage : TImage;

        //TRAY
        TrayIcon: TTrayIcon;
        TrayMenu : TPopupMenu;
        TrayTimer : TTimer;

        procedure FormCreate(Sender: TObject);
	procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	procedure FormShow(Sender: TObject);

        procedure TrayIconClick(Sender: TObject);
        procedure BuildTrayMenu(Sender: TObject);
        procedure TrayMenuClicked(Sender : TObject);
        procedure MainMenuClicked(Sender : TObject);

        procedure CheckCaseSensitiveChange(Sender: TObject);
        procedure SearchBoxChange(Sender: TObject);

        procedure SGNotesResize(Sender: TObject);
	procedure SGNotesDblClick(Sender: TObject);
        procedure SGNotesPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
        procedure SGNotesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
        procedure SGNotesClick(Sender: TObject);

        procedure SGNotebooksClick(Sender: TObject);
        procedure SGNotebooksPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
        procedure SGNotebooksResize(Sender: TObject);
	procedure SGNotebooksDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
private
        NotesList : TNoteInfoList;
        NotebooksList : TStringList;

        LastSync : TDateTime;
        SyncTimer : TTimer;
        ScanTimer : TTimer;
        ShowTimer : TTimer;

        SearchCaseSensitive : boolean;      // Flag linked to the checkbox
        SelectedNotebook : String;         // Notebook selected
        SelectedNote : String;         // Note ID selected

        procedure ShowLists(sender: TObject);
        function AddNotebook(nb : String) : boolean;

        procedure ScanNotes(Sender: TObject);
        procedure ProcessSync(Sender: TObject);
        procedure ProcessSyncUpdates(const DeletedList, DownList: TStringList);

        function MoveWindowHere(WTitle: string): boolean;
        { If there is an open note from the passed filename, it will be marked read Only,
          If deleted, remove entry from NoteLister, will accept a GUID, Filename or FullFileName inc path }
        procedure MarkNoteReadOnly(const FullFileName: string; const WasDeleted : boolean);

        { Gets called with a title and filename (clicking grid), with just a title
          (clicked a note link or recent menu item or Link Button) or nothing
          (new note). If its just Title but Title does not exist, its Link
          Button. }
        procedure OpenNote(NoteTitle : String = ''; FullFileName : string = ''; TemplateIs : AnsiString = '');
        { Deletes the actual file then removes the indicated note from the internal
        data about notes, refreshes Grid }
        procedure DeleteNote(const FullFileName : ANSIString);

end;

implementation

{$R *.lfm}



uses
    LCLType,
    LazFileUtils,
    TRsettings,		// Manages settings.
    TRsync,
    TRnoteEdit,
    process,        // Linux, we call wmctrl to move note to current workspace
    NoteBook;


procedure TFormSearch.FormCreate(Sender: TObject);
var
    m1,m2,m3 : TMenuItem;
begin
   TRlog('TFormSearch.FormCreate');

    NotesList := TNoteInfoList.Create;
    NotebooksList := TStringList.Create;

    SGNotes.Clear;
    SGNotes.FixedCols := 0;
    SGNotes.Columns.Add;
    SGNotes.Columns[0].Title.Caption := rsName;
    SGNotes.Columns.Add;
    SGNotes.Columns[1].Title.Caption := rsLastChange;
    SGNotes.FixedRows:=1;
    SGNotes.Columns[1].Width := self.Canvas.GetTextWidth(' 2020-01-31 14:36:00 ');
    SGNotes.Columns.Add;
    SGNotes.Columns[2].Title.Caption := 'ID';
    SGNotes.Columns[2].Visible := false;
    SGNotes.GridLineStyle:=TPenStyle.psDashDotDot;
    SGNotes.Options := SGNotes.Options - [goRowHighlight]+[goRowSelect];
    SGNotes.ScrollBars := ssAutoVertical;
    SGNotes.FocusRectVisible := false;
    SGNotes.TitleStyle := tsNative;
    SGNotes.GridLineStyle:=TPenStyle.psClear;

    SGNotebooks.Clear;
    SGNotebooks.FixedCols := 0;
    SGNotebooks.Columns.Add;
    SGNotebooks.Columns[0].Title.Caption := rsNotebooks;
    SGNotebooks.FixedRows:=1;
    SGNotebooks.GridLineStyle:=TPenStyle.psClear;
    SGNotebooks.Hint := rsNotebookHint;
    SGNotebooks.Options := SGNotebooks.Options - [goRowHighlight];
    SGNotebooks.ScrollBars := ssAutoVertical;
    SGNotebooks.FocusRectVisible := false;

    {
    stringgrid has -
    AltColor - color of alternating rows
    Fixedcolor - color of fixed cells
    color - color of 'control'.
    focuscolor - hollow rectangle around selected cell
    }

    SearchBox.Hint:=rsSearchHint;
    SearchBox.Text := '';

    if UseTrayIcon then
    begin
        TrayMenu := TPopupMenu.Create(Self);
        TrayIcon.PopUpMenu := TrayMenu;

        TrayIcon.Show;
        TrayTimer := TTimer.Create(Nil);
        TrayTimer.OnTimer:= @BuildTrayMenu;
        TrayTimer.Interval:=200;
        TrayTimer.Enabled := True;
    end;

    MainMenu.Items.Clear;
    // 'File'
    m1 := TMenuItem.Create(MainMenu);
    m1.Caption := rsMenuNotes;
    MainMenu.Items.Add(m1);
    // 'File' / New Note
    m2 := TMenuItem.Create(m1);
    m2.Tag := ord(mtNewNote);
    m2.Caption := rsMenuNewNote;
    m2.OnClick := @MainMenuClicked;
    m1.Add(m2);
    // 'File' / Sync
    m2 := TMenuItem.Create(m1);
    m2.Tag := ord(mtSync);
    m2.Caption := rsMenuSync;
    m2.OnClick := @MainMenuClicked;
    m1.Add(m2);
    // 'File' / Quit
    m2 := TMenuItem.Create(m1);
    m2.Tag := ord(mtQuit);
    m2.Caption := rsMenuQuit;
    m2.OnClick := @MainMenuClicked;
    m1.Add(m2);

    // 'Options'
    m1 := TMenuItem.Create(MainMenu);
    m1.Caption := rsMenuOptions;
    MainMenu.Items.Add(m1);

    // Options / Export
    m2 := TMenuItem.Create(m1);
    m2.Caption := rsMenuExport;
    m1.Add(m2);
    // Options / Export1
    m3 := TMenuItem.Create(m1);
    m3.Tag := ord(mtExport1);
    m3.Caption := rsMenuExport;
    m2.Add(m3);

    // Options / Settings
    m2 := TMenuItem.Create(m1);
    m2.Tag := ord(mtSettings);
    m2.Caption := rsMenuSettings;
    m2.OnClick := @MainMenuClicked;
    m1.Add(m2);

    // 'Help'
    m1 := TMenuItem.Create(MainMenu);
    m1.Caption := rsMenuHelp;
    MainMenu.Items.Add(m1);
    // Help / About
    m2 := TMenuItem.Create(m1);
    m2.Caption := rsMenuAbout;
    m1.Add(m2);


    // TIMERS

    ScanTimer := TTimer.Create(Self);
    ScanTimer.OnTimer := @ScanNotes;
    ScanTimer.Interval := 200;
    ScanTimer.Enabled := True;

    SyncTimer := TTimer.Create(Self);
    SyncTimer.OnTimer := @ProcessSync;
    SyncTimer.Interval := 1000;
    SyncTimer.Enabled := True;

end;


{ ====== TRAY WORK ====== }

procedure TFormSearch.TrayIconClick(Sender: TObject);
var
    p : TPoint;
begin
    p := TrayIcon.GetPosition;
    TrayMenu.PopUp(p.x,p.y+24);
end;

procedure TFormSearch.BuildTrayMenu(Sender : TObject);
var
    m1,m2 : TMenuItem;
begin
   TRlog('BuildTrayMenu');

   TrayTimer.Enabled := False;
   FreeAndNil(TrayTimer);

   TrayMenu.Items.Clear;
   // New Note
   m1 := TMenuItem.Create(TrayMenu);
   m1.Tag := ord(ttNewNote);
   m1.Caption := rsTrayNewNote;
   m1.OnClick := @TrayMenuClicked;
   TrayMenu.Items.Add(m1);

   // App menu
   m1 := TMenuItem.Create(TrayMenu);
   m1.Caption := rsTrayApplication;
   TrayMenu.Items.Add(m1);

   // Search
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttSearch);
   m2.Caption := rsTraySearchNote;
   m2.OnClick := @TrayMenuClicked;
   m1.Add(m2);
   // Sync
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttSync);
   m2.Caption := rsTraySync;
   m2.OnClick := @TrayMenuClicked;
   m1.Add(m2);
   // Settings
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttSettings);
   m2.Caption := rsTraySettings;
   m2.OnClick := @TrayMenuClicked;
   m1.Add(m2);
   // About
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttAbout);
   m2.Caption := rsTrayAbout;
   m2.OnClick := @TrayMenuClicked;
   m1.Add(m2);
   // Quit
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttQuit);
   m2.Caption := rsTrayQuit;
   m2.OnClick := @TrayMenuClicked;
   m1.Add(m2);

   TrayMenu.Items.AddSeparator;

   // Notebooks
   m1 := TMenuItem.Create(TrayMenu);
   m1.Caption := rsTrayNotebooks;
   TrayMenu.Items.Add(m1);

   // List notebooks
   m2 := TMenuItem.Create(m1);
   m2.Caption := 'test NB';
   //m2.OnClick := nil;
   m1.Add(m2);

   // Add latest notes

end;

procedure TFormSearch.TrayMenuClicked(Sender : TObject);
var
    FormSettings : TSettings;
    FormSync : TFormSync;
begin

   TRlog('TrayMenuClicked');

   case TTrayTags(TMenuItem(Sender).Tag) of
        ttNewNote : if (NotesDir = '')
                  then ShowMessage(rsSetupNotesDirFirst)
                  else OpenNote();
        ttSettings:
          begin
            TRlog('TrayMenuClicked Settings');
            FormSettings := TSettings.Create(self);
            FormSettings.ShowModal;
            FreeAndNil(FormSettings);
          end;
        ttSync :
          begin
            TRlog('TrayMenuClicked Sync');
            if(isSyncConfigured()) then
            begin
               try
                  FormSync := TFormSync.Create(Self);
                  FormSync.SyncVisible();
                  TRlog('menu click ttSync done');
                  FreeAndNil(FormSync);
               except on E:Exception do TRlog(E.message);
               end;
            end
            else ShowMessage(rsSetupSyncFirst);
          end;
        ttQuit :      Self.close;

   end;

   TrayTimer := TTimer.Create(Nil);
   TrayTimer.OnTimer:= @BuildTrayMenu;
   TrayTimer.Interval:=200;
   TrayTimer.Enabled := True;

end;

{ ======= MAIN MENU ====== }

procedure TFormSearch.MainMenuClicked(Sender : TObject);
var
    FormSettings : TSettings;
    FormSync : TFormSync;
begin

   //type TMenuTags = (mtNewNote, mtNewTemplate, mtQuit, mtSync, mtExport, mtSettings, mtAbout);

   TRlog('MainMenuClicked');

   case TMenuTags(TMenuItem(Sender).Tag) of
        mtSettings: begin
            TRlog('MainMenuClicked Settings');
            FormSettings := TSettings.Create(self);
            FormSettings.ShowModal;
            FreeAndNil(FormSettings);
            end;
        mtSync : begin
            TRlog('MainMenuClicked Sync');
            if(isSyncConfigured()) then
            begin
               try
                  FormSync := TFormSync.Create(Self);
                  FormSync.SyncVisible();
                  TRlog('menu click mtSync done');
                  FreeAndNil(FormSync);
               except on E:Exception do TRlog(E.message);
               end;
            end
            else ShowMessage(rsSetupSyncFirst);
            end;
   end;
end;

{ ======= LISTS MANAGEMENT ====== }

function TFormSearch.AddNotebook(nb : String): boolean;
var
   i : integer;
begin
   i:=0;
   while(i<NotebooksList.Count) do
    begin
        if(CompareText(NotebooksList.Strings[i],nb) = 0) then exit(false);
        inc(i);
    end;

   NotebooksList.Add(nb);
   Result:=true;
end;

procedure TFormSearch.ScanNotes(Sender : TObject);
Var
    ID : String;
    n : PNoteInfo;
    i,c1,c2 : integer;
    Info : TSearchRec;
begin
   TRlog('ScanNotes');

   ScanTimer.Enabled := False;
   FreeAndNil(ScanTimer);

   NotebooksList.Clear;

   c1:=0; c2:=0;

   if FindFirstUTF8(GetLocalNoteFile('*'), faAnyFile, Info)=0 then
   repeat
        ID := copy(Info.Name, 1, 36);
        n := NotesList.FindID(ID);
        if(n = nil) then
        begin
           new(n);
           n^.Action:=SynUnset;
           n^.ID := ID;
           n^.Rev := -1;
           n^.LastSyncGMT := 0;
           n^.LastSync := '';
           n^.OpenNote:= nil;
           NotesList.Add(n);
        end
        else FreeAndNil(n^.Tags);

        inc(c1);
        FileToNote(GetLocalNoteFile(ID), n);
        i:=0;
        while(i<n^.Tags.Count) do
        begin
            if(AddNotebook(n^.Tags.Strings[i])) then inc(c2);
            inc(i);
        end;
   until FindNext(Info) <> 0;
   FindClose(Info);

   TRlog('Found '+IntToStr(c1)+' local notes and '+IntToStr(c2) + ' notebooks');

   ShowLists(Self);

   StatusBar1.SimpleText:= 'Found '+IntToStr(c1)+' local notes and '+IntToStr(c2) + ' notebooks';

end;

procedure TFormSearch.ProcessSync(Sender : TObject);
begin
   SyncTimer.Enabled := False;
   FreeAndNil(SyncTimer);

   TRlog('ProcessSync');
   //StatusBar1.SimpleText:= 'ProcessSync '+IntToStr(Random(10000));

   if(SyncRepeat>0) then begin
      TRlog('Should process sync every '+IntToStr(SyncRepeat));
   end else TRlog('AutoSync not enabled');

   SyncTimer := TTimer.Create(Self);
   SyncTimer.OnTimer := @ProcessSync;
   SyncTimer.Interval := 60000;
   SyncTimer.Enabled := True;

end;

procedure TFormSearch.ProcessSyncUpdates(const DeletedList, DownList : TStringList);
var
    i : integer;
    n : PNoteInfo;
    changes : boolean;
begin

   changes := false;
   i:=0;
   while(i<DeletedList.Count) do
    begin
        n := NotesList.FindID(DeletedList.Strings[i]);
        if(n<>nil) then
        begin
           if(n^.OpenNote <> nil) then
           begin
                n^.OpenNote^.Close;
                n^.OpenNote := nil;
           end;
           NotesList.Remove(n);
           changes := true;
        end;
        inc(i);
    end;

   i:=0;
   while(i<DownList.Count) do
    begin
        n := NotesList.FindID(DeletedList.Strings[i]);
        if(n = nil) then
        begin
           new(n);
           FileToNote(GetLocalNoteFile(n^.ID),n);
           NotesList.Add(n);
        end else
        begin
           if(n^.OpenNote <> nil) then
           begin
                n^.OpenNote^.Close;
                n^.OpenNote := nil;
           end;
           FileToNote(GetLocalNoteFile(n^.ID),n);
           changes := true;
        end;
        inc(i);
    end;

   if(changes) then ShowLists(Self);
end;

{ ====== UI MANAGEMENT ====== }

procedure TFormSearch.SGNotebooksPrepareCanvas(sender: TObject; aCol,
    aRow: Integer; aState: TGridDrawState);
begin
   TRlog('SGNotebooksPrepareCanvas');

   if (length(SelectedNoteBook) > 0) and (CompareText(SGNotebooks.Cells[0,aRow],SelectedNotebook) = 0)
   then  SGNotebooks.canvas.brush.color := clLtGray;

   if((length(SelectedNoteBook) = 0) and (aRow = 1))
   then  SGNotebooks.canvas.brush.color := clLtGray;

end;

procedure TFormSearch.SGNotesPrepareCanvas(sender: TObject; aCol,
    aRow: Integer; aState: TGridDrawState);
begin
   TRlog('SGNotesPrepareCanvas SelectedNote='+SelectedNote+' SGNITES='+SGNotes.Cells[2,aRow]);

   if (length(SelectedNote) > 0) and (CompareText(SGNotes.Cells[2,aRow],SelectedNote) = 0)
   then  SGNotes.canvas.brush.color := clYellow;

end;

procedure TFormSearch.SGNotebooksResize(Sender: TObject);
begin
    SGNotebooks.Columns[0].Width := SGNotebooks.width-1;
end;


procedure TFormSearch.DeleteNote(const FullFileName: ANSIString);
var
    NewName, ShortFileName : ANSIString;
    //LocalMan : TSync;
begin
    TRlog('TO BE DONE');
    ShowMessage('TO BE DONE');
    {
    ShortFileName := ExtractFileNameOnly(FullFileName);
    if NoteLister.IsATemplate(ShortFileName) then begin
        NoteLister.DeleteNoteBookwithID(ShortFileName);
      	DeleteFileUTF8(FullFileName);
        ButtonClearFiltersClick(self);
    end else begin
        NoteLister.DeleteNote(ShortFileName);
     	NewName := GetLocalNoteFile(ShortFileName,GetLocalBackupPath());

        if not RenameFileUTF8(FullFileName, NewName)
    		then TRlog('Failed to move ' + FullFileName + ' to ' + NewName);
    end;
    UseList();
    }
end;

procedure TFormSearch.ShowLists(sender: TObject);
var
   sl : TStringList;
   s : STring;
   i : integer;
   n : PNoteInfo;
begin
   TRlog('ShowLists');

   if(assigned(ShowTimer)) then
   begin
      ShowTimer.Enabled := False;
      FreeAndNil(ShowTimer);
   end;

   if not Visible then exit;

   sl := TStringList.Create;
   s := SearchBox.Text;
   s := Trim(s);
   sl.Delimiter:=' ';
   sl.StrictDelimiter:=true;
   sl.DelimitedText:=s;
   i:=0;

   while(i<sl.Count) do
    begin
        sl.Strings[i] := Trim(sl.Strings[i]);
        inc(i);
    end;

   // Show notebooks
   while SGNotebooks.RowCount > 1 do SGNotebooks.DeleteRow(SGNotebooks.RowCount-1);
   SGNotebooks.InsertRowWithValues(SGNotebooks.RowCount,[rsAnyNotebook]);
   for i := 0 to NotebooksList.Count - 1 do
        SGNotebooks.InsertRowWithValues(SGNotebooks.RowCount, [NotebooksList.Strings[i]]);

   // Show notes
   while SGNotes.RowCount > 1 do SGNotes.DeleteRow(SGNotes.RowCount-1);
   for i := 0 to NotesList.Count - 1 do
   begin
       n := NotesList.Items[i];
       if(not NoteBelongs(SelectedNotebook,n)) then continue;
       if(not NoteContains(sl,n,SearchCaseSensitive)) then continue;

       SGNotes.InsertRowWithValues(SGNotes.RowCount, [n^.Title,GetDisplayTimeFromGMT(n^.LastChangeGMT),n^.ID]);
   end;

   sl.Free;
end;

procedure TFormSearch.SearchBoxChange(Sender: TObject);
begin
   TRlog('SearchBoxChange');

   if(assigned(ShowTimer)) then
    begin
      ShowTimer.Enabled := False;
      FreeAndNil(ShowTimer);
    end;

   ShowTimer:= TTimer.Create(Self);
   ShowTimer.OnTimer := @ShowLists;
   ShowTimer.Interval := 1000;
   ShowTimer.Enabled := True;
end;

procedure TFormSearch.FormDestroy(Sender: TObject);
begin
  FreeAndNil(NotesList);
  FreeAndNil(NotebooksList);
end;

procedure TFormSearch.FormKeyDown(Sender: TObject; var Key: Word;
    Shift: TShiftState);
begin
     if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then begin
       if key = ord('N') then begin OpenNote(); Key := 0; exit(); end;
       //if key = VK_Q then MainForm.Close();
     end;
end;

procedure TFormSearch.FormShow(Sender: TObject);
begin
    Left := Placement + random(Placement*2);
    Top := Placement + random(Placement * 2);
end;

procedure TFormSearch.CheckCaseSensitiveChange(Sender: TObject);
begin
    SearchCaseSensitive := CheckCaseSensitive.Checked;

    if(assigned(ShowTimer)) then
    begin
      ShowTimer.Enabled := False;
      FreeAndNil(ShowTimer);
    end;
    ShowTimer:= TTimer.Create(Self);
    ShowTimer.OnTimer := @ShowLists;
    ShowTimer.Interval := 100;
    ShowTimer.Enabled := True;

end;

procedure TFormSearch.MarkNoteReadOnly(const FullFileName : string; const WasDeleted : boolean);
var
    TheForm : TForm;
begin
    {
    if NoteLister = nil then exit;
    if NoteLister.IsThisNoteOpen(FullFileName, TheForm) then begin
       // if user opened and then closed, we wont know we cannot access
        try
       	    TNoteEditForm(TheForm).SetReadOnly();
            exit();
        except on  EAccessViolation do
       	    TRlog('Tried to mark a closed note as readOnly, thats OK');
   	    end;
    end;
    if WasDeleted then
        NoteLister.DeleteNote(FullFileName);
        }
end;

function TFormSearch.MoveWindowHere(WTitle: string): boolean;
var
    AProcess: TProcess;
    List : TStringList = nil;
begin
   {
    Result := False;
    {$IFDEF LINUX}      // ToDo : Apparently, Windows now has something like Workspaces, implement .....
    //TRlog('In MoveWindowHere with ', WTitle);
    AProcess := TProcess.Create(nil);
    AProcess.Executable:= 'wmctrl';
    AProcess.Parameters.Add('-R' + WTitle);
    AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];
    try
        AProcess.Execute;
        Result := (AProcess.ExitStatus = 0);        // says at least one packet got back
    except on
        E: EProcess do TRlog('Is wmctrl available ? Cannot move ' + WTitle);
    end;
    {if not Result then
        TRlog('wmctrl exit error trying to move ' + WTitle); }  // wmctrl always appears to return something !
    List := TStringList.Create;
    List.LoadFromStream(AProcess.Output);       // just to clear it away.
    //TRlog('Process List ' + List.Text);
    List.Free;
    AProcess.Free;
    {$endif}
    }
end;

procedure TFormSearch.OpenNote(NoteTitle: String; FullFileName: string;
		TemplateIs: AnsiString);
// Might be called with no Title (NewNote) or a Title with or without a Filename
var
    EBox : TNoteEditForm;
    NoteFileName : string;
    TheForm : TForm;
begin
   {
    NoteFileName := FullFileName;
    if (NoteTitle <> '') then begin
        if FullFileName = '' then Begin
        	if NoteLister.FileNameForTitle(NoteTitle, NoteFileName) then
            	NoteFileName := NotesDir+ NoteFileName
            else NoteFileName := '';
		end else NoteFileName := FullFileName;
        // if we have a Title and a Filename, it might be open aleady
        if NoteLister.IsThisNoteOpen(NoteFileName, TheForm) then begin
            // if user opened and then closed, we wont know we cannot re-show
            try
            	TheForm.Show;
                MoveWindowHere(TheForm.Caption);
                TheForm.EnsureVisible(true);
                exit();
			except on  EAccessViolation do
            	TRlog('Tried to re show a closed note, thats OK');
			end;
            // We catch the exception and proceed .... but it should never happen.
        end;
    end;
    // if to here, we need open a new window. If Filename blank, its a new note
    //if (NoteFileName = '') and (NoteTitle ='') and ButtonNoteBookOptions.Enabled then  // a new note with notebook selected.
    //   TemplateIs := SGNotebooks.Cells[0, SGNotebooks.Row];

    EBox := TNoteEditForm.Create(Application);

    if (NoteFileName <> '') and (NoteTitle <> '') and (SearchBox.Text <> '') and (SearchBox.Text <> 'Search') then
        // Looks like we have a search in progress, lets take user there when note opens.
        EBox.SearchedTerm := SearchBox.Text
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
    }
end;

procedure TFormSearch.SGNotesDblClick(Sender: TObject);
var
   r : integer;
   n : PNoteInfo;
   ID : String;
begin
   r := SGNotes.Row;
   TRlog('Double Clicked (Notes) on line : '+IntToStr(r));

   if(r>0) then begin
     ID := SGNotes.Cells[2, r];
     TRlog('Double Clicked (Notes) -> ID = '+ID);
     n := NotesList.FindID(ID);
     if(n<>nil) then
     begin
          TRlog('Clicked on Note:'+n^.ID+' Title='+n^.Title);
     end
     else TRlog('ID not found...');
   end;

    { TODO : If user double clicks title bar, we dont detect that and open some other note.  }
    // TRlog('Clicked on row ' + inttostr(SGNotesList.Row));
    {
   NoteTitle := SGNotesList.Cells[0, SGNotesList.Row];
    if NoteLister.FileNameForTitle(NoteTitle, FullFileName) then begin
        FullFileName := NotesDir + FullFileName;
  	    if not FileExistsUTF8(FullFileName) then begin
      	    showmessage('Cannot open ' + FullFileName);
      	    exit();
  	    end;
    end;
  	if length(NoteTitle) > 0 then
        OpenNote(NoteTitle, FullFileName);
        }
end;

procedure TFormSearch.SGNotesDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
   TRlog('SGNotesDrawCell col='+IntToStr(Acol)+' row='+IntToStr(Arow));

   //SGNotesList.Canvas.Draw(Rect.Left, Rect.Top, SGImage.Picture.Graphic);

end;

procedure TFormSearch.SGNotebooksDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
begin
   TRlog('SGNotebooksDrawCell col='+IntToStr(Acol)+' row='+IntToStr(Arow));

   //SGNotesList.Canvas.Draw(Rect.Left, Rect.Top, SGImage.Picture.Graphic);

end;

procedure TFormSearch.SGNotebooksClick(Sender: TObject);
begin
   TRlog('SGNotebooksClick on row '+IntToStr(SGNotebooks.Row));
   if(SGNotebooks.Row<2)
   then SelectedNoteBook := ''
   else SelectedNoteBook := SGNotebooks.Cells[0,SGNotebooks.Row];

   if(assigned(ShowTimer)) then
   begin
      ShowTimer.Enabled := False;
      FreeAndNil(ShowTimer);
   end;

   ShowTimer := TTimer.Create(Self);
   ShowTimer.OnTimer := @ShowLists;
   ShowTimer.Interval := 100;
   ShowTimer.Enabled := True;

   StatusBar1.SimpleText:= 'Notebook "'+SelectedNoteBook+'" selected';

   TRlog('SGNotebooksClick NB = '+SelectedNotebook);
end;

procedure TFormSearch.SGNotesClick(Sender: TObject);
begin
   TRlog('SGNotesClick on row '+IntToStr(SGNotes.Row));
   if(SGNotes.Row<1)
   then SelectedNote := ''
   else SelectedNote := SGNotes.Cells[2,SGNotes.Row];

   SGNotes.Refresh;

   StatusBar1.SimpleText:= 'Note "'+SelectedNote+'" selected';

   TRlog('SGNotesClick ID = '+SelectedNote);
end;

procedure TFormSearch.SGNotesResize(Sender: TObject);
begin
    SGNotes.Columns[0].Width := SGNotes.Width - SGNotes.Columns[1].Width; // -15;
end;

end.

