unit TRmain;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, Menus, ComCtrls, ExtCtrls, FileUtil, ActnList, DateUtils,
    Grids, lazLogger, Math, LCLType, LazFileUtils, process, LazUTF8,
    TRcommon, TRtexts, TRsettings, TRsync, TRnote, TRabout;


type TTrayTags = (ttNewNote, ttSearch, ttAbout, ttSync, ttSettings, ttQuit);

// These are choices for main popup menus.
type TMenuTags = (mtNewNote, mtNewNotebook, mtDeleteNote, mtDeleteNotebook, mtQuit, mtSync, mtExport1, mtSettings, mtAbout);


type TFormMain = class(TForm)

        CheckCaseSensitive: TCheckBox;
        MenuIconList: TImageList;
        MainMenu: TMainMenu;
        FileMenu : TMenuItem;
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

        procedure FormCreate(Sender: TObject);
	procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
        procedure FormDestroy(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
	procedure FormShow(Sender: TObject);

        procedure TrayIconClick(Sender: TObject);
        procedure BuildTrayMenu(Sender: TObject);
        procedure TrayMenuClicked(Sender : TObject);
        procedure TrayNoteClicked(Sender : TObject);

        procedure BuildFileMenu(Sender: TObject);
        procedure MainMenuClicked(Sender : TObject);
        procedure MenuNewNotebookNote(Sender : TObject);

        procedure OpenNote(ID : UTF8String = ''; Notebook : UTF8String = '' ; Title : UTF8String = '');
        procedure OpenNoteByTitle(t : UTF8String);
        procedure PostScan();

        procedure CheckCaseSensitiveChange(Sender: TObject);
        procedure SearchBoxChange(Sender: TObject);

        procedure SGNotesResize(Sender: TObject);
	procedure SGNotesDblClick(Sender: TObject);
        procedure SGNotesPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
        procedure SGNotesClick(Sender: TObject);

        procedure SGNotebooksClick(Sender: TObject);
        procedure SGNotebooksPrepareCanvas(sender: TObject; aCol, aRow: Integer; aState: TGridDrawState);
        procedure SGNotebooksResize(Sender: TObject);
	procedure SGNotebooksDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);

        function AddNewNotebook(nb : UTF8String): boolean;

private
        NotesList : TNoteInfoList;
        NewNotebooksList : TStringList;

        LastSync, LastClick : TDateTime;
        SyncTimer : TTimer;
        ScanTimer : TTimer;
        ShowTimer : TTimer;
        fsett,fabout : TForm;

        SearchCaseSensitive : boolean;      // Flag linked to the checkbox
        FirstList : boolean;
        SelectedNotebook : UTF8String;         // Notebook selected
        SelectedNote : UTF8String;         // Note ID selected

        LastClickedRow : integer;

        syncshallrun : boolean;
        procedure ShowLists(sender: TObject);
        function AddNotebook(nb : UTF8String) : boolean;

        function AddLastUsed(ID : UTF8String; atend : boolean = false) : boolean;

        procedure ScanNotes(Sender: TObject);
        procedure ProcessSync(Sender: TObject);
        procedure ProcessSyncUpdates(const DeletedList, DownList: TStringList);

        function DeleteNote(ID: UTF8String) : boolean;
        function SaveNote(ID: UTF8String) : boolean;

        function DeleteNotebook(nb : UTF8String): boolean;


public
        NotebooksList : TStringList;
        ToBeOpened : String;

        procedure SnapSync();
        procedure ShowSettings();
        procedure ShowAbout();

end;


implementation

{$R *.lfm}


procedure TFormMain.FormCreate(Sender: TObject);
var
    m1,m2,m3 : TMenuItem;
begin
   TRlog('TFormMain.FormCreate');

   syncshallrun := true;

   ToBeOpened := '';

   NotesList := TNoteInfoList.Create;
    NotesList.Lname := 'TSearch list';

    NotebooksList := TStringList.Create;
    NewNotebooksList := TStringList.Create;

    LastUsed := TStringList.Create;

    SGNotes.Clear;
    SGNotes.FixedCols := 0;
    SGNotes.GridLineStyle:=TPenStyle.psDashDotDot;
    SGNotes.Options := SGNotes.Options - [goRowHighlight]; //+[goRowSelect];
    SGNotes.ScrollBars := ssVertical;
    SGNotes.FocusRectVisible := false;
    SGNotes.TitleStyle := tsNative;
    SGNotes.GridLineStyle:=TPenStyle.psClear;

    SGNotes.Columns.Add;
    SGNotes.Columns[0].Title.Caption := rsName;

    SGNotes.Columns.Add;
    SGNotes.Columns[1].Title.Caption := rsLastChange;
    SGNotes.Columns[1].Width := SGNotes.Canvas.GetTextWidth(' 0000-00-30 00:00:00 ');

    SGNotes.Columns.Add;
    SGNotes.Columns[2].Title.Caption := 'ID';
    SGNotes.Columns[2].Visible := false;

    SGNotes.FixedRows:=1;
    SGNotes.AutoFillColumns:= true;
    SGNotes.Columns[0].SizePriority:=1;
    SGNotes.Columns[1].SizePriority:=0;


    SGNotebooks.Clear;
    SGNotebooks.FixedCols := 0;
    SGNotebooks.Columns.Add;
    SGNotebooks.Columns[0].Title.Caption := '';
    SGNotebooks.Columns.Add;
    SGNotebooks.Columns[1].Title.Caption := rsNotebooks;
    SGNotebooks.Columns.Add;
    SGNotebooks.Columns[2].Title.Caption := 'Notebook';
    SGNotebooks.Columns[2].Visible := false;
    SGNotebooks.FixedRows:=1;
    SGNotebooks.GridLineStyle:=TPenStyle.psClear;
    SGNotebooks.Hint := rsNotebookHint;
    SGNotebooks.Options := SGNotebooks.Options - [goRowHighlight]; // +[goRowSelect];
    SGNotebooks.ScrollBars := ssAutoVertical;
    SGNotebooks.FocusRectVisible := false;
    SGNotebooks.TitleStyle := tsNative;

    FirstList := false;

    {
    stringgrid has -
    AltColor - color of alternating rows
    Fixedcolor - color of fixed cells
    color - color of 'control'.
    focuscolor - hollow rectangle around selected cell
    }

    SearchBox.Hint:=rsSearchHint;
    SearchBox.Text := '';

    MainMenu.Items.Clear;
    MainMenu.Images := MenuIconList;

    PostScan();

    // 'File'
    FileMenu := TMenuItem.Create(MainMenu);
    FileMenu.Caption := rsMenuNotes;
    MainMenu.Items.Add(FileMenu);

    // 'Tools'
    m1 := TMenuItem.Create(MainMenu);
    m1.Caption := rsMenuTools;
    MainMenu.Items.Add(m1);

    // 'Tools' / Sync
    m2 := TMenuItem.Create(m1);
    m2.Tag := ord(mtSync);
    m2.Caption := rsMenuSync+ '(Ctrl-S)';
    m2.OnClick := @MainMenuClicked;
    m2.ImageIndex:=10;
    m1.Add(m2);

    // Tools / Export
    m2 := TMenuItem.Create(m1);
    m2.Caption := rsMenuExport;
    m1.Add(m2);
    // Tools / Export1
    m3 := TMenuItem.Create(m1);
    m3.Tag := ord(mtExport1);
    m3.Caption := rsMenuExport;
    m2.Add(m3);
    // Tools / Settings
    m2 := TMenuItem.Create(m1);
    m2.Tag := ord(mtSettings);
    m2.Caption := rsMenuSettings + '(Ctrl-O)';
    m2.OnClick := @MainMenuClicked;
    m2.ImageIndex:=8;
    m1.Add(m2);

    // 'Help'
    m1 := TMenuItem.Create(MainMenu);
    m1.Caption := rsMenuHelp;
    MainMenu.Items.Add(m1);
    // Help / About
    m2 := TMenuItem.Create(m1);
    m2.Caption := rsMenuAbout;
    m2.Tag := ord(mtAbout);
    m2.OnClick := @MainMenuClicked;
    m2.ImageIndex:=9;
    m1.Add(m2);


    if UseTrayIcon then
    begin
        TrayMenu := TPopupMenu.Create(Self);
        TrayIcon.PopUpMenu := TrayMenu;
        MenuIconList.GetIcon(2, TrayIcon.Icon);
        TrayIcon.Show;
    end;

    LastSync := now;

    // TIMERS
    SyncTimer := TTimer.Create(nil);
    SyncTimer.OnTimer := @ProcessSync;
    SyncTimer.Interval := 1000;
    SyncTimer.Enabled := True;

    Application.ProcessMessages;

    TRlog('Checking searchatstart '+BoolToStr(SearchAtStart));
    if(not SearchAtStart) then Hide();
end;


{ ====== TRAY WORK ====== }

procedure TFormMain.TrayIconClick(Sender: TObject);
var
    p : TPoint;
begin
    BuildTrayMenu(Sender);
    p := TrayIcon.GetPosition;
    TrayMenu.PopUp(p.x,p.y+24);
end;


procedure TFormMain.BuildFileMenu(Sender : TObject);
var
    m1,m2 : TMenuItem;
    i : integer;
begin
    TRlog('BuildFileMenu');

    FileMenu.Clear;

    // 'File' / New Note
    m1 := TMenuItem.Create(FileMenu);
    m1.Tag := ord(mtNewNote);
    m1.Caption := rsMenuNewNote+ '(Ctrl-N)';
    m1.OnClick := @MainMenuClicked;
    m1.ImageIndex:=3;
    FileMenu.Add(m1);

    // Notebooks menu
    m1 := TMenuItem.Create(FileMenu);
    m1.Caption := rsNotebooks;
    m1.ImageIndex:=39;
    FileMenu.Add(m1);

    // Notebooks submenu
    m2 := TMenuItem.Create(m1);
    m2.Tag := ord(mtNewNoteBook);
    m2.Caption := rsMenuNewNotebook;
    m2.OnClick := @MainMenuClicked;
    m2.ImageIndex:=1;
    m1.Add(m2);

    if(NotebooksList.Count>0) then m1.AddSeparator;
    i:=0;
    while(i<NotebooksList.Count) do
    begin
         m2 := TMenuItem.Create(m1);
         m2.Tag := i;
         m2.Caption := 'New "'+NotebooksList.Strings[i] + '" Note';
         m2.OnClick := @MenuNewNotebookNote;
         m2.ImageIndex:=3;
         m1.Add(m2);
         inc(i);
    end;

    FileMenu.AddSeparator;

    // Delete note
    m1 := TMenuItem.Create(FileMenu);
    m1.Tag := ord(mtDeleteNote);
    if(length(SelectedNote)=36) then
    begin
        m1.Caption := rsMenuDeleteNote + ' ' + SelectedNote;
        m1.OnClick := @MainMenuClicked;
        m1.ImageIndex:=11;
    end else begin
        m1.Caption := rsMenuDeleteNote + '...';
        m1.Enabled:=false;
    end;
    FileMenu.Add(m1);

    // Delete notenook
    m1 := TMenuItem.Create(FileMenu);
    m1.Tag := ord(mtDeleteNotebook);
    if((length(SelectedNotebook)>0) and (CompareText(SelectedNotebook,'-')<>0)) then
    begin
         m1.Caption := rsMenuDeleteNotebook + ' "' + SelectedNotebook + '"';
         m1.OnClick := @MainMenuClicked;
         m1.ImageIndex:=12;
    end else begin
        m1.Caption := rsMenuDeleteNotebook + '...';
        m1.Enabled:=false;
    end;
    FileMenu.Add(m1);

    FileMenu.AddSeparator;

    // 'File' / Quit
    m1 := TMenuItem.Create(FileMenu);
    m1.Tag := ord(mtQuit);
    m1.ImageIndex:=7;
    m1.Caption := rsMenuQuit + '(Ctrl-Q)';
    m1.OnClick := @MainMenuClicked;
    FileMenu.Add(m1);
end;

procedure TFormMain.BuildTrayMenu(Sender : TObject);
var
    m1,m2 : TMenuItem;
    n : PNOteInfo;
    i : integer;
begin
   TRlog('BuildTrayMenu');

   if(not UseTrayIcon) then exit();


   TrayMenu.Items.Clear;
   TrayMenu.Images := MenuIconList;
   // New Note
   m1 := TMenuItem.Create(TrayMenu);
   m1.Tag := ord(ttNewNote);
   m1.Caption := rsTrayNewNote;
   m1.OnClick := @TrayMenuClicked;
   m1.ImageIndex:=3;
   TrayMenu.Items.Add(m1);

   // Search
   m1 := TMenuItem.Create(TrayMenu);
   m1.Tag := ord(ttSearch);
   m1.Caption := rsTraySearchNote;
   m1.ImageIndex:=5;
   m1.OnClick := @TrayMenuClicked;
   TrayMenu.Items.Add(m1);

   // Notebooks
   m1 := TMenuItem.Create(TrayMenu);
   m1.Caption := rsTrayNotebooks;
   m1.ImageIndex:=39;
   TrayMenu.Items.Add(m1);

   // List notebooks
   m2 := TMenuItem.Create(m1);
   m2.Caption := 'test NB';
   //m2.OnClick := nil;
   m1.Add(m2);

   // App menu
   m1 := TMenuItem.Create(TrayMenu);
   m1.Caption := rsMenuTools;
   TrayMenu.Items.Add(m1);

   // Sync
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttSync);
   m2.ImageIndex:=10;
   m2.Caption := rsTraySync;
   m2.OnClick := @TrayMenuClicked;
   m1.Add(m2);
   // Settings
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttSettings);
   m2.Caption := rsTraySettings;
   m2.ImageIndex:=8;
   m2.OnClick := @TrayMenuClicked;
   m1.Add(m2);
   // About
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttAbout);
   m2.Caption := rsTrayAbout;
   m2.OnClick := @TrayMenuClicked;
   m2.ImageIndex:=9;
   m1.Add(m2);
   // Quit
   m2 := TMenuItem.Create(m1);
   m2.Tag := ord(ttQuit);
   m2.ImageIndex:=7;
   m2.Caption := rsTrayQuit;
   m2.OnClick := @TrayMenuClicked;
   m1.Add(m2);

   TrayMenu.Items.AddSeparator;

   // Add latest notes
   i:=0;
   while(i<LastUsed.Count) do
   begin
      n := NotesList.FindID(LastUsed.Strings[i]);
      if(n<>nil) then
      begin
         m1 := TMenuItem.Create(TrayMenu);
         m1.Tag := PtrInt(n);
         m1.Caption := n^.Title;
         m1.ImageIndex:=6;
         m1.OnClick := @TrayNoteClicked;
         TrayMenu.Items.Add(m1);
      end;
      inc(i);
   end;
end;

procedure TFormMain.OpenNoteByTitle(t : UTF8String);
var
   i : integer;
   b : boolean;
begin
   TRlog('Searching Note by title : ' + t);

   t:=UTF8Trim(t);

   b := false;

   for i := 0 to NotesList.Count-1 do
      if (UTF8CompareText(UTF8Trim(NotesList.Items[i]^.Title),t) = 0)
      then begin
        OpenNote(NotesList.Items[i]^.ID);
        b := true;
      end;

   if(not b) then OpenNote('','',t);

end;

procedure TFormMain.MenuNewNotebookNote(Sender : TObject);
begin
   ShowMessage('Opening new note on notebook '+SelectedNotebook);
   OpenNote('',NotebooksList.Strings[TMenuItem(Sender).Tag]);
end;

procedure TFormMain.TrayNoteClicked(Sender : TObject);
var
    n : PNoteInfo;
begin

   TRlog('TrayNoteClicked');

   try
       n := PNoteInfo(TMenuItem(Sender).Tag);
       if(n<>nil) then
       begin
          AddLastUsed(n^.ID);
          OpenNote(n^.ID);
       end;
   except on E:Exception do TRlog(E.message);
   end;
end;

procedure TFormMain.ShowAbout();
begin
   if(assigned(fabout)) then begin fabout.Hide; fabout.Show; exit(); end;

   fabout := TFormAbout.Create(self);
   fabout.ShowModal;
   FreeAndNil(fabout);
end;

procedure TFormMain.ShowSettings();
begin
   if(assigned(fsett)) then begin fsett.Hide; fsett.Show; exit(); end;

   syncshallrun := false;
   fsett := TFormSettings.Create(self);
   fsett.ShowModal;
   FreeAndNil(fsett);
   syncshallrun := true;
end;

procedure TFormMain.SnapSync();
var
    FormSync : TFormSync;
begin
    if(not syncshallrun) then begin ShowMessage(rsOtherSyncProcess); exit(); end;
    syncshallrun := false;

    TRlog('DoSync');
    if(isSyncConfigured()) then
    begin
       try
          FormSync := TFormSync.Create(Self);
          FormSync.SyncVisible();
          ProcessSyncUpdates(FormSync.DeletedList, FormSync.DownloadList);
          FreeAndNil(FormSync);
          LastSync := now;
       except on E:Exception do TRlog(E.message);
       end;
    end
    else ShowMessage(rsSetupSyncFirst);
    syncshallrun := true;
end;

procedure TFormMain.TrayMenuClicked(Sender : TObject);
var
    sats : boolean;
begin

   TRlog('TrayMenuClicked');

   case TTrayTags(TMenuItem(Sender).Tag) of

        ttNewNote : if (NotesDir = '')
                  then ShowMessage(rsSetupNotesDirFirst)
                  else OpenNote();

        ttSettings: ShowSettings();

        ttSync : SnapSync();

        ttAbout : ShowAbout();

        ttSearch : begin sats:= SearchAtStart; SearchAtStart := true; Show(); SearchAtStart := sats; end;

        ttQuit : begin ConfigWrite('TrayMenu Quit'); Application.terminate; end;

   end;

   BuildTrayMenu(Self);

end;

{ ======= MAIN MENU ====== }

procedure TFormMain.MainMenuClicked(Sender : TObject);
var
    s : String;
    s2 : UTF8String;
begin

   TRlog('MainMenuClicked');

   case TMenuTags(TMenuItem(Sender).Tag) of

        mtNewNote : if (NotesDir = '')
            then ShowMessage(rsSetupNotesDirFirst)
            else OpenNote();

        mtNewNotebook : begin
            s:='';
            InputQuery(rsNotebooks,rsEnterNewNotebook ,s);
            s2 := Trim(CleanTitle(s));
            if(AddNewNotebook(s2)) then
            begin
               SelectedNotebook := s2;
               ShowLists(Sender);
               StatusBar1.SimpleText:= 'Notebook "'+SelectedNoteBook+'" selected'
            end;
          end;

        mtSettings: ShowSettings();

        mtSync : SnapSync();

        mtAbout : ShowAbout();

        mtQuit : begin ConfigWrite('MainMenu QUit'); Application.terminate; end;
   end;
end;

{ ======= LISTS MANAGEMENT ====== }

function TFormMain.AddLastUsed(ID : UTF8String; atend : boolean = false) : boolean;
var
   i : integer;
begin
   i:=0;

   if(atend) then
   begin
      while(i<LastUsed.Count) do
      begin
        if(CompareText(LastUsed.Strings[i],ID) = 0) then exit(false);
        inc(i);
      end;
      LastUsed.Add(ID);
      exit(true);
   end;

   while(i<LastUsed.Count) do
    begin
        if(CompareText(LastUsed.Strings[i],ID) = 0) then LastUsed.Delete(i)
        else inc(i);
    end;

   TrLog('Insert at first');
   LastUsed.Insert(0,ID);

   Result := true;
end;

function TFormMain.AddNewNotebook(nb : UTF8String): boolean;
var
   i : integer;
begin
   nb := Trim(nb);
   if(length(nb)<1) then exit(false);

   i:=0;
   while(i<NewNotebooksList.Count) do
    begin
        if(CompareText(NewNotebooksList.Strings[i],nb) = 0) then exit(false);
        inc(i);
    end;

   NewNotebooksList.Add(nb);
   AddNotebook(nb);

   Result:=true;
end;

function TFormMain.AddNotebook(nb : UTF8String): boolean;
var
   i : integer;
begin
   nb := Trim(nb);
   if((length(nb)=0) or (CompareText(nb,'-') =0)) then exit(false);

   i:=0;
   while(i<NotebooksList.Count) do
    begin
        if(CompareText(NotebooksList.Strings[i],nb) = 0) then exit(false);
        inc(i);
    end;

   NotebooksList.Add(nb);
   Result:=true;
end;

function TFormMain.DeleteNotebook(nb : UTF8String): boolean;
var
   i,j : integer;
   deleted : boolean;
   n : PNoteInfo;
   s : UTF8String;
begin
   nb := Trim(nb);
   if((length(nb)=0) or (CompareText(nb,'-') =0)) then exit(false);

   s:= rsConfirmDeleteNotebook1 + nb + sLineBreak + rsConfirmDeleteNotebook2;

   if(Application.MessageBox(PChar(s), 'Tomboy Reborn', MB_YESNO) <> IDYES)
   then exit(false);

   i:=0;
   while(i<NewNotebooksList.Count) do
   begin
      if(CompareText(NewNotebooksList.Strings[i],nb) = 0) then NewNotebooksList.Delete(i)
      else inc(i);
   end;

   deleted := false;
   i:=0;
   while(i<NotebooksList.Count) do
   begin
      if(CompareText(NotebooksList.Strings[i],nb) = 0) then begin NotebooksList.Delete(i); deleted:= true; end
      else inc(i);
   end;

   if(not deleted) then exit(false);

   for i := 0 to NotesList.Count - 1 do
   begin
      n := NotesList.Items[i];
      if(not NoteBelongs(nb,n)) then continue;

      j:=0;
      while(j<n^.Tags.Count) do
      begin
         if(CompareText('systen:notebook:'+nb,n^.Tags.Strings[j]) = 0) then n^.Tags.Delete(j)
         else inc(j);
      end;
      if(not NoteToFile(n,GetLocalNoteFile(n^.ID))) then deleted := false;;
   end;

   Result:=deleted;
end;

procedure TFormMain.PostScan();
begin
   Trlog('TFormMain.PostScan');

  if(assigned(ScanTimer)) then
  begin
     ScanTimer.Enabled := False;
     FreeAndNil(ScanTimer);
  end;

  ScanTimer := TTimer.Create(nil);
  ScanTimer.OnTimer := @ScanNotes;
  ScanTimer.Interval := 10;
  ScanTimer.Enabled := True;
end;

procedure TFormMain.ScanNotes(Sender : TObject);
Var
    ID : UTF8String;
    n,n2 : PNoteInfo;
    i,c1,c2 : integer;
    Info : TSearchRec;
    s : UTF8String;
    dt : TDateTime;
begin
   TRlog('ScanNotes');

   if(assigned(ScanTimer)) then
   begin
     ScanTimer.Enabled := False;
     FreeAndNil(ScanTimer);
   end;

   if(not syncshallrun) then
   begin
      ScanTimer := TTimer.Create(nil);
      ScanTimer.OnTimer := @ScanNotes;
      ScanTimer.Interval := 100;
      ScanTimer.Enabled := True;
   end;

   syncshallrun := false;

   NotebooksList.Clear;

   c1:=0; c2:=0;
   dt := now;

   if FindFirstUTF8(GetLocalNoteFile('*'), faAnyFile, Info)=0 then
   repeat
        ID := copy(Info.Name, 1, 36);
        n := NotesList.FindID(ID);
        if(n = nil) then
        begin
           TRlog('Scan note : New note ('+ID+')');
           n := EmptyNote();
           n^.ID := ID;
           NotesList.Add(n);
        end
        else n^.Tags.Clear;

        n2 := EmptyNote();
        CopyNote(n,n2);
        inc(c1);
        s := GetLocalNoteFile(ID);
        if(not FileToNote(s, n2)) then begin ShowMessage('Error reading '+ID +' : '+n2^.Error ); Dispose(n2); continue; end;
        CopyNote(n2,n);
        Dispose(n2);
        i:=0;
        while(i<n^.Tags.Count) do
        begin
            s := n^.Tags.Strings[i];
            if(Copy(s,1,16) = 'system:notebook:') then
            if(AddNotebook(Copy(s,17))) then inc(c2);
            inc(i);
        end;
        n^.Scan:= dt;
   until FindNext(Info) <> 0;
   FindClose(Info);

   i:=0;
   while(i<NotesList.Count)
   do begin
      if(NotesList.Items[i]^.Scan = dt)
      then inc(i)
      else begin
        n := NotesList.Items[i];
        NotesList.Delete(i);
        Dispose(n);
      end;
   end;

   i:=0;
   while(i<NewNotebooksList.Count) do
   begin
        AddNotebook(NewNotebooksList.Strings[i]);
        inc(i);
   end;

   // Cleanup LastUsed
   TRlog('Cleaning up last used');
   i:=0;
   while(i<LastUsed.Count) do
   begin
       if(NotesList.FindID(LastUsed.Strings[i]) = nil) then LastUsed.Delete(i)
       else inc(i);
   end;
   if(LastUsed.Count<Max(LastUsedNB,NotesList.Count)) then
   begin
      NotesList.Sort(@NoteTimeOrder);

      i:=0;
      while((i<NotesList.Count) and (LastUsed.Count<10)) do
      begin
          AddLastUsed(NotesList.Items[i]^.ID,true);
          inc(i);
      end;
   end;

   TRlog('ScanNotes : Found '+IntToStr(c1)+' local notes and '+IntToStr(c2) + ' notebooks');

   ShowLists(Self);

   StatusBar1.SimpleText:= 'Scanning : Found '+IntToStr(c1)+' local notes and '+IntToStr(c2) + ' notebooks';

   syncshallrun := true;

   if(Length(ToBeOpened)>0)
   then begin
      ID := ToBeOpened;
      ToBeOpened := '';
      OpenNote(ID);
   end;
end;

procedure TFormMain.ProcessSync(Sender : TObject);
var
  m : integer;
  FormSync : TFormSync;
begin
   if(Assigned(SyncTimer))
   then begin
     SyncTimer.Enabled := False;
     FreeAndNil(SyncTimer);
   end;

   TRlog('ProcessSync');

   if(not syncshallrun) then TRlog(rsOtherSyncProcess)
   else if(SyncRepeat>0) then
   begin
      m := Round((now-LastSync)*1440.0*60.0);
      TRlog('DElAY '+IntToStr(m)+' / '+IntToStr(SyncRepeat*60));

      if(m<SyncRepeat*60)
      then TRlog('Should process sync every '+IntToStr(SyncRepeat*60)+' secs (now waiting for '+IntToStr(m)+' secs)')
      else begin
         // DO SYNC
         syncshallrun := false;
         TRlog('Starting background Sync');
         if(isSyncConfigured()) then
         begin
           try
              FormSync := TFormSync.Create(Self);
              FormSync.SyncHidden();
              ProcessSyncUpdates(FormSync.DeletedList, FormSync.DownloadList);
              FreeAndNil(FormSync);
           except on E:Exception do TRlog(E.message);
           end;
         end
         else ShowMessage(rsSetupSyncFirst);
         LastSync := now;
         syncshallrun := true;
      end;
   end
   else TRlog('AutoSync not enabled');

   SyncTimer := TTimer.Create(nil);
   SyncTimer.OnTimer := @ProcessSync;
   SyncTimer.Interval := 9863;
   SyncTimer.Enabled := True;

end;

procedure TFormMain.ProcessSyncUpdates(const DeletedList, DownList : TStringList);
var
    i : integer;
    n : PNoteInfo;
    changes : boolean;
begin
   TRlog('ProcessSyncUpdates');

   changes := false;
   i:=0;
   while(i<DeletedList.Count) do
    begin
        n := NotesList.FindID(DeletedList.Strings[i]);
        if(n<>nil) then
        begin
           if(n^.Display <> nil) then
           begin
                n^.Display^.Close;
                FreeAndNil(n^.Display);
           end;
           NotesList.Remove(n);
           Dispose(n);
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
        end
        else begin
           if(n^.Display <> nil) then
           begin
                n^.Display^.Close;
                FreeAndNil(n^.Display);
           end;
           FileToNote(GetLocalNoteFile(n^.ID),n);
           NotesList.Add(n);
        end;
        changes := true;
        inc(i);
    end;

   if(changes) then ShowLists(Self);
end;

{ ====== UI MANAGEMENT ====== }

procedure TFormMain.SGNotebooksPrepareCanvas(sender: TObject; aCol,
    aRow: Integer; aState: TGridDrawState);
begin
   //TRlog('SGNotebooksPrepareCanvas');

   if ((CompareText(SGNotebooks.Cells[2,aRow],SelectedNotebook) = 0) and (aRow>0))
   then begin
     SGNotebooks.canvas.brush.color := clOlive;
     SGNotebooks.canvas.Font.Color:= clWhite;
   end;


end;

procedure TFormMain.SGNotesPrepareCanvas(sender: TObject; aCol,
    aRow: Integer; aState: TGridDrawState);
begin
   //TRlog('SGNotesPrepareCanvas SelectedNote='+SelectedNote+' SGNITES='+SGNotes.Cells[2,aRow]);

   if (length(SelectedNote) > 0) and (CompareText(SGNotes.Cells[2,aRow],SelectedNote) = 0)
   then begin
     SGNotes.canvas.brush.color := clTeal;
     SGNotes.canvas.Font.Color:= clWhite;
   end;

end;

procedure TFormMain.SGNotebooksResize(Sender: TObject);
begin
    SGNotebooks.Columns[0].Width := 16;
    SGNotebooks.Columns[1].Width := SGNotebooks.width - 17;
end;


function TFormMain.DeleteNote(ID: UTF8String) : boolean;
var
    s,d : UTF8String;
    n : PNoteInfo;
begin

   s:= rsConfirmDeleteNote + ID;
   if(Application.MessageBox(PChar(s), 'Tomboy Reborn', MB_YESNO) = IDYES)
   then begin



   if(length(ID)<>36) then exit(false);

   s:= GetLocalNoteFile(ID);
   d:= GetLocalNoteFile(ID,GetLocalBackupPath());

   if FileExistsUTF8(s) then
   begin
      ForceDirectoriesUTF8(GetLocalBackupPath());

      if CopyFile(s,d)
      then DeleteFileUTF8(s)
      else begin
         s:= 'Failed to backup file '+ s + ' to Backup ' + d;
         TRlog(s);
         ShowMessage(s);
         exit(false);
      end;

      n := NotesList.FindID(ID);
      if(n<>nil) then
      begin
         if(n^.Display <> nil) then
         begin
            n^.Display^.Close;
            FreeAndNil(n^.Display);
         end;
         NotesList.Remove(n);
         Dispose(n);
      end;

      exit(true);
   end;


   end;
   Result := false;
end;

function TFormMain.SaveNote(ID: UTF8String) : boolean;
var
    s,d : UTF8String;
    n : PNoteInfo;
begin

   if(length(ID)<>36) then exit(false);

   s:= GetLocalNoteFile(ID);
   d:= GetLocalNoteFile(ID,GetLocalBackupPath());

   n := NotesList.FindID(ID);
   if(n = nil) then exit(false);

   if FileExistsUTF8(s) then
   begin
      ForceDirectoriesUTF8(GetLocalBackupPath());

      if CopyFile(s,d)
      then begin
         if(NoteToFile(n,s)) then exit(true);
         s:= 'Failed to save Note '+ ID + ' to ' + ID;
         TRlog(s);
         ShowMessage(s);
      end
      else begin
         s:= 'Failed to backup file '+ s + ' to Backup ' + d;
         TRlog(s);
         ShowMessage(s);
      end;
   end;
   Result := false;
end;


procedure TFormMain.ShowLists(sender: TObject);
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

   i:=0;
   while(i<NewNotebooksList.Count) do
   begin
        AddNotebook(NewNotebooksList.Strings[i]);
        inc(i);
   end;

   // Show notebooks
   s:='';
   while SGNotebooks.RowCount > 1 do SGNotebooks.DeleteRow(SGNotebooks.RowCount-1);
   SGNotebooks.InsertRowWithValues(SGNotebooks.RowCount,['',rsAnyNotebook,'']);
   SGNotebooks.InsertRowWithValues(SGNotebooks.RowCount,['',rsNoNotebook,'-']);
   for i := 0 to NotebooksList.Count - 1 do
   begin
        SGNotebooks.InsertRowWithValues(SGNotebooks.RowCount, ['',NotebooksList.Strings[i],NotebooksList.Strings[i]]);
        if(CompareText(NotebooksList.Strings[i],SelectedNoteBook)=0) then s:=SelectedNoteBook;
   end;
   if(CompareText(SelectedNoteBook,'-')<>0) then SelectedNotebook := s;

   // Show notes
   s:='';
   while SGNotes.RowCount > 1 do SGNotes.DeleteRow(SGNotes.RowCount-1);
   for i := 0 to NotesList.Count - 1 do
   begin
       n := NotesList.Items[i];
       if(not NoteBelongs(SelectedNotebook,n)) then continue;
       if(not NoteContains(sl,n,SearchCaseSensitive)) then continue;

       if(CompareText(n^.ID,SelectedNote)=0) then s:=SelectedNote;

       SGNotes.InsertRowWithValues(SGNotes.RowCount, [n^.Title,GetDisplayTimeFromGMT(n^.LastChangeGMT),n^.ID]);
   end;
   SelectedNote:=s;
   SGNotes.AutoSizeColumn(1);
   SGNotes.columns[1].Width := SGNotes.columns[1].Width +10;

   if(not FirstList) then
   begin
     SGNotes.SortOrder := soDescending;
     SGNotes.SortColRow(true, 1, SGNotes.FixedRows, SGNotes.RowCount-1);
   end;
   BuildFileMenu(Self);

   sl.Free;
end;

procedure TFormMain.SGNotesDblClick(Sender: TObject);
var
   r : integer;
   n : PNoteInfo;
   ID : UTF8String;
begin
   r := SGNotes.Row;

   if((r>0) and (MilliSecondsBetween(LastClick,now)<300) and (r = LastClickedRow)) then
   begin
     ID := SGNotes.Cells[2, r];
     n := NotesList.FindID(ID);
     if(n<>nil) then
     begin
          AddLastUsed(ID);
          OpenNote(ID);
     end;
   end;
end;

procedure TFormMain.SGNotebooksDrawCell(Sender: TObject; ACol, ARow: Integer; Rect: TRect; State: TGridDrawState);
var
   bm : TBitmap;
begin
   if((ACol = 0) and (ARow>0)) then
   begin
     // Put icon
     bm := TBitmap.Create;
     MenuIconList.GetBitmap(9, bm);
     SGNotebooks.Canvas.Draw(floor((Rect.Left+Rect.Right)/2-8), floor((Rect.Top+Rect.Bottom)/2-8),bm);
     bm.Free;
   end;
end;

procedure TFormMain.SGNotebooksClick(Sender: TObject);
begin
   TRlog('SGNotebooksClick on row '+IntToStr(SGNotebooks.Row));

   if(assigned(ShowTimer)) then
   begin
      ShowTimer.Enabled := False;
      FreeAndNil(ShowTimer);
   end;

   ShowTimer := TTimer.Create(nil);
   ShowTimer.OnTimer := @ShowLists;
   ShowTimer.Interval := 100;
   ShowTimer.Enabled := True;

   SelectedNoteBook := SGNotebooks.Cells[2,SGNotebooks.Row];

   BuildFileMenu(Self);

   if(length(SelectedNoteBook)=0)
   then StatusBar1.SimpleText := 'Notes with any notebook(s) selected'
   else if SelectedNoteBook = '-'
   then StatusBar1.SimpleText := 'Notes without notebook selected'
   else StatusBar1.SimpleText := 'All notebooks selected';

   TRlog('SGNotebooksClick NB = '+SelectedNotebook);
end;

procedure TFormMain.SearchBoxChange(Sender: TObject);
begin
   TRlog('SearchBoxChange');

   if(assigned(ShowTimer)) then
    begin
      ShowTimer.Enabled := False;
      FreeAndNil(ShowTimer);
    end;

   ShowTimer:= TTimer.Create(nil);
   ShowTimer.OnTimer := @ShowLists;
   ShowTimer.Interval := 1000;
   ShowTimer.Enabled := True;
end;

procedure TFormMain.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
   TRlog('FormCloseQuery');
   if(UseTrayIcon) then
   begin
     TrLog('FormCloseQuery NIET');
     Hide();
     CanClose := false;
   end;

end;

procedure TFormMain.FormDestroy(Sender: TObject);
begin
  TRlog('FormDestroy');

   if(assigned(ScanTimer)) then
   begin
     ScanTimer.Enabled := False;
     FreeAndNil(ScanTimer);
   end;
   if(assigned(SyncTimer)) then
   begin
     SyncTimer.Enabled := False;
     FreeAndNil(SyncTimer);
   end;
   if(assigned(ShowTimer)) then
   begin
     ShowTimer.Enabled := False;
     FreeAndNil(ShowTimer);
   end;
  FreeAndNil(SGNotebooks);
  FreeAndNil(SGNotes);
  FreeAndNil(NotesList);
  FreeAndNil(NotebooksList);
  FreeAndNil(NewNotebooksList);
end;

procedure TFormMain.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
   ID : UTF8String;
   n : PNoteInfo;
begin
  TRlog('KeyDown '+IntToStr(Key));

  // CTRL
  if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then
  begin

     // New Note
     if key = ord('N') then begin TrLog('Ctrl-N'); OpenNote(); Key := 0; exit(); end;

     // Quit
     if key = ord('Q') then begin TRlog('Ctrl-Q'); ConfigWrite('Ctrl-Q Quit'); Key := 0; Application.terminate; exit(); end;

     // Sync
     if key = ord('S') then
     begin
        TRlog('Ctrl-S');
        SnapSync();
        exit();
     end;

     // Settings
     if key = ord('O') then ShowSettings();

     exit();
   end;

  // SHIFT
  if ssShift in Shift then
  begin
    if(DeleteNotebook(SelectedNotebook)) then Key := 0;
    exit();
  end;

  // OTHER
  if((Key = VK_DELETE) and (Sender<>SearchBox)) then
  begin
    if(DeleteNote(SelectedNote)) then Key := 0;
    exit();
  end;

  if ((Key = VK_RETURN) and (SGNotes.Row>1))
  then begin
     ID := SGNotes.Cells[2, SGNotes.Row];
     n := NotesList.FindID(ID);
     if(n<>nil) then
     begin
          AddLastUsed(ID);
          OpenNote(ID);
     end;
  end;

end;

procedure TFormMain.FormShow(Sender: TObject);
begin

  TRlog('FormShow with searchatstart '+BoolToStr(SearchAtStart));

  Left := random(250)+50;
  Top := random(250)+50;

  ShowLists(Self);
  BuildFileMenu(Self);
  BuildTrayMenu(Self);

  if(not SearchAtStart) then self.Hide();

end;

procedure TFormMain.CheckCaseSensitiveChange(Sender: TObject);
begin
    SearchCaseSensitive := CheckCaseSensitive.Checked;

    if(assigned(ShowTimer)) then
    begin
      ShowTimer.Enabled := False;
      FreeAndNil(ShowTimer);
    end;
    ShowTimer:= TTimer.Create(nil);
    ShowTimer.OnTimer := @ShowLists;
    ShowTimer.Interval := 100;
    ShowTimer.Enabled := True;

end;


procedure TFormMain.OpenNote(ID : UTF8String = ''; Notebook : UTF8String = '' ; Title : UTF8String = '');
var
    EBox : PNoteEditForm;
    n : PNoteInfo;
begin
   TRlog('OpenNote('+ID+' , '+Notebook+' )');

   n := NotesList.FindID(ID);

   TRlog('Done searching');

   Title := Trim(Title);

   TRlog('Done cleaning title');

   if(n = nil) then
   begin
      TRlog('Creating new note (old ID= '+ID+' )');
      n := EmptyNote();
      TRlog('Creating new note (new ID= '+n^.ID+' )');

      if((length(Notebook)>0) and (CompareText(Notebook,'-')<>0)) then n^.Tags.Add('system:notebook:'+Notebook);
      NotesList.Add(n);
   end;

   TRlog('Setting title');

   if(Length(Title)>0) then n^.Title := Title;

   TRlog('Testing TNoteEditForm');

   if(n^.Display <> nil) then
   begin
      TRlog('Closing TNoteEditForm');
      EBox := PNoteEditForm(n^.Display);
      EBox^.Hide();
      EBox^.Close;
      FreeAndNil(n^.Display);
   end;

   new(EBox);

   TRlog('Creating TNoteEditForm yo');

   Ebox^ := TFormNote.Create(Self);

   TRlog('Assigning data ');
   n^.Display := Ebox;
   EBox^.note := n;

   TRlog('Showing FormEdit');


   EBox^.Show();
end;


procedure TFormMain.SGNotesClick(Sender: TObject);
begin
   TRlog('SGNotesClick on row '+IntToStr(SGNotes.Row));
   if(SGNotes.Row<1)
   then SelectedNote := ''
   else begin
      SelectedNote := SGNotes.Cells[2,SGNotes.Row];
      LastClick := now;
      LastClickedRow := SGNotes.Row;
   end;

   SGNotes.Refresh;

   BuildFileMenu(Self);

   StatusBar1.SimpleText:= 'Note "'+SelectedNote+'" selected';

   TRlog('SGNotesClick ID = '+SelectedNote);
end;

procedure TFormMain.SGNotesResize(Sender: TObject);
begin
    SGNotes.Columns[0].Width := SGNotes.Width - SGNotes.Columns[1].Width; // -15;
end;

end.

