unit TRsync;
{   A Unit to manage Tomboy Reborn sync behaviour. }


{$mode objfpc}{$H+}


interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    ExtCtrls, StdCtrls, Grids, LazFileUtils,
    LazLogger,  LCLType, Buttons, ComCtrls,
    TRcommon, TRtexts, TRtransport, TRsyncClashUI;

type

{ TFormSync }

 TFormSync = class(TForm)

     LabelTitle: TLabel;
     LabelStats: TLabel;
     SettingsCancel: TSpeedButton;
     SettingsOK: TSpeedButton;

     StringGridReport: TStringGrid;


     procedure FormShow(Sender: TObject);
     procedure FormHide(Sender: TObject);
     procedure SettingsCancelClick(Sender: TObject);
     procedure SettingsOKClick(Sender: TObject);



     procedure FormCreate(Sender: TObject);
     procedure FormDestroy(Sender: TObject);

     procedure SyncVisible();
     procedure SyncHidden();

public
     ErrorString : String;

private

    UpNew, UpEdit, DownNew, DownEdit, DelLoc, DelRem, CreateCopy, DoNothing, Undecided : integer;

    LocalTimer : TTimer;
    Transport : TTomboyTrans;

    NoteMetaData : TNoteInfoList;
    LocalMetaData : TNoteInfoList;

    { Default action to take in case of clash }
    ClashAction : TSyncAction;

    { May return : SyncXMLError, SyncNoRemoteDir, SyncNoRemoteWrite,
              SyncNoRemoteRepo, SyncBadRemote, SyncMismatch. Checks if the connecton
              looks viable, either (fileSync) it has right files there and write access
              OR (NetSync) network answers somehow (?). Reads local manifest if
              RepoAction=RepoUse and compares ts serverID with one found by
              Trans.testConnection. SyncReady means we can proceed to StartSync, else must
              do something first, setup new connect, consult user etc.}
    function TestConnection() : TSyncStatus;

    // Load memory and sort out the job to be done
    procedure PrepareSync(Sender : TObject);
    // Execute the job to be done
    function DoSync() : boolean;


    function GetNoteTitle(const ID : ANSIString) : ANSIString;

    { Stats }
    procedure SyncSummary();

    { we will pass address of this function to Sync }
    function ResolveClashUI(const ClashRec : TClashRecord) : TSyncAction;

    function getManifestName() : String;

    {   Asks Transport for a list of the notes remote server knows about. }
    function LoadRemoteNotes() : boolean;

    {   Load local notes in memory }
    function LoadLocalNotes() : boolean;

    { Find Remotes notes not in local repo }
    procedure FindNewRemoteNotes();

    { Find Local notes not in remote repo }
    procedure FindNewLocalNotes();

    { Find Server notes that have been deleted locally }
    procedure FindDeletedLocalNotes();

    { Find Local notes that have been deleted remotely }
    procedure FindDeletedServerNotes();

    { Set actions to be done that are systemic }
    procedure SetSystemicActions();

    { Searches list for any clashes, refering each one to user. Done after
        list is filled out in case we want to ask user for general instrucions }
    function ProcessClashes() : boolean;

    { Looks at a clash and determines if its an up or down depending on
        possible higher order TSyncActions such as newer, older }
    function ResolveAction(const SNote, LNote : PNoteInfo) : TSyncAction;

    { Returns true if the passed dates are pretty close, see code for just how close }
    function DatesCompare(const DS1, DS2: TDateTime): double;

    { We call transport for all the notes in the list we need download.
        Transport does most of the work. In TestMode, does nothing }
    function DoDownloads(): boolean;

    { Backs up and then removes Notes marked as SyDeleteLocal }
    function DoDeleteLocal(): boolean;

    { Select and call Transport to push changes }
    function PushChanges(): boolean;

    { We write a remote manifest out localy if we have any uploads or to handle
              the delete from server a note that was deleted locally to do. Then, if
              TestMode is false, call Transport to deal with it. Writing it locally is fast
              and we get to check for and isolate any data errors. Initially
              written to $CONFIG/manifest.xml-remote and copied (moved ?).}
    function DoManifest(): boolean;

end;


implementation

{$R *.lfm}

uses laz2_DOM, laz2_XMLRead, TRtransportFile, TRtransportNC;


procedure TFormSync.FormCreate(Sender: TObject);
begin
  TRlog('FormSync Create');

  NoteMetaData := TNoteInfoList.Create;
  NoteMetaData.LName := 'Remote list';

  LocalMetaData := TNoteInfoList.Create;
  LocalMetaData.LName := 'Local list';

  Transport := nil;
end;


procedure TFormSync.FormDestroy(Sender: TObject);
begin
   TRlog('FormSync Destroy');

   FreeandNil(LocalMetaData);
   FreeandNil(NoteMetaData);
   FreeandNil(Transport);
   FreeAndNil(LocalTimer);
end;

function TFormSync.getManifestName() : String;
begin
    TRlog('GetManifestName');

    Result := Format('%s%s-%s-manifest.xml',[NotesDir, Transport.getPrefix() , Transport.ServerID ]);
end;


function TFormSync.GetNoteTitle(const ID : ANSIString) : ANSIString;
var
    note : PNoteInfo;
begin
   TRlog('GetNoteTitle');

    Result := '--Error--';

    note := NoteMetaData.FindID(ID);
    if(note <> nil) then exit(note^.Title);

    note := LocalMetaData.FindID(ID);
    if(note <> nil) then exit(note^.Title);
end;


{ =====================   D A T A    C H E C K I N G    M E T H O D S  ============= }

function TFormSync.DatesCompare(const DS1, DS2 : TDateTime) : double;
var
    Margin : TDateTime = 0.00001;      // a second

begin
    TRlog('DatesCompare S1='+DateToStr(DS1)+' vs S2='+DateToStr(DS2));

    if (DS1 > DS2 + Margin) then exit(DS1-DS2);
    if (DS2 > DS1 + Margin) then exit(DS1-DS2);

    Result := 0;
end;


procedure TFormSync.FindDeletedServerNotes();
var
    PNote,LNote : PNoteInfo;
    ID : String;
    i,c : integer;
begin
   TRlog('FindDeletedServerNotes');

   c := 0;
    for i := 0 to LocalMetaData.Count -1 do
    begin
        LNote := LocalMetaData.Items[i];
        if(LNote^.Deleted) then continue; // Do not consider already deleted
        if(LNote^.Rev<0) then continue; // Do not consider never sync

        ID := LNote^.ID;
        PNote := NoteMetaData.FindID(ID);
        if(PNote <> Nil) then continue;

        new(PNote);
        CopyNote(LNote, PNote);

        if(DatesCompare(LNote^.LastChangeGMT,LNote^.LastSyncGMT)<0)
        then begin
            PNote^.Action:=SynDeleteLocal;
            inc(c);
        end
        else PNote^.Action:=SynUploadEdit;

        LNote^.Action:=PNote^.Action;
        NoteMetaData.Add(PNote);
    end;
    TRlog('Found '+IntToStr(c)+' deleted server notes');
end;

procedure TFormSync.FindDeletedLocalNotes();
var
    LNote,PNote : PNoteInfo;
    c,i : integer;
begin
   TRlog('FindDeletedLocalNotes');

    c:=0;
    for i := 0 to LocalMetaData.Count -1 do
    begin
        LNote := LocalMetaData.Items[i];
        if(LNote^.Action <> SynUnset) then continue; // Don't look at notes already scrutinized
        if(not LNote^.Deleted) then continue; // Consider only deleted

        PNote := NoteMetaData.FindID(LNote^.ID);
        if(PNote = Nil) then continue;
        if(PNote^.Action = SynClash) then continue;

        if((DatesCompare(PNote^.LastMetaChangeGMT,LNote^.LastSyncGMT)>0) or (DatesCompare(PNote^.LastChangeGMT,LNote^.LastSyncGMT)>0))
        then PNote^.Action := SynClash
        else begin
            inc(c);
            PNote^.Action:=SynDeleteRemote;
        end;
        LNote^.Action:=PNote^.Action;
    end;
    TRlog('Found '+IntToStr(c)+' deleted local notes');
end;

procedure TFormSync.FindNewLocalNotes();
var
    ID : String;
    c,i : integer;
    PNote, LNote : PNoteInfo;
begin
   TRlog('FindNewLocalNotes');

   c :=0;
   for i := 0 to LocalMetaData.Count -1 do
   begin
      LNote := LocalMetaData.Items[i];
      if(LNote^.Action <> SynUnset) then continue; // Don't look at notes already scrutinized
      if(LNote^.Deleted) then continue; // Do not consider deleted

      ID := LNote^.ID;
      PNote := NoteMetaData.FindID(ID);
      if(PNote <> Nil) then continue;

      TRlog('Found one ! '+ID);

      new(PNote);
      CopyNote(LNote, PNote);
      PNote^.Action:=SynUploadNew;
      LNote^.Action:=PNote^.Action;

      NoteMetaData.Add(PNote);
      inc(c);
   end;
   TRlog('Found '+IntToStr(c)+' new local notes');
end;

procedure TFormSync.FindNewRemoteNotes();
var
    ID : String;
    c,i : integer;
    PNote,LNote : PNoteInfo;
begin
    TRlog('FindNewRemoteNotes');

    c := 0;
    for i := 0 to NoteMetaData.Count -1 do
    begin
        PNote := NoteMetaData.Items[i];
        if(PNote^.Action <> SynUnset) then continue; // Don't look at notes already scrutinized

        ID := PNote^.ID;
        LNote := LocalMetaData.FindID(ID);
        if(LNote <> Nil) then continue;

        PNote^.Action := SynDownLoadNew;
        inc(c);
    end;
    TRlog('Found '+IntToStr(c)+' new remote notes');
end;

procedure TFormSync.SetSystemicActions();
var
    ID : String;
    i : integer;
    LNote, SNote : PNoteInfo;
begin
    TRlog('SetSystemicActions');

    for i := 0 to NoteMetaData.Count -1 do
    begin
        SNote := NoteMetaData.Items[i];

        if(SNote^.Action <> SynUnset) then continue; // Don't look at notes already scrutinized
        TRlog('SYSTEMIC '+ SNote^.ID + ' APPLYING RULES' );

        ID := SNote^.ID;
        LNote := LocalMetaData.FindID(ID); // Must be not null , otherwise, we have a bug in previous scan

        if((DatesCompare(LNote^.LastChangeGMT,SNote^.LastChangeGMT) = 0 ) and (DatesCompare(LNote^.LastMetaChangeGMT, SNote^.LastMetaChangeGMT) = 0 )) then
        begin
           SNote^.Action := SynNothing;
           continue;
        end;

        if((DatesCompare(LNote^.LastChangeGMT,SNote^.LastChangeGMT)>=0 ) and (DatesCompare(LNote^.LastMetaChangeGMT, SNote^.LastMetaChangeGMT)>=0 ))
        then begin
           SNote^.Action := SynUploadEdit;
           continue;
        end;

        if((DatesCompare(LNote^.LastChangeGMT,SNote^.LastChangeGMT)<=0 ) and (DatesCompare(LNote^.LastMetaChangeGMT, SNote^.LastMetaChangeGMT)<=0 ))
        then begin
           SNote^.Action := SynDownloadEdit;
           continue;
        end;

        SNote^.Action := SynClash;

        if(LNote^.Rev <> SNote^.Rev) then
        begin
            SNote^.Error := rsSyncRevisionError;
            continue;
        end;

        SNote^.Error := rsSyncChangeError;
    end;
end;


function TFormSync.ResolveAction(const SNote, LNote : PNoteInfo) : TSyncAction;
begin
    TRlog('ResolveAction');

    Result := ClashAction;

    case ClashAction of
        SynAllLocal : exit(SynUpLoadEdit);
        SynAllRemote : exit(SynDownLoadEdit);
        SynAllCopy : exit(SynCopy);

        SynAllNewest : if (DatesCompare(LNote^.LastChangeGMT,SNote^.LastChangeGMT)>0)
                     then exit(SynUploadEdit)
                     else exit(SynDownLoadEdit);

        SynAllOldest : if (DatesCompare(LNote^.LastChangeGMT,SNote^.LastChangeGMT)<0)
                     then exit(SynUploadEdit)
                     else exit(SynDownLoadEdit);

        SynDownLoadEdit : ClashAction := SynUnset;
        SynUpLoadEdit : ClashAction := SynUnset;
    end;
end;


function TFormSync.ProcessClashes() : boolean;
var
    ClashRec : TClashRecord;
    i,c : integer;
    remote,local : PNoteInfo;
begin
    TRlog('ProcessClashes');

    ClashAction := SynUnset;

    case SyncClashOption of
         TSyncClashOption.UseLocal : ClashAction := SynAllLocal;
         TSyncClashOption.UseServer : ClashAction := SynAllRemote;
         TSyncClashOption.MakeCopy : ClashAction := SynAllCopy;
    end;

    c:=0;
    for i := 0 to NoteMetaData.Count -1 do
    begin
        remote := NoteMetaData.Items[i];
        if remote^.Action = SynUnset then
           begin
               ErrorString := rsSyncCoreError;
               exit(false);
           end;

        if remote^.Action = SynClash then
        begin
           local := LocalMetaData.FindID(remote^.ID);
           if ClashAction = SynUnset then
           begin
                ClashRec.RemoteNote := remote;
                ClashRec.LocalNote := local;
                ClashAction := ResolveClashUI(Clashrec);
           end;
           remote^.Action := ResolveAction(remote, local);
           if(remote^.Action = SynUnset) then
              begin
                 ErrorString := rsSyncClashCanceled;
                 exit(false);
              end;
           inc(c);
        end;
    end;
    TRlog('Managed '+IntToStr(c)+' clashes');

    // Process needs for copy
    c:=0;
    for i := 0 to NoteMetaData.Count-1 do
    begin
        remote := NoteMetaData.Items[i];
        if remote^.Action <> SynCopy then continue;

        new(local);
        CopyNote(remote,local);

        local^.Title := local^.Title + ' (Server Rev=' + IntToStr(Transport.ServerRev) +' )';
        local^.ID := GetNewID();
        local^.Action := SynDownloadNew;

        NoteMetaData.Add(local);

        remote^.Action := SynUploadEdit;
        inc(c);
    end;
    TRlog('Managed '+IntToStr(c)+' copies');

    Result:= true;
end;


{ ========================  N O T E   M O V E M E N T    M E T H O D S ================}

function TFormSync.DoDownloads() : boolean;
var
    i : integer;
    dest,backupdir,backup,ID : String;
    f : TextFile;
    note : PNoteInfo;
begin
    TRlog('DoDownloads');

    backupdir := NotesDir + 'Backup' + PathDelim;

    if not DirectoryExistsUTF8(backupdir) then
        if not ForceDirectoriesUTF8(backupdir) then
        begin
            ErrorString := 'Failed to create Backup directory.';
            exit(False);
        end;

    for i := 0 to NoteMetaData.Count-1 do
    begin
        note := NoteMetaData.Items[i];
        if (note^.Action <> SynDownLoadEdit) and (note^.Action <> SynDownLoadNew) then continue;
        ID := note^.ID;

        dest := GetLocalNoteFile(ID);
        backup := GetLocalBackupPath();
        if FileExistsUTF8(dest) then
        begin
           ForceDirectoriesUTF8(backup);
           if not CopyFile(dest, GetLocalNoteFile(ID, backup)) then
                begin
                    ErrorString := 'Failed to copy file '+ dest + ' to Backup ' + backup;
                    TRlog(ErrorString);
                    exit(False);
                end;
        end;
        NoteToFile(note,dest);
    end;

    result := True;
end;


function TFormSync.DoDeleteLocal() : boolean;
var
    I : integer;
    s,d,ID : String;
begin
    TRlog('DoDeleteLocal');

    for I := 0 to NoteMetaData.Count -1 do
    begin
        if NoteMetaData.Items[i]^.Action <> SynDeleteLocal then continue;
        ID := NoteMetaData.Items[i]^.ID;

        s:= GetLocalNoteFile(ID);
        d:= GetLocalNoteFile(ID,GetLocalBackupPath());

        if FileExistsUTF8(s) then
        begin
            ForceDirectoriesUTF8(GetLocalBackupPath());

            if CopyFile(s,d)
            then DeleteFileUTF8(s)
            else begin
               ErrorString := 'Failed to backup file '+ s + ' to Backup ' + d;
               TRlog(ErrorString);
               exit(False);
            end;
        end;
    end;
    result := true;
end;


function TFormSync.PushChanges() : boolean;
var
    notes : TNoteInfoList;
    ID : String;
    i : integer;
    n,l : PNoteInfo;
begin
    TRlog('PushChanges . ServerRev = ' + inttostr(Transport.ServerRev));

    notes := TNoteInfoList.Create;
    notes.LName := 'Push changes';

    for i := 0 to NoteMetaData.Count -1 do
    begin

       n := NoteMetaData.Items[i];

       if((n^.Action  = SynUploadNew) or (n^.Action  = SynUploadEdit)) then
          begin
             l := LocalMetaData.FindID(n^.ID);
             new(n);
             CopyNote(l,n);
             n^.Action := NoteMetaData.Items[i]^.Action;
             notes.Add(n);
          end
       else if n^.Action = SynDeleteRemote then
          begin
              new(n);
              CopyNote(NoteMetaData.Items[i],n);
              notes.Add(n)
          end;
    end;

    Result:= true;

    if(notes.Count > 0) then
    begin
         Result := Transport.PushChanges(notes);
         ErrorString := Transport.ErrorString;
    end;

    FreeAndNil(notes);
end;


function TFormSync.DoManifest(): boolean;
var
    OutFile: TStringList;
    Index : integer;
    r : string;
    n,l : PNoteInfo;
    ok : boolean;
begin
    TRlog('DoManifest');

    ok := true;

    OutFile := TStringList.Create;
    OutFile.Add('<?xml version="1.0" encoding="utf-8"?>');
    OutFile.Add('<sync revision="' + IntToStr(Transport.ServerRev) + '" server-id="' + Transport.ServerID + '">');

    for Index := 0 to NoteMetaData.Count - 1 do
    begin
        n := NoteMetaData[Index];

        if not (n^.Action in [SynUploadNew, SynUpLoadEdit, SynDownLoadNew, SynDownLoadEdit, SynNothing]) then
           begin
               TRlog('Skipping '+n^.ID + ' because Action=' + SyncActionName(n^.Action));
               continue;
           end;

        if n^.Action in [SynUploadNew, SynUpLoadEdit, SynDownLoadNew, SynDownLoadEdit]
        then r := IntToStr(Transport.ServerRev)
        else begin
           l := LocalMetaData.FindID(n^.ID);
           r := IntToStr(l^.Rev);
        end;

        OutFile.Add('  <note guid="' + n^.ID + '" last-revision="' + r + '" last-sync-date="' + GetCurrentTimeStr() + '" />');
    end;

    OutFile.Add('</sync>');

    OutFile.LineBreak := sLineBreak;

    if(not Transport.DoRemoteManifest(OutFile)) then ok := false;

    if(ok) then
    try
         OutFile.SaveToFile(getManifestName());


    except on E:Exception do begin TRlog(E.message); ok := false; end;
    end;

    OutFile.Free;

    Result := ok;
end;


{ =================  S T A R T   U P   M E T H O D S ============== }


function TFormSync.TestConnection(): TSyncStatus;
var
    res : TSyncStatus;
begin
    TRlog('TestConnection');

    Transport.ServerRev:=-1;

    res := Transport.TestTransport();
    if res <> SyncReady
    then begin
         ErrorString := Transport.ErrorString;
         exit;
    end;

    Result := res;
end;

function TFormSync.LoadLocalNotes(): boolean;
var
   ID,s : String;
   c,j : integer;
   Info : TSearchRec;
   PNote : PNoteInfo;
   Doc : TXMLDocument;
   manifest : String;
   NodeList : TDOMNodeList;
   Node : TDOMNode;
begin
    TRlog('LoadLocalNotes ' + GetLocalNoteFile('*'));

    LocalMetaData.Clear;
    c :=0;
    if FindFirstUTF8(GetLocalNoteFile('*'), faAnyFile, Info)=0 then
    repeat
        ID := copy(Info.Name, 1, 36);
        new(PNote);
        PNote^.Action:=SynUnset;
        PNote^.ID := ID;
        PNote^.Rev := -1;
        PNote^.LastSyncGMT := 0;
        PNote^.LastSync := '';

        s := GetLocalNoteFile(ID);
        FileToNote(s, PNote );

        LocalMetaData.Add(PNote);
        inc(c);

    until FindNext(Info) <> 0;
    FindClose(Info);
    TRlog('Found '+IntToStr(c)+' local notes');

    manifest:= getManifestName();
    TRlog('Reading manifest '+manifest);
    if not FileExistsUTF8(manifest) then exit();

    try
         ReadXMLFile(Doc, manifest);
    except on E:Exception do
       begin
          TRlog('We failed to read XML file '+manifest);
          TRlog(E.message);
          exit(false);
       end;
    end;

    NodeList := Doc.DocumentElement.ChildNodes;

    if not assigned(NodeList) then
    begin
         TRlog('We failed to read XML children in the remote manifest file '+manifest);
         Doc.Free;
         exit();
    end;

    c :=0;
    for j := 0 to NodeList.Count-1 do
    begin
        TRlog('XML '+IntToStr(j));
        Node := NodeList.Item[j].Attributes.GetNamedItem('guid');
        ID := Node.TextContent;

        PNote := LocalMetaData.FindID(ID);
        if(PNote = Nil) then begin
            new(PNote);
            PNote^.Action:=SynUnset;
            PNote^.ID := ID;
            PNote^.Deleted := true;
            PNote^.LastSyncGMT := 0;
            PNote^.LastSync := '';
            LocalMetaData.Add(PNote);
        end;

        TRlog('searching last-revision');
        Node := NodeList.Item[j].Attributes.GetNamedItem('last-revision');
        if(assigned(Node)) then
        begin
             TRlog('last-revision found : '+ Node.TextContent);
             PNote^.Rev := StrToint(Node.TextContent);
        end else
        begin
             TRlog('last-revision not found');
             PNote^.Rev := -1;
        end;

        TRlog('searching last-sync-date');
        Node := NodeList.Item[j].Attributes.GetNamedItem('last-sync-date');
        if(assigned(Node)) then
        begin
             TRlog('last-sync-date found : '+ Node.TextContent);
             PNote^.LastSync := Node.TextContent;
        end else
        begin
             TRlog('last-sync-date not found');
             PNote^.LastSync := '';
        end;

        if PNote^.LastSync <> ''
        then PNote^.LastSyncGMT := GetGMTFromStr(PNote^.LastSync)
        else PNote^.LastSyncGMT := 0;
        inc(c);
        TRlog('c = '+IntToStr(c));
    end;
    TRlog('Found '+IntToStr(c)+' manifest notes');
end;

function TFormSync.LoadRemoteNotes(): boolean;
begin
    TRlog('LoadRemoteNotes');

    NoteMetaData.Clear;

    Result := Transport.GetNotes(NoteMetaData);

    TRlog('LoadRemoteNotes found ' + inttostr(NoteMetaData.Count) + ' remote notes');
end;


procedure TFormSync.PrepareSync(Sender : TObject);
var
   SyncAvail : TSyncStatus;
begin

    if(LocalTimer <> nil) then
    begin
        LocalTimer.Enabled := False;
        FreeAndNil(LocalTimer);
    end;

    TRlog(#10 + '******* StartSync');

    LabelTitle.Caption:= rsTestingRepo;
    LabelStats.Caption := '';
    StringGridReport.Clear;

    Application.ProcessMessages;

    TRlog(#10 + '******* SetTransport');

    ErrorString := '';
    FreeAndNil(Transport);

    case SyncType of
        SyncFile : begin
        	Transport := TFileSync.Create;
	        Transport.setParam('RemoteAddress',AppendPathDelim(SyncFileRepo));
                end;
	SyncNextCloud : begin
                Transport := TNextSync.Create;
	        Transport.setParam('URL', SyncNCUrl);
                Transport.setParam('KEY', SyncNCKey);
                Transport.setParam('TOKEN', SyncNCToken);
                Transport.setParam('SECRET', SyncNCSecret);
                end;
        SyncNone : begin
            ErrorString := rsNoSync;
            LabelStats.Caption := ErrorString;
            SettingsOK.Enabled:=false;
            SettingsOK.Visible:=false;
            exit();
        end;
    end;

    TRlog(#10 + '******* TestTransport');

    LabelTitle.Caption:=rsTestingSync;

    Application.ProcessMessages;

    SyncAvail := TestConnection();
    if SyncAvail <> SyncReady then
    begin
       LabelTitle.Caption:= rsUnableToProceed;
       LabelStats.Caption := Transport.ErrorString;
       ErrorString := rsUnableToProceed + ' ' + ErrorString;
       TRlog(ErrorString);

       SettingsOK.Enabled:=false;
       SettingsOK.Visible:=false;
       exit();
    end;

    LabelTitle.Caption:=rsLookingatLocalNotes;
    Application.ProcessMessages;

    TRlog(#10 + '******* Step 0.1 : LoadRemoteNotes');
    if not LoadRemoteNotes() then
    begin
       LabelTitle.Caption:= rsErrorLoadingRemote;
       ErrorString := Transport.ErrorString;
       LabelStats.Caption := ErrorString;

       ErrorString := rsUnableToProceed + ' ' + ErrorString;
       TRlog(ErrorString);

       SettingsOK.Enabled:=false;
       SettingsOK.Visible:=false;
       exit();
    end;

    LabelTitle.Caption:=rsLookingatRemoteNotes;
    Application.ProcessMessages;

    TRlog(#10 + '******* Step 0.2 : LoadLocalNotes');
    if not LoadLocalNotes() then
    begin
       LabelTitle.Caption:= rsErrorLoadingLocal;
       ErrorString := Transport.ErrorString;
       LabelStats.Caption := ErrorString;

       ErrorString := rsUnableToProceed + ' ' + ErrorString;
       TRlog(ErrorString);

       SettingsOK.Enabled:=false;
       SettingsOK.Visible:=false;
       exit();
    end;

    TRlog(#10 + '******* Step 1 : FindDeletedServerNotes');
    LabelTitle.Caption:= rsFindDeletedServerNotes;
    Application.ProcessMessages;
    FindDeletedServerNotes(); // Step 1 : compare local manifest and server status for locally existing notes

    TRlog(#10 + '******* Step 2 : FindDeletedLocalNotes');
    LabelTitle.Caption:= rsFindDeletedLocalNotes;
    Application.ProcessMessages;
    FindDeletedLocalNotes();  // Step 2 : compare local manifest and server status for none locally existing notes

    TRlog(#10 + '******* Step 3 : FindNewLocalNotes');
    LabelTitle.Caption:= rsFindNewLocalNotes;
    Application.ProcessMessages;
    FindNewLocalNotes();      // Step 3 : Add newly created local notes

    TRlog(#10 + '******* Step 4 : FindNewRemoteNotes');
    LabelTitle.Caption:= rsFindNewRemoteNotes;
    Application.ProcessMessages;
    FindNewRemoteNotes();     // Step 4 : Add newly created remote notes

    TRlog(#10 + '******* Step 5 : SetSystemicActions');
    LabelTitle.Caption:= rsSetSystemicActions;
    Application.ProcessMessages;
    SetSystemicActions();     // Step 5 : Set systemic actions

    TRlog(#10 + '******* Step 6 : ProcessClashes');
    LabelTitle.Caption:= rsProcessClashes;
    Application.ProcessMessages;
    if(not ProcessClashes())         // Step 6 : Process unresolved cases
    then begin
       //ShowError(ErrorString);
       LabelStats.Caption := ErrorString;
       SettingsOK.Visible := false;
    end else begin
       TRlog(#10 + '******* Step 7 : Statistics');
       LabelTitle.Caption := rsStatistics;
       Application.ProcessMessages;
       SyncSummary();
    end;
end;

function TFormSync.DoSync() : boolean;
// ========= Proceed with real actions
begin
    TRlog(#10 + '******* Step 8 : DoDownLoads');
    if not DoDownLoads() then
    begin
        ShowMessage(ErrorString);
        exit(false);
    end;

    TRlog(#10 + '******* Step 9 : DoDeleteLocal');
    if not DoDeleteLocal() then
    begin
        ShowMessage(ErrorString);
        exit(false);
    end;

    TRlog(#10 + '******* Step 10 : PushChanges');
    // Write remote manifest (only applicable for SyncFile)
    if not PushChanges() then
    begin
        ShowMessage(ErrorString);
        exit(false);
    end;

    TRlog(#10 + '******* Sync done .. reporting (manifest)');
    if not DoManifest() then
    begin
        ShowMessage(ErrorString);
        exit(false);
    end;
    Result:=true;
end;


{ ======= UI FONCTION =======}

function TFormSync.ResolveClashUI(const ClashRec : TClashRecord) : TSyncAction;
var
    clash : TFormClash;
begin
    TRlog('ResolveClashUI');

    clash := TFormClash.Create(self);

    clash.LabelSyncError.Caption := ClashRec.RemoteNote^.Error;
    clash.NoteID.Caption := 'Note ID ; '+ ClashRec.LocalNote^.ID;
    clash.TitleLocal.Caption := Copy(ClashRec.LocalNote^.Title,0,35);
    clash.ChangeLocal.Caption := ClashRec.LocalNote^.LastChange;
    clash.TitleRemote.Caption := Copy(ClashRec.RemoteNote^.Title,0,35);
    clash.ChangeRemote.Caption := ClashRec.RemoteNote^.LastChange;

    clash.MemoLocal.ReadOnly := true;
    clash.MemoLocal.Text := RemoveXml(ClashRec.LocalNote^.Content);
    clash.MemoRemote.ReadOnly := true;
    clash.MemoRemote.Text := RemoveXml(ClashRec.RemoteNote^.Content);

    case clash.ShowModal of
            mrYes      : Result := SynDownLoadEdit;
            mrNo       : Result := SynUpLoadEdit;
            mrNoToAll  : Result := SynAllLocal;
            mrYesToAll : Result := SynAllRemote;
            mrAll      : Result := SynAllNewest;
            mrRetry    : Result := SynAllCopy;
            mrClose    : Result := SynAllOldest;
    else
            Result := SynUnSet;   // Should not get there
    end;
    TRlog('Clash resolved as '+SyncActionName(Result));

    clash.Free;
    Application.ProcessMessages;
end;

procedure TFormSync.SyncSummary();
var
    i,j : integer;
    St : String;
    n : PNoteInfo;
    act : TSyncAction;
begin
    UpNew := 0; UpEdit := 0; DownNew := 0; DownEdit := 0; Undecided := 0;
    DelLoc := 0; DelRem := 0; DoNothing := 0; CreateCopy :=0;

    for i := 0 to NoteMetaData.Count -1 do
    begin
        case NoteMetaData.Items[i]^.Action of
            SynUpLoadNew : inc(UpNew);
            SynCopy : inc(CreateCopy);
            SynUpLoadEdit : inc(UpEdit);
            SynDownLoadNew : inc(DownNew);
            SynDownLoadEdit : inc(DownEdit);
            SynDeleteLocal : inc(DelLoc);
            SynDeleteRemote : inc(DelRem);
            SynNothing : inc(DoNothing);
            SynUnset : inc(Undecided);
	end;
    end;

    StringGridReport.Clean;

    TRlog('Dump status');

    j:=0;
    for I := 0 to NoteMetaData.Count -1 do
    begin
        n := NoteMetaData.Items[i];
        act := n^.Action;
        if(act in [SynUploadEdit, SynUploadNew]) then
        begin
            n := LocalMetaData.FindID(n^.ID);
        end;
        St := ' Rev=' + inttostr(n^.Rev);
        while length(St) < 9 do St := St + ' ';

        TRlog(n^.ID + St + SyncActionName(act)
          + ' LastChange='+n^.LastChange + ' Title=' + n^.Title);

        if n^.Action = SynNothing then continue;

        StringGridReport.InsertRowWithValues(j,
           [SyncActionName(act), n^.Title, n^.ID]);
         inc(j);
    end;

    if(Undecided>0) then ShowMessage('Real error out there... Undecided >0 !');

    StringGridReport.AutoSizeColumn(0);
    StringGridReport.AutoSizeColumn(1);
    StringGridReport.AutoSizeColumn(2);

    LabelTitle.Caption := rsSyncReport;
    LabelStats.Caption := Trim(rsNewUploads)+' : '+IntToStr(UpNew)+', ' + Trim(rsEditUploads) + ' : '+IntToStr(UpEdit)
            +', ' + Trim(rsNewDownloads)+' : '+IntToStr(DownNew)+', '+ Trim(rseditDownloads) + ' : '+IntToStr(DownEdit)
            +', ' + Trim(rsLocalDeletes)+' : '+IntToStr(DelLoc)+', '+ Trim(rsRemoteDeletes) + ' : '+IntToStr(DelRem)
            +', Duplicated : '+IntToStr(CreateCopy)+', '+ Trim(rsDoNothing) + ' : '+IntToStr(DoNothing)
            +', Errors : '+IntToStr(Undecided);

end;

procedure TFormSync.FormHide(Sender: TObject);
begin
    TRlog('FormHide');
end;

procedure TFormSync.SettingsCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormSync.SettingsOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;


procedure TFormSync.SyncVisible();
begin
    LabelTitle.Caption := rsRunningSync;
    LabelStats.Caption := rsRunningSync;
    StringGridReport.Clear;

    LocalTimer := TTimer.Create(Nil);
    LocalTimer.OnTimer:= @PrepareSync;
    LocalTimer.Interval:=500;
    LocalTimer.Enabled := True;

    if(self.ShowModal = mrCancel) then
    begin
        TRlog('User canceled operation');
        exit();
    end;

    DoSync();

end;

procedure TFormSync.SyncHidden();
begin
    LabelTitle.Caption := rsRunningSync;
    LabelStats.Caption := rsRunningSync;
    StringGridReport.Clear;

    PrepareSync(nil);

    if((Undecided>0) or (DelLoc>0) or (DelRem>0)) then
    begin
        TRlog('Hidden raise errors');

        if(self.ShowModal = mrCancel) then
        begin
             TRlog('User did not confirm');
             ErrorString := rsSyncCanceledByUser;
             TRlog(ErrorString);
             exit();
        end;
    end;

    DoSync();
end;

procedure TFormSync.FormShow(Sender: TObject);
    begin
    Left := 100 + random(55);
    Top := 100 + random(55);
end;


end.

