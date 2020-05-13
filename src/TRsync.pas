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


     { Do actual sync, but if TestRun=True just report on what you'd do.
              Assumes a Transport has been selected and remote address is set.
              We must already be a member of this sync, ie, its remote ID is recorded
              in our local manifest. }
     function StartSync(display : boolean) : boolean;

     procedure FormCreate(Sender: TObject);
     procedure FormDestroy(Sender: TObject);

     procedure SyncEvent(Sender : TObject);

public
     ErrorString : String;
     DeletedList, DownList, GridReportList : TStringList;

private

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

    function GetNoteTitle(const ID : ANSIString) : ANSIString;

    { Reports on contents of a created and filled list }
    function ReportMetaData(out UpNew, UpEdit, DownNew, DownEdit, DelLoc, DelRem, CreateCopy, DoNothing, Undecided: integer) : integer;

    { Populates the string grid with details of notes to be actioned }
    procedure ShowReport;

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
    procedure ProcessClashes();

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
  debugln('FormSync Create');

  NoteMetaData := TNoteInfoList.Create;
  LocalMetaData := TNoteInfoList.Create;
  DeletedList := TStringList.Create;
  DownList := TStringList.Create;
  GridReportList := TStringList.Create;
  Transport := nil;
end;


procedure TFormSync.FormDestroy(Sender: TObject);
begin
   debugln('FormSync Destroy');

   FreeandNil(LocalMetaData);
   FreeandNil(NoteMetaData);
   FreeandNil(Transport);
   FreeandNil(DeletedList);
   FreeandNil(DownList);

   if LocalTimer = Nil then exit();
    LocalTimer.Free;
    LocalTimer := nil;
end;

function TFormSync.getManifestName() : String;
begin
    debugln('GetManifestName');

    Result := Format('%s%s-%s-manifest.xml',[NotesDir, Transport.getPrefix() , Transport.ServerID ]);
end;


{ =================   E X T E R N A L   C A L L   O U T S ===========================}


function TFormSync.ReportMetaData(out UpNew, UpEdit, DownNew, DownEdit, DelLoc, DelRem, CreateCopy, DoNothing, Undecided : integer) : integer;
var
    Index : integer;
begin
    debugln('ReportMetaData');

    UpNew := 0; UpEdit := 0; DownNew := 0; DownEdit := 0; Undecided := 0;
    DelLoc := 0; DelRem := 0; DoNothing := 0; CreateCopy :=0;

    for Index := 0 to NoteMetaData.Count -1 do
    begin
        case NoteMetaData.Items[Index]^.Action of
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
    result := NoteMetaData.Count;
end;


function TFormSync.GetNoteTitle(const ID : ANSIString) : ANSIString;
var
    note : PNoteInfo;
begin
   debugln('GetNoteTitle');

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
    debugln('DatesCompare S1='+DateToStr(DS1)+' vs S2='+DateToStr(DS2));

    debugln('DS1 '+DateTimeToStr(DS1));
    debugln('DS1+ '+DateTimeToStr(DS1 + Margin));
    debugln('DS1- '+DateTimeToStr(DS1 - Margin));
    debugln('DS2 '+DateTimeToStr(DS2));
    debugln('DS2+ '+DateTimeToStr(DS2 + Margin));
    debugln('DS2- '+DateTimeToStr(DS2 - Margin));

    if (DS1 > DS2 + Margin) then exit(DS1-DS2);
    if (DS2 > DS1 + Margin) then exit(DS1-DS2);

    Result := 0;
end;


procedure TFormSync.FindDeletedServerNotes();
var
    PNote : PNoteInfo;
    ID : String;
    i,c : integer;
begin
   debugln('FindDeletedServerNotes');

   c := 0;
    for i := 0 to LocalMetaData.Count -1 do
    begin
        if(LocalMetaData.Items[i]^.Deleted) then continue; // Do not consider deleted
        if(LocalMetaData.Items[i]^.Rev<0) then continue; // Do not consider never sync

        ID := LocalMetaData.Items[i]^.ID;
        PNote := NoteMetaData.FindID(ID);
        if(PNote <> Nil) then continue;

        new(PNote);
        PNote^.Action:=SynDeleteLocal;
        PNote^.ID := ID;
                  // Whatever the content, we will backup/delete it
        NoteMetaData.Add(PNote);
        inc(c);
    end;
    debugln('Found '+IntToStr(c)+' deleted server notes');
end;

procedure TFormSync.FindDeletedLocalNotes();
var
    ID : String;
    PNote : PNoteInfo;
    c,i : integer;
begin
   debugln('FindDeletedLocalNotes');

    c:=0;
    for i := 0 to LocalMetaData.Count -1 do
    begin
        if(not LocalMetaData.Items[i]^.Deleted) then continue; // Consider only deleted

        ID := LocalMetaData.Items[i]^.ID;
        PNote := NoteMetaData.FindID(ID);
        if(PNote = Nil) then continue;
        inc(c);
        PNote^.Action:=SynDeleteRemote;
    end;
    debugln('Found '+IntToStr(c)+' deleted local notes');
end;

procedure TFormSync.FindNewLocalNotes();
var
    ID : String;
    c,i : integer;
    PNote : PNoteInfo;
begin
   debugln('FindNewLocalNotes');

   c :=0;
    for i := 0 to LocalMetaData.Count -1 do
    begin
        if(LocalMetaData.Items[i]^.Deleted) then continue; // Do not consider deleted
        ID := LocalMetaData.Items[i]^.ID;
        PNote := NoteMetaData.FindID(ID);
        if(PNote <> Nil) then continue;

        new(PNote);
        PNote^ := LocalMetaData.Items[i]^;
        PNote^.Action:=SynUploadNew;
        NoteMetaData.Add(PNote);
        inc(c);
    end;
    debugln('Found '+IntToStr(c)+' new local notes');
end;

procedure TFormSync.FindNewRemoteNotes();
var
    ID : String;
    c,i : integer;
    PNote : PNoteInfo;
begin
    debugln('FindNewRemoteNotes');

    c := 0;
    for i := 0 to NoteMetaData.Count -1 do
    begin
         if(NoteMetaData.Items[i]^.Action <> SynUnset) then continue; // Don't look at notes already scrutinized

         ID := NoteMetaData.Items[i]^.ID;
         PNote := LocalMetaData.FindID(ID);
         if(PNote = Nil) then continue;

         NoteMetaData.Items[i]^.Action := SynDownLoadNew;
         inc(c);
    end;
    debugln('Found '+IntToStr(c)+' new remote notes');
end;

procedure TFormSync.SetSystemicActions();
var
    ID : String;
    i : integer;
    LNote, SNote : PNoteInfo;
begin
    debugln('SetSystemicActions');

    for i := 0 to NoteMetaData.Count -1 do
    begin
        SNote := NoteMetaData.Items[i];
        if(SNote^.Action <> SynUnset) then continue; // Don't look at notes already scrutinized

        ID := SNote^.ID;
        LNote := LocalMetaData.FindID(ID); // Must be not null !

        if(LNote^.Rev > SNote^.Rev) then continue; // This is abnormal -> Must check manually

        if((DatesCompare(LNote^.LastSyncGMT,SNote^.LastChangeGMT)>0) and (DatesCompare(LNote^.LastSyncGMT,LNote^.LastChangeGMT)>0)) then
        begin
           if((DatesCompare(LNote^.LastSyncGMT,SNote^.LastMetaChangeGMT)>0 ) and (DatesCompare(LNote^.LastSyncGMT, LNote^.LastMetaChangeGMT)>0 ))
           then SNote^.Action := SynNothing
           else if(DatesCompare(LNote^.LastSyncGMT, SNote^.LastMetaChangeGMT)>0 )
                then SNote^.Action := SynUploadEdit
                else if(DatesCompare(LNote^.LastSyncGMT, LNote^.LastMetaChangeGMT)>0 )
                     then SNote^.Action := SynDownloadEdit
                     else SNote^.Action := SynNothing;
        end;

        if(DatesCompare(LNote^.LastSyncGMT, SNote^.LastChangeGMT)>0 ) // i.e. not (LNote^.LastSyncGMT > LNote^.LastChangeGMT ))
        then SNote^.Action := SynUploadEdit
        else if(DatesCompare(LNote^.LastSyncGMT, LNote^.LastChangeGMT)>0 ) // i.e. not (LNote^.LastSyncGMT > SNote^.LastChangeGMT )
             then SNote^.Action := SynDownloadEdit;

        if((LNote^.Rev < SNote^.Rev) and (SNote^.Action <> SynDownloadEdit))  then // This is abnormal -> Must check manually
        begin
           SNote^.Action := SynUnset;
           continue;
        end;
    end;
end;


function TFormSync.ResolveAction(const SNote, LNote : PNoteInfo) : TSyncAction;
begin
    debugln('ResolveAction');

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


procedure TFormSync.ProcessClashes();
var
    ClashRec : TClashRecord;
    Index : integer;
    remote,local : PNoteInfo;
begin
    debugln('ProcessClashes');

    ClashAction := SynUnset;

    case SyncClashOption of
         TSyncClashOption.UseLocal : ClashAction := SynAllLocal;
         TSyncClashOption.UseServer : ClashAction := SynAllRemote;
         TSyncClashOption.MakeCopy : ClashAction := SynAllCopy;
    end;

    for Index := 0 to NoteMetaData.Count -1 do
    begin
        remote := NoteMetaData.Items[Index];
        if remote^.Action = SynUnset then
        begin
           local := LocalMetaData.FindID(remote^.ID);
           if ClashAction = SynUnset then
           begin
                ClashRec.RemoteNote := remote;
                ClashRec.LocalNote := local;
                ClashAction := ResolveClashUI(Clashrec);
           end;
           remote^.Action := ResolveAction(remote, local);
        end;
    end;
end;


{ ========================  N O T E   M O V E M E N T    M E T H O D S ================}

function TFormSync.DoDownloads() : boolean;
var
    i : integer;
    dest,backupdir,backup,ID : String;
    f : TextFile;
    note : PNoteInfo;
    GUID : TGUID;
begin
    debugln('DoDownloads');

    backupdir := NotesDir + 'Backup' + PathDelim;

    if not DirectoryExists(backupdir) then
        if not ForceDirectory(backupdir) then
        begin
            ErrorString := 'Failed to create Backup directory.';
            exit(False);
        end;

    for i := 0 to NoteMetaData.Count-1 do
    begin
        if NoteMetaData.Items[i]^.Action <> SynCopy then continue;
        new(note);
        note^ := NoteMetaData.Items[i]^;
        note^.Title := note^.Title + ' (Server)';
        CreateGUID(GUID);
        note^.ID := copy(GUIDToString(GUID), 2, 36);
        note^.Action := SynDownloadNew;
        NoteMetaData.Add(note);
        NoteMetaData.Items[i]^.Action := SynUploadEdit;
    end;

    DownList.Clear;

    for i := 0 to NoteMetaData.Count-1 do
    begin
        if (NoteMetaData.Items[i]^.Action <> SynDownLoadEdit) and (NoteMetaData.Items[i]^.Action <> SynDownLoadNew) then continue;
        ID := NoteMetaData.Items[i]^.ID;

        DownList.Add(ID);
        dest := GetLocalNoteFile(ID);
        backup := GetLocalBackupPath();
        if FileExists(dest) then
        begin
           ForceDirectories(backup);
           if not CopyFile(dest, GetLocalNoteFile(ID, backup)) then
                begin
                    ErrorString := 'Failed to copy file '+ dest + ' to Backup ' + backup;
                    debugln(ErrorString);
                    exit(False);
                end;
        end;
        AssignFile(f,dest);
        write(f, NoteMetaData.Items[i]^.Content);
        CloseFile(f);
    end;

    result := True;
end;


function TFormSync.DoDeleteLocal() : boolean;
var
    I : integer;
    s,d,ID : String;
begin
    debugln('DoDeleteLocal');

    DeletedList.Clear;

    for I := 0 to NoteMetaData.Count -1 do
    begin
        if NoteMetaData.Items[i]^.Action <> SynDeleteLocal then continue;
        ID := NoteMetaData.Items[i]^.ID;
        DeletedList.Add(ID);

        s:= GetLocalNoteFile(ID);
        d:= GetLocalNoteFile(ID,GetLocalBackupPath());

        if FileExists(s) then
        begin
            ForceDirectories(s+'Backup');

            if CopyFile(s,d)
            then DeleteFile(s);
        end;
    end;
    result := true;
end;


function TFormSync.PushChanges() : boolean;
var
    notes : TNoteInfoList;
    i : integer;
    note,l : PNoteInfo;
begin
    debugln('PushChanges . ServerRev = ' + inttostr(Transport.ServerRev));

    notes := TNoteInfoList.Create;

    for i := 0 to NoteMetaData.Count -1 do
    begin
        note := NoteMetaData.Items[i];

        if(note^.Action  = SynUploadNew) then
        begin
            note^.Rev := Transport.ServerRev + 1;
            notes.Add(note);
        end;

        if(note^.Action  = SynUploadEdit) then
        begin
            l := LocalMetaData.FindID(note^.ID);
            l^.Action := SynUploadEdit;
            l^.Rev := Transport.ServerRev + 1;
            notes.Add(l);
        end;

        if note^.Action = SynDeleteRemote then
        begin
            notes.Add(l);
        end;
    end;

    if(notes.Count = 0) then Result:= false
    else Result := Transport.PushChanges(notes);

    FreeAndNil(notes);

end;


function TFormSync.DoManifest(): boolean;
var
    OutFile: String;
    Index : integer;
    d,globalrev,rev : string;
    needRev : boolean;
    f : TextFile;
begin
    debugln('DoManifest');

    needRev := False;
    for Index := 0 to NoteMetaData.Count - 1 do
    begin
        if NoteMetaData[Index]^.Action in [SynUploadNew, SynUpLoadEdit, SynDeleteRemote] then needRev := True;
    end;

    if(needRev) then globalrev := inttostr(Transport.ServerRev + 1)
    else globalrev := inttostr(Transport.ServerRev);

    OutFile := '<?xml version="1.0" encoding="utf-8"?>';
    OutFile := OutFile + '<sync revision="' + globalrev;
    OutFile := OutFile + '" server-id="' + Transport.ServerID + '">';

    for Index := 0 to NoteMetaData.Count - 1 do
    begin
         if NoteMetaData[Index]^.Deleted then continue;

         if NoteMetaData.Items[Index]^.LastChangeGMT = 0
         then d := GetCurrentTimeStr()
         else d := GetTimeFromGMT(NoteMetaData.Items[Index]^.LastChangeGMT);

         if NoteMetaData[Index]^.Action in [SynUploadNew, SynUpLoadEdit]
         then begin
              rev := IntToStr(Transport.ServerRev+1);
              d := GetCurrentTimeStr();
         end else rev := IntToStr(NoteMetaData[Index]^.Rev);


         if NoteMetaData[Index]^.Action in [SynUploadNew, SynUpLoadEdit, SynDownLoadNew, SynDownLoadEdit, SynNothing] then
         begin
	      OutFile := OutFile +  '  <note guid="' + NoteMetaData.Items[Index]^.ID + '" latest-revision="'
              + rev + '" last-change-date="' + d + '" />';
         end
         else debugln('Skipping '+NoteMetaData[Index]^.ID + ' because Action=' + NoteMetaData.ActionName(NoteMetaData[Index]^.Action));
    end;
    OutFile := OutFile + '</sync>';

    result := Transport.DoRemoteManifest(OutFile);

    try
       AssignFile(f,getManifestName());
       Rewrite(f);
       Write(f,Outfile);
       CloseFile(f);
    except on E:Exception do begin
       ErrorString := E.message;
       debugln(ErrorString);
       exit(false);
       end;
    end;

end;


{ =================  S T A R T   U P   M E T H O D S ============== }


function TFormSync.TestConnection(): TSyncStatus;
var
    res : TSyncStatus;
begin
    debugln('TestConnection');

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
    debugln('LoadLocalNotes ' + GetLocalNoteFile('*'));

    FreeAndNil(LocalMetaData);
    LocalMetaData := TNoteInfoList.Create;
    c :=0;
    if FindFirst(GetLocalNoteFile('*'), faAnyFile, Info)=0 then
    repeat
        ID := copy(Info.Name, 1, 36);
        new(PNote);
        PNote^.Action:=SynUnset;
        PNote^.ID := ID;
        PNote^.Rev := -1;
        PNote^.LastSyncGMT := 0;
        PNote^.LastSync := '';

        s := GetLocalNoteFile(ID);
        if(FileToNote(s, PNote ))
        then begin
             LocalMetaData.Add(PNote);
             inc(c);
        end
        else begin
             Dispose(PNote);
             debugln('Local note '+s+' not readable');
        end;
    until FindNext(Info) <> 0;
    FindClose(Info);
    debugln('Found '+IntToStr(c)+' total local notes');

    manifest:= getManifestName();
    if not FileExists(manifest) then exit();

    try
         ReadXMLFile(Doc, manifest);
    except on E:Exception do begin debugln(E.message); exit(false); end;
    end;

    NodeList := Doc.DocumentElement.ChildNodes;

    if not assigned(NodeList) then
    begin
         Doc.Free;
         debugln('We failed to read XML children in the remote manifest file '+manifest);
         exit();
    end;

    for j := 0 to NodeList.Count-1 do
    begin
        Node := NodeList.Item[j].Attributes.GetNamedItem('guid');
        ID := Node.NodeValue;

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

        Node := NodeList.Item[j].Attributes.GetNamedItem('latest-revision');
        PNote^.Rev := StrToint(Node.NodeValue);
        Node := NodeList.Item[j].Attributes.GetNamedItem('latest-sync-date');
        PNote^.LastSync := Node.NodeValue;
        if PNote^.LastSync <> ''
        then PNote^.LastSyncGMT := GetGMTFromStr(PNote^.LastSync)
        else PNote^.LastSyncGMT := 0;
    end;
end;

function TFormSync.LoadRemoteNotes(): boolean;
begin
    debugln('LoadRemoteNotes');

    Result := True;
    FreeAndNil(NoteMetaData);
    NoteMetaData := TNoteInfoList.Create;

    Result := Transport.GetNotes(NoteMetaData);

    debugln('LoadRemoteNotes found ' + inttostr(NoteMetaData.Count) + ' remote notes');
end;

                        { ---------- The Lets Do it Function ------------- }

function TFormSync.StartSync(display : boolean): boolean;
var
   i : integer;
   SyncAvail : TSyncStatus;
   UpNew, UpEdit, DownNew, DownEdit, DelLoc, DelRem, CreateCopy, DoNothing, Undecided : integer;
   St : String;
begin
    debugln('StartSync');

    LabelTitle.Caption:= rsTestingRepo;
    StringGridReport.Clear;

    Application.ProcessMessages;

    debugln('SetTransport');

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
            exit(true);
        end;
    end;

    debugln('TestTransport');

    LabelTitle.Caption:=rsTestingSync;

    Application.ProcessMessages;

    SyncAvail := TestConnection();
    if SyncAvail <> SyncReady then
    begin
       LabelTitle.Caption:= rsUnableToProceed;
       LabelStats.Caption := ErrorString;
       ErrorString := rsUnableToProceed + ' ' + ErrorString;
       debugln(ErrorString);

       SettingsCancel.Enabled:=false;
       SettingsCancel.Visible:=false;
       self.ShowModal;
       SettingsCancel.Enabled:=true;
       SettingsCancel.Visible:=true;

       exit(false);
    end;

    LabelTitle.Caption:=rsLookingatNotes;

    Application.ProcessMessages;

    Debugln('Step 0.1 : LoadRemoteNotes');
    if not LoadRemoteNotes() then
    begin
       LabelTitle.Caption:= rsErrorLoadingRemote;
       ErrorString := Transport.ErrorString;
       LabelStats.Caption := ErrorString;

       ErrorString := rsUnableToProceed + ' ' + ErrorString;
       debugln(ErrorString);

       SettingsCancel.Enabled:=false;
       SettingsCancel.Visible:=false;
       self.ShowModal;
       SettingsCancel.Enabled:=true;
       SettingsCancel.Visible:=true;

       exit(False);
    end;

    Application.ProcessMessages;

    Debugln('Step 0.2 : LoadLocalNotes');
    if not LoadLocalNotes() then
    begin
       LabelTitle.Caption:= rsErrorLoadingLocal;
       ErrorString := Transport.ErrorString;
       LabelStats.Caption := ErrorString;

       ErrorString := rsUnableToProceed + ' ' + ErrorString;
       debugln(ErrorString);

       SettingsCancel.Enabled:=false;
       SettingsCancel.Visible:=false;
       self.ShowModal;
       SettingsCancel.Enabled:=true;
       SettingsCancel.Visible:=true;

       exit(false);
    end;

    Debugln('Step 1 : FindDeletedServerNotes');
    LabelTitle.Caption:= rsFindDeletedServerNotes;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    FindDeletedServerNotes(); // Step 1 : compare local manifest and server status for locally existing notes

    Debugln('Step 2 : FindDeletedLocalNotes');
    LabelTitle.Caption:= rsFindDeletedLocalNotes;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    FindDeletedLocalNotes();  // Step 2 : compare local manifest and server status for none locally existing notes

    Debugln('Step 3 : FindNewLocalNotes');
    LabelTitle.Caption:= rsFindNewLocalNotes;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    FindNewLocalNotes();      // Step 3 : Add newly created local notes

    Debugln('Step 4 : FindNewRemoteNotes');
    LabelTitle.Caption:= rsFindNewRemoteNotes;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    FindNewRemoteNotes();     // Step 4 : Add newly created remote notes

    Debugln('Step 5 : SetSystemicActions');
    LabelTitle.Caption:= rsSetSystemicActions;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    SetSystemicActions();     // Step 5 : Set systemic actions

    Debugln('Step 6 : ProcessClashes');
    LabelTitle.Caption:= rsProcessClashes;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    ProcessClashes();         // Step 6 : Process unresolved cases

    Debugln('Step 7 : Statistics');
    LabelTitle.Caption := rsStatistics;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    ReportMetaData(UpNew, UpEdit, DownNew, DownEdit, DelLoc, DelRem, CreateCopy, DoNothing, Undecided);

    if(display or (Undecided>0) or (DelLoc>0) or (DelRem>0)) then
       begin
          debugln('Dump status');

          for I := 0 to NoteMetaData.Count -1 do
          begin
              St := ' ' + inttostr(NoteMetaData.Items[i]^.Rev);
              while length(St) < 5 do St := St + ' ';
              debugln(NoteMetaData.Items[I]^.ID + St + NoteMetaData.ActionName(NoteMetaData.Items[i]^.Action)
                 + NoteMetaData.Items[i]^.LastChange + '   ' + NoteMetaData.Items[I]^.Title);
          end;

          GridReportList.Clear;
          for i := 0 to NoteMetaData.Count -1 do
          begin
               if NoteMetaData.Items[i]^.Action = SynNothing then continue;

               GridReportList.Add(NoteMetaData.ActionName(NoteMetaData.Items[i]^.Action));
               GridReportList.Add(NoteMetaData.Items[i]^.Title);
               GridReportList.Add(NoteMetaData.Items[i]^.ID);
          end;

          LabelTitle.Caption := rsSyncReport;
          LabelStats.Caption := 'New local notes : '+IntToStr(UpNew)+', Updated locally : '+IntToStr(UpEdit)
            +', New remote notes : '+IntToStr(DownNew)+', Updated remotely : '+IntToStr(DownEdit)
            +', Deleted locally : '+IntToStr(DelLoc)+', Deleted remotely : '+IntToStr(DelRem)
            +', Duplicated : '+IntToStr(CreateCopy)+', Unchanged : '+IntToStr(DoNothing)
            +', Errors : '+IntToStr(Undecided);

          if(self.ShowModal = mrCancel) then
          begin
             ErrorString := rsSyncCanceledByUser;
             debugln(ErrorString);
             exit(false);
          end;
       end;

    // ========= Proceed with real actions

    Debugln('Step 8 : DoDownLoads');
    LabelTitle.Caption := rsSyncDoDownloads;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    if not DoDownLoads() then exit(false);

    Debugln('Step 9 : DoDeleteLocal');
    LabelTitle.Caption := rsDoDeleteLocal;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    if not DoDeleteLocal() then exit(false);

    // Write remote manifest (only applicable for SyncFile)
    LabelTitle.Caption := rsDoManifest;
    LabelStats.Caption := '';
    Application.ProcessMessages;
    if not PushChanges() then exit(false);

    DebugLn('Sync done .. reporting');

    Result := DoManifest();
end;


{ ======= UI FONCTION =======}

function TFormSync.ResolveClashUI(const ClashRec : TClashRecord) : TSyncAction;
var
    clash : TFormClash;
begin
    debugln('ResolveClashUI');

    clash := TFormClash.Create(self);

    clash.NoteID.Caption := 'Note ID ; '+ ClashRec.LocalNote^.ID;
    clash.TitleLocal.Caption := ClashRec.LocalNote^.Title;
    clash.ChangeLocal.Caption := ClashRec.LocalNote^.LastChange;
    clash.TitleRemote.Caption := ClashRec.RemoteNote^.Title;
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
    clash.Free;
    Application.ProcessMessages;
end;

procedure TFormSync.FormHide(Sender: TObject);
begin
    debugln('FormHide');
end;

procedure TFormSync.SettingsCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TFormSync.SettingsOKClick(Sender: TObject);
begin
  ModalResult := mrOk;
end;


procedure TFormSync.SyncEvent(Sender : TObject);
begin
    debugln('SyncEvent 1');

    LocalTimer.Enabled := False;
    FreeAndNil(LocalTimer);

    debugln('SyncEvent 2');

    // A REFAIRE (AFFICHAGE)

    if(isSyncConfigured()) then
    begin
       debugln('SyncEvent 3');
       if(not StartSync(true)) then ShowMessage(ErrorString);
    end
    else ShowMessage(rsNoSync);

end;

procedure TFormSync.FormShow(Sender: TObject);
begin
    Left := 55 + random(55);
    Top := 55 + random(55);

    LabelTitle.Caption := rsRunningSync;
    LabelStats.Caption := rsRunningSync;
    StringGridReport.Clear;

    //LocalTimer := TTimer.Create(Nil);
    //LocalTimer.OnTimer:= @AfterShown;
    //LocalTimer.Interval:=500;
    //LocalTimer.Enabled := True;
end;


procedure TFormSync.ShowReport;

var
    Rows,i : integer;
begin
    StringGridReport.Clean;
    i := 0;
    Rows :=0;
    while (i<GridReportList.Count) do
    begin
        StringGridReport.InsertRowWithValues(
            Rows,
            [GridReportList.Strings[i],
            GridReportList.Strings[i+1],
            GridReportList.Strings[i+2]]);

        // HintText := FileSync.ReportList.Items[ARow]^.Message;

        inc(Rows);
        i := i+3;
    end;

    StringGridReport.AutoSizeColumn(0);
    StringGridReport.AutoSizeColumn(1);

    //if  Rows = 0
    //then Memo1.Append(rsNoNotesNeededSync)
    //else Memo1.Append(inttostr(Rows) + rsNotesWereDealt);
end;


end.

