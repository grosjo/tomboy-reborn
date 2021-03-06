unit TRcommon;

interface

uses
    {$ifdef unix}
    cthreads,
    cmem,
    {$endif}
    Classes, Forms, SysUtils, Dialogs, StdCtrls, LazFileUtils, laz2_DOM,
    ExtCtrls, laz2_XMLRead, DateUtils,
    {$ifdef VER3_2} sslbase, openssl, fphttpclient, ssockets, opensslsockets, {$endif}
    {$ifndef VER3_2} fphttpclient, ssockets, sslsockets, fpopenssl, openssl, {$endif}
    hmac, strutils, IniFiles, LazLogger, Graphics,
    {$ifdef LINUX} Unix, {$endif} LazUTF8,
    FileInfo, TRAutoStartCtrl, Trtexts;


type TSyncClashOption = (AlwaysAsk, UseServer, UseLocal, MakeCopy);	// Relating to sync clash pref in config file

type TSyncTransport=(
        SyncFile,  // Sync to locally available dir, things like smb: mount, google drive etc
        SyncNextCloud,  // Sync to NextCloud using Nextcloud Notes
        SyncNone); // Not syncing

type TSyncAction = (
        SynUnset,      // initial state, should not be like this at end.
        SynClash,      // Cant decide
        SynNothing,      // This note, previously synced has not changed.
        SynUploadNew,    // This a new local note, upload it.
        SynUploadEdit,   // A previously synced note, edited locally, upload.
        SynDownloadNew,     // A new note from elsewhere, download.
        SynDownloadEdit,     // An edited note from elsewhere, download.
        SynDeleteLocal,  // Synced previously but no longer present on server, delete locally
        SynDeleteRemote, // Marked as having been deleted locally, so remove from server.
        SynCopy,         // Make a copy hen replace by server
        SynError,
        SynAllRemote,    // Clash Decision - Use remote note for all subsquent clashes
        SynAllCopy,      // Clash Decision - Make copies for all subsquent clashes
        SynAllLocal,     // Clash Decision - Use local note for all subsquent clashes
        SynAllNewest,    // Clash Decision - Use newest note for all subsquent clashes
        SynAllOldest);   // Clash Decision - Use oldest note for all subsquent clashes

        // Indicates the readyness of a sync connection
type TSyncStatus = (
        SyncNotYet,        // Initial state.
        SyncReady,          // We are ready to sync, looks good to go.
        SyncNoRemoteWrite,  // Remote not writeable
        SyncNoRemoteEnd,    // Remote end does not exist (no folder, no conenction, ...)
        SyncNoRemoteMan,    // No remote manifest, an uninitialized repo perhaps ?
        SyncBadRemote,      // General error on remote side
        SyncXMLError,       // Housten, we have an XML error in a manifest !
        SyncBadError,       // Some other error, must NOT proceed.
        SyncNetworkError);  // Remove server/device not responding


type TNoteInfo =
  record
        ID : UTF8String;            // The 36 char ID
        CreateDate : UTF8String;
        CreateDateGMT : TDateTime;
        LastChange : UTF8String;
        LastChangeGMT : TDateTime;
        LastMetaChange : UTF8String;
        LastMetaChangeGMT : TDateTime;
        Version : UTF8String;
        Rev : Integer;
        LastSync: UTF8String;
        LastSyncGMT : TDateTime;
        Action : TSyncAction;
        Title : UTF8String;
        Content : UTF8String;
        OpenOnStartup : boolean;
        Pinned : boolean;
        CursorPosition : integer;
        SelectBoundPosition : integer;
        Width : integer;
        Height : integer;
        X : integer;
        Y : integer;
        Tags : TStringList;
        Error : UTF8String;
        Deleted : boolean;
        Display : ^TForm;
        Scan : TDateTime;
  end;

type PNoteInfo=^TNoteInfo;

type TNoteInfoList =
  class(TList)
    private
     	function Get(Index: integer): PNoteInfo;
    public
        LName : UTF8String;
        destructor Destroy; override;
        function Add(ANote : PNoteInfo) : integer;
        function Remove(ANote : PNoteInfo) : integer;
        function FindID(const ID : UTF8String) : PNoteInfo;
        property Items[Index: integer]: PNoteInfo read Get; default;
  end;

type TClashRecord =
      record
         RemoteNote : PNoteInfo;
         LocalNote : PNoteInfo;
      end;


// Environmment
const Backup = 'Backup';
procedure TRlog(s : UTF8String);
function AboutString() : UTF8String;
function GetDefaultConfigDir() : UTF8String;
function GetDefaultNotesDir() : UTF8String;
function GetLocalNoteFile(NoteID : UTF8String; altrep : UTF8String = ''): UTF8String;
function GetLocalBackupPath(): UTF8String;
function GetTempFile() : UTF8String;
function ConfigWrite(source : UTF8String) : boolean;
function ConfigRead(source : UTF8String) : integer;

// Note generic function
function GetNewID() : UTF8String;
function NoteIDLooksOK(const ID : UTF8String) : boolean;
function FileToNote(filename : UTF8String; NoteInfo : PNoteInfo) : boolean;
function NoteToFile(note : PNoteInfo; filename : UTF8String) : boolean;
function RemoveXml(const St : UTF8String) : UTF8String;
procedure CopyNote(A : PNoteInfo; c : PNoteInfo);
function EmptyNote() : PNoteInfo;
function NoteContains(const TermList: TStringList; N : PNoteInfo ; CaseSensitive : boolean = false): boolean;
function NoteBelongs(const notebook : UTF8String ; N : PNoteInfo ): boolean;
function NoteTimeOrder(Item1: Pointer;Item2: Pointer):Integer;

// String function
function ReplaceAngles(const Str : UTF8String) : UTF8String;
function EncodeAngles(const Str : UTF8String) : UTF8String;
function isURL(u : UTF8String) : boolean;
function CleanTitle(u: UTF8String) : UTF8String;

// Font
function GetDefaultFixedFont() : UTF8String;
function GetDefaultUsualFont() : UTF8String;

// Datetime
function GetGMTFromStr(const DateStr: UTF8String): TDateTime;
function GetTimeFromGMT(d : TDateTime) : UTF8String;
function GetCurrentTimeStr() : UTF8String;
function GetDisplayTimeFromGMT(d : TDateTime) : UTF8String;


// Spelling
function GetDictDefaultPath() : UTF8String;
function GetDictDefaultLibrary() : UTF8String;

// Network
function URLDecode(s: UTF8String): UTF8String;
function URLEncode(s: UTF8String): UTF8String;
function WebPut(u : UTF8String; params : TStrings; data : UTF8String) : UTF8String;
function WebPost(u : UTF8String; params : TStrings) : UTF8String;
function WebGet(u : UTF8String; params : TStrings) : UTF8String;

// Oauth 1.0
const OAuthCallbackUrl = 'http://localhost:8000/tomboy-web-sync/';
procedure OauthBaseParams(const p : TStrings; Key : UTF8String; Token : UTF8String = ''; Verifier : UTF8String = '');
function OauthTimestamp() : UTF8String;
function OauthNonce() : UTF8String;
procedure OauthParamsSort(const params : TStrings) ;
procedure OauthSign(u : UTF8String; mode : UTF8String; params : TStrings; Key,Secret : UTF8String);

// Sync
function SyncActionName(Act : TSyncAction) : UTF8String;
function isSyncConfigured() : boolean;

var
    ConfigDir : UTF8String;
    NotesDir : UTF8String;
    ConfigFile : UTF8String;
    MainErrorString : UTF8String;

    ManyNoteBooks, Autostart, SearchAtStart, UseTrayIcon : boolean;

    UsualFont, FixedFont : UTF8String;
    FontScale : integer;

    SyncType : TSyncTransport;
    SyncClashOption : TSyncClashOption;

    SyncFileRepo, SyncNCurl, SyncNCKey, SyncNCToken, SyncNCSecret : UTF8String;
    SyncRepeat,LastUsedNB : integer;

    DictLibrary, DictPath, DictFile : UTF8String;

    BackGndColour, TextColour, HiColour, TitleColour : TColor;

    LastUsed: TStringList; // Latest used notes

    mainWindow : TForm;

    Debug : boolean;

Const
  Placement = 45;

implementation


{ ===== ENVIRONMENT ==== }

procedure TRlog(s : UTF8String);
begin
  if(Debug) then DebugLn(s);
end;

function AboutString() : UTF8String;
var
   Stg : UTF8String;
   FileVerInfo: TFileVersionInfo;
begin
   Stg := rsAbout + #10;
   FileVerInfo:=TFileVersionInfo.Create(nil);
   try
      FileVerInfo.ReadFileInfo;
      Stg := Stg + 'Company: ' + FileVerInfo.VersionStrings.Values['CompanyName'] + #10;
      Stg := Stg + 'File description: ' + FileVerInfo.VersionStrings.Values['FileDescription'] + #10;
      Stg := Stg + 'File version: ' + FileVerInfo.VersionStrings.Values['FileVersion'] + #10;
      Stg := Stg + 'Internal name: ' + FileVerInfo.VersionStrings.Values['InternalName'] + #10;
      Stg := Stg + 'Product name:  ' + FileVerInfo.VersionStrings.Values['ProductName'] + #10;
      Stg := Stg + 'Product version:  ' + FileVerInfo.VersionStrings.Values['ProductVersion'] + #10;
   finally
      FileVerInfo.Free;
   end;
   Result := Stg + rsAboutCPU + ' ' + {$i %FPCTARGETCPU%} + '  '
            + rsAboutOperatingSystem + ' ' + {$i %FPCTARGETOS%}
            + ' ' + GetEnvironmentVariable('XDG_CURRENT_DESKTOP');
end;

function GetDefaultConfigDir() : UTF8String;
begin
    {$ifdef DARWIN}
        // First we try the right place, if there use it, else try unix place, if
        // its not there, go back to right place.
        Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-Reborn/Config';
        if not DirectoryExistsUTF8(Result) then begin
            Result := GetAppConfigDirUTF8(False);
            if not DirectoryExistsUTF8(Result) then  // must be new install, put in right place
                Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-Reborn/Config';
        end;
    {$else}
        Result := GetAppConfigDirUTF8(False);
    {$endif}

    Result := AppendPathDelim(ChompPathDelim(Result));
    ForceDirectoriesUTF8(Result);
    TRlog('Default Conf Dir ='+Result);
end;

function GetDefaultNotesDir() : UTF8String;
begin
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + '/.local/share/TomboyReborn/';
    {$ENDIF}
    {$IFDEF DARWIN}
    // try the correct place first, if not there, lets try the old, wrong place
    // if at neither, we go back to correct place.
    Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/TomboyReborn/Notes/';
    if DirectoryExistsUTF8(Result) then exit;
    Result := GetEnvironmentVariable('HOME') + '/.local/share/TomboyReborn/';
    if not DirectoryExistsUTF8(Result) then
        Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/TomboyReborn/Notes/';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('APPDATA') + '\TomboyReborn\notes\';
    // %APPDATA%\Tomboy\notes\
    {$ENDIF}
    ForceDirectoriesUTF8(Result);
end;

function GetLocalNoteFile(NoteID : UTF8String; altrep : UTF8String = ''): UTF8String;
begin
    altrep := chomppathdelim(altrep);
    if(length(altrep)>0)
    then begin
      ForceDirectoriesUTF8(altrep);
      Result := altrep + PathDelim + NoteID + '.note';
    end else Result := NotesDir + NoteID + '.note';
end;

function GetLocalBackupPath(): UTF8String;
begin
    Result := NotesDir + 'Backup' + PathDelim;
end;

function GetTempFile() : UTF8String;
var
    d : UTF8String;
begin
    d := GetTempDir(true);
    Result := GetTempFileName(d, 'TB_');
end;

function ConfigWrite(source : UTF8String) : boolean;
var
    f : TINIFile;
    i : integer;
    autos : TAutoStartCtrl;
begin
    Result := True;
    TRlog('ConfigWrite '+source);

    try
      DeleteFileUTF8(ConfigFile);
      f :=  TINIFile.Create(ConfigFile);
    except on E: Exception do begin
       showmessage('Unable to write config '+ ConfigFile);
       exit(False);
       end;
    end;

    f.writestring('Basic', 'NotesDir', NotesDir);

    f.writestring('Basic', 'ManyNotebooks', BoolToStr(ManyNoteBooks, true) );
    f.WriteString('Basic', 'Autostart', BoolToStr(Autostart, true));
    f.WriteString('Basic', 'ShowSearchAtStart', BoolToStr(SearchAtStart, true));

    f.writestring('Fonts', 'UsualFont', UsualFont);
    f.writestring('Fonts', 'FixedFont', FixedFont);
    f.writestring('Fonts', 'Scale', IntToStr(FontScale));

    f.writestring('Colors', 'BackGndColour', ColorToString(BackGndColour));
    f.writestring('Colors', 'HiColour', ColorToString(HiColour));
    f.writestring('Colors', 'TextColour', ColorToString(TextColour));
    f.writestring('Colors', 'TitleColour', ColorToString(TitleColour));


    f.WriteString('Sync', 'Autosync', IntToStr(SyncRepeat));
    case SyncClashOption of
         TSyncClashOption.AlwaysAsk :  f.writestring('Sync', 'ClashOption', 'AlwaysAsk');
         TSyncClashOption.UseLocal :   f.writestring('Sync', 'ClashOption', 'UseLocal');
         TSyncClashOption.UseServer :  f.writestring('Sync', 'ClashOption', 'UseServer');
	 TSyncClashOption.MakeCopy :   f.writestring('Sync', 'ClashOption', 'MakeCopy');
    end;

    if(SyncType = TSyncTransport.SyncNone)
    then f.writestring('Sync', 'Type', 'none')
    else if(SyncType = TSyncTransport.SyncFile )
       then f.writestring('Sync', 'Type', 'file')
       else f.writestring('Sync', 'Type', 'nextcloud');

    f.writestring('Sync', 'FileRepo', SyncFileRepo);
    f.writestring('Sync', 'NCURL', SyncNCUrl);
    f.writestring('Sync', 'NCKey', SyncNCKey);
    f.writestring('Sync', 'NCToken', SyncNCToken);
    f.writestring('Sync', 'NCSecret', SyncNCSecret);

    f.writestring('Spelling', 'Library', DictLibrary);
    f.writestring('Spelling', 'DictPath', DictPath);
    f.writestring('Spelling', 'DictFile', DictFile);

    f.writestring('UI', 'LastUsed', IntToStr(LastUsedNB));
    i:=0;
    while((i<LastUsedNB) and (i<LastUsed.Count)) do
    begin
         f.writestring('UI', 'Latest'+IntToStr(i), LastUsed.Strings[i]);
         inc(i);
    end;

    f.Free;

    autos := TAutoStartCtrl.Create('tomboy-reborn', AutoStart);
    if autos.ErrorMessage <> '' then
          ShowMessage('Error setting autstart' + Autos.ErrorMessage);
    FreeAndNil(Autos);

    TRlog('ConfigWrite DONE '+source);
end;

function ConfigRead(source : UTF8String) : integer;
var
    f : TINIFile;
    i : integer;
    s : UTF8String;
begin

    TRlog('ConfigRead(' + source + ')');

    if FileExistsUTF8(ConfigFile) then
    begin
       try
          f :=  TINIFile.Create(ConfigFile);
       except on E: Exception do
          begin
             TRlog(E.Message);
             exit(-1);
          end;
       end;

       NotesDir:= f.readstring('Basic', 'NotesDir', GetDefaultNotesDir());

       ManyNoteBooks        := StrToBool(f.readstring('Basic', 'ManyNotebooks', 'false'));
       Autostart            := StrToBool(f.readstring('Basic', 'Autostart', 'false'));
       SearchAtStart        := StrToBool(f.readstring('Basic', 'ShowSearchAtStart', 'true'));

       UsualFont            := f.readstring('Fonts', 'UsualFont', GetDefaultUsualFont());
       FixedFont            := f.readstring('Fonts', 'FixedFont', GetDefaultFixedFont());
       try
           FontScale            := StrToInt(f.readstring('Fonts', 'Scale', '100'));
       except on E:Exception do
           FontScale            := 100;
       end;

       BackGndColour := StringToColor(f.ReadString('Colors', 'BackGndColour',ColorToString(clCream)));
       HiColour      := StringToColor(f.ReadString('Colors', 'HiColour',ColorToString(clYellow)));
       TextColour    := StringToColor(f.ReadString('Colors', 'TextColour',ColorToString(clBlack)));
       TitleColour   := StringToColor(f.ReadString('Colors', 'TitleColour',ColorToString(clBlue)));

       SyncRepeat       := StrToInt(f.ReadString('Sync', 'Autosync', '0'));
       SyncClashOption  := TSyncClashOption.AlwaysAsk;
       case f.ReadString('Sync', 'ClashOption', 'AlwaysAsk') of
              'UseLocal'  : SyncClashOption := TSyncClashOption.UseLocal;
              'UseServer' : SyncClashOption := TSyncClashOption.UseServer;
              'MakeCopy'  : SyncClashOption := TSyncClashOption.MakeCopy;
       end;

       SyncType         := TSyncTransport.SyncNone;
       case f.ReadString('Sync', 'Type','none') of
              'file'      : SyncType := TSyncTransport.SyncFile;
              'nextcloud' : SyncType := TSyncTransport.SyncNextCloud;
       end;

       SyncFileRepo     := f.ReadString('Sync', 'FileRepo', '');
       SyncNCUrl        := f.ReadString('Sync', 'NCUrl', '');
       SyncNCKey        := f.ReadString('Sync', 'NCKey', '');
       SyncNCToken      := f.ReadString('Sync', 'NCToken', '');
       SyncNCSecret     := f.ReadString('Sync', 'NCSecret', '');

       DictLibrary      := f.ReadString('Spelling', 'Library',GetDictDefaultLibrary());
       DictPath         := f.ReadString('Spelling', 'DictPath', GetDictDefaultPath());
       DictFile         := f.ReadString('Spelling', 'DictFile', DictFile);
       DictPath         := AppendPathDelim(ChompPathDelim(DictPath));

       LastUsedNB       := StrToInt(f.ReadString('UI', 'LastUsed', '10'));
       i:=0; LastUsed.Clear;
       while(i<LastUsedNB) do
       begin
          s := f.ReadString('UI', 'Latest'+IntToStr(i), '');
          if(length(s)>0) then LastUsed.Add(Trim(s));
          inc(i);
       end;

       FreeAndNil(f);

       Result:= 1;
    end else begin

    // No config file
        GetDefaultNotesDir();

        ManyNoteBooks := false;
        Autostart := true;
        SearchAtStart := false;

        SyncRepeat := 0;
        SyncClashOption  := TSyncClashOption.AlwaysAsk;

        SyncRepeat      := 0;
        SyncType        := TSyncTransport.SyncNone;
        SyncClashOption := TSyncClashOption.AlwaysAsk;
        SyncFileRepo    := '';
        SyncNCUrl       := '';
        SyncNCKey       := '';
        SyncNCToken     := '';
        SyncNCSecret    := '';

        FixedFont := GetDefaultFixedFont();
        UsualFont := GetDefaultUsualFont();
        FontScale := 100;

        DictLibrary      := GetDictDefaultLibrary();
        DictPath         := GetDictDefaultPath();
        DictFile         := '';

        BackGndColour := clCream;
        HiColour := clYellow;
        TextColour := clBlack;
        TitleColour := clBlue;

        LastUsedNB :=10;
        LastUsed.Clear;

        ConfigWrite('CheckConfigFile');

        Result := 0;
    end;

    TRlog('ConfigRead(' + source + ') DONE');
end;



{ ===== NOTE FUNCTIONS ==== }

function GetNewID() : UTF8String;
var
  GUID : TGUID;
  s : UTF8String;
begin
   CreateGUID(GUID);
   s := copy(GUIDToString(GUID), 2, 36);
   Result := UTF8LowerCase(s);
end;

function NoteIDLooksOK(const ID : UTF8String) : boolean;
begin
    if length(ID) <> 36 then exit(false);
    if pos('-', ID) <> 9 then exit(false);
    result := True;
end;

function NoteContains(const TermList: TStringList; N : PNoteInfo ; CaseSensitive : boolean = false): boolean;
var
  ok : boolean;
  i :integer;
  a,b,c : UTF8String;

begin
   if(not assigned(N)) then exit(false);

   b := N^.Title;
   c := RemoveXML(N^.Content);

   if(not CaseSensitive) then
       begin
            b := UTF8LowerCase(b);
            c := UTF8LowerCase(c);
       end;

   ok := true;
   i :=0;

   while(ok and (i<TermList.Count))
   do begin
      a := TermList.Strings[i];
      if(not CaseSensitive) then a:= LowerCase(a);
      if((UTF8Pos(a,b)<1) and (UTF8Pos(a,c)<1)) then ok:=false;
      inc(i);
   end;
   Result := ok;
end;

function NoteBelongs(const notebook : UTF8String ; N : PNoteInfo ): boolean;
var
   i : integer;
   ok : boolean;
   s : UTF8String;
begin

   if(length(notebook) = 0) then exit(true);

   i:=0;

   if(CompareText(notebook,'-') =0 )
   then begin
      ok := true;
      while(ok and (i<N^.Tags.Count))
      do begin
         s:= N^.Tags.Strings[i];
         inc(i);
         if(CompareText('system:notebook:',Copy(s,1,16)) <> 0) then continue;
         s:= Trim(Copy(s,17));
         if(length(s)>0) then ok:=false;
      end;
      exit(ok);
   end;

   ok := false;
   while((not ok) and (i<N^.Tags.Count))
   do begin
      s:= N^.Tags.Strings[i];
      inc(i);
      if(CompareText('system:notebook:',Copy(s,1,16)) <> 0) then continue;
      if(CompareText(notebook,Trim(Copy(s,17))) = 0) then ok:= true;
   end;
   Result := ok;
end;

procedure CopyNote(A : PNoteInfo; c : PNoteInfo);
var
   i : integer;
begin

  TRlog('Copy note '+A^.ID);

  c^.ID := A^.ID;
  c^.CreateDate := A^.CreateDate;
  c^.CreateDateGMT := A^.CreateDateGMT;
  c^.LastChange := A^.LastChange;
  c^.LastChangeGMT := A^.LastChangeGMT;
  c^.LastMetaChange := A^.LastMetaChange;
  c^.LastMetaChangeGMT := A^.LastMetaChangeGMT;
  c^.Version := A^.Version;
  c^.Rev := A^.Rev;
  c^.LastSync := A^.LastSync;
  c^.LastSyncGMT := A^.LastSyncGMT;
  c^.Action := A^.Action;
  c^.Title := A^.Title;
  c^.Content := A^.Content;
  c^.OpenOnStartup := A^.OpenOnStartup;
  c^.Pinned := A^.Pinned;
  c^.CursorPosition := A^.CursorPosition;
  c^.SelectBoundPosition := A^.SelectBoundPosition;
  c^.Width := A^.Width;
  c^.Height := A^.Height;
  c^.X := A^.X;
  c^.Y := A^.Y;
  c^.X := A^.X;

  c^.Tags.Clear;
  for i:=0 to A^.Tags.Count -1 do c^.Tags.Add(A^.Tags.Strings[i]);

  c^.Error := A^.Error;
  c^.Deleted := A^.Deleted;

  c^.Display := A^.Display;

end;

function EmptyNote() : PNoteInfo;
var
   n : PNoteInfo;
begin
   new(n);
   n^.Action:=SynUnset;
   n^.ID := GetNewID();
   n^.Rev := -1;
   n^.LastSyncGMT := 0;
   n^.LastSync := '';
   n^.Display := nil;
   n^.Tags := TStringList.Create;

   n^.Deleted := false;
   n^.CreateDate := GetCurrentTimeStr();
   n^.CreateDateGMT := GetGMTFromStr(n^.CreateDate);

   n^.LastChange := GetCurrentTimeStr();
   n^.LastChangeGMT := GetGMTFromStr(n^.LastChange);
   n^.LastMetaChange := GetCurrentTimeStr();
   n^.LastMetaChangeGMT := GetGMTFromStr(n^.LastMetaChange);

   n^.Version := '0.3';
   n^.Title := 'New Note '+ n^.CreateDate;
   n^.Content := n^.Title + rsLineBreak + rsLineBreak + 'Empty note';

   n^.X := 0;
   n^.Y := 0;

   n^.width := 400;
   n^.height := 300;

   n^.CursorPosition := 0;
   n^.SelectBoundPosition :=0;

   n^.Pinned := false;
   n^.OpenOnStartup := false;

   n^.Error := '';
   n^.Deleted := false;
   n^.Display := nil;

   n^.Scan := 0;
   Result := n;
end;

function ReplaceAngles(const Str : UTF8String) : UTF8String;
var
    s : UTF8String;
begin
   s := UTF8StringReplace(Str,'&lt;','<',[rfReplaceAll]);
   s := UTF8StringReplace(s,'&gt;','>',[rfReplaceAll]);
   s := UTF8StringReplace(s,'&#x9;',#9,[rfReplaceAll]);
   s := UTF8StringReplace(s,'&apos;','''',[rfReplaceAll]);
   s := UTF8StringReplace(s,'&quot;','"',[rfReplaceAll]);
   Result := UTF8StringReplace(s,'&amp;','&',[rfReplaceAll]);
end;

function EncodeAngles(const Str : UTF8String) : UTF8String;
var
    s : UTF8String;
begin
   s := UTF8StringReplace(Str,'<','&lt;',[rfReplaceAll]);
   s := UTF8StringReplace(s,'>','&gt;',[rfReplaceAll]);
   s := UTF8StringReplace(s,#9,'&#x9;',[rfReplaceAll]);
   s := UTF8StringReplace(s,'''','&apos;',[rfReplaceAll]);
   s := UTF8StringReplace(s,'"','&quot;',[rfReplaceAll]);
   Result := UTF8StringReplace(s,'&','&amp;',[rfReplaceAll]);
end;

function NoteToFile(note : PNoteInfo; filename : UTF8String) : boolean;
var
   f : TStringList;
   ok : boolean;
   i : integer;
begin
   TRlog('Note to file ' + note^.ID + ' into ' + filename );

   ok:=false;

   f := TStringList.Create;

   f.Add('<?xml version="1.0" encoding="utf-8"?>');
   f.Add('<note version="' + note^.Version + '">');
   f.Add('<title>' + EncodeAngles(note^.Title) + '</title>');
   f.Add('<create-date>' + note^.CreateDate + '</create-date>');
   f.Add('<last-change-date>' + note^.LastChange + '</last-change-date>');
   f.Add('<last-metadata-change-date>' + note^.LastMetaChange + '</last-metadata-change-date>');
   f.Add('<width>' + IntToStr(note^.Width) + '</width>');
   f.Add('<height>' + IntToStr(note^.Height) + '</height>');
   f.Add('<x>' + IntToStr(note^.X) + '</x>');
   f.Add('<y>' + IntToStr(note^.Y) + '</y>');
   f.Add('<selection-bound-position>' + IntToStr(note^.SelectBoundPosition) + '</selection-bound-position>');
   f.Add('<cursor-position>' + IntToStr(note^.CursorPosition) + '</cursor-position>');
   f.Add('<pinned>' + BoolToStr(note^.Pinned) + '</pinned>');
   f.Add('<open-on-startup>' + BoolToStr(note^.OpenOnStartup) + '</open-on-startup>');
   f.Add('<text xml:space="preserve"><note-content version="' + note^.Version + '">' + note^.Content + '</note-content></text>');
   f.Add('<tags>');
   i:=0;
   while(i<note^.Tags.Count)
   do begin
      TRlog('Adding tag : '+note^.Tags[i]);
      f.Add('<tag>'+note^.Tags[i]+'</tag>');
      inc(i);
   end;
   f.Add('</tags>');
   f.Add('</note>');

   f.LineBreak := rsLineBreak;

   try
      f.SaveToFile(filename);
      ok :=true;
   except on E:Exception do TRlog(E.message);
   end;

   f.Free;
   TRlog('Note to file end');
   result := ok;
end;

function FileToNote(filename : UTF8String; NoteInfo : PNoteInfo) : boolean;
var
    Doc : TXMLDocument;
    Node,Child : TDOMNode;
    NodeList : TDOMNodeList;
    i,j,k : integer;
    xmlstream : TFileStream;
    ts : TStringStream;
    ok : boolean;
    data : UTF8String;
begin
   TRlog('File to note '+filename);

   ok :=true;

   filename := UTF8Trim(filename);

   try
      xmlstream := TFileStream.Create(filename,fmOpenRead);
      i := xmlstream.Size;
      SetLength(data, i);
      xmlstream.Read(data[1], i);
      xmlstream.Free;
   except on E:Exception do
       begin
           TRlog('File to note : Error Reading '+filename);
           TRlog(E.message);
           NoteInfo^.Error := E.message;
           NoteInfo^.Action:= SynClash;
           exit(false);
       end;
   end;

   i := UTF8Pos('<note-content',data);
   j := UTF8Pos('>',data,i+1);
   k := UTF8RPos('</note-content>',data);

   if((i<1) or (k<1) or (j>k))
   then begin
      data := 'Invalid data from '+filename;
      TRlog(data);
      NoteInfo^.Error := data;
      NoteInfo^.Action:= SynClash;
      exit(false);
  end;

  NoteInfo^.Content:= UTF8Copy(data,j+1,k-j-1);
  NoteInfo^.Version := '0.3';

  data := UTF8Copy(data,1,j) + UTF8Copy(data,k,UTF8Length(data)-k+1);

  ts := TStringStream.Create(data);

  try
       ReadXMLFile(Doc,ts);
   except on E:Exception do
       begin
           TRlog('File to note : XML data wrong from '+filename);
           TRlog(E.message);
           NoteInfo^.Error := E.message;
           NoteInfo^.Action:= SynClash;
           xmlstream.Free;
           Doc.Free;
           exit(false);
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('text');
     if(assigned(Node)) then
     begin
        Node := Node.FindNode('note-content');
        if(assigned(Node)) then
        begin
           Child := Node.Attributes.GetNamedItem('version');
           if(Assigned(Child)) then NoteInfo^.Version := Child.NodeValue;
        end;
        Node.Free;
     end;
   except on E:Exception do
       begin
         TRlog('Error CONTENT FileToNote '+filename);
         NoteInfo^.Error := rsInvalidNoteContent + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;


   try
     Node := Doc.DocumentElement.FindNode('create-date');
     NoteInfo^.CreateDate := '';
     if(assigned(Node)) then NoteInfo^.CreateDate := Node.FirstChild.NodeValue;
     if NoteInfo^.CreateDate = '' then NoteInfo^.CreateDate := GetCurrentTimeStr();
     NoteInfo^.CreateDateGMT := GetGMTFromStr(NoteInfo^.CreateDate);
     if(assigned(Node)) then Node.Free;
   except on E:Exception do
       begin
         TRlog('Error CREATEDATE FileToNote '+filename);
         NoteInfo^.Error := rsInvalidCreateDAte+ '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('last-change-date');
     NoteInfo^.LastChange := '';
     if(assigned(Node)) then NoteInfo^.LastChange := Node.FirstChild.NodeValue;
     if NoteInfo^.LastChange = '' then NoteInfo^.LastChange := GetCurrentTimeStr();
     NoteInfo^.LastChangeGMT := GetGMTFromStr(NoteInfo^.LastChange);
     if(assigned(Node)) then Node.Free;
   except on E:Exception do
       begin
         TRlog('Error LASTCHANGEDATE FileToNote '+filename);
         NoteInfo^.Error := rsInvalidLastChangeDate+ '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('last-metadata-change-date');
     NoteInfo^.LastMetaChange := '';
     if(assigned(Node)) then NoteInfo^.LastMetaChange := Node.FirstChild.NodeValue;
     if NoteInfo^.LastMetaChange = '' then NoteInfo^.LastMetaChange := GetCurrentTimeStr();
     NoteInfo^.LastMetaChangeGMT := GetGMTFromStr(NoteInfo^.LastMetaChange);
     if(assigned(Node)) then Node.Free;
   except on E:Exception do
       begin
         TRlog('Error LASTMETADATE FileToNote '+filename);
         NoteInfo^.Error := rsInvalidLastMetaChangeDate+ '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('title');
     if(assigned(Node))
     then begin NoteInfo^.Title := ReplaceAngles(Node.FirstChild.NodeValue); Node.Free; end
     else NoteInfo^.Title := 'Note ' + NoteInfo^.ID;
   except on E:Exception do
       begin
         TRlog('Error TITLE FileToNote '+filename);
         NoteInfo^.Error := rsInvalidTitle+ '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('cursor-position');
     NoteInfo^.CursorPosition := 0;
     if(assigned(Node))
     then begin NoteInfo^.CursorPosition := StrToInt(Node.FirstChild.NodeValue); Node.Free; end;
   except on E:Exception do
       begin
         TRlog('Error CURSORPOSITION FileToNote '+filename);
         NoteInfo^.Error := rsInvalidCursorPosition + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('selection-bound-position');
     NoteInfo^.SelectBoundPosition := 0;
     if(assigned(Node))
     then begin NoteInfo^.SelectBoundPosition := StrToInt(Node.FirstChild.NodeValue); Node.Free; end;
   except on E:Exception do
       begin
         TRlog('Error BOUDPOSITION FileToNote '+filename);
         NoteInfo^.Error := rsInvalidBoundPosition + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('width');
     NoteInfo^.Width := 0;
     if(assigned(Node))
     then begin NoteInfo^.Width := StrToInt(Node.FirstChild.NodeValue); Node.Free; end;
   except on E:Exception do
       begin
         TRlog('Error WIDTH FileToNote '+filename);
         NoteInfo^.Error := rsInvalidWidth + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('height');
     NoteInfo^.Height := 0;
     if(assigned(Node))
     then begin NoteInfo^.Height := StrToInt(Node.FirstChild.NodeValue); Node.Free; end;
   except on E:Exception do
       begin
         TRlog('Error HEIGHT FileToNote '+filename);
         NoteInfo^.Error := rsInvalidHeight + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('x');
     NoteInfo^.X := 0;
     if(assigned(Node))
     then begin NoteInfo^.X := StrToInt(Node.FirstChild.NodeValue); Node.Free; end;
   except on E:Exception do
       begin
         TRlog('Error POSX FileToNote '+filename);
         NoteInfo^.Error := rsInvalidPosX + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('y');
     NoteInfo^.Y := 0;
     if(assigned(Node))
     then begin NoteInfo^.Y := StrToInt(Node.FirstChild.NodeValue); Node.Free; end;
   except on E:Exception do
       begin
         TRlog('Error POSY FileToNote '+filename);
         NoteInfo^.Error := rsInvalidPosY + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('open-on-startup');
     NoteInfo^.OpenOnStartup := false;
     if(assigned(Node))
     then begin NoteInfo^.OpenOnStartup := StrToBool(Node.FirstChild.NodeValue); Node.Free; end;
   except on E:Exception do
       begin
         TRlog('Error OPENSTART FileToNote '+filename);
         NoteInfo^.Error := rsInvalidOpenOnStartup + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     Node := Doc.DocumentElement.FindNode('pinned');
     NoteInfo^.Pinned := false;
     if(assigned(Node))
     then begin NoteInfo^.Pinned := StrToBool(Node.FirstChild.NodeValue); Node.Free; end;
   except on E:Exception do
       begin
         TRlog('Error PINNED FileToNote '+filename);
         NoteInfo^.Error := rsInvalidPinned + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   try
     NoteInfo^.Tags.Clear;
     Node := Doc.DocumentElement.FindNode('tags');
     if(assigned(Node)) then
     begin
        NodeList := Node.ChildNodes;
        for j := 0 to NodeList.Count-1 do
        begin
           //Trlog('Found : '+NodeList.Item[j].TextContent);
           NoteInfo^.Tags.Add(NodeList.Item[j].TextContent);
        end;
        Node.Free;
     end;
   except on E:Exception do
       begin
         TRlog('Error TAGS FileToNote '+filename);
         NoteInfo^.Error := rsInvalidTags + '('+E.Message+')';
         NoteInfo^.Action:= SynClash;
         TRlog(E.message);
         ok := false;
       end;
   end;

   NoteInfo^.Deleted := false;
   ts.Free;
   Doc.Free;

   Result := ok;
end;

function NoteTimeOrder(Item1: Pointer;Item2: Pointer):Integer;
var
    n1,n2 : PNoteInfo;
    d1,d2 : TDateTime;
begin

  n1 := PNoteInfo(Item1);
  n2 := PNoteInfo(Item2);

  d1 := n1^.LastChangeGMT; if (d1 = 0) then d1 := n1^.CreateDateGMT;
  d2 := n2^.LastChangeGMT; if (d2 = 0) then d2 := n2^.CreateDateGMT;

  if(d1<d2) then exit(1);
  if(d1>d2) then exit(-1);

  Result := 0;
end;

{ ===== XML ===== }

function RemoveXml(const St : UTF8String) : UTF8String;
var
    X, Y : integer;
    FoundOne : boolean = false;
begin
    Result := St;
    repeat
        FoundOne := False;
        X := UTF8Pos('<', Result);
        if X > 0 then begin
            Y := UTF8Pos('>', Result);
            if Y > X then begin
                UTF8Delete(Result, X, Y-X+1);
                FoundOne := True;
            end;
        end;
    until not FoundOne;
end;


function isURL(u : UTF8String) : boolean;
begin
  u := Trim(u);
  if(CompareText('http://',Copy(u,1,7))=0) then exit(true);
  if(CompareText('https://',Copy(u,1,8))=0) then exit(true);
  if(CompareText('ftp://',Copy(u,1,6))=0) then exit(true);
  if(CompareText('mailto://',Copy(u,1,9))=0) then exit(true);
  Result := false;
end;


function isNoteLink(u : UTF8String) : boolean;
begin
  u := Trim(u);
  if(CompareText('note://',Copy(u,1,7))=0) then exit(true);
  Result := false;
end;

function CleanTitle(u: UTF8String) : UTF8String;
var
   i : integer;
   s : AnsiString;
begin
  i:=0;
  while(i<Length(u))
  do begin
     s := u;
     if(s.Chars[i]<' ') then u := StringReplace(u, Copy(s,i+1,1),' ',[rfReplaceAll]);
     inc(i);
  end;
  result := u;
end;

{ ===== DATETIME ==== }

function GetTimeFromGMT(d : TDateTime) : UTF8String;
begin;
    Result := FormatDateTime('YYYY-MM-DD',d) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000+00:00"',d);

end;

function GetDisplayTimeFromGMT(d : TDateTime) : UTF8String;
var
   offset : double;
begin;
  offset := GetLocalTimeOffset; // minutes
  offset := offset / 1440.0;
  d := d - offset;

  Result := FormatDateTime('YYYY/MM/DD hh:mm:ss',d);

end;

function GetCurrentTimeStr(): UTF8String;
var
   n : TDateTime;
   Res : UTF8String;
   Off : longint;
begin
    n:=Now;
    Result := FormatDateTime('YYYY-MM-DD',n) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000"',n);
    Off := GetLocalTimeOffset();
    if (Off div -60) >= 0 then Res := '+'
        else Res := '-';
        if abs(Off div -60) < 10 then Res := Res + '0';
        Res := Res + inttostr(abs(Off div -60)) + ':';
        if (Off mod 60) = 0 then
                Res := res + '00'
        else Res := Res + inttostr(abs(Off mod 60));
    Result := Result + res;
end;

function GetGMTFromStr(const DateStr: UTF8String): TDateTime;
var
    TimeZone : TDateTime;
begin
    if DateStr = '' then exit(0);

    if length(DateStr) <> 33 then exit(0);

    try
       if not TryEncodeTimeInterval( strtoint(copy(DateStr, 29, 2)),strtoint(copy(DateStr, 32, 2)),0,0,TimeZone)
       then TRlog('Fail on interval encode ');
    except on EConvertError do begin
       TRlog('FAIL on converting time interval ' + DateStr);
       TRlog('Hour ' + copy(DateStr, 29, 2) + ' minutes ' + copy(DateStr, 32, 2));
       end;
    end;

    try
       if not TryEncodeDateTime(strtoint(copy(DateStr, 1, 4)),   	// Year
          strtoint(copy(DateStr, 6, 2)),              // Month
	  strtoint(copy(DateStr, 9, 2)),				// Day
	  strtoint(copy(DateStr, 12, 2)),				// Hour
	  strtoint(copy(DateStr, 15, 2)),				// Minutes
	  strtoint(copy(DateStr, 18, 2)),				// Seconds
	  strtoint(copy(DateStr, 21, 3)),				// mSeconds
	  Result)
       then TRlog('Fail on date time encode ');
    except on EConvertError do
       begin
          TRlog('FAIL on converting date time ' + DateStr);
          exit(0.0);
       end;
    end;

    try
       if DateStr[28] = '+'
       then Result := Result - TimeZone
       else if DateStr[28] = '-'
          then Result := Result + TimeZone
	  else TRlog('******* Bugger, we are not parsing DATE UTF8String ********');
    except on EConvertError do
       begin
       TRlog('FAIL on calculating GMT ' + DateStr);
       exit(0.0);
       end;
    end;
end;


{ ===== FONT ==== }


function GetDefaultFixedFont() : UTF8String;
    var  T : UTF8String;
    FontNames : array[1..7] of UTF8String
      = ('Monospace', 'Monaco', 'Nimbus Mono L', 'Liberation Mono', 'Lucida Console', 'Lucida Sans Typewriter', 'Courier New' );

    f : TForm;

    function IsMono(FontName : UTF8String) : boolean;
    begin
      f.Canvas.Font.Name := FontName;
      TRlog('IsMono '+FontName);
      result := f.Canvas.TextWidth('i') = f.Canvas.TextWidth('w');
    end;

    function IsDifferentSizes() : boolean;
    var
        ASize : integer;
    begin
        f.Canvas.Font.Size := 13;
        ASize := f.Canvas.TextHeight('H');
        f.Canvas.Font.Size := 14;
        if ASize = f.Canvas.TextHeight('H')
            then exit(False);
        ASize := f.Canvas.TextHeight('H');
        f.Canvas.Font.Size := 15;
        If ASize = f.Canvas.TextHeight('H')
            then exit(False);
        result := True;
    end;

begin
    f := TForm.Create(nil);
    Result := '';
    for T in FontNames do begin
        if not IsMono(T) then continue;
        if not IsDifferentSizes() then continue;
        TRlog('Found : '+T);
        Result := T;
        break;
    end;
    FreeAndNil(f);
end;

function GetDefaultUsualFont() : UTF8String;
var
    f : TForm;
begin
    f := TForm.Create(nil);
    Result := GetFontData(f.Font.Handle).Name;
    TRlog('DefaultUsualFont = ' + Result);
    FreeAndNil(f);
end;


{ ===== NETWORK ==== }

type TRSockecktHandler = class(TObject)
public
    procedure HttpClientGetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
end;

procedure TRSockecktHandler.HttpClientGetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  If UseSSL then begin
     {$ifdef VER3_2} AHandler:=TOpenSSLSocketHandler.Create;
     TOpenSSLSocketHandler(AHandler).SSLType := stTLSv1_2; {$endif}
     {$ifndef VER3_2} AHandler:=TSSLSocketHandler.Create;
     TSSLSocketHandler(AHandler).SSLType := stTLSv1_2; {$endif}

  end else AHandler := TSocketHandler.Create;
end;

function URLDecode(s: UTF8String): UTF8String;
var
  i,lengthsource: integer;
  source: PAnsiChar;
begin
  result := '';
  source := pansichar(s);
  lengthsource := length(source);
  i:=1;
  while (i<=lengthsource) do
    begin
      if source[i-1] <> '%' then
        result := result + source[i-1]
      else if (source[i-1] = '%') and (i+1<=lengthsource) then
        try
          begin
            result := result + Chr(StrToInt('0x'+source[i]+source[i+1]));
            i:=i+2;
          end;
        except
        end
      else
        result := result + source[i-1];
      inc(i);
    end;
end;

function URLEncode(s: UTF8String): UTF8String;
var
  i: integer;
  source: PAnsiChar;
begin
  Result := '';
  source := pansichar(s);
  i :=0;
  while(i<length(source)) do begin
    if (source[i] = ' ') then Result := Result + '+'
    else if not (source[i] in ['A'..'Z', 'a'..'z', '0'..'9', '-', '_', '.']) then
      Result := Result + '%' + IntToHex(ord(source[i]), 2)
    else
      Result := Result + source[i];
    i := i + 1;
  end;
end;

function WebPut(u : UTF8String; params : TStrings; data : UTF8String) : UTF8String;
var
  Client: TFPHttpClient;
  res : TStringStream;
  i : integer;
  handler : TRSockecktHandler;
  auth : UTF8String;
begin

  Client := TFPHttpClient.Create(nil);
  Client.OnGetSocketHandler := @handler.HttpClientGetSocketHandler;
  Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  Client.AllowRedirect := true;

  res := TStringStream.Create('');

  i:=0;
  auth :='';
  while(i<params.Count) do begin
    auth := auth + ',' + params[i] + '="' + URLEncode(params[i+1]) + '"';
    i := i +2;
  end;

  auth := 'OAuth realm="Snowy"'+auth;
  TRlog('Adding headers : '+auth);

  Client.AddHeader('Authorization',auth);
  Client.AddHeader('content-type', 'application/json');
  Client.RequestBody := TStringStream.Create(data);

  try
    Client.HTTPMethod('PUT',u,res,[]);
    Result := res.DataString;
  except on E:Exception do begin
    MainErrorString := E.message;
    Result := '';
    end;
  end;
  FreeAndNil(Client);
  FreeAndNil(res);
end;

function WebGet(u : UTF8String; params : TStrings) : UTF8String;
var
  Client: TFPHttpClient;
  i : integer;
  handler : TRSockecktHandler;
begin
  Client := TFPHttpClient.Create(nil);
  Client.OnGetSocketHandler := @handler.HttpClientGetSocketHandler;
  Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  Client.AllowRedirect := true;

  i:=0;
  while(i<params.Count) do begin
    if(i=0) then u := u + '?' else u := u + '&';
    u := u + URLEncode(params[i]) + '=' + URLEncode(params[i+1]);
    i := i +2;
  end;

  try
    Result := Client.Get(u);
  except on E:Exception do begin
    MainErrorString := E.message;
    Result :='';
    end;
  end;
  Client.Free;
end;


function WebPost(u : UTF8String; params : TStrings) : UTF8String;
var
  Client: TFPHttpClient;
  res : TStringStream;
  p : TStrings;
  i : integer;
  handler : TRSockecktHandler;

begin

  Client := TFPHttpClient.Create(nil);
  Client.OnGetSocketHandler := @handler.HttpClientGetSocketHandler;
  Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  Client.AllowRedirect := true;

  res := TStringStream.Create('');

  p := TStringList.Create();
  i:=0;
  while(i<params.Count) do begin
    p.Add(Format('%s=%s',[params[i],params[i+1]]));
    i := i +2;
  end;

  try
    Client.FormPost(u,p,res);
    Result := res.DataString;
  except on E:Exception do begin
    MainErrorString := E.message;
    Result := '';
    end;
  end;
  FreeAndNil(Client);
  FreeAndNil(p);
  FreeAndNil(res);
end;


{ ===== OAUTH 1.0 ==== }

procedure OauthBaseParams(const p : TStrings; Key : UTF8String; Token : UTF8String = ''; Verifier : UTF8String = '');
begin
  //OAuth setup
  p.Add('oauth_version');
  p.Add('1.0');
  p.Add('oauth_signature_method');
  p.Add('HMAC-SHA1');
  // NONCE
  p.Add('oauth_nonce');
  p.Add(OauthNonce());
  // TIMESTAMP
  p.Add('oauth_timestamp');
  p.Add(OauthTimestamp());
  // Key
  p.Add('oauth_consumer_key');
  p.Add(Key);
  // Token
  if(length(Token)>0) then begin
	p.Add('oauth_token');
  	p.Add(Token);
  end;
  // Verifier
  if(length(Verifier)>0) then begin
	p.Add('oauth_verifier');
  	p.Add(Verifier);
  end;
end;

function OauthTimestamp() : UTF8String;
begin
  Result := Format('%d',[Trunc((Now - EncodeDate(1970, 1 ,1)) * 24 * 60 * 60)]);
end;

function OauthNonce() : UTF8String;
begin
  Result := Format('%d',[Random(9999999-123400)+123400]);
end;

procedure OauthParamsSort(const params : TStrings) ;
var
  p : TStrings;
  i : integer;
  j : integer;
begin
  p := TStringList.Create();
  i:=0;
  while(i<params.Count) do
    begin
      j := 0;
      while((j<p.Count) and (CompareText(params[i],p[j])>0)) do j:= j +2;

      if(j<p.Count) then
        begin
          p.Insert(j,params[i]);
          p.Insert(j+1,params[i+1]);
        end else  begin
          p.Add(params[i]);
          p.Add(params[i+1]);
        end;
        i := i + 2;
    end;

  params.Clear();
  i:=0;
  while(i<p.Count) do begin
    params.Add(p[i]);
    i := i+1;
  end;
  FreeAndNil(p);
end;


procedure OauthSign(u : UTF8String; mode : UTF8String; params : TStrings; Key,Secret : UTF8String);
var
  data : UTF8String;
  p : UTF8String;
  i : integer;
  j : integer;
  hashkey : UTF8String;
  signature : UTF8String;
  s2 : UTF8String;
  c : UTF8String;
  b64 : UTF8String;
begin
  p :='';
  i:=0;
  while(i<params.Count) do begin
    if(i>0) then p := p + '&' ;
    p := p + params.Strings[i] + '=' + URLEncode(params.Strings[i+1]);
    i := i +2;
  end;

  hashkey := Key + '&' + secret;

  TRlog('HASHKEY = '+hashkey);

  data := mode + '&' + URLEncode(u) + '&' + URLEncode(p);

  signature := HMACSHA1(hashkey, data);

  b64 := 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

  i:=1;
  s2 :='';
  while(i<=length(signature)) do begin
    c := '0x' + copy(signature,i,2);
    j := StrToInt(c);
    s2 := s2 + intToBin(j,8);
    i := i+2;
  end;

  i :=1;
  signature :='';
  while(i<=length(s2)) do begin
    c := '%' + copy(s2,i,6);
    p :='';
    if(length(c)<7) then begin c:= c + '00'; p:=p+'='; end;
    if(length(c)<7) then begin c:= c + '00'; p:=p+'='; end;
    j := StrToInt(c);
    signature := signature + b64[j+1] + p;
    i := i+6;
  end;

  params.Add('oauth_signature');
  params.Add(signature);

end;

{ ===== SPELLING ==== }

function GetDictDefaultPath() : UTF8String;
begin
    {$ifdef WINDOWS}
    Result := 'C:\Program Files\LibreOffice 5\share\extensions\dict-en\';
    {$ENDIF}
    {$ifdef DARWIN}
    Result := '/Library/Spelling/';
    {$endif}
    {$ifdef LINUX}
    Result := '/usr/share/hunspell/';
    {$ENDIF}
end;

function GetDictDefaultLibrary() : UTF8String;
begin
    {$ifdef WINDOWS}
    Result := 'C:\Program Files\LibreOffice 5\share\extensions\dict-en\';
    {$ENDIF}
    {$ifdef DARWIN}
    Result := '/Library/Spelling/';
    {$endif}
    {$ifdef LINUX}
    Result := '/usr/lib/libhunspell.so';
    {$ENDIF}
end;


{ ===== SYNC ==== }

function isSyncConfigured() : boolean;
begin
  TRlog('isSyncCOnf : Testing FILE');
  if(SyncType = TSyncTransport.SyncFile) then exit(length(SyncFileRepo)>0);

  TRlog('isSyncCOnf : Testing NEXT');
  if(SyncType = TSyncTransport.SyncNextCloud) then
    begin
       if(length(SyncNCURL) = 0) then exit(false);
       if(length(SyncNCKey) = 0) then exit(false);
       if(length(SyncNCToken) = 0) then exit(false);
       if(length(SyncNCSecret) = 0) then exit(false);
       exit(true);
    end;

  TRlog('isSyncCOnf : Default NONE');
  exit(false);
end;

function SyncActionName(Act: TSyncAction): UTF8String;
begin
    Result := ' Unknown (Error) ';
    case Act of
        TSyncAction.SynUnset          : Result := rsUndecided;
        TSyncAction.SynNothing        : Result := rsDoNothing;
        TSyncAction.SynUploadNew      : Result := rsNewUploads;
        TSyncAction.SynUpLoadEdit     : Result := rsEditUploads;
        TSyncAction.SynDownloadNew    : Result := rsNewDownloads;
        TSyncAction.SynDownloadEdit   : Result := rsEditDownloads;
        TSyncAction.SynCopy           : Result := rsSynCopies;
        TSyncAction.SynDeleteLocal    : Result := rsLocalDeletes;
        TSyncAction.SynDeleteRemote   : Result := rsRemoteDeletes;
        TSyncAction.SynError          : Result := ' ** ERROR **';
        TSyncAction.SynAllLocal       : Result := ' AllLocal ';
        TSyncAction.SynAllCopy        : Result := ' AllCopy ';
        TSyncAction.SynAllRemote      : Result := ' AllRemote ';
        TSyncAction.SynAllNewest      : Result := ' AllNewest ';
        TSyncAction.SynAllOldest      : Result := ' AllOldest ';
    end;
    while length(result) < 15 do Result := Result + ' ';
end;


{ ========= TNoteInfoList ========= }

function TNoteInfoList.Add(ANote : PNoteInfo) : integer;
begin
  if(not assigned(ANote^.Tags)) then ANote^.Tags := TStringList.Create;
  result := inherited Add(ANote);
end;

function TNoteInfoList.Remove(ANote : PNoteInfo) : integer;
begin
  result := inherited Remove(ANote);
end;


{ This will be quite slow with a big list notes, consider an AVLTree ? }
function TNoteInfoList.FindID(const ID: UTF8String): PNoteInfo;
var
    Index : longint;
begin
    Result := Nil;
    //TRlog('Searching TNoteList ' + LName + ' for ID=' + ID);
    for Index := 0 to Count-1 do begin
        if Items[Index]^.ID = ID then begin
            Result := Items[Index];
            exit()
        end;
    end;
end;

destructor TNoteInfoList.Destroy;
var
I : integer;
begin
  for I := 0 to Count-1 do dispose(Items[I]);
  inherited;
end;

function TNoteInfoList.Get(Index: integer): PNoteInfo;
begin
    Result := PNoteInfo(inherited get(Index));
end;

initialization

TRlog('Init TRcommon');

InitSSLInterface;

end.

