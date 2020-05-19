unit TRcommon;

interface

uses
    Classes, Forms, SysUtils, Dialogs, StdCtrls, LazFileUtils, laz2_DOM,
    ExtCtrls, laz2_XMLRead, DateUtils, fphttpclient, ssockets, sslsockets,
    fpopenssl, openssl, hmac, strutils, IniFiles, LazLogger, Graphics,
    {$ifdef LINUX} Unix, {$endif} LazUTF8,
    FileInfo, TRAutoStartCtrl, Trtexts;


type TFontRange = (FontHuge, FontBig, FontMedium, FontSmall);	// Relating to sync clash pref in config file

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
        ID : ANSIString;            // The 36 char ID
        CreateDate : ANSIString;
        CreateDateGMT : TDateTime;
        LastChange : ANSIString;
        LastChangeGMT : TDateTime;
        LastMetaChange : ANSIString;
        LastMetaChangeGMT : TDateTime;
        Version : String;
        Rev : Integer;
        LastSync: ANSIString;
        LastSyncGMT : TDateTime;
        Action : TSyncAction;
        Title : String;
        Content : String;
        OpenOnStartup : boolean;
        Pinned : boolean;
        CursorPosition : integer;
        SelectBoundPosition : integer;
        Width : integer;
        Height : integer;
        X : integer;
        Y : integer;
        Tags : TStringList;
        Error : String;
        Deleted : boolean;
        FormEdit : ^TForm;
  end;

type PNoteInfo=^TNoteInfo;

type TNoteInfoList =
  class(TList)
    private
     	function Get(Index: integer): PNoteInfo;
    public
        LName : String;
        destructor Destroy; override;
        function Add(ANote : PNoteInfo) : integer;
        function Remove(ANote : PNoteInfo) : integer;
        function FindID(const ID : ANSIString) : PNoteInfo;
        property Items[Index: integer]: PNoteInfo read Get; default;
  end;

type TClashRecord =
      record
         RemoteNote : PNoteInfo;
         LocalNote : PNoteInfo;
      end;


// Environmment
const Backup = 'Backup';
procedure TRlog(s : String);
function AboutString() : String;
function GetDefaultConfigDir() : string;
function GetDefaultNotesDir() : string;
function GetLocalNoteFile(NoteID : string; altrep : String = ''): string;
function GetLocalBackupPath(): string;
function GetTempFile() : string;
function ConfigWrite(source : String) : boolean;
function ConfigRead(source : String) : integer;

// Note generic function
function GetNewID() : String;
function NoteIDLooksOK(const ID : string) : boolean;
procedure FileToNote(filename : String; NoteInfo : PNoteInfo);
function NoteToFile(note : PNoteInfo; filename : String) : boolean;
function RemoveBadXMLCharacters(const InStr : String; DoQuotes : boolean = false) : String;
function RemoveXml(const St : String) : String;
procedure CopyNote(A : PNoteInfo; c : PNoteInfo);
function NoteContains(const TermList: TStringList; N : PNoteInfo ; CaseSensitive : boolean = false): boolean;
function NoteBelongs(const notebook : String ; N : PNoteInfo ): boolean;
function NoteTimeOrder(Item1: Pointer;Item2: Pointer):Integer;

// Font
function GetDefaultFixedFont() : string;
function GetDefaultUsualFont() : string;
procedure setFontSizes(out FontSizeSmall  : Integer; out FontSizeLarge  : Integer; out FontSizeHuge   : Integer; out FontSizeTitle  : Integer; out FontSizeNormal : Integer);


// Datetime
function GetGMTFromStr(const DateStr: ANSIString): TDateTime;
function GetTimeFromGMT(d : TDateTime) : String;
function GetCurrentTimeStr() : String;
function GetDisplayTimeFromGMT(d : TDateTime) : String;


// Spelling
function GetDictDefaultPath() : String;
function GetDictDefaultLibrary() : String;

// Network
function URLDecode(s: String): String;
function URLEncode(s: string): string;
function WebPut(u : String; params : TStrings; data : String) : String;
function WebPost(u : String; params : TStrings) : String;
function WebGet(u : String; params : TStrings) : String;

// Oauth 1.0
const OAuthCallbackUrl = 'http://localhost:8000/tomboy-web-sync/';
procedure OauthBaseParams(const p : TStrings; Key : string; Token : String = ''; Verifier : String = '');
function OauthTimestamp() : String;
function OauthNonce() : String;
procedure OauthParamsSort(const params : TStrings) ;
procedure OauthSign(u : String; mode : String; params : TStrings; Key,Secret : String);

// Sync
function SyncActionName(Act : TSyncAction) : string;
function isSyncConfigured() : boolean;

var
    ConfigDir : String;
    NotesDir : String;
    ConfigFile : String;

    ShowIntLinks,ShowExtLinks, ManyNoteBooks,
      Autostart, SearchAtStart, UseTrayIcon : boolean;

    UsualFont, FixedFont : string;
    FontRange : TFontRange;

    SyncType : TSyncTransport;
    SyncClashOption : TSyncClashOption;

    SyncFileRepo, SyncNCurl, SyncNCKey, SyncNCToken, SyncNCSecret : String;
    SyncRepeat,LastUsedNB : integer;

    DictLibrary, DictPath, DictFile : String;

    BackGndColour, TextColour, HiColour, TitleColour : TColor;

    LastUsed: TStringList; // Latest used notes

    mainWindow : TForm;

    Debug : boolean;

Const
  Placement = 45;

implementation


{ ===== ENVIRONMENT ==== }

procedure TRlog(s : String);
begin
  if(Debug) then DebugLn(s);
end;

function AboutString() : string;
var
   Stg : string;
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

function GetDefaultConfigDir() : string;
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

function GetDefaultNotesDir() : string;
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

function GetLocalNoteFile(NoteID : string; altrep : String = ''): string;
begin
    altrep := chomppathdelim(altrep);
    if(length(altrep)>0)
    then begin
      ForceDirectoriesUTF8(altrep);
      Result := altrep + PathDelim + NoteID + '.note';
    end else Result := NotesDir + NoteID + '.note';
    TRlog('GetLocalNoteFile -> '+Result);

end;

function GetLocalBackupPath(): string;
begin
    Result := NotesDir + 'Backup' + PathDelim;
end;

function GetTempFile() : String;
var
    d : String;
begin
    d := GetTempDir(true);
    Result := GetTempFileName(d, 'TB_');
end;

function ConfigWrite(source : String) : boolean;
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
    f.writestring('Basic', 'ShowIntLinks', BoolToStr(ShowIntLinks, true));
    f.writestring('Basic', 'ShowExtLinks', BoolToStr(ShowExtLinks, true));
    f.WriteString('Basic', 'Autostart', BoolToStr(Autostart, true));
    f.WriteString('Basic', 'ShowSearchAtStart', BoolToStr(SearchAtStart, true));

    f.writestring('Fonts', 'UsualFont', UsualFont);
    f.writestring('Fonts', 'FixedFont', FixedFont);
    case FontRange of
         TFontRange.FontHuge :   f.writestring('Fonts', 'FontRange', 'huge');
         TFontRange.FontBig :    f.writestring('Fonts', 'FontRange', 'big');
         TFontRange.FontMedium : f.writestring('Fonts', 'FontRange', 'medium');
         TFontRange.FontSmall :  f.writestring('Fonts', 'FontRange', 'small');
    end;

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

function ConfigRead(source : String) : integer;
var
    f : TINIFile;
    i : integer;
    s : String;
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
       ShowIntLinks         := StrToBool(f.readstring('Basic', 'ShowIntLinks', 'true'));
       ShowExtLinks         := StrToBool(f.readstring('Basic', 'ShowExtLinks', 'true'));
       Autostart            := StrToBool(f.readstring('Basic', 'Autostart', 'false'));
       SearchAtStart        := StrToBool(f.readstring('Basic', 'ShowSearchAtStart', 'true'));

       UsualFont            := f.readstring('Fonts', 'UsualFont', GetDefaultUsualFont());
       FixedFont            := f.readstring('Fonts', 'FixedFont', GetDefaultFixedFont());
       FontRange := TFontRange.FontMedium;
       case f.readstring('Fonts', 'FontRange', 'medium') of
          'huge'    : FontRange := TFontRange.FontHuge;
          'big'     : FontRange := TFontRange.FontBig;
          'small'   : FontRange := TFontRange.FontSmall;
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

         //TRlog('READSYNCTYPE = '+f.ReadString('Sync', 'Type','none'));

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

        ShowIntLinks := true;
        ShowExtLinks := true;
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
        FontRange := TFontRange.FontMedium;

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

function GetNewID() : String;
var
  GUID : TGUID;
  s : String;
begin
   CreateGUID(GUID);
   s := copy(GUIDToString(GUID), 2, 36);
   Result := LowerCase(s);
end;

function NoteIDLooksOK(const ID : string) : boolean;
begin
    if length(ID) <> 36 then exit(false);
    if pos('-', ID) <> 9 then exit(false);
    result := True;
end;

function NoteContains(const TermList: TStringList; N : PNoteInfo ; CaseSensitive : boolean = false): boolean;
var
  ok : boolean;
  i :integer;
  a,b,c : String;

begin
   if(not assigned(N)) then exit(false);

   b := N^.Title;
   c := RemoveXML(N^.Content);

   if(not CaseSensitive) then
       begin
            b := LowerCase(b);
            c := LowerCase(c);
       end;

   ok := true;
   i :=0;

   while(ok and (i<TermList.Count))
   do begin
      a := TermList.Strings[i];
      if(not CaseSensitive) then a:= LowerCase(a);
      if((Pos(a,b)<1) and (Pos(a,c)<1)) then ok:=false;
      inc(i);
   end;
   Result := ok;
end;

function NoteBelongs(const notebook : String ; N : PNoteInfo ): boolean;
var
   i : integer;
   ok : boolean;
   s : String;
begin

   if(length(notebook) = 0) then exit(true);

   i:=0;

   if(CompareText(notebook,'-') =0 ) then
       begin
          ok := true;
          while(ok and (i<N^.Tags.Count)) do
          begin
             s:= N^.Tags.Strings[i];
             inc(i);
             if(CompareText('system:notebook:',Copy(s,1,16)) <> 0) then continue;
             s:= Trim(Copy(s,17));
             if(length(s)>0) then ok:=false;
          end;
          exit(ok);
       end;

   ok := false;
   while((not ok) and (i<N^.Tags.Count)) do
     begin
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

  if(not assigned(A^.Tags)) then A^.Tags := TStringList.Create;
  c^.Tags := TStringList.Create;
  for i:=0 to A^.Tags.Count -1 do c^.Tags.Add(A^.Tags.Strings[i]);

  c^.Error := A^.Error;
  c^.Deleted := A^.Deleted;

  c^.FormEdit := nil;

end;


function NoteToFile(note : PNoteInfo; filename : String) : boolean;
var
   f : TStringList;
   ok : boolean;
begin
   TRlog('Note to file ' + note^.ID + ' into ' + filename );

   ok:=false;

   f := TStringList.Create;

   f.Add('<?xml version="1.0" encoding="utf-8"?>');
   f.Add('<note version="' + note^.Version + '">');
   f.Add('<title>' + RemoveBadXMLCharacters(note^.Title) + '</title>');
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
   f.Add('<text xml:space="preserve"><note-content version="' + note^.Version + '">' + RemoveBadXMLCharacters(note^.Content) + '</note-content></text>');
   f.Add('</note>');

   f.LineBreak := sLineBreak;

   try
      f.SaveToFile(filename);
      ok :=true;
   except on E:Exception do TRlog(E.message);
   end;

   f.Free;

   result := ok;
end;

procedure FileToNote(filename : String; NoteInfo : PNoteInfo);
var
    Doc : TXMLDocument;
    Node : TDOMNode;
    NodeList : TDOMNodeList;
    j : integer;
    xmlfile : String ;
begin
  xmlfile := Trim(filename);
   TRlog('File to note '+xmlfile);

   try
        ReadXMLFile(Doc, Trim(xmlfile));
   except on E:Exception do
        begin
          TRlog('File to note ERROR '+filename);
          TRlog(E.message);
          NoteInfo^.Error := E.message;
          NoteInfo^.Action:= SynClash;
          FreeAndNil(Doc);
          exit();
        end;
   end;

   NoteInfo^.Tags := TStringList.Create;

     try
        Node := Doc.DocumentElement.FindNode('create-date');
        NoteInfo^.CreateDate := '';
        if(assigned(Node)) then NoteInfo^.CreateDate := Node.FirstChild.NodeValue;
        if NoteInfo^.CreateDate = '' then NoteInfo^.CreateDate := GetCurrentTimeStr();
        NoteInfo^.CreateDateGMT := GetGMTFromStr(NoteInfo^.CreateDate);
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for last-change-date');
        Node := Doc.DocumentElement.FindNode('last-change-date');
        NoteInfo^.LastChange := '';
        if(assigned(Node)) then NoteInfo^.LastChange := Node.FirstChild.NodeValue;
        if NoteInfo^.LastChange = '' then NoteInfo^.LastChange := GetCurrentTimeStr();
        NoteInfo^.LastChangeGMT := GetGMTFromStr(NoteInfo^.LastChange);
        //TRlog('Found ' + NoteInfo^.LastChange);
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for last-metadata-change-date');
        Node := Doc.DocumentElement.FindNode('last-metadata-change-date');
        NoteInfo^.LastMetaChange := '';
        if(assigned(Node)) then NoteInfo^.LastMetaChange := Node.FirstChild.NodeValue;
        if NoteInfo^.LastMetaChange = '' then NoteInfo^.LastMetaChange := GetCurrentTimeStr();
        NoteInfo^.LastMetaChangeGMT := GetGMTFromStr(NoteInfo^.LastMetaChange);
        //TRlog('Found ' + NoteInfo^.LastMetaChange);
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for title');
        Node := Doc.DocumentElement.FindNode('title');
        if(assigned(Node)) then NoteInfo^.Title := Node.FirstChild.NodeValue
        else NoteInfo^.Title := 'Note ' + NoteInfo^.ID;
        //TRlog('Found ' + NoteInfo^.Title);
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for cursor-position');
        Node := Doc.DocumentElement.FindNode('cursor-position');
        NoteInfo^.CursorPosition := 0;
        if(assigned(Node)) then NoteInfo^.CursorPosition := StrToInt(Node.FirstChild.NodeValue);
        //TRlog('Found '+ IntToStr(NoteInfo^.CursorPosition));
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for selection-bound-position');
        Node := Doc.DocumentElement.FindNode('selection-bound-position');
        NoteInfo^.SelectBoundPosition := 0;
        if(assigned(Node)) then NoteInfo^.SelectBoundPosition := StrToInt(Node.FirstChild.NodeValue);
        //TRlog('Found '+ IntToStr(NoteInfo^.SelectBoundPosition));
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for width');
        Node := Doc.DocumentElement.FindNode('width');
        NoteInfo^.Width := 0;
        if(assigned(Node)) then NoteInfo^.Width := StrToInt(Node.FirstChild.NodeValue);
        //TRlog('Found '+ IntToStr(NoteInfo^.Width));
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for height');
        Node := Doc.DocumentElement.FindNode('height');
        NoteInfo^.Width := 0;
        if(assigned(Node)) then NoteInfo^.Width := StrToInt(Node.FirstChild.NodeValue);
        //TRlog('Found '+ IntToStr(NoteInfo^.Height));
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for X');
        Node := Doc.DocumentElement.FindNode('x');
        NoteInfo^.X := 0;
        if(assigned(Node)) then NoteInfo^.X := StrToInt(Node.FirstChild.NodeValue);
        //TRlog('Found '+ IntToStr(NoteInfo^.X));
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for Y');
        Node := Doc.DocumentElement.FindNode('y');
        NoteInfo^.Y := 0;
        if(assigned(Node)) then NoteInfo^.Y := StrToInt(Node.FirstChild.NodeValue);
        //TRlog('Found '+ IntToStr(NoteInfo^.Y));
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for open-on-startup');
        Node := Doc.DocumentElement.FindNode('open-on-startup');
        NoteInfo^.OpenOnStartup := false;
        if(assigned(Node)) then NoteInfo^.OpenOnStartup := StrToBool(Node.FirstChild.NodeValue);
        //TRlog('Found '+ BoolToStr(NoteInfo^.OpenOnStartup, true));
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for pinned');
        Node := Doc.DocumentElement.FindNode('pinned');
        NoteInfo^.Pinned := false;
        if(assigned(Node)) then NoteInfo^.Pinned := StrToBool(Node.FirstChild.NodeValue);
        //TRlog('Found '+ BoolToStr(NoteInfo^.Pinned, true));
        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for text');
        Node := Doc.DocumentElement.FindNode('text');
        NoteInfo^.Content := 'empty';
        NoteInfo^.Version := '0.3';
        if(assigned(Node)) then
        begin
             NoteInfo^.Content := Node.FindNode('note-content').TextContent;
             Node := Node.FindNode('note-content').Attributes.GetNamedItem('version');
             if(Assigned(Node)) then NoteInfo^.Version := Node.NodeValue;
        end
        else TRlog('No text');

        if(assigned(Node)) then Node.Free;

        //TRlog('Looking for tags');
        Node := Doc.DocumentElement.FindNode('tags');
        if(assigned(Node)) then
        begin
           NodeList := Node.ChildNodes;
           for j := 0 to NodeList.Count-1 do
               begin
                //TRlog('Found tag ' + NodeList.Item[j].TextContent);
                NoteInfo^.Tags.Add(NodeList.Item[j].TextContent);
               end;
        end
        //else TRlog('No tags')
        ;

        if(assigned(Node)) then Node.Free;



        NoteInfo^.Deleted := false;

     except on E:Exception do
        begin
         TRlog('File to note ERROR2 '+filename);

          NoteInfo^.Error := E.Message;
          NoteInfo^.Action:= SynClash;
          TRlog(E.message);
        end;
     end;

     FreeAndNil(Doc);
     TRlog('File to note DONE '+filename);

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

  if(d1<d2) then exit(-1);
  if(d1>d2) then exit(1);

  Result := 0;
end;

{ ===== XML ===== }

function RemoveXml(const St : String) : String;
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

function RemoveBadXMLCharacters(const InStr : String; DoQuotes : boolean = false) : String;
var
   Index : longint = 1;
   Start : longint = 1;
begin
   Result := '';
   while Index <= length(InStr) do
     begin
          if InStr[Index] = '<' then
          begin
             Result := Result + Copy(InStr, Start, Index - Start);
             Result := Result + '&lt;';
             inc(Index);
             Start := Index;
             continue;
          end;

          if InStr[Index] = '>' then
          begin
             Result := Result + Copy(InStr, Start, Index - Start);
             Result := Result + '&gt;';
             inc(Index);
             Start := Index;
             continue;
          end;

          if InStr[Index] = '&' then
          begin
             Result := Result + Copy(InStr, Start, Index - Start);
             Result := Result + '&amp;';
             inc(Index);
             Start := Index;
             continue;
          end;

          if DoQuotes then
          begin
             if InStr[Index] = '''' then
             begin
                Result := Result + Copy(InStr, Start, Index - Start);
                Result := Result + '&apos;';
                inc(Index);
                Start := Index;
                continue;
             end;

             if InStr[Index] = '"' then
             begin
                Result := Result + Copy(InStr, Start, Index - Start);
                Result := Result + '&quot;';
                inc(Index);
                Start := Index;
                continue;
             end;
          end;

          inc(Index);
     end;

   Result := Result + Copy(InStr, Start, Index - Start);
end;


{ ===== DATETIME ==== }

function GetTimeFromGMT(d : TDateTime) : String;
begin;
    Result := FormatDateTime('YYYY-MM-DD',d) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000+00:00"',d);

end;

function GetDisplayTimeFromGMT(d : TDateTime) : String;
var
   offset : double;
begin;
  offset := GetLocalTimeOffset; // minutes
  offset := offset / 1440.0;
  d := d - offset;

  Result := FormatDateTime('YYYY/MM/DD hh:mm:ss',d);

end;

function GetCurrentTimeStr(): ANSIstring;
var
   ThisMoment : TDateTime;
   Res : ANSIString;
   Off : longint;
begin
    {$ifdef LINUX}
    ReReadLocalTime();    // in case we are near daylight saving time changeover
    {$endif}
    ThisMoment:=Now;
    Result := FormatDateTime('YYYY-MM-DD',ThisMoment) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000"',ThisMoment);
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

function GetGMTFromStr(const DateStr: ANSIString): TDateTime;
var
    TimeZone : TDateTime;
begin
    if DateStr = '' then exit(0);

    if length(DateStr) <> 33 then begin
        TRlog('ERROR received invalid date string - [' + DateStr + ']');
        exit(0);
    end;

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
	  else TRlog('******* Bugger, we are not parsing DATE String ********');
    except on EConvertError do
       begin
       TRlog('FAIL on calculating GMT ' + DateStr);
       exit(0.0);
       end;
    end;
    TRlog('GetGMTFromStr : "' + DateStr + '"  -> ' + DatetoStr(Result) + ' '+ TimetoStr(Result));
end;


{ ===== FONT ==== }


function GetDefaultFixedFont() : string;
var  T : string;
    FontNames : array[1..7] of string
      = ('Monospace', 'Monaco', 'Nimbus Mono L', 'Liberation Mono', 'Lucida Console', 'Lucida Sans Typewriter', 'Courier New' );

    f : TForm;

    function IsMono(FontName : String) : boolean;
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

function GetDefaultUsualFont() : String;
var
    f : TForm;
begin
    f := TForm.Create(nil);
    Result := GetFontData(f.Font.Handle).Name;
    TRlog('DefaultUsualFont = ' + Result);
    FreeAndNil(f);
end;

procedure setFontSizes(out FontSizeSmall  : Integer; out FontSizeLarge  : Integer; out FontSizeHuge   : Integer; out FontSizeTitle  : Integer; out FontSizeNormal : Integer);
begin
   if(FontRange = FontBig) then begin
    	FontSizeSmall  := 9;
     	FontSizeLarge  := 17;
     	FontSizeHuge   := 20;
     	FontSizeTitle  := 18;			// Dont set this to one of the other sizes !
     	FontSizeNormal := 14;
   end
   else if (FontRange = FontHuge) then begin
        FontSizeSmall  := 11;
        FontSizeLarge  := 20;
        FontSizeHuge   := 23;
        FontSizeTitle  := 21;			// Dont set this to one of the other sizes !
        FontSizeNormal := 16;
   end
   else if (FontRange = FontMedium)then begin
    	FontSizeSmall  := 8;
 	FontSizeLarge  := 14;
 	FontSizeHuge   := 18;
 	FontSizeTitle  := 16;			// Dont set this to one of the other sizes !
 	FontSizeNormal := 11;
   end else begin
    	FontSizeSmall  := 7;
        FontSizeLarge  := 13;
 	FontSizeHuge   := 16;
 	FontSizeTitle  := 14;			// Dont set this to one of the other sizes !
 	FontSizeNormal := 10;
   end;
end;

{ ===== NETWORK ==== }

type TRSockecktHandler = class(TObject)
public
    procedure HttpClientGetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
end;

procedure TRSockecktHandler.HttpClientGetSocketHandler(Sender: TObject; const UseSSL: Boolean; out AHandler: TSocketHandler);
begin
  If UseSSL then begin
    AHandler:=TSSLSocketHandler.Create;
    TSSLSocketHandler(AHandler).SSLType:=stTLSv1_2;
  end else
      AHandler := TSocketHandler.Create;
end;

function URLDecode(s: String): String;
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

function URLEncode(s: string): string;
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

function WebPut(u : String; params : TStrings; data : String) : String;
var
  Client: TFPHttpClient;
  res : TStringStream;
  i : integer;
  handler : TRSockecktHandler;
  auth : String;
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
    ShowMessage(E.message);
    Result := '';
    end;
  end;
  FreeAndNil(Client);
  FreeAndNil(res);
end;

function WebGet(u : String; params : TStrings) : String;
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
    ShowMessage(E.message);
    Result :='';
    end;
  end;
  Client.Free;
end;


function WebPost(u : String; params : TStrings) : String;
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
    ShowMessage(E.message);
    Result := '';
    end;
  end;
  FreeAndNil(Client);
  FreeAndNil(p);
  FreeAndNil(res);
end;


{ ===== OAUTH 1.0 ==== }

procedure OauthBaseParams(const p : TStrings; Key : string; Token : String = ''; Verifier : String = '');
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

function OauthTimestamp() : String;
begin
  Result := Format('%d',[Trunc((Now - EncodeDate(1970, 1 ,1)) * 24 * 60 * 60)]);
end;

function OauthNonce() : String;
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


procedure OauthSign(u : String; mode : String; params : TStrings; Key,Secret : String);
var
  data : String;
  p : String;
  i : integer;
  j : integer;
  hashkey : String;
  signature : String;
  s2 : String;
  c : String;
  b64 : String;
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

function GetDictDefaultPath() : String;
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

function GetDictDefaultLibrary() : String;
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

function SyncActionName(Act: TSyncAction): string;
begin
    Result := ' Unknown ';
    case Act of
        SynUnset : Result := rsUndecided;
        SynNothing : Result := rsDoNothing;
        SynUploadNew  : Result := rsNewUploads;   // we differentiate in case of a write to remote fail.
        SynUpLoadEdit : Result := rsEditUploads;
        SynDownloadNew: Result := rsNewDownloads;
        SynDownloadEdit: Result := rsEditDownloads;
        SynCopy: Result := rsSynCopies;
        SynDeleteLocal  : Result := rsLocalDeletes;
        SynDeleteRemote : Result := rsRemoteDeletes;
        SynError : Result := ' ** ERROR **';
        SynAllLocal : Result := ' AllLocal ';
        SynAllCopy : Result := ' AllCopy ';
        SynAllRemote : Result := ' AllRemote ';
        SynAllNewest : Result := ' AllNewest ';
        SynAllOldest : Result := ' AllOldest ';
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
function TNoteInfoList.FindID(const ID: ANSIString): PNoteInfo;
var
    Index : longint;
begin
    Result := Nil;
    TRlog('Searching TNoteList ' + LName + ' for ID=' + ID);
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
  TRlog('Destroy NoteInfoList ' + LName);
  for I := 0 to Count-1 do
  begin
       //TRlog('Destroy NoteInfoList FreeAndNil '+IntToStr(I) + ' ID='+Items[I]^.ID);
       //Items[I]^.tags.Free;
       TRlog('Destroy NoteInfoList Disposing '+IntToStr(I)+' '+Items[I]^.ID);
       dispose(Items[I]);
  end;
  TRlog('Destroy NoteInfoList '+LName+' done');
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

