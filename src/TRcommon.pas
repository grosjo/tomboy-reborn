unit TRcommon;

interface

uses
    Classes, Forms, SysUtils, Dialogs, StdCtrls, LazFileUtils, laz2_DOM,
    ExtCtrls, laz2_XMLRead, DateUtils, fphttpclient, ssockets, sslsockets,
    fpopenssl, openssl, hmac, strutils, IniFiles, LazLogger, Graphics,
    FileInfo, TRAutoStartCtrl, Trtexts;


type TFontRange = (FontHuge, FontBig, FontMedium, FontSmall);	// Relating to sync clash pref in config file

type TSyncClashOption = (AlwaysAsk, UseServer, UseLocal, MakeCopy);	// Relating to sync clash pref in config file

type TSyncTransport=(
        SyncFile,  // Sync to locally available dir, things like smb: mount, google drive etc
        SyncNextCloud,  // Sync to NextCloud using Nextcloud Notes
        SyncNone); // Not syncing

type TSyncAction=(
        SynUnset,      // initial state, should not be like this at end.
        SynNothing,      // This note, previously synced has not changed.
        SynUploadNew,    // This a new local note, upload it.
        SynUploadEdit,   // A previously synced note, edited locally, upload.
        SynDownload,     // A new or edited note from elsewhere, download.
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
type TSyncAvailable=(
        SyncNotYet,        // Initial state.
        SyncReady,          // We are ready to sync, looks good to go.
        SyncNoLocal,        // We dont have a local manifest, only an error if config thinks there should be one.
        SyncNoRemoteMan,    // No remote manifest, an uninitialized repo perhaps ?
        SyncNoRemoteRepo,   // Filesystem is OK but does not look like a repo.
        SyncBadRemote,      // Has either Manifest or '0' dir but not both.
        SyncNoRemoteDir,    // Perhaps sync device is not mounted, Tomdroid not installed ?
        SyncNoRemoteWrite,  // no write permission, do not proceed!
        SyncMismatch,       // Its a repo, Captain, but not as we know it.
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
        Deleted : boolean;
  end;

type PNoteInfo=^TNoteInfo;

type TNoteInfoList =
  class(TList)
    private
     	function Get(Index: integer): PNoteInfo;
    public
        ServerID : string;              // Partially implemented, don't rely yet ....
        LastSyncDateSt : string;        // Partially implemented, don't rely yet ....
        LastSyncDate : TDateTime;       // Partially implemented, don't rely yet ....
        LastRev : integer;              // Partially implemented, don't rely yet ....
        destructor Destroy; override;
        function Add(ANote : PNoteInfo) : integer;
        function FindID(const ID : ANSIString) : PNoteInfo;
        function ActionName(Act : TSyncAction) : string;
        property Items[Index: integer]: PNoteInfo read Get; default;
  end;

type
    TClashRecord =
      record
         RemoteNote : PNoteInfo;
         LocalNote : PNoteInfo;
      end;

type    TClashFunction = function(const ClashRec : TClashRecord): TSyncAction of object;


// Environmment
const Backup = 'Backup';
function AboutString() : String;
function GetDefaultConfigDir() : string;
function GetDefaultNotesDir() : string;
function GetLocalNoteFile(NoteID : string; altrep : String = ''): string;
function GetLocalBackupPath(): string;
function GetTempFile() : string;
function ConfigWrite(source : String) : boolean;
function ConfigRead(source : String) : boolean;

// Note generic function
function GetNewID() : String;
function NoteIDLooksOK(const ID : string) : boolean;
function FileToNote(xml : String; NoteInfo : PNoteInfo) : boolean;

// Font
function GetDefaultFixedFont() : string;
function GetDefaultUsualFont() : string;
procedure setFontSizes();

// Datetime
function GetGMTFromStr(const DateStr: ANSIString): TDateTime;
function GetTimeFromGMT(d : TDateTime) : String;

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
function isSyncConfigured() : boolean;

var
    ConfigDir : String;
    NotesDir : String;
    ConfigFile : String;
    ConfigReading : boolean;
    ConfigWriting : boolean;

    ShowIntLinks,ShowExtLinks, ManyNoteBooks, SearchCaseSensitive,
      Autostart, SearchAtStart, UseTrayIcon : boolean;

    UsualFont, FixedFont : string;
    FontRange : TFontRange;
    FontSizeSmall  : Integer;
    FontSizeLarge  : Integer;
    FontSizeHuge   : Integer;
    FontSizeTitle  : Integer;	// Dont set this to one of the other sizes !
    FontSizeNormal : Integer;

    SyncType : TSyncTransport;
    SyncClashOption : TSyncClashOption;
    SyncFirstRun : boolean;
    SyncFileRepo, SyncNCurl, SyncNCKey, SyncNCToken, SyncNCSecret : String;
    SyncRepeat : integer;

    DictLibrary, DictPath, DictFile : String;

    BackGndColour, TextColour, HiColour, TitleColour : TColor;

    mainWindow : TForm;

Const
  Placement = 45;

implementation


{ ===== ENVIRONMENT ==== }

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
    debugln('Default Conf Dir ='+Result);
end;

function GetDefaultNotesDir() : string;
begin
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + '/.local/share/TomboyReborn/';
    {$ENDIF}
    {$IFDEF DARWIN}
    // try the correct place first, if not there, lets try the old, wrong place
    // if at neither, we go back to correct place.
    Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Notes/';
    if DirectoryExistsUTF8(Result) then exit;
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-reborn/';
    if not DirectoryExistsUTF8(Result) then
        Result := GetEnvironmentVariable('HOME') + '/Library/Application Support/Tomboy-ng/Notes/';
    {$ENDIF}
    {$IFDEF WINDOWS}
    Result := GetEnvironmentVariable('APPDATA') + '\tomboy-reborn\notes\';
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
    autos : TAutoStartCtrl;
begin
    Result := True;
    debugln('ConfigWrite '+source);

    if ConfigWriting then
    begin
         debugln('Already writing');
         exit(false);
    end;
    if ConfigReading then
    begin
         debugln('Reading in process');
         exit(false);
    end;

    ConfigWriting := true;

    debugln('ConfigWrite('+source+') proceeding '+ConfigFile);

    try
      DeleteFile(ConfigFile);
      f :=  TINIFile.Create(ConfigFile);
    except on E: Exception do begin
       showmessage('Unable to write config '+ ConfigFile);
       ConfigWriting := false;
       exit(False);
       end;
    end;

    f.writestring('Basic', 'NotesDir', NotesDir);

    f.writestring('Basic', 'ManyNotebooks', BoolToStr(ManyNoteBooks) );
    f.writestring('Basic', 'CaseSensitive', BoolToStr(SearchCaseSensitive));
    f.writestring('Basic', 'ShowIntLinks', BoolToStr(ShowIntLinks));
    f.writestring('Basic', 'ShowExtLinks', BoolToStr(ShowExtLinks));
    f.WriteString('Basic', 'Autostart', BoolToStr(Autostart));
    f.WriteString('Basic', 'ShowSearchAtStart', BoolToStr(SearchAtStart));

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
    f.writestring('Sync', 'Tested', BoolToStr(not SyncFirstRun));

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

    f.Free;

    autos := TAutoStartCtrl.Create('tomboy-reborn', AutoStart);
    if autos.ErrorMessage <> '' then
          ShowMessage('Error setting autstart' + Autos.ErrorMessage);
    FreeAndNil(Autos);

    ConfigWriting := false;
end;

function ConfigRead(source : String) : boolean;
var
    f : TINIFile;
    s : String;
begin

    if(ConfigReading) then begin
        debugln('ConfigRead(' + source + ') already reading');
        exit(false);
    end;
    ConfigReading := True;
    debugln('ConfigRead(' + source + ') go reading');

    if fileexists(ConfigFile) then
    begin
         try
            f :=  TINIFile.Create(ConfigFile);
         except on E: Exception do begin
           debugln(E.Message);
           exit(false);
           end;
         end;

         NotesDir:= f.readstring('Basic', 'NotesDir', GetDefaultNotesDir());

         ManyNoteBooks        := StrToBool(f.readstring('Basic', 'ManyNotebooks', 'false'));
         SearchCaseSensitive  := StrToBool(f.readstring('Basic', 'CaseSensitive', 'false'));
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
         SyncFirstRun     := StrToBool(f.ReadString('Sync', 'Tested', 'false'));
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
         DictPath := AppendPathDelim(ChompPathDelim(DictPath));

         f.free;

         ConfigReading := false;

    end else begin

    // No config file
        GetDefaultNotesDir();

        ShowIntLinks := true;
        ShowExtLinks := true;
        ManyNoteBooks := false;
        SearchCaseSensitive := false;
        Autostart := true;
        SearchAtStart := false;

        SyncRepeat := 0;
        SyncClashOption  := TSyncClashOption.AlwaysAsk;

        SyncFirstRun     := false;
        SyncType         := TSyncTransport.SyncNone;

        SyncRepeat      := 0;
        SyncType        := TSyncTransport.SyncNone;
        SyncClashOption := TSyncClashOption.AlwaysAsk;
        SyncFirstRun    := true;
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

        ConfigReading := false;

        ConfigWrite('CheckConfigFile');
    end;

    SetFontSizes();

    Result := true;

end;



{ ===== NOTE FUNCTIONS ==== }

function GetNewID() : ANSIString;
var
  GUID : TGUID;
begin
   CreateGUID(GUID);
   Result := copy(GUIDToString(GUID), 2, 36);
end;


function NoteIDLooksOK(const ID : string) : boolean;
begin
    if length(ID) <> 36 then exit(false);
    if pos('-', ID) <> 9 then exit(false);
    result := True;
end;

function FileToNote(xml : String; NoteInfo : PNoteInfo) : boolean;
var
    Doc : TXMLDocument;
    Node : TDOMNode;

begin
     try
        ReadXMLFile(Doc, xml);
     except on E:Exception do begin debugln(E.message); exit(false); end;
     end;

     try
        Node := Doc.DocumentElement.FindNode('create-date');
        NoteInfo^.CreateDate := Node.FirstChild.NodeValue;
        if NoteInfo^.CreateDate <> '' then
           NoteInfo^.CreateDateGMT := GetGMTFromStr(NoteInfo^.CreateDate)
           else NoteInfo^.CreateDateGMT := 0;

        Node := Doc.DocumentElement.FindNode('last-change-date');
        NoteInfo^.LastChange := Node.FirstChild.NodeValue;
        if NoteInfo^.LastChange <> '' then
           NoteInfo^.LastChangeGMT := GetGMTFromStr(NoteInfo^.LastChange)
        else NoteInfo^.LastChangeGMT := 0;

        Node := Doc.DocumentElement.FindNode('last-metadata-change-date');
        NoteInfo^.LastMetaChange := Node.FirstChild.NodeValue;
        if NoteInfo^.LastMetaChange <> '' then
           NoteInfo^.LastMetaChangeGMT := GetGMTFromStr(NoteInfo^.LastMetaChange)
        else NoteInfo^.LastMetaChangeGMT := 0;

        NoteInfo^.Title := Doc.DocumentElement.FindNode('title').FirstChild.NodeValue;

        Node := Doc.DocumentElement.FindNode('text');
        NoteInfo^.Content := Node.FindNode('note-content').NodeValue;
        NoteInfo^.Version := Node.FindNode('note-content').Attributes.GetNamedItem('version').NodeValue;

        Node := Doc.DocumentElement.FindNode('open-on-startup');
        NoteInfo^.OpenOnStartup := (Node.FirstChild.NodeValue.ToLower = 'true');

        Node := Doc.DocumentElement.FindNode('pinned');
        NoteInfo^.Pinned := (Node.FirstChild.NodeValue.ToLower = 'true');

        Node := Doc.DocumentElement.FindNode('cursor-position');
        NoteInfo^.CursorPosition := StrToInt(Node.FirstChild.NodeValue);
        Node := Doc.DocumentElement.FindNode('selection-bound-position');
        NoteInfo^.SelectBoundPosition := StrToInt(Node.FirstChild.NodeValue);

        Node := Doc.DocumentElement.FindNode('width');
        NoteInfo^.Width := StrToInt(Node.FirstChild.NodeValue);
        Node := Doc.DocumentElement.FindNode('height');
        NoteInfo^.Height := StrToInt(Node.FirstChild.NodeValue);

        Node := Doc.DocumentElement.FindNode('x');
        NoteInfo^.X := StrToInt(Node.FirstChild.NodeValue);
        Node := Doc.DocumentElement.FindNode('y');
        NoteInfo^.Y := StrToInt(Node.FirstChild.NodeValue);

        //NoteInfo^.Source := Doc.ToString;
        NoteInfo^.Deleted := false;

     except on E:Exception do begin Doc.Free; debugln(E.message); exit(false); end;
     end;

     Doc.Free;
     Result := true;
end;


{ ===== DATETIME ==== }

function GetTimeFromGMT(d : TDateTime) : String;
begin;
    Result := FormatDateTime('YYYY-MM-DD',d) + 'T'
                   + FormatDateTime('hh:mm:ss.zzz"0000+00:00"',d);

end;


function GetGMTFromStr(const DateStr: ANSIString): TDateTime;
var
    TimeZone : TDateTime;
begin
    if DateStr = '' then exit(0);

    if length(DateStr) <> 33 then begin
        debugln('ERROR received invalid date string - [' + DateStr + ']');
        exit(0);
    end;

    try
       if not TryEncodeTimeInterval( strtoint(copy(DateStr, 29, 2)),strtoint(copy(DateStr, 32, 2)),0,0,TimeZone)
       then DebugLn('Fail on interval encode ');
    except on EConvertError do begin
       DebugLn('FAIL on converting time interval ' + DateStr);
       DebugLn('Hour ', copy(DateStr, 29, 2), ' minutes ', copy(DateStr, 32, 2));
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
       then DebugLn('Fail on date time encode ');
    except on EConvertError do
       begin
          DebugLn('FAIL on converting date time ' + DateStr);
          exit(0.0);
       end;
    end;

    try
       if DateStr[28] = '+'
       then Result := Result - TimeZone
       else if DateStr[28] = '-'
          then Result := Result + TimeZone
	  else debugLn('******* Bugger, we are not parsing DATE String ********');
    except on EConvertError do
       begin
       DebugLn('FAIL on calculating GMT ' + DateStr);
       exit(0.0);
       end;
    end;
    debugln('Date from ' + DateStr + ' is ', DatetoStr(Result), ' ', TimetoStr(Result));
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
      debugln('IsMono '+FontName);
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
        debugln('Found : '+T);
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
    debugln('DefaultUsualFont = ' + Result);
    FreeAndNil(f);
end;

procedure setFontSizes();
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
begin

  Client := TFPHttpClient.Create(nil);
  Client.OnGetSocketHandler := @handler.HttpClientGetSocketHandler;
  Client.AddHeader('User-Agent','Mozilla/5.0 (compatible; fpweb)');
  Client.AllowRedirect := true;

  res := TStringStream.Create('');

  i:=0;
  while(i<params.Count) do begin
    if(i=0) then u := u + '?' else u := u + '&';
    u := u + URLEncode(params[i]) + '=' + URLEncode(params[i+1]);
    i := i +2;
  end;

  try
    Client.FormPost(u,data,res);
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
  // callbackUrl
  p.Add('oauth_callback');
  p.Add(OAuthCallbackUrl);
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

  debugln('HASHKEY = '+hashkey);

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
  if(SyncType = TSyncTransport.SyncNone) then exit(false);
  if(SyncType = TSyncTransport.SyncFile) then exit(length(SyncFileRepo)>0);
  if(SyncType = TSyncTransport.SyncNextCloud) then
    begin
       if(length(SyncNCURL) = 0) then exit(false);
       if(length(SyncNCKey) = 0) then exit(false);
       if(length(SyncNCToken) = 0) then exit(false);
       if(length(SyncNCSecret) = 0) then exit(false);
       exit(true);
    end;
  exit(false);
end;


{ ========= TNoteInfoList ========= }

function TNoteInfoList.Add(ANote : PNoteInfo) : integer;
begin
    result := inherited Add(ANote);
end;

{ This will be quite slow with a big list notes, consider an AVLTree ? }
function TNoteInfoList.FindID(const ID: ANSIString): PNoteInfo;
var
    Index : longint;
begin
    Result := Nil;
    for Index := 0 to Count-1 do begin
        if Items[Index]^.ID = ID then begin
            Result := Items[Index];
            exit()
        end;
    end;
end;

function TNoteInfoList.ActionName(Act: TSyncAction): string;
begin
    Result := ' Unknown ';
    case Act of
        SynUnset : Result := ' Unset ';
        SynNothing : Result := ' Nothing ';
        SynUploadNew  : Result := ' UploadNew ';   // we differentiate in case of a write to remote fail.
        SynUpLoadEdit : Result := ' UpLoadEdit ';
        SynDownload: Result := ' Download ';
        SynCopy: Result := ' MakeCopy ';
        SynDeleteLocal  : Result := ' DeleteLocal ';
        SynDeleteRemote : Result := ' DeleteRemote ';
        SynError : Result := ' ** ERROR **';
        SynAllLocal : Result := ' AllLocal ';
        SynAllCopy : Result := ' AllCopy ';
        SynAllRemote : Result := ' AllRemote ';
        SynAllNewest : Result := ' AllNewest ';
        SynAllOldest : Result := ' AllOldest ';
    end;
    while length(result) < 15 do Result := Result + ' ';
end;

destructor TNoteInfoList.Destroy;
var
I : integer;
begin
    for I := 0 to Count-1 do
        dispose(Items[I]);
    inherited;
end;

function TNoteInfoList.Get(Index: integer): PNoteInfo;
begin
    Result := PNoteInfo(inherited get(Index));
end;

initialization

debugln('Init TRcommon');

InitSSLInterface;

ConfigReading := false;
ConfigWriting := false;

end.

