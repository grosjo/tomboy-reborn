unit TRcommon;

interface

uses
    Classes, Forms, SysUtils, Dialogs, StdCtrls, LazFileUtils, laz2_DOM,
    laz2_XMLRead, DateUtils, fphttpclient, ssockets, sslsockets, fpopenssl,
    openssl, hmac, strutils, IniFiles, LazLogger;


type TSyncOption = (AlwaysAsk, UseServer, UseLocal, MakeCopy);	// Relating to sync clash pref in config file

type TSyncTransport=(
        SyncFile,  // Sync to locally available dir, things like smb: mount, google drive etc
        SyncNextCloud,  // Sync to NextCloud using Nextcloud Notes
        SyncAndroid,  // Simple one to one Android Device
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

type
    TClashRecord =
      record
         RemoteNote : PNoteInfo;
         LocalNote : PNoteInfo;
      end;

type    TClashFunction = function(const ClashRec : TClashRecord): TSyncAction of object;


// Environmment
const Backup = 'Backup';
function GetDefaultConfigDir() : string;
function GetDefaultNotesDir() : string;
function GetLocalNoteFile(NoteID : string; altrep : String = ''): string;
function GetLocalBackupPath(): string;
function GetTempFile() : string;
function ConfigSave(source : String) : boolean;
procedure ConfigRead(source : String);

// Note generic function
function GetNewID() : String;
function NoteIDLooksOK(const ID : string) : boolean;
function FileToNote(xml : String; NoteInfo : PNoteInfo) : boolean;

// Font
function GetDefaultFixedFont() : string;

// Datetime
function GetGMTFromStr(const DateStr: ANSIString): TDateTime;
function GetTimeFromGMT(d : TDateTime) : String;

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

var
    ConfigDir : String;
    NotesDir : String;
    ConfigFile : String;
    ConfigReading : boolean;
    ConfigWriting : boolean;

    ShowIntLinks,ShowExtLinks, ManyNoteBooks, SearchCaseSensitive, ShowSplash,
      Autostart, SearchAtStart : boolean;

    UsualFont, FixedFont : string;
    FontSmall  : Integer;
    FontLarge  : Integer;
    FontHuge   : Integer;
    FontTitle  : Integer;			// Dont set this to one of the other sizes !
    FontNormal : Integer;

    SyncType : TSyncTransport;
    SyncOption : TSyncOption;
    SyncFirstRun : boolean;
    NCurl, NCKey, NCToken, NCSecret : String;
    SyncRepeat : integer;



implementation


{ ===== ENVIRONMENT ==== }

function GetDefaultConfigDir() : string;
begin
    Result := '';
    if Application.HasOption('config-dir') then
        Result := Application.GetOptionValue('config-dir');
    if Result = '' then begin
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
    end;
    Result := AppendPathDelim(ChompPathDelim(Result));
    ForceDirectoriesUTF8(Result);
end;

function GetDefaultNotesDir() : string;
begin
    {$IFDEF UNIX}
    Result := GetEnvironmentVariable('HOME') + '/.local/share/tomboy-reborn/';
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

function ConfigSave(source : String) : boolean;
var
	f : TINIFile;
begin
    Result := True;
    debugln('ConfigSave '+source);

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

    debugln('ConfigSave proceeding '+source);

    f :=  TINIFile.Create(ConfigFile);
    try
        try
            f.writestring('BasicSettings', 'NotesDir', NotesDir);

            f.writestring('BasicSettings', 'ManyNotebooks', BoolToStr(ManyNoteBooks) );
            f.writestring('BasicSettings', 'CaseSensitive', BoolToStr(SearchCaseSensitive));
            f.writestring('BasicSettings', 'ShowIntLinks', BoolToStr(ShowIntLinks));
            f.writestring('BasicSettings', 'ShowExtLinks', BoolToStr(ShowExtLinks));
            f.WriteString('BasicSettings', 'ShowSplash', BoolToStr(ShowSplash));
            f.WriteString('BasicSettings', 'Autostart', BoolToStr(Autostart));
            f.WriteString('BasicSettings', 'ShowSearchAtStart', BoolToStr(SearchAtStart));


            if RadioFontBig.Checked then
                ConfigFile.writestring('BasicSettings', 'FontSize', 'big')
            else if RadioFontMedium.Checked then
                ConfigFile.writestring('BasicSettings', 'FontSize', 'medium')
            else if RadioFontSmall.Checked then
                ConfigFile.writestring('BasicSettings', 'FontSize', 'small')
            else if RadioFontHuge.Checked then
                ConfigFile.writestring('BasicSettings', 'FontSize', 'huge');
            ConfigFile.writestring('BasicSettings', 'UsualFont', UsualFont);
            ConfigFile.writestring('BasicSettings', 'FixedFont', FixedFont);
            //(Sel_CText = 0) and (Sel_CBack = 0) and (Sel_CHiBack = 0) and (Sel_CTitle = 0)
            if UserSetColours then begin
                ConfigFile.writestring('BasicSettings', 'BackGndColour', ColorToString(BackGndColour));
                ConfigFile.writestring('BasicSettings', 'HiColour', ColorToString(HiColour));
                ConfigFile.writestring('BasicSettings', 'TextColour', ColorToString(TextColour));
                ConfigFile.writestring('BasicSettings', 'TitleColour', ColorToString(TitleColour));
	    end else begin
                ConfigFile.writestring('BasicSettings', 'BackGndColour', '0');
                ConfigFile.writestring('BasicSettings', 'HiColour', '0');
                ConfigFile.writestring('BasicSettings', 'TextColour', '0');
                ConfigFile.writestring('BasicSettings', 'TitleColour', '0');
	    end;

            ConfigFile.WriteString('SyncSettings', 'Autosync', MyBoolStr(CheckBoxAutosync.Checked));
            ConfigFile.WriteString('SyncSettings', 'Autosync', EditTimerSync.Text);

            case SyncOption of
		AlwaysAsk : ConfigFile.writestring('SyncSettings', 'SyncOption', 'AlwaysAsk');
                UseLocal : ConfigFile.writestring('SyncSettings', 'SyncOption', 'UseLocal');
                UseServer : ConfigFile.writestring('SyncSettings', 'SyncOption', 'UseServer');
	        MakeCopy : ConfigFile.writestring('SyncSettings', 'SyncOption', 'MakeCopy');
	    end;

            if getSyncTested() then
                ConfigFile.writestring('SyncSettings', 'SyncTested', 'true')
                else ConfigFile.writestring('SyncSettings', 'SyncTested', 'false');

            if RadioSyncFile.checked then
                ConfigFile.writestring('SyncSettings', 'SyncType', 'file')
            else ConfigFile.writestring('SyncSettings', 'SyncType', 'nextcloud');
            if (LabelFileSync.Caption = '') or (LabelFileSync.Caption = rsSyncNotConfig) then
                ConfigFile.writestring('SyncSettings', 'SyncRepo', '')
            else  ConfigFile.writestring('SyncSettings', 'SyncRepo', LabelFileSync.Caption);
            if (LabelNCSyncURL.Caption = '') or (LabelNCSyncURL.Caption = rsSyncNotConfig) then
                ConfigFile.writestring('SyncSettings', 'SyncNCURL', '')
            else  ConfigFile.writestring('SyncSettings', 'SyncNCURL', LabelNCSyncURL.Caption);

            ConfigFile.writestring('SyncSettings', 'SyncNCKey', NCKey);
            ConfigFile.writestring('SyncSettings', 'SyncNCToken', NCToken);
            ConfigFile.writestring('SyncSettings', 'SyncNCSecret', NCSecret);

        if SpellConfig then begin
                ConfigFile.writestring('Spelling', 'Library', LabelLibrary.Caption);
                ConfigFile.writestring('Spelling', 'Dictionary', LabelDic.Caption);
            end;

        HaveConfig := true;

        finally
    	    ConfigFile.Free;
        end;
    	except on E: Exception do begin
            showmessage('Unable to write config (from '+source+') to ' + LabelSettingPath.Caption);
            Result := False;
        end;
    end;

    ConfigWriting := false;
end;

procedure ConfigRead(source : String);
var
    ConfigFile : TINIFile;
begin

    if(ConfigReading) then begin
        debugln('already reading');
        exit;
    end;
    ConfigReading := True;

    if fileexists(LabelSettingPath.Caption) then
    begin
         ConfigFile :=  TINIFile.Create(LabelSettingPath.Caption);

         NotesDir:= ConfigFile.readstring('BasicSettings', 'NotesDir', GetDefaultNotesDir());
         ShowIntLinks := StrToBool(ConfigFile.readstring('BasicSettings', 'ShowIntLinks', 'true');
         ShowExtLinks := StrToBool(ConfigFile.readstring('BasicSettings', 'ShowExtLinks', 'true');
         ManyNoteBooks := Configfile.readstring('BasicSettings', 'ManyNotebooks', 'false');
         CaseSensitive := Configfile.readstring('BasicSettings', 'CaseSensitive', 'false');
         ShowSplash := Configfile.ReadString('BasicSettings', 'ShowSplash', 'true');
         Autostart := Configfile.ReadString('BasicSettings', 'Autostart', 'false');
         SearchAtStart := Configfile.ReadString('BasicSettings', 'ShowSearchAtStart', 'false');

         ReqFontSize := ConfigFile.readstring('BasicSettings', 'FontSize', 'medium');
         case ReqFontSize of
              'huge'   : RadioFontHuge.Checked := true;
              'big'    : RadioFontBig.Checked := true;
              'medium' : RadioFontMedium.Checked := true;
              'small'  : RadioFontSmall.Checked := true;
         end;
         UsualFont := ConfigFile.readstring('BasicSettings', 'UsualFont', GetFontData(Self.Font.Handle).Name);
         ButtonFont.Hint := UsualFont;
         FixedFont := ConfigFile.readstring('BasicSettings', 'FixedFont', DefaultFixedFont);
         if FixedFont = '' then FixedFont := DefaultFixedFont;
         ButtonFixedFont.Hint := FixedFont;

         BackGndColour:=   StringToColor(Configfile.ReadString('BasicSettings', 'BackGndColour', '0'));
         HiColour :=   StringToColor(Configfile.ReadString('BasicSettings', 'HiColour', '0'));
         TextColour := StringToColor(Configfile.ReadString('BasicSettings', 'TextColour', '0'));
         TitleColour :=  StringToColor(Configfile.ReadString('BasicSettings', 'TitleColour', '0'));
         UserSetColours := not ((BackGndColour = 0) and (HiColour = 0) and (TextColour = 0) and (TitleColour = 0));
            	// Note - '0' is a valid colour, black. So, what says its not set is they are all '0';

         case ConfigFile.readstring('SyncSettings', 'SyncOption', 'AlwaysAsk') of
              'AlwaysAsk' : begin SyncOption := AlwaysAsk; RadioAlwaysAsk.Checked := True; end;
              'UseLocal'  : begin SyncOption := UseLocal;  RadioUseLocal.Checked  := True; end;
              'UseServer' : begin SyncOption := UseServer; RadioUseServer.Checked := True; end;
              'MakeCopy' : begin SyncOption := MakeCopy; RadioMakeCopy.Checked := True; end;
	 end;

         tmp := ConfigFile.readstring('SyncSettings', 'SyncType', '');          // this is new way to do it, file, nextcloud, etc
         if(tmp = '') then
         begin
              // Legacy
              tmp := ConfigFile.readstring('SyncSettings', 'UseFileSync', '');
              if (tmp = 'true') then tmp := 'file';
         end;
         if(tmp = 'file') then
         begin
              RadioSyncFile.checked := true;
              RadioSyncNC.checked := false;
              RadioSyncNone.checked := false;
         end else if(tmp = 'nextcloud') then
   	 begin
              RadioSyncFile.checked := false;
              RadioSyncNC.checked := true;
              RadioSyncNone.checked := false;
         end else begin
             RadioSyncFile.checked := false;
             RadioSyncNC.checked := false;
             RadioSyncNone.checked := true;
	 end;

         CheckBoxAutoSync.enabled := not RadioSyncNone.checked;

	 LabelFileSync.Caption := ConfigFile.readstring('SyncSettings', 'SyncRepo', '');
         if LabelFileSync.Caption = '' then LabelFileSync.Caption := rsSyncNotConfig;

         NCUrl    := ConfigFile.readstring('SyncSettings', 'SyncNCUrl', rsSyncNCDefault);
	 LabelNCSyncURL.Caption := NCUrl;

         if (length(LabelNCSyncURL.Caption)<10) then LabelNCSyncURL.Caption := rsSyncNotConfig;
         NCKey    := ConfigFile.readstring('SyncSettings', 'SyncNCKey', MD5Print(MD5String(Format('%d',[Random(9999999-123400)+123400]))));
	 NCToken  := ConfigFile.readstring('SyncSettings', 'SyncNCToken', '');
	 NCSecret  := ConfigFile.readstring('SyncSettings', 'SyncNCSecret', '');

         SyncFirstRun := (ConfigFile.readstring('SyncSettings', 'SyncTested', 'true') = 'true');

         LabelLibrary.Caption := ConfigFile.readstring('Spelling', 'Library', '');
         LabelDic.Caption := ConfigFile.readstring('Spelling', 'Dictionary', '');
         SpellConfig := (LabelLibrary.Caption <> '') and (LabelDic.Caption <> '');     // indicates it worked once...

         CheckBoxAutoSync.checked := ('true' = Configfile.ReadString('SyncSettings', 'Autosync', 'false'));
         EditTimerSync.Text := Configfile.ReadString('SyncSettings', 'AutosyncElapse', '10');
         EditTimerSync.Enabled := CheckBoxAutoSync.checked;
         Label16.Enabled := CheckBoxAutoSync.checked;
         Label17.Enabled := CheckBoxAutoSync.checked;

         ConfigFile.free;

         LabelNotespath.Caption := NotesDir;
         ShowIntLinks := CheckShowIntLinks.Checked;
         SetFontSizes();

         ConfigReading := false;
         ConfigSave('CheckConfigFile1');

    end else begin
    // No config file
        GetDefaultNotesDir();

        ShowIntLinks := true;
        ShowExtLinks := true;
        ManyNoteBooks := false;
        SearchCaseSensitive := false;
        ShowSplash := true;
        Autostart := true;
        SearchAtStart := false;

        NCUrl := rsSyncNCDefault;
        NCKey := MD5Print(MD5String(Format('%d',[Random(9999999-123400)+123400])));
        NCToken :='';
        NCSecret :='';

        FixedFont := GetDefaultFixedFont();


        RadioFontMedium.Checked := True;
        CheckShowIntLinks.Checked:= True;
        CheckShowExtLinks.Checked := True;
        CheckBoxAutoSync.Checked := False;
        EditTimerSync.Text := '10';
        UsualFont := GetFontData(Self.Font.Handle).Name;
        FixedFont := DefaultFixedFont;
        LabelFileSync.Caption := rsSyncNotConfig;
        SyncFirstRun := false;
        RadioSyncNone.checked := true;

        ConfigReading := false;

        ConfigSave('CheckConfigFile2');
    end;


end;



{ ===== GENERIC FUNCTION ==== }

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

    function IsMono(FontName : String) : boolean;
    begin
      Label1.Canvas.Font.Name := FontName;
      result := Label1.Canvas.TextWidth('i') = Label1.Canvas.TextWidth('w');
    end;

    function IsDifferentSizes() : boolean;
    var
        ASize : integer;
    begin
        Label1.Canvas.Font.Size := 13;
        ASize := Label1.Canvas.TextHeight('H');
        Label1.Canvas.Font.Size := 14;
        if ASize = Label1.Canvas.TextHeight('H')
            then exit(False);
        ASize := Label1.Canvas.TextHeight('H');
        Label1.Canvas.Font.Size := 15;
        If ASize = Label1.Canvas.TextHeight('H')
            then exit(False);
        result := True;
    end;

begin
    Result := '';
    for T in FontNames do begin
        if not IsMono(T) then continue;
        if not IsDifferentSizes() then continue;
        Result := T;
        exit;
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


initialization

debugln('Init TRcommon');

InitSSLInterface;

ConfigReading := false;
ConfigWriting := false;

end.

