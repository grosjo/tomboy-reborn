unit TRsettings;

{	HISTORY
	2020/05/06 - Reshaping @DavidBannon work to adpat to real needs
}

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, Menus, ComCtrls, ExtCtrls, md5,
    TRcommon, TRtexts, TRnextSetup, Types;

// Types;

type

    { TSettings }

    TSettings = class(TForm)
          ButtDefaultNoteDir: TButton;
          ButtonNCSetup: TSpeedButton;
	  CheckBoxAutoSync: TCheckBox;
          GroupBox2: TGroupBox;
          Label16: TLabel;
          EditTimerSync: TEdit;
          GroupBox1: TGroupBox;
	  GroupBoxSync: TGroupBox;
          Label17: TLabel;
	  Label4: TLabel;
	  Label5: TLabel;
	  LabelFileSync: TLabel;
	  LabelNCSyncURL: TLabel;
          Panel4: TPanel;
          RadioMakeCopy: TRadioButton;
          RadioSyncNone: TRadioButton;
	  RadioSyncFile: TRadioButton;
	  RadioSyncNC: TRadioButton;
          ButtonFixedFont: TButton;
          ButtonFont: TButton;
          ButtonSetSpellLibrary: TButton;
          ButtonSetDictionary: TButton;

	  ButtonSetNotePath: TButton;
          CheckAutoStart : TCheckBox;
          CheckCaseSensitive: TCheckBox;
          CheckManyNotebooks: TCheckBox;
          CheckShowSearchAtStart: TCheckBox;
          CheckShowSplash: TCheckBox;
	  CheckShowExtLinks: TCheckBox;
	  CheckShowIntLinks: TCheckBox;
          FontDialog1: TFontDialog;
	  GroupBox4: TGroupBox;
	  GroupBox5: TGroupBox;
	  Label1: TLabel;
          Label12: TLabel;
          Label13: TLabel;
          Label14: TLabel;
          Label15: TLabel;
          LabelDicPrompt: TLabel;
          LabelDic: TLabel;
          LabelError: TLabel;
          LabelLibrary: TLabel;
          LabelDicStatus: TLabel;
          LabelLibraryStatus: TLabel;
	  Label2: TLabel;
	  Label3: TLabel;
	  LabelNotesPath: TLabel;
	  LabelSettingPath: TLabel;
          ListBoxDic: TListBox;
          MenuFriday: TMenuItem;
          MenuSaturday: TMenuItem;
          MenuSunday: TMenuItem;
          MenuMonday: TMenuItem;
          MenuTuesday: TMenuItem;
          MenuWednesday: TMenuItem;
          MenuThursday: TMenuItem;
          OpenDialogLibrary: TOpenDialog;
          OpenDialogDictionary: TOpenDialog;
	  PageControl1: TPageControl;
          PopupDay: TPopupMenu;
          PMenuMain: TPopupMenu;
	  RadioAlwaysAsk: TRadioButton;
          RadioFontHuge: TRadioButton;
	  RadioFontBig: TRadioButton;
	  RadioFontMedium: TRadioButton;
	  RadioFontSmall: TRadioButton;
	  RadioUseLocal: TRadioButton;
	  RadioUseServer: TRadioButton;
	  SelectDirectoryDialog1: TSelectDirectoryDialog;
          SpeedButHide: TSpeedButton;
	  SpeedButHelp: TSpeedButton;
          SpeedButtTBMenu: TSpeedButton;
          ButtonFileSetup: TSpeedButton;
	  TabBasic: TTabSheet;
          TabSpell: TTabSheet;
	  TabSync: TTabSheet;
          TimerAutoSync: TTimer;


        procedure ButtDefaultNoteDirClick(Sender: TObject);
        procedure ButtonNCSetupClick(Sender: TObject);
	procedure ButtonSetColoursClick(Sender: TObject);
        procedure ButtonFixedFontClick(Sender: TObject);
        procedure ButtonFontClick(Sender: TObject);
        procedure ButtonSetDictionaryClick(Sender: TObject);
	procedure ButtonSetNotePathClick(Sender: TObject);
        procedure ButtonSetSpellLibraryClick(Sender: TObject);
	procedure CheckAutostartChange(Sender: TObject);
        procedure CheckBoxAutoSyncChange(Sender: TObject);
        procedure CheckManyNotebooksChange(Sender: TObject);
        procedure CheckShowExtLinksChange(Sender: TObject);
        procedure CheckShowIntLinksChange(Sender: TObject);
        procedure CheckShowSearchAtStartChange(Sender: TObject);
        procedure CheckShowSplashChange(Sender: TObject);
        procedure EditTimerSyncChange(Sender: TObject);
        procedure onCheckCaseSensitive(Sender: TObject);
	procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
        procedure FormShow(Sender: TObject);
        procedure ListBoxDicClick(Sender: TObject);
	procedure PageControl1Change(Sender: TObject);
        procedure RadioConflictChange(Sender: TObject);
        procedure RadioFontBigChange(Sender: TObject);
        procedure RadioFontHugeChange(Sender: TObject);
        procedure RadioFontMediumChange(Sender: TObject);
        procedure RadioFontSmallChange(Sender: TObject);
	procedure SpeedButHelpClick(Sender: TObject);
        procedure SpeedButHideClick(Sender: TObject);
        procedure SpeedButtTBMenuClick(Sender: TObject);
	procedure ButtonFileSetupClick(Sender: TObject);
        procedure RadioSyncChange(Sender: TObject);
        procedure TabBasicContextPopup(Sender: TObject; MousePos: TPoint;
          var Handled: Boolean);
        procedure TabSpellResize(Sender: TObject);
        procedure TimerAutoSyncTimer(Sender: TObject);
        procedure SetColours;
        procedure LoadSettings();
    private
        UserSetColours : boolean;


        // Ret true and displays on screen if passed Full name is a usable dictonary
        // sets SpellConfig and triggers a config save if successful
        function CheckDictionary(const FullDicName : string): boolean;

        // Returns the number of files that could be dictionaries in indicated directory
        function CheckForDic(const DictPath: ANSIString): integer;
        { If LabelLib has a valid full name (of hunspell library), tests it, otherwise asks hunspell
          to guess some names. In either case, exits if fail, if successful then tries for
          a dictionary, either using default directories and populating listbox or if it finds one
          or a full name was provided in DicFullName, just tests that name.
          If successfull show on screen and saves config }
        procedure CheckSpelling(const DicFullName: string='');
        procedure DicDefaults(var DicPathAlt: string);



    public
        AreClosing : boolean;       // False until set true by mainUnit FormClose.
        BackGndColour : TColor;     // Next three set in main unit.
        TextColour : TColor;
        HiColour : TColor;
        TitleColour : TColor;
        DefaultFixedFont : string;
        DarkTheme : boolean;

        { Indicates Spell is configured and LabelLibrary and LabelDic should
            contain valid full file names.}
        SpellConfig : boolean;


    end;

var
    FormSettings : TSettings;


const
                                // Note we set DarkTheme colors and all HiLight colours in MainUnit
    Placement = 45;				// where we position an opening window. Its, on average, 1.5 time Placement;



implementation

{$R *.lfm}

{ TSett }


uses LazLogger,
    LazFileUtils,   // LazFileUtils needed for TrimFileName(), cross platform stuff;
    TRsearchUnit,		// So we can call IndexNotes() after altering Notes Dir
    TRsyncUI,
    hunspell,       // spelling check
    LCLType,        // Keycodes ....
    Autostart,
    Colours;

var
    Spell: THunspell;
    // Initially the first place we look for dictionaries, later its the path to
    // dictionaries listed in ListBoxDic
     DicPath : AnsiString;

{ TSettings }

procedure TSettings.ButtDefaultNoteDirClick(Sender: TObject);
begin
     NotesDir := GetDefaultNotesDir();
     LabelNotespath.Caption := NotesDir;
     ConfigSave('ButtDefaultNoteDirClick');
     SearchForm.IndexNotes();
end;

procedure TSettings.ButtonNCSetupClick(Sender: TObject);
begin
   if(NCUrl = '')
   then FormNCSetup.URL.Text := rsSyncNCDefault
   else FormNCSetup.URL.Text   :=  NCUrl;

   FormNCSetup.setKey(NCKey);
   FormNCSetup.setToken(NCToken);

   FormNCSetup.ShowModal();

   if( FormNCSetup.isSuccess() ) then
   begin
      NCUrl := FormNCSetup.URL.Text;
      NCKey := FormNCSetup.getKey();
      NCToken := FormNCSetup.getToken();
      NCSecret := FormNCSetup.getTokenSecret();

      LabelNCSyncURL.Caption := NCUrl;

      RadioSyncNC.checked := true;
      SyncFirstRun := true;
      ConfigSave('ButtonNCSetupClick');
   end;
end;

procedure TSettings.ButtonSetColoursClick(Sender: TObject);
begin

end;

procedure TSettings.ButtonFixedFontClick(Sender: TObject);
begin
     FontDialog1.Font.Name := FixedFont;
     FontDialog1.Font.Size := 10;
     FontDialog1.Title := 'Select Fixed Spacing Font';
     FontDialog1.PreviewText:= 'abcdef ABCDEF 012345';
     FontDialog1.Options := FontDialog1.Options + [fdFixedPitchOnly];
     If FontDialog1.Execute then BEGIN
        FixedFont := FontDialog1.Font.name;
        ConfigSave('ButtonFixedFontClick');
     end;
     ButtonFixedFont.Hint := FixedFont;
end;

procedure TSettings.ButtonFontClick(Sender: TObject);
begin
     FontDialog1.Font.Name := UsualFont;
     FontDialog1.Font.Size := 10;
     FontDialog1.Title := 'Select Usual Font';
     FontDialog1.PreviewText:= 'abcdef ABCDEF 012345';
     If FontDialog1.Execute then BEGIN
        UsualFont := FontDialog1.Font.name;
        ConfigSave('ButtonFontUsualClick');
     end;
     ButtonFont.Hint := UsualFont;
end;

procedure TSettings.ButtonSetDictionaryClick(Sender: TObject);
begin
   OpenDialogDictionary.InitialDir := ExtractFilePath(LabelDic.Caption);
   OpenDialogDictionary.Filter := 'Dictionary|*.dic';
   OpenDialogDictionary.Title := rsSelectDictionary;
   if OpenDialogDictionary.Execute then
      CheckDictionary(TrimFilename(OpenDialogDictionary.FileName));
end;

procedure TSettings.ButtonSetNotePathClick(Sender: TObject);
begin
   if SelectDirectoryDialog1.Execute then
   begin
        NotesDir :=  AppendPathDelim(chomppathdelim(SelectDirectoryDialog1.FileName));
        LabelNotespath.Caption := NotesDir;
        ConfigSave('ButtonSetNotePathClick');
        SearchForm.IndexNotes();
   end;
end;

procedure TSettings.ButtonSetSpellLibraryClick(Sender: TObject);
begin

end;

procedure TSettings.CheckAutostartChange(Sender: TObject);
var
     auto : TAutoStartCtrl;
begin
   Autostart := CheckAutostartChange.Checked;

   auto := TAutoStartCtrl.Create('tomboy-reborn', CheckAutostart.Checked);
   if auto.ErrorMessage <> '' then
        ShowMessage('Error setting autstart' + Auto.ErrorMessage);
   FreeAndNil(Auto);

   ConfigSave('CheckAutostartChange');
end;

procedure TSettings.CheckBoxAutoSyncChange(Sender: TObject);
begin
   EditTimerSync.Enabled := CheckBoxAutoSync.checked;
   Label16.Enabled := CheckBoxAutoSync.checked;
   Label17.Enabled := CheckBoxAutoSync.checked;

   if(not CheckBoxAutoSync.checked) then SyncRepeat:=0;

   ConfigSave('CheckBoxAutoSyncChange');
end;

procedure TSettings.CheckManyNotebooksChange(Sender: TObject);
begin
  ManyNoteBooks := CheckManyNotebooksChange.Checked;
  ConfigSave('CheckManyNotebooksChange');
end;

procedure TSettings.CheckShowExtLinksChange(Sender: TObject);
begin
  ShowExtLinks := CheckShowExtLinksChange.Checked;
  ConfigSave('CheckShowExtLinksChange');
end;

procedure TSettings.CheckShowIntLinksChange(Sender: TObject);
begin
  ShowIntLinks := CheckShowIntLinksChange.Checked;
  ConfigSave('CheckShowIntLinksChange');
end;

procedure TSettings.CheckShowSearchAtStartChange(Sender: TObject);
begin
  ShowSearchAtStart := CheckShowSearchAtStartChange.Checked;
  ConfigSave('CheckShowSearchAtStartChange');
end;

procedure TSettings.CheckShowSplashChange(Sender: TObject);
begin
  ShowSplash := CheckShowSplashChange.Checked;
  ConfigSave('CheckShowSplashChange');
end;

procedure TSettings.EditTimerSyncChange(Sender: TObject);
begin
  try
     SyncRepeat := StrToInt(EditTimerSync.Caption);
  except on E:Exception do begin
     debugln(E.message);
     SyncRepeat := 0;
     EditTimerSync.Caption := '0';
     end;
    end;

end;

procedure TSettings.onCheckCaseSensitive(Sender: TObject);
begin
  CaseSensitive := onCheckCaseSensitive.Checked;
  ConfigSave('onCheckCaseSensitive');
end;

procedure TSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TSettings.FormCreate(Sender: TObject);
begin

end;

procedure TSettings.FormDestroy(Sender: TObject);
begin

end;

procedure TSettings.FormHide(Sender: TObject);
begin

end;

procedure TSettings.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

end;

procedure TSettings.FormShow(Sender: TObject);
begin

end;

procedure TSettings.ListBoxDicClick(Sender: TObject);
begin

end;

procedure TSettings.PageControl1Change(Sender: TObject);
begin

end;

procedure TSettings.RadioConflictChange(Sender: TObject);
begin
     if RadioAlwaysAsk.Checked then SyncOption := AlwaysAsk;
     else if RadioUseLocal.Checked then SyncOption := UseLocal
     else if RadioUseServer.Checked then SyncOption := UseServer
     else if RadioMakeCopy.Checked then SyncOption := MakeCopy;
     SaveConfig('RadioFontBigChange');
end;

procedure TSettings.RadioFontBigChange(Sender: TObject);
begin
  if RadioFontBig.checked then begin
    	FontSmall  := 9;
     	FontLarge  := 17;
     	FontHuge   := 20;
     	FontTitle  := 18;			// Dont set this to one of the other sizes !
     	FontNormal := 14;
  end;
  SaveConfig('RadioFontBigChange');
end;

procedure TSettings.RadioFontHugeChange(Sender: TObject);
begin
  if RadioFontHuge.checked then begin
        FontSmall  := 11;
        FontLarge  := 20;
        FontHuge   := 23;
        FontTitle  := 21;			// Dont set this to one of the other sizes !
        FontNormal := 16;
    end;
  SaveConfig('RadioFontHugeChange');
end;

procedure TSettings.RadioFontMediumChange(Sender: TObject);
begin
  if RadioFontMedium.checked then begin
    	FontSmall  := 8;
 	FontLarge  := 14;
 	FontHuge   := 18;
 	FontTitle  := 16;			// Dont set this to one of the other sizes !
 	FontNormal := 11;
  end;
  SaveConfig('RadioFontMediumChange');
end;

procedure TSettings.RadioFontSmallChange(Sender: TObject);
begin
    if RadioFontSmall.Checked then begin
    	FontSmall  := 7;
        FontLarge  := 13;
 	FontHuge   := 16;
 	FontTitle  := 14;			// Dont set this to one of the other sizes !
 	FontNormal := 10;
    end;
    SaveConfig('RadioFontSmallChange');
end;

procedure TSettings.SpeedButHelpClick(Sender: TObject);
begin

end;

procedure TSettings.SpeedButHideClick(Sender: TObject);
begin

end;

procedure TSettings.SpeedButtTBMenuClick(Sender: TObject);
begin

end;

procedure TSettings.ButtonFileSetupClick(Sender: TObject);
begin
   if SelectDirectoryDialog1.Execute then
   begin
      SyncFirstRun := false;
      LabelFileSync.Caption := TrimFilename(SelectDirectoryDialog1.FileName + PathDelim);
      NotesDir := LabelFileSync.Caption;
      ConfigSave('ButtonFileSetupClick');
   end;
end;

procedure TSettings.RadioSyncChange(Sender: TObject);
var
   s : TSyncTransport;
begin
   CheckBoxAutoSync.Enabled := not RadioSyncNone.checked;
   EditTimerSync.Enabled := CheckBoxAutoSync.Checked and not RadioSyncNone.checked;
   Label16.Enabled := CheckBoxAutoSync.Checked and not RadioSyncNone.checked;
   Label17.Enabled := CheckBoxAutoSync.Checked and not RadioSyncNone.checked;

   if(RadioSyncNone.Enabled) then s := TSyncTransport.SyncNone
    else if(RadioSyncFile.Enabled) then s :=TSyncTransport.SyncFile
    else s := TSyncTransport.SyncNextCloud

   if(s <> SyncType) then
   begin
     SyncType :=s;
     SyncFirstRun :=false;
   end;

   ConfigSave(RadioSyncChange);
end;

procedure TSettings.TabBasicContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: Boolean);
begin

end;

procedure TSettings.TabSpellResize(Sender: TObject);
begin

end;

procedure TSettings.TimerAutoSyncTimer(Sender: TObject);
begin

end;

procedure TSettings.SetColours;
begin

end;

procedure TSettings.LoadSettings();
begin

end;

procedure TSettings.ConfigCheck;
begin

end;

function TSettings.CheckDictionary(const FullDicName: string): boolean;
begin
  result := false;
    if fileexists(FullDicName) then begin
        if assigned(Spell) then begin
            SpellConfig := Spell.SetDictionary(FullDicName);
            if SpellConfig then begin
               LabelDicStatus.Caption := rsDictionaryLoaded;
               LabelDic.Caption := FullDicName;
               ConfigSave('CheckDictionary');
               Result := True;
            end else begin
                LabelDicStatus.Caption := rsDictionaryNotFound;
            end;
        end;
    end else debugln('ERROR - called CheckDictionary with Spell nil');
    debugln('CheckDictionary ' + FullDicName + ' return ' + booltostr(Result, True));
end;

function TSettings.CheckForDic(const DictPath: ANSIString): integer;
begin

end;

procedure TSettings.CheckSpelling(const DicFullName: string);
begin

end;

procedure TSettings.DicDefaults(var DicPathAlt: string);
begin

end;

procedure TSettings.LoadSettings();
begin
    CheckShowIntLinks.Checked := ShowIntLinks;
    CheckShowExtLinks.Checked := ShowExtLinks;
    CheckManyNoteBooks.checked := ManyNoteBooks;
    CheckCaseSensitive.Checked := SearchCaseSensitive;
    CheckShowSplash.Checked := ShowSplash;
    CheckAutostart.Checked := Autostart;
    CheckShowSearchAtStart.Checked := ShowSearchAtStart;

    if(SyncOption = AlwaysAsk) then RadioAlwaysAsk.Checked := true
    else if(SyncOption = UseLocal) then RadioUseLocal.Checked := true
    else if(SyncOption = UserServer) then RadioUseServer.Checked := true
    else RadioMakeCopy.Checked := true;
end;


procedure TSettings.RadioConflictChange(Sender: TObject);
begin
   if RadioAlwaysAsk.Checked then SyncOption := AlwaysAsk
   else if RadioUseLocal.Checked then SyncOption := UseLocal
   else if RadioUseServer.Checked then SyncOption := UseServer
   else if RadioMakeCopy.Checked then SyncOption := MakeCopy;
end;


procedure TSett.SpeedButHelpClick(Sender: TObject);
begin
        MainForm.ShowHelpNote('sync-ng.note');
end;

procedure TSett.SpeedButHideClick(Sender: TObject);
begin
  FormHide(Sender);
end;


procedure TSett.SpeedButtTBMenuClick(Sender: TObject);
begin
    PMenuMain.Popup;
end;


procedure TSett.RadioSyncChange(Sender: TObject);
begin
    SyncFirstRun :=false;

    CheckBoxAutoSync.Enabled:= not RadioSyncNone.checked;
    ConfigSave('RadioSyncChange');
end;

procedure TSett.TabSpellResize(Sender: TObject);
begin
    ButtonSetSpellLibrary.Width := (TabSpell.Width div 2) -7;
    ButtonSetDictionary.Width := ButtonSetSpellLibrary.Width;
end;



    { ----------------- S P E L L I N G ----------------------}

ResourceString
    rsSelectLibrary = 'Select your hunspell library';
    rsSelectDictionary = 'Select the dictionary you want to use';
    rsDictionaryLoaded = 'Dictionary Loaded OK';
    rsDictionaryFailed = 'Library Not Loaded';
    rsDictionaryNotFound = 'No Dictionary Found';

procedure TSett.ButtonSetSpellLibraryClick(Sender: TObject);
begin
    OpenDialogLibrary.InitialDir := ExtractFilePath(LabelLibrary.Caption);
    OpenDialogLibrary.Filter := 'Library|libhunspell*';
    OpenDialogLibrary.Title := rsSelectLibrary;
    if OpenDialogLibrary.Execute then begin
        LabelLibrary.Caption := TrimFilename(OpenDialogLibrary.FileName);
        CheckSpelling();
    end;
end;


function TSett.CheckForDic(const DictPath : ANSIString) : integer;
var
    Info : TSearchRec;
begin
    LabelError.Caption := '';
    ListBoxDic.Clear;
    ListBoxDic.Enabled := False;
    if FindFirst(AppendPathDelim(DictPath) + '*.dic', faAnyFile and faDirectory, Info)=0 then begin
        repeat
            ListBoxDic.Items.Add(Info.Name);
        until FindNext(Info) <> 0;
    end;
    FindClose(Info);
    if DebugModeSpell then debugln('CheckForDic searched ' + DictPath + ' and found ' + inttostr(ListBoxDic.Items.Count));
    if ListBoxDic.Items.Count > 0 then begin
        DicPath := DictPath;
        LabelDic.Caption := DictPath;
    end;
    exit(ListBoxDic.Items.Count);
end;


procedure TSett.ListBoxDicClick(Sender: TObject);
begin
    if ListBoxDic.ItemIndex > -1 then
        CheckDictionary(AppendPathDelim(DicPath) + ListBoxDic.Items.Strings[ListBoxDic.ItemIndex]);
end;


procedure TSett.DicDefaults(var DicPathAlt : string);
begin
    DicPathAlt := ExtractFilePath(Application.ExeName);
    {$ifdef WINDOWS}
    DicPath := 'C:\Program Files\LibreOffice 5\share\extensions\dict-en\';
    {$ENDIF}
    {$ifdef DARWIN}
    DicPath := '/Library/Spelling/';
    DicPathAlt := '/Applications/tomboy-reborn.app/Contents/Resources/';
    {$endif}
    {$ifdef LINUX}
    DicPath := '/usr/share/hunspell/';
    DicPathAlt := '/usr/share/myspell/';
    {$ENDIF}
end;

procedure TSett.CheckSpelling(const DicFullName : string = '');
var
    DicPathAlt, DicToCheck : AnsiString;

begin
    { The hunspell unit tries to find a library using some educated guesses.
      Once found, its saved in config and we pass that to hunspell as a suggested
      first place to try.
      We set likely dictionary locations here.
    }
    DicToCheck := '';
    LabelError.Caption:='';
    ListBoxDic.enabled:= False;
    LabelDic.Visible := False;
    LabelDicStatus.Visible := False;
    LabelDicPrompt.Visible := False;
    SpellConfig := False;
    if DicFullName = '' then DicDefaults(DicPathAlt);        // startup mode
    DebugModeSpell := Application.HasOption('debug-spell');
    // LabelLibrary.Caption := '/usr/local/Cellar/hunspell/1.6.2/lib/libhunspell-1.6.0.dylib';
    if fileexists(LabelLibrary.Caption) then		// make sure file from config is still valid
    	Spell :=  THunspell.Create(DebugModeSpell, LabelLibrary.Caption)
    else Spell :=  THunspell.Create(DebugModeSpell);
    if Spell.ErrorMessage <> '' then begin
        LabelLibraryStatus.Caption := rsDictionaryFailed;
        exit();
    end;
    if DebugModeSpell then debugln('Library OK, lets look for dictionary');
    LabelLibraryStatus.caption := rsDictionaryLoaded;
    LabelLibrary.Caption := Spell.LibraryFullName;
    LabelDicStatus.Visible := True;
    LabelDic.Visible := True;
    if DicFullName = '' then begin
        if (not DirectoryExistsUTF8(LabelDic.Caption))
                    and (FileExistsUTF8(LabelDic.Caption)) then  // we have a nominated file from config
            if CheckDictionary(LabelDic.Caption) then exit;      // All good, use it !
        if  0 = CheckForDic(DicPath) then begin                  // We ll try our defaults ....
            if 0 = CheckForDic(DicPathAlt) then begin
                LabelDicStatus.Caption := rsDictionaryNotFound;
                exit();
            end;
        end;
     end else DicToCheck := DicFullName;
     if ListBoxDic.Items.Count = 1 then
         DicToCheck := AppendPathDelim(LabelDic.Caption) + ListBoxDic.Items.Strings[0];
    if  ListBoxDic.Items.Count > 1 then begin                     // user must select
        LabelDicStatus.Caption := rsSelectDictionary;
        ListBoxDic.Enabled:= True;
        exit();
    end;
     // if to here, we have 1 candidate dictionary, either exactly 1 found or DicFullName has content
    if CheckDictionary(DicToCheck) then
        if DebugModeSpell then debugln('Spelling Configured.');
end;

{ --------------------- H O U S E    K E E P I NG -------------------- }

procedure TSett.FormHide(Sender: TObject);
begin
    FreeandNil(Spell);
end;

procedure TSett.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
    if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then begin
      if key = ord('N') then begin SearchForm.OpenNote(); Key := 0; exit(); end;
      if key = VK_Q then MainForm.Close();
    end;
end;

procedure TSett.FormShow(Sender: TObject);
begin
    if not assigned(Spell) then
        Spell := THunspell.Create(Application.HasOption('debug-spell'), LabelLibrary.Caption);
    Label15.Caption:='';
end;

// We only really close when told by RTSearch that The Exit Menu choice from TrayIcon was clicked.
procedure TSett.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
	if AllowClose then begin
    	CloseAction := caFree;
        SearchForm.Close;
	end else CloseAction := caHide;
end;

procedure TSett.FormCreate(Sender: TObject);
begin
    AreClosing := false;
    Top := 100;
    Left := 300;

    ConfigReading := False;
    ConfigWriting := False;

    ExportPath := '';
    LabelLibrary.Caption := '';
    labelNotesPath.Caption := NotesDir;

    ConfigCheck();

    PageControl1.ActivePage := TabBasic;
    //NeedRefresh := False;
    CheckSpelling();
    PageControl1.ActivePage := TabBasic;

    ButtonNCSetup.enabled := false;
    RadioSyncNC.enabled := false;
    LabelNCSyncURL.Hint := 'NextCloud / Grauphel will be in a future Release';
    LabelNCSyncURL.ShowHint := True;
end;

procedure TSett.FormDestroy(Sender: TObject);
begin
    FreeandNil(Spell);
end;


function TSett.getSyncConfigured() : boolean;
begin
    Result := (getSyncType() <> TSyncTransport.SyncNone);
end;




procedure TSett.ButtonNCSetupClick(Sender: TObject);
begin

end;

procedure TSett.SetColours();
begin
    if UserSetColours then exit;        // will have already been set by config or by colour form.
    if DarkTheme then begin
        //debugln('Its definltly a Dark Theme');
        BackGndColour:= clBlack;
        HiColour := clDkGray;
        TextColour := clLtGray;
        TitleColour:= clTeal;
    end else begin
        BackGndColour := clCream;
        HiColour := clYellow;
        TextColour := clBlack;
        TitleColour := clBlue;
    end;
end;

procedure TSett.ButtonSetColoursClick(Sender: TObject);
begin
    FormColours.CBack   := BackGndColour;
    FormColours.CHiBack := HiColour;
    FormColours.CText   := TextColour;
    FormColours.CTitle  := TitleColour;
    case FormColours.ShowModal of
        mrRetry  :  begin
                        UserSetColours := False;
                        SetColours();
                        ConfigSave('ButtonSetColoursClick');
                    end;
        mrOK     :  begin
	                    BackGndColour := FormColours.CBack;
	                    HiColour := FormColours.CHiBack;
	                    TextColour := FormColours.CText;
	                    TitleColour := FormColours.CTitle;
                         UserSetColours := True;
                        ConfigSave('ButtonSetColoursClick2');
                    end;
//        mrCancel : showmessage('Do nothing');
	end;
end;


procedure TSett.TimerAutoSyncTimer(Sender: TObject);
var
   elapse : LongInt;
begin
    elapse := StrToInt(EditTimerSync.Text);

    if(elapse<1) then begin
        CheckBoxAutoSync.checked := false;
        exit;
    end;

    if (not CheckBoxAutoSync.checked) then exit;

    if (getSyncConfigured() and getSyncTested()) then begin
       FormSync.RunSyncHidden();
    end;

    TimerAutoSync.Interval:= elapse*60*1000;
    TimerAutoSync.Enabled := true;
end;


end.

