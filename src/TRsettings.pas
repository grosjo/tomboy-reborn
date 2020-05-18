unit TRsettings;

{	HISTORY
	2020/05/06 - Reshaping @DavidBannon work to adpat to real needs
}

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, Menus, ComCtrls, ExtCtrls,
    TRcommon, TRtexts;

// Types;

type

    { TSettings }

    TSettings = class(TForm)
          ButtDefaultNoteDir: TButton;
          ButtonSetColours: TButton;
          ButtonNCSetup: TSpeedButton;
	  CheckBoxAutoSync: TCheckBox;
          Label14: TLabel;
          LastUsedMax: TEdit;
          GroupBox2: TGroupBox;
          Label10: TLabel;
          Label11: TLabel;
          LabelPath: TLabel;
          Label16: TLabel;
          EditTimerSync: TEdit;
	  GroupBoxSync: TGroupBox;
          Label17: TLabel;
	  Label4: TLabel;
	  Label5: TLabel;
          Label6: TLabel;
          Label7: TLabel;
          Label8: TLabel;
          Label9: TLabel;
          LabelError: TLabel;
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
          CheckManyNotebooks: TCheckBox;
          CheckShowSearchAtStart: TCheckBox;
	  CheckShowExtLinks: TCheckBox;
	  CheckShowIntLinks: TCheckBox;
          GroupBox4: TGroupBox;
	  GroupBox5: TGroupBox;
	  Label1: TLabel;
          Label12: TLabel;
          Label13: TLabel;
          Label15: TLabel;
          LabelDic: TLabel;
          LabelLibrary: TLabel;
          LabelDicStatus: TLabel;
          LabelLibraryStatus: TLabel;
	  Label2: TLabel;
	  Label3: TLabel;
	  LabelNotesPath: TLabel;
	  LabelSettingPath: TLabel;
          ListBoxDic: TListBox;

          PageControl1: TPageControl;
          RadioAlwaysAsk: TRadioButton;
          RadioFontHuge: TRadioButton;
	  RadioFontBig: TRadioButton;
	  RadioFontMedium: TRadioButton;
	  RadioFontSmall: TRadioButton;
	  RadioUseLocal: TRadioButton;
	  RadioUseServer: TRadioButton;
          SettingsCancel: TSpeedButton;
          ButtonFileSetup: TSpeedButton;
          SettingsOK: TSpeedButton;
	  TabBasic: TTabSheet;
          TabSpell: TTabSheet;
	  TabSync: TTabSheet;



        procedure ButtDefaultNoteDirClick(Sender: TObject);
        procedure ButtonSetColoursClick(Sender: TObject);
        procedure ButtonNCSetupClick(Sender: TObject);
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
        procedure EditTimerSyncChange(Sender: TObject);
        procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure FormShow(Sender: TObject);
        procedure LastUsedMaxChange(Sender: TObject);
        procedure ListBoxDicClick(Sender: TObject);
	procedure RadioConflictChange(Sender: TObject);
        procedure RadioFontBigChange(Sender: TObject);
        procedure RadioFontHugeChange(Sender: TObject);
        procedure RadioFontMediumChange(Sender: TObject);
        procedure RadioFontSmallChange(Sender: TObject);
        procedure SettingsOKClick(Sender: TObject);
	procedure SettingsCancelClick(Sender: TObject);
        procedure ButtonFileSetupClick(Sender: TObject);
        procedure RadioSyncChange(Sender: TObject);
        procedure TimerAutoSyncTimer(Sender: TObject);
   private
      settingsloading : boolean;
      procedure LoadSettings();
      procedure VerifyDictLibrary(const libname : String);
      function DictFilesSearch() : integer;
      function VerifyDictFile(const FullDicName: string): boolean;

    end;


implementation

{$R *.lfm}


uses LazLogger,
    LazFileUtils,
    LCLType,
    TRcolours, TRnextSetup, TRhunspell;


procedure TSettings.ButtDefaultNoteDirClick(Sender: TObject);
begin
   TRlog('ButtDefaultNoteDirClick');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   NotesDir := GetDefaultNotesDir();
   LabelNotespath.Caption := NotesDir;
end;

procedure TSettings.ButtonSetColoursClick(Sender: TObject);
begin
   TRlog('ButtonSetColoursClick');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   FormColours.CBack   := BackGndColour;
   FormColours.CHiBack := HiColour;
   FormColours.CText   := TextColour;
   FormColours.CTitle  := TitleColour;

   case FormColours.ShowModal of
      mrOK     :  begin
         BackGndColour := FormColours.CBack;
	 HiColour := FormColours.CHiBack;
	 TextColour := FormColours.CText;
	 TitleColour := FormColours.CTitle;
      end;
   end;
end;

procedure TSettings.ButtonNCSetupClick(Sender: TObject);
var
  FormNCSetup: TFormNCSetup;
begin
   TRlog('ButtonNCSetupClick');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   Application.CreateForm(TFormNCSetup, FormNCSetup);

   if(SyncNCUrl = '')
   then FormNCSetup.URL.Text := rsSyncNCDefault
   else FormNCSetup.URL.Text   :=  SyncNCUrl;

   FormNCSetup.setKey(SyncNCKey);
   FormNCSetup.setToken(SyncNCToken);

   FormNCSetup.ShowModal;

   if( FormNCSetup.isSuccess() ) then
   begin
      SyncNCUrl := FormNCSetup.URL.Text;
      SyncNCKey := FormNCSetup.getKey();
      SyncNCToken := FormNCSetup.getToken();
      SyncNCSecret := FormNCSetup.getTokenSecret();

      LabelNCSyncURL.Caption := SyncNCUrl;

      RadioSyncNC.checked := true;
   end;

   FreeAndNil(FormNCSetup);

end;


procedure TSettings.ButtonFixedFontClick(Sender: TObject);
var
  fd : TFontDialog;
begin
   TRlog('ButtonFixedFontClick');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   fd := TFontDialog.Create(Self);
   fd.Font.Name := FixedFont;
   fd.Font.Size := 10;
   fd.Title := 'Select Fixed Spacing Font';
   fd.PreviewText:= 'abcdef ABCDEF 012345';
   fd.Options := fd.Options + [fdFixedPitchOnly];

   If fd.Execute then FixedFont := fd.Font.name;

   ButtonFixedFont.Hint := FixedFont;

   FreeAndNil(fd);
end;

procedure TSettings.ButtonFontClick(Sender: TObject);
var
  fd : TFontDialog;
begin
   TRlog('ButtonFontClick');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   fd := TFontDialog.Create(Self);
   fd.Font.Name := UsualFont;
   fd.Font.Size := 10;
   fd.Title := 'Select Usual Font';
   fd.PreviewText:= 'abcdef ABCDEF 012345';

   If fd.Execute then UsualFont := fd.Font.name;

   ButtonFont.Hint := UsualFont;

   FreeAndNil(fd);
end;


procedure TSettings.ButtonSetNotePathClick(Sender: TObject);
var
    dd : TSelectDirectoryDialog;
begin
   TRlog('ButtonSetNotePathClick');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   dd := TSelectDirectoryDialog.Create(Self);
   if dd.Execute then
   begin
        NotesDir :=  AppendPathDelim(chomppathdelim(dd.FileName));
        LabelNotespath.Caption := NotesDir;
   end;
   FreeAndNil(dd);
end;


procedure TSettings.CheckAutostartChange(Sender: TObject);
begin
   TRlog('CheckAutostartChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   Autostart := CheckAutostart.Checked;
end;

procedure TSettings.CheckBoxAutoSyncChange(Sender: TObject);
begin

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   EditTimerSync.Enabled := CheckBoxAutoSync.checked;
   Label16.Enabled := CheckBoxAutoSync.checked;
   Label17.Enabled := CheckBoxAutoSync.checked;

   if(not CheckBoxAutoSync.checked) then SyncRepeat:=0;

   TRlog('CheckBoxAutoSyncChange');
end;

procedure TSettings.CheckManyNotebooksChange(Sender: TObject);
begin
   TRlog('CheckManyNotebooksChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   ManyNoteBooks := CheckManyNotebooks.Checked;
end;

procedure TSettings.CheckShowExtLinksChange(Sender: TObject);
begin
   TRlog('CheckShowExtLinksChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   ShowExtLinks := CheckShowExtLinks.Checked;
end;

procedure TSettings.CheckShowIntLinksChange(Sender: TObject);
begin
   TRlog('CheckShowIntLinksChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   ShowIntLinks := CheckShowIntLinks.Checked;
end;

procedure TSettings.CheckShowSearchAtStartChange(Sender: TObject);
begin
   TRlog('CheckShowSearchAtStartChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   if((not UseTrayIcon) and (not CheckShowSearchAtStart.Checked)) then
   begin
       ShowMessage(rsCanNotHideSearch);
       CheckShowSearchAtStart.Checked := true;
   end else SearchAtStart := CheckShowSearchAtStart.Checked;
end;

procedure TSettings.EditTimerSyncChange(Sender: TObject);
begin
   TRlog('EditTimerSyncChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   try
      SyncRepeat := StrToInt(EditTimerSync.Caption);
   except on E:Exception do
      begin
         TRlog(E.message);
         SyncRepeat := 0;
         EditTimerSync.Caption := '0';
      end;
   end;
end;

procedure TSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TRlog('FormClose');
  ConfigRead('FormClose');
end;

procedure TSettings.FormCreate(Sender: TObject);
begin
  TRlog('FormCreate');
  settingsloading := true;
end;

procedure TSettings.FormDestroy(Sender: TObject);
begin
  TRlog('FormSettingsDestroy');
end;

procedure TSettings.FormHide(Sender: TObject);
begin
  TRlog('FormSettingsHide');
end;

procedure TSettings.FormShow(Sender: TObject);
begin
     TRlog('FormSettingsShow');
     LoadSettings();
     Left := mainWindow.Left + random(100);
     Top := mainWindow.Top + random(100);
end;

procedure TSettings.LastUsedMaxChange(Sender: TObject);
begin
  TRlog('LastUsedMaxChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   try
      LastUsedNB := StrToInt(LastUsedMax.Caption);
   except on E:Exception do
      begin
         TRlog(E.message);
         LastUsedNB := 10;
         LastUsedMax.Caption := '10';
      end;
   end;
end;

procedure TSettings.RadioConflictChange(Sender: TObject);
begin
   TRlog('RadioConflictChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   if RadioAlwaysAsk.Checked then SyncClashOption := TSyncClashOption.AlwaysAsk
     else if RadioUseLocal.Checked then SyncClashOption := TSyncClashOption.UseLocal
     else if RadioUseServer.Checked then SyncClashOption := TSyncClashOption.UseServer
     else if RadioMakeCopy.Checked then SyncClashOption := TSyncClashOption.MakeCopy;
end;

procedure TSettings.RadioFontBigChange(Sender: TObject);
begin
   TRlog('RadioFontBigChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   if RadioFontBig.checked then FontRange := FontBig;
end;

procedure TSettings.RadioFontHugeChange(Sender: TObject);
begin
   TRlog('RadioFontHugeChange');

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   if RadioFontHuge.checked then FontRange := FontHuge;
end;

procedure TSettings.RadioFontMediumChange(Sender: TObject);
begin
  TRlog('RadioFontMediumChange');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

  if RadioFontMedium.checked then FontRange := FontMedium;
end;

procedure TSettings.RadioFontSmallChange(Sender: TObject);
begin
  TRlog('RadioFontSmallChange');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

  if RadioFontSmall.Checked then FontRange := FontSmall;
end;

procedure TSettings.SettingsOKClick(Sender: TObject);
begin
  TRlog('SettingsOKClick');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

  ConfigWrite('SettingsOKClick');
  Self.Close;
end;

procedure TSettings.SettingsCancelClick(Sender: TObject);
begin
  TRlog('SettingsCancelClick');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

  Self.Close;
end;


procedure TSettings.ButtonFileSetupClick(Sender: TObject);
var
    dd : TSelectDirectoryDialog;
begin
  TRlog('ButtonFileSetupClick');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   dd := TSelectDirectoryDialog.Create(Self);
   dd.FileName := SyncFileRepo;
   if dd.Execute then
   begin
      LabelFileSync.Caption := AppendPathDelim(ChompPathDelim(dd.FileName));
      SyncFileRepo := LabelFileSync.Caption;
      TRlog('ButtonFileSetupClick SyncFileRepo='+SyncFileRepo);
   end;
   FreeAndNil(dd);
end;

procedure TSettings.RadioSyncChange(Sender: TObject);
var
   s : TSyncTransport;
begin
   TRlog('RadioSyncChange '+Sender.ToString);

   if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

   CheckBoxAutoSync.Enabled := not RadioSyncNone.checked;
   EditTimerSync.Enabled := CheckBoxAutoSync.Checked and not RadioSyncNone.checked;
   Label16.Enabled := CheckBoxAutoSync.Checked and not RadioSyncNone.checked;
   Label17.Enabled := CheckBoxAutoSync.Checked and not RadioSyncNone.checked;

   if(RadioSyncNone.Checked) then s := TSyncTransport.SyncNone
    else if(RadioSyncFile.Checked) then s :=TSyncTransport.SyncFile
    else s := TSyncTransport.SyncNextCloud;

   if(s <> SyncType) then
   begin
     SyncType :=s;
   end;
end;


procedure TSettings.LoadSettings();
var
   i : integer;
begin

  TRlog('LoadSettings');

  settingsloading :=true;

  LabelSettingPath.Caption := ConfigFile;
  LabelNotesPath.Caption := NotesDir;

  CheckShowIntLinks.Checked := ShowIntLinks;
  CheckShowExtLinks.Checked := ShowExtLinks;
  CheckManyNoteBooks.checked := ManyNoteBooks;
  CheckAutostart.Checked := Autostart;
  CheckShowSearchAtStart.Checked := SearchAtStart;

  if(SyncClashOption = TSyncClashOption.AlwaysAsk) then RadioAlwaysAsk.Checked := true
    else if(SyncClashOption = TSyncClashOption.UseLocal) then RadioUseLocal.Checked := true
    else if(SyncClashOption = TSyncClashOption.UseServer) then RadioUseServer.Checked := true
    else RadioMakeCopy.Checked := true;

  ButtonFont.Hint := UsualFont;
  ButtonFixedFont.Hint := FixedFont;

  case FontRange of
     FontHuge   : RadioFontHuge.Checked := true;
     FontBig    : RadioFontBig.Checked := true;
     FontMedium   : RadioFontMedium.Checked := true;
     FontSmall   : RadioFontSmall.Checked := true;
  end;

  LastUsedMax.Caption:= IntToStr(LastUsedNB);

  CheckBoxAutoSync.Checked := (SyncRepeat>0);
  if(SyncRepeat>0) then EditTimerSync.Caption:= IntToStr(SyncRepeat);

  case SyncClashOption  of
     TSyncClashOption.AlwaysAsk   : RadioAlwaysAsk.Checked  := true;
     TSyncClashOption.UseLocal    : RadioUseLocal.Checked   := true;
     TSyncClashOption.UseServer   : RadioUseServer.Checked  := true;
     TSyncClashOption.MakeCopy    : RadioMakeCopy.Checked   := true;
  end;

  case SyncType of
     TSyncTransport.SyncNone      : RadioSyncNone.Checked   := true;
     TSyncTransport.SyncFile      : RadioSyncFile.Checked   := true;
     TSyncTransport.SyncNextCloud : RadioSyncNC.Checked     := true;
  end;

  if(length(SyncFileRepo)>0)
     then LabelFileSync.Caption := SyncFileRepo
     else LabelFileSync.Caption := rsSyncNotConfig;

  if(length(SyncNCUrl)>0)
     then LabelNCSyncURL.Caption := SyncNCUrl
     else LabelNCSyncURL.Caption := rsSyncNotConfig;

  LabelLibrary.Caption := DictLibrary;

  ListBoxDic.Clear;
  VerifyDictLibrary(DictLibrary);
  DictFilesSearch();
  VerifyDictFile(DictFile);

  LabelPath.Caption := DictPath;
  LabelDic.Caption := DictFile;

  i:= 0;
  while(i<ListBoxDic.Items.Count) do
  begin
     ListBoxDic.Selected[i]:=(CompareStr(DictFile,DictPath+ListBoxDic.Items[i])=0);
     i := i + 1;
  end;

  settingsloading := false;
  TRlog('Done loading settings');

end;


    { ----------------- S P E L L I N G ----------------------}

procedure TSettings.ButtonSetSpellLibraryClick(Sender: TObject);
var
    odd: TOpenDialog;
begin
  TRlog('ButtonSetSpellLibraryClick');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

  odd := TOpenDialog.Create(Self);

  odd.InitialDir := ExtractFilePath(DictLibrary);
  odd.Filter := 'Library|libhunspell*';
  odd.Title := rsSelectLibrary;
  if odd.Execute then
  begin
        VerifyDictLibrary(odd.FileName);
  end;
  FreeAndNil(odd);
end;

procedure TSettings.ButtonSetDictionaryClick(Sender: TObject);
var
    odd: TOpenDialog;
begin
  TRlog('ButtonSetDictionaryClick');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

  odd := TOpenDialog.Create(Self);

  odd.InitialDir := DictPath;
  odd.Filter := 'Dictionary|*.dic';
  odd.Title := rsSelectDictionary;
  if odd.Execute then
  begin
      DictPath := AppendPathDelim(ChompPathDelim(ExtractFilePath(odd.FileName)));
      TRlog('New DictPath = '+DictPath+ ' from '+odd.FileName);
      LabelPath.Caption := DictPath;
      DictFilesSearch();
  end;

  FreeAndNil(odd);
end;

function TSettings.DictFilesSearch() : integer;
var
    Info : TSearchRec;
begin
    ListBoxDic.Clear;
    ListBoxDic.Enabled := False;
    if FindFirstUTF8(DictPath + '*.dic', faAnyFile, Info)=0 then
    begin
        repeat ListBoxDic.Items.Add(Info.Name);
        until FindNext(Info) <> 0;
    end;
    FindClose(Info);
    TRlog('CheckForDic searched ' + DictPath + ' and found ' + inttostr(ListBoxDic.Items.Count));
    if ListBoxDic.Items.Count > 0 then
    begin
         ListBoxDic.Enabled := True;
    end;
    LabelDic.Caption := '';
    LabelDicStatus.Caption := rsSpellNoDic;
    exit(ListBoxDic.Items.Count);
end;

function TSettings.VerifyDictFile(const FullDicName: string): boolean;
var
    Spell : THunspell;
begin
   result := false;

   if not FileExistsUTF8(FullDicName) then
   begin
      TRlog('VerifyDictFile: ' + FullDicName + 'does not exist');
      LabelDicStatus.Caption := rsCannotFind + FullDicName;
      exit(false);
   end;

   Spell :=  THunspell.Create(DictLibrary);

   if Spell.SetDictionary(FullDicName)
   then begin
        LabelDicStatus.Caption := rsDictionaryLoaded;
        LabelDic.Caption := FullDicName;
        DictFile := FullDicName;
        Result := True;
   end else begin
      TRlog(rsDictionaryNotFound);
       LabelDicStatus.Caption := rsDictionaryNotFound;
   end;

   FreeAndNil(Spell);
   TRlog('CheckDictionary ' + FullDicName + ' return ' + booltostr(Result, True));
end;

procedure TSettings.ListBoxDicClick(Sender: TObject);
begin
  TRlog('ListBoxDicClick');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

    if ListBoxDic.ItemIndex > -1 then
        VerifyDictFile(DictPath + ListBoxDic.Items.Strings[ListBoxDic.ItemIndex]);
end;

procedure TSettings.VerifyDictLibrary(const libname : String);
var
    Spell : THunspell;
begin
    LabelLibraryStatus.Caption := '';

    if FileExistsUTF8(libname)
    then Spell :=  THunspell.Create(libname)
    else Spell :=  THunspell.Create();

    if Spell.ErrorMessage <> '' then
    begin
        LabelLibraryStatus.Caption := rsDictionaryFailed;
        LabelLibrary.Caption := '';
        FreeAndNil(Spell);
        exit();
    end;

    DictLibrary := Spell.LibraryFullName;
    TRlog('Library OK, lets look for dictionary');
    LabelLibraryStatus.caption := rsDictionaryLoaded;
    LabelLibrary.Caption := DictLibrary;
    FreeAndNil(Spell);
end;

procedure TSettings.TimerAutoSyncTimer(Sender: TObject);
begin
  TRlog('TimerAutoSyncTimer');

  if(settingsloading) then begin TRlog('Event while loading'); exit(); end;

  SyncRepeat := StrToInt(EditTimerSync.Text);

  if(SyncRepeat<1) then
  begin
        CheckBoxAutoSync.checked := false;
        SyncRepeat := 0;
  end;
end;


end.

