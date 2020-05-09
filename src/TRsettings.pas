unit TRsettings;

{	HISTORY
	2020/05/06 - Reshaping @DavidBannon work to adpat to real needs
}

{$mode objfpc}{$H+}

interface

uses Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
    Buttons, Menus, ComCtrls, ExtCtrls, StrUtils,
    TRcommon, TRtexts;

// Types;

type

    { TSettings }

    TSettings = class(TForm)
          ButtDefaultNoteDir: TButton;
          ButtonSetColours: TButton;
          ButtonNCSetup: TSpeedButton;
	  CheckBoxAutoSync: TCheckBox;
          GroupBox2: TGroupBox;
          Label10: TLabel;
          LabelPath: TLabel;
          Label16: TLabel;
          EditTimerSync: TEdit;
          GroupBox1: TGroupBox;
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
          CheckCaseSensitive: TCheckBox;
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
        procedure onCheckCaseSensitive(Sender: TObject);
	procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
        procedure FormCreate(Sender: TObject);
        procedure FormDestroy(Sender: TObject);
        procedure FormHide(Sender: TObject);
        procedure FormShow(Sender: TObject);
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
     NotesDir := GetDefaultNotesDir();
     LabelNotespath.Caption := NotesDir;
     //SearchForm.IndexNotes();
end;

procedure TSettings.ButtonSetColoursClick(Sender: TObject);
begin
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
         //UserSetColours := True;
      end;
   end;
end;

procedure TSettings.ButtonNCSetupClick(Sender: TObject);
var
  FormNCSetup: TFormNCSetup;
begin
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
      SyncFirstRun := true;
   end;

   FreeAndNil(FormNCSetup);

end;


procedure TSettings.ButtonFixedFontClick(Sender: TObject);
var
  fd : TFontDialog;
begin
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
   Autostart := CheckAutostart.Checked;
   debugln('CheckAutostartChange');
end;

procedure TSettings.CheckBoxAutoSyncChange(Sender: TObject);
begin
   EditTimerSync.Enabled := CheckBoxAutoSync.checked;
   Label16.Enabled := CheckBoxAutoSync.checked;
   Label17.Enabled := CheckBoxAutoSync.checked;

   if(not CheckBoxAutoSync.checked) then SyncRepeat:=0;

   debugln('CheckBoxAutoSyncChange');
end;

procedure TSettings.CheckManyNotebooksChange(Sender: TObject);
begin
  ManyNoteBooks := CheckManyNotebooks.Checked;
  debugln('CheckManyNotebooksChange');
end;

procedure TSettings.CheckShowExtLinksChange(Sender: TObject);
begin
  ShowExtLinks := CheckShowExtLinks.Checked;
  debugln('CheckShowExtLinksChange');
end;

procedure TSettings.CheckShowIntLinksChange(Sender: TObject);
begin
  ShowIntLinks := CheckShowIntLinks.Checked;
  debugln('CheckShowIntLinksChange');
end;

procedure TSettings.CheckShowSearchAtStartChange(Sender: TObject);
begin
  debugln('CheckShowSearchAtStartChange');
  if((not UseTrayIcon) and (not CheckShowSearchAtStart.Checked)) then
  begin
       ShowMessage(rsCanNotHideSearch);
       CheckShowSearchAtStart.Checked := true;
  end else SearchAtStart := CheckShowSearchAtStart.Checked;
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
  SearchCaseSensitive := CheckCaseSensitive.Checked;
  debugln('onCheckCaseSensitive');
end;

procedure TSettings.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  debugln('FormSettingsClose');
  ConfigRead('SettingsOKClick');
end;

procedure TSettings.FormCreate(Sender: TObject);
begin
  debugln('FormSettingsCreate');
end;

procedure TSettings.FormDestroy(Sender: TObject);
begin
  debugln('FormSettingsDestroy');
end;

procedure TSettings.FormHide(Sender: TObject);
begin
  debugln('FormSettingsHide');
end;

procedure TSettings.FormShow(Sender: TObject);
begin
     debugln('FormSettingsShow');
     LoadSettings();
end;

procedure TSettings.RadioConflictChange(Sender: TObject);
begin
     if RadioAlwaysAsk.Checked then SyncClashOption := TSyncClashOption.AlwaysAsk
     else if RadioUseLocal.Checked then SyncClashOption := TSyncClashOption.UseLocal
     else if RadioUseServer.Checked then SyncClashOption := TSyncClashOption.UseServer
     else if RadioMakeCopy.Checked then SyncClashOption := TSyncClashOption.MakeCopy;
     debugln('RadioFontBigChange');
end;

procedure TSettings.RadioFontBigChange(Sender: TObject);
begin
  if RadioFontBig.checked then FontRange := FontBig;
  end;

procedure TSettings.RadioFontHugeChange(Sender: TObject);
begin
  if RadioFontHuge.checked then FontRange := FontHuge;
end;

procedure TSettings.RadioFontMediumChange(Sender: TObject);
begin
  if RadioFontMedium.checked then FontRange := FontMedium;
end;

procedure TSettings.RadioFontSmallChange(Sender: TObject);
begin
    if RadioFontSmall.Checked then FontRange := FontSmall;
end;

procedure TSettings.SettingsOKClick(Sender: TObject);
begin
  debugln('SettingsOKClick');
  ConfigWrite('SettingsOKClick');
  Self.Close;
end;

procedure TSettings.SettingsCancelClick(Sender: TObject);
begin
  debugln('SettingsCancelClick(');
  Self.Close;
end;


procedure TSettings.ButtonFileSetupClick(Sender: TObject);
var
    dd : TSelectDirectoryDialog;
begin
   dd := TSelectDirectoryDialog.Create(Self);
   if dd.Execute then
   begin
      SyncFirstRun := false;
      LabelFileSync.Caption := AppendPathDelim(ChompPathDelim(ExtractFilePath(dd.FileName)));
      SyncFileRepo := LabelFileSync.Caption;
      debugln('ButtonFileSetupClick');
   end;
   FreeAndNil(dd);
end;

procedure TSettings.RadioSyncChange(Sender: TObject);
var
   s : TSyncTransport;
begin
   debugln('RadioSyncChange '+Sender.ToString);

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
     SyncFirstRun := true;
   end;
end;


procedure TSettings.LoadSettings();
var
   i : integer;
begin

  debugln('LoadSettings');

  LabelSettingPath.Caption := ConfigFile;
  LabelNotesPath.Caption := NotesDir;

  CheckShowIntLinks.Checked := ShowIntLinks;
  CheckShowExtLinks.Checked := ShowExtLinks;
  CheckManyNoteBooks.checked := ManyNoteBooks;
  CheckCaseSensitive.Checked := SearchCaseSensitive;
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

  CheckBoxAutoSync.Checked := (SyncRepeat>0);

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

  debugln('Done loading settings');

end;


    { ----------------- S P E L L I N G ----------------------}

procedure TSettings.ButtonSetSpellLibraryClick(Sender: TObject);
var
    odd: TOpenDialog;
begin
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
  odd := TOpenDialog.Create(Self);

  odd.InitialDir := DictPath;
  odd.Filter := 'Dictionary|*.dic';
  odd.Title := rsSelectDictionary;
  if odd.Execute then
  begin
      DictPath := AppendPathDelim(ChompPathDelim(ExtractFilePath(odd.FileName)));
      DebugLn('New DictPath = '+DictPath+ ' from '+odd.FileName);
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
    if FindFirst(DictPath + '*.dic', faAnyFile, Info)=0 then
    begin
        repeat ListBoxDic.Items.Add(Info.Name);
        until FindNext(Info) <> 0;
    end;
    FindClose(Info);
    debugln('CheckForDic searched ' + DictPath + ' and found ' + inttostr(ListBoxDic.Items.Count));
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

   if not FileExists(FullDicName) then
   begin
      debugln('VerifyDictFile: ' + FullDicName + 'does not exist');
      LabelDicStatus.Caption := rsCannotFindNote + FullDicName;
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
      debugln(rsDictionaryNotFound);
       LabelDicStatus.Caption := rsDictionaryNotFound;
   end;

   FreeAndNil(Spell);
   debugln('CheckDictionary ' + FullDicName + ' return ' + booltostr(Result, True));
end;

procedure TSettings.ListBoxDicClick(Sender: TObject);
begin
    if ListBoxDic.ItemIndex > -1 then
        VerifyDictFile(DictPath + ListBoxDic.Items.Strings[ListBoxDic.ItemIndex]);
end;

procedure TSettings.VerifyDictLibrary(const libname : String);
var
    Spell : THunspell;
begin
    LabelLibraryStatus.Caption := '';

    if fileexists(libname)
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
    debugln('Library OK, lets look for dictionary');
    LabelLibraryStatus.caption := rsDictionaryLoaded;
    LabelLibrary.Caption := DictLibrary;
    FreeAndNil(Spell);
end;

procedure TSettings.TimerAutoSyncTimer(Sender: TObject);
begin
    SyncRepeat := StrToInt(EditTimerSync.Text);

    if(SyncRepeat<1) then begin
        CheckBoxAutoSync.checked := false;
        SyncRepeat := 0;
    end;
end;


end.

