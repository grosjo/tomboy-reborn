program Tomboy_Reborn;

{$mode objfpc}{$H+}

uses
    Interfaces, Classes, Forms, SysUtils, StdCtrls, LazFileUtils, LazLogger,
    ExtCtrls, UniqueInstanceRaw, Dialogs,
    TRcommon, TRtexts , TRmain, TRsettings;

{$R *.res}

var
    CmdLineErrorMsg : String;
    fs : TFormSettings;

begin
    Application.Scaled:=True;
    Application.Title:='TomboyReborn';
    RequireDerivedFormResource:=True;
    Application.Initialize;

    if(InstanceRunning())
    then begin
       ShowMessage('Another Tomboy Reborn is already running');
       debugln('Another Tomboy Reborn is already running');
       exit();
    end;

    ConfigDir := GetDefaultConfigDir();
    ConfigFile := ConfigDir + 'tomboy-reborn.cfg';
    NotesDir := GetDefaultNotesDir();

    CmdLineErrorMsg := Application.CheckOptions('hgdc:v', 'help gnome3 delay-start config-dir: version debug');
    if Application.HasOption('h', 'help') then
    begin
       debugln('   --delay-start                ' + rsHelpDelay);
       debugln('   -h --help                    ' + rsHelpHelp);
       debugln('   -v --version                    ' + rsHelpVersion);
       debugln('   -g --gnome3                  ' + rsHelpRedHat);
       debugln('   -c --config-dir=PATH_to_DIR     ' + rsHelpConfig);
       debugln('   -d --debug		' + rsHelpDebug);
       exit();
    end;

    if Application.HasOption('c', 'config-dir') then
       ConfigDir := Application.GetOptionValue('c', 'config-dir');

    if Application.HasOption('delay-start') then
       Sleep(2000);

    if Application.HasOption('d', 'debug') then
       Debug :=true;

    if Application.HasOption('v', 'version') then
    begin
       debugln(AboutString());
       exit();
    end;

    LastUsed := TStringList.Create;

    case ConfigRead('main') of
        -1 : begin
	     debugln('Error reading config');
	     exit();
           end;
        0 : begin
             Application.CreateForm(TFormSettings, fs);
             fs.ShowModal;
             FreeAndNil(fs);
           end;
    end;

    UseTrayIcon := true;

    if Application.HasOption('g', 'gnome3') then begin
        UseTrayIcon := false;
        SearchAtStart := true;
    end;

    Application.CreateForm(TFormMain, mainWindow);

    Application.Run;

    LastUsed.Free;

    TRlog('End of LPR');
end.

