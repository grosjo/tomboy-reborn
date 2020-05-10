program Tomboy_Reborn;

{$mode objfpc}{$H+}

uses
    {$DEFINE UseCThreads}
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, Classes, Forms, SysUtils, StdCtrls, LazFileUtils, LazLogger,
    ExtCtrls,
    TRcommon, TRtexts , TRsearchUnit, TRsettings;

{$R *.res}

var
    CmdLineErrorMsg : String;
    s : TSettings;
begin
    Application.Scaled:=True;
  Application.Title:='TomboyReborn';
    RequireDerivedFormResource:=True;
    Application.Initialize;

    ConfigDir := GetDefaultConfigDir();
    ConfigFile := ConfigDir + 'tomboy-reborn.cfg';
    NotesDir := GetDefaultNotesDir();

    CmdLineErrorMsg := Application.CheckOptions('hgdc:v', 'help gnome3 delay-start config-dir: version');
    if Application.HasOption('h', 'help') then
    begin
       debugln('   --delay-start                ' + rsHelpDelay);
       debugln('   -h --help                    ' + rsHelpHelp);
       debugln('   --version                    ' + rsHelpVersion);
       debugln('   -g --gnome3                  ' + rsHelpRedHat);
       debugln('   --config-dir=PATH_to_DIR     ' + rsHelpConfig);
       exit();
    end;

    if Application.HasOption('c', 'config-dir') then
       ConfigDir := Application.GetOptionValue('c', 'config-dir');

    if Application.HasOption('d', 'delay-start') then
       Sleep(2000);

    if Application.HasOption('v', 'version') then
    begin
       debugln(AboutString());
       exit();
    end;

    case ConfigRead('main') of
        -1 : begin
	     debugln('Error reading config');
	     exit();
           end;
        0 : begin
             Application.CreateForm(TSettings, s);
             s.ShowModal;
             FreeAndNil(s);
           end;
    end;

    UseTrayIcon := true;

    if Application.HasOption('g', 'gnome3') then begin
        UseTrayIcon := false;
        SearchAtStart := true;
    end;

    Application.CreateForm(TSearchForm, mainWindow);

    Application.Run;
end.

