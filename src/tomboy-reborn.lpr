program Tomboy_Reborn;

{$mode objfpc}{$H+}

uses
    {$DEFINE UseCThreads}
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, Classes, Forms, SysUtils, StdCtrls, LazFileUtils, LazLogger,
    Spelling,
    TRcommon, TRsearchUnit, TRsettings, TRsyncUI,
    TRcolours, {Mainunit,} markdown, Index, SyncError,
    hunspell, syncutils, TRtexts, TRnextSetup;

{$R *.res}

begin
    Application.Scaled:=True;
    Application.Title:='Tomboy Reborn';
    RequireDerivedFormResource:=True;
    Application.Initialize;

    ConfigDir := GetDefaultConfigDir();
    ConfigFile := ConfigDir + 'tomboy-reborn.cfg';
    NotesDir := GetDefaultNotesDir();

    //Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TFormColours, FormColours);

    Application.CreateForm(TFormNCSetup, FormNCSetup);
    Application.CreateForm(TSettings, FormSettings);
    Application.CreateForm(TSearchForm, SearchForm);
    Application.CreateForm(TFormSync, FormSync);
    Application.CreateForm(TFormMarkdown, FormMarkdown);
    Application.CreateForm(TFormSyncError, FormSyncError);
    Application.Run;
end.

