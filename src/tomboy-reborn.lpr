program Tomboy_Reborn;

{$mode objfpc}{$H+}

uses
    {$DEFINE UseCThreads}
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, Classes, Forms, SysUtils, StdCtrls, LazFileUtils, LazLogger,
    Spelling, Colours,
    TRcommon, TRsearchUnit, TRsettings, TRsyncUI,
    Mainunit, markdown, Index, SyncError,
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

    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TFormNCSetup, FormNCSetup);
    Application.CreateForm(TSett, Sett);
    Application.CreateForm(TSearchForm, SearchForm);
    Application.CreateForm(TFormSync, FormSync);
    Application.CreateForm(TFormMarkdown, FormMarkdown);
    Application.CreateForm(TFormSyncError, FormSyncError);
    Application.CreateForm(TFormColours, FormColours);
    Application.Run;
end.

