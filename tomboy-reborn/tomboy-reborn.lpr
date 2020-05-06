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
    hunspell, syncutils, ResourceStr,
    ncsetup;

{$R *.res}

begin
    Application.Scaled:=True;
    Application.Title:='Tomboy Reborn';
    RequireDerivedFormResource:=True;
    Application.Initialize;

    ConfigDir := GetDefaultConfigDir();
    ConfigFile := ConfigDir + 'tomboy-reborn.cfg';

    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TFormNCSetup, FormNCSetup);
    Application.CreateForm(TSett, Sett);
    Application.CreateForm(TSearchForm, SearchForm);
    Application.CreateForm(TFormSync, FormSync);
    Application.CreateForm(TFormMarkdown, FormMarkdown);
    //Application.CreateForm(TFormHelpNotes, FormHelpNotes);
    Application.CreateForm(TFormSyncError, FormSyncError);
    Application.CreateForm(TFormColours, FormColours);
    // Application.CreateForm(TNoteBookPick, NoteBookPick);
    // Application.CreateForm(TFormSpell, FormSpell);
    // Application.CreateForm(TEditBoxForm, EditBoxForm);
    Application.Run;
end.

