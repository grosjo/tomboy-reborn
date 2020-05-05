program Tomboy_Reborn;

{ History
	27/12/2017 - Altered order to make the settings form the main one instead of RTSearch
}

{$mode objfpc}{$H+}

uses
    {$DEFINE UseCThreads}
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, 
    Forms, printer4lazarus, SearchUnit, settings, SyncGUI, Notebook, Spelling,
    Mainunit, BackupView, recover, markdown, Index, autostart,
    hunspell, sync, syncutils, helpnotes, ResourceStr, SyncError,
    colours, ncsetup;

{$R *.res}

begin
    Application.Scaled:=True;
    Application.Title:='tomboy-reborn';
    RequireDerivedFormResource:=True;
    Application.Initialize;
    Application.CreateForm(TMainForm, MainForm);
    Application.CreateForm(TFormNCSetup, FormNCSetup);
    Application.CreateForm(TSett, Sett);
    Application.CreateForm(TSearchForm, SearchForm);
    Application.CreateForm(TFormSync, FormSync);
    Application.CreateForm(TFormMarkdown, FormMarkdown);
    Application.CreateForm(TFormHelpNotes, FormHelpNotes);
    Application.CreateForm(TFormSyncError, FormSyncError);
    Application.CreateForm(TFormColours, FormColours);
    // Application.CreateForm(TNoteBookPick, NoteBookPick);
    // Application.CreateForm(TFormSpell, FormSpell);
    // Application.CreateForm(TEditBoxForm, EditBoxForm);
    Application.Run;
end.

