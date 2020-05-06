program Tomboy_Reborn;

{$mode objfpc}{$H+}

uses
    {$DEFINE UseCThreads}
    {$IFDEF UNIX}{$IFDEF UseCThreads}
    cthreads,
    {$ENDIF}{$ENDIF}
    Interfaces, 
    Forms, printer4lazarus, TRcommon, SearchUnit, TRsettings, TrSyncGUI,
    Notebook, Spelling, Mainunit, recover, markdown, Index, autostart,
    hunspell, sync, syncutils, helpnotes, ResourceStr, SyncError,
    colours, ncsetup;

{$R *.res}

var
	ConfigDir : String;
	NotesDir : String;
	ConfigFile : String;


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
    {$ifndef DARWIN}
    MainForm.SetAltHelpPath(Result);
    {$endif}
end;

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
    Application.CreateForm(TFormHelpNotes, FormHelpNotes);
    Application.CreateForm(TFormSyncError, FormSyncError);
    Application.CreateForm(TFormColours, FormColours);
    // Application.CreateForm(TNoteBookPick, NoteBookPick);
    // Application.CreateForm(TFormSpell, FormSpell);
    // Application.CreateForm(TEditBoxForm, EditBoxForm);
    Application.Run;
end.

