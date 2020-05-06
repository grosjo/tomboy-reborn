unit TRcommon;

interface

const
    Backup = 'Backup';

var
    ConfigDir : String;
    NotesDir : String;
    ConfigFile : String;

function GetDefaultConfigDir() : string;

implementation

uses
    Classes, Forms, SysUtils, StdCtrls, LazFileUtils, LazLogger;

{ ===== ENVIRONMENT ==== }

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
end;

end.

