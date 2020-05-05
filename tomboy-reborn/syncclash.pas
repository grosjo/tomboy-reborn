unit syncclash;
{ History
    2020/05 Reshape Tomboy-ng
}

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, ComCtrls, Buttons, kmemo;
type

    { TFormClash }

    TFormClash = class(TForm)
        BitBtnUseRemote: TBitBtn;
        BitBtnUseLocal: TBitBtn;
        ButtAllOldest: TButton;
        ButtAllNewest: TButton;
        ButtAllLocal: TButton;
        ButtAllRemote: TButton;
        Button1: TButton;
        MemoRemote: TMemo;
        MemoLocal: TMemo;
        TitleLocal: TLabel;
        ChangeLocal: TLabel;
        TitleRemote: TLabel;
        ChangeRemote: TLabel;
        Label1: TLabel;
        NoteID: TLabel;
        function RemoveXml(const St: AnsiString): AnsiString;

    end;

implementation

{$R *.lfm}

uses LazLogger, laz2_DOM, laz2_XMLRead, LazFileUtils, DateUtils{, syncutils};

{ TFormClash }

function TFormClash.RemoveXml(const St : AnsiString) : AnsiString;
var
    X, Y : integer;
    FoundOne : boolean = false;
begin
    Result := St;
    repeat
        FoundOne := False;
        X := Pos('<', Result);      // dont use UTF8Pos for byte operations
        if X > 0 then begin
            Y := Pos('>', Result);
            if Y > 0 then begin
                Delete(Result, X, Y-X+1);
                FoundOne := True;
            end;
        end;
    until not FoundOne;
    Result := trim(Result);
end;


end.

