unit TRabout;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  RichMemo, ExtendedNotebook, TRcommon;


type

{ TFormAbout }

 TFormAbout = class(TForm)
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    procedure FormShow(Sender: TObject);

 private

  public

  end;

implementation

{$R *.lfm}

procedure TFormAbout.FormShow(Sender: TObject);
begin
    Left := mainWindow.Left + random(50);
    Top := mainWindow.Top + random(50);
end;

end.

