unit TRsyncClashUI;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, ComCtrls, Buttons;

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
     Label2: TLabel;
     Label3: TLabel;
     LabelSyncError: TLabel;
     MemoRemote: TMemo;
     MemoLocal: TMemo;
     TitleLocal: TLabel;
     ChangeLocal: TLabel;
     TitleRemote: TLabel;
     ChangeRemote: TLabel;
     Label1: TLabel;
     NoteID: TLabel;
end;


implementation

{$R *.lfm}

end.

