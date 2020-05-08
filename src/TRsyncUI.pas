unit TRsyncUI;

{$mode objfpc}{$H+}


interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
    ExtCtrls, StdCtrls, Grids, LazFileUtils,
    LazLogger,  LCLType,
    TRcommon, TRtexts, SyncError;

type

    { TFormSync }

TFormSync = class(TForm)
    ButtonSave: TButton;
    ButtonCancel: TButton;
    ButtonClose: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter3: TSplitter;
    StringGridReport: TStringGrid;

    { Runs a sync without showing form. Ret False if error or its not setup.
      Caller must ensure that Sync is config and that the Sync dir is available.
      If clash, user will see dialog. }
    function RunSyncHidden() : boolean;

    procedure ButtonCancelClick(Sender: TObject);
    procedure ButtonCloseClick(Sender: TObject);
    procedure ButtonSaveClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormHide(Sender: TObject);

    { At Show, depending on SetUpSync, we'll either go ahead and do it, any
      error is fatal or, if True, walk user through process. }
    procedure FormShow(Sender: TObject);
    procedure StringGridReportGetCellHint(Sender: TObject; ACol, ARow: Integer;
                                  var HintText: String);
private
    FormShown : boolean;
    LocalTimer : TTimer;
    procedure AfterShown(Sender : TObject);

    // Display a summary of sync actions to user.
    function DisplaySync(): string;

    { Called when user wants to join a (possibly uninitialised) Repo,
      will handle some problems with user's help. }
    procedure DrySync();

    { Called to do a sync assuming its all setup. Any problem is fatal }
    function DoSync(): boolean;

    { Populates the string grid with details of notes to be actioned }
    procedure ShowReport;

public
    Busy : boolean; // indicates that there is some sort of sync in process now.
    procedure MarkNoteReadOnly(const Filename : string; const WasDeleted : Boolean = False);

    { we will pass address of this function to Sync }
    function DefineDefaultAction(const ClashRec : TClashRecord) : TSyncAction;
end;

var
   FormSync: TFormSync;

implementation

uses TRsearchUnit, TRsync,  TRclashUI;

{$R *.lfm}

var
        ASync : TSync;
{ TFormSync }

procedure TFormSync.MarkNoteReadOnly(const Filename : string; const WasDeleted : Boolean = False);
begin
    SearchForm.MarkNoteReadOnly(FileName, WasDeleted);
end;

function TFormSync.DefineDefaultAction(const ClashRec : TClashRecord) : TSyncAction;
var
    clash : TFormClash;
begin

    clash := TFormClash.Create(self);

    clash.NoteID.Caption := 'Note ID ; '+ ClashRec.LocalNote^.ID;
    clash.TitleLocal.Caption := ClashRec.LocalNote^.Title;
    clash.ChangeLocal.Caption := ClashRec.LocalNote^.LastChange;
    clash.TitleRemote.Caption := ClashRec.RemoteNote^.Title;
    clash.ChangeRemote.Caption := ClashRec.RemoteNote^.LastChange;

    clash.MemoLocal.ReadOnly := true;
    clash.MemoLocal.Text := clash.RemoveXml(ClashRec.LocalNote^.Content);
    clash.MemoRemote.ReadOnly := true;
    clash.MemoRemote.Text := clash.RemoveXml(ClashRec.RemoteNote^.Content);

    case clash.ShowModal of
            mrYes      : Result := SynDownLoad;
            mrNo       : Result := SynUpLoadEdit;
            mrNoToAll  : Result := SynAllLocal;
            mrYesToAll : Result := SynAllRemote;
            mrAll      : Result := SynAllNewest;
            mrRetry    : Result := SynAllCopy;
            mrClose    : Result := SynAllOldest;
    else
            Result := SynUnSet;   // Should not get there
    end;
    clash.Free;
    Application.ProcessMessages;
end;

procedure TFormSync.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    FreeandNil(ASync);
    Busy := False;
end;

procedure TFormSync.FormHide(Sender: TObject);
begin
    if LocalTimer = Nil then exit();
    LocalTimer.Free;
    LocalTimer := nil;
end;


// Following resourcestrings defined in syncUtils.pas

function TFormSync.DisplaySync(): string;
var
    UpNew, UpEdit, Down, DelLoc, DelRem, CreateCopy, DoNothing, Undecided : integer;
begin
    ASync.ReportMetaData(UpNew, UpEdit, Down, DelLoc, DelRem, CreateCopy, DoNothing, Undecided);
    Memo1.Append(rsNewUploads + inttostr(UpNew));
    Memo1.Append(rsEditUploads + inttostr(UpEdit));
    Memo1.Append(rsDownloads + inttostr(Down));
    Memo1.Append(rsLocalDeletes + inttostr(DelLoc));
    Memo1.Append(rsRemoteDeletes + inttostr(DelRem));
    Memo1.Append(rsSynCopies + inttostr(CreateCopy));
    Memo1.Append(rsDoNothing + inttostr(DoNothing));
    Memo1.Append(rsUndecided + inttostr(Undecided));
    result := 'Uploads=' + inttostr(UpNew+UpEdit) + ' downloads=' + inttostr(Down) + ' deletes=' + inttostr(DelLoc + DelRem);
end;

    // User is only allowed to press Cancel or Save when this is finished.
procedure TFormSync.DrySync();
var
    SyncAvail : TSyncAvailable;
begin
    debugln('DrySync');

    freeandnil(ASync);

    ASync := TSync.Create;
    Label1.Caption:= rsTestingRepo;
    Application.ProcessMessages;
    ASync.ClashFunction:= @DefineDefaultAction;
    Async.SetTransport(SyncType);
    SyncAvail := ASync.TestConnection();
    if SyncAvail <> SyncReady then begin
        showmessage(rsUnableToProceed + ' ' + ASync.ErrorString);
        ModalResult := mrCancel;
        exit;
    end;
    Label1.Caption:=rsLookingatNotes;
    Application.ProcessMessages;
    if ASync.StartSync(true) then begin
        DisplaySync();
        ShowReport();
        Label1.Caption:=rsLookingatNotes;
        Label2.Caption := rsSaveAndSync;
        ButtonSave.Enabled := True;
    end  else
        Showmessage(rsSyncError + ' ' + ASync.ErrorString);
    ButtonCancel.Enabled := True;
end;

procedure TFormSync.AfterShown(Sender : TObject);
begin
    LocalTimer.Enabled := False;             // Dont want to hear from you again
    if SyncFirstRun then begin
        DrySync();
    end else
        DoSync();
end;

//RESOURCESTRING
//  rsPleaseWait = 'Please wait a minute or two ...';

procedure TFormSync.FormShow(Sender: TObject);
begin
    Busy := True;
    Left := 55 + random(55);
    Top := 55 + random(55);
    FormShown := False;
    Label2.Caption := rsNextBitSlow;
    Memo1.Clear;
    StringGridReport.Clear;
    ButtonSave.Enabled := False;
    ButtonClose.Enabled := False;
    ButtonCancel.Enabled := False;
    // We call a timer to get out of OnShow so ProcessMessages works as expected
    LocalTimer := TTimer.Create(Nil);
    LocalTimer.OnTimer:= @AfterShown;
    LocalTimer.Interval:=500;
    LocalTimer.Enabled := True;
end;

function TFormSync.RunSyncHidden(): boolean;
begin
    //debugln('In RunSyncHidden');
    //if not Sett.getSyncTested() then exit(False);      // should never call this in setup mode but to be sure ...

    busy := true;
    StringGridReport.Clear;

    Result := DoSync();
end;

        // User is only allowed to press Close when this is finished.
function TFormSync.DoSync() : boolean;
var
    SyncState : TSyncAvailable = SyncNotYet;
begin
    debugln('DoSync');

    Label1.Caption := rsTestingSync;
    Application.ProcessMessages;

    ASync := TSync.Create;

    try
        ASync.ClashFunction:= @DefineDefaultAction;
        Async.SetTransport(SyncType);
        SyncState := ASync.TestConnection();
        while SyncState <> SyncReady do begin
            debugln('Failed testConnection');

            FormSyncError.Label1.caption := rsUnableToSync + ':';
            FormSyncError.label3.caption := ASync.ErrorString;
            FormSyncError.ButtRetry.Visible := not Visible;
            ModalResult := FormSyncError.ShowModal;
            if ModalResult = mrCancel then exit(false);

            SyncState := ASync.TestConnection();
        end;
        Label1.Caption:= rsRunningSync;
        Application.ProcessMessages;

        ASync.StartSync(false);
        SearchForm.UpdateSyncStatus(rsLastSync + ' ' + FormatDateTime('YYYY-MM-DD hh:mm', now)  + ' ' + DisplaySync());
        ShowReport();
        SearchForm.ProcessSyncUpdates(Async.DeletedList, Async.DownList);
        Label1.Caption:=rsAllDone;
        Label2.Caption := rsPressClose;
        ButtonClose.Enabled := True;
        Result := True;
    finally
        FreeandNil(ASync);
        Busy := False;
    end;
end;

procedure TFormSync.ShowReport;

var
    Rows,i : integer;
begin
    StringGridReport.Clean;
    i := 0;
    Rows :=0;
    while (i<Async.GridReportList.Count) do
    begin
        StringGridReport.InsertRowWithValues(
            Rows,
            [Async.GridReportList.Strings[i],
            Async.GridReportList.Strings[i+1],
            Async.GridReportList.Strings[i+2]]);
        inc(Rows);
        i := i+3;
    end;

    StringGridReport.AutoSizeColumn(0);
    StringGridReport.AutoSizeColumn(1);

    if  Rows = 0
    then Memo1.Append(rsNoNotesNeededSync)
    else Memo1.Append(inttostr(Rows) + rsNotesWereDealt);
end;

procedure TFormSync.StringGridReportGetCellHint(Sender: TObject; ACol,
  ARow: Integer; var HintText: String);
begin
// HintText := FileSync.ReportList.Items[ARow]^.Message;
end;

procedure TFormSync.ButtonCancelClick(Sender: TObject);
begin
    ModalResult := mrCancel;
end;

procedure TFormSync.ButtonCloseClick(Sender: TObject);
begin
	ModalResult := mrOK;
end;

procedure TFormSync.ButtonSaveClick(Sender: TObject);
begin
    Label2.Caption:=rsNextBitSlow;
    Label1.Caption:='First Time Sync';
    Memo1.Clear;
    Application.ProcessMessages;
    ButtonCancel.Enabled := False;
    ButtonSave.Enabled := False;
    //ASync.TestRun := False;
    if ASync.StartSync(false) then begin
        SearchForm.UpdateSyncStatus(rsLastSync + ' ' + FormatDateTime('YYYY-MM-DD hh:mm', now)  + ' ' + DisplaySync());
        ShowReport();
        SearchForm.ProcessSyncUpdates(Async.DeletedList, Async.DownList);
        Label1.Caption:=rsAllDone;
        Label2.Caption := rsPressClose;
	SyncFirstRun := false;
    end  else
        Showmessage(rsSyncError + ASync.ErrorString);
    ButtonClose.Enabled := True;
end;

end.

