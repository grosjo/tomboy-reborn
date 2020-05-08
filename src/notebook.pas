unit Notebook;


{$mode objfpc}{$H+}

interface

uses
		Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, CheckLst,
		ExtCtrls, StdCtrls, Buttons, ComCtrls, LazFileUtils, LCLProc,
                TRcommon;

type

		{ TNoteBookPick }

  TNoteBookPick = class(TForm)
				Button1: TButton;
				ButtonOK: TButton;
				CheckListBox1: TCheckListBox;
                EditNewNotebookName: TEdit;
				EditNewNotebook: TEdit;
				Label1: TLabel;
				Label2: TLabel;
				Label3: TLabel;
				Label4: TLabel;
				Label5: TLabel;
                Label6: TLabel;
                Label7: TLabel;
                Label8: TLabel;
                Label9: TLabel;
				PageControl1: TPageControl;
				Panel1: TPanel;
				TabExisting: TTabSheet;
				TabNewNoteBook: TTabSheet;
                TabChangeName: TTabSheet;
				procedure ButtonOKClick(Sender: TObject);
                procedure CheckListBox1ItemClick(Sender: TObject; Index: integer);
                procedure EditNewNotebookNameEditingDone(Sender: TObject);
                procedure FormShow(Sender: TObject);
		private
                                      { A list pointer that will point to list of notes that are
                                        members of the notebook who's name we are about to change }
                NBIDList : TStringList;
                                    { Actually do all the stuff necessary when we change a notebook name }
                function ChangeNoteBookName(NewName: string): boolean;
                function RewriteTempate(const FileName, NewName: string
                    ): boolean;
                function RewriteWithNewNotebookName(FileName: string): boolean;
                                    { User wants to change the name of a Notebook, Title is name of Notebook }
                procedure SetupForChange();
                                    { Allow user to select an existing Notebook or make a new one }
                procedure SetupForNewSelect();
		public
        	    FullFileName : ANSIString;
                Title : ANSIString;
                ChangeMode : boolean;   // Allow renaming of existing notebook.
		end;

{var
		NoteBookPick: TNoteBookPick;   }

implementation

{$R *.lfm}

{ TNoteBookPick }

uses TRSearchUnit, SaveNote, EditBox,
                SyncUtils, TRtexts;

procedure TNoteBookPick.SetupForNewSelect();
var
        SL : TStringList;
        Index, I : Integer;
begin
    Label1.Caption := Title;
    if ManyNotebooks then
        Label2.Caption := rsMultipleNoteBooks
    else Label2.Caption := rsOneNoteBook;
    PageControl1.ActivePage := TabExisting;
    Label3.Caption := rsSetTheNotebooks;
    SL := TStringList.Create;
    SearchForm.NoteLister.GetNotebooks(SL, '');
    CheckListBox1.Items.Assign(SL);
    SL.Free;
    SL := TStringList.Create;
    SearchForm.NoteLister.GetNotebooks(SL, ExtractFileNameOnly(FullFileName) + '.note');
    for I := 0 to CheckListBox1.Count-1 do
        CheckListBox1.Checked[I] := False;
    for Index := 0 to SL.Count -1 do
    	for I := 0 to CheckListBox1.Count-1 do
			if SL[Index] = CheckListBox1.Items[I] then
            	CheckListBox1.Checked[I] := True;
    SL.Free;
    TabChangeName.TabVisible := False;
end;


procedure TNoteBookPick.SetupForChange();
var
    NoteID : String;
begin
    //  Note : NBIDList does not need to be created or freed. Just a pointer.
    TabExisting.TabVisible:=False;
    TabNewNoteBook.TabVisible:=False;
    PageControl1.ActivePage := TabChangeName;
    Label3.Caption := rsChangeNameofNotebook;
    Label7.Caption := Title;
    if SearchForm.NoteLister.GetNotesInNoteBook(NBIDList, Title) then begin
        for NoteID in NBIDList do
            DebugLn('Notebook.pas #158 Note - '+ NoteID);
    end else
        debugln('Notebook.pas #160 No member notes found');
    Label1.Caption := format(rsNumbNotesAffected, [NBIDList.Count]);
    EditNewNotebookName.SetFocus;
end;

procedure TNoteBookPick.FormShow(Sender: TObject);
begin
    if ChangeMode then
        SetUpForChange()
    else SetupForNewSelect();
end;

procedure TNoteBookPick.CheckListBox1ItemClick(Sender: TObject; Index: integer);
var
	I : integer;
begin
    if ManyNotebooks then exit;
    // ensure only one clicked.
    if (Sender as TCheckListBox).Checked[Index] then begin
        for I := 0 to CheckListBox1.Count -1 do
            CheckListBox1.Checked[I] := False;
        CheckListBox1.Checked[Index] := True;
    end;
end;

procedure TNoteBookPick.EditNewNotebookNameEditingDone(Sender: TObject);
begin
    ButtonOK.Click;
end;

function TNoteBookPick.RewriteWithNewNotebookName(FileName : string) : boolean;
var
    InFile, OutFile: TextFile;
    {NoteDateSt, }InString, TempName, NextSeekString : string;
begin
  if not fileexists(NotesDir + FileName) then exit(false);     // if its not there, the note has just been deleted
  TempName := AppendPathDelim(NotesDir) + 'tmp';
  if not DirectoryExists(TempName) then
      CreateDir(AppendPathDelim(tempname));
  TempName := tempName + pathDelim + FileName;
  AssignFile(InFile, NotesDir + FileName);
  AssignFile(OutFile, TempName);
  try
      try
          Reset(InFile);
          Rewrite(OutFile);
          NextSeekString := '<last-change-date>';
          while not eof(InFile) do begin
              readln(InFile, InString);
              if (Pos(NextSeekString, InString) > 0) then begin
                    case NextSeekString of
                        '<last-change-date>' : begin
                                                    writeln(outFile, '  <last-change-date>'
                                                        + GetCurrentTimeStr() + '</last-change-date>');
                                                    NextSeekString := '<last-metadata-change-date>';
                                                end;
                        '<last-metadata-change-date>' : begin
                                                    writeln(outFile, '  <last-metadata-change-date>'
                                                        + GetCurrentTimeStr() + '</last-metadata-change-date>');
                                                    NextSeekString := '<y>';
                                                end;
                        '<y>' :     begin
                                        writeln(OutFile, InString);
                                        write(OutFile, SearchForm.NoteLister.NoteBookTags(Filename));
                                        NextSeekString := '<tags>';
                                    end;
                        '<tags>' :  begin
                                        readln(InFile, InString);                   // Danger, wot if we hit EOF ?
                                        while pos('<tag>', Instring) > 0 do
                                            readln(InFile, InString);               // now we have the </tags> line.
                                        NextSeekString := '321-blar-blar-blar-blar-123';
                                    end;
                    end;
              end else
                    writeln(OutFile, InString);
          end;                                  // end of while loop.
          //writeln(OutFile, '</note>');
      finally
          CloseFile(OutFile);
          CloseFile(InFile);
      end;
  except
    on E: EInOutError do begin
        debugln('File handling error occurred updating clean note location. Details: ' + E.Message);
        exit(False);
    end;
  end;
  {$ifdef WINDOWS}
  if not SafeWindowsDelete(Sett.NoteDirectory + FileName, NextSeekstring) then begin
      showmessage(NextSeekString);
      exit(false);
  end;
  {$endif}
  result := CopyFile(TempName, NotesDir + FileName);
end;

function TNoteBookPick.RewriteTempate(const FileName, NewName : string) : boolean;
var
    InFile, OutFile: TextFile;
    InString, TempName, NextSeekString : string;
begin
  if not fileexists(NotesDir + FileName) then exit(false);     // if its not there, the note has just been deleted
  TempName := AppendPathDelim(NotesDir) + 'tmp';
  if not DirectoryExists(TempName) then
      CreateDir(AppendPathDelim(tempname));
  TempName := tempName + pathDelim + FileName;
  AssignFile(InFile, NotesDir + FileName);
  AssignFile(OutFile, TempName);
  try
      try
          Reset(InFile);
          Rewrite(OutFile);
          NextSeekString := '<title>';
          while not eof(InFile) do begin
              readln(InFile, InString);
              if (Pos(NextSeekString, InString) > 0) then begin
                    case NextSeekString of
                        '<title>' : begin
                                        writeln(outFile, '<title>' + NewName + ' Template</title>');
                                        NextSeekString := '<note-content version=';
                                    end;
                        '<note-content version=' :
                                    begin
                                        writeln(outFile, '<text xml:space="preserve"><note-content version="0.1">'
                                                + NewName + ' Template');
                                        NextSeekString := '<last-change-date>';
                                    end;
                        '<last-change-date>' : begin
                                                    writeln(outFile, '  <last-change-date>'
                                                        + GetCurrentTimeStr() + '</last-change-date>');
                                                    NextSeekString := '<last-metadata-change-date>';
                                                end;
                        '<last-metadata-change-date>' : begin
                                                    writeln(outFile, '  <last-metadata-change-date>'
                                                        + GetCurrentTimeStr() + '</last-metadata-change-date>');
                                                    NextSeekString := '<y>';
                                                end;
                        '<y>' :     begin
                                        writeln(OutFile, InString);
                                        writeln(OutFile, '  <tags>');
                                        writeln(OutFile, '    <tag>system:template</tag>');
                                        writeln(OutFile, '    <tag>system:notebook:' + NewName + '</tag>');
                                        writeln(OutFile, '  </tags>');
                                        NextSeekString := '<tags>';
                                    end;
                        '<tags>' :  begin                                           // just drop on floor.
                                        readln(InFile, InString);                   // Danger, wot if we hit EOF ?
                                        while pos('<tag>', Instring) > 0 do
                                            readln(InFile, InString);               // now we have the </tags> line.
                                        NextSeekString := '321-blar-blar-blar-blar-123';  // wow, if we find that ???
                                    end;
                    end;
              end else
                    writeln(OutFile, InString);
          end;                                  // end of while loop.
      finally
          CloseFile(OutFile);
          CloseFile(InFile);
      end;
  except
    on E: EInOutError do begin
        debugln('File handling error occurred updating template. Details: ' + E.Message);
        exit(False);
    end;
  end;
  {$ifdef WINDOWS}
  if not SafeWindowsDelete(Sett.NoteDirectory + FileName, NextSeekstring) then begin
      showmessage(NextSeekString);
      exit(false);
  end;
  {$endif}
  result := CopyFile(TempName, NotesDir + FileName);
end;




function TNoteBookPick.ChangeNoteBookName(NewName : string) : boolean;
            { 1. We have a list of all the notes that are members of this notebook.
              2. Change the Notebook name stored in the Notebook data structure.
              3. For notes that are open, just force a write, reach in and mark dirty....
              4. For notes that are not open, we rewrite them, setting a new list of notebook tags
                 according to the data structure, a new last-change-date and a last-metadata-change-date.
              5. Rewrite Template, give it a new Title (which is new Notebook Name plus ' Template')
                 which needs to be written twice, updated last-change-date and last-metadata-change-date,
                 finally remove its one Notebook name and replace it with new notebook name.
              VERY IMPORTANT that end user has fully sync'ed before doing this. Else we
              might leave notes on remote machine that believe they belong to a missing notebook. }
var
    IDstr, TemplateID : string;
    OpenForm : TForm; //TEditBoxForm;
begin
    result := true;
    TemplateID := SearchForm.notelister.NotebookTemplateID(Title);
    if TemplateID = '' then begin
        showmessage('Failed to ID Template [' + Title + ']');
        exit(false);
    end;
    SearchForm.NoteLister.AlterNotebook(Title, NewName);
    for IDstr in NBIDList do begin
        if SearchForm.NoteLister.IsThisNoteOpen(IDStr, OpenForm) then
            TEditBoxForm(OpenForm).Dirty:= true
        else RewriteWithNewNotebookName(IDstr);
    end;
    // OK, now change template ......
    // debugln('template is ' + SearchForm.notelister.NotebookTemplateID(Title));
    RewriteTempate(TemplateID, RemoveBadXMLCharacters(NewName));
end;


procedure TNoteBookPick.ButtonOKClick(Sender: TObject);
var
        SL : TStringList;
        Index : Integer;
        Saver : TSaveNote;
begin
    if PageControl1.ActivePage = TabExisting then begin
        SL := TStringList.Create;
        try
            for Index := 0 to CheckListBox1.Count -1 do
        	if CheckListBox1.Checked[Index] then SL.Add(CheckListBox1.Items[Index]);
    		SearchForm.NoteLister.SetNotebookMembership(ExtractFileNameOnly(FullFileName) + '.note', SL);
        finally
            Sl.Free;
        end;
    end;
    if PageControl1.ActivePage = TabNewNotebook then
        if EditNewNotebook.Text <> '' then begin
            Saver := TSaveNote.Create();
            try
                Saver.SaveNewTemplate(EditNewNotebook.Text);
                // OK, now add current note to the new Notebook
                SearchForm.NoteLister.AddNoteBook(ExtractFileNameOnly(FullFileName) + '.note', EditNewNotebook.Text, False);
                SearchForm.RefreshNotebooks();
			finally
                Saver.Destroy;
			end;
        end else begin
            showmessage(rsEnterNewNotebook);
            exit;
        end;
    if PageControl1.ActivePage = TabChangeName then
            if EditNewNotebookName.Text <> '' then
                if not ChangeNoteBookName(EditNewNotebookName.Text) then exit;
	ModalResult := mrOK;
end;

end.

