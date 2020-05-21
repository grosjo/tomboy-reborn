unit TRnote;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, { FileUtil,} Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Menus, StdCtrls, Buttons, kmemo, LazLogger, PrintersDlgs,
    clipbrd, lcltype,      // required up here for copy on selection stuff.
    fpexprpars,         // for calc stuff ;
    TRcommon,
    SaveNote;      		// TO BE REMOVED


type TFontRange = (FontHuge, FontLarge, FontNormal, FontSmall, FontTitle);

type TTagType = ( TagNone, TagBold, TagItalic, TagHighLight, TagUnderline, TagStrikeout, TagMonospace, TagSizeSmall, TagSizeLarge, TagSizeHuge, TagList);

type

{ TFormNote }

 TFormNote = class(TForm)
    KMemo1: TKMemo;
    MenuBold: TMenuItem;
    MenuItalic: TMenuItem;
    MenuHighLight: TMenuItem;
    MenuHuge: TMenuItem;
    MenuBullet: TMenuItem;
    MenuItem1: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItemSettings: TMenuItem;
    MenuItemEvaluate: TMenuItem;
    MenuItemIndex: TMenuItem;
    MenuItemExportMarkdown: TMenuItem;
    MenuItemSpell: TMenuItem;
    MenuItemExportRTF: TMenuItem;
    MenuItemExportPlainText: TMenuItem;
    MenuItemPrint: TMenuItem;
    MenuItemSelectAll: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItemDelete: TMenuItem;
    MenuItemPaste: TMenuItem;
    MenuItemCopy: TMenuItem;
    MenuItemFind: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItemCut: TMenuItem;
    MenuItemSync: TMenuItem;
    MenuItemExport: TMenuItem;
    MenuSmall: TMenuItem;
    MenuItem2: TMenuItem;
    MenuNormal: TMenuItem;
    MenuLarge: TMenuItem;
    MenuFixedWidth: TMenuItem;
    MenuUnderline: TMenuItem;
    MenuStrikeout: TMenuItem;
    Panel1: TPanel;
    PopupMenuRightClick: TPopupMenu;
    PopupMenuTools: TPopupMenu;
    PopupMenuText: TPopupMenu;
    PrintDialog1: TPrintDialog;
    SpeedButtonDelete: TSpeedButton;
    SpeedButtonLink: TSpeedButton;
    SpeedButtonNotebook: TSpeedButton;
    SpeedButtonSearch: TSpeedButton;
    SpeedButtonText: TSpeedButton;
    SpeedButtonTools: TSpeedButton;

    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    { gets called under a number of conditions, easy one is just a re-show,
              or for a new note or a new note with a title from Link button
              or for an existing note where we get note file name
              or a new note from template where we have a note filename but IsTemplate
              also set, here we discard file name and make a new one. }
    procedure FormShow(Sender: TObject);
    procedure KMemo1Change(Sender: TObject);

    { Watchs for  backspace affecting a bullet point, and whole lot of ctrl, shift, alt
              combinations. For things we let KMemo handle, just exit, for things we handle
              must set key to 0 after doing so. }
    procedure KMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure KMemo1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure KMemo1MouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure MenuBoldClick(Sender: TObject);
    procedure MenuBulletClick(Sender: TObject);
    procedure MenuFixedWidthClick(Sender: TObject);
    procedure MenuHighLightClick(Sender: TObject);
    procedure MenuHugeClick(Sender: TObject);
    procedure MenuItalicClick(Sender: TObject);
    procedure MenuItemEvaluateClick(Sender: TObject);
    procedure MenuItemExportMarkdownClick(Sender: TObject);
    procedure MenuItemSettingsClick(Sender: TObject);
    procedure MenuUnderlineClick(Sender: TObject);
    procedure MenuStrikeoutClick(Sender: TObject);
    procedure MenuItemCopyClick(Sender: TObject);
    procedure MenuItemCutClick(Sender: TObject);
    procedure MenuItemDeleteClick(Sender: TObject);
    procedure MenuItemExportPlainTextClick(Sender: TObject);
    procedure MenuItemExportRTFClick(Sender: TObject);
    procedure MenuItemPasteClick(Sender: TObject);
    procedure MenuItemPrintClick(Sender: TObject);
    procedure MenuItemSelectAllClick(Sender: TObject);
    procedure MenuItemSpellClick(Sender: TObject);
    procedure MenuItemSyncClick(Sender: TObject);
    procedure MenuLargeClick(Sender: TObject);
    procedure MenuNormalClick(Sender: TObject);
    procedure MenuSmallClick(Sender: TObject);
    procedure SpeedButtonDeleteClick(Sender: TObject);
    procedure SpeedButtonLinkClick(Sender: TObject);
    procedure SpeedButtonNotebookClick(Sender: TObject);
    procedure SpeedButtonSearchClick(Sender: TObject);
    procedure SpeedButtonTextClick(Sender: TObject);
    procedure SpeedButtonToolsClick(Sender: TObject);
    procedure TimerSaveTimer(Sender: TObject);
    procedure TimerHousekeepingTimer(Sender: TObject);


private
    Processing : boolean;
    InitialLoaded : boolean;


    { To save us checking the title if user is well beyond it }
    BlocksInTitle : integer;
    LastFind : integer;

    // Set True by the delete button so we don't try and save it.
    DeletingThisNote : boolean;

    procedure NoteToMemo();
    procedure MemoToNote();
    procedure MarkDirty();
    procedure Commit();

    { Take a piece of text into KMemo block recursively }
    procedure TextToMemo(s : String; Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet : boolean; FontSize : TFontRange);

    function ReplaceAngles(const Str : String) : String;

    { Makes sure the first (and only the first) line is marked as Title
          Title should be Blue, Underlined and FontTitle big.
          Note that when a new note is loaded from disk, this function is not called,
          the Load unit knows how to do it itself. Saves 200ms with a big (20K) note. }
    procedure MarkTitle();


        { Alters the Font of Block as indicated }
        procedure AlterBlockFont(const FirstBlockNo, BlockNo: longint;
				const Command: integer; const NewFontSize: integer=0);
        { Alters the font etc of selected area as indicated }
        procedure AlterFont(const Command : integer; const NewFontSize: integer = 0);
        { If Toggle is true, sets bullets to what its currently no. Otherwise sets to TurnOn}
        procedure BulletControl(const Toggle, TurnOn: boolean);
        { Looks between StartS and EndS, marking any http link. Byte, not char indexes.
          A weblink has leading and trailing whitespace, starts with http:// or https://
          and has a dot and char after the dot. We expect kmemo1 is locked at this stage.}
        procedure CheckForHTTP(const PText: pchar; const StartS, EndS: longint);
        procedure CleanUTF8();
        function ColumnCalculate(out AStr: string): boolean;
        function ComplexCalculate(out AStr: string): boolean;
        procedure ExprTan(var Result: TFPExpressionResult;
            const Args: TExprParameterArray);
        function FindIt(Term: string; GoForward, CaseSensitive: boolean
            ): boolean;
        function FindNumbersInString(const AStr: string; out AtStart, AtEnd: string
            ): boolean;
        procedure InsertDate();
        function ParagraphTextTrunc(): string;
        function RelativePos(const Term: ANSIString; const MText: PChar;
            StartAt: integer): integer;
        function PreviousParagraphText(const Backby: integer): string;
        function SimpleCalculate(out AStr: string): boolean;
        // procedure CancelBullet(const BlockNo: longint; const UnderBullet: boolean);

		procedure ClearLinks(const StartScan : longint =0; EndScan : longint = 0);
        { Looks around current block looking for link blocks. If invalid, 'unlinks' them.
          Http or local links, we need to clear the colour and underline of any text nearby
          that have been 'smeared' from user editing at the end of a link. When this happens,
          new text appears within the link block, bad .....  }
        procedure ClearNearLink(const StartS, EndS: integer);
        function DoCalculate(CalcStr: string): string;
        procedure DoHousekeeping();

        procedure CheckForLinks(const StartScan : longint = 1; EndScan : longint = 0);

        { Returns with the title, that is the first line of note, returns False if title is empty }
        function GetTitle(out TheTitle: ANSIString): boolean;

        procedure InitiateCalc();

        { Searches for all occurances of Term in the KMemo text, makes them Links
          Does not bother with single char terms. Expects KMemo1 to be already locked.}
        procedure MakeAllLinks(const PText: PChar; const Term: ANSIString;
            const StartScan: longint=1; EndScan: longint=0);

        { Makes a link at passed position as long as it does not span beyond a block.
            And if it does span beyond one block, I let that go through to the keeper.
            Making a Hyperlink, deleting the origional text is a very slow process so we
            make heroic efforts to avoid having to do so. Index is char count, not byte.
            Its a SelectionIndex.  Note we no longer need pass this p the Link, remove ? }
		procedure MakeLink(const Index, Len: longint);

        { Returns true if current cursor is 'near' a bullet item. That could be because we are
  		on a Para Marker thats a Bullet and/or either Leading or Trailing Para is a Bullet.
  		We return with IsFirstChar true if we are on the first visible char of a line (not
  		necessarily a bullet line). If we return FALSE, passed parameters may not be set. }
		function NearABulletPoint(out Leading, Under, Trailing, IsFirstChar, NoBulletPara: Boolean;
            	out BlockNo, TrailOffset, LeadOffset: longint): boolean;
        { Responds when user clicks on a hyperlink }
		procedure OnUserClickLink(sender: TObject);
        // A method called by this or other apps to get what we might have selected
        procedure PrimaryCopy(const RequestedFormatID: TClipboardFormat;
            Data: TStream);
        // Pastes into KMemo whatever is returned by the PrimarySelection system.
        procedure PrimaryPaste(SelIndex: integer);

        function CleanCaption() : ANSIString;
        procedure SetBullet(PB: TKMemoParagraph; Bullet: boolean);
        // Advises other apps we can do middle button paste
        procedure SetPrimarySelection;
        // Cancels any indication we can do middle button paste cos nothing is selected
        procedure UnsetPrimarySelection;
        function UpdateNote(NRec: TNoteUpdaterec): boolean;
    public
        note : PNoteInfo;

    private
        AlreadyLoaded : boolean;
        Dirty : boolean;

        SearchedTerm : string;  // If not empty, opening is associated with a search, go straight there.
    end;

type PNoteEditForm = ^TFormNote;

implementation

{$R *.lfm}

{ TFormNote }
uses LazUTF8,
    keditcommon,        // Holds some editing defines
    LazFileUtils,		// For ExtractFileName()
    math,
    FileUtil, strutils,         // just for ExtractSimplePath ... ~#1620
    LCLIntf,

    Spelling,
    K_Prn,              // Custom print unit.
    Markdown;


const
        LinkScanRange = 100;	// when the user changes a Note, we search +/- around
     							// this value for any links that need adjusting.

{  ---- U S E R   C L I C K   F U N C T I O N S ----- }



procedure TFormNote.SpeedButtonTextClick(Sender: TObject);
begin
   PopupMenuText.PopUp;
end;

procedure TFormNote.SpeedButtonToolsClick(Sender: TObject);
begin
   PopupMenuTools.PopUp;
end;

procedure TFormNote.SpeedButtonSearchClick(Sender: TObject);
begin
    //SearchForm.Show;
end;

procedure TFormNote.SpeedButtonDeleteClick(Sender: TObject);
var
    St : string;
begin
    if KMemo1.ReadOnly then exit();
    St := CleanCaption();
   if IDYES = Application.MessageBox('Delete this Note', PChar(St), MB_ICONQUESTION + MB_YESNO) then begin
        //if SingleNoteMode then
        //    DeleteFileUTF8(NoteFileName)
   	//	else if NoteFileName <> '' then
	   		    //SearchForm.DeleteNote(NoteFileName);
        Dirty := False;
        //DeletingThisNote := True;
		Close;
   end;
end;

procedure TFormNote.SpeedButtonLinkClick(Sender: TObject);
var
    ThisTitle : ANSIString;
    Index : integer;
    SL : TStringList;
begin
   if KMemo1.ReadOnly then exit();
	if KMemo1.Blocks.RealSelLength > 1 then begin
         ThisTitle := KMemo1.SelText;
        // Titles must not start or end with space or contain low characters
        while ThisTitle[1] = ' ' do UTF8Delete(ThisTitle, 1, 1);
        while ThisTitle[UTF8Length(ThisTitle)] = ' ' do UTF8Delete(ThisTitle, UTF8Length(ThisTitle), 1);
        Index := Length(ThisTitle);
        While Index > 0 do begin
            if ThisTitle[Index] < ' ' then delete(ThisTitle, Index, 1);
            dec(Index);
		end;
		// showmessage('[' + KMemo1.SelText +']' + LineEnding + '[' + ThisTitle + ']' );
        if UTF8Length(ThisTitle) > 1 then begin
            SL := TStringList.Create;
            //SearchForm.NoteLister.GetNotebooks(SL, ExtractFileNameOnly(NoteFileName));      // that should be just ID
            //if SL.Count > 0 then
            //    SearchForm.OpenNote(ThisTitle, '', SL.Strings[0])
        //	else SearchForm.OpenNote(ThisTitle);
            KMemo1Change(self);
            SL.Free;
		end;
	end;
end;
          {
function TFormNote.GetSize() : integer;
begin
    case range of
        FontHuge : Result := round(18*FontScale/100.0);
        FontBig  : Result := round(14*FontScale/100.0);
        FontMedium : Result := round(11*FontScale/100.0);
        FontSmall : Result := round(8*FontScale/100.0);
        FontTitle : Result := round(16*FontScale/100.0);
    end;
end;
         }
function TFormNote.ReplaceAngles(const Str : String) : String;
var
    s : String;
begin
   s := StringReplace(Str,'&lt;','<',[rfReplaceAll]);
   s := StringReplace(s,'&gt;','>',[rfReplaceAll]);
   Result := StringReplace(s,'&amp;','&',[rfReplaceAll]);
end;


procedure TFormNote.TextToMemo(s : String; Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet : boolean; FontSize : TFontRange);
var
    i,j,k : integer;
    Ktext,tagtext,sub : String;
    tagtype : TTagType;
    ch : Char;
    par : TKMemoParagraph;
    ktb : TKMemoTextBlock;
    f : TFont;
begin
   i:=0; j:= length(s);
   Ktext := '';

   TRlog('TFormNote.TextToMemo (j='+IntToStr(j)+')');
   TRlog('SUB "'+s+'"');

   par := KMemo1.Blocks.AddParagraph;
   if InBullet then begin
      par.Numbering := pnuBullets;
      par.NumberingListLevel.FirstIndent := -20;    // Note, these numbers need match SettBullet() in editbox
      par.NumberingListLevel.LeftIndent := 30;
   end;

   while (i<j) do
   begin
      ch := s.Chars[i];
      if Ch = #13 then begin inc(i); continue; end; // Micro$ bug
      if Ch = #9 then Ch := ' ';    // dont care tabs

      tagtext :='';
      tagtype := TTagType.TagNone;

      if (Ch = '<') then  // new tag
      begin
//        type TTagType = ( TagBold, TagItalic, TagHighLight, TagUnderline, TagStrikeout, TagMonospace, TagSizeSmall, TagSizeLarge, TagSizeHuge, TagList);

         if(CompareStr(LowerCase(Copy(s,i,4)),'bold')=0)                    then begin tagtext := 'bold'; tagtype := TTagType.TagBold; end
          else if(CompareStr(LowerCase(Copy(s,i,6)),'italic')=0)            then begin tagtext := 'italic'; tagtype := TTagType.TagItalic; end
          else if(CompareStr(LowerCase(Copy(s,i,9)),'highlight')=0)         then begin tagtext := 'highLight'; tagtype := TTagType.TagHighLight; end
          else if(CompareStr(LowerCase(Copy(s,i,9)),'underline')=0)         then begin tagtext := 'underline'; tagtype := TTagType.TagUnderline; end
          else if(CompareStr(LowerCase(Copy(s,i,9)),'strikeout')=0)         then begin tagtext := 'strikeout'; tagtype := TTagType.TagStrikeout; end
          else if(CompareStr(LowerCase(Copy(s,i,9)),'monospace')=0)         then begin tagtext := 'monospace'; tagtype := TTagType.TagMonospace; end
          else if(CompareStr(LowerCase(Copy(s,i,10)),'size:small')=0)       then begin tagtext := 'size:small'; tagtype := TTagType.TagSizeSmall; end
          else if(CompareStr(LowerCase(Copy(s,i,10)),'size:large')=0)       then begin tagtext := 'size:large'; tagtype := TTagType.TagSizeLarge; end
          else if(CompareStr(LowerCase(Copy(s,i,10)),'size:huge')=0)        then begin tagtext := 'size:huge'; tagtype := TTagType.TagSizeHuge; end
          else if(CompareStr(LowerCase(Copy(s,i,10)),'list-item')=0)        then begin tagtext := 'list-item'; tagtype := TTagType.TagList; end;

          while( (i<j) and (s.Chars[i] <> '>')) do inc(i); // end of opening tag
          sub := LowerCase(Copy(s,i));
          k:= Pos('</'+tagtext,sub);
          if(k<1) then k:=length(sub);
          sub:=Copy(s,i,k-1);
          i:= i+k;
          while( (i<j) and (s.Chars[i] <> '>')) do inc(i); // end of closing tag

          case tagtype of
              TTagType.TagBold         : TextToMemo(sub, true, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, FontSize);
              TTagType.TagItalic       : TextToMemo(sub, Bold, true,   HighLight, Underline, Strikeout, FixedWidth, InBullet, FontSize);
              TTagType.TagHighlight    : TextToMemo(sub, Bold, Italic, true,      Underline, Strikeout, FixedWidth, InBullet, FontSize);
              TTagType.TagUnderline    : TextToMemo(sub, Bold, Italic, HighLight, true,      Strikeout, FixedWidth, InBullet, FontSize);
              TTagType.TagStrikeout    : TextToMemo(sub, Bold, Italic, HighLight, Underline, true,      FixedWidth, InBullet, FontSize);
              TTagType.TagMonospace    : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, true,       InBullet, FontSize);
              TTagType.TagSizeSmall    : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, TFontRange.FontSmall );
              TTagType.TagSizeLarge    : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, TFontRange.FontLarge );
              TTagType.TagSizeHuge     : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, TFontRange.FontHuge );
              TTagType.TagList         : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, true,     FontSize);
              else KText := KText + sub;
          end;
      end;

      if(Ch >= ' ') then Ktext := Ktext + ch;

      if((length(Ktext)>0) and ((Ch < ' ') or (tagtype <> TagNone) or (i>j-1))) then
      begin
         ktb := KMemo1.Blocks.AddTextBlock(ReplaceAngles(Ktext));  // We have to scan InStr for &lt; and &gt;  being < and >

         f := TFont.Create();
         f.Style := [];
         if Bold then f.Style := f.Style + [fsBold];
         if Italic then f.Style := f.Style + [fsItalic];
         if Underline then f.Style := f.Style + [fsUnderline];
         if Strikeout then f.Style := f.Style + [fsStrikeout];
         if FixedWidth then f.Name := FixedFont else f.Name := UsualFont;
         if FixedWidth then f.Pitch := fpFixed;
         f.Color := TextColour;

         ktb.TextStyle.Font := f;
         if HighLight then ktb.TextStyle.Brush.Color := HiColour;

         f.Free;
         Ktext := '';
      end;

      inc(i);

      if (Ch<' ') then // add Paragraph
      begin
         TRlog('New par i='+IntToStr(i)+' j='+IntToStr(j)+' s="'+s+'"');
         TextToMemo(Copy(s,i+1), Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, FontSize);
         i:=j;
      end;
   end;
end;

procedure TFormNote.NoteToMemo();
begin

   Trlog('NoteToMemo');

   Processing := True;

   // DISPLAY

   // First of all, deal with zero or neg settings
   if note^.Y < 20 then note^.Y := 20;
   if note^.X < 20 then note^.X := 20;
   if note^.Width < 50 then note^.Width := 50;
   if note^.Height < 50 then note^.Height := 50;

   //Make sure Note is within screen
   if (note^.X + (note^.Width div 3)) > Screen.Width then
        note^.X := Screen.Width - (Width div 3);

   if (note^.Y + (note^.Height div 3)) > Screen.Height then
        note^.Y := Screen.Height - (Height div 3);

   Left := note^.X;
   Top := note^.Y;
   Height := note^.Height;
   Width := note^.Width;

   // DEFAULT TITLE

   Caption := note^.Title;


   // KMEMO
   TRlog('Dealing with content');
   KMemo1.Blocks.LockUpdate;
   TRlog('Dealing with content2');
   KMemo1.Clear;
   TRlog('Dealing with content3');

   TextToMemo(note^.Content, false, false, false, false, false, false, false, TFontRange.FontNormal);

   TRlog('Dealing with content end');

   KMemo1.Blocks.UnlockUpdate;

   TRlog('Dealing with links');

   if (ShowIntLinks or ShowExtLinks) then CheckForLinks();

   TRlog('Done !');

   Processing := false;
end;


procedure TFormNote.MemoToNote();
begin

end;

procedure TFormNote.Commit();
begin

end;

procedure TFormNote.SpeedButtonNotebookClick(Sender: TObject);
//var
    //NotebookPick : TNotebookPick;
begin
    {
    NotebookPick := TNotebookPick.Create(Application);
    NotebookPick.FullFileName := NoteFileName;
    NotebookPick.Title := NoteTitle;
    NotebookPick.ChangeMode := False;
    NotebookPick.Top := Top;
    NotebookPick.Left := Left;
    if mrOK = NotebookPick.ShowModal then MarkDirty();
    NotebookPick.Free;
    }
end;

procedure TFormNote.BulletControl(const Toggle, TurnOn : boolean);
var
      BlockNo : longint = 1;
      LastBlock,  Blar : longint;
      BulletOn : boolean;
      FirstPass : boolean = True;
begin
    if not Toggle then begin    // We'll set it all to TurnOn
        FirstPass := False;     // So its not changed
        BulletOn := not TurnOn;
    end;
    if KMemo1.ReadOnly then exit();
    MarkDirty();
    BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, Blar);
    LastBlock := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, Blar);

    if (BlockNo = LastBlock) and (BlockNo > 1) and
        KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
        dec(LastBlock);
        dec(BlockNo);
    end;

    // Don't change any trailing empty lines.
    while KMemo1.Blocks.Items[LastBlock].ClassNameIs('TKMemoParagraph') do
        if LastBlock > BlockNo then dec(LastBlock)
        else break;

    // OK, we are now in a TextBlock, possibly both start and end there. Must mark
    // next para as numb and then all subsquent ones until we do the one after end.
    repeat
        inc(BlockNo);
        if BlockNo >= Kmemo1.Blocks.count then	// no para after block (yet)
            Kmemo1.Blocks.AddParagraph();
        if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
            if FirstPass then begin
                FirstPass := False;
                BulletOn := (TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering = pnuBullets);
            end;
            SetBullet(TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]), not BulletOn);
            // TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering := pnuBullets;
        end;
    until (BlockNo > LastBlock) and KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');
end;

procedure TFormNote.MenuBulletClick(Sender: TObject);
{var
      BlockNo : longint = 1;
      LastBlock,  Blar : longint;
      BulletOn : boolean;
      FirstPass : boolean = True;       }
begin
    BulletControl(True, False);
    exit();

{    if KMemo1.ReadOnly then exit();
    MarkDirty();
    BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, Blar);
    LastBlock := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, Blar);

    if (BlockNo = LastBlock) and (BlockNo > 1) and
        KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
        dec(LastBlock);
        dec(BlockNo);
    end;
    // Don't change any trailing empty lines.
    while KMemo1.Blocks.Items[LastBlock].ClassNameIs('TKMemoParagraph') do
        if LastBlock > BlockNo then dec(LastBlock)
        else break;

    // OK, we are now in a TextBlock, possibly both start and end there. Must mark
    // next para as numb and then all subsquent ones until we do the one after end.
    repeat
        inc(BlockNo);
        if BlockNo >= Kmemo1.Blocks.count then	// no para after block (yet)
            Kmemo1.Blocks.AddParagraph();
        if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
            if FirstPass then begin
                FirstPass := False;
                BulletOn := (TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering = pnuBullets);
            end;
            SetBullet(TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]), not BulletOn);
            // TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering := pnuBullets;
        end;
    until (BlockNo > LastBlock) and KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');   }
end;

procedure TFormNote.KMemo1MouseDown(Sender: TObject; Button: TMouseButton;
		Shift: TShiftState; X, Y: Integer);
begin
    //{$ifdef LCLCOCOA}
    if ssCtrl in Shift then PopupMenuRightClick.popup;
    //{$else}
	if Button = mbRight then PopupMenuRightClick.PopUp;
    //{$endif}
end;


// ------------------  COPY ON SELECTION METHODS for LINUX and Windows ------

procedure TFormNote.KMemo1MouseUp(Sender: TObject; Button: TMouseButton;
    Shift: TShiftState; X, Y: Integer);
{$IFNDEF DARWIN}    // Mac cannot do Primary Paste, ie XWindows Paste
var
    Point : TPoint;
    LinePos : TKmemoLinePosition; {$endif}
begin
    {$IFNDEF DARWIN}
    if Button = mbMiddle then begin
      Point := TPoint.Create(X, Y); // X and Y are pixels, not char positions !
      LinePos := eolEnd;
      while X > 0 do begin          // we might be right of the eol marker.
            KMemo1.PointToIndex(Point, true, true, LinePos);
            if LinePos = eolInside then break;
            dec(Point.X);
      end;
      PrimaryPaste(KMemo1.PointToIndex(Point, true, true, LinePos));
      exit();
    end;
    if KMemo1.SelAvail and
        (Kmemo1.Blocks.SelLength <> 0) then
            SetPrimarySelection()
        else
            UnsetPrimarySelection();
    {$endif}
end;

procedure TFormNote.SetPrimarySelection;
var
  FormatList: Array [0..1] of TClipboardFormat;
begin
  if (PrimarySelection.OnRequest=@PrimaryCopy) then exit;
  FormatList[0] := CF_TEXT;
  try
    PrimarySelection.SetSupportedFormats(1, @FormatList[0]);
    PrimarySelection.OnRequest:=@PrimaryCopy;
  except
  end;
end;

procedure TFormNote.UnsetPrimarySelection;
begin
  if PrimarySelection.OnRequest=@PrimaryCopy then
    PrimarySelection.OnRequest:=nil;
end;

procedure TFormNote.PrimaryCopy(
  const RequestedFormatID: TClipboardFormat;  Data: TStream);
var
  s : string;
begin
    S := KMemo1.Blocks.SelText;
    if RequestedFormatID = CF_TEXT then
        if length(S) > 0 then
            Data.Write(s[1],length(s));
end;

procedure TFormNote.PrimaryPaste(SelIndex : integer);
var
  Buff : string;
begin
    if PrimarySelection.HasFormat(CF_TEXT) then begin  // I don't know if this is useful at all.
        Buff := PrimarySelection().AsText;
        if Buff <> '' then begin
            KMemo1.Blocks.InsertPlainText(SelIndex, Buff);
            KMemo1.SelStart := SelIndex;
            Kmemo1.SelEnd := SelIndex + length(Buff);
        end;
    end;
end;

procedure TFormNote.InsertDate();
var
  I : integer;
begin
    // showmessage(FormatDateTime('YYYY-MM-DD hh:mm:ss', now()));
    KMemo1.ExecuteCommand(ecInsertString, pchar(FormatDateTime(' YYYY-MM-DD hh:mm:ss ', now())));
    for I := 0 to 20 do
        KMemo1.ExecuteCommand(ecRight);
end;

{ -------------- U S E R   F O N T    C H A N G E S ----------------}

const
 ChangeSize   = 1;     // Used by AlterFont(..) and its friends.
 ChangeBold   = 2;
 ChangeItalic = 3;
 ChangeColor  = 4;
 ChangeFixedWidth = 5;
 ChangeStrikeout = 6;
 ChangeUnderline = 7;
 //DefaultFontName = 'default';             Font names determined in Settings
 //{$ifdef LINUX}
   //MonospaceFont = 'monospace';
 //{$else}
   //MonospaceFont = 'Lucida Console';
   //MonospaceFont = 'Monaco';        // might be a better choice
 //{$ifend}

{ This complex function will set font size, Bold or Italic or Color depending on the
  constant passed as first parameter. NewFontSize is ignored (and can be ommitted)
  if Command is ChangeBold or ChangeItalic, then toggle. If the function finds
  that the first char of selection already has that attribute it negates it,
  ie size becomes normal or no bold, no italics.

  It has to deal with several possible combinations and does so in three parts -
  1. Dealing with what happens around the SelStart. Possibly splitting once or twice
  2. Dealing with any complete blocks between start and end.
  3. Dealing with the stuff around the end. If its not already been done by 1.

  The actual Commands are defined above and are not used outside this unit.

  Consider possible ways this function can be called -
  a. With selstart at first char in a block, Selend at end of same block.
  b. Selstart at other than first char, selend at end of same block.
  c. Selstart after first char and selend before last char of same block.
  d, e, f. as above but spanning blocks.
  a. & d. Require no splitting.  Just apply change to block or blocks.
  b. & e. Needs one split. Split at SelStart and Apply to new and subsquent if any.
  c. & f. Needs two splits. Split at SelStar and SelEnd-1, then as above.

  So, decide what blocks we apply to, then apply. Sounds easy.

  AlterFont() is the entry point, it identifies and, if necessary splits blocks
  and calls AlterBlockFont() to do the changes, block by block.
  The decision as to turning [Colour,Bold,Italics] on or off SHOULD be made in
  AlterFont based on first char of selection and passed to AlterBlockFont.
}

procedure TFormNote.AlterFont(const Command : integer; const NewFontSize : integer = 0);
var
	FirstBlockNo, LastBlockNo, IntIndex, LastChar, FirstChar : longint;
	SplitStart : boolean = false;
begin
    if KMemo1.ReadOnly then exit();

    Processing := True;

    MarkDirty();
	LastChar := Kmemo1.RealSelEnd;			// SelEnd points to first non-selected char
    FirstChar := KMemo1.RealSelStart;
	FirstBlockNo := Kmemo1.Blocks.IndexToBlockIndex(FirstChar, IntIndex);
    if IntIndex <> 0 then			// Not Starting on block boundary.
		SplitStart := True;
    LastBlockNo := Kmemo1.Blocks.IndexToBlockIndex(LastChar, IntIndex);
    if IntIndex <> (length(Kmemo1.Blocks.Items[LastBlockNo].Text) -1) then 	// Not Last char in block
        LastBlockNo := KMemo1.SplitAt(LastChar) -1;       // we want whats before the split.
    while LastBlockNo > FirstBlockNo do begin
        AlterBlockFont(FirstBlockNo, LastBlockNo, Command, NewFontSize);
        dec(LastBlockNo);
    end;
    // Now, only First Block to deal with
    if SplitStart then
		FirstBlockNo := KMemo1.SplitAt(FirstChar);
    AlterBlockFont(FirstBlockNo, FirstBlockNo, Command, NewFontSize);
    KMemo1.SelEnd := LastChar;	// Any splitting above seems to subtly alter SelEnd, reset.
    KMemo1.SelStart := FirstChar;

    Processing := False;
end;


	{  Takes a Block number and applies changes to that block }
procedure TFormNote.AlterBlockFont(const FirstBlockNo, BlockNo : longint; const Command : integer; const NewFontSize : integer = 0);
var
	Block, FirstBlock : TKMemoTextBlock;

begin
    FirstBlock := TKMemoTextBlock(KMemo1.Blocks.Items[FirstBlockNo]);
	Block := TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]);
    //if (Command = ChangeSize) and (NewFontSize = FontSizeNormal) then begin  // Don't toggle, just set to FontNormal
    //     Block.TextStyle.Font.Size := FontSizeNormal;
    //     exit();
    //end;
    case Command of
		{ChangeSize :	if Block.TextStyle.Font.Size = NewFontSize then begin
						Block.TextStyle.Font.Size := Sett.FontNormal;
					end else begin
 						Block.TextStyle.Font.Size := NewFontSize;
					end;  }
        ChangeSize : Block.TextStyle.Font.Size := NewFontSize;
		ChangeBold :   	if fsBold in FirstBlock.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsBold];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsBold];
					end;
		ChangeItalic :
					if fsItalic in FirstBlock.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsItalic];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsItalic];
					end;
        ChangeFixedWidth :
                    if FirstBlock.TextStyle.Font.Name <> FixedFont then begin
                       Block.TextStyle.Font.Pitch := fpFixed;
                       Block.TextStyle.Font.Name := FixedFont;
                    end else begin
                       Block.TextStyle.Font.Pitch := fpVariable;
	                    Block.TextStyle.Font.Name := UsualFont;
                    end;

        ChangeStrikeout :
					if fsStrikeout in FirstBlock.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsStrikeout];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsStrikeout];
					end;
        ChangeUnderline :
					if fsUnderline in FirstBlock.TextStyle.Font.style then begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsUnderline];
					end else begin
						Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsUnderline];
					end;
		ChangeColor :
                    if FirstBlock.TextStyle.Brush.Color <> HiColour then begin
                        Block.TextStyle.Brush.Color := HiColour;
                    end else begin
                        Block.TextStyle.Brush.Color := BackGndColour; { clDefault; }
                    end;
	end;
end;

procedure TFormNote.MenuHighLightClick(Sender: TObject);
begin
    AlterFont(ChangeColor);
end;

procedure TFormNote.MenuLargeClick(Sender: TObject);
begin
   //AlterFont(ChangeSize, FontSizeLarge);
end;

procedure TFormNote.MenuNormalClick(Sender: TObject);
begin
   //AlterFont(ChangeSize, FontSizeNormal);	// Note, this won't toggle !
end;

procedure TFormNote.MenuSmallClick(Sender: TObject);
begin
    //AlterFont(ChangeSize, FontSizeSmall);
end;

procedure TFormNote.MenuHugeClick(Sender: TObject);
begin
   //AlterFont(ChangeSize, FontSizeHuge);
end;

procedure TFormNote.MenuBoldClick(Sender: TObject);
begin
	AlterFont(ChangeBold);
end;

procedure TFormNote.MenuItalicClick(Sender: TObject);
begin
	AlterFont(ChangeItalic);
end;

procedure TFormNote.MenuItemEvaluateClick(Sender: TObject);
begin
   InitiateCalc();
end;

procedure TFormNote.MenuItemExportMarkdownClick(Sender: TObject);
begin
  FormMarkDown.TheKMemo := KMemo1;
  FormMarkDown.Caption:= CleanCaption();
  FormMarkDown.Show;
end;

{
procedure TFormNote.MenuItemIndexClick(Sender: TObject);
var
    IForm : TFormIndex;
begin
    IForm := TFormIndex.Create(Self);
    IForm.TheKMemo := KMemo1;
    IForm.Left := Left;
    IForm.Top := Top;
    IForm.ShowModal;
    if IForm.SelectedBlock >= 0 then begin
        KMemo1.SelStart := KMemo1.Blocks.BlockToIndex(KMemo1.Blocks.Items[IForm.SelectedBlock]);
        KMemo1.SelLength := 0;
    end;
    IForm.Free;
    KMemo1.SetFocus;
end;
}

procedure TFormNote.MenuItemSettingsClick(Sender: TObject);
begin
    //FormSettings.show;
end;

procedure TFormNote.MenuUnderlineClick(Sender: TObject);
begin
    AlterFont(ChangeUnderline);
end;

procedure TFormNote.MenuStrikeoutClick(Sender: TObject);
begin
        AlterFont(ChangeStrikeout);
end;

procedure TFormNote.MenuFixedWidthClick(Sender: TObject);
begin
       AlterFont(ChangeFixedWidth);
end;

{ ------- S T A N D A R D    E D I T I N G    F U N C T I O N S ----- }

    // Locates if it can Term and selects it. Ret False if not found.
    // Uses regional var, LastFind to start its search from, set to 1 for new search
function TFormNote.FindIt(Term : string; GoForward, CaseSensitive : boolean) : boolean;
var
    NewPos : integer = 0;
    {$ifdef WINDOWS}
    Ptr, EndP : PChar;
    {$endif}
    NumbCR : integer = 0;
begin
    Result := False;
    if GoForward then begin
        if CaseSensitive then
            NewPos := PosEx(Term, KMemo1.Blocks.Text, LastFind + 1)
        else
            NewPos := PosEx(uppercase(Term), uppercase(KMemo1.Blocks.Text), LastFind + 1);
    end else begin
        if CaseSensitive then
            NewPos := RPosEx(Term, KMemo1.Blocks.Text, LastFind)
        else
            NewPos := RPosEx(uppercase(Term), uppercase(KMemo1.Blocks.Text), LastFind);
    end;
    //Memo1.append('Pos = ' + inttostr(NewPos) + '  Found=' + inttostr(FoundPos));
    if NewPos > 0 then begin
        {$ifdef WINDOWS}                // does no harm in Unix but a bit slow ?
        Ptr := PChar(KMemo1.Blocks.text);
        EndP := Ptr + NewPos-1;
        while Ptr < EndP do begin
            if Ptr^ = #13 then inc(NumbCR);
            inc(Ptr);
        end;
        {$endif}
        KMemo1.SelStart := UTF8Length(pchar(KMemo1.Blocks.Text), NewPos-1) - NumbCR;
        LastFind := NewPos;
        KMemo1.SelLength := UTF8length(Term);
        Result := True;
    end;
end;


procedure TFormNote.MenuItemCopyClick(Sender: TObject);
begin
	KMemo1.ExecuteCommand(ecCopy);
end;

procedure TFormNote.MenuItemCutClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    KMemo1.ExecuteCommand(ecCut);
    MarkDirty();
    //if not Dirty then TimerSave.Enabled := true;
    //Dirty := true;
    //Label1.Caption := 'd';
end;

procedure TFormNote.MenuItemDeleteClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    // KMemo1.ExecuteCommand(ecClearSelection);
    KMemo1.Blocks.ClearSelection;
    MarkDirty();
    //if not Dirty then TimerSave.Enabled := true;
    //Dirty := true;
    //Label1.Caption := 'd';
end;

procedure TFormNote.MenuItemExportPlainTextClick(Sender: TObject);
begin
     //SaveNoteAs('txt');
end;

procedure TFormNote.MenuItemExportRTFClick(Sender: TObject);
begin
   //SaveNoteAs('rtf');
end;


procedure TFormNote.MarkDirty();
begin
    Dirty := true;
    if Caption = '' then Caption := '*'
    else if Caption[1] <> '*' then
        Caption := '* ' + Caption;
end;


function TFormNote.CleanCaption(): ANSIString;
begin
    if Caption = '' then exit('');
    if Caption[1] = '*' then
        Result := Copy(Caption, 3, 256)
    else Result := Caption;
end;


procedure TFormNote.MenuItemPasteClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
    Processing := True;
    KMemo1.ExecuteCommand(ecPaste);
    MarkDirty();
    Processing := False;
end;


procedure TFormNote.MenuItemPrintClick(Sender: TObject);
var
    KPrint : TKprn;
begin
    if PrintDialog1.Execute then begin
      KPrint := TKPrn.Create;
      KPrint.PrintKmemo(KMemo1);
      FreeandNil(KPrint);
    end;
end;

procedure TFormNote.MenuItemSelectAllClick(Sender: TObject);
begin
	KMemo1.ExecuteCommand(ecSelectAll);
end;

procedure TFormNote.MenuItemSpellClick(Sender: TObject);
var
    SpellBox : TFormSpell;
begin
    if KMemo1.ReadOnly then exit();
    //if Sett.SpellConfig then begin
        SpellBox := TFormSpell.Create(Application);
        // SpellBox.Top := Placement + random(Placement*2);
        // SpellBox.Left := Placement + random(Placement*2);
        SpellBox.TextToCheck:= KMemo1.Blocks.Text;
        SpellBox.TheKMemo := KMemo1;
        SpellBox.ShowModal;
    //end else showmessage('Sorry, spelling not configured');
end;

procedure TFormNote.MenuItemSyncClick(Sender: TObject);
begin
    if KMemo1.ReadOnly then exit();
	//if Dirty then SaveTheNote();
    //Sett.Synchronise();
end;

{ - - - H O U S E   K E E P I N G   F U C T I O N S ----- }

procedure TFormNote.TimerSaveTimer(Sender: TObject);
begin
    //TimerSave.Enabled:=False;
	// showmessage('Time is up');
    //SaveTheNote();
end;

procedure TFormNote.CleanUTF8();

        function BitSet(Value : byte; TheBit : integer) : boolean;      // theBit 0-7
        begin
            Result := ((Value shr TheBit) and 1) = 1;
        end;

        function CleanedUTF8(var TheText : string) : boolean;
        var cnt : integer = 1;
            NumbBytes : integer = 0;
            i : integer;
        begin
            Result := false;
            while Cnt <= TheText.Length do begin
                if BitSet(byte(TheText[cnt]), 7) then begin
                    // OK, we have a utf8 code. It will need at least one extra byte, maybe 2 or 3
                    NumbBytes := 1;
                    if BitSet(byte(TheText[cnt]), 5) then inc(NumbBytes);
                    if BitSet(byte(TheText[cnt]), 4) then inc(NumbBytes);
                    if Cnt + NumbBytes > TheText.Length then begin      // enough bytes remaining ....
                        delete(TheText, Cnt, 1);
                        Result := true;
                        continue;
                    end;
                    for i := 1 to NumbBytes do begin            // are they the right sort of bytes ?
                        if not BitSet(byte(TheText[cnt + i]), 7) then begin
                            delete(TheText, Cnt, 1);            //
                            NumbBytes := -1;                    // so the dec below does not skip a char
                            Result := true;
                            break;
                        end;
                    end;
                    Cnt := Cnt + NumbBytes;
                end;
                inc(cnt);
            end;
        end;

var
    i : integer = 0;
    AStr : string;
    TB : TKMemoTextBlock;
begin
   KMemo1.blocks.LockUpdate;
    while i < Kmemo1.blocks.count do begin
        AStr := Kmemo1.Blocks.Items[i].text;
        if KMemo1.Blocks.Items[i].ClassNameis('TKMemoTextBlock')
            or KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink') then begin
                if CleanedUTF8(AStr) then begin
                    TB := KMemo1.Blocks.AddTextBlock(AStr, i);
                    TB.TextStyle.Font := TKMemoTextBlock(KMemo1.blocks.Items[i+1]).TextStyle.Font;
                    TB.TextStyle.Brush := TKMemoTextBlock(KMemo1.blocks.Items[i+1]).TextStyle.Brush;
                    KMemo1.Blocks.Delete(i+1);
                end;
        end;
        inc(i);
    end;
    KMemo1.blocks.UnLockUpdate;
end;

procedure TFormNote.FormShow(Sender: TObject);
begin
   TRlog('TFormNote.FormShow');
   if Processing then exit();				// its a "re-show" event. Already have a note loaded.

   TRlog('TFormNote.FormShow Testing loaded');

   if not AlreadyLoaded then NoteToMemo();

   TRlog('TFormNote.FormShow Notetomemo done');


   {$ifdef LINUX}
   KMemo1.ExecuteCommand(ecPaste);         // this to deal with a "first copy" issue.
   {$endif}

   MarkTitle();

   KMemo1.SelStart := KMemo1.Text.Length;  // set curser pos to end
   KMemo1.SelEnd := Kmemo1.Text.Length;

   KMemo1.SetFocus;
   Dirty := False;

   if SearchedTerm <> '' then
        FindIt(SearchedTerm, True, False)
    else begin
        KMemo1.executecommand(ecEditorTop);
        KMemo1.ExecuteCommand(ecDown);          // DRB Playing
    end;
   KMemo1.Blocks.LockUpdate;
   {$ifdef windows}
    Color:= TextColour;
   {$endif}
   KMemo1.Colors.BkGnd:= BackGndColour;
   Kmemo1.Blocks.DefaultTextStyle.Font.Color := TextColour;
   KMemo1.Blocks.UnLockUpdate;
end;

	{ This gets called when the TrayMenu quit entry is clicked }
    { No it does not, only when user manually closes this form. }
procedure TFormNote.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
    Release;
end;

procedure TFormNote.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  TRlog('TFormNote.FormCloseQuery');

    CanClose := True;
end;

procedure TFormNote.FormCreate(Sender: TObject);
begin
  TRlog('TFormNote.FormCreate');
  AlreadyLoaded := false;
  {$ifdef DARWIN}
    MenuBold.ShortCut      := KeyToShortCut(VK_B, [ssMeta]);
    MenuItalic.ShortCut    := KeyToShortCut(VK_I, [ssMeta]);
    MenuStrikeout.ShortCut := KeyToShortCut(VK_S, [ssMeta]);
    MenuHighLight.ShortCut := KeyToShortCut(VK_H, [ssAlt]);
    MenuFixedWidth.ShortCut:= KeyToShortCut(VK_T, [ssMeta]);
    MenuUnderline.ShortCut := KeyToShortCut(VK_U, [ssMeta]);
    MenuItemFind.ShortCut  := KeyToShortCut(VK_F, [ssMeta]);
    MenuItemEvaluate.ShortCut := KeyToShortCut(VK_E, [ssMeta]);
  {$endif}
end;


// As UpdateNote does not record Notebook membership, abandon it for now.
// Maybe come back later and see if it can be patched, its probably quicker.
// Was only called on a clean note ....
function TFormNote.UpdateNote(NRec : TNoteUpdaterec) : boolean;
var
    InFile, OutFile: TextFile;
    {NoteDateSt, }InString, TempName : string;
begin
  if not FileExistsUTF8(NRec.FFName) then exit(false);     // if its not there, the note has just been deleted
  TempName := GetTempFile();
  AssignFile(InFile, NRec.FFName);
  AssignFile(OutFile, TempName);
  try
      try
          Reset(InFile);
          Rewrite(OutFile);
          while not eof(InFile) do begin
              readln(InFile, InString);
              if (Pos('<cursor-position>', InString) > 0) then break;
              writeln(OutFile, InString);
          end;
          // OK, we are looking atthe part we want to change, ignore infile, we know better.
          writeln(OutFile, '  <cursor-position>' + NRec.CPos + '</cursor-position>');
          writeln(OutFile, '  <selection-bound-position>1</selection-bound-position>');
          writeln(OutFile, '  <width>' + NRec.Width + '</width>');
          writeln(OutFile, '  <height>' + NRec.height + '</height>');
          writeln(OutFile, '  <x>' + NRec.X + '</x>');
          writeln(OutFile, '  <y>' + NRec.Y + '</y>');
          writeln(OutFile, '  <open-on-startup>' + NRec.OOS + '</open-on-startup>');

          //Must see if this note is in a notebook, if so, record here.

          writeln(OutFile, '</note>');
      finally
          CloseFile(OutFile);
          CloseFile(InFile);
      end;
  except
    on E: EInOutError do begin
        TRlog('File handling error occurred updating clean note location. Details: ' + E.Message);
        exit(False);
    end;
  end;
  result := CopyFile(TempName, Nrec.FFName);    // wrap this in a Try
  if result = false then TRlog('ERROR copying [' + TempName + '] to [' + NRec.FFName + ']');
  result := DeleteFileUTF8(TempName);
  if result = false then TRlog('ERROR deleting [' + TempName + '] ');
end;

procedure TFormNote.FormDestroy(Sender: TObject);
{var
    ARec : TNoteUpdateRec; }
begin
    UnsetPrimarySelection;                                      // tidy up copy on selection.
    //if (length(NoteFileName) = 0) and (not Dirty) then exit;    // A new, unchanged note, no need to save.
    //if not Kmemo1.ReadOnly then
        //if not DeletingThisNote then
            //if (not SingleNoteMode) or Dirty then       // We always save, except in SingleNoteMode (where we save only if dirty)
            //    SaveTheNote(Sett.AreClosing);           // Jan 2020, just call SaveTheNote, it knows how to record the notebook state
    //SearchForm.NoteClosing(NoteFileName);

end;

function TFormNote.GetTitle(out TheTitle : ANSIString) : boolean;
var
    BlockNo : longint = 0;
    //TestSt : ANSIString;
begin
    Result := False;
    TheTitle := '';
    while Kmemo1.Blocks.Items[BlockNo].ClassName <> 'TKMemoParagraph' do begin
	// while Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoTextBlock' do begin
        TheTitle := TheTitle + Kmemo1.Blocks.Items[BlockNo].Text;
       	inc(BlockNo);
        //TestSt := Kmemo1.Blocks.Items[BlockNo].ClassName;
        if BlockNo >= Kmemo1.Blocks.Count then break;
    end;                            // Stopped at first TKMemoParagraph if it exists.
    if TheTitle <> '' then Result := True;
end;

procedure TFormNote.MarkTitle();
var
    BlockNo : integer = 0;
    EndBlock, blar : integer;
begin
   if Processing then exit();
    { if there is more than one block, and the first, [0], is a para, delete it.}
    if KMemo1.Blocks.Count <= 2 then exit();	// Don't try to mark title until more blocks.
    Processing := true;
    Kmemo1.Blocks.LockUpdate;
    if Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoParagraph' then
          Kmemo1.Blocks.DeleteEOL(0);

        while Kmemo1.Blocks.Items[BlockNo].ClassName <> 'TKMemoParagraph' do begin
            if Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock') then begin    // just possible its an image, ignore ....
                //TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Size := FontSizeTitle;
                TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Color := TitleColour;
                TKMemoTextBlock(Kmemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style := [fsUnderline];
            end;
           	inc(BlockNo);
            if BlockNo >= Kmemo1.Blocks.Count then begin
                //AtTheEnd := True;
                break;
            end;
       	end;                                // Stopped at first TKMemoParagraph if it exists.
        BlocksInTitle := BlockNo;
        { Make sure user has not smeared Title charactistics to next line
          Scan back from cursor to end of title, if Title font, reset. }
        EndBlock := KMemo1.Blocks.IndexToBlockIndex(KMemo1.Selstart, Blar);
        while EndBlock > BlocksInTitle do begin
            //if KMemo1.Blocks.Items[EndBlock].ClassNameIs('TKMemoTextBlock') and
            //    (TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Size = FontSizeTitle) then begin
            //        TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Size := FontSizeNormal;
            //        TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Color := TextColour;
            //        TKMemoTextBlock(Kmemo1.Blocks.Items[EndBlock]).TextStyle.Font.Style := [];
            //    end;
            dec(EndBlock);
        end;

    KMemo1.Blocks.UnLockUpdate;
    Processing := False;
end;


{ -----------  L I N K    R E L A T E D    F U N C T I O N S  ---------- }

procedure TFormNote.MakeLink({const Link : ANSIString;} const Index, Len : longint);
var
	Hyperlink, HL: TKMemoHyperlink;
    TrueLink : string;
	BlockNo, BlockOffset, Blar{, i} : longint;
	// DontSplit : Boolean = false;
    // blk : TKMemoTextBlock;
begin
	// Is it already a Hyperlink ? We leave valid hyperlinks in place.
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(Index, BlockOffset);
    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKHyperlink') then exit();
	// Is it all in the same block ?
    if BlockNo <> Kmemo1.Blocks.IndexToBlockIndex(Index + Len -1, Blar) then exit();
    TrueLink := utf8copy(Kmemo1.Blocks.Items[BlockNo].Text, BlockOffset+1, Len);
    if length(Kmemo1.Blocks.Items[BlockNo].Text) = Len {length(TrueLink)} then begin
         Kmemo1.Blocks.Delete(BlockNo);
         //writeln('Block deleted');
    end
    else  begin
        KMemo1.SelStart:= Index;
        KMemo1.SelLength:=Len;
        KMemo1.ClearSelection();
        BlockNo := KMemo1.SplitAt(Index);
        //writeln('Block Split');
    end;

	Hyperlink := TKMemoHyperlink.Create;
	// Hyperlink.Text := Link;
    Hyperlink.Text := TrueLink;
    // Hyperlink.TextStyle.Font.Color:= clRed {Sett.TitleColour};
    Hyperlink.Textstyle.StyleChanged   :=  true;
	Hyperlink.OnClick := @OnUserClickLink;
	HL := KMemo1.Blocks.AddHyperlink(Hyperlink, BlockNo);
    HL.TextStyle.Font.Color:= TitleColour;
    // Note the colour seems to get set to some standard that TK likes when added.

(*
i := BlockNo-2;
while I < (BlockNo + 10) do begin
    Blk := TKMemoTextBlock(KMemo1.Blocks.Items[i]);
    write('no=' + inttostr(i));
    if fsUnderline in Blk.TextStyle.Font.style then
        write(' Underlined ')
    else
        write(' plain ');
    writeln(' Type=' + KMemo1.Blocks.Items[i].ClassName + ' Text=' + KMemo1.Blocks.Items[i].Text);
    inc(i);
end; *)
end;



// Starts searching a string at StartAt for Term, returns 1 based offset from start of str if found, 0 if not. Like UTF8Pos(
function TFormNote.RelativePos(const Term : ANSIString; const MText : PChar; StartAt : integer) : integer;
begin
  result := Pos(Term, MText+StartAt);
  if Result <> 0 then
      Result := Result + StartAt;
end;


procedure TFormNote.MakeAllLinks(const PText : PChar; const Term : ANSIString; const StartScan : longint =1; EndScan : longint = 0);
var
	Offset, NumbCR   : longint;
    {$ifdef WINDOWS}
    Ptr, EndP : PChar;                  // Will generate "not used" warning in Unix
    {$endif}
begin
    Offset := RelativePos(Term, PText, StartScan);
    while Offset > 0 do begin
    	NumbCR := 0;
        {$ifdef WINDOWS}                // compensate for Windows silly two char newlines
        EndP := PText + Offset;
        while EndP > PText do begin
            if EndP^ = #13 then inc(NumbCR);
            dec(EndP);
        end;
        {$endif}
        if (PText[Offset-2] in [' ', #10, #13, ',', '.']) and
                        (PText[Offset + length(Term) -1] in [' ', #10, #13, ',', '.']) then
            MakeLink(UTF8Length(PText, Offset) -1 -NumbCR, UTF8length(Term));
        Offset := RelativePos(Term, PText, Offset + 1);
        if EndScan > 0 then
        	if Offset> EndScan then break;
    end;
end;


procedure TFormNote.CheckForHTTP(const PText : pchar; const StartS, EndS : longint);

    function ValidWebLength(StartAt : integer) : integer;               // stupid !  Don't pass StartAt, use local vars, cheaper....
    var
        I : integer;
    begin
        I := 7;                                                         // '7' being length of 'http://'
        if not(PText[StartAt-2] in [' ', ',', #10, #13]) then exit(0);  // no leading whitespace
        while PText[StartAt+I] <> '.' do begin
            if (StartAt + I) > EndS then exit(0);                       // beyond our scan zone before dot
            if PText[StartAt+I] in [' ', ',', #10, #13] then exit(0);   // hit whitespace before a dot
            inc(I);
        end;
        inc(i);
        if (PText[StartAt+I] in [' ', ',', #10, #13]) then exit(0);     // the dot is at the end !
        while (not(PText[StartAt+I] in [' ', ',', #10, #13])) do begin
            if (StartAt + I) > EndS then exit(0);                       // beyond our scan zone before whitespace
            inc(I);
        end;
        if PText[StartAt+I-1] = '.' then
            Result := I
        else
            Result := I+1;
    end;

var
    http, Offset, NumbCR : integer;
    Len : integer;
    {$ifdef WINDOWS}EndP : PChar; {$endif}
begin
    OffSet := StartS;
    http := pos('http', PText+Offset);
    while (http <> 0) and ((http+Offset) < EndS) do begin
        if (copy(PText, Offset+http+4, 3) = '://') or (copy(PText, Offset+http+4, 4) = 's://') then begin
            Len := ValidWebLength(Offset+http);
            if Len > 0 then begin
                NumbCR := 0;
                {$ifdef WINDOWS}
                EndP := PText + Offset + http;
                while EndP > PText do begin
                    if EndP^ = #13 then inc(NumbCR);
                    dec(EndP);
                end;
                {$endif}
                MakeLink({copy(PText, Offset+http, Len), } UTF8Length(PText, OffSet + http)-1 -NumbCR, Len);
//    TRlog('CheckForHTTP Index = ' + inttostr(UTF8Length(PText, OffSet + http)-1 -NumbCR) + ' and Len = ' + inttostr(Len));
            end;
            if len > 0 then
        end;
        inc(Offset, http+1);
        http := pos('http', PText + Offset);
    end;
end;


procedure TFormNote.CheckForLinks(const StartScan : longint =1; EndScan : longint = 0);
var
    Searchterm : ANSIstring;
    Len, httpLen : longint;
//    Tick, Tock : qword;
    pText : pchar;
begin
   if Processing then exit();

   // There is a thing called KMemo1.Blocks.SelectableLength but it returns the number of characters, not bytes, much faster though
   // Note, we dont need Len if only doing http and its not whole note being checked (at startup). So, could save a bit ....
   Len := length(KMemo1.Blocks.text);              // saves 7mS by calling length() only once ! But still 8mS
    if StartScan >= Len then exit;                  // prevent crash when memo almost empty
    if EndScan > Len then EndScan := Len;
    if EndScan = 0 then
        httpLen := Len
    else  httpLen := EndScan;
    Processing := True;
	//SearchForm.StartSearch();
    KMemo1.Blocks.LockUpdate;
    //Tick := gettickcount64();
    PText := PChar(lowerCase(KMemo1.Blocks.text));
    if ShowExtLinks then          // OK, what are we here for ?
        CheckForHTTP(PText, StartScan, httpLen);
    if ShowIntLinks then
        //while SearchForm.NextNoteTitle(SearchTerm) do
          //  if SearchTerm <> NoteTitle then             // My tests indicate lowercase() has neglible overhead and is UTF8 ok.
          //      MakeAllLinks(PText, lowercase(SearchTerm), StartScan, EndScan);
    //Tock := gettickcount64();
    KMemo1.Blocks.UnLockUpdate;
    //TRlog('MakeAllLinks ' + inttostr(Tock - Tick) + 'mS');
    Processing := True;
end;



procedure TFormNote.ClearNearLink(const StartS, EndS : integer); //CurrentPos : longint);
var
    {BlockNo,}  Blar, StartBlock, EndBlock : longint;
    LinkText  : ANSIString;

    function ValidWebLink() : boolean;                  // returns true if LinkText is valid web address
    var
        DotSpot : integer;
        Str : String;
    begin
//writeln('Scanning for web address ' + LinkText);
        if pos(' ', LinkText) > 0 then exit(false);
        if (copy(LinkText,1, 8) <> 'https://') and (copy(LinkText, 1, 7) <> 'http://') then exit(false);
        Str := TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock-1]).Text;
        if (KMemo1.Blocks.Items[StartBlock-1].ClassName <> 'TKMemoParagraph') and
            not Str.EndsText(' ', Str) then exit(false);
        if (KMemo1.Blocks.Items[StartBlock+1].ClassName <> 'TKMemoParagraph') and
            (not TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock+1]).Text.StartsWith(' ')) then exit(false);
        DotSpot := pos('.', LinkText);
        if DotSpot = 0 then exit(false);
        if (DotSpot < 8) or (DotSpot > length(LinkText)-1) then exit(false);
        if LinkText.EndsWith('.') then exit(false);
        result := true;
        //writeln(' Valid http or https addess');
    end;

begin
    Processing := True;
    StartBlock := KMemo1.Blocks.IndexToBlockIndex(StartS, Blar);
    EndBlock := KMemo1.Blocks.IndexToBlockIndex(EndS, Blar);
    if StartBlock < 2 then StartBlock := 2;
    if EndBlock > Kmemo1.Blocks.Count then EndBlock := Kmemo1.Blocks.Count;
    KMemo1.Blocks.LockUpdate;
    try
    while StartBlock < EndBlock do begin
        //if TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Size = FontSizeTitle then begin
        //    inc(StartBlock);
        //    continue;
        //end;
        if KMemo1.Blocks.Items[StartBlock].ClassNameIs('TKMemoHyperlink') then begin
            LinkText := Kmemo1.Blocks.Items[StartBlock].Text;
        //	if not (SearchForm.IsThisaTitle(LinkText) or ValidWebLink()) then begin
                // Must check here if its also not a valid HTTP link.
//writeln('Removing link ' + LinkText);
        //        Kmemo1.Blocks.Delete(StartBlock);
        //		KMemo1.Blocks.AddTextBlock(Linktext, StartBlock);
        //    end;
        end else begin
            // Must check here that its not been subject to the copying of a links colour and underline
            // we know its not a link and we know its not title. So, check color ...
            if TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Color = TitleColour then begin    // we set links to title colour
                TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Style
                    := TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Style - [fsUnderLine];
                TKMemoTextBlock(KMemo1.Blocks.Items[StartBlock]).TextStyle.Font.Color := TextColour;
            end;
        end;
        inc(StartBlock);
    end;
    finally
        KMemo1.Blocks.UnlockUpdate;
        Processing := False;
    end;

(*          remove all this after a testing cycle.

    exit;



    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurrentPos, Blar);
    Ready := False;
    LinkText := Kmemo1.Blocks.Items[BlockNo].Text;              // debug
    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink') then begin
        LinkText := Kmemo1.Blocks.Items[BlockNo].Text;
    	if Not SearchForm.IsThisaTitle(LinkText) then begin
        	KMemo1.Blocks.LockUpdate;                         // I don't think we should lock here.
    		Kmemo1.Blocks.Delete(BlockNo);
    		KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
        	KMemo1.Blocks.UnlockUpdate;
        end;
    end;
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurrentPos-1, Blar);

    if KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoHyperlink') then begin
        LinkText := Kmemo1.Blocks.Items[BlockNo].Text;
        if Not SearchForm.IsThisaTitle(LinkText) then begin
        	KMemo1.Blocks.LockUpdate;
    		Kmemo1.Blocks.Delete(BlockNo);
    		KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
        	KMemo1.Blocks.UnlockUpdate;
        end;
    end;
    Ready := True;           *)
end;


	{ Scans across whole note removing any links it finds. Block containing link
      must be removed and new non-link block created in its place.
      Note that the scaning is very quick, gets bogged down doing the remove/add

      This function is not needed at present but leave it here in case its
      useful after user chooses to not display links. }
procedure TFormNote.ClearLinks(const StartScan : longint =0; EndScan : longint = 0);
var
      BlockNo, EndBlock, Blar : longint;
      LinkText : ANSIString;
begin
    Processing := True;
    BlockNo := KMemo1.Blocks.IndexToBlockIndex(StartScan, Blar); // DANGER, we must adjust StartScan to block boundary
    EndBlock := KMemo1.Blocks.IndexToBlockIndex(EndScan, Blar);	 // DANGER, we must adjust EndScan to block boundary
    KMemo1.Blocks.LockUpdate;
    while BlockNo <= EndBlock do begin							// DANGER, must check these block numbers work
        if Kmemo1.Blocks.Items[BlockNo].ClassName = 'TKMemoHyperlink' then begin
            LinkText := Kmemo1.Blocks.Items[BlockNo].Text;
            Kmemo1.Blocks.Delete(BlockNo);
            KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
		end;
        inc(BlockNo);
	end;
    KMemo1.Blocks.UnLockUpdate;
    Processing := False;
end;

procedure TFormNote.OnUserClickLink(sender : TObject);
begin
    if (copy(TKMemoHyperlink(Sender).Text, 1, 7) = 'http://') or
        (copy(TKMemoHyperlink(Sender).Text, 1, 8) = 'https://') then
            OpenUrl(TKMemoHyperlink(Sender).Text)
    else
	    //SearchForm.OpenNote(TKMemoHyperlink(Sender).Text);
end;


procedure TFormNote.DoHousekeeping();
var
    CurserPos, SelLen, StartScan, EndScan, BlockNo, Blar : longint;
    TempTitle : ANSIString;
    // TS1, TS2, TS3, TS4 : TTimeStamp;           // Temp time stamping to test speed
begin
    if KMemo1.ReadOnly then exit();
    CurserPos := KMemo1.RealSelStart;
    SelLen := KMemo1.RealSelLength;
    StartScan := CurserPos - LinkScanRange;
    if StartScan < length(Caption) then StartScan := length(Caption);
    EndScan := CurserPos + LinkScanRange;
    if EndScan > length(KMemo1.Text) then EndScan := length(KMemo1.Text);   // Danger - should be KMemo1.Blocks.Text !!!
    // TS1:=DateTimeToTimeStamp(Now);

    BlockNo := KMemo1.Blocks.IndexToBlockIndex(CurserPos, Blar);

    if ((BlocksInTitle + 10) > BlockNo) then begin
          // We don't check title if user is not close to it.
  	    MarkTitle();
  	    GetTitle(TempTitle);
        if Dirty then
            Caption := '* ' + TempTitle
        else
            Caption := TempTitle;
    end;

    // OK, if we are in the first or second (?) block, no chance of a link anyway.
    if BlockNo < 2 then begin
        if KMemo1.Blocks.Count = 0 then 		// But bad things happen if its really empty !
            KMemo1.Blocks.AddParagraph();
  	        exit();
    end;
    if ShowIntLinks or ShowExtLinks then begin
  	    ClearNearLink(StartScan, EndScan {CurserPos});
  	    // TS2:=DateTimeToTimeStamp(Now);
        CheckForLinks(StartScan, EndScan);
        // TS3:=DateTimeToTimeStamp(Now);
    end;
    KMemo1.SelStart := CurserPos;
    KMemo1.SelLength := SelLen;
    //TRlog('Housekeeper called');

  // Memo1.append('Clear ' + inttostr(TS2.Time-TS1.Time) + 'ms  Check ' + inttostr(TS3.Time-TS2.Time));

  { Some notes about timing, 'medium' powered Linux laptop, 20k note.
    Checks and changes to Title - less than mS
    ClearNearLinks (none present) - less than mS
    CheckForLinks (none present) - 180mS, thats mostly used up by MakeLinks()
    	but length(KMemo1.Blocks.text) needs about 7mS too.

    Can do better !
  }
end;

procedure TFormNote.TimerHousekeepingTimer(Sender: TObject);
begin
    DoHouseKeeping();
end;


{ ---------------------- C A L C U L A T E    F U N C T I O N S ---------------}

procedure TFormNote.ExprTan(var Result: TFPExpressionResult;
    const Args: TExprParameterArray);
var
  x: Double;
begin
  x := ArgToFloat(Args[0]);
  Result.resFloat := tan(x);
end;

function TFormNote.DoCalculate(CalcStr : string) : string;
var
    FParser: TFPExpressionParser;
    parserResult: TFPExpressionResult;
begin
    result := '';
    if length(CalcStr) < 1 then exit('');
    if CalcStr[length(CalcStr)] = '=' then
        CalcStr := copy(CalcStr, 1, length(CalcStr)-1);
    FParser := TFPExpressionParser.Create(nil);
    try
        try
            FParser.Identifiers.AddFunction('tan', 'F', 'F', @ExprTan);
            FParser.Builtins := [bcMath];
            FParser.Expression := CalcStr;
            parserResult := FParser.Evaluate;
            case parserResult.ResultType of
                rtInteger : result := inttostr(parserResult.ResInteger);
                rtFloat : result := floattostrf(parserResult.ResFloat, ffFixed, 0, 3);
            end;
        finally
          FParser.Free;
        end;
    except on E: EExprParser do showmessage(E.Message);
    end;
end;

RESOURCESTRING
    rsUnabletoEvaluate = 'Unable to find an expression to evaluate';
// Called from a Ctrl-E, 'Equals', maybe 'Evaluate' ? Anyway, directs to appropriate
// methods.
procedure TFormNote.InitiateCalc();
var
    AnsStr : string;
begin
    if Kmemo1.blocks.RealSelLength > 0 then begin
        if not ComplexCalculate(AnsStr) then exit;
        AnsStr := '=' + AnsStr;
    end
        else if not SimpleCalculate(AnsStr) then
            if not ColumnCalculate(AnsStr) then exit;
    if AnsStr = '' then
        showmessage(rsUnabletoEvaluate)
    else begin
        //TRlog('KMemo1.SelStart=' + inttostr(KMemo1.SelStart) + 'KMemo1.RealSelStart=' + inttostr(KMemo1.RealSelStart));
        KMemo1.SelStart := KMemo1.Blocks.RealSelEnd;
        KMemo1.SelLength := 0;
        KMemo1.Blocks.InsertPlainText(KMemo1.SelStart, AnsStr);
        KMemo1.SelStart := KMemo1.SelStart + length(AnsStr);
        KMemo1.SelLength := 0;
        //TRlog('KMemo1.SelStart=' + inttostr(KMemo1.SelStart) + 'KMemo1.RealSelStart=' + inttostr(KMemo1.RealSelStart));
    end;
end;

// Returns all text in a para, 0 says current one, 1 previous para etc ...
function TFormNote.PreviousParagraphText(const Backby : integer) : string;
var
    BlockNo, StopBlockNo, Index : longint;
begin
     Result := '';
    StopBlockNo := KMemo1.NearestParagraphIndex;   // if we are on first line, '1'.
    Index := BackBy + 1;                           // we want to overshoot
    BlockNo := StopBlockNo;
    while Index > 0 do begin
        dec(BlockNo);
        dec(Index);
        if BlockNo < 1 then begin TRlog('underrun1'); exit; end;  // its all empty up there ....
        while not Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') do begin
            dec(BlockNo);
            if BlockNo < 1 then begin TRlog('Underrun 2'); exit; end;
        end;
        if Index = 1 then StopBlockNo := BlockNo;       // almost there yet ?
    end;
    inc(BlockNo);
    while BlockNo < StopBlockNo do begin
        Result := Result + Kmemo1.Blocks.Items[BlockNo].Text;
        inc(BlockNo);
    end;
    //TRlog('PREVIOUS BlockNo=' + inttostr(BlockNo) + '  StopBlockNo=' + inttostr(StopBlockNo));
end;


// Return content of paragraph that caret is within, up to caret pos.
function TFormNote.ParagraphTextTrunc() : string;
var
    BlockNo, StopBlockNo, PosInBlock : longint;
begin
    Result := '';
    StopBlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, PosInBlock);
    if StopBlockNo < 0 then StopBlockNo := 0;
    BlockNo := StopBlockNo-1;
    while (BlockNo > 0) and (not Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph')) do
        dec(BlockNo);
    // TRlog('BlockNo=' + inttostr(BlockNo) + ' StopBlock=' + inttostr(StopBlockNo) + '  PosInBlock=' + inttostr(PosInBlock));
    if BlockNo > 0 then inc(BlockNo);
    if BlockNo < 0 then BlockNo := 0;
    if (BlockNo > StopBlockNo) then exit;
    while BlockNo < StopBlockNo do begin
        Result := Result + Kmemo1.Blocks.Items[BlockNo].Text;
        inc(BlockNo);
    end;
    if (PosInBlock > 0) then begin
        Result := Result + copy(KMemo1.Blocks.Items[BlockNo].Text, 1, PosInBlock);
    end;
end;

// Looks for a number at both begining and end of string. Ret empty ones if unsuccessful
function TFormNote.FindNumbersInString(const AStr: string; out AtStart, AtEnd : string) : boolean;
var
    Index : integer = 1;
begin
    if AStr = '' then exit(false);
    AtStart := '';
    AtEnd := '';
    while Index <= length(AStr) do begin
        if AStr[Index] in ['0'..'9', '.'] then AtStart := AtStart + AStr[Index]
        else break;
        inc(Index);
    end;
    Index := length(AStr);
    while Index > 0 do begin
        if AStr[Index] in ['0'..'9', '.'] then AtEnd :=  AStr[Index] + AtEnd
        else break;
        dec(Index);
    end;
    result := (AtStart <> '') or (AtEnd <> '');
end;

// Tries to find a column of numbers above, trying to rhs, then lhs.
// if we find tow or more lines, use it.
function TFormNote.ColumnCalculate(out AStr : string) : boolean;
var
    TheLine, AtStart, AtEnd, CalcStrStart, CalcStrEnd : string;
    Index : integer = 1;
    StartDone : boolean = False;
    EndDone : boolean = False;
begin
    AStr := '';
    CalcStrStart := '';
    CalcStrEnd := '';
    repeat
        TheLine := PreviousParagraphText(Index);
        FindNumbersInString(TheLine, AtStart, AtEnd);
        //TRlog('Scanned string [' + TheLine + '] and found [' + AtStart + '] and [' + atEnd + ']');
        if AtStart = '' then
            if EndDone then break
            else StartDone := True;
        if AtEnd = '' then
            if StartDone then break
            else EndDone := True;
        if (AtStart <> '') and (not StartDone) then
            if CalcStrStart = '' then CalcStrStart := AtStart
            else CalcStrStart := CalcStrStart + ' + ' + AtStart;
        if (AtEnd <> '') and (not EndDone) then
            if CalcStrEnd = '' then CalcStrEnd := AtEnd
            else CalcStrEnd := CalcStrEnd + ' + ' + AtEnd;
        inc(Index);
    until (AtStart = '') and (AtEnd = '');
    if not EndDone then AStr := CalcStrEnd;
    if not StartDone then AStr := CalcStrStart;
    AStr := DoCalculate(AStr);
    Result := (AStr <> '');
end;

// Assumes that the current selection contains a complex calc expression.
function TFormNote.ComplexCalculate(out AStr : string) : boolean;
var
    BlockNo, Temp : longint;
begin
    BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd-1, Temp);
    if kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
        // TRlog('Para cleanup in progress');
        Temp := KMemo1.SelLength;
        Kmemo1.SelStart := KMemo1.Blocks.RealSelStart;
        KMemo1.SelLength := Temp-1;
    end;
    if abs(KMemo1.SelLength) < 1 then exit(false);
   // TRlog('Complex Calc [' + KMemo1.Blocks.SelText + ']');
   AStr := DoCalculate(KMemo1.Blocks.SelText);
   Result := (AStr <> '');
end;

const
    CalcChars : set of char =  ['0'..'9'] + ['^', '*', '-', '+', '/'] + ['.', '=', ' ', '(', ')'];

// acts iff char under curser or to left is an '='
function TFormNote.SimpleCalculate(out AStr : string) : boolean;
var
    Index : longint;
    GotEquals : boolean = false;
begin
    Result := False;
    AStr := ParagraphTextTrunc();
    // look for equals
    while length(AStr) > 0 do begin
        if AStr[length(AStr)] = ' ' then begin
            delete(AStr, length(AStr), 1);
            continue;
        end;
        if AStr[length(AStr)] = '=' then begin
            delete(AStr, length(AStr), 1);
            GotEquals := True;
            continue;
        end;
        if not GotEquals then exit
        else break;
    end;
    // if to here, we have a string that used to start with =, lets see what else it has ?
    Index := length(AStr);
    if Index = 0 then exit;
    while AStr[Index] in CalcChars do begin
        dec(Index);
        if Index < 1 then break;
    end;
    delete(AStr, 1, Index);
    // TRlog('SimpleCalc=[' + AStr + ']');
    AStr := DoCalculate(AStr);
    exit(AStr <> '');
end;

	{ Any change to the note text and this gets called. So, vital it be quick }
procedure TFormNote.KMemo1Change(Sender: TObject);
begin
    if Processing then exit();           // don't do any of this while starting up.

    MarkDirty();
end;

function TFormNote.NearABulletPoint(out Leading, Under, Trailing, IsFirstChar, NoBulletPara : Boolean;
        								out BlockNo, TrailOffset, LeadOffset : longint ) : boolean;
	// on medium linux laptop, 20k note this function takes less than a mS
var
    PosInBlock, Index, CharCount : longint;
begin
    Under := False;
    NoBulletPara := False;
    BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
    if kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
  		Under := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo]).Numbering = pnuBullets);
        NoBulletPara := not Under;
    end;
    Index := 1;
    CharCount := PosInBlock;
    while BlockNo >= Index do begin
	    if kmemo1.blocks.Items[BlockNo-Index].ClassNameIs('TKMemoParagraph') then break;
  	    CharCount := CharCount + kmemo1.blocks.Items[BlockNo-Index].Text.Length;
	    inc(Index);
        // Danger - what if we don't find one going left ?
    end;
    if BlockNo < Index then begin
        Result := False;
        TRlog('Returning False as we appear to be playing in Heading.');
        exit();
    end else Leading := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo-Index]).Numbering = pnuBullets);
    IsFirstChar := (CharCount = 0);
    LeadOffset := Index;
    Index := 0;
    while true do begin
        // must not call Classnameis with blockno = count
        TRlog('Doing para seek, C=' + inttostr(KMemo1.Blocks.Count) + ' B=' + inttostr(BlockNo) + ' I=' + inttostr(Index));
        inc(Index);
        if (BlockNo + Index) >= (Kmemo1.Blocks.Count) then begin

            TRlog('Overrun looking for a para marker.');
            // means there are no para markers beyond here.  So cannot be TrailingBullet
            Index := 0;
            break;
        end;
	    if kmemo1.blocks.Items[BlockNo+Index].ClassNameIs('TKMemoParagraph') then break;
    end;
    TrailOffset := Index;
    if TrailOffset > 0 then
  	    Trailing := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo+Index]).Numbering = pnuBullets)
    else Trailing := False;
    Result := (Leading or Under or Trailing);
    	    TRlog('IsNearBullet -----------------------------------');
        TRlog('      Result      =' + booltostr(Result, true));
        TRlog('      Leading     =' + booltostr(Leading, true));
        TRlog('      Under       =' + booltostr(Under, true));
        TRlog('      Trailing    =' + booltostr(Trailing, true));
        TRlog('      IsFirstChar =' + booltostr(IsFirstChar, true));
        TRlog('      NoBulletPara=' + booltostr(NoBulletPara, true));
        TRlog('      LeadOffset  =' + inttostr(LeadOffset));
        TRlog('      TrailOffset =' + inttostr(Trailoffset));
        TRlog('      BlockNo     =' + inttostr(BlockNo));

end;

{
procedure TFormNote.CancelBullet(const BlockNo : longint; const UnderBullet : boolean);
begin
    TRlog('Cancel this bullet');
    if UnderBullet then begin
            if Kmemo1.Blocks.Items[BlockNo].ClassNameis('TKMemoParagraph') then
                if TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering = pnuBullets then
                    SetBullet(TKMemoParagraph(kmemo1.blocks.Items[BlockNo]), False);
    end else
        if (BlockNo+1) < Kmemo1.Blocks.Count then
            if Kmemo1.Blocks.Items[BlockNo+1].ClassNameis('TKMemoParagraph') then begin
                if TKMemoParagraph(KMemo1.Blocks.Items[BlockNo+1]).Numbering = pnuBullets then
                    SetBullet(TKMemoParagraph(kmemo1.blocks.Items[BlockNo+1]), False);
            end;
end;
}

{	To behave like end users expect when pressing BackSpace we have to alter KMemo's way of thinking.

a	If the cursor is at the end of a Bullet Text, KMemo would remove the Bullet
    Marker, we stop that and remove the last character of the visible string.

b   If the cursor is at the begininng of a Bullet Text we must cancel the bullet (which is at the
    end of the Text) and not merge this line with one above. We know this is the case if the
    trailing paragraph marker is bullet AND we are the first char of the first block of the text.

c   If the cursor is on next line after a bullet, on a para marker that is not a bullet and there
	is no text on that line after the cursor, all we do is delete that para marker.

d   Again, we are on first char of the line after a bullet, this line is not a bullet itself
	and it has some text after the cursor. We merge that text up to the bullet line above,
    retaining its bulletness. So, mark trailing para bullet, delete leading.


x	A blank line, no bullet between two bullet lines. Use BS line should dissapear.
    That is, delete para under cursor, move cursor to end line above. This same as c

y   There is nothing after our bullet para marker. So, on an empty bulletline, user presses
	BS to cancel bullet but that cancels bullet and moves us up to next (bulleted) line.
    It has to, there is nowhere else to go. Verbose shows this as a case c ????

     	Lead Under Trail First OnPara(not bulleted)
    a     ?    T     ?    F        remove the last character of the visible string to left.
    b     ?    F     T    T    F   Cursor at start, cancel bullet, don't merge

    x     T    F     T    T    T   Just delete this para. if Trailing move cursor to end of line above.
    c     T    F     F    T    T   Just delete this para. if Trailing move cursor to end of line above.
    y     T    T     F    T    F   Like c but add a para and move down. Not happy .....
    d     T    F     F    T    F   mark trailing para as bullet, delete leading.
    e     T    T     T    T    F   must remove Bullet for para under cursor

    Special case where curser is at end of a bullet and there is no para beyond there ?
    So, its should act as (a) but did, once, act as (d) ?? Needs more testing ......
}

procedure TFormNote.KMemo1KeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  TrailOffset,
  BlockNo,              // Will hold block number cursor is under.
  LeadOffset  : longint;
  LeadingBullet,        // The para immediatly previous to cursor is a bullet
  UnderBullet,          // We are under a Para and its a Bullet
  TrailingBullet,       // We are under Text but the block behind us is a Bullet.
  FirstChar  : boolean; // Cursor is under the first character of a line of text.
  NoBulletPara : boolean = false;
begin
    if Processing then exit();                   // should we drop key on floor ????
    // don't let any ctrl char get through the kmemo on mac
    {$ifdef DARWIN}
    if [ssCtrl] = Shift then begin
        case Key of
             VK_1 : MenuSmallClick(Sender);
             VK_2 : MenuNormalClick(Sender);
             VK_3 : MenuLargeClick(Sender);
             VK_4 : MenuHugeClick(Sender);
        end;
        Key := 0;
        exit;
    end;
    if ([ssAlt, ssShift] = Shift) and ((Key = VK_RIGHT) or (Key = VK_LEFT)) then exit; // KMemo - extend selection one word left or right
    {$endif}
    {$ifndef DARWIN}
    // -------------- Shift -------------------
    if [ssShift] = shift then begin
        if (Key = VK_LEFT) or (Key = VK_RIGHT) then exit; // KMemo - extend selection one char left or right
    end;

    {$endif}
    // -------------- Control ------------------
    if {$ifdef Darwin}[ssMeta] = Shift {$else}[ssCtrl] = Shift{$endif} then begin
        case key of
            //VK_Q : MainForm.close();
            VK_1 : MenuSmallClick(Sender);
            VK_2 : MenuNormalClick(Sender);
            VK_3 : MenuLargeClick(Sender);
            VK_4 : MenuHugeClick(Sender);
            VK_B : MenuBoldClick(Sender);
            VK_I : MenuItalicClick(Sender);
            VK_S : MenuStrikeOutClick(Sender);
            VK_T : MenuFixedWidthClick(Sender);
            VK_H : MenuHighLightClick(Sender);
            VK_U : MenuUnderLineClick(Sender);
            //VK_F : MenuItemFindClick(self);
            VK_L : SpeedButtonLinkClick(Sender);
            VK_D : InsertDate();
            //VK_N : SearchForm.OpenNote();      // MainForm.MMNewNoteClick(self);    ok as long as notes dir set .....
            VK_E : InitiateCalc();
            VK_F4 : close;                      // close just this note, normal saving will take place
            VK_M : begin FormMarkDown.TheKMemo := KMemo1; FormMarkDown.Show; end;
            VK_X, VK_C, VK_V, VK_Y, VK_A, VK_HOME, VK_END, VK_UP, VK_DOWN, VK_LEFT, VK_RIGHT, VK_PRIOR, VK_NEXT, VK_RETURN, VK_INSERT :
                exit;    // so key is not set to 0 on the way out, KMemo will handle
        end;
        Key := 0;    // so we don't get a ctrl key character in the text
        exit();
    end;
    // ------------- Alt (or Option in Mac) ------------------
    if [ssAlt] = Shift then begin
        case key of
                {$ifdef DARWIN}
            VK_H  : begin MenuHighLightClick(Sender); Key := 0; end; {$endif}
            VK_RIGHT : begin BulletControl(False, True); Key := 0; end;
            VK_LEFT  : begin BulletControl(False, False); Key := 0; end;
        end;
        exit();
    end;
    //if KMemo1.ReadOnly then begin Key := 0; exit(); end;
    // ------------------ Control and Shift ----------------
    if [ssCtrl, ssShift] = Shift then begin
       if key = ord('F') then begin SpeedButtonSearchClick(self); Key := 0; exit(); end;
       {$ifndef DARWIN}
       if (key = VK_RIGHT) or (Key = VK_LEFT) then exit;{$endif}            // KMemo knows how to do this, select word ...
       Key := 0;
    end;
    if Key = VK_TAB then begin
      KMemo1.InsertChar(KMemo1.Blocks.RealSelStart, #09);
      Key := 0;
      exit;
    end;
    if Key <> 8 then exit();    // We are watching for a BS on a Bullet Marker
    // Mac users don't have a del key, they use a backspace key thats labled 'delete'. Sigh...
    if KMemo1.Blocks.RealSelEnd > KMemo1.Blocks.RealSelStart then exit();
    if not NearABulletPoint(LeadingBullet, UnderBullet, TrailingBullet, FirstChar, NoBulletPara,
    				BlockNo, TrailOffset, LeadOffset) then exit();
    if (not FirstChar) and (not UnderBullet) then exit();
    // We do have to act, don't pass key on.
    Key := 0;
    Processing := True;
    MarkDirty();

    // KMemo1.Blocks.LockUpdate;  Dont lock because we move the cursor down here.
    	if UnderBullet and (not FirstChar) then begin   // case a
            KMemo1.ExecuteCommand(ecDeleteLastChar);
            TRlog('Case a');
            Processing := false;
            exit();
        end;
        // anything remaining must have FirstChar
        if TrailingBullet and (not NoBulletPara) then begin	// case b
            TRlog('Case b or e');
            if UnderBullet then  						// case e
              	TrailOffset := 0;
            if kmemo1.blocks.Items[BlockNo+TrailOffset].ClassNameIs('TKMemoParagraph') then
                SetBullet(TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]), False)
            	// TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]).Numbering := pnuNone
            else TRlog('ERROR - this case b block should be a para');
            Processing := False;
            exit();
        end;
        // anything remaining is outside bullet list, looking in. Except if Trailing is set...
        if  kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then begin
            KMemo1.Blocks.Delete(BlockNo);		// delete this blank line.
            if TrailingBullet then begin
            	KMemo1.ExecuteCommand(ecUp);
            	KMemo1.ExecuteCommand(ecLineEnd);
                TRlog('Case x');
			end else begin
            	if UnderBullet then begin				// this test is wrong, real test is are we at end of text ?

                TRlog('Case y');
                    KMemo1.Blocks.AddParagraph();		// Maybe only need add that if at end of text, NearABulletPoint() could tell us ?
                    KMemo1.ExecuteCommand(ecDown);
                end else

                TRlog('Case c');
            end;
        end else begin				// merge the current line into bullet above.
            if kmemo1.blocks.Items[BlockNo+TrailOffset].ClassNameIs('TKMemoParagraph') then
                SetBullet(TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]), True)
            	// TKMemoParagraph(kmemo1.blocks.Items[BlockNo+TrailOffset]).Numbering := pnuBullets;
            else TRlog('ERROR - this case d block should be a para');
            if  kmemo1.blocks.Items[BlockNo-Leadoffset].ClassNameIs('TKMemoParagraph') then begin
            	KMemo1.Blocks.Delete(BlockNo-LeadOffset);

                TRlog('Case d');
        	end;
    	end;
    Processing := False;
    // most of the intevention paths through this method take ~180mS on medium powered linux laptop
end;

procedure TFormNote.SetBullet(PB : TKMemoParagraph; Bullet : boolean);
var
  Index : integer;
  //Tick, Tock : qword;
begin
    // Note - do not play with the NumberingListLevel  thingos unless a bullet is set.
    // =========== WARNING - very ugly code that needs fixing =======================
    // I find that when you reset the NumberListLevel for one block, it changes every
    // block in the document !  ????
    // So, until  make sense of that, I'll scan over the whole document and set any
    // existing bullets to the proper indent.  On a large doc with lots of bullets,
    // this seems to take about 2mS on lower end laptop.
    KMemo1.Blocks.lockUpdate;
    try
        if Bullet then begin
            PB.Numbering:=pnuBullets;
            PB.NumberingListLevel.FirstIndent:=-20;
            PB.NumberingListLevel.LeftIndent:=30;
        end else begin
            if PB.Numbering <> pnuBullets then begin
                TRlog('ERROR - changing indent before Bullet set');
                exit();
            end;
            PB.NumberingListLevel.FirstIndent:=0;
            PB.NumberingListLevel.LeftIndent:=0;
            PB.Numbering:=pnuNone;
            //Tick := gettickcount64();
            for Index := 0 to KMemo1.Blocks.Count-1 do
                if KMemo1.Blocks.Items[Index].ClassNameIs('TKMemoParagraph') then
                    if TKMemoParagraph(KMemo1.Blocks.Items[Index]).Numbering = pnuBullets then begin
                        TKMemoParagraph(KMemo1.Blocks.Items[Index]).NumberingListLevel.FirstIndent:=-20;
                        TKMemoParagraph(KMemo1.Blocks.Items[Index]).NumberingListLevel.LeftIndent:=30;
                    end;
            //Tock := gettickcount64();
            // showmessage('Scan to reset bullet indent '+ inttostr(Tock-Tick) + 'mS');
        end;
    finally
        KMemo1.Blocks.UnlockUpdate;
    end;
end;


end.
