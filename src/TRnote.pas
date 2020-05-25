unit TRnote;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, { FileUtil,} Forms, Controls, Graphics, Dialogs, ExtCtrls,
    Menus, StdCtrls, Buttons, kmemo, LazLogger, PrintersDlgs,
    clipbrd, lcltype,      // required up here for copy on selection stuff.
    fpexprpars, LazUTF8,        // for calc stuff ;
    keditcommon,        // Holds some editing defines
    LazFileUtils,		// For ExtractFileName()
    math,
    FileUtil, strutils,         // just for ExtractSimplePath ... ~#1620
    LCLIntf,

    TRcommon, TrTexts;


type TFontRange = (FontHuge, FontLarge, FontNormal, FontSmall, FontTitle);

type TTagType = ( TagNone, TagBold, TagItalic, TagHighLight, TagUnderline,
              TagStrikeout, TagMonospace, TagSizeSmall, TagSizeLarge,
              TagSizeHuge, TagList, TagLinkInternal, TagLinkUrl);

type TNoteMenuTags = (ntCommit, ntSync, ntFind, ntSearchAll, ntSettings,
      ntAbout,ntDuplicate, ntDelete, ntMarkdown, ntRTF, ntPlain, ntPrint,
      ntRedo, ntUndo, ntCopy, ntCut, ntPaste, ntLink, ntURL, ntBold, ntItalic,
      ntStrike, ntUnderlined, ntFixed, ntHighlight, ntFontPLus, ntFontMinus,
      ntBullet, ntNoNotebook, ntNewNotebook,
      ntSpelling, ntSelectAll, ntInsertDate    );

type TNoteAction = ( ChangeSize, ToggleBold, ToggleItalic, ToggleHighlight, ToggleFont, ToggleStrikeout, ToggleUnderline);


type

{ TFormNote }

 TFormNote = class(TForm)

    ButtonFindPrev: TButton;
    ButtonFindNext: TButton;
    CheckboxFindInNote: TCheckBox;
    EditFindInNote: TEdit;

    KMemo1: TKMemo;
    MainMenu: TMainMenu;
    FileMenu, EditMenu, FormatMenu, ToolsMenu, NotebooksMenu : TMenuItem;
    PopMenu: TPopupMenu;

    Panel1: TPanel;
    Panel2: TPanel;
    PrintDialog1: TPrintDialog;

    procedure MainMenuClicked(Sender : TObject);
    procedure ToggleNotebook(Sender : TObject);

    procedure ButtonFindNextClick(Sender: TObject);
    procedure ButtonFindPrevClick(Sender: TObject);
    procedure CheckboxFindInNoteChange(Sender: TObject);
    procedure EditFindInNoteChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormDestroy(Sender: TObject);

    procedure onMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure onMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure onKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure onKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure onChange(Sender: TObject);

    procedure FormShow(Sender: TObject);

    // Links inside note
    procedure ExternalLink(sender : TObject);
    procedure InternalLink(sender : TObject);

private
    AlreadyLoaded : boolean;
    Dirty : boolean;

    HouseKeeper : TTimer;

    FontSizeNormal, FontSizeLarge, FontSizeTitle, FontSizeHuge, FontSizeSmall : integer;
    procedure SetFontSizes();

    procedure NoteToMemo();
    procedure MemoToNote();
    procedure MarkTitle(force : boolean);
    procedure MarkDirty();
    procedure MarkClean();
    procedure Commit();

    procedure ShowSearchPanel(s : boolean);
    procedure BuildMenus(Sender: TObject);

    function isInBullet() : boolean;
    function isBold() : boolean;
    function isItalic() : boolean;
    function isUnderlined() : boolean;
    function isStriked() : boolean;
    function isHighlight() : boolean;
    function isFixedFont() : boolean;

    procedure ToggleBullet();

    procedure InsertDate();

    function exportRTF() : boolean;
    function exportTXT() : boolean;
    function exportMarkDown() : boolean;

    procedure NotePrint();

    procedure SpellSuggest(word : String; suggestions : TStrings);
    procedure ReplaceSel(s : String);

    { Take a piece of text into KMemo block recursively }
    procedure TextToMemo(s : String; Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, newpar, linkinternal, linkexternal : boolean; FontSize : TFontRange; level : integer);

    { Alters the font etc of selected area as indicated }
    procedure AlterFont(const Command : TNoteAction; const param: integer = 0);
    { Alters the Font of Block as indicated }
    procedure AlterBlockFont(const FirstBlockNo, BlockNo: longint; const Command: TNoteAction; const param: integer=0);

    procedure DoHouseKeeping(Sender: TObject);

        function ColumnCalculate(out AStr: string): boolean;
        function ComplexCalculate(out AStr: string): boolean;
        procedure ExprTan(var Result: TFPExpressionResult;
            const Args: TExprParameterArray);

        function FindNumbersInString(const AStr: string; out AtStart, AtEnd: string
            ): boolean;
        function ParagraphTextTrunc(): string;
        function RelativePos(const Term: ANSIString; const MText: PChar;
            StartAt: integer): integer;
        function PreviousParagraphText(const Backby: integer): string;
        function SimpleCalculate(out AStr: string): boolean;

		procedure ClearLinks(const StartScan : longint =0; EndScan : longint = 0);
        { Looks around current block looking for link blocks. If invalid, 'unlinks' them.
          Http or local links, we need to clear the colour and underline of any text nearby
          that have been 'smeared' from user editing at the end of a link. When this happens,
          new text appears within the link block, bad .....  }
        procedure ClearNearLink(const StartS, EndS: integer);
        function DoCalculate(CalcStr: string): string;

        procedure CheckForLinks(const StartScan : longint = 1; EndScan : longint = 0);


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
	function NearABulletPoint(out Leading, Under, Trailing, IsFirstChar, NoBulletPara: Boolean; out BlockNo, TrailOffset, LeadOffset: longint): boolean;
        { Responds when user clicks on a hyperlink }
		procedure OnUserClickLink(sender: TObject);
        // A method called by this or other apps to get what we might have selected
        procedure PrimaryCopy(const RequestedFormatID: TClipboardFormat;
            Data: TStream);
        // Pastes into KMemo whatever is returned by the PrimarySelection system.
        //procedure PrimaryPaste(SelIndex: integer);

        // Advises other apps we can do middle button paste
        procedure SetPrimarySelection;
        // Cancels any indication we can do middle button paste cos nothing is selected
        procedure UnsetPrimarySelection;

    public
          note : PNoteInfo;



    end;

type PNoteEditForm = ^TFormNote;

implementation

{$R *.lfm}

{ TFormNote }
uses
    TRabout,TRmain, TRprint, TRhunspell;

const
        LinkScanRange = 100;	// when the user changes a Note, we search +/- around
     							// this value for any links that need adjusting.


procedure TFormNote.SetFontSizes();
begin
    FontSizeNormal   := round(12.0*FontScale/100.0);

    FontSizeLarge    := Max(round(14.0*FontScale/100.0),FontSizeNormal+1);
    FontSizeTitle    := Max(round(16.0*FontScale/100.0),FontSizeLarge+1);
    FontSizeHuge     := Max(round(18.0*FontScale/100.0),FontSizeTitle+1);
    FontSizeSmall    := Min(round(10.0*FontScale/100.0),FontSizeNormal-1);
end;

procedure TFormNote.ExternalLink(sender : TObject);
var
   u : String;
begin
   u := TKMemoHyperlink(Sender).Text;
   showmessage('External Link ' + u);
   OpenUrl(u);
end;

procedure TFormNote.InternalLink(sender : TObject);
var
   u : String;
begin
   u := TKMemoHyperlink(Sender).Text;
   showmessage('Internal Link ' + u);
   TFormMain(mainWindow).OpenUrlNoteByTitle(u);
end;

function TFormNote.ExportMarkDown() : boolean;
var
   i,j : integer;
   Block : TKMemoBlock;
   FT : TKMemoTextBlock;
   s2,partext : String;
   tfs : TStringList;
   dd : TSelectDirectoryDialog;
   filename : String;
 begin
   TRlog('exportMarkDown');

   dd := TSelectDirectoryDialog.Create(Self);
   if dd.Execute then begin FreeAndNil(dd); exit(false); end;

   filename := AppendPathDelim(ChompPathDelim(dd.FileName)) + note^.ID + '.md';
   TRlog('exportMarkdown to ' + filename);
   FreeAndNil(dd);

   MarkTitle(false);

   i:=2;

   tfs := TStringList.Create;
   Block := KMemo1.Blocks.Items[1];
   tfs.Add(Trim(Block.Text));
   tfs.Add('');

   while(i<KMemo1.Blocks.Count) do
   begin
      Block := KMemo1.Blocks.Items[i];

      // Search paragraph interval : [i;j[
      if Block.ClassNameIs('TKMemoParagraph')
          then j:=i+1
          else j:=i;

      while( (j<KMemo1.Blocks.Count) and (not KMemo1.Blocks.Items[j].ClassNameIs('TKMemoParagraph')) )
         do inc(j);

      partext:='';

      while(i<j) do
      begin
         if(KMemo1.Blocks.Items[i].ClassNameIs('TKMemoTextBlock') or KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink'))
         then begin
              FT := TKMemoTextBlock(KMemo1.Blocks.Items[i]);
              s2 := EncodeAngles(FT.Text);
         end else begin
            inc(i);
            continue;
         end;

         if(fsBold in FT.TextStyle.Font.Style) then s2:= '**'+s2+'**';
         if(fsItalic in FT.TextStyle.Font.Style) then s2:= '_'+s2+'_';
         if(fsUnderline in FT.TextStyle.Font.Style) then s2:= '++'+s2+'++';
         if(fsStrikeout in FT.TextStyle.Font.Style) then s2:= '~~'+s2+'~~';
         if(CompareText(FT.TextStyle.Font.Name,FixedFont)=0) then s2:= '`'+s2+'`';
         if(FT.TextStyle.Brush.Color = HiColour) then s2:='=='+s2+'==';
         if(FT.TextStyle.Font.Size = FontSizeLarge) then s2:='<size:large>'+s2+'</size:large>';
         if(FT.TextStyle.Font.Size = FontSizeHuge) then s2:='<size:huge>'+s2+'</size:huge>';
         if(FT.TextStyle.Font.Size = FontSizeSmall) then s2:='<sub>'+s2+'</sub>';

         partext := partext + s2;
         inc(i);
      end;
      tfs.Add(partext);
   end;

   tfs.LineBreak := rsLineBreak;

   try
      tfs.SaveToFile(filename);
   except on E:Exception do begin TRlog(E.message); ShowMessage(E.message); exit(false); end;
   end;

   Result := true;
end;


function TFormNote.exportRTF(): boolean;
var
   dd : TSelectDirectoryDialog;
   filename : String;
begin
   TRlog('exportRFT');

   dd := TSelectDirectoryDialog.Create(Self);
   if dd.Execute then
   begin
     try
        filename := AppendPathDelim(ChompPathDelim(dd.FileName)) + note^.ID + '.rtf';
        TRlog('exportRFT to ' + filename);
        KMemo1.SaveToRTF(filename);
     except on E:Exception do begin TRlog(E.message); ShowMessage(E.message); exit(false); end;
     end;
   end;
   FreeAndNil(dd);
   Result := true;
end;

function TFormNote.exportTXT(): boolean;
var
   dd : TSelectDirectoryDialog;
   filename : String;
begin
   TRlog('exportTXT');

   dd := TSelectDirectoryDialog.Create(Self);
   if dd.Execute then
   begin
     try
        filename := AppendPathDelim(ChompPathDelim(dd.FileName)) + note^.ID + '.txt';
        TRlog('exportTXT to ' + filename);
        KMemo1.SaveToTXT(filename);
     except on E:Exception do begin TRlog(E.message); ShowMessage(E.message); exit(false); end;
        end;
   end;
   FreeAndNil(dd);
   Result := true;
end;

procedure TFormNote.ReplaceSel(s : String);
var
  i,j,k : integer;
begin
  j := KMemo1.RealSelLength;
  i:=0;
  k := KMemo1.RealSelStart;

  while i < j do
  begin
  	KMemo1.Blocks.DeleteChar(k);
  	inc(i);
  end;
  KMemo1.Blocks.InsertPlainText(k,s);
end;

procedure TFormNote.SpellSuggest(word : String; suggestions : TStrings);
var
    spell : THunspell;
begin
   TRlog('TFormNote.SpellSuggest');

   suggestions.Clear;

   if(length(DictLibrary)=0) then begin suggestions.Add('Spelling libray not setup (found : "'+DictLibrary+'")'); exit(); end;
   if(length(DictFile)=0) then begin suggestions.Add('Spelling dictionnary file incorrect (found : "'+DictFile+'")'); exit(); end;
   spell :=  THunspell.Create(DictLibrary);
   if (Spell.ErrorMessage <> '') then begin suggestions.Add(Spell.ErrorMessage); exit(); end;
   if not Spell.SetDictionary(DictFile) then begin suggestions.Add(Spell.ErrorMessage); exit(); end;

   if not Spell.Spell(word) then begin suggestions.Add('Word "'+word+'" can not be processed'); exit(); end;

   spell.Suggest(word, suggestions);
   suggestions.Add(word);
end;

procedure TFormNote.TextToMemo(s : String; Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, newpar, linkinternal, linkexternal : boolean; FontSize : TFontRange; level : integer);
var
    i,j,k,m : integer;
    Ktext,tagtext,sub,chr : String;
    tagtype : TTagType;
    par : TKMemoParagraph;
    ktb : TKMemoTextBlock;
    f : TFont;
    hl : TKMemoHyperlink;
    ch : Char;
begin
   i:=1; j:= length(s);

   Ktext := '';

   TRlog('TFormNote.TextToMemo (j='+IntToStr(j)+') LEVEL='+IntToStr(Level));

   while (i<=j) do
   begin
      chr := Copy(s,i,1);
      ch := chr.Chars[0];
      if Ch = #13 then begin inc(i); continue; end; // Micro$ bug
      if Ch = #9 then begin Ch := ' ';  Chr := '   '; end; // Tabs

      tagtext :='';
      tagtype := TTagType.TagNone;

      if ((Ch >= ' ') and (Ch <> '<') ) then
      begin
          //TRlog('Adding CHR = "'+Chr+'" ; Ch = "'+Ch+'")');
          Ktext := Ktext + Chr;
          inc(i);
      end;

      if((length(Ktext)>0) and ((Ch < ' ') or (Ch = '<') or (i>j))) then
      begin
         //Trlog('Pushing block (current blocks ='+IntToStr(KMemo1.Blocks.Count)+')');
         Ktext := ReplaceAngles(Ktext); // We have to scan InStr for &lt; and &gt;  being < and >

         f := TFont.Create();
         f.Style := [];
         if Bold then f.Style := f.Style + [fsBold];
         if Italic then f.Style := f.Style + [fsItalic];
         if Underline then f.Style := f.Style + [fsUnderline];
         if Strikeout then f.Style := f.Style + [fsStrikeout];
         if FixedWidth then f.Name := FixedFont else f.Name := UsualFont;
         if FixedWidth then f.Pitch := fpFixed else f.Pitch := fpVariable;
         f.Color := TextColour;

         case FontSize of
             TFontRange.FontSmall : f.Size:= FontSizeSmall;
             TFontRange.FontLarge : f.Size:= FontSizeLarge;
             TFontRange.FontHuge : f.Size:= FontSizeHuge;
             else f.Size:= Self.FontSizeNormal;
         end;


         if(linkinternal) then
         begin
            //Trlog('Internal link on block '+ IntToStr(KMemo1.Blocks.Count-1)+' : ' + Ktext);
            hl := TKMemoHyperlink.Create;
            hl.Text := KText;
            KMemo1.Blocks.AddHyperlink(hl);
            hl.URL := 'note://'+KText;
            hl.OnClick := @InternalLink;
            f.Color := clBlue;
            f.Style := f.Style + [fsUnderline];
            hl.TextStyle.Font := f;
            if HighLight then hl.TextStyle.Brush.Color := HiColour;
         end
         else if(linkexternal) then
         begin
            //Trlog('External link on block '+ IntToStr(KMemo1.Blocks.Count-1)+' : ' + Ktext);
            hl := TKMemoHyperlink.Create;
            hl.Text := KText;
            KMemo1.Blocks.AddHyperlink(hl);
            hl.URL := KText;
            hl.OnClick := @ExternalLink;
            f.Color := clBlue;
            f.Style := f.Style + [fsUnderline];
            hl.TextStyle.Font := f;
            if HighLight then hl.TextStyle.Brush.Color := HiColour;
         end
         else begin
            ktb := KMemo1.Blocks.AddTextBlock(KText);
            ktb.TextStyle.Font := f;
            if HighLight then ktb.TextStyle.Brush.Color := HiColour;
         end;

         //Trlog('Pushing block (after blocks ='+IntToStr(KMemo1.Blocks.Count)+')');

         f.Free;


         Ktext := '';
      end;

      if (Ch<' ') then // add Paragraph
      begin
         par := KMemo1.Blocks.AddParagraph;
         if InBullet then
         begin
            par.Numbering := pnuBullets;
            par.NumberingListLevel.FirstIndent := -20;    // Note, these numbers need match SettBullet() in editbox
            par.NumberingListLevel.LeftIndent := 30;
         end;
         inc(i);
      end;

      if (Ch = '<') then  // new tag
      begin
         tagtext:= LowerCase(Trim(Copy(s,i+1,20)));
         k:=Pos('>',tagtext); if(k<1) then k := length(tagtext)+1;
         tagtext := Copy(tagtext,0,k-1);
         k:=Pos(' ',tagtext); if(k<1) then k := length(tagtext)+1;
         tagtext := Copy(tagtext,0,k-1);

         if(CompareStr(tagtext,'bold')=0)                    then      tagtype := TTagType.TagBold
          else if(CompareStr(tagtext,'italic')=0)            then      tagtype := TTagType.TagItalic
          else if(CompareStr(tagtext,'highlight')=0)         then      tagtype := TTagType.TagHighLight
          else if(CompareStr(tagtext,'underline')=0)         then      tagtype := TTagType.TagUnderline
          else if(CompareStr(tagtext,'strikeout')=0)         then      tagtype := TTagType.TagStrikeout
          else if(CompareStr(tagtext,'monospace')=0)         then      tagtype := TTagType.TagMonospace
          else if(CompareStr(tagtext,'size:small')=0)        then      tagtype := TTagType.TagSizeSmall
          else if(CompareStr(tagtext,'size:large')=0)        then      tagtype := TTagType.TagSizeLarge
          else if(CompareStr(tagtext,'size:huge')=0)         then      tagtype := TTagType.TagSizeHuge
          else if(CompareStr(tagtext,'list-item')=0)         then      tagtype := TTagType.TagList
          else if(CompareStr(tagtext,'link:url')=0)          then      tagtype := TTagType.TagLinkUrl
          else if(CompareStr(tagtext,'link:internal')=0)     then      tagtype := TTagType.TagLinkInternal;

         sub := Copy(s,i+1);
         k:= Pos('>', sub); if(k<1) then k := length(sub)+1;
         sub := LowerCase(Copy(s,i+k+1));
         m:= Pos('</'+tagtext,sub); if(m<1) then m:=length(sub)+1;
         sub:=Copy(s,i+k+1,m-1);

         case tagtype of
            TTagType.TagBold         : TextToMemo(sub, true, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, FontSize, level+1);
            TTagType.TagItalic       : TextToMemo(sub, Bold, true,   HighLight, Underline, Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, FontSize, level+1);
            TTagType.TagHighlight    : TextToMemo(sub, Bold, Italic, true,      Underline, Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, FontSize, level+1);
            TTagType.TagUnderline    : TextToMemo(sub, Bold, Italic, HighLight, true,      Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, FontSize, level+1);
            TTagType.TagStrikeout    : TextToMemo(sub, Bold, Italic, HighLight, Underline, true,      FixedWidth, InBullet, false, linkinternal, linkexternal, FontSize, level+1);
            TTagType.TagMonospace    : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, true,       InBullet, false, linkinternal, linkexternal, FontSize, level+1);
            TTagType.TagSizeSmall    : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, TFontRange.FontSmall, level+1);
            TTagType.TagSizeLarge    : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, TFontRange.FontLarge, level+1);
            TTagType.TagSizeHuge     : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, TFontRange.FontHuge, level+1);
            TTagType.TagList         : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, true,     true,  linkinternal, linkexternal, FontSize, level+1);
            TTagType.TagLinkInternal : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, true,         false,        FontSize, level+1);
            TTagType.TagLinkUrl      : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, false,        true,         FontSize, level+1);
            else TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, FontSize, level+1);
         end;

         i:=i+k+m;
         sub := Copy(s,i+1);
         k:= Pos('>', sub); if(k<1) then k := length(sub)+1;
         i:=i+k+1;
         TRlog('After tag : '+Copy(s,i,30)+' ...');
      end;

      //TRlog('New loop LEVEL='+IntToStr(level)+' i='+IntToStr(i)+' j='+IntToStr(j) );
   end;
   if(newpar) then begin
      //TRlog('NEWPAR');
      par := KMemo1.Blocks.AddParagraph;
      if InBullet then
      begin
         par.Numbering := pnuBullets;
         par.NumberingListLevel.FirstIndent := -20;    // Note, these numbers need match SettBullet() in editbox
         par.NumberingListLevel.LeftIndent := 30;
      end;
   end;
end;

procedure TFormNote.NoteToMemo();
begin

   Trlog('NoteToMemo');

   Dirty := False;

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
   KMemo1.Clear;

   TextToMemo(note^.Content, false, false, false, false, false, false, false, false,false,false,TFontRange.FontNormal,0);

   TRlog('Cursor position '+IntToStr(note^.CursorPosition));

   KMemo1.SelStart := note^.CursorPosition;
   KMemo1.SelLength := 0;

   TRlog('Dealing with content end');

   KMemo1.Blocks.UnlockUpdate;

   TRlog('Dealing with links');

   if (ShowIntLinks or ShowExtLinks) then CheckForLinks();

   AlreadyLoaded := true;

   TRlog('Done !');
end;

procedure TFormNote.MemoToNote();
var
   i,j : integer;
   Block : TKMemoBlock;
   FT : TKMemoTextBlock;
   s,s2,partext : String;
   lines : TStringList;
 begin

   lines := TStringList.Create;

   MarkTitle(false);

   lines.Add(note^.Title);
   lines.Add('');

   i:=2;
   s:='';

   while(i<KMemo1.Blocks.Count) do
   begin
      j:=i;
      // Seraching Paragraph
      while( (j<KMemo1.Blocks.Count) and (not KMemo1.Blocks.Items[j].ClassNameIs('TKMemoParagraph')) )
         do inc(j);

      partext:='';

      while(i<j) do
      begin
         if(KMemo1.Blocks.Items[i].ClassNameIs('TKMemoTextBlock'))
         then begin
              FT := TKMemoTextBlock(KMemo1.Blocks.Items[i]);
              s2 := EncodeAngles(FT.Text);
         end else
         if(KMemo1.Blocks.Items[i].ClassNameIs('TKMemoHyperlink'))
         then begin
              FT := TKMemoTextBlock(KMemo1.Blocks.Items[i]);
              if(isURL(FT.Text))
                  then s2 := '<link:url>'+EncodeAngles(FT.Text)+'</link:url>'
                  else s2 := '<link:internal>'+EncodeAngles(FT.Text)+'</link:internal>';
         end else begin
            inc(i);
            continue;
         end;

         if(fsBold in FT.TextStyle.Font.Style) then s2:= '<bold>'+s2+'</bold>';
         if(fsItalic in FT.TextStyle.Font.Style) then s2:= '<italic>'+s2+'</italic>';
         if(fsUnderline in FT.TextStyle.Font.Style) then s2:= '<underline>'+s2+'</underline>';
         if(fsStrikeout in FT.TextStyle.Font.Style) then s2:= '<strikeout>'+s2+'</strikeout>';
         if(CompareText(FT.TextStyle.Font.Name,FixedFont)=0) then s2:= '<monospace>'+s2+'</monospace>';
         if(FT.TextStyle.Brush.Color = HiColour) then s2:='<highlight>'+s2+'</highlight>';
         if(FT.TextStyle.Font.Size = FontSizeLarge) then s2:='<size:large>'+s2+'</size:large>';
         if(FT.TextStyle.Font.Size = FontSizeHuge) then s2:='<size:huge>'+s2+'</size:huge>';
         if(FT.TextStyle.Font.Size = FontSizeSmall) then s2:='<size:small>'+s2+'</size:small>';

         partext := partext + s2;
         inc(i);
      end;

      if((j<KMemo1.Blocks.Count) and (TKMemoParagraph(KMemo1.Blocks.Items[j]).Numbering = pnuBullets) )
             then lines.Add( '<list><list-item dir="ltr">' + partext + '</list-item dir="ltr"></list>')
             else lines.Add(partext);
      i:=j+1;
   end;

   lines.LineBreak := rsLineBreak;

   s := lines.Text;

   // Delete opposite tags
   s2:='';
   while(CompareStr(s,s2)<>0) do
   begin
      s2:=s;
      s := StringReplace(s,'</list><list>','',[rfReplaceAll]);
      s := StringReplace(s,'</size:small><size:small>','',[rfReplaceAll]);
      s := StringReplace(s,'</size:huge><size:huge>','',[rfReplaceAll]);
      s := StringReplace(s,'</size:large><size:large>','',[rfReplaceAll]);
      s := StringReplace(s,'</highlight><highlight>','',[rfReplaceAll]);
      s := StringReplace(s,'</monospace><monospace>','',[rfReplaceAll]);
      s := StringReplace(s,'</strikeout><strikeout>','',[rfReplaceAll]);
      s := StringReplace(s,'</underline><underline>','',[rfReplaceAll]);
      s := StringReplace(s,'</italic><italic>','',[rfReplaceAll]);
      s := StringReplace(s,'</bold><bold>','',[rfReplaceAll]);

      s := StringReplace(s,'</list>'+#10+'<list>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</size:small>'+#10+'<size:small>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</size:huge>'+#10+'<size:huge>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</size:large>'+#10+'<size:large>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</highlight>'+#10+'<highlight>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</monospace>'+#10+'<monospace>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</strikeout>'+#10+'<strikeout>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</underline>'+#10+'<underline>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</italic>'+#10+'<italic>',#10,[rfReplaceAll]);
      s := StringReplace(s,'</bold>'+#10+'<bold>',#10,[rfReplaceAll]);
   end;
   TRlog('NOTE ='+s);

   if(CompareStr(s, note^.Content) <> 0) then
   begin
     note^.Content := s;
     note^.LastChange:= GetCurrentTimeStr();
     note^.LastChangeGMT:= GetGMTFromStr(note^.LastChange);
   end;

   s := Trim(KMemo1.Blocks.Items[1].Text);
   if(CompareStr(s, note^.Title) <> 0) then
   begin
     note^.Title := s;
     note^.LastChange:= GetCurrentTimeStr();
     note^.LastChangeGMT:= GetGMTFromStr(note^.LastChange);
   end;

   if((Left <> note^.X) or (Top <> note^.Y) or (KMemo1.RealSelStart <> note^.SelectBoundPosition)
            or (not note^.OpenOnStartup) or (Height <> note^.Height) or (Width <> note^.Width))
   then begin
      note^.X := Left;
      note^.Y := Top;
      note^.SelectBoundPosition := KMemo1.SelStart;
      note^.OpenOnStartup := true;
      note^.Height := Height;
      note^.Width := Width;
      note^.LastMetaChange := GetCurrentTimeStr();
      note^.LastMetaChangeGMT := GetGMTFromStr(note^.LastMetaChange);
   end;

   MarkClean();
end;

procedure TFormNote.Commit();
var
   filename : String;
begin
  if(Dirty) then
  begin
    MemoToNote();
    filename := GetLocalNoteFile(note^.ID);

    NoteToFile(note,filename);
    TFormMain(mainWindow).ForceScan();
  end;
end;

function TFormNote.isBold() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(fsBold in TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style);
end;

function TFormNote.isItalic() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(fsItalic in TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style);
end;

function TFormNote.isUnderlined() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(fsUnderline in TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style);
end;

function TFormNote.isStriked() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(fsStrikeOut in TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]).TextStyle.Font.Style);
end;

function TFormNote.isHighlight() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]).TextStyle.Brush.Color = HiColour);
end;

function TFormNote.isFixedFont() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(CompareText(TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]).TextStyle.Font.Name,FixedFont) = 0);
end;

procedure TFormNote.ToggleBullet();
var
   BlockNo, LastBlock, PosInBlock,i : integer;
   state : boolean;
begin
  TRlog('TFormNote.ToggleBullet');

  BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
  LastBlock := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, PosInBlock);

  while( (LastBlock<KMemo1.Blocks.Count) and (not KMemo1.Blocks.Items[LastBlock].ClassNameIs('TKMemoParagraph'))) do inc(LastBlock);
  if(LastBlock = KMemo1.Blocks.Count) then Kmemo1.Blocks.AddParagraph();

  state := false;
  while((BlockNo<=LastBlock) and (not KMemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph'))) do inc(BlockNo);
  TRlog('yo22');
  state := (TKMemoParagraph(KMemo1.Blocks.Items[BlockNo]).Numbering <> pnuBullets) ;

  i:=BlockNo;

  while(i<=LastBlock) do
  begin
     TRlog('yoset '+IntToStr(i) + ' / '+IntToStr(LastBlock));

     if(KMemo1.Blocks.Items[i].ClassNameIs('TKMemoParagraph')) then
     begin
       if(state) then
       begin
          TrLog('set bullet');
          if(TKMemoParagraph(KMemo1.Blocks.Items[i]).Numbering <> pnuBullets) then
          begin
            TKMemoParagraph(KMemo1.Blocks.Items[i]).Numbering:=pnuBullets;
            TKMemoParagraph(KMemo1.Blocks.Items[i]).NumberingListLevel.FirstIndent:=-20;
            TKMemoParagraph(KMemo1.Blocks.Items[i]).NumberingListLevel.LeftIndent:=30;
          end;
       end
       else begin
          TrLog('rm bullet');
          if(TKMemoParagraph(KMemo1.Blocks.Items[i]).Numbering=pnuBullets) then
          begin
            TKMemoParagraph(KMemo1.Blocks.Items[i]).NumberingListLevel.FirstIndent:=0;
            TKMemoParagraph(KMemo1.Blocks.Items[i]).NumberingListLevel.LeftIndent:=0;
            TKMemoParagraph(KMemo1.Blocks.Items[i]).Numbering:=pnuNone;
          end;
       end;
     end;
     inc(i);
  end;

  for i :=0 to KMemo1.Blocks.count-1 do
  begin
       if(((i<BlockNo) or (i>LastBlock)) and (KMemo1.Blocks.Items[i].ClassNameIs('TKMemoParagraph')))
       then begin
         if(TKMemoParagraph(KMemo1.Blocks.Items[i]).Numbering=pnuBullets) then
          begin
            TKMemoParagraph(KMemo1.Blocks.Items[i]).NumberingListLevel.FirstIndent:=-20;
            if(TKMemoParagraph(KMemo1.Blocks.Items[i]).NumberingListLevel.LeftIndent<30) then TKMemoParagraph(KMemo1.Blocks.Items[i]).NumberingListLevel.LeftIndent := 30;
          end;
       end;
  end;
  TRlog('done');
end;

function TFormNote.isInBullet() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   while((BlockNo<KMemo1.blocks.count) and (not kmemo1.blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph')))
   do inc(BlockNo);
   if(BlockNo>=KMemo1.blocks.count) then exit(false);

   Result := (TKMemoParagraph(kmemo1.blocks.Items[BlockNo]).Numbering = pnuBullets);
end;


procedure TFormNote.onMouseDown(Sender: TObject; Button: TMouseButton;
		Shift: TShiftState; X, Y: Integer);
begin
   BuildMenus(Sender);
   if ((ssCtrl in Shift) or (Button = mbRight)) then PopMenu.PopUp;
end;


// ------------------  COPY ON SELECTION METHODS for LINUX and Windows ------


procedure TFormNote.onMouseUp(Sender: TObject; Button: TMouseButton;
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
      //PrimaryPaste(KMemo1.PointToIndex(Point, true, true, LinePos));
      exit();
    end;
    if KMemo1.SelAvail and
        (Kmemo1.Blocks.SelLength <> 0) then
            SetPrimarySelection()
        else
            UnsetPrimarySelection();
    {$endif}
    MarkTitle(false);
    BuildMenus(Sender);
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
    {
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
}

procedure TFormNote.InsertDate();
var
  I : integer;
begin
    KMemo1.ExecuteCommand(ecInsertString, pchar(FormatDateTime(' YYYY-MM-DD hh:mm:ss ', now())));
    for I := 0 to 20 do
        KMemo1.ExecuteCommand(ecRight);
end;

procedure TFormNote.AlterFont(const Command : TNoteAction; const param: integer = 0);
var
   FirstBlockNo, LastBlockNo, IntIndex, LastChar, FirstChar : longint;
   SplitStart : boolean = false;
begin
   TrLog('TFormNote.AlterFont');

   LastChar := Kmemo1.RealSelEnd;
   FirstChar := KMemo1.RealSelStart;

   FirstBlockNo := Kmemo1.Blocks.IndexToBlockIndex(FirstChar, IntIndex);
   if IntIndex <> 0 then SplitStart := True;

   LastBlockNo := Kmemo1.Blocks.IndexToBlockIndex(LastChar, IntIndex);
   if IntIndex <> (length(Kmemo1.Blocks.Items[LastBlockNo].Text)) then LastBlockNo := KMemo1.SplitAt(LastChar) - 1;

   while LastBlockNo > FirstBlockNo do
   begin
      AlterBlockFont(FirstBlockNo, LastBlockNo, Command, param);
      dec(LastBlockNo);
   end;

   if SplitStart then FirstBlockNo := KMemo1.SplitAt(FirstChar);
   AlterBlockFont(FirstBlockNo, FirstBlockNo, Command, param);

   KMemo1.SelEnd := LastChar;
   KMemo1.SelStart := FirstChar;

   MarkDirty();
end;

procedure TFormNote.AlterBlockFont(const FirstBlockNo, BlockNo : longint; const Command : TNoteAction; const param : integer = 0);
var
   Block, FirstBlock : TKMemoTextBlock;
begin
   FirstBlock := TKMemoTextBlock(KMemo1.Blocks.Items[FirstBlockNo]);
   Block := TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]);

   case Command of

      TNoteAction.ChangeSize : Block.TextStyle.Font.Size := param;

      TNoteAction.ToggleBold :
         if fsBold in FirstBlock.TextStyle.Font.style
            then Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsBold]
	    else Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsBold];

      TNoteAction.ToggleItalic :
         if fsItalic in FirstBlock.TextStyle.Font.style
            then Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsItalic]
	    else Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsItalic];

      TNoteAction.ToggleStrikeout :
         if fsStrikeout in FirstBlock.TextStyle.Font.style
            then Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsStrikeout]
            else Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsStrikeout];

      TNoteAction.ToggleUnderline :
         if fsUnderline in FirstBlock.TextStyle.Font.style
            then Block.TextStyle.Font.Style := Block.TextStyle.Font.Style - [fsUnderline]
            else Block.TextStyle.Font.Style := Block.TextStyle.Font.Style + [fsUnderline];

      TNoteAction.ToggleHighlight :
         if FirstBlock.TextStyle.Brush.Color <> HiColour
            then Block.TextStyle.Brush.Color := HiColour
            else Block.TextStyle.Brush.Color := BackGndColour;

      TNoteAction.ToggleFont :
         if FirstBlock.TextStyle.Font.Name <> FixedFont then
            begin
               Block.TextStyle.Font.Pitch := fpFixed;
               Block.TextStyle.Font.Name := FixedFont;
            end else
            begin
               Block.TextStyle.Font.Pitch := fpVariable;
	       Block.TextStyle.Font.Name := UsualFont;
            end;
   end;
end;

procedure TFormNote.MarkDirty();
begin
    Dirty := true;
    Caption := '* ' + note^.Title;
end;

procedure TFormNote.MarkClean();
begin
    Dirty := false;
    Caption := note^.Title;
end;

procedure TFormNote.FormShow(Sender: TObject);
begin
   TRlog('TFormNote.FormShow');

   if not AlreadyLoaded then
   begin
     NoteToMemo();
     MarkTitle(true);
   end;

   KMemo1.SelStart := KMemo1.Text.Length;  // set curser pos to end
   KMemo1.SelEnd := Kmemo1.Text.Length;

   KMemo1.SetFocus;

   KMemo1.Blocks.LockUpdate;
   {$ifdef windows}
    Color:= TextColour;
   {$endif}
   KMemo1.Colors.BkGnd:= BackGndColour;
   Kmemo1.Blocks.DefaultTextStyle.Font.Color := TextColour;
   KMemo1.Blocks.UnLockUpdate;

   ShowSearchPanel(false);

   BuildMenus(Sender);
end;

procedure TFormNote.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  TRlog('TFormNote.FormCloseQuery');
  HouseKeeper.Enabled := false;
  Commit();
  CanClose := True;
end;

procedure TFormNote.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TRlog('TFormNote.FormClose');
  note^.Display:=nil;

end;

procedure TFormNote.ShowSearchPanel(s : boolean);
begin
  if(s) then
  begin
     TRlog('Showing Search panel');
     Panel2.Visible:=true;
     //Panel1.AnchorSideBottom.Control := Panel2;
  end
  else
  begin
     TRlog('Hidding Search panel');
     //Panel1.AnchorSideBottom.Control := Self;
     Panel2.Visible:=false;
  end;
end;

procedure TFormNote.FormCreate(Sender: TObject);
var
    m1,m2 : TMenuItem;
begin
  TRlog('TFormNote.FormCreate');
  AlreadyLoaded := false;
  SetFontSizes();
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

  MainMenu.Items.Clear;
  MainMenu.Images := TFormMain(mainWindow).MenuIconList;

  // 'File'
  FileMenu := TMenuItem.Create(MainMenu);
  FileMenu.Caption := rsMenuNotes;
  MainMenu.Items.Add(FileMenu);
  // 'File' / Search All
  m1 := TMenuItem.Create(FileMenu);
  m1.Tag := ord(ntSearchAll);
  m1.Caption := rsTraySearchNote;
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=5;
  FileMenu.Add(m1);
  // 'File' / Search This
  m1 := TMenuItem.Create(FileMenu);
  m1.Tag := ord(ntFind);
  m1.Caption := rsMenuFind + ' (Ctrl-F)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=15;
  FileMenu.Add(m1);
  // 'File' / Duplicate
  FileMenu.AddSeparator;
  m1 := TMenuItem.Create(FileMenu);
  m1.Tag := ord(ntDuplicate);
  m1.Caption := rsMenuDuplicate;
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=13;
  FileMenu.Add(m1);
  // 'File' / Commit
  m1 := TMenuItem.Create(FileMenu);
  m1.Tag := ord(ntCommit);
  m1.Caption := rsMenuSave + ' (Ctrl-W)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=14;
  FileMenu.Add(m1);
  // 'File' / Delete
  FileMenu.AddSeparator;
  m1 := TMenuItem.Create(FileMenu);
  m1.Tag := ord(ntDelete);
  m1.Caption := rsMenuDelete;
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=11;
  FileMenu.Add(m1);

  // 'Edit'
  EditMenu := TMenuItem.Create(MainMenu);
  EditMenu.Caption := rsMenuEdit;
  MainMenu.Items.Add(EditMenu);

  // 'Format'
  FormatMenu := TMenuItem.Create(MainMenu);
  FormatMenu.Caption := rsMenuFormat;
  MainMenu.Items.Add(FormatMenu);

  // 'Notebooks'
  NotebooksMenu := TMenuItem.Create(MainMenu);
  NotebooksMenu.Caption := rsTrayNotebooks;
  MainMenu.Items.Add(NotebooksMenu);

  // 'Tools'
  ToolsMenu := TMenuItem.Create(MainMenu);
  ToolsMenu.Caption := rsMenuTools;
  MainMenu.Items.Add(ToolsMenu);

  // 'Tools' / Sync
  m1 := TMenuItem.Create(ToolsMenu);
  m1.Tag := ord(ntSync);
  m1.Caption := rsMenuSync;
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=10;
  ToolsMenu.Add(m1);

  // Tools / Export
  m1 := TMenuItem.Create(ToolsMenu);
  m1.Caption := rsMenuExport;
  m1.ImageIndex:=16;
  ToolsMenu.Add(m1);
  // Tools / Export / Markdown
  m2 := TMenuItem.Create(m1);
  m2.Tag := ord(ntMarkdown);
  m2.Caption := rsMenuExportMarkdown;
  m2.OnClick := @MainMenuClicked;
  m2.ImageIndex:=18;
  m1.Add(m2);
  // Tools / Export / RTF
  m2 := TMenuItem.Create(m1);
  m2.Tag := ord(ntRTF);
  m2.Caption := rsMenuExportRTF;
  m2.OnClick := @MainMenuClicked;
  m2.ImageIndex:=17;
  m1.Add(m2);
  // Tools / Export / Plain
  m2 := TMenuItem.Create(m1);
  m2.Tag := ord(ntPlain);
  m2.Caption := rsMenuExportPlain;
  m2.OnClick := @MainMenuClicked;
  m2.ImageIndex:=19;
  m1.Add(m2);

  // 'Tools' / Print
  m1 := TMenuItem.Create(ToolsMenu);
  m1.Tag := ord(ntPrint);
  m1.Caption := rsMenuPrint+' (Ctrl-P)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=20;
  ToolsMenu.Add(m1);


  // Tools / Settings
  ToolsMenu.AddSeparator;
  m1 := TMenuItem.Create(ToolsMenu);
  m1.Tag := ord(ntSettings);
  m1.Caption := rsMenuSettings + '(Ctrl-O)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=8;
  ToolsMenu.Add(m1);

  // 'Help'
  m1 := TMenuItem.Create(MainMenu);
  m1.Caption := rsMenuHelp;
  MainMenu.Items.Add(m1);

  // Help / About
  m2 := TMenuItem.Create(m1);
  m2.Caption := rsMenuAbout;
  m2.Tag := ord(ntAbout);
  m2.OnClick := @MainMenuClicked;
  m2.ImageIndex:=9;
  m1.Add(m2);

  // Housekeeping
  HouseKeeper := TTimer.Create(nil);
  HouseKeeper.OnTimer := @DoHouseKeeping;
  HouseKeeper.Interval := 60000;
  HouseKeeper.Enabled := True;

end;

procedure TFormNote.DoHouseKeeping(Sender: TObject);
begin
  HouseKeeper.Enabled := False;
  FreeAndNil(HouseKeeper);

  Commit();

  HouseKeeper := TTimer.Create(nil);
  HouseKeeper.OnTimer := @DoHouseKeeping;
  HouseKeeper.Interval := 60000;
  HouseKeeper.Enabled := True;
end;

procedure TFormNote.ButtonFindPrevClick(Sender: TObject);
var
   s, lo: String;
   i,j,k : integer;
begin
   s := LowerCase(Trim(EditFindInNote.Caption));
   lo := LowerCase( KMemo1.Text);

   if(Length(s)>0) then
   begin
     i :=0;
     k :=-1;
     while(i<KMemo1.RealSelStart) do
     begin
        k:= i;
        j:=Pos(s,Copy(lo,i+2));
        if(j>0) then
        begin
          i := i +j +1;
        end;
     end;
     if(k>=0) then
     begin
       KMemo1.SelStart:=k;
       KMemo1.SelEnd:=KMemo1.SelStart+Length(s);
     end ;
   end;
end;

procedure TFormNote.CheckboxFindInNoteChange(Sender: TObject);
begin
  if(not CheckboxFindInNote.Checked) then ShowSearchPanel(false);
end;

procedure TFormNote.EditFindInNoteChange(Sender: TObject);
begin
   ButtonFindNextClick(Sender)
end;

procedure TFormNote.ButtonFindNextClick(Sender: TObject);
var
    s,lo: String;
    i : integer;
begin
   s := LowerCase(Trim(EditFindInNote.Caption));
   lo := Lowercase(KMemo1.Text);
   if(Length(s)>0) then
   begin
     i := Pos(s,Copy(lo,KMemo1.RealSelStart+2));
     if(i>0) then
     begin
       KMemo1.SelStart:=KMemo1.RealSelStart + i+1;
       KMemo1.SelEnd:=KMemo1.SelStart+Length(s);
     end ;
   end;
end;

procedure TFormNote.FormDestroy(Sender: TObject);
{var
    ARec : TNoteUpdateRec; }
begin
  TrLog('TFormNote.FormDestroy');
  HouseKeeper.Enabled := False;
  FreeAndNil(HouseKeeper);
end;

procedure TFormNote.MarkTitle(force : boolean);
var
    BlockNo : integer;
    title : String;
    ktb : TKMemoTextBlock;
    needspace : boolean;
begin
   TRlog('MarkTitle FontSizeTitle='+IntToStr(FontSizeTitle));

   BlockNo :=0;
   title :='';

   while ((BlockNo < Kmemo1.Blocks.Count) and ((length(Trim(Title))=0) or (Kmemo1.Blocks.Items[BlockNo].ClassName <> 'TKMemoParagraph'))) do
   begin
      if Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock') then Title := Title + Kmemo1.Blocks.Items[BlockNo].Text;
      inc(BlockNo);
   end;

   TRlog('Found title : '+title + ' with '+IntToStr(BlockNo)+' blocks');

   KMemo1.Blocks.AddParagraph(BlockNo);
   KMemo1.Blocks.AddParagraph(BlockNo);

   if(force) then title := note^.Title;

   ktb := KMemo1.Blocks.AddTextBlock(title,BlockNo+1);

   ktb.TextStyle.Font.Name := UsualFont;
   ktb.TextStyle.Font.Size := FontSizeTitle;
   ktb.TextStyle.Font.Color := TitleColour;
   ktb.TextStyle.Font.Style := [fsUnderline];

   needspace := false;
   if(BlockNo +4< Kmemo1.Blocks.Count) then
   begin
      if (not Kmemo1.Blocks.Items[BlockNo+3].ClassNameIs('TKMemoParagraph')) then
      begin
         if (Kmemo1.Blocks.Items[BlockNo+3].ClassNameIs('TKMemoTextBlock') and (Length(Trim(Kmemo1.Blocks.Items[BlockNo+3].Text))>0)) then needspace := true;
      end;
   end;
   if(needspace) then KMemo1.Blocks.AddParagraph(BlockNo+2);

   //if Kmemo1.Blocks.Items[BlockNo].ClassNameIs('TKMemoTextBlock') then Title := Title + Kmemo1.Blocks.Items[BlockNo].Text;
   while(BlockNo>=0) do
   begin
       Kmemo1.Blocks.Delete(0);
       dec(BlockNo);
   end;

   BlockNo:=2;
   while ((BlockNo < Kmemo1.Blocks.Count)) do
   begin
      ktb := TKMemoTextBlock(KMemo1.Blocks.Items[BlockNo]);
      if ktb.ClassNameIs('TKMemoTextBlock') and (ktb.TextStyle.Font.Size = FontSizeTitle) then
      begin
           ktb.TextStyle.Font.Size := FontSizeNormal;
           ktb.TextStyle.Font.Color := TextColour;
           ktb.TextStyle.Font.Style := [];
      end;
      inc(BlockNo);
   end;

   while((Kmemo1.Blocks.Count>3) and Kmemo1.Blocks.Items[3].ClassNameIs('TKMemoParagraph'))
   do Kmemo1.Blocks.Delete(3);

   // Update title
   if(CompareStr(title, note^.Title) <>0) then
   begin
      TRlog('NOTE TITLE NOT EQUAL');
      note^.Title := Title;
      MarkDirty();
   end;

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

procedure TFormNote.CheckForLinks(const StartScan : longint =1; EndScan : longint = 0);
var
    Searchterm : ANSIstring;
    Len, httpLen : longint;
//    Tick, Tock : qword;
    pText : pchar;
begin
  {
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
    }
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
end;

procedure TFormNote.OnUserClickLink(sender : TObject);
begin
    if (copy(TKMemoHyperlink(Sender).Text, 1, 7) = 'http://') or
        (copy(TKMemoHyperlink(Sender).Text, 1, 8) = 'https://') then
            OpenUrl(TKMemoHyperlink(Sender).Text)
    else
	    //SearchForm.OpenNote(TKMemoHyperlink(Sender).Text);
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
procedure TFormNote.onChange(Sender: TObject);
begin
    //MarkTitle(false);
    MarkDirty();
    BuildMenus(Sender);
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

procedure TFormNote.NotePrint();
var
   p : TKPrn;
begin
   if PrintDialog1.Execute then
   begin
      p := TKPrn.Create;
      p.PrintKmemo(KMemo1);
      FreeandNil(p);
   end;
end;

procedure TFormNote.onKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  TRlog('TFormNote.KMemo1KeyDown '+IntToStr(Key));

  // CTRL
  if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then
  begin

     if key = ord('B') then begin TrLog('Ctrl-B'); AlterFont(ToggleBold); Key := 0; exit(); end;
     if key = ord('I') then begin TrLog('Ctrl-I'); AlterFont(ToggleItalic); Key := 0; exit(); end;
     if key = ord('S') then begin TrLog('Ctrl-S'); AlterFont(ToggleStrikeout); Key := 0; exit(); end;
     if key = ord('U') then begin TrLog('Ctrl-U'); AlterFont(ToggleUnderline); Key := 0; exit(); end;
     if key = ord('T') then begin TrLog('Ctrl-T'); AlterFont(ToggleFont); Key := 0; exit(); end;
     if key = ord('H') then begin TrLog('Ctrl-H'); AlterFont(ToggleHighlight); Key := 0; exit(); end;

     if key = ord('A') then begin TrLog('Ctrl-A'); KMemo1.ExecuteCommand(ecSelectAll); Key := 0; exit(); end;

     if key = ord('D') then begin TrLog('Ctrl-D'); InsertDate(); Key := 0; exit(); end;

     if key = ord('C') then begin TrLog('Ctrl-C'); KMemo1.ExecuteCommand(TKEditCommand.ecCopy); MarkDirty(); Key := 0; exit(); end;
     if key = ord('X') then begin TrLog('Ctrl-X'); KMemo1.ExecuteCommand(TKEditCommand.ecCut); MarkDirty(); Key := 0; exit(); end;
     if key = ord('V') then begin TrLog('Ctrl-V'); KMemo1.ExecuteCommand(TKEditCommand.ecPaste); MarkDirty(); Key := 0; exit(); end;

     if key = ord('W') then begin TrLog('Ctrl-W'); Commit(); Key := 0; exit(); end;

     if key = ord('O') then begin TrLog('Ctrl-O'); TFormMain(mainWIndow).ShowSettings(); Key := 0; exit(); end;

     if key = ord('P') then begin TrLog('Ctrl-P'); NotePrint(); Key :=0; exit(); end;

     if key = ord('Z') then begin TrLog('Ctrl-Z'); KMemo1.ExecuteCommand(TKEditCommand.ecUndo); Key := 0; exit(); end;
     if key = ord('Y') then begin TrLog('Ctrl-Y'); KMemo1.ExecuteCommand(TKEditCommand.ecRedo); Key := 0; exit(); end;

     if key = ord('F') then begin TrLog('Ctrl-F'); CheckboxFindInNote.Checked := true; ShowSearchPanel(true); EditFindInNote.SetFocus; Key :=0; exit(); end;


     exit();
   end;

  // SHIFT
  if ssShift in Shift then
  begin
    ///if key = VK_TAB then begin TrLog('ShiftTab'); IndentDecrease(); Key := 0; exit(); end;

    exit();
  end;

  // OTHER

  //if key = VK_TAB then begin TrLog('Tab'); IndentIncrease(); Key := 0; exit(); end;
end;

procedure TFormNote.onKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
     TRlog('TFormNote.KMemo1KeyUp '+IntToStr(Key));
  MarkTitle(false);
end;


{ ======= MAIN MENU ====== }

procedure TFormNote.ToggleNotebook(Sender : TObject);
begin

   TRlog('TFormNote.ToggleNotebook');


end;

procedure TFormNote.MainMenuClicked(Sender : TObject);
var
    FormAbout : TFormAbout;
    s : String;
begin

   TRlog('MainMenuClicked');

   case TNoteMenuTags(TMenuItem(Sender).Tag) of

      // FILE
      ntSearchAll : mainWindow.Show();

      ntFind : begin CheckboxFindInNote.Checked := true; ShowSearchPanel(true); EditFindInNote.SetFocus; end;

      //ntDuplicate :

      ntCommit :            Commit();

      //ntDelete :

      // EDIT
      ntRedo :              begin KMemo1.ExecuteCommand(TKEditCommand.ecRedo); MarkDirty(); end;
      ntUndo :              begin KMemo1.ExecuteCommand(TKEditCommand.ecUndo); MarkDirty(); end;
      ntSelectAll :         KMemo1.ExecuteCommand(ecSelectAll);
      ntCopy :              begin KMemo1.ExecuteCommand(TKEditCommand.ecCopy); MarkDirty(); end;
      ntCut :               begin KMemo1.ExecuteCommand(TKEditCommand.ecCut); MarkDirty(); end;
      ntPaste :             begin KMemo1.ExecuteCommand(TKEditCommand.ecPaste); MarkDirty(); end;
      ntInsertDate :        InsertDate();

      //ntLink :
      //ntURL :

      // SPELLING
      ntSpelling :          ReplaceSel(TMenuItem(Sender).Caption);

      // FORMAT
      ntBold :              AlterFont(ToggleBold);
      ntItalic :            AlterFont(ToggleItalic);
      ntStrike :            AlterFont(ToggleStrikeout);
      ntUnderlined :        AlterFont(ToggleUnderline);
      ntFixed :             AlterFont(ToggleFont);
      ntHighlight :         AlterFont(ToggleHighlight);

      //ntFontPLus :
      //ntFontMinus ;

      ntBullet :            ToggleBullet();

      // NOTEBOOK
      //ntNoNotebook :
      //ntNewNotebook :

      //TOOLS
      ntSync :              TFormMain(mainWIndow).SnapSync();

      ntMarkdown :          exportMarkdown();
      ntRTF :               exportRTF();
      ntPlain :             exportTXT();

      ntPrint :             NotePrint();

      ntSettings:           TFormMain(mainWIndow).ShowSettings();


      ntAbout :             TFormMain(mainWindow).ShowAbout();

   end;
end;

procedure TFormNote.BuildMenus(Sender: TObject);
var
    m1,m2 : TMenuItem;
    i : integer;
    s : TStringList;
begin
  TRlog('TFormNote.BuildMenus');

  PopMenu.Items.Clear;

  // Pop / SelectAll
  m1 := TMenuItem.Create(PopMenu);
  m1.Tag := ord(ntSelectAll);
  m1.Caption := rsMenuSelectAll+' (Ctrl-A)';
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=40;
  PopMenu.Items.Add(m1);

  // POP / Check spell
  m1 := TMenuItem.Create(PopMenu);
  m1.ImageIndex:=40;
  m1.Tag := ord(ntSpelling);
  PopMenu.Items.Add(m1);

  if(KMemo1.RealSelLength>0)
  then begin
    m1.Caption := rsCheckSel;
    s := TStringList.Create;
    SpellSuggest(KMemo1.SelText,s);
    i:=0;
    while(i<s.Count) do
    begin
         m2 := TMenuItem.Create(PopMenu);
         m2.Caption := s.Strings[i];
         m2.OnClick := @MainMenuClicked;
         m1.Add(m2);
         inc(i);
    end;
  end else begin
      m1.Caption := rsCheckSelNop ;
      m1.Enabled := false;
  end;

  // Pop / InsertDate
  m1 := TMenuItem.Create(PopMenu);
  m1.Tag := ord(ntInsertDate);
  m1.Caption := rsInsertDate +' (Ctrl-D)';
  m1.OnClick := @MainMenuClicked;
  PopMenu.Items.Add(m1);

  EditMenu.Clear;

  // Edit / Undo
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntUndo);
  m1.Caption := rsMenuUndo+' (Ctrl-Z)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=24;
  EditMenu.Add(m1);

  // Edit / Redo
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntRedo);
  m1.Caption := rsMenuRedo+' (Ctrl-Y)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=25;
  EditMenu.Add(m1);

  EditMenu.AddSeparator;

  // Edit / SelectAll
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntSelectAll);
  m1.Caption := rsMenuSelectAll+' (Ctrl-A)';
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=40;
  EditMenu.Add(m1);

  EditMenu.AddSeparator;

  // Edit / Cut
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntCut);
  m1.Caption := rsMenuCut+' (Ctrl-X)';
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=23;
  EditMenu.Add(m1);
  // Edit / Copy
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntCopy);
  m1.Caption := rsMenuCopy+' (Ctrl-C)';
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=21;
  EditMenu.Add(m1);
  // Edit / Paste
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntPaste);
  m1.Caption := rsMenuPaste+' (Ctrl-V)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=22;
  EditMenu.Add(m1);

  EditMenu.AddSeparator;

  // Edit / Link to Note
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntLink);
  m1.Caption := rsMenuLink;
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=26;
  EditMenu.Add(m1);
  // Edit / Link to URL
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntURL);
  m1.Caption := rsMenuURL;
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=27;
  EditMenu.Add(m1);

  FormatMenu.Clear;

  // Format / Bold
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntBold);
  m1.Caption := rsMenuBold+' (Ctrl-B)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isBold();
  m1.ImageIndex:=32;
  FormatMenu.Add(m1);
  // Format / Italic
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntItalic);
  m1.Caption := rsMenuItalic+' (Ctrl-I)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isItalic();
  m1.ImageIndex:=33;
  FormatMenu.Add(m1);
  // Format / Striekout
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntStrike);
  m1.Caption := rsMenuStrikeout +' (Ctrl-S)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isStriked();
  m1.ImageIndex:=29;
  FormatMenu.Add(m1);
  // Format / Underlined
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntUnderlined);
  m1.Caption := rsMenuUnderlined+' (Ctrl-U)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isUnderlined();
  m1.ImageIndex:=28;
  FormatMenu.Add(m1);
  // Format / Highlight
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntHighlight);
  m1.Caption := rsMenuHighlight+' (Ctrl-H)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isHighlight();
  m1.ImageIndex:=30;
  FormatMenu.Add(m1);
  // Format / FixedFont
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntFixed);
  m1.Caption := rsMenuFixed+' (Ctrl-T)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isFixedFont();
  m1.ImageIndex:=31;
  FormatMenu.Add(m1);

  FormatMenu.AddSeparator;

  // Format / Font+
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntFontPlus);
  m1.Caption := rsMenuFontPlus+' (Ctrl-+)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=35;
  FormatMenu.Add(m1);
  // Format / Font-
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntFontMinus);
  m1.Caption := rsMenuFontMinus+' (Ctrl--)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=34;
  FormatMenu.Add(m1);

  FormatMenu.AddSeparator;

  // Format / Bullet Enable/Disable
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntBullet);
  m1.Caption := rsMenuBullet;
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isInBullet();
  m1.ImageIndex:=38;
  FormatMenu.Add(m1);

  NotebooksMenu.Clear;

  // Notebooks / None
  m1 := TMenuItem.Create(NotebooksMenu);
  m1.Tag := ord(ntNoNotebook);
  m1.Caption := rsNoNotebook;
  m1.OnClick := @MainMenuClicked;
  m1.Checked := NoteBelongs('-',note);
  m1.ImageIndex:=39;
  NotebooksMenu.Add(m1);

  // Notebooks / List
  i:=0;
  while(i< TFormMain(mainWindow).NotebooksList.Count)
  do begin
     m1 := TMenuItem.Create(NotebooksMenu);
     m1.Tag := i;
     m1.Caption := TFormMain(mainWindow).NotebooksList.Strings[i];
     m1.OnClick := @ToggleNotebook;
     m1.Checked := NoteBelongs(TFormMain(mainWindow).NotebooksList.Strings[i],note);
     NotebooksMenu.Add(m1);
     inc(i);
  end;

  NotebooksMenu.AddSeparator;

  // Notebooks / New
  m1 := TMenuItem.Create(NotebooksMenu);
  m1.Tag := ord(ntNewNotebook);
  m1.Caption := rsMenuNewNotebook;
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=1;
  m1.Enabled:=true;
  NotebooksMenu.Add(m1);

end;

end.
