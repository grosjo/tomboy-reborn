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
              TagSizeHuge, TagList, TagListItem, TagLinkInternal, TagLinkUrl);

type TNoteMenuTags = (ntCommit, ntSync, ntFind, ntSearchAll, ntSettings,
      ntAbout,ntDuplicate, ntDelete, ntMarkdown, ntRTF, ntPlain, ntPrint,
      ntRedo, ntUndo, ntCopy, ntCut, ntPaste, ntNoteLink, ntURLLink,
      ntRemoveLink, ntBold, ntItalic, ntStrike, ntUnderlined, ntFixed,
      ntHighlight, ntFontPLus, ntFontMinus, ntBullet, ntNewNotebook,
      ntSpelling, ntSelectAll, ntInsertDate    );

type TNoteAction = ( IncreaseSize, DecreaseSize, ToggleBold, ToggleItalic,
     ToggleHighlight, ToggleFont, ToggleStrikeout, ToggleUnderline);


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

    procedure FormShow(Sender: TObject);

    procedure onMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure onKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure onMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure onKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure onChange(Sender: TObject);


private
    AlreadyLoaded, hasdata : boolean;
    Dirty : boolean;
    ProcessingChange, ProcessingTitle, isClosing, manualClosing : boolean;

    HouseKeeper, TitleFormatter, MenuBuilder : TTimer;

    FontSizeNormal, FontSizeLarge, FontSizeTitle, FontSizeHuge, FontSizeSmall : integer;

    findtext, oldtext : UTF8String;
    oldselstart, oldselend : Integer;

    procedure SetFontSizes();

    procedure PostFormatTitle();
    procedure PostBuildMenus();
    procedure ReceiveFormatTitle(Sender: TObject);
    function MarkTitle(force : boolean) : boolean;

    procedure NoteToMemo();
    procedure MemoToNote();
    procedure MarkDirty();
    procedure MarkClean();

    procedure Commit(Sender : TObject);

    procedure ShowSearchPanel(s : boolean);
    procedure BuildMenus(Sender: TObject);

    procedure FindNext(start : integer);
    procedure FindPrev(start : integer);

    function isInBullet() : boolean;
    function isBold() : boolean;
    function isItalic() : boolean;
    function isUnderlined() : boolean;
    function isStriked() : boolean;
    function isHighlight() : boolean;
    function isFixedFont() : boolean;

    procedure ToggleBullet(forcestatus : integer);

    procedure InsertDate();

    function exportRTF() : boolean;
    function exportTXT() : boolean;
    function exportMarkDown() : boolean;

    procedure NotePrint();

    procedure SpellSuggest(word : UTF8String; suggestions : TStrings);
    procedure ReplaceSel(s : UTF8String);

    procedure TextToMemo_addpar(Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet : boolean; FontSize : TFontRange ; addblank : boolean);
    procedure TextToMemo(s : UTF8String; Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, newpar, linkinternal, linkexternal : boolean; FontSize : TFontRange; level : integer);

    procedure AlterFont(const Command : TNoteAction);
    procedure AlterBlockFont(const FirstBlockNo, BlockNo: longint; const Command: TNoteAction);
    function IncFontSize(n : integer) : integer;
    function DecFontSize(n : integer) : integer;

    procedure PostCommit();


    // COPY/PASTE
    procedure PrimaryCopy(const RequestedFormatID: TClipboardFormat; Data: TStream);
    procedure PrimaryPaste();
    procedure SetPrimarySelection();
    procedure UnsetPrimarySelection();

    // LINKS
    procedure CreateLink(isweb : boolean);
    procedure RemoveLink();
    procedure CheckLinks();
    procedure ExternalLink(sender : TObject);
    procedure InternalLink(sender : TObject);

    public
          note : PNoteInfo;

    end;

type PNoteEditForm = ^TFormNote;

implementation

{$R *.lfm}

{ TFormNote }
uses
    TRmain, TRprint, TRhunspell;

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
   u : UTF8String;
begin
   u := Trim(TKMemoHyperlink(Sender).Text);
   TRlog('External Link ' + u);
   if(not isURL(u)) then u:='http://'+u;
   OpenUrl(u);
end;

procedure TFormNote.InternalLink(sender : TObject);
var
   u : UTF8String;
begin
   u := UTF8Trim(TKMemoHyperlink(Sender).Text);
   TRlog('Internal Link ' + u);
   TFormMain(mainWindow).OpenNoteByTitle(u);
end;

function TFormNote.ExportMarkDown() : boolean;
var
   i,j : integer;
   Block : TKMemoBlock;
   FT : TKMemoTextBlock;
   s2,partext : UTF8String;
   tfs : TStringList;
   dd : TSelectDirectoryDialog;
   filename : UTF8String;
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
   Block := KMemo1.Blocks[1];
   tfs.Add(Trim(Block.Text));
   tfs.Add('');

   while(i<KMemo1.Blocks.Count) do
   begin
      Block := KMemo1.Blocks[i];

      // Search paragraph interval : [i;j[
      if Block.ClassNameIs('TKMemoParagraph')
          then j:=i+1
          else j:=i;

      while( (j<KMemo1.Blocks.Count) and (not KMemo1.Blocks[j].ClassNameIs('TKMemoParagraph')) )
         do inc(j);

      partext:='';

      while(i<j) do
      begin
         if(KMemo1.Blocks[i].ClassNameIs('TKMemoTextBlock') or KMemo1.Blocks[i].ClassNameIs('TKMemoHyperlink'))
         then begin
              FT := TKMemoTextBlock(KMemo1.Blocks[i]);
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
   filename : UTF8String;
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
   filename : UTF8String;
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

procedure TFormNote.ReplaceSel(s : UTF8String);
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

procedure TFormNote.SpellSuggest(word : UTF8String; suggestions : TStrings);
var
    spell : THunspell;
begin
   TRlog('TFormNote.SpellSuggest');

   suggestions.Clear;

   if(length(DictLibrary)=0) then begin suggestions.Add(rsDictNotSetup +' (found : "'+DictLibrary+'")'); exit(); end;
   if(length(DictFile)=0) then begin suggestions.Add(rsDictNotFound + ' (found : "'+DictFile+'")'); exit(); end;
   spell :=  THunspell.Create(DictLibrary);
   if (Spell.ErrorMessage <> '') then begin suggestions.Add(Spell.ErrorMessage); exit(); end;
   if not Spell.SetDictionary(DictFile) then begin suggestions.Add(Spell.ErrorMessage); exit(); end;

   if not Spell.Spell(word) then begin suggestions.Add('"'+word+'" '+rsWordInvalid); exit(); end;

   spell.Suggest(word, suggestions);
   suggestions.Add(word);
end;

procedure TFormNote.TextToMemo_addpar(Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet : boolean; FontSize : TFontRange ; addblank : boolean );
var
    f : TFont;
    par : TKMemoParagraph;
    ktb : TKMemoTextBlock;
begin
   TRlog('Adding actual par');
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
        else f.Size:= FontSizeNormal;
   end;

   if(addblank)
   then begin
     ktb := KMemo1.Blocks.AddTextBlock('');
     ktb.TextStyle.Font := f;
   end;

   par := KMemo1.Blocks.AddParagraph;
   if InBullet then
   begin
      par.Numbering := pnuBullets;
      par.NumberingListLevel.FirstIndent := -20;
      par.NumberingListLevel.LeftIndent := 30;
   end;
   par.TextStyle.Font := f;

   f.Free;
end;

procedure TFormNote.TextToMemo(s : UTF8String; Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, newpar, linkinternal, linkexternal : boolean; FontSize : TFontRange; level : integer);
var
    i,j,k,m : integer;
    Ktext,tagtext,sub : UTF8String;
    tagtype : TTagType;
    par : TKMemoParagraph;
    ktb : TKMemoTextBlock;
    f : TFont;
    hl : TKMemoHyperlink;
    chr : AnsiString;
    needpar : boolean;
    ch : Char;
begin
   i:=1; j:= UTF8length(s);

   Ktext := '';
   needpar := false;

   TRlog('TFormNote.TextToMemo (j='+IntToStr(j)+') LEVEL='+IntToStr(Level));

   while (i<=j) do
   begin
      TRlog('i= '+IntToStr(i)+' j= '+IntToStr(j)+' LEVEL='+IntToStr(Level));
      chr := UTF8Copy(s,i,1);
      ch := chr.Chars[0];

      //TRlog('CHAR = '+chr+' ORD='+IntToStr(Ord(Ch))+' SUB= "'+UTF8Copy(s,i,10)+'"');

      if Ch = #13 then begin inc(i); continue; end; // Micro$ bug
      if Ch = #9 then begin Ch := ' ';  Chr := '   '; end; // Tabs

      tagtext :='';
      tagtype := TTagType.TagNone;

      if ((Ch >= ' ') and (Ch <> '<') ) then
      begin
          if(needpar)
          then begin
            TextToMemo_addpar(Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, FontSize, false);
            needpar := false;
          end;
          Ktext := Ktext + Chr;
          inc(i);
      end;

      if((UTF8Length(KText)>0) and ((Ch < ' ') or (Ch = '<') or (i>j))) then
      begin
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
             else f.Size:= FontSizeNormal;
         end;

         if(linkinternal) then
         begin
            //Trlog('Internal link on block '+ IntToStr(KMemo1.Blocks.Count-1)+' : ' + Ktext);
            hl := TKMemoHyperlink.Create;
            hl.Text := KText;
            KMemo1.Blocks.AddHyperlink(hl);
            hl.URL := KText;
            hl.OnDblClick := @InternalLink;
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
            hl.OnDblClick := @ExternalLink;
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

         hasdata := true;

         f.Free;

         Ktext := '';
      end;

      if (Ch<' ') then
      begin
        TRlog('NEEDPAR');

        if(needpar)
        then  TextToMemo_addpar(Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, FontSize,true);

        inc(i);
        needpar := true;
      end;

      if (Ch = '<') then  // new tag
      begin
         if(needpar) then TextToMemo_addpar(Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, FontSize, false);
         needpar := false;

         tagtext:= UTF8LowerCase(UTF8Trim(UTF8Copy(s,i+1,20)));
         k:=UTF8Pos('>',tagtext); if(k<1) then k := UTF8Length(tagtext)+1;
         tagtext := UTF8Trim(UTF8Copy(tagtext,1,k-1));
         k:=UTF8Pos(' ',tagtext); if(k<1) then k := UTF8Length(tagtext)+1;
         tagtext := UTF8Copy(tagtext,1,k-1);

         TRlog('NEW Starting TAG 4 <'+tagtext+'>');

         if(UTF8CompareStr(tagtext,'bold')=0)                    then      tagtype := TTagType.TagBold
          else if(UTF8CompareStr(tagtext,'italic')=0)            then      tagtype := TTagType.TagItalic
          else if(UTF8CompareStr(tagtext,'highlight')=0)         then      tagtype := TTagType.TagHighLight
          else if(UTF8CompareStr(tagtext,'underline')=0)         then      tagtype := TTagType.TagUnderline
          else if(UTF8CompareStr(tagtext,'strikeout')=0)         then      tagtype := TTagType.TagStrikeout
          else if(UTF8CompareStr(tagtext,'monospace')=0)         then      tagtype := TTagType.TagMonospace
          else if(UTF8CompareStr(tagtext,'size:small')=0)        then      tagtype := TTagType.TagSizeSmall
          else if(UTF8CompareStr(tagtext,'size:large')=0)        then      tagtype := TTagType.TagSizeLarge
          else if(UTF8CompareStr(tagtext,'size:huge')=0)         then      tagtype := TTagType.TagSizeHuge
          else if(UTF8CompareStr(tagtext,'list-item')=0)         then      tagtype := TTagType.TagListItem
          else if(UTF8CompareStr(tagtext,'list')=0)              then      tagtype := TTagType.TagList
          else if(UTF8CompareStr(tagtext,'link:url')=0)          then      tagtype := TTagType.TagLinkUrl
          else if(UTF8CompareStr(tagtext,'link:internal')=0)     then      tagtype := TTagType.TagLinkInternal;

         sub := UTF8Copy(s,i+1,j);
         k:= UTF8Pos('>', sub); if(k<1) then k := UTF8length(sub)+1;
         sub := UTF8LowerCase(UTF8Copy(s,i+k+1,j));
         m:= UTF8Pos('</'+tagtext,sub); if(m<1) then m:=UTF8length(sub)+1;
         sub := UTF8Copy(s,i+k+1,m-1);
         i:=i+k+m;

         //TRlog('TAG <'+tagtext+'>  SUB='+UTF8Copy(sub,1,30));

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
            TTagType.TagListItem     :
               begin
                                     while((KMemo1.Blocks.Count>0) and KMemo1.Blocks[KMemo1.Blocks.Count-1].ClassNameIs('TKMemoTextBlock') and (Length(UTF8Trim(TKMemoTextBlock(KMemo1.Blocks[KMemo1.Blocks.Count-1]).Text))=0))
                                     do KMemo1.Blocks.Delete(KMemo1.Blocks.Count-1);
                                     TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, true,     true,  linkinternal, linkexternal, FontSize, level+1);
               end;
            TTagType.TagList         :
               begin
                                     while((KMemo1.Blocks.Count>0) and KMemo1.Blocks[KMemo1.Blocks.Count-1].ClassNameIs('TKMemoTextBlock') and (Length(UTF8Trim(TKMemoTextBlock(KMemo1.Blocks[KMemo1.Blocks.Count-1]).Text))=0))
                                     do KMemo1.Blocks.Delete(KMemo1.Blocks.Count-1);
                                     TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, false,     false,  linkinternal, linkexternal, FontSize, level+1);
               end;
            TTagType.TagLinkInternal : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, true,         false,        FontSize, level+1);
            TTagType.TagLinkUrl      : TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, false,        true,         FontSize, level+1);
                                  else TextToMemo(sub, Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, false, linkinternal, linkexternal, FontSize, level+1);
         end;

         sub := UTF8Copy(s,i+1,j);
         TRlog('ENDTAG <'+tagtext+'>  SUB='+UTF8Copy(sub,1,30));

         k:= UTF8Pos('>', sub); if(k<1) then k := UTF8length(sub)+1;
         i:=i+k+1;

         if(i<=j)
         then begin
           chr := UTF8Copy(s,i,1);
           if (((tagtype = TTagType.TagListItem) or (tagtype = TTagType.TagList)) and (chr.Chars[0] < ' ')) then inc(i);
         end;
         TRlog('After tag : '+Copy(s,i,30)+' ...');
      end;

      //TRlog('New loop LEVEL='+IntToStr(level)+' i='+IntToStr(i)+' j='+IntToStr(j) );
   end;

   if(newpar or needpar)
   then TextToMemo_addpar(Bold, Italic, HighLight, Underline, Strikeout, FixedWidth, InBullet, FontSize,false);

end;

procedure TFormNote.NoteToMemo();
var
    i : integer;
begin

   Trlog('NoteToMemo');

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

   // DEFAULT TITLE
   Caption := note^.Title;


   // KMEMO
   TRlog('Dealing with content');
   KMemo1.Blocks.LockUpdate;
   KMemo1.Clear;

   hasdata := false;

   TextToMemo(note^.Content, false, false, false, false, false, false, false, false,false,false,TFontRange.FontNormal,0);

   if(hasdata) then KMemo1.Blocks.Delete(0);

   i:=0;
   while(i<KMemo1.Blocks.Count)
   do begin
      TRlog('BLOCK('+IntToStr(i)+') '+KMemo1.Blocks[i].ClassName+' (size='+IntToStr(TKMemoTextBlock(KMemo1.Blocks[i]).TextStyle.Font.Size)+' '+TKMemoTextBlock(KMemo1.Blocks[i]).TextStyle.Font.Name+') : "'+KMemo1.Blocks[i].Text+'"');
      inc(i);
   end;

   TRlog('Dealing with content end : blocks = '+IntToStr(Kmemo1.Blocks.Count));

   KMemo1.Blocks.UnlockUpdate;

   Dirty := False;

   TRlog('Cursor position '+IntToStr(note^.CursorPosition));

   KMemo1.SelStart := note^.CursorPosition;
   KMemo1.SelEnd := note^.CursorPosition;

   oldtext := KMemo1.Text;
   oldselstart := KMemo1.RealSelStart;
   oldselend := KMemo1.RealSelend;

   TRlog('NoteToMemo Done !');
end;

procedure TFormNote.MemoToNote();
var
   i,j : integer;
   FT : TKMemoTextBlock;
   FL : TKMemoHyperlink;
   s,s2,partext : UTF8String;
   lines : TStringList;
   changedtitle : boolean;
 begin

   lines := TStringList.Create;

   changedtitle := MarkTitle(false);

   lines.Add(note^.Title);
   lines.Add('');

   i:=4;
   s:='';

   while(i<KMemo1.Blocks.Count) do
   begin
      j:=i;
      // Searching Paragraph
      while( (j<KMemo1.Blocks.Count) and (not KMemo1.Blocks[j].ClassNameIs('TKMemoParagraph')) )
         do inc(j);

      partext:='';

      while(i<j) do
      begin
         if(KMemo1.Blocks[i].ClassNameIs('TKMemoTextBlock'))
         then begin
              FT := TKMemoTextBlock(KMemo1.Blocks[i]);
              s2 := EncodeAngles(FT.Text);
              if(fsUnderline in FT.TextStyle.Font.Style) then s2:= '<underline>'+s2+'</underline>';
         end
         else if(KMemo1.Blocks[i].ClassNameIs('TKMemoHyperlink'))
         then begin
              FL := TKMemoHyperlink(KMemo1.Blocks[i]);
              FT := TKMemoTextBlock(FL);
              s2 := EncodeAngles(FT.Text);
              if(FL.OnDblClick = @ExternalLink)
                  then s2 := '<link:url>'+s2+'</link:url>'
                  else s2 := '<link:internal>'+s2+'</link:internal>';
         end else
         begin
            inc(i);
            continue;
         end;

         if(fsBold in FT.TextStyle.Font.Style) then s2:= '<bold>'+s2+'</bold>';
         if(fsItalic in FT.TextStyle.Font.Style) then s2:= '<italic>'+s2+'</italic>';
         if(fsStrikeout in FT.TextStyle.Font.Style) then s2:= '<strikeout>'+s2+'</strikeout>';
         if(CompareText(FT.TextStyle.Font.Name,FixedFont)=0) then s2:= '<monospace>'+s2+'</monospace>';
         if(FT.TextStyle.Brush.Color = HiColour) then s2:='<highlight>'+s2+'</highlight>';
         if(FT.TextStyle.Font.Size = FontSizeLarge) then s2:='<size:large>'+s2+'</size:large>';
         if(FT.TextStyle.Font.Size = FontSizeHuge) then s2:='<size:huge>'+s2+'</size:huge>';
         if(FT.TextStyle.Font.Size = FontSizeSmall) then s2:='<size:small>'+s2+'</size:small>';

         partext := partext + s2;
         inc(i);
      end;

      if((j<KMemo1.Blocks.Count) and (TKMemoParagraph(KMemo1.Blocks[j]).Numbering = pnuBullets) )
         then partext := '<list><list-item dir="ltr">' + partext + '</list-item></list>';

      TRlog('MemotoNote : Adding line : "'+partext+'"');
      lines.Add(partext);

      i:=j+1;
   end;

   lines.LineBreak := rsLineBreak;

   i:=0;
   while(i<lines.Count)
      do begin
         TRlog('Line('+IntToStr(i)+') = "'+lines.Strings[i]+'"');
         inc(i);
      end;
   s := lines.Text;

   TRlog('=================');
   TRlog('NOTE v0 ="'+s+'"');

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
   TRlog('=================');
   TRlog('NOTE v1 ="'+s+'"');

   if(changedtitle or (CompareStr(s, note^.Content) <> 0)) then
   begin
     note^.Content := s;
     note^.LastChange:= GetCurrentTimeStr();
     note^.LastChangeGMT:= GetGMTFromStr(note^.LastChange);
   end;

   note^.X := Left;
   note^.Y := Top;
   note^.CursorPosition := KMemo1.RealSelStart;
   note^.SelectBoundPosition := KMemo1.RealSelEnd;
   note^.Height := Height;
   note^.Width := Width;
   note^.OpenOnStartup := not manualClosing;
   note^.LastMetaChange := GetCurrentTimeStr();
   note^.LastMetaChangeGMT := GetGMTFromStr(note^.LastMetaChange);

   MarkClean();
end;

procedure TFormNote.Commit(Sender : TObject);
var
   filename : UTF8String;
begin
  TRlog(' Commit');
  ProcessingChange := true;
  if(Assigned(HouseKeeper))
  then begin
     HouseKeeper.Enabled := False;
     FreeAndNil(HouseKeeper);
  end;

  if(Dirty or (KMemo1.RealSelStart<> note^.CursorPosition)
   or (KMemo1.RealSelEnd<> note^.SelectBoundPosition)
    or (Self.Left<> note^.X)
    or (Self.Top<> note^.Y)
    or (Self.Width<> note^.Width)
    or (Self.Height<> note^.Height)
    or (not manualClosing <> note^.OpenOnStartup)
  )
  then begin
    MemoToNote();
    filename := GetLocalNoteFile(note^.ID);

    TRlog(' Commit title='+note^.Title);

    NoteToFile(note,filename);
    TFormMain(mainWindow).PostScan();
  end;

  ProcessingChange := false;

  if(not isClosing) then
  begin
     HouseKeeper := TTimer.Create(nil);
     HouseKeeper.OnTimer := @Commit;
     HouseKeeper.Interval := 30000;
     HouseKeeper.Enabled := True;
  end;
end;

function TFormNote.isBold() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(fsBold in TKMemoTextBlock(KMemo1.Blocks[BlockNo]).TextStyle.Font.Style);
end;

function TFormNote.isItalic() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(fsItalic in TKMemoTextBlock(KMemo1.Blocks[BlockNo]).TextStyle.Font.Style);
end;

function TFormNote.isUnderlined() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(fsUnderline in TKMemoTextBlock(KMemo1.Blocks[BlockNo]).TextStyle.Font.Style);
end;

function TFormNote.isStriked() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(fsStrikeOut in TKMemoTextBlock(KMemo1.Blocks[BlockNo]).TextStyle.Font.Style);
end;

function TFormNote.isHighlight() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(TKMemoTextBlock(KMemo1.Blocks[BlockNo]).TextStyle.Brush.Color = HiColour);
end;

function TFormNote.isFixedFont() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   if(not KMemo1.Blocks[BlockNo].ClassNameIs('TKMemoTextBlock')) then exit(false);
   exit(CompareText(TKMemoTextBlock(KMemo1.Blocks[BlockNo]).TextStyle.Font.Name,FixedFont) = 0);
end;

procedure TFormNote.ToggleBullet(forcestatus : integer);
var
   BlockNo, LastBlock, PosInBlock,i : integer;
   state : boolean;
begin
  TRlog('TFormNote.ToggleBullet');

  BlockNo := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
  LastBlock := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, PosInBlock);

  while( (LastBlock<KMemo1.Blocks.Count) and (not KMemo1.Blocks[LastBlock].ClassNameIs('TKMemoParagraph'))) do inc(LastBlock);
  if(LastBlock = KMemo1.Blocks.Count) then Kmemo1.Blocks.AddParagraph();

  while((BlockNo<=LastBlock) and (not KMemo1.Blocks[BlockNo].ClassNameIs('TKMemoParagraph'))) do inc(BlockNo);

  case forcestatus of
     0 : state := (TKMemoParagraph(KMemo1.Blocks[BlockNo]).Numbering <> pnuBullets) ;
     -1 : state := false;
     1 : state := true;
  end;

  i:=BlockNo;

  while(i<=LastBlock) do
  begin
     if(KMemo1.Blocks[i].ClassNameIs('TKMemoParagraph')) then
     begin
       if(state) then
       begin
          TrLog('set bullet');
          if(TKMemoParagraph(KMemo1.Blocks[i]).Numbering <> pnuBullets) then
          begin
            TKMemoParagraph(KMemo1.Blocks[i]).Numbering:=pnuBullets;
            TKMemoParagraph(KMemo1.Blocks[i]).NumberingListLevel.FirstIndent:=-20;
            TKMemoParagraph(KMemo1.Blocks[i]).NumberingListLevel.LeftIndent:=30;
          end;
       end
       else begin
          TrLog('rm bullet');
          if(TKMemoParagraph(KMemo1.Blocks[i]).Numbering=pnuBullets) then
          begin
            TKMemoParagraph(KMemo1.Blocks[i]).NumberingListLevel.FirstIndent:=0;
            TKMemoParagraph(KMemo1.Blocks[i]).NumberingListLevel.LeftIndent:=0;
            TKMemoParagraph(KMemo1.Blocks[i]).Numbering:=pnuNone;
          end;
       end;
     end;
     inc(i);
  end;

  for i :=0 to KMemo1.Blocks.count-1 do
  begin
       if(((i<BlockNo) or (i>LastBlock)) and (KMemo1.Blocks[i].ClassNameIs('TKMemoParagraph')))
       then begin
         if(TKMemoParagraph(KMemo1.Blocks[i]).Numbering=pnuBullets) then
          begin
            TKMemoParagraph(KMemo1.Blocks[i]).NumberingListLevel.FirstIndent:=-20;
            if(TKMemoParagraph(KMemo1.Blocks[i]).NumberingListLevel.LeftIndent<30) then TKMemoParagraph(KMemo1.Blocks[i]).NumberingListLevel.LeftIndent := 30;
          end;
       end;
  end;
  TRlog('TFormNote.ToggleBullet() done');
end;

function TFormNote.isInBullet() : boolean;
var
   BlockNo, PosInBlock: longint;
begin
   BlockNo := kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, PosInBlock);
   while((BlockNo<KMemo1.blocks.count) and (not kmemo1.Blocks[BlockNo].ClassNameIs('TKMemoParagraph')))
   do inc(BlockNo);
   if(BlockNo>=KMemo1.blocks.count) then exit(false);

   Result := (TKMemoParagraph(kmemo1.Blocks[BlockNo]).Numbering = pnuBullets);
end;


procedure TFormNote.onMouseUp(Sender: TObject; Button: TMouseButton;
		Shift: TShiftState; X, Y: Integer);
begin
   TRlog('TFormNote.KMemo1MouseUp ');

   if((oldselstart<>KMemo1.RealSelStart) or (oldselend<>KMemo1.RealSelend))
   then begin
      TRlog(' Selection has changed (mouseup)');
      oldselstart := KMemo1.RealSelStart;
      oldselend := KMemo1.RealSelend;
      PostBuildMenus();
   end;
end;

procedure TFormNote.onMouseDown(Sender: TObject; Button: TMouseButton;
		Shift: TShiftState; X, Y: Integer);
begin
   BuildMenus(Sender);
   if ((ssCtrl in Shift) or (Button = mbRight)) then PopMenu.PopUp;
end;

procedure TFormNote.SetPrimarySelection();
var
  FormatList: Array [0..1] of TClipboardFormat;
begin
  TRlog('TFormNote.SetPrimarySelection');
  if (PrimarySelection.OnRequest=@PrimaryCopy) then exit;
  FormatList[0] := CF_TEXT;
  try
    PrimarySelection.SetSupportedFormats(1, @FormatList[0]);
    PrimarySelection.OnRequest:=@PrimaryCopy;
  except on E:Exception do TRlog(E.message);
  end;
  TRlog('TFormNote.SetPrimarySelection done');

end;

procedure TFormNote.UnsetPrimarySelection();
begin
  TRlog('TFormNote.UnsetPrimarySelection');
  if PrimarySelection.OnRequest=@PrimaryCopy then
    PrimarySelection.OnRequest:=nil;
end;

procedure TFormNote.PrimaryCopy(const RequestedFormatID: TClipboardFormat;  Data: TStream);
var
   s : UTF8String;
begin
   TRlog('TFormNote.PrimaryCopy');
   S := KMemo1.Blocks.SelText;
   if (RequestedFormatID = CF_TEXT) then Data.Write(s[1],length(s));
end;

procedure TFormNote.PrimaryPaste();
var
  Buff : UTF8String;
  i : integer;
begin
  TRlog('TFormNote.PrimaryPaste');

  Buff := PrimarySelection().AsText;
  if Buff <> '' then
   begin
      i := KMemo1.RealSelStart;
      KMemo1.Blocks.InsertPlainText(i, Buff);
      KMemo1.SelStart := i;
      Kmemo1.SelEnd := i + length(Buff);
      MarkDirty();
   end;
end;

procedure TFormNote.InsertDate();
var
  d,f,FirstBlockNo, LastBlockNo, IntIndex : integer;
begin
   TrLog('TFormNote.InsertDate');

   KMemo1.blocks.LockUpdate;

   d := KMemo1.RealSelStart;
   f := Kmemo1.RealSelEnd;

   if(f>d) then
   begin
      FirstBlockNo := Kmemo1.Blocks.IndexToBlockIndex(d, IntIndex);
      if IntIndex <> 0 then FirstBlockNo := KMemo1.SplitAt(d);

      LastBlockNo := Kmemo1.Blocks.IndexToBlockIndex(f, IntIndex);
      if IntIndex <> (length(Kmemo1.Blocks[LastBlockNo].Text)) then LastBlockNo := KMemo1.SplitAt(f) - 1;

      while(LastBlockNo>=FirstBlockNo) do
      begin
         Kmemo1.Blocks.Delete(LastBlockNo);
         dec(LastBlockNo);
      end;
   end;

   KMemo1.blocks.InsertPlainText(d,FormatDateTime(' YYYY-MM-DD hh:mm:ss ', now()));

   KMemo1.blocks.UnLockUpdate;
   MarkDirty();

   Kmemo1.SelStart := d+21;
   Kmemo1.SelEnd := d+21;
end;

procedure TFormNote.AlterFont(const Command : TNoteAction);
var
   FirstBlockNo, LastBlockNo, IntIndex, LastChar, FirstChar : longint;
   SplitStart : boolean = false;
begin
   TrLog('TFormNote.AlterFont');
   ProcessingChange := true;

   KMemo1.blocks.LockUpdate;
   LastChar := Kmemo1.RealSelEnd;
   FirstChar := KMemo1.RealSelStart;

   FirstBlockNo := Kmemo1.Blocks.IndexToBlockIndex(FirstChar, IntIndex);
   if IntIndex <> 0 then SplitStart := True;

   LastBlockNo := Kmemo1.Blocks.IndexToBlockIndex(LastChar, IntIndex);
   if IntIndex <> (length(Kmemo1.Blocks[LastBlockNo].Text)) then LastBlockNo := KMemo1.SplitAt(LastChar) - 1;

   while LastBlockNo > FirstBlockNo do
   begin
      AlterBlockFont(FirstBlockNo, LastBlockNo, Command);
      dec(LastBlockNo);
   end;

   if SplitStart then FirstBlockNo := KMemo1.SplitAt(FirstChar);
   AlterBlockFont(FirstBlockNo, FirstBlockNo, Command);

   KMemo1.blocks.UnLockUpdate;

   ProcessingChange := false;

   KMemo1.SelEnd := LastChar;
   KMemo1.SelStart := FirstChar;

   MarkDirty();
end;

function TFormNote.IncFontSize(n : integer) : integer;
begin
  if(n < FontSizeSmall) then exit(FontSizeSmall);
  if(n < FontSizeNormal) then exit(FontSizeNormal);
  if(n < FontSizeLarge) then exit(FontSizeLarge);
  Result := FontSizeHuge;
end;

function TFormNote.DecFontSize(n : integer) : integer;
begin
  if(n > FontSizeHuge) then exit(FontSizeHuge);
  if(n > FontSizeLarge) then exit(FontSizeLarge);
  if(n > FontSizeNormal) then exit(FontSizeNormal);
  Result := FontSizeSmall;
end;

procedure TFormNote.AlterBlockFont(const FirstBlockNo, BlockNo : longint; const Command : TNoteAction);
var
   Block, FirstBlock : TKMemoTextBlock;
begin
   FirstBlock := TKMemoTextBlock(KMemo1.Blocks[FirstBlockNo]);
   Block := TKMemoTextBlock(KMemo1.Blocks[BlockNo]);

   case Command of

      TNoteAction.IncreaseSize : Block.TextStyle.Font.Size := IncFontSize(Block.TextStyle.Font.Size);

      TNoteAction.DecreaseSize : Block.TextStyle.Font.Size := DecFontSize(Block.TextStyle.Font.Size);

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

   KMemo1.Blocks.LockUpdate;

   ProcessingChange := true;

   note^.OpenOnStartup := true;

   if not AlreadyLoaded then
   begin
     NoteToMemo();
     MarkTitle(true);
   end;

   Left := note^.X;
   Top := note^.Y;
   Height := note^.Height;
   Width := note^.Width;

   KMemo1.SelStart := KMemo1.Text.Length;  // set curser pos to end
   KMemo1.SelEnd := Kmemo1.Text.Length;

   KMemo1.SetFocus;

   {$ifdef windows}
    Color:= TextColour;
   {$endif}
   KMemo1.Colors.BkGnd:= BackGndColour;
   Kmemo1.Blocks.DefaultTextStyle.Font.Color := TextColour;

   ShowSearchPanel(false);

   PostBuildMenus();

   SetPrimarySelection();

   AlreadyLoaded := true;

   ProcessingChange := false;

   KMemo1.Blocks.UnLockUpdate;

end;

procedure TFormNote.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  TRlog('TFormNote.FormCloseQuery');
  isClosing := true;
  manualClosing := true;
  Commit(Sender);
  CanClose := True;
end;

procedure TFormNote.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  TRlog('TFormNote.FormClose');
  isClosing := true;
  note^.Display:=nil;
end;

procedure TFormNote.ShowSearchPanel(s : boolean);
begin
  TRlog('ShowSearchPanel('+BoolToStr(s)+')');

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
  ProcessingChange := false;
  ProcessingTitle := false;
  isClosing := false;
  manualClosing := false;
  SetFontSizes();

  findtext :='';
  EditFindInNote.Caption:='';


  KMemo1.TextStyle.Font.Size := FontSizeNormal;
  KMemo1.TextStyle.Font.Color := TextColour;
  KMemo1.TextStyle.Font.Name := UsualFont;

  KMemo1.blocks.DefaultTextStyle.Font.Size := FontSizeNormal;
  KMemo1.blocks.DefaultTextStyle.Font.Color := TextColour;
  KMemo1.blocks.DefaultTextStyle.Font.Name := UsualFont;

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
  m1.Caption := rsMenuFind + ' (Ctrl F)';
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
  m1.Caption := rsMenuSave + ' (Ctrl W)';
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
  m1.Caption := rsMenuPrint+' (Ctrl P)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=20;
  ToolsMenu.Add(m1);


  // Tools / Settings
  ToolsMenu.AddSeparator;
  m1 := TMenuItem.Create(ToolsMenu);
  m1.Tag := ord(ntSettings);
  m1.Caption := rsMenuSettings + '(Ctrl O)';
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

  TRlog('TFormNote.FormCreate - ENd menu');

  // Housekeeping
  HouseKeeper := TTimer.Create(nil);
  HouseKeeper.OnTimer := @Commit;
  HouseKeeper.Interval := 30000;
  HouseKeeper.Enabled := True;

  TRlog('TFormNote.FormCreate done');

end;

procedure TFormNote.PostBuildMenus();
begin
  if(assigned(MenuBuilder)) then
  begin
    MenuBuilder.Enabled:=false;
    FreeAndNil(MenuBuilder);
  end;

  MenuBuilder := TTimer.Create(nil);
  MenuBuilder.OnTimer := @BuildMenus;
  MenuBuilder.Interval := 20;
  MenuBuilder.Enabled := True;
end;

procedure TFormNote.PostFormatTitle();
begin
  if(assigned(TitleFormatter)) then
  begin
    TitleFormatter.Enabled:=false;
    FreeAndNil(TitleFormatter);
  end;

  TitleFormatter := TTimer.Create(nil);
  TitleFormatter.OnTimer := @ReceiveFormatTitle;
  TitleFormatter.Interval := 1000;
  TitleFormatter.Enabled := True;
end;


procedure TFormNote.ReceiveFormatTitle(Sender: TObject);
begin
   MarkTitle(false);
end;


procedure TFormNote.PostCommit();
begin
  TRlog('PostCommit');

  if(Assigned(HouseKeeper)) then
  begin
    HouseKeeper.Enabled := False;
    FreeAndNil(HouseKeeper);
  end;

  HouseKeeper := TTimer.Create(nil);
  HouseKeeper.OnTimer := @Commit;
  HouseKeeper.Interval := 100;
  HouseKeeper.Enabled := True;
end;

procedure TFormNote.CheckboxFindInNoteChange(Sender: TObject);
begin
  if(not CheckboxFindInNote.Checked) then ShowSearchPanel(false);
end;

procedure TFormNote.EditFindInNoteChange(Sender: TObject);
begin
  if(UTF8Length(EditFindInNote.Caption)>=UTF8Length(findtext))
  then FindNext(KMemo1.RealSelStart)
  else FindPrev(KMemo1.RealSelStart);
  findtext := EditFindInNote.Caption;
end;

procedure TFormNote.ButtonFindNextClick(Sender: TObject);
begin
   FindNext(KMemo1.RealSelStart+1);
end;

procedure TFormNote.FindNext(start : integer);
var
    s,lo: UTF8String;
    i : integer;
begin
   s := UTF8LowerCase(EditFindInNote.Caption);
   lo := UTF8Lowercase(KMemo1.Text);
   TRlog('Search Next for '+s);
   if(UTF8Length(s)>0) then
   begin
     i := UTF8Pos(s,lo,start + 1);
     if(i>0) then
     begin
       KMemo1.SelStart := i -1;
       KMemo1.SelEnd := i -1 + UTF8Length(s);
     end ;
   end;
end;

procedure TFormNote.ButtonFindPrevClick(Sender: TObject);
begin
  FindPrev(KMemo1.RealSelStart);
end;

procedure TFormNote.FindPrev(start : integer);
var
   s, lo: UTF8String;
   i,j,k : integer;
begin
   s := UTF8LowerCase(EditFindInNote.Caption);
   lo := UTF8LowerCase( KMemo1.Text);
   TRlog('Search Prev for '+s);

   if(UTF8Length(s)>0) then
   begin
     i :=0;
     k :=-1;
     while(i<start) do
     begin
        j:=UTF8Pos(s,lo,i+1);
        if(j>0) then
        begin
          if(j-1<KMemo1.RealSelStart) then k := j-1;
          i := i + j;
        end
        else i := KMemo1.RealSelStart +1;
     end;
     if(k>=0) then
     begin
       KMemo1.SelStart:=k;
       KMemo1.SelEnd:=k+UTF8Length(s);
     end;
   end;
end;

procedure TFormNote.FormDestroy(Sender: TObject);
{var
    ARec : TNoteUpdateRec; }
begin
  TrLog('TFormNote.FormDestroy');
  HouseKeeper.Enabled := False;
  FreeAndNil(HouseKeeper);
  UnsetPrimarySelection();
end;

function TFormNote.MarkTitle(force : boolean) : boolean;
var
    title : UTF8String;
    ktb : TKMemoTextBlock;
    i : integer;
begin
   if(assigned(TitleFormatter)) then
   begin
      TitleFormatter.Enabled:=false;
      FreeAndNil(TitleFormatter);
   end;

   if(ProcessingTitle) then exit(false);
   ProcessingTitle:=true;

   KMemo1.Blocks.LockUpdate;

   TRlog('MarkTitle FontSizeTitle='+IntToStr(FontSizeTitle));

   While(Kmemo1.Blocks.Count<4)
   do begin
     KMemo1.Blocks.AddTextBlock('');
     KMemo1.Blocks.AddParagraph();
   end;

   i :=0;
   title :='';

   while ((i < Kmemo1.Blocks.Count) and ((UTF8Length(UTF8Trim(Title))=0) or (not Kmemo1.Blocks[i].ClassNameIs('TKMemoParagraph'))))
   do begin
      if Kmemo1.Blocks[i].ClassNameIs('TKMemoTextBlock') then Title := Title + CleanTitle(Kmemo1.Blocks[i].Text);
      inc(i);
   end;

   TRlog('Found title : "'+title + '" with '+IntToStr(i)+' blocks');

   if(force) then title := UTF8Trim(CleanTitle(note^.Title))
   else title := UTF8Trim(CleanTitle(title));

   TRlog('Now title is "'+title + '"');

   if((i<>1) or (Kmemo1.Blocks[1].ClassName <> 'TKMemoParagraph') or (not Kmemo1.Blocks[0].ClassNameIs('TKMemoTextBlock')))
   then begin
     TRlog('wrong block1');
     while(i>0) do begin Kmemo1.Blocks.Delete(0); dec(i); end;
     KMemo1.Blocks.AddTextBlock(title,0);
     KMemo1.Blocks.AddParagraph(1);
   end
   else TKMemoTextBlock(Kmemo1.Blocks[0]).Text := title;

   if((Kmemo1.Blocks.Count<3) or (not Kmemo1.Blocks[2].ClassNameIs('TKMemoTextBlock')) or (Length(UTF8Trim(Kmemo1.Blocks[2].Text)) > 0))
   then begin
      TRlog('wrong block2 : adding empty text');
      KMemo1.Blocks.AddTextBlock('',2);
   end;

   if((Kmemo1.Blocks.Count<4) or (Kmemo1.Blocks[3].ClassName <> 'TKMemoParagraph'))
   then begin
     TRlog('wrong block3 : Adding par');
      KMemo1.Blocks.AddParagraph(3);
   end;

   TKMemoTextBlock(Kmemo1.Blocks[2]).Text := '';

   i:=0;
   while(i<4)
   do begin
      ktb := TKMemoTextBlock(Kmemo1.Blocks[i]);
      //TRlog('Testing block '+IntToStr(i)+' : '+ktb.Text);
      if(CompareStr(ktb.TextStyle.Font.Name,UsualFont)<>0) then ktb.TextStyle.Font.Name := UsualFont;
      if(ktb.TextStyle.Font.Size <> FontSizeTitle) then ktb.TextStyle.Font.Size := FontSizeTitle;
      if(ktb.TextStyle.Font.Color <> TitleColour) then ktb.TextStyle.Font.Color := TitleColour;
      if(not (ktb.TextStyle.Font.Style = [fsUnderline])) then ktb.TextStyle.Font.Style := [fsUnderline];
      inc(i);
   end;

   TRlog('Cleaning further blocks');
   i:=4;
   while ((i < Kmemo1.Blocks.Count)) do
   begin
      ktb := TKMemoTextBlock(Kmemo1.Blocks[i]);
      if (KMemo1.Blocks[i].ClassNameIs('TKMemoTextBlock') or KMemo1.Blocks[i].ClassNameIs('TKMemoParagraph')) and (TKMemoTextBlock(KMemo1.Blocks[i]).TextStyle.Font.Size = FontSizeTitle)
      then begin
           ktb := TKMemoTextBlock(KMemo1.Blocks[i]);
           ktb.TextStyle.Font.Size := FontSizeNormal;
           ktb.TextStyle.Font.Color := TextColour;
           ktb.TextStyle.Font.Style := [];
      end;
      inc(i);
   end;

   Result := false;

   // Update title
   if(CompareStr(title, note^.Title) <>0) then
   begin
      note^.Title := UTF8Trim(CleanTitle(Title));
      result := true;
      MarkDirty();
   end;

   KMemo1.Blocks.UnLockUpdate;

   ProcessingTitle := false;
end;

procedure TFormNote.CreateLink(isweb : boolean);
var
    s,s2 : UTF8String;
    n,m,i,j,k : Integer;
    hl : TKMemoHyperlink;
    f,fn : TFont;
    b1,b2 : integer;
begin
   TRlog('CreateLink('+BoolToStr(isweb)+') : '+KMemo1.SelText);

   s := trim(CleanTitle(KMemo1.SelText));
   TRlog('URL = '+s);

   if(Length(s)<1) then exit();

   KMemo1.Blocks.LockUpdate;

   b1 := KMemo1.RealSelStart;
   b2 := KMemo1.RealSelEnd;

   n := KMemo1.Blocks.IndexToBlockIndex(b1, i);

   while(CompareStr(' ',Copy(KMemo1.Blocks[n].Text,i+1,1))=0)
   do begin
      inc(i);
      inc(b1);
   end;
   if(i>0) then n := KMemo1.SplitAt(b1);

   f := TFont.Create;
   f.Name := TKMemoTextBlock(KMemo1.Blocks[n]).TextStyle.Font.Name;
   f.Size := TKMemoTextBlock(KMemo1.Blocks[n]).TextStyle.Font.Size;
   f.Color := clBlue;
   f.Style := [fsUnderline];
   f.Pitch := TKMemoTextBlock(KMemo1.Blocks[n]).TextStyle.Font.Pitch;

   fn := TFont.Create;
   fn.Name := f.Name;
   fn.Size := f.Size;
   fn.Color := TextColour;
   fn.Style := [];
   fn.Pitch := f.Pitch;

   m := KMemo1.Blocks.IndexToBlockIndex(b2, i);

   while(CompareStr(' ',Copy(KMemo1.Blocks[m].Text,i,1))=0)
   do begin
      dec(i);
      dec(b2);
   end;
   if(i=0) then dec(m)
   else if(i<Length(KMemo1.Blocks[m].Text))
   then m := KMemo1.SplitAt(b2)-1;

   i:=n;
   while(i<=m) do
   begin

      if(isweb and not KMemo1.Blocks[i].ClassNameIs('TKMemoParagraph') )
      then begin
        k := Pos(' ', KMemo1.Blocks[i].Text);
        if(k>1) then
        begin
          k := KMemo1.Blocks.BlockToIndex(KMemo1.Blocks[i]) + k-1;
          KMemo1.SplitAt(k);
          inc(m);
        end else
        if(k=1) then
        begin
          while(CompareStr(' ', Copy(KMemo1.Blocks[i].Text, k+1,1)) = 0 ) do inc(k);
          if(k<Length(KMemo1.Blocks[i].Text))
          then begin
             k := KMemo1.Blocks.BlockToIndex(KMemo1.Blocks[i]) + k;
             KMemo1.SplitAt(k);
             inc(m);
          end;
        end;
      end;
      inc(i);
   end;

   i:=n; j:=n;
   s:='';
   while(i<=m) do
   begin
      TKMemoTextBlock(KMemo1.Blocks[i]).TextStyle.Font := fn;

      s2 := CleanTitle(KMemo1.Blocks[i].Text);
      if(isweb) then s2 := Trim(s2);

      if(not KMemo1.Blocks[i].ClassNameIs('TKMemoParagraph') )
      then begin
        s := s + s2;
      end;

      if((i = m) or KMemo1.Blocks[i].ClassNameIs('TKMemoParagraph') or (Length(s2)=0))
      then begin
        if(Length(s)>0)
        then begin
           while(i>j) do
           begin
              KMemo1.Blocks.Delete(j);
              dec(i);
              dec(m);
           end;

           if((not KMemo1.Blocks[i].ClassNameIs('TKMemoParagraph')) and (Length(s2)>0)) then
           begin
              KMemo1.Blocks.Delete(j);
              dec(i);
              dec(m);
           end;

           hl := TKMemoHyperlink.Create;
           hl.Text := s;
           hl.URL := s;
           hl := KMemo1.Blocks.AddHyperlink(hl,j);
           hl.TextStyle.Font := f;
           if(isweb) then hl.OnDblClick := @ExternalLink
           else begin
             hl.OnDblClick := @InternalLink;
             TFormMain(mainWindow).OpenNoteByTitle(s);
           end;
           i := j;
           inc(m);
        end;
        j := i+1;
        s := '';
      end;
      inc(i);
   end;

   KMemo1.Blocks.UnLockUpdate;

   f.Free;
   fn.Free;
end;

procedure TFormNote.RemoveLink();
var
   BlockNo, EndBlock, Blar : longint;
   LinkText : UTF8String;
   tb : TKMemoTextBlock;
   f : Tfont;
begin
   BlockNo := KMemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, Blar);
   EndBlock := KMemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelEnd, Blar);

   KMemo1.Blocks.LockUpdate;

   f := TFont.Create;
   f.Name := UsualFont;
   f.Size := FontSizeNormal;
   f.Color := TextColour;
   f.Style := [];
   f.Pitch := fpVariable;

   while BlockNo <= EndBlock do
   begin
      if Kmemo1.Blocks[BlockNo].ClassNameIs('TKMemoHyperlink') then
      begin
         LinkText := Kmemo1.Blocks[BlockNo].Text;
         Kmemo1.Blocks.Delete(BlockNo);
         tb := KMemo1.Blocks.AddTextBlock(Linktext, BlockNo);
         tb.TextStyle.Font := f;
      end;
      inc(BlockNo);
   end;

   f.Free;
   KMemo1.Blocks.UnLockUpdate;
end;

procedure TFormNote.CheckLinks();
var
   i,j : integer;
   hl : TKMemoHyperlink;
   s : UTF8String;
   isweb : boolean;
   tb : TKMemoTextBlock;
   f : Tfont;
begin
  TRlog('CheckLinks');

  KMemo1.Blocks.LockUpdate;

  f := TFont.Create;
  f.Name := UsualFont;
  f.Size := FontSizeNormal;
  f.Color := TextColour;
  f.Style := [];
  f.Pitch := fpVariable;

  i:=0;
  while(i < KMemo1.Blocks.Count) do
  begin
     if Kmemo1.Blocks[i].ClassNameIs('TKMemoHyperlink') then
     begin
        hl := TKMemoHyperlink(Kmemo1.Blocks[i]);
        s := hl.URL;
        isweb := (hl.OnDblClick = @ExternalLink);

        if(not isweb) then
        begin
           if(CompareStr(s,hl.Text)<>0)
           then begin
              s := hl.Text;
              KMemo1.Blocks.Delete(i);
              tb := KMemo1.Blocks.AddTextBlock(s,i);
              tb.TextStyle.Font := f;
           end;
        end
        else begin
           j := Length(hl.Text);
           if(CompareStr(Copy(s,1,j),hl.Text)=0) // ok !
           then begin
              hl.URL := hl.Text;
           end
           else begin
             s := hl.Text;
              KMemo1.Blocks.Delete(i);
              tb := KMemo1.Blocks.AddTextBlock(s,i);
              tb.TextStyle.Font := f;
           end ;
        end;
     end;
     inc(i);
  end;

  f.Free;
  KMemo1.Blocks.UnLockUpdate;
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

procedure TFormNote.onChange(Sender : TObject);
begin
   if not AlreadyLoaded then exit();
   if ProcessingChange then exit();

   ProcessingChange := true;
   TRlog(' TFormNote.onChange');

   if(CompareStr(oldtext,KMemo1.Text)<>0)
   then begin
     TRlog(' Text has changed LENOLD='+IntToStr(Length(oldtext))+'  LENNEW='+IntToStr(Length(KMemo1.Text)));
     //TRlog(' Text has changed OLD='+oldtext+'  EW='+KMemo1.Text);
     CheckLinks();
     PostFormatTitle();
     MarkDirty();
     oldtext := KMemo1.Text;
     oldselstart := KMemo1.RealSelStart;
     oldselend := KMemo1.RealSelend;
     PostBuildMenus();
     TRlog('New text length = '+IntToStr(Length(oldtext)));
   end else
   if((oldselstart<>KMemo1.RealSelStart) or (oldselend<>KMemo1.RealSelend))
   then begin
      TRlog(' Selection has changed (mouseup)');
      oldselstart := KMemo1.RealSelStart;
      oldselend := KMemo1.RealSelend;
      PostBuildMenus();
   end;
   ProcessingChange := false;
end;

procedure TFormNote.onKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    i,j : integer;
begin
  i := KMemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, j);
   TRlog('TFormNote.KMemo1KeyUp '+IntToStr(Key)+ ' Block='+IntToStr(i) + ' Pos='+IntToStr(KMemo1.RealSelStart)+' Tex@pos= '+Copy(KMemo1.Text, KMemo1.RealSelStart+1,10));

   if((oldselstart<>KMemo1.RealSelStart) or (oldselend<>KMemo1.RealSelend))
   then begin
      TRlog(' Selection has changed (keyup)');
      oldselstart := KMemo1.RealSelStart;
      oldselend := KMemo1.RealSelend;
      PostBuildMenus();
   end;
end;

procedure TFormNote.onKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
var
    i,j : integer;
    par : TKMemoParagraph;
begin
//  TRlog('TFormNote.KMemo1KeyDown '+IntToStr(Key));
  i := KMemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, j);
  //TRlog('TFormNote.KMemo1KeyDown '+IntToStr(Key)+ ' Block='+IntToStr(i) + ' Pos='+IntToStr(KMemo1.RealSelStart)+' Tex@pos= '+Copy(KMemo1.Text, KMemo1.RealSelStart+1,10));
  //TRlog('TFormNote.KMemo1KeyDown '+IntToStr(Key)+ ' Block='+IntToStr(i) + ' Pos='+IntToStr(KMemo1.RealSelStart)+' Tex@poaClass='+KMemo1.Blocks[i].ClassName + ' Fontsize='+IntToStr(TKMemoTextBlock(KMemo1.Blocks[i]).TextStyle.Font.Size));


  // CTRL
  if {$ifdef DARWIN}ssMeta{$else}ssCtrl{$endif} in Shift then
  begin

     if key = VK_B then begin TrLog('Ctrl-B'); AlterFont(ToggleBold); Key := 0; end;
     if key = VK_I then begin TrLog('Ctrl-I'); AlterFont(ToggleItalic); Key := 0; end;
     if key = VK_S then begin TrLog('Ctrl-S'); AlterFont(ToggleStrikeout); Key := 0; end;
     if key = VK_U then begin TrLog('Ctrl-U'); AlterFont(ToggleUnderline); Key := 0; end;
     if key = VK_T then begin TrLog('Ctrl-T'); AlterFont(ToggleFont); Key := 0; end;
     if key = VK_H then begin TrLog('Ctrl-H'); AlterFont(ToggleHighlight); Key := 0; end;

     if key = VK_A then begin TrLog('Ctrl-A'); KMemo1.ExecuteCommand(ecSelectAll); Key := 0; end;

     if key = VK_D then begin TrLog('Ctrl-D'); InsertDate(); Key := 0; end;

     if key = VK_C then begin TrLog('Ctrl-C'); KMemo1.ExecuteCommand(TKEditCommand.ecCopy); MarkDirty(); Key := 0; end;
     if key = VK_X then begin TrLog('Ctrl-X'); KMemo1.ExecuteCommand(TKEditCommand.ecCut); MarkDirty(); Key := 0; end;
     if key = VK_V then begin TrLog('Ctrl-V'); PrimaryPaste(); {KMemo1.ExecuteCommand(TKEditCommand.ecPaste); MarkDirty(); } Key := 0; end;

     if key = VK_W then begin TrLog('Ctrl-W'); PostCommit(); Key := 0; end;

     if key = VK_O then begin TrLog('Ctrl-O'); TFormMain(mainWIndow).ShowSettings(); Key := 0; end;

     if key = VK_P then begin TrLog('Ctrl-P'); NotePrint(); Key := 0; end;

     if key = VK_Z then begin TrLog('Ctrl-Z'); KMemo1.ExecuteCommand(TKEditCommand.ecUndo); Key := 0; end;
     if key = VK_Y then begin TrLog('Ctrl-Y'); KMemo1.ExecuteCommand(TKEditCommand.ecRedo); Key := 0; end;

     if key = VK_F then begin TrLog('Ctrl-F'); CheckboxFindInNote.Checked := true; ShowSearchPanel(true); EditFindInNote.SetFocus; Key := 0; end;

     if key = VK_LCL_EQUAL then begin TrLog('Ctrl +'); AlterFont(IncreaseSize); Key := 0; end;
     if key = VK_LCL_MINUS then begin TrLog('Ctrl -'); AlterFont(DecreaseSize); Key := 0; end;

     exit();
   end;

  // ALT
  if ssAlt in Shift then
  begin
    if key = VK_LEFT then begin TrLog('Alt-Left'); ToggleBullet(-1); Key := 0; exit(); end;
    if key = VK_RIGHT then begin TrLog('Alt-Right'); ToggleBullet(1); Key := 0; exit(); end;

    exit();
  end;

  // SHIFT
  if ssShift in Shift then
  begin

    exit();
  end;

  // OTHER

  if key = VK_TAB then begin TrLog('Tab'); KMemo1.Blocks.AddTextBlock('   '); Key := 0; exit(); end;

  if (Key  =  VK_BACK) and (KMemo1.RealSelStart = KMemo1.RealSelEnd)   // DEALING WITH BACKSPACE NEAR A BULLET
  then begin
    i := Kmemo1.Blocks.IndexToBlockIndex(KMemo1.RealSelStart, j);
    if(j>0) then exit();
    if(i=0) then exit();

    if(not KMemo1.Blocks[i-1].ClassNameIs('TKMemoParagraph')) then exit();

    j:=i;
    while((j<KMemo1.Blocks.Count) and not KMemo1.Blocks[j].ClassNameIs('TKMemoParagraph'))
    do inc(j);

    if(j>=KMemo1.BLocks.Count)
    then begin
      par := KMemo1.Blocks.AddParagraph();
      par.TextStyle.Font := TKMemoParagraph(KMemo1.Blocks[i-1]).TextStyle.Font;
    end;

    if(TKMemoParagraph(KMemo1.Blocks[j]).Numbering <> pnuBullets)
    then begin
      if (TKMemoParagraph(KMemo1.Blocks[i-1]).Numbering = pnuBullets) then ToggleBullet(1);
      exit();
    end;

    ToggleBullet(-1);
    Key :=0;
  end;
end;


{ ======= MAIN MENU ====== }

procedure TFormNote.ToggleNotebook(Sender : TObject);
var
   i : integer;
   s,s2 : UTF8String;
   ok : boolean;
begin
   TRlog('TFormNote.ToggleNotebook');

   i := TMenuItem(Sender).Tag;
   TRlog('tag '+IntToStr(i));

   if(i>=TFormMain(mainWindow).NotebooksList.Count) then exit();

   if(i<0)
   then begin
      note^.Tags.Clear;
      PostBuildMenus();
      exit();
   end;

   s := TFormMain(mainWindow).NotebooksList.Strings[i];

   TRlog('got notebook "'+s+'"');

   if(length(s) = 0) then exit();

   if(ManyNoteBooks) then
   begin
      i:=0;
      ok := true;
      while(ok and (i<note^.Tags.Count))
      do begin
         s2:= note^.Tags.Strings[i];
         inc(i);
         if(CompareText('system:notebook:',Copy(s2,1,16)) <> 0) then continue;
         if(CompareText(Trim(Copy(s2,17)),s) = 0) then ok:=false;
      end;
      if(ok) then note^.Tags.Add('system:notebook:'+s);
   end
   else begin
      note^.Tags.Clear;
      note^.Tags.Add('system:notebook:'+s);
   end;
   PostBuildMenus();

end;

procedure TFormNote.MainMenuClicked(Sender : TObject);
var
    s : String;
    s2 : UTF8String;
begin

   TRlog('MainMenuClicked');

   case TNoteMenuTags(TMenuItem(Sender).Tag) of

      // FILE
      ntSearchAll : mainWindow.Show();

      ntFind : begin CheckboxFindInNote.Checked := true; ShowSearchPanel(true); EditFindInNote.SetFocus; end;

      //ntDuplicate :

      ntCommit :            PostCommit();

      //ntDelete :

      // EDIT
      ntRedo :              begin KMemo1.ExecuteCommand(TKEditCommand.ecRedo); MarkDirty(); end;
      ntUndo :              begin KMemo1.ExecuteCommand(TKEditCommand.ecUndo); MarkDirty(); end;
      ntSelectAll :         KMemo1.ExecuteCommand(ecSelectAll);
      ntCopy :              begin KMemo1.ExecuteCommand(TKEditCommand.ecCopy); MarkDirty(); end;
      ntCut :               begin KMemo1.ExecuteCommand(TKEditCommand.ecCut); MarkDirty(); end;
      ntPaste :             begin PrimaryPaste(); { KMemo1.ExecuteCommand(TKEditCommand.ecPaste); MarkDirty();} end;
      ntInsertDate :        InsertDate();

      ntNoteLink :          CreateLink(false);
      ntURLLink :           CreateLink(true);
      ntRemoveLink :        RemoveLink();

      // SPELLING
      ntSpelling :          ReplaceSel(TMenuItem(Sender).Caption);

      // FORMAT
      ntBold :              AlterFont(ToggleBold);
      ntItalic :            AlterFont(ToggleItalic);
      ntStrike :            AlterFont(ToggleStrikeout);
      ntUnderlined :        AlterFont(ToggleUnderline);
      ntFixed :             AlterFont(ToggleFont);
      ntHighlight :         AlterFont(ToggleHighlight);

      ntFontPlus :          AlterFont(IncreaseSize);
      ntFontMinus :         AlterFont(DecreaseSize);

      ntBullet :            ToggleBullet(0);

      // NOTEBOOK
      ntNewNotebook :       begin
                                 s:='';
                                 InputQuery(rsNotebooks,rsEnterNewNotebook ,s);
                                 TRlog('New notebook : '+s);
                                 s2 := Trim(CleanTitle(s));
                                 TRlog('New notebook cleaned : '+s);
                                 if(TFormMain(mainWindow).AddNewNotebook(s2))
                                 then PostBuildMenus();
                             end;

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

  if(assigned(MenuBuilder)) then
  begin
    MenuBuilder.Enabled:=false;
    FreeAndNil(MenuBuilder);
  end;


  PopMenu.Items.Clear;
  PopMenu.Images := TFormMain(mainWindow).MenuIconList;

  // Pop / InsertDate
  m1 := TMenuItem.Create(PopMenu);
  m1.Tag := ord(ntInsertDate);
  m1.Caption := rsInsertDate +' (Ctrl D)';
  m1.OnClick := @MainMenuClicked;
  PopMenu.Items.Add(m1);

  PopMenu.Items.AddSeparator;

  // POP / Insert link
  m1 := TMenuItem.Create(PopMenu);
  m1.ImageIndex:=27;
  m1.Tag := ord(ntURLLink);
  m1.Caption := rsAddURLLink;
  m1.OnClick := @MainMenuClicked;
  m1.Enabled := (KMemo1.RealSelLength>0);
  PopMenu.Items.Add(m1);

  m1 := TMenuItem.Create(PopMenu);
  m1.ImageIndex:=27;
  m1.Tag := ord(ntNoteLink);
  m1.Caption := rsAddNoteLink;
  m1.OnClick := @MainMenuClicked;
  m1.Enabled := (KMemo1.RealSelLength>0);
  PopMenu.Items.Add(m1);

  m1 := TMenuItem.Create(PopMenu);
  //m1.ImageIndex:=27;
  m1.Tag := ord(ntRemoveLink);
  m1.Caption := rsRemoveLink;
  m1.OnClick := @MainMenuClicked;
  PopMenu.Items.Add(m1);

  PopMenu.Items.AddSeparator;

  // POP / Check spell
  m1 := TMenuItem.Create(PopMenu);
  m1.ImageIndex:=41;
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
    s.Free;
  end else begin
      m1.Caption := rsCheckSelNop ;
      m1.Enabled := false;
  end;


  PopMenu.Items.AddSeparator;

  // Pop / SelectAll
  m1 := TMenuItem.Create(PopMenu);
  m1.Tag := ord(ntSelectAll);
  m1.Caption := rsMenuSelectAll+' (Ctrl A)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=40;
  PopMenu.Items.Add(m1);

  EditMenu.Clear;

  // Edit / Undo
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntUndo);
  m1.Caption := rsMenuUndo+' (Ctrl Z)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=24;
  EditMenu.Add(m1);

  // Edit / Redo
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntRedo);
  m1.Caption := rsMenuRedo+' (Ctrl Y)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=25;
  EditMenu.Add(m1);

  EditMenu.AddSeparator;

  // Edit / SelectAll
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntSelectAll);
  m1.Caption := rsMenuSelectAll+' (Ctrl A)';
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=40;
  EditMenu.Add(m1);

  EditMenu.AddSeparator;

  // Edit / Cut
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntCut);
  m1.Caption := rsMenuCut+' (Ctrl X)';
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=23;
  EditMenu.Add(m1);
  // Edit / Copy
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntCopy);
  m1.Caption := rsMenuCopy+' (Ctrl C)';
  m1.OnClick := @MainMenuClicked;
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.ImageIndex:=21;
  EditMenu.Add(m1);
  // Edit / Paste
  m1 := TMenuItem.Create(EditMenu);
  m1.Tag := ord(ntPaste);
  m1.Caption := rsMenuPaste+' (Ctrl V)';
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=22;
  EditMenu.Add(m1);

  EditMenu.AddSeparator;

   // Edit / Insert link
  m1 := TMenuItem.Create(EditMenu);
  m1.ImageIndex:=27;
  m1.Caption := rsLinks;
  EditMenu.Add(m1);

  m2 := TMenuItem.Create(m1);
  m2.Caption := rsAddURLLink;
  m2.Enabled := (KMemo1.RealSelLength>0);
  m2.Tag := ord(ntURLLink);
  m2.OnClick := @MainMenuClicked;
  m1.Add(m2);

  m2 := TMenuItem.Create(m1);
  m2.Caption := rsAddNoteLink;
  m2.Enabled := (KMemo1.RealSelLength>0);
  m2.Tag := ord(ntNoteLink);
  m2.OnClick := @MainMenuClicked;
  m1.Add(m2);

  m2 := TMenuItem.Create(m1);
  m2.Tag := ord(ntRemoveLink);
  m2.Caption := rsRemoveLink;
  m2.OnClick := @MainMenuClicked;
  m1.Add(m2);

  FormatMenu.Clear;

  // Format / Bold
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntBold);
  m1.Caption := rsMenuBold+' (Ctrl B)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isBold();
  m1.ImageIndex:=32;
  FormatMenu.Add(m1);
  // Format / Italic
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntItalic);
  m1.Caption := rsMenuItalic+' (Ctrl I)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isItalic();
  m1.ImageIndex:=33;
  FormatMenu.Add(m1);
  // Format / Striekout
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntStrike);
  m1.Caption := rsMenuStrikeout +' (Ctrl S)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isStriked();
  m1.ImageIndex:=29;
  FormatMenu.Add(m1);
  // Format / Underlined
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntUnderlined);
  m1.Caption := rsMenuUnderlined+' (Ctrl U)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isUnderlined();
  m1.ImageIndex:=28;
  FormatMenu.Add(m1);
  // Format / Highlight
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntHighlight);
  m1.Caption := rsMenuHighlight+' (Ctrl H)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isHighlight();
  m1.ImageIndex:=30;
  FormatMenu.Add(m1);
  // Format / FixedFont
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntFixed);
  m1.Caption := rsMenuFixed+' (Ctrl T)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isFixedFont();
  m1.ImageIndex:=31;
  FormatMenu.Add(m1);

  FormatMenu.AddSeparator;

  // Format / Font+
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntFontPlus);
  m1.Caption := rsMenuFontPlus+' (Ctrl +)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=35;
  FormatMenu.Add(m1);
  // Format / Font-
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntFontMinus);
  m1.Caption := rsMenuFontMinus+' (Ctrl -)';
  m1.Enabled:= (KMemo1.RealSelLength>0);
  m1.OnClick := @MainMenuClicked;
  m1.ImageIndex:=34;
  FormatMenu.Add(m1);

  FormatMenu.AddSeparator;

  // Format / Bullet Enable/Disable
  m1 := TMenuItem.Create(FormatMenu);
  m1.Tag := ord(ntBullet);
  m1.Caption := rsMenuBullet+'(Ctrl ⬅, Ctrl ➡)';
  m1.OnClick := @MainMenuClicked;
  m1.Checked := isInBullet();
  m1.ImageIndex:=38;
  FormatMenu.Add(m1);

  NotebooksMenu.Clear;

  // Notebooks / None
  m1 := TMenuItem.Create(NotebooksMenu);
  m1.Tag := -1;
  m1.Caption := rsNoNotebook;
  m1.OnClick := @ToggleNotebook;
  m1.Checked := NoteBelongs('-',note);
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
