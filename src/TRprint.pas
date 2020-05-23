unit TRprint;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, Graphics, Printers, LazUTF8, Kmemo;


type PWord=^TWord;

     TWord= record
      AWord : String;
      Size : integer;
      Bold, Italic, NewLine : boolean;
      Colour : TColor;
    end;

type TWordList = class(TList)
   private
      function Get(Index : Integer) : PWord;
   public
      destructor Destroy; Override;
      procedure Add(TheWord : ANSIString; S : integer; B, I, NL : boolean; Colour : TColor);
      property Items[Index : integer] : PWord read Get; default;
   end;

type TKPrn = class
   private
        FirstLine : Boolean;
        WordList : TWordList;
        CurrentY : Integer;
        MaxY     : Integer;
        LeftEdge, RightEdge : integer;
        BlankLineHeight : integer;
        { Determines a suitable height for blank lines }
        function BlankLineH(): integer;
        { Copies the interesting font characteristics }
        procedure CopyFont(FromFont, ToFont: TFont);
        { Copies the KMemo into WordList, one word per item }
        function KMemoRead(const TheKMemo: TKMemo) : boolean;
        { returns the height of higest char in Line between passed params }
        function LineHeight(const SWord, EWord: integer): integer;
        { Prints, at CurrentY, the Line between passed params }
        procedure LinePrint(const SWord, EWord: integer);
        { Indicates width, in display pixels, of words between passed params }
        function LineWidth(const SWord, EWord: integer): integer;
        { set the style of the printer to that of the the Item in WordList }
        procedure SetPrinter(ItemNo: integer);
    public
        { call this to print the KMemo via previously setup Printer }
        function PrintKmemo(KM1 : TKMemo) : boolean;
        destructor Destroy; Override;
        constructor Create();
    end;


implementation

Const
    VNudge = 0.85;      // Vert align of different font sizes. Small value pulls
                        // larger fonts lower compared to small characters.
                        // So, if big fonts appear to sit above base line, drop
                        // this number down a bit.

    Margin = 0.05;      // Fraction of page width and height reserved for margins.

function TWordList.Get(Index: Integer): PWord;
begin
    Result := PWord(inherited get(Index));
end;


destructor TWordList.Destroy;
var
    I : integer;
begin
    for I := 0 to Count-1 do dispose(Items[I]);

    inherited Destroy;
end;

procedure TWordList.Add(TheWord: ANSIString; S: integer; B, I, NL: boolean; Colour : TColor);
var
    PL : PWord;
begin
    new(PL);
    PL^.AWord:=TheWord;
    PL^.Size:=S;
    PL^.Bold:=B;
    PL^.Italic:=I;
    PL^.Colour:=Colour;
    PL^.NewLine:= NL;
    inherited Add(PL);
end;

function TKPrn.PrintKmemo(KM1: TKMemo): boolean;
var
    StartWord : integer = 0;
    EndWord : integer = 0;
begin
   if not KMemoRead(KM1) then exit(False);
   try
      Printer.BeginDoc;
      BlankLineHeight := BlankLineH();
      while EndWord < WordList.Count do
      begin
         inc(Endword);

         if EndWord = WordList.Count then break;

         if WordList.Items[EndWord]^.NewLine then
         begin
            if WordList.Items[EndWord-1]^.AWord = ''
              then inc(CurrentY, BlankLineHeight)
              else LinePrint(StartWord, EndWord);
            StartWord := EndWord;
            continue;
         end;

         if Linewidth(StartWord, EndWord) >  (RightEdge-LeftEdge) then
         begin
            dec(EndWord);
            LinePrint(StartWord, EndWord);
            StartWord := EndWord;
         end;
      end;
    finally
       Printer.EndDoc;
    end;
end;

destructor TKPrn.Destroy;
begin
    FreeandNil(WordList);
    inherited Destroy;
end;

constructor TKPrn.Create();
begin
    inherited Create();
    LeftEdge := round(Printer.PageWidth * Margin);
    RightEdge := Printer.PageWidth - (2 * LeftEdge);
    MaxY := round(Printer.PageHeight * (1-(2*Margin)));
    CurrentY := round(Printer.PageHeight * Margin);
    FirstLine := True;
end;

function TKPrn.BlankLineH() : integer;
begin
    Printer.Canvas.Font.Size := 10;
    Printer.Canvas.Font.Bold :=  False;
    Printer.Canvas.Font.Italic :=  False;
    Result := Printer.Canvas.TextHeight('I');
end;

procedure TKPrn.SetPrinter(ItemNo : integer);
begin
    Printer.Canvas.Font.Size := WordList.Items[ItemNo]^.Size;
    Printer.Canvas.Font.Color := WordList.Items[ItemNo]^.Colour;
    Printer.Canvas.Font.Bold :=  WordList.Items[ItemNo]^.Bold;
    Printer.Canvas.Font.Italic :=  WordList.Items[ItemNo]^.Italic;
end;

procedure TKPrn.LinePrint(const SWord, EWord : integer);
var
    ItemNo, XOffset, YOffset : integer;
begin
   ItemNo := SWord;
   XOffset := LeftEdge;
   CurrentY := CurrentY + LineHeight(SWord, EWord);
   while ItemNo < EWord do
   begin
      SetPrinter(ItemNo);
      YOffset := CurrentY - round(VNudge * Printer.Canvas.TextHeight('I'));    // nudge factor to get all sizes font on same baseline
      Printer.Canvas.TextOut(XOffset, YOffset, WordList.Items[ItemNo]^.AWord);
      XOffset := XOffset + Printer.Canvas.TextWidth(WordList.Items[ItemNo]^.AWord);
      inc(ItemNo);
   end;

   if FirstLine then
   begin
      printer.canvas.Line(LeftEdge, CurrentY, RightEdge, CurrentY);
      printer.canvas.Line(LeftEdge, CurrentY+1, RightEdge, CurrentY+1);
      CurrentY := CurrentY + round(0.5 * BlankLineHeight);
      FirstLine := False;
   end;

   if CurrentY > MaxY then
   begin
      Printer.EndDoc;
      Printer.BeginDoc;
      CurrentY := round(Printer.PageHeight * Margin);
   end;
end;

function TKPrn.LineWidth(const SWord, EWord : integer) : integer;
var
    ItemNo : integer;
begin
   Result := 0;
   ItemNo := SWord;
   while ItemNo < EWord do
   begin
      SetPrinter(ItemNo);
      Result := Result + Printer.Canvas.TextWidth(WordList.Items[ItemNo]^.AWord);
      inc(ItemNo);
   end;
end;

function TKPrn.LineHeight(const SWord, EWord: integer): integer;
var
   ItemNo : integer;
begin
   ItemNo := SWord;
   Result := 0;
   while ItemNo < EWord do
   begin
      SetPrinter(ItemNo);
      if Printer.Canvas.TextHeight('I') > Result
        then Result := Printer.Canvas.TextHeight('I');
      inc(ItemNo);
   end;
   Result := Result + Round(0.2 * BlankLineHeight);
end;

procedure TKPrn.CopyFont(FromFont, ToFont : TFont);
begin
   ToFont.Bold := FromFont.Bold;
   ToFont.Italic := FromFont.Italic;
   ToFont.Size := FromFont.Size;
   ToFont.Color := FromFont.Color;
end;

function TKPrn.KMemoRead(const TheKMemo : TKMemo) : boolean;
var
   BlockNo : integer = 0;
   I : integer;
   ExFont : TFont;
   St : ANSIString = '';
begin
   if WordList <> Nil then WordList.Free;
   WordList := TWordList.Create;

   ExFont := TFont.Create();
   CopyFont(TKMemoTextBlock(TheKmemo.Blocks.Items[0]).TextStyle.Font, ExFont);    // Carefull, what if its a Para ?
   for BlockNo := 0 to TheKMemo.Blocks.Count-1 do
   begin
      if not TheKMemo.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph') then
      begin
         CopyFont(TKMemoTextBlock(TheKmemo.Blocks.Items[BlockNo]).TextStyle.Font, ExFont);
         for I := 0 to TheKMemo.Blocks.Items[BlockNo].WordCount-1 do
         begin
            St := TheKMemo.Blocks.Items[BlockNo].Words[I];
            WordList.Add(St, ExFont.Size, ExFont.Bold, ExFont.Italic, False, ExFont.Color);
         end;
      end
      else WordList.Add('', ExFont.Size, ExFont.Bold, ExFont.Italic, True, ExFont.Color);
   end;

   FreeandNil(ExFont);
   result := (WordList.Count > 1);
end;

end.
