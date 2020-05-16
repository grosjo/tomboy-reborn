unit Index;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
    ExtCtrls, KMemo;

type

    { TFormIndex }

    TFormIndex = class(TForm)
        ListBox1: TListBox;
        Panel1: TPanel;
        procedure FormShow(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
    private
        function IsHeading(const BlkNo: integer): integer;

    public
        TheKMemo : TKMemo;
        SelectedBlock : integer;
    end;

var
    FormIndex: TFormIndex;

implementation

{$R *.lfm}

{ TFormIndex }

uses TRcommon;

procedure TFormIndex.FormShow(Sender: TObject);
var
    Index : integer = 0;
begin
    SelectedBlock := -1;
    while Index < TheKmemo.Blocks.Count do begin
        while TheKMemo.Blocks.Items[Index].ClassNameIs('TKMemoParagraph') do begin
            inc(Index);
            if Index >= TheKMemo.Blocks.Count then exit;
        end;
        Index := IsHeading(Index);
    end;
end;

procedure TFormIndex.ListBox1Click(Sender: TObject);
begin
    if Listbox1.ItemIndex = -1 then
        exit;
    SelectedBlock := PtrInt(Listbox1.Items.Objects[Listbox1.ItemIndex]);
    //ShowMessage('Attached value: ' + IntToStr(SelectedBlock));
    Modalresult := mrOK;
end;

function TFormIndex.IsHeading(const BlkNo : integer) : integer;
var
    Index : integer;
    St : string = '';
begin
    Index := BlkNo;
    Result := BlkNo;
    // write('[CALLED=' + inttostr(blkno) + '] ');
    while Result < TheKmemo.Blocks.Count do begin
        if TheKMemo.Blocks.Items[Result].ClassNameIs('TKMemoParagraph') then break;
        inc(result);
    end;
    // writeln('[PARA=' + inttostr(Result)+']');
    // Result is now pointing to first Para beyond BlkNo OR beyond kmemo content
    //while Index < Result do begin
        // writeln('[' + inttostr(Index) + '] ' + TKmemoTextBlock(TheKMemo.Blocks.Items[Index]).Text);
//        if (TheKMemo.Blocks.Items[Index].ClassNameIs('TKMemoTextBlock')
//                    and (TKmemoTextBlock(TheKMemo.Blocks.Items[Index]).TextStyle.Font.Size
//                        in [FontSizeTitle, FontSizeLarge, FontSizeHuge]))
//        then {begin writeln('================ Examined ' + TKmemoTextBlock(TheKMemo.Blocks.Items[Index]).Text);} inc(Index){; end }
//       else                    // its not a heading
//exit(Result)        // Remember we may be beyond the content ....
    //end;
    // OK, its a heading, all blocks are Large or Huge. Huge=NoSpaces; LargeBold=2 spaces; Large=4 spaces
    //if TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.Size = FontSizeLarge then begin
    //    St := '.  ';
    //    if not (fsBold in TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.style) then
    //        St := St + '   ';
    //end;
    Index := BlkNo;
    while Index < Result do begin
        St := St + TKmemoTextBlock(TheKMemo.Blocks.Items[Index]).Text;
        inc(Index);
    end;
    ListBox1.AddItem(St, TObject(PtrInt(BlkNo)));
    inc(Result);
end;

    (*
                and ((BlkNo +1) < TheKMemo.Blocks.Count)
                and TheKMemo.Blocks.Items[BlkNo+1].ClassNameIs('TKMemoParagraph'))
     end;
    if
    then begin
        // OK, its a single block line. But is it a heading ?
        if TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).TextStyle.Font.Size
                    in [Sett.FontTitle, Sett.FontLarge, Sett.FontHuge] then begin
              ListBox1.AddItem(TKmemoTextBlock(TheKMemo.Blocks.Items[BlkNo]).Text, TObject(PtrInt(BlkNo)));
              inc(BlkNo);
              exit(BlkNo + 1);
        end;
    end;
    // if to here, the BlkNo did not point to start of a heading. Skip to next Para ...
    while BlkNo < TheKmemo.Blocks.Count do begin
        if
    end;
    need to skip ahead to next block following a parmarker.
end;    *)

end.

