unit SaveNote;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, KMemo, Graphics, LazLogger, TRcommon;


{type TNoteLocation = record
  X, Y, Width, Height : integer;
end;}

type TNoteUpdateRec =
  record
     CPos : shortstring;
     X, Y : shortstring;
     Width, Height : shortstring;
     OOS : shortstring;
     FFName : string;
     LastChangeDate : string;   // if '', its a content save, generate a new timestamp
     ErrorStr : string;         // '' if all OK, not used everywhere....
  end;


type

{ TSaveNote }

 TSaveNote = class

 private
        OutStream:TMemoryStream;
        ID : ANSIString;
        FSize : integer;
	Bold : boolean;
	Italics : boolean;
	HiLight : boolean;
        Underline : boolean;
        Strikeout : boolean;
        FixedWidth : boolean;
        PrevFSize : integer;
	PrevBold : boolean;
	PrevItalics : boolean;
	PrevHiLight : boolean;
        PrevUnderline : boolean;
        PrevStrikeout : boolean;
        PrevFixedWidth : boolean;
	InList : boolean;
        KM : TKMemo;
        function AddTag(const FT : TKMemoTextBlock; var Buff : ANSIString; CloseOnly : boolean = False) : ANSIString;
	function BlockAttributes(Bk: TKMemoBlock): AnsiString;
	procedure BulletList(var Buff: ANSIString);
        procedure CopyLastFontAttr();
	function FontAttributes(const Ft : TFont; Ts : TKMemoTextStyle): ANSIString;

        function SetFontXML(FontSize : TFontRange; s : String) : string;

        function Header() : ANSIstring;
        function Footer(Loc : TNoteUpdateRec) : string;

 public
       TimeStamp : string;
       Title : ANSIString;

       CreateDate : ANSIString;
       procedure SaveNewTemplate(NotebookName: ANSIString);
       procedure ReadKMemo(FileName : ANSIString; KM1 : TKMemo);
       function WriteToDisk(const FileName: ANSIString; var NoteLoc: TNoteUpdateRec): boolean;
       constructor Create;
       destructor Destroy;  override;
 end;


implementation

uses FileUtil,LazUTF8,TRSearchUnit,LazFileUtils,SyncUtils;

const
  {$ifdef LINUX}
  MonospaceFont = 'monospace';
  {$ifend}
  {$ifdef WINDOWS}
  MonospaceFont = 'Lucida Console';
  {$ifend}
  {$ifdef DARWIN}
  MonospaceFont = 'Lucida Console';
  {$ifend}

constructor TSaveNote.Create;
begin
    OutStream := Nil;
end;

destructor TSaveNote.Destroy;
begin
    if OutStream <> Nil then begin
        debugln('ERROR - ID=' + ID + '  outstream was not routinly freed .....');
        OutStream.Free;
        OutStream := Nil;
    end;
end;

function TSaveNote.SetFontXML(FontSize : TFontRange; s : String) : string;
begin
    Result := s;
    if FontSize = TFontRange.FontHuge
    then Result  := '<size:huge>' + s + '</size:huge>'
    else if FontSize = TFontRange.FontBig then Result  := '<size:large>' + s + '</size:large>'
    else if FontSize = TFontRange.FontSmall then Result  := '<size:small>' + s + '</size:small>';
end;


function TSaveNote.AddTag(const FT : TKMemoTextBlock; var Buff : ANSIString; CloseOnly : boolean = False) : ANSIString;
{var
   TestVar : Boolean;}
begin
    // Important that we keep the tag order consistent. Good xml requires no cross over
    // tags. If the note is to be readable by Tomboy, must comply. (EditBox does not care)
    // Tag order -
    // FontSize HiLite Ital Bold Bullet TEXT BulletOff BoldOff ItalOff HiLiteOff FontSize
	// Processing Order is the reverese -
    // ListOff BoldOff ItalicsOff HiLiteOff FontSize HiLite Ital Bold List

    //debugln(BlockAttributes(FT));

  // When Bold Turns OFF
    if (Bold and (not (fsBold in FT.TextStyle.Font.Style))) then begin
        Buff := Buff + '</bold>';
        Bold := false;
    end;

    // When Italic turns OFF
    if (Italics and (not (fsItalic in FT.TextStyle.Font.Style))) then begin
		if Bold then Buff := Buff + '</bold>';
        Buff := Buff + '</italic>';
        if Bold then Buff := Buff + '<bold>';
        Italics := false;
    end;

    // When Highlight turns OFF
    if (HiLight and (not (FT.TextStyle.Brush.Color = HiColour))) then begin
		if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        Buff := Buff + '</highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        HiLight := false;
    end;

    // When Underline turns OFF
    if (Underline and (not (fsUnderline in FT.TextStyle.Font.Style))) then begin
                  if Bold then Buff := Buff + '</bold>';
                  if Italics then Buff := Buff + '</italic>';
                  if HiLight then Buff := Buff + '</highlight>';
                  Buff := Buff + '</underline>';
                  if HiLight then Buff := Buff + '<highlight>';
                  if Italics then Buff := Buff + '<italic>';
                  if Bold then Buff := Buff + '<bold>';
                  Underline := false;
    end;

    // When Strikeout turns OFF
    if (Strikeout and (not (fsStrikeout in FT.TextStyle.Font.Style))) then begin
                  if Bold then Buff := Buff + '</bold>';
                  if Italics then Buff := Buff + '</italic>';
                  if HiLight then Buff := Buff + '</highlight>';
                  if Underline then Buff := Buff + '</underline>';
                  Buff := Buff + '</strikeout>';
                  if Underline then Buff := Buff + '<underline>';
                  if HiLight then Buff := Buff + '<highlight>';
                  if Italics then Buff := Buff + '<italic>';
                  if Bold then Buff := Buff + '<bold>';
                  Strikeout := false;
    end;

    // When FixedWidth turns OFF
    //if (FixedWidth <> (FT.TextStyle.Font.Pitch = fpFixed) or (FT.TextStyle.Font.Name = MonospaceFont)) then begin
    if (FixedWidth and ((FT.TextStyle.Font.Pitch <> fpFixed) or (FT.TextStyle.Font.Name <> MonospaceFont))) then begin
                  //TestVar := (FT.TextStyle.Font.Pitch <> fpFixed);
                  //TestVar := (FT.TextStyle.Font.Name <> MonospaceFont);
                  if Bold then Buff := Buff + '</bold>';
                  if Italics then Buff := Buff + '</italic>';
                  if HiLight then Buff := Buff + '</highlight>';
                  if Underline then Buff := Buff + '</underline>';
                  if Strikeout then Buff := Buff + '</strikeout>';
                  Buff := Buff + '</monospace>';
                  if Strikeout then Buff := Buff + '<strikeout>';
                  if Underline then Buff := Buff + '<underline>';
                  if HiLight then Buff := Buff + '<highlight>';
                  if Italics then Buff := Buff + '<italic>';
                  if Bold then Buff := Buff + '<bold>';
                  FixedWidth := false;
    end;

    // When Font size changes
    if (FSize <> FT.TextStyle.Font.Size) and (FT.TextStyle.Font.Size <> FontSizeTitle) then begin
		if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        if HiLight then Buff := Buff + '</highlight>';
        if Underline then Buff := Buff + '</underline>';                      //
        if Strikeout then Buff := Buff + '</strikeout>';                      //
                              // if strikeout, underline, fixedwidth here?

        //Buff := Buff + SetFontXML(FSize, false); -> JOAN : SHALL NOT BE COMMENTED

        // better for pretty tags but generates invalid tags ! See below ....
        //Buff := Buff + SetFontXML(FT.TextStyle.Font.Size, true); -> JOAN : SHALL NOT BE COMMENTED

        if Strikeout then Buff := Buff + '<strikeout>';                         //
        if Underline then Buff := Buff + '<underline>';                         //

        if HiLight then Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        FSize := FT.TextStyle.Font.Size;
    end;

    if CloseOnly then exit(Buff);
    // this is not ideal, it should happen after we have closed all fonts, before
    // we write new sizes but that difficult as we have only one flag, "FSize"
    // difficulity is that font size change is two step, other things are On/Off
    // Result is that xml tag for a new font jumps up blank lines. Not pretty
    // be nice to find another way..... DRB

    // FixedWidth turns ON
    if ((not FixedWidth) and ((FT.TextStyle.Font.Name = MonospaceFont) or (FT.TextStyle.Font.Pitch = fpFixed))) then begin
        //TestVar := (FT.TextStyle.Font.Name = MonospaceFont);
        //TestVar :=  (FT.TextStyle.Font.Pitch = fpFixed);
        if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        if HiLight then Buff := Buff + '</highlight>';
        if Underline then Buff := Buff + '</underline>';
        if Strikeout then Buff := Buff + '</strikeout>';
        Buff := Buff + '<monospace>';
        if Strikeout then Buff := Buff + '<strikeout>';
        if Underline then Buff := Buff + '<underline>';
        if HiLight then Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        FixedWidth := true;
    end;

    // Strikeout turns ON
    if ((not Strikeout) and (fsStrikeout in FT.TextStyle.Font.Style)) then begin
        if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        if HiLight then Buff := Buff + '</highlight>';
        if Underline then Buff := Buff + '</underline>';
        Buff := Buff + '<strikeout>';
        if Underline then Buff := Buff + '<underline>';
        if HiLight then Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        Strikeout := true;
    end;


    // Underline turns ON
    if ((not Underline) and (fsUnderline in FT.TextStyle.Font.Style)) then begin
       if not FT.ClassNameIs('TKMemoHyperlink') then begin                            // Hyperlinks also have underline, don't save
           if Bold then Buff := Buff + '</bold>';
           if Italics then Buff := Buff + '</italic>';
           if HiLight then Buff := Buff + '</highlight>';
           Buff := Buff + '<underline>';
           if HiLight then Buff := Buff + '<highlight>';
           if Italics then Buff := Buff + '<italic>';
           if Bold then Buff := Buff + '<bold>';
           Underline := true;
       end;
    end;

    // Highlight turns ON
    if ((not HiLight) and (FT.TextStyle.Brush.Color = HiColour)) then begin
        if Bold then Buff := Buff + '</bold>';
        if Italics then Buff := Buff + '</italic>';
        Buff := Buff + '<highlight>';
        if Italics then Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        HiLight := true;
    end;

    // Italic turns On
    if ((not Italics) and (fsItalic in FT.TextStyle.Font.Style)) then begin
        if Bold then Buff := Buff + '</bold>';
        Buff := Buff + '<italic>';
        if Bold then Buff := Buff + '<bold>';
        Italics := true;
    end;

    // Bold turns On
    if ((not Bold) and (fsBold in FT.TextStyle.Font.Style)) then begin
        Buff := Buff + '<bold>';
        Bold := true;
    end;
    Result := Buff;
end;

	{ This function takes an existing parsed string and wraps it in the necessary
      bullet tags but has to resolve any pending formatting tags first and restore
      then afterwards. Its horrible. If you are debugging this, I am truly sorry.
    }
    // ListOff BoldOff ItalicsOff HiLiteOff FontSize HiLite Ital Bold List

procedure TSaveNote.BulletList(var Buff : ANSIString);
var
   StartStartSt, StartEndSt, EndStartSt, EndEndSt : ANSIString;
begin
	//writeln('Status Bold=', Bold=True, ' PBold=', PrevBold=True, ' High=', HiLight=True, ' PHigh=', PrevHiLight=True);
    StartStartSt := '';
    StartEndSt := '';
    EndStartSt := '';
    EndEndSt := '';
    if PrevBold then begin
        StartStartSt := '</bold>';     // Starting String, Start
        StartEndSt := '<bold>';        // Starting String, End
	end;
	if Bold then begin
        EndStartSt := '</bold>';		// End String, start of it
        EndEndSt := '<bold>';			// End String, end of it
    end;
    if PrevItalics then begin
        StartStartSt := StartStartSt + '</italic>';
        StartEndSt := '<italic>' + StartEndSt;
	end;
	if Italics then begin
        EndStartSt := EndStartSt + '</italic>';
        EndEndSt := '<italic>' + EndEndSt;
	end;
    if PrevHiLight then begin
        StartStartSt := StartStartSt + '</highlight>';
        StartEndSt := '<highlight>' + StartEndSt;
	end;
	if HiLight then begin
        EndStartSt := EndStartSt + '</highlight>';
        EndEndSt := '<highlight>' + EndEndSt;
    end;
    if PrevUnderline then begin
        StartStartSt := StartStartSt + '</underline>';
        StartEndSt := '<underline>' + StartEndSt;
        // EndEndSt := '<underline>' + EndEndSt;
    end;
    if Underline then begin
        EndStartSt := EndStartSt + '</underline>';
        EndEndSt := '<underline>' + EndEndSt;
    end;
    if PrevStrikeout then begin
        StartStartSt := StartStartSt + '</strikeout>';
        EndEndSt := '<strikeout>' + EndEndSt;
    end;
    if Strikeout then begin
        EndStartSt := EndStartSt + '</strikeout>';
        EndEndSt := '<strikeout>' + EndEndSt;
    end;
    if PrevFixedWidth then begin
        StartStartSt := StartStartSt + '</monospace>';
        StartEndSt := '<monospace>' + StartEndSt;
        // EndEndSt := '<monospace>' + EndEndSt;
    end;
    if FixedWidth then begin
        EndStartSt := EndStartSt + '</monospace>';
        EndEndSt := '<monospace>' + EndEndSt;
    end;
    //if PrevFSize <> FontSizeNormal then begin
    //    StartStartSt := StartStartSt + SetFontXML(PrevFSize, False);
    //    StartEndSt := SetFontXML(PrevFSize, True) + StartEndSt;
//	end;
    //if FSize <> FontSizeNormal then begin
    //    EndStartSt := EndStartSt + SetFontXML(FSize, False);
    //    EndEndSt := SetFontXML(FSize, True) + EndEndSt;
//	end;

    {writeLn('Buff at Start [' + Buff + ']');
    writeln('StartStart    [' + StartStartSt + ']');
    writeln('StartEnd      [' + StartEndSt + ']');
    writeln('EndStart      [' + EndStartSt + ']');
    writeln('EndEnd        [' + EndEndSt + ']');          }

    Buff := StartStartSt + '<list><list-item dir="ltr">' + StartEndSt
    		+ Buff + EndStartSt + '</list-item></list>' + EndEndSt;

    //writeLn('Buff at End [' + Buff + ']');         // **************************************
end;

// This is just a debug function.
function TSaveNote.BlockAttributes(Bk : TKMemoBlock) : AnsiString;
begin
   Result := TKMemoTextBlock(BK).ClassName;
   if fsBold in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Bold ';
   if fsItalic in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Italic ';
   if TKMemoTextBlock(BK).TextStyle.Brush.Color = HiColour then
   Result := Result + ' HighLight ';
   Result := Result + ' size=' + inttostr(TKMemoTextBlock(BK).TextStyle.Font.Size);
   if fsUnderline in TKMemoTextBlock(BK).TextStyle.Font.Style then
        Result := Result + ' Underline ';
   if fsStrikeout in TKMemoTextBlock(BK).TextStyle.Font.Style then
   Result := Result + ' Strikeout ';
   if TKMemoTextBlock(BK).TextStyle.Font.Pitch = fpFixed then
   Result := Result + ' FixedWidth ';
   if TKMemoTextBlock(BK).ClassNameIs('TKMemoTextBlock') then Result := Result + ' [' + TKMemoTextBlock(BK).Text + ']';
   //else Result := 'Not Text';
end;

    // I suspect this function is no longer used.
function TSaveNote.FontAttributes(const Ft : TFont; Ts : TKMemoTextStyle) : ANSIString;
begin
   Result := '';
   if fsBold in Ft.Style then
   Result := Result + ' Bold ';
   if fsItalic in Ft.Style then
   Result := Result + ' Italic ';
   if Ts.Brush.Color = HiColour then
   Result := Result + ' HighLight ';
   Result := Result + inttostr(Ft.Size);
   if fsUnderline in Ft.Style then
   Result := Result + ' Underline ';
   if fsStrikeout in Ft.Style then
   Result := Result + ' Strikeout ';
   if Ft.Pitch = fpFixed then
   Result := Result + ' FixedWidth ';
end;

procedure TSaveNote.SaveNewTemplate(NotebookName : ANSIString);
var
   GUID : TGUID;
   OStream:TFilestream;
   Buff { TemplateID }: ANSIString;
   Loc :  TNoteUpdateRec {TNoteLocation};
begin
   CreateGUID(GUID);
   Title := NotebookName  + ' Template';
   ID := copy(GUIDToString(GUID), 2, 36) ;
   SearchForm.NoteLister.AddNoteBook(ID, NotebookName, True);
   Ostream :=TFilestream.Create(GetLocalNoteFile(ID), fmCreate);
   Loc.Y := '20'; Loc.X := '20'; Loc.Height := '200'; Loc.Width:='300';
   Loc.OOS := 'False'; Loc.CPos:='1';
   try
        Buff := Header();
        OStream.Write(Buff[1], length(Buff));
        Buff := Title + #10#10#10;
        OStream.Write(Buff[1], length(Buff));
        Buff := Footer(Loc);
        OStream.Write(Buff[1], length(Buff));
   finally
       OStream.Free;
   end;
end;

procedure TSaveNote.CopyLastFontAttr();
begin
  PrevFSize := FSize;
  PrevBold := Bold;
   PrevItalics := Italics;
   PrevHiLight := HiLight;
  PrevUnderline := Underline;
  PrevStrikeout := Strikeout;
  PrevFixedWidth := FixedWidth;
  PrevFSize := FSize;
end;

procedure TSaveNote.ReadKMemo(FileName : ANSIString; KM1 : TKMemo);
var
   Buff : ANSIstring = '';
   // OutStream:TFilestream;
   BlockNo : integer = 0;
   Block : TKMemoBlock;
   NextBlock : integer;
   // BlankFont : TFont;
 begin
    KM := KM1;
    FSize := FontSizeNormal;
    Bold := false;
     Italics := False;
     HiLight := False;
    Underline := False;
     InList := false;
    FixedWidth := False;
    ID := ExtractFileNameOnly(FileName) + '.note';
    // ID needs to be set so we can get list of notebooks for the footer.
    // Must deal with an empty list !
//    try
        outstream :=TMemoryStream.Create({FileName, fmCreate});
        // Write and WriteBuffer accept a buffer, not a string !  Need to start at pos 1
        // when sending string or ANSIstring otherwise it uses first byte which makes it look like a binary file.
        // http://free-pascal-general.1045716.n5.nabble.com/Creating-text-files-with-TFileStream-td2824859.html
        Buff := Header();
        OutStream.Write(Buff[1], length(Buff));
        Buff := '';
        try
            repeat
                CopyLastFontAttr();
                repeat
                    Block := KM1.Blocks.Items[BlockNo];
                    // debugln('Block=' + inttostr(BlockNo) + ' ' +BlockAttributes(Block));

                    if Block.ClassNameIs('TKMemoParagraph') then break;	// discard end prev para
                    if Block.ClassNameIs('TKMemoTextBlock') then begin
                         if Block.Text.Length > 0 then begin
                        	AddTag(TKMemoTextBlock(Block), Buff);
                        	Buff := Buff + RemoveBadXMLCharacters(Block.Text);
						 end;
					end;
                    if Block.ClassNameIs('TKMemoHyperlink') then begin
                        AddTag(TKMemoHyperlink(Block), Buff);
                        Buff := Buff + RemoveBadXMLCharacters(Block.Text);
                    end;
                    //debugln('Block=' + inttostr(BlockNo) + ' ' +BlockAttributes(Block));
                    inc(BlockNo);
                    if BlockNo >= KM1.Blocks.Count then break;

                    // debugln('Inner Buff=[' + Buff + ']');

				until KM1.Blocks.Items[BlockNo].ClassNameIs('TKMemoParagraph');
                if BlockNo >= KM1.Blocks.Count then break;
                if  TKMemoParagraph(KM1.Blocks.Items[BlockNo]).Numbering = pnuBullets then
                     BulletList(Buff);

                // Add tags about to terminate to end of line, pretty XML
                // However does not work for font size changes !

                // Note - para blocks CAN have font attributs (eg, underline etc).
                // debugln('Outer 1 Buff=[' + Buff + ']');
                // Now, look ahead and see if we need close things ....
                // This makes bad decision for font size changes, we end up with empty tags but does no real harm.
                NextBlock := BlockNo + 1;
                while NextBlock < KM1.Blocks.Count do begin
                    if KM1.Blocks.Items[NextBlock].ClassNameIs('TKMemoTextBlock') then begin
                        AddTag(TKMemoTextBlock(KM1.Blocks.Items[NextBlock]), Buff, True);
                        break;
                    end else inc(NextBlock);
                end;
                Buff := Buff + LineEnding;
                // debugln('Outer Buff=[' + Buff + ']');
                OutStream.Write(Buff[1], length(Buff));
                Buff := '';
                // debugln('Block=' + inttostr(BlockNo) + ' ' +BlockAttributes(KM1.Blocks.Items[BlockNo]));
                inc(BlockNo);
                if BlockNo >= KM1.Blocks.Count then break;
			until false;

            { At this point we may have unsaved content in Buff cos last block was not
              a Para. But it cannot be Bullet. If it was a Para, Buff is empty. But we
              could still have hanging xml tags. So either case, send it to add tag with
              an empty Font.
            }
            if Buff <> '' then
                OutStream.Write(Buff[1], length(Buff));
            Buff := '';
            if Bold then Buff := '</bold>';
            if Italics then Buff := Buff + '</italic>';
            if HiLight then Buff := Buff + '</highlight>';
            if Underline then Buff := Buff + '</underline>';
            if Strikeout then Buff := Buff + '</strikeout>';
            if FixedWidth then Buff := Buff + '</monospace>';
            //if FSize <> FontSizeNormal then
            //     Buff := Buff + SetFontXML(FSize, False);
            if length(Buff) > 0 then
                  OutStream.Write(Buff[1], length(Buff));
            //Buff := Footer();
            //OutStream.Write(Buff[1], length(Buff));

         Except     { TODO 1 : Must test this to see what happens with an empty
         				list of blocks. Probably makes sense to not save anything
                        that does not have at least one TKMemotextBlock  }
            on EListError do begin
                debugln('ERROR - EListError while writing note to stream.');
                { we now do footer in the WriteToDisk()
            	Buff := Footer();
            	OutStream.Write(Buff[1], length(Buff)); }
            end;
        end;
{ 	finally
        OutStream.Free;
    end;       }
end;

// gets called (from outside) after all content assembled.  Its done from outside
// as the calling unit has control of KMemo's locking.
function TSaveNote.WriteToDisk(const FileName: ANSIString; var NoteLoc : TNoteUpdateRec) : boolean;
var
   Buff : string = '';
   TmpName : string;
   {$ifdef WINDOWS}FileAttr : longint;
   ErrorMsg : string; {$endif}
begin

    Result := True;
    // we write out the footer here so we can do the searching to notebook stuff
    // after we have released to lock on KMemo.
    Buff := Footer(NoteLoc);
    OutStream.Write(Buff[1], length(Buff));
    // We save the file in tmp, when closed,
    // move it over to the actual position. That will prevent, to some extent, poweroff
    // crashes messing with files.  May generate an EStreamError

    {$define STAGEDWRITE}
    {$ifdef STAGEDWRITE}
        TmpName := AppendPathDelim(NotesDir) + 'tmp';
        if not DirectoryExists(TmpName) then
           if not CreateDir(AppendPathDelim(tmpname)) then begin
                NoteLoc.ErrorStr:='Failed Create Dir';
                exit(False);
            end;
        TmpName := TmpName + pathDelim + extractFileName(FileName);
        try
            OutStream.SaveToFile(TmpName);
        finally
            OutStream.Free;
            OutStream := nil;
        end;
        {$ifdef WINDOWS}
            if FileExists(FileName) then    // will not be there if its a new note.
                if not SafeWindowsDelete(FileName, ErrorMsg) then
                   exit(false);
        {$endif}
        result := RenameFileUTF8(TmpName, FileName);    // Unix ok to over write, windows is not !
    {$else}        // thats the ifdef StagedWrite, here we write directly to note file.
        try
            OutStream.SavetoFile(FileName);
        finally
            OutStream.Free;
            OutStream := nil;
        end;
    {$endif}                                        // thats the ifdef StagedWrite
    if not Result then NoteLoc.ErrorStr:='Failed Rename';
end;

function TSaveNote.Header(): ANSIstring;
var
   S1, S2, S3, S4 : ANSIString;
begin
  S1 := '<?xml version="1.0" encoding="utf-8"?>'#10'<note version="0.3" xmlns:link="';
  S2 := 'http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size"';
  S3 := ' xmlns="http://beatniksoftware.com/tomboy">'#10'  <title>';
  S4 := '</title>'#10'  <text xml:space="preserve"><note-content version="0.1">';
  Result := S1 + S2 + S3 + RemoveBadXMLCharacters(Title) + S4;
end;


function TSaveNote.Footer(Loc : TNoteUpdateRec {TNoteLocation}): ANSIstring;
var
   S1, S2, S3, S4, S5, S6 : string;
begin
  if Loc.LastChangeDate = '' then
    TimeStamp := GetCurrentTimeStr()   // get actual time date in format like Tomboy's
  else TimeStamp := Loc.LastChangeDate;
  S1 := '</note-content></text>'#10'  <last-change-date>';
  S2 := '</last-change-date>'#10'  <last-metadata-change-date>';
  S3 := '</last-metadata-change-date>'#10'  <create-date>';
  S4 := '</create-date>'#10'  <cursor-position>' + Loc.CPos + '</cursor-position>'#10'  <selection-bound-position>1</selection-bound-position>'#10;
  S5 := '  <width>' + Loc.Width + '</width>'#10'  <height>' + Loc.Height + '</height>'#10'  <x>'
        + Loc.X + '</x>'#10'  <y>' + Loc.Y + '</y>'#10;
  S6 := '  <open-on-startup>' + Loc.OOS + '</open-on-startup>'#10'</note>';
  if CreateDate = '' then CreateDate := TimeStamp;
  if SearchForm.NoteLister <> Nil then
        Result := S1 + TimeStamp + S2 + TimeStamp + S3 + CreateDate + S4 + S5
            + SearchForm.NoteLister.NoteBookTags(ID) + S6
  else
        Result := S1 + TimeStamp + S2 + TimeStamp + S3 + CreateDate + S4 + S5 + S6;
  // ToDo : That will mean no Notebook tags in single note mode, is that an issue ?
  // Most singe notes are out of their repo so won't have notebooks anyway but we could
  // save any tag list and restore it on save ??
end;

end.
