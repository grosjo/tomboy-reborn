unit TRtransportFile;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, LazLogger,
    TRcommon, TRtransport;

type TFileSync = Class(TTomboyTrans)
    private
        function GetRemoteNotePath(Rev: integer; NoteID : string = ''): string;
        function GetRemoteNoteLastChange(const ID : string; rev : Integer; out Error : string) : string;
    public
        function TestTransport() : TSyncStatus; override;
        function GetNotes(const NoteMeta : TNoteInfoList) : boolean; override;
        function PushChanges(notes : TNoteInfoList): boolean; override;
        function DoRemoteManifest(const RemoteManifest : string) : boolean; override;
        function IDLooksOK() : boolean; Override;
        function getPrefix(): string; Override;
    end;


implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil;


function TFileSync.getPrefix(): string;
begin
  Result := 'file';
end;

function TFileSync.TestTransport(): TSyncStatus;
var
    Doc : TXMLDocument;
    repo : String;
    ManExists, ZeroExists : boolean; // for readability of code only
begin
    debugln('TransportFIle : TestTransport');

    setParam('RemoteAddess',AppendPathDelim(ChompPathDelim(getParam('RemoteAddress'))));
    repo := getParam('RemoteAddress');

    if not DirectoryExists(repo) then
        if not DirectoryExists(repo) then begin    // try again because it might be just remounted.
           ErrorString := 'Remote Dir does not exist : ' + repo;
	   exit(SyncNoRemoteEnd);
        end;

    if not DirectoryIsWritable(repo) then begin
      ErrorString := 'Remote directory NOT writable ' + repo;
      exit(SyncNoRemoteWrite);
    end;

    ManExists := FileExists(repo + 'manifest.xml');
    ZeroExists := DirectoryExists(repo + '0');

    if (ManExists) and (not ZeroExists) then
    begin
        ErrorString := 'Apparently damaged repo, missing 0 dir at ' + repo;
    	exit(SyncBadRemote);
    end;
    if (not ManExists) and (ZeroExists) then
    begin
        ErrorString := 'Apparently damaged repo, missing manifest at ' + repo;
    	exit(SyncBadRemote);
    end;

    if (not ManExists) and (not ZeroExists) then
    begin
        ServerID := GetNewID();
        ServerRev := -1;
        GetRemoteNotePath(0);
        exit(SyncReady);
    end;

    debugln('Reading XML to search GUID/Rev');
    try
       ReadXMLFile(Doc, repo + 'manifest.xml');
    except on E:Exception do
       begin
           ErrorString := E.message;
           exit(SyncXMLERROR);
       end;
    end;

    debugln('Finalizing');
    try
       ServerID := Doc.DocumentElement.GetAttribute('server-id');
       ServerRev := strtoint(Doc.DocumentElement.GetAttribute('revision'));
    except on E:Exception do
       begin
            ErrorString := E.message;
            Doc.Free;
            exit(SyncXMLERROR);
       end;
    end;
    Doc.Free;

    if not IDLooksOK()
    then begin
        ErrorString := 'Invalid ServerID '+ServerID;
        exit(SyncXMLError);
    end;

    debugln('ServerID : '+ServerID + ' Rev='+IntToStr(ServerRev));

    Result := SyncReady;
end;

function TFileSync.GetNotes(const NoteMeta: TNoteInfoList): boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    NoteInfo : PNoteInfo;
    manifest,note : String;
begin
    debugln('TransportFile : Get Notes');

    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNotes()';
        exit(False);
    end;

    manifest:= getParam('RemoteAddress') + 'manifest.xml';
    if not FileExists(manifest) then
    begin
        debugln('Manifest notexistant : '+manifest);
        exit(true);
    end;

    debugln('Read XML : '+manifest);
    try
         ReadXMLFile(Doc, manifest);
    except on E:Exception do begin debugln(E.message); exit(false); end;
    end;

    NodeList := Doc.DocumentElement.ChildNodes;

    if not assigned(NodeList) then
    begin
         Doc.Free;
         debugln('We failed to read XML children in the remote manifest file '+manifest);
         exit(false);
    end;

    debugln('Found '+IntToStr(NodeList.Count) + ' remote notes');
    for j := 0 to NodeList.Count-1 do
    begin
         debugln('new Note '+IntToStr(j));
         new(NoteInfo);

         NoteInfo^.Action:=SynUnset;
         Node := NodeList.Item[j].Attributes.GetNamedItem('guid');
         if(not assigned(Node)) then
         begin
              debugln('Wrong XML syntax -> "guid" not found');
              Node := NodeList.Item[j].Attributes.GetNamedItem('id');
              if(not assigned(Node)) then
              begin
                   debugln('Wrong XML syntax -> even "id" not found');
                   Dispose(NoteInfo);
                   continue;
              end;
         end;
         NoteInfo^.ID := Node.NodeValue;
         debugln('Note ID = ' + NoteInfo^.ID);
         If(not NoteIdLooksOk(NoteInfo^.ID)) then
         begin
              debugln('Note with wrong ID');
              dispose(NoteInfo);
              continue;
         end;

         Node := NodeList.Item[j].Attributes.GetNamedItem('latest-revision');
         if(not assigned(Node)) then
         begin
              debugln('Wrong XML syntax -> "latest-revision" not found');
              NoteInfo^.Rev := ServerRev;
         end
         else NoteInfo^.Rev := strtoint(Node.NodeValue);

         note := GetRemoteNotePath(NoteInfo^.Rev, NoteInfo^.ID);
         debugln('File to note from '+note);
         if(FileToNote(note, NoteInfo ))
              then NoteMeta.Add(NoteInfo)
              else dispose(NoteInfo);
    end;

    Doc.Free;

    Debugln('Transfile.ReadRemoteManifest - read OK');
    Result := true;

end;



function TFileSync.PushChanges(notes : TNoteInfoList): boolean;
var
    i : integer;
    d,n : string;
    f : TStringList;
    note : PNoteInfo;
    ok : boolean;
begin
   d := GetRemoteNotePath(ServerRev + 1);

   ok := true;
   for i := 0 to notes.Count -1 do
   begin
       note := notes.Items[i];
       if(not (note^.Action in [SynUploadEdit, SynUploadNew])) then continue;

       n := GetRemoteNotePath(ServerRev + 1,note^.ID);

       f := TStringList.Create;
       f.Add('<?xml version="1.0" encoding="utf-8"?>');
       f.Add('<note version="' + note^.Version + '" xmlns:link="http://beatniksoftware.com/tomboy/link" xmlns:size="http://beatniksoftware.com/tomboy/size" xmlns="http://beatniksoftware.com/tomboy">');
       f.Add('<title>' + note^.Title + '</title>');
       f.Add('<create-date>' + note^.CreateDate + '</create-date>');
       f.Add('<last-change-date>' + note^.LastChange + '</last-change-date>');
       f.Add('<last-metadata-change-date>' + note^.LastMetaChange + '</last-metadata-change-date>');
       f.Add('<width>' + IntToStr(note^.Width) + '</width>');
       f.Add('<height>' + IntToStr(note^.Height) + '</height>');
       f.Add('<x>' + IntToStr(note^.X) + '</x>');
       f.Add('<y>' + IntToStr(note^.Y) + '</y>');
       f.Add('<selection-bound-position>' + IntToStr(note^.SelectBoundPosition) + '</selection-bound-position>');
       f.Add('<cursor-position>' + IntToStr(note^.CursorPosition) + '</cursor-position>');
       f.Add('<pinned>' + BoolToStr(note^.Pinned) + '</pinned>');
       f.Add('<open-on-startup>' + BoolToStr(note^.OpenOnStartup) + '</open-on-startup>');
       f.Add('<text xml:space="preserve"><note-content version="' + note^.Version + '">' + note^.Content + '</note-content></text> ');

       debugln('Uploading ' + note^.ID + ' into '+n );
       try
          f.SaveToFile(n);
       except on E:Exception do
           begin
              ErrorString := E.message;
              debugln(ErrorString);
              ok := false;
           end;
       end;
       f.Free;
   end;
   result := ok;
end;

function TFileSync.DoRemoteManifest(const RemoteManifest: string): boolean;
var
    d : String;
    f : TextFile;
begin
    debugln('DoRemote Manifest ' + RemoteManifest);

    try
       AssignFile(f,getParam('RemoteAddress') + 'manifest.xml');
       Rewrite(f);
       Write(f,RemoteManifest);
       Close(f);

       d := GetRemoteNotePath(ServerRev + 1);

       AssignFile(f,d + 'manifest.xml');
       Rewrite(f);
       Write(f,RemoteManifest);
       Close(f)
    except on E:Exception do begin
       ErrorString := E.message;
       debugln(ErrorString);
       exit(false);
       end;
    end;

    Result := True;
end;


function TFileSync.IDLooksOK() : boolean;
begin
    if length(ServerID) <> 36 then exit(false);
    if pos('-', ServerID) <> 9 then exit(false);
    result := True;
end;

function TFileSync.GetRemoteNotePath(Rev: integer; NoteID : string = ''): string;
var
   s,path : String;
   SearchResult : TSearchRec;
begin

    path := getParam('RemoteAddress');

    //if ((Rev<0) or (FindFirst(path + '*.note',faAnyFile,SearchResult) = 0))
    if (Rev<0)
    then s := path
    else s := appendpathDelim(path
        + inttostr(Rev div 100) + pathDelim + inttostr(rev));

    ForceDirectoriesUTF8(path);

    if NoteID <> '' then
        s := s + NoteID + '.note';

    Result := s;
end;

function TFileSync.GetRemoteNoteLastChange(const ID : string; rev : Integer; out Error : string) : string;
var
   Doc : TXMLDocument;
   Node : TDOMNode;
   filename : string;
begin
   filename := GetRemoteNotePath(rev,ID);

   if not FileExists(filename) then
   begin
        Error := 'ERROR - File not found, cant read note change date for remote ' +  filename;
        exit('');
    end;

    try
       ReadXMLFile(Doc, filename);
       Node := Doc.DocumentElement.FindNode('last-change-date');
       Result := Node.FirstChild.NodeValue;
    except on E:Exception do begin
       Error := E.message;
       debugln(Error);
       Result := '';
       end;
    end;
end;

end.

