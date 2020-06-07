unit TRtransportFile;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, LazLogger,
    TRcommon, TRtransport, TRtexts;

type TFileSync = Class(TTomboyTrans)
    private
        function GetRemoteNotePath(Rev: integer; NoteID : UTF8String = ''): UTF8String;
        function GetRemoteNoteLastChange(const ID : UTF8String; rev : Integer; out Error : UTF8String) : UTF8String;
    public
        function TestTransport() : TSyncStatus; override;
        function GetNotes(const NoteMeta : TNoteInfoList) : boolean; override;
        function PushChanges(notes : TNoteInfoList): boolean; override;
        function DoRemoteManifest(const RemoteManifest : TStringList) : boolean; override;
        function IDLooksOK() : boolean; Override;
        function getPrefix(): UTF8String; Override;
    end;


implementation

uses laz2_DOM, laz2_XMLRead, LazFileUtils, FileUtil;


function TFileSync.getPrefix(): UTF8String;
begin
  Result := 'file';
end;

function TFileSync.TestTransport(): TSyncStatus;
var
    Doc : TXMLDocument;
    repo : UTF8String;
    ManExists, ZeroExists : boolean; // for readability of code only
begin
    TRlog('TransportFIle : TestTransport');

    setParam('RemoteAddess',AppendPathDelim(ChompPathDelim(getParam('RemoteAddress'))));
    repo := getParam('RemoteAddress');

    if not DirectoryExistsUTF8(repo) then
        if not DirectoryExistsUTF8(repo) then begin    // try again because it might be just remounted.
           ErrorString := 'Remote Dir does not exist : ' + repo;
	   exit(SyncNoRemoteEnd);
        end;

    if not DirectoryIsWritable(repo) then begin
      ErrorString := 'Remote directory NOT writable ' + repo;
      exit(SyncNoRemoteWrite);
    end;

    ManExists := FileExistsUTF8(repo + 'manifest.xml');
    ZeroExists := DirectoryExistsUTF8(repo + '0');

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

    TRlog('Reading XML to search GUID/Rev');
    try
       ReadXMLFile(Doc, repo + 'manifest.xml');
    except on E:Exception do
       begin
           ErrorString := E.message;
           exit(SyncXMLERROR);
       end;
    end;

    TRlog('Finalizing');
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

    TRlog('ServerID : '+ServerID + ' Rev='+IntToStr(ServerRev));

    Result := SyncReady;
end;

function TFileSync.GetNotes(const NoteMeta: TNoteInfoList): boolean;
var
    Doc : TXMLDocument;
    NodeList : TDOMNodeList;
    Node : TDOMNode;
    j : integer;
    NoteInfo : PNoteInfo;
    manifest,filename : UTF8String;
begin
    TRlog(#10 + '******* TransportFile : Get Notes');

    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNotes()';
        exit(False);
    end;

    manifest:= getParam('RemoteAddress') + 'manifest.xml';
    if not FileExistsUTF8(manifest) then
    begin
        TRlog('Manifest notexistant : '+manifest);
        exit(true);
    end;

    TRlog('Read XML : '+manifest);
    try
         ReadXMLFile(Doc, manifest);
    except on E:Exception do begin TRlog(E.message); exit(false); end;
    end;

    NodeList := Doc.DocumentElement.ChildNodes;

    if not assigned(NodeList) then
    begin
         Doc.Free;
         TRlog('We failed to read XML children in the remote manifest file '+manifest);
         exit(false);
    end;

    TRlog('Found '+IntToStr(NodeList.Count) + ' remote notes');
    for j := 0 to NodeList.Count-1 do
    begin
         TRlog('new Note '+IntToStr(j));

         NoteInfo := EmptyNote();

         NoteInfo^.Action:=SynUnset;
         Node := NodeList.Item[j].Attributes.GetNamedItem('guid');
         if(not assigned(Node)) then
         begin
              TRlog('Wrong XML syntax -> "guid" not found');
              Node := NodeList.Item[j].Attributes.GetNamedItem('id');
              if(not assigned(Node)) then
              begin
                   TRlog('Wrong XML syntax -> even "id" not found');
                   Dispose(NoteInfo);
                   continue;
              end;
         end;
         NoteInfo^.ID := Node.NodeValue;
         TRlog('Note ID = ' + NoteInfo^.ID);
         If(not NoteIdLooksOk(NoteInfo^.ID)) then
         begin
              TRlog('Note with wrong ID');
              dispose(NoteInfo);
              continue;
         end;

         Node := NodeList.Item[j].Attributes.GetNamedItem('last-revision');
         if(not assigned(Node)) then
         begin
              TRlog('Wrong XML syntax -> "last-revision" not found');
              NoteInfo^.Rev := ServerRev;
         end
         else NoteInfo^.Rev := strtoint(Node.NodeValue);

         filename := GetRemoteNotePath(NoteInfo^.Rev, NoteInfo^.ID);

         TRlog('TRANSPORT File to note from '+filename);
         FileToNote(filename, NoteInfo );

         NoteMeta.Add(NoteInfo)
    end;

    Doc.Free;

    TRlog('Transfile.ReadRemoteManifest - read OK');
    Result := true;

end;



function TFileSync.PushChanges(notes : TNoteInfoList): boolean;
var
    i : integer;
    d,n : UTF8String;
    note : PNoteInfo;
    ok : boolean;
begin
   TRlog('SyncFile Push Changes Rev=' + IntToStr(ServerRev));

   inc(ServerRev);
   d := GetRemoteNotePath(ServerRev);
   TRlog('SyncFile Push folder = '+d);

   ok := true;
   for i := 0 to notes.Count -1 do
   begin
       note := notes.Items[i];
       if(not (note^.Action in [SynUploadEdit, SynUploadNew])) then continue;

       n := GetRemoteNotePath(ServerRev,note^.ID);

       if(not NoteToFile(note, n)) then begin ErrorString := rsErrorCannotWrite + n; ok := false; end;
   end;
   result := ok;
end;

function TFileSync.DoRemoteManifest(const RemoteManifest: TStringList): boolean;
var
    d : UTF8String;
begin
   d := getParam('RemoteAddress') + 'manifest.xml';

   TRlog('DoRemote Manifest ' + d);

    try
       RemoteManifest.SaveToFile(d);
    except on E:Exception do begin
       ErrorString := E.message;
       TRlog(ErrorString);
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

function TFileSync.GetRemoteNotePath(Rev: integer; NoteID : UTF8String = ''): UTF8String;
var
   s,path : UTF8String;
begin
    TRlog('GetRemoteNotePath ( '+IntToStr(Rev)+' , '+ NoteID+' ) ');

    path := getParam('RemoteAddress');

    if (Rev<0)
    then s := path
    else s := appendpathDelim(path
        + inttostr(Rev div 100) + pathDelim + inttostr(rev));

    TRlog('Creating path '+s);
    ForceDirectoriesUTF8(s);

    if NoteID <> '' then
        s := s + NoteID + '.note';

    Result := s;
end;

function TFileSync.GetRemoteNoteLastChange(const ID : UTF8String; rev : Integer; out Error : UTF8String) : UTF8String;
var
   Doc : TXMLDocument;
   Node : TDOMNode;
   filename : UTF8String;
begin
   filename := GetRemoteNotePath(rev,ID);

   if not FileExistsUTF8(filename) then
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
       TRlog(Error);
       Result := '';
       end;
    end;
end;

end.

