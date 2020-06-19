unit TRtransportNC;

{$mode objfpc}{$H+}

interface

uses
    Classes, LazLogger, SysUtils, Dialogs, fpjson, jsonparser,
    TRcommon, TRTransport;

type TNextSync = Class(TTomboyTrans)
    public
        constructor create;
        destructor Destroy; override;
        function TestTransport(): TSyncStatus; override;
        function GetNotes(const NoteMeta : TNoteInfoList) : boolean; override;
        function PushChanges(notes : TNoteInfoList) : boolean; override;
        function DoRemoteManifest(const RemoteManifest : TStringList) : boolean; override;
        function IDLooksOK() : boolean; Override;
        function getPrefix(): UTF8String; Override;
    private
        Token, TokenSecret, Key : UTF8String;
        function JsonEscape(s : UTF8String) : UTF8String;

 end;


implementation


constructor TNextSync.create;
begin
    inherited Create;
end;

destructor TNextSync.Destroy;
begin
    inherited Destroy;
end;

function TNextSync.getPrefix() : UTF8String;
begin
  Result := 'nc';
end;

function TNextSync.TestTransport(): TSyncStatus;
var
  resturl,res : UTF8String;
  json : TJSONData;
  jObject : TJSONObject;
  p : TStrings;
  ok : boolean;
  sid : UTF8String;
  rev : integer;
begin
    WriteLn('Next-TestTransport');
    Token := getParam('TOKEN');
    TokenSecret := getParam('SECRET');
    Key := getParam('KEY');

    // TESTING AUTH
    resturl := getParam('URL') + '/api/1.0/';
    p := TstringList.Create();
    OauthBaseParams(p,Key,Token);
    OauthParamsSort(p);
    OauthSign(resturl, 'GET', p,Key,TokenSecret);
    res := WebGet(resturl,p);
    TRlog(res);
    FreeAndNil(p);

    if (res = '') then begin ErrorString :=  'Next-TestTransport: Unable to et initial data'; exit(SyncBadError); end;

    ok := true;
    ErrorString := '';

    try
       json := GetJSON(res);

       json := json.Items[4];
       jObject := TJSONObject(json);
       res := jObject.Get('api-ref');
       FreeAndNil(jObject);
    except on E:Exception do begin
       ErrorString := E.message;
       ok:= false;
       end;
    end;
    if (not ok) then begin ErrorString :=  'Next-TestTransport: '+ErrorString; exit(SyncBadError); end;
    if(length(res)<10) then begin ErrorString :=  'Next-TestTransport: Server returns invalid OAuth URLs '+ErrorString; exit(SyncBadError); end;

    // YEAH
    setParam('URLUSER',res);
    p := TstringList.Create();
    OauthBaseParams(p,Key,Token);
    OauthParamsSort(p);
    OauthSign(res, 'GET', p,Key,TokenSecret);
    res := WebGet(res,p);
    FreeAndNil(p);
    ok := true;
    ErrorString := '';
    try
       json := GetJSON(res);
       jObject := TJSONObject(json);
       sid := jObject.Get('current-sync-guid');
       rev := StrToInt(jObject.Get('latest-sync-revision'));
       FreeAndNil(jObject);

       json := GetJSON(res);
       json:= json.Items[3];
       jObject := TJSONObject(json);
       res := jObject.Get('api-ref');
       setParam('URLNOTES',res);
       FreeAndNil(jObject);
    except on E:Exception do begin
       ErrorString := E.message;
       ok:= false;
       end;
    end;
    if (not ok) then begin ErrorString :=  'Next-TestTransport: '+ErrorString; exit(SyncBadError); end;
    if(length(res)<10) then begin ErrorString :=  'Next-TestTransport: Server returns invalid OAuth URLs '+ErrorString; exit(SyncBadError); end;

    ServerID:= sid;
    ServerRev := rev;

    ErrorString :=  '';

    if not IDLooksOK() then begin
        ErrorString := 'Invalid ServerID '+ServerID;
        exit(SyncBadRemote);
    end;

    Result := SyncReady;
end;


function TNextSync.GetNotes(const NoteMeta: TNoteInfoList): boolean;
var
  res : UTF8String;
  json: TJSONData;
  jObject : TJSONObject;
  jnotes,jtags : TJSONArray;
  p : TStrings;
  ok : boolean;
  nbnotes,i,j : integer;
  NoteInfo : PNoteInfo;
  d : double;
begin
    WriteLn('Next-GetNotes');

    if NoteMeta = Nil then begin
        ErrorString := 'Passed an uncreated list to GetNotes()';
        exit(False);
    end;

    // HTTP REQUEST
    res := getParam('URLNOTES');
    p := TstringList.Create();
    OauthBaseParams(p,Key,Token);
    p.Add('include_notes');
    p.Add('true');
    OauthParamsSort(p);
    OauthSign(res, 'GET', p, Key, TokenSecret);
    res := WebGet(res,p);
    FreeAndNil(p);

    if (res = '') then begin ErrorString :=  'Next-GetNotes: Unable to get initial data'; exit(false); end;

    ok := true;
    ErrorString := '';
    try
       json := GetJSON(res);
       jObject := TJSONObject(json);
       i:= jObject.Get('latest-sync-revision',0);
       if(i>ServerRev) then ServerRev :=i;

       jnotes :=  jObject.Get('notes',jnotes);
    except on E:Exception do begin
       ErrorString := E.message;
       TRlog(ErrorString);
       ok:= false;
       end;
    end;

    if (not ok) then begin ErrorString :=  'Next-GetNotes: '+ErrorString; TRlog(ErrorString); exit(false); end;

    nbnotes := jnotes.Count;
    TRlog('Nb notes '+IntToStr(nbnotes));

    ok :=true;

    i:=0;
    while(ok and (i<nbnotes)) do
    begin
       new(NoteInfo);

       try
          TRlog('Note '+IntToStr(i) + ' / ' + IntToStr(nbnotes));

          json := jnotes.Items[i];
          jObject := TJSONObject(json);

          NoteInfo^.Action:=SynUnset;
          NoteInfo^.ID := jObject.Get('guid');
          NoteInfo^.Rev := jObject.Get('last-sync-revision',-1);

          NoteInfo^.CreateDate:=jObject.Get('create-date','');
          if NoteInfo^.CreateDate <> '' then
             NoteInfo^.CreateDateGMT := GetGMTFromStr(NoteInfo^.CreateDate);
          NoteInfo^.LastChange:=jObject.Get('last-change-date','');
          if NoteInfo^.LastChange <> '' then
             NoteInfo^.LastChangeGMT := GetGMTFromStr(NoteInfo^.LastChange);
          NoteInfo^.LastMetaChange:=jObject.Get('last-metadata-change-date','');
          if NoteInfo^.LastMetaChange <> '' then
             NoteInfo^.LastMetaChangeGMT := GetGMTFromStr(NoteInfo^.LastMetaChange);


          res := jObject.Get('note-content-version');
          d := StrToFloat(res);
          j := round(d*10);
          d := j * 0.1;
          NoteInfo^.Version := Format('%0.1f',[d]);

          NoteInfo^.Deleted := false;
          NoteInfo^.Title := jObject.Get('title','');
          NoteInfo^.Content := jObject.Get('note-content','');

          NoteInfo^.OpenOnStartup := jObject.Get('open-on-startup',false);

          NoteInfo^.Pinned := jObject.Get('pinned',false);

          NoteInfo^.CursorPosition := jObject.Get('cursor-position',1);

          NoteInfo^.SelectBoundPosition := jObject.Get('selection-bound-position',0);

          NoteInfo^.Width := jObject.Get('width',0);
          NoteInfo^.Height := jObject.Get('height',0);
          NoteInfo^.X := jObject.Get('x',-1);
          NoteInfo^.Y := jObject.Get('y',-1);

          jtags := jObject.Get('tags',jtags);

          NoteInfo^.Tags := TStringList.Create;
          j:=0;
          while(j< jtags.Count) do
          begin
             res:= jtags.Items[j].AsString;
             NoteInfo^.Tags.Add(res);
             inc(j);
          end;

       except on E:Exception do begin ok := false; TRlog(E.message); end;
       end;

       if(ok) then NoteMeta.Add(NoteInfo)
       else Dispose(NoteInfo);

       i := i+1;
    end;

    FreeAndNil(jnotes);

    result := ok;
end;

function TNextSync.JsonEscape(s : UTF8String) : UTF8String;
var
   r : UTF8String;
   i : integer;
begin
{
    Backspace is replaced with \b
    Form feed is replaced with \f
    Newline is replaced with \n
    Carriage return is replaced with \r
    Tab is replaced with \t
    Double quote is replaced with \"
    Backslash is replaced with \\
}
    r:='';

    for i := 1 to Length (s) do
    begin
       if(s[i] = #13) then r:= r + '\n'
       else if(s[i] = #9) then r:= r + '\t'
       else if(s[i] = #10) then r:= r + '\r'
       else if(s[i] = '\') then r:= r + '\\'
       else if(s[i] = '"') then r:= r + '\"'
       else r:=r + S[i];
    end;

    Result := r;
end;

function TNextSync.PushChanges(notes : TNoteInfoList) : boolean;
var
    i,j : integer;
    note : PNoteInfo;
    res : UTF8String;
    json: TJSONData;
    p : TStrings;
    jObject : TJSONObject;
begin
    //res := '{ "note-changes": [';
    res := '{ "latest-sync-revision": "'+IntToStr(ServerRev+1)+'", "note-changes": [';

    for i := 0 to notes.Count -1 do
    begin
       note := notes.Items[i];
       if note^.Action = SynDeleteRemote
       then res := res + '{ "guid": "' + note^.ID + '", "command": "delete" }'
       else begin
           res := res + '{ "guid": "' + note^.ID + '",';
           res := res + ' "title": "' + JsonEscape(note^.Title) + '",';
           res := res + ' "note-content": "' + JsonEscape(note^.Content) + '",';
           res := res + ' "note-content-version": "' + note^.Version + '",';
           res := res + ' "last-change-date": "' + note^.LastChange + '",';
           res := res + ' "last-metadata-change-date": "' + note^.LastMetaChange + '",';
           res := res + ' "create-date": "' + note^.CreateDate + '",';
           res := res + ' "open-on-startup": ' + BoolToStr(note^.OpenOnStartup,true) + ',';
           res := res + ' "pinned": ' + BoolToStr(note^.Pinned,true) + ',';
           res := res + ' "x": "' + IntToStr(note^.X) + '",';
           res := res + ' "y": "' + IntToStr(note^.Y) + '",';
           res := res + ' "height": "' + IntToStr(note^.Height) + '",';
           res := res + ' "width": "' + IntToStr(note^.Width) + '",';
           res := res + ' "selection-bound-position": "' + IntToStr(note^.SelectBoundPosition) + '",';
           res := res + ' "cursor-position": "' + IntToStr(note^.CursorPosition) + '",';
           res := res + ' "tags": [ ';
           j:=0;
           while(j<note^.Tags.Count) do
           begin
              res := res + '"' + JsonEscape(note^.Tags.Strings[j]) + '"' ;
              inc(j);
              if(j<note^.Tags.Count) then res := res + ', ';
           end;
           res := res + ' ] }';
       end;
       if(i<notes.Count -1 ) then res := res + ', ';
    end;
    res := res + ' ] } ';

    try
       json := GetJSON(res);
    except on E:Exception do begin ErrorString := E.message; TRlog(E.message); exit(false); end;
    end;

    // HTTP REQUEST
    res := getParam('URLNOTES');
    p := TstringList.Create();
    OauthBaseParams(p,Key,Token);
    OauthParamsSort(p);
    OauthSign(res, 'PUT', p, Key, TokenSecret);
    OauthParamsSort(p);
    res := WebPut(res,p,json.AsJSON);
    FreeAndNil(p);

    TRlog('RES PUSH = '+res);

    if (res = '') then begin ErrorString :=  'Push CHanges: Unable to push data'; exit(false); end;

    try
       json := GetJSON(res);
       TRlog(json.FormatJSON());
       jObject := TJSONObject(json);
       ServerRev := jObject.Get('latest-sync-revision',ServerRev+1);
       FreeAndNil(jObject);
    except on E:Exception do begin ErrorString := E.message; TRlog(E.message); exit(false); end;
    end;

    result := True;
end;


function TNextSync.DoRemoteManifest(const RemoteManifest: TStringList): boolean;
begin
	WriteLn('Next-DoRemoteManifest');
    result := True;
end;

function TNextSync.IDLooksOK() : boolean;
var
  n,m : Integer;
begin
    n := pos('-', ServerID);
    m := pos('.', ServerID);

    if((m-n) <> 15) then exit(false);
    if((length(ServerID)-m) <> 8) then exit(false);
    result := True;
end;

end.

