unit TRtransport;

{$mode objfpc}{$H+}

interface

uses
    Classes, SysUtils, TRcommon;


type TTomboyTrans = class
    
    private
        names : TStringList;
        values : TStringList;

    public

        ErrorString : UTF8String;
        { The current server ID. Is set with a successful TestTransport call. }
        ServerID : UTF8String;
        { The current Server Rev, before we upload. Is set with a successful
              TestTransport call. }
        ServerRev : integer;

        constructor create;
        destructor Destroy; override;

        { Tests availability of remote part of connection. For file sync (eg) thats
            existance of remote manifest and 0 dir, write access. Sets its own ServerID.
            This would be a good place to put lock or authenticate as  necessary}
        function TestTransport() : TSyncStatus;     virtual; abstract;

        {Request a list of all notes the server knows about. Returns with Last Change
            Date (LCD) if easily available and always if GetLCD is true. We don't use all
            fields in TInfoList, the list must have been created.}
        function GetNotes(const NoteMeta : TNoteInfoList) : boolean; virtual; abstract;

        { Execute changes om the server }
        function PushChanges(notes : TNoteInfoList) : boolean; virtual; abstract;

        { Tells Trans to deal with with remote mainfest. This is the trigger
              for a new revision on the server, the server must now do whatever
              it needs to accomodate the new new revision, some new or update
              notes will be sent to it a bit later. New RevNo will be RemoteServerRev plus 1 }
        function DoRemoteManifest(const RemoteManifest : TStringList) : boolean; virtual; abstract;

        function getPrefix(): UTF8String; virtual; abstract;

        function IDLooksOK() : boolean; virtual; abstract;

        procedure setParam(p : UTF8String; v : UTF8String);
        function getParam(p : UTF8String) : UTF8String;


  end;


implementation

constructor TTomboyTrans.Create;
begin
  names := TStringList.Create();
  values := TStringList.Create();
end;

destructor TTomboyTrans.Destroy;
begin
  FreeAndNil(names);
  FreeAndNil(values);
  inherited Destroy;

end;


procedure TTomboyTrans.setParam(p : UTF8String; v : UTF8String);
var
  i : LongInt;
begin
  i := names.IndexOf(p);
  if(i<0) then begin
     names.add(p);
     values.add(v);
  end else values.Strings[i] :=v;
  i := names.IndexOf(p);
end;

function TTomboyTrans.getParam(p : UTF8String) : UTF8String ;
var
  i : LongInt;
  v : UTF8String;
begin
  i := names.IndexOf(p);
  if(i<0) then v := ''
  else v := values.strings[i];
  Result := v;
end;

end.





