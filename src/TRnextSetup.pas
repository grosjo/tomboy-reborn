unit TRnextSetup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, LCLIntf, StdCtrls,
  fpjson, jsonparser, strutils, fphttpserver, LazLogger, openssl, md5,
  TRcommon;

type     { TRServer }
   TRServer = class(TThread)
   private
      server: TFPHTTPServer;
   public
      destructor Destroy; override;
      procedure Setup(APort: word; const OnRequest: THTTPServerRequestHandler);
      procedure Execute(); override;
      procedure Finish();
   end;

type    { TFormNCSetup }
  TFormNCSetup = class(TForm)
    NCAuth: TButton;
    URL: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    SetupStatus: TLabel;
    NCDone: TToggleBox;
    function isSuccess() : boolean;
    function getKey() : UTF8String;
    function getToken() : UTF8String;
    function getTokenSecret() : UTF8String;
    procedure setKey(s : UTF8String);
    procedure setToken(s : UTF8String);
    procedure NCAuthClick(Sender: TObject);
    procedure AuthSuccess(i : Int64);
    procedure AuthFailure(i : Int64);
    procedure FormCreate(Sender: TObject);
    procedure NCDoneChange(Sender: TObject);
  private
    success : boolean;
    listening : boolean;
    Key : UTF8String;
    Token : UTF8String;
    TokenSecret : UTF8String;
    Verifier : UTF8String;
    requestTokenUrl : UTF8String;
    authorizeUrl : UTF8String;
    accessTokenUrl : UTF8String;
    web: TRServer;
    procedure DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
  end;


implementation

{$R *.lfm}

{ TRServer }

procedure TRServer.Setup(APort: word; const OnRequest: THTTPServerRequestHandler);
 begin
   server := TFPHTTPServer.Create(nil);
   server.Port := APort;
   server.OnRequest := OnRequest;
   FreeOnTerminate := true;
 end;

destructor TRServer.Destroy;
begin
  server.Free;
end;

procedure TRServer.Execute;
begin
  try
     server.Active := True;
  except on E: Exception do writeln(E.Message);
  end;
end;

procedure TRServer.Finish();
begin
   server.Active := False;
end;




{ TFormNCSetup }

procedure TFormNCSetup.FormCreate(Sender: TObject);
begin
  TRlog('NC FormCreate');
  success :=false;
  listening := false;
  Left := TForm(Sender).Left + random(100);
  Top := TForm(Sender).Top + random(100);
end;


function TFormNCSetup.isSuccess() : boolean;
begin
  TRlog('NC isSuccess '+BoolToStr(success,true));
  Result := success;
end;

function TFormNCSetup.getKey() : UTF8String;
begin
  Result := Key;
end;

function TFormNCSetup.getToken() : UTF8String;
begin
  Result := Token;
end;

function TFormNCSetup.getTokenSecret() : UTF8String;
begin
  Result := TokenSecret;
end;

procedure TFormNCSetup.setKey(s : UTF8String);
begin
  if(length(s)<5) then
     Key := MD5Print(MD5String(Format('%d',[Random(9999999-123400)+123400])))
  else Key := s;
end;

procedure TFormNCSetup.setToken(s : UTF8String);
begin
  Token :=s;
end;

procedure TFormNCSetup.NCAuthClick(Sender: TObject);
var
  res, s1, s2, s3, resturl, u : UTF8String;
  jData : TJSONData;
  jObject : TJSONObject;
  p : TStrings;
  ok : boolean;
  ts : TSysCharSet;
begin
  if(listening) then begin
     SetupStatus.Caption := 'OAuth: Prcessus canceled';
     URL.Enabled:= true;
     NCAuth.Caption := 'Authenticate';
     web.Finish();
     listening :=false;
     exit;
  end;

  success :=false;

  NCAuth.Enabled :=false;
  try
    // GET OAUTH URLS
     SetupStatus.Caption := 'OAuth: Getting URLs settings';
     resturl := Trim(URL.Text) + '/api/1.0/';
     p := TstringList.Create();
     res := WebGet(resturl,p);
     FreeAndNil(p);
     if(length(res)>0) then begin
        ok := true; s1 :=''; s2:=''; s3:='';
        try
           jData := GetJSON(res);
           TRlog('JSON1 OAUTH = ' + jData.FormatJSON());
           jObject := TJSONObject(jData);
           s1 := jObject.Get('oauth_request_token_url');
           s2 := jObject.Get('oauth_authorize_url');
           s3 := jObject.Get('oauth_access_token_url');
           FreeAndNil(jObject);
        except on E:Exception do begin
          ShowMessage(E.message);
          ok:= false;
          end;
        end;
        if (not ok) then begin SetupStatus.Caption := 'Error JSON'; exit; end;
        if((length(s1)<10) or (length(s2)<10) or (length(s3)<10)) then
        begin SetupStatus.Caption := 'Server returns invalid OAuth URLs'; exit; end;

        requestTokenUrl := s1;
        authorizeUrl    := s2;
        accessTokenUrl  := s3;

        // REQUEST TOKEN
        SetupStatus.Caption := 'OAuth: Getting Request Token';
        p := TStringList.Create();
        OauthBaseParams(p,Key);
        // callbackUrl
        p.Add('oauth_callback');
        p.Add(OAuthCallbackUrl);
        OauthParamsSort(p);
        OauthSign(requestTokenUrl, 'POST', p, Key, '');
        res := WebPost(requestTokenUrl,p);
        FreeAndNil(p);

        TRlog('Request token reply : '+res);
        //Example : oauth_token=r36747eda81d3a14c&oauth_token_secret=s05507586554bb6bf&oauth_callback_confirmed=true

        ts:=['&'];
        s1 := ExtractWord(1, res, ts);
        s2 := ExtractWord(2, res, ts);
        s3 := ExtractWord(3, res, ts);

        ts:=['='];

        if( ExtractWord(1, s1, ts) <> 'oauth_token' ) then begin SetupStatus.Caption := 'OAuth: token invalid'; ShowMessage(SetupStatus.Caption); exit; end;
        if( ExtractWord(1, s2, ts) <> 'oauth_token_secret' ) then begin SetupStatus.Caption := 'OAuth token Secret invalid'; ShowMessage(SetupStatus.Caption); exit; end;
        if( ExtractWord(1, s3, ts) <> 'oauth_callback_confirmed' ) then begin SetupStatus.Caption := 'OAuth not confirmed'; ShowMessage(SetupStatus.Caption); exit; end;

        Token := ExtractWord(2, s1,ts);
        TokenSecret:= ExtractWord(2,s2,ts);

        // AUTORIZE
        SetupStatus.Caption := 'OAuth: Asking user to autorize';
        u:= authorizeUrl + '?oauth_token=' + Token + '&client=TomboyReborn&oauth_callback=' + URLEncode(OAuthCallbackUrl);
        NCAuth.Caption := 'Cancel OAuth';
        URL.Enabled:= false;

        web := TRServer.Create(false);
        web.Setup(8000,@DoHandleRequest);
        listening :=true;
        openUrl(u);
     end;
  except on E:Exception do
    ShowMessage(E.message);
  end;
  NCAuth.Enabled := true;

end;

procedure TFormNCSetup.DoHandleRequest(Sender: TObject; var ARequest: TFPHTTPConnectionRequest; var AResponse: TFPHTTPConnectionResponse);
var
  restok, resverif : UTF8String;
begin
  AResponse.Code := 200;
  AResponse.ContentType := 'text/html;charset=utf-8';


  restok := ARequest.QueryFields.Values['oauth_token'];
  resverif := ARequest.QueryFields.Values['oauth_verifier'];
  web.Finish();
web := nil;
  listening :=false;
  if(restok = Token) then begin
    Verifier := resverif;
    AResponse.Contents.Text := '<h2>Congratulation, your Tomboy Reborn is authenticated</h2>';
    //web.Synchronize(web,@AuthSuccess);
    Application.QueueAsyncCall(@AuthSuccess,0);
  end else begin
     AResponse.Contents.Text := '<h2>A error occured : URL is '+ARequest.URL+'/h2>';
     //web.Synchronize(web,@AuthFailure);
     Application.QueueAsyncCall(@AuthFailure,0);
  end;
end;

procedure TFormNCSetup.AuthFailure(i : Int64);
begin
  SetupStatus.Caption := 'OAuth: Error occured';
  URL.Enabled:= true;
  NCAuth.Caption := 'Authenticate';
end;

procedure TFormNCSetup.AuthSuccess(i : Int64);
var
  p : TStrings;
  res, s1, s2 : UTF8String;
  ts : TSysCharSet;
begin
  NCAuth.Enabled :=false;

  // ACCESS TOKEN
  SetupStatus.Caption := 'OAuth: Getting Access Token';
  p := TStringList.Create();
  OauthBaseParams(p,Key, Token, Verifier);
  OauthParamsSort(p);
  OauthSign(accessTokenUrl, 'POST', p, Key, TokenSecret);
  res := WebPost(accessTokenUrl,p);
  FreeAndNil(p);

  ts:=['&'];
  s1 := ExtractWord(1, res, ts);
  s2 := ExtractWord(2, res, ts);

  ts:=['='];

  NCAuth.Enabled := true;
  NCAuth.Caption := 'Re-authenticate';

  if( ExtractWord(1, s1, ts) <> 'oauth_token' ) then begin SetupStatus.Caption := 'OAuth: token invalid'; ShowMessage(SetupStatus.Caption); exit; end;
  if( ExtractWord(1, s2, ts) <> 'oauth_token_secret' ) then begin SetupStatus.Caption := 'OAuth token Secret invalid'; ShowMessage(SetupStatus.Caption); exit; end;

  Token := ExtractWord(2, s1,ts);
  TokenSecret:= ExtractWord(2,s2,ts);

  success :=true;
  SetupStatus.Caption := 'Congratulation, your Tomboy Reborn is authenticated';

end;

procedure TFormNCSetup.NCDoneChange(Sender: TObject);
begin
    Close;
end;

end.
