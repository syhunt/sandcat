unit LAPI_CEF;

{
  Sandcat Chromium OSR (Off-Screen Renderer) LUA Object
  Copyright (c) 2011-2015, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Classes, SysUtils, Lua, pLua, LuaObject, CatChromiumOSRX,
  Dialogs, CatChromiumLib, uLiveHeaders;

type
  { TSandOSR }
  TSandOSR = class(TLuaObject)
  private
    fGetSourceAsText: boolean;
    procedure AddressChange(Sender: TObject; const URL: string);
    procedure BeforeDownload(Sender: TObject; const id: integer;
      const suggestedName: string);
    procedure BeforePopup(Sender: TObject; var URL: string;
      out Result: boolean);
    procedure BrowserMessage(const msg: integer; const str: string);
    procedure LoadEnd(Sender: TObject; httpStatusCode: integer);
    procedure LoadError(Sender: TObject; const errorCode: integer;
      const errorText, failedUrl: string);
    procedure LoadingStateChange(Sender: TObject;
      const isLoading, canGoBack, canGoForward: boolean);
    procedure ConsoleMessage(Sender: TObject; const message, source: string;
      line: integer);
    procedure HandleRequestDetails(const json: string);
    procedure SendRequest(const method, URL, postdata: string;
      const load: boolean = false);
    procedure SendRequestCustom(req: TCatChromiumRequest;
      load: boolean = false);
    procedure SourceAvailable(const s, headers: string);
  public
    obj: TCatChromiumOSRX;
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : boolean; override;
    destructor Destroy; override;
  end;

procedure RegisterCEF(L: PLua_State);
function BuildJSCallFromLuaTable(L: PLua_State): TCatCustomJSCall;
function BuildRequestFromJSON(json: string): TCatChromiumRequest;
function BuildRequestFromLuaTable(L: PLua_State): TCatChromiumRequest;
function BuildRequestDetailsFromLuaTable(L: PLua_State): TSandcatRequestDetails;
function BuildRequestDetailsFromJSON(const json: string;
  const response: string = ''): TSandcatRequestDetails;

procedure lua_pushrequestdetails(L: PLua_State; r: TSandcatRequestDetails);

implementation

uses uMain, uConst, uUIComponents, uSettings, pLuaTable, CatCEFCache, CatFiles,
  CatHTTP, CatStrings;

const
  REQUESTKEY_METHOD = 'method';
  REQUESTKEY_URL = 'url';
  REQUESTKEY_POSTDATA = 'postdata';
  REQUESTKEY_HEADERS = 'headers';
  REQUESTKEY_DETAILS = 'details';
  REQUESTKEY_IGNORECACHE = 'ignorecache';
  REQUESTKEY_USECOOKIES = 'usecookies';
  REQUESTKEY_USEAUTH = 'useauth';
  REQUESTKEY_TAB = 'tab';
  REQUESTKEY_FILTER = 'filter';
  REQUESTKEY_USERNAME = 'username';
  REQUESTKEY_PASSWORD = 'password';
  REQUESTKEY_CALLBACK = 'callback';

function BuildJSCallFromLuaTable(L: PLua_State): TCatCustomJSCall;
var
  t: TLuaTable;
begin
  t := TLuaTable.Create(L, true);
  Result.Code := t.ReadString('code');
  Result.URL := t.ReadString('url');
  Result.StartLine := t.ReadInteger('startln', 0);
  Result.Silent := t.ReadBool('silent', false);
  t.Free;
end;

function BuildRequestFromLuaTable(L: PLua_State): TCatChromiumRequest;
var
  t: TLuaTable;
begin
  t := TLuaTable.Create(L, true);
  Result.method := t.ReadString(REQUESTKEY_METHOD);
  Result.URL := t.ReadString(REQUESTKEY_URL);
  Result.postdata := t.ReadString(REQUESTKEY_POSTDATA);
  Result.headers := t.ReadString(REQUESTKEY_HEADERS);
  Result.ignorecache := t.ReadBool(REQUESTKEY_IGNORECACHE, true);
  Result.usecookies := t.ReadBool(REQUESTKEY_USECOOKIES, true);
  Result.usecachedcredentials := t.ReadBool(REQUESTKEY_USEAUTH, true);
  Result.details := t.ReadString(REQUESTKEY_DETAILS);
  t.Free;
end;

function BuildRequestFromJSON(json: string): TCatChromiumRequest;
var
  j: TSandJSON;
begin
  j := TSandJSON.Create;
  j.text := json;
  Result.method := j.GetValue(REQUESTKEY_METHOD, 'GET');
  Result.URL := j.GetValue(REQUESTKEY_URL, emptystr);
  Result.postdata := j.GetValue(REQUESTKEY_POSTDATA, emptystr);
  Result.headers := j.GetValue(REQUESTKEY_HEADERS, emptystr);
  Result.ignorecache := j.GetValue(REQUESTKEY_IGNORECACHE, true);
  Result.usecookies := j.GetValue(REQUESTKEY_USECOOKIES, true);
  Result.usecachedcredentials := j.GetValue(REQUESTKEY_USEAUTH, true);
  Result.details := j.GetValue(REQUESTKEY_DETAILS, emptystr);
  j.Free;
end;

function BuildRequestDetailsFromLuaTable(L: PLua_State): TSandcatRequestDetails;
var
  t: TLuaTable;
begin
  t := TLuaTable.Create(L, true);
  Result.host := t.ReadString('host', emptystr);
  Result.port := t.ReadString('port', emptystr);
  Result.SentHead := t.ReadString(REQUESTKEY_HEADERS, emptystr);
  Result.RcvdHead := t.ReadString('responseheaders', emptystr);
  Result.method := t.ReadString(REQUESTKEY_METHOD, emptystr);
  Result.URL := t.ReadString(REQUESTKEY_URL, emptystr);
  Result.postdata := t.ReadString(REQUESTKEY_POSTDATA, emptystr);
  Result.StatusCode := t.ReadInteger('status', 0);
  Result.Length := t.ReadInteger('length', 0);
  Result.MimeType := t.ReadString('mimetype', emptystr);
  Result.details := t.ReadString(REQUESTKEY_DETAILS, emptystr);
  Result.reqid := t.ReadString('reqid', emptystr);
  Result.responsefilename := t.ReadString('responsefilename', emptystr);
  Result.response := t.ReadString('response', emptystr);
  Result.isredir := t.ReadBool('isredir', false);
  Result.IsLow := t.ReadBool('islow', false);
  t.Free;
end;

function BuildRequestDetailsFromJSON(const json: string;
  const response: string = ''): TSandcatRequestDetails;
var
  j: TSandJSON;
begin
  j := TSandJSON.Create;
  j.text := json;
  Result.host := j.GetValue('host', emptystr);
  Result.port := j.GetValue('port', emptystr);
  Result.SentHead := j.GetValue(REQUESTKEY_HEADERS, emptystr);
  Result.RcvdHead := j.GetValue('responseheaders', emptystr);
  Result.method := j.GetValue(REQUESTKEY_METHOD, emptystr);
  Result.URL := j.GetValue(REQUESTKEY_URL, emptystr);
  Result.postdata := j.GetValue(REQUESTKEY_POSTDATA, emptystr);
  Result.StatusCode := j.GetValue('status', emptystr);
  Result.Length := j.GetValue('length', emptystr);
  Result.MimeType := j.GetValue('mimetype', emptystr);
  Result.details := j.GetValue(REQUESTKEY_DETAILS, emptystr);
  Result.reqid := j.GetValue('reqid', emptystr);
  if response = emptystr then
    Result.responsefilename := j.GetValue('responsefilename', emptystr)
  else
    Result.response := response;
  Result.isredir := j.GetValue('isredir', false);
  Result.IsLow := j.GetValue('islow', false);
  j.Free;
end;

procedure lua_pushrequestdetails(L: PLua_State; r: TSandcatRequestDetails);
begin
  lua_newtable(L);
  plua_SetFieldValue(L, 'host', r.host);
  plua_SetFieldValue(L, 'port', r.port);
  plua_SetFieldValue(L, 'requestid', r.reqid);
  plua_SetFieldValue(L, REQUESTKEY_DETAILS, r.details);
  plua_SetFieldValue(L, REQUESTKEY_METHOD, r.method);
  plua_SetFieldValue(L, REQUESTKEY_URL, r.URL);
  plua_SetFieldValue(L, REQUESTKEY_POSTDATA, r.postdata);
  plua_SetFieldValue(L, 'status', r.StatusCode);
  plua_SetFieldValue(L, 'mimetype', r.MimeType);
  plua_SetFieldValue(L, 'length', r.Length);
  plua_SetFieldValue(L, REQUESTKEY_HEADERS, r.SentHead);
  plua_SetFieldValue(L, 'responseheaders', r.RcvdHead);
  plua_SetFieldValue(L, 'response', r.response);
  plua_SetFieldValue(L, 'responsefilename', r.responsefilename);
  plua_SetFieldValue(L, 'isredir', r.isredir);
  plua_SetFieldValue(L, 'islow', r.IsLow);
  plua_SetFieldValue(L, 'filename', r.Filename);
end;

function get_temp_preview_filename(URL: string): string;
var
  dir, urlext: string;
begin
  urlext := ExtractUrlFileExt(URL);
  urlext := CleanFilename(urlext);
  dir := GetSandcatDir(SCDIR_PREVIEW, true);
  Result := dir + inttostr(sandbrowser.Handle) + urlext;
end;

function method_savetofile(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
  outfilename: string;
begin
  outfilename := lua_tostring(L, 2);
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  if outfilename = emptystr then
    outfilename := get_temp_preview_filename(ht.obj.GetURL);
  ht.obj.SaveToFile(outfilename);
  lua_pushstring(L, outfilename);
  Result := 1;
end;

function method_loadurl(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  ht.obj.reset;
  ht.obj.load(lua_tostring(L, 2));
  Result := 1;
end;

function method_loadurlfromcache(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  ht.obj.loadfromcache(lua_tostring(L, 2));
  Result := 1;
end;

function method_loadsource(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  ht.obj.reset;
  ht.obj.loadfromstring(lua_tostring(L, 2), lua_tostring(L, 3));
  Result := 1;
end;

function method_loadrequest(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  ht.obj.Reset;
  if lua_istable(L, 2) then // user provided a Lua table
    ht.SendRequestCustom(BuildRequestFromLuaTable(L), true)
  else
    ht.SendRequest(lua_tostring(L, 2), lua_tostring(L, 3),
      lua_tostring(L, 4), true);
  Result := 1;
end;

function method_sendrequest(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  if lua_istable(L, 2) then // user provided a Lua table
    ht.SendRequestCustom(BuildRequestFromLuaTable(L), false)
  else
    ht.SendRequest(lua_tostring(L, 2), lua_tostring(L, 3),
      lua_tostring(L, 4), false);
  Result := 1;
end;

function method_runjavascript(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  if lua_istable(L, 2) then // user provided a Lua table
    ht.obj.RunJavaScript(BuildJSCallFromLuaTable(L))
  else
    ht.obj.RunJavaScript(lua_tostring(L, 2), lua_tostring(L, 3),
      lua_tointeger(L, 4));
  Result := 1;
end;

function method_showauthdialog(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  ht.obj.ShowAuthDialog(lua_tostring(L, 2), lua_tostring(L, 3));
  Result := 1;
end;

function method_reload(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
  igncache: boolean;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  igncache := false;
  if lua_isnone(L, 2) = false then
    igncache := lua_toboolean(L, 2);
  ht.obj.Reload(igncache);
  Result := 1;
end;

function method_stopload(L: PLua_State): integer; cdecl;
var
  ht: TSandOSR;
begin
  ht := TSandOSR(LuaToTLuaObject(L, 1));
  ht.obj.Stop;
  Result := 1;
end;

procedure register_methods(L: PLua_State; classTable: integer);
begin
  RegisterMethod(L, 'loadurl', @method_loadurl, classTable);
  RegisterMethod(L, 'loadcached', @method_loadurlfromcache, classTable);
  RegisterMethod(L, 'loadsource', @method_loadsource, classTable);
  RegisterMethod(L, 'loadrequest', @method_loadrequest, classTable);
  RegisterMethod(L, 'reload', @method_reload, classTable);
  RegisterMethod(L, 'runjs', @method_runjavascript, classTable);
  RegisterMethod(L, 'savetofile', @method_savetofile, classTable);
  RegisterMethod(L, 'sendrequest', @method_sendrequest, classTable);
  RegisterMethod(L, 'showauthdialog', @method_showauthdialog, classTable);
  RegisterMethod(L, 'stop', method_stopload, classTable);
end;

const
  ObjName = 'osr';

function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  Result := TSandOSR.Create(L, AParent);
end;

function Create(L: PLua_State): integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @new_callback;
  Result := new_LuaObject(L, ObjName, p);
end;

procedure RegisterCEF(L: PLua_State);
begin
  RegisterTLuaObject(L, ObjName, @Create, @register_methods);
end;

// Called before starting a file download
procedure TSandOSR.BeforeDownload(Sender: TObject; const id: integer;
  const suggestedName: string);
begin
  if LocateEvent('ondownloadstart') then
  begin
    lua_newtable(L);
    plua_SetFieldValue(L, 'id', id);
    plua_SetFieldValue(L, 'suggestedname', suggestedName);
    lua_pcall(L, 1, 0, 0);
  end;
  downloads.SetDownloadFilename(id, suggestedName);
end;

procedure TSandOSR.LoadingStateChange(Sender: TObject;
  const isLoading, canGoBack, canGoForward: boolean);
begin
  if LocateEvent('onloadstatechange') then
  begin
    lua_newtable(L);
    plua_SetFieldValue(L, 'isloading', isLoading);
    plua_SetFieldValue(L, 'cangoback', canGoBack);
    plua_SetFieldValue(L, 'cangoforward', canGoForward);
    lua_pcall(L, 1, 0, 0);
  end;
end;

// Calls the object method (if set) and pushes the request details as a Lua table
// in the first parameter and a JSON object in the second parameter
procedure TSandOSR.HandleRequestDetails(const json: string);
var
  r: TSandcatRequestDetails;
begin
  if LocateEvent('onrequestdone') then
  begin
    r := BuildRequestDetailsFromJSON(json);
    lua_pushrequestdetails(L, r);
    lua_pushstring(L, json);
    lua_pcall(L, 2, 0, 0);
  end;
end;

// Handling of messages originating from the Chromium component
// Can also originate from the V8 engine running in the isolated tab process
procedure TSandOSR.BrowserMessage(const msg: integer; const str: string);
begin
  case (msg) of
    CRM_NEWPAGERESOURCE:
      CallEvent('onresourcefound', [str]);
    CRM_LOG_REQUEST_JSON:
      HandleRequestDetails(str);
  end;
end;

// Sends (and optionally loads) a HTTP request
procedure TSandOSR.SendRequest(const method, URL, postdata: string;
  const load: boolean = false);
begin
  obj.SendRequest(buildrequest(method, URL, postdata), load);
end;

// Sends (and optionally loads) a custom HTTP request
procedure TSandOSR.SendRequestCustom(req: TCatChromiumRequest;
  load: boolean = false);
begin
  // If no URL is provided, uses current tab url as the request URL
  if req.URL = emptystr then
    req.URL := obj.GetURL;
  obj.SendRequest(req, load);
end;

procedure TSandOSR.SourceAvailable(const s, headers: string);
begin
  CallEvent('onsetsource', [s, headers]);
end;

procedure TSandOSR.LoadError(Sender: TObject; const errorCode: integer;
  const errorText, failedUrl: string);
begin
  if LocateEvent('onloaderror') then
  begin
    lua_newtable(L);
    plua_SetFieldValue(L, 'errorcode', errorCode);
    plua_SetFieldValue(L, 'errortext', errorText);
    plua_SetFieldValue(L, 'failedurl', failedUrl);
    lua_pcall(L, 1, 0, 0);
  end;
end;

procedure TSandOSR.LoadEnd(Sender: TObject; httpStatusCode: integer);
begin
  if LocateEvent('onloadend') then
  begin
    lua_newtable(L);
    plua_SetFieldValue(L, 'statuscode', httpStatusCode);
    lua_pcall(L, 1, 0, 0);
  end;
  if EventExists('onsetsource') then
  begin
    if obj.iscachedurl then
      fgetsourceastext := true;
    if fgetsourceastext then
      obj.GetSourceAsText
    else
      obj.GetSource;
  end;
end;

// Called before a popup window is opened
procedure TSandOSR.BeforePopup(Sender: TObject; var URL: string;
  out Result: boolean);
begin
  if LocateEvent('onbeforepopup') then
  begin
    lua_newtable(L);
    plua_SetFieldValue(L, 'url', URL);
    lua_pcall(L, 1, 0, 0);
  end;
end;

procedure TSandOSR.ConsoleMessage(Sender: TObject;
  const message, source: string; line: integer);
begin
  if LocateEvent('onconsolemessage') then
  begin
    lua_newtable(L);
    plua_SetFieldValue(L, 'message', message);
    plua_SetFieldValue(L, 'source', source);
    plua_SetFieldValue(L, 'line', line);
    lua_pcall(L, 1, 0, 0);
  end;
end;

procedure TSandOSR.AddressChange(Sender: TObject; const URL: string);
begin
  CallEvent('onaddresschange', [URL]);
end;

constructor TSandOSR.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  fGetSourceAsText := false;
  obj := TCatChromiumOSRX.Create(nil);
  obj.OnLoadEnd := LoadEnd;
  obj.OnAfterSetSourceSpecial := SourceAvailable;
  obj.OnBrowserMessage := BrowserMessage;
  obj.OnConsoleMessage := ConsoleMessage;
  obj.OnLoadError := LoadError;
  obj.OnAddressChange := AddressChange;
  obj.OnLoadingStateChange := LoadingStateChange;
  obj.OnBeforePopup := BeforePopup;
  obj.OnBeforeDownload := BeforeDownload;
  obj.LoadSettings(settings.preferences.current, settings.preferences.Default);
end;

function TSandOSR.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'captureurls') = 0 then
    Result := obj.LogURLs
  else if CompareText(propName, 'downloadfiles') = 0 then
    Result := obj.EnableDownloads
  else if CompareText(propName, 'getsourceastext') = 0 then
    Result := fgetsourceastext
  else if CompareText(propName, 'isloading') = 0 then
    Result := obj.isLoading
  else if CompareText(propName, 'reslist') = 0 then
    Result := obj.ResourceList.text
  else if CompareText(propName, 'url') = 0 then
    Result := obj.GetURLSpecial
  else if CompareText(propName, 'urllist') = 0 then
    Result := obj.URLLog.text
  else
    Result := inherited GetPropValue(propName);
end;

function TSandOSR.SetPropValue(propName: String; const AValue: Variant)
  : boolean;
begin
  Result := true;
  if CompareText(propName, 'captureurls') = 0 then
    obj.LogURLs := AValue
  else if CompareText(propName, 'downloadfiles') = 0 then
    obj.EnableDownloads := AValue
  else if CompareText(propName, 'getsourceastext') = 0 then
    fgetsourceastext := AValue
  else if CompareText(propName, 'isloading') = 0 then
    Result := true // readonly
  else if CompareText(propName, 'reslist') = 0 then
    Result := true // readonly
  else if CompareText(propName, 'url') = 0 then
    obj.load(string(AValue))
  else if CompareText(propName, 'urllist') = 0 then
    Result := true // readonly
  else
    Result := inherited SetPropValue(propName, AValue);
end;

destructor TSandOSR.Destroy;
begin
  obj.Free;
  inherited Destroy;
end;

end.
