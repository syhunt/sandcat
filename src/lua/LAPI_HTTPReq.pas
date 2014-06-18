unit LAPI_HTTPReq;
{
  Sandcat HTTP Request LUA Object
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Classes, Forms, SysUtils, Graphics, Dialogs, Lua, LuaObject, uTab,
  CatChromium;

type
  TSandcatXHRRequest = class
  private
    ASync: boolean;
  public
    Details: string;
    Method: string;
    URL: string;
    ReqHeaders: string;
    PostData: string;
    UserFilter: string;
    Username: string;
    Password: string;
    LuaScript_RequestSent: string;
    Tab: TSandcatTab;
    procedure Open(Method, URL: string; ASync: boolean = true);
    function Send(PostData: string): string;
    function SendHTTPRequest: string;
    constructor Create;
    destructor Destroy; override;
  end;

type
  TSCBXHRObject = class(TLuaObject)
  private
  public
    XHR: TSandcatXHRRequest;
    constructor Create(LuaState: PLua_State;
      AParent: TLuaObject = nil); overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : boolean; override;
    destructor Destroy; override;
  end;

procedure RegisterSCBHTTPRequest_Sandcat(L: PLua_State);
procedure SendXHR(params: TCatChromiumXHR; sourcetab: TSandcatTab);

implementation

uses
  uMain, uUIComponents, CatStrings, plua, uConst, uRequests, uMisc;

type
  TEncodedRequest = record
    Details: string;
    Method: string;
    URL: string;
    Username: string;
    Password: string;
  end;

procedure SendXHR(params: TCatChromiumXHR; sourcetab: TSandcatTab);
var
  XHR: TSandcatXHRRequest;
begin
  XHR := TSandcatXHRRequest.Create;
  XHR.Details := 'XHR Call';
  if params.Details <> emptystr then
    XHR.Details := params.Details;
  if params.Tab <> emptystr then
    XHR.Tab := tabmanager.gettab(params.Tab)
  else
    XHR.Tab := sourcetab;
  XHR.UserFilter := params.filters;
  XHR.Username := params.Username;
  XHR.Password := params.Password;
  XHR.ReqHeaders := params.headers;
  XHR.LuaScript_RequestSent := params.callback;
  if params.Method = emptystr then
    params.Method := 'GET';
  if params.URL = emptystr then
    params.URL := sourcetab.geturl;
  XHR.Open(params.Method, params.URL);
  XHR.Send(params.PostData);
  XHR.free;
end;

constructor TSandcatXHRRequest.Create;
begin
  inherited Create;
  if tabmanager.activetab <> nil then
    Tab := tabmanager.activetab;
end;

destructor TSandcatXHRRequest.Destroy;
begin
  Tab := nil;
  inherited Destroy;
end;

function TSandcatXHRRequest.SendHTTPRequest: string;
var
  js, jsheaders: tstringlist;
  slp: TSandSLParser;
var
  jscode, rheader, rvalue: string;
  reqid: string;
  e: TEncodedRequest;
  function getAsync(b: boolean): string;
  begin
    if b = true then
      result := 'true'
    else
      result := 'false';
  end;
  function getAuth: string;
  begin
    result := emptystr;
    if Username <> emptystr then
      result := ',' + e.Username + ',' + e.Password;
  end;

begin
  result := emptystr;
  if Tab = nil then
    exit;
  Global_LoggedRequests := Global_LoggedRequests + 1;
  reqid := inttostr(Tab.handle) + '_' + inttostr(Global_LoggedRequests);
  result := reqid;
  js := tstringlist.Create;
  jsheaders := tstringlist.Create;
  if ReqHeaders <> emptystr then
  begin
    slp := TSandSLParser.Create;
    slp.LoadFromString(ReqHeaders);
    while slp.Found do
    begin
      if MatchStrings(slp.current, '*:*') then
      begin
        rheader := before(slp.current, ': ');
        rvalue := after(slp.current, ': ');
        if rheader <> vSCRIDHeader then
          jsheaders.add('xmlhttp.setRequestHeader(' + sandenc(rheader) + ',' +
            sandenc(rvalue) + ');');
      end;
    end;
    slp.free;
  end;
  if Method = 'GET' then
    e.Method := '"GET"'
  else if Method = 'POST' then
    e.Method := '"POST"'
  else if Method = 'HEAD' then
    e.Method := '"HEAD"'
  else
    e.Method := sandenc(Method);
  debug('Sending request [' + URL + ']...', 'XHR');
  e.URL := sandenc(URL);
  e.Details := sandenc(Details);
  e.Username := sandenc(Username);
  e.Password := sandenc(Password);
  js.add('function httpfilter(http) {');
  js.add('var canlog = true;');
  js.add('if (http.status == 0) { canlog = false; }');
  if UserFilter <> emptystr then
    js.add(UserFilter);
  if LuaScript_RequestSent <> emptystr then
  begin
    // whitelists the Lua script for execution
    Tab.whitelistlua(LuaScript_RequestSent);
    js.add('Sandcat.CallWL(' + sandenc(LuaScript_RequestSent) + ');');
  end;
  js.add('return canlog;');
  js.add('}');
  js.add('xmlhttp=new XMLHttpRequest();');
  js.add('xmlhttp.open(' + e.Method + ',' + e.URL + ',' + getAsync(ASync) +
    getAuth() + ');');
  js.add('xmlhttp.onreadystatechange = function () {');
  js.add('if (this.readyState == 4) {');
  js.add('if (httpfilter(this) == true) { ');
  js.add('Sandcat.LogRequest(' + e.Details + ',"' + reqid + '",' + e.Method +
    ',' + e.URL +
    ',"HTTP/1.1 "+this.status.toString()+" "+this.statusText+"\n"+this.getAllResponseHeaders(),this.responseText);');
  js.add('}');
  js.add('}');
  js.add('};');
  js.add('xmlhttp.setRequestHeader("' + vSCRIDHeader + '","' + reqid + '");');
  if PostData <> emptystr then
  begin
    js.add('xmlhttp.setRequestHeader("Content-Type","application/x-www-form-urlencoded");');
    if ReqHeaders <> emptystr then
      js.addstrings(jsheaders);
    js.add('xmlhttp.send(' + sandenc(PostData) + ');');
  end
  else
  begin
    if ReqHeaders <> emptystr then
      js.addstrings(jsheaders);
    js.add('xmlhttp.send();');
  end;
  jscode := js.text;
  js.free;
  jsheaders.free;
  try
    Tab.runJavaScript(jscode, emptystr, 0, emptystr);
  except
  end;
end;

procedure TSandcatXHRRequest.Open(Method, URL: string; ASync: boolean = true);
begin
  self.ASync := ASync;
  self.Method := Method;
  self.URL := URL;
end;

function TSandcatXHRRequest.Send(PostData: string): string;
begin
  self.PostData := PostData;
  result := SendHTTPRequest;
end;

// Lua Object ******************************************************************
function method_open(L: PLua_State): Integer; cdecl;
var
  r: TSCBXHRObject;
begin
  r := TSCBXHRObject(LuaToTLuaObject(L, 1));
  if lua_isnone(L, 4) then
    r.XHR.Open(lua_tostring(L, 2), lua_tostring(L, 3))
  else
    r.XHR.Open(lua_tostring(L, 2), lua_tostring(L, 3), lua_toboolean(L, 4));
  result := 1;
end;

function method_send(L: PLua_State): Integer; cdecl;
var
  r: TSCBXHRObject;
begin
  r := TSCBXHRObject(LuaToTLuaObject(L, 1));
  lua_pushstring(L, r.XHR.Send(lua_tostring(L, 2)));
  result := 1;
end;

function TSCBXHRObject.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'callback') = 0 then
    result := XHR.LuaScript_RequestSent
  else if CompareText(propName, 'details') = 0 then
    result := XHR.Details
  else if CompareText(propName, 'filter') = 0 then
    result := XHR.UserFilter
  else if CompareText(propName, 'method') = 0 then
    result := XHR.Method
  else if CompareText(propName, 'password') = 0 then
    result := XHR.Password
  else if CompareText(propName, 'postdata') = 0 then
    result := XHR.PostData
  else if CompareText(propName, 'requestheaders') = 0 then
    result := XHR.ReqHeaders
  else if CompareText(propName, 'tab') = 0 then
    result := XHR.Tab.UID
  else if CompareText(propName, 'url') = 0 then
    result := XHR.URL
  else if CompareText(propName, 'username') = 0 then
    result := XHR.Username
  else
    result := inherited GetPropValue(propName);
end;

function TSCBXHRObject.SetPropValue(propName: String;
  const AValue: Variant): boolean;
begin
  result := true;
  if CompareText(propName, 'callback') = 0 then
    XHR.LuaScript_RequestSent := String(AValue)
  else if CompareText(propName, 'details') = 0 then
    XHR.Details := String(AValue)
  else if CompareText(propName, 'filter') = 0 then
    XHR.UserFilter := String(AValue)
  else if CompareText(propName, 'method') = 0 then
    XHR.Method := String(AValue)
  else if CompareText(propName, 'password') = 0 then
    XHR.Password := String(AValue)
  else if CompareText(propName, 'postdata') = 0 then
    XHR.PostData := String(AValue)
  else if CompareText(propName, 'requestheaders') = 0 then
    XHR.ReqHeaders := String(AValue)
  else if CompareText(propName, 'tab') = 0 then
    XHR.Tab := tabmanager.gettab(String(AValue))
  else if CompareText(propName, 'url') = 0 then
    XHR.URL := String(AValue)
  else if CompareText(propName, 'username') = 0 then
    XHR.Username := String(AValue)
  else
    result := inherited SetPropValue(propName, AValue);
end;

procedure RegisterSCBHTTPRequest_Sandcat(L: PLua_State);
const
  aObjectName = 'SandcatXHR';
  procedure register_methods(L: PLua_State; classTable: Integer);
  begin
    RegisterMethod(L, 'open', @method_open, classTable);
    RegisterMethod(L, 'send', @method_send, classTable);
  end;
  function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
  begin
    result := TSCBXHRObject.Create(L, AParent);
  end;
  function Create(L: PLua_State): Integer; cdecl;
  var
    p: TLuaObjectNewCallback;
  begin
    p := @new_callback;
    result := new_LuaObject(L, aObjectName, p);
  end;

begin
  RegisterTLuaObject(L, aObjectName, @Create, @register_methods);
end;

constructor TSCBXHRObject.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  XHR := TSandcatXHRRequest.Create;
end;

destructor TSCBXHRObject.Destroy;
begin
  XHR.free;
  inherited Destroy;
end;

end.
