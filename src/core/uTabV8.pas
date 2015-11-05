unit uTabV8;

{
  Sandcat Browser V8 Extensions (Experimental)
  Copyright (c) 2011-2015 Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

{$I Catarinka.inc}
// {$DEFINE USEV8EXTENSIONS}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF}
{$IFDEF USEWACEF}
  WACefInterfaces, WACefTypes, WACefOwns, WACefLib, WACefRefs,
{$ELSE}
  cefvcl, ceflib,
{$ENDIF}
  CatJSON;

type
  TSandcatV8Extension = class(TCefv8HandlerOwn)
  private
    fV8MsgHandle: integer;
  protected
{$IFDEF USEWACEF}
    function Execute(const name: ustring; const obj: ICefv8Value;
      ArgumentsCount: csize_t; const arguments: TCefv8ValueArray;
      var retval: ICefv8Value; var exception: ustring): Boolean; override;
{$ELSE}
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; override;
{$ENDIF}
  public
    constructor Create; override;
  end;

  TCustomRenderProcessHandler = class(TCefRenderProcessHandlerOwn)
  private
    fSandcatV8Extension: TSandcatV8Extension;
  protected
    function OnProcessMessageReceived(const Browser: ICefBrowser;
      sourceProcess: TCefProcessId; const message: ICefProcessMessage)
      : Boolean; override;
    procedure OnWebKitInitialized; override;
  end;

implementation

uses CatChromium, CatJINI, CatStrings;

{ procedure Send_WriteValue(desthandle: integer; key, value: string);
  var
  j: TCatJSON;
  begin
  j := TCatJSON.Create;
  j['k'] := key;
  j['v'] := value;
  SendCDMessage(desthandle, CRM_JS_WRITEVALUE, j.text);
  j.free;
  end;

  procedure LogRequest(Details, rid, Method, url, rcvdheader, Response: string);
  var
  j: tjinilist;
  begin
  j := tjinilist.Create;
  j.values['ReqID'] := rid;
  j.values['Details'] := Details;
  j.values['ResponseHeaders'] := rcvdheader;
  j.values['Method'] := Method;
  j.values['URL'] := url;
  j.values['ResponseFilename'] := SaveResponseToFile(Response);
  SendCDMessage(fV8MsgHandle, CRM_XHR_LOG, j.text);
  j.free;
  end;
}

function NewCEFString(const s: string): ICefv8Value;
begin
  result := TCefv8ValueRef.{$IFDEF USEWACEF}CreateString{$ELSE}NewString{$ENDIF}(s);
end;

constructor TSandcatV8Extension.Create;
begin
  inherited Create;
end;

{$IFDEF USEWACEF}

function TSandcatV8Extension.Execute(const name: ustring;
  const obj: ICefv8Value; ArgumentsCount: csize_t;
  const arguments: TCefv8ValueArray; var retval: ICefv8Value;
  var exception: ustring): Boolean;
{$ELSE}

function TSandcatV8Extension.Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
{$ENDIF}
begin
  result := false;
  if (name = 'base64encode') then
  begin
    if (Length(arguments) <> 1) or (not arguments[0].IsString) then
    begin
      result := false;
      exit;
    end;
    retval := NewCEFString(base64encode(arguments[0].GetStringValue));
    result := true;
  end
  else if (name = 'base64decode') then
  begin
    if (Length(arguments) <> 1) or (not arguments[0].IsString) then
    begin
      result := false;
      exit;
    end;
    retval := NewCEFString(base64decode(arguments[0].GetStringValue));
    result := true;
    { end
      else if (name = 'consoleoutput') then
      begin
      if (Length(arguments) = 0) or (not arguments[0].IsBool) then
      begin
      Result := false;
      exit;
      end;
      if arguments[0].GetBoolValue = false then
      SendCDMessage(fV8MsgHandle, CRM_CONSOLE_ENDEXTERNALOUTPUT, emptystr);
      Result := true;
      end
      else if (name = 'logrequest') then
      begin
      if (Length(arguments) <> 6) or (not arguments[0].IsString) or
      (not arguments[1].IsString) or (not arguments[2].IsString) or
      (not arguments[3].IsString) or (not arguments[4].IsString) or
      (not arguments[5].IsString) then
      begin
      Result := false;
      exit;
      end;
      LogRequest(arguments[0].GetStringValue, arguments[1].GetStringValue,
      arguments[2].GetStringValue, arguments[3].GetStringValue,
      arguments[4].GetStringValue, arguments[5].GetStringValue);
      Result := true;
      end
      else if (name = 'writevalue') then
      begin
      if (Length(arguments) <> 2) or (not arguments[0].IsString) or
      (not arguments[1].IsString) then
      begin
      Result := false;
      exit;
      end;
      Send_WriteValue(fV8MsgHandle, arguments[0].GetStringValue,
      CEFV8ValueToStr(arguments[1]));
      Result := true;
      end
      else if (name = 'writeln') then
      begin
      if (Length(arguments) <> 1) then
      begin
      Result := false;
      exit;
      end;
      SendCDMessage(fV8MsgHandle, CRM_JS_WRITELN, CEFV8ValueToStr(arguments[0]));
      Result := true;
      end
      else if (name = 'write') then
      begin
      if (Length(arguments) <> 1) then
      begin
      Result := false;
      exit;
      end;
      SendCDMessage(fV8MsgHandle, CRM_JS_WRITE, CEFV8ValueToStr(arguments[0]));
      Result := true; }
  end;
end;

procedure TCustomRenderProcessHandler.OnWebKitInitialized;
const
  v8extension = '' + 'var Sandcat;' + 'if (!Sandcat) Sandcat = {};' +
    '(function() {' +
    'Sandcat.Base64Encode = function(s) { native function base64encode(); return base64encode(s); };'
    + 'Sandcat.Base64Decode = function(s) { native function base64decode(); return base64decode(s); };'
    + 'Sandcat.LogRequest = function(details,rid,method,url,rcvdhead,response) { native function logrequest(); logrequest(details,rid,method,url,rcvdhead,response); };'
    + 'Sandcat.ConsoleOutput = function(b) { native function consoleoutput(); consoleoutput(b); };'
    + 'Sandcat.WriteLn = function(s) { native function writeln(); writeln(s); };'
    + 'Sandcat.WriteValue = function(key,value) { native function writevalue(); writevalue(key,value); };'
    + 'Sandcat.Write = function(s) { native function write(); write(s); };'
    + '})();';
begin
  fSandcatV8Extension := TSandcatV8Extension.Create;
{$IFDEF USEWACEF}
  TWACef.RegisterExtension('v8/browser', v8extension,
    fSandcatV8Extension as ICefV8Handler);
{$ELSE}
  CefRegisterExtension('v8/browser', v8extension,
    fSandcatV8Extension as ICefV8Handler);
{$ENDIF}
end;

function TCustomRenderProcessHandler.OnProcessMessageReceived
  (const Browser: ICefBrowser; sourceProcess: TCefProcessId;
  const message: ICefProcessMessage): Boolean;
begin
  result := false;
  if (message.getName = 'msg') then
  begin
    result := true;
    case message.getArgumentList.GetInt(0) of
      SCTM_SET_V8_MSGHANDLE:
        fSandcatV8Extension.fV8MsgHandle := message.getArgumentList.GetInt(1);
      SCTM_V8_REGISTEREXTENSION:
        begin // CefRegisterExtension not working from here
          // CefRegisterExtension('v8/browserx',message.getArgumentList.GetString(1), SandcatV8Extension as ICefV8Handler);
        end;
    end;
  end;
end;

initialization

{$IFDEF USEV8EXTENSIONS}
  CefRenderProcessHandler := TCustomRenderProcessHandler.Create;
{$ENDIF}

end.
