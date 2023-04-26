unit uTabV8;

{
  Sandcat Browser V8 Extensions (Experimental)
  Copyright (c) 2011-2015 Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

{$I Catarinka.inc}
{$I SandcatEngine.inc}
// {$DEFINE USEV8EXTENSIONS}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes,
{$ELSE}
  Classes,
{$ENDIF}
{$IFNDEF USEWEBVIEW2}
  cefvcl, ceflib,
{$ENDIF}
  CatJSON;

 {$IFNDEF USEWEBVIEW2}
type
  TSandcatV8Extension = class(TCefv8HandlerOwn)
  private
    fV8MsgHandle: integer;
  protected
    function Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean; override;
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
{$ENDIF}

implementation

uses CatChromium, CatChromiumLib, CatJINI, CatStrings;

{$IFNDEF USEWEBVIEW2}

constructor TSandcatV8Extension.Create;
begin
  inherited Create;
end;


function TSandcatV8Extension.Execute(const name: ustring; const obj: ICefv8Value;
      const arguments: TCefv8ValueArray; var retval: ICefv8Value;
      var exception: ustring): Boolean;
begin
  result := false;
  if (name = 'base64encode') then
  begin
    if (Length(arguments) <> 1) or (not arguments[0].IsString) then
    begin
      result := false;
      exit;
    end;
    retval := StrToCEFV8Value(base64encode(arguments[0].GetStringValue));
    result := true;
  end
  else if (name = 'base64decode') then
  begin
    if (Length(arguments) <> 1) or (not arguments[0].IsString) then
    begin
      result := false;
      exit;
    end;
    retval := StrToCEFV8Value(base64decode(arguments[0].GetStringValue));
    result := true;
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
  CefRegisterExtension('v8/browser', v8extension,
    fSandcatV8Extension as ICefV8Handler);
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

{$ENDIF}

initialization

{$IFDEF USEV8EXTENSIONS}
  {$IFNDEF USEWEBVIEW2}
  CefRenderProcessHandler := TCustomRenderProcessHandler.Create;
  {$ENDIF}
{$ENDIF}

end.
