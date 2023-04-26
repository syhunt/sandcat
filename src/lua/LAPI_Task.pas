unit LAPI_Task;

{
  Sandcat Task Process component
  A Sandcat task that runs in a separate process

  Copyright (c) 2011-2020, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Windows, Classes, Forms, Messages, SysUtils, Dialogs, Lua, ShellAPI, Variants,
  TypInfo, SyncObjs, LuaObject, uUIComponents;

type
  TSandcatTaskProcess = class(TComponent)
  private
    fLoaded: Boolean;
    fLuaWrap: TSandLua;
  public
    procedure Run(const tid: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ScriptExceptionHandler(Title: string; Line: Integer; Msg: string;
      var handled: Boolean);
  end;

type
  TSandcatTaskProcessLuaObject = class(TLuaObject)
  private
    fCaption: string;
    fStatus: string;
    fTag: string;
  public
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: string): Variant; override;
    function SetPropValue(propName: string; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

function RunTaskSeparateProcess(tid, script: string; tabhandle: Integer;
  UserParams: TSandJSON): Cardinal;
function GetTaskFileName(tid: string): string;
procedure RegisterTaskProcessObject(L: PLua_State);

implementation

uses
  uTab, uSettings, uMain, LAPI_Browser, CatFiles, CatUI, CatZIP, CatStrings, pLua,
  CatTasks, CatChromium, uConst, CatUtils, CatMsgCromis, LAPI_CEF;

var
  UserParams: TSandJSON;
  TaskID: string;
  ProgressMax: Integer = 100;
  tabhandle: Integer = 0;
  TaskStopped: Boolean;

procedure SendJSONCmd(json: string);
begin
  SendCromisMessage(tabhandle, SCBM_TASK_RUNJSONCMD, json);
end;

function SendJSONCmdStr(cmd, str: string): Integer;
var
  j: TSandJSON;
begin
  result := 1;
  j := TSandJSON.Create;
  j['tid'] := TaskID;
  j['cmd'] := cmd;
  j['s'] := str;
  SendJSONCmd(j.Text);
  j.free;
end;

function SendTabJSONCmdStr(cmd, str: string): Integer;
var
  j: TSandJSON;
begin
  result := 1;
  j := TSandJSON.Create;
  j['tid'] := TaskID;
  j['cmd'] := cmd;
  j['s'] := str;
  SendCromisMessage(tabhandle, SCBM_RUNJSONCMD, j.Text);
  j.free;
end;

procedure LogLuaError(sender: string; Line: Integer; Msg: string);
var
  j: TSandJSON;
begin
  j := TSandJSON.Create;
  j['tid'] := TaskID;
  j['sender'] := sender;
  j['line'] := Line;
  j['msg'] := Msg;
  SendCromisMessage(tabhandle, SCBM_LOGCUSTOMSCRIPTERROR, j.Text);
  j.free;
end;

procedure Task_SetParam(tabhandle: Integer; tid, name, value: string);
var
  p: TSandJINI;
begin
  p := TSandJINI.Create;
  p.values['TID'] := tid;
  p.values['Name'] := name;
  p.WriteString('data', 'Value', value, 'base64');
  SendCromisMessage(tabhandle, SCBM_TASK_SETPARAM, p.Text);
  p.free;
end;

function PrintSpecial(const specialcmd, s: string): Integer;
begin
  result := 1;
  SendJSONCmdStr('special', specialcmd);
  SendJSONCmdStr('writeln', s);
end;

function lua_method_printsuccess(L: PLua_State): Integer; cdecl;
begin
  result := PrintSpecial('paintgreen', plua_AnyToString(L, 1));
end;

function lua_method_printfailure(L: PLua_State): Integer; cdecl;
begin
  result := PrintSpecial('paintred', plua_AnyToString(L, 1));
end;

function lua_method_printfatalerror(L: PLua_State): Integer; cdecl;
begin
  result := PrintSpecial('paintyellow', plua_AnyToString(L, 1));
end;

function lua_method_outputmsg(L: PLua_State): Integer; cdecl;
var
  j: TSandJSON;
  Msg: string;
  imgindex: Integer;
begin
  Msg := lua_tostring(L, 1);
  imgindex := -1;
  if lua_isnone(L, 2) = false then
    imgindex := lua_tointeger(L, 2);
  j := TSandJSON.Create;
  j['tid'] := TaskID;
  j['cmd'] := 'outputmsg';
  j['s'] := Msg;
  j.sObject.I['i'] := imgindex;
  SendCromisMessage(tabhandle, SCBM_TASK_RUNJSONCMD, j.Text);
  j.free;
  result := 1;
end;

function lua_task_writeln(L: PLua_State): Integer; cdecl;
begin
  result := SendJSONCmdStr('writeln', plua_AnyToString(L, 1));
end;

function lua_task_write(L: PLua_State): Integer; cdecl;
begin
  result := SendJSONCmdStr('write', plua_AnyToString(L, 1));
end;

function lua_console_writeln(L: PLua_State): Integer; cdecl;
begin
  result := SendCromisMessage(tabhandle, SCBM_LOGWRITELN,
    plua_AnyToString(L, 1));
end;

function lua_console_write(L: PLua_State): Integer; cdecl;
begin
  result := SendCromisMessage(tabhandle, SCBM_LOGWRITE, plua_AnyToString(L, 1));
end;

function lua_ScriptLogError(L: PLua_State): Integer; cdecl;
begin
  LogLuaError('Task', lua_tointeger(L, 1), lua_tostring(L, 2));
  result := 1;
end;

function lua_getParamStr(L: PLua_State): Integer; cdecl;
var
  s: string;
begin
  if UserParams.HasPath(lua_tostring(L, 1)) then
  begin
    s := UserParams.sObject.s[lua_tostring(L, 1)];
    lua_pushstring(L, s);
  end
  else
    lua_pushstring(L, lua_tostring(L, 2));
  result := 1;
end;

function lua_getParamInt(L: PLua_State): Integer; cdecl;
begin
  if UserParams.HasPath(lua_tostring(L, 1)) then
    plua_pushintnumber(L, UserParams.sObject.I[lua_tostring(L, 1)])
  else
    plua_pushintnumber(L, lua_tointeger(L, 2));
  result := 1;
end;

function lua_getParamBool(L: PLua_State): Integer; cdecl;
begin
  if UserParams.HasPath(lua_tostring(L, 1)) then
    lua_pushboolean(L, UserParams.sObject.b[lua_tostring(L, 1)])
  else
    lua_pushboolean(L, lua_toboolean(L, 2));
  result := 1;
end;

procedure TSandcatTaskProcess.ScriptExceptionHandler(Title: string;
  Line: Integer; Msg: string; var handled: Boolean);
begin
  LogLuaError('Task', Line, format('%s: %s', [Title, Msg]));
  handled := true;
end;

function lua_Params_getParam(L: PLua_State): Integer; cdecl;
var
  v: variant;
begin
  if UserParams.HasPath(lua_tostring(L, 2)) then
  begin
    v := UserParams.GetValue(lua_tostring(L, 2),variants.null);
    plua_pushvariant(L, v);
  end;
  result := 1;
end;

function lua_Params_SetParam(L: PLua_State): Integer; cdecl;
begin
  if UserParams.HasPath(lua_tostring(L, 2)) then
    UserParams.SetValue(lua_tostring(L, 2),plua_tovariant(L, 3));
  Task_SetParam(tabhandle, TaskID, lua_tostring(L, 2), lua_tostring(L, 3));
  result := 1;
end;

function lua_getappdir(L: PLua_State): Integer; cdecl;
begin
  lua_pushstring(L, extractfilepath(paramstr(0)));
  result := 1;
end;

function lua_method_runtabcmd(L: PLua_State): Integer; cdecl;
begin
  result := SendTabJSONCmdStr(lua_tostring(L, 1), lua_tostring(L, 2));
end;

function lua_method_runcmd(L: PLua_State): Integer; cdecl;
begin
  result := SendJSONCmdStr(lua_tostring(L, 1), lua_tostring(L, 2));
end;

constructor TSandcatTaskProcess.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fLuaWrap := TSandLua.Create(self);
  fLuaWrap.OnException := ScriptExceptionHandler;
  fLuaWrap.UseDebug := false;
  fLoaded := false;
  fLuaWrap.RegisterLuaMethod('print', @lua_task_writeln);
  fLuaWrap.RegisterLuaMethod('printfailure', @lua_method_printfailure);
  fLuaWrap.RegisterLuaMethod('printfatalerror', @lua_method_printfatalerror);
  fLuaWrap.RegisterLuaMethod('printsuccess', @lua_method_printsuccess);
  fLuaWrap.RegisterLuaMethod('outputmsg', @lua_method_outputmsg);
  fLuaWrap.RegisterLuaMethod('runtabcmd', @lua_method_runtabcmd);
  fLuaWrap.RegisterLuaMethod('runcmd', @lua_method_runcmd);
  // for IO redirect from Underscript library
  fLuaWrap.RegisterLuaMethod('sandcat_writeln', @lua_task_writeln);
  fLuaWrap.RegisterLuaMethod('sandcat_write', @lua_task_write);
  fLuaWrap.RegisterLuaMethod('sandcat_logerror', @lua_ScriptLogError);
  // LuaWrap.RegisterLuaMethod('cswrite', @lua_console_Write);
  // LuaWrap.RegisterLuaMethod('cswriteln', @lua_console_WriteLn);
  fLuaWrap.RegisterLuaMethod('getappdir', @lua_getappdir);
  fLuaWrap.RegisterLuaMethod('paramstr', @lua_getParamStr);
  fLuaWrap.RegisterLuaMethod('parambool', @lua_getParamBool);
  fLuaWrap.RegisterLuaMethod('paramint', @lua_getParamInt);
  RegisterTaskProcessObject(fLuaWrap.LuaState);
  RegisterCEF(fLuaWrap.LuaState);
  fLuaWrap.RegisterLuaTable('params', @lua_Params_getParam,
    @lua_Params_SetParam);
  UserParams := TSandJSON.Create;
  fLuaWrap.LoadScript(emptystr);
end;

destructor TSandcatTaskProcess.Destroy;
begin
  UserParams.free;
  fLuaWrap.free;
  inherited Destroy;
end;

function GetTaskFileName(tid: string): string;
begin
  result := GetSandcatDir(SCDIR_TASKS) + tid + '.json';
end;

function RunTaskSeparateProcess(tid, script: string; tabhandle: Integer;
  UserParams: TSandJSON): Cardinal;
var
  f: string;
  j: TSandJINI;
begin
  result := 0;
  if tid = emptystr then
    exit;
  if script = emptystr then
    exit;
  f := GetTaskFileName(tid);
  j := TSandJINI.Create;
  forcedir(extractfilepath(f));
  j.Filename := f;
  j.WriteInteger(tid, 'tabhandle', tabhandle);
  if UserScript.Lua_Task_Init <> emptystr then
    j.WriteString(tid, 'initscript', UserScript.Lua_Task_Init);
  j.WriteString(tid, 'script', base64encode(script));
  j.WriteString(tid, 'params', base64encode(UserParams.Text));
  j.SaveToFile;
  j.free;
  result := RunTask(paramstr(0) + ' ' + cBgTaskPrefix + tid, false, SW_HIDE);
end;

procedure TSandcatTaskProcess.Run(const tid: string);
var
  f, script: string;
  j: TSandJINI;
begin
  TaskStopped := false;
  f := GetTaskFileName(tid);
  if fileexists(f) = false then
    exit;
  j := TSandJINI.Create;
  j.Filename := f;
  j.LoadFromFile;
  deletefile(f);
  TaskID := tid;
  fLuaWrap.value['tid'] := tid;
  fLuaWrap.value['pid'] := GetCurrentProcessID;
  tabhandle := j.ReadInteger(tid, 'tabhandle', 0);
  fLuaWrap.ExecuteCmd('task = SandcatTaskProcess:new()');
  fLuaWrap.ExecuteCmd('task.params = params');
  fLuaWrap.ExecuteCmd('task.id = tid');
  fLuaWrap.ExecuteCmd('task.pid = pid');
  fLuaWrap.ExecuteCmd(j.ReadString(tid, 'initscript', emptystr));
  UserParams.Text := base64decode(j.ReadString(tid, 'params', emptystr));
  script := base64decode(j.ReadString(tid, 'script', emptystr));
  fLuaWrap.ExecuteCmd(script);
  if TaskStopped = false then
    fLuaWrap.ExecuteCmd('task:finish(task.status)');
  fLuaWrap.ExecuteCmd('task:release()');
  j.free;
  SendCromisMessage(tabhandle, SCBM_CONSOLE_ENDEXTERNALOUTPUT, emptystr);
  application.ProcessMessages;
  catdelay(1);
end;

// Lua Object ******************************************************************
function lua_readpakfile(L: PLua_State): Integer; cdecl;
var
  pakfile: string;
  script: tstringlist;
begin
  pakfile := GetSandcatDir(SCDIR_PLUGINS) + lua_tostring(L, 2);
  if fileexists(pakfile) then
  begin
    script := tstringlist.Create;
    script.Text := GetTextFileFromZIP(pakfile, lua_tostring(L, 3));
    lua_pushstring(L, script.Text);
    script.free;
  end;
  result := 1;
end;

function lua_dofile(L: PLua_State): Integer; cdecl;
var
  extfile: string;
  script: tstringlist;
begin
  extfile := GetSandcatDir(SCDIR_PLUGINS) + lua_tostring(L, 2);
  if fileexists(extfile) then
  begin
    script := tstringlist.Create;
    script.Text := GetTextFileFromZIP(extfile, lua_tostring(L, 3));
    showmessage(script.Text);
    plua_dostring(L, script.Text);
    script.free;
  end;
  result := 1;
end;

function lua_LogRequest(L: PLua_State): Integer; cdecl;
begin
  if lua_tostring(L, 2) <> emptystr then
    SendCromisMessage(tabhandle, SCBM_LOGEXTERNALREQUEST_JSON,
      lua_tostring(L, 2));
  result := 1;
end;

function lua_request_send(L: PLua_State): Integer; cdecl;
begin
  result := SendCromisMessage(tabhandle, SCBM_REQUEST_SEND, lua_tostring(L, 2));
end;

function lua_browser_dostring(L: PLua_State): Integer; cdecl;
begin
  result := SendCromisMessage(tabhandle, SCBM_LUA_RUN, lua_tostring(L, 2));
end;

function lua_browser_newtab(L: PLua_State): Integer; cdecl;
begin
  result := SendCromisMessage(tabhandle, SCBM_NEWTAB, lua_tostring(L, 2));
end;

function lua_setprogress(L: PLua_State): Integer; cdecl;
  procedure Task_SetProgress(pos: Integer; max: Integer = 100);
  var
    j: TSandJSON;
  begin
    j := TSandJSON.Create;
    j['tid'] := TaskID;
    j['cmd'] := 'setprogress';
    j.sObject.I['p'] := pos;
    j.sObject.I['m'] := max;
    SendCromisMessage(tabhandle, SCBM_TASK_RUNJSONCMD, j.Text);
    j.free;
  end;

begin
  if lua_isnone(L, 3) then
    Task_SetProgress(lua_tointeger(L, 2))
  else
    Task_SetProgress(lua_tointeger(L, 2), lua_tointeger(L, 3));
  result := 1;
end;

function lua_setscript(L: PLua_State): Integer; cdecl;
var
  j: TSandJSON;
  event, script: string;
begin
  event := lua_tostring(L, 2);
  script := lua_tostring(L, 3);
  j := TSandJSON.Create;
  j['tid'] := TaskID;
  j['cmd'] := 'setscript';
  j['e'] := event;
  j['s'] := script;
  result := SendCromisMessage(tabhandle, SCBM_TASK_RUNJSONCMD, j.Text);
  j.free;
end;

function lua_showmessage(L: PLua_State): Integer; cdecl;
begin
  result := SendJSONCmdStr('showmsg', lua_tostring(L, 2));
end;

function lua_method_special(L: PLua_State): Integer; cdecl;
begin
  result := SendJSONCmdStr('special', lua_tostring(L, 2));
end;

function lua_method_stop(L: PLua_State): Integer; cdecl;
begin
  TaskStopped := true;
  result := SendJSONCmdStr('stop', lua_tostring(L, 2));
end;

function lua_method_finish(L: PLua_State): Integer; cdecl;
begin
  TaskStopped := true;
  result := SendJSONCmdStr('finish', lua_tostring(L, 2));
end;

procedure register_methods(L: PLua_State; classTable: Integer);
begin
  RegisterMethod(L, 'browserdostring', @lua_browser_dostring, classTable);
  RegisterMethod(L, 'browsernewtab', @lua_browser_newtab, classTable);
  RegisterMethod(L, 'dopackfile', @lua_dofile, classTable);
  RegisterMethod(L, 'getpackfile', @lua_readpakfile, classTable);
  RegisterMethod(L, 'logrequest', @lua_LogRequest, classTable);
  RegisterMethod(L, 'sendrequest', @lua_request_send, classTable);
  RegisterMethod(L, 'setprogress', @lua_setprogress, classTable);
  RegisterMethod(L, 'setscript', @lua_setscript, classTable);
  RegisterMethod(L, 'showmessage', @lua_showmessage, classTable);
  RegisterMethod(L, 'special', @lua_method_special, classTable);
  RegisterMethod(L, 'stop', @lua_method_stop, classTable);
  RegisterMethod(L, 'finish', @lua_method_finish, classTable);
end;

const
  aObjectName = 'SandcatTaskProcess';

function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TSandcatTaskProcessLuaObject.Create(L, AParent);
end;

function Create(L: PLua_State): Integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @new_callback;
  result := new_LuaObject(L, aObjectName, p);
end;

procedure RegisterTaskProcessObject(L: PLua_State);
begin
  RegisterTLuaObject(L, aObjectName, @Create, @register_methods);
end;

type
  TProps = (prop_status, prop_caption, prop_tag);

function TSandcatTaskProcessLuaObject.GetPropValue(propName: string): Variant;
begin
  case TProps(GetEnumValue(TypeInfo(TProps), 'prop_' + lowercase(propName))) of
    prop_caption:
      result := fCaption;
    prop_status:
      result := fStatus;
    prop_tag:
      result := fTag;
  else
    result := inherited GetPropValue(propName);
  end;
end;

function TSandcatTaskProcessLuaObject.SetPropValue(propName: string;
  const AValue: Variant): Boolean;
  function setprop(const cmd, value: string):string;
  begin
    SendJSONCmdStr(cmd, value);
    result := value;
  end;

begin
  result := true;
  case TProps(GetEnumValue(TypeInfo(TProps), 'prop_' + lowercase(propName))) of
    prop_caption:
      fCaption := setprop('setcaption',string(AValue));
    prop_status:
      fStatus := setprop('setstatus', string(AValue));
    prop_tag:
      fStatus := setprop('settag', string(AValue));
  else
    result := inherited SetPropValue(propName, AValue);
  end;
end;

constructor TSandcatTaskProcessLuaObject.Create(LuaState: PLua_State;
  AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
end;

destructor TSandcatTaskProcessLuaObject.Destroy;
begin
  inherited Destroy;
end;

end.
