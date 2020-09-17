unit LAPI_TaskMan;

{
  Sandcat Task Manager Lua functions
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses Windows, Messages, Classes, Forms, SysUtils, Dialogs, Controls,
  StdCtrls, Lua, LuaObject, uTaskMan;

type
  TSandcatTaskLuaObject = class(TLuaObject)
  private
  public
    Task: TSandcatTask;
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : boolean; override;
    destructor Destroy; override;
  end;

procedure RegisterSandcatTaskObject(L: PLua_State);
function lua_addbackgroundtask(L: PLua_State): integer; cdecl;
function lua_addbackgroundtask_hidden(L: PLua_State): integer; cdecl;
function lua_bgtaskclear(L: PLua_State): integer; cdecl;
function lua_bgtasksetstatus(L: PLua_State): integer; cdecl;
function lua_bgtasksetcaption(L: PLua_State): integer; cdecl;
function lua_bgtaskseticon(L: PLua_State): integer; cdecl;
function lua_bgtaskremove(L: PLua_State): integer; cdecl;
function lua_bgtaskrunscript(L: PLua_State): integer; cdecl;
function lua_bgtaskstop(L: PLua_State): integer; cdecl;
function lua_bgtasksetparam(L: PLua_State): integer; cdecl;
function lua_bgtasksetparams(L: PLua_State): integer; cdecl;
function lua_bgtaskgetparam(L: PLua_State): integer; cdecl;
function lua_bgtaskspecial(L: PLua_State): integer; cdecl;
function lua_bgtaskgethandle(L: PLua_State): integer; cdecl;
function lua_bgtaskgettidbytag(L: PLua_State): integer; cdecl;
function lua_bgtasksetscript(L: PLua_State): integer; cdecl;
function lua_gettasklist(L: PLua_State): integer; cdecl;
function lua_bgtaskgetstatus(L: PLua_State): integer; cdecl;
function lua_bgtaskgetcaption(L: PLua_State): integer; cdecl;
function lua_bgtaskgetinfo(L: PLua_State): integer; cdecl;
function lua_getdownloadlist(L: PLua_State): integer; cdecl;
function lua_bgtasksuspend(L: PLua_State): integer; cdecl;

implementation

uses uMain, pLua;

// Lua Functions ***************************************************************
function lua_bgtasksetscript(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
  script, event: string;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  event := lua_tostring(L, 2);
  script := lua_tostring(L, 3);
  if Task <> nil then
    Task.SetScript(event, script);
  result := 1;
end;

function lua_bgtaskspecial(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
  cmd: string;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  cmd := lua_tostring(L, 2);
  if Task <> nil then
    Task.DoSpecial(cmd);
  result := 1;
end;

function lua_bgtasksetstatus(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    Task.Status := lua_tostring(L, 2);
  result := 1;
end;

function lua_bgtaskgetstatus(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    lua_pushstring(L, Task.Status);
  result := 1;
end;

function lua_bgtasksetcaption(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    Task.Caption := lua_tostring(L, 2);
  result := 1;
end;

function lua_bgtaskgetcaption(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    lua_pushstring(L, Task.Caption);
  result := 1;
end;

function lua_bgtaskgetinfo(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    Task.GetInfoL(L);
  result := 1;
end;

function lua_bgtaskseticon(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    Task.Icon := lua_tostring(L, 2);
  result := 1;
end;

function lua_bgtasksuspend(L: PLua_State): integer; cdecl;
begin
  if lua_isnone(L, 2) then
    tasks.SuspendResumeTask(lua_tostring(L, 1))
  else
    tasks.SuspendTask(lua_tostring(L, 1), lua_toboolean(L, 2));
  result := 1;
end;

function lua_bgtaskremove(L: PLua_State): integer; cdecl;
begin
  tasks.RemoveTask(lua_tostring(L, 1));
  result := 1;
end;

function lua_bgtaskrunscript(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
  begin
    if lua_isnone(L, 3) = false then
      Task.SetParams(lua_tostring(L, 3));
    Task.RunScript(lua_tostring(L, 2));
  end;
  result := 1;
end;

function lua_bgtaskgettidbytag(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1), true);
  if Task <> nil then
    lua_pushstring(L, Task.TID) else
    lua_pushstring(L, emptystr);
  result := 1;
end;

function lua_bgtaskstop(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    Task.Stop(lua_tostring(L, 2));
  result := 1;
end;

function lua_bgtasksetparam(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    Task.SetParam(lua_tostring(L, 2), lua_tostring(L, 3));
  result := 1;
end;

function lua_bgtasksetparams(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    Task.SetParams(lua_tostring(L, 2));
  result := 1;
end;

function lua_bgtaskgetparam(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
  s: string;
begin
  s := emptystr;
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    s := Task.GetParam(lua_tostring(L, 2), lua_tostring(L, 3));
  lua_pushstring(L, s);
  result := 1;
end;

function lua_bgtaskclear(L: PLua_State): integer; cdecl;
begin
  tasks.clearinactivetasks;
  result := 1;
end;

function lua_gettasklist(L: PLua_State): integer; cdecl;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  tasks.GetTaskList(sl);
  lua_pushstring(L, sl.Text);
  sl.Free;
  result := 1;
end;

function lua_getdownloadlist(L: PLua_State): integer; cdecl;
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  tasks.GetDownloadList(sl);
  lua_pushstring(L, sl.Text);
  sl.Free;
  result := 1;
end;

function lua_bgtaskgethandle(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
  h: integer;
begin
  h := 0;
  Task := tasks.SelectTask(lua_tostring(L, 1));
  if Task <> nil then
    h := Task.msg.MsgHandle;
  plua_pushintnumber(L, h);
  result := 1;
end;

function lua_addbackgroundtask(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.AddTask(lua_tostring(L, 1), false);
  lua_pushstring(L, Task.tid);
  result := 1;
end;

function lua_addbackgroundtask_hidden(L: PLua_State): integer; cdecl;
var
  Task: TSandcatTask;
begin
  Task := tasks.AddTask(lua_tostring(L, 1), true);
  lua_pushstring(L, Task.tid);
  result := 1;
end;

procedure register_methods(L: PLua_State; classTable: integer);
begin
  // RegisterMethod(L,'open', @method_open, classTable);
end;

// Lua Object ******************************************************************
procedure RegisterSandcatTaskObject(L: PLua_State);
const
  aObjectName = 'SandcatTask';
  function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
  begin
    result := TSandcatTaskLuaObject.Create(L, AParent);
  end;
  function Create(L: PLua_State): integer; cdecl;
  var
    p: TLuaObjectNewCallback;
  begin
    p := @new_callback;
    result := new_LuaObject(L, aObjectName, p);
  end;

begin
  RegisterTLuaObject(L, aObjectName, @Create, @register_methods);
end;

function TSandcatTaskLuaObject.GetPropValue(propName: String): Variant;
begin
  result := inherited GetPropValue(propName);
end;

function TSandcatTaskLuaObject.SetPropValue(propName: String;
  const AValue: Variant): boolean;
begin
  result := inherited SetPropValue(propName, AValue);
end;

constructor TSandcatTaskLuaObject.Create(LuaState: PLua_State;
  AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
  Task := tasks.AddTask(emptystr, false);
end;

destructor TSandcatTaskLuaObject.Destroy;
begin
  Task.Free;
  inherited Destroy;
end;

end.
