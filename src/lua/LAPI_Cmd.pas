unit LAPI_Cmd;

{
  Sandcat Console Command LUA Object
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Classes, Forms, SysUtils, Graphics, Lua, LuaObject;

type
  TSCBCmdObject = class(TLuaObject)
  private
  public
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

procedure RegisterSCBCmd_Sandcat(L: PLua_State);

implementation

uses
  uMain, CatStrings, plua, uZones;

procedure register_methods(L: PLua_State; classTable: Integer);
begin
  // RegisterMethod(L,'methodname',method_func, classTable);
end;

function TSCBCmdObject.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'name') = 0 then
    result := before(SandConsole.LastCommand, ' ')
  else if CompareText(propName, 'params') = 0 then
    result := after(SandConsole.LastCommand, ' ')
  else
    result := inherited GetPropValue(propName);
end;

function TSCBCmdObject.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  // if CompareText(propName, 'propname') = 0 then someproc(String(avalue)) else
  // end else
  result := inherited SetPropValue(propName, AValue);
end;

const
  cObjName = 'SandcatBrowserCommand';

function new_callback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TSCBCmdObject.Create(L, AParent);
end;

function Create(L: PLua_State): Integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @new_callback;
  result := new_LuaObject(L, cObjName, p);
end;

procedure RegisterSCBCmd_Sandcat(L: PLua_State);
begin
  RegisterTLuaObject(L, cObjName, @Create, @register_methods);
end;

constructor TSCBCmdObject.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
end;

destructor TSCBCmdObject.Destroy;
begin
  inherited Destroy;
end;

end.
