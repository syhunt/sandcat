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
    constructor Create(LuaState: PLua_State;
      AParent: TLuaObject = nil); overload;
    function GetPropValue(propName: AnsiString): Variant; override;
    function SetPropValue(propName: AnsiString; const AValue: Variant)
      : Boolean; override;
  public
    destructor Destroy; override;
  published
  end;

procedure RegisterSCBCmd_Sandcat(L: PLua_State);

implementation

uses
  uMain, CatStrings, plua, uZones;

procedure methods_XCL(L: PLua_State; classTable: Integer);
begin
  // RegisterMethod(L,'methodname',method_func, classTable);
end;

function TSCBCmdObject.GetPropValue(propName: AnsiString): Variant;
begin
  if CompareText(propName, 'name') = 0 then
    result := before(SandConsole.LastCommand, ' ')
  else if CompareText(propName, 'params') = 0 then
    result := after(SandConsole.LastCommand, ' ')
  else
    result := inherited GetPropValue(propName);
end;

function TSCBCmdObject.SetPropValue(propName: AnsiString;
  const AValue: Variant): Boolean;
begin
  result := true;
  // if CompareText(propName, 'propname') = 0 then someproc(AnsiString(avalue)) else
  // end else
  result := inherited SetPropValue(propName, AValue);
end;

function XCL_new(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TSCBCmdObject.Create(L, AParent);
end;

function new_XCL(L: PLua_State): Integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @XCL_new;
  result := new_LuaObject(L, 'SandcatBrowserCommand', p);
end;

procedure RegisterSCBCmd_Sandcat(L: PLua_State);
begin
  RegisterTLuaObject(L, 'SandcatBrowserCommand', @new_XCL, @methods_XCL);
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
