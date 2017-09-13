unit LAPI_Element;
{
  Sandcat UI Element LUA Object
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Classes, Forms, SysUtils, Graphics, Dialogs, Lua, LuaObject, Variants;

type
  TSCBUIElementObject = class(TLuaObject)
  private
  public
    Engine: string;
    Selector: string;
    EngineID: integer;
    procedure SetEngine(Name: string);
    procedure EnableElement(Enable: Boolean = true);
    function GetElementValue(Selector: string): Variant;
    procedure SetElementValue(Selector, NewValue: Variant);
    function GetElementAttribute(Selector, Name: string): Variant;
    procedure SetElementAttribute(Selector, Name: string; NewValue: Variant);
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

type
  TSCBUIEngineObject = class(TLuaObject)
  private
    function GetUIXTable: string;
  public
    Engine: string;
    EngineID: integer;
    procedure SetEngine(Name: string);
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

procedure RegisterSCBUIElement_Sandcat(L: PLua_State);
procedure RegisterSCBUIEngine_Sandcat(L: PLua_State);
function ElemValueToText(value: string): string;

implementation

uses
  uMain, plua, uUIComponents, uMisc, CatStrings, CatHTTP, uZones, uTab, uConst;

function ElemValueToText(value: string): string;
begin
  result := htmlunescape(value);
  result := replacestr(result, '<br/>', crlf);
  result := striphtml(result);
  result := trim(result);
end;

function GetSciterObject(L: PLua_State): TSandUIEngine;
begin
  result := GetZoneByID(TSCBUIElementObject(LuaToTLuaObject(L, 1)).EngineID);
end;

function GetEngine(L: PLua_State): TSandUIEngine;
begin
  result := GetZoneByID(TSCBUIEngineObject(LuaToTLuaObject(L, 1)).EngineID);
end;

function method_select(L: PLua_State): integer; cdecl;
var
  o: TSCBUIElementObject;
begin
  o := TSCBUIElementObject(LuaToTLuaObject(L, 1));
  o.Selector := lua_tostring(L, 2);
  if lua_tostring(L, 3) <> emptystr then
    o.SetEngine(lua_tostring(L, 3));
  result := 1;
end;

function method_getval(L: PLua_State): integer; cdecl;
var
  v: Variant;
  o: TSCBUIElementObject;
begin
  o := TSCBUIElementObject(LuaToTLuaObject(L, 1));
  v := o.GetElementValue(o.Selector);
  plua_pushvariant(L, v);
  result := 1;
end;

function method_setval(L: PLua_State): integer; cdecl;
var
  nv: Variant;
  o: TSCBUIElementObject;
begin
  o := TSCBUIElementObject(LuaToTLuaObject(L, 1));
  nv := plua_tovariant(L, 3);
  o.SetElementValue(o.Selector, nv);
  result := 1;
end;

function method_setattrib(L: PLua_State): integer; cdecl;
var
  newval: Variant;
  o: TSCBUIElementObject;
begin
  o := TSCBUIElementObject(LuaToTLuaObject(L, 1));
  newval := plua_tovariant(L, 3);
  o.SetElementAttribute(o.Selector, lua_tostring(L, 2), newval);
  result := 1;
end;

function method_getattrib(L: PLua_State): integer; cdecl;
var
  v: Variant;
  o: TSCBUIElementObject;
begin
  o := TSCBUIElementObject(LuaToTLuaObject(L, 1));
  v := o.GetElementAttribute(o.Selector, lua_tostring(L, 2));
  plua_pushvariant(L, v);
  result := 1;
end;

function method_setstyleattrib(L: PLua_State): integer; cdecl;
var
  nv: Variant;
  o: TSCBUIElementObject;
  e: ISandUIElement;
begin
  o := TSCBUIElementObject(LuaToTLuaObject(L, 1));
  nv := plua_tovariant(L, 3);
  e := GetZoneByID(o.EngineID).root.Select(o.Selector);
  if e <> nil then
    e.styleattr[lua_tostring(L, 2)] := nv;
  result := 1;
end;

function method_getstyleattrib(L: PLua_State): integer; cdecl;
var
  v: Variant;
  o: TSCBUIElementObject;
  e: ISandUIElement;
begin
  o := TSCBUIElementObject(LuaToTLuaObject(L, 1));
  e := GetZoneByID(o.EngineID).root.Select(o.Selector);
  if e <> nil then
    v := e.styleattr[lua_tostring(L, 2)]
  else
    v := null;
  plua_pushvariant(L, v);
  result := 1;
end;

function method_engine_eval(L: PLua_State): integer; cdecl;
var
  s: TSandUIEngine;
  str: widestring;
  res: string;
begin
  s := GetEngine(L);
  str := lua_tostring(L, 2);
  if s <> nil then
  begin
    res := s.eval(str);
    lua_pushstring(L, res);
  end;
  result := 1;
end;

function method_engine_hide(L: PLua_State): integer; cdecl;
var
  s: TSandUIEngine;
begin
  s := GetEngine(L);
  if s <> nil then
    s.Height := 0;
  result := 1;
end;

function method_engine_load(L: PLua_State): integer; cdecl;
var
  s, tablename: string;
var
  o: TSCBUIEngineObject;
const
  cContent = '{$Content}';
begin
  o := TSCBUIEngineObject(LuaToTLuaObject(L, 1));
  s := lua_tostring(L, 2);
  tablename := htmlescape(lua_tostring(L, 3));
  if s = emptystr then
    s := cBlank_Htm;
  if tablename <> emptystr then
    s := '<meta name="SandcatUIX" content="' + tablename + '">' + crlf + s;
  case o.EngineID of
    ENGINE_BOTTOMBAR:
      BottomBar.LoadBottomBar(s);
    ENGINE_EXTENSIONPAGE:
      contentarea.ToolsBar.LoadPage(s);
    ENGINE_CUSTOMTAB:
      tabmanager.ActiveTab.LoadExtensionPage(s);
  else
    s := replacestr(GetPakResourceAsString('tab_custom.html'), cContent, s);
    GetEngine(L).loadhtml(s, pluginsdir);
  end;
  result := 1;
end;

// Standard loading
function method_engine_loadstd(L: PLua_State): integer; cdecl;
var
  s: string;
var
  o: TSCBUIEngineObject;
begin
  o := TSCBUIEngineObject(LuaToTLuaObject(L, 1));
  s := lua_tostring(L, 2);
  if s = emptystr then
    s := cBlank_Htm;
  case o.EngineID of
    ENGINE_BOTTOMBAR:
      BottomBar.Engine.loadhtml(s, pluginsdir);
    ENGINE_EXTENSIONPAGE:
      ExtensionPage.loadhtml(s, pluginsdir);
    ENGINE_CUSTOMTAB:
      tabmanager.ActiveTab.LoadExtensionPage(s);
  else
    GetEngine(L).loadhtml(s, pluginsdir);
  end;
  result := 1;
end;

function method_engine_loadurl(L: PLua_State): integer; cdecl;
var
  s: string;
  // o: TSCBUIEngineObject;
begin
  // o := TSCBUIEngineObject(LuaToTLuaObject(L, 1));
  s := lua_tostring(L, 2);
  GetEngine(L).loadurl(s);
  result := 1;
end;

// Element Object **************************************************************
procedure TSCBUIElementObject.SetEngine(Name: string);
begin
  if name = emptystr then
    exit;
  EngineID := GetZoneID(Name);
  Engine := name;
end;

function TSCBUIElementObject.GetElementValue(Selector: string): Variant;
var
  e: ISandUIElement;
begin
  e := GetZoneByID(EngineID).root.Select(Selector);
  if e <> nil then
    result := e.Value
  else
    result := null;
end;

procedure TSCBUIElementObject.SetElementValue(Selector, NewValue: Variant);
var
  e: ISandUIElement;
begin
  e := GetZoneByID(EngineID).root.Select(Selector);
  if e <> nil then
    e.Value := NewValue;
end;

function TSCBUIElementObject.GetElementAttribute(Selector,
  Name: string): Variant;
var
  e: ISandUIElement;
begin
  e := GetZoneByID(EngineID).root.Select(Selector);
  if e <> nil then
    result := e.attr[Name]
  else
    result := null;
end;

procedure TSCBUIElementObject.SetElementAttribute(Selector, Name: string;
  NewValue: Variant);
var
  e: ISandUIElement;
begin
  e := GetZoneByID(EngineID).root.Select(Selector);
  if e <> nil then
    e.attr[Name] := NewValue;
end;

procedure TSCBUIElementObject.EnableElement(Enable: Boolean = true);
begin
  if Enable then
    SetElementAttribute(Selector, 'disabled', emptystr) // enabled
  else
    SetElementAttribute(Selector, 'disabled', 'True');
end;

function TSCBUIElementObject.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'engine') = 0 then
    result := Engine
  else if CompareText(propName, 'selector') = 0 then
    result := Selector
  else if CompareText(propName, 'valueastext') = 0 then
    result := ElemValueToText(GetElementValue(Selector))
  else if CompareText(propName, 'value') = 0 then
    result := GetElementValue(Selector)
  else
    result := inherited GetPropValue(propName);
end;

function TSCBUIElementObject.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  result := true;
  if CompareText(propName, 'enabled') = 0 then
    EnableElement(AValue)
  else if CompareText(propName, 'engine') = 0 then
    SetEngine(String(AValue))
  else if CompareText(propName, 'selector') = 0 then
    Selector := String(AValue)
  else if CompareText(propName, 'value') = 0 then
    SetElementValue(Selector, AValue)
  else
    result := inherited SetPropValue(propName, AValue);
end;

constructor TSCBUIElementObject.Create(LuaState: PLua_State;
  AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
end;

destructor TSCBUIElementObject.Destroy;
begin
  inherited Destroy;
end;
// *****************************************************************************

// Engine Object ***************************************************************
function TSCBUIEngineObject.GetUIXTable: string;
var
  e: ISandUIElement;
begin
  result := emptystr;
  e := GetZoneByID(EngineID).root.Select('meta[name=''SandcatUIX'']');
  if e <> nil then
    result := e.attr['content'];
end;

procedure TSCBUIEngineObject.SetEngine(Name: string);
begin
  if name = emptystr then
    exit;
  EngineID := GetZoneID(Name);
  Engine := name;
end;

function TSCBUIEngineObject.GetPropValue(propName: String): Variant;
begin
  if CompareText(propName, 'name') = 0 then
    result := Engine
  else if CompareText(propName, 'uix') = 0 then
    result := GetUIXTable()
  else
    result := inherited GetPropValue(propName);
end;

function TSCBUIEngineObject.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  result := true;
  if CompareText(propName, 'name') = 0 then
    SetEngine(String(AValue))
  else
    result := inherited SetPropValue(propName, AValue);
end;

constructor TSCBUIEngineObject.Create(LuaState: PLua_State;
  AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
end;

destructor TSCBUIEngineObject.Destroy;
begin
  inherited Destroy;
end;
// *****************************************************************************

procedure register_methods_element(L: PLua_State; classTable: integer);
begin
  RegisterMethod(L, 'select', @method_select, classTable);
  // RegisterMethod(L,'getvalue', @method_getval, classTable);
  // RegisterMethod(L,'setvalue', @method_setval, classTable);
  RegisterMethod(L, 'getattrib', @method_getattrib, classTable);
  RegisterMethod(L, 'setattrib', @method_setattrib, classTable);
  RegisterMethod(L, 'getstyle', @method_getstyleattrib, classTable);
  RegisterMethod(L, 'setstyle', @method_setstyleattrib, classTable);
end;

const
  AClassNameElement = 'SandcatUIElement';

function newcallback_element(L: PLua_State; AParent: TLuaObject = nil)
  : TLuaObject;
begin
  result := TSCBUIElementObject.Create(L, AParent);
end;

function Create_Element(L: PLua_State): integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @newcallback_element;
  result := new_LuaObject(L, AClassNameElement, p);
end;

procedure RegisterSCBUIElement_Sandcat(L: PLua_State);
begin
  RegisterTLuaObject(L, AClassNameElement, @Create_Element,
    @register_methods_element);
end;

function lua_zone_addtis(L: PLua_State): integer; cdecl;
var
  o: TSCBUIEngineObject;
begin
  o := TSCBUIEngineObject(LuaToTLuaObject(L, 1));
  uix.AddTIS(lua_tostring(L, 2), o.Engine);
  result := 1;
end;

function lua_zone_addhtml(L: PLua_State): integer; cdecl;
var
  o: TSCBUIEngineObject;
begin
  o := TSCBUIEngineObject(LuaToTLuaObject(L, 1));
  uix.AddHTML(o.Engine, lua_tostring(L, 2), lua_tostring(L, 3));
  result := 1;
end;

function lua_zone_addhtmlfile(L: PLua_State): integer; cdecl;
var
  o: TSCBUIEngineObject;
begin
  o := TSCBUIEngineObject(LuaToTLuaObject(L, 1));
  uix.AddHTMLFile(o.Engine, lua_tostring(L, 2), lua_tostring(L, 3));
  result := 1;
end;

function lua_zone_inserthtml(L: PLua_State): integer; cdecl;
var
  idx: string;
  o: TSCBUIEngineObject;
begin
  o := TSCBUIEngineObject(LuaToTLuaObject(L, 1));
  case lua_type(L, 2) of
    LUA_TNUMBER:
      idx := inttostr(lua_tointeger(L, 2));
    LUA_TSTRING:
      idx := '$("' + lua_tostring(L, 2) + '").index+1';
  end;
  uix.InsertHTML(o.Engine, idx, lua_tostring(L, 3), lua_tostring(L, 4));
  result := 1;
end;

function lua_zone_inserthtmlfile(L: PLua_State): integer; cdecl;
var
  idx: string;
  o: TSCBUIEngineObject;
begin
  o := TSCBUIEngineObject(LuaToTLuaObject(L, 1));
  case lua_type(L, 2) of
    LUA_TNUMBER:
      idx := inttostr(lua_tointeger(L, 2));
    LUA_TSTRING:
      idx := '$("' + lua_tostring(L, 2) + '").index';
  end;
  uix.InsertHTMLFile(o.Engine, idx, lua_tostring(L, 3), lua_tostring(L, 4));
  result := 1;
end;

procedure register_methods(L: PLua_State; classTable: integer);
begin
  RegisterMethod(L, 'addhtml', @lua_zone_addhtml, classTable);
  RegisterMethod(L, 'addhtmlfile', @lua_zone_addhtmlfile, classTable);
  RegisterMethod(L, 'addtiscript', @lua_zone_addtis, classTable);
  RegisterMethod(L, 'inserthtml', @lua_zone_inserthtml, classTable);
  RegisterMethod(L, 'inserthtmlfile', @lua_zone_inserthtmlfile, classTable);
  RegisterMethod(L, 'hide', @method_engine_hide, classTable);
  RegisterMethod(L, 'eval', @method_engine_eval, classTable);
  RegisterMethod(L, 'loadx', @method_engine_load, classTable);
  RegisterMethod(L, 'loadx_std', @method_engine_loadstd, classTable);
  RegisterMethod(L, 'loadx_url', @method_engine_loadurl, classTable);
end;

const
  AClassName = 'SandcatUIZone';

function newcallback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TSCBUIEngineObject.Create(L, AParent);
end;

function Create(L: PLua_State): integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @newcallback;
  result := new_LuaObject(L, AClassName, p);
end;

procedure RegisterSCBUIEngine_Sandcat(L: PLua_State);
begin
  RegisterTLuaObject(L, AClassName, @Create, @register_methods);
end;

end.
