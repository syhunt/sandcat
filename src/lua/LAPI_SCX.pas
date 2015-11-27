unit LAPI_SCX;
{
  Sandcat Extension LUA Object
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Classes, Forms, SysUtils, TypInfo, Lua, LuaObject;

type
  TSCXObject = class(TLuaObject)
  private
    FPackFilename: string;
    FFilename: string;
    procedure SetFilename(s: string);
  public
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
    // properties
    property Filename: string read FFilename write SetFilename;
    property PakFilename: string read FPackFilename;
  end;

procedure RegisterSCX(L: PLua_State);

implementation

uses
  uMain, CatStrings, CatFiles, CatZIP, pLua, uSettings, uConst;

type
  TProps = (prop_filename);

procedure TSCXObject.SetFilename(s: string);
var
  pak: string;
begin
  FFilename := s;
  pak := GetSandcatDir(SCDIR_PLUGINS) + s;
  if fileexists(pak) then
    FPackFilename := pak;
end;

function TSCXObject.GetPropValue(propName: String): Variant;
begin
  case TProps(GetEnumValue(TypeInfo(TProps), 'prop_' + lowercase(propName))) of
    prop_filename:
      result := Filename;
  else
    result := inherited GetPropValue(propName);
  end;
end;

function TSCXObject.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  result := true;
  case TProps(GetEnumValue(TypeInfo(TProps), 'prop_' + lowercase(propName))) of
    prop_filename:
      Filename := String(AValue);
  else
    result := inherited SetPropValue(propName, AValue);
  end;
end;

function method_dofile(L: PLua_State): integer; cdecl;
var
  scx: TSCXObject;
  s: string;
begin
  scx := TSCXObject(LuaToTLuaObject(L, 1));
  s := emptystr;
  if scx.PakFilename <> emptystr then
  begin
    s := GetTextFileFromZIP(scx.PakFilename, lua_tostring(L, 2));
    debug('doing file:' + lua_tostring(L, 2) + ' (from: ' +
      scx.PakFilename + ')');
    Extensions.RunLua(s);
  end;
  result := 1;
end;

function method_require(L: PLua_State): integer; cdecl;
var
  scx: TSCXObject;
  module, outfilename, pakshortname, outdir: string;
begin
  scx := TSCXObject(LuaToTLuaObject(L, 1));
  if scx.PakFilename <> emptystr then
  begin
    pakshortname := extractfilename(scx.Filename);
    pakshortname := changefileext(pakshortname, '');
    module := replacestr(lua_tostring(L, 2), '.', '\');
    module := module + '.lua';
    outfilename := GetAppDataDir + 'Lib\lua\' + pakshortname + '\' + module;
    outdir := extractfilepath(outfilename);
    if ZIPFileExists(scx.PakFilename, module) then
    begin
      ForceDir(outdir);
      ExtractZIPFile(scx.PakFilename, module, outfilename);
      debug('requiring file:' + module + ' (from: ' + scx.PakFilename + ')');
      lua_getglobal(L, 'require');
      lua_pushstring(L, pakshortname + '.' + lua_tostring(L, 2));
      lua_call(L, 1, 1);
    end
    else
    begin
      debug('require ' + lua_tostring(L, 2) + ' not found in ZIP.');
    end;
  end;
  result := 1;
end;

function method_readfile(L: PLua_State): integer; cdecl;
var
  scx: TSCXObject;
  s: string;
begin
  scx := TSCXObject(LuaToTLuaObject(L, 1));
  s := emptystr;
  if scx.PakFilename <> emptystr then
    s := GetTextFileFromZIP(scx.PakFilename, lua_tostring(L, 2));
  lua_pushstring(L, s);
  result := 1;
end;

function method_fileexists(L: PLua_State): integer; cdecl;
var
  scx: TSCXObject;
  b: Boolean;
begin
  scx := TSCXObject(LuaToTLuaObject(L, 1));
  b := false;
  if scx.PakFilename <> emptystr then
    b := ZIPFileExists(scx.PakFilename, lua_tostring(L, 2));
  lua_pushboolean(L, b);
  result := 1;
end;

function method_addtoimagelist(L: PLua_State): integer; cdecl;
var
  scx: TSCXObject;
  i: integer;
begin
  scx := TSCXObject(LuaToTLuaObject(L, 1));
  i := uix.imagelistadd(scx.PakFilename, lua_tostring(L, 2));
  lua_pushinteger(L, i);
  result := 1;
end;

procedure registermethods(L: PLua_State; classTable: integer);
begin
  RegisterMethod(L, 'dofile', @method_dofile, classTable);
  RegisterMethod(L, 'fileexists', @method_fileexists, classTable);
  RegisterMethod(L, 'getfile', @method_readfile, classTable);
  RegisterMethod(L, 'imagelist_add', @method_addtoimagelist, classTable);
  RegisterMethod(L, 'require', @method_require, classTable);
end;

const
  cObj = 'extensionpack';

function newcallback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TSCXObject.Create(L, AParent);
end;

function Create(L: PLua_State): integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @newcallback;
  result := new_LuaObject(L, cObj, p);
end;

procedure RegisterSCX(L: PLua_State);
begin
  RegisterTLuaObject(L, cObj, @Create, @registermethods);
end;

constructor TSCXObject.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
end;

destructor TSCXObject.Destroy;
begin
  inherited Destroy;
end;

end.
