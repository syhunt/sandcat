unit uMisc;

{
  Sandcat - Miscellaneous functions
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Windows, Forms, Messages, SysUtils, Classes, Controls, Graphics,
  ComCtrls, Lua, ExtCtrls, SynEdit, uUIComponents, CatChromium, uTab, uTabMan;

function BeginsWithSpecialParam(param: string): boolean;
function BuildRequestFromJSON(json: string): TCatChromiumRequest;
function BuildCustomTabFromLuaTable(L: PLua_State): TCustomTabSettings;
function BuildRequestFromLuaTable(L: PLua_State): TCatChromiumRequest;
function BuildXHRFromJSON(json: string): TCatChromiumXHR;
function BuildXHRFromLuaTable(L: PLua_State): TCatChromiumXHR;
function CaptureChromeBitmap(tab: TSandcatTab; filename: string = ''): string;
function GetPakResourceAsString(filename: string): string;
function ShortTitle(s: string; maxchars: integer = 30): string;
function SandEnc(s: string): string;
function UseSingleInstance: boolean;
procedure ConfigPanel(p: TPanel; align: TAlign = alNone);
procedure ConfigSynEdit(s: TSandSynEdit);
// procedure SearchSource(s: string);
procedure SendCommandLineParams(DestHandle: integer);
procedure SetUserCSS(s: string);

implementation

uses uMain, CatStrings, CatUI, CatFiles, uConst, uSettings,
  uZones, pngimage, CatZIP, CatLuaUtils, CatCLUtils;

function BeginsWithSpecialParam(param: string): boolean;
begin
  result := false;
  if beginswith(param, cNewWinParam) then
    result := true
  else if beginswith(param, cModeParam) then
    result := true
  else if beginswith(param, cBgTaskPrefix) then
    result := true;
end;

// TODO: rewrite single instance mode handling
function UseSingleInstance: boolean;
begin
  result := false;
  if IsMultipleInstancesAllowed then
    exit;
  if BeginsWithSpecialParam(paramstr(1)) = false then
  begin
    if FindWindow(pchar(cMainClass), nil) <> 0 then
      result := true;
  end;
end;

function BuildCustomTabFromLuaTable(L: PLua_State): TCustomTabSettings;
var
  idx: integer;
var
  def: TCustomTabSettings;
begin
  def := tabmanager.GetTabDefaultSettings;
  idx := lua_gettop(L);
  result.activepage := pLua_GetFieldStr(L, idx, 'activepage', def.activepage);
  result.HTML := pLua_GetFieldStr(L, idx, 'html');
  result.icon := pLua_GetFieldStr(L, idx, 'icon');
  result.LoadNew := pLua_GetFieldBool(L, idx, 'loadnew', def.LoadNew);
  result.ShowNavBar := pLua_GetFieldBool(L, idx, 'shownavbar', def.ShowNavBar);
  result.ShowPageStrip := pLua_GetFieldBool(L, idx, 'showpagestrip',
    def.ShowPageStrip);
  result.Table := pLua_GetFieldStr(L, idx, 'table');
  result.Tag := pLua_GetFieldStr(L, idx, 'tag');
  result.Title := pLua_GetFieldStr(L, idx, 'title');
  result.Toolbar := pLua_GetFieldStr(L, idx, 'toolbar');
end;

function BuildRequestFromLuaTable(L: PLua_State): TCatChromiumRequest;
var
  idx: integer;
begin
  idx := lua_gettop(L);
  result.method := pLua_GetFieldStr(L, idx, REQUESTKEY_METHOD);
  result.url := pLua_GetFieldStr(L, idx, REQUESTKEY_URL);
  result.postdata := pLua_GetFieldStr(L, idx, REQUESTKEY_POSTDATA);
  result.headers := pLua_GetFieldStr(L, idx, REQUESTKEY_HEADERS);
  result.ignorecache := pLua_GetFieldBool(L, idx, REQUESTKEY_IGNORECACHE, true);
  result.usecookies := pLua_GetFieldBool(L, idx, REQUESTKEY_USECOOKIES, true);
  result.usecachedcredentials := pLua_GetFieldBool(L, idx,
    REQUESTKEY_USEAUTH, true);
  result.details := pLua_GetFieldStr(L, idx, REQUESTKEY_DETAILS);
end;

function BuildRequestFromJSON(json: string): TCatChromiumRequest;
var
  j: TSandJSON;
begin
  j := TSandJSON.create;
  j.text := json;
  result.method := j.GetValue(REQUESTKEY_METHOD, 'GET');
  result.url := j.GetValue(REQUESTKEY_URL, emptystr);
  result.postdata := j.GetValue(REQUESTKEY_POSTDATA, emptystr);
  result.headers := j.GetValue(REQUESTKEY_HEADERS, emptystr);
  result.ignorecache := j.GetValue(REQUESTKEY_IGNORECACHE, true);
  result.usecookies := j.GetValue(REQUESTKEY_USECOOKIES, true);
  result.usecachedcredentials := j.GetValue(REQUESTKEY_USEAUTH, true);
  result.details := j.GetValue(REQUESTKEY_DETAILS, emptystr);
  j.Free;
end;

function BuildXHRFromJSON(json: string): TCatChromiumXHR;
var
  j: TSandJSON;
begin
  j := TSandJSON.create(json);
  result.details := j.GetValue(REQUESTKEY_DETAILS, emptystr);
  result.tab := j.GetValue(REQUESTKEY_TAB, emptystr);
  result.filters := j.GetValue(REQUESTKEY_FILTER, emptystr);
  result.username := j.GetValue(REQUESTKEY_USERNAME, emptystr);
  result.password := j.GetValue(REQUESTKEY_PASSWORD, emptystr);
  result.headers := j.GetValue(REQUESTKEY_HEADERS, emptystr);
  result.callback := j.GetValue(REQUESTKEY_CALLBACK, emptystr);
  result.method := j.GetValue(REQUESTKEY_METHOD, 'GET');
  result.url := j.GetValue(REQUESTKEY_URL, emptystr);
  result.postdata := j.GetValue(REQUESTKEY_POSTDATA, emptystr);
  j.Free;
end;

function BuildXHRFromLuaTable(L: PLua_State): TCatChromiumXHR;
var
  idx: integer;
begin
  idx := lua_gettop(L);
  result.details := pLua_GetFieldStr(L, idx, REQUESTKEY_DETAILS);
  result.tab := pLua_GetFieldStr(L, idx, REQUESTKEY_TAB);
  result.filters := pLua_GetFieldStr(L, idx, REQUESTKEY_FILTER);
  result.username := pLua_GetFieldStr(L, idx, REQUESTKEY_USERNAME);
  result.password := pLua_GetFieldStr(L, idx, REQUESTKEY_PASSWORD);
  result.headers := pLua_GetFieldStr(L, idx, REQUESTKEY_HEADERS);
  result.callback := pLua_GetFieldStr(L, idx, REQUESTKEY_CALLBACK);
  result.method := pLua_GetFieldStr(L, idx, REQUESTKEY_METHOD, 'GET');
  result.url := pLua_GetFieldStr(L, idx, REQUESTKEY_URL);
  result.postdata := pLua_GetFieldStr(L, idx, REQUESTKEY_POSTDATA);
end;

procedure SetUserCSS(s: string);
begin
  UserScript.CSS_UserStyleSheet := 'data:text/css;charset=utf-8;base64,' +
    Base64EnCode(s);
end;

function CaptureChromeBitmap(tab: TSandcatTab; filename: string = ''): string;
var
  Bitmap: TBitmap;
  FromLeft, FromTop: integer;
  dir: string;
  PNG: TPNGObject;
begin
  debug('sbt.capture');
  dir := GetSandcatDir(SCDIR_TEMP) + 'Capture\';
  forcedir(dir);
  if filename = emptystr then
    filename := dir + inttostr(tab.handle) + '.png';
  application.ProcessMessages;
  PNG := TPNGObject.create;
  Bitmap := TBitmap.create;
  try
    Bitmap.Width := tab.Chrome.crm.Width;
    Bitmap.Height := tab.Chrome.crm.Height;
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height),
      tab.BrowserPanel.Canvas, Rect(tab.Chrome.crm.Left, tab.Chrome.crm.Top,
      tab.Chrome.crm.Left + tab.Chrome.crm.Width - 1,
      tab.Chrome.crm.Top + tab.Chrome.crm.Height - 1));
    PNG.Assign(Bitmap); // Convert data into png
    PNG.SaveToFile(filename);
  finally
    Bitmap.Free;
    PNG.Free;
  end;
  result := filename;
end;

procedure ConfigPanel(p: TPanel; align: TAlign = alNone);
begin
  p.ParentBackground := true;
  p.ParentBackground := false; // VCL bug workaround
  p.BevelInner := bvNone;
  p.BevelOuter := bvNone;
  p.Caption := emptystr;
  p.Color := clWindow;
  p.Visible := true;
  if align <> alNone then
    p.align := align;
end;

procedure ConfigSynEdit(s: TSandSynEdit);
begin
  s.Gutter.ShowLineNumbers := true;
  s.Gutter.Font.Color := $00808080;
  s.Gutter.Font.Size := 8;
  s.Gutter.Color := $00F0F0F0;
  s.Gutter.BorderColor := $00EEEEEE;
  s.Gutter.UseFontStyle := true;
  s.options := s.options - [eoShowScrollHint];
  // Makes the horizontal bar behave normally
  s.options := s.options - [eoScrollPastEOL];
  s.RightEdge := 0;
  s.ReadOnly := true;
end;

function SandEnc(s: string): string;
begin
  result := '""';
  if s <> emptystr then
    result := 'Sandcat.Base64Decode("' + Base64EnCode(s) + '")';
end;

procedure SendCommandLineParams(DestHandle: integer);
var
  pData: PCopyDataStruct;
  s: string;
begin
  s := CatCLUtils.GetCmdLine;
  pData := nil;
  try
    New(pData);
    pData^.dwData := SCBF_CMDLINEPARAMS;
    pData^.cbData := Length(s) + 1;
    pData^.lpData := PAnsiChar(AnsiString(s));
    SendMessage(DestHandle, WM_COPYDATA, application.handle, integer(pData));
  finally
    Dispose(pData);
  end;
end;

{ procedure SearchSource(s: string);
  begin
  extensions.LuaWrap.Value['Search_Temp'] := s;
  extensions.RunLuaCmd('SearchSource:search(Search_Temp)', 'Syhunt.scx',
  'Scripts/SearchSource.lua');
  end; }

function GetPakResourceAsString(filename: string): string;
begin
  result := GetTextFileFromZIP(pluginsdir + cResourcesPak, filename);
end;

// Shortens a title string (if s exceeds maxchars adds dots)
function ShortTitle(s: string; maxchars: integer = 30): string;
const
  cDots = '...';
begin
  result := s;
  if Length(s) > maxchars then
  begin
    result := copy(s, 1, maxchars);
    if endswith(result, cDots) = false then
      result := result + cDots;
  end;
end;

// ------------------------------------------------------------------------//
end.
