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
  ComCtrls, Lua, ExtCtrls, SynEdit, uUIComponents, CatChromium, CatChromiumLib,
  uTab, uTabMan;

function BeginsWithSpecialParam(param: string): boolean;
function BuildRequestFromJSON(json: string): TCatChromiumRequest;
function BuildCustomTabFromLuaTable(L: PLua_State): TCustomTabSettings;
function BuildRequestFromLuaTable(L: PLua_State): TCatChromiumRequest;
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

uses uMain, pLua, pLuaTable, CatStrings, CatUI, CatFiles, uConst, uSettings,
  uZones, pngimage, CatZIP, CatCLUtils;

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
  def: TCustomTabSettings;
  t: TLuaTable;
begin
  def := tabmanager.GetTabDefaultSettings;
  t := TLuaTable.Create(L, true);
  result.activepage := t.readstring('activepage', def.activepage);
  result.HTML := t.readstring('html');
  result.icon := t.readstring('icon');
  result.LoadNew := t.readbool('loadnew', def.LoadNew);
  result.ShowNavBar := t.readbool('shownavbar', def.ShowNavBar);
  result.ShowPageStrip := t.readbool('showpagestrip', def.ShowPageStrip);
  result.Table := t.readstring('table');
  result.Tag := t.readstring('tag');
  result.Title := t.readstring('title');
  result.Toolbar := t.readstring('toolbar');
  t.Free;
end;

function BuildRequestFromLuaTable(L: PLua_State): TCatChromiumRequest;
var
  t: TLuaTable;
begin
  t := TLuaTable.Create(L, true);
  result.method := t.readstring(REQUESTKEY_METHOD);
  result.url := t.readstring(REQUESTKEY_URL);
  result.postdata := t.readstring(REQUESTKEY_POSTDATA);
  result.headers := t.readstring(REQUESTKEY_HEADERS);
  result.ignorecache := t.readbool(REQUESTKEY_IGNORECACHE, true);
  result.usecookies := t.readbool(REQUESTKEY_USECOOKIES, true);
  result.usecachedcredentials := t.readbool(REQUESTKEY_USEAUTH, true);
  result.details := t.readstring(REQUESTKEY_DETAILS);
  t.Free;
end;

function BuildRequestFromJSON(json: string): TCatChromiumRequest;
var
  j: TSandJSON;
begin
  j := TSandJSON.Create;
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

procedure SetUserCSS(s: string);
begin
  UserScript.CSS_UserStyleSheet := 'data:text/css;charset=utf-8;base64,' +
    Base64EnCode(s);
end;

function CaptureChromeBitmap(tab: TSandcatTab; filename: string = ''): string;
var
  Bitmap: TBitmap;
  dir: string;
  PNG: TPNGImage;
begin
  debug('sbt.capture');
  dir := GetSandcatDir(SCDIR_TEMP) + 'Capture\';
  forcedir(dir);
  if filename = emptystr then
    filename := dir + inttostr(tab.handle) + '.png';
  application.ProcessMessages;
  PNG := TPNGImage.Create;
  Bitmap := TBitmap.Create;
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
