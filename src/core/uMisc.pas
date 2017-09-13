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
function CaptureChromeBitmap(tab: TSandcatTab; filename: string = ''): string;
function GetPakResourceAsString(filename: string): string;
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
    Bitmap.Width := tab.browser.c.Width;
    Bitmap.Height := tab.browser.c.Height;
    Bitmap.PixelFormat := pf24bit;
    Bitmap.Canvas.CopyRect(Rect(0, 0, Bitmap.Width, Bitmap.Height),
      tab.BrowserPanel.Canvas, Rect(tab.browser.c.Left, tab.browser.c.Top,
      tab.browser.c.Left + tab.browser.c.Width - 1,
      tab.browser.c.Top + tab.browser.c.Height - 1));
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
  extensions.RunLuaCmd('SearchSource:search(Search_Temp)', 'PenTools.scx',
  'Scripts/SearchSource.lua');
  end; }

function GetPakResourceAsString(filename: string): string;
begin
  result := GetTextFileFromZIP(pluginsdir + cResourcesPak, filename);
end;

// ------------------------------------------------------------------------//
end.
