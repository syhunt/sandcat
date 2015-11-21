unit LAPI_App;

{
  Sandcat App Lua Library
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

{$I Catarinka.inc}

uses Lua, Classes, Windows, Messages, SysUtils, Forms, Dialogs, TypInfo,
  FileCtrl;

function lua_getappinfo(L: plua_State): Integer; cdecl;
function lua_setappinfo(L: plua_State): Integer; cdecl;
function app_askyesorno(L: plua_State): Integer; cdecl;
function app_editlist(L: plua_State): Integer; cdecl;
function app_gettitle(L: plua_State): Integer; cdecl;
function app_processmessages(L: plua_State): Integer; cdecl;
function app_settitle(L: plua_State): Integer; cdecl;
function app_showalert(L: plua_State): Integer; cdecl;
function app_showcustomdialog(L: plua_State): Integer; cdecl;
function app_showinputdialog(L: plua_State): Integer; cdecl;
function app_showmessage(L: plua_State): Integer; cdecl;
function app_showmessage_classic(L: plua_State): Integer; cdecl;
function app_showopendialog(L: plua_State): Integer; cdecl;
function app_showopendirdialog(L: plua_State): Integer; cdecl;
function app_showsavedialog(L: plua_State): Integer; cdecl;
function app_showhtmlmessage(L: plua_State): Integer; cdecl;
function app_seticonfromres(L: plua_State): Integer; cdecl;

implementation

uses uMain, uConst, uSettings, uExtensions, CatStrings, CatListEditor, pLua,
  uZones, LAPI, CatFiles;

type
  TAppInfoType = (info_abouturl, info_cachedir, info_ceflibrary, info_configdir,
    info_commands, info_downloads, info_exefilename, info_extensions,
    info_errorlog, info_fullname, info_handle, info_iconfilename, info_initmode,
    info_libraries, info_name, info_options, info_previewdir, info_proxy,
    info_tasks, info_useragent, info_version, info_tempscript);

function lua_setappinfo(L: plua_State): Integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 2);
  case TAppInfoType(GetEnumValue(TypeInfo(TAppInfoType),
    'info_' + lowercase(s))) of
    info_abouturl:
      vAppURL := lua_tostring(L, 3);
    info_fullname:
      vAppNameShort := lua_tostring(L, 3);
    info_name:
      begin
        vAppNameShortest := lua_tostring(L, 3);
        vAppNameShortNoCompany := vAppNameShortest;
        if tabmanager.activetab <> nil then
          tabmanager.SetAppTitle(tabmanager.activetab.Title);
      end;
    info_exefilename:
      vExeFileName := lua_tostring(L, 3);
  end;
  result := 1;
end;

function lua_getappinfo(L: plua_State): Integer; cdecl;
var
  s: string;
  function gettasklist: string;
  var
    sl: TStringList;
  begin
    sl := TStringList.Create;
    tasks.gettasklist(sl);
    result := sl.Text;
    sl.free;
  end;
  function getdownloadlist: string;
  var
    sl: TStringList;
  begin
    sl := TStringList.Create;
    tasks.getdownloadlist(sl);
    result := sl.Text;
    sl.free;
  end;

begin
  result := 1;
  s := lua_tostring(L, 2);
  case TAppInfoType(GetEnumValue(TypeInfo(TAppInfoType),
    'info_' + lowercase(s))) of
    info_abouturl:
      lua_pushstring(L, vAppURL);
    info_cachedir:
      lua_pushstring(L, GetSandcatDir(SCDIR_CACHE));
    info_ceflibrary:
      lua_pushstring(L, {$IFDEF USEWACEF}'wacef'{$ELSE}'dcef'{$ENDIF});
    info_commands:
      lua_pushstring(L, extensions.GetCommandList);
    info_configdir:
      lua_pushstring(L, GetSandcatDir(SCDIR_CONFIG));
    info_downloads:
      lua_pushstring(L, getdownloadlist);
    info_exefilename:
      lua_pushstring(L, vExeFileName);
    info_extensions:
      lua_pushstring(L, extensions.GetList);
    info_errorlog:
      lua_pushstring(L, extensions.ErrorList.Text);
    info_fullname:
      lua_pushstring(L, vAppNameShort);
    info_handle:
      lua_pushinteger(L, SandBrowser.Handle);
    info_iconfilename:
      lua_pushstring(L, vExeFileName);
    info_initmode:
      lua_pushstring(L, extensions.CurrentInitMode);
    info_libraries:
      lua_pushstring(L, extensions.LibraryList);
    info_name:
      lua_pushstring(L, vAppNameShortest);
    info_options:
      lua_pushstring(L, settings.preferences.CIDList);
    info_previewdir:
      lua_pushstring(L, GetSandcatDir(SCDIR_PREVIEW));
    info_proxy:
      lua_pushstring(L, settings.preferences.GetValue(SCO_PROXY_SERVER,
        emptystr));
    info_tasks:
      lua_pushstring(L, gettasklist);
    info_useragent:
      lua_pushstring(L, settings.preferences.GetValue(SCO_USERAGENT, emptystr));
    info_version:
      lua_pushstring(L, GetFileVersion(vExeFileName));
    info_tempscript:
      lua_pushstring(L, extensions.TempScript);
  else
    result := 0;
  end;

end;

function app_editlist(L: plua_State): Integer; cdecl;
var
  s, prevlist, exampletext, wintitle, caption: string;
begin
  prevlist := lua_tostring(L, 1);
  caption := lua_tostring(L, 2);
  exampletext := lua_tostring(L, 3);
  wintitle := lua_tostring(L, 4);
  if wintitle = emptystr then
    wintitle := 'Edit List';
  if ShowEditListDialog(prevlist, exampletext, wintitle, caption, s) then
    lua_pushstring(L, s)
  else
    lua_pushstring(L, prevlist);
  result := 1;
end;

function app_seticonfromres(L: plua_State): Integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 1);
  Application.Icon.Handle := LoadIcon(HInstance, pWideChar(s));
  SandBrowser.Icon := Application.Icon;
  result := 1;
end;

function app_showinputdialog(L: plua_State): Integer; cdecl;
var
  s, caption: string;
begin
  caption := lua_tostring(L, 3);
  if caption = emptystr then
    caption := vAppNameShort;
  s := inputbox(caption, lua_tostring(L, 1), lua_tostring(L, 2));
  lua_pushstring(L, s);
  result := 1;
end;

function app_gettitle(L: plua_State): Integer; cdecl;
begin
  lua_pushstring(L, Application.Title);
  result := 1;
end;

function app_settitle(L: plua_State): Integer; cdecl;
begin
  Application.Title := lua_tostring(L, 1);
  result := 1;
end;

function app_showmessage(L: plua_State): Integer; cdecl;
begin
  sanddlg.ShowMessage(lua_tostring(L, 1));
  result := 1;
end;

function app_showmessage_classic(L: plua_State): Integer; cdecl;
begin
  Dialogs.ShowMessage(lua_tostring(L, 1));
  result := 1;
end;

function app_showalert(L: plua_State): Integer; cdecl;
begin
  sanddlg.ShowAlert(lua_tostring(L, 1), true);
  result := 1;
end;

function app_showhtmlmessage(L: plua_State): Integer; cdecl;
begin
  sanddlg.ShowHTMLMessage(lua_tostring(L, 1));
  result := 1;
end;

function app_processmessages(L: plua_State): Integer; cdecl;
begin
  Application.processmessages;
  result := 1;
end;

function app_askyesorno(L: plua_State): Integer; cdecl;
var
  button: Integer;
  msg, caption: string;
begin
  msg := lua_tostring(L, 1);
  caption := lua_tostring(L, 2);
  if caption = emptystr then
    caption := vAppNameShort;
  button := Application.MessageBox(pWideChar(msg), pWideChar(caption),
    mb_YesNo + mb_DefButton1);
  if button = IDYes then
    lua_pushboolean(L, true);
  if button = IDNo then
    lua_pushboolean(L, false);
  result := 1;
end;

function app_showopendirdialog(L: plua_State): Integer; cdecl;
var
  dir, caption: string;
begin
  caption := lua_tostring(L, 1);
  if caption = emptystr then
    caption := '';
  if SelectDirectory(caption, dir, dir) = true then
    lua_pushstring(L, dir)
  else
    lua_pushstring(L, emptystr);
  result := 1;
end;

function app_showopendialog(L: plua_State): Integer; cdecl;
var
  sd: topendialog;
  f: string;
begin
  f := lua_tostring(L, 3);
  f := replacestr(f, '\\', '\');
  sd := topendialog.Create(SandBrowser);
  sd.InitialDir := emptystr;
  sd.DefaultExt := lua_tostring(L, 2); // eg 'cfg'
  sd.FileName := f;
  sd.Filter := lua_tostring(L, 1);
  // eg filter  format: 'Sandcat Configuration File (*.xcfg)|*.xcfg'
  if sd.execute then
    lua_pushstring(L, sd.FileName)
  else
    lua_pushstring(L, emptystr);
  sd.free;
  result := 1;
end;

function app_showsavedialog(L: plua_State): Integer; cdecl;
var
  sd: tsavedialog;
  f: string;
begin
  f := lua_tostring(L, 3);
  f := replacestr(f, '\\', '\');
  sd := tsavedialog.Create(SandBrowser);
  sd.InitialDir := emptystr;
  sd.DefaultExt := lua_tostring(L, 2); // eg 'cfg'
  sd.FileName := f;
  sd.Options := sd.Options + [ofoverwriteprompt];
  // showmessage(lua_tostring(L,1));
  sd.Filter := lua_tostring(L, 1);
  // eg filter  format: 'Sandcat Configuration File (*.xcfg)|*.xcfg'
  if sd.execute then
    lua_pushstring(L, sd.FileName)
  else
    lua_pushstring(L, emptystr);
  sd.free;
  result := 1;
end;

function app_showcustomdialog(L: plua_State): Integer; cdecl;
begin
  sanddlg.ShowCustomDialog(lua_tostring(L, 1), lua_tostring(L, 2));
  result := 1;
end;

end.
