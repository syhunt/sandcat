unit LAPI_Browser;

{
  Sandcat Browser Lua functions
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses Windows, Messages, Classes, Forms, SysUtils, Dialogs, Controls,
  StdCtrls, Lua, uTabMan;

function lua_addlibraryinfo(L: plua_State): integer; cdecl;
function lua_scriptlogerror(L: plua_State): integer; cdecl;
function lua_closetab(L: plua_State): integer; cdecl;
function lua_closetabs(L: plua_State): integer; cdecl;
function lua_readscxfile(L: plua_State): integer; cdecl;
function lua_addjavascript(L: plua_State): integer; cdecl;
function lua_addluascript(L: plua_State): integer; cdecl;
function lua_addtis(L: plua_State): integer; cdecl;
function lua_inserthtml(L: plua_State): integer; cdecl;
function lua_inserthtmlfile(L: plua_State): integer; cdecl;
function lua_newtab(L: plua_State): integer; cdecl;
function lua_tabs_addcustom(L: plua_State): integer; cdecl;
function lua_gototab(L: plua_State): integer; cdecl;
function lua_setsearchengine(L: plua_State): integer; cdecl;

function lua_addtodebuglog(L: plua_State): integer; cdecl;
function lua_enabledebugmode(L: plua_State): integer; cdecl;
function lua_newwindow(L: plua_State): integer; cdecl;
function lua_browserexit(L: plua_State): integer; cdecl;
function lua_highlightsource(L: plua_State): integer; cdecl;
function lua_var_replace(L: plua_State): integer; cdecl;
function lua_method_getjsvalue(L: plua_State): integer; cdecl;
function lua_method_runluascript(L: plua_State): integer; cdecl;
function lua_method_bookmark(L: plua_State): integer; cdecl;
function lua_clearprivatedata(L: plua_State): integer; cdecl;
function lua_setactivepage(L: plua_State): integer; cdecl;
function lua_loadpagex(L: plua_State): integer; cdecl;
function lua_closepage(L: plua_State): integer; cdecl;
function lua_getbrowseroption(L: plua_State): integer; cdecl;
function lua_setbrowseroption(L: plua_State): integer; cdecl;
function lua_saveresource(L: plua_State): integer; cdecl;
function lua_showbottombar(L: plua_State): integer; cdecl;
function lua_showurl(L: plua_State): integer; cdecl;
function lua_showreqbuilderbar(L: plua_State): integer; cdecl;
function lua_builder_getrequestoption(L: plua_State): integer; cdecl;
function lua_builder_setrequestoption(L: plua_State): integer; cdecl;
function lua_sidebar_loaddir(L: plua_State): integer; cdecl;
function lua_sidebar_clear(L: plua_State): integer; cdecl;
function lua_setinitmode(L: plua_State): integer; cdecl;

function lua_sandcatsettings_get(L: plua_State): integer; cdecl;
function lua_sandcatsettings_getdefault(L: plua_State): integer; cdecl;
function lua_sandcatsettings_set(L: plua_State): integer; cdecl;
function lua_sandcatsettings_getalljson(L: plua_State): integer; cdecl;
function lua_sandcatsettings_getalldefaultjson(L: plua_State): integer; cdecl;
function lua_sandcatsettings_save(L: plua_State): integer; cdecl;
function lua_sandcatsettings_settext(L: plua_State): integer; cdecl;
function lua_sandcatsettings_getfilename(L: plua_State): integer; cdecl;
function lua_sandcatsettings_getsiteprefsfilename(L: plua_State): integer; cdecl;
function lua_sandcatsettings_registerdefault(L: plua_State): integer; cdecl;
function lua_sandcatsettings_update(L: plua_State): integer; cdecl;
function lua_sandcatsettings_savetofile(L: plua_State): integer; cdecl;
function lua_sandcatsettings_loadfromfile(L: plua_State): integer; cdecl;

implementation

uses uMain, pLua, CatStrings, CatFiles, CatTime, uUIComponents, uConst,
  CatTasks, CatZIP, CatHTTP, CatChromium, CatChromiumLib, uSettings, TypInfo,
  uZones, uTab, uMisc, LAPI_Task, LAPI_Tab, CatConsole, CatPrefs, uTaskMan;

type
  TReqOptionType = (ropt_headers, ropt_postdata, ropt_showheaders,
    ropt_showpostdata, ropt_showprefs, ropt_url, ropt_referer, ropt_agent,
    ropt_usecookies, ropt_usecredentials, ropt_ignorecache);

function lua_builder_getrequestoption(L: plua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 2);
  case TReqOptionType(GetEnumValue(TypeInfo(TReqOptionType),
    'ropt_' + lowercase(s))) of
    ropt_url:
      lua_pushstring(L, BottomBar.ReqBuilder.urledit.text);
    ropt_headers:
      lua_pushstring(L, BottomBar.ReqBuilder.getheaders);
    ropt_postdata:
      lua_pushstring(L, BottomBar.ReqBuilder.getpostdata);
    ropt_referer:
      lua_pushstring(L, BottomBar.ReqBuilder.RefererEdit.text);
    ropt_agent:
      lua_pushstring(L, BottomBar.ReqBuilder.AgentEdit.text);
    ropt_usecookies:
      lua_pushboolean(L, BottomBar.ReqBuilder.Prefs['usecookies']);
    ropt_usecredentials:
      lua_pushboolean(L, BottomBar.ReqBuilder.Prefs['usecredentials']);
    ropt_ignorecache:
      lua_pushboolean(L, BottomBar.ReqBuilder.Prefs['ignorecache']);
  end;
  result := 1;
end;

function lua_builder_setrequestoption(L: plua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 2);
  case TReqOptionType(GetEnumValue(TypeInfo(TReqOptionType),
    'ropt_' + lowercase(s))) of
    ropt_url:
      BottomBar.ReqBuilder.urledit.text := lua_tostring(L, 3);
    ropt_postdata:
      BottomBar.ReqBuilder.setpostdata(lua_tostring(L, 3));
    ropt_headers:
      BottomBar.ReqBuilder.headersedit.text := lua_tostring(L, 3);
    ropt_showheaders:
      BottomBar.ReqBuilder.headersedit.Visible := lua_toboolean(L, 3);
    ropt_showpostdata:
      BottomBar.ReqBuilder.POSTDataEdit.Visible := lua_toboolean(L, 3);
    ropt_showprefs:
      BottomBar.ReqBuilder.QuickPrefsPanel.Visible := lua_toboolean(L, 3);
    ropt_referer:
      BottomBar.ReqBuilder.RefererEdit.text := lua_tostring(L, 3);
    ropt_agent:
      BottomBar.ReqBuilder.setuseragent(lua_tostring(L, 3));
    ropt_usecookies:
      BottomBar.ReqBuilder.Prefs['usecookies'] := lua_toboolean(L, 3);
    ropt_usecredentials:
      BottomBar.ReqBuilder.Prefs['usecredentials'] := lua_toboolean(L, 3);
    ropt_ignorecache:
      BottomBar.ReqBuilder.Prefs['ignorecache'] := lua_toboolean(L, 3);
  end;
  result := 1;
end;

type
  TOptionType = (opt_showbottombar, opt_showheaders, opt_showconsole,
    opt_showpagestrip, opt_showsidebar);

function lua_getbrowseroption(L: plua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 2);
  { case TOptionType(GetEnumValue(TypeInfo(TOptionType), 'opt_'+lowercase(s))) of
    opt_showheaders: lua_pushstring(L,rudLibName);
    end; }
  result := 1;
end;

function lua_setbrowseroption(L: plua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 2);
  case TOptionType(GetEnumValue(TypeInfo(TOptionType),
    'opt_' + lowercase(s))) of
    opt_showbottombar:
      BottomBar.ViewBottomBar(lua_toboolean(L, 3));
    opt_showconsole:
      contentarea.ViewConsole(lua_toboolean(L, 3));
    opt_showheaders:
      tabmanager.ViewHeaders(lua_toboolean(L, 3));
    opt_showpagestrip:
      PageBar.StripVisible:=lua_toboolean(L, 3);
    opt_showsidebar:
      sidebar.visible:=lua_toboolean(L, 3);
  end;
  result := 1;
end;

function lua_sidebar_clear(L: plua_State): integer; cdecl;
begin
  sidebar.Clear;
  result := 1;
end;

function lua_sidebar_loaddir(L: plua_State): integer; cdecl;
begin
  if lua_tostring(L, 1) <> emptystr then
    SideBar.LoadDir(lua_tostring(L, 1));
  result:= 1;
end;

function lua_setinitmode(L: plua_State): integer; cdecl;
begin
  extensions.InitModes.Values[lua_tostring(L, 1)] := lua_tostring(L, 2);
  result:= 1;
end;

function lua_showurl(L: plua_State): integer; cdecl;
begin
  BottomBar.showurl(lua_tostring(L, 1), lua_tostring(L, 2));
  result := 1;
end;

function lua_showreqbuilderbar(L: plua_State): integer; cdecl;
begin
  BottomBar.ShowRequestBuilderBar;
  result := 1;
end;

function lua_showbottombar(L: plua_State): integer; cdecl;
begin
  BottomBar.LoadBottomBarRight(lua_tostring(L, 1));
  result := 1;
end;

function lua_closepage(L: plua_State): integer; cdecl;
begin
  contentarea.toolsbar.closepage(lua_tostring(L, 1));
  result := 1;
end;

function lua_loadpagex(L: plua_State): integer; cdecl;
var
  html, subtabname, tablename: string;
begin
  subtabname := lua_tostring(L, 1);
  html := lua_tostring(L, 2);
  if lua_isnone(L, 3) then
    tablename := emptystr
  else
    tablename := lua_tostring(L, 3);
  if tablename <> emptystr then
    html := '<meta name="SandcatUIX" content="' + tablename + '">' +
      crlf + html;
  if subtabname <> emptystr then
    contentarea.toolsbar.LoadPage(html, subtabname);
  result := 1;
end;

function lua_tabs_addcustom(L: plua_State): integer; cdecl;
var
  tab: TSandcatTab;
begin
  tab := tabmanager.NewTab_Custom(BuildCustomTabFromLuaTable(L));
  if tab.UID <> emptystr then
    lua_pushinteger(L, tab.number)
  else
    lua_pushinteger(L, 0);
  result := 1;
end;

function lua_method_runluascript(L: plua_State): integer; cdecl;
begin
  if lua_isnone(L, 2) then
    extensions.RunLua(lua_tostring(L, 1))
  else
  begin
    if lua_toboolean(L, 2) = true then
    begin
      extensions.QueueLuaCmd(lua_tostring(L, 1))
    end
    else
      extensions.RunLua(lua_tostring(L, 1));
  end;
  result := 1;
end;

function lua_saveresource(L: plua_State): integer; cdecl;
begin
  sanddlg.SaveResource(lua_tostring(L, 1), lua_toboolean(L, 2));
  result := 1;
end;

function lua_method_bookmark(L: plua_State): integer; cdecl;
begin
  if lua_isnone(L, 1) then
  begin
    if tabmanager.ActiveTab <> nil then
    begin
      tabmanager.ActiveTab.State.IsBookmarked := true;
      settings.AddToBookmarks(tabmanager.ActiveTab.Title,
        tabmanager.ActiveTab.GetURL);
    end;
  end
  else
    settings.AddToBookmarks(lua_tostring(L, 1), lua_tostring(L, 2));
  result := 1;
end;

function lua_method_getjsvalue(L: plua_State): integer; cdecl;
var
  v: variant;
  key: string;
begin
  key := lua_tostring(L, 2);
  v := settings.readjsvalue(key);
  plua_pushvariant(L, v);
  result := 1;
end;

function lua_browserexit(L: plua_State): integer; cdecl;
begin
  SandBrowser.close;
  result := 1;
end;

function lua_highlightsource(L: plua_State): integer; cdecl;
var s:string;
begin
  s:=Highlighters.HighlightSourceByFileExt(lua_tostring(L, 1),lua_tostring(L, 2));
  lua_pushstring(L,s);
  result := 1;
end;

function lua_newwindow(L: plua_State): integer; cdecl;
begin
  tabmanager.newwindow(lua_tostring(L, 1));
  result := 1;
end;

function lua_closetab(L: plua_State): integer; cdecl;
begin
  tabmanager.closetab(lua_tostring(L, 1));
  result := 1;
end;

function lua_closetabs(L: plua_State): integer; cdecl;
var
  tab: TSandcatTab;
begin
  if lua_isnone(L, 1) = true then
  begin
    tab := tabmanager.NewTab(cURL_HOME);
    tabmanager.CloseAllTabs(false, tab);
  end
  else
  begin
    tab := tabmanager.GetTab(lua_tostring(L, 1));
    tabmanager.CloseAllTabs(false, tab);
  end;
  result := 1;
end;

function lua_readscxfile(L: plua_State): integer; cdecl;
var
  extfile: string;
  script: tstringlist;
begin
  extfile := GetSandcatDir(SCDIR_PLUGINS) + lua_tostring(L, 1);
  if fileexists(extfile) then
  begin
    script := tstringlist.create;
    script.text := GetTextFileFromZIP(extfile, lua_tostring(L, 2));
    // showmessage('pushing:'+s);
    lua_pushstring(L, script.text);
    script.free;
  end;
  result := 1;
end;

function lua_addjavascript(L: plua_State): integer; cdecl;
var
  event: string;
begin
  if (lua_tostring(L, 2) <> emptystr) then
  begin
    event := lowercase(lua_tostring(L, 1));
    if event = 'loadend' then
      userscript.JS_Tab_LoadEnd := userscript.JS_Tab_LoadEnd + crlf +
        lua_tostring(L, 2);
    if event = 'v8' then
      userscript.JS_V8_Extension := userscript.JS_V8_Extension +
        lua_tostring(L, 2);
  end;
  result := 1;
end;

function lua_addluascript(L: plua_State): integer; cdecl;
var
  event: string;
begin
  if lua_tostring(L, 2) <> emptystr then
  begin
    event := lowercase(lua_tostring(L, 1));
    if event = 'task.init' then
      userscript.Lua_Task_Init := userscript.Lua_Task_Init + crlf +
        lua_tostring(L, 2);
  end;
  result := 1;
end;

function lua_addtis(L: plua_State): integer; cdecl;
begin
  uix.AddTIS(lua_tostring(L, 2), lua_tostring(L, 1));
  result := 1;
end;

function lua_inserthtml(L: plua_State): integer; cdecl;
begin
  uix.InsertHTML(lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3),
    lua_tostring(L, 4));
  result := 1;
end;

function lua_inserthtmlfile(L: plua_State): integer; cdecl;
begin
  uix.InsertHTMLFile(lua_tostring(L, 1), lua_tostring(L, 2), lua_tostring(L, 3),
    lua_tostring(L, 4));
  result := 1;
end;

function lua_clearprivatedata(L: plua_State): integer; cdecl;
begin
  settings.ClearPrivateData(lua_tostring(L, 1));
  result := 1;
end;

function lua_scriptlogerror(L: plua_State): integer; cdecl;
begin
  extensions.LogScriptError('Runik', inttostr(lua_tointeger(L, 1)),
    lua_tostring(L, 2));
  result := 1;
end;

function lua_newtab(L: plua_State): integer; cdecl;
var
  tab: TSandcatTab;
begin
  tab := tabmanager.NewTab(lua_tostring(L, 1), lua_tostring(L, 2));
  if tab.UID <> emptystr then
    lua_pushinteger(L, tab.number)
  else
    lua_pushinteger(L, 0);
  result := 1;
end;

function lua_gototab(L: plua_State): integer; cdecl;
begin
  tabmanager.gototab(lua_tostring(L, 1));
  result := 1;
end;

function lua_setsearchengine(L: plua_State): integer; cdecl;
begin
  vSearchEngine_Name := lua_tostring(L, 1);
  vSearchEngine_QueryURL := lua_tostring(L, 2);
  vSearchEngine_Icon := lua_tostring(L, 3);
  navbar.UpdateSearchEngine;
  result := 1;
end;

function lua_setactivepage(L: plua_State): integer; cdecl;
begin
  if tabmanager.activetab<> nil then
  tabmanager.activetab.SetActivePage(lua_tostring(L, 1));
  result := 1;
end;

function lua_var_replace(L: plua_State): integer; cdecl;
var
  s: string;
begin
  s := sanddlg.DlgReplace(lua_tostring(L, 1));
  lua_pushstring(L, s);
  result := 1;
end;

{function lua_pluginsenable(L: plua_State): integer; cdecl;
begin
  settings.preferences[CRMO_PLUGINS] := lua_toboolean(L, 1);
  tabmanager.ReconfigureAllTabs;
  result := 1;
end; }

function lua_enabledebugmode(L: plua_State): integer; cdecl;
begin
  if lua_toboolean(L, 1) = true then
  EnableDebugMode;
  result := 1;
end;

function lua_addtodebuglog(L: plua_State): integer; cdecl;
begin
  debug(lua_tostring(L, 1), 'Lua Debug');
  result := 1;
end;

function lua_addlibraryinfo(L: plua_State): integer; cdecl;
begin
  extensions.AddLibraryInfo(lua_tostring(L, 1), lua_tostring(L, 2),
    lua_tostring(L, 3), lua_tostring(L, 4));
  result := 1;
end;

// Settings ----------------------------------------------------------------- //

function lua_sandcatsettings_savetofile(L: plua_State): integer; cdecl;
begin
  if lua_tostring(L, 1) <> emptystr then
    Settings.Preferences.SaveToFile(lua_tostring(L, 1));
  result := 1;
end;

function lua_sandcatsettings_loadfromfile(L: plua_State): integer; cdecl;
begin
  if lua_tostring(L, 1) <> emptystr then
    Settings.Preferences.loadfromfile(lua_tostring(L, 1));
  result := 1;
end;

function lua_sandcatsettings_get(L: plua_State): integer; cdecl;
begin
  if lua_isnone(L, 2) then
    plua_pushvariant(L, Settings.Preferences.getvalue(lua_tostring(L, 1)))
  else
    plua_pushvariant(L, Settings.Preferences.getvalue(lua_tostring(L, 1),
      plua_tovariant(L, 2)));
  result := 1;
end;

function lua_sandcatsettings_getalljson(L: plua_State): integer; cdecl;
begin
  // If param 1 is provided and is false, returns default settings
  lua_pushstring(L, Settings.Preferences.Current.Text);
  result := 1;
end;

function lua_sandcatsettings_getalldefaultjson(L: plua_State): integer; cdecl;
begin
  lua_pushstring(L, Settings.Preferences.Default.Text);
  result := 1;
end;

function lua_sandcatsettings_save(L: plua_State): integer; cdecl;
begin
  Settings.Save;
  result := 1;
end;

function lua_sandcatsettings_settext(L: plua_State): integer; cdecl;
begin
  Settings.Preferences.loadfromstring(lua_tostring(L, 1));
  result := 1;
end;

function lua_sandcatsettings_getfilename(L: plua_State): integer; cdecl;
begin
  lua_pushstring(L, Settings.Preferences.filename);
  result := 1;
end;

function lua_sandcatsettings_getsiteprefsfilename(L: plua_State): integer; cdecl;
begin
  lua_pushstring(L, Settings.GetSitePrefsFilename(lua_tostring(L, 1)));
  result := 1;
end;

function lua_sandcatsettings_getdefault(L: plua_State): integer; cdecl;
begin
  plua_pushvariant(L, Settings.Preferences.Default[lua_tostring(L, 1)]);
  result := 1;
end;

function lua_sandcatsettings_registerdefault(L: plua_State): integer; cdecl;
var
  custom:TCatPrefsCustomOption;
begin
  // debug('registering default: '+lua_tostring(L,1)+' value:'+s,'Settings');
  custom.HideInOptionList := true;
    Settings.Preferences.RegisterDefault(lua_tostring(L, 1),
      plua_tovariant(L, 2), custom);
  result := 1;
end;

function lua_sandcatsettings_set(L: plua_State): integer; cdecl;
begin
  Settings.Preferences[lua_tostring(L, 1)] := plua_tovariant(L, 2);
  result := 1;
end;

function lua_sandcatsettings_update(L: plua_State): integer; cdecl;
begin
  Settings.Update;
  result := 1;
end;


end.



