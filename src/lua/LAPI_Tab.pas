unit LAPI_Tab;
{
  Sandcat Tab LUA Object
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Classes, Forms, SysUtils, Graphics, TypInfo, Dialogs, Lua, LuaObject, uTabMan;

type
  TSCBTabObject = class(TLuaObject)
  private
  public
    constructor Create(LuaState: PLua_State; AParent: TLuaObject = nil);
      overload; override;
    function GetPropValue(propName: String): Variant; override;
    function SetPropValue(propName: String; const AValue: Variant)
      : Boolean; override;
    destructor Destroy; override;
  end;

procedure RegisterSCBTab_Sandcat(L: PLua_State);
function BuildCustomTabFromLuaTable(L: PLua_State): TCustomTabSettings;

implementation

uses
  uMain, uTaskMan, uTab, plua, CatFiles, uMisc, CatStrings, uUIComponents,
  CatChromium, uSettings, uConst, uZones, uLiveHeaders, pLuaTable, LAPI_CEF;

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

function method_gotourl(L: PLua_State): integer; cdecl;
begin
  result := 1;
  if tabmanager.ActiveTab = nil then
    exit;
  if lua_tostring(L, 2) = emptystr then // no url provided, go to url in bar
    tabmanager.ActiveTab.GoToURL(navbar.url)
  else
    tabmanager.ActiveTab.GoToURL(lua_tostring(L, 2), lua_tostring(L, 3));
end;

function method_viewdevtools(L: PLua_State): integer; cdecl;
begin
  if tabmanager.ActiveTab <> nil then
    tabmanager.ActiveTab.ViewDevTools;
  result := 1;
end;

function method_evaljavascript(L: PLua_State): integer; cdecl;
var
  r: Variant;
begin
  if tabmanager.ActiveTab <> nil then
    r := tabmanager.ActiveTab.evaljavascript(lua_tostring(L, 2));
  plua_pushvariant(L, r);
  result := 1;
end;

function method_runjavascript(L: PLua_State): integer; cdecl;
var
  reporterrors: Boolean;
begin
  reporterrors := true;
  if lua_isnone(L, 5) = false then
    reporterrors := lua_toboolean(L, 5);
  if tabmanager.ActiveTab <> nil then
    tabmanager.ActiveTab.runjavascript(lua_tostring(L, 2), lua_tostring(L, 3),
      lua_tointeger(L, 4), reporterrors);
  result := 1;
end;

function method_goback(L: PLua_State): integer; cdecl;
begin
  if tabmanager.ActiveTab.Chrome <> nil then
    tabmanager.ActiveTab.Chrome.GoBack;
  result := 1;
end;

function method_goforward(L: PLua_State): integer; cdecl;
begin
  if tabmanager.ActiveTab.Chrome <> nil then
    tabmanager.ActiveTab.Chrome.GoForward;
  result := 1;
end;

function method_reload(L: PLua_State): integer; cdecl;
var
  igncache: Boolean;
begin
  igncache := false;
  if lua_isnone(L, 2) = false then
    igncache := lua_toboolean(L, 2);
  if tabmanager.ActiveTab.Chrome <> nil then
    tabmanager.ActiveTab.Chrome.Reload(igncache);
  result := 1;
end;

function method_load(L: PLua_State): integer; cdecl;
var
  s, tablename: string;
begin
  s := lua_tostring(L, 2);
  tablename := lua_tostring(L, 3);
  if tablename <> emptystr then
    s := '<meta name="SandcatUIX" content="' + tablename + '">' + crlf + s;
  tabmanager.ActiveTab.LoadExtensionPage(s);
  result := 1;
end;

function method_loadrequest(L: PLua_State): integer; cdecl;
begin
  if lua_istable(L, 2) then // user provided a Lua table
    tabmanager.ActiveTab.SendRequestCustom(BuildRequestFromLuaTable(L), true)
  else
    tabmanager.ActiveTab.SendRequest(lua_tostring(L, 2), lua_tostring(L, 3),
      lua_tostring(L, 4), true);
  result := 1;
end;

function method_sendrequest(L: PLua_State): integer; cdecl;
begin
  if lua_istable(L, 2) then // user provided a Lua table
    tabmanager.ActiveTab.SendRequestCustom(BuildRequestFromLuaTable(L), false)
  else
    tabmanager.ActiveTab.SendRequest(lua_tostring(L, 2), lua_tostring(L, 3),
      lua_tostring(L, 4), false);
  result := 1;
end;

function method_logrequest(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.Requests.LogRequest
    (BuildRequestDetailsFromJSON(lua_tostring(L, 2), lua_tostring(L, 3)));
  result := 1;
end;

function method_logerrortoconsole(L: PLua_State): integer; cdecl;
var
  b: Boolean;
begin
  extensions.ScriptExceptionHandler(lua_tostring(L, 2), lua_tointeger(L, 3),
    lua_tostring(L, 4), b);
  result := 1;
end;

function method_log(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.log.lines.add(lua_tostring(L, 2));
  result := 1;
end;

function method_clearlog(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.log.lines.Clear;
  result := 1;
end;

function method_clearheaders(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.Requests.Clear;
  result := 1;
end;

function method_addjavascript(L: PLua_State): integer; cdecl;
var
  event: string;
begin
  if lua_tostring(L, 3) <> emptystr then
  begin
    event := lua_tostring(L, 2);
    if event = 'loadend' then
      tabmanager.ActiveTab.usertabscript.JS_LoadEnd :=
        tabmanager.ActiveTab.usertabscript.JS_LoadEnd + crlf +
        lua_tostring(L, 3);
  end;
  result := 1;
end;

function method_dosearch(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.DoSearch(navbar.SearchText);
  result := 1;
end;

function method_cache_storestring(L: PLua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 3);
  tabmanager.ActiveTab.Cache.StoreString(lua_tostring(L, 2), s);
  application.ProcessMessages;
  result := 1;
end;

function method_cache_gettextfile(L: PLua_State): integer; cdecl;
var
  s: string;
begin
  s := tabmanager.ActiveTab.Cache.gettextfile(lua_tostring(L, 2));
  lua_pushstring(L, s);
  result := 1;
end;

function method_cache_extractfile(L: PLua_State): integer; cdecl;
var
  outdir, outfilename: string;
begin
  outfilename := emptystr;
  if tabmanager.ActiveTab.Cache.cachedfileexists(lua_tostring(L, 2)) then
  begin
    outdir := GetSandcatDir(SCDIR_TEMP) + 'Preview\';
    forcedir(outdir);
    outfilename := outdir + inttostr(tabmanager.ActiveTab.handle) + '.resp';
    tabmanager.ActiveTab.Cache.extractfile(lua_tostring(L, 2), outfilename);
  end;
  lua_pushstring(L, outfilename);
  result := 1;
end;

function method_gotosrcline(L: PLua_State): integer; cdecl;
var
  s: TSandSynEdit;
begin
  tabmanager.ActiveTab.SetActivePage('source');
  s := tabmanager.ActiveTab.SourceInspect.Source;
  s.GotoLineAndCenter(lua_tointeger(L, 2));
  s.ActiveLineColor := cSourceActiveLineColor;
  result := 1;
end;

function method_resources_loadcol(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.Resources.RedefineColumns(lua_tostring(L, 2),
    lua_tostring(L, 3));
  result := 1;
end;

function method_resources_clear(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.Resources.Lv.Items.Clear;
  result := 1;
end;

{ function method_setparam(L:plua_State):integer; cdecl;
  const cParams='Params';
  var t:integer; section:string;
  begin
  t:=lua_type(L,3); section:=lua_tostring(L,2);
  case t of
  LUA_TSTRING: tabmanager.ActiveTab.UserParams.WriteString(cParams,section,lua_tostring(L,3),'base64');
  LUA_TBOOLEAN: tabmanager.ActiveTab.UserParams.WriteBool(cParams,section,lua_toboolean(L,3));
  LUA_TNUMBER:  tabmanager.ActiveTab.UserParams.WriteInteger(cParams,section,lua_tointeger(L,3));
  end;
  //v:=plua_tovariant(L,2);
  result:=1;
  end; }

function method_setparam(L: PLua_State): integer; cdecl;
var
  s: string;
begin
  s := lua_tostring(L, 3);
  tabmanager.ActiveTab.UserData[lua_tostring(L, 2)] := s;
  result := 1;
end;

function method_getparam(L: PLua_State): integer; cdecl;
begin
  lua_pushstring(L, tabmanager.ActiveTab.UserData.getvalue(lua_tostring(L, 2),
    lua_tostring(L, 3)));
  result := 1;
end;

function method_seticon(L: PLua_State): integer; cdecl;
begin
  if lua_isnone(L, 3) = true then
    tabmanager.ActiveTab.SetIcon(lua_tostring(L, 2))
  else
    tabmanager.ActiveTab.SetIcon(lua_tostring(L, 2), lua_toboolean(L, 3));
  result := 1;
end;

function method_showrequest(L: PLua_State): integer; cdecl;
begin
  uix.ShowRequest(tabmanager.ActiveTab.Requests, lua_tostring(L, 2));
  result := 1;
end;

function method_cache_export(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.Cache.savetofile(lua_tostring(L, 2));
  result := 1;
end;

function method_cache_import(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.Cache.loadfromfile(lua_tostring(L, 2));
  result := 1;
end;

function method_viewsourceexternal(L: PLua_State): integer; cdecl;
begin
  if tabmanager.ActiveTab.Chrome <> nil then
    tabmanager.ActiveTab.Chrome.ViewSourceExternalEditor;
  result := 1;
end;

function method_stopload(L: PLua_State): integer; cdecl;
begin
  if tabmanager.ActiveTab.Chrome <> nil then
    tabmanager.ActiveTab.Chrome.Stop;
  result := 1;
end;

function method_showauthdialog(L: PLua_State): integer; cdecl;
begin
  if tabmanager.ActiveTab.Chrome <> nil then
    tabmanager.ActiveTab.Chrome.ShowAuthDialog(lua_tostring(L, 2),
      lua_tostring(L, 3));
  result := 1;
end;

function method_request_setresponse(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.Requests.UpdateRequest(lua_tostring(L, 2),
    lua_tostring(L, 3));
  result := 1;
end;

function method_request_exists(L: PLua_State): integer; cdecl;
begin
  lua_pushboolean(L, tabmanager.ActiveTab.Requests.requestexists
    (lua_tostring(L, 2)));
  result := 1;
end;

function method_request_getdetails(L: PLua_State): integer; cdecl;
var
  r: TSandcatRequestDetails;
begin
  r := tabmanager.ActiveTab.Requests.GetRequest(lua_tostring(L, 2));
  lua_pushrequestdetails(L, r);
  result := 1;
end;

function method_runluatask(L: PLua_State): integer; cdecl;
var
  task: TSandcatTask;
begin
  task := tasks.AddTask(lua_tostring(L, 4), false);
  if lua_isnone(L, 3) = false then
    task.SetParams(lua_tostring(L, 3));
  task.runscript(lua_tostring(L, 2));
  lua_pushstring(L, task.TID);
  result := 1;
end;

function method_runluaonlog(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.RunLuaOnLog(lua_tostring(L, 2), lua_tostring(L, 3));
  result := 1;
end;

function method_loadheaders(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.liveheaders.loadfromfile(lua_tostring(L, 2));
  result := 1;
end;

function method_saveheaders(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.liveheaders.savetofile(lua_tostring(L, 2));
  result := 1;
end;

function method_tree_clear(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.SideTree_Clear;
  result := 1;
end;

function method_tree_loaddir(L: PLua_State): integer; cdecl;
var
  makebold: Boolean;
begin
  makebold := false;
  if lua_isnone(L, 3) = false then
    makebold := lua_toboolean(L, 3);
  tabmanager.ActiveTab.SideTree_LoadDir(lua_tostring(L, 2), makebold);
  if lua_tostring(L, 4) <> emptystr then
    tabmanager.ActiveTab.SideTree_LoadAffectedScripts(lua_tostring(L, 4));
  result := 1;
end;

function method_loadsourcetabs(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.SourceInspect.LoadTabs(lua_tostring(L, 2),
    lua_tostring(L, 3));
  result := 1;
end;

function method_loadsourcemsgs(L: PLua_State): integer; cdecl;
begin
  tabmanager.ActiveTab.SourceInspect.LoadItems(lua_tostring(L, 2),
    tabmanager.ActiveTab.SourceInspect.ActiveTab);
  result := 1;
end;

type
  TSourceCmds = (copy, cut, paste, undo, readonly, redo, loadfromfile, loadmsgs,
    savetofile, showmsgs);

function method_runsourcecommand(L: PLua_State): integer; cdecl;
var
  tab: TSandcatTab;
begin
  tab := tabmanager.ActiveTab;
  case TSourceCmds(GetEnumValue(TypeInfo(TSourceCmds),
    lowercase(lua_tostring(L, 2)))) of
    copy:
      tab.SourceInspect.Source.CopyToClipboard;
    cut:
      tab.SourceInspect.Source.CutToClipboard;
    paste:
      tab.SourceInspect.Source.PasteFromClipboard;
    undo:
      tab.SourceInspect.Source.undo;
    readonly:
      tab.SourceInspect.Source.ReadOnly := lua_toboolean(L, 3);
    redo:
      tab.SourceInspect.Source.redo;
    loadmsgs:
      tab.SourceInspect.LoadItems(lua_tostring(L, 3),
        tab.SourceInspect.ActiveTab);
    loadfromfile:
      tab.LoadSourceFile(lua_tostring(L, 3));
    savetofile:
      tab.SourceInspect.Source.lines.savetofile(lua_tostring(L, 3));
    showmsgs:
      tab.SourceInspect.MsgsPanel.visible := lua_toboolean(L, 3);
  end;
  result := 1;
end;

type
  TProps = (activepage, datafilename, handle, icon, lastjslogmsg, loadend,
    loadendjs, capture, capturebrowser, capturerealtime, captureurls,
    downloadfiles, headersfilter, logtext, mode, name, rcvdheaders, reslist,
    screenshot, sentheaders, showtree, siteprefsfilename, Source,
    sourcefilename, statuscode, status, Title, updatesource, url, urldev,
    urllist, zoomlevel);

function TSCBTabObject.GetPropValue(propName: String): Variant;
begin
  case TProps(GetEnumValue(TypeInfo(TProps), lowercase(propName))) of
    datafilename:
      result := tabmanager.ActiveTab.Cache.getFileName;
    handle:
      result := tabmanager.ActiveTab.Msg.msgHandle;
    icon:
      result := tabmanager.ActiveTab.icon;
    capture:
      result := tabmanager.ActiveTab.Requests.logrequests;
    capturebrowser:
      result := tabmanager.ActiveTab.logbrowserrequests;
    captureurls:
      if tabmanager.ActiveTab.Chrome <> nil then
        result := tabmanager.ActiveTab.Chrome.LogURLs;
    headersfilter:
      result := tabmanager.ActiveTab.liveheaders.FilterEdit.Text;
    lastjslogmsg:
      result := tabmanager.ActiveTab.LastConsoleLogMessage;
    logtext:
      result := tabmanager.ActiveTab.log.lines.Text;
    name:
      result := tabmanager.ActiveTab.UID;
    rcvdheaders:
      if tabmanager.ActiveTab.Chrome <> nil then
        result := tabmanager.ActiveTab.Chrome.Headers.rcvdhead;
    reslist:
      if tabmanager.ActiveTab.Chrome <> nil then
        result := tabmanager.ActiveTab.Chrome.ResourceList.Text;
    screenshot:
      result := tabmanager.ActiveTab.GetScreenshot;
    sentheaders:
      if tabmanager.ActiveTab.Chrome <> nil then
        result := tabmanager.ActiveTab.Chrome.Headers.senthead;
    siteprefsfilename:
      result := tabmanager.ActiveTab.SitePrefsFile;
    Source:
      result := tabmanager.ActiveTab.SourceInspect.Source.Text;
    sourcefilename:
      result := tabmanager.ActiveTab.SourceInspect.sourcefilename;
    statuscode:
      if tabmanager.ActiveTab.Chrome <> nil then
        result := strtointsafe(tabmanager.ActiveTab.Chrome.Headers.statuscode);
    Title:
      result := tabmanager.ActiveTab.Title;
    url:
      result := tabmanager.ActiveTab.GetURL;
    urllist:
      if tabmanager.ActiveTab.Chrome <> nil then
        result := tabmanager.ActiveTab.Chrome.URLLog.Text;
    zoomlevel:
      if tabmanager.ActiveTab.Chrome <> nil then
        result := tabmanager.ActiveTab.Chrome.zoomlevel;
  else
    result := inherited GetPropValue(propName);
  end;
end;

function TSCBTabObject.SetPropValue(propName: String;
  const AValue: Variant): Boolean;
begin
  result := true;
  case TProps(GetEnumValue(TypeInfo(TProps), lowercase(propName))) of
    downloadfiles:
      if tabmanager.ActiveTab.Chrome <> nil then
        tabmanager.ActiveTab.Chrome.EnableDownloads := AValue;
    icon:
      tabmanager.ActiveTab.SetIcon(String(AValue), true);
    loadend:
      tabmanager.ActiveTab.usertabscript.Lua_LoadEnd_RunOnce := String(AValue);
    loadendjs:
      tabmanager.ActiveTab.usertabscript.JS_LoadEnd_RunOnce := String(AValue);
    capture:
      tabmanager.ActiveTab.Requests.logrequests := AValue;
    capturebrowser:
      tabmanager.ActiveTab.logbrowserrequests := AValue;
    captureurls:
      if tabmanager.ActiveTab.Chrome <> nil then
        tabmanager.ActiveTab.Chrome.LogURLs := AValue;
    headersfilter:
      tabmanager.ActiveTab.liveheaders.FilterEdit.Text := String(AValue);
    logtext:
      tabmanager.ActiveTab.log.lines.Text := String(AValue);
    Source:
      begin
        tabmanager.ActiveTab.AdjustHighlighter;
        tabmanager.ActiveTab.SourceInspect.Source.Text := String(AValue);
      end;
    showtree:
      tabmanager.ActiveTab.ShowSideTree(AValue);
    status:
      StatBar.Text := String(AValue);
    // ToDo: associate with current tab
    Title:
      tabmanager.ActiveTab.SetTitle(String(AValue));
    updatesource:
      tabmanager.ActiveTab.CanUpdateSource := AValue;
    zoomlevel:
      if tabmanager.ActiveTab.Chrome <> nil then
        tabmanager.ActiveTab.Chrome.zoomlevel := AValue;
  else
    result := inherited SetPropValue(propName, AValue);
  end;
end;

procedure register_methods(L: PLua_State; classTable: integer);
begin
  // JavaScript to be executed after loading web sites. Affects just the active tab.
  RegisterMethod(L, 'addjavascript', method_addjavascript, classTable);
  RegisterMethod(L, 'cache_storestring', method_cache_storestring, classTable);
  RegisterMethod(L, 'cache_import', method_cache_import, classTable);
  RegisterMethod(L, 'cache_export', method_cache_export, classTable);
  RegisterMethod(L, 'cache_gettextfile', method_cache_gettextfile, classTable);
  RegisterMethod(L, 'cache_getrequestdetails', method_request_getdetails,
    classTable);
  RegisterMethod(L, 'cache_extractfile', method_cache_extractfile, classTable);
  RegisterMethod(L, 'cache_setreqresp', method_request_setresponse, classTable);
  RegisterMethod(L, 'cache_requestexists', method_request_exists, classTable);
  RegisterMethod(L, 'clearlog', method_clearlog, classTable);
  RegisterMethod(L, 'clearheaders', method_clearheaders, classTable);
  RegisterMethod(L, 'evaljs', method_evaljavascript, classTable);
  RegisterMethod(L, 'goback', method_goback, classTable);
  RegisterMethod(L, 'goforward', method_goforward, classTable);
  RegisterMethod(L, 'gotosrcline', method_gotosrcline, classTable);
  RegisterMethod(L, 'gotourl', method_gotourl, classTable);
  RegisterMethod(L, 'loadrequest', method_loadrequest, classTable);
  RegisterMethod(L, 'loadheaders', method_loadheaders, classTable);
  RegisterMethod(L, 'loadsourcetabs', method_loadsourcetabs, classTable);
  RegisterMethod(L, 'loadsourcemsgs', method_loadsourcemsgs, classTable);
  RegisterMethod(L, 'loadx', method_load, classTable);
  RegisterMethod(L, 'log', method_log, classTable);
  RegisterMethod(L, 'logerror', method_logerrortoconsole, classTable);
  RegisterMethod(L, 'logrequest', method_logrequest, classTable);
  RegisterMethod(L, 'reload', method_reload, classTable);
  RegisterMethod(L, 'sendrequest', method_sendrequest, classTable);
  RegisterMethod(L, 'resources_clear', method_resources_clear, classTable);
  RegisterMethod(L, 'resources_loadcol', method_resources_loadcol, classTable);
  RegisterMethod(L, 'runluaonlog', method_runluaonlog, classTable);
  RegisterMethod(L, 'runjs', method_runjavascript, classTable);
  RegisterMethod(L, 'runsrccmd', method_runsourcecommand, classTable);
  RegisterMethod(L, 'runtask', method_runluatask, classTable);
  RegisterMethod(L, 'saveheaders', method_saveheaders, classTable);
  RegisterMethod(L, 'search', method_dosearch, classTable);
  // RegisterMethod(L,'seticon',method_seticon,classTable);
  RegisterMethod(L, 'showauthdialog', method_showauthdialog, classTable);
  RegisterMethod(L, 'showrequest', method_showrequest, classTable);
  RegisterMethod(L, 'stopload', method_stopload, classTable);
  RegisterMethod(L, 'tree_clear', method_tree_clear, classTable);
  RegisterMethod(L, 'tree_loaddir', method_tree_loaddir, classTable);
  RegisterMethod(L, 'userdata_get', method_getparam, classTable);
  RegisterMethod(L, 'userdata_set', method_setparam, classTable);
  RegisterMethod(L, 'viewdevtools', method_viewdevtools, classTable);
  RegisterMethod(L, 'viewsource', method_viewsourceexternal, classTable);
end;

const
  AClassName = 'SandcatBrowserTab';

function newcallback(L: PLua_State; AParent: TLuaObject = nil): TLuaObject;
begin
  result := TSCBTabObject.Create(L, AParent);
end;

function Create(L: PLua_State): integer; cdecl;
var
  p: TLuaObjectNewCallback;
begin
  p := @newcallback;
  result := new_LuaObject(L, AClassName, p);
end;

procedure RegisterSCBTab_Sandcat(L: PLua_State);
begin
  RegisterTLuaObject(L, AClassName, @Create, @register_methods);
end;

constructor TSCBTabObject.Create(LuaState: PLua_State; AParent: TLuaObject);
begin
  inherited Create(LuaState, AParent);
end;

destructor TSCBTabObject.Destroy;
begin
  inherited Destroy;
end;

end.
