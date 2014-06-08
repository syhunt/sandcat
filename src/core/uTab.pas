unit uTab;
{
  Sandcat Browser Tab component
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.

  TODO: Make it smaller
}

interface

uses
{$IF CompilerVersion >= 23} // XE2 or higher
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ComCtrls, System.TypInfo,
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, TypInfo,
{$IFEND}
  CatUI, uUIComponents, CatConsole, CatChromium, uRequests, SynUnicode,
  uLiveHeaders, uCodeInspect;

type // Used for restoring the state of a tab when switching tabs
  TTabState = class
  private
  public
    ActivePage: string;
    ActivePageOri: string;
    CustomDefaultPage: string;
    HasConsole: boolean;
    HasCustomToolbar: boolean;
    IsCustom: boolean;
    IsBookmarked: boolean;
    ProtoIcon: string;
    ResourcesAscending: boolean;
    ResourcesLastSortedColumn: integer;
    ShowNavBar: boolean;
    ShowTabsStrip: boolean;
    URL: string;
    procedure LoadDefault;
    procedure LoadState(const TabID, CurrentURL: string);
    procedure SaveState;
    constructor Create;
    destructor Destroy; override;
  end;

type
  TSandcatTabUserScript = record
    JS_LoadEnd: string;
    JS_LoadEnd_RunOnce: string;
    Lua_LoadEnd_RunOnce: string;
    Lua_UserRequestSent_RunOnce: string;
  end;

type
  TSandcatTabOnMessage = procedure(ASender: TObject; const msgid: integer;
    const msg: array of string) of object;

type
  TSandcatTab = class(TCustomControl)
  private
    fAllowedLuaScripts: TStringList;
    fBrowserPanel: TCanvasPanel;
    fCache: TSandCache;
    fCanUpdateSource: boolean;
    fChrome: TCatChromium;
    fCustomTab: TSandUIEngine;
    fCustomToolbar: TSandUIEngine;
    fDefaultIcon: string;
    fDownloadsList: TStringList;
    fIsClosing: boolean;
    fLiveHeaders: TLiveHeaders;
    fLoading: boolean;
    fLog: TMemo;
    fLogBrowserRequests: boolean;
    fMainPanel: TPanel;
    fMsgHandle: HWND;
    fNumber: integer; // unique tab number
    fOnMessage: TSandcatTabOnMessage;
    fRequests: TSandcatRequests;
    fResourcesLv: TListView;
    fRetrieveFavIcon: boolean;
    fSideTree: TTreeView;
    fSyncWithTask: boolean;
    fSourceInspect: TSyCodeInspector;
    fSourceManual: string;
    fSubTabs: TNoteBook;
    fState: TTabState;
    fTitle: string;
    fTreeSplitter: TSplitter;
    fUID: string; // unique tab ID
    fUserJSExecuted: boolean;
    fUserData: TSandJSON;
    fUserTag: string;
    fV8MsgHandle: HWND;
    function GetIcon: string;
    function GetSitePrefsFile: string;
    function GetTitle: string;
    function IsLuaWhiteListed(const script: string): boolean;
    procedure AddPageResource(const URL: string);
    procedure BrowserMessage(const msg: integer; const str: string);
    procedure CopyDataMessage(const msg: integer; const str: string);
    procedure CodeEditDropFiles(Sender: TObject; X, Y: integer;
      AFiles: TUnicodeStrings);
    procedure InitChrome;
    procedure CreateLiveHeaders;
    procedure CreateMainPanel;
    procedure CreateSideTree;
    procedure CrmBeforePopup(Sender: TObject; var URL: string;
      out Result: boolean);
    procedure CrmTitleChange(Sender: TObject; const title: string);
    procedure CrmLoadStart(Sender: TObject);
    procedure CrmLoadEnd(Sender: TObject; httpStatusCode: integer);
    procedure CrmStatusMessage(Sender: TObject; const value: string);
    procedure CrmAddressChange(Sender: TObject; const URL: string);
    procedure CrmConsoleMessage(Sender: TObject; const message, source: string;
      line: integer);
    procedure CrmBeforeDownload(Sender: TObject; const id: integer;
      const suggestedName: string);
    procedure CrmDownloadUpdated(Sender: TObject; var cancel: boolean;
      const id, state, percentcomplete: integer; const fullPath: string);
    procedure CrmLoadingStateChange(Sender: TObject;
      const isLoading, canGoBack, canGoForward: boolean);
    procedure CrmLoadError(Sender: TObject; const errorCode: integer;
      const errorText, failedUrl: string);
    procedure JavaScriptExecutionEnd;
    procedure MainThreadMsgWindow(var AMsg: TMessage);
    procedure ResourcesListViewClick(Sender: TObject);
    procedure ResourcesListviewColumnClick(Sender: TObject;
      Column: TListColumn);
    procedure RunJSONCmd(const json: string);
    procedure RunUserScript(var script: string; const lang: integer;
      const runonce: boolean = false);
    procedure RunUserScripts(const event: integer);
    procedure SetLoading(const b: boolean);
    procedure SideTree_LoadItem(const path: string);
    procedure SideTreeChange(Sender: TObject; Node: TTreeNode);
    procedure SideTreeDblClick(Sender: TObject);
    procedure UpdateSourceCode;
    procedure V8Msg(var AMsg: TMessage);
    procedure WMCopyData(var message: TMessage); message WM_COPYDATA;
  public
    UserTabScript: TSandcatTabUserScript;
    function Close(const silent: boolean = false): boolean;
    function EvalJavaScript(const script: string): variant;
    function GetScreenshot: string;
    function GetURL: string;
    function IsActiveTab: boolean;
    procedure AdjustHighlighter(const URL: string = '');
    procedure BeforeLoad(const URL: string);
    procedure GoToURL(const URL: string; const source: string = '');
    procedure LoadSettings;
    procedure LoadState;
    procedure LoadExtensionPage(const html: string);
    procedure LoadExtensionToolbar(const html: string);
    procedure LoadSourceFile(const filename: string);
    procedure LogWriteLn(const s: string);
    procedure LogWrite(const s: string);
    procedure DoSearch(const term: string; const newtab: boolean = false);
    procedure RunLua(const lua: string);
    procedure RunJavaScript(const script: string); overload;
    procedure RunJavaScript(const script: string; const scripturl: string;
      const startline: integer; const luacallback: string;
      const reporterrors: boolean = false); overload;
    procedure SendRequest(const method, URL, postdata: string;
      const load: boolean = false);
    procedure SendRequestCustom(req: TCatChromiumRequest;
      load: boolean = false);
    procedure SendRequestXHR(const req: TCatChromiumXHR);
    procedure SetActivePage(const name: string);
    procedure SetIcon(const URL: string; const force: boolean = false);
    procedure SetTitle(const title: string);
    procedure ShowSideTree(const visible: boolean);
    procedure SideTree_Clear;
    procedure SideTree_LoadDir(const dir: string;
      const makebold: boolean = true);
    procedure SideTree_LoadAffectedScripts(const paths: string);
    procedure WhiteListLua(const script: string; const allowed: boolean = true);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // properties
    property BrowserPanel: TCanvasPanel read fBrowserPanel;
    property Cache: TSandCache read fCache;
    property CanUpdateSource: boolean read fCanUpdateSource
      write fCanUpdateSource;
    property Chrome: TCatChromium read fChrome;
    property CustomTab: TSandUIEngine read fCustomTab;
    property CustomToolbar: TSandUIEngine read fCustomToolbar;
    property Icon: string read GetIcon;
    property IsClosing: boolean read fIsClosing;
    property LiveHeaders: TLiveHeaders read fLiveHeaders;
    property Loading: boolean read fLoading write SetLoading;
    property Log: TMemo read fLog;
    property LogBrowserRequests: boolean read fLogBrowserRequests
      write fLogBrowserRequests;
    property OnMessage: TSandcatTabOnMessage read fOnMessage write fOnMessage;
    property MsgHandle: HWND read fMsgHandle;
    property Number: integer read fNumber write fNumber;
    property Requests: TSandcatRequests read fRequests;
    property SitePrefsFile: string read GetSitePrefsFile;
    property SourceInspect: TSyCodeInspector read fSourceInspect;
    property state: TTabState read fState;
    property SubTabs: TNoteBook read fSubTabs;
    property title: string read GetTitle;
    property UID: string read fUID write fUID;
    property UserData: TSandJSON read fUserData;
    property UserTag: string read fUserTag write fUserTag;
  end;

const // tab events to be sent to the tab manager
  SCBT_NEWTITLE = 1;
  SCBT_GOTOURL = 2;
  SCBT_LOADEND = 3;
  SCBT_STATUS = 4;
  SCBT_URLCHANGE = 5;
  SCBT_LOADSTART = 6;
  SCBT_LOADERROR = 7;
  SCBT_LOADTABEND = 8;
  SCBT_GETSCREENSHOT = 9;

const // messages from the V8 extension or Sandcat tasks
  SCBM_LOGWRITELN = 2;
  SCBM_LOGWRITE = 3;
  SCBM_CONSOLE_ENDEXTERNALOUTPUT = 4;
  SCBM_TASK_RUNJSONCMD = 6;
  SCBM_TASK_SETPARAM = 7;
  SCBM_LOGADD = 8;
  SCBM_LOGDYNAMICREQUEST = 9;
  SCBM_XHR_LOG = 10;
  SCBM_LOGEXTERNALREQUEST_JSON = 11;
  SCBM_LUA_RUN = 14;
  SCBM_MONITOR_EVAL = 15;
  SCBM_XHR_SEND = 16;
  SCBM_REQUEST_SEND = 17;
  SCBM_RUNJSONCMD = 18;
  SCBM_TASK_STOPPED = 19;
  SCBM_TASK_SUSPENDED = 20;
  SCBM_TASK_RESUMED = 21;

implementation

uses
  uMain, uConst, uZones, uMisc, uTaskMan, uSettings, CatStrings, CatHTTP,
  CatUtils, LAPI_HTTPReq, LAPI_Task, LAPI_Browser, CatFiles;

var
  SandcatBrowserTab: TSandcatTab;

procedure Debug(const s: string; const component: string = 'Tab');
begin
  uMain.Debug(s, component);
end;

// Sorts the resources page listview columns
function Resources_SortByColumn(Item1, Item2: TListItem; Data: integer)
  : integer; stdcall;
begin
  if Data = 0 then
    Result := AnsiCompareText(Item1.Caption, Item2.Caption)
  else
    Result := AnsiCompareText(Item1.SubItems[Data - 1],
      Item2.SubItems[Data - 1]);
  if not tabmanager.ActiveTab.state.ResourcesAscending then
    Result := -Result;
end;

// Returns the filename of a site preferences file. This is a JSON file that
// can be used for storing user preferences for each specific host:port
function TSandcatTab.GetSitePrefsFile: string;
begin
  Result := Format('%s [%s].json', [ExtractURLHost(GetURL),
    IntToStr(ExtractURLPort(GetURL))]);
  Result := GetSandcatDir(SCDIR_CONFIGSITE, true) + Result;
end;

// Returns true if this is the currently active tab, false if otherwise
function TSandcatTab.IsActiveTab: boolean;
begin
  Result := UID = tabmanager.ActiveTabID;
end;

// Loads a source code from a file in the source page
procedure TSandcatTab.LoadSourceFile(const filename: string);
begin
  fSourceInspect.LoadFromFile(filename);
  // Adopts highlighter based on the filename extension
  fSourceInspect.source.highlighter := Highlighters.GetByFileExtension
    (extractfileext(filename));
end;

// Clears the side tree
procedure TSandcatTab.SideTree_Clear;
begin
  fSideTree.OnChange := nil;
  fSideTree.Items.Clear;
  fSideTree.OnChange := SideTreeChange;
end;

// Handles side tree item selection changes
procedure TSandcatTab.SideTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if (fSideTree.Selected = nil) then
    exit;
  SetNodeBoldState(fSideTree.Selected, false);
  SideTree_LoadItem(GetFullPath(fSideTree.Selected));
end;

// Handles side tree item doubleclicks
procedure TSandcatTab.SideTreeDblClick(Sender: TObject);
begin
  if (fSideTree.Selected = nil) then
    exit;
  SideTree_LoadItem(GetFullPath(fSideTree.Selected));
end;

// Updates site tree item images, highlighting scripts that have some issue
// Used by Sandcat extensions
procedure TSandcatTab.SideTree_LoadAffectedScripts(const paths: string);
begin
  UIX.Tree_SetAffectedImages(fSideTree, paths);
end;

// Can be called by extensions to load a directory tree as the side tree items
procedure TSandcatTab.SideTree_LoadDir(const dir: string;
  const makebold: boolean = true);
begin
  UIX.Tree_FilePathToTreeNode(fSideTree, nil, dir, true, makebold);
end;

// Called when an item from the side tree has been clicked
procedure TSandcatTab.SideTree_LoadItem(const path: string);
begin
  if fIsClosing then
    exit;
  Extensions.LuaWrap.value['_temppath'] := path;
  Extensions.RunLuaCmd('tab.tree_loaditem(_temppath)');
end;

// Displays the side tree (used by Sandcat extensions)
procedure TSandcatTab.ShowSideTree(const visible: boolean);
begin
  fSideTree.visible := visible;
  pagebar.AdjustPageStrip(fSideTree);
  fTreeSplitter.visible := visible;
  fTreeSplitter.Left := fSideTree.Left + 1;
end;

// Handles file drops in the code editor
procedure TSandcatTab.CodeEditDropFiles(Sender: TObject; X, Y: integer;
  AFiles: TUnicodeStrings);
begin
  { CodeEdit_DroppedFiles := trim(AFiles.Text);
    if CodeEdit_DropEnd <> emptystr then
    Extensions.RunLuaCmd(CodeEdit_DropEnd); }
end;

// Loads a custom extension page (used by Sandcat extensions)
procedure TSandcatTab.LoadExtensionPage(const html: string);
begin
  if fCustomTab = nil then
  begin
    fCustomTab := TSandUIEngine.Create(fMainPanel);
    fCustomTab.Parent :=
      TPage(fSubTabs.Pages.Objects[fSubTabs.Pages.IndexOf('extension')]);
    fCustomTab.Align := AlClient;
    fCustomTab.OnonStdOut := UIX.StdOut;
    fCustomTab.OnonStdErr := UIX.StdErr;
  end;
  SetActivePage('extension');
  fCustomTab.loadhtml(replacestr(UIX.Pages.Tab_Custom, cContent, html),
    pluginsdir);
end;

// Loads a custom toolbar. This is used by Sandcat extensions as part of custom
// tabs
procedure TSandcatTab.LoadExtensionToolbar(const html: string);
begin
  if fCustomToolbar = nil then
  begin
    Navbar.Note.Pages.Add(UID);
    fCustomToolbar := TSandUIEngine.Create(fMainPanel);
    fCustomToolbar.Parent :=
      TPage(Navbar.Note.Pages.Objects[Navbar.Note.Pages.IndexOf(UID)]);
    fCustomToolbar.Align := AlClient;
    fCustomToolbar.OnonStdOut := UIX.StdOut;
    fCustomToolbar.OnonStdErr := UIX.StdErr;
  end;
  Navbar.Note.ActivePage := UID;
  fCustomToolbar.loadhtml(replacestr(UIX.Pages.Tab_Toolbar, cContent, html),
    pluginsdir);
end;

// Updates the navigation bar to reflect the tab state being loaded/restored
procedure TSandcatTab.LoadState;
begin
  fState.LoadState(UID, GetURL);
  fSubTabs.ActivePage := fState.ActivePage;

  SetLoading(fLoading); // Reloads the state of stop/reload button
  contentarea.SetActivePage(fState.ActivePageOri);
  if fState.IsCustom then
    fSubTabs.ActivePage := fState.CustomDefaultPage;
  Navbar.IsBookmarked := fState.IsBookmarked;
  pagebar.SelectPage(fState.ActivePageOri);
  pagebar.AdjustPageStrip(fSideTree);
end;

// Sets the active page by the page name
procedure TSandcatTab.SetActivePage(const name: string);
begin
  fState.ActivePage := name;
  fSubTabs.ActivePage := name;
end;

// Called when a page starts laoding or finishes loading
procedure TSandcatTab.SetLoading(const b: boolean);
begin
  fLoading := b;
  if IsActiveTab = true then
    Navbar.isLoading := b; // Updates the nav bar
end;

// Returns the title of the tab or page.
function TSandcatTab.GetTitle: string;
begin
  if fChrome <> nil then
    Result := fChrome.title;
  if Result = emptystr then
  begin
    // Title is empty, uses the current URL as page title
    Result := GetURL;
    // If result is still empty (no URL loaded), see if a custom title set by
    // a extension is available
    if Result = emptystr then
      Result := fTitle;
  end;
end;

procedure TSandcatTab.RunLua(const lua: string);
begin
  if IsLuaWhiteListed(lua) then
    Extensions.RunLuaCmd(lua);
end;

// Returns true if the script supplied has been previously whitelisted using the
// WhiteListLua() procedure, otherwise returns false
function TSandcatTab.IsLuaWhiteListed(const script: string): boolean;
begin
  Result := fAllowedLuaScripts.IndexOf(strtohex(script)) <> -1;
end;

// Whitelists a piece of Lua code to be called from a web page (can be called by
// Sandcat extensions). This should be used with caution
// Note: if the second parameter is supplied and is false, removes the Lua code
// from the whitelist
procedure TSandcatTab.WhiteListLua(const script: string;
  const allowed: boolean = true);
var
  h: string;
  index: integer;
begin
  if script = emptystr then
    exit;
  h := strtohex(script);
  index := fAllowedLuaScripts.IndexOf(h);
  if allowed then
  begin
    if index = -1 then
      fAllowedLuaScripts.Add(h);
  end
  else
  begin
    if index <> -1 then
      fAllowedLuaScripts.Delete(index);
  end;
end;

// Evaluates some JavaScript (not fully implemented)
function TSandcatTab.EvalJavaScript(const script: string): variant;
begin
  fUserJSExecuted := true;
  InitChrome; // Initializes chrome, if not initialized before
  Result := fChrome.EvalJavaScript(script);
end;

// Executes a piece of JavaScript code (usually called by the user via some
// extension)
procedure TSandcatTab.RunJavaScript(const script: string);
begin
  RunJavaScript(script, emptystr, 0, emptystr, false);
end;

// Executes a piece of JavaScript code (on demand)
procedure TSandcatTab.RunJavaScript(const script: string;
  const scripturl: string; const startline: integer; const luacallback: string;
  const reporterrors: boolean = false);
var
  scp: string;
begin
  fUserJSExecuted := true;
  scp := script;
  InitChrome; // Initializes chrome, if not initialized before
  scp := scp + crlf + 'Sandcat.ConsoleOutput(false);';
  if luacallback <> emptystr then
  begin
    WhiteListLua(luacallback);
    scp := scp + crlf + 'Sandcat.CallWL(' + sandenc(luacallback) + ');';
  end;
  fChrome.RunJavaScript(scp, scripturl, startline, reporterrors);
end;

// Performs a web search using the selected search engine in the navigation bar
procedure TSandcatTab.DoSearch(const term: string;
  const newtab: boolean = false);
begin
  if newtab then
    tabmanager.newtab(vSearchEngine_QueryURL + term)
  else
    GoToURL(vSearchEngine_QueryURL + term);
end;

// Adds a line to the log memo in the log page
procedure TSandcatTab.LogWriteLn(const s: string);
begin
  fLog.lines.Text := fLog.lines.Text + s + crlf;
  contentarea.Console_WriteLn(s);
end;

// Writes a string to the log memo in the log page
procedure TSandcatTab.LogWrite(const s: string);
begin
  fLog.lines.Text := fLog.lines.Text + s;
  contentarea.Console_Write(s);
end;

// Handling of WM_COPYDATA messages
procedure TSandcatTab.CopyDataMessage(const msg: integer; const str: string);
begin
  if fIsClosing then
    exit;
  case (msg) of
    SCBM_MONITOR_EVAL:
      taskmonitor.Eval(str);
    SCBM_RUNJSONCMD:
      RunJSONCmd(str);
    SCBM_TASK_STOPPED:
      if fSyncWithTask then
        SetIcon('@ICON_STOP');
    SCBM_TASK_SUSPENDED:
      if fSyncWithTask then
        SetIcon('@ICON_PAUSE');
    SCBM_TASK_RESUMED:
      if fSyncWithTask then
        SetIcon('@ICON_LOADING');
    SCBM_TASK_RUNJSONCMD:
      tasks.RunJSONCmd(str);
    SCBM_TASK_SETPARAM:
      tasks.SetTaskParam_JSON(str);
    SCBM_LUA_RUN:
      Extensions.RunLuaCmd(str);
    SCBM_LOGWRITELN:
      LogWriteLn(str);
    SCBM_LOGWRITE:
      LogWrite(str);
    SCBM_LOGADD:
      fLog.lines.Add(str);
    SCBM_LOGDYNAMICREQUEST:
      fRequests.LogDynamicRequest(str);
    SCBM_LOGEXTERNALREQUEST_JSON:
      fRequests.LogRequestJSON(str);
    SCBM_XHR_LOG:
      fRequests.LogXMLHTTPRequest(str);
    SCBM_REQUEST_SEND:
      SendRequestCustom(BuildRequestFromJSON(str));
    SCBM_XHR_SEND:
      SendRequestXHR(BuildXHRFromJSON(str));
    SCBM_CONSOLE_ENDEXTERNALOUTPUT:
      JavaScriptExecutionEnd;
  end;
end;

// Adds a resource to the resource list in the resource page
procedure TSandcatTab.AddPageResource(const URL: string);
var
  r: TSandcatRequest;
begin
  r.URL := URL;
  with fResourcesLv.Items.Add do
  begin
    Caption := extracturlfilename(URL);
    SubItems.Add(URL);
    imageindex := fLiveHeaders.GetImageIndex(r);
  end;
end;

// Handling of messages originating from the Sandcat Chromium component
procedure TSandcatTab.BrowserMessage(const msg: integer; const str: string);
begin
  if fIsClosing then
    exit;
  case (msg) of
    CRM_NEWPAGERESOURCE:
      AddPageResource(str);
    CRM_JS_ALERT:
      sanddlg.ShowAlertText(str);
    CRM_JS_WRITELN:
      if fUserJSExecuted then
        LogWriteLn(str);
    CRM_JS_WRITE:
      if fUserJSExecuted then
        LogWrite(str);
    CRM_JS_RUN_WHITELISTED_LUA:
      RunLua(str);
    // do not accept JS values unless the user manually executed a JavaScript before
    CRM_JS_WRITEVALUE:
      if fUserJSExecuted then
        settings.WriteJSValue_FromJSON(str);
    CRM_XHR_LOG:
      if fUserJSExecuted then
        fRequests.LogXMLHTTPRequest(str);
    CRM_LOG_REQUEST_JSON:
      if fLogBrowserRequests then
        fRequests.LogRequestJSON(str);
    CRM_CONSOLE_ENDEXTERNALOUTPUT:
      JavaScriptExecutionEnd;
    CRM_NEWTAB:
      tabmanager.newtab(str);
    CRM_NEWTAB_INBACKGROUND:
      tabmanager.newtab(str, emptystr, false, true);
    CRM_SEARCHWITHENGINE:
      DoSearch(str);
    CRM_SEARCHWITHENGINE_INNEWTAB:
      DoSearch(str, true);
    CRM_SAVECACHEDRESOURCE:
      sanddlg.SaveResource(str, false);
    CRM_SAVECLOUDRESOURCE:
      sanddlg.SaveResource(str, true);
    CRM_BOOKMARKURL:
      settings.AddToBookmarks(GetTitle, str);
  end;
end;

// Called when the execution of a custom JavaScript ends (usually a call from
// the user)
procedure TSandcatTab.JavaScriptExecutionEnd;
begin
  if fChrome <> nil then
    fChrome.LogJavaScriptErrors := false;
  contentarea.Console_Output(false);
end;

// Handles WM_COPYDATA messages
procedure TSandcatTab.WMCopyData(var message: TMessage);
var
  pData: PCopyDataStruct;
  str: string;
begin
  message.Result := 0;
  pData := PCopyDataStruct(message.LParam);
  if (pData = nil) then
    exit;
  str := StrPas(PAnsiChar(pData^.lpData));
  CopyDataMessage(pData^.dwData, str);
  message.Result := 1;
end;

// For receiving messages from the V8 engine running in the isolated tab process
procedure TSandcatTab.V8Msg(var AMsg: TMessage);
var
  pData: PCopyDataStruct;
  str: string;
begin
  try
    case AMsg.msg of
      WM_COPYDATA:
        begin
          AMsg.Result := 0;
          pData := PCopyDataStruct(AMsg.LParam);
          if (pData = nil) then
            exit;
          str := StrPas(PAnsiChar(pData^.lpData));
          BrowserMessage(pData^.dwData, str);
          AMsg.Result := 1;
        end;
    end;
  except
  end;
end;

// For receiving WM_COPYDATA messages
procedure TSandcatTab.MainThreadMsgWindow(var AMsg: TMessage);
begin
  try
    case AMsg.msg of
      WM_COPYDATA:
        WMCopyData(AMsg);
    end;
    // Workaround for CEF windows focus related issue, when closing tab while loading
  except
  end;
end;

// Sorts items by the clicked resources page column
procedure TSandcatTab.ResourcesListviewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if Column.index = state.ResourcesLastSortedColumn then
    state.ResourcesAscending := not state.ResourcesAscending
  else
    state.ResourcesLastSortedColumn := Column.index;
  TListView(Sender).CustomSort(@Resources_SortByColumn, Column.index);
end;

// Called when a list item is clicked in the resources page, displays the
// resource (usually from the cache)
procedure TSandcatTab.ResourcesListViewClick(Sender: TObject);
begin
  if (fResourcesLv.Selected <> nil) then
    UIX.ShowResouce(fResourcesLv.Selected.SubItems[0]);
end;

// Called when a page starts loading, updates the navigation bar
procedure TSandcatTab.CrmLoadStart(Sender: TObject);
begin
  if fIsClosing then
    exit; // no need to update the UI
  Loading := true;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_LOADSTART, []);
  if fCanUpdateSource then
    fSourceInspect.setsource(emptystr);
  fstate.ProtoIcon := '@ICON_GLOBE';
  fstate.IsBookmarked := false;
  fResourcesLv.Items.Clear;
  if IsActiveTab then
  begin
    // update the nav bar only if this is not a tab loading in the background
    Navbar.ProtoIcon := fstate.ProtoIcon;
    Navbar.IsBookmarked := false;
  end;
end;

// Called when the URL of this tab changes
procedure TSandcatTab.CrmAddressChange(Sender: TObject; const URL: string);
begin
  state.URL := URL;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_URLCHANGE, [URL]);
end;

// Called when there is a new status bar message, updates the status bar text
procedure TSandcatTab.CrmStatusMessage(Sender: TObject; const value: string);
begin
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_STATUS, [value]);
end;

// Called when there is a JavaScript execution error
procedure TSandcatTab.CrmConsoleMessage(Sender: TObject;
  const message, source: string; line: integer);
begin
  Extensions.LogScriptError('JavaScript', inttostr(line), message);
  contentarea.Console_Output(false);
end;

// Called immediately after a page finishes loading.
procedure TSandcatTab.CrmLoadEnd(Sender: TObject; httpStatusCode: integer);
begin
  if fIsClosing then
    exit;
  Loading := false;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_LOADEND, []);
  // If you landed in a HTTPS URL, updates the navigation bar and adds the
  // secure icon.
  if beginswith(GetURL, 'https') then
  begin
    state.ProtoIcon := '@ICON_SECURE';
    if IsActiveTab then
      Navbar.ProtoIcon := state.ProtoIcon;
  end;
  UpdateSourceCode;
  fChrome.SetV8MsgHandle(fV8MsgHandle);
  RunUserScripts(SCBT_LOADEND);
end;

// Updates the source code in the source page
procedure TSandcatTab.UpdateSourceCode;
begin
  if fCanUpdateSource = false then
    exit;
  if fSourceManual = emptystr then // default source page update mechanism
    fChrome.getSource
  else
  begin
    // manually setting the source code in the source page
    fSourceInspect.setsource(fSourceManual);
    fSourceManual := emptystr;
  end;
end;

// Sets a new tab title
procedure TSandcatTab.SetTitle(const title: string);
begin
  fTitle := title;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_NEWTITLE, [title]);
end;

// Called when the page title changes
procedure TSandcatTab.CrmTitleChange(Sender: TObject; const title: string);
begin
  if Loading then
    settings.AddToHistory(title, GetURL);
  SetTitle(title);
end;

// Called before a popup is opened, used to open the popup window as a new tab
procedure TSandcatTab.CrmBeforePopup(Sender: TObject; var URL: string;
  out Result: boolean);
begin
  // Now handled via SCBM_NEWTAB message (to avoid weird crash with CEF3 lib)
  // sandbrowser.newtab(url);
end;

// Called when there is an error loading a page
procedure TSandcatTab.CrmLoadError(Sender: TObject; const errorCode: integer;
  const errorText, failedUrl: string);
begin
  Loading := false;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_LOADERROR, []);
  // This was not working as expected, commented out for later review:
  // It is firing during file downloads
  // if failedurl<>cHOMEURL then sanddlg.ShowAlert('Failed to load '+failedURL+'. '+errortext);
end;

// Called when the page loading stage changes, updates the navigation bar
procedure TSandcatTab.CrmLoadingStateChange(Sender: TObject;
  const isLoading, canGoBack, canGoForward: boolean);
begin
  Loading := isLoading;
  Navbar.LoadingStateChange(isLoading, canGoBack, canGoForward);
end;

// Called before starting a download
procedure TSandcatTab.CrmBeforeDownload(Sender: TObject; const id: integer;
  const suggestedName: string);
begin
  Loading := false;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_LOADEND, []);
  downloads.SetDownloadFilename(id, suggestedName);
end;

// Called when there is a status update about an active download
procedure TSandcatTab.CrmDownloadUpdated(Sender: TObject; var cancel: boolean;
  const id, state, percentcomplete: integer; const fullPath: string);
begin
  downloads.HandleUpdate(fDownloadsList, cancel, id, state, percentcomplete,
    fullPath);
end;

// Sets the tab icon. If the second parameter is supplied and is true, favicons
// will be ignored after loading a new page
procedure TSandcatTab.SetIcon(const URL: string; const force: boolean = false);
begin
  fDefaultIcon := URL;
  if force then
    fRetrieveFavIcon := false;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_LOADEND, []);
end;

// Returns the icon for the current tab (if a favicon is available, returns the
// favicon URL)
function TSandcatTab.GetIcon: string;
var
  URL: string;
begin
  Result := fDefaultIcon;
  if (fChrome <> nil) then
  begin
    URL := GetURL;
    if beginswith(URL, 'http') and (fRetrieveFavIcon = true) then
    begin
      if extracturlfilename(URL) = emptystr then
        Result := URL + cFavIconFileName
      else
        Result := replacestr(URL + ' ', '/' + ExtractURLPath(URL) + ' ',
          '/' + cFavIconFileName);
      Result := htmlescape(Result);
      Result := 'url(' + Result + ')';
      if URL = emptystr then
        Result := fDefaultIcon;
    end;
  end;
  if Loading then
    Result := '@ICON_LOADING';
end;

// Returns the current URL
function TSandcatTab.GetURL: string;
begin
  if fChrome <> nil then
    Result := Chrome.GetURL
  else
    Result := emptystr;
end;

// Loads the Chromium settings from the Sandcat configuration file
procedure TSandcatTab.LoadSettings;
begin
  if fChrome <> nil then
    fChrome.LoadSettings(settings.preferences.current,
      settings.preferences.Default);
end;

// Makes the source page highlighter adapt to the URL filename extension
procedure TSandcatTab.AdjustHighlighter(const URL: string = '');
var
  ext, aurl: string;
begin
  aurl := URL;
  if aurl = emptystr then // no URL supplied, uses the current URL
    aurl := GetURL;
  ext := lowercase(extracturlfileext(aurl));
  fSourceInspect.source.highlighter := Highlighters.GetByFileExtension(ext);
end;

// Called before loading a URL, updates the user interface and resets some
// variables
procedure TSandcatTab.BeforeLoad(const URL: string);
begin
  if URL = emptystr then
    exit;
  fUserJSExecuted := false;
  fRetrieveFavIcon := true;
  fSourceManual := emptystr;
  fSourceInspect.source.highlighter := Highlighters.WebHtml;
  state.URL := URL;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_URLCHANGE, [URL]);
  if beginswith(lowercase(URL), 'http') then
  begin
    Loading := true;
    if Assigned(OnMessage) then
      OnMessage(self, SCBT_GOTOURL, []);
  end;
  Chrome.visible := true;
  AdjustHighlighter(URL);
end;

// Sends (and optionally loads) a HTTP request
procedure TSandcatTab.SendRequest(const method, URL, postdata: string;
  const load: boolean = false);
begin
  InitChrome;
  if load then
    BeforeLoad(URL);
  fChrome.SendRequest(buildrequest(method, URL, postdata), load);
end;

// Sends (and optionally loads) a custom HTTP request
procedure TSandcatTab.SendRequestCustom(req: TCatChromiumRequest;
  load: boolean = false);
begin
  InitChrome;
  // If no URL is provided, uses current tab url as the request URL
  if req.URL = emptystr then
    req.URL := GetURL;
  if load then
    BeforeLoad(req.URL);
  fChrome.SendRequest(req, load);
end;

// Sends a XMLHTTPRequest
procedure TSandcatTab.SendRequestXHR(const req: TCatChromiumXHR);
begin
  SendXHR(req, self);
end;

// Loads a URL. If a source parameter is supplied, loads the page from the
// source string
procedure TSandcatTab.GoToURL(const URL: string; const source: string = '');
begin
  if (URL <> emptystr) and (URL <> cHOMEURL) then
  begin
    InitChrome;
    BeforeLoad(URL);
    if source = emptystr then
      fChrome.load(URL) // default load mechanism
    else
    begin
      fChrome.LoadFromString(source, URL);
      fSourceManual := source;
    end;
  end;
end;

// Takes a screenshot of the page and returns the temporary screenshot filename
function TSandcatTab.GetScreenshot: string;
const
  hidesbscript = 'document.documentElement.style.overflow = ''%s'';';
begin
  if Chrome.IsFrameNil then
    exit;
  OnMessage(self, SCBT_GETSCREENSHOT, []);
  SetActivePage('browser');
  fChrome.RunJavaScript(Format(hidesbscript, ['hidden'])); // hide the scrollbar
  catdelay(250);
  Result := CaptureChromeBitmap(self);
  fChrome.RunJavaScript(Format(hidesbscript, ['scroll'])); // restore the SB
end;

// Creates the Chromium component (if not already created)
procedure TSandcatTab.InitChrome;
begin
  if fChrome = nil then
  begin
    fChrome := TCatChromium.Create(self);
    fChrome.Parent := fBrowserPanel;
    fChrome.visible := false;
    fChrome.Align := AlClient;
    // OnAfterSetSource:
    // Called by the Sandcat Chromium component after the source code has been
    // accessed. This is using a callback for getting the source. There is no
    // other way to do this using the current CEF3 release AFAIK
    fChrome.OnAfterSetSource := fSourceInspect.setsource;
    fChrome.OnBrowserMessage := BrowserMessage;
    fChrome.OnTitleChange := CrmTitleChange;
    fChrome.OnLoadEnd := CrmLoadEnd;
    fChrome.OnLoadError := CrmLoadError;
    fChrome.OnLoadStart := CrmLoadStart;
    fChrome.OnAddressChange := CrmAddressChange;
    fChrome.OnStatusMessage := CrmStatusMessage;
    fChrome.OnLoadingStateChange := CrmLoadingStateChange;
    fChrome.OnBeforePopup := CrmBeforePopup;
    fChrome.OnBeforeDownload := CrmBeforeDownload;
    fChrome.OnDownloadUpdated := CrmDownloadUpdated;
    fChrome.OnConsoleMessage := CrmConsoleMessage;
    // currently not needed:
    // fChrome.OnBeforeContextMenu:=crmBeforeContextMenu;
    // fChrome.OnGetAuthCredentials:=crmAuthCredentials;
    // fChrome.OnJsdialog:=crmJsdialog;
    // fChrome.OnProcessMessageReceived:=crmProcessMessageReceived;
    LoadSettings;
    // Sends the v8 message handle of this tab to the Chromium V8 extension in
    // the tab process
    fChrome.SetV8MsgHandle(fV8MsgHandle);
  end
  else // already created, resend the v8 handle
    fChrome.SetV8MsgHandle(fV8MsgHandle);
end;

// Called before freeing a tab, if there is any active download, asks the user
// if he/she really wants to proceed.
// Returns true if the tab manager is allowed to close the tab
function TSandcatTab.Close(const silent: boolean = false): boolean;
begin
  Result := true;
  if silent = false then
  begin
    if fDownloadsList.Count <> 0 then
    begin
      // Asks the user if he/she wants to close the tab, canceling active downloads
      Result := AskYN('Closing this tab will cancel a download. Continue?');
      if Result = true then
        downloads.CancelList(fDownloadsList);
    end;
  end;
  if Result = true then
  begin
    fIsClosing := true;
    fRequests.tabwillclose;
    if fChrome <> nil then
      fChrome.InterceptRequests := false;
  end;
end;

// Runs simple commands in the form of a JSON object (used by Sandcat tasks
// that run in an isolated process)

type
  TJSONCmds = (cmd_runtbtis, cmd_setaffecteditems, cmd_seticon,
    cmd_syncwithtask);

procedure TSandcatTab.RunJSONCmd(const json: string);
var
  j: TSandJSON;
  cmd: string;
begin
  j := TSandJSON.Create(json);
  cmd := lowercase(j['cmd']);
  case TJSONCmds(GetEnumValue(TypeInfo(TJSONCmds), 'cmd_' + cmd)) of
    cmd_runtbtis:
      if fCustomToolbar <> nil then
        fCustomToolbar.Eval(j['s']);
    cmd_setaffecteditems:
      SideTree_LoadAffectedScripts(j['s']);
    cmd_seticon:
      SetIcon(j['s']);
    cmd_syncwithtask:
      fSyncWithTask := true;
  end;
  j.Free;
end;

// Runs a user script (JavaScript or Lua)
procedure TSandcatTab.RunUserScript(var script: string; const lang: integer;
  const runonce: boolean = false);
begin
  if script = emptystr then
    exit;
  case lang of
    1:
      RunJavaScript(script);
    2:
      Extensions.RunLuaCmd(script);
  end;
  if runonce = true then
    script := emptystr;
end;

// Runs user scripts (if any) for a specific tab event
// TODO: This needs to be re-implemented
procedure TSandcatTab.RunUserScripts(const event: integer);
const
  cJS = 1;
  cLua = 2;
begin
  case event of
    SCBT_LOADEND:
      begin
        RunUserScript(userscript.JS_Tab_LoadEnd, cJS);
        RunUserScript(UserTabScript.JS_LoadEnd, cJS);
        RunUserScript(UserTabScript.JS_LoadEnd_RunOnce, cJS, true);
        RunUserScript(UserTabScript.Lua_LoadEnd_RunOnce, cLua, true);
      end;
  end;
end;

// Creates a side tree that can be used by extensions (invisible by default)
procedure TSandcatTab.CreateSideTree;
begin
  fSideTree := TTreeView.Create(self);
  fSideTree.Parent := self;
  fSideTree.Align := AlLeft;
  fSideTree.Images := SandBrowser.LiveImages;
  fSideTree.ReadOnly := true;
  fSideTree.HideSelection := false;
  fSideTree.Width := 300;
  fSideTree.visible := false;
  fSideTree.OnChange := SideTreeChange;
  fSideTree.OnDblClick := SideTreeDblClick;
  fTreeSplitter := TSplitter.Create(self);
  fTreeSplitter.Parent := self;
  fTreeSplitter.Width := 1;
  fTreeSplitter.visible := false;
  fTreeSplitter.Color := clBtnShadow;
end;

// Creates the main panel
procedure TSandcatTab.CreateMainPanel;
begin
  fMainPanel := TPanel.Create(self);
  fMainPanel.Parent := self;
  ConfigPanel(fMainPanel, AlClient);
  fSubTabs := TNoteBook.Create(fMainPanel);
  fSubTabs.Parent := fMainPanel;
  fSubTabs.Color := clWindow;
  fSubTabs.Align := AlClient;
  fSubTabs.Pages.Add('source');
  fSubTabs.Pages.Add('browser');
  fSubTabs.Pages.Add('log');
  fSubTabs.Pages.Add('extension');
  fSubTabs.Pages.Add('resources');
  fSubTabs.ActivePage := 'browser'; // default page
  // Creates the browser page
  fBrowserPanel := TCanvasPanel.Create(fMainPanel);
  fBrowserPanel.Parent :=
    TPage(fSubTabs.Pages.Objects[fSubTabs.Pages.IndexOf('browser')]);
  ConfigPanel(fBrowserPanel, AlClient);
  // Creates the source page
  fSourceInspect := TSyCodeInspector.Create(nil);
  fSourceInspect.Parent :=
    TPage(fSubTabs.Pages.Objects[fSubTabs.Pages.IndexOf('source')]);
  fSourceInspect.Align := AlClient;
  fSourceInspect.SetImageList(SandBrowser.LiveImages);
  ConfigSynEdit(fSourceInspect.source);
  // Creates the log page
  fLog := TMemo.Create(fBrowserPanel);
  fLog.Parent := TPage(fSubTabs.Pages.Objects[fSubTabs.Pages.IndexOf('log')]);
  fLog.Align := AlClient;
  fLog.ReadOnly := true;
  fLog.Color := clBtnFace;
  fLog.ScrollBars := ssBoth;
  // Creates the resources page
  fResourcesLv := TListView.Create(fBrowserPanel);
  fResourcesLv.Parent :=
    TPage(fSubTabs.Pages.Objects[fSubTabs.Pages.IndexOf('resources')]);
  fResourcesLv.Align := AlClient;
  fResourcesLv.SmallImages := SandBrowser.LiveImages;
  fResourcesLv.ReadOnly := true;
  fResourcesLv.DoubleBuffered := true;
  fResourcesLv.ViewStyle := vsReport;
  fResourcesLv.RowSelect := true;
  fResourcesLv.HideSelection := false;
  fResourcesLv.OnDblClick := ResourcesListViewClick;
  fResourcesLv.OnColumnClick := ResourcesListviewColumnClick;
  fResourcesLv.SortType := stBoth;
  with fResourcesLv.Columns.Add do
  begin
    Caption := 'Name';
    Width := 200;
  end;
  with fResourcesLv.Columns.Add do
  begin
    Caption := 'URL';
    autosize := true;
  end;
end;

// Creates the live headers panel and associated components
procedure TSandcatTab.CreateLiveHeaders;
begin
  fLiveHeaders := TLiveHeaders.Create(fMainPanel);
  fLiveHeaders.Parent := fMainPanel;
  fLiveHeaders.Align := AlClient;
  fCache := TSandCache.Create;
  fCache.new(GetSandcatDir(SCDIR_HEADERS) + 't_' + inttostr(fMsgHandle));
  fCache.MakeTemporary;
  fRequests := TSandcatRequests.Create(self, fMsgHandle);
  fRequests.headers := fLiveHeaders;
  fRequests.Cache := fCache;
end;

// Creates the tab and all its sub components
constructor TSandcatTab.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Debug('create.begin');
  SandcatBrowserTab := self;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Align := AlClient;
  Color := clWindow;
  fMsgHandle :=
{$IF CompilerVersion >= 23}System.{$IFEND}Classes.AllocateHWnd
    (MainThreadMsgWindow);
  fV8MsgHandle :=
{$IF CompilerVersion >= 23}System.{$IFEND}Classes.AllocateHWnd(V8Msg);
  fState := TTabState.Create;
  fDefaultIcon := '@ICON_EMPTY';
  fIsClosing := false;
  fLoading := false;
  fUserJSExecuted := false;
  fLogBrowserRequests := true;
  fRetrieveFavIcon := true;
  fCanUpdateSource := true;
  fSyncWithTask := false;
  fUserData := TSandJSON.Create;
  fDownloadsList := TStringList.Create;
  fAllowedLuaScripts := TStringList.Create;
  CreateMainPanel;
  CreateLiveHeaders;
  CreateSideTree;
  fSourceInspect.source.highlighter := Highlighters.WebHtml;
  Debug('create.end');
end;

// Destroys the tab, freeing the Chromium component, custom toolbars and content
// (if any)
destructor TSandcatTab.Destroy;
begin
  Debug('destroy:' + UID);
{$IF CompilerVersion >= 23}System.{$IFEND}Classes.DeallocateHWnd(fMsgHandle);
{$IF CompilerVersion >= 23}System.{$IFEND}Classes.DeallocateHWnd(fV8MsgHandle);
  if fCustomTab <> nil then
    fCustomTab.Free; // Free Sciter engine
  if fCustomToolbar <> nil then
    fCustomToolbar.Free; // Free Sciter engine
  OnMessage := nil;
  fCache.Free;
  fRequests.Free;
  fSideTree.OnChange := nil;
  fSideTree.Free;
  fTreeSplitter.Free;
  if fChrome <> nil then
    fChrome.Free;
  Debug('destroy.chrome.end:' + UID);
  fDownloadsList.Free;
  fLog.Free;
  fSourceInspect.Free;
  fLiveHeaders.Free;
  fResourcesLv.Free;
  fBrowserPanel.Free;
  fSubTabs.Free;
  fAllowedLuaScripts.Free;
  fUserData.Free;
  fState.Free;
  Debug('destroy.end:' + UID);
  inherited;
end;

// ------------------------------------------------------------------------//
// TTabState                                                               //
// ------------------------------------------------------------------------//

// Loads the default/initial state of the tab
procedure TTabState.LoadDefault;
begin
  IsCustom := false;
  IsBookmarked := false;
  ShowNavBar := true;
  ShowTabsStrip := true;
  HasConsole := false;
  HasCustomToolbar := false;
  ActivePage := 'default';
  ActivePageOri := 'browser';
  ProtoIcon := '@ICON_BLANK';
  URL := emptystr;
end;

procedure TTabState.LoadState(const TabID, CurrentURL: string);
begin
  if HasCustomToolbar then
    Navbar.Note.ActivePage := TabID
  else
    Navbar.Note.ActivePage := 'default';
  if ShowNavBar = false then
    Navbar.Height := 0
  else
    Navbar.Height := Navbar.DefaultHeight;
  pagebar.stripvisible := ShowTabsStrip;
  if URL <> emptystr then
    Navbar.URL := URL
  else
  begin
    Navbar.FocusURL;
    Navbar.URL := CurrentURL;
  end;
  Navbar.ProtoIcon := ProtoIcon;
end;

procedure TTabState.SaveState;
begin
  URL := Navbar.URL;
end;

constructor TTabState.Create;
begin
  inherited Create;
  LoadDefault;
end;

destructor TTabState.Destroy;
begin
  inherited Destroy;
end;

end.
