unit uTab;
{
  Sandcat Browser Tab component
  Copyright (c) 2011-2017, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.

  Important Notes:
  Chromium is on "standby" through TCatChromiumStandBy. By directly accessing
  the Browser.c property the Chromium browser gets created and ready to use.

}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  Vcl.StdCtrls, Vcl.ComCtrls, Vcl.Menus, System.TypInfo,
{$ELSE}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, ComCtrls, Menus, TypInfo,
{$ENDIF}
  CatUI, uUIComponents, CatConsole, CatChromium, CatChromiumLib, CatChromiumSB,
  uRequests, SynUnicode, uLiveHeaders, uCodeInspect, uTabResources,
  uTabResponse, uExtensions, uZones, CatMsg, CatPanels;

type // Used for restoring the state of a tab when switching tabs
  TTabState = class
  private
  public
    ActivePage: string;
    CustomDefaultPage: string;
    HasConsole: boolean;
    HasCustomToolbar: boolean;
    IsCustom: boolean;
    IsBookmarked: boolean;
    ProtoIcon: string;
    ShowNavBar: boolean;
    ShowTabsStrip: boolean;
    StatusBarText: string;
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
    fBrowser: TCatChromiumStandBy;
    fBrowserPanel: TCanvasPanel;
    fCache: TSandCache;
    fCanUpdateSource: boolean;
    fCustomTab: TSandUIEngine;
    fCustomToolbar: TSandUIEngine;
    fDefaultIcon: string;
    fDownloadsList: TStringList;
    fIsClosing: boolean;
    fLastConsoleLogMessage: string;
    fLuaOnLog: TSandJSON;
    fLiveHeaders: TLiveHeaders;
    fLoading: boolean;
    fLog: TSandLogMemo;
    fLogBrowserRequests: boolean;
    fMainPanel: TPanel;
    fMsg: TCatMsg;
    fMsgV8: TCatMsg;
    fNumber: integer; // unique tab number
    fOnMessage: TSandcatTabOnMessage;
    fRequests: TSandcatRequests;
    fResources: TTabResourceList;
    fResponse: TTabResponseView;
    fRetrieveFavIcon: boolean;
    fSideBar: TSandcatSidebar;
    fSyncWithTask: boolean;
    fSourceInspect: TSyCodeInspector;
    fSourceManual: string;
    fSubTabs: TNoteBook;
    fState: TTabState;
    fTitle: string;
    fTreeSplitter: TSplitter;
    fUID: string; // unique tab ID
    fUseLuaOnLog: boolean;
    fUserData: TSandJSON;
    fUserTag: string;
    function GetIcon: string;
    function GetSitePrefsFile: string;
    function GetStatusBarText: string;
    function GetTitle: string;
    procedure BrowserMessage(const msg: integer; const str: string);
    procedure CopyDataMessage(const msg: integer; const str: string);
    // procedure CodeEditDropFiles(Sender: TObject; X, Y: integer;
    // AFiles: TUnicodeStrings);
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
    procedure Debug(const s: string);
    procedure InitChrome(const crm: TCatChromium);
    procedure LogCustomScriptError(const JSON: string);
    procedure RunJSONCmd(const JSON: string);
    procedure RunUserScript(var script: string; const lang: TUserScriptLanguage;
      const runonce: boolean = false);
    procedure RunUserScripts(const event: integer);
    procedure SetLoading(const b: boolean);
    procedure SetStatusBarText(const s: string);
    procedure UpdateSourceCode;
    procedure UpdateV8Handle;
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
    procedure LoadCachedURL(const URL: string);
    procedure LoadSettings;
    procedure LoadState;
    procedure LoadExtensionPage(const html: string);
    procedure LoadExtensionToolbar(const html: string);
    procedure LoadSourceFile(const filename: string);
    procedure DoSearch(const term: string);
    procedure RunLuaOnLog(const msg, lua: string);
    procedure RunJavaScript(const script: string; const aURL: string = '';
      const StartLine: integer = 0); overload;
    procedure RunJavaScript(const script: TCatCustomJSCall); overload;
    procedure SendRequest(const method, URL, postdata: string;
      const load: boolean = false);
    procedure SendRequestCustom(req: TCatChromiumRequest;
      load: boolean = false);
    procedure SetActivePage(const name: string);
    procedure SetIcon(const URL: string; const force: boolean = false);
    procedure SetTitle(const title: string);
    procedure ShowRequest(const filename: string);
    procedure ShowSideTree(const visible: boolean);
    procedure ViewDevTools;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // properties
    property Browser: TCatChromiumStandBy read fBrowser;
    property BrowserPanel: TCanvasPanel read fBrowserPanel;
    property Cache: TSandCache read fCache;
    property CanUpdateSource: boolean read fCanUpdateSource
      write fCanUpdateSource;
    property CustomTab: TSandUIEngine read fCustomTab;
    property CustomToolbar: TSandUIEngine read fCustomToolbar;
    property Icon: string read GetIcon;
    property IsClosing: boolean read fIsClosing;
    property LastConsoleLogMessage: string read fLastConsoleLogMessage;
    property LiveHeaders: TLiveHeaders read fLiveHeaders;
    property Loading: boolean read fLoading write SetLoading;
    property Log: TSandLogMemo read fLog;
    property LogBrowserRequests: boolean read fLogBrowserRequests
      write fLogBrowserRequests;
    property OnMessage: TSandcatTabOnMessage read fOnMessage write fOnMessage;
    property msg: TCatMsg read fMsg;
    property Number: integer read fNumber write fNumber;
    property Requests: TSandcatRequests read fRequests;
    property Resources: TTabResourceList read fResources;
    property Response: TTabResponseView read fResponse;
    property SideBar: TSandcatSidebar read fSideBar;
    property SitePrefsFile: string read GetSitePrefsFile;
    property SourceInspect: TSyCodeInspector read fSourceInspect;
    property state: TTabState read fState;
    property StatusBarText: string read GetStatusBarText write SetStatusBarText;
    property SubTabs: TNoteBook read fSubTabs;
    property title: string read GetTitle;
    property UID: string read fUID write fUID;
    property UserData: TSandJSON read fUserData;
    property UserTag: string read fUserTag write fUserTag;
  end;

const
  // the default page 'browser' must be the last one in this array
  cTabSubPageNames: array [1 .. 6] of string = ('source', 'log', 'extension',
    'resources', 'response', 'browser');

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
  SCBM_REQUEST_SEND = 17;
  SCBM_RUNJSONCMD = 18;
  SCBM_TASK_STOPPED = 19;
  SCBM_TASK_SUSPENDED = 20;
  SCBM_TASK_RESUMED = 21;
  SCBM_LOGCUSTOMSCRIPTERROR = 22;

implementation

uses
  uMain, uConst, uMisc, uTaskMan, uSettings, uTabV8, CatStrings, CatHTTP,
  CatUtils, CatFiles, LAPI_Task, LAPI_Browser, LAPI_CEF;

var
  SandcatBrowserTab: TSandcatTab;

procedure TSandcatTab.Debug(const s: string);
begin
  uMain.Debug(s, 'Tab');
end;

// Returns the filename of a site preferences file
function TSandcatTab.GetSitePrefsFile: string;
begin
  Settings.GetSitePrefsFilename(GetURL);
end;

// Returns true if this is the currently active tab, false if otherwise
function TSandcatTab.IsActiveTab: boolean;
begin
  Result := UID = tabmanager.ActiveTabID;
end;

// Loads a URL from the browser cache
procedure TSandcatTab.LoadCachedURL(const URL: string);
begin
  SetActivePage('response');
  fResponse.LoadCachedURL(URL);
end;

// Loads a source code from a file in the source page
procedure TSandcatTab.LoadSourceFile(const filename: string);
begin
  fSourceInspect.LoadFromFile(filename);
  // Adopts highlighter based on the filename extension
  fSourceInspect.source.highlighter := Highlighters.GetByFileExtension
    (extractfileext(filename));
end;

procedure TSandcatTab.ShowRequest(const filename: string);
var
  r: TSandcatRequestDetails;
begin
  SetActivePage('response');
  if fRequests.requestexists(filename) then
    fResponse.LoadFromRequest(fRequests.GetRequest(filename));
end;

// Displays the side tree (used by Sandcat extensions)
procedure TSandcatTab.ShowSideTree(const visible: boolean);
begin
  fSideBar.visible := visible;
  pagebar.AdjustPageStrip(fSideBar);
  fTreeSplitter.visible := visible;
  fTreeSplitter.Left := fSideBar.Left + 1;
end;

{ // Handles file drops in the code editor
  procedure TSandcatTab.CodeEditDropFiles(Sender: TObject; X, Y: integer;
  AFiles: TUnicodeStrings);
  begin
  CodeEdit_DroppedFiles := trim(AFiles.Text);
  if CodeEdit_DropEnd <> emptystr then
  Extensions.RunLuaCmd(CodeEdit_DropEnd);
  end; }

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
  // debug('seting active page:'+fState.ActivePageName);
  tabmanager.activetab.SetActivePage(fState.ActivePage);
  // debug('active page set');
  if fState.IsCustom then
    fSubTabs.ActivePage := fState.CustomDefaultPage;
  Navbar.IsBookmarked := fState.IsBookmarked;
  pagebar.SelectPage(fState.ActivePage);
  pagebar.AdjustPageStrip(fSideBar);
end;

// Sets the active page by the page name
procedure TSandcatTab.SetActivePage(const name: string);
var
  nl: string;
begin
  nl := lowercase(name);
  fState.ActivePage := nl;
  fSubTabs.ActivePage := nl;
  pagebar.SelectPage(nl);
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
  if fBrowser.Available then
    Result := fBrowser.c.title;
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

// Associates a piece of Lua code to be executed when a specific log message is
// received through JS via console.log()
procedure TSandcatTab.RunLuaOnLog(const msg, lua: string);
begin
  Debug('runluaonlog:' + msg + ';' + lua);
  fUseLuaOnLog := true;
  fLuaOnLog[msg] := lua;
end;

// Evaluates some JavaScript (not fully implemented)
function TSandcatTab.EvalJavaScript(const script: string): variant;
begin
  Result := fBrowser.c.EvalJavaScript(script);
end;

// Executes a piece of JavaScript code (usually called by the user via some
// extension)
procedure TSandcatTab.RunJavaScript(const script: string;
  const aURL: string = ''; const StartLine: integer = 0);
var
  s: TCatCustomJSCall;
begin
  s.Code := script;
  s.URL := aURL;
  s.StartLine := StartLine;
  s.silent := false;
  RunJavaScript(s);
end;

// Executes a piece of JavaScript code
procedure TSandcatTab.RunJavaScript(const script: TCatCustomJSCall);
begin
  fBrowser.c.RunJavaScript(script);
end;

// Performs a web search using the selected search engine in the navigation bar
procedure TSandcatTab.DoSearch(const term: string);
begin
  GoToURL(vSearchEngine_QueryURL + term);
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
      fLog.WriteLn(str);
    SCBM_LOGWRITE:
      fLog.Write(str);
    SCBM_LOGADD:
      fLog.lines.Add(str);
    SCBM_LOGCUSTOMSCRIPTERROR:
      LogCustomScriptError(str);
    SCBM_LOGDYNAMICREQUEST:
      fRequests.LogDynamicRequest(str);
    SCBM_LOGEXTERNALREQUEST_JSON:
      fRequests.LogRequest(BuildRequestDetailsFromJSON(str));
    SCBM_XHR_LOG:
      fRequests.LogXMLHTTPRequest(str);
    SCBM_REQUEST_SEND:
      SendRequestCustom(BuildRequestFromJSON(str));
    SCBM_CONSOLE_ENDEXTERNALOUTPUT:
      contentarea.Console_Output(false);
  end;
end;

// Handling of messages originating from the Chromium component
// Can also originate from the V8 engine running in the isolated tab process
procedure TSandcatTab.BrowserMessage(const msg: integer; const str: string);
begin
  if fIsClosing then
    exit;
  case (msg) of
    CRM_NEWPAGERESOURCE:
      Resources.AddPageResource(str, fLiveHeaders.GetImageIndexForURL(str));
    CRM_JS_ALERT:
      sanddlg.ShowAlertText(str);
    CRM_LOG_REQUEST_JSON:
      if fLogBrowserRequests then
        fRequests.LogRequest(BuildRequestDetailsFromJSON(str));
    CRM_NEWTAB:
      tabmanager.newtab(str);
    CRM_NEWTAB_INBACKGROUND:
      tabmanager.newtab(str, emptystr, false, true);
    CRM_SEARCHWITHENGINE:
      DoSearch(str);
    CRM_SEARCHWITHENGINE_INNEWTAB:
      tabmanager.newtab_search(str);
    CRM_SAVECACHEDRESOURCE:
      sanddlg.SaveResource(str, false);
    CRM_SAVECLOUDRESOURCE:
      sanddlg.SaveResource(str, true);
    CRM_BOOKMARKURL:
      Settings.AddToBookmarks(GetTitle, str);
  end;
end;

// Sends the v8 message handle of this tab to the Chromium V8 extension in
// the tab process
procedure TSandcatTab.UpdateV8Handle;
begin
  fBrowser.c.SetV8MsgHandle(fMsgV8.msgHandle);
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
  fState.ProtoIcon := '@ICON_GLOBE';
  fState.IsBookmarked := false;
  fResources.Lv.Items.Clear;
  if IsActiveTab then
  begin
    // update the nav bar only if this is not a tab loading in the background
    Navbar.ProtoIcon := fState.ProtoIcon;
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

// Returns the current status bar text
function TSandcatTab.GetStatusBarText: string;
begin
  Result := state.StatusBarText;
end;

// Saves the status bar text. Update it only if this is the active tab
procedure TSandcatTab.SetStatusBarText(const s: string);
begin
  state.StatusBarText := s;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_STATUS, [s]);
end;

// Called when there is a new status bar message, updates the status bar text
procedure TSandcatTab.CrmStatusMessage(Sender: TObject; const value: string);
begin
  StatusBarText := value;
end;

// Used by Sandcat tasks to log a Lua error during execution
procedure TSandcatTab.LogCustomScriptError(const JSON: string);
var
  j: TSandJSON;
begin
  j := TSandJSON.Create(JSON);
  Extensions.LogScriptError(j['sender'], j['line'], j['msg'], false);
  j.Free;
end;

// Called when there is a JavaScript execution error or when console.log is called
procedure TSandcatTab.CrmConsoleMessage(Sender: TObject;
  const message, source: string; line: integer);
var
  storemsg: boolean;
begin
  Debug('Console message:' + message);
  storemsg := true;
  if (fUseLuaOnLog = true) then
  begin
    if fLuaOnLog.GetValue(message, emptystr) <> emptystr then
    begin
      storemsg := false;
      fUseLuaOnLog := false;
      Extensions.RunLuaCmd(fLuaOnLog.GetValue(message, emptystr));
    end;
  end
  else
    Extensions.LogScriptError('JavaScript', IntToStr(line), message);
  if storemsg then
    fLastConsoleLogMessage := message;
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
  UpdateV8Handle;
  RunUserScripts(SCBT_LOADEND);
end;

// Updates the source code in the source page
// Uses the default source page update mechanism if fSourceManual is empty
// Otherwise, manually sets the source code from fSourceManual
procedure TSandcatTab.UpdateSourceCode;
begin
  if fCanUpdateSource = false then
    exit;
  if fSourceManual = emptystr then
    fBrowser.c.getSource()
  else
    fSourceInspect.setsourcevar(fSourceManual, true);
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
    Settings.AddToHistory(title, GetURL);
  SetTitle(title);
end;

// Called before a popup is opened, used to open the popup window as a new tab
procedure TSandcatTab.CrmBeforePopup(Sender: TObject; var URL: string;
  out Result: boolean);
begin
  // Now handled via SCBM_NEWTAB message
  // sandbrowser.newtab(url);
end;

// Called when there is an error loading a page
procedure TSandcatTab.CrmLoadError(Sender: TObject; const errorCode: integer;
  const errorText, failedUrl: string);
  procedure ShowError(msg: string);
  begin
    if errorText <> emptystr then
      msg := Format('%s (%s)', [msg, errorText]);
    sanddlg.ShowAlert('Failed to load ' + failedUrl + '. ' + msg);
  end;

begin
  Loading := false;
  if Assigned(OnMessage) then
    OnMessage(self, SCBT_LOADERROR, []);
  if failedUrl = cURL_HOME then
    exit;
  // showmessage(inttostr(errorCode));
  case errorCode of
    - 105:
      ShowError('The host name could not be resolved.');
    -302:
      ShowError('The scheme of the URL is unknown.');
  end;
end;

// Called when the page loading stage changes, updates the navigation bar
procedure TSandcatTab.CrmLoadingStateChange(Sender: TObject;
  const isLoading, canGoBack, canGoForward: boolean);
begin
  Loading := isLoading;
  Navbar.LoadingStateChange(isLoading, canGoBack, canGoForward);
end;

// Called before starting a file download
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
  if (fBrowser.Available) then
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
  if fBrowser.Available then
    Result := fBrowser.c.GetURL
  else
    Result := emptystr;
end;

// Loads the Chromium settings from the Sandcat configuration file
procedure TSandcatTab.LoadSettings;
begin
  Debug('Loading tab Chromium settings...');
  if fBrowser.Available then // ToDo: check if  fBrowser.Available needed here
    fBrowser.c.LoadSettings(Settings.preferences.current,
      Settings.preferences.Default);
end;

// Makes the source page highlighter adapt to the URL filename extension
procedure TSandcatTab.AdjustHighlighter(const URL: string = '');
var
  ext, aURL: string;
begin
  aURL := URL;
  if aURL = emptystr then // no URL supplied, uses the current URL
    aURL := GetURL;
  ext := lowercase(extracturlfileext(aURL));
  fSourceInspect.source.highlighter := Highlighters.GetByFileExtension(ext);
end;

// Called before loading a URL, updates the user interface and resets some
// variables
procedure TSandcatTab.BeforeLoad(const URL: string);
begin
  if URL = emptystr then
    exit;
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
  Browser.c.visible := true;
  AdjustHighlighter(URL);
end;

// Sends (and optionally loads) a HTTP request
procedure TSandcatTab.SendRequest(const method, URL, postdata: string;
  const load: boolean = false);
begin
  if load then
    BeforeLoad(URL);
  Browser.c.SendRequest(buildrequest(method, URL, postdata), load);
end;

// Sends (and optionally loads) a custom HTTP request
procedure TSandcatTab.SendRequestCustom(req: TCatChromiumRequest;
  load: boolean = false);
begin
  // If no URL is provided, uses current tab url as the request URL
  if req.URL = emptystr then
    req.URL := GetURL;
  if load then
    BeforeLoad(req.URL);
  fBrowser.c.SendRequest(req, load);
end;

// Loads a URL. If a source parameter is supplied, loads the page from the
// source string
procedure TSandcatTab.GoToURL(const URL: string; const source: string = '');
begin
  Debug('gotourl:' + URL);
  if (URL <> emptystr) and (URL <> cURL_HOME) then
  begin
    BeforeLoad(URL);
    if source = emptystr then
      fBrowser.c.load(URL) // default load mechanism
    else
    begin
      fBrowser.c.LoadFromString(source, URL);
      fSourceManual := source;
    end;
  end;
end;

// Takes a screenshot of the page and returns the temporary screenshot filename
function TSandcatTab.GetScreenshot: string;
const
  hidesbscript = 'document.documentElement.style.overflow = ''%s'';';
begin
  if fBrowser.c.IsFrameNil then
    exit;
  OnMessage(self, SCBT_GETSCREENSHOT, []);
  SetActivePage('browser');
  fBrowser.c.RunJavaScript(Format(hidesbscript, ['hidden']));
  // hide the scrollbar
  catdelay(250);
  Result := CaptureChromeBitmap(self);
  fBrowser.c.RunJavaScript(Format(hidesbscript, ['scroll'])); // restore the SB
end;

// Called when the Chromium component is created
procedure TSandcatTab.InitChrome(const crm: TCatChromium);
begin
  Debug('Chromium initilized.');
  crm.visible := false;
  // OnAfterSetSource:
  // Called by the Sandcat Chromium component after the source code has been
  // accessed. This is using a callback for getting the source. There is no
  // other way to do this using the current CEF3 release AFAIK
  crm.OnAfterSetSource := fSourceInspect.setsource;
  crm.OnBrowserMessage := BrowserMessage;
  crm.OnTitleChange := CrmTitleChange;
  crm.OnLoadEnd := CrmLoadEnd;
  crm.OnLoadError := CrmLoadError;
  crm.OnLoadStart := CrmLoadStart;
  crm.OnAddressChange := CrmAddressChange;
  crm.OnStatusMessage := CrmStatusMessage;
  crm.OnLoadingStateChange := CrmLoadingStateChange;
  crm.OnBeforePopup := CrmBeforePopup;
  crm.OnBeforeDownload := CrmBeforeDownload;
  crm.OnDownloadUpdated := CrmDownloadUpdated;
  crm.OnConsoleMessage := CrmConsoleMessage;
  // currently not needed:
  // fChrome.OnBeforeContextMenu:=crmBeforeContextMenu;
  // fChrome.OnGetAuthCredentials:=crmAuthCredentials;
  // fChrome.OnJsdialog:=crmJsdialog;
  // fChrome.OnProcessMessageReceived:=crmProcessMessageReceived;
  LoadSettings;
  UpdateV8Handle; // send or resend the v8 handle
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
    fSideBar.CanExecLua := false;
    fRequests.tabwillclose;
    if fBrowser.Available then
      fBrowser.c.InterceptRequests := false;
  end;
end;

type
  TJSONCmds = (cmd_settreeurls, cmd_resaddcustomitem, cmd_runtbtis,
    cmd_setaffecteditems, cmd_seticon, cmd_setstatus, cmd_syncwithtask);

  // Runs simple commands in the form of a JSON object (used by Sandcat tasks
  // that run in an isolated process)
procedure TSandcatTab.RunJSONCmd(const JSON: string);
var
  j: TSandJSON;
  cmd, str: string;
begin
  j := TSandJSON.Create(JSON);
  cmd := lowercase(j['cmd']);
  str := j['s'];
  Debug('received JSON cmd:' + cmd + ' with content:' + str);
  case TJSONCmds(GetEnumValue(TypeInfo(TJSONCmds), 'cmd_' + cmd)) of
    cmd_resaddcustomitem:
      fResources.AddPageResourceCustom(str);
    cmd_runtbtis:
      if fCustomToolbar <> nil then
        fCustomToolbar.Eval(str);
    cmd_setaffecteditems:
      SideBar.LoadAffectedScripts(str);
    cmd_settreeurls:
      SideBar.SetURLList(str);
    cmd_seticon:
      SetIcon(str);
    cmd_setstatus:
      SetStatusBarText(str);
    cmd_syncwithtask:
      fSyncWithTask := true;
  end;
  j.Free;
end;

// Runs a user script (JavaScript or Lua)
procedure TSandcatTab.RunUserScript(var script: string;
  const lang: TUserScriptLanguage; const runonce: boolean = false);
begin
  if script = emptystr then
    exit;
  case lang of
    usJS:
      RunJavaScript(script);
    usLua:
      Extensions.RunLuaCmd(script);
  end;
  if runonce = true then
    script := emptystr;
end;

// Runs user scripts (if any) for a specific tab event
procedure TSandcatTab.RunUserScripts(const event: integer);
begin
  case event of
    SCBT_LOADEND:
      begin
        RunUserScript(UserScript.JS_Tab_LoadEnd, usJS);
        RunUserScript(UserTabScript.JS_LoadEnd, usJS);
        RunUserScript(UserTabScript.JS_LoadEnd_RunOnce, usJS, true);
        RunUserScript(UserTabScript.Lua_LoadEnd_RunOnce, usLua, true);
      end;
  end;
end;

// If a page is loaded, opens the Developer Tools for the tab
procedure TSandcatTab.ViewDevTools;
begin
  if fBrowser.Available = false then
    exit;
{$IFNDEF USEWACEF}
  // DCEF will display the DevTools as part of the browser tab instead of a
  // new window, so switch to it
  SetActivePage('browser');
{$ENDIF}
  fBrowser.c.ViewDevTools;
end;

// Creates a side tree that can be used by extensions (invisible by default)
procedure TSandcatTab.CreateSideTree;
begin
  fSideBar := TSandcatSidebar.Create(self);
  fSideBar.Parent := self;
  fSideBar.Align := AlLeft;
  fSideBar.LoadTreeItemFunc := 'tab.tree_loaditem';
  fTreeSplitter := TSplitter.Create(self);
  fTreeSplitter.Parent := self;
  fTreeSplitter.Width := 1;
  fTreeSplitter.visible := false;
  fTreeSplitter.Color := clBtnShadow;
end;

// Creates the main panel
procedure TSandcatTab.CreateMainPanel;
var
  i: integer;
  function GetPage(aPageName: string): TPage;
  begin
    Result := TPage(fSubTabs.Pages.Objects[fSubTabs.Pages.IndexOf(aPageName)]);
  end;

begin
  fMainPanel := TPanel.Create(self);
  fMainPanel.Parent := self;
  ConfigPanel(fMainPanel, AlClient);
  fSubTabs := TNoteBook.Create(fMainPanel);
  fSubTabs.Parent := fMainPanel;
  fSubTabs.Color := clWindow;
  fSubTabs.Align := AlClient;
  for i := Low(cTabSubPageNames) to High(cTabSubPageNames) do
    fSubTabs.Pages.Add(cTabSubPageNames[i]);
  // Creates the browser page
  fBrowserPanel := TCanvasPanel.Create(fMainPanel);
  fBrowserPanel.Parent := GetPage('browser');
  ConfigPanel(fBrowserPanel, AlClient);
  fBrowser := TCatChromiumStandBy.Create(fBrowserPanel);
  fBrowser.Parent := fBrowserPanel;
  fBrowser.Align := AlClient;
  fBrowser.OnInitialize := InitChrome;
  // Creates the source page
  fSourceInspect := TSyCodeInspector.Create(nil);
  fSourceInspect.Parent := GetPage('source');
  fSourceInspect.Align := AlClient;
  fSourceInspect.SetImageList(SandBrowser.LiveImages);
  ConfigSynEdit(fSourceInspect.source);
  // Creates the log page
  fLog := TSandLogMemo.Create(fBrowserPanel);
  fLog.Parent := GetPage('log');
  fLog.Align := AlClient;
  fLog.ReadOnly := true;
  fLog.Color := clBtnFace;
  fLog.ScrollBars := ssBoth;
  // Creates the resources page
  fResources := TTabResourceList.Create(fBrowserPanel);
  fResources.Parent := GetPage('resources');
  fResources.Align := AlClient;
  // Creates the response page
  fResponse := TTabResponseView.Create(fBrowserPanel);
  fResponse.Parent := GetPage('response');
  fResponse.Align := AlClient;
end;

// Creates the live headers panel and associated components
procedure TSandcatTab.CreateLiveHeaders;
begin
  fLiveHeaders := TLiveHeaders.Create(fMainPanel);
  fLiveHeaders.Parent := fMainPanel;
  fLiveHeaders.Align := AlClient;
  fCache := TSandCache.Create;
  fCache.new(GetSandcatDir(SCDIR_HEADERS) + 't_' + IntToStr(fMsg.msgHandle));
  fCache.MakeTemporary;
  fRequests := TSandcatRequests.Create(self, fMsg.msgHandle);
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
  fMsg := TCatMsg.Create;
  fMsg.OnDataMessage := CopyDataMessage;
  fMsgV8 := TCatMsg.Create;
  fMsgV8.OnDataMessage := BrowserMessage;
  fState := TTabState.Create;
  fDefaultIcon := '@ICON_EMPTY';
  fIsClosing := false;
  fLoading := false;
  fUseLuaOnLog := false;
  fLogBrowserRequests := true;
  fRetrieveFavIcon := true;
  fCanUpdateSource := true;
  fSyncWithTask := false;
  fUserData := TSandJSON.Create;
  fLuaOnLog := TSandJSON.Create;
  fDownloadsList := TStringList.Create;
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
  fMsgV8.Free;
  fMsg.Free;
  if fCustomTab <> nil then
    fCustomTab.Free; // Free Sciter engine
  if fCustomToolbar <> nil then
    fCustomToolbar.Free; // Free Sciter engine
  OnMessage := nil;
  fCache.Free;
  fRequests.Free;
  fSideBar.Free;
  fTreeSplitter.Free;
  fBrowser.Free;
  Debug('destroy.chrome.end');
  fDownloadsList.Free;
  fLog.Free;
  fSourceInspect.Free;
  fLiveHeaders.Free;
  fResponse.Free;
  fResources.Free;
  fBrowserPanel.Free;
  fSubTabs.Free;
  fLuaOnLog.Free;
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
  ActivePage := 'browser';
  ProtoIcon := '@ICON_BLANK';
  URL := emptystr;
  StatusBarText := emptystr;
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
    Navbar.FocusAndSetURL(CurrentURL);
  Navbar.ProtoIcon := ProtoIcon;
  StatBar.Text := StatusBarText;
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
