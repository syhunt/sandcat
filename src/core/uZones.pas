unit uZones;

{
  Sandcat User Interface Zones

  Copyright (c) 2011-2017, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.

}

interface

uses
{$IF CompilerVersion >= 23}
  Winapi.Windows, System.Classes, Vcl.Forms, System.SysUtils, Vcl.Controls,
  Vcl.ExtCtrls, Vcl.Dialogs, Vcl.Graphics, Vcl.StdCtrls, Winapi.CommCtrl,
  Vcl.ImgList, Vcl.Imaging.pngimage, System.TypInfo, Vcl.ComCtrls,
{$ELSE}
  Windows, Classes, Forms, SysUtils, Controls, ExtCtrls, Dialogs, Graphics,
  StdCtrls, CommCtrl, ImgList, pngimage, TypInfo, ComCtrls,
{$IFEND}
  SynUnicode, CatChromium, CatChromiumOSR, CatConsole, uReqBuilder, uRequests,
  uUIComponents, uTaskMon;

type
  TSandcatNavigationBar = class(TCustomControl)
  private
    e: ISandUIElement;
    fDefaultHeight: integer;
    fLoading: boolean;
    fBookmarked: boolean;
    fConsoleVisible: boolean;
    fHeadersVisible: boolean;
    fNote: TNoteBook;
    fEngine: TSandUIEngine;
    function GetSearchText: string;
    function GetURL: string;
    procedure SetBookmarked(const b: boolean);
    procedure SetConsoleVisible(const b: boolean);
    procedure SetHeadersVisible(const b: boolean);
    procedure SetLoading(const b: boolean);
    procedure SetProtoIcon(const URL: string);
    procedure SetURL(const URL: string);
  public
    procedure AnimateTasksIcon(const b: boolean);
    procedure EvalTIS(const s: string);
    procedure FocusURL;
    procedure Load;
    procedure LoadingStateChange(const isLoading, canGoBack,
      canGoForward: boolean);
    procedure UpdateSearchEngine;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // properties
    property ConsoleVisible: boolean read fConsoleVisible
      write SetConsoleVisible;
    property DefaultHeight: integer read fDefaultHeight;
    property Engine: TSandUIEngine read fEngine;
    property HeadersVisible: boolean read fHeadersVisible
      write SetHeadersVisible;
    property IsBookmarked: boolean read fBookmarked write SetBookmarked;
    property isLoading: boolean read fLoading write SetLoading;
    property Note: TNoteBook read fNote;
    property ProtoIcon: string write SetProtoIcon;
    property SearchText: string read GetSearchText;
    property URL: string read GetURL write SetURL;
  end;

type
  TSandcatTabstrip = class(TCustomControl)
  private
    fe: ISandUIElement;
    fEngine: TSandUIEngine;
  public
    procedure AddTab(const name: string);
    procedure RemoveTab(const name: string);
    procedure SelectTab(const name: string);
    procedure SetTabIcon(const tabid, src: string);
    procedure SetTabTitle(const tabid, newtitle: string);
    procedure EvalTIS(const s: string);
    procedure Load;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Engine: TSandUIEngine read fEngine;
  end;

type
  TSandcatStatusbar = class(TCustomControl)
  private
    e: ISandUIElement;
    fErrorLogIcon: string;
    fLoaded: boolean;
    fText: string;
    fEngine: TSandUIEngine;
    procedure SetErrorLogIcon(const src: string);
    procedure SetText(const s: string);
  public
    procedure Load;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // properties
    property Engine: TSandUIEngine read fEngine;
    property ErrorLogIcon: string read fErrorLogIcon write SetErrorLogIcon;
    property Text: string read fText write SetText;
  end;

type
  TSandcatSidebar = class(TCustomControl)
  private
    fCanExecLua: boolean;
    fLoadTreeItemFunc: string;
    fNote: TNoteBook;
    fTV: TTreeView;
    procedure LoadTreeItem(const path: string);
    procedure SideBarTreeChange(Sender: TObject; Node: TTreeNode);
    procedure SideBarTreeDblClick(Sender: TObject);
  public
    procedure Clear;
    procedure LoadAffectedScripts(const paths: string);
    procedure LoadDir(const dir: string; const makebold: boolean = true);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // properties
    property CanExecLua: boolean read fCanExecLua write fCanExecLua;
    property LoadTreeItemFunc: string read fLoadTreeItemFunc
      write fLoadTreeItemFunc;
    property Tree: TTreeView read fTV;
  end;

type
  TSandcatContentArea = class(TCustomControl)
  private
    fConsoleLastHeight: integer;
    fExtensionEngine: TSandUIEngine;
    fHasConsole: boolean;
    fIsClosing: boolean;
    fNote: TNoteBook;
    fPageHTML: string;
    fShowTabConsoleBottom: boolean;
    fTabsNotebook: TNoteBook;
    fUseManualConsole: boolean;
    procedure CreateConsole;
    function IsStandardPage(name: string): boolean;
    procedure ConsoleScriptCommand(const Code: string);
    procedure SetActiveSciter(page: TPage);
  public
    function PageExists(const name: string): boolean;
    procedure Console_Output(const b: boolean);
    procedure Console_WriteLn(const s: string);
    procedure Console_Write(const s: string);
    procedure ClosePage(const name: string);
    procedure CreatePage(const SubTabName: string);
    procedure LoadPage(const HTML: string; const SubTabName: string = '');
    procedure ViewConsole(const visible: boolean = true);
    procedure SetActivePage(const name: string);
    procedure Shutdown;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Note: TNoteBook read fNote;
    property TabsNotebook: TNoteBook read fTabsNotebook;
    property UseManualConsole: boolean read fUseManualConsole
      write fUseManualConsole;
  end;

type
  TSandcatBottomBar = class(TCustomControl)
  private
    fCrm: TCatChromium;
    fEngine: TSandUIEngine;
    fNote: TNoteBook;
    fReqBuilder: TSandRequestPanel;
    fSplitter: TSplitter;
    fTaskMsgs: TTaskMessages;
  public
    procedure Load;
    procedure LoadBottomBar(const HTML: string);
    procedure LoadBottomBarRight(const pagename: string);
    procedure LoadSettings(settings, DefaultSettings: TSandJSON);
    procedure ViewBottomBar(const b: boolean = true);
    procedure ShowRequestBuilderBar;
    procedure ShowURL(const URL: string; const source: string = '');
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Engine: TSandUIEngine read fEngine;
    property Note: TNoteBook read fNote;
    property TaskMsgs: TTaskMessages read fTaskMsgs;
    property ReqBuilder: TSandRequestPanel read fReqBuilder;
  end;

type
  TSandcatPageBar = class(TCustomControl)
  private
    e: ISandUIElement;
    fEngine: TSandUIEngine;
    fStripVisible: boolean;
    procedure SetStripVisible(const b: boolean);
  public
    procedure AddPage(const name: string);
    procedure AdjustPageStrip(const SideBar: TSandcatSidebar);
    procedure EvalTIS(const s: string);
    procedure ShowPage(const name: string; const visible: boolean);
    procedure SelectPage(const name: string);
    procedure Load;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Engine: TSandUIEngine read fEngine;
    property StripVisible: boolean read fStripVisible write SetStripVisible;
  end;

type
  TSandcatDialogs = class
  private
  public
    function DlgReplace(const s: string): string;
    procedure AppMinimize(Sender: TObject);
    procedure SaveResource(const URL: string; const fromcloud: boolean = false);
    procedure SaveURLAs(const URL: string);
    procedure ShowAlert(const msg: string; const escape_html: boolean = true);
    procedure ShowAlertText(const msg: string);
    procedure ShowCustomDialog(const HTML: string; const id: string = '');
    procedure ShowHTMLMessage(const HTML: string);
    procedure ShowMessage(const msg: string; const escape: boolean = true);
    constructor Create;
    destructor Destroy; override;
  end;

type // Holds the HTML from the resources.pak for fast loading
  TSandcatHTMLPages = record
    BottomBar: string;
    ReqBuilderBar: string;
    Tab_Custom: string;
    Tab_Tasks: string;
    Tab_Toolbar: string;
  end;

type // User TIScripts set by the user via Sandcat Extensions
  TSandcatTISUserScript = record
    DialogAbout: string;
    DialogPreferences: string;
    ReqBuilderToolbar: string;
    TabBar: string;
    TabBottomBar: string;
    TabNavBar: string;
  end;

type
  TSandcatUIX = class
  private
    fActiveMemo: TMemo;
    fImageListRegistry: TStringList;
    function GetFileImageIndex(const filename: string;
      const isselect: boolean = false): integer;
    function ImageListGetIndex(const src: string): integer;
  public
    TIS: TSandcatTISUserScript;
    Pages: TSandcatHTMLPages;
    function ImageListAdd(const Pak, ImageFile: string): integer;
    procedure AddHTML(const Engine, Selector, HTML: string;
      const index: string = 'undefined');
    procedure AddHTMLFile(const Engine, Selector, HTMLFilename: string);
    procedure AddTIS(const Script, Zone: string);
    procedure CreateElement(const Engine, Table, Selector: string);
    procedure InsertHTML(const Engine, index, Selector, HTML: string);
    procedure InsertHTMLFile(const Engine, index, Selector,
      HTMLFilename: string);
    procedure LoadUI;
    procedure ShowRequest(const Requests: TSandcatRequests;
      const filename: string);
    procedure ShowResource(const URL: string);
    procedure StdOut(ASender: TObject; const msg: WideString);
    procedure StdErr(ASender: TObject; const msg: WideString);
    procedure Tree_FilePathToTreeNode(aTreeView: TTreeView; aRoot: TTreeNode;
      path: string; Recurse: boolean; makebold: boolean = true);
    procedure Tree_SetAffectedImages(tv: TTreeView; paths: string);
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    // properties
    property ActiveMemo: TMemo read fActiveMemo write fActiveMemo;
  end;

const // IDs for the main Sciter engines
  ENGINE_BOTTOMBAR = 1;
  ENGINE_CUSTOMTAB = 2;
  ENGINE_NAVBAR = 3;
  ENGINE_LIVEHEADERS = 5;
  ENGINE_PAGEBAR = 6;
  ENGINE_STATBAR = 7;
  ENGINE_EXTENSIONPAGE = 8;
  ENGINE_CUSTOMTABTOOLBAR = 9;
  ENGINE_REQBUILDERTOOLBAR = 10;

const
  cContent = '{$Content}';
  cUIX = 'UIX';
  cUIXUpdate = 'SandcatUIX.Update();';

var
  TaskMonitor, ExtensionPage: TSandUIEngine;
  Navbar: TSandcatNavigationBar;
  StatBar: TSandcatStatusbar;
  PageBar: TSandcatPageBar;
  BottomBar: TSandcatBottomBar;
  Tabstrip: TSandcatTabstrip;
  SideBar: TSandcatSidebar;
  ConSplitter: TSplitter;
  ContentArea: TSandcatContentArea;
  SandDlg: TSandcatDialogs;

function GetZoneByID(const n: integer): TSandUIEngine;
function GetZoneID(const name: string): integer;

implementation

uses uMain, CatRegEx, CatCEFCache, CatStrings, CatHTTP, uConst, uLiveHeaders,
  uSettings, uMisc, CatFiles, CatUI, CatZIP, CatRes;

procedure Debug(const s: string; const component: string = cUIX);
begin
  uMain.Debug(s, component);
end;

// Returns an unique ID for an UI zone by the zone name
function GetZoneID(const name: string): integer;
var
  z: string;
begin
  result := -1;
  z := lowercase(name);
  if beginswith(z, 'tab.') then
  begin
    if z = 'tab.custom' then
      result := ENGINE_CUSTOMTAB
    else if z = 'tab.engine' then
      result := ENGINE_CUSTOMTAB
    else if z = 'tab.toolbar' then
      result := ENGINE_CUSTOMTABTOOLBAR;
  end
  else if beginswith(z, 'browser.') then
  begin
    if z = 'browser.bottombar' then
      result := ENGINE_BOTTOMBAR
    else if z = 'browser.navbar' then
      result := ENGINE_NAVBAR
    else if z = 'browser.pagex' then
      result := ENGINE_EXTENSIONPAGE
    else if z = 'browser.pagebar' then
      result := ENGINE_PAGEBAR
    else if z = 'browser.statbar' then
      result := ENGINE_STATBAR;
  end
  else if z = 'reqbuilder.toolbar' then
    result := ENGINE_REQBUILDERTOOLBAR;
end;

// Returns the UI Zone engine by its unique numeric ID
function GetZoneByID(const n: integer): TSandUIEngine;
begin
  result := nil;
  case n of
    ENGINE_BOTTOMBAR:
      result := BottomBar.Engine;
    ENGINE_CUSTOMTAB:
      result := tabmanager.ActiveTab.customtab;
    ENGINE_EXTENSIONPAGE:
      result := ExtensionPage;
    ENGINE_NAVBAR:
      result := Navbar.Engine;
    ENGINE_PAGEBAR:
      result := PageBar.Engine;
    ENGINE_STATBAR:
      result := StatBar.Engine;
    ENGINE_CUSTOMTABTOOLBAR:
      result := tabmanager.ActiveTab.CustomToolbar;
    ENGINE_REQBUILDERTOOLBAR:
      result := BottomBar.ReqBuilder.ToolbarBar;
  end;
end;

function TSandcatUIX.ImageListAdd(const Pak, ImageFile: string): integer;
var
  p: TPngImage;
  bmp: TBitmap;
  ms: tmemorystream;
  idx: string;
begin
  result := -1;
  if Pak = emptystr then
    exit;
  ms := tmemorystream.Create;
  ExtractZIPFileToStream(Pak, ImageFile, ms);
  p := TPngImage.Create;
  p.LoadFromStream(ms);
  bmp := TBitmap.Create;
  p.AssignTo(bmp);
  bmp.AlphaFormat := afIgnored;
  ImageList_Add(sandbrowser.LiveImages.Handle, bmp.Handle, 0);
  result := sandbrowser.LiveImages.count - 1;
  idx := inttostr(result);
  fImageListRegistry.Values
    [strtohex(extractfilename(Pak) + '#' + ImageFile)] := idx;
  bmp.Free;
  p.Free;
  ms.Free;
end;

function TSandcatUIX.ImageListGetIndex(const src: string): integer;
var
  idx: string;
begin
  idx := fImageListRegistry.Values[strtohex(src)];
  if idx = emptystr then
    result := -1
  else
    result := strtoint(idx);
end;

type
  TFileExts = (asp, aspx, bmp, gif, ico, jpg, jpeg, png, svg, css, htm, HTML,
    js, json, jsp, lua, lp, phtml, pl, py, wsgi, psp, psp_, rb, swf, vb,
    vbs, xml);

function TSandcatUIX.GetFileImageIndex(const filename: string;
  const isselect: boolean = false): integer;
var
  ext, ext2, f: string;
begin
  result := ICONIDX_UNKNOWN; // unknown type
  f := extractfilename(filename);
  ext := lowercase(extractfileext(f));
  if ext = emptystr then
  begin
    if beginswith(f, '.') = false then
    begin
      if isselect then
        result := ICONIDX_FOLDER_OPEN
      else
        result := ICONIDX_FOLDER_CLOSED;
    end;
  end;
  ext2 := lowercase(after(ext, '.'));
  // Tries to get file image index by file extension
  case TFileExts(GetEnumValue(TypeInfo(TFileExts), ext2)) of
    asp, aspx, vb, vbs:
      result := ICONIDX_SCRIPT;
    bmp, gif, ico, jpg, jpeg, png, svg:
      result := ICONIDX_IMAGE;
    css:
      result := ICONIDX_CSS;
    htm, HTML:
      result := ICONIDX_HTML;
    lua, lp:
      result := ICONIDX_SCRIPT;
    js:
      result := ICONIDX_JAVASCRIPT;
    json:
      result := ICONIDX_JSON;
    jsp:
      result := ICONIDX_SCRIPT;
    phtml:
      result := ICONIDX_PHPSCRIPT;
    pl:
      result := ICONIDX_SCRIPT;
    py, wsgi, psp, psp_:
      result := ICONIDX_SCRIPT;
    rb:
      result := ICONIDX_SCRIPT;
    swf:
      result := ICONIDX_FLASH;
    xml:
      result := ICONIDX_XML;
  end;
  if pos('.php', ext) <> 0 then
    result := ICONIDX_PHPSCRIPT;
end;

procedure TSandcatUIX.Tree_SetAffectedImages(tv: TTreeView; paths: string);
var
  i, c: integer;
  pl: TStringList;
  fp: string;
  procedure SetAsVulnerable(tn: TTreeNode);
  var
    ext: string;
  begin
    ext := extractfileext(lowercase(tn.Text));
    if beginswith(ext, '.php') then
      tn.ImageIndex := ICONIDX_PHPSCRIPT_VULNERABLE
    else
      tn.ImageIndex := ICONIDX_SCRIPT_VULNERABLE;
    tn.SelectedIndex := tn.ImageIndex;
  end;

begin
  pl := TStringList.Create;
  pl.Text := paths;
  c := tv.Items.count;
  for i := 0 to c - 1 do
  begin
    if i < c then
    begin
      fp := GetFullPath(tv.Items[i]);
      if pl.IndexOf(fp) <> -1 then
        SetAsVulnerable(tv.Items[i]);
    end;
  end;
  pl.Free;
end;

procedure TSandcatUIX.Tree_FilePathToTreeNode(aTreeView: TTreeView;
  aRoot: TTreeNode; path: string; Recurse: boolean; makebold: boolean = true);
var
  NewNode: TTreeNode;
  SRec: TSearchRec;
begin
  if FindFirst(path + '*.*', faAnyFile, SRec) = 0 then
    repeat
      if (SRec.name = '.') or (SRec.name = '..') then
        Continue;
      if ((SRec.Attr and (faHidden)) = 0) then
      begin
        NewNode := aTreeView.Items.AddChild(aRoot, SRec.name);
        NewNode.ImageIndex := GetFileImageIndex(SRec.name);
        NewNode.SelectedIndex := GetFileImageIndex(SRec.name, true);
        if makebold then
          SetNodeBoldState(NewNode, true);
      end;
      if Recurse and ((SRec.Attr and faDirectory) <> 0) then
        Tree_FilePathToTreeNode(aTreeView, NewNode,
          path + SRec.name + '\', true);
    until FindNext(SRec) <> 0;
end;

procedure TSandcatUIX.ShowRequest(const Requests: TSandcatRequests;
  const filename: string);
var
  e: ISandUIElement;
begin
  if settings.preferences.getvalue(SCO_EXTENSIONS_ENABLED, true) = false then
    exit;
  e := ExtensionPage.Root.Select('meta[content=''ReqViewer.ui'']');
  if e = nil then
    Extensions.RunLuaCmd('ReqViewer:load()', cResourcesPak, 'reqviewer.lua');
  ContentArea.SetActivePage('response');
  Extensions.LuaWrap.value['_tempfilename'] := filename;
  Extensions.RunLuaCmd('ReqViewer:loadrequest(_tempfilename)');
end;

procedure TSandcatUIX.ShowResource(const URL: string);
var
  e: ISandUIElement;
begin
  if settings.preferences.getvalue(SCO_EXTENSIONS_ENABLED, true) = false then
    exit;
  ContentArea.SetActivePage('response');
  e := ExtensionPage.Root.Select('meta[content=''ReqViewer.ui'']');
  if e = nil then
    Extensions.RunLuaCmd('ReqViewer:load()', cResourcesPak, 'reqviewer.lua');
  Extensions.LuaWrap.value['_tempresourceurl'] := URL;
  Extensions.RunLuaCmd('ReqViewer:loadcachedurl(_tempresourceurl)');
end;

procedure TSandcatUIX.StdErr(ASender: TObject; const msg: WideString);
begin
  if pos('assuming namespace declaration', string(msg)) = 0 then
    Debug(msg, 'TIS Warning');
end;

procedure TSandcatUIX.StdOut(ASender: TObject; const msg: WideString);
begin
  if beginswith(msg, '{') then
    Extensions.RunJSONCmd(msg)
  else
    ContentArea.Console_WriteLn(msg);
end;

procedure TSandcatUIX.AddTIS(const Script, Zone: string);
var
  z: string;
begin
  z := lowercase(Zone);
  if (z <> emptystr) and (Script <> emptystr) then
  begin
    if z = 'dlg.about' then
      TIS.DialogAbout := TIS.DialogAbout + crlf + Script
    else if z = 'dlg.prefs' then
      TIS.DialogPreferences := TIS.DialogPreferences + crlf + Script
    else if z = 'browser.bottombar' then
      TIS.TabBottomBar := TIS.TabBottomBar + crlf + Script
    else if z = 'browser.navbar' then
      TIS.TabNavBar := TIS.TabNavBar + crlf + Script
    else if z = 'browser.tabbar' then
      TIS.TabBar := TIS.TabBar + crlf + Script
    else if z = 'reqbuilder.toolbar' then
      TIS.ReqBuilderToolbar := TIS.ReqBuilderToolbar + crlf + Script;
  end;
end;

procedure TSandcatUIX.AddHTML(const Engine, Selector, HTML: string;
  const index: string = 'undefined');
const
  cScript = 'SandcatUIX.InsertHTML($("%s"),%s,%s);';
var
  j: TSandJSON;
begin
  j := TSandJSON.Create;
  j['html'] := HTML;
  AddTIS(format(cScript, [htmlescape(Selector), j.TextUnquoted, index]
    ), Engine);
  j.Free;
end;

procedure TSandcatUIX.InsertHTML(const Engine, index, Selector, HTML: string);
begin
  AddHTML(Engine, Selector, HTML, index);
end;

procedure TSandcatUIX.InsertHTMLFile(const Engine, index, Selector,
  HTMLFilename: string);
const
  cScript = '$("%s").insert("<include src=''%s''/>",%s);';
begin
  AddTIS(format(cScript, [htmlescape(Selector), htmlescape(HTMLFilename), index]
    ), Engine);
end;

procedure TSandcatUIX.AddHTMLFile(const Engine, Selector, HTMLFilename: string);
const
  cScript = '$("%s").insert("<include src=''%s''/>");';
begin
  AddTIS(format(cScript, [htmlescape(Selector), htmlescape(HTMLFilename)]
    ), Engine);
end;

procedure TSandcatUIX.CreateElement(const Engine, Table, Selector: string);
var
  id, prop: string;
  Script: TStringList;
begin
  id := after(Selector, '#');
  id := replacestr(id, '-', '_');
  // debug('Creating element '+selector+' in '+engine);
  prop := Table + '.' + id;
  Script := TStringList.Create;
  Script.add('if ' + Table + ' == nil then ' + Table + ' = {} end');
  Script.add('if ' + prop + ' == nil then');
  Script.add(prop + ' = SandcatUIElement:new()');
  Script.add(prop + ':select("' + Selector + '","' + Engine + '")');
  Script.add('else');
  // Just updades the selector
  Script.add(prop + '.selector = "' + Selector + '"');
  Script.add('end');
  Extensions.RunLuaCmd(Script.Text);
  Script.Free;
end;

procedure TSandcatUIX.LoadUI;
begin
  BottomBar.Load;
  Tabstrip.Load;
  PageBar.Load;
  Navbar.Load;
  StatBar.Load;
  if Extensions.CurrentInitMode = 'sandcat' then
    application.ProcessMessages;
  // run user scripts
  Tabstrip.EvalTIS(TIS.TabBar + crlf + cUIXUpdate);
  PageBar.EvalTIS(TIS.TabBottomBar + crlf + cUIXUpdate);
  Navbar.EvalTIS(TIS.TabNavBar + crlf + cUIXUpdate);
end;

constructor TSandcatUIX.Create(AOwner: TWinControl);
const
  cHTML = 'HTML';
begin
  inherited Create;
  fImageListRegistry := TStringList.Create;
  Pages.ReqBuilderBar := GetResourceAsString('REQUESTBUILDER', cHTML);
  Pages.BottomBar := GetResourceAsString('TAB_BOTTOMBAR', cHTML);
  Pages.Tab_Custom := GetResourceAsString('TAB_CUSTOM', cHTML);
  Pages.Tab_Tasks := GetResourceAsString('TAB_TASKS', cHTML);
  Pages.Tab_Toolbar := GetResourceAsString('TAB_TOOLBAR', cHTML);
  // create the UI engines
  Navbar := TSandcatNavigationBar.Create(MainPanel);
  Navbar.Parent := MainPanel;
  Navbar.Align := AlTop;
  PageBar := TSandcatPageBar.Create(MainPanel);
  PageBar.Parent := MainPanel;
  PageBar.Align := AlBottom;
  BottomBar := TSandcatBottomBar.Create(MainPanel);
  BottomBar.Parent := MainPanel;
  BottomBar.Align := AlBottom;
end;

destructor TSandcatUIX.Destroy;
begin
  Debug('destroy.begin');
  BottomBar.Free;
  PageBar.Free;
  Navbar.Free;
  fImageListRegistry.Free;
  Debug('destroy.end');
  inherited;
end;

// ------------------------------------------------------------------------//
// TSandcatNavigationBar                                                   //
// ------------------------------------------------------------------------//

procedure TSandcatNavigationBar.Load;
begin
  fEngine.LoadHTML(GetResourceAsString('TAB_NAVBAR', 'HTML'), pluginsdir);
end;

procedure TSandcatNavigationBar.AnimateTasksIcon(const b: boolean);
begin
  fEngine.Eval('AnimateTasksIcon(' + lowercase(BoolToStr(b)) + ')')
end;

procedure TSandcatNavigationBar.FocusURL;
begin
  fEngine.Eval('view.focus = $(#url);');
end;

function TSandcatNavigationBar.GetSearchText: string;
begin
  result := fEngine.Root.Select('widget#search').value;
end;

procedure TSandcatNavigationBar.LoadingStateChange(const isLoading, canGoBack,
  canGoForward: boolean);
begin
  if fEngine = nil then
    exit;
  if canGoBack then
  begin
    e := fEngine.Root.Select('#goback');
    if e <> nil then
      e.StyleAttr['display'] := 'block';
    e := fEngine.Root.Select('#url');
    if e <> nil then
    begin
      e.StyleAttr['border-left'] := '0px';
      e.StyleAttr['border-radius'] := '0px';
    end;
  end;
  e := fEngine.Root.Select('#goforward');
  if e <> nil then
    if canGoForward then
      e.StyleAttr['display'] := 'block';
end;

procedure TSandcatNavigationBar.SetBookmarked(const b: boolean);
begin
  fBookmarked := b;
  e := fEngine.Root.Select('#bookmark');
  if e <> nil then
    e.StyleAttr['foreground-image-transformation'] :=
      iif(b, 'none', 'colorize(gray)')
end;

procedure TSandcatNavigationBar.SetHeadersVisible(const b: boolean);
var
  ViewHeaders, headersmenu, headersmenuflt: ISandUIElement;
begin
  fHeadersVisible := b;
  ViewHeaders := fEngine.Root.Select('#viewheaders');
  headersmenu := fEngine.Root.Select('#headersmenu');
  headersmenuflt := fEngine.Root.Select('#headersmenuflt');
  if b then // visible
  begin
    if ViewHeaders <> nil then
      ViewHeaders.StyleAttr['border-radius'] := '4px 0px 0px 4px';
    if headersmenu <> nil then
      headersmenu.StyleAttr['display'] := 'block';
    if headersmenuflt <> nil then
      headersmenuflt.StyleAttr['display'] := 'block';
    fEngine.Eval('$(#viewheaders).checked = true; view.update(true);');
  end
  else
  begin
    if ViewHeaders <> nil then
      ViewHeaders.StyleAttr['border-radius'] := '4px 4px 4px 4px';
    if headersmenu <> nil then
      headersmenu.StyleAttr['display'] := 'none';
    if headersmenuflt <> nil then
      headersmenuflt.StyleAttr['display'] := 'none';
    fEngine.Eval('$(#viewheaders).checked = false; view.update(true);');
  end;
end;

procedure TSandcatNavigationBar.SetConsoleVisible(const b: boolean);
var
  csmenu, csbtn: ISandUIElement;
begin
  fConsoleVisible := b;
  csmenu := fEngine.Root.Select('#csmenu');
  csbtn := fEngine.Root.Select('#viewconsole');
  if b then
  begin
    fEngine.Eval('$(#viewconsole).checked = true;');
    if csmenu <> nil then
      csmenu.StyleAttr['display'] := 'block';
    if csbtn <> nil then
      csbtn.StyleAttr['border-radius'] := '4px 0px 0px 4px';
  end
  else
  begin
    fEngine.Eval('$(#viewconsole).checked = false;');
    if csmenu <> nil then
      csmenu.StyleAttr['display'] := 'none';
    if csbtn <> nil then
      csbtn.StyleAttr['border-radius'] := '4px 4px 4px 4px';
  end;
end;

procedure TSandcatNavigationBar.SetLoading(const b: boolean);
var
  icon, onclick, titleid: string;
begin
  if fEngine = nil then
    exit;
  fLoading := b;
  icon := '@ICON_RELOAD';
  titleid := 'for-sandcat_reload';
  onclick := 'tab:reload()';
  if b = true then
  begin
    icon := '@ICON_STOP';
    titleid := 'for-sandcat_stop';
    onclick := 'tab:stopload()';
  end;
  e := fEngine.Root.Select('#reload');
  if e <> nil then
  begin
    e.StyleAttr['foreground-image'] := icon;
    e.StyleAttr['onclick'] := onclick;
    e.StyleAttr['titleid'] := titleid;
  end;
end;

procedure TSandcatNavigationBar.SetProtoIcon(const URL: string);
begin
  e := fEngine.Root.Select('#url');
  if e <> nil then
    e.StyleAttr['foreground-image'] := URL;
end;

function TSandcatNavigationBar.GetURL: string;
begin
  e := fEngine.Root.Select('#url');
  if e <> nil then
    result := e.value
  else
    result := emptystr;
end;

procedure TSandcatNavigationBar.SetURL(const URL: string);
begin
  e := fEngine.Root.Select('#url');
  if e <> nil then
    e.value := URL;
end;

procedure TSandcatNavigationBar.EvalTIS(const s: string);
begin
  if s <> emptystr then
    fEngine.Eval(s);
end;

procedure TSandcatNavigationBar.UpdateSearchEngine;
begin
  e := fEngine.Root.Select('#searcheng');
  if e <> nil then
    e.StyleAttr['foreground-image'] := vSearchEngine_Icon;
  e := fEngine.Root.Select('#search');
  if e <> nil then
    e.Attr['novalue'] := format('Search with %s', [vSearchEngine_Name]);
end;

constructor TSandcatNavigationBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  fDefaultHeight := 35;
  height := fDefaultHeight;
  fNote := TNoteBook.Create(Self);
  fNote.Parent := Self;
  fNote.Align := alClient;
  fNote.height := fDefaultHeight;
  fEngine := TSandUIEngine.Create(MainPanel);
  fEngine.Parent := TPage(fNote.Pages.Objects[fNote.Pages.IndexOf('Default')]);;
  fEngine.Align := alClient;
  fEngine.OnonStdOut := uix.StdOut;
  fEngine.OnonStdErr := uix.StdErr;
end;

destructor TSandcatNavigationBar.Destroy;
begin
  fEngine.Free;
  fNote.Free;
  inherited Destroy;
end;

// ------------------------------------------------------------------------//
// TSandcatStatusBar                                                       //
// ------------------------------------------------------------------------//

procedure TSandcatStatusbar.SetErrorLogIcon(const src: string);
begin
  fErrorLogIcon := src;
  if fEngine = nil then
    exit;
  e := fEngine.Root.Select('#errorlog');
  if e <> nil then
    e.StyleAttr['foreground-image'] := src;
end;

procedure TSandcatStatusbar.SetText(const s: string);
begin
  fText := s;
  e := fEngine.Root.Select('#status');
  if e <> nil then
    e.value := s;
end;

procedure TSandcatStatusbar.Load;
begin
  fLoaded := true;
  fEngine := TSandUIEngine.Create(Self);
  fEngine.Parent := Self;
  fEngine.Align := alClient;
  fEngine.LoadHTML(GetResourceAsString('STATBAR', 'HTML'), pluginsdir);
end;

constructor TSandcatStatusbar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  height := 22;
end;

destructor TSandcatStatusbar.Destroy;
begin
  if fLoaded then
    fEngine.Free;
  inherited Destroy;
end;

// ------------------------------------------------------------------------//
// TSandcatSideBar                                                         //
// ------------------------------------------------------------------------//

constructor TSandcatSidebar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Width := 300;
  visible := false;
  fLoadTreeItemFunc := 'sidebar.loadtreeitem'; // tab must use tab.tree_loaditem
  fCanExecLua := true;
  fNote := TNoteBook.Create(Self);
  fNote.Parent := Self;
  fNote.Align := alClient;
  fNote.Pages.add('treeview');
  fTV := TTreeView.Create(Self);
  fTV.Parent := TPage(fNote.Pages.Objects[fNote.Pages.IndexOf('treeview')]);
  fTV.Align := alClient;
  fTV.Images := sandbrowser.LiveImages;
  fTV.ReadOnly := true;
  fTV.HideSelection := false;
  fTV.ShowLines := false;
  fTV.OnChange := SideBarTreeChange;
  fTV.OnDblClick := SideBarTreeDblClick;
end;

destructor TSandcatSidebar.Destroy;
begin
  fTV.OnChange := nil;
  fTV.Free;
  fNote.Free;
  inherited Destroy;
end;

// Clears the side tree
procedure TSandcatSidebar.Clear;
begin
  fTV.OnChange := nil;
  fTV.Items.Clear;
  fTV.OnChange := SideBarTreeChange;
end;

// Can be called by extensions to load a directory tree as the side tree items
procedure TSandcatSidebar.LoadDir(const dir: string;
  const makebold: boolean = true);
begin
  uix.Tree_FilePathToTreeNode(fTV, nil, dir, true, makebold);
end;

// Updates site tree item images, highlighting scripts that have some issue
// Used by Sandcat extensions
procedure TSandcatSidebar.LoadAffectedScripts(const paths: string);
begin
  uix.Tree_SetAffectedImages(fTV, paths);
end;

// Called when an item from the side tree has been clicked
procedure TSandcatSidebar.LoadTreeItem(const path: string);
begin
  if fCanExecLua = false then
    exit;
  Extensions.LuaWrap.value['_temppath'] := path;
  Extensions.RunLuaCmd(fLoadTreeItemFunc + '(_temppath)');
end;

// Handles side tree item doubleclicks
procedure TSandcatSidebar.SideBarTreeDblClick(Sender: TObject);
begin
  if (fTV.Selected <> nil) then
    LoadTreeItem(GetFullPath(fTV.Selected));
end;

// Handles side tree item selection changes
procedure TSandcatSidebar.SideBarTreeChange(Sender: TObject; Node: TTreeNode);
begin
  if (fTV.Selected = nil) then
    exit;
  LoadTreeItem(GetFullPath(fTV.Selected));
  SetNodeBoldState(fTV.Selected, false);
  Application.ProcessMessages;
end;

// ------------------------------------------------------------------------//
// TSandcatContentArea                                                     //
// ------------------------------------------------------------------------//

procedure TSandcatContentArea.CreateConsole;
begin
  if fHasConsole then
    exit; // Already created
  fHasConsole := true;
  SandConsole := TCatConsole.Create(sandbrowser);
  SandConsole.Parent := sandbrowser;
  SandConsole.Align := alClient;
  SandConsole.OnScriptCommand := ConsoleScriptCommand;
  SandConsole.Console.Font.Color :=
    HtmlColorToColor(settings.preferences[SCO_CONSOLE_FONT_COLOR]);
  SandConsole.Console.Color :=
    HtmlColorToColor(settings.preferences[SCO_CONSOLE_BGCOLOR]);
  SandConsole.Boot;
  SandConsole.Parent :=
    TPage(Note.Pages.Objects[Note.Pages.IndexOf('console')]);
  ConSplitter := TSplitter.Create(MainPanel);
  ConSplitter.Parent := MainPanel;
  ConSplitter.Color := clBtnShadow;
  ConSplitter.Align := AlBottom;
  ConSplitter.height := 2;
  ConSplitter.visible := false;
  SandConsole.LoadSettings(settings.preferences);
end;

procedure TSandcatContentArea.SetActiveSciter(page: TPage);
var
  i: integer;
begin
  for i := page.ControlCount - 1 downto 0 do
  begin
    if page.Controls[i] is TSandUIEngine then
      ExtensionPage := TSandUIEngine(page.Controls[i]);
  end;
end;

procedure TSandcatContentArea.LoadPage(const HTML: string;
  const SubTabName: string = '');
var
  p, ht: string;
begin
  p := SubTabName;
  if p = emptystr then
    p := 'extension';
  SetActivePage(p);
  ht := replacestr(fPageHTML, cContent, HTML);
  ExtensionPage.LoadHTML(ht, pluginsdir);
end;

procedure TSandcatContentArea.ClosePage(const name: string);
begin
  PageBar.ShowPage(name, false);
  SetActivePage('browser');
end;

procedure TSandcatContentArea.ViewConsole(const visible: boolean = true);
begin
  fShowTabConsoleBottom := visible;
  if visible then
  begin
    CreateConsole;
    Navbar.ConsoleVisible := true;
    SandConsole.Align := AlBottom;
    SandConsole.Parent := Self;
    SandConsole.height := fConsoleLastHeight;
    SandConsole.visible := true;
    ConSplitter.visible := true;
    ConSplitter.Parent := SandConsole.Parent;
    ConSplitter.Top := SandConsole.Top + 1;
  end
  else
  begin
    Navbar.ConsoleVisible := false;
    if fHasConsole then
    begin
      SandConsole.visible := false;
      ConSplitter.visible := false;
    end;
  end;
end;

procedure TSandcatContentArea.Console_Output(const b: boolean);
begin
  if fIsClosing then
    exit;
  if UseManualConsole then
    exit; // will be manually reset by the user
  if fHasConsole then
  begin
    if SandConsole <> nil then
      SandConsole.ConsoleOutput(b);
  end;
end;

procedure TSandcatContentArea.Console_WriteLn(const s: string);
begin
  if fIsClosing then
    exit;
  if fShowTabConsoleBottom = false then
    ViewConsole(true);
  SandConsole.writeln(s);
end;

procedure TSandcatContentArea.Console_Write(const s: string);
begin
  if fIsClosing then
    exit;
  if fShowTabConsoleBottom = false then
    ViewConsole(true);
  SandConsole.Write(s);
end;

function TSandcatContentArea.PageExists(const name: string): boolean;
begin
  if Note.Pages.IndexOf(Name) = -1 then
    result := false
  else
    result := true;
end;

procedure TSandcatContentArea.ConsoleScriptCommand(const Code: string);
begin
  Extensions.RunLuaCmd(Code);
end;

function TSandcatContentArea.IsStandardPage(name: string): boolean;
begin
  result := false;
  if name = 'browser' then
    result := true
  else if name = 'source' then
    result := true
  else if name = 'log' then
    result := true
  else if name = 'resources' then
    result := true
  else if name = 'codeedit' then
    result := true;
end;

procedure TSandcatContentArea.SetActivePage(const name: string);
var
  n: string;
begin
  n := lowercase(name);
  tabmanager.ActiveTab.State.ActivePageName := n;
  if IsStandardPage(n) then
  begin
    tabmanager.ActiveTab.SetActivePage(n);
    n := 'default';
  end;
  CreatePage(n);
  Note.ActivePage := n;
  SetActiveSciter(TPage(Note.Pages.Objects[Note.Pages.IndexOf(n)]));
  PageBar.SelectPage(name);
end;

// Creates a new bottom tab (if inexistent)
procedure TSandcatContentArea.CreatePage(const SubTabName: string);
var
  new: TSandUIEngine;
  backref: string;
begin
  if PageExists(SubTabName) = false then
  begin
    // showmessage('creating new tab:'+SubTabName);
    backref := Note.ActivePage;
    Note.Pages.add(SubTabName);
    new := TSandUIEngine.Create(StatBar);
    new.Parent := TPage(Note.Pages.Objects[Note.Pages.IndexOf(SubTabName)]);
    new.Align := alClient;
    new.OnonStdOut := uix.StdOut;
    new.OnonStdErr := uix.StdErr;
    if SubTabName = 'tasks' then
      TaskMonitor := new;
    PageBar.AddPage(SubTabName);
    Note.ActivePage := backref; // Gets back to the previous page
  end
  else
    PageBar.ShowPage(SubTabName, true);
  // Tab already created. If it is hidden, displays it
end;

procedure TSandcatContentArea.Shutdown;
begin
  fIsClosing := true;
  EnableConsoleInteraction := false;
  if ConSplitter <> nil then
    ConSplitter.Free;
  if SandConsole <> nil then
    SandConsole.Free;
end;

constructor TSandcatContentArea.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  fHasConsole := false;
  fIsClosing := false;
  UseManualConsole := false;
  fConsoleLastHeight := 80; // default height
  fShowTabConsoleBottom := false;
  fNote := TNoteBook.Create(Self);
  fNote.Parent := Self;
  fNote.Align := alClient;
  fNote.Pages.add('console');
  fNote.Pages.add('extension');
  fNote.ActivePage := 'default';
  // extension tab
  fExtensionEngine := TSandUIEngine.Create(MainPanel);
  fExtensionEngine.Parent :=
    TPage(fNote.Pages.Objects[fNote.Pages.IndexOf('extension')]);
  fExtensionEngine.Align := alClient;
  fExtensionEngine.OnonStdOut := uix.StdOut;
  fExtensionEngine.OnonStdErr := uix.StdErr;
  ExtensionPage := fExtensionEngine;
  fPageHTML := GetResourceAsString('PAGE_EXTENSION', 'HTML');
  // default page
  fTabsNotebook := TNoteBook.Create(Self);
  fTabsNotebook.Parent :=
    TPage(Note.Pages.Objects[fNote.Pages.IndexOf('Default')]);
  fTabsNotebook.Align := alClient;
  fTabsNotebook.Color := clWindow;
end;

destructor TSandcatContentArea.Destroy;
begin
  fTabsNotebook.Free;
  fExtensionEngine.Free;
  fNote.Free;
  inherited Destroy;
end;

// ------------------------------------------------------------------------//
// TSandcatPageBar                                                         //
// ------------------------------------------------------------------------//

procedure TSandcatPageBar.AdjustPageStrip(const SideBar: TSandcatSidebar);
begin
  e := fEngine.Root.Select('#tabstrip');
  if e <> nil then
    e.StyleAttr['margin-left'] := iif(SideBar.visible, inttostr(SideBar.Width) +
      'px', '0px');
end;

// default true
procedure TSandcatPageBar.SetStripVisible(const b: boolean);
begin
  fStripVisible := true;
  e := fEngine.Root.Select('#tabs');
  if e <> nil then
    e.StyleAttr['display'] := BoolToDisplayState(b);
end;

procedure TSandcatPageBar.EvalTIS(const s: string);
begin
  if s <> emptystr then
    fEngine.Eval(s);
end;

procedure TSandcatPageBar.AddPage(const name: string);
begin
  fEngine.Eval(format('Tabs.Add("%s","browser.setactivepage([[%s]])")',
    [Name, Name]));
end;

procedure TSandcatPageBar.ShowPage(const name: string; const visible: boolean);
begin
  e := fEngine.Root.Select('div[panel="' + name + '"]');
  if e <> nil then
    e.StyleAttr['display'] := BoolToDisplayState(visible);
end;

procedure TSandcatPageBar.SelectPage(const name: string);
begin
  fEngine.Eval('Tabs.Select("' + name + '");');
end;

procedure TSandcatPageBar.Load;
begin
  fEngine.LoadHTML(GetResourceAsString('PAGEBAR', 'HTML'), pluginsdir);
end;

constructor TSandcatPageBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  fEngine := TSandUIEngine.Create(MainPanel);
  fEngine.Parent := MainPanel;
  fEngine.Align := AlBottom;
  fEngine.height := 25;
  fEngine.OnonStdOut := uix.StdOut;
  fEngine.OnonStdErr := uix.StdErr;
end;

destructor TSandcatPageBar.Destroy;
begin
  fEngine.Free;
  inherited Destroy;
end;

// ------------------------------------------------------------------------//
// TSandcatBottomBar                                                       //
// ------------------------------------------------------------------------//

procedure TSandcatBottomBar.LoadBottomBar(const HTML: string);
var
  ht: string;
begin
  ViewBottomBar(true);
  ht := replacestr(uix.Pages.BottomBar, cContent, HTML);
  fEngine.LoadHTML(ht, pluginsdir);
end;

procedure TSandcatBottomBar.LoadBottomBarRight(const pagename: string);
begin
  LoadBottomBar
    ('<style>html {background-color:#f3f3f3 #ebebeb #f3f3f3 #ebebeb;} </style>');
  fEngine.Align := alright;
  fEngine.Width := 50;
  fNote.visible := true;
  fNote.ActivePage := pagename;
end;

procedure TSandcatBottomBar.ShowRequestBuilderBar;
begin
  LoadBottomBarRight('reqbuilder');
  ReqBuilder.Load;
end;

procedure TSandcatBottomBar.ShowURL(const URL: string;
  const source: string = '');
begin
  LoadBottomBarRight('browser');
  if fCrm = nil then
  begin // Creates Chromium
    fCrm := TCatChromium.Create(StatBar);
    fCrm.Parent := TPage(fNote.Pages.Objects[fNote.Pages.IndexOf('Browser')]);
    fCrm.Align := alClient;
  end;
  if source = emptystr then
    fCrm.Load(URL)
  else
    fCrm.LoadFromString(URL, source);
end;

procedure TSandcatBottomBar.ViewBottomBar(const b: boolean = true);
begin
  if b then
  begin
    fNote.visible := false;
    if fEngine.Align <> alClient then
      fEngine.Align := alClient;
    fSplitter.visible := true;
    fSplitter.Align := AlBottom;
    visible := true;
    PageBar.Top := BottomBar.Top + 10;
    fSplitter.Top := BottomBar.Top + 10;
  end
  else
  begin
    fSplitter.visible := false;
    visible := false;
    fEngine.LoadHTML(cBlank_Htm, pluginsdir);
  end;
end;

procedure TSandcatBottomBar.LoadSettings(settings, DefaultSettings: TSandJSON);
begin
  if fCrm <> nil then
    fCrm.LoadSettings(settings, DefaultSettings);
end;

procedure TSandcatBottomBar.Load;
begin
  fReqBuilder.SetWordWrap(true);
end;

constructor TSandcatBottomBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];

  fSplitter := TSplitter.Create(MainPanel);
  fSplitter.Parent := MainPanel;
  fSplitter.Width := 2;
  fSplitter.visible := false;
  fSplitter.Color := clBtnShadow;

  visible := false;
  height := 300;
  fEngine := TSandUIEngine.Create(Self);
  fEngine.Parent := Self;
  fEngine.Align := alClient;
  fEngine.OnonStdOut := uix.StdOut;
  fEngine.OnonStdErr := uix.StdErr;

  fNote := TNoteBook.Create(Self);
  fNote.Parent := Self;
  fNote.Color := clWindow;
  fNote.Align := alClient;
  fNote.Pages.add('codeedit');
  fNote.Pages.add('browser');
  fNote.Pages.add('reqbuilder');
  fNote.Pages.add('taskmon');
  fNote.ActivePage := 'Default';

  fReqBuilder := TSandRequestPanel.Create(Self);
  fReqBuilder.Parent :=
    TPage(fNote.Pages.Objects[fNote.Pages.IndexOf('reqbuilder')]);
  fReqBuilder.Align := alClient;
  fTaskMsgs := TTaskMessages.Create(Self);
  fTaskMsgs.Parent :=
    TPage(fNote.Pages.Objects[fNote.Pages.IndexOf('taskmon')]);
  fTaskMsgs.Align := alClient;
end;

destructor TSandcatBottomBar.Destroy;
begin
  fTaskMsgs.Free;
  fReqBuilder.Free;
  fNote.Free;
  fEngine.Free;
  fSplitter.Free;
  inherited Destroy;
end;

// ------------------------------------------------------------------------//
// TSandcatTabstrip                                                        //
// ------------------------------------------------------------------------//

procedure TSandcatTabstrip.AddTab(const name: string);
begin
  fEngine.Eval('Tabs.AddTab("' + name + '");');
end;

procedure TSandcatTabstrip.RemoveTab(const name: string);
begin
  fEngine.Eval('Tabs.RemoveTab("' + name + '");');
end;

procedure TSandcatTabstrip.SelectTab(const name: string);
begin
  fEngine.Eval('Tabs.SelectTab("' + name + '");');
end;

procedure TSandcatTabstrip.SetTabIcon(const tabid, src: string);
begin
  fe := fEngine.Root.Select('img#' + tabid);
  if fe <> nil then
    fe.StyleAttr['foreground-image'] := src;
end;

procedure TSandcatTabstrip.SetTabTitle(const tabid, newtitle: string);
begin
  fe := fEngine.Root.Select('code#' + tabid);
  if fe <> nil then
    fe.value := strmaxlen(newtitle, 25, true);
  fe := fEngine.Root.Select('popup#for-sandcat_' + tabid);
  if fe <> nil then
    fe.value := strmaxlen(newtitle, 30, true);
end;

procedure TSandcatTabstrip.EvalTIS(const s: string);
begin
  if s <> emptystr then
    fEngine.Eval(s);
end;

procedure TSandcatTabstrip.Load;
begin
  fEngine := TSandUIEngine.Create(Self);
  fEngine.Align := alClient;
  fEngine.Parent := Self;
  fEngine.OnonStdOut := uix.StdOut;
  fEngine.OnonStdErr := uix.StdErr;
  fEngine.LoadHTML(GetResourceAsString('TABBAR', 'HTML'), pluginsdir);
end;

constructor TSandcatTabstrip.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  height := 33;
  Color := clWindow;
end;

destructor TSandcatTabstrip.Destroy;
begin
  fEngine.Free;
  inherited Destroy;
end;

// ------------------------------------------------------------------------//
// TSandcatDialogs                                                         //
// ------------------------------------------------------------------------//

procedure TSandcatDialogs.AppMinimize(Sender: TObject);
begin
  application.Restore;
end;

procedure TSandcatDialogs.ShowCustomDialog(const HTML: string;
  const id: string = '');
const
  cScript = '<script type="text/tiscript">%s</script>';
var
  j: TSandJSON;
  page: string;
begin
  j := TSandJSON.Create;
  // Sciter dialog bug workaround
  page := DlgReplace(GetPakResourceAsString('dialog_custom.htm'));
  if id <> emptystr then
  begin
    if id = 'about' then
      page := format(cScript, [uix.TIS.DialogAbout]) + page
    else if id = 'prefs' then
      page := format(cScript, [uix.TIS.DialogPreferences]) + page;
  end;
  page := replacestr(page, cContent, HTML);
  j.sObject.s['html'] := page;
  // Prevent minimize while the dialog is shown (fix for minor Sciter bug)
  application.OnMinimize := AppMinimize;
  Tabstrip.Engine.Eval('SandcatUIX.ShowDialog(' + j.TextUnquoted + ');');
  application.OnMinimize := nil;
  j.Free;
end;

procedure TSandcatDialogs.ShowAlertText(const msg: string);
begin
  ShowAlert(
    '<plaintext style="width:300px;height:150px;overflow-x:none;margin:10px;" readonly="true">'
    + htmlescape(msg) + '</plaintext>', false);
end;

procedure TSandcatDialogs.ShowAlert(const msg: string;
  const escape_html: boolean = true);
var
  j: TSandJSON;
  amsg: string;
begin
  amsg := msg;
  if escape_html then
    amsg := htmlescape(amsg);
  j := TSandJSON.Create;
  j['msg'] := amsg;
  j['title'] := vAppNameShort;
  Tabstrip.Engine.Eval('SandcatUIX.ShowAlert(' + j.TextUnquoted + ')');
  j.Free;
end;

procedure TSandcatDialogs.ShowMessage(const msg: string;
  const escape: boolean = true);
var
  j: TSandJSON;
  s: string;
begin
  s := msg;
  j := TSandJSON.Create;
  if escape = true then
    s := htmlescape(s);
  j['msg'] := s;
  j['title'] := vAppNameShort;
  Tabstrip.Engine.Eval('SandcatUIX.ShowMessage(' + j.TextUnquoted + ')');
  j.Free;
end;

procedure TSandcatDialogs.ShowHTMLMessage(const HTML: string);
begin
  ShowMessage(HTML, false);
end;

// Workaround for path location Sciter issues with the view.dialog() function
function TSandcatDialogs.DlgReplace(const s: string): string;
begin
  result := replacestr(s, '{$PluginsDir}', GetSandcatDir(SCDIR_PLUGINS));
end;

// Saves a resource as a file from the cache or from the web/cloud
procedure TSandcatDialogs.SaveResource(const URL: string;
  const fromcloud: boolean = false);
begin
  if fromcloud = true then
    SandDlg.SaveURLAs(URL)
  else
  begin
    Extensions.LuaWrap.value['_temppath'] := URL;
    Extensions.RunLuaCmd('PageMenu:SaveCachedAs(_temppath)');
  end;
end;

procedure TSandcatDialogs.SaveURLAs(const URL: string);
var
  j: TSandJSON;
begin
  j := TSandJSON.Create;
  j['url'] := URL;
  Tabstrip.Engine.Eval(format('SandcatDownloader.SaveURL_AsJSON(%s)',
    [j.TextUnquoted]));
  j.Free;
end;

constructor TSandcatDialogs.Create;
begin
  inherited Create;
end;

destructor TSandcatDialogs.Destroy;
begin
  inherited Destroy;
end;

// ------------------------------------------------------------------------//
end.
