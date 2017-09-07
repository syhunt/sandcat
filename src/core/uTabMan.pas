unit uTabMan;

{
  Sandcat Tab Manager
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Windows, Messages, Classes, Forms, Controls, Graphics, uTab, SysUtils,
  ExtCtrls, Dialogs, ShellAPI, uUIComponents;

type
  TCustomTabSettings = record
    ActivePage: string;
    HTML: string;
    Icon: string;
    LoadNew: boolean;
    ShowNavBar: boolean;
    ShowPageStrip: boolean;
    Table: string;
    Tag: string;
    Title: string;
    Toolbar: string;
  end;

type
  TSandcatTabManager = class
  private
    fActiveTab: TSandcatTab;
    fActiveTabName: string;
    fLiveNotebook: TNoteBook;
    fTabCount: integer;
    fTabsCache: TSandObjCache;
    fTabsNotebook: TNoteBook;
    fHSplitter: TSplitter;
    procedure Debug(const s: string);
  public
    function DestroyTab(tab: TSandcatTab; IsShutDown: boolean = false): boolean;
    function GetActivePageTab: TSandcatTab;
    function GetTabDefaultSettings: TCustomTabSettings;
    function GetTab(const name: string): TSandcatTab;
    function GetTabByTag(const Tag: string): TSandcatTab;
    function NewTab(const url: string = ''; const source: string = '';
      const hascustomtb: boolean = false; const inbg: boolean = false)
      : TSandcatTab;
    function NewTab_Custom(t: TCustomTabSettings): TSandcatTab;
    function NewTab_Search(const term:string): TSandcatTab;
    procedure CloseAllTabs(const Silent: boolean = false;
      But: TSandcatTab = nil);
    procedure CloseTab(const tabname: string = '');
    procedure GoToTab(const tabname: string);
    procedure HandleDropFiles(var Msg: TMessage);
    procedure LoadWelcomePage(const url: string);
    procedure NewWindow(const url: string = '');
    procedure OpenURLList(const list: TStringList);
    procedure ReConfigureAllTabs;
    procedure SetAppTitle(const s: string);
    procedure TabMessage(ASender: TObject; const msgid: integer;
      const msg: array of string);
    procedure ViewHeaders(const visible: boolean = true;
      const updatenavbar: boolean = true);
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    // properties
    property ActiveTab: TSandcatTab read fActiveTab;
    property ActiveTabID: string read fActiveTabName write fActiveTabName;
  end;

implementation

uses uMain, uZones, uConst, uMisc, CatChromium, CatChromiumLib, CatHTTP,
  CatStrings;

const
  cDefaultNewTabTitle = 'New Tab';

procedure TSandcatTabManager.Debug(const s: string);
begin
  uMain.Debug(s, 'TabMan');
end;

procedure TSandcatTabManager.HandleDropFiles(var Msg: TMessage);
var
  filename: array [0 .. 255] of WideChar;
  dropcount, i: integer;
  files: TStringList;
begin
  dropcount := DragQueryFile(TWMDropFiles(Msg).Drop, $FFFFFFFF, nil, 0);
  if dropcount >= 1 then
  begin
    files := TStringList.Create;
    for i := 0 to dropcount - 1 do
    begin
      DragQueryFile(TWMDropFiles(Msg).Drop, i, filename, 255);
      files.add(filename);
    end;
    TabManager.OpenURLList(files);
    files.Free;
  end;
  DragFinish(TWMDropFiles(Msg).Drop);
  Msg.result := 0;
end;

procedure TSandcatTabManager.OpenURLList(const list: TStringList);
var
  SLP: TSandSLParser;
begin
  SLP := TSandSLParser.Create;
  SLP.Load(list);
  while SLP.found do
    NewTab(SLP.current);
  SLP.Free;
end;

procedure TSandcatTabManager.LoadWelcomePage(const url: string);
begin
  if url = emptystr then // Standard launch
    NewTab(settings.StartupHomepage)
  else
  begin
    // Opens URL if passed via command-line parameter
    // Special parameters mode: and newwin: are handled differently
    if beginswith(url, cModeParam) then begin
      Extensions.RunInitMode(after(url, ':'));
      navbar.Cover.Visible := false;
    end else if beginswith(url, cNewWinParam) then
      NewTab(cURL_HOME)
    else
      NewTab(url);
  end;
end;

procedure TSandcatTabManager.ViewHeaders(const visible: boolean = true;
  const updatenavbar: boolean = true);
begin
  if visible then
    fLiveNotebook.Height := 160
  else
    fLiveNotebook.Height := 0;
  fHSplitter.visible := visible;
  fHSplitter.Top := fLiveNotebook.Height + 1;
  if updatenavbar = false then
    exit;
  headersvisible := visible;
  Navbar.headersvisible := visible;
end;

procedure TSandcatTabManager.SetAppTitle(const s: string);
var
  Title: string;
begin
  Title := s;
  if Title = emptystr then
    Title := cDefaultNewTabTitle;
  sandbrowser.caption := Title + ' - ' + vAppNameShort;
  application.Title := sandbrowser.caption;
end;

procedure TSandcatTabManager.TabMessage(ASender: TObject; const msgid: integer;
  const msg: array of string);
var
  tab: TSandcatTab;
begin
  tab := TSandcatTab(ASender);
  case msgid of
    SCBT_GETSCREENSHOT:
      contentarea.SetActivePage('browser');
    SCBT_NEWTITLE:
      begin
        if fActiveTab.UID = tab.UID then
          SetAppTitle(msg[0]);
        tabstrip.SetTabTitle(tab.UID, msg[0]);
      end;
    SCBT_STATUS:
      if fActiveTab.UID = tab.UID then
        StatBar.Text := msg[0];
    SCBT_LOADSTART, SCBT_GOTOURL:
      tabstrip.SetTabIcon(tab.UID, '@ICON_LOADING');
    SCBT_URLCHANGE:
      if fActiveTab.UID = tab.UID then
        Navbar.url := msg[0];
    SCBT_LOADEND, SCBT_LOADERROR:
      tabstrip.SetTabIcon(tab.UID, tab.Icon);
  end;
end;

procedure TSandcatTabManager.CloseAllTabs(const Silent: boolean = false;
  But: TSandcatTab = nil);
var
  m, c: integer;
  tab: TSandcatTab;
  candestroy: boolean;
begin
  Debug('closealltabs');
  m := fTabsCache.Count;
  for c := m - 1 downto 0 do
  begin
    tab := TSandcatTab(fTabsCache.ObjectAt(c));
    candestroy := true;
    if tab <> nil then
    begin
      if But <> nil then
      begin
        if tab = But then
          candestroy := false;
      end;
      if candestroy then
        DestroyTab(tab, Silent);
    end;
  end;
  if But <> nil then
    tabmanager.GoToTab(But.UID);
  Debug('closealltabs.end');
end;

procedure TSandcatTabManager.CloseTab(const tabname: string = '');
var
  c, m: integer;
  tn, lasttab: string;
  foundtab: boolean;
  tab, tabtodestroy: TSandcatTab;
begin
  Debug('closetab');
  tn := tabname;
  if fTabsCache.Count = 1 then
    exit; // Just one tab is open, exiting...
  if tn = emptystr then
  begin
    // If no tabname was specified, assign the current tab
    if fActiveTab <> nil then
      tn := fTabsNotebook.ActivePage;
  end;

  m := fTabsCache.Count;
  foundtab := false;
  tabtodestroy := nil;
  for c := m - 1 downto 0 do
  begin
    if fTabsCache.ObjectAt(c) <> nil then
    begin
      tab := TSandcatTab(fTabsCache.ObjectAt(c));
      with tab do
      begin
        if UID = tn then
        begin
          foundtab := true;
          tabtodestroy := tab;
        end
        else
          lasttab := UID;
      end;
    end;
  end;
  if foundtab = false then
    exit;
  if tabtodestroy.isclosing then
    exit;
  if DestroyTab(tabtodestroy) then
  begin
    Debug('sb.closetab.tabdestroyed');
    GoToTab(lasttab);
  end;
  Debug('closetab.end');
end;

function TSandcatTabManager.DestroyTab(tab: TSandcatTab;
  IsShutDown: boolean = false): boolean;
var
  name: string;
  i: integer;
begin
  result := false;
  if tab = nil then
    exit;
  if tab.isclosing then
    exit;
  if tab.close(IsShutDown) then
  begin
    Debug('destroytab:' + tab.UID);
    name := tab.UID;
    fTabsCache.Remove(tab);
    if IsShutDown = false then
    begin
      tabstrip.RemoveTab(name);
      i := fTabsNotebook.Pages.IndexOf(name);
      if (i <> -1) then
        fTabsNotebook.Pages.Delete(i);
      i := fLiveNotebook.Pages.IndexOf(name);
      if (i <> -1) then
        fLiveNotebook.Pages.Delete(i);
      i := Navbar.note.Pages.IndexOf(name);
      if (i <> -1) then
        Navbar.note.Pages.Delete(i);
    end;
    result := true;
    Debug('destroytab.end');
  end;
end;

function TSandcatTabManager.GetTabByTag(const Tag: string): TSandcatTab;
var
  c, m: integer;
  tab: TSandcatTab;
begin
  result := nil;
  m := fTabsCache.Count;
  for c := m - 1 downto 0 do
  begin
    if result <> nil then
      exit; // found the tab
    tab := TSandcatTab(fTabsCache.ObjectAt(c));
    if tab <> nil then
    begin
      if tab.usertag = Tag then
        result := tab;
    end;
  end;
end;

function TSandcatTabManager.GetTab(const name: string): TSandcatTab;
var
  c, m: integer;
  tab: TSandcatTab;
begin
  result := nil;
  m := fTabsCache.Count;
  for c := m - 1 downto 0 do
  begin
    if result <> nil then
      exit; // found the tab
    tab := TSandcatTab(fTabsCache.ObjectAt(c));
    if tab <> nil then
    begin
      if tab.UID = name then
        result := tab;
    end;
  end;
end;

function TSandcatTabManager.GetActivePageTab: TSandcatTab;
var
  c, m: integer;
  tab: TSandcatTab;
begin
  result := nil;
  m := fTabsCache.Count;
  for c := m - 1 downto 0 do
  begin
    if result <> nil then
      exit;
    tab := TSandcatTab(fTabsCache.ObjectAt(c));
    if tab <> nil then
    begin
      if tab.UID = fTabsNotebook.ActivePage then
        result := tab;
    end;
  end;
end;

procedure TSandcatTabManager.GoToTab(const tabname: string);
begin
  Debug('gototab:' + tabname);
  fTabsNotebook.ActivePage := tabname;
  fLiveNotebook.ActivePage := tabname;
  fActiveTab := GetActivePageTab;
  fActiveTabName := tabname;
  if fActiveTab.State.IsCustom = false then
    tabmanager.ViewHeaders(headersvisible, false)
  else
    tabmanager.ViewHeaders(false, false);
  fActiveTab.LoadState;
  SetAppTitle(fActiveTab.Title);
  if Navbar.url = emptystr then
    Navbar.focusurl;
  tabstrip.SelectTab(tabname);
end;

function TSandcatTabManager.GetTabDefaultSettings: TCustomTabSettings;
begin
  result.ActivePage := 'extension';
  result.HTML := emptystr;
  result.Icon := emptystr;
  result.LoadNew := false;
  result.ShowNavBar := true;
  result.ShowPageStrip := false;
  result.Table := emptystr;
  result.Tag := emptystr;
  result.Title := emptystr;
  result.Toolbar := emptystr;
end;

function TSandcatTabManager.NewTab_Custom(t: TCustomTabSettings): TSandcatTab;
var
  tab: TSandcatTab;
var
  cancreate: boolean;
begin
  tab := nil;
  if t.HTML = emptystr then
    t.HTML := cBlank_Htm;
  cancreate := true;
  if t.Tag <> emptystr then
  begin // checks for any tab with this unique tag
    tab := GetTabByTag(t.Tag);
    if tab <> nil then
    begin
      GoToTab(tab.UID);
      if t.LoadNew = true then
        contentarea.LoadPage(t.HTML);
      cancreate := false;
    end;
  end;
  if cancreate then
  begin
    tab := NewTab(emptystr, emptystr, true);
    tab.usertag := t.Tag;
    tab.State.IsCustom := true;
    tab.State.HasCustomToolbar := true;
    t.Table := htmlescape(t.Table);
    t.Toolbar := htmlescape(t.Toolbar);
    tab.State.CustomDefaultPage := t.ActivePage;
    if t.Table <> emptystr then
      t.HTML := '<meta name="SandcatUIX" content="' + t.Table + '">' +
        crlf + t.HTML;
    if t.ShowNavBar = false then
    begin
      Navbar.Height := 0;
      tab.State.ShowNavBar := false;
    end;
    tabmanager.ViewHeaders(false, false);
    tab.State.ShowTabsStrip := t.ShowPageStrip;
    pagebar.StripVisible := tab.State.ShowTabsStrip;
    tab.SubTabs.ActivePage := 'extension';
    if t.Title <> emptystr then
      tab.SetTitle(t.Title);
    if t.Icon <> emptystr then
      tab.SetIcon(t.Icon, true);
    fActiveTab.LoadExtensionPage(t.HTML);
    if t.ActivePage <> emptystr then
      tab.SetActivePage(t.ActivePage);
    if t.Toolbar <> emptystr then
    begin
      if t.Table <> emptystr then
        tab.LoadExtensionToolbar('<meta name="SandcatUIX" content="' + t.Table +
          '">' + '<include src="' + t.Toolbar + '"/>')
      else
        tab.LoadExtensionToolbar('<include src="' + t.Toolbar + '"/>');
    end
    else
      tab.LoadExtensionToolbar(emptystr); // Loads an empty toolbar
  end;
  result := tab;
end;

function TSandcatTabManager.NewTab(const url: string = '';
  const source: string = ''; const hascustomtb: boolean = false;
  const inbg: boolean = false): TSandcatTab;
var
  tab: TSandcatTab;
  uniqueid: string;
begin
  Debug('newtab.begin');
  fTabCount := fTabCount + 1;
  if uniqueid = emptystr then
    uniqueid := cTabNamePrefix + inttostr(fTabCount);
  fTabsNotebook.Pages.Add(uniqueid);
  fLiveNotebook.Pages.Add(uniqueid);
  fTabsNotebook.ActivePage := uniqueid;
  fLiveNotebook.ActivePage := uniqueid;
  tab := TSandcatTab.Create(sandbrowser);
  Debug('newtab.created');
  if hascustomtb then
    tab.State.HasCustomToolbar := true;
  tab.LiveHeaders.Parent :=
    TPage(fLiveNotebook.Pages.Objects[fLiveNotebook.PageIndex]);
  tab.OnMessage := TabMessage;
  tab.Parent := TPage(fTabsNotebook.Pages.Objects[fTabsNotebook.PageIndex]);
  tab.UID := uniqueid;
  tab.Number := fTabCount;
  contentarea.note.ActivePage := 'default';
  if tasks.running then
    Navbar.AnimateTasksIcon(true);
  Navbar.UpdateSearchEngine;
  fTabsCache.Add(tab);
  tabstrip.AddTab(tab.UID);

  if inbg = false then
  begin
    fActiveTab := tab;
    tabmanager.GoToTab(tab.UID);
  end
  else
  begin
    fTabsNotebook.ActivePage := fActiveTab.UID;
    fLiveNotebook.ActivePage := fActiveTab.UID;
    tabmanager.GoToTab(fActiveTab.UID);
  end;

  if url <> emptystr then
    tab.GoToURL(url, source)
  else
    Navbar.url := emptystr;

  result := tab;
  Debug('newtab.end');
end;

function TSandcatTabManager.NewTab_Search(const term:string): TSandcatTab;
begin
  result := newtab(vSearchEngine_QueryURL + term);
end;

procedure TSandcatTabManager.NewWindow(const url: string = '');
var
  s: string;
begin
  s := url;
  if s = emptystr then
    s := cNewWinParam;
  ShellExecute(0, nil, pWideChar(ProgDir + cExecutableFile), pWideChar(s),
    pWideChar(ProgDir), SW_SHOWNORMAL);
end;

procedure TSandcatTabManager.ReConfigureAllTabs;
var
  m, c: integer;
  tab: TSandcatTab;
begin
  m := fTabsCache.Count;
  for c := m - 1 downto 0 do
  begin
    tab := TSandcatTab(fTabsCache.ObjectAt(c));
    if tab <> nil then
      tab.LoadSettings;
  end;
  BottomBar.LoadSettings(settings.preferences.current,
    settings.preferences.Default);
end;

constructor TSandcatTabManager.Create(AOwner: TWinControl);
begin
  inherited Create;
  fTabCount := 0;
  tabstrip := TSandcatTabstrip.Create(AOwner);
  tabstrip.Parent := AOwner;
  tabstrip.Align := altop;
  fTabsNotebook := contentarea.TabsNotebook;
  // live headers begin
  fHSplitter := TSplitter.Create(AOwner);
  fHSplitter.Parent := contentarea;
  fHSplitter.Color := clBtnShadow;
  fHSplitter.Width := 2;
  fHSplitter.Align := altop;
  fHSplitter.visible := false;
  fLiveNotebook := TNoteBook.Create(AOwner);
  fLiveNotebook.Parent := contentarea;
  fLiveNotebook.Align := altop;
  fLiveNotebook.Height := 0; // Hides it from view
  // live headers end
  fTabsCache := TSandObjCache.Create(1000, true);
end;

destructor TSandcatTabManager.Destroy;
begin
  Debug('destroy.begin');
  fTabsCache.Free;
  fHSplitter.Free;
  fLiveNotebook.Free;
  tabstrip.Free;
  Debug('destroy.end');
  inherited;
end;

end.
