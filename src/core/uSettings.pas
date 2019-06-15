unit uSettings;
{
  Sandcat Settings Manager
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

{$I Catarinka.inc}

uses
  Windows, Classes, Dialogs, Messages, Forms, SysUtils, Controls, Variants,
{$IFDEF USEWACEF}
  waceflib, WACefInterfaces, WACefTypes,
{$ELSE}
  ceflib,
{$ENDIF}
  uUIComponents, CatPrefs;

type
  TSandcatStartupSettings = record
    ProxyServer: string;
    Anonymize: boolean;
    AuditXSS: boolean;
    AllowMultipleInstances: boolean;
    UserAgent: string;
  end;

type
  TSandcatSettings = class
  private
    fCacheFilesInUse: boolean;
    fJSONValues: TSandJSON;
    fPreferences: TCatPreferences;
    function GetStartupHomepage: string;
    procedure DeleteCacheFile(const filename: string;
      const journal: boolean = false);
  public
    function GetSitePrefsFilename(const url: string): string;
    function ReadJSValue(const Key: string): Variant;
    procedure AddToBookmarks(const PageName, url: string);
    procedure AddToHistory(const PageName, url: string);
    procedure AddToURLList(const PageName, url: string; const HistFile: string;
      const Limit: integer = 0);
    procedure ClearPrivateData(const DataType: string = '');
    procedure Load;
    procedure OnbeforeCmdLineWACEF(const processType: ustring;
      const commandLine: ICefCommandLine);
    procedure Update;
    procedure Save;
    procedure WriteJSValue(const Key: string; const Value: Variant);
    // procedure WriteJSValue_FromJSON(json: string);
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    property Preferences: TCatPreferences read fPreferences;
    property StartupHomepage: string read GetStartupHomepage;
  end;

const // Sandcat Settings
  SCO_STARTUP_HOMEPAGE = 'sandcat.startup.homepage';
  SCO_STARTUP_WELCOME_METHOD = 'sandcat.startup.welcomemethod';
  SCO_STARTUP_MULTIPLE_INSTANCES = 'sandcat.startup.multiwin';
  SCO_CONSOLE_FONT_COLOR = 'sandcat.console.font.color';
  SCO_CONSOLE_BGCOLOR = 'sandcat.console.bgcolor';
  SCO_EXTENSIONS_ENABLED = 'sandcat.extensions.enabled';
  SCO_EXTENSION_ENABLED_PREFIX = 'sandcat.extensions.scx';
  SCO_FORM_STATE = 'sandcat.form.state';
  SCO_FORM_TOP = 'sandcat.form.top';
  SCO_FORM_LEFT = 'sandcat.form.left';
  SCO_FORM_HEIGHT = 'sandcat.form.height';
  SCO_FORM_WIDTH = 'sandcat.form.width';
  SCO_USERAGENT = 'sandcat.browser.useragent';
  SCO_PROXY_SERVER = 'sandcat.browser.proxy.server';
  SCO_PROXY_ANONYMIZE = 'sandcat.browser.proxy.anonymize';
  SCO_SEARCHENGINE_NAME = 'sandcat.search.name';
  SCO_SEARCHENGINE_ICON = 'sandcat.search.icon';
  SCO_SEARCHENGINE_QUERYURL = 'sandcat.search.queryurl';
  SCO_SECURITY_XSSAUDITOR_ENABLED =
    'sandcat.browser.security.xssauditor.enabled';
  SCO_OPTIONS_OPENPOPUPSINNEWTAB =
    'sandcat.options.openpopupsinnewtab';

var
  IsSandcatPortable: boolean = false;

function GetAppDataDir: string;
function GetStartupSettings: TSandcatStartupSettings;
function GetSandcatDir(dir: integer; Create: boolean = false): string;
procedure OnbeforeCmdLine(const processType: ustring;
  const commandLine: ICefCommandLine);

implementation

uses uMain, uMisc, uConst, CatChromium, CatChromiumLib, CatUI, CatTime,
  CatStrings, CatFiles, CatHTTP;

procedure OnbeforeCmdLine(const processType: ustring;
  const commandLine: ICefCommandLine);
var
  s: TSandcatStartupSettings;
begin
  s := GetStartupSettings;
  commandLine.AppendSwitch('--enable-system-flash');
  if FileExists(GetSandcatDir(SCDIR_PLUGINS) + 'PenTools.scx') then
    s.AuditXSS := false;
  if s.ProxyServer <> emptystr then
  begin
    commandLine.AppendSwitchWithValue('proxy-server', s.ProxyServer);
    commandLine.AppendSwitchWithValue('host-resolver-rules',
      'MAP * 0.0.0.0 , EXCLUDE 127.0.0.1');
    if s.Anonymize = true then
    begin
      // Keep XSS protection enabled
      s.AuditXSS := true;
      // Prevent plugins from running
      commandLine.AppendSwitch('disable-plugins');
      // Disable restoring session state (cookies, session storage, etc.) when
      // restoring the browsing session
      commandLine.AppendSwitch('disable-restore-session-state');
    end;
  end;
  if s.AuditXSS = false then
    commandLine.AppendSwitch('--disable-xss-auditor');
  // if commandLine.IsValid then ShowMessage(commandLine.CommandLineString);
end;

function GetStartupSettings: TSandcatStartupSettings;
var
  j: TSandJSON;
  jf: string;
begin
  jf := GetSandcatDir(SCDIR_CONFIG) + vConfigFile;
  j := TSandJSON.Create;
  if FileExists(jf) then
    j.loadfromfile(jf);
  result.ProxyServer := j.getvalue(SCO_PROXY_SERVER, emptystr);
  result.Anonymize := j.getvalue(SCO_PROXY_ANONYMIZE, false);
  result.AuditXSS := j.getvalue(SCO_SECURITY_XSSAUDITOR_ENABLED, true);
  result.AllowMultipleInstances :=
    j.getvalue(SCO_STARTUP_MULTIPLE_INSTANCES, false);
  result.UserAgent := j.getvalue(SCO_USERAGENT, emptystr);
  j.Free;
end;

function GetAppDataDir: string;
begin
  if IsSandcatPortable then
    result := extractfilepath(paramstr(0))
  else
    result := GetSpecialFolderPath(CSIDL_LOCAL_APPDATA, true) +
      '\Syhunt\Sandcat\';
end;

function GetSandcatDir(dir: integer; Create: boolean = false): string;
var
  s, progdir: string;
begin
  progdir := extractfilepath(paramstr(0));
  case dir of
    SCDIR_CACHE:
      s := GetAppDataDir + 'Cache\';
    SCDIR_PLUGINS:
      s := progdir + 'Packs\Extensions\';
    SCDIR_CONFIG:
      s := GetAppDataDir + 'Config\';
    SCDIR_CONFIGSITE:
      s := GetAppDataDir + 'Config\Site\';
    SCDIR_LOGS:
      s := GetAppDataDir + 'Logs\';
    SCDIR_HEADERS:
      s := GetAppDataDir + 'Cache\Headers\';
    SCDIR_PREVIEW:
      s := GetAppDataDir + 'Temp\Preview\';
    SCDIR_TEMP:
      s := GetAppDataDir + 'Temp\';
    SCDIR_TASKS:
      s := GetAppDataDir + 'Temp\Tasks\';
  end;
  if Create then
    forcedir(s);
  result := s;
end;

procedure TSandcatSettings.OnbeforeCmdLineWACEF(const processType: ustring;
  const commandLine: ICefCommandLine);
begin
  OnbeforeCmdLine(processType, commandLine);
end;

// Returns the filename of a site preferences file. This is a JSON file that
// can be used for storing user preferences for each specific host:port
function TSandcatSettings.GetSitePrefsFilename(const url: string): string;
begin
  result := Format('%s [%s].json', [ExtractURLHost(url),
    IntToStr(ExtractURLPort(url))]);
  result := CleanFilename(result);
  result := GetSandcatDir(SCDIR_CONFIGSITE, true) + result;
end;

function TSandcatSettings.GetStartupHomepage: string;
var
  method: string;
const
  defaultmethod = 'blank';
begin
  result := emptystr;
  method := Settings.Preferences.getvalue(SCO_STARTUP_WELCOME_METHOD,
    defaultmethod);
  if method = defaultmethod then
    result := cURL_HOME;
  if method = 'homepage' then
    result := Settings.Preferences.getvalue(SCO_STARTUP_HOMEPAGE, emptystr);
  if result = emptystr then
    result := cURL_HOME;
end;

procedure TSandcatSettings.AddToBookmarks(const PageName, url: string);
begin
  AddToURLList(PageName, url, cBookmarksFile);
end;

procedure TSandcatSettings.AddToHistory(const PageName, url: string);
begin
  AddToURLList(PageName, url, cHistoryFile, 100);
end;

procedure TSandcatSettings.AddToURLList(const PageName, url: string;
  const HistFile: string; const Limit: integer = 0);
var
  history: tstringlist;
  hfile, id, page, pageurl: string;
  canadd: boolean;
begin
  pageurl := url;
  if beginswith(lowercase(pageurl), 'http') = false then
    exit;
  history := tstringlist.Create;
  hfile := GetSandcatDir(SCDIR_CONFIG) + HistFile;
  canadd := true;
  page := htmlescape(PageName);
  pageurl := htmlescape(pageurl);
  if FileExists(hfile) then
  begin
    if filecanbeopened(hfile) then
    begin
      SL_LoadFromFile(history, hfile);
    end
    else
      canadd := false;
  end;
  if canadd then
  begin
    id := IntToStr(DateTimeToUnix(now)) + '-' + IntToStr(history.count);
    if Limit <> 0 then
    begin // If it is 0 then there is no item limit
      if history.count >= Limit then
        history.Delete(history.count - 1);
    end;
    history.insert(0, '<item id="' + id + '" url="' + pageurl + '" name="' +
      page + '" visited="' + DateTimeToStr(now) + '"/>');
    SL_SaveToFile(history, hfile);
  end;
  history.Free;
end;

{ procedure TSandcatSettings.WriteJSValue_FromJSON(json: string);
  var
  j: TSandJSON;
  Key: string;
  Value: Variant;
  begin
  j := TSandJSON.Create;
  j.Text := json;
  Key := j.sObject.s['k'];
  Value := j['v'];
  WriteJSValue(Key, Value);
  j.Free;
  end; }

procedure TSandcatSettings.WriteJSValue(const Key: string;
  const Value: Variant);
begin
  fJSONValues[Key] := Value;
end;

function TSandcatSettings.ReadJSValue(const Key: string): Variant;
begin
  result := Settings.fJSONValues[Key];
end;

procedure TSandcatSettings.DeleteCacheFile(const filename: string;
  const journal: boolean = false);
begin
  if FileExists(filename) = false then
    exit;
  if filecanbeopened(filename) then
  begin
    deletefile(filename);
    if journal then
      deletefile(filename + '-journal');
  end
  else
    fCacheFilesInUse := true;
end;

procedure TSandcatSettings.ClearPrivateData(const DataType: string = '');
var
  cachedir, configdir: string;
  slp: TSandSLParser;
begin
  cachedir := GetSandcatDir(SCDIR_CACHE);
  configdir := GetSandcatDir(SCDIR_CONFIG);
  if DataType = 'beginclear' then
    fCacheFilesInUse := false;
  if DataType = 'check' then
  begin
    if fCacheFilesInUse then
      showmessage
        ('Error: Not all data was deleted. Try again after restarting Sandcat.');
  end;
  if DataType = 'all' then
  begin
    DeleteFolder(cachedir);
    forcedir(cachedir); // Recreates it
  end;
  if DataType = 'cache' then
  begin
    deletefile(cachedir + 'index');
    slp := TSandSLParser.Create;
    GetFiles(cachedir + '*.*', slp.List, true, true);
    while slp.Found do
    begin
      if beginswith(extractfilename(slp.Current), 'data_') then
        DeleteCacheFile(slp.Current);
      if beginswith(extractfilename(slp.Current), 'f_') then
        DeleteCacheFile(slp.Current);
    end;
    slp.Free;
  end;
  if DataType = 'appcache' then
  begin
    DeleteFolder(cachedir + 'Application Cache\');
    DeleteCacheFile(cachedir + 'QuotaManager', true);
  end;
  if DataType = 'cookies' then
  begin
    DeleteCacheFile(cachedir + 'Cookies', true);
  end;
  if DataType = 'databases' then
  begin
    DeleteFolder(cachedir + 'databases\');
    // Web Storage and DOM Storage, Web SQL Database
    DeleteFolder(cachedir + 'Local Storage\');
    DeleteFolder(cachedir + 'IndexedDB\');
  end;
  if DataType = 'history' then
  begin
    DeleteCacheFile(configdir + cHistoryFile, false);
  end;
  if DataType = 'settings' then
  begin
    deletefile(fPreferences.filename);
    fPreferences.restoredefaults;
  end;
end;

procedure TSandcatSettings.Update;
begin
  tabmanager.ReconfigureAllTabs;
end;

procedure TSandcatSettings.Load;
var
  State: integer;
  procedure load_default_settings;
  begin
    fPreferences.OptionList.Text := GetCEFDefaults(fPreferences.Default);
    // Registers the default settings
    fPreferences.RegisterDefault(SCO_STARTUP_WELCOME_METHOD, 'blank');
    fPreferences.RegisterDefault(SCO_STARTUP_HOMEPAGE, emptystr);
    fPreferences.RegisterDefault(SCO_USERAGENT, emptystr);
    fPreferences.RegisterDefault(SCO_PROXY_SERVER, emptystr);
    fPreferences.RegisterDefault(SCO_PROXY_ANONYMIZE, false);
    fPreferences.RegisterDefault(SCO_EXTENSIONS_ENABLED, true);
    fPreferences.RegisterDefault(SCO_STARTUP_MULTIPLE_INSTANCES, false);
    fPreferences.RegisterDefault(SCO_CONSOLE_BGCOLOR, '#262626');
    fPreferences.RegisterDefault(SCO_CONSOLE_FONT_COLOR, '#ffffff');
    fPreferences.RegisterDefault(SCO_SECURITY_XSSAUDITOR_ENABLED, true);
    fPreferences.RegisterDefault(SCO_OPTIONS_OPENPOPUPSINNEWTAB, false);

  end;

begin
  load_default_settings;
  if FileExists(fPreferences.filename) then
    fPreferences.loadfromfile(fPreferences.filename);
  State := fPreferences.Current.getvalue(SCO_FORM_STATE, Ord(wsNormal)); // int
  sandbrowser.Top := fPreferences.Current.getvalue(SCO_FORM_TOP,
    sandbrowser.Top); // int
  sandbrowser.Left := fPreferences.Current.getvalue(SCO_FORM_LEFT,
    sandbrowser.Left); // int
  sandbrowser.Height := fPreferences.Current.getvalue(SCO_FORM_HEIGHT,
    sandbrowser.Height); // int
  sandbrowser.Width := fPreferences.Current.getvalue(SCO_FORM_WIDTH,
    sandbrowser.Width); // int
  // str
  vSearchEngine_Name := fPreferences.Current.getvalue(SCO_SEARCHENGINE_NAME,
    vSearchEngine_Name); // str
  vSearchEngine_QueryURL := fPreferences.Current.getvalue
    (SCO_SEARCHENGINE_QUERYURL, vSearchEngine_QueryURL); // str
  vSearchEngine_Icon := fPreferences.Current.getvalue(SCO_SEARCHENGINE_ICON,
    vSearchEngine_Icon); // str
  if State = Ord(wsMinimized) then
  begin
    sandbrowser.Visible := true;
    Application.Minimize;
  end
  else
  begin
    if State = Ord(wsMaximized) then
      SendMessage(sandbrowser.Handle, WM_SYSCOMMAND, SC_MAXIMIZE, 0)
    else
      sandbrowser.WindowState := TWindowState(State);
  end;
end;

procedure TSandcatSettings.Save;
var
  Pl: TWindowPlacement;
  R: TRect;
  State: integer;
begin
  Pl.Length := SizeOf(TWindowPlacement);
  GetWindowPlacement(sandbrowser.Handle, @Pl);
  R := Pl.rcNormalPosition;
  if IsIconic(Application.Handle) then
    State := Ord(wsMinimized)
  else
    State := Ord(sandbrowser.WindowState);
  fPreferences.setvalue(SCO_FORM_STATE, State); // int
  fPreferences.setvalue(SCO_FORM_HEIGHT, R.Bottom - R.Top); // int
  fPreferences.setvalue(SCO_FORM_WIDTH, R.Right - R.Left); // int
  fPreferences.setvalue(SCO_FORM_TOP, R.Top); // int
  fPreferences.setvalue(SCO_FORM_LEFT, R.Left); // int
  if vSearchEngine_Name <> emptystr then
    fPreferences.setvalue(SCO_SEARCHENGINE_NAME, vSearchEngine_Name); // str
  if vSearchEngine_QueryURL <> emptystr then
    fPreferences.setvalue(SCO_SEARCHENGINE_QUERYURL, vSearchEngine_QueryURL);
  // str
  if vSearchEngine_Icon <> emptystr then
    fPreferences.setvalue(SCO_SEARCHENGINE_ICON, vSearchEngine_Icon); // str
  fPreferences.SaveToFile(fPreferences.filename);
end;

constructor TSandcatSettings.Create(AOwner: TWinControl);
begin
  inherited Create;
  // Creates the temporary directories
  forcedir(GetSandcatDir(SCDIR_HEADERS));
  forcedir(GetSandcatDir(SCDIR_TEMP));
  fPreferences := TCatPreferences.Create;
  fPreferences.filename := (GetSandcatDir(SCDIR_CONFIG, true) + vConfigFile);
  fJSONValues := TSandJSON.Create;
  fCacheFilesInUse := false;
end;

destructor TSandcatSettings.Destroy;
begin
  fJSONValues.Free;
  fPreferences.Free;
  // Deletes the temporary directories
  DeleteFolder(GetSandcatDir(SCDIR_PREVIEW));
  // Deletes and recreates the Headers logs folder
  DeleteFolder(GetSandcatDir(SCDIR_HEADERS));
  forcedir(GetSandcatDir(SCDIR_HEADERS));
  inherited;
end;

// ------------------------------------------------------------------------//
end.
