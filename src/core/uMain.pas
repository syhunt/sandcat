unit uMain;

{
  Syhunt Sandcat Browser
  Copyright (c) 2011-2014, Syhunt Informatica

  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.

  This software uses the Catarinka components. Catarinka is distributed under
  the same license as Sandcat. Copyright (c) 2003-2014, Felipe Daragon
}

interface

uses
{$IF CompilerVersion >= 23} // XE2 or higher
  Winapi.Windows, Vcl.Graphics, Winapi.Messages, System.SysUtils,
  System.Classes, Vcl.Controls, Vcl.Forms, Vcl.StdCtrls, Vcl.Dialogs,
  Vcl.ExtCtrls, Winapi.ShellAPI, Vcl.Menus, Vcl.ComCtrls, Vcl.ImgList,
{$ELSE}
  Windows, Graphics, Messages, SysUtils, Classes, Controls, Forms, StdCtrls,
  Dialogs, ExtCtrls, ShellAPI, Menus, ComCtrls, ImgList,
{$IFEND}
  uTab, uExtensions, uTabMan, uTaskMan, uSettings, LAPI_Task,
  CatHighlighters, CatConsole, uZones;

type
  TSandBrowser = class(TForm)
    LiveImages: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  protected
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
  private
    FStartupTimer: TTimer;
    procedure StartupTimer(Sender: TObject);
    procedure WMCopyData(var message: TMessage); message WM_COPYDATA;
  public
  end;

var
  SandBrowser: TSandBrowser;
  DebugMemo: TMemo;
  Downloads: TSandcatDownloadManager;
  Extensions: TSandcatExtensions;
  Highlighters: TCatHighlighters;
  MainPanel: TPanel;
  SandTask: TSandcatTaskProcess;
  Settings: TSandcatSettings;
  TabManager: TSandcatTabManager;
  Tasks: TSandcatTaskManager;
  UIX: TSandcatUIX;
  UserScript: TSandcatUserScript;
  SandConsole: TCatConsole;

var
  CreateDebugLog: boolean = false;
  DebugMode: boolean = false;
  DbgLogFileName: string;
  EnableConsoleInteraction: boolean = true;
  HeadersVisible: boolean = false;
  ProgDir, PluginsDir: string;

  // Copy data messages to the main form
const
  SCBF_CMDLINEPARAMS = 1;

function ExitBeforeInitializing: boolean;
procedure Debug(const s: string; const component: string = 'UI');

implementation

uses uConst, uUIComponents, CatChromium, uMisc, CatStrings, CatFiles, CatCLUtils,
  CatUI, ceflib, CatTasks, CatStdSysMenu;

{$R *.dfm}

// Called from Sandcat.dpr during startup
// Returns false for a standard launch, or true if it must exit before
// initializing the user interface
function ExitBeforeInitializing: boolean;
begin
  result := false;
  ProgDir := extractfilepath(paramstr(0));
  if fileexists(ProgDir + 'Config\Portable.json') then
    IsSandcatPortable := true;
  PluginsDir := GetSandcatDir(SCDIR_PLUGINS);
  CefSingleProcess := false;
  CefCache := GetSandcatDir(SCDIR_CACHE);
  CefLocalesDirPath := progdir+'Packs\CEF\Locales\';
  CefResourcesDirPath := ProgDir + 'Packs\CEF\Resources\';
  CefUserAgent := GetCustomUserAgent;
  // TODO: figure out how to pass CLI proxy settings from here:
  // --proxy-server="socks5://localhost:9050"

  if not CefLoadLibDefault then
    result := true // This is a CEF renderer process
  else // Not a CEF renderer, check if this is a Sandcat task process
    if beginswith(paramstr(1), cBgTaskPrefix) then
    begin
      result := true; // This is a Sandcat task process
      SandTask := TSandcatTaskProcess.Create(nil);
      SandTask.Run(after(paramstr(1), cBgTaskPrefix));
      SandTask.Free;
    end
    else
    begin
      // Standard UI startup
      if RegisterAxSciter = false then
        result := true; // There was a problem registering AxSciter
      if UseSingleInstance = true then
      begin
        SendCommandLineParams(GetWindowClassHandle(cMainClass));
        result := true;
      end;
    end;
end;

procedure Debug(const s: string; const component: string = 'UI');
var
  Msg: string;
begin
  if DebugMode = false then
    exit;
  Msg := component + ': ' + s;
  DebugMemo.Lines.add(Msg);
  if CreateDebugLog then
    SendToLog(DbgLogFileName, Msg);
end;

procedure TSandBrowser.WMDropFiles(var Msg: TMessage);
begin
  if TabManager <> nil then
    TabManager.HandleDropFiles(Msg);
end;

procedure TSandBrowser.WMCopyData(var message: TMessage);
var
  pData: PCopyDataStruct;
  s: string;
begin
  message.result := 0;
  pData := PCopyDataStruct(message.LParam);
  if (pData = nil) then
    exit;
  s := string(StrPas(PAnsiChar(pData^.lpData)));
  case pData^.dwData of
    SCBF_CMDLINEPARAMS:
      begin
        ForceForegroundWindow(SandBrowser.handle);
        application.Restore;
        application.ProcessMessages;
        TabManager.NewTab(s);
      end;
  end;
  message.result := 1;
end;

procedure TSandBrowser.StartupTimer(Sender: TObject);
begin
  Debug('sb.startuptimer.begin');
  FStartupTimer.Enabled := false;
  DragAcceptFiles(handle, true);
  Extensions := TSandcatExtensions.Create(self);
  Tasks := TSandcatTaskManager.Create(self);
  Downloads := TSandcatDownloadManager.Create(self);
  UIX := TSandcatUIX.Create(self);
  Highlighters := TCatHighlighters.Create(self);
  Extensions.Load(paramstr(1));
  UIX.LoadUI;
  Extensions.AfterLoad;
  ChDir(ProgDir);
  // SetUserCSS(GetPakResourceAsString('browser.css')); // CSS test
  TabManager.LoadWelcomePage(paramstr(1));
  StdSysMenu(self);
  Debug('sb.startuptimer.end');
end;

procedure TSandBrowser.FormShow(Sender: TObject);
begin
  Debug(crlf + 'sb.formshow.end');
  FStartupTimer := TTimer.Create(self);
  FStartupTimer.Interval := 1;
  FStartupTimer.OnTimer := StartupTimer;
  Debug(crlf + 'sb.formshow.end');
end;

procedure TSandBrowser.FormCreate(Sender: TObject);
begin
  DbgLogFileName := ProgDir + '\' + vDebugFile;
  if fileexists(DbgLogFileName) then
    deletefile(DbgLogFileName);
  vExeFileName := paramstr(0);
  Debug(crlf + 'sb.formcreate.start');
  color := clWindow;
  MainPanel := TPanel.Create(self);
  MainPanel.parent := self;
  ConfigPanel(MainPanel, alClient);
  SideBar := TSandcatSidebar.Create(self);
  SideBar.parent := self;
  SideBar.align := alLeft;
  ContentArea := TSandcatContentArea.Create(self);
  ContentArea.parent := MainPanel;
  ContentArea.align := alClient;
  SandDlg := TSandcatDialogs.Create;
  TabManager := TSandcatTabManager.Create(self);
  StatBar := TSandcatStatusbar.Create(self);
  StatBar.parent := self;
  StatBar.align := albottom;
  Settings := TSandcatSettings.Create(self);
  Settings.Load;
  Debug('sb.formcreate.end');
end;

procedure TSandBrowser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  Debug(crlf + 'sb.formclose.begin');
  visible := false;
  ContentArea.Shutdown;
  SandDlg.Free;
  Settings.Save;
  TabManager.Free;
  Tasks.Free;
  Downloads.Free;
  Extensions.Free;
  UIX.Free;
  Settings.Free;
  FStartupTimer.Free;
  Debug('sb.formclose.end');
  Debug(crlf + 'cefshutdown.begin');
  CatCEFShutdown(true);
  Debug('cefshutdown.end');
end;

end.
