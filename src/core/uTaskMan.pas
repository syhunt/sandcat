unit uTaskMan;

{
  Sandcat Task Manager
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Windows, Classes, Messages, Controls, SysUtils, Dialogs, ExtCtrls,
  Forms, TypInfo, Lua, LuaObject, uUIComponents, uRequests, CatMsg;

type
  TSandcatTaskOnStop = procedure(const tid: string) of object;

type
  TSandcatTaskScripts = record
    OnClick: string;
    OnDoubleClick: string;
    OnParamChange: string;
    OnStop: string;
  end;

type
  TSandcatTask = class
  private
    fCaption: string;
    fDownloadFilename: string;
    fEnabled: boolean;
    fHasProgressBar: boolean;
    fHasMonitor: boolean;
    fHidden: boolean;
    fIcon: string;
    fIsDownload: boolean;
    fLog: TStringList;
    fMenuHTML: string;
    fMonitor: TSandUIEngine;
    fMonitorQueue: TStringList;
    fMonitorQueueTimer: TTimer;
    fMsg:TCatMsg;
    fOnStop: TSandcatTaskOnStop;
    fOriginatorTab: string;
    fParams: TSandJSON;
    fPID: integer;
    fProgressPos: integer;
    fProgressMax: integer;
    fScripts: TSandcatTaskScripts;
    fSuspended: boolean;
    fStatus: string;
    fStopped: boolean;
    fTabMsgHandle: HWND;
    fTID: string;
    function Format(const s: string): string;
    procedure MonitorEval(const tis: string);
    procedure QueueTIS(const s: string);
    procedure RemoveMonitor;
    procedure SetIcon(const url: string);
    procedure SetIconAni(const url: string);
    procedure SetCaption(const s: string);
    procedure SetStatus(const s: string);
    procedure SetMonitor(const m: TSandUIEngine);
    procedure Suspend(const resume: boolean = false);
    procedure CopyDataMessage(const msg: integer; const str: string);
    procedure MonitorQueueTimerTimer(Sender: TObject);
    procedure TaskUpdated;
    procedure Write(const s: string);
    procedure WriteLn(const s: string);
  public
    function GetParam(const name, default: string): string;
    procedure DoSpecial(const s: string);
    procedure GetInfoL(L: PLua_State);
    procedure RunScript(const s: string);
    procedure SetParam(const name, value: string);
    procedure SetParams(const json: string);
    procedure SetProgress(const pos: integer = 0; const max: integer = 100);
    procedure SetScript(const event, script: string);
    procedure Stop(const reason: string = ''; const quickstop: boolean = false);
    constructor Create(const tid: string);
    destructor Destroy; override;
    // properties
    property Caption: string read fCaption write SetCaption;
    property DownloadFilename: string read fDownloadFilename
      write fDownloadFilename;
    property Enabled: boolean read fEnabled;
    property Icon: string read fIcon write SetIcon;
    property IsDownload: boolean read fIsDownload;
    property IsSuspended: boolean read fSuspended;
    property Msg: TCatMsg read fMsg;
    property OnStop: TSandcatTaskOnStop read fOnStop write fOnStop;
    property Status: string read fStatus write SetStatus;
    property TID: string read fTID;
  end;

type
  TSandcatTaskManager = class
  private
    fCache: TSandObjCache;
    fRunning: boolean;
    fStartedTasks: integer;
    function CountActive: integer;
    function TaskExists(const tid: string): boolean;
    procedure KillActiveTasks;
    procedure ShutDown;
    procedure TaskStopped(const tid: string);
  public
    function AddTask(const MenuHTML: string; const Hidden: boolean = false): TSandcatTask;
    function SelectTask(const tid: string): TSandcatTask;
    procedure GetDownloadList(var sl: TStringList);
    procedure GetTaskList(var sl: TStringList);
    procedure RemoveTask(const tid: string);
    procedure RunJSONCmd(const json: string);
    procedure SetTaskParam_JSON(const json:string);
    procedure StopTask(const tid: string);
    procedure SuspendResumeTask(const tid: string);
    procedure SuspendTask(const tid: string; const resume: boolean = false);
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    property Running: boolean read fRunning write fRunning;
  end;

type
  TSandcatDownload = TSandcatTask;

type
  TSandcatDownloadManager = class
  private
    fDownloads: TSandJINI;
  public
    function Add(did: integer; fullpath: string): TSandcatDownload;
    function Get(did: integer): TSandcatDownload;
    procedure CancelList(list:TStringList);
    procedure Delete(did: integer);
    procedure HandleUpdate(list:TStringList; var cancel: boolean;
     const id, state, percentcomplete: integer; const fullPath: string);
    procedure RemoveDownloadFromList(list:TStringList;const id: string);
    procedure SetDownloadFilename(did: integer; suggestedname: string);
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
  end;

const
  SCTASK_WRITELN = 1;
  SCTASK_LOGREQUEST_DYNAMIC = 2;

implementation

uses uMain, uZones, uTab, uMisc, LAPI_Task, CatHTTP, CatUI, pLua, pLuaTable,
  uConst, CatTime, CatStrings, CatTasks, CatChromium;

var
  tasks_shutdown: boolean = false;

procedure SendAMessage(desthandle, msgid: integer; msgstr: string);
var
  pData: PCopyDataStruct;
begin
  pData := nil;
  try
    New(pData);
    pData^.dwData := msgid;
    pData^.cbData := Length(msgstr) + 1;
    pData^.lpData := PAnsiChar(AnsiString(msgstr));
    SendMessage(desthandle, WM_COPYDATA, application.Handle, integer(pData));
  finally
    Dispose(pData);
  end;
end;

type
  TJSONCmds = (cmd_setcaption, cmd_setprogress, cmd_setscript, cmd_setstatus,
    cmd_special, cmd_print, cmd_outputmsg, cmd_showmsg, cmd_stop, cmd_writeln,
    cmd_write);

procedure TSandcatTaskManager.SetTaskParam_JSON(const json:string);
var
    p: TSandJINI;
    task: tsandcattask;
begin
    p := TSandJINI.Create;
    p.Text := json;
    task := SelectTask(p.values['TID']);
    if task <> nil then
      task.SetParam(p.values['Name'], base64decode(p.values['Value']));
    p.Free;
end;

procedure TSandcatTaskManager.RunJSONCmd(const json: string);
var
  j: TSandJSON;
  Task: TSandcatTask;
  cmd: string;
begin
  j := TSandJSON.Create(json);
  Task := tasks.SelectTask(j['tid']);
  cmd := lowercase(j['cmd']);
  if Task <> nil then
  begin
    case TJSONCmds(GetEnumValue(TypeInfo(TJSONCmds), 'cmd_' + cmd)) of
      cmd_outputmsg:
        BottomBar.TaskMsgs.AddMessage(j['s'], Task.fPID, j.sObject.I['i']);
      cmd_setcaption:
        Task.SetCaption(j['s']);
      cmd_setprogress:
        Task.SetProgress(j['p'], j['m']);
      cmd_setscript:
        Task.SetScript(j['e'], j['s']);
      cmd_setstatus:
        Task.SetStatus(j['s']);
      cmd_special:
        Task.DoSpecial(j['s']);
      cmd_print:
        Task.writeln(j['s']);
      cmd_showmsg:
        sanddlg.showmessage(j.GetValue('s', emptystr));
      cmd_stop:
        Task.Stop(j.GetValue('s', emptystr));
      cmd_writeln:
        Task.writeln(j['s']);
      cmd_write:
        Task.Write(j['s']);
    end;
  end;
  j.Free;
end;

function TSandcatTaskManager.CountActive: integer;
var
  c: integer;
  Task: TSandcatTask;
begin
  result := 0;
  for c := fCache.Count - 1 downto 0 do
  begin
    Task := TSandcatTask(fCache.ObjectAt(c));
    if Task <> nil then
    begin
      if Task.fEnabled then
        result := result + 1;
    end;
  end;
end;

procedure TSandcatTaskManager.TaskStopped(const tid: string);
begin
  if CountActive = 0 then
    Running := false
  else
    Running := true;
  if Running = false then
    navbar.AnimateTasksIcon(false);
end;

procedure TSandcatTaskManager.GetTaskList(var sl: TStringList);
var
  c: integer;
  Task: TSandcatTask;
begin
  sl.clear;
  for c := fCache.Count - 1 downto 0 do
  begin
    Task := TSandcatTask(fCache.ObjectAt(c));
    if Task <> nil then
      sl.Add(Task.ftid);
  end;
end;

procedure TSandcatTaskManager.GetDownloadList(var sl: TStringList);
var
  c: integer;
  Task: TSandcatTask;
begin
  sl.clear;
  for c := fCache.Count - 1 downto 0 do
  begin
    Task := TSandcatTask(fCache.ObjectAt(c));
    if Task <> nil then
    begin
      if Task.fIsDownload then
        sl.Add(Task.ftid);
    end;
  end;
end;

function TSandcatTaskManager.TaskExists(const tid: string): boolean;
var
  c: integer;
  Task: TSandcatTask;
begin
  result := false;
  for c := fCache.Count - 1 downto 0 do
  begin
    Task := TSandcatTask(fCache.ObjectAt(c));
    if Task <> nil then
      if Task.ftid = tid then
        result := true;
  end;
end;

function TSandcatTaskManager.SelectTask(const tid: string): TSandcatTask;
var
  c: integer;
  Task: TSandcatTask;
begin
  result := nil;
  for c := fCache.Count - 1 downto 0 do
  begin
    Task := TSandcatTask(fCache.ObjectAt(c));
    if Task <> nil then
      if Task.ftid = tid then
        result := Task;
  end;
end;

procedure TSandcatTaskManager.StopTask(const tid: string);
var
  Task: TSandcatTask;
begin
  Task := SelectTask(tid);
  if Task <> nil then
    Task.Stop;
end;

procedure TSandcatTaskManager.SuspendResumeTask(const tid: string);
var
  Task: TSandcatTask;
begin
  Task := SelectTask(tid);
  if Task = nil then
    exit;
  Task.Suspend(Task.IsSuspended);
end;

procedure TSandcatTaskManager.SuspendTask(const tid: string; const resume: boolean = false);
var
  Task: TSandcatTask;
begin
  Task := SelectTask(tid);
  if Task = nil then
    exit;
  Task.Suspend(resume);
end;

procedure TSandcatTaskManager.RemoveTask(const tid: string);
var
  Task: TSandcatTask;
begin
  Task := SelectTask(tid);
  if Task = nil then
    exit;
  Task.Stop(emptystr, true);
  Task.MonitorEval('Tasks.Remove("' + tid + '")');
  fCache.Remove(Task);
end;

function TSandcatTaskManager.AddTask(const MenuHTML: string; const Hidden: boolean = false)
  : TSandcatTask;
const
  basic_menu = '<li .stop onclick="browser.stoptask([[%t]])">Stop</li>' + crlf +
    '<li .suspend onclick="browser.suspendtask([[%t]])">Suspend/Resume</li>' +
    crlf + '<li .remove onclick="browser.removetask([[%t]])">Remove</li>';
var
  Task: TSandcatTask;
  taskid,menu: string;
  tab: TSandcatTab;
  j: TSandJSON;
  function myformat(s: string): string;
  begin
    result := replacestr(s, '%t', taskid);
  end;

begin
  result := nil;
  menu:=menuhtml;
  fStartedTasks := fStartedTasks + 1;
  taskid := inttostr(DateTimeToUnix(Now)) + '-' + inttostr(fStartedTasks);
  if TaskExists(taskid) then
    exit;
  Running := true;
  if menu <> emptystr then
    menu := menu + '<hr/>' + myformat(basic_menu)
  else
    menu := myformat(basic_menu);
  Task := TSandcatTask.Create(taskid);
  fCache.Add(Task);
  navbar.AnimateTasksIcon(true);
  Task.OnStop := TaskStopped;
  Task.fMenuHTML := myformat(menu);
  Task.fHidden := Hidden;
  result := Task;
  if Hidden = false then
  begin
    tab := tabmanager.ActiveTab;
    if tab <> nil then
    begin
      Task.fOriginatorTab := tab.UID;
      Task.fTabMsgHandle := tab.Msg.msgHandle;
      if contentarea.PageExists('tasks') = false then
      begin
        contentarea.createpage('tasks');
        TaskMonitor.LoadHtml(uix.Pages.Tab_Tasks, pluginsdir);
      end;
      Task.SetMonitor(TaskMonitor);
      // Associates the task monitor with this task
      j := TSandJSON.Create;
      j['tid'] := taskid;
      j['menu'] := Task.fMenuHTML;
      Task.fMonitor.eval('Tasks.Add(' + j.TextUnquoted + ')');
      j.Free;
      Task.SetIconAni('@ICON_RUNNING');
      contentarea.SetActivePage('tasks');
      Task.SetCaption('Starting Task...');
    end;
  end;
end;

procedure TSandcatTaskManager.KillActiveTasks;
var
  c: integer;
  Task: TSandcatTask;
begin
  debug('kill.activetasks.begin', 'TaskMan');
  for c := fCache.Count - 1 downto 0 do
  begin
    Task := TSandcatTask(fCache.ObjectAt(c));
    if Task <> nil then
    begin
      Task.Stop(emptystr, true);
      if Task.fPID <> 0 then
      begin
        KillProcessByPID(Task.fPID);
        Task.fPID := 0;
      end;
    end;
  end;
  debug('kill.activetasks.end', 'TaskMan');
end;

constructor TSandcatTaskManager.Create(AOwner: TWinControl);
begin
  inherited Create;
  fStartedTasks := 0;
  Running := false;
  fCache := TSandObjCache.Create(1000, true);
end;

procedure TSandcatTaskManager.ShutDown;
begin
  debug('shutdown', 'TaskMan');
  tasks_shutdown := true;
  KillActiveTasks;
end;

destructor TSandcatTaskManager.Destroy;
begin
  tasks.ShutDown;
  inherited;
end;

// Sandcat Download Manager ****************************************************
procedure TSandcatDownloadManager.SetDownloadFilename(did: integer;
  suggestedname: string);
begin
  fDownloads.writestring(inttostr(did), 'file', suggestedname);
end;

procedure TSandcatDownloadManager.Delete(did: integer);
begin
  try
    fDownloads.DeleteSection(inttostr(did));
  except
  end;
end;

function TSandcatDownloadManager.Add(did: integer; fullpath: string)
  : TSandcatDownload;
var
  Task: TSandcatDownload;
begin
  Task := tasks.AddTask(emptystr);
  Task.fIsDownload := true;
  fDownloads.writestring(inttostr(did), 'tid', Task.ftid);
  Task.SetCaption(fDownloads.readstring(inttostr(did), 'file', emptystr));
  result := Task;
end;

function TSandcatDownloadManager.Get(did: integer): TSandcatDownload;
begin
  result := tasks.SelectTask(fDownloads.readstring(inttostr(did), 'tid',
    emptystr));
end;

// Cancels any active downloads that are in a list of download IDs
procedure TSandcatDownloadManager.CancelList(list:TStringList);
var
  slp: TSandSLParser;
  d: TSandcatDownload;
begin
  slp := TSandSLParser.Create(List);
  while slp.Found do
  begin
    d := Get(strtoint(slp.current));
    if d <> nil then
    begin
      d.stop;
      RemoveDownloadFromList(List,slp.current);
    end;
  end;
  slp.Free;
end;

procedure TSandcatDownloadManager.HandleUpdate(list:TStringList; var cancel: boolean;
  const id, state, percentcomplete: integer; const fullPath: string);
var
  d: TSandcatDownload;
begin
  d := Get(id);
  if d = nil then
  begin // Task not found, creates it
    if state = SCD_INPROGRESS then
    begin
      if List.IndexOf(inttostr(id)) = -1 then
        List.Add(inttostr(id));
      d := Add(id, fullPath);
      contentarea.SetActivePage('tasks');
    end;
  end;
  if d = nil then
    exit;
  if d.Enabled = false then
    cancel := true; // Download cancelled by the user
  case state of
    SCD_INPROGRESS:
      begin
        if percentcomplete <= 100 then
        begin
          d.SetProgress(percentcomplete);
          d.Status := 'Downloading (' + inttostr(percentcomplete) + '%)...';
        end;
      end;
    SCD_COMPLETE:
      begin
        d.SetProgress(100);
        d.stop('Download Complete.');
        d.Icon := ICON_CHECKED;
        if fullPath <> emptystr then
        begin
          d.SetScript('ondblclick', 'Downloader:launch(slx.base64.decode[[' +
            base64encode(fullPath) + ']])');
          d.DownloadFilename := fullPath;
        end;
        RemoveDownloadFromList(List, inttostr(id));
      end;
    SCD_CANCELED:
      begin
        if percentcomplete <= 100 then
          d.SetProgress(percentcomplete);
        d.stop;
        RemoveDownloadFromList(List, inttostr(id));
      end;
  end;
end;

// Removes a download from a list of downloads by its ID
procedure TSandcatDownloadManager.RemoveDownloadFromList(list:TStringList;const id: string);
begin
  if List.IndexOf(id) <> -1 then
  begin
    List.Delete(List.IndexOf(id));
    Delete(strtoint(id));
  end;
end;

constructor TSandcatDownloadManager.Create(AOwner: TWinControl);
begin
  inherited Create;
  fDownloads := TSandJINI.Create;
end;

destructor TSandcatDownloadManager.Destroy;
begin
  fDownloads.Free;
  inherited;
end;

// Sandcat Task ****************************************************************
procedure TSandcatTask.CopyDataMessage(const msg: integer; const str: string);
begin
  case (msg) of
    SCTASK_WRITELN:
      WriteLn(str);
    SCTASK_LOGREQUEST_DYNAMIC:
      SendAMessage(fTabMsgHandle, SCBM_LOGDYNAMICREQUEST, str);
  end;
end;

procedure TSandcatTask.GetInfoL(L: PLua_State);
var
  progress_desc, progress_icon: string;
  function getprog: string;
  begin
    result := inttostr(getpercentage(fProgressPos, fProgressMax)) + '%';
  end;

begin
  lua_newtable(L);
  plua_SetFieldValue(L, 'caption', fCaption);
  plua_SetFieldValue(L, 'menuhtml', fMenuHTML);
  if fIcon = emptystr then
  begin
    if fIsDownload then
      fIcon := ICON_DOWNLOADS
    else
      fIcon := ICON_LUA;
  end;
  plua_SetFieldValue(L, 'icon', fIcon);
  plua_SetFieldValue(L, 'filename', fDownloadFilename);
  plua_SetFieldValue(L, 'status', fStatus);
  plua_SetFieldValue(L, 'onclick', fScripts.OnClick);
  plua_SetFieldValue(L, 'ondblclick', fScripts.OnDoubleClick);
  if fEnabled then
  begin
    progress_icon := ICON_TASK_RUNNING;
    if fHasProgressBar then
      progress_desc := getprog()
    else
      progress_desc := 'Running';
  end
  else
  begin
    if fStopped then
      progress_icon := ICON_BLANK
    else
      progress_icon := ICON_CHECKED;
    if fHasProgressBar then
      progress_desc := 'Done (' + getprog() + ').'
    else
      progress_desc := 'Done.';
  end;
  plua_SetFieldValue(L, 'progressicon', progress_icon);
  plua_SetFieldValue(L, 'progressdesc', progress_desc);
  plua_SetFieldValue(L, 'pid', fPID);
end;

procedure TSandcatTask.TaskUpdated;
begin
  if fScripts.OnParamChange <> emptystr then
    SendAMessage(fTabMsgHandle, SCBM_LUA_RUN, fScripts.OnParamChange);
end;

function TSandcatTask.GetParam(const name, default: string): string;
begin
  if fParams.HasPath(name) then
    result := fParams[name]
  else
    result := default;
end;

procedure TSandcatTask.SetParams(const json: string);
begin
  fParams.Text := json;
  TaskUpdated;
end;

procedure TSandcatTask.SetParam(const name, value: string);
begin
  // debug(tid+': Param '+params[name]+' set');
  fParams[name] := value;
  TaskUpdated;
end;

procedure TSandcatTask.RunScript(const s: string);
var
  processid: cardinal;
  e: ISandUIElement;
begin
  processid := RunTaskSeparateProcess(ftid, s,
    tabmanager.ActiveTab.Msg.msgHandle, fParams);
  self.fPID := processid;
  if fHasMonitor then
  begin
    e := fMonitor.Root.Select('code.pid[tid="' + ftid + '"]');
    e.value := 'PID ' + inttostr(fPID);
  end;
end;

procedure TSandcatTask.SetStatus(const s: string);
var
  e: ISandUIElement;
  ns:string;
begin
  ns := s;
  ns := ShortTitle(ns, 200);
  fStatus := ns;
  if fHasMonitor then
  begin
    e := fMonitor.Root.Select('code.stat[tid="' + ftid + '"]');
    e.value := ns;
  end;
end;

procedure TSandcatTask.DoSpecial(const s: string);
var
  e: ISandUIElement;
  cMainDiv: string;
  procedure setfontcolor(color: string);
  begin
    e := fMonitor.Root.Select('code.pid[tid="' + ftid + '"]');
    e.StyleAttr['color'] := color;
    e := fMonitor.Root.Select('code.stat[tid="' + ftid + '"]');
    e.StyleAttr['color'] := color;
    e := fMonitor.Root.Select('table.log[tid="' + ftid + '"]');
    e.StyleAttr['color'] := color;
  end;

begin
  if fHasMonitor = false then
    exit;
  cMainDiv := 'div[tid="' + ftid + '"]';
  if s = 'paintred' then
  begin
    e := fMonitor.Root.Select(cMainDiv);
    e.StyleAttr['background-color'] := '#d55935 #b33515 #a12200 #8c0000';
    e.StyleAttr['border-color'] := '#e98c72 #d67860 #c66654 #b75548';
    e.StyleAttr['color'] := 'white';
    setfontcolor('white');
    SetIconAni('@ICON_BLANK');
  end;
  if s = 'paintgreen' then
  begin
    e := fMonitor.Root.Select(cMainDiv);
    e.StyleAttr['background-color'] := '#59d535 #35b315 #22a100 #008c00';
    e.StyleAttr['border-color'] := '#8ce972 #78d660 #66c654 #55b748';
    e.StyleAttr['color'] := 'white';
    setfontcolor('white');
    SetIconAni('@ICON_BLANK');
  end;
end;

procedure TSandcatTask.SetScript(const event, script: string);
var
  e: ISandUIElement;
  ev,s:string;
begin
  ev := lowercase(event);
  s := Format(script);
  if ev = 'onparamchange' then
    fScripts.OnParamChange := s;
  if ev = 'onstop' then
    fScripts.OnStop := s;
  if fHasMonitor then
  begin
    if ev = 'onclick' then
    begin
      fScripts.OnClick := s;
      e := fMonitor.Root.Select('div[tid="' + ftid + '"]');
      if e <> nil then
        e.Attr[ev] := s;
    end;
    if ev = 'ondblclick' then
    begin
      fScripts.OnDoubleClick := s;
      e := fMonitor.Root.Select('div[tid="' + ftid + '"]');
      if e <> nil then
        e.Attr[ev] := s;
    end;
  end;
end;

procedure TSandcatTask.RemoveMonitor;
begin
  fHasMonitor := false;
  fHasProgressBar := false;
  fMonitor := nil;
end;

procedure TSandcatTask.SetMonitor(const m: TSandUIEngine);
begin
  fMonitor := m;
  fHasMonitor := true;
end;

procedure TSandcatTask.SetCaption(const s: string);
var
  e: ISandUIElement;
begin
  fCaption := ShortTitle(s, 100);
  if fHasMonitor then
  begin
    e := fMonitor.Root.Select('code.caption[tid="' + ftid + '"]');
    if e <> nil then
      e.value := fCaption;
  end;
end;

procedure TSandcatTask.SetIconAni(const url: string);
var
  e: ISandUIElement;
begin
  if fHasMonitor then
  begin
    e := fMonitor.Root.Select('img.staticon[tid="' + ftid + '"]');
    if e <> nil then
      e.StyleAttr['foreground-image'] := url;
  end;
end;

procedure TSandcatTask.SetIcon(const url: string);
begin
  fIcon := url;
end;

procedure TSandcatTask.SetProgress(const pos: integer = 0; const max: integer = 100);
var
  e: ISandUIElement;
begin
  fHasProgressBar := true;
  fProgressPos := pos;
  fProgressMax := max;
  // debug('settings progress:'+inttostr(pos)+' : '+inttostr(max));
  if fHasMonitor then
  begin
    e := fMonitor.Root.Select('div.dprog[tid="' + ftid + '"]');
    e.StyleAttr['display'] := 'block';
    e := fMonitor.Root.Select('progress.prog[tid="' + ftid + '"]');
    if e <> nil then
    begin
      e.value := integer(GetPercentage(pos, max));
    end;
  end;
end;

procedure TSandcatTask.MonitorQueueTimerTimer(Sender: TObject);
begin
  if fMonitorQueue.Text = emptystr then
    exit;
  MonitorEval(fMonitorQueue.Text);
  fMonitorQueue.clear;
end;

procedure TSandcatTask.MonitorEval(const tis: string);
begin
  if fHasMonitor then
    SendAMessage(fTabMsgHandle, SCBM_MONITOR_EVAL, tis);
end;

procedure TSandcatTask.QueueTIS(const s: string);
begin
  fMonitorQueue.Add(s);
  // resets the timer count
  fMonitorQueueTimer.Enabled := false;
  fMonitorQueueTimer.Enabled := true;
end;

procedure TSandcatTask.Write(const s: string);
begin
  fLog.Text := fLog.Text + s;
end;

procedure TSandcatTask.writeln(const s: string);
var
  j: TSandJSON;
begin
  fLog.Add(s);
  j := TSandJSON.Create;
  j['ln'] := s;
  QueueTIS('Tasks.Print("' + ftid + '",' + j.TextUnquoted + ');');
  j.Free;
end;

procedure TSandcatTask.Suspend(const resume: boolean = false);
begin
  if fPID <> 0 then
  begin
    if resume = false then
    begin
      fSuspended := true;
      SuspendProcess(fPID);
      SetIconAni('@ICON_SUSPENDED');
      SetStatus('Suspended.');
      SendAMessage(fTabMsgHandle, SCBM_TASK_SUSPENDED, '1');
    end
    else
    begin
      fSuspended := false;
      ResumeProcess(fPID);
      SetIconAni('@ICON_RUNNING');
      SetStatus('Resumed.');
      SendAMessage(fTabMsgHandle, SCBM_TASK_RESUMED, '1');
    end;
  end;
end;

procedure TSandcatTask.Stop(const reason: string = ''; const quickstop: boolean = false);
var
  e: ISandUIElement;
begin
  if fEnabled = false then
    exit; // Already stoped
  fEnabled := false;
  fScripts.OnParamChange := emptystr; // Clears the Lua code
  if fPID <> 0 then
  begin
    KillProcessByPID(fPID);
    fPID := 0;
  end;
  if fScripts.OnStop <> emptystr then
    extensions.RunLua(fScripts.OnStop);
  if quickstop then
    exit;
  if Assigned(OnStop) then
    OnStop(ftid);
  if reason = emptystr then
  begin
    SendAMessage(fTabMsgHandle, SCBM_TASK_STOPPED, '1');
    SetStatus('Stopped.');
    fStopped := true;
  end
  else
  begin
    SetStatus(reason);
  end;
  if fHasMonitor then
  begin
    e := fMonitor.Root.Select('menu#' + ftid + '-menu > li.stop');
    if e <> nil then
      e.Attr['disabled'] := 'disabled';
    e := fMonitor.Root.Select('menu#' + ftid + '-menu > li.suspend');
    if e <> nil then
      e.Attr['disabled'] := 'disabled';
    e := fMonitor.Root.Select('img.stop[tid="' + ftid + '"]');
    if e <> nil then
      e.StyleAttr['display'] := 'none';
    e := fMonitor.Root.Select('img.staticon[tid="' + ftid + '"]');
    if e <> nil then
      e.StyleAttr['foreground-image'] := '@ICON_BLANK';
    e := fMonitor.Root.Select('code.pid[tid="' + ftid + '"]');
    if e <> nil then
      e.StyleAttr['color'] := 'gray';
  end;
end;

function TSandcatTask.Format(const s: string): string;
begin
  result := replacestr(s, '%t', ftid);
end;

constructor TSandcatTask.Create(const tid: string);
begin
  inherited Create;
  self.ftid := tid;
  fMsg:=TCatMsg.Create;
  fMsg.OnCopyDataMessage:=CopyDataMessage;
  debug('task created (handle ' + inttostr(fMsg.msgHandle) + ')');
  fEnabled := true;
  fStopped := false;
  fIsDownload := false;
  fHidden := false;
  fSuspended := false;
  fHasMonitor := false;
  fHasProgressBar := false;
  fProgressMax := 100;
  fParams := TSandJSON.Create;
  fLog := TStringList.Create;
  fMonitorQueue := TStringList.Create;
  fMonitorQueueTimer := TTimer.Create(SandBrowser);
  fMonitorQueueTimer.Interval := 500;
  fMonitorQueueTimer.OnTimer := MonitorQueueTimerTimer;
end;

destructor TSandcatTask.Destroy;
begin
  RemoveMonitor;
  fMonitorQueueTimer.Enabled := false;
  fMonitorQueueTimer.OnTimer := nil;
  fMonitorQueueTimer.Free;
  fMonitorQueue.Free;
  fLog.Free;
  fParams.Free;
  fMsg.Free;
  inherited;
end;

end.
