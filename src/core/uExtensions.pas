unit uExtensions;

{
  Sandcat Lua Extension System
  Copyright (c) 2011-2015, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  SysUtils, Classes, Forms, Controls, Dialogs, TypInfo, ExtCtrls,
  Lua, uUIComponents;

type
  TSandcatExtensions = class
  private
    fCanRunLua: boolean;
    fCurrentInitMode: string;
    fErrorList: TStringList;
    fExtensionFiles: TSandSLParser;
    fExtensionList: TStringList;
    fInitializedExtensions: TStringList;
    fInitModes: TStringList;
    fLibraryList: TStringList;
    fLuaQueue: TStringList;
    fLuaWrap: TSandLua;
    fRunLuaTimer: TTimer;
    fTempScript: string;
    function GetExtensionsEnabled: boolean;
    function GetIDFromPak(const PakFilename: string): string;
    function GetLibraryList: string;
    procedure CreateLuaEngine;
    procedure GetPakFilenameVersion(var filename, version: string);
    procedure LoadPakScript(const pakname, scriptname: string);
    procedure RegisterExtension(const PakFilename, json: string);
    procedure RunExtensionMethod(const PakFilename, cmd: string;
      const RegExt: boolean = false);
    procedure RunLuaCmd_JSON(const json: string);
    procedure Broadcast(const cmd: string;
      const RegisterExtensions: boolean = false);
    procedure RunLuaTimerProc(Sender: TObject);
    procedure Shutdown;
  public
    function GetCommandList: string;
    function GetList: string;
    function IsExtensionEnabled(const ID: string): boolean;
    procedure AddLibraryInfo(const name, version, author: string;
      const licfile: string = '');
    procedure AfterLoad;
    procedure Load(const param: string);
    procedure LogScriptError(const Sender, line, msg: string;
      const writetoconsole: boolean = true);
    procedure QueueLuaCmd(const cmd: string);
    procedure RunInitMode(const Mode: string);
    procedure RunJSONCmd(const json: string);
    procedure RunLua(const cmd: string);
    procedure RunLuaCmd(const cmd: string; const pakname: string = '';
      const scriptname: string = '');
    procedure ScriptExceptionHandler(Title: string; line: Integer; msg: string;
      var handled: boolean);
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
    // properties
    property CurrentInitMode: string read fCurrentInitMode
      write fCurrentInitMode;
    property ErrorList: TStringList read fErrorList;
    property Enabled: boolean read GetExtensionsEnabled;
    property InitModes: TStringList read fInitModes;
    property LibraryList: string read GetLibraryList;
    property LuaWrap: TSandLua read fLuaWrap;
    property TempScript: string read fTempScript;
  end;

type
  TSandcatUserScript = record
    CSS_UserStyleSheet: string;
    JS_Tab_LoadEnd: string;
    JS_V8_Extension: string;
    Lua_Task_Init: string;
  end;

procedure Debug(const s: string; const component: string = 'Extensions');

implementation

uses uMain, pLua, uZones, uConst, uSettings, CatConsole, LAPI_SCX, CatRes,
  CatStrings, CatFiles, CatZIP, CatHTTP, LAPI, LAPI_Browser, LAPI_Tab, LAPI_Cmd,
  LAPI_Element, LAPI_App;

const
  cLicLink =
    '<a href="#" onclick="%s"><img filename="n.txt" style="behavior:file-icon;" alt="License"></a>';
  cManifest = 'Manifest.json';
  cExtensionSystemVersion = 'v2';

procedure Debug(const s: string; const component: string = 'Extensions');
begin
  uMain.Debug(s, component);
end;

procedure Register_Sandcat(L: plua_State);
begin
  RegisterConsole(L);
  RegisterBrowser(L);
  RegisterApp(L);
  RegisterSCX(L);
  RegisterSideBar(L);
  RegisterRequestBuilder(L);
  RegisterActiveCodeEdit(L);
  // RegisterTaskMan(L);
  RegisterSettings(L);
  RegisterSCBUIEngine_Sandcat(L);
  RegisterSCBUIElement_Sandcat(L);
  RegisterSCBTab_Sandcat(L);
  RegisterSCBCmd_Sandcat(L);
end;

procedure TSandcatExtensions.CreateLuaEngine;
begin
  debug('createluaengine.begin');
  fCanRunLua := true;
  fLuaWrap := TSandLua.Create(nil);
  fLuaWrap.UseDebug := false;
  fLuaWrap.OnException := ScriptExceptionHandler;
  fLuaWrap.RegisterLuaTable('_appoptions', @lua_getbrowseroption,
    @lua_setbrowseroption);
  fLuaWrap.RegisterLuaTable('_appinfo', @lua_getappinfo, @lua_setappinfo);
  fLuaWrap.RegisterLuaTable('_builderreq', @lua_builder_getrequestoption,
    @lua_builder_setrequestoption);
  fLuaWrap.RegisterLuaTable('_jsvalues', @lua_method_getjsvalue,
    @lua_method_getjsvalue);
  Register_Sandcat(fLuaWrap.LuaState);
  fLuaWrap.Value['ProgDir'] := progdir;
  debug('createluaengine.sandcatlua.begin');
  fLuaWrap.ExecuteCmd(GetResourceAsString('SANDCAT', 'Lua'));
  debug('createluaengine.sandcatlua.end');
  fLuaWrap.ExecuteCmd('Sandcat:Init()');
  // fLuaWrap.ExecuteCmd(GetPakResourceAsString('Sandcat.lua'));
  debug('createluaengine.end');
end;

procedure TSandcatExtensions.RunLua(const cmd: string);
begin
  if fCanRunLua then
    fLuaWrap.ExecuteCmd(cmd);
end;

// Loads script code from extension package (.scx) and executes it
procedure TSandcatExtensions.LoadPakScript(const pakname, scriptname: string);
var
  extensionfilename: string;
  script: TStringList;
begin
  extensionfilename := GetSandcatDir(SCDIR_PLUGINS) + pakname;
  fLuaWrap.LoadScript(emptystr);
  if fileexists(extensionfilename) then
  begin
    script := TStringList.Create;
    script.Text := GetTextFileFromZIP(extensionfilename, scriptname);
    fLuaWrap.ExecuteCmd(script.Text);
    script.free;
  end;
end;

procedure TSandcatExtensions.RunLuaCmd(const cmd: string;
  const pakname: string = ''; const scriptname: string = '');
begin
  if (fCanRunLua = true) and (cmd <> emptystr) then
  begin
    // debug(cmd,'Lua');
    if pakname <> emptystr then
      LoadPakScript(pakname, scriptname);
    fLuaWrap.ExecuteCmd(cmd);
    if EnableConsoleInteraction then
      contentarea.Console_Output(false);
  end;
end;

procedure TSandcatExtensions.RunLuaTimerProc(Sender: TObject);
begin
  fRunLuaTimer.Enabled := false;
  RunLuaCmd(extensions.fLuaQueue.Text);
  fLuaQueue.Clear;
end;

procedure TSandcatExtensions.LogScriptError(const Sender, line, msg: string;
  const writetoconsole: boolean = true);
const
  cErrLine = '[%s] Line %s: %s';
  cTR = '<tr role="option"><td>%s</td><td>%s</td><td>%s</td>%s<td></td><td></td></tr>';
begin
  if writetoconsole then
    contentarea.Console_WriteLn(format(cErrLine, [Sender, line, msg]));
  if fErrorList <> nil then
  begin
    if fErrorList.Count > 1000 then
      fErrorList.Clear;
    StatBar.ErrorLogIcon := '@ICON_ERROR';
    fErrorList.add(format(cTR, [htmlescape(Sender), line, htmlescape(msg),
      datetimetostr(now)]));
  end;
end;

procedure TSandcatExtensions.ScriptExceptionHandler(Title: string;
  line: Integer; msg: string; var handled: boolean);
begin
  LogScriptError('Lua', inttostr(line), format('%s: %s', [Title, msg]));
  handled := true;
end;

function TSandcatExtensions.GetLibraryList: string;
begin
  fLibraryList.Sort;
  result := fLibraryList.Text;
end;

function TSandcatExtensions.GetList: string;
begin
  fExtensionList.Sort;
  result := fExtensionList.Text;
end;

function TSandcatExtensions.GetIDFromPak(const PakFilename: string): string;
var
  ID: string;
begin
  ID := extractfilename(PakFilename);
  ID := lowercase(ID);
  if pos('.', ID) <> 0 then
    ID := before(ID, '.');
  result := ID;
end;

function TSandcatExtensions.IsExtensionEnabled(const ID: string): boolean;
begin
  result := settings.preferences.GetValue(SCO_EXTENSION_ENABLED_PREFIX + '.' +
    ID, true);
end;

procedure TSandcatExtensions.GetPakFilenameVersion(var filename,
  version: string);
begin
  filename := 'nofile';
  if beginswith(version, 'file:') then
  begin
    filename := progdir + '\' + after(version, ':');
    try
      version := GetFileVersion(filename);
    except
    end;
    filename := extractfilename(filename);
  end;
end;

procedure TSandcatExtensions.RegisterExtension(const PakFilename, json: string);
var
  j: TSandJSON;
  cid, ID, link, filename, name: string;
  htmlicon, icon: string;
  version, author, licensescript: string;
  canreg: boolean;
const
  cTR = '<tr role="option" libname=%s><td><input .scxenabler type="checkbox" cid="%s">%s%s</td><td>%s</td><td>%s</td><td></td></tr>';
begin
  canreg := false;
  j := TSandJSON.Create;
  j.Text := json;
  name := j.GetValue('name', emptystr);
  icon := j.GetValue('icon', emptystr);;
  ID := GetIDFromPak(PakFilename);
  if pos('.', ID) <> 0 then
    ID := before(ID, '.');
  version := j.GetValue('version', emptystr);
  author := j.GetValue('author', emptystr);
  licensescript := j.GetValue('script.license', emptystr);
  j.free;
  // Registers only if there is enough info about extension
  if (name <> emptystr) and (version <> emptystr) and (author <> emptystr) then
    canreg := true;
  if canreg = true then
  begin
    cid := SCO_EXTENSION_ENABLED_PREFIX + '.' + ID;
    if licensescript <> emptystr then
      link := format(cLicLink, [htmlescape(licensescript)]);
    // link:='<a href="#" onclick="slx.file.exec([['+progdir+'\'+licfile+']])"><img filename="'+extractfilename(progdir+'\'+licfile)+'" style="behavior:file-icon;" alt="License"></a>';
    GetPakFilenameVersion(filename, version);
    settings.preferences.registerdefault(cid, true);
    htmlicon := format('<img .lvfileicon filename="%s">', [filename]);
    fExtensionList.add(format(cTR, [htmlescape(name), cid, htmlicon, name,
      version, author, link]));
  end;
end;

procedure TSandcatExtensions.AddLibraryInfo(const name, version, author: string;
  const licfile: string = '');
var
  link, filename, ver: string;
const
  cLibTr = '<tr role="option" libname=%s><td><img .lvfileicon filename="%s">%s</td><td>%s</td><td>%s</td><td>%s</td></tr>';
begin
  ver := version;
  if licfile <> emptystr then
    link := format(cLicLink, [htmlescape(licfile)]);
  GetPakFilenameVersion(filename, ver);
  fLibraryList.add(format(cLibTr, [htmlescape(name), filename, name, ver,
    author, link]));
end;

function TSandcatExtensions.GetExtensionsEnabled: boolean;
begin
  result := settings.preferences.GetValue(SCO_EXTENSIONS_ENABLED, true);
end;

procedure TSandcatExtensions.AfterLoad;
begin
  fLuaWrap.ExecuteCmd('Sandcat:AfterLoad()');
  if Enabled = true then
    Broadcast('afterinit');
end;

procedure TSandcatExtensions.Load(const param: string);
begin
  if beginswith(param, cModeParam) then
    CurrentInitMode := after(param, ':');
  CreateLuaEngine;
  if Enabled = true then
    Broadcast('init', true);
end;

procedure TSandcatExtensions.QueueLuaCmd(const cmd: string);
begin
  fLuaQueue.add(cmd);
  fRunLuaTimer.Enabled := true;
end;

procedure TSandcatExtensions.RunExtensionMethod(const PakFilename, cmd: string;
  const RegExt: boolean = false);
var
  ID, script, scriptfilename, luacmd, author, compat: string;
  manifest: TSandJSON;
  iscompatible: boolean;
begin
  iscompatible := true;
  manifest := TSandJSON.Create;
  manifest.Text := GetTextFileFromZIP(PakFilename, cManifest);
  author := manifest.GetValue('author', emptystr);
  compat := manifest.GetValue('compat', emptystr);

  if author = 'Syhunt' then
  begin // Syhunt extensions must declare compatibility key
    if compat <> cExtensionSystemVersion then
      iscompatible := false;
  end;
  if compat <> emptystr then
  begin // For user extensions, compat key is optional
    if compat <> cExtensionSystemVersion then
      iscompatible := false;
  end;

  if iscompatible then
  begin
    if RegExt then
      RegisterExtension(PakFilename, manifest.Text);
    ID := GetIDFromPak(PakFilename);
    scriptfilename := manifest.GetValue('script.filename', emptystr);
    if IsExtensionEnabled(ID) then
    begin
      if cmd = 'init' then
      begin
        fInitializedExtensions.add(ID);
        if scriptfilename <> emptystr then
        begin
          script := GetTextFileFromZIP(PakFilename, scriptfilename);
          RunLuaCmd(script);
        end;
      end;
      if fInitializedExtensions.IndexOf(ID) <> -1 then
      begin
        luacmd := manifest.GetValue('script.' + cmd, emptystr);
        if luacmd <> emptystr then
          RunLuaCmd(luacmd);
      end;
    end;
  end;
  manifest.free;
end;

// Executes a Lua code in all active Sandcat extensions
procedure TSandcatExtensions.Broadcast(const cmd: string;
  const RegisterExtensions: boolean = false);
var
  pak: string;
begin
  if RegisterExtensions then
    GetFiles(PluginsDir + '*' + cPakExtension, fExtensionFiles.list,
      false, false)
  else
    fExtensionFiles.reset;
  while fExtensionFiles.Found do
  begin
    pak := PluginsDir + fExtensionFiles.current + cPakExtension;
    RunExtensionMethod(pak, cmd, RegisterExtensions);
  end;
end;

procedure TSandcatExtensions.RunInitMode(const Mode: string);
var
  code: string;
begin
  code := fInitModes.Values[Mode];
  if code <> emptystr then
    RunLua(code);
end;

type
  TJSONCmds = (cfg, dbg, gotourl, newelement, run, store, write, writeln,
    writevalue);

procedure TSandcatExtensions.RunLuaCmd_JSON(const json: string);
var
  d: TSandJSON;
  s: string;
begin
  d := TSandJSON.Create;
  d.Text := json;
  if d['lang'] = 'lua' then
  begin
    s := d.GetValue('code', emptystr);
    Debug(s, 'Lua (Q)');
    if d.GetValue('q', false) = true then
      extensions.QueueLuaCmd(s)
    else
    begin
      extensions.RunLuaCmd(s);
      application.ProcessMessages;
    end;
  end;
  d.free;
end;

procedure TSandcatExtensions.RunJSONCmd(const json: string);
var
  s, cmd: string;
  d: TSandJSON;
begin
  s := json;
  d := TSandJSON.Create;
  d.Text := s;
  cmd := d['cmd'];
  case TJSONCmds(GetEnumValue(TypeInfo(TJSONCmds), lowercase(cmd))) of
    cfg:
      if d['act'] = 'set' then
      begin
        settings.preferences.setvalue(d['cid'], d['value']);
      end;
    newelement:
      if tabmanager.activetab <> nil then
        uix.CreateElement(d['engine'], d['table'], d['selector']);
    dbg:
      if debugmode then
        Debug(d['s'], 'UIX Debug');
    gotourl:
      begin
        if d['newtab'] = true then
          tabmanager.NewTab(d['url'])
        else
          tabmanager.activetab.gotourl(d['url']);
      end;
    run:
      RunLuaCmd_JSON(json);
    store:
      if tabmanager.activetab <> nil then
        tabmanager.activetab.Cache.StoreString(d.sobject.s['filename'],
          d.sobject.s['text']);
    write:
      if tabmanager.activetab <> nil then
        contentarea.Console_Write(d['s']);
    writeln:
      if tabmanager.activetab <> nil then
        contentarea.Console_WriteLn(d['s']);
    writevalue:
      settings.WriteJSValue(d.sobject.s['key'], d.sobject.s['value']);
  end;
  d.free;
end;

constructor TSandcatExtensions.Create(AOwner: TWinControl);
begin
  inherited Create;
  fCurrentInitMode := 'sandcat';
  fExtensionFiles := TSandSLParser.Create;
  fExtensionList := TStringList.Create;
  fInitModes := TStringList.Create;
  fErrorList := TStringList.Create;
  fLibraryList := TStringList.Create;
  fInitializedExtensions := TStringList.Create;
  fLuaQueue := TStringList.Create;
  fRunLuaTimer := TTimer.Create(nil);
  fRunLuaTimer.Interval := 1;
  fRunLuaTimer.Enabled := false;
  fRunLuaTimer.OnTimer := RunLuaTimerProc;
end;

procedure TSandcatExtensions.Shutdown;
begin
  Debug('shutdown.begin');
  if Enabled = true then
    Broadcast('shutdown');
  fCanRunLua := false;
  fRunLuaTimer.Enabled := false;
  Debug('shutdown.end');
end;

destructor TSandcatExtensions.Destroy;
begin
  Debug('destroy.begin');
  Shutdown;
  fRunLuaTimer.free;
  fLuaQueue.free;
  fInitializedExtensions.free;
  fLibraryList.free;
  fInitModes.free;
  fErrorList.free;
  fExtensionList.free;
  fExtensionFiles.free;
  Debug('destroy.end');
  inherited;
end;

function TSandcatExtensions.GetCommandList: string;
var
  sl: TStringList;
begin
  readdiskcommands;
  sl := TStringList.Create;
  sl.add('Available commands:<br><br><table>');
  sl.add(tabcommandsdesc.Text);
  sl.add('</table>');
  result := sl.Text;
  sl.free;
end;

end.
