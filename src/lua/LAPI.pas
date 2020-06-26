unit LAPI;

{
  Sandcat Lua API
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses SysUtils, Lua;

procedure RegisterApp(L: plua_State);
procedure RegisterBrowser(L: plua_State);
procedure RegisterConsole(L: plua_State);
procedure RegisterSettings(L: plua_State);
procedure RegisterSideBar(L: plua_State);
procedure RegisterRequestBuilder(L: plua_State);
procedure RegisterActiveCodeEdit(L: plua_State);

implementation

uses pLua, pLuaTable, LAPI_Browser, uSettings, uTaskMan, LAPI_Console, LAPI_App,
  LAPI_CodeEdit, LAPI_TaskMan, LAPI_CEF;

procedure RegisterBrowser(L: plua_State);
const
  sandcatbrowser_table: array [1 .. 36] of luaL_reg = (
  (name: 'addjs'; func: lua_addjavascript),
  (name: 'addlibinfo'; func: lua_addlibraryinfo),
  (name: 'addlua'; func: lua_addluascript),
  (name: 'addtis'; func: lua_addtis),
  (name: 'bookmark'; func: lua_method_bookmark),
  (name: 'cleardata'; func: lua_clearprivatedata),
  (name: 'cleartasks'; func: lua_bgtaskclear),
  (name: 'closepage'; func: lua_closepage),
  (name: 'closetab'; func: lua_closetab),
  (name: 'closetabs'; func: lua_closetabs),
  // (name:'dofile';func:lua_dofile),
  (name: 'dostring'; func: lua_method_runluascript),
  (name: 'exit'; func: lua_browserexit),
  (name: 'getpackfile'; func: lua_readscxfile),
  (name: 'gettaskinfo'; func: lua_bgtaskgetinfo),
  (name: 'gettaskidbytag'; func: lua_bgtaskgettidbytag),
  (name: 'gototab'; func: lua_gototab),
  (name: 'highlightsrc'; func: lua_highlightsource),
  (name: 'inserthtml'; func: lua_inserthtml),
  (name: 'inserthtmlfile'; func: lua_inserthtmlfile),
  (name: 'loadpagex'; func: lua_loadpagex),
  (name: 'newtab'; func: lua_newtab),
  (name: 'newtabx'; func: lua_tabs_addcustom),
  (name: 'newwindow'; func: lua_newwindow),
  (name: 'removetask'; func: lua_bgtaskremove),
  (name: 'saveresource'; func: lua_saveresource),
  (name: 'setactivepage'; func: lua_setactivepage),
  (name: 'setinitmode'; func: lua_setinitmode),
  (name: 'setsearcheng'; func: lua_setsearchengine),
  (name: 'showbottombar'; func: lua_showbottombar),
  (name: 'showreqbuilder'; func: lua_showreqbuilderbar),
  (name: 'showtasks'; func: lua_showtasks),
  (name: 'showurl'; func: lua_showurl),
  (name: 'stoptask'; func: lua_bgtaskstop),
  (name: 'suspendtask'; func: lua_bgtasksuspend),
  // (name:'showmessage';func:lua_showmessage),
  // (name:'showmessagec';func:lua_showmessagec),
  (name: 'var_replace'; func: lua_var_replace),
  (name: nil; func: nil)
  );
  sandcatdebug_table: array [1 .. 3] of luaL_reg = (
  (name: 'enable'; func: lua_enabledebugmode),
  (name: 'print'; func: lua_addtodebuglog),
  (name: nil; func: nil)
  );
begin
  lual_register(L, 'browser', @sandcatbrowser_table);
  // sets browser.info, browser.options...
  plua_SetFieldValue(L,'info', @lua_getappinfo, @lua_setappinfo);
  plua_SetFieldValue(L,'jsvalues', @lua_method_getjsvalue, @lua_method_getjsvalue);
  plua_SetFieldValue(L,'options', @lua_getbrowseroption, @lua_setbrowseroption);
  lual_register(L, 'debug', @sandcatdebug_table);
end;

procedure RegisterApp(L: plua_State);
const
  app_table: array [1 .. 20] of luaL_reg = (
  (name: 'ask_yn';func: app_askyesorno),
  (name: 'bringtofront';func: app_bringtofront),
  (name: 'editlist'; func: app_editlist),
  (name: 'gettitle'; func: app_gettitle),
  (name: 'openfile'; func: app_showopendialog),
  (name: 'savefile'; func: app_showsavedialog),
  (name: 'selectdir'; func: app_showopendirdialog),
  (name: 'seticonfromfile'; func: app_seticonfromfile),
  (name: 'seticonfromres'; func: app_seticonfromres),
  (name: 'settitle'; func: app_settitle),
  (name: 'showalert'; func: app_showalert),
  (name: 'showalerttext'; func: app_showalerttext),
  (name: 'showalerttextx'; func: app_showalerttextx),  
  (name: 'showdialogx'; func: app_showcustomdialog),
  (name: 'showinputdialog'; func: app_showinputdialog),
  (name: 'showmessage'; func: app_showmessage),
  (name: 'showmessagesmpl'; func: app_showmessage_classic),
  (name: 'showmessagex'; func: app_showhtmlmessage),
  (name: 'update'; func: app_processmessages),
  (name: nil; func: nil)
  );
begin
  lual_register(L, PAnsiChar('app'), @app_table);
  plua_setfieldvalue(L,'dir',extractfilepath(paramstr(0)));
  plua_setfieldvalue(L,'datadir',GetAppDataDir);
end;

procedure RegisterSettings(L: plua_State);
const
  sandcatsettings_table: array [1 .. 13] of luaL_reg = (
  (name: 'get'; func: lua_sandcatsettings_get),
  (name: 'getall'; func: lua_sandcatsettings_getalljson),
  (name: 'getalldefault'; func: lua_sandcatsettings_getalldefaultjson),
  (name: 'getdefault'; func: lua_sandcatsettings_getdefault),
  (name: 'getsiteprefsfilename'; func: lua_sandcatsettings_getsiteprefsfilename),
  (name: 'load'; func: lua_sandcatsettings_settext),
  (name: 'loadfromfile'; func: lua_sandcatsettings_loadfromfile),
  (name: 'save'; func: lua_sandcatsettings_save),
  (name: 'savetofile'; func: lua_sandcatsettings_savetofile),
  (name: 'set'; func: lua_sandcatsettings_set),
  (name: 'regdefault'; func: lua_sandcatsettings_registerdefault),
  (name: 'update'; func: lua_sandcatsettings_update),
  (name: nil; func: nil));
begin
  lual_register(L, 'prefs', @sandcatsettings_table);
end;

procedure RegisterConsole(L: plua_State);
const
  sandcatconsole_table: array [0 .. 12] of luaL_reg = (
  (name: 'addcmd'; func: lua_addconsolecommand),
  (name: 'clear'; func: lua_console_clear),
  (name: 'gethandler'; func: lua_console_gethandler),
  (name: 'output'; func: lua_console_output),
  (name: 'reset'; func: lua_console_restore),
  (name: 'setcolor'; func: lua_console_setcolor),
  (name: 'setcurline'; func: lua_console_setcurrentline),
  (name: 'setfontcolor'; func: lua_console_setfontcolor),
  (name: 'sethandler'; func: lua_console_sethandler),
  (name: 'setmanual'; func: lua_console_setmanual),
  (name: 'writeln'; func: lua_console_writeln),
  (name: 'write'; func: lua_console_write),
  (name: nil; func: nil)
  );
begin
  lual_register(L, 'console', @sandcatconsole_table);
  lua_register(L, 'print', @lua_console_writeln);
  // for io redirect from Runik.dll
  lua_register(L, 'sandcat_writeln', @lua_console_writeln);
  lua_register(L, 'sandcat_write', @lua_console_write);
  lua_register(L, 'sandcat_logerror', @lua_scriptlogerror);
end;

procedure RegisterActiveCodeEdit(L: plua_State);
const
  sandcatcodeedit_table: array [1 .. 12] of luaL_reg = (
  (name: 'gettext'; func: lua_activecodeedit_gettext),
  (name: 'settext'; func: lua_activecodeedit_settext),
  (name: 'getsel'; func: lua_activecodeedit_getseltext),
  (name: 'insert'; func: lua_activecodeedit_inserttext),
  (name: 'replacesel'; func: lua_activecodeedit_replacesel),
  (name: 'copy'; func: lua_activecodeedit_copy),
  (name: 'cut'; func: lua_activecodeedit_cut),
  (name: 'paste'; func: lua_activecodeedit_paste),
  (name: 'undo'; func: lua_activecodeedit_undo),
  (name: 'redo'; func: lua_activecodeedit_redo),
  (name: 'setfocus'; func: lua_activecodeedit_setfocus),
  (name: nil; func: nil)
  );
begin
  lual_register(L, 'activecodeedit', @sandcatcodeedit_table);
end;

procedure RegisterRequestBuilder(L: plua_State);
const
  sandcatbuilder_table: array [1 .. 1] of luaL_reg = (
  (name: nil; func: nil)
  );
begin
  lual_register(L, 'reqbuilder', @sandcatbuilder_table);
  // sets reqbuilder.request
  plua_SetFieldValue(L,'request', @lua_builder_getrequestoption, @lua_builder_setrequestoption);
end;

procedure RegisterSideBar(L: plua_State);
const
  sidebar_table: array [1 .. 3] of luaL_reg = (
  (name: 'clear'; func: lua_sidebar_clear),
  (name: 'loaddir'; func: lua_sidebar_loaddir),
  (name: nil; func: nil)
  );
begin
  lual_register(L, 'sidebar', @sidebar_table);
end;

{ procedure RegisterTaskMan(L : Plua_State);
  const
  sandcattaskman_table : array [1..20] of luaL_reg =
  (
  (name:'add';func:lua_addbackgroundtask),
  (name:'addhide';func:lua_addbackgroundtask_hidden),
  (name:'getstatus';func:lua_bgtaskgetstatus),
  (name:'getcaption';func:lua_bgtaskgetcaption),
  (name:'gethandle';func:lua_bgtaskgethandle),
  (name:'getinfo';func:lua_bgtaskgetinfo),
  (name:'gettasklist';func:lua_gettasklist),
  (name:'getdownloadlist';func:lua_getdownloadlist),
  (name:'getparam';func:lua_bgtaskgetparam),
  (name:'remove';func:lua_bgtaskremove),
  (name:'runscript';func:lua_bgtaskrunscript),
  (name:'stop';func:lua_bgtaskstop),
  (name:'setscript';func:lua_bgtasksetscript),
  (name:'setparam';func:lua_bgtasksetparam),
  (name:'setparams';func:lua_bgtasksetparams),
  (name:'setstatus';func:lua_bgtasksetstatus),
  (name:'setcaption';func:lua_bgtasksetcaption),
  (name:'seticon';func:lua_bgtaskseticon),
  (name:'special';func:lua_bgtaskspecial),
  (name:nil;func:nil)
  );
  begin
  lual_register(L,'taskman',@sandcattaskman_table);
  end; }

end.
