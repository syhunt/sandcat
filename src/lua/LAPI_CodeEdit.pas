unit LAPI_CodeEdit;

{
  Sandcat CodeEdit Lua Library
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses Windows, Messages, Lua, LuaObject, SysUtils, Dialogs, uUIComponents;

function lua_activecodeedit_copy(L: PLua_State): integer; cdecl;
function lua_activecodeedit_cut(L: PLua_State): integer; cdecl;
function lua_activecodeedit_paste(L: PLua_State): integer; cdecl;
function lua_activecodeedit_undo(L: PLua_State): integer; cdecl;
function lua_activecodeedit_redo(L: PLua_State): integer; cdecl;
function lua_activecodeedit_getfilename(L: PLua_State): integer; cdecl;
function lua_activecodeedit_getseltext(L: PLua_State): integer; cdecl;
function lua_activecodeedit_replacesel(L: PLua_State): integer; cdecl;
function lua_activecodeedit_inserttext(L: PLua_State): integer; cdecl;
function lua_activecodeedit_setfocus(L: PLua_State): integer; cdecl;
function lua_activecodeedit_gettext(L: PLua_State): integer; cdecl;
function lua_activecodeedit_settext(L: PLua_State): integer; cdecl;

implementation

uses uMain, pLua, CatStrings;

function lua_activecodeedit_setfocus(L: PLua_State): integer; cdecl;
begin
  try
    if uix.ActiveMemo <> nil then
      uix.ActiveMemo.SetFocus;
  except
  end;
  result := 1;
end;

function lua_activecodeedit_inserttext(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
    uix.ActiveMemo.SelText := lua_tostring(L, 1);
  result := 1;
end;

function lua_activecodeedit_settext(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
    uix.ActiveMemo.lines.Text := lua_tostring(L, 1);
  result := 1;
end;

function lua_activecodeedit_gettext(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
    lua_pushstring(L, uix.ActiveMemo.lines.Text)
  else
    lua_pushstring(L, emptystr);
  result := 1;
end;

function lua_activecodeedit_getseltext(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
    lua_pushstring(L, uix.ActiveMemo.SelText)
  else
    lua_pushstring(L, emptystr);
  result := 1;
end;

function lua_activecodeedit_getfilename(L: PLua_State): integer; cdecl;
begin
  // ToDo: implement external file loading...
  lua_pushstring(L, 'untitled.js');
  result := 1;
end;

function lua_activecodeedit_replacesel(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
  begin
    if uix.ActiveMemo.SelText <> emptystr then
      uix.ActiveMemo.SelText := lua_tostring(L, 1);
  end;
  result := 1;
end;

function lua_activecodeedit_copy(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
    uix.ActiveMemo.CopyToClipboard;
  result := 1;
end;

function lua_activecodeedit_cut(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
    uix.ActiveMemo.CutToClipboard;
  result := 1;
end;

function lua_activecodeedit_paste(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
    uix.ActiveMemo.PasteFromClipboard;
  result := 1;
end;

function lua_activecodeedit_undo(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
    uix.ActiveMemo.Undo;
  result := 1;
end;

function lua_activecodeedit_redo(L: PLua_State): integer; cdecl;
begin
  if uix.ActiveMemo <> nil then
  begin
    if uix.ActiveMemo.HandleAllocated then
      SendMessage(uix.ActiveMemo.Handle, EM_UNDO, 1, 0);
  end;
  result := 1;
end;

end.
