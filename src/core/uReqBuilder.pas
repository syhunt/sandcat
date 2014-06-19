unit uReqBuilder;

{
  Sandcat Request Panel
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses Classes, SysUtils, Windows, Graphics, StdCtrls,
{$IF CompilerVersion >= 23}
  Vcl.Controls, Vcl.ExtCtrls,
{$ELSE}
  Controls, ExtCtrls,
{$IFEND}
  uUIComponents;

type
  TSandRequestPanel = class(TCustomControl)
  private
    fLoaded: boolean;
    fToolbarPanel: TPanel;
    procedure HeadersEditEnter(Sender: TObject);
    procedure POSTEditEnter(Sender: TObject);
    procedure URLEditEnter(Sender: TObject);
  protected
  public
    AgentEdit: TEdit;
    HeadersEdit: TMemo;
    POSTDataEdit: TMemo;
    Prefs: TSandJSON;
    QuickPrefsPanel: TPanel;
    RefererEdit: TEdit;
    ToolbarBar: TSandUIEngine;
    URLEdit: TMemo;
    function GetHeaders: string;
    function GetPOSTData: string;
    function GetURL: string;
    procedure Load;
    procedure SetPOSTData(s: string);
    procedure SetUserAgent(s: string);
    procedure SetWordWrap(b: boolean);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses uMain, CatStrings, uMisc, uZones;

function TSandRequestPanel.GetHeaders: string;
const
  cRef = 'Referer:';
  cAgent = 'User-Agent:';
var
  s: string;
begin
  s := HeadersEdit.Text;
  if AgentEdit.Text <> emptystr then
  begin
    if pos(cAgent, s) = 0 then
      s := s + crlf + cAgent + ' ' + AgentEdit.Text;
  end;
  if RefererEdit.Text <> emptystr then
  begin
    if pos(cRef, s) = 0 then
      s := s + crlf + cRef + ' ' + RefererEdit.Text;
  end;
  result := s;
end;

procedure TSandRequestPanel.SetPOSTData(s: string);
begin
  POSTDataEdit.Text := s;
  POSTDataEdit.Visible := true;
  ToolbarBar.Eval('$(#viewreqdata).checked = true;');
end;

procedure TSandRequestPanel.SetUserAgent(s: string);
begin
  AgentEdit.Text := s;
  QuickPrefsPanel.Visible := true;
  ToolbarBar.Eval('$(#viewprefs).checked = true;');
end;

function TSandRequestPanel.GetURL: string;
begin
  result := URLEdit.Text;
  result := replacestr(result, crlf, emptystr);
end;

function TSandRequestPanel.GetPOSTData: string;
begin
  result := POSTDataEdit.Text;
  result := replacestr(result, crlf, emptystr);
end;

procedure TSandRequestPanel.SetWordWrap(b: boolean);
begin
  URLEdit.WordWrap := b;
  POSTDataEdit.WordWrap := b;
end;

procedure TSandRequestPanel.Load;
begin
  if URLEdit.Text = emptystr then
    URLEdit.Text := tabmanager.ActiveTab.GetURL;
  if fLoaded then
    exit;
  fLoaded := true;
  ToolbarBar.LoadHTML(uix.Pages.reqbuilderbar, pluginsdir);
  if uix.TIS.ReqBuilderToolbar <> emptystr then
    ToolbarBar.Eval(uix.TIS.ReqBuilderToolbar + crlf + cUIXUpdate);
  URLEdit.SetFocus;
  uix.ActiveMemo := URLEdit;
end;

procedure TSandRequestPanel.URLEditEnter(Sender: TObject);
begin
  uix.ActiveMemo := URLEdit;
end;

procedure TSandRequestPanel.POSTEditEnter(Sender: TObject);
begin
  uix.ActiveMemo := POSTDataEdit;
end;

procedure TSandRequestPanel.HeadersEditEnter(Sender: TObject);
begin
  uix.ActiveMemo := HeadersEdit;
end;

constructor TSandRequestPanel.Create(AOwner: TComponent);
  procedure SetMemoDefaults(m: TMemo; a: TAlign; texthint: string = '');
  begin
    m.Parent := self;
    m.Align := a;
    m.ReadOnly := false;
    m.HideSelection := false;
    m.ScrollBars := ssBoth;
    m.texthint := texthint;
  end;

begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  fLoaded := false;
  Prefs := TSandJSON.Create;
  Prefs['usecookies'] := true;
  Prefs['usecredentials'] := true;
  Prefs['ignorecache'] := true;
  fToolbarPanel := TPanel.Create(self);
  fToolbarPanel.Align := altop;
  fToolbarPanel.ParentBackground := true;
  fToolbarPanel.ParentBackground := false;
  fToolbarPanel.Parent := self;
  fToolbarPanel.Height := 35;
  fToolbarPanel.BevelInner := bvNone;
  fToolbarPanel.BevelOuter := bvNone;
  ToolbarBar := TSandUIEngine.Create(fToolbarPanel);
  ToolbarBar.Parent := fToolbarPanel;
  ToolbarBar.Align := alclient;
  // URLEdit
  URLEdit := TMemo.Create(self);
  SetMemoDefaults(URLEdit, alclient, 'http://');
  URLEdit.OnEnter := URLEditEnter;
  // POSTDataEdit
  POSTDataEdit := TMemo.Create(self);
  SetMemoDefaults(POSTDataEdit, AlBottom, 'POST Data');
  POSTDataEdit.OnEnter := POSTEditEnter;
  POSTDataEdit.Visible := false;
  // HeadersEdit
  HeadersEdit := TMemo.Create(self);
  SetMemoDefaults(HeadersEdit, AlRight, 'Headers');
  HeadersEdit.OnEnter := HeadersEditEnter;
  HeadersEdit.Visible := false;
  HeadersEdit.Width := 400;
  // QuickPrefsPanel
  QuickPrefsPanel := TPanel.Create(self);
  QuickPrefsPanel.Parent := self;
  QuickPrefsPanel.Align := AlBottom;
  QuickPrefsPanel.Visible := false;
  QuickPrefsPanel.Height := 44;
  QuickPrefsPanel.ParentBackground := true; // vcl fix
  QuickPrefsPanel.ParentBackground := false; // vcl fix
  // RefererEdit
  RefererEdit := TEdit.Create(self);
  RefererEdit.Parent := QuickPrefsPanel;
  RefererEdit.Align := altop;
  RefererEdit.texthint := 'Referer';
  // AgentEdit
  AgentEdit := TEdit.Create(self);
  AgentEdit.Parent := QuickPrefsPanel;
  AgentEdit.Align := alclient;
  AgentEdit.texthint := 'User-Agent';
end;

destructor TSandRequestPanel.Destroy;
begin
  SetWordWrap(false); // weird vcl crash workaround
  HeadersEdit.Free;
  RefererEdit.Free;
  AgentEdit.Free;
  URLEdit.Free;
  POSTDataEdit.Free;
  ToolbarBar.Free;
  fToolbarPanel.Free;
  QuickPrefsPanel.Free;
  Prefs.Free;
  inherited Destroy;
end;

end.
