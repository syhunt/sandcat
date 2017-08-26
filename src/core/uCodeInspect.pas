unit uCodeInspect;

{
  Sandcat - Source Code Inspector
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

//{$DEFINE MINIMAP}

uses Classes, SysUtils,
{$IF CompilerVersion >= 23}
  Vcl.Controls, Vcl.ExtCtrls, Vcl.Tabs, Vcl.ComCtrls,
{$ELSE}
  Controls, ExtCtrls, Tabs, ComCtrls,
{$IFEND}
{$IFDEF MINIMAP}
  SynMiniMap,
{$ENDIF}
  uUIComponents;

type
  TSyCodeInspector = class(TCustomControl)
  private
    fAliases: TStringList;
    fItemList: string;
    fMainLv: TListView;
{$IFDEF MINIMAP}
    fMiniMap: TSynMiniMap;
{$ENDIF}
    fMsgsPanel: TPanel;
    fSource: TSandSynEdit;
    fSourceFilename: string;
    fTabStrip: TTabSet;
    function GetActiveTab: string;
    procedure MainListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure MainListViewClick(Sender: TObject);
    procedure TabStrip1Change(Sender: TObject; NewTab: Integer;
      var AllowChange: Boolean);
{$IFDEF MINIMAP}
    procedure SynMiniMap1Click(Sender: TObject; Data: PSynMiniMapEventData);
{$ENDIF}
  public
    procedure LoadFromFile(const filename: string);
    procedure LoadItems(const s, activetab: string;
      const silent: Boolean = false);
    procedure LoadTabs(const csv: string; const aliaslist: string = '');
    procedure SetImageList(const il: TImageList);
    procedure SetSource(const s: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property activetab: string read GetActiveTab;
    property MsgsPanel: TPanel read fMsgsPanel;
    property Source: TSandSynEdit read fSource;
    property SourceFilename: string read fSourceFilename;
  end;

implementation

uses uConst;

function IsInteger(const s: string): Boolean;
var
  v, c: Integer;
begin
  Val(s, v, c);
  if v=0 then begin // hide H2077 compiler warning
  end;
  result := c = 0;
end;

function TSyCodeInspector.GetActiveTab: string;
begin
  result := 'All';
  if fTabStrip.TabIndex > 0 then
    result := fTabStrip.Tabs[fTabStrip.TabIndex];
end;

procedure TSyCodeInspector.SetSource(const s: string);
begin
  fSource.text := s;
end;

procedure TSyCodeInspector.TabStrip1Change(Sender: TObject; NewTab: Integer;
  var AllowChange: Boolean);
begin
  LoadItems(fItemList, fTabStrip.Tabs[NewTab], true);
end;

procedure TSyCodeInspector.MainListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  MainListViewClick(Sender);
end;

procedure TSyCodeInspector.MainListViewClick(Sender: TObject);
begin
  if (fMainLv.Selected = nil) then
    exit;
  fSource.GotoLineAndCenter(strtoint(fMainLv.Selected.Caption));
  fSource.ActiveLineColor := cSourceActiveLineColor;
end;

procedure TSyCodeInspector.LoadFromFile(const filename: string);
begin
  fSourceFilename := filename;
  fSource.Lines.LoadFromFile(filename);
end;

procedure TSyCodeInspector.LoadTabs(const csv: string;
  const aliaslist: string = '');
begin
  fTabStrip.Tabs.Commatext := csv;
  fTabStrip.TabIndex := 0;
  fAliases.text := aliaslist;
end;

procedure TSyCodeInspector.LoadItems(const s, activetab: string;
  const silent: Boolean = false);
var
  csv: TSandCSVParser;
  itemtype: string;
  function risktoimageindex(it, r: string): Integer;
  begin
    r := lowercase(r);
    result := -1;
    if it = 'HTML' then
      result := ICONIDX_HTML
    else if it = 'JavaScript' then
      result := ICONIDX_JAVASCRIPT;
    case r[1] of
      'h':
        result := ICONIDX_RISK_HIGH;
      'm':
        result := ICONIDX_RISK_MEDIUM;
      'l':
        result := ICONIDX_RISK_LOW;
    end;
  end;
  function canadditem(atab, ityp: string): Boolean;
  begin
    result := false;
    if atab = 'All' then
      result := true
    else if atab = ityp then
      result := true
    else if atab = fAliases.values[ityp] then
      result := true;
  end;

begin
  if silent = false then
    fMsgsPanel.Visible := true;
  fItemList := s;
  fMainLv.Items.Clear;
  csv := TSandCSVParser.Create;
  csv.LoadFromString(fItemList);
  while csv.Found do
  begin
    itemtype := csv.Values['t'];
    if canadditem(activetab, itemtype) then
    begin
      with fMainLv.Items.Add do
      begin
        Caption := csv.Values['l'];
        imageindex := risktoimageindex(itemtype, csv.Values['r']);
        if IsInteger(Caption) then
          Caption := inttostr(strtoint(Caption) + 1);
        subitems.Add('[' + itemtype + '] ' + csv.Values['c']);
      end;
    end;
  end;
  csv.Free;
end;

procedure TSyCodeInspector.SetImageList(const il: TImageList);
begin
  fMainLv.SmallImages := il;
end;

{$IFDEF MINIMAP}

procedure TSyCodeInspector.SynMiniMap1Click(Sender: TObject;
  Data: PSynMiniMapEventData);
begin
  fMiniMap.Editor.GotoLineAndCenter(Data.Coord.Line);
  fMiniMap.Editor.CaretX := Data.Coord.Char;
end;
{$ENDIF}

constructor TSyCodeInspector.Create(AOwner: TComponent);
  procedure ConfigLV(lv: TListView);
  begin
    lv.Align := alClient;
    lv.ReadOnly := true;
    lv.DoubleBuffered := true;
    lv.ViewStyle := vsReport;
    lv.RowSelect := true;
    lv.HideSelection := false;
    with lv.Columns.Add do
    begin
      Caption := 'Line';
      width := 100;
      autosize := false;
    end;
    with lv.Columns.Add do
    begin
      Caption := 'Description';
      width := 300;
      autosize := true;
    end;
  end;

begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  fSource := TSandSynEdit.Create(self);
  fSource.Parent := self;
  fSource.Align := alClient;

  fMsgsPanel := TPanel.Create(self);
  fMsgsPanel.Align := albottom;
  fMsgsPanel.ParentBackground := true;
  fMsgsPanel.ParentBackground := false;
  fMsgsPanel.Parent := self;
  fMsgsPanel.BevelInner := bvNone;
  fMsgsPanel.BevelOuter := bvNone;
  fMsgsPanel.Height := 200;
  fMsgsPanel.Visible := false;

  fTabStrip := TTabSet.Create(fMsgsPanel);
  fTabStrip.Parent := fMsgsPanel;
  fTabStrip.Align := alTop;
  fTabStrip.Style := tsModernPopout;
  fTabStrip.TabPosition := tpTop;
  fTabStrip.onChange := TabStrip1Change;

  fMainLv := TListView.Create(fMsgsPanel);
  fMainLv.Parent := fMsgsPanel;
  fMainLv.onChange := MainListViewChange;
  fMainLv.OnClick := MainListViewClick;
  ConfigLV(fMainLv);

  fAliases := TStringList.Create;

{$IFDEF MINIMAP}
  fMiniMap := TSynMiniMap.Create(self);
  fMiniMap.Parent := self;
  fMiniMap.Align := alright;
  fMiniMap.Colors.Highlight := cSourceActiveLineColor;
  fMiniMap.Options.AllowScroll := false;
  fMiniMap.Editor := fSource;
  fMiniMap.OnClick := SynMiniMap1Click;
{$ENDIF}
end;

destructor TSyCodeInspector.Destroy;
begin
  fMainLv.onChange := nil;
  fMainLv.Free;
{$IFDEF MINIMAP}
  fMiniMap.Free;
{$ENDIF}
  fTabStrip.Free;
  fMsgsPanel.Free;
  fSource.Free;
  fAliases.Free;
  inherited Destroy;
end;

end.
