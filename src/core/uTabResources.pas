unit uTabResources;

{
  Sandcat Resource List
  Copyright (c) 2011-2017, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ComCtrls, Vcl.Menus,
{$ELSE}
  SysUtils, Classes, Controls, ComCtrls, Menus,
{$ENDIF}
  uUIComponents;

type
  TTabResourceList = class(TCustomControl)
  private
    fLv: TListView;
    fAscending: boolean;
    fClickFunc: string;
    fCustomized: boolean;
    fDblClickFunc: string;
    fPage: TSandUIEngine;
    fPopupMenu: TPopupMenu;
    fLastSortedColumn: integer;
    procedure ListViewDblClick(Sender: TObject);
    procedure ListViewClick(Sender: TObject);
    procedure ListviewColumnClick(Sender: TObject; Column: TListColumn);
    procedure MenuCopyClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AddPageResource(const URL: string; ImgIdx: integer);
    procedure AddCustomItem(const JSON: string);
    procedure LoadHTML(const html: string);
    procedure RedefineColumns(const def, clickfunc, dblclickfunc: string);
    procedure UpdateHTMLPage(const csvlist: string);
    // properties
    property Lv: TListView read fLv;
    property Ascending: boolean read fAscending;
  end;

implementation

uses uMain, uZones, CatStrings, CatHTTP, CatUI;

// Sorts the resources page listview columns
function Resources_SortByColumn(Item1, Item2: TListItem; Data: integer)
  : integer; stdcall;
begin
  if Data = 0 then
    Result := AnsiCompareText(Item1.Caption, Item2.Caption)
  else
    Result := AnsiCompareText(Item1.SubItems[Data - 1],
      Item2.SubItems[Data - 1]);
  if not tabmanager.activetab.Resources.Ascending then
    Result := -Result;
end;

// Sorts items by the clicked resources page column
procedure TTabResourceList.ListviewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if Column.index = fLastSortedColumn then
    fAscending := not fAscending
  else
    fLastSortedColumn := Column.index;
  TListView(Sender).CustomSort(@Resources_SortByColumn, Column.index);
end;

// Called when a list item is double clicked in the resources page, displays the
// resource (usually from the cache)
procedure TTabResourceList.ListViewDblClick(Sender: TObject);
begin
  if (fLv.Selected = nil) then
    exit;
  if fCustomized = false then
    tabmanager.activetab.LoadCachedURL(fLv.Selected.SubItems[0])
    // regular display of URL
  else
  begin
    if fDblClickFunc <> emptystr then
    begin
      Extensions.LuaWrap.value['_temppath'] := fLv.Selected.SubItems
        [fLv.Selected.SubItems.Count - 1]; // gets parameter from last subitem
      Extensions.RunLuaCmd(fDblClickFunc + '(_temppath)');
    end;
  end;
end;

// Called when a list item is clicked in the resources page
procedure TTabResourceList.ListViewClick(Sender: TObject);
begin
  if (fLv.Selected = nil) then
    exit;
  if fClickFunc <> emptystr then
  begin
    Extensions.LuaWrap.value['_temppath'] := fLv.Selected.SubItems
      [fLv.Selected.SubItems.Count - 1]; // gets parameter from last subitem
    Extensions.RunLuaCmd(fClickFunc + '(_temppath)');
  end;
end;

// Adds a resource URL (like a .js or .css) to the resource list
procedure TTabResourceList.AddPageResource(const URL: string; ImgIdx: integer);
begin
  if fCustomized then
    exit; // no longer update URL resources if this is a custom user resources tab
  with fLv.Items.Add do
  begin
    Caption := extracturlfilename(URL);
    SubItems.Add(URL);
    imageindex := ImgIdx;
  end;
end;

// Experimental: allows an extension to add custom items
procedure TTabResourceList.AddCustomItem(const JSON: string);
var
  j: TSandJSON;
  i, c: integer;
  itemstr: string;
begin
  j := TSandJSON.Create(JSON);
  c := j.GetValue('subitemcount', 0);
  Debug('add custom page resource with subitemcount:' + IntToStr(c));
  with fLv.Items.Add do
  begin
    Caption := j['caption'];
    imageindex := j.GetValue('imageindex', -1);
    if c <> 0 then
    begin
      for i := 1 to c do
      begin
        itemstr := j['subitem' + IntToStr(i)];
        SubItems.Add(itemstr);
      end;
    end;
  end;
  j.Free;
end;

// Experimental: allows an extension to quickly update info from a loaded HTML
procedure TTabResourceList.UpdateHTMLPage(const csvlist: string);
var
  e: ISandUIElement;
  csv: TSandCSVParser;
  selector: string;
begin
  if fPage = nil then
    exit;
  csv := TSandCSVParser.Create(csvlist);
  while csv.Found do
  begin
    if csv.current <> emptystr then
    begin
      selector := csv['s'];
      e := fPage.Root.Select(selector); // eg code.pid
      if e <> nil then
        e.value := csv['v'];
    end;
  end;
  csv.Free;
end;

// Experimental: Allows an extension to load a HTML page
procedure TTabResourceList.LoadHTML(const html: string);
var
  ht: string;
begin
  if fPage = nil then
  begin
    tabmanager.activetab.SetActivePage('results');
    fPage := TSandUIEngine.Create(self);
    fPage.Parent := self;
    fPage.Align := alClient;
    fLv.Align := alBottom;
    fLv.Height := 200;
  end;
  ht := replacestr(uix.Pages.Tab_Results, cContent, HTML);
  fPage.LoadHTML(ht, pluginsdir);
end;

// Experimental: allows an extension to redefine the resource listview columns
procedure TTabResourceList.RedefineColumns(const def, clickfunc,
  dblclickfunc: string);
var
  csv: TSandCSVParser;
begin
  fCustomized := true;
  fClickFunc := clickfunc;
  fDblClickFunc := dblclickfunc;
  fLv.Columns.Clear;
  fLv.SortType := stNone;
  csv := TSandCSVParser.Create;
  csv.LoadFromString(def);
  while csv.Found do
  begin
    if csv.current <> emptystr then
    begin
      with fLv.Columns.Add do
      begin
        Caption := csv['c'];
        if csv['a'] = '1' then
          AutoSize := true
        else
          AutoSize := false;
        Width := strtointdef(csv['w'], 0);
      end;
    end;
  end;
  csv.Free;
end;

procedure TTabResourceList.MenuCopyClick(Sender: TObject);
begin
  if (fLv.Selected <> nil) then
    GetLVItemAsString(fLv, fLv.Selected, true);
end;

constructor TTabResourceList.Create(AOwner: TComponent);
var
  mi: TMenuItem;
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  fCustomized := false;
  fLv := TListView.Create(self);
  fLv.Parent := self;
  fLv.Align := alClient;
  fLv.SmallImages := SandBrowser.LiveImages;
  fLv.ReadOnly := true;
  fLv.DoubleBuffered := true;
  fLv.ViewStyle := vsReport;
  fLv.RowSelect := true;
  fLv.HideSelection := false;
  fLv.OnDblClick := ListViewDblClick;
  fLv.OnClick := ListViewClick;
  fLv.OnColumnClick := ListviewColumnClick;
  fLv.SortType := stBoth;
  with fLv.Columns.Add do
  begin
    Caption := 'Name';
    Width := 200;
  end;
  with fLv.Columns.Add do
  begin
    Caption := 'URL';
    AutoSize := true;
  end;

  fPopupMenu := TPopupMenu.Create(self);
  mi := TMenuItem.Create(fPopupMenu);
  mi.Caption := '&Copy';
  mi.OnClick := MenuCopyClick;
  fPopupMenu.Items.Add(mi);
  fLv.PopupMenu := fPopupMenu;
end;

destructor TTabResourceList.Destroy;
begin
  if fPage <> nil then
    fPage.Free;
  fPopupMenu.Free;
  fLv.Free;
  inherited Destroy;
end;

end.
