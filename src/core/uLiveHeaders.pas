unit uLiveHeaders;

{
  Sandcat Live Headers
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses Forms, SysUtils, Windows, Controls, Graphics, Classes, StdCtrls, Dialogs,
  ExtCtrls, Types, Menus, ComCtrls, CatChromiumLib;

type
  TLiveHeadersFilter = record
    Text: string;
    ContentType: string;
    FileExt: string;
    Params: string;
    Details: string;
    SearchStr: string;
    Site: string;
    CmdStr: string;
    Method: string;
    Status: string;
  end;

type
  TLiveHeaders = class(TCustomControl)
  private
    fFrontLv: TListView;
    fMainLv: TListView;
    fFilterLv: TListView;
    fFilterMode: boolean;
    fControlPanel: TPanel;
    fPauseBtn: TButton;
    fEraseBtn: TButton;
    fFilterEdit: TEdit;
    fFilterTimer: TTimer;
    fPaused: boolean;
    fPopupMenu: TPopupMenu;
    fHeadersLastSortedColumn: integer;
    fFilteredLastSortedColumn: integer;
    fHeadersAscending: boolean;
    fFilteredAscending: boolean;
    fMaxItems: integer;
    procedure Timer1Timer(Sender: TObject);
    procedure FilterEditChange(Sender: TObject);
    procedure PauseBtnClick(Sender: TObject);
    procedure ClearBtnClick(Sender: TObject);
    procedure HeadersListViewChange(Sender: TObject; Item: TListItem;
      Change: TItemChange);
    procedure HeadersListViewClick(Sender: TObject);
    procedure HeadersListviewColumnClick(Sender: TObject; Column: TListColumn);
    procedure FilteredListviewColumnClick(Sender: TObject; Column: TListColumn);
    procedure MenuCopyClick(Sender: TObject);
    procedure MenuOpenURLNewTabClick(Sender: TObject);
    procedure MenuSaveFromCacheClick(Sender: TObject);
    procedure MenuSaveFromWebClick(Sender: TObject);
  protected
  public
    Filter: TLiveHeadersFilter;
    function GetImageIndex(const request: TSandcatRequestDetails): integer;
    function GetImageIndexForURL(const URL: string): integer;
    function GetStatusImageIndex(const Status: integer): integer;
    function PassFilter(const request: TSandcatRequestDetails): boolean;
    procedure AddRequest(const request: TSandcatRequestDetails);
    procedure LoadFromFile(const Filename: string);
    procedure SaveToFile(const Filename: string);
    procedure AddRequestToList(const lv: TListView;
      const request: TSandcatRequestDetails);
    procedure ApplyFilter(const s: string);
    procedure Clear;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property HeadersAscending: boolean read fHeadersAscending;
    property FilteredAscending: boolean read fFilteredAscending;
    property FilterEdit: TEdit read fFilterEdit;
  end;

implementation

uses uMain, uZones, uConst, CatFiles, CatStrings, CatUI, CatHTTP, CatHTML,
  CatMatch, uMisc;

function GetSearchParam(line, param: string; def_value: string = ''): string;
begin
  line := ' ' + line + ' ';
  result := emptystr;
  result := after(line, ' ' + param + ':');
  result := before(result, ' ');
  result := trim(result);
  if result = emptystr then
    result := def_value;
end;

function Headers_SortByColumn(Item1, Item2: TListItem; Data: integer)
  : integer; stdcall;
begin
  if Data = 0 then
    result := AnsiCompareText(Item1.Caption, Item2.Caption)
  else
    result := AnsiCompareText(Item1.SubItems[Data - 1],
      Item2.SubItems[Data - 1]);
  if not tabmanager.ActiveTab.liveheaders.HeadersAscending then
    result := -result;
end;

function Filtered_SortByColumn(Item1, Item2: TListItem; Data: integer)
  : integer; stdcall;
begin
  if Data = 0 then
    result := AnsiCompareText(Item1.Caption, Item2.Caption)
  else
    result := AnsiCompareText(Item1.SubItems[Data - 1],
      Item2.SubItems[Data - 1]);
  if not tabmanager.ActiveTab.liveheaders.FilteredAscending then
    result := -result;
end;

procedure TLiveHeaders.LoadFromFile(const Filename: string);
var
  i, m: integer;
begin
  fMainLv.Items.Clear;
  LoadListviewStrings(fMainLv, Filename);
  m := fMainLv.Items.Count;
  for i := m - 1 downto 0 do
  begin
    fMainLv.Items[i].ImageIndex := strtointdef(fMainLv.Items[i].SubItems[6], 0);
  end;
end;

procedure TLiveHeaders.SaveToFile(const Filename: string);
begin
  SaveListviewStrings(fMainLv, Filename);
end;

procedure TLiveHeaders.HeadersListviewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if Column.Index = fHeadersLastSortedColumn then
    fHeadersAscending := not fHeadersAscending
  else
    fHeadersLastSortedColumn := Column.Index;
  TListView(Sender).CustomSort(@Headers_SortByColumn, Column.Index);
end;

procedure TLiveHeaders.FilteredListviewColumnClick(Sender: TObject;
  Column: TListColumn);
begin
  if Column.Index = fFilteredLastSortedColumn then
    fFilteredAscending := not fFilteredAscending
  else
    fFilteredLastSortedColumn := Column.Index;
  TListView(Sender).CustomSort(@Filtered_SortByColumn, Column.Index);
end;

procedure TLiveHeaders.HeadersListViewChange(Sender: TObject; Item: TListItem;
  Change: TItemChange);
begin
  // HeadersListViewClick(Sender);
end;

procedure TLiveHeaders.HeadersListViewClick(Sender: TObject);
var
 fn:string;
begin
  if (fFrontLv.Selected = nil) then
    exit;
  application.ProcessMessages;
  fn := fFrontLv.Selected.SubItems[fFrontLv.Selected.SubItems.Count - 1];
  tabmanager.ActiveTab.ShowRequest(fn);
end;

procedure TLiveHeaders.ClearBtnClick(Sender: TObject);
begin
  tabmanager.ActiveTab.requests.Clear;
end;

procedure TLiveHeaders.PauseBtnClick(Sender: TObject);
begin
  fPaused := not fPaused;
  if fPaused = true then
  begin
    fPauseBtn.Hint := 'Unpause';
    fPauseBtn.ImageIndex := 11;
  end
  else
  begin
    fPauseBtn.Hint := 'Pause';
    fPauseBtn.ImageIndex := 10;
  end;
end;

function TLiveHeaders.GetStatusImageIndex(const Status: integer): integer;
begin
  result := -1;
  case Status of
    404:
      result := 13;
    500:
      result := 14;
    503:
      result := 15;
  end;
end;

function TLiveHeaders.GetImageIndexForURL(const URL: string): integer;
var
  r: TSandcatRequestDetails;
begin
  r.URL := URL;
  result := GetImageIndex(r);
end;

function TLiveHeaders.GetImageIndex(const request
  : TSandcatRequestDetails): integer;
var
  ext: string;
begin
  result := ICONIDX_UNKNOWN; // unknown type
  ext := lowercase(ExtractUrlFileExt(request.URL));
  // by file extension
  if ext = '.css' then
    result := ICONIDX_CSS
  else if ext = '.js' then
    result := ICONIDX_JAVASCRIPT
  else if ext = '.json' then
    result := ICONIDX_JSON
  else if ext = '.swf' then
    result := ICONIDX_FLASH
  else if ext = '.html' then
    result := ICONIDX_HTML
  else if ext = '.xml' then
    result := ICONIDX_XML;
  // by mimetype
  if pos('css', request.MimeType) <> 0 then
    result := ICONIDX_CSS;
  if pos('html', request.MimeType) <> 0 then
  begin
    result := ICONIDX_HTML;
    if pos('.php', ext) <> 0 then
      result := ICONIDX_PHP;
  end;
  if pos('flash', request.MimeType) <> 0 then
    result := ICONIDX_FLASH;
  if pos('javascript', request.MimeType) <> 0 then
    result := ICONIDX_JAVASCRIPT;
  if pos('json', request.MimeType) <> 0 then
    result := ICONIDX_JSON;
  if pos('xml', request.MimeType) <> 0 then
    result := ICONIDX_XML;
  if beginswith(request.MimeType, 'image') then
    result := ICONIDX_IMAGE
  else if RegExpFind(ext, '.bmp|.gif|.ico|.jpg|.jpeg|.png|.svg') <> emptystr
  then
    result := ICONIDX_IMAGE;
  if beginswith(request.MimeType, 'video') then
    result := ICONIDX_VIDEO;
  if beginswith(request.MimeType, 'audio') then
    result := ICONIDX_AUDIO;
end;

function TLiveHeaders.PassFilter(const request: TSandcatRequestDetails)
  : boolean;
var
  matched, foundext, foundtype: boolean;
  URL: string;
  function beginswithcmd(src: string): boolean;
  begin
    result := false;
    if beginswith(src, 'ext') then
      result := true
    else if beginswith(src, 'type') then
      result := true
    else if beginswith(src, 'details') then
      result := true
    else if beginswith(src, 'method') then
      result := true
    else if beginswith(src, 'status') then
      result := true
    else if beginswith(src, 'site') then
      result := true
    else if beginswith(src, 'params') then
      result := true;
  end;
  function ReMatch(s, re: string): boolean;
  begin
    result := false;
    try
      if RegExpFind(s, re) <> emptystr then
        result := true;
    except
      result := false;
    end;
  end;

begin
  result := false;
  matched := false;
  foundext := true;
  foundtype := true;
  URL := request.URL;
  if pos(Filter.SearchStr, ExtractURLPath(URL)) <> 0 then
    matched := true;
  if beginswithcmd(Filter.Text) then
    matched := true;
  if matched then
  begin // matched begin
    result := true;
    if (Filter.Site <> emptystr) and (pos(Filter.Site, ExtractURLHost(URL)) = 0)
    then
      result := false;
    if (Filter.Method <> emptystr) then
    begin
      if ReMatch(request.Method, Filter.Method) = false then
        result := false;
    end;
    if (Filter.Params <> emptystr) and (pos(Filter.Params, request.PostData) = 0)
    then
      result := false;
    if (Filter.Status <> emptystr) then
    begin
      if ReMatch(inttostr(request.StatusCode), Filter.Status) = false then
        result := false;
    end;
    if (Filter.FileExt <> emptystr) then
    begin
      if ReMatch(ExtractUrlFileExt(URL), Filter.FileExt) = false then
        foundext := false;
    end;
    if (Filter.ContentType <> emptystr) then
    begin
      if ReMatch(request.MimeType, Filter.ContentType) = false then
        foundtype := false;
    end;
    if (Filter.FileExt <> emptystr) and (Filter.ContentType <> emptystr) then
    begin
      if (foundext = false) and (foundtype = false) then
        result := false;
    end
    else
    begin
      if (Filter.FileExt <> emptystr) and (foundext = false) then
        result := false;
      if (Filter.ContentType <> emptystr) and (foundtype = false) then
        result := false;
    end;
    if (Filter.Details <> emptystr) and
      (pos(Filter.Details, request.Details) = 0) then
      result := false;
  end; // matched end
end;

procedure TLiveHeaders.ApplyFilter(const s: string);
var
  i, m: integer;
  request: TSandcatRequestDetails;
begin
  if s = emptystr then
  begin
    fMainLv.BringToFront;
    fFrontLv := fMainLv;
    fFilterMode := false;
  end
  else
  begin
    fFilterMode := true;
    fFilterLv.Clear;
    fFilterLv.BringToFront;
    fFrontLv := fFilterLv;
    m := fMainLv.Items.Count;
    fFilterLv.Items.BeginUpdate;
    Filter.Text := s;
    Filter.SearchStr := s;
    if pos(' ', s) <> 0 then
      Filter.SearchStr := before(Filter.SearchStr, ' ');
    Filter.Method := uppercase(GetSearchParam(s, 'method'));
    Filter.FileExt := GetSearchParam(s, 'ext');
    Filter.Status := GetSearchParam(s, 'status');
    Filter.Params := GetSearchParam(s, 'params');
    Filter.Details := GetSearchParam(s, 'details');
    Filter.Site := GetSearchParam(s, 'site');
    Filter.ContentType := GetSearchParam(s, 'type');
    Filter.CmdStr := before(s, ':');
    for i := 0 to m do
    begin
      if i < m then
      begin
        // Loads the request details from an unfiltered list view item
        request.URL := fMainLv.Items[i].SubItems[0];
        request.Method := fMainLv.Items[i].Caption;
        request.PostData := fMainLv.Items[i].SubItems[1];
        request.StatusCode := strtointdef(fMainLv.Items[i].SubItems[2], 0);
        request.MimeType := fMainLv.Items[i].SubItems[3];
        request.Details := fMainLv.Items[i].SubItems[5];
        request.Filename := fMainLv.Items[i].SubItems[7];
        if PassFilter(request) then
          AddRequestToList(fFilterLv, request);
      end;
    end
  end;
  fFilterLv.Items.EndUpdate;
end;

procedure TLiveHeaders.Timer1Timer(Sender: TObject);
begin
  fFilterTimer.enabled := false;
  ApplyFilter(fFilterEdit.Text);
end;

procedure TLiveHeaders.FilterEditChange(Sender: TObject);
begin
  // Resets the timer
  fFilterTimer.enabled := false;
  fFilterTimer.enabled := true;
end;

procedure TLiveHeaders.AddRequestToList(const lv: TListView;
  const request: TSandcatRequestDetails);
begin
  with lv.Items.Add do
  begin
    Caption := request.Method;
    if pos('%', request.URL) <> 0 then
      SubItems.Add(urldecode(request.URL))
    else
      SubItems.Add(request.URL);
    ImageIndex := -1;
    subitemimages[0] := GetImageIndex(request);
    if request.PostData <> emptystr then
      SubItems.Add(strmaxlen(request.PostData, 100, true))
    else
      SubItems.Add(emptystr);
    SubItems.Add(inttostr(request.StatusCode));
    ImageIndex := GetStatusImageIndex(request.StatusCode);
    if request.MimeType <> emptystr then
      SubItems.Add(request.MimeType)
    else // display assumed mime type
      SubItems.Add(FilenameToMimeType(extracturlfilename(request.URL)));
    // Length will be zero for 304 because Chromium reads the resource
    // from the cache
    if request.StatusCode <> 304 then
      SubItems.Add(getsizedescription(request.Length))
    else
      SubItems.Add('Unknown');
    SubItems.Add(request.Details);
    SubItems.Add(inttostr(ImageIndex));
    SubItems.Add(request.Filename);
    makevisible(true);
  end;
  if lv.Items.Count > fMaxItems then
    lv.Items[0].Delete;
end;

procedure TLiveHeaders.AddRequest(const request: TSandcatRequestDetails);
begin
  if fPaused = true then
    exit; // No need to add the request
  AddRequestToList(fMainLv, request);
  if Filter.Text = emptystr then
    exit; // No need to add to filtered list
  if PassFilter(request) then
    AddRequestToList(fFilterLv, request);
end;

procedure TLiveHeaders.Clear;
begin
  fMainLv.Clear;
  fFilterLv.Clear;
end;

procedure TLiveHeaders.MenuCopyClick(Sender: TObject);
begin
  if (fFrontLv.Selected <> nil) then
    GetLVItemAsString(fFrontLv, fFrontLv.Selected, true);
end;

procedure TLiveHeaders.MenuOpenURLNewTabClick(Sender: TObject);
begin
  if (fFrontLv.Selected <> nil) then
    tabmanager.NewTab(fFrontLv.Selected.SubItems[0]);
end;

procedure TLiveHeaders.MenuSaveFromCacheClick(Sender: TObject);
begin
  if (fFrontLv.Selected <> nil) then
    SandDlg.SaveResource(fFrontLv.Selected.SubItems[0]);
end;

procedure TLiveHeaders.MenuSaveFromWebClick(Sender: TObject);
begin
  if (fFrontLv.Selected <> nil) then
    SandDlg.SaveURLAs(fFrontLv.Selected.SubItems[0]);
end;

constructor TLiveHeaders.Create(AOwner: TComponent);
var
  mi: TMenuItem;
  procedure AddColumn(lv: TListView; c: string; w: integer = 0);
  begin
    with lv.Columns.Add do
    begin
      Caption := c;
      if c = 'URL' then
        autosize := true
      else
      begin
        width := w;
        autosize := false;
      end;
    end;
  end;
  procedure AddColums(lv: TListView);
  begin
    AddColumn(lv, 'Method', 60);
    AddColumn(lv, 'URL');
    AddColumn(lv, 'Params', 100);
    AddColumn(lv, 'Status', 50);
    AddColumn(lv, 'Content Type', 100);
    AddColumn(lv, 'Length', 80);
    AddColumn(lv, 'Details', 300);
  end;
  procedure ConfigLV(lv: TListView);
  begin
    lv.Parent := self;
    lv.Align := alClient;
    lv.SmallImages := SandBrowser.LiveImages;
    lv.ReadOnly := true;
    lv.DoubleBuffered := true;
    lv.ViewStyle := vsReport;
    lv.RowSelect := true;
    lv.HideSelection := false;
    AddColums(lv);
  end;

begin
  inherited Create(AOwner);
  fMaxItems := 1000;
  ControlStyle := ControlStyle + [csAcceptsControls];
  fMainLv := TListView.Create(self);
  fMainLv.OnClick := HeadersListViewClick;
  fMainLv.OnColumnClick := HeadersListviewColumnClick;
  fMainLv.OnChange := HeadersListViewChange;
  ConfigLV(fMainLv);
  fFilterLv := TListView.Create(self);
  fFilterLv.OnClick := HeadersListViewClick;
  fFilterLv.OnColumnClick := FilteredListviewColumnClick;
  ConfigLV(fFilterLv);
  fMainLv.BringToFront;
  fFrontLv := fMainLv;
  fPaused := false;
  fFilterMode := false;
  fControlPanel := TPanel.Create(self);
  fControlPanel.Align := albottom;
  fControlPanel.ParentBackground := true;
  fControlPanel.ParentBackground := false;
  fControlPanel.Color := $00DEC9B0;
  fControlPanel.Parent := self;
  fControlPanel.Height := 22;
  fControlPanel.BevelInner := bvNone;
  fControlPanel.BevelOuter := bvNone;
  fFilterEdit := TEdit.Create(fControlPanel);
  fFilterEdit.Align := alClient;
  fFilterEdit.OnChange := FilterEditChange;
  fFilterEdit.TextHint := 'Display Filter';
  fFilterEdit.Parent := fControlPanel;

  fEraseBtn := TButton.Create(fControlPanel);
  fEraseBtn.Parent := fControlPanel;
  fEraseBtn.Align := alleft;
  fEraseBtn.Caption := emptystr;
  fEraseBtn.Hint := 'Clear Live Headers';
  fEraseBtn.width := 26;
  fEraseBtn.ImageAlignment := iaCenter;
  fEraseBtn.ShowHint := true;
  fEraseBtn.Images := SandBrowser.LiveImages;
  fEraseBtn.ImageIndex := 12;
  fEraseBtn.OnClick := ClearBtnClick;

  fPauseBtn := TButton.Create(fControlPanel);
  fPauseBtn.Parent := fControlPanel;
  fPauseBtn.Align := alleft;
  fPauseBtn.Caption := emptystr;
  fPauseBtn.Hint := 'Pause';
  fPauseBtn.width := 26;
  fPauseBtn.ImageAlignment := iaCenter;
  fPauseBtn.ShowHint := true;
  fPauseBtn.Images := SandBrowser.LiveImages;
  fPauseBtn.ImageIndex := 10;
  fPauseBtn.OnClick := PauseBtnClick;

  fFilterTimer := TTimer.Create(nil);
  fFilterTimer.Interval := 500;
  fFilterTimer.enabled := false;
  fFilterTimer.OnTimer := Timer1Timer;

  fPopupMenu := TPopupMenu.Create(self);
  // Copy menu item
  mi := TMenuItem.Create(fPopupMenu);
  mi.Caption := '&Copy';
  mi.OnClick := MenuCopyClick;
  fPopupMenu.Items.Add(mi);
  // Separator
  mi := TMenuItem.Create(fPopupMenu);
  mi.Caption := '-';
  fPopupMenu.Items.Add(mi);
  // Open URL in New Tab
  mi := TMenuItem.Create(fPopupMenu);
  mi.Caption := '&Open URL in New Tab';
  mi.OnClick := MenuOpenURLNewTabClick;
  fPopupMenu.Items.Add(mi);
  // Separator
  mi := TMenuItem.Create(fPopupMenu);
  mi.Caption := '-';
  fPopupMenu.Items.Add(mi);
  // Save From Cache As...
  mi := TMenuItem.Create(fPopupMenu);
  mi.Caption := '&Save From Cache As...';
  mi.OnClick := MenuSaveFromCacheClick;
  fPopupMenu.Items.Add(mi);
  // Save From Web As...
  mi := TMenuItem.Create(fPopupMenu);
  mi.Caption := '&Save From Web As...';
  mi.OnClick := MenuSaveFromWebClick;
  fPopupMenu.Items.Add(mi);
  // Render Response
  fMainLv.PopupMenu := fPopupMenu;
  fFilterLv.PopupMenu := fPopupMenu;
end;

destructor TLiveHeaders.Destroy;
begin
  fPopupMenu.Free;
  fFilterTimer.Free;
  fMainLv.Free;
  fFilterLv.Free;
  fFilterEdit.Free;
  fPauseBtn.Free;
  fEraseBtn.Free;
  fControlPanel.Free;
  inherited Destroy;
end;

end.
