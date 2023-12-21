unit uTabResponse;

{
  Sandcat Response Viewer
  Copyright (c) 2011-2017, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.SysUtils, System.Classes, Vcl.Controls, Vcl.ComCtrls, Vcl.Menus,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Tabs, Vcl.Dialogs,
{$ELSE}
  SysUtils, Classes, Controls, ComCtrls, Menus, StdCtrls, ExtCtrls, Graphics,
  Tabs, Dialogs,
{$ENDIF}
  uUIComponents, uLiveHeaders, CatChromiumOSRX, SynEditHighlighter,
  ExtPascalUtils, CatChromiumLib;

type
  TSandcatPreviewSettings = record
    Preview: string;
    PreviewHTML: string;
    Highlighter: TSynCustomHighlighter;
  end;

type
  TSandcatResponseTabDetails = record
    Method:string;
    URL:string;
    Headers_Request:string;
    Headers_Response:string;
    Response:string;
    Preview_Request:string;
    Preview_Response:string;
  end;

type
  TTabResponseView = class(TCustomControl)
  private
    fOSR: TCatChromiumOSRX;
    fHeadersRequest: TSandSynEditExtended;
    fHeadersResponse: TSandSynEditExtended;
    fMethodEdit: TEdit;
    fPreviewHTML: TSandUIEngine;
    fRespPages: TNotebook;
    fResponse: TSandSynEditExtended;
    fTabStrip: TTabSet;
    fURLEdit: TEdit;
    fURLPanel: TPanel;
    fSource: TSandSynEditExtended;
    function GetPreviewHTML(const r: TSandcatRequestDetails): string;
    function GetPreviewSettings(const r: TSandcatRequestDetails)
      : TSandcatPreviewSettings;
    procedure SourceAvailable(const s, headers: string);
    procedure LoadEnd(Sender: TObject; httpStatusCode: integer);
    procedure TabStrip1Change(Sender: TObject; NewTab: integer;
      var AllowChange: Boolean);
  public
    function GetResponse:TSandcatResponseTabDetails;
    procedure Clear;
    procedure LoadCachedURL(const url: string);
    procedure LoadFromRequest(const r: TSandcatRequestDetails);
    procedure SetHeaders(const headers:string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

uses uMain, CatStrings, CatHTTP, CatHTML, CatUI, uMisc, LAPI_Element;

const
  cPageNames: array [1 .. 3] of string = ('Preview', 'Headers',
    'Raw Response');

function HighlightHeaders(const s: string): string;
begin
  result := replacestr(s, crlf, '<br>');
  result := replacestr(result, 'GET', '<b>GET</b>');
  result := replacestr(result, 'POST', '<b>POST</b>');
  result := replacestr(result, 'HEAD', '<b>HEAD</b>');
  result := replacestr(result, 'HTTP/1.1', '<b>HTTP/1.1</b>');
  result := replacestr(result, 'HTTP/1.0', '<b>HTTP/1.0</b>');
  result := '<font color="black">' + result + '</font>';
end;

procedure TTabResponseView.Clear;
begin
  fHeadersRequest.Text := emptystr;
  fHeadersResponse.Text := emptystr;
  fMethodEdit.Text := emptystr;
  fURLEdit.Text := emptystr;
  fResponse.Text := emptystr;
  fSource.Text := emptystr;
  fSource.Highlighter := nil;
end;

function TTabResponseView.GetPreviewHTML
  (const r: TSandcatRequestDetails): string;
var
  urlext: string;
  restype: (unknown, image);
begin
  restype := unknown;
  urlext := ExtractUrlFileExt(lowercase(r.url));
  if ContainsAnyOfStrings(urlext, ['bmp,gif,ico,jpg,jpe,jpeg,png,svg']) then
    restype := image;
  if BeginsWith(r.MimeType, 'image/') then
    restype := image;
  case restype of
    image:
      result := '<div style="background-color:white;padding:5px;border:0;"><img src="'
        + htmlescape(r.url) + '"></div>';
  end;
end;

function TTabResponseView.GetPreviewSettings(const r: TSandcatRequestDetails)
  : TSandcatPreviewSettings;
var
  urlext: string;
begin
  urlext := ExtractUrlFileExt(lowercase(r.url));
  result.Preview := r.response;
  result.PreviewHTML := GetPreviewHTML(r);
  if urlext = '.js' then
    result.Preview := BeautifyJS(result.Preview)
  else if urlext = '.css' then
    result.Preview := BeautifyCSS(result.Preview);
  result.Highlighter := Highlighters.GetByContentType(r.MimeType);
  if result.Highlighter = nil then
    result.Highlighter := Highlighters.GetByFileExtension(urlext);
  if result.Highlighter = nil then
    result.Highlighter := Highlighters.GetByResponseText(r.response);
end;

procedure TTabResponseView.LoadCachedURL(const url: string);
begin
  Clear;
  if fOSR = nil then
  begin
    fOSR := TCatChromiumOSRX.Create(self);
    //fOSR.OnLoadEnd := LoadEnd;
    fOSR.OnAfterSetSourceSpecial := SourceAvailable;
  end;
  //if fOSR.isLoading then
  //  fOSR.Stop;
  fOSR.LoadFromCache(url);
end;

function TTabResponseView.GetResponse:TSandcatResponseTabDetails;
var
  e: ISandUIElement;
begin
  result.Method := fMethodEdit.Text;
  result.URL := fURLEdit.Text;
  result.Headers_Request := fHeadersRequest.Text;
  result.Headers_Response := fHeadersResponse.Text;
  result.Preview_Response := fSource.Text;
  result.Preview_Request := emptystr;
  if fPreviewHTML <> nil then begin
    e := fPreviewHTML.Root.Select('#rt_reqhead');
    if e <> nil then
      result.Preview_Request := ElemValueToText(e.Value);
  end;
end;

procedure TTabResponseView.SetHeaders(const headers:string);
var
  e: ISandUIElement;
begin
  e := fPreviewHTML.Root.Select('#rt_reqhead');
  if e <> nil then
    e.Value := HighlightHeaders(headers);
end;

procedure TTabResponseView.LoadFromRequest(const r: TSandcatRequestDetails);
var
  e: ISandUIElement;
  prev: TSandcatPreviewSettings;
  procedure CreateHTMLPreviewer;
  begin
    fTabStrip.TabIndex := 0;
    fPreviewHTML := TSandUIEngine.Create(self);
    fPreviewHTML.Parent :=
      TPage(fRespPages.Pages.Objects[fRespPages.Pages.IndexOf('Preview')]);
    fPreviewHTML.Align := alTop;
    fPreviewHTML.LoadHtml(uix.Pages.Tab_Response, pluginsdir);
  end;

begin
  if r.StatusCode = 304 then
    LoadCachedURL(r.url);
  if r.StatusCode = 304 then
    Exit;
  Clear;
  prev := GetPreviewSettings(r);
  fMethodEdit.Text := r.Method;
  fURLEdit.Text := r.url;
  fSource.Text := prev.Preview;
  fSource.Filename := extracturlfilename(r.url);
  fSource.Highlighter := prev.Highlighter;
  fHeadersRequest.Text := r.SentHead;
  fHeadersResponse.Text := r.RcvdHead;
  fResponse.Text := r.response;
  fResponse.Filename := extracturlfilename(r.url);
  if fPreviewHTML = nil then
    CreateHTMLPreviewer;
  e := fPreviewHTML.Root.Select('#rt_preview');
  if e <> nil then
    e.Value := prev.PreviewHTML;
  SetHeaders(r.SentHead);
  // fPreviewHTML.LoadHtml(prev.PreviewHTML, pluginsdir);
end;

procedure TTabResponseView.LoadEnd(Sender: TObject; httpStatusCode: integer);
begin
  //if fOSR.IsCachedURL then
  //  fOSR.GetSourceAsText;
end;

procedure TTabResponseView.SourceAvailable(const s, headers: string);
var
  r: TSandcatRequestDetails;
begin
  r.url := fOSR.GetURLSpecial;
  r.Method := 'Cached';
  r.SentHead := headers;
  r.response := s;
  LoadFromRequest(r);
end;

procedure TTabResponseView.TabStrip1Change(Sender: TObject; NewTab: integer;
  var AllowChange: Boolean);
begin
  fRespPages.ActivePage := fTabStrip.Tabs[NewTab];
end;

constructor TTabResponseView.Create(AOwner: TComponent);
var
  i: integer;
  function GetPage(aPageName: string): TPage;
  begin
    result := TPage(fRespPages.Pages.Objects
      [fRespPages.Pages.IndexOf(aPageName)]);
  end;

begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  BorderWidth := 10;
  fURLPanel := TPanel.Create(self);
  fURLPanel.Align := alTop;
  fURLPanel.ParentBackground := true;
  fURLPanel.ParentBackground := false;
  fURLPanel.Color := clWhite;
  fURLPanel.Parent := self;
  fURLPanel.Height := 22;
  fURLPanel.BevelInner := bvNone;
  fURLPanel.BevelOuter := bvNone;
  fURLEdit := TEdit.Create(fURLPanel);
  fURLEdit.Parent := fURLPanel;
  fURLEdit.Align := alClient;
  fURLEdit.ReadOnly := true;
  fMethodEdit := TEdit.Create(fURLPanel);
  fMethodEdit.Parent := fURLPanel;
  fMethodEdit.Align := alLeft;
  fMethodEdit.Color := clBlack;
  fMethodEdit.Width := 80;
  fMethodEdit.Font.Color := clWhite;
  fMethodEdit.ReadOnly := true;
  // create tab strip
  fTabStrip := TTabSet.Create(self);
  fTabStrip.Parent := self;
  fTabStrip.Align := alTop;
  fTabStrip.Style := tsModernPopout;
  fTabStrip.TabPosition := tpTop;
  for i := Low(cPageNames) to High(cPageNames) do
    fTabStrip.Tabs.Add(cPageNames[i]);
  fTabStrip.TabIndex := 0;
  fTabStrip.onChange := TabStrip1Change;

  // create tab control
  fRespPages := TNotebook.Create(self);
  fRespPages.Parent := self;
  fRespPages.Align := alClient;
  for i := High(cPageNames) downto Low(cPageNames) do
    fRespPages.Pages.Add(cPageNames[i]);

  // create source edit
  fSource := TSandSynEditExtended.Create(self);
  fSource.Parent := GetPage('Preview');
  fSource.Align := alClient;
  fSource.ReadOnly := true;
  ConfigSynEdit(fSource);

  // create response memo
  fResponse := TSandSynEditExtended.Create(self);
  fResponse.Align := alClient;
  fResponse.Parent := GetPage('Raw Response');
  fResponse.ReadOnly := true;
  ConfigSynEdit(fResponse);

  // create header edits
  fHeadersRequest := TSandSynEditExtended.Create(self);
  fHeadersRequest.Parent := GetPage('Headers');
  fHeadersRequest.Align := alTop;
  fHeadersRequest.ReadOnly := true;
  fHeadersResponse := TSandSynEditExtended.Create(self);
  fHeadersResponse.Parent := GetPage('Headers');
  fHeadersResponse.Align := alClient;
  fHeadersResponse.ReadOnly := true;
end;

destructor TTabResponseView.Destroy;
begin
  if fOSR <> nil then
    fOSR.Free;
  if fPreviewHTML <> nil then
    fPreviewHTML.Free;
  fResponse.Free;
  fHeadersRequest.Free;
  fHeadersResponse.Free;
  fSource.Free;
  fTabStrip.Free;
  fRespPages.Free;
  fMethodEdit.Free;
  fURLEdit.Free;
  fURLPanel.Free;
  inherited Destroy;
end;

end.
