unit CatChromium;

{
  Catarinka Browser Component (WebView2-Based)
  Copyright (c) 2011-2023 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}
{$I webview2.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  Vcl.Forms, System.SysUtils, System.SyncObjs, Vcl.Dialogs, Vcl.Clipbrd,
  System.TypInfo, Vcl.ExtCtrls, Winapi.ActiveX, Winapi.ShellAPI,
{$ELSE}
  Classes, Windows, Messages, Controls, Graphics, Forms, SysUtils, SyncObjs,
  Dialogs, Clipbrd, TypInfo, ExtCtrls, ActiveX, ShellAPI,
{$ENDIF}
  uWVBrowser, uWVWinControl, uWVWindowParent, uWVTypes, uWVConstants, uWVTypeLibrary,
  uWVLibFunctions, uWVLoader, uWVInterfaces, uWVCoreWebView2Args,
  uWVBrowserBase, uWVCoreWebView2HttpRequestHeaders,uWVCoreWebView2Delegates,
  uWVCoreWebView2HttpResponseHeaders,uWVCoreWebView2HttpHeadersCollectionIterator,
  uWVCoreWebView2WebResourceRequest, uLiveHeaders,
  CatJSON, CatMsg, CatMsgCromis, CatChromiumLib, CatUtils,
  CatAxUtils;

type
  TCatChromium = class(TWVWindowParent)
  private
    fDevTools: TCatDevTools;
    fAdjustSourceDisplayMethod: Boolean;
    fAutoGetSource: Boolean;
    fBrowserCreated : Boolean;
    fCriticalSection: TCriticalSection;
    fCrm: TWVBrowser;
    fEnableDownloads: Boolean;
    fHeaders: TCatRequestHeaders;
    fInterceptRequests: Boolean;
    fLastStatusCode: integer;
    fLastTitle: string;
    fLastJSExecutionResult: string;
    fLoggedRequests: array of TSandcatRequestDetails;
    fLoggedRequestsCount: int64;
    fLogJavaScriptErrors: Boolean;
    fLogURLs: Boolean;
    fMsg: TCatMsg;
    fMsgCrom: TCatMsgCromis;
    fNeedRecreate: Boolean;
    fOnBrowserMessage: TCatChromiumOnBrowserMessage;
    fOnAfterSetSource: TCatChromiumOnAfterSetSource;
    fOnTitleChange: TCatChromiumOnTitleChange;
    fOnLoadEnd: TCatChromiumOnLoadEnd;
    fOnLoadStart: TCatChromiumOnLoadStart;
    fOnAddressChange: TCatChromiumOnAddressChange;
    fOnStatusMessage: TCatChromiumOnStatusMessage;
    // fOnRequestComplete:TCatChromiumOnRequestComplete;
    fOnBeforePopup: TCatChromiumOnBeforePopup;
    fOnConsoleMessage: TCatChromiumOnConsoleMessage;
    //fOnBeforeResourceLoad: TCatChromiumOnBeforeResourceLoad;
    fOnBeforeDownload: TCatChromiumOnBeforeDownload;
    //fOnCertificateError: TCatChromiumOnCertificateError;
    fOnDownloadUpdated: TCatChromiumOnDownloadUpdated;
    fOnLoadingStateChange: TCatChromiumOnLoadingStateChange;
    fOnLoadError: TCatChromiumOnLoadError;
    fOnRequestDone: TCatChromiumOnRequestDone;
    fPreventPopup: Boolean;
    fResourceList: TStringList;
    fSentRequests: integer;
    fSource: string;
    fSplitter: TSplitter;
    fURLLog: TStringList;
    fUserAgent:string;
    procedure CreateBrowser(const clearrd:boolean);
    procedure ClearRequestData;
    procedure UpdateNavButtons(const aIsNavigating : boolean);
    procedure WVBrowser1StatusBarTextChanged(Sender: TObject; const aWebView: ICoreWebView2);
    procedure WVBrowser1WebMessageReceived(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2WebMessageReceivedEventArgs);
    procedure WVBrowser1WebResourceResponseReceived(Sender: TObject;
     const aWebView: ICoreWebView2; const aArgs: ICoreWebView2WebResourceResponseReceivedEventArgs);
    procedure WVBrowser1NavigationStarting(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2NavigationStartingEventArgs);
    procedure WVBrowser1NavigationCompleted(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2NavigationCompletedEventArgs);
    procedure WVBrowser1RetrieveHTMLCompleted(Sender: TObject; aResult: Boolean; const aHTML: wvstring);
    procedure WVBrowser1RetrieveTextCompleted(Sender: TObject; aResult: Boolean; const aText: wvstring);
    procedure WVBrowser1URLChanged(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2SourceChangedEventArgs);
    procedure WVBrowser1WebResourceResponseViewGetContentCompleted(Sender: TObject; aErrorCode: HRESULT; const aContents: IStream; aResourceID: Integer);
    procedure LogRequest(const json: string);
    procedure LogURL(const url: string);
    procedure SendMessageToTab(const id: integer; const s: string);
    procedure SetZoomLevel(const zl: double);
    function GetZoomLevel: double;
    function GetURLShort: string;
    procedure WMCopyData(const msgid: integer; const str: string);
    procedure WVBrowser1AfterCreated(Sender: TObject);
    procedure WVBrowser1InitializationError(Sender: TObject;
       aErrorCode: HRESULT; const aErrorMessage: wvstring);
    procedure WVBrowser1DocumentTitleChanged(Sender: TObject);
    procedure WVBrowser1OfflineCompleted(Sender: TObject; aResult: boolean);
    procedure WVBrowser1ExecuteScriptCompleted(Sender: TObject; aErrorCode: HRESULT; const aResultObjectAsJson: wvstring; aExecutionID: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function EvalJavaScript(const script: string;ExecutionID:integer): string;
    function GetURL: string;
    procedure AddToResourceList(const url: string);
    procedure GoBack;
    procedure GoForward;
    function IsFrameNil: Boolean;
    procedure Load(const url: string);
    procedure LoadBlank(const WaitLoad: Boolean = false);
    procedure LoadFromString(const s, url: string);
    procedure LoadSettings(settings, DefaultSettings: TCatJSON);
    procedure NotifyParentWindowPositionChanged;
    procedure RunJavaScript(const script: string); overload;
    procedure RunJavaScript(const Script: TCatCustomJSCall); overload;
    procedure RegisterNewV8Extension(const v8js: string);
    procedure Reload(const IgnoreCache: Boolean = false);
    procedure SendMessage(const msg: integer; const msgstr: string);
    procedure SendRequest(const req: TCatChromiumRequest;
      const Load: Boolean = false);
    procedure SetV8MsgHandle(const handle: integer);
    procedure SetUserAgent(const ua:string);
    procedure Stop(const waitstop: Boolean = false);
    procedure GetSource; // callback
    procedure GetSourceAsText; // callback
    procedure SetSource(const s: string);
    function isLoading: Boolean;
    procedure ShowAuthDialog(const Username: string = '';
      const Password: string = '');
    procedure ViewDevTools;
    procedure ViewSourceExternalEditor;
    // properties
    property AdjustSourceDisplayMethod: Boolean read fAdjustSourceDisplayMethod
      write fAdjustSourceDisplayMethod;
    property Crm: TWVBrowser read fCrm;
    property EnableDownloads: Boolean read fEnableDownloads
      write fEnableDownloads;
    property Headers: TCatRequestHeaders read fHeaders;
    property InterceptRequests: Boolean read fInterceptRequests
      write fInterceptRequests;
    property LogURLs: Boolean read fLogURLs write fLogURLs;
    property LogJavaScriptErrors: Boolean read fLogJavaScriptErrors
      write fLogJavaScriptErrors;
    property ResourceList: TStringList read fResourceList;
    property title: string read fLastTitle;
    property URLLog: TStringList read fURLLog;
    property URLShort: string read GetURLShort;
    property UserAgent: string read fUserAgent write SetUserAgent;
    property ZoomLevel: double read GetZoomLevel write SetZoomLevel;
  published
    property OnAfterSetSource: TCatChromiumOnAfterSetSource
      read fOnAfterSetSource write fOnAfterSetSource;
    property OnBrowserMessage: TCatChromiumOnBrowserMessage
      read fOnBrowserMessage write fOnBrowserMessage;
    //property OnCertificateError: TCatChromiumOnCertificateError
    //  read fOnCertificateError write fOnCertificateError;
    property OnLoadEnd: TCatChromiumOnLoadEnd read fOnLoadEnd write fOnLoadEnd;
    property OnLoadStart: TCatChromiumOnLoadStart read fOnLoadStart
      write fOnLoadStart;
    property OnTitleChange: TCatChromiumOnTitleChange read fOnTitleChange
      write fOnTitleChange;
    property OnAddressChange: TCatChromiumOnAddressChange read fOnAddressChange
      write fOnAddressChange;
    property OnRequestDone: TCatChromiumOnRequestDone read fOnRequestDone
      write fOnRequestDone;
    property OnStatusMessage: TCatChromiumOnStatusMessage read fOnStatusMessage
      write fOnStatusMessage;
    // property OnRequestComplete:TCatChromiumOnRequestComplete read FOnRequestComplete write FOnRequestComplete;
    property OnBeforePopup: TCatChromiumOnBeforePopup read fOnBeforePopup
      write fOnBeforePopup;
    property OnConsoleMessage: TCatChromiumOnConsoleMessage
      read fOnConsoleMessage write fOnConsoleMessage;
    //property OnBeforeResourceLoad: TCatChromiumOnBeforeResourceLoad
    //  read fOnBeforeResourceLoad write fOnBeforeResourceLoad;
    property OnBeforeDownload: TCatChromiumOnBeforeDownload
      read fOnBeforeDownload write fOnBeforeDownload;
    property OnDownloadUpdated: TCatChromiumOnDownloadUpdated
      read fOnDownloadUpdated write fOnDownloadUpdated;
    property OnLoadingStateChange: TCatChromiumOnLoadingStateChange
      read fOnLoadingStateChange write fOnLoadingStateChange;
    property OnLoadError: TCatChromiumOnLoadError read fOnLoadError
      write fOnLoadError;
  end;

implementation

uses uAuthentication, CatStringLoop, CatCSCommand, CatUI, CatStrings, CatMatch, CatHTTP,
 uSettings, uConst, CatFiles;

// ------------------------------------------------------------------------//
// TCatChromium                                                            //
// ------------------------------------------------------------------------//

procedure TCatChromium.SendMessage(const msg: integer; const msgstr: string);
//var
  //m: ICefProcessMessage;
begin
  {if fCrm.Browser = nil then
    exit;
  m := TCefProcessMessageRef.New('msg');
  m.getArgumentList.SetInt(0, msg);
  m.getArgumentList.SetString(1, msgstr);
  fCrm.Browser.SendProcessMessage(PID_RENDERER, m);   }
end;

procedure TCatChromium.RegisterNewV8Extension(const v8js: string);
begin
  SendMessage(SCTM_V8_REGISTEREXTENSION, v8js);
end;

procedure TCatChromium.SetV8MsgHandle(const handle: integer);
//var
  //m: ICefProcessMessage;
begin
  {if fCrm.Browser = nil then
    exit;
  m := TCefProcessMessageRef.New('msg');
  m.getArgumentList.SetInt(0, SCTM_SET_V8_MSGHANDLE);
  m.getArgumentList.SetInt(1, handle);
  fCrm.Browser.SendProcessMessage(PID_RENDERER, m);  }
end;

procedure TCatChromium.SetUserAgent(const ua:string);
begin
  fUserAgent := ua;
  if ua<>emptystr then
    fcrm.UserAgent := ua;
end;

function TCatChromium.GetZoomLevel: double;
begin
  Result := 0;
  if fCrm = nil then
    exit;
  result := fCrm.ZoomFactor;
end;

procedure TCatChromium.SetZoomLevel(const zl: double);
begin
  if fCrm <> nil then
    fCrm.ZoomFactor := zl;
end;

procedure TCatChromium.GoBack;
begin
  if fCrm <> nil then
    fCrm.GoBack;
end;

procedure TCatChromium.GoForward;
begin
  if fCrm <> nil then
    fCrm.GoForward;
end;

// Url has changed
procedure TCatChromium.WVBrowser1URLChanged(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2SourceChangedEventArgs);
begin
  if assigned(OnAddressChange) then
    OnAddressChange(Sender, fcrm.Source);
end;

procedure TCatChromium.SetSource(const s: string);
begin
  fSource := s;
  if assigned(OnAfterSetSource) then
    OnAfterSetSource(s);
end;

// kept for compatibility with CEF's Based CatChromium.pas
function TCatChromium.IsFrameNil: Boolean;
begin
  Result := false;
end;

procedure TCatChromium.GetSourceAsText;
begin
  fcrm.RetrieveText;
end;

procedure TCatChromium.GetSource;
var
  ext: string;
  showtext: Boolean;
begin
  ext := lowercase(extracturlfileext(GetURL));
  showtext := false;
  if fAdjustSourceDisplayMethod and MatchStrInArray(ext,['.js','.css','.xml'],true) then
      showtext := true;
  if showtext then
    fcrm.RetrieveText
    else
    fcrm.RetrieveHTML;
end;

function TCatChromium.GetURL: string;
begin
  if fCrm = nil then
    result := emptystr else
    result := fcrm.Source;
end;

function ExtractURLHostShort(const url:string):string;
var
  u:string;
begin
    u := CatHTTP.ExtractURLHost(u);
    u := lowercase(u);
    if beginswith(u, 'www.') then
    begin
      u := after(u, '.');
      u := before(u, '.');
    end
    else
    begin
      u := before(u, '.');
    end;
    Result := u;
end;

function TCatChromium.GetURLShort: string;
var
  u: string;
begin
  u := GetURL;
  if u = cURL_HOME then
    Result := emptystr
  else
    Result := ExtractURLHostShort(u);
end;

function TCatChromium.EvalJavaScript(const script: string; ExecutionID:integer): string;
var
  evalresult: string;
  ms:int64;
  function waitresult:boolean;
  begin
    inc(ms);
    result := true;
    if fLastJSExecutionResult <> emptystr then
      result := false;
    if ms > 10000 then
      result := false;
  end;
begin
  fLastJSExecutionResult := emptystr;
  fcrm.ExecuteScript(script,ExecutionID);
  while waitresult = true do
    catdelay(1);
  evalresult := fLastJSExecutionResult;
  result := evalresult;
end;

procedure TCatChromium.RunJavaScript(const script: string);
var
  s: TCatCustomJSCall;
begin
  s.Code := script;
  s.ExecutionID := 0;
  //s.URL := aURL;
  //s.StartLine := StartLine;
  s.Silent := false;
  RunJavaScript(s);
end;

procedure TCatChromium.RunJavaScript(const Script: TCatCustomJSCall);
begin
  fLogJavaScriptErrors := not script.silent;
  // Chromium will not execute the JS if no URL is loaded,
  // so we load a blank URL before
  if GetURL = emptystr then
    LoadBlank;
  if fCrm = nil then
    exit;
  fcrm.ExecuteScript(script.Code, script.ExecutionID);
end;

{procedure TCatChromium.crmGetAuthCredentials(Sender: TObject;
  const Browser: ICefBrowser; const frame: ICefFrame; isProxy: Boolean;
  const host: ustring; port: integer; const realm, scheme: ustring;
  const Callback: ICefAuthCallback; out Result: Boolean);
var
  u, p: ustring;
  r: Boolean;
begin
  TThread.Synchronize(nil,
    procedure
    begin
      with TPasswordDlg.Create(nil) do
        try
          if ShowModal = mrOk then
          begin
            u := edtusername.text;
            p := edtPassword.text;
            r := true;
          end
          else
            r := false;
        finally
          Free;
        end
    end);

  Result := r;
  if r = true then
    Callback.Cont(u, p);
end;  }

procedure TCatChromium.ShowAuthDialog(const Username: string = '';
const Password: string = '');
var
  u, p: string;
  r: Boolean;
begin
  with TPasswordDlg.Create(nil) do
    try
      edtusername.text := Username;
      edtPassword.text := Password;
      if ShowModal = mrOk then
      begin
        u := edtusername.text;
        p := edtPassword.text;
        r := true;
      end
      else
        r := false;
    finally
      Free;
    end;
  if r = true then
  begin
    {req := TCefRequestRef.New;
    req.url := GetURL;
    req.Method := 'GET';
    Map := TCefStringMultiMapOwn.Create;
    req.GetHeaderMap(Map);
    Map.Append('Authorization', 'Basic ' + base64encode(u + ':' + p));
    req.SetHeaderMap(Map);
    fCrm.Browser.MainFrame.LoadRequest(req);        }
  end;
end;

{
procedure TCatChromium.crmLoadEnd(Sender: TObject; const Browser: ICefBrowser;
const frame: ICefFrame; httpStatusCode: integer);
begin
  if IsMain(Browser, frame) = false then
    exit;
  fLastStatusCode := httpStatusCode;
  if assigned(OnLoadEnd) then
    OnLoadEnd(Sender, httpStatusCode);
  if fAutoGetSource then
    GetSource;
end;

procedure TCatChromium.crmLoadStart(Sender: TObject; const Browser: ICefBrowser;
const frame: ICefFrame; transitionType: TCefTransitionType);
begin
  if IsMain(Browser, frame) = false then
    exit;
  fResourceList.clear;
  if assigned(OnLoadStart) then
    OnLoadStart(Sender);
end;

procedure TCatChromium.crmLoadError(Sender: TObject; const Browser: ICefBrowser;
const frame: ICefFrame; errorCode: integer;
const errorText, failedUrl: ustring);
begin
  if IsMain(Browser, frame) = false then
    exit;
  if assigned(OnLoadError) then
    OnLoadError(Sender, integer(errorCode), errorText, failedUrl);
end;

procedure TCatChromium.crmLoadingStateChange(Sender: TObject;
const Browser: ICefBrowser; isLoading, canGoBack, canGoForward: Boolean);
begin
  if IsMain(Browser) = false then
    exit;
  if assigned(OnLoadingStateChange) then
    OnLoadingStateChange(Sender, isLoading, canGoBack, canGoForward);
end;

procedure TCatChromium.crmPluginCrashed(Sender: TObject;
const Browser: ICefBrowser; const pluginPath: ustring);
begin
  // TODO
end;

procedure TCatChromium.crmTitleChange(Sender: TObject;
const Browser: ICefBrowser; const title: ustring);
begin
  if IsMain(Browser) = false then
    exit;
  fLastTitle := title;
  if assigned(OnTitleChange) then
    OnTitleChange(Sender, title);
end;

procedure TCatChromium.crmBeforeContextMenu(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame;
const params: ICefContextMenuParams; const model: ICefMenuModel);
var
  fn: string;
  addsep, canclear: Boolean;
  framemodel: ICefMenuModel;
begin
  addsep := false;
  canclear := true;
  if params.IsEditable = false then
  begin
    if not(CM_TYPEFLAG_SELECTION in params.TypeFlags) then
    begin
      model.InsertSeparatorAt(2);
      model.InsertItemAt(3, integer(MENU_ID_RELOAD), 'Reload');
      model.InsertItemAt(4, integer(MENU_ID_RELOAD_NOCACHE),
        'Reload (Ignore Cache)');
      model.InsertSeparatorAt(5);
      model.InsertItemAt(6, integer(CRMMENU_ID_PAGE_BOOKMARK), 'Bookmark Page');
      model.InsertItemAt(7, integer(CRMMENU_ID_PAGE_SAVEAS), 'Save Page As...');
      model.InsertItemAt(8, CRMMENU_ID_COPYADDRESS, 'Copy Location');
      model.InsertSeparatorAt(9);
      model.InsertItemAt(10, integer(MENU_ID_SELECT_ALL), 'Select All');
      if CM_TYPEFLAG_FRAME in params.TypeFlags then
      begin
        model.AddSeparator;
        framemodel := model.AddSubMenu(CRMMENU_ID_FRAMEMENU, 'Frame');
        framemodel.AddItem(CRMMENU_ID_FRAMEMENU_OPEN, 'Open');
        framemodel.AddItem(CRMMENU_ID_FRAMEMENU_OPEN_INNEWTAB,
          'Open in New Tab');
        framemodel.AddItem(CRMMENU_ID_FRAMEMENU_OPEN_INBGTAB,
          'Open in Background Tab');
        framemodel.AddSeparator;
        framemodel.AddItem(CRMMENU_ID_FRAMEMENU_COPYADDRESS, 'Copy Location')
      end;
    end
    else
    begin
      model.AddSeparator;
      model.AddItem(CRMMENU_ID_SEARCH, 'Search');
      model.AddItem(CRMMENU_ID_SEARCH_INNEWTAB, 'Search in New Tab');
    end;
  end;
  if CM_TYPEFLAG_LINK in params.TypeFlags then
  begin
    if canclear then
      model.clear;
    canclear := false;
    model.AddItem(CRMMENU_ID_OPENLINK, 'Open Link Location');
    model.AddItem(CRMMENU_ID_OPENLINK_INNEWTAB, 'Open Link in New Tab');
    model.AddItem(CRMMENU_ID_OPENLINK_INBGTAB, 'Open Link in Background Tab');
    model.AddSeparator;
    model.AddItem(CRMMENU_ID_LINK_BOOKMARK, 'Bookmark Link');
    model.AddItem(CRMMENU_ID_LINK_COPYADDRESS, 'Copy Link');
    model.AddItem(CRMMENU_ID_LINK_SAVEAS, 'Save Link As...');
    addsep := true;
  end;
  // CM_TYPEFLAG_FRAME
  // CM_TYPEFLAG_SELECTION
  // CM_TYPEFLAG_EDITABLE
  if CM_TYPEFLAG_MEDIA in params.TypeFlags then
  begin
    if CM_MEDIATYPE_IMAGE = params.MediaType then
    begin
      if canclear then
        model.clear;
      // canclear := false;
      if addsep then
        model.AddSeparator;
      fn := extracturlfilename(params.SourceUrl);
      if Length(fn) <= 50 then
        model.AddItem(CRMMENU_ID_OPENIMAGE, 'Open Image (' + fn + ')')
      else
        model.AddItem(CRMMENU_ID_OPENIMAGE, 'Open Image');
      model.AddItem(CRMMENU_ID_OPENIMAGE_INNEWTAB, 'Open Image in New Tab');
      model.AddSeparator;
      model.AddItem(CRMMENU_ID_COPYIMAGEADDRESS, 'Copy Image Location');
      model.AddItem(CRMMENU_ID_SAVEIMAGEAS, 'Save Image As...');
    end;
  end;
end;

procedure TCatChromium.crmContextMenuCommand(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame;
const params: ICefContextMenuParams; commandId: integer;
eventFlags: TCefEventFlags; out Result: Boolean);
begin
  case commandId of
    CRMMENU_ID_OPENLINK:
      Load(params.LinkUrl);
    CRMMENU_ID_OPENLINK_INNEWTAB:
      SendMessageToTab(CRM_NEWTAB, params.LinkUrl);
    CRMMENU_ID_OPENLINK_INBGTAB:
      SendMessageToTab(CRM_NEWTAB_INBACKGROUND, params.LinkUrl);
    CRMMENU_ID_OPENIMAGE:
      Load(params.SourceUrl);
    CRMMENU_ID_OPENIMAGE_INNEWTAB:
      SendMessageToTab(CRM_NEWTAB, params.SourceUrl);
    CRMMENU_ID_COPYIMAGEADDRESS:
      clipboard.AsText := params.SourceUrl;
    CRMMENU_ID_SAVEIMAGEAS:
      SendMessageToTab(CRM_SAVECACHEDRESOURCE, params.SourceUrl);
    CRMMENU_ID_COPYADDRESS:
      clipboard.AsText := GetURL;
    CRMMENU_ID_SEARCH:
      SendMessageToTab(CRM_SEARCHWITHENGINE, params.SelectionText);
    CRMMENU_ID_SEARCH_INNEWTAB:
      SendMessageToTab(CRM_SEARCHWITHENGINE_INNEWTAB, params.SelectionText);
    CRMMENU_ID_LINK_COPYADDRESS:
      clipboard.AsText := params.LinkUrl;
    CRMMENU_ID_FRAMEMENU_OPEN:
      Load(params.FrameUrl);
    CRMMENU_ID_FRAMEMENU_OPEN_INNEWTAB:
      SendMessageToTab(CRM_NEWTAB, params.FrameUrl);
    CRMMENU_ID_FRAMEMENU_OPEN_INBGTAB:
      SendMessageToTab(CRM_NEWTAB_INBACKGROUND, params.FrameUrl);
    CRMMENU_ID_FRAMEMENU_COPYADDRESS:
      clipboard.AsText := params.FrameUrl;
    CRMMENU_ID_PAGE_SAVEAS:
      SendMessageToTab(CRM_SAVECACHEDRESOURCE, GetURL);
    CRMMENU_ID_LINK_SAVEAS:
      SendMessageToTab(CRM_SAVECLOUDRESOURCE, params.LinkUrl);
    CRMMENU_ID_PAGE_BOOKMARK:
      SendMessageToTab(CRM_BOOKMARKURL, GetURL);
    CRMMENU_ID_LINK_BOOKMARK:
      SendMessageToTab(CRM_BOOKMARKURL, params.LinkUrl);
  end;
end;    }

procedure TCatChromium.WVBrowser1DocumentTitleChanged(Sender: TObject);
begin
  //if IsMain(Browser) = false then
  //  exit;
  fLastTitle :=  fcrm.DocumentTitle;
  if assigned(OnTitleChange) then
    OnTitleChange(Sender, fLastTitle);
end;

procedure TCatChromium.WVBrowser1NavigationStarting(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2NavigationStartingEventArgs);
begin
  //if IsMain(Browser, frame) = false then
  //  exit;
  UpdateNavButtons(true);
  fResourceList.clear;
  if assigned(OnLoadStart) then
    OnLoadStart(Sender);
end;

procedure TCatChromium.WVBrowser1NavigationCompleted(Sender: TObject; const aWebView: ICoreWebView2; const aArgs: ICoreWebView2NavigationCompletedEventArgs);
var
  TempArgs : TCoreWebView2NavigationCompletedEventArgs;
  httpStatusCode:integer;
begin
  TempArgs := TCoreWebView2NavigationCompletedEventArgs.Create(aArgs);
  httpStatusCode := TempArgs.HttpStatusCode;
  TempArgs.Free;
  fLastStatusCode := httpStatusCode;
  if assigned(OnLoadEnd) then
    OnLoadEnd(Sender, httpStatusCode);
  if fAutoGetSource then
    GetSource;
  UpdateNavButtons(False);
end;

procedure TCatChromium.SendMessageToTab(const id: integer; const s: string);
begin
  SendCDMessage(fMsg.MsgHandle, id, s);
end;


procedure TCatChromium.AddToResourceList(const url: string);
  function ResourceAllowed: Boolean;
  begin
    Result := true;
    if fResourceList.Count > 2000 then
      Result := false;
    if fResourceList.IndexOf(url) <> -1 then
      Result := false;
    if url = GetURL then
      Result := false;
    if pos('?', url) <> 0 then
      Result := false; // url with params, most likely not an object
  end;

begin
  if ResourceAllowed = false then
    exit;
  fResourceList.Add(url);
  if assigned(OnBrowserMessage) then
    OnBrowserMessage(CRM_NEWPAGERESOURCE, url);
end;

{
procedure TCatChromium.crmGetResourceHandler(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest;
out Result: ICefResourceHandler);
var
  req: ICefUrlRequest;
  reqown: TSpecialCEFReq;
  reqctx: ICefRequestContext;
begin
  if fInterceptRequests = false then
    exit;
  if Browser = nil then
    exit;
  fSentRequests := fSentRequests + 1;
  // sendmessagetotab(fmsg.msghandle,CRM_LOGWRITELN,'getresourcehandler:'+request.getUrl);
  reqown := TSpecialCEFReq.Create;
  reqown.MsgHandle := self.fMsgCrom.MsgHandle;
  req := TCefUrlRequestRef.New(request, reqown, reqctx) as ICefUrlRequest;
end;

procedure TCatChromium.crmBeforeResourceLoad(Sender: TObject;
const Browser: ICefBrowser; const frame: ICefFrame; const request: ICefRequest;
const Callback: ICefRequestCallback; out Result: TCefReturnValue);
begin
  // result:=false; // unnecessary
  // This would avoid a weird crash during navigation with past CEF3 releases
end;

procedure TCatChromium.crmBeforePopup(Sender: TObject;
const aBrowser: ICefBrowser; const aFrame: ICefFrame;
const aTargetUrl, aTargetFrameName: ustring;
aTargetDisposition: TCefWindowOpenDisposition; aUserGesture: Boolean;
var aPopupFeatures: TCefPopupFeatures; var aWindowInfo: TCefWindowInfo;
var aClient: ICefClient; var aSettings: TCefBrowserSettings;
var aNoJavascriptAccess: Boolean; out Result: Boolean);
var
  u: string;
begin
  Result := fPreventPopup;
  u := aTargetUrl;
  if fPreventPopup = true then
    SendMessageToTab(CRM_NEWTAB, u);
  if assigned(OnBeforePopup) then
    OnBeforePopup(Sender, u, Result);
end;

procedure TCatChromium.crmCertificateError(Sender: TObject;
const aBrowser: ICefBrowser; aCertError: TCefErrorCode;
const aRequestUrl: ustring; const aSslInfo: ICefSslinfo;
const aCallback: ICefRequestCallback; out Result: Boolean);
var
  button: integer;
  msg, caption: string;
begin
  if assigned(OnCertificateError) then
    OnCertificateError(Sender, aCertError, aRequestUrl, aSslInfo,
      aCallback, Result);
  msg := format('Warning: %s Proceed anyway?',
    [CertErrorCodeToErrorName(aCertError)]);
  caption := aRequestUrl;
  button := application.MessageBox(pWideChar(msg), pWideChar(caption),
    mb_YesNo + mb_DefButton1 + mb_ICONWARNING);
  case button of
    IDYes:
      aCallback.Cont(true);
    IDNo:
      aCallback.Cont(false);
  end;
end;

procedure TCatChromium.crmConsoleMessage(Sender: TObject;
const Browser: ICefBrowser; const message, source: ustring; line: integer;
out Result: Boolean);
begin
  if fLogJavaScriptErrors = false then
    exit;
  if assigned(OnConsoleMessage) then
    OnConsoleMessage(Sender, message, source, line);
end;

procedure TCatChromium.crmJsdialog(Sender: TObject; const browser: ICefBrowser; const originUrl: ustring;
    adialogType: TCefJsDialogType; const amessageText, defaultPromptText: ustring;
    callback: ICefJsDialogCallback; out asuppressMessage: Boolean; out Result: Boolean);
begin
  case aDialogType of
    JSDIALOGTYPE_ALERT:
      begin
        if assigned(OnBrowserMessage) then
          OnBrowserMessage(CRM_JS_ALERT, aMessageText);
        // ShowMessage(MessageText);
        aSuppressMessage := true;
      end;
  end;
end;  }

procedure TCatChromium.ViewDevTools;
begin
  if fCrm <> nil then
  fcrm.OpenDevToolsWindow;
  {if fCrm.Browser = nil then
    exit;
  if fCrm.Browser.GetHost = nil then
    exit;
  fDevTools.Align := AlBottom;
  fDevTools.View(fCrm.Browser);
  if fSplitter.visible = false then
  begin
    fDevTools.height := 300;
    fSplitter.visible := true;
    fSplitter.Align := AlBottom;
    fSplitter.Top := fDevTools.Top + 10;
  end;  }
end;

procedure TCatChromium.ViewSourceExternalEditor;
var
 sl:TStringList;
 fn:string;
begin
  fn := GetTempFile('.txt');
  sl := TStringList.Create;
  sl.Text := fSource;
  sl.SaveToFile(fn);
  sl.Free;
  ShellExecute(0,nil,pchar('notepad'),pchar(fn),nil,Integer(SW_SHOWNORMAL));
end;

{
procedure TCatChromium.crmBeforeDownload(Sender: TObject;
const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
const suggestedName: ustring; const Callback: ICefBeforeDownloadCallback);
var
  s: string;
  fn: ustring;
begin
  if fEnableDownloads = false then
    exit;
  s := suggestedName;
  // debug
  // sendmessagetotab(fmsg.msghandle,CRM_LOGWRITELN,'beforedownload:'+inttostr(downloaditem.getid));
  fn := GetSpecialFolderPath(CSIDL_PERSONAL, false) + '\' + suggestedName;
  Callback.Cont(fn, true);
  if assigned(OnBeforeDownload) then
    OnBeforeDownload(Sender, downloadItem.getid, s);
end;

procedure TCatChromium.crmDownloadUpdated(Sender: TObject;
const Browser: ICefBrowser; const downloadItem: ICefDownloadItem;
const Callback: ICefDownloadItemCallback);
var
  cancel: Boolean;
  state: integer;
begin
  if fEnableDownloads = false then
    exit;
  cancel := false;
  state := SCD_UNKNOWN;
  if downloadItem.IsInProgress then
  begin
    if downloadItem.IsValid then
      if downloadItem.getPercentComplete <> 0 then
        state := SCD_INPROGRESS;
  end;
  if downloadItem.IsComplete then
    state := SCD_COMPLETE;
  if downloadItem.IsCanceled then
    state := SCD_CANCELED;
  // debug
  // sendmessagetotab(fmsg.msghandle,CRM_LOGWRITELN,'downloadupdated: '+downloadstatetostr(state)+downloaditem.getfullpath);
  if assigned(OnDownloadUpdated) then
    OnDownloadUpdated(Sender, cancel, downloadItem.getid, state,
      downloadItem.getPercentComplete, downloadItem.getfullpath);
  if cancel = true then
    Callback.cancel;
end;   }

procedure TCatChromium.LogURL(const url: string);
begin
  if url = emptystr then
    exit;
  if fURLLog.IndexOf(url) = -1 then
    fURLLog.Add(url);
end;

procedure TCatChromium.LogRequest(const json: string);
var
  j: TCatJSON;
begin
  j := TCatJSON.Create;
  j.text := json;
  if fLogURLs = true then
    LogURL(j['url']);
  if j['mimetype'] <> 'text/html' then
    AddToResourceList(j['url']);
  if fHeaders.StatusCode = emptystr then
  begin // we just want the response for the first request
    if j['url'] = GetURL then
    begin
      fHeaders.SentHead := j['headers'];
      fHeaders.RcvdHead := j['responseheaders'];
      fHeaders.StatusCode := j['status'];
    end;
  end;
  j.Free;
end;

procedure TCatChromium.WMCopyData(const msgid: integer; const str: string);
begin
  case msgid of
    CRM_LOG_REQUEST_JSON:
      LogRequest(str);
  end;
  if assigned(OnBrowserMessage) then
    OnBrowserMessage(msgid, str);
end;

{
procedure TCatChromium.crmRenderProcessTerminated(Sender: TObject;
const Browser: ICefBrowser; status: TCefTerminationStatus);
begin
  if assigned(OnLoadEnd) then
    OnLoadEnd(Sender, 0);
end;  }


{
  // No longer supported in DCEF
  procedure TCatChromium.LoadCustomCSS;
  begin
  fneedrecreate:=true;
  FChrome.crm.options.UserStyleSheetEnabled :=true;
  FChrome.crm.UserStyleSheetLocation:=UserScript.CSS_UserStyleSheet;
  //FChrome.crm.Options.UniversalAccessFromFileUrlsAllowed:=true;
  //FChrome.Crm.Options.FileAccessFromFileUrlsAllowed:=true;
  end;
}

{
procedure TCatChromium.SetOptionState(settings, DefaultSettings: TCatJSON;
const curstate: TCefState; const propname: string);
var
  CID: string;
  value, DefaultValue: Boolean;
begin
  CID := cOptions + lowercase(propname);
  DefaultValue := DefaultSettings[CID];
  value := settings.GetValue(CID, DefaultValue);
  // GetPropValue(fCrm.Options,'ApplicationCache');
  if value <> CEFStateToBool(curstate) then
  begin
    fNeedRecreate := true;
    if value = true then
      SetPropValue(fCrm.Options, propname, STATE_ENABLED)
    else
      SetPropValue(fCrm.Options, propname, STATE_DISABLED);
  end;
end;
}

procedure TCatChromium.LoadSettings(settings, DefaultSettings: TCatJSON);
var
  ua:string;
begin
  ua := settings.GetValue(cSandcatOptions+'useragent', emptystr);
  SetUserAgent(fUserAgent);
  {// LoadCustomCSS;
  fPreventPopup := settings.GetValue(cSandcatOptions+'openpopupsinnewtab', false);
  fNeedRecreate := false;
  SetState(fCrm.Options.ApplicationCache, 'ApplicationCache');
  //SetState(fCrm.Options.CaretBrowsing, 'CaretBrowsing');
  SetState(fCrm.Options.Databases, 'Databases');
  SetState(fCrm.Options.FileAccessFromFileUrls, 'FileAccessFromFileUrls');
  SetState(fCrm.Options.ImageLoading, 'ImageLoading');
  SetState(fCrm.Options.ImageShrinkStandaloneToFit,
    'ImageShrinkStandaloneToFit');
  // SetState(fCrm.Options.Java, 'Java');
  SetState(fCrm.Options.Javascript, 'Javascript');
  SetState(fCrm.Options.JavascriptAccessClipboard, 'JavascriptAccessClipboard');
  SetState(fCrm.Options.JavascriptCloseWindows, 'JavascriptCloseWindows');
  SetState(fCrm.Options.JavascriptDomPaste, 'JavascriptDomPaste');
  SetState(fCrm.Options.JavascriptOpenWindows, 'JavascriptOpenWindows');
  SetState(fCrm.Options.LocalStorage, 'LocalStorage');
  SetState(fCrm.Options.Plugins, 'Plugins');
  SetState(fCrm.Options.TabToLinks, 'TabToLinks');
  SetState(fCrm.Options.TextAreaResize, 'TextAreaResize');
  SetState(fCrm.Options.UniversalAccessFromFileUrls,
    'UniversalAccessFromFileUrls');
  SetState(fCrm.Options.Webgl, 'Webgl');
  SetState(fCrm.Options.WebSecurity, 'WebSecurity');

  if fNeedRecreate then
  begin
    // showmessage(CEFStateToStr(crm.Options.Javascript));
    if fCrm.Browser <> nil then
    begin
      fNeedRecreate := false;
      ReCreateBrowser(GetURL);
    end;
  end;   }

end;

procedure TCatChromium.WVBrowser1AfterCreated(Sender: TObject);
begin
  self.UpdateSize;
  fcrm.AddWebResourceRequestedFilter('*', COREWEBVIEW2_WEB_RESOURCE_CONTEXT_IMAGE);
  fcrm.AddWebResourceRequestedFilter('*', COREWEBVIEW2_WEB_RESOURCE_CONTEXT_MEDIA);
end;

procedure TCatChromium.WVBrowser1InitializationError(Sender: TObject;
  aErrorCode: HRESULT; const aErrorMessage: wvstring);
begin
  showmessage(aErrorMessage);
end;

procedure TCatChromium.WVBrowser1RetrieveHTMLCompleted(Sender: TObject;
  aResult: Boolean; const aHTML: wvstring);
begin
  if aResult = true then
   SetSource(aHTML);
end;


procedure TCatChromium.WVBrowser1RetrieveTextCompleted(Sender: TObject;
  aResult: Boolean; const aText: wvstring);
begin
  if aResult = true then
    SetSource(aText);
end;

procedure TCatChromium.WVBrowser1WebMessageReceived(Sender: TObject;
const aWebView: ICoreWebView2; const aArgs: ICoreWebView2WebMessageReceivedEventArgs);
var
 a:TCoreWebView2WebMessageReceivedEventArgs;
begin
  if fLogJavaScriptErrors = false then
    exit;
 a := TCoreWebView2WebMessageReceivedEventArgs.Create(aArgs);
  if assigned(OnConsoleMessage) then
    OnConsoleMessage(Sender, a.WebMessageAsString, emptystr, 0);
 a.Free;
end;

procedure TCatChromium.WVBrowser1WebResourceResponseViewGetContentCompleted(Sender: TObject; aErrorCode: HRESULT; const aContents: IStream; aResourceID: Integer);
var
  infs:TInterfaceStream;
  req :TSandcatRequestDetails;
  i: integer;
  Response:string;
begin
 response := emptystr;
 if (aContents <> nil) then begin
   infs := TInterfaceStream.Create;
   infs.Clear;
   infs.Position := 0;
   infs.LoadFromIStream(aContents);
   try
   Response := memstreamtostr(infs);
   //showmessage(fLastRequestResponse);
   except
   end;
  infs.Free;
 end;

  for i := 0 to high(fLoggedRequests) do begin
    if fLoggedRequests[i].ResourceID = aResourceID then begin
      fLoggedRequests[i].Response := Response;
      if Assigned(fOnRequestDone) then
         fOnRequestDone(fLoggedRequests[i]);
    end;
  end;

 // todo: logrequest() adds items to resources... is that needed for resources tab?

end;

procedure TCatChromium.WVBrowser1WebResourceResponseReceived(Sender: TObject;
const aWebView: ICoreWebView2; const aArgs: ICoreWebView2WebResourceResponseReceivedEventArgs);
var
 req:TSandcatRequestDetails;
 a:TCoreWebView2WebResourceResponseReceivedEventArgs;
 uri, method, headerso, reason:PWideChar;
 headerName, headerValue:wvstring;
 content: IStream;
 infs: TInterfaceStream;
 headers:ICoreWebView2HttpRequestHeaders;
 hdr:TCoreWebView2HttpRequestHeaders;
 rcvdhdr:TCoreWebView2HttpResponseHeaders;
 aIterator: TCoreWebView2HttpHeadersCollectionIterator;
 aResponse: ICoreWebView2WebResourceResponseViewGetContentCompletedHandler;
 TempHeaders  : ICoreWebView2HttpResponseHeaders;
 TempIterator : TCoreWebView2HttpHeadersCollectionIterator;
 headerStr, contentstr, path, httpver:string;
 anext,statuscode:integer;
begin
  if fInterceptRequests = false then
    exit;
  if Browser = nil then
    exit;
 Inc(fLoggedRequestsCount);
 req.postdata := emptystr;
 a := TCoreWebView2WebResourceResponseReceivedEventArgs.Create(aArgs);
 a.Request.Get_uri(uri);
 a.Request.Get_Method(method);
 a.Request.Get_Content(content);
 a.Request.Get_Headers(headers);
 a.Response.Get_StatusCode(statuscode);
 a.Response.Get_ReasonPhrase(reason);
 httpver := 'HTTP/1.1';

 path := '/'+extracturlpath(WideCharToString(uri));
 req.SentHead := WideCharToString(method)+' '+path+' '+httpver+crlf;
 try
  hdr := TCoreWebView2HttpRequestHeaders.Create(headers);
  aIterator:= TCoreWebView2HttpHeadersCollectionIterator.Create(hdr.Iterator);
  while aIterator.HasCurrentHeader do
  begin
    if aIterator.GetCurrentHeader(headerName, headerValue) then begin
      req.SentHead :=  req.SentHead+headerName + ': ' + headerValue+crlf;
    end;
    aIterator.MoveNext;
  end;
  if getfield('host',req.SentHead) = emptystr then
  req.SentHead := req.SentHead+'Host: '+ExtractURLHost(WideCharToString(uri));
 freeandnil(hdr);
 freeandnil(aIterator);
 except
 end;


 a.Response.Get_Headers(TempHeaders);
  req.RcvdHead := httpver+' '+inttostr(statuscode)+' '+GetStatusCodeDescription(statuscode)+crlf;
 try
  rcvdhdr := TCoreWebView2HttpResponseHeaders.Create(TempHeaders);
  aIterator:= TCoreWebView2HttpHeadersCollectionIterator.Create(rcvdhdr.Iterator);
  while aIterator.HasCurrentHeader do
  begin
    if aIterator.GetCurrentHeader(headerName, headerValue) then begin
      req.RcvdHead :=  req.RcvdHead+headerName + ': ' + headerValue+crlf;
    end;
    aIterator.MoveNext;
  end;

 req.Length := strtointdef(cathttp.GetField('content-length',req.RcvdHead),0);
 freeandnil(rcvdhdr);
 freeandnil(aIterator);
 except
 end;



 if (content <> nil) then begin
   infs := TInterfaceStream.Create;
   infs.Clear;
   infs.Position := 0;
   infs.LoadFromIStream(content);
   try
   req.PostData := memstreamtostr(infs);
   except
   end;
  infs.Free;
 end;

 req.URL := WideCharToString(uri);
 req.Method := WideCharToString(method);
 req.StatusCode := statuscode;
 req.ResourceID := fLoggedRequestsCount;
  SetLength(fLoggedRequests, length(fLoggedRequests) + 1);
  fLoggedRequests[High(fLoggedRequests)] := req;


 aResponse  := TCoreWebView2WebResourceResponseViewGetContentCompletedHandler.Create(fcrm, fLoggedRequestsCount);
 a.Response.GetContent(aResponse);
 aresponse := nil;
 {if (aresponse <> nil) then begin
   //req.Response := fLastRequestResponse;
   //showmessage( fLastRequestResponse);
     //aResponseT:= TCoreWebView2WebResourceResponseViewGetContentCompletedHandler.Create(;
 end;    }
 a.Free;

// if Assigned(fOnRequestDone) then
 //  fOnRequestDone(req);
 // todo: logrequest() adds items to resources... is that needed for resources tab?
end;

procedure TCatChromium.WVBrowser1StatusBarTextChanged(Sender: TObject; const aWebView: ICoreWebView2);
begin
  if Assigned(fOnStatusMessage) then
    fOnStatusMessage(sender,fcrm.StatusBarText);
end;

procedure TCatChromium.WVBrowser1OfflineCompleted(Sender: TObject; aResult: boolean);
begin
  //
end;

procedure TCatChromium.WVBrowser1ExecuteScriptCompleted(Sender: TObject; aErrorCode: HRESULT; const aResultObjectAsJson: wvstring; aExecutionID: integer);
var
  j:TCatJSON;
  json:string;
begin
  if (aExecutionID<>0) and Assigned(fOnBrowserMessage) then begin
     j := TCatJSON.Create;
     j.text := '{"result":'+aResultObjectAsJson+'}';
     json := j.getvalue('result',emptystr);
     j.free;
     fLastJSExecutionResult := json;
    fOnBrowserMessage(CRM_JS_EXECUTION_END, inttostr(aExecutionID)+':'+json);
  end;
end;

constructor TCatChromium.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fBrowserCreated := false;
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clWindow;
  fMsgCrom := TCatMsgCromis.Create;
  fMsgCrom.OnDataMessage:= WMCopyData;
  fMsg := TCatMsg.Create;
  fMsg.OnDataMessage := WMCopyData;
  fCriticalSection := TCriticalSection.Create;
  fPreventPopup := false;
  fInterceptRequests := true;
  fLogURLs := false;
  fEnableDownloads := true;
  fAdjustSourceDisplayMethod := true;
  fAutoGetSource := false;
  fLogJavaScriptErrors := false;
  fUserAgent := emptystr;
  fLastJSExecutionResult := emptystr;
  fLoggedRequestsCount := 0;
  fResourceList := TStringList.Create;
  fURLLog := TStringList.Create;

  fSplitter := TSplitter.Create(self);
  fSplitter.Parent := self;
  fSplitter.width := 2;
  fSplitter.visible := false;
  fSplitter.Color := clBtnShadow;
  fDevTools := TCatDevTools.Create(self);
  fDevTools.Parent := self;
  fDevTools.Splitter := fSplitter;
  fCrm := TWVBrowser.Create(self);
  fcrm.OnAfterCreated := WVBrowser1AfterCreated;
  fcrm.OnInitializationError := WVBrowser1InitializationError;
  Self.Browser := fCrm;

  fcrm.OnNavigationStarting := WVBrowser1NavigationStarting;
  fcrm.OnNavigationCompleted := WVBrowser1NavigationCompleted;
  fcrm.OnRetrieveHTMLCompleted := WVBrowser1RetrieveHTMLCompleted;
  fcrm.OnRetrieveTextCompleted := WVBrowser1RetrieveTextCompleted;
  fcrm.OnDocumentTitleChanged := WVBrowser1DocumentTitleChanged;
  fcrm.OnSourceChanged := WVBrowser1URLChanged;
  fcrm.OnWebMessageReceived := WVBrowser1WebMessageReceived;
  fcrm.OnWebResourceResponseReceived := WVBrowser1WebResourceResponseReceived;
  fcrm.OnWebResourceResponseViewGetContentCompleted := WVBrowser1WebResourceResponseViewGetContentCompleted;
  fcrm.OnStatusBarTextChanged := WVBrowser1StatusBarTextChanged;
  fcrm.OnOfflineCompleted :=  WVBrowser1OfflineCompleted;
  fcrm.OnExecuteScriptCompleted := WVBrowser1ExecuteScriptCompleted;

  //fcrm.OnDevToolsProtocolEventReceived :=


  //catdelay(1000);
  {fCrm := TChromium.Create(nil);
  fCrm.visible := false;
  fCrm.Color := clWindow;
  fCrm.Parent := self;
  fCrm.Align := alclient;
  fCrm.OnGetAuthCredentials := crmGetAuthCredentials;
  fCrm.OnBeforeContextMenu := crmBeforeContextMenu;
  fCrm.OnBeforeResourceLoad := crmBeforeResourceLoad;
  // fCrm.OnGetAuthCredentials := crmGetAuthCredentials;
  fCrm.OnBeforePopup := crmBeforePopup;
  fCrm.OnCertificateError := crmCertificateError;
  fCrm.OnConsoleMessage := crmConsoleMessage;
  fCrm.OnJsdialog := crmJsdialog;
  fCrm.OnBeforeDownload := crmBeforeDownload;
  fCrm.OnDownloadUpdated := crmDownloadUpdated;
  fCrm.OnGetResourceHandler := crmGetResourceHandler;
  fCrm.OnProcessMessageReceived := crmProcessMessageReceived;
  fCrm.OnLoadError := crmLoadError;
  fCrm.OnLoadingStateChange := crmLoadingStateChange;
  fCrm.OnPluginCrashed := crmPluginCrashed;
  fCrm.OnRenderProcessTerminated := crmRenderProcessTerminated;
  fCrm.OnContextMenuCommand := crmContextMenuCommand;  }
end;

procedure TCatChromium.ClearRequestData;
begin
  fLastStatusCode := 0;
  fSentRequests := 0;
  fHeaders.SentHead := emptystr;
  fHeaders.RcvdHead := emptystr;
  fHeaders.StatusCode := emptystr;
end;

procedure TCatChromium.SendRequest(const req: TCatChromiumRequest;
const Load: Boolean = false);
var
  slp: TStringLoop;
  rheader, rvalue: string;
  TempRequestIntf : ICoreWebView2WebResourceRequest;
  TempRequest : TCoreWebView2WebResourceRequestRef;
  TempAdapter : IStream;
  TempParams, TempMethod : string;
  TempHeaders  : TCoreWebView2HttpRequestHeaders;
  FBody    : TStringStream;
begin
  if (Load = true) then
    exit;
  fSentRequests := fSentRequests + 1;

  try
    TempMethod  := StrDef(req.method, 'GET');
    if TempMethod = 'POST' then
    TempParams  := req.PostData;
    FBody       := TStringStream.Create(TempParams);
    TempAdapter := TStreamAdapter.Create(FBody, soReference);

    if fcrm.CoreWebView2Environment.CreateWebResourceRequest(req.url, TempMethod, TempAdapter, '', TempRequestIntf) then
      begin
        TempRequest := TCoreWebView2WebResourceRequestRef.Create(TempRequestIntf);
        TempHeaders := TCoreWebView2HttpRequestHeaders.Create(TempRequest.Headers);
        //if TempMethod = 'POST' then
        //TempHeaders.SetHeader('Content-Type', 'application/x-www-form-urlencoded');
        //TempHeaders.SetHeader('X-MyCustomHeader', 'MyCustomValue');
       if req.Headers<>emptystr then begin
         slp := TStringLoop.Create(req.Headers);
         while slp.found do
         begin
           if MatchWildcard(slp.current, '*:*') then
           begin
             rheader := before(slp.current, ': ');
             rvalue := after(slp.current, ': ');
             TempHeaders.SetHeader(rheader, rvalue);
           end;
         end;
       slp.Free;
       end;
        fcrm.NavigateWithWebResourceRequest(TempRequestIntf);
      end;
  finally
    if assigned(TempRequest) then FreeAndNil(TempRequest);
    if assigned(TempHeaders) then FreeAndNil(TempHeaders);
    TempAdapter := nil;
  end;
end;

procedure TCatChromium.LoadFromString(const s, url: string);
begin
  CreateBrowser(true);
  if GetURL = emptystr then
    LoadBlank; // CEF3 LoadString Bug Workaround
  if fCrm = nil then
    exit;
  fCrm.NavigateToString(s);
end;

procedure TCatChromium.CreateBrowser(const clearrd:boolean);
begin
  if clearrd=true then
    ClearRequestData;
  if fBrowserCreated = true then
    exit;
  if GlobalWebView2Loader.InitializationError then
    showmessage(GlobalWebView2Loader.ErrorMessage)
   else
    if GlobalWebView2Loader.Initialized then begin
      fBrowserCreated := true;
      fCrm.CreateBrowser(self.Handle);
      catdelay(1000);
    end;
end;

procedure TCatChromium.NotifyParentWindowPositionChanged;
begin
  if (fcrm <> nil) then
    fcrm.NotifyParentWindowPositionChanged;
end;

procedure TCatChromium.Load(const url: string);
begin

  CreateBrowser(true);
  fCrm.Navigate(url);
  {
  if fNeedRecreate then
    ReCreateBrowser(url); }
end;

procedure TCatChromium.LoadBlank(const WaitLoad: Boolean = false);
begin
  CreateBrowser(true);
  fCrm.Navigate(cURL_HOME);
  if WaitLoad then
    exit;
  while isLoading do
    application.ProcessMessages;
end;

function TCatChromium.isLoading: Boolean;
begin
  Result := false;
  if fCrm = nil then
    exit;
  Result := fCrm.IsNavigating;//fCrm.Browser.isLoading;
end;

procedure TCatChromium.UpdateNavButtons(const aIsNavigating : boolean);
begin
  if assigned(OnLoadingStateChange) then
    OnLoadingStateChange(Self, aIsNavigating, fCrm.CanGoBack, fCrm.CanGoForward);
end;

procedure TCatChromium.Reload(const IgnoreCache: Boolean = false);
begin
  ClearRequestData;
  if isLoading then
    Stop;
  if fCrm = nil then
    exit;
  if IgnoreCache then
    fCrm.refreshignorecache
  else
    fCrm.refresh; // standard reload
end;

procedure TCatChromium.Stop(const waitstop: Boolean = false);
begin
  if fCrm <> nil then
    fCrm.Stop;  // StopLoad
  if waitstop = false then
    exit;
  while isLoading do
    application.ProcessMessages;
end;

destructor TCatChromium.Destroy;
begin
  fMsg.Free;
  fMsgCrom.Free;
  fInterceptRequests := false;
  OnAfterSetSource := nil;
  OnBrowserMessage := nil;
  NilComponentMethods(fCrm);
  fSplitter.Free;
  fDevTools.Free;
  fCrm.Free;
  fURLLog.Free;
  fResourceList.Free;
  fCriticalSection.Free;
  inherited Destroy;
end;

end.

