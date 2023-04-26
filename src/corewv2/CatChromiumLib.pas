unit CatChromiumLib;

{
  Catarinka Browser Component (WebView2-Based)
  Copyright (c) 2011-2023 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}
{$I SandcatEngine.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Winapi.Windows, Winapi.Messages, Vcl.Controls, Vcl.Graphics,
  Vcl.Forms, System.SysUtils, System.SyncObjs, Vcl.Dialogs, Vcl.Clipbrd,
  System.TypInfo, Vcl.StdCtrls, Vcl.ExtCtrls, System.UITypes,
{$ELSE}
  Classes, Windows, Messages, Controls, Graphics, Forms, SysUtils, SyncObjs,
  Dialogs, Clipbrd, TypInfo, StdCtrls, ExtCtrls,
{$ENDIF}
  superobject, CatJSON, CatMsg, CatPanels, uLiveHeaders, uConst;

type
  TCatChromiumOnRequestDone  = procedure (const req:TSandcatRequestDetails) of object;
  TCatChromiumOnBrowserMessage = procedure(const msg: integer;
    const str: string) of object;
  TCatChromiumOnAfterSetSource = procedure(const s: string) of object;
  TCatChromiumOnTitleChange = procedure(Sender: TObject; const title: string)
    of object;
  TCatChromiumOnLoadEnd = procedure(Sender: TObject; httpStatusCode: integer)
    of object;
  TCatChromiumOnLoadStart = procedure(Sender: TObject) of object;
  TCatChromiumOnAddressChange = procedure(Sender: TObject; const url: string)
    of object;
  TCatChromiumOnStatusMessage = procedure(Sender: TObject; const value: string)
    of object;
  // TCatChromiumOnRequestComplete = procedure(const s:string) of object;
  TCatChromiumOnBeforePopup = procedure(Sender: TObject; var url: string;
    out Result: Boolean) of object;
  TCatChromiumOnConsoleMessage = procedure(Sender: TObject;
    const message, source: string; line: integer) of object;
  TCatChromiumOnBeforeDownload = procedure(Sender: TObject; const id: integer;
    const suggestedName: string) of object;
  TCatChromiumOnDownloadUpdated = procedure(Sender: TObject;
    var cancel: Boolean; const id, state, percentcomplete: integer;
    const fullPath: string) of object;
  TCatChromiumOnLoadingStateChange = procedure(Sender: TObject;
    const isLoading, canGoBack, canGoForward: Boolean) of object;
  TCatChromiumOnLoadError = procedure(Sender: TObject; const errorCode: integer;
    const errorText, failedUrl: string) of object;

type
  TCatCustomJSCall = record
   Code: string;
   URL: string;
   StartLine: integer;
   ExecutionID: integer;
   Silent: Boolean;
  end;

type
  TCatRequestHeaders = record
    StatusCode: string;
    SentHead: string;
    RcvdHead: string;
  end;

type
  TCatChromiumRequest = record
    Method: string;
    url: string;
    PostData: string;
    Headers: string;
    IgnoreCache: Boolean;
    UseCachedCredentials: Boolean;
    UseCookies: Boolean;
    Details: string;
  end;

type
  TCatDevTools = class(TCustomControl)
  private
    //fDevTools: TChromiumDevTools;
    fTitlePanel: TBarTitlePanel;
    fSplitter: TSplitter;
    procedure CloseBtnClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    //procedure View(browser: ICefBrowser);
    property Splitter: TSplitter read fSplitter write fSplitter;
  end;

const
  cURL_ABOUTBLANK = 'about:blank';
  cURL_HOME = 'sandcat:home';
  cOptions = 'chrome.options.';
  cSandcatOptions = 'sandcat.options.';

const // Messages from the Chromium renderer to the Sandcat Tab object
  CRM_LOG_REQUEST_JSON = 1;
  CRM_RENDER_PROCESSTERMINATED = 3;
  CRM_NEWTAB = 4;
  CRM_JS_ALERT = 10;
  CRM_NEWPAGERESOURCE = 11;
  CRM_SAVECACHEDRESOURCE = 12;
  CRM_SEARCHWITHENGINE = 13;
  CRM_SEARCHWITHENGINE_INNEWTAB = 14;
  CRM_NEWTAB_INBACKGROUND = 15;
  CRM_SAVECLOUDRESOURCE = 16;
  CRM_BOOKMARKURL = 17;
  CRM_JS_EXECUTION_END = 18;

const // Messages from the Sandcat tab to the Chromium renderer object
  SCTM_SET_V8_MSGHANDLE = 1;
  SCTM_V8_REGISTEREXTENSION = 2;

const // Download related
  SCD_UNKNOWN = 0;
  SCD_INPROGRESS = 1;
  SCD_COMPLETE = 2;
  SCD_CANCELED = 3;

const // Shutdown modes
  SHTD_STANDARD = 10;
  SHTD_FORCED = 11;
  SHTD_MANUAL = 12;

{const // Context menu options
  CRMMENU_ID_USER_FIRST = integer(MENU_ID_USER_FIRST);
  CRMMENU_ID_OPENIMAGE = CRMMENU_ID_USER_FIRST;
  CRMMENU_ID_OPENIMAGE_INNEWTAB = CRMMENU_ID_USER_FIRST + 1;
  CRMMENU_ID_COPYIMAGEADDRESS = CRMMENU_ID_USER_FIRST + 2;
  CRMMENU_ID_SAVEIMAGEAS = CRMMENU_ID_USER_FIRST + 3;
  CRMMENU_ID_OPENLINK = CRMMENU_ID_USER_FIRST + 4;
  CRMMENU_ID_OPENLINK_INNEWTAB = CRMMENU_ID_USER_FIRST + 5;
  CRMMENU_ID_COPYADDRESS = CRMMENU_ID_USER_FIRST + 6;
  CRMMENU_ID_SEARCH = CRMMENU_ID_USER_FIRST + 7;
  CRMMENU_ID_SEARCH_INNEWTAB = CRMMENU_ID_USER_FIRST + 8;
  CRMMENU_ID_LINK_COPYADDRESS = CRMMENU_ID_USER_FIRST + 9;
  CRMMENU_ID_OPENLINK_INBGTAB = CRMMENU_ID_USER_FIRST + 10;
  CRMMENU_ID_FRAMEMENU = CRMMENU_ID_USER_FIRST + 11;
  CRMMENU_ID_FRAMEMENU_OPEN = CRMMENU_ID_USER_FIRST + 12;
  CRMMENU_ID_FRAMEMENU_OPEN_INNEWTAB = CRMMENU_ID_USER_FIRST + 13;
  CRMMENU_ID_FRAMEMENU_OPEN_INBGTAB = CRMMENU_ID_USER_FIRST + 14;
  CRMMENU_ID_FRAMEMENU_COPYADDRESS = CRMMENU_ID_USER_FIRST + 15;
  CRMMENU_ID_PAGE_SAVEAS = CRMMENU_ID_USER_FIRST + 16;
  CRMMENU_ID_LINK_SAVEAS = CRMMENU_ID_USER_FIRST + 17;
  CRMMENU_ID_PAGE_BOOKMARK = CRMMENU_ID_USER_FIRST + 18;
  CRMMENU_ID_LINK_BOOKMARK = CRMMENU_ID_USER_FIRST + 19; }

//function CertErrorCodeToErrorName(c: TCefErrorCode): string;
function CatCEFLoadLib:boolean;
function DownloadStateToStr(i: integer): string;
function BuildRequest(Method, url: string; PostData: string = '')
  : TCatChromiumRequest;
procedure CatCEFShutdown(mode: integer);
function SaveResponseToFile(s: string): string;

implementation

uses CatTasks, CatStrings, CatHTTP, CatTime, CatMsgCromis, uSettings;

var
  TempFileCount: integer = 0;

 // kept for compatibibility with CEF but with webview we just return true
function CatCEFLoadLib:boolean;
begin
  result := true;
end;

function BuildRequest(Method, url: string; PostData: string = '')
  : TCatChromiumRequest;
begin
  Result.Method := Method;
  Result.url := url;
  Result.PostData := PostData;
  Result.Headers := emptystr;
  Result.IgnoreCache := true;
  Result.UseCookies := true;
  Result.UseCachedCredentials := true;
  Result.Details := emptystr;
end;

procedure CatCEFShutdown(mode: integer);
begin
  case mode of
    SHTD_STANDARD:
      ; // do nothing
    SHTD_FORCED:
      KillTaskbyPID(GetCurrentProcessId);
    SHTD_MANUAL:
      begin
        //ceflib.CefShutDown;
        ExitProcess(0);
      end;
  end;
end;

{function CertErrorCodeToErrorName(c: TCefErrorCode): string;
begin
  case c of
    ERR_CERT_COMMON_NAME_INVALID:
      Result := 'Certificate common name invalid.';
    ERR_CERT_DATE_INVALID:
      Result := 'Certificate date invalid.';
    ERR_CERT_AUTHORITY_INVALID:
      Result := 'Certificate authority invalid.';
    ERR_CERT_CONTAINS_ERRORS:
      Result := 'Certificate contains errors.';
    ERR_CERT_NO_REVOCATION_MECHANISM:
      Result := 'No certificate revocation mechanism.';
    ERR_CERT_UNABLE_TO_CHECK_REVOCATION:
      Result := 'Unable to check certificate revocation.';
    ERR_CERT_REVOKED:
      Result := 'Certificate revoked.';
    ERR_CERT_INVALID:
      Result := 'Invalid certificate.';
    ERR_CERT_END:
      Result := 'Certificate end.';
  end;
end;   }

function DownloadStateToStr(i: integer): string;
begin
  case i of
    SCD_INPROGRESS:
      Result := 'inprogress';
    SCD_CANCELED:
      Result := 'canceled';
    SCD_COMPLETE:
      Result := 'complete';
  end;
end;

function GetTempFile: string;
var
  f: string;
begin
  TempFileCount := TempFileCount + 1;
  f := inttostr(GetCurrentProcessId) + ' - ' + inttostr(DateTimeToUnix(now)) +
    '-' + inttostr(TempFileCount) + '.tmp';
  Result := GetSandcatDir(SCDIR_HEADERS, true)+ f;
end;

function SaveResponseToFile(s: string): string;
var
  sl: TStringList;
begin
  Result := GetTempFile;
  sl := TStringList.Create;
  sl.text := s;
  sl.SaveToFile(Result);
  sl.Free;
end;

// ------------------------------------------------------------------------//
// TCatDevTools                                                            //
// ------------------------------------------------------------------------//

{procedure TCatDevTools.View(browser: ICefBrowser);
begin
  if fDevTools = nil then
  begin
    fDevTools := TChromiumDevTools.Create(self);
    fDevTools.Parent := self;
    fDevTools.Align := AlClient;
  end;
  fDevTools.ShowDevTools(browser);
end; }

procedure TCatDevTools.CloseBtnClick(Sender: TObject);
begin
  height := 0;
  if fSplitter <> nil then
    fSplitter.Visible := false;
end;

constructor TCatDevTools.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clWindow;
  fTitlePanel := TBarTitlePanel.Create(self);
  fTitlePanel.Parent := self;
  fTitlePanel.Align := alTop;
  fTitlePanel.Caption := 'DevTools';
  fTitlePanel.CloseButton.OnClick := CloseBtnClick;
end;

destructor TCatDevTools.Destroy;
begin
  //if fDevTools <> nil then
  //  fDevTools.Free;
  fTitlePanel.Free;
  inherited Destroy;
end;

end.
