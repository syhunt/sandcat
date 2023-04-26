unit uRequests;
{
  Sandcat HTTP Request Logger
  Copyright (c) 2011-2023, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  Windows, Forms, Classes, SysUtils, Controls, ExtCtrls, SyncObjs, Dialogs,
  uUIComponents, uLiveHeaders;

type
  TSandcatRequests = class
  private
    fIsClosing: boolean;
    fLogRequests: boolean;
    fLoggedRequests: integer;
    fTabHandle: HWND;
  public
    Cache: TSandCache;
    Headers: TLiveHeaders;
    procedure Clear;
    function GetRequest(const jsonrequestfile: string): TSandcatRequestDetails;
    function RequestExists(const jsonrequestfile: string): boolean;
    procedure LogRequest(request: TSandcatRequestDetails);
    procedure LogDynamicRequest(const json: string);
    procedure LogXMLHTTPRequest(const json: string);
    procedure TabWillClose;
    procedure UpdateRequest(const jsonrequestfile: string;
      const response: string);
    constructor Create(AOwner: TWinControl; MsgHandle: HWND);
    destructor Destroy; override;
    // properties
    property LogRequests: boolean read fLogRequests write fLogRequests;
  end;

var
  Global_LoggedRequests: integer = 0;

const
  cReqFolder = '\Requests\';

implementation

uses uMain, CatDCP, uMisc, CatTime, CatChromium, CatHTTP, CatStrings,
  CatFiles, uSettings, uConst, CatCryptKey;

procedure TSandcatRequests.Clear;
begin
  Headers.Clear;
  Cache.Clear;
end;

procedure TSandcatRequests.TabWillClose;
begin
  fIsClosing := true;
end;

function TSandcatRequests.RequestExists(const jsonrequestfile: string): boolean;
begin
  result := Cache.cachedFileExists(cReqFolder + jsonrequestfile);
end;

function TSandcatRequests.GetRequest(const jsonrequestfile: string)
  : TSandcatRequestDetails;
var
  j: TSandJINI;
  responsefile: string;
begin
  if RequestExists(jsonrequestfile) = false then
    exit;
  j := TSandJINI.Create;
  try
  j.text := Cache.gettextfile(cReqFolder + jsonrequestfile);
  except
  end;
  result.URL := aestostr(j.Values['URL'],GetCatKey(CATKEY_REQUESTHEADERS));
  result.response := j.Values['Response'];
  result.MimeType := j.Values['MimeType'];
  result.StatusCode := StrToIntDef(j.Values['Status'],0);
  if j.Values['ResponseFilename'] <> emptystr then
  begin
    responsefile := cReqFolder + jsonrequestfile + '.resp';
    result.response := Cache.gettextfile(responsefile);
    result.Filename := responsefile;
  end;
  result.RcvdHead := j.Values['ResponseHeaders'];
  result.SentHead := aestostr(j.Values['Headers'],GetCatKey(CATKEY_REQUESTHEADERS));
  result.Method := j.Values['Method'];
  if j.Values['IsLow'] = '1' then
    result.IsLow := true
  else
    result.IsLow := false;
  j.Free;
end;

procedure TSandcatRequests.UpdateRequest(const jsonrequestfile: string;
  const response: string);
var
  j: TSandJINI;
begin
  if RequestExists(jsonrequestfile) = false then
    exit;
  j := TSandJINI.Create;
  j.text := Cache.gettextfile(cReqFolder + jsonrequestfile);
  if response <> emptystr then
    j.Values['Response'] := response;
  debug('Updating request ' + jsonrequestfile + ' ...');
  // updates the request file in the cache
  Cache.StoreString(cReqFolder + jsonrequestfile, j.text);
  j.Free;
end;

// main procedure for logging a Sandcat Request (to a json file) in VFS
procedure TSandcatRequests.LogRequest(request: TSandcatRequestDetails);
var
  logfile: TSandJINI;
  hasreqid, canlog: boolean;
  hdrmethod:string;
  procedure reconstruct_missing_details;
  begin
    if request.Method = emptystr then begin // get it from the sent header
      request.method := 'GET';
      hdrmethod := before(request.SentHead, ' ');
      if hdrmethod = 'POST' then
      request.Method := hdrmethod;
    end;
    if request.URL = emptystr then
    begin
      // checks if host and port were provided and reconstruct from them
      if (request.host <> emptystr) and (request.SentHead <> emptystr) then
      begin
        request.URL := generateurl(request.host, strtointdef(request.port, 0));
        request.URL := request.URL + ExtractHTTPRequestPath(request.SentHead);
      end;
    end;
    if request.StatusCode = 0 then // get it from the response headers
      request.StatusCode := ExtractHTTPResponseStatusCode(request.RcvdHead);
    if request.MimeType = emptystr then // get it from the response headers
      request.MimeType := trim(getfield('Content-Type', request.RcvdHead));
  end;

begin
  canlog := true;
  if fIsClosing then
    canlog := false;
  if Headers.Height = 0 then
  begin
    canlog := false; // headers not visible, no need to log the request
    deletefile(request.Filename); // deletes the temporary response filename
  end;
  if canlog = false then
    exit;
  hasreqid := false;
  Global_LoggedRequests := Global_LoggedRequests + 1;
  fLoggedRequests := fLoggedRequests + 1;
  logfile := TSandJINI.Create;
  // Reconstructs any missing info (usually from a low level HTTP request)
  reconstruct_missing_details;
  // Associates a request filename with the request
  request.Filename := inttostr(Global_LoggedRequests) + '.json';
  logfile.Filename := request.Filename;
  if pos(vSCRIDHeader, request.SentHead) <> 0 then
  begin
    request.Filename := trim(getfield(vSCRIDHeader, request.SentHead))
      + '.json';
    hasreqid := true;
  end;
  if request.reqid <> emptystr then
  begin
    hasreqid := true;
    request.Filename := request.reqid + '.json';
  end;
  if hasreqid then
  begin
    logfile.Filename := request.Filename;
    if RequestExists(logfile.Filename) then
      logfile.text := Cache.gettextfile(cReqFolder + logfile.Filename);
  end;
  if request.URL <> emptystr then
    logfile.Values['URL'] := strtoaes(request.URL,GetCatKey(CATKEY_REQUESTHEADERS));
  if request.Method <> emptystr then
    logfile.Values['Method'] := request.Method;
  logfile.Values['Status'] := IntToStr(request.StatusCode);
  if request.RcvdHead <> emptystr then
    logfile.Values['ResponseHeaders'] := request.RcvdHead;
  if request.MimeType <> emptystr then
    logfile.Values['MimeType'] := request.MimeType;
  if request.SentHead <> emptystr then
    logfile.Values['Headers'] := strtoaes(request.SentHead,GetCatKey(CATKEY_REQUESTHEADERS));
  if request.IsLow then
    logfile.Values['IsLow'] := '1';
  if request.postdata <> emptystr then
    logfile.Values['PostData'] := strtoaes(request.postdata,GetCatKey(CATKEY_REQUESTHEADERS));
  if request.details <> emptystr then
    logfile.Values['Details'] := request.details;
  if request.response <> emptystr then
    logfile.WriteString('data', 'Response', request.response, 'base64');
  if request.responsefilename <> emptystr then
  begin
    if filecanbeopened(request.responsefilename) then
    begin
      logfile.Values['ResponseFilename'] := request.responsefilename;
      // Update the file size
      request.Length := GetFileSize(request.responsefilename);
      Cache.StoreFile(cReqFolder + logfile.Filename + '.resp',
        request.responsefilename);
      deletefile(request.responsefilename);
    end;
  end;
  // showmessage('storing:'+logfile.text);
  try
    Cache.StoreString(cReqFolder + logfile.Filename, logfile.text);
  except
  end;

  logfile.Free;
  if (request.details = 'Browser Request') and (hasreqid = true) then
    exit; // no need to add the request again to the live list
  if fLogRequests then
    Headers.AddRequest(request);
end;

// Logs a XHR request executed by a JavaScript (user requested)
procedure TSandcatRequests.LogXMLHTTPRequest(const json: string);
var
  pkt: TSandJINI;
  r: TSandcatRequestDetails;
begin
  if fIsClosing then
    exit;
  pkt := TSandJINI.Create;
  pkt.text := json;
  r.reqid := pkt.Values['ReqID'];
  r.details := pkt.Values['Details'];
  r.Method := pkt.Values['Method'];
  r.URL := pkt.Values['URL'];
  r.postdata := emptystr;
  r.SentHead := emptystr;
  r.RcvdHead := pkt.Values['ResponseHeaders'];
  r.response := pkt.Values['Response'];
  r.responsefilename := pkt.Values['ResponseFilename'];
  r.StatusCode := ExtractHTTPResponseStatusCode(r.RcvdHead);
  r.MimeType := trim(getfield('Content-Type', r.RcvdHead));
  r.Length := Length(r.response);
  r.isredir := false;
  r.IsLow := false;
  pkt.Free;
  LogRequest(r);
end;

// Logs a request executed by external Syhunt scanner
// Experimental
procedure TSandcatRequests.LogDynamicRequest(const json: string);
var
  pkt: TSandJINI;
  reqtype: string;
  r: TSandcatRequestDetails;
begin
  if fIsClosing then
    exit;
  pkt := TSandJINI.Create;
  pkt.text := json;
  reqtype := pkt.Values['type'];
  if reqtype = 'Vuln' then
    reqtype := 'Vulnerability';
  if reqtype = 'First' then
    reqtype := 'Spider Snapshot';
  r.details := reqtype;
  r.host := pkt.Values['host'];
  r.port := pkt.Values['port'];
  r.Length := strtointdef(pkt.Values['crsp'], 0);
  r.SentHead := pkt.Values['rqsh'];
  r.RcvdHead := pkt.Values['head'];
  r.response := pkt.Values['rspn'];
  pkt.Free;
  LogRequest(r);
end;

constructor TSandcatRequests.Create(AOwner: TWinControl; MsgHandle: HWND);
begin
  inherited Create;
  fTabHandle := MsgHandle;
  fLogRequests := true;
  fIsClosing := false;
  fLoggedRequests := 0;
end;

destructor TSandcatRequests.Destroy;
begin
  inherited;
end;

initialization

vSCRIDHeader := 'X-' + uppercase(randomstring(1)) + randomstring(2) + '-' +
  uppercase(randomstring(1)) + randomstring(3);

// ------------------------------------------------------------------------//
end.
