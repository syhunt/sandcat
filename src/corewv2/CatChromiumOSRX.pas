unit CatChromiumOSRX;

{
  Catarinka Browser OSR Experimental Extended Component
  Copyright (c) 2011-2017 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Vcl.Controls, Vcl.Graphics,
{$ELSE}
  Classes, Controls, Graphics,
{$ENDIF}
  CatChromiumLib;

type
  TCatChromiumOnAfterSetSourceSpecial = procedure(const s, headers: string)
    of object;

type
  TCatChromiumOSRX = class(TCustomControl) //TCatChromiumOSR
  private
    fIsCachedURL: boolean;
    fCachedSource: string;
    fOnAfterSetSourceSpecial: TCatChromiumOnAfterSetSourceSpecial;
    procedure SourceAvailable(const s: string);
  public
    function GetURLSpecial:string;
    procedure LoadFromCache(const URL: string);
    procedure Reset;
    procedure SaveToFile(const filename: string);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property IsCachedURL: boolean read fIsCachedURL;
    property OnAfterSetSourceSpecial: TCatChromiumOnAfterSetSourceSpecial
      read fOnAfterSetSourceSpecial write fOnAfterSetSourceSpecial;
  end;

implementation

uses CatCEFCache, CatStrings;

const
  cURL_Cache = 'chrome://view-http-cache/';

function TCatChromiumOSRX.GetURLSpecial:string;
begin
  Result := '';//GetURL;
  if fiscachedurl = true then
    Result := after(result, cURL_Cache);
end;

procedure TCatChromiumOSRX.Reset;
begin
  fIsCachedURL := false;
end;

procedure TCatChromiumOSRX.LoadFromCache(const URL: string);
begin
  fIsCachedURL := true;
  //load(cURL_Cache + URL);
end;

procedure TCatChromiumOSRX.SaveToFile(const filename: string);
begin
  if fIsCachedURL = true then
    ChromeCacheExtract(fCachedSource, filename);
end;

procedure TCatChromiumOSRX.SourceAvailable(const s: string);
var
  ds: string;
begin
  if fIsCachedURL = false then
    ds := s
  else
  begin
    // store the response for using if the savetofile method is called
    fCachedSource := s;
    if Assigned(fOnAfterSetSourceSpecial) then
      fOnAfterSetSourceSpecial(ChromeCacheToString(s),
        GetChromeCacheResponseHeaders(s));
  end;
end;

constructor TCatChromiumOSRX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //EnableDownloads := false;
  //AdjustSourceDisplayMethod := false;
  fIsCachedURL := false;
  //OnAfterSetSource := SourceAvailable;
end;

destructor TCatChromiumOSRX.Destroy;
begin
  inherited Destroy;
end;

end.
