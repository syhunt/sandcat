unit CatChromiumSB;

{
  Catarinka Browser Stand-By Component
  Copyright (c) 2011-2017 Syhunt Informatica
  License: 3-clause BSD
  See https://github.com/felipedaragon/catarinka/ for details

  By directly accessing the c property the Chromium browser gets created and
  ready to use.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  System.Classes, Vcl.Controls, Vcl.Graphics,
{$ELSE}
  Classes, Controls, Graphics,
{$ENDIF}
  CatChromium, CatChromiumLib;

type
  TCatChromiumSBInit = procedure(const crm: TCatChromium) of object;

type
  TCatChromiumStandBy = class(TCustomControl)
  private
    fInitialized: boolean;
    fChromium: TCatChromium;
    fOnInitialize: TCatChromiumSBInit;
    function GetChromium: TCatChromium;
    procedure InitChromium;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Available: boolean read fInitialized;
    property C: TCatChromium read GetChromium;
    property OnInitialize: TCatChromiumSBInit read fOnInitialize
      write fOnInitialize;
  end;

implementation

procedure TCatChromiumStandBy.InitChromium;
begin
  fInitialized := true;
  fChromium := TCatChromium.Create(self);
  fChromium.Parent := self;
  fChromium.Align := AlClient;
  if Assigned(fOnInitialize) then
    fOnInitialize(fChromium);
end;

function TCatChromiumStandBy.GetChromium: TCatChromium;
begin
  // Creates the Chromium component (if not already created)
  if fInitialized = false then
    InitChromium;
  Result := fChromium;
end;

constructor TCatChromiumStandBy.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csAcceptsControls];
  Color := clWindow;
  fInitialized := false;
end;

destructor TCatChromiumStandBy.Destroy;
begin
  if fInitialized = true then
    fChromium.Free;
  inherited Destroy;
end;

end.
