unit uUIComponents;
{
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

{$DEFINE USEAXSCITER}

uses
  Windows, SysUtils, Registry,
  LuaWrapper,
  CatStorage, CatStringLoop, CatSynEdit, CatJSON, CatJINI,
{$IFDEF USEAXSCITER}
  CatSciterAx,
{$ELSE}
  CatSciter, Sciter,
{$ENDIF}
  unitObjectCache;

type
  TSandJSON = TCatJSON;
  TSandJINI = TJIniList;

type
  TSandLua = TLua;

type
  TSandSLParser = TStringLoop;
  TSandSynEdit = TCatSynEdit;
  TSandCache = TCatStorage;
  TSandObjCache = TObjectCache;

type
  TSandUIEngine = {$IFDEF USEAXSCITER}TSciter{$ELSE}TCatSciter{$ENDIF};
  ISandUIElement = IElement;

function RegisterAxSciter: boolean;
function SciterExists: boolean;

implementation

// Registers the Sciter library
// Returns false if there is a problem registering
function RegisterAxSciter: boolean;
type
  TDllRegisterServer = function: HResult; stdcall;
var
  DLLHandle: THandle;
  RegFunc: TDllRegisterServer;
begin
  result := true;
  try
    DLLHandle := LoadLibrary(pwidechar(extractfilepath(paramstr(0)) +
      '\AxSciter.dll'));
    RegFunc := GetProcAddress(DLLHandle, 'DllRegisterServer');
    if RegFunc <> 0 then
      result := false;
    FreeLibrary(DLLHandle);
  except
    result := false;
  end;
end;

function SciterExists: boolean;
var
  Reg: TRegistry;
begin
  try
    Reg := TRegistry.Create;
    try
      Reg.RootKey := HKEY_CLASSES_ROOT;
      result := Reg.KeyExists('CLSID\' + GuidToString(CLASS_Sciter));
    finally
      Reg.Free;
    end;
  except
    result := false;
  end;
end;

// ------------------------------------------------------------------------//
end.
