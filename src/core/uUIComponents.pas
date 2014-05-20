unit uUIComponents;
{
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses
  LuaWrapper,
  CatStorage, CatStringLoop, CatSynEdit, CatJSON, CatJINI,
  unitObjectCache, CatSciterAx;

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
  TSandUIEngine = TSciter;
  ISandUIElement = IElement;

implementation

// ------------------------------------------------------------------------//
end.
