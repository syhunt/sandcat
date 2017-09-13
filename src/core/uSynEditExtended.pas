unit uSynEditExtended;

{
  Catarinka TCatSynEditExtended - Enhanced SynEdit
  Copyright (c) 2013-2017 Felipe Daragon
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

{$I Catarinka.inc}

uses
{$IFDEF DXE2_OR_UP}
  Winapi.Messages, System.Classes, System.Types, System.SysUtils,
  Winapi.Windows, Vcl.Menus, Vcl.ActnList, Vcl.Dialogs,
{$ELSE}
  Messages, Classes, Types, SysUtils, Windows, Menus, ActnList, Dialogs,
{$ENDIF}
  CatSynEdit;

type
  TCatSynEditExtended = class(TCatSynEdit)
  private
    fFilename: string;
    fSearch: TMenuItem;
    fSearchNewTab:TMenuItem;
    procedure MenuOnPopup(Sender: TObject);
    procedure MenuSaveAsClick(Sender: TObject);
    procedure MenuSearchClick(Sender: TObject);
    procedure MenuSearchNewTabClick(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Filename: string read fFilename write fFilename;
  end;

implementation

uses
  uMain, CatFiles;

procedure TCatSynEditExtended.MenuSearchClick(Sender: TObject);
begin
  if self.SelText <> emptystr then
    tabmanager.activetab.DoSearch(self.SelText);
end;

procedure TCatSynEditExtended.MenuSearchNewTabClick(Sender: TObject);
begin
  if self.SelText <> emptystr then
    tabmanager.NewTab_Search(self.SelText);
end;

procedure TCatSynEditExtended.MenuSaveAsClick(Sender: TObject);
var
  sd: TSaveDialog;
begin
  sd := TSaveDialog.Create(self);
  sd.DefaultExt := extractfileext(fFilename);
  sd.Filename := fFilename;
  sd.Filter := 'All Files|*.*';
  sd.Options := sd.Options + [ofoverwriteprompt];
  if sd.execute then
    SL_SaveToFile(self.lines, sd.Filename);
  sd.free;
end;

procedure TCatSynEditExtended.MenuOnPopup(Sender: TObject);
begin
  fSearch.Enabled := (SelText <> emptystr);
  fSearchNewTab.Enabled := fSearch.Enabled;
end;

constructor TCatSynEditExtended.Create(AOwner: TComponent);
var
  mi: TMenuItem;
begin
  inherited;
  PopupMenu.OnPopup := MenuOnPopup;
  // preview text popup menu items
  mi := TMenuItem.Create(PopupMenu);
  mi.Caption := '-';
  PopupMenu.Items.Add(mi);
  // search
  fSearch := TMenuItem.Create(PopupMenu);
  fSearch.Caption := 'Search';
  fSearch.OnClick := MenuSearchClick;
  PopupMenu.Items.Add(fSearch);
  fSearchNewTab := TMenuItem.Create(PopupMenu);
  fSearchNewTab.Caption := 'Search in New Tab';
  fSearchNewTab.OnClick := MenuSearchNewTabClick;
  PopupMenu.Items.Add(fSearchNewTab);
  // save as
  mi := TMenuItem.Create(PopupMenu);
  mi.Caption := 'Save As...';
  mi.OnClick := MenuSaveAsClick;
  PopupMenu.Items.Add(mi);
end;

destructor TCatSynEditExtended.Destroy;
begin
  // No need to free popup items here because Destroy of TCatSynEdit's popupmenu
  // will do it
  inherited;
end;

end.
