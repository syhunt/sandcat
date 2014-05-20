unit uTaskMon;

{
  Sandcat Task Monitor
  Copyright (c) 2011-2014, Syhunt Informatica
  License: 3-clause BSD license
  See https://github.com/felipedaragon/sandcat/ for details.
}

interface

uses Forms, SysUtils, Windows, Controls, Graphics, Classes, StdCtrls, ExtCtrls,
  ComCtrls, Menus;

type
  TTaskMessages = class(TCustomControl)
  private
    fClearItem: TMenuItem;
    fMaxItems: integer;
    fMainLv: TListView;
    fFilterLv: TListView;
    fPopupMenu: TPopupMenu;
    procedure AddMessageToList(lv: TListView; s: string;
      pid, imgindex: integer);
    procedure ClearItemClick(Sender: TObject);
  protected
  public
    procedure AddMessage(s: string; pid, imgindex: integer);
    procedure Clear;
    constructor Create(AOwner: TWinControl);
    destructor Destroy; override;
  end;

implementation

uses uMain;

procedure TTaskMessages.ClearItemClick(Sender: TObject);
begin
  Clear;
end;

procedure TTaskMessages.AddMessageToList(lv: TListView; s: string;
  pid, imgindex: integer);
begin
  with lv.Items.Add do
  begin
    caption := s;
    subitems.Add(inttostr(pid));
    imageindex := imgindex;
  end;
  if lv.Items.Count > fMaxItems then
    lv.Items[0].Delete;
end;

procedure TTaskMessages.AddMessage(s: string; pid, imgindex: integer);
begin
  AddMessageToList(fMainLv, s, pid, imgindex);
  AddMessageToList(fFilterLv, s, pid, imgindex);
end;

procedure TTaskMessages.Clear;
begin
  fMainLv.Clear;
  fFilterLv.Clear;
end;

constructor TTaskMessages.Create(AOwner: TWinControl);
  procedure AddColumn(lv: TListView; c: string; w: integer = 0);
  begin
    with lv.Columns.Add do
    begin
      caption := c;
      width := w;
      autosize := false;
    end;
  end;
  procedure AddColums(lv: TListView);
  begin
    AddColumn(lv, 'Message', 1000);
    AddColumn(lv, 'PID', 50);
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
    lv.PopupMenu := fPopupMenu;
    lv.ColumnClick := false;
    AddColums(lv);
  end;

begin
  inherited Create(AOwner);
  fMaxItems := 10000;
  ControlStyle := ControlStyle + [csAcceptsControls];
  fPopupMenu := TPopupMenu.Create(self);
  fClearItem := TMenuItem.Create(fPopupMenu);
  fClearItem.caption := 'Clear';
  fClearItem.OnClick := ClearItemClick;
  fPopupMenu.Items.Add(fClearItem);

  fMainLv := TListView.Create(self);
  ConfigLV(fMainLv);
  fFilterLv := TListView.Create(self);
  ConfigLV(fFilterLv);
  fMainLv.BringToFront;
end;

destructor TTaskMessages.Destroy;
begin
  fClearItem.free;
  fPopupMenu.free;
  fMainLv.free;
  fFilterLv.free;
  inherited Destroy;
end;

end.
