unit UExportChangeListForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.Buttons, Vcl.StdCtrls, Vcl.CheckLst, Vcl.ExtCtrls;

type
  TfrmExportChangeListForm = class(TForm)
    pnlBottom: TPanel;
    btnOK: TButton;
    btnCancel: TButton;
    pnlClient: TPanel;
    cklboxChangeList: TCheckListBox;
    gbChangeList: TGroupBox;
    lblChangeGroup: TLabel;
    cmbChangeGroup: TComboBox;
    btnSelectAll: TBitBtn;
    btnDeSelectAll: TBitBtn;
    procedure FormCreate(Sender: TObject);
    procedure cmbChangeGroupChange(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeSelectAllClick(Sender: TObject);
    procedure cklboxChangeListClickCheck(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    FChangeGroupsList    : TList;
    FChangeListsList     : TStringList;
    procedure PopulateChangeGroups;
    procedure PopulateChangeLists;
    function CheckedCount: integer;
  public
    { Public declarations }
    procedure Populate(AChangeGroupsList: TList; AChangeListsList : TStringList);
    function GetSelections(var ASelectedChangeList: string): boolean;
  end;

var
  frmExportChangeListForm: TfrmExportChangeListForm;

implementation

{$R *.dfm}

uses
  {$WARN UNIT_PLATFORM OFF}
  Vcl.FileCtrl,
  {$WARN UNIT_PLATFORM ON}
  UChangeData,
  UErrorHandlingOperations;

procedure TfrmExportChangeListForm.FormCreate(Sender: TObject);
const OPNAME = 'TfrmExportChangeListForm.FormCreate';
begin
  try
    FChangeGroupsList    := nil;
    FChangeListsList     := nil;
    cklboxChangeList.MultiSelect := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmExportChangeListForm.FormDestroy(Sender: TObject);
const OPNAME = 'TfrmExportChangeListForm.FormDestroy';
begin
  try
    FChangeGroupsList    := nil;
    FChangeListsList     := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmExportChangeListForm.Populate(AChangeGroupsList: TList;AChangeListsList: TStringList);
const OPNAME = 'TfrmExportChangeListForm.Populate';
begin
  try
    FChangeGroupsList    := AChangeGroupsList;
    FChangeListsList     := AChangeListsList;
    PopulateChangeGroups;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmExportChangeListForm.PopulateChangeGroups;
const OPNAME = 'TfrmExportChangeListForm.PopulateChangeGroups';
var
  LIndex: integer;
  lChangeGroup   : TChangeGroup;
begin
  try
    cmbChangeGroup.Items.Clear;
    cmbChangeGroup.Text := '';
    if(FChangeGroupsList <> nil) and (FChangeGroupsList.Count > 0)then
    begin
      cmbChangeGroup.Items.AddObject('All Groups',TObject(0));
      for LIndex := 0 to FChangeGroupsList.Count-1 do
      begin
        lChangeGroup := TChangeGroup(FChangeGroupsList.Items[LIndex]);
        if lChangeGroup.ContainsChangeLists then
          cmbChangeGroup.Items.AddObject(lChangeGroup.GroupName,TObject(LIndex));
      end;
      cmbChangeGroup.ItemIndex := 0;
      cmbChangeGroup.Text := cmbChangeGroup.Items[cmbChangeGroup.ItemIndex];
      PopulateChangeLists;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmExportChangeListForm.PopulateChangeLists;
const OPNAME = 'TfrmExportChangeListForm.PopulateChangeLists';
var
  LCount,
  LIndex: integer;
  lChangeList   : TChangeList;
  lChangeGroup   : TChangeGroup;
  LChangeGroupElement : TChangeGroupElement;
begin
  try
    cklboxChangeList.Items.Clear;
    if(FChangeListsList <> nil) and (FChangeListsList.Count > 0)then
    begin
      if(cmbChangeGroup.ItemIndex  = 0) then
      begin
        for LIndex := 0 to FChangeListsList.Count-1 do
        begin
          lChangeList := TChangeList(FChangeListsList.Objects[LIndex]);
          cklboxChangeList.Items.AddObject(lChangeList.ChangeListName,TObject(LIndex));
        end;
      end
      else
      begin
        if(cmbChangeGroup.ItemIndex  > 0)  and (cmbChangeGroup.ItemIndex  < FChangeGroupsList.Count) then
        begin
          LIndex := Integer(cmbChangeGroup.Items.Objects[cmbChangeGroup.ItemIndex]);
          lChangeGroup := TChangeGroup(FChangeGroupsList.Items[LIndex]);
          if lChangeGroup.ContainsChangeLists then
          begin
            for LIndex := 0 to lChangeGroup.ElementCount-1 do
            begin
              LChangeGroupElement := lChangeGroup.CastChangeGroupElementByIndex(LIndex);
              if not LChangeGroupElement.IsElementGroup then
              begin
                for LCount := 0 to FChangeListsList.Count-1 do
                begin
                  lChangeList := TChangeList(FChangeListsList.Objects[LCount]);
                  if(lChangeList.ChangeListID = LChangeGroupElement.ElementID) then
                  begin
                    cklboxChangeList.Items.AddObject(lChangeList.ChangeListName,TObject(LCount));
                    Break;
                  end;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    btnDeSelectAll.Enabled := (cklboxChangeList.Items.Count > 0);
    btnSelectAll.Enabled   := (cklboxChangeList.Items.Count > 0);
    btnOK.Enabled          := False;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmExportChangeListForm.cmbChangeGroupChange(Sender: TObject);
const OPNAME = 'TfrmExportChangeListForm.cmbChangeGroupChange';
begin
  try
    PopulateChangeLists;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmExportChangeListForm.btnSelectAllClick(Sender: TObject);
const OPNAME = 'TfrmExportChangeListForm.btnSelectAllClick';
var
  LIndex: integer;
begin
  try
    for LIndex := 0 to cklboxChangeList.Items.Count -1 do
      cklboxChangeList.Checked[LIndex] := True;
    btnOK.Enabled  := (CheckedCount > 0) ;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmExportChangeListForm.btnDeSelectAllClick(Sender: TObject);
const OPNAME = 'TfrmExportChangeListForm.btnDeSelectAllClick';
var
  LIndex: integer;
begin
  try
    for LIndex := 0 to cklboxChangeList.Items.Count -1 do
      cklboxChangeList.Checked[LIndex] := False;
    btnOK.Enabled  := (CheckedCount > 0);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TfrmExportChangeListForm.CheckedCount: integer;
const OPNAME = 'TfrmExportChangeListForm.CheckedCount';
var
  LIndex: integer;
begin
  Result := 0;
  try
    for LIndex := 0 to cklboxChangeList.Items.Count -1 do
      if cklboxChangeList.Checked[LIndex] then
        Result := Result + 1;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TfrmExportChangeListForm.GetSelections(var ASelectedChangeList: string): boolean;
const OPNAME = 'TfrmExportChangeListForm.GetSelections';
var
  LSelections : TStringList;
  lChangeList : TChangeList;
  LCount,
  LIndex      : integer;
begin
  Result := False;
  try
    ASelectedChangeList := '';
    if (CheckedCount > 0) then
    begin
      LSelections := TStringList.Create;
      try
        for LCount := 0 to cklboxChangeList.Items.Count -1 do
        begin
          if cklboxChangeList.Checked[LCount] then
          begin
            LIndex := Integer(cklboxChangeList.Items.Objects[LCount]);
            lChangeList :=  TChangeList(FChangeListsList.Objects[LIndex]);
            LSelections.Add(IntToStr(lChangeList.ChangeListID));
          end;
        end;
        ASelectedChangeList := LSelections.CommaText;
      finally
        LSelections.Free;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TfrmExportChangeListForm.cklboxChangeListClickCheck(Sender: TObject);
const OPNAME = 'TfrmExportChangeListForm.cklboxChangeListClickCheck';
begin
  try
    btnOK.Enabled  := (CheckedCount > 0);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
