//
//
//  UNIT      : Contains TCopyScenarioDataDialog Classes
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 19/09/2005
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UCopyScenarioDataDialog;

interface
uses
  Classes,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.CheckLst,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent;

type
  TCopyScenarioDataDialog = class(TAbstractForm)
  protected
    pnlTop             : TPanel;
    pnlBottom          : TPanel;
    chklstBoxCopyItems : TCheckListBox;
    lblStudy           : TLabel;
    cmbStudy           : TComboBox;
    lblSubArea         : TLabel;
    cmbSubArea         : TComboBox;
    lblScenario        : TLabel;
    cmbScenario        : TComboBox;
    btnCancel          : TButton;
    btnCopyData        : TButton;
    btnSelectAll       : TButton;
    btnDeselectAll     : TButton;
    chkboxInclude      : TCheckBox;
    FCaptionStr        : string;
    FStudy             : string;
    FSubArea           : string;
    FScenario          : string;
    FTableName         : string;
    FIndexField        : string;
    FDisplayFieldName  : string;
    FWhereClause       : string;
    FSelectedDataContainer : TStringList;
    FExistingDataContainer : TStringList;
    FDisplayDataContainer  : TStringList;

    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure cmbStudyChange(Sender: TObject);
    procedure cmbSubAreaChange(Sender: TObject);
    procedure cmbScenarioChange(Sender: TObject);
    procedure chklstBoxCopyItemsClickCheck(Sender: TObject);
    procedure chkboxIncludeClick(Sender: TObject);
    procedure btnSelectAllClick(Sender: TObject);
    procedure btnDeselectAllClick(Sender: TObject);

    procedure PopulateStudies;
    procedure PopulateSubAreas;
    procedure PopulateScenario;
    procedure PopulateData;

  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    function CopyDataFromScenario(ATableName, AIndexFieldName, ADisplayFieldName, AWhereClause: string;
             ASelectedDataContainer,AExistingDataContainer : TStrings): boolean;
    procedure Resize; override;
    property CaptionStr : string read FCaptionStr write FCaptionStr;
    property Study             : string  read FStudy;
    property SubArea           : string  read FSubArea;
    property Scenario          : string  read FScenario;
  end;

implementation

uses
  Math,
  UDataSetType,
  VCL.Graphics,
  SysUtils,
  UErrorHandlingOperations;

{ TCopyScenarioDataDialog }

procedure TCopyScenarioDataDialog.CreateMemberObjects;
const OPNAME = 'TCopyScenarioDataDialog.CreateMemberObjects';
begin
  inherited;
  try
    //Self.BorderIcons  := [];

    FExistingDataContainer  := TStringList.Create;
    FSelectedDataContainer  := TStringList.Create;
    FDisplayDataContainer   := TStringList.Create;

    pnlTop             := TPanel.Create(Self);
    pnlBottom          := TPanel.Create(Self);
    chklstBoxCopyItems := TCheckListBox.Create(Self);
    lblStudy           := TLabel.Create(Self);
    cmbStudy           := TComboBox.Create(Self);
    lblSubArea         := TLabel.Create(Self);
    cmbSubArea         := TComboBox.Create(Self);
    lblScenario        := TLabel.Create(Self);
    cmbScenario        := TComboBox.Create(Self);
    chkboxInclude      := TCheckBox.Create(Self);
    btnCancel          := TButton.Create(Self);
    btnCopyData        := TButton.Create(Self);
    btnSelectAll       := TButton.Create(Self);
    btnDeselectAll     := TButton.Create(Self);

    pnlTop.Parent              := Self;
    pnlBottom.Parent           := Self;
    chklstBoxCopyItems.Parent  := Self;
    lblStudy.Parent            := pnlTop;
    cmbStudy.Parent            := pnlTop;
    lblSubArea.Parent          := pnlTop;
    cmbSubArea.Parent          := pnlTop;
    lblScenario.Parent         := pnlTop;
    cmbScenario.Parent         := pnlTop;
    chkboxInclude.Parent       := pnlTop;
    btnCancel.Parent           := pnlBottom;
    btnCopyData.Parent         := pnlBottom;
    btnSelectAll.Parent        := pnlBottom;
    btnDeselectAll.Parent      := pnlBottom;

    cmbStudy.OnChange          := cmbStudyChange;
    cmbSubArea.OnChange        := cmbSubAreaChange;
    cmbScenario.OnChange       := cmbScenarioChange;
    chkboxInclude.OnClick      := chkboxIncludeClick;
    btnSelectAll.OnClick       := btnSelectAllClick;
    btnDeselectAll.OnClick     := btnDeselectAllClick;
    chklstBoxCopyItems.OnClickCheck := chklstBoxCopyItemsClickCheck;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.DestroyMemberObjects;
const OPNAME = 'TCopyScenarioDataDialog.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FExistingDataContainer);
    FreeAndNil(FSelectedDataContainer);
    FreeAndNil(FDisplayDataContainer);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCopyScenarioDataDialog.Initialise: boolean;
const OPNAME = 'TCopyScenarioDataDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FExistingDataContainer.Clear;
    FSelectedDataContainer.Clear;
    FDisplayDataContainer.Clear;
    Self.ActiveControl        := cmbStudy;
    btnCopyData.ModalResult   := 1;
    btnCancel.ModalResult     := 2;

    chklstBoxCopyItems.Items.Clear;
    cmbStudy.Items.Clear;
    cmbSubArea.Items.Clear;
    cmbScenario.Items.Clear;
    cmbStudy.Text := '';
    cmbSubArea.Text := '';
    cmbScenario.Text := '';

    Self.Color           := clBtnFace;
    Self.Font.Color      := clWindowText;
    Self.Font.Height     := -11;
    Self.Font.Name       := 'MS Sans Serif';
    Self.Font.Style      := [];
    Self.OldCreateOrder  := False;
    Self.Position        := poDefaultPosOnly;
    Self.PixelsPerInch   := 96;

    pnlTop.BevelInner    := bvLowered;
    pnlTop.TabOrder      := 0;

    lblStudy.ParentFont  := True;
    lblStudy.Font.Style  := [fsBold];

    lblSubArea.ParentFont := True;
    lblSubArea.Font.Style := [fsBold];

    lblScenario.ParentFont := True;
    lblScenario.Font.Style := [fsBold];

    cmbStudy.Style         := csDropDownList;
    cmbStudy.ItemHeight    := 13;
    cmbStudy.TabOrder      := 0;

    cmbSubArea.Style       := csDropDownList;
    cmbSubArea.ItemHeight  := 13;
    cmbSubArea.TabOrder    := 1;

    cmbScenario.Style      := csDropDownList;
    cmbScenario.ItemHeight := 13;
    cmbScenario.TabOrder   := 2;

    chkboxInclude.TabOrder := 3;

    pnlBottom.BevelInner   := bvLowered;
    pnlBottom.TabOrder     := 1;

    btnCancel.ModalResult  := 2;
    btnCancel.TabOrder     := 9;

    btnCopyData.Enabled     := False;
    btnCopyData.ModalResult := 1;
    btnCopyData.TabOrder    := 8;

    btnSelectAll.Enabled     := False;
    btnDeselectAll.Enabled     := False;

    chklstBoxCopyItems.Align       := alClient;
    chklstBoxCopyItems.ItemHeight  := 13;
    chklstBoxCopyItems.TabOrder    := 4;
    chklstBoxCopyItems.MultiSelect := True;

    FStudy             := '';
    FSubArea           := '';
    FScenario          := '';
    FTableName         := '';
    FIndexField        := '';
    FDisplayFieldName  := '';
    FWhereClause       := '';

    PopulateStudies;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.Resize;
const OPNAME = 'TCopyScenarioDataDialog.Resize';
begin
  inherited;
  try
    Self.Left            := 170;
    Self.Top             := 112;
    Self.Width           := 772;
    Self.Height          := 502;

    pnlTop.Left          := 0;
    pnlTop.Top           := 0;
    pnlTop.Width         := 764;
    pnlTop.Height        := 52;
    pnlTop.Align         := alTop;

    lblStudy.Left        := 8;
    lblStudy.Top         := 20;
    lblStudy.Width       := 37;
    lblStudy.Height      := 13;

    lblSubArea.Left       := 200;
    lblSubArea.Top        := 20;
    lblSubArea.Width      := 53;
    lblSubArea.Height     := 13;

    lblScenario.Left       := 408;
    lblScenario.Top        := 20;
    lblScenario.Width      := 55;
    lblScenario.Height     := 13;

    cmbStudy.Left          := 48;
    cmbStudy.Top           := 16;
    cmbStudy.Width         := 145;
    cmbStudy.Height        := 21;

    cmbSubArea.Left        := 256;
    cmbSubArea.Top         := 16;
    cmbSubArea.Width       := 145;
    cmbSubArea.Height      := 21;

    cmbScenario.Left       := 467;
    cmbScenario.Top        := 16;
    cmbScenario.Width      := 145;
    cmbScenario.Height     := 21;

    chkboxInclude.Left     := 635;
    chkboxInclude.Top      := 18;
    chkboxInclude.Width    := 124;
    chkboxInclude.Height   := 17;

    pnlBottom.Left         := 0;
    pnlBottom.Top          := 424;
    pnlBottom.Width        := 764;
    pnlBottom.Height       := 44;
    pnlBottom.Align        := alBottom;

    btnCancel.Left         := 680;
    btnCancel.Top          := 10;
    btnCancel.Width        := 75;
    btnCancel.Height       := 25;

    btnCopyData.Left        := 592;
    btnCopyData.Top         := 10;
    btnCopyData.Width       := 75;
    btnCopyData.Height      := 25;

    btnSelectAll.Enabled     := False;
    btnSelectAll.Left        := 20;
    btnSelectAll.Top         := 10;
    btnSelectAll.Width       := 75;
    btnSelectAll.Height      := 25;

    btnDeselectAll.Enabled     := False;
    btnDeselectAll.Left        := 120;
    btnDeselectAll.Top         := 10;
    btnDeselectAll.Width       := 75;
    btnDeselectAll.Height      := 25;

   chklstBoxCopyItems.Left       := 0;
   chklstBoxCopyItems.Top        := 52;
   chklstBoxCopyItems.Width      := 764;
   chklstBoxCopyItems.Height     := 372;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCopyScenarioDataDialog.LanguageHasChanged: boolean;
const OPNAME = 'TCopyScenarioDataDialog.LanguageHasChanged';
begin
  Result := Inherited LanguageHasChanged;
  try

    Self.Caption           := CaptionStr;
    lblStudy.Caption       := FAppModules.Language.GetString('TCopyScenarioDataDialog.strStudy');
    lblSubArea.Caption     := FAppModules.Language.GetString('TCopyScenarioDataDialog.strSubArea');
    lblScenario.Caption    := FAppModules.Language.GetString('TCopyScenarioDataDialog.strScenario');
    btnCancel.Caption      := FAppModules.Language.GetString('TCopyScenarioDataDialog.strCancel');
    btnCopyData.Caption    := FAppModules.Language.GetString('TCopyScenarioDataDialog.strStartCopy');
    chkboxInclude.Caption  := FAppModules.Language.GetString('TCopyScenarioDataDialog.strInclude');
    btnSelectAll.Caption   := FAppModules.Language.GetString('TCopyScenarioDataDialog.strSelectAll');
    btnDeselectAll.Caption := FAppModules.Language.GetString('TCopyScenarioDataDialog.strDeselectAll');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TCopyScenarioDataDialog.CopyDataFromScenario(ATableName, AIndexFieldName, ADisplayFieldName, AWhereClause: string;
         ASelectedDataContainer, AExistingDataContainer : TStrings): boolean;
const OPNAME = 'TCopyScenarioDataDialog.CopyDataFromScenario';
var
  LData,
  LIndex    : integer;
begin
  Result := False;
  try
    ATableName          :=  Trim(ATableName);
    AIndexFieldName     :=  Trim(AIndexFieldName);
    ADisplayFieldName   :=  Trim(ADisplayFieldName);

    if(ATableName  = '') then
      raise Exception.Create('Table Name parameter cannot be empty.');
    if(AIndexFieldName  = '') then
      raise Exception.Create('AIndex Field parameter cannot be empty.');
    if(ADisplayFieldName  = '') then
      raise Exception.Create('ADisplay Field Name parameter cannot be empty.');
    if not Assigned(ASelectedDataContainer) then
      raise Exception.Create('Index container parameter is not yet assigned.');
    if not Assigned(AExistingDataContainer) then
      raise Exception.Create('Existing names container parameter is not yet assigned.');

    Self.Initialise;
    Self.LanguageHasChanged;

    FTableName          := ATableName;
    FIndexField         := AIndexFieldName;
    FDisplayFieldName   := ADisplayFieldName;
    FWhereClause        := AWhereClause;
    FExistingDataContainer.Assign(AExistingDataContainer);
    ASelectedDataContainer.Clear;
    FSelectedDataContainer.Clear;
    FDisplayDataContainer.Clear;

    Self.ShowModal;
    Result := (Self.ModalResult = mrOk);
    if Result then
    begin
      for LIndex := 0 to chklstBoxCopyItems.Items.Count-1 do
      begin
        if chklstBoxCopyItems.Checked[LIndex] then
        begin
           LData := integer(chklstBoxCopyItems.Items.Objects[LIndex]);
           ASelectedDataContainer.AddObject(chklstBoxCopyItems.Items[LIndex], TObject(LData));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.cmbStudyChange(Sender: TObject);
const OPNAME = 'TCopyScenarioDataDialog.cmbStudyChange';
begin
  try
    FStudy    := cmbStudy.Text;
    FSubArea  := '';
    FScenario := '';
    PopulateSubAreas;
    FSelectedDataContainer.Clear;
    FDisplayDataContainer.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.cmbSubAreaChange(Sender: TObject);
const OPNAME = 'TCopyScenarioDataDialog.cmbSubAreaChange';
begin
  try
    FSubArea  := cmbSubArea.Text;
    FScenario := '';
    PopulateScenario;
    FSelectedDataContainer.Clear;
    FDisplayDataContainer.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.cmbScenarioChange(Sender: TObject);
const OPNAME = 'TCopyScenarioDataDialog.cmbScenarioChange';
begin
  try
    FScenario := cmbScenario.Text;
    FSelectedDataContainer.Clear;
    FDisplayDataContainer.Clear;
    PopulateData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.chklstBoxCopyItemsClickCheck(Sender: TObject);
const OPNAME = 'TCopyScenarioDataDialog.chklstBoxCopyItemsClickCheck';
Var
  LIndex : integer;
begin
  try
    btnCopyData.Enabled := False;
    for LIndex := 0 to chklstBoxCopyItems.Items.Count-1 do
    begin
      if chklstBoxCopyItems.Checked[LIndex] then
      begin
        btnCopyData.Enabled := True;
        Break;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.PopulateStudies;
const OPNAME = 'TCopyScenarioDataDialog.PopulateStudies';
var
  LSQL,
  LData : string;
  lDataSet : TAbstractModelDataset;
begin
  try
    FSelectedDataContainer.Clear;
    FDisplayDataContainer.Clear;
    chklstBoxCopyItems.Items.Clear;
    btnCopyData.Enabled := False;
    btnSelectAll.Enabled := False;
    btnDeselectAll.Enabled := False;
    cmbStudy.Items.Clear;
    cmbSubArea.Items.Clear;
    cmbScenario.Items.Clear;
    cmbStudy.Text := '';
    cmbSubArea.Text := '';
    cmbScenario.Text := '';

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
     LSQL := 'SELECT Model, StudyAreaName'+
             ' FROM StudyScenario'+
             ' WHERE Model        = '+ QuotedStr(FAppModules.StudyArea.ModelCode);
             //+' AND StudyAreaName <> '+ QuotedStr(FAppModules.StudyArea.StudyAreaCode);
     lDataSet.SetSQL(LSQL);
     lDataSet.DataSet.Open;
     while not lDataSet.DataSet.Eof do
     begin
       LData :=  Trim(lDataSet.DataSet.FieldByName('StudyAreaName').AsString);
       if(cmbStudy.Items.IndexOf(LData) < 0) then
       begin
         cmbStudy.Items.Add(LData);
       end;
       lDataSet.DataSet.Next;
     end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.PopulateSubAreas;
const OPNAME = 'TCopyScenarioDataDialog.PopulateSubAreas';
var
  LSQL,
  LData : string;
  lDataSet : TAbstractModelDataset;
begin
  try
    chklstBoxCopyItems.Items.Clear;
    btnCopyData.Enabled := False;
    btnSelectAll.Enabled := False;
    btnDeselectAll.Enabled := False;
    cmbSubArea.Items.Clear;
    cmbScenario.Items.Clear;
    cmbSubArea.Text := '';
    cmbScenario.Text := '';

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
     LSQL := 'SELECT Model, StudyAreaName,SubArea'+
             ' FROM StudyScenario'+
             ' WHERE Model        = '+ QuotedStr(FAppModules.StudyArea.ModelCode)+
             ' AND StudyAreaName  = '+ QuotedStr(cmbStudy.Text);
     lDataSet.SetSQL(LSQL);
     lDataSet.DataSet.Open;
     while not lDataSet.DataSet.Eof do
     begin
       LData :=  Trim(lDataSet.DataSet.FieldByName('SubArea').AsString);
       if(cmbSubArea.Items.IndexOf(LData) < 0) then
       begin
         cmbSubArea.Items.Add(LData);
       end;
       lDataSet.DataSet.Next;
     end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.PopulateScenario;
const OPNAME = 'TCopyScenarioDataDialog.PopulateScenario';
var
  LSQL,
  LData : string;
  lDataSet : TAbstractModelDataset;
begin
  try
    chklstBoxCopyItems.Items.Clear;
    btnCopyData.Enabled := False;
    btnSelectAll.Enabled := False;
    btnDeselectAll.Enabled := False;
    cmbScenario.Items.Clear;
    cmbScenario.Text := '';

    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
     LSQL := 'SELECT Model,StudyAreaName,SubArea,Scenario'+
             ' FROM StudyScenario'+
             ' WHERE Model        = '+ QuotedStr(FAppModules.StudyArea.ModelCode)+
             ' AND StudyAreaName  = '+ QuotedStr(cmbStudy.Text)+
             ' AND SubArea        = '+ QuotedStr(cmbSubArea.Text);
     lDataSet.SetSQL(LSQL);
     lDataSet.DataSet.Open;
     while not lDataSet.DataSet.Eof do
     begin
       LData :=  Trim(lDataSet.DataSet.FieldByName('Scenario').AsString);
       if(cmbScenario.Items.IndexOf(LData) < 0) and (FAppModules.StudyArea.ScenarioCode <> LData)then
       begin
         cmbScenario.Items.Add(LData);
       end;
       lDataSet.DataSet.Next;
     end;
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.PopulateData;
const OPNAME = 'TCopyScenarioDataDialog.PopulateData';
var
  LDisplayData            : string;
  LSQL                    : string;
  LIndexFieldData         : integer;
  LDisplayFieldNameData   : string;
  lDataSet                : TAbstractModelDataset;
begin
  try
    FSelectedDataContainer.Clear;
    FDisplayDataContainer.Clear;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      LSQL := 'SELECT ' + FIndexField + ',' + FDisplayFieldName +
              ' FROM '+ FTableName + ' WHERE '+
              ' (Model         = ' + QuotedStr(FAppModules.StudyArea.ModelCode)+ ') AND ' +
              ' (StudyAreaName = ' + QuotedStr(cmbStudy.Text) + ') AND ' +
              ' (SubArea       = ' + QuotedStr(cmbSubArea.Text)   + ') AND ' +
              ' (Scenario      = ' + QuotedStr(cmbScenario.Text)  + ') ';
      if(Trim(FWhereClause) <> '') then
        LSQL := LSQL + '  '+ FWhereClause;
      lDataSet.SetSQL(LSQL);
      lDataSet.DataSet.Open;
      while not lDataSet.DataSet.Eof do
      begin
       LIndexFieldData         := lDataSet.DataSet.FieldByName(FIndexField).AsInteger;
       LDisplayFieldNameData   := Trim(lDataSet.DataSet.FieldByName(FDisplayFieldName).AsString);
       LDisplayData            := '('+ IntToStr(LIndexFieldData)+') '+ LDisplayFieldNameData;
       FDisplayDataContainer .AddObject(LDisplayData,TObject(LIndexFieldData));
       FSelectedDataContainer.AddObject(LDisplayFieldNameData,TObject(LIndexFieldData));
       lDataSet.DataSet.Next;
      end;
      chkboxIncludeClick(nil);
    finally
      lDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.chkboxIncludeClick(Sender: TObject);
const OPNAME = 'TCopyScenarioDataDialog.chkboxIncludeClick';
var
  LIndex  : integer;
  LDisplayFieldNameData   : string;
begin
  try
    chklstBoxCopyItems.Items.Clear;
    btnCopyData.Enabled := False;
    if not chkboxInclude.Checked then
    begin
      for LIndex := 0 to FSelectedDataContainer.Count-1 do
      begin
        LDisplayFieldNameData := FSelectedDataContainer[LIndex];
        if(FExistingDataContainer.IndexOf(LDisplayFieldNameData) < 0) then
           chklstBoxCopyItems.Items.AddObject(FDisplayDataContainer[LIndex],FDisplayDataContainer.Objects[LIndex]);
      end;
    end
    else
    begin
      chklstBoxCopyItems.Items.Assign(FDisplayDataContainer);
    end;

    btnSelectAll.Enabled   := chklstBoxCopyItems.Items.Count > 0;
    btnDeselectAll.Enabled := chklstBoxCopyItems.Items.Count > 0;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.btnSelectAllClick(Sender: TObject);
const OPNAME = 'TCopyScenarioDataDialog.btnSelectAllClick';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to chklstBoxCopyItems.Items.Count-1 do
    begin
      if chklstBoxCopyItems.ItemEnabled[LIndex] then
        chklstBoxCopyItems.Checked[LIndex] := True;
    end;
    chklstBoxCopyItemsClickCheck(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TCopyScenarioDataDialog.btnDeselectAllClick(Sender: TObject);
const OPNAME = 'TCopyScenarioDataDialog.btnDeselectAllClick';
var
  LIndex : integer;
begin
  try
    for LIndex := 0 to chklstBoxCopyItems.Items.Count-1 do
      chklstBoxCopyItems.Checked[LIndex] := False;
    chklstBoxCopyItemsClickCheck(nil);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
