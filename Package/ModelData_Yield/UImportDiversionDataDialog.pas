//
//
//  UNIT      : Contains TImportDiversionDataDialog Classes
//  AUTHOR    : Dhlamini Samuel (Cornastone)
//  DATE      : 15/01/2009
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UImportDiversionDataDialog;

interface
uses
  Classes,
  VCL.Controls,
  VCL.Forms,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  UConstants,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent;

type
  TImportDiversionDataDialog = class(TAbstractForm)
  protected
    pnlTop              : TPanel;
    pnlBottom           : TPanel;
    lblStudy            : TStaticText;
    cmbStudy            : TComboBox;
    lblSubArea          : TStaticText;
    cmbSubArea          : TComboBox;
    lblScenario         : TStaticText;
    cmbScenario         : TComboBox;
    lblDiversionStation : TStaticText;
    cmbDiversionStation : TComboBox;
    lblDiversionData    : TStaticText;
    FstrgrdData         : TFieldStringGrid;

    btnCancel           : TButton;
    btnImportData       : TButton;
    FCaptionStr         : string;
    FStudy              : string;
    FSubArea            : string;
    FScenario           : string;
    FDiversionStation   : string;
    FReferenceData      : TStringList;
    FDiversionData      : TStringList;
    FNondiversionData   : TStringList;
    FStationID          : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure cmbStudyChange(Sender: TObject);
    procedure cmbSubAreaChange(Sender: TObject);
    procedure cmbScenarioChange(Sender: TObject);
    procedure cmbDiversionStationChange(Sender: TObject);

    procedure PopulateStudies;
    procedure PopulateSubAreas;
    procedure PopulateScenario;
    procedure PopulateDiversionStation;
    procedure PopulateData;
  public
    function GetDiversionData(var AStationID : integer;AReferenceData,ADiversionData, ANondiversionData : TStrings) : boolean;
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    procedure Resize; override;
    property CaptionStr : string read FCaptionStr write FCaptionStr;
    property Study             : string  read FStudy;
    property SubArea           : string  read FSubArea;
    property Scenario          : string  read FScenario;
    property DiversionStation  : string  read FDiversionStation;
  end;

implementation

uses
  Math,
  UDataSetType,
  VCL.Graphics,
  SysUtils,
  UErrorHandlingOperations,
  UProgressDialog, DB, VCL.Grids;

{ TImportDiversionDataDialog }

procedure TImportDiversionDataDialog.CreateMemberObjects;
const OPNAME = 'TImportDiversionDataDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    //Self.BorderIcons  := [];

    FReferenceData := TStringList.Create;
    FDiversionData := TStringList.Create;
    FNondiversionData  := TStringList.Create;
      
    pnlTop                  := TPanel.Create(Self);
    pnlBottom               := TPanel.Create(Self);
    lblStudy                := TStaticText.Create(Self);
    cmbStudy                := TComboBox.Create(Self);
    lblSubArea              := TStaticText.Create(Self);
    cmbSubArea              := TComboBox.Create(Self);
    lblScenario             := TStaticText.Create(Self);
    cmbScenario             := TComboBox.Create(Self);
    lblDiversionStation     := TStaticText.Create(Self);
    cmbDiversionStation     := TComboBox.Create(Self);
    lblDiversionData        := TStaticText.Create(Self);
    FstrgrdData             := TFieldStringGrid.Create(nil,FAppModules);

    btnCancel               := TButton.Create(Self);
    btnImportData           := TButton.Create(Self);

    pnlTop.Parent              := Self;
    pnlBottom.Parent           := Self;
    lblStudy.Parent            := pnlTop;
    cmbStudy.Parent            := pnlTop;
    lblSubArea.Parent          := pnlTop;
    cmbSubArea.Parent          := pnlTop;
    lblScenario.Parent         := pnlTop;
    cmbScenario.Parent         := pnlTop;
    lblDiversionStation.Parent := pnlTop;
    cmbDiversionStation.Parent := pnlTop;
    FstrgrdData.Parent         := pnlTop;
    lblDiversionData.Parent    := pnlTop;

    btnCancel.Parent           := pnlBottom;
    btnImportData.Parent       := pnlBottom;

    cmbStudy.OnChange          := cmbStudyChange;
    cmbSubArea.OnChange        := cmbSubAreaChange;
    cmbScenario.OnChange       := cmbScenarioChange;
    cmbDiversionStation.OnChange := cmbDiversionStationChange;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImportDiversionDataDialog.DestroyMemberObjects;
const OPNAME = 'TImportDiversionDataDialog.DestroyMemberObjects';
begin
  inherited;
  try
    FreeAndNil(FReferenceData);
    FreeAndNil(FDiversionData);
    FreeAndNil(FNondiversionData);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImportDiversionDataDialog.Initialise: boolean;
const OPNAME = 'TImportDiversionDataDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FReferenceData.Clear;
    FDiversionData.Clear;
    FNondiversionData.Clear;

    Self.ActiveControl        := cmbStudy;
    btnImportData.ModalResult := 1;
    btnCancel.ModalResult     := 2;
    
    cmbStudy.Items.Clear;
    cmbSubArea.Items.Clear;
    cmbScenario.Items.Clear;
    cmbStudy.Text            := '';
    cmbSubArea.Text          := '';
    cmbScenario.Text         := '';
    cmbDiversionStation.Text := '';

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

    lblDiversionStation.ParentFont := True;
    lblDiversionStation.Font.Style := [fsBold];

    lblDiversionData.ParentFont := True;
    lblDiversionData.Font.Style := [fsBold];
    lblDiversionData.Visible    := False;

    FstrgrdData.ColCount         := 4;
    FstrgrdData.RowCount         := 2;
    FstrgrdData.FixedCols        := 1;
    FstrgrdData.FixedRows        := 1;
    FstrgrdData.WrapHeaderText   := True;
    FstrgrdData.DefaultRowHeight := 15;
    FstrgrdData.RowHeights[0] := 30;
    FstrgrdData.ColWidths[0] := 20;
    FstrgrdData.ColWidths[1] := 75;
    FstrgrdData.ColWidths[2] := 75;
    FstrgrdData.ColWidths[3] := 75;
    FstrgrdData.Height := 300;
    FstrgrdData.Width := 170;

    cmbStudy.Style         := csDropDownList;
    cmbStudy.ItemHeight    := 13;
    cmbStudy.TabOrder      := 0;

    cmbSubArea.Style       := csDropDownList;
    cmbSubArea.ItemHeight  := 13;
    cmbSubArea.TabOrder    := 1;

    cmbScenario.Style      := csDropDownList;
    cmbScenario.ItemHeight := 13;
    cmbScenario.TabOrder   := 2;

    FstrgrdData.Visible := False; 

    pnlBottom.BevelInner   := bvLowered;
    pnlBottom.TabOrder     := 1;

    btnCancel.ModalResult  := 2;
    btnCancel.TabOrder     := 9;

    btnImportData.Enabled     := False;
    btnImportData.ModalResult := 1;
    btnImportData.TabOrder    := 8;

    FStudy             := '';
    FSubArea           := '';
    FScenario          := '';
    FDiversionStation  := '';

    PopulateStudies;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImportDiversionDataDialog.Resize;
const OPNAME = 'TImportDiversionDataDialog.Resize';
begin
  inherited;
  try
    pnlTop.Left          := 0;
    pnlTop.Top           := 0;
    pnlTop.Width         := 464;
    pnlTop.Height        := 420;
    pnlTop.Align         := alTop;

    lblStudy.Left        := 8;
    lblStudy.Top         := 20;
    lblStudy.Width       := 37;
    lblStudy.Height      := 13;

    lblSubArea.Left       := 8;
    lblSubArea.Top        := 50;
    lblSubArea.Width      := 53;
    lblSubArea.Height     := 13;

    lblScenario.Left       := 8;
    lblScenario.Top        := 80;
    lblScenario.Width      := 55;
    lblScenario.Height     := 13;

    lblDiversionStation.Left       := 8;
    lblDiversionStation.Top        := 110;
    lblDiversionStation.Width      := 120;
    lblDiversionStation.Height     := 13;

    lblDiversionData.Left       := 8;
    lblDiversionData.Top        := 140;
    lblDiversionData.Width      := 150;
    lblDiversionData.Height     := 13;

    cmbStudy.Left          := 160;
    cmbStudy.Top           := 16;
    cmbStudy.Width         := 145;
    cmbStudy.Height        := 21;

    cmbSubArea.Left        := 160;
    cmbSubArea.Top         := 46;
    cmbSubArea.Width       := 145;
    cmbSubArea.Height      := 21;

    cmbScenario.Left       := 160;
    cmbScenario.Top        := 76;
    cmbScenario.Width      := 145;
    cmbScenario.Height     := 21;

    cmbDiversionStation.Left    := 160;
    cmbDiversionStation.Top     := 106;
    cmbDiversionStation.Width   := 145;
    cmbDiversionStation.Height  := 21;

    FstrgrdData.Left        := 160;
    FstrgrdData.Top         := 136;
    FstrgrdData.Width := (FstrgrdData.DefaultColWidth*FstrgrdData.ColCount);
    FstrgrdData.Height := 230;

    pnlBottom.Left         := 0;
    pnlBottom.Top          := 291;
    pnlBottom.Width        := 464;
    pnlBottom.Height       := 44;
    pnlBottom.Align        := alBottom;

    btnCancel.Left         := 380;
    btnCancel.Top          := 10;
    btnCancel.Width        := 75;
    btnCancel.Height       := 25;

    btnImportData.Left        := 280;
    btnImportData.Top         := 10;
    btnImportData.Width       := 75;
    btnImportData.Height      := 25;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TImportDiversionDataDialog.LanguageHasChanged: boolean;
const OPNAME = 'TImportDiversionDataDialog.LanguageHasChanged';
begin
  Result := Inherited LanguageHasChanged;
  try
    Self.Caption           := FAppModules.Language.GetString('TImportDiversionDataDialog.ImportDataFromOtherScenario');
    lblStudy.Caption       := FAppModules.Language.GetString('TImportDiversionDataDialog.Study');
    lblSubArea.Caption     := FAppModules.Language.GetString('TImportDiversionDataDialog.SubArea');
    lblScenario.Caption    := FAppModules.Language.GetString('TImportDiversionDataDialog.Scenario');
    lblDiversionStation.Caption    := FAppModules.Language.GetString('TImportDiversionDataDialog.DiversionStation');
    lblDiversionData.Caption       := FAppModules.Language.GetString('TImportDiversionDataDialog.DiversionRelationship');
    btnCancel.Caption      := FAppModules.Language.GetString('TImportDiversionDataDialog.Cancel');
    btnImportData.Caption  := FAppModules.Language.GetString('TImportDiversionDataDialog.Import');
    FstrgrdData.cells[0,0] := FAppModules.Language.GetString('GridHeading.Rank');
    FstrgrdData.cells[1,0] := FAppModules.Language.GetString('GridHeading.ReferenceFlow');
    FstrgrdData.cells[2,0] := FAppModules.Language.GetString('GridHeading.DiversionFlow');
    FstrgrdData.cells[3,0] := FAppModules.Language.GetString('GridHeading.NonDiversionFlow');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImportDiversionDataDialog.cmbStudyChange(Sender: TObject);
const OPNAME = 'TImportDiversionDataDialog.cmbStudyChange';
begin
  try
    FStudy    := cmbStudy.Text;
    FSubArea  := '';
    FScenario := '';
    FDiversionStation := '';
    PopulateSubAreas;
    FReferenceData.Clear;
    FDiversionData.Clear;
    FNondiversionData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImportDiversionDataDialog.cmbSubAreaChange(Sender: TObject);
const OPNAME = 'TImportDiversionDataDialog.cmbSubAreaChange';
begin
  try
    FSubArea  := cmbSubArea.Text;
    FScenario := '';
    PopulateScenario;
    FReferenceData.Clear;
    FDiversionData.Clear;
    FNondiversionData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImportDiversionDataDialog.cmbScenarioChange(Sender: TObject);
const OPNAME = 'TImportDiversionDataDialog.cmbScenarioChange';
begin
  try
    FScenario := cmbScenario.Text;
    FDiversionStation := '';
    PopulateDiversionStation;
    FReferenceData.Clear;
    FDiversionData.Clear;
    FNondiversionData.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImportDiversionDataDialog.cmbDiversionStationChange(Sender: TObject);
const OPNAME = 'TImportDiversionDataDialog.cmbDiversionStationChange';
begin
  try
    FDiversionStation := cmbDiversionStation.Text;
    FReferenceData.Clear;
    FDiversionData.Clear;
    FNondiversionData.Clear;
    PopulateData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TImportDiversionDataDialog.PopulateStudies;
const OPNAME = 'TImportDiversionDataDialog.PopulateStudies';
var
  LSQL,
  LData : string;
  LDataSet : TAbstractModelDataset;
begin
  try
    FReferenceData.Clear;
    FDiversionData.Clear;
    FNondiversionData.Clear;
    btnImportData.Enabled := False;
    cmbStudy.Items.Clear;
    cmbSubArea.Items.Clear;
    cmbScenario.Items.Clear;
    cmbDiversionStation.Items.Clear;
    cmbStudy.Text := '';
    cmbSubArea.Text := '';
    cmbScenario.Text := '';
    cmbDiversionStation.Text := '';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
     LSQL := 'SELECT Model, StudyAreaName'+
             ' FROM StudyScenario'+
             ' WHERE Model        = '+ QuotedStr(CDailyDiversion);
     LDataSet.SetSQL(LSQL);
     LDataSet.DataSet.Open;
     while not LDataSet.DataSet.Eof do
     begin
       LData :=  Trim(LDataSet.DataSet.FieldByName('StudyAreaName').AsString);
       if(cmbStudy.Items.IndexOf(LData) < 0) then
       begin
         cmbStudy.Items.Add(LData);
       end;
       lDataSet.DataSet.Next;
     end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImportDiversionDataDialog.PopulateSubAreas;
const OPNAME = 'TImportDiversionDataDialog.PopulateSubAreas';
var
  LSQL,
  LData : string;
  lDataSet : TAbstractModelDataset;
begin
  try
    btnImportData.Enabled := False;
    cmbSubArea.Items.Clear;
    cmbScenario.Items.Clear;
    cmbDiversionStation.Items.Clear;
    cmbSubArea.Text := '';
    cmbScenario.Text := '';
    cmbDiversionStation.Text := '';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
     LSQL := 'SELECT Model, StudyAreaName,SubArea'+
             ' FROM StudyScenario'+
             ' WHERE Model        = '+ QuotedStr(CDailyDiversion)+
             ' AND StudyAreaName  = '+ QuotedStr(cmbStudy.Text);
     LDataSet.SetSQL(LSQL);
     LDataSet.DataSet.Open;
     while not lDataSet.DataSet.Eof do
     begin
       LData :=  Trim(LDataSet.DataSet.FieldByName('SubArea').AsString);
       if(cmbSubArea.Items.IndexOf(LData) < 0) then
       begin
         cmbSubArea.Items.Add(LData);
       end;
       LDataSet.DataSet.Next;
     end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TImportDiversionDataDialog.PopulateScenario;
const OPNAME = 'TImportDiversionDataDialog.PopulateScenario';
var
  LSQL,
  LData : string;
  LDataSet : TAbstractModelDataset;
begin
  try
    btnImportData.Enabled := False;
    cmbScenario.Items.Clear;
    cmbDiversionStation.Items.Clear;
    cmbScenario.Text := '';
    cmbDiversionStation.Text := '';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
     LSQL := 'SELECT Model,StudyAreaName,SubArea,Scenario'+
             ' FROM StudyScenario'+
             ' WHERE Model        = '+ QuotedStr(CDailyDiversion)+
             ' AND StudyAreaName  = '+ QuotedStr(cmbStudy.Text)+
             ' AND SubArea        = '+ QuotedStr(cmbSubArea.Text);
     LDataSet.SetSQL(LSQL);
     LDataSet.DataSet.Open;
     while not LDataSet.DataSet.Eof do
     begin
       LData :=  Trim(LDataSet.DataSet.FieldByName('Scenario').AsString);
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


procedure TImportDiversionDataDialog.PopulateDiversionStation;
const OPNAME = 'TImportDiversionDataDialog.PopulateDiversionStation';
var
  LSQL,
  LData : string;
  LDataSet : TAbstractModelDataset;
begin
  try
    btnImportData.Enabled := False;
    cmbDiversionStation.Items.Clear;
    cmbDiversionStation.Text := '';
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
     LSQL := 'SELECT StationNo, StationID'+
             ' FROM DailyDiversionStation'+
             ' WHERE Model        = '+ QuotedStr(CDailyDiversion)+
             ' AND StudyAreaName  = '+ QuotedStr(cmbStudy.Text)+
             ' AND SubArea        = '+ QuotedStr(cmbSubArea.Text);
     LDataSet.SetSQL(LSQL);
     LDataSet.DataSet.Open;
     while not LDataSet.DataSet.Eof do
     begin
       LData :=  Trim(LDataSet.DataSet.FieldByName('StationNo').AsString);
       if(cmbDiversionStation.Items.IndexOf(LData) < 0) then
         cmbDiversionStation.Items.AddObject(LData,TObject(LDataSet.DataSet.FieldByName('StationID').AsInteger));

       LDataSet.DataSet.Next;
     end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TImportDiversionDataDialog.PopulateData;
const OPNAME = 'TImportDiversionDataDialog.PopulateData';
var
  LSQL                    : string;
  lDataSet                : TAbstractModelDataset;
  LIndex                  : integer;
begin
  try
    FReferenceData.Clear;
    FDiversionData.Clear;
    FNondiversionData.Clear;
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      LSQL := 'SELECT dw.Identifier,dw.StationID,dw.ReferenceFlow,dw.DiversionFlow,dw.NonDiversionFlow ' +
              ' FROM DailyDiversionWRYMData dw, DailyDiversionStation ds  WHERE '+
              ' (dw.Model         = ' + QuotedStr(CDailyDiversion)+ ') AND ' +
              ' (dw.StudyAreaName = ' + QuotedStr(cmbStudy.Text) + ') AND ' +
              ' (dw.SubArea       = ' + QuotedStr(cmbSubArea.Text)   + ') AND ' +
              ' (dw.Scenario      = ' + QuotedStr(cmbScenario.Text)  + ') AND ' +
              ' (dw.StationID     = ' + IntToStr(integer(cmbDiversionStation.Items.Objects[cmbDiversionStation.ItemIndex]))+ ') AND ' +
              ' (dw.Model         = ds.Model ) AND ' +
              ' (dw.StudyAreaName = ds.StudyAreaName) AND ' +
              ' (dw.SubArea       = ds.SubArea) AND ' +
              ' (dw.Scenario      = ds.Scenario) AND ' +
              ' (ds.StationNo     = ' + QuotedStr(cmbDiversionStation.Text)+')';

      LDataSet.SetSQL(LSQL);
      LDataSet.DataSet.Open;
      LDataSet.DataSet.Last;
      LDataSet.DataSet.First;
      FStationID := integer(cmbDiversionStation.Items.Objects[cmbDiversionStation.ItemIndex]);
      FstrgrdData.Visible := LDataSet.DataSet.RecordCount>0;
      btnImportData.Enabled := FstrgrdData.Visible;
      lblDiversionData.Visible := FstrgrdData.Visible;
      FstrgrdData.RowCount := LDataSet.DataSet.RecordCount+1;
      LIndex := 0;
      while not lDataSet.DataSet.Eof do
      begin

        FstrgrdData.Cells[0, LIndex + 1] :=IntToStr(LIndex+1);
        if LDataSet.DataSet.FieldByName('ReferenceFlow').AsFloat <> NullFloat then
        begin
          FstrgrdData.Cells[1, LIndex + 1] := FormatFloat('####0.000',LDataSet.DataSet.FieldByName('ReferenceFlow').AsFloat);
          FReferenceData.Add(FormatFloat('####0.000',LDataSet.DataSet.FieldByName('ReferenceFlow').AsFloat));
        end
        else
          FstrgrdData.Cells[1, LIndex + 1] := '';

        if LDataSet.DataSet.FieldByName('DiversionFlow').AsFloat <> NullFloat then
        begin
          FstrgrdData.Cells[2, LIndex + 1] := FormatFloat('####0.000',LDataSet.DataSet.FieldByName('DiversionFlow').AsFloat);
          FDiversionData.Add(FormatFloat('####0.000',LDataSet.DataSet.FieldByName('DiversionFlow').AsFloat));
        end
        else
          FstrgrdData.Cells[2, LIndex + 1] := '';
        if LDataSet.DataSet.FieldByName('NonDiversionFlow').AsFloat <> NullFloat then
        begin
          FstrgrdData.Cells[3, LIndex + 1] := FormatFloat('####0.000',LDataSet.DataSet.FieldByName('NonDiversionFlow').AsFloat);
          FNondiversionData.Add(FormatFloat('####0.000',LDataSet.DataSet.FieldByName('NonDiversionFlow').AsFloat));
        end  
        else
          FstrgrdData.Cells[3, LIndex + 1] := '';
        LIndex := LIndex+1;
        LDataSet.DataSet.Next;
      end;
    finally
      LDataset.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TImportDiversionDataDialog.GetDiversionData(var AStationID : integer;AReferenceData,ADiversionData, ANondiversionData : TStrings) : boolean;
const OPNAME = 'TImportDiversionDataDialog.GetDiversionData';
begin
  Result := False;
  try
    if Assigned(AReferenceData) then
      AReferenceData.Assign(FReferenceData);
    if Assigned(ADiversionData) then
      ADiversionData.Assign(FDiversionData);
    if Assigned(ANondiversionData) then
      ANondiversionData.Assign(FNondiversionData);
    AStationID := FStationID;
    
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
