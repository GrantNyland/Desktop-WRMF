unit URunOffAfforestationDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentRunOffAfforestation,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TRunOffAfforestationDlg = class(TForm)
    PnlBottom                   : TPanel;
    BtnApply                    : TButton;
    BtnReset                    : TButton;
    FXMLDocumentIn              : TXMLDocument;
    FXMLDocumentOut             : TXMLDocument;
    ScrClient                   : TScrollBox;
    LblAffAlgorithm             : TLabel;
    LblPineAreaPercentage       : TLabel;
    LblPineRotationPeriod       : TLabel;
    LblEucalyptusAreaPercentage : TLabel;
    LblEucalyptusRotationPeriod : TLabel;
    LblWattleAreaPercentage     : TLabel;
    LblWattleRotationPeriod     : TLabel;
    LblOptimalAreaPercentage    : TLabel;
    LblSFRReductionMAR          : TLabel;
    LblSFRReductionLowFlows     : TLabel;
    EdtSFRReductionLowFlows     : TWRMFEdit;
    EdtSFRReductionMAR          : TWRMFEdit;
    EdtAffOptimalAreaPercentage : TWRMFEdit;
    EdtWattleRotationPeriod     : TWRMFEdit;
    EdtWattleAreaPercentage     : TWRMFEdit;
    EdtEucalyptusRotationPeriod : TWRMFEdit;
    EdtEucalyptusAreaPercentage : TWRMFEdit;
    EdtPineRotationPeriod       : TWRMFEdit;
    EdtPineAreaPercentage       : TWRMFEdit;
    CbxAffAlgorithm             : TWRMFComboBox;
    GrdAffAreaData              : TWRMFGrid;
    BtnAddRow                   : TSpeedButton;
    BtnDeleteRow                : TSpeedButton;
    BtnRowUp                    : TSpeedButton;
    BtnRowDown                  : TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ParamChangeIndicatorClicked (Sender: TObject);
    procedure MetaDataIndicatorClicked (Sender: TObject);
    procedure GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure BtnAddRowClick(Sender: TObject);
    procedure BtnDeleteRowClick(Sender: TObject);
    procedure BtnRowUpClick(Sender: TObject);
    procedure BtnRowDownClick(Sender: TObject);
    procedure GrdAffAreaDataDataCellExit(ASender: TObject; ACol,ARow: Integer);
    procedure GrdAffAreaDataExit(Sender: TObject);
  private
    { Private declarations }
    FErrorList        : TStringList;
    FIdentifier       : String;
    FKeyValues        : WideString;
    FControlIterator  : TControlIterator;
    function LoadXMLData : Boolean;
    function StoreXMLData : Boolean;
    function DoValidation (AContext      : TValidationContext;
                           APropertyName : String;
                           AFieldIndex   : String) : Boolean;
    procedure SetIndicators;
    procedure SetControls;
    procedure SetAllValid;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentRunOffAfforestation;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TRunOffAfforestationDlg.FormCreate(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentRunOffAfforestation.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.FormShow(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdAffAreaData.Cells[0, 0] := 'Year';
    GrdAffAreaData.Cells[1, 0] := 'Area';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.SetControls;
const OPNAME = 'TRunOffAfforestationDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnAddRow.Enabled    := FMayChangeNetwork;
    BtnDeleteRow.Enabled := FMayChangeNetwork;
    BtnRowUp.Enabled     := FMayChangeNetwork;
    BtnRowDown.Enabled   := FMayChangeNetwork;

    BtnApply.Enabled     := FMayChangeNetwork;
    BtnReset.Enabled     := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.SetIndicators;
const OPNAME = 'TRunOffAfforestationDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.SetAllValid;
const OPNAME = 'TRunOffAfforestationDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestationDlg.LoadXMLData: boolean;
const OPNAME = 'TRunOffAfforestationDlg.LoadXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // Populate comboboxes
    CbxAffAlgorithm.Items.Clear;
    CbxAffAlgorithm.Items.AddObject('Van der Zel', pointer(0));
    CbxAffAlgorithm.Items.AddObject('CSIR', pointer(1));
    CbxAffAlgorithm.Items.AddObject('Smoothed Gush/Pitman', pointer(2));
    CbxAffAlgorithm.Items.AddObject('LUT Gush', pointer(3));
    CbxAffAlgorithm.Items.AddObject('User defined reduction', pointer(4));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestationDlg.StoreXMLData : Boolean;
const OPNAME = 'TRunOffAfforestationDlg.StoreXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.ControlExit(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.GrdAffAreaDataDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TRunOffAfforestationDlg.GrdAffAreaDataDataCellExit';
var
  LCellInfo : TCellInfo;
  LGrid     : TWRMFGrid;
begin
  try
    LGrid := TWRMFGrid(ASender);
    LCellInfo := LGrid.CellInfo[ARow, ACol];
    LCellInfo.IsValid := TRUE;
    DoValidation(tcChange, TWRMFGrid(ASender).CellInfo[ARow, ACol].PropertyName, IntToStr(ARow));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.GrdAffAreaDataExit(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.GrdAffAreaDataExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdAffAreaDataDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.BtnApplyClick';
begin
  try
    SetAllValid;
    if (DoValidation(tcApply, '', '')) then
    begin
      FHydrologyModel.UpdateNetworkData(FXMLDocumentOut.XML.Text);
      LoadXMLData;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAfforestationDlg.DoValidation (AContext      : TValidationContext;
                                               APropertyName : String;
                                               AFieldIndex   : String) : Boolean;
const OPNAME = 'TRunOffAfforestationDlg.DoValidation';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    if ((AContext = tcChange) OR (AContext = tcApply)) then
    begin
      Result := StoreXMLData;
      if (Result) then
      begin
        LRootNode := FXMLDocumentOut.DocumentElement;
        Result := FControlIterator.DoValidation(AContext, APropertyName, AFieldIndex,
                                      FXMLDocumentIn.XML.Text, FXMLDocumentOut.XML.Text, FALSE,
                                      FErrorList, FXMLAgent, LRootNode);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.ParamChangeIndicatorClicked';
{var
  LControl : TWRMFControl;}
begin
  try
{    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if (LControl.HasParamChange) then
        ShowMessage ('SHOW Param Changes for ' + LControl.PropertyName + ' ' + FKeyValues)
      else
        ShowMessage ('NEW Param Changes for ' + LControl.PropertyName + ' ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.MetaDataIndicatorClicked';
{var
  LControl : TWRMFControl;}
begin
  try
{    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if ((Sender is TWRMFControl) AND (TWRMFControl(Sender).HasMetaData)) then
        ShowMessage ('SHOW Meta Data for ' + LControl.PropertyName + ' ' + FKeyValues)
      else
        ShowMessage ('NEW Meta Data for ' + LControl.PropertyName + ' ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffAfforestationDlg.GridParamChangeIndicatorClicked';
{var
  LGrid     : TWRMFGrid;
  LCellInfo : TCellInfo;}
begin
  try
{    if (Sender is TWRMFGrid) then
    begin
      LGrid := TWRMFGrid(Sender);
      LCellInfo := LGrid.CellInfo[ARow, ACol];
      if (LCellInfo.HasParamChange) then
        ShowMessage ('SHOW Param Changes for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues)
      else
        ShowMessage ('NEW Param Changes for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffAfforestationDlg.GridMetaDataIndicatorClicked';
{var
  LGrid     : TWRMFGrid;
  LCellInfo : TCellInfo;}
begin
  try
{    if (Sender is TWRMFGrid) then
    begin
      LGrid := TWRMFGrid(Sender);
      LCellInfo := LGrid.CellInfo[ARow, ACol];
      if (LCellInfo.HasMetaData) then
        ShowMessage ('SHOW Meta Data for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues)
      else
        ShowMessage ('NEW Meta Data for ' + LCellInfo.PropertyName + ' (' + IntToStr(ARow) + ') ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.BtnAddRowClick';
begin
  try
    GrdAffAreaData.InsertRow(GrdAffAreaData.Row);
    if (GrdAffAreaData.RowCount > GrdAffAreaData.CellsInfo.NoOfHeadingRows) then
      GrdAffAreaData.FixedRows := GrdAffAreaData.CellsInfo.NoOfHeadingRows;
    GrdAffAreaData.Enabled := (GrdAffAreaData.RowCount > GrdAffAreaData.CellsInfo.NoOfHeadingRows);
    if (GrdAffAreaData.Enabled) then
      GrdAffAreaData.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;                           

procedure TRunOffAfforestationDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.BtnDeleteRowClick';
begin
  try
    if (GrdAffAreaData.Row >= GrdAffAreaData.CellsInfo.NoOfHeadingRows) then
    begin
      GrdAffAreaData.DeleteRow(GrdAffAreaData.Row);
      GrdAffAreaData.Enabled := (GrdAffAreaData.RowCount > GrdAffAreaData.CellsInfo.NoOfHeadingRows);
      if (GrdAffAreaData.Enabled) then
        GrdAffAreaData.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.BtnRowUpClick';
begin
  try
    if (GrdAffAreaData.Row > GrdAffAreaData.FixedRows) then
    begin
      GrdAffAreaData.MoveRowUp(GrdAffAreaData.Row);
      if (GrdAffAreaData.Row > 0) then
        GrdAffAreaData.Row := GrdAffAreaData.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAfforestationDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TRunOffAfforestationDlg.BtnRowDownClick';
begin
  try
    if (GrdAffAreaData.Row >= GrdAffAreaData.FixedRows) AND (GrdAffAreaData.Row < GrdAffAreaData.RowCount - 1) then
    begin
      GrdAffAreaData.MoveRowDown(GrdAffAreaData.Row);
      if (GrdAffAreaData.Row < GrdAffAreaData.RowCount) then
        GrdAffAreaData.Row := GrdAffAreaData.Row + 1;
    end;    
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
