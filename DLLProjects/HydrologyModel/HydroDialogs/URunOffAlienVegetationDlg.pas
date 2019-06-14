unit URunOffAlienVegetationDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentRunOffAlienVegetation,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TRunOffAlienVegetationDlg = class(TForm)
    PnlBottom                        : TPanel;
    BtnApply                         : TButton;
    BtnReset                         : TButton;
    FXMLDocumentIn                   : TXMLDocument;
    FXMLDocumentOut                  : TXMLDocument;
    ScrClient                        : TScrollBox;
    LblAlienVegAlgorithm             : TLabel;
    LblRiparianVegetationArea        : TLabel;
    LblTallTreeAreaPercentage        : TLabel;
    LblTallTreeAge                   : TLabel;
    LblMediumTreeAreaPercentage      : TLabel;
    LblMediumTreeAge                 : TLabel;
    LblTallSchrubAreaPercentage      : TLabel;
    LblTallSchrubAge                 : TLabel;
    LblOptimalAreaPercentage         : TLabel;
    EdtOptimalAreaPercentage         : TWRMFEdit;
    EdtTallSchrubAge                 : TWRMFEdit;
    EdtTallSchrubAreaPercentage      : TWRMFEdit;
    EdtMediumTreeAge                 : TWRMFEdit;
    EdtMediumTreeAreaPercentage      : TWRMFEdit;
    EdtTallTreeAge                   : TWRMFEdit;
    EdtTallTreeAreaPercentage        : TWRMFEdit;
    EdtRiparianVegetationArea        : TWRMFEdit;
    CbxAlienVegetationAlgorithm      : TWRMFComboBox;
    GrdAlienVegAreaData              : TWRMFGrid;
    BtnAddRow                        : TSpeedButton;
    BtnDeleteRow                     : TSpeedButton;
    BtnRowUp                         : TSpeedButton;
    BtnRowDown                       : TSpeedButton;
    procedure FormShow(Sender        : TObject);
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
    procedure GrdAlienVegAreaDataDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdAlienVegAreaDataExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentRunOffAlienVegetation;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TRunOffAlienVegetationDlg.FormCreate(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentRunOffAlienVegetation.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.FormShow(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdAlienVegAreaData.Cells[0, 0] := 'Year';
    GrdAlienVegAreaData.Cells[1, 0] := 'Area';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.SetControls;
const OPNAME = 'TRunOffAlienVegetationDlg.SetControls';
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

procedure TRunOffAlienVegetationDlg.SetIndicators;
const OPNAME = 'TRunOffAlienVegetationDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.SetAllValid;
const OPNAME = 'TRunOffAlienVegetationDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAlienVegetationDlg.LoadXMLData: boolean;
const OPNAME = 'TRunOffAlienVegetationDlg.LoadXMLData';
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
    CbxAlienVegetationAlgorithm.Items.Clear;
    CbxAlienVegetationAlgorithm.Items.AddObject('None', pointer(0));
    CbxAlienVegetationAlgorithm.Items.AddObject('CSIR', pointer(1));
    CbxAlienVegetationAlgorithm.Items.AddObject('Riparian', pointer(2));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAlienVegetationDlg.StoreXMLData : Boolean;
const OPNAME = 'TRunOffAlienVegetationDlg.StoreXMLData';
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

procedure TRunOffAlienVegetationDlg.ControlExit(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.GrdAlienVegAreaDataDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TRunOffAlienVegetationDlg.GrdAlienVegAreaDataDataCellExit';
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

procedure TRunOffAlienVegetationDlg.GrdAlienVegAreaDataExit(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.GrdAlienVegAreaDataExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdAlienVegAreaDataDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.BtnApplyClick';
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

procedure TRunOffAlienVegetationDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffAlienVegetationDlg.DoValidation (AContext      : TValidationContext;
                                                 APropertyName : String;
                                                 AFieldIndex   : String) : Boolean;
const OPNAME = 'TRunOffAlienVegetationDlg.DoValidation';
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

procedure TRunOffAlienVegetationDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.ParamChangeIndicatorClicked';
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

procedure TRunOffAlienVegetationDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.MetaDataIndicatorClicked';
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

procedure TRunOffAlienVegetationDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffAlienVegetationDlg.GridParamChangeIndicatorClicked';
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

procedure TRunOffAlienVegetationDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffAlienVegetationDlg.GridMetaDataIndicatorClicked';
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

procedure TRunOffAlienVegetationDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.BtnAddRowClick';
begin
  try
    GrdAlienVegAreaData.InsertRow(GrdAlienVegAreaData.Row);
    if (GrdAlienVegAreaData.RowCount > GrdAlienVegAreaData.CellsInfo.NoOfHeadingRows) then
      GrdAlienVegAreaData.FixedRows := GrdAlienVegAreaData.CellsInfo.NoOfHeadingRows;
    GrdAlienVegAreaData.Enabled := (GrdAlienVegAreaData.RowCount > GrdAlienVegAreaData.CellsInfo.NoOfHeadingRows);
    if (GrdAlienVegAreaData.Enabled) then
      GrdAlienVegAreaData.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.BtnDeleteRowClick';
begin
  try
    if (GrdAlienVegAreaData.Row >= GrdAlienVegAreaData.CellsInfo.NoOfHeadingRows) then
    begin
      GrdAlienVegAreaData.DeleteRow(GrdAlienVegAreaData.Row);
      GrdAlienVegAreaData.Enabled := (GrdAlienVegAreaData.RowCount > GrdAlienVegAreaData.CellsInfo.NoOfHeadingRows);
      if (GrdAlienVegAreaData.Enabled) then
        GrdAlienVegAreaData.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.BtnRowUpClick';
begin
  try
    if (GrdAlienVegAreaData.Row > GrdAlienVegAreaData.FixedRows) then
    begin
      GrdAlienVegAreaData.MoveRowUp(GrdAlienVegAreaData.Row);
      if (GrdAlienVegAreaData.Row > 0) then
        GrdAlienVegAreaData.Row := GrdAlienVegAreaData.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffAlienVegetationDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TRunOffAlienVegetationDlg.BtnRowDownClick';
begin
  try
    if (GrdAlienVegAreaData.Row >= GrdAlienVegAreaData.FixedRows) AND (GrdAlienVegAreaData.Row < GrdAlienVegAreaData.RowCount - 1) then
    begin
      GrdAlienVegAreaData.MoveRowDown(GrdAlienVegAreaData.Row);
      if (GrdAlienVegAreaData.Row < GrdAlienVegAreaData.RowCount) then
        GrdAlienVegAreaData.Row := GrdAlienVegAreaData.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
