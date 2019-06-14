unit UIrrigationAllocationGrowthDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentIrrigationAllocationGrowth,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TIrrigationAllocationGrowthDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdAllocationGrowth      : TWRMFGrid;
    LblAllocationGrowthInterpolationType: TLabel;
    CbxAllocationGrowthInterpolationType: TWRMFComboBox;
    BtnAddRow                : TSpeedButton;
    BtnDeleteRow             : TSpeedButton;
    BtnRowUp                 : TSpeedButton;
    BtnRowDown               : TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure ControlExit(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure BtnAddRowClick(Sender: TObject);
    procedure BtnDeleteRowClick(Sender: TObject);
    procedure BtnRowUpClick(Sender: TObject);
    procedure BtnRowDownClick(Sender: TObject);
    procedure GrdAllocationGrowthDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdAllocationGrowthExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentIrrigationAllocationGrowth;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TIrrigationAllocationGrowthDlg.FormCreate(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentIrrigationAllocationGrowth.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.FormShow(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdAllocationGrowth.Cells[0, 0] := 'Year';
    GrdAllocationGrowth.Cells[1, 0] := 'Growth';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.SetControls;
const OPNAME = 'TIrrigationAllocationGrowthDlg.SetControls';
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

procedure TIrrigationAllocationGrowthDlg.SetIndicators;
const OPNAME = 'TIrrigationAllocationGrowthDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.SetAllValid;
const OPNAME = 'TIrrigationAllocationGrowthDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAllocationGrowthDlg.LoadXMLData: boolean;
const OPNAME = 'TIrrigationAllocationGrowthDlg.LoadXMLData';
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

    // InterpolationType
    CbxAllocationGrowthInterpolationType.Items.Clear;
    CbxAllocationGrowthInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxAllocationGrowthInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxAllocationGrowthInterpolationType.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAllocationGrowthDlg.StoreXMLData : Boolean;
const OPNAME = 'TIrrigationAllocationGrowthDlg.StoreXMLData';
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

procedure TIrrigationAllocationGrowthDlg.ControlExit(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.BtnApplyClick';
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

procedure TIrrigationAllocationGrowthDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAllocationGrowthDlg.DoValidation (AContext      : TValidationContext;
                                                      APropertyName : String;
                                                      AFieldIndex   : String) : Boolean;
const OPNAME = 'TIrrigationAllocationGrowthDlg.DoValidation';
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

procedure TIrrigationAllocationGrowthDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationAllocationGrowthDlg.GridParamChangeIndicatorClicked';
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

procedure TIrrigationAllocationGrowthDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationAllocationGrowthDlg.GridMetaDataIndicatorClicked';
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

procedure TIrrigationAllocationGrowthDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.BtnAddRowClick';
begin
  try
    GrdAllocationGrowth.InsertRow(GrdAllocationGrowth.Row);
    if (GrdAllocationGrowth.RowCount > GrdAllocationGrowth.CellsInfo.NoOfHeadingRows) then
      GrdAllocationGrowth.FixedRows := GrdAllocationGrowth.CellsInfo.NoOfHeadingRows;
    GrdAllocationGrowth.Enabled := (GrdAllocationGrowth.RowCount > GrdAllocationGrowth.CellsInfo.NoOfHeadingRows);
    if (GrdAllocationGrowth.Enabled) then
      GrdAllocationGrowth.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.BtnDeleteRowClick';
begin
  try
    if (GrdAllocationGrowth.Row >= GrdAllocationGrowth.CellsInfo.NoOfHeadingRows) then
    begin
      GrdAllocationGrowth.DeleteRow(GrdAllocationGrowth.Row);
      GrdAllocationGrowth.Enabled := (GrdAllocationGrowth.RowCount > GrdAllocationGrowth.CellsInfo.NoOfHeadingRows);
      if (GrdAllocationGrowth.Enabled) then
        GrdAllocationGrowth.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.BtnRowUpClick';
begin
  try
    if (GrdAllocationGrowth.Row > GrdAllocationGrowth.FixedRows) then
    begin
      GrdAllocationGrowth.MoveRowUp(GrdAllocationGrowth.Row);
      if (GrdAllocationGrowth.Row > 0) then
        GrdAllocationGrowth.Row := GrdAllocationGrowth.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.BtnRowDownClick';
begin
  try
    if (GrdAllocationGrowth.Row >= GrdAllocationGrowth.FixedRows) AND (GrdAllocationGrowth.Row < GrdAllocationGrowth.RowCount - 1) then
    begin
      GrdAllocationGrowth.MoveRowDown(GrdAllocationGrowth.Row);
      if (GrdAllocationGrowth.Row < GrdAllocationGrowth.RowCount) then
        GrdAllocationGrowth.Row := GrdAllocationGrowth.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAllocationGrowthDlg.GrdAllocationGrowthDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TIrrigationAllocationGrowthDlg.GrdAllocationGrowthDataCellExit';
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

procedure TIrrigationAllocationGrowthDlg.GrdAllocationGrowthExit(Sender: TObject);
const OPNAME = 'TIrrigationAllocationGrowthDlg.GrdAllocationGrowthExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdAllocationGrowthDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
