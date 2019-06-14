unit UIrrigationReturnFlowDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentIrrigationReturnFlow,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TIrrigationReturnFlowDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdReturnFlow            : TWRMFGrid;
    LblReturnFlowInterpolationType: TLabel;
    CbxReturnFlowInterpolationType: TWRMFComboBox;
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
    procedure ParamChangeIndicatorClicked (Sender: TObject);
    procedure MetaDataIndicatorClicked (Sender: TObject);
    procedure GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure BtnAddRowClick(Sender: TObject);
    procedure BtnDeleteRowClick(Sender: TObject);
    procedure BtnRowUpClick(Sender: TObject);
    procedure BtnRowDownClick(Sender: TObject);
    procedure GrdReturnFlowDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdReturnFlowExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentIrrigationReturnFlow;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TIrrigationReturnFlowDlg.FormCreate(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentIrrigationReturnFlow.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.FormShow(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdReturnFlow.Cells[0, 0] := 'Year';
    GrdReturnFlow.Cells[1, 0] := 'Data';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.SetControls;
const OPNAME = 'TIrrigationReturnFlowDlg.SetControls';
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

procedure TIrrigationReturnFlowDlg.SetIndicators;
const OPNAME = 'TIrrigationReturnFlowDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.SetAllValid;
const OPNAME = 'TIrrigationReturnFlowDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationReturnFlowDlg.LoadXMLData: boolean;
const OPNAME = 'TIrrigationReturnFlowDlg.LoadXMLData';
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
    CbxReturnFlowInterpolationType.Items.Clear;
    CbxReturnFlowInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxReturnFlowInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxReturnFlowInterpolationType.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationReturnFlowDlg.StoreXMLData : Boolean;
const OPNAME = 'TIrrigationReturnFlowDlg.StoreXMLData';
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

procedure TIrrigationReturnFlowDlg.ControlExit(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.BtnApplyClick';
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

procedure TIrrigationReturnFlowDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationReturnFlowDlg.DoValidation (AContext      : TValidationContext;
                                                APropertyName : String;
                                                AFieldIndex   : String) : Boolean;
const OPNAME = 'TIrrigationReturnFlowDlg.DoValidation';
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

procedure TIrrigationReturnFlowDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.ParamChangeIndicatorClicked';
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

procedure TIrrigationReturnFlowDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.MetaDataIndicatorClicked';
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

procedure TIrrigationReturnFlowDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationReturnFlowDlg.GridParamChangeIndicatorClicked';
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

procedure TIrrigationReturnFlowDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationReturnFlowDlg.GridMetaDataIndicatorClicked';
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

procedure TIrrigationReturnFlowDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.BtnAddRowClick';
begin
  try
    GrdReturnFlow.InsertRow(GrdReturnFlow.Row);
    if (GrdReturnFlow.RowCount > GrdReturnFlow.CellsInfo.NoOfHeadingRows) then
      GrdReturnFlow.FixedRows := GrdReturnFlow.CellsInfo.NoOfHeadingRows;
    GrdReturnFlow.Enabled := (GrdReturnFlow.RowCount > GrdReturnFlow.CellsInfo.NoOfHeadingRows);
    if (GrdReturnFlow.Enabled) then
      GrdReturnFlow.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.BtnDeleteRowClick';
begin
  try
    if (GrdReturnFlow.Row >= GrdReturnFlow.CellsInfo.NoOfHeadingRows) then
    begin
      GrdReturnFlow.DeleteRow(GrdReturnFlow.Row);
      GrdReturnFlow.Enabled := (GrdReturnFlow.RowCount > GrdReturnFlow.CellsInfo.NoOfHeadingRows);
      if (GrdReturnFlow.Enabled) then
        GrdReturnFlow.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.BtnRowUpClick';
begin
  try
    if (GrdReturnFlow.Row > GrdReturnFlow.FixedRows) then
    begin
      GrdReturnFlow.MoveRowUp(GrdReturnFlow.Row);
      if (GrdReturnFlow.Row > 0) then
        GrdReturnFlow.Row := GrdReturnFlow.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.BtnRowDownClick';
begin
  try
    if (GrdReturnFlow.Row >= GrdReturnFlow.FixedRows) AND (GrdReturnFlow.Row < GrdReturnFlow.RowCount - 1) then
    begin
      GrdReturnFlow.MoveRowDown(GrdReturnFlow.Row);
      if (GrdReturnFlow.Row < GrdReturnFlow.RowCount) then
        GrdReturnFlow.Row := GrdReturnFlow.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationReturnFlowDlg.GrdReturnFlowDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TIrrigationReturnFlowDlg.GrdReturnFlowDataCellExit';
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

procedure TIrrigationReturnFlowDlg.GrdReturnFlowExit(Sender: TObject);
const OPNAME = 'TIrrigationReturnFlowDlg.GrdReturnFlowExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdReturnFlowDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
