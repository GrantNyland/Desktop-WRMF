unit UIrrigationFactorsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentIrrigationFactors,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TIrrigationFactorsDlg = class(TForm)
    PnlBottom        : TPanel;
    BtnApply         : TButton;
    BtnReset         : TButton;
    FXMLDocumentIn   : TXMLDocument;
    FXMLDocumentOut  : TXMLDocument;
    ScrClient        : TScrollBox;
    GrdFactors       : TWRMFGrid;
    procedure FormShow(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GrdFactorsDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdFactorsExit(Sender: TObject);
    procedure GrdFactorsMetaDataClick(Sender: TObject; Col, Row: Integer);
    procedure GrdFactorsParamChangeClick(Sender: TObject; Col,
      Row: Integer);
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
    FXMLAgent         : TXMLAgentIrrigationFactors;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TIrrigationFactorsDlg.FormCreate(Sender: TObject);
const OPNAME = 'TIrrigationFactorsDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentIrrigationFactors.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationFactorsDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TIrrigationFactorsDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationFactorsDlg.FormShow(Sender: TObject);
const OPNAME = 'TIrrigationFactorsDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdFactors.Cells[0, 0] := 'Month';
    GrdFactors.Cells[1, 0] := 'PIndex factors';
    GrdFactors.Cells[2, 0] := 'Rainfall factors';
    GrdFactors.Cells[3, 0] := 'Crop factors';
    GrdFactors.Cells[4, 0] := 'A-pan factors';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationFactorsDlg.SetControls;
const OPNAME = 'TIrrigationFactorsDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnApply.Enabled  := FMayChangeNetwork;
    BtnReset.Enabled  := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationFactorsDlg.SetIndicators;
const OPNAME = 'TIrrigationFactorsDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationFactorsDlg.SetAllValid;
const OPNAME = 'TIrrigationFactorsDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationFactorsDlg.LoadXMLData: boolean;
const OPNAME = 'TIrrigationFactorsDlg.LoadXMLData';
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

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationFactorsDlg.StoreXMLData : Boolean;
const OPNAME = 'TIrrigationFactorsDlg.StoreXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode     := FXMLDocumentOut.DocumentElement;
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result := FControlIterator.StoreXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationFactorsDlg.GrdFactorsDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TIrrigationFactorsDlg.GrdFactorsDataCellExit';
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

procedure TIrrigationFactorsDlg.GrdFactorsExit(Sender: TObject);
const OPNAME = 'TIrrigationFactorsDlg.GrdFactorsExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdFactorsDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationFactorsDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TIrrigationFactorsDlg.BtnApplyClick';
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

procedure TIrrigationFactorsDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TIrrigationFactorsDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationFactorsDlg.DoValidation (AContext      : TValidationContext;
                                             APropertyName : String;
                                             AFieldIndex   : String) : Boolean;
const OPNAME = 'TIrrigationFactorsDlg.DoValidation';
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

procedure TIrrigationFactorsDlg.GrdFactorsMetaDataClick(Sender: TObject; Col, Row: Integer);
const OPNAME = 'TIrrigationFactorsDlg.GrdFactorsMetaDataClick';
{var
  LGrid     : TWRMFGrid;
  LCellInfo : TCellInfo;}
begin
  try
{    if (Sender is TWRMFGrid) then
    begin
      LGrid := TWRMFGrid(Sender);
      LCellInfo := LGrid.CellInfo[Row, Col];
      if (LCellInfo.HasMetaData) then
        ShowMessage ('SHOW Meta Data for ' + LCellInfo.PropertyName + ' (' + IntToStr(Row) + ') ' + FKeyValues)
      else
        ShowMessage ('NEW Meta Data for ' + LCellInfo.PropertyName + ' (' + IntToStr(Row) + ') ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationFactorsDlg.GrdFactorsParamChangeClick(Sender: TObject; Col, Row: Integer);
const OPNAME = 'TIrrigationFactorsDlg.GrdFactorsParamChangeClick';
{var
  LGrid     : TWRMFGrid;
  LCellInfo : TCellInfo;}
begin
  try
{    if (Sender is TWRMFGrid) then
    begin
      LGrid := TWRMFGrid(Sender);
      LCellInfo := LGrid.CellInfo[Row, Col];
      if (LCellInfo.HasParamChange) then
        ShowMessage ('SHOW Param Changes for ' + LCellInfo.PropertyName + ' (' + IntToStr(Row) + ') ' + FKeyValues)
      else
        ShowMessage ('NEW Param Changes for ' + LCellInfo.PropertyName + ' (' + IntToStr(Row) + ') ' + FKeyValues);
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
