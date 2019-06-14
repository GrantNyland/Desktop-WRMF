unit UReservoirVolumeAreaDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentReservoirVolumeArea,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TReservoirVolumeAreaDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    BtnAddRow                : TSpeedButton;
    BtnDeleteRow             : TSpeedButton;
    BtnRowUp                 : TSpeedButton;
    BtnRowDown               : TSpeedButton;
    GrdVolumeArea            : TWRMFGrid;
    procedure FormShow(Sender: TObject);
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
    procedure GrdVolumeAreaDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdVolumeAreaExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentReservoirVolumeArea;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TReservoirVolumeAreaDlg.FormCreate(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentReservoirVolumeArea.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.FormShow(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdVolumeArea.Cells[0, 0] := 'Year';
    GrdVolumeArea.Cells[1, 0] := 'FullSupplyVolume';
    GrdVolumeArea.Cells[2, 0] := 'SurfaceArea';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.SetControls;
const OPNAME = 'TReservoirVolumeAreaDlg.SetControls';
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

procedure TReservoirVolumeAreaDlg.SetAllValid;
const OPNAME = 'TReservoirVolumeAreaDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.SetIndicators;
const OPNAME = 'TReservoirVolumeAreaDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirVolumeAreaDlg.LoadXMLData: boolean;
const OPNAME = 'TReservoirVolumeAreaDlg.LoadXMLData';
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

function TReservoirVolumeAreaDlg.StoreXMLData : Boolean;
const OPNAME = 'TReservoirVolumeAreaDlg.StoreXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LRootNode.ChildNodes['Identifier'].Text := FIdentifier;
    Result    := FControlIterator.StoreXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.BtnApplyClick';
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

procedure TReservoirVolumeAreaDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TReservoirVolumeAreaDlg.DoValidation (AContext      : TValidationContext;
                                               APropertyName : String;
                                               AFieldIndex   : String) : Boolean;
const OPNAME = 'TReservoirVolumeAreaDlg.DoValidation';
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

procedure TReservoirVolumeAreaDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.ParamChangeIndicatorClicked';
//var
//  LControl : TWRMFControl;
begin
  try
{
    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if (LControl.HasParamChange) then
        ShowMessage ('SHOW Param Changes for ' + LControl.PropertyName + ' ' + FKeyValues)
      else
        ShowMessage ('NEW Param Changes for ' + LControl.PropertyName + ' ' + FKeyValues);
    end;
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.MetaDataIndicatorClicked';
//var
//  LControl : TWRMFControl;
begin
  try
{
    if (Sender is TWRMFControl) then
    begin
      LControl := TWRMFControl(Sender);
      if ((Sender is TWRMFControl) AND (TWRMFControl(Sender).HasMetaData)) then
        ShowMessage ('SHOW Meta Data for ' + LControl.PropertyName + ' ' + FKeyValues)
      else
        ShowMessage ('NEW Meta Data for ' + LControl.PropertyName + ' ' + FKeyValues);
    end;
}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirVolumeAreaDlg.GridParamChangeIndicatorClicked';
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

procedure TReservoirVolumeAreaDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TReservoirVolumeAreaDlg.GridMetaDataIndicatorClicked';
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

procedure TReservoirVolumeAreaDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.BtnAddRowClick';
begin
  try
    GrdVolumeArea.InsertRow(GrdVolumeArea.Row);
    if (GrdVolumeArea.RowCount > GrdVolumeArea.CellsInfo.NoOfHeadingRows) then
      GrdVolumeArea.FixedRows := GrdVolumeArea.CellsInfo.NoOfHeadingRows;
    GrdVolumeArea.Enabled := (GrdVolumeArea.RowCount > GrdVolumeArea.CellsInfo.NoOfHeadingRows);
    if (GrdVolumeArea.Enabled) then
      GrdVolumeArea.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.BtnDeleteRowClick';
begin
  try
    if (GrdVolumeArea.Row >= GrdVolumeArea.CellsInfo.NoOfHeadingRows) then
    begin
      GrdVolumeArea.DeleteRow(GrdVolumeArea.Row);
      GrdVolumeArea.Enabled := (GrdVolumeArea.RowCount > GrdVolumeArea.CellsInfo.NoOfHeadingRows);
      if (GrdVolumeArea.Enabled) then
        GrdVolumeArea.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.BtnRowUpClick';
begin
  try
    if (GrdVolumeArea.Row > GrdVolumeArea.FixedRows) then
    begin
      GrdVolumeArea.MoveRowUp(GrdVolumeArea.Row);
      if (GrdVolumeArea.Row > 0) then
        GrdVolumeArea.Row := GrdVolumeArea.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.BtnRowDownClick';
begin
  try
    if (GrdVolumeArea.Row >= GrdVolumeArea.FixedRows) AND (GrdVolumeArea.Row < GrdVolumeArea.RowCount - 1) then
    begin
      GrdVolumeArea.MoveRowDown(GrdVolumeArea.Row);
      if (GrdVolumeArea.Row < GrdVolumeArea.RowCount) then
        GrdVolumeArea.Row := GrdVolumeArea.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TReservoirVolumeAreaDlg.GrdVolumeAreaDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TReservoirVolumeAreaDlg.GrdVolumeAreaDataCellExit';
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

procedure TReservoirVolumeAreaDlg.GrdVolumeAreaExit(Sender: TObject);
const OPNAME = 'TReservoirVolumeAreaDlg.GrdVolumeAreaExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdVolumeAreaDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
