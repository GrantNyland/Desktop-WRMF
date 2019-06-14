unit UIrrigationAreaDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentIrrigationArea,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TIrrigationAreaDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdArea                  : TWRMFGrid;
    BtnAddRow                : TSpeedButton;
    BtnDeleteRow             : TSpeedButton;
    BtnRowUp                 : TSpeedButton;
    BtnRowDown               : TSpeedButton;
    LblAreaInterpolationType : TLabel;
    CbxAreaInterpolationType : TWRMFComboBox;
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
    procedure GrdAreaDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdAreaExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentIrrigationArea;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TIrrigationAreaDlg.FormCreate(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentIrrigationArea.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.FormShow(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdArea.Cells[0, 0] := 'Year';
    GrdArea.Cells[1, 0] := 'Area';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.SetControls;
const OPNAME = 'TIrrigationAreaDlg.SetControls';
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

procedure TIrrigationAreaDlg.SetIndicators;
const OPNAME = 'TIrrigationAreaDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.SetAllValid;
const OPNAME = 'TIrrigationAreaDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaDlg.LoadXMLData: boolean;
const OPNAME = 'TIrrigationAreaDlg.LoadXMLData';
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

    // AreaInterpolationType
    CbxAreaInterpolationType.Items.Clear;
    CbxAreaInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxAreaInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxAreaInterpolationType.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaDlg.StoreXMLData : Boolean;
const OPNAME = 'TIrrigationAreaDlg.StoreXMLData';
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

procedure TIrrigationAreaDlg.ControlExit(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.BtnApplyClick';
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

procedure TIrrigationAreaDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationAreaDlg.DoValidation (AContext      : TValidationContext;
                                          APropertyName : String;
                                          AFieldIndex   : String) : Boolean;
const OPNAME = 'TIrrigationAreaDlg.DoValidation';
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

procedure TIrrigationAreaDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationAreaDlg.GridParamChangeIndicatorClicked';
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

procedure TIrrigationAreaDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationAreaDlg.GridMetaDataIndicatorClicked';
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

procedure TIrrigationAreaDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.BtnAddRowClick';
begin
  try
    GrdArea.InsertRow(GrdArea.Row);
    if (GrdArea.RowCount > GrdArea.CellsInfo.NoOfHeadingRows) then
      GrdArea.FixedRows := GrdArea.CellsInfo.NoOfHeadingRows;
    GrdArea.Enabled := (GrdArea.RowCount > GrdArea.CellsInfo.NoOfHeadingRows);
    if (GrdArea.Enabled) then
      GrdArea.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.BtnDeleteRowClick';
begin
  try
    if (GrdArea.Row >= GrdArea.CellsInfo.NoOfHeadingRows) then
    begin
      GrdArea.DeleteRow(GrdArea.Row);
      GrdArea.Enabled := (GrdArea.RowCount > GrdArea.CellsInfo.NoOfHeadingRows);
      if (GrdArea.Enabled) then
        GrdArea.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.BtnRowUpClick';
begin
  try
    if (GrdArea.Row > GrdArea.FixedRows) then
    begin
      GrdArea.MoveRowUp(GrdArea.Row);
      if (GrdArea.Row > 0) then
        GrdArea.Row := GrdArea.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.BtnRowDownClick';
begin
  try
    if (GrdArea.Row >= GrdArea.FixedRows) AND (GrdArea.Row < GrdArea.RowCount - 1) then
    begin
      GrdArea.MoveRowDown(GrdArea.Row);
      if (GrdArea.Row < GrdArea.RowCount) then
        GrdArea.Row := GrdArea.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationAreaDlg.GrdAreaDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TIrrigationAreaDlg.GrdAreaDataCellExit';
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

procedure TIrrigationAreaDlg.GrdAreaExit(Sender: TObject);
const OPNAME = 'TIrrigationAreaDlg.GrdAreaExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdAreaDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
