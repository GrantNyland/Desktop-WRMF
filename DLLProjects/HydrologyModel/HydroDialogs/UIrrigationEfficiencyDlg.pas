unit UIrrigationEfficiencyDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentIrrigationEfficiency,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TIrrigationEfficiencyDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdEfficiency            : TWRMFGrid;
    LblEfficiencyInterpolationType : TLabel;
    CbxEfficiencyInterpolationType : TWRMFComboBox;
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
    procedure GrdEfficiencyDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdEfficiencyExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentIrrigationEfficiency;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TIrrigationEfficiencyDlg.FormCreate(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentIrrigationEfficiency.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.FormShow(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdEfficiency.Cells[0, 0] := 'Year';
    GrdEfficiency.Cells[1, 0] := 'Efficiency';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.SetControls;
const OPNAME = 'TIrrigationEfficiencyDlg.SetControls';
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

procedure TIrrigationEfficiencyDlg.SetIndicators;
const OPNAME = 'TIrrigationEfficiencyDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.SetAllValid;
const OPNAME = 'TIrrigationEfficiencyDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationEfficiencyDlg.LoadXMLData: boolean;
const OPNAME = 'TIrrigationEfficiencyDlg.LoadXMLData';
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
    CbxEfficiencyInterpolationType.Items.Clear;
    CbxEfficiencyInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxEfficiencyInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxEfficiencyInterpolationType.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationEfficiencyDlg.StoreXMLData : Boolean;
const OPNAME = 'TIrrigationEfficiencyDlg.StoreXMLData';
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

procedure TIrrigationEfficiencyDlg.ControlExit(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.BtnApplyClick';
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

procedure TIrrigationEfficiencyDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationEfficiencyDlg.DoValidation (AContext      : TValidationContext;
                                                APropertyName : String;
                                                AFieldIndex   : String) : Boolean;
const OPNAME = 'TIrrigationEfficiencyDlg.DoValidation';
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

procedure TIrrigationEfficiencyDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.ParamChangeIndicatorClicked';
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

procedure TIrrigationEfficiencyDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.MetaDataIndicatorClicked';
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

procedure TIrrigationEfficiencyDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationEfficiencyDlg.GridParamChangeIndicatorClicked';
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

procedure TIrrigationEfficiencyDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationEfficiencyDlg.GridMetaDataIndicatorClicked';
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


procedure TIrrigationEfficiencyDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.BtnAddRowClick';
begin
  try
    GrdEfficiency.InsertRow(GrdEfficiency.Row);
    if (GrdEfficiency.RowCount > GrdEfficiency.CellsInfo.NoOfHeadingRows) then
      GrdEfficiency.FixedRows := GrdEfficiency.CellsInfo.NoOfHeadingRows;
    GrdEfficiency.Enabled := (GrdEfficiency.RowCount > GrdEfficiency.CellsInfo.NoOfHeadingRows);
    if (GrdEfficiency.Enabled) then
      GrdEfficiency.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.BtnDeleteRowClick';
begin
  try
    if (GrdEfficiency.Row >= GrdEfficiency.CellsInfo.NoOfHeadingRows) then
    begin
      GrdEfficiency.DeleteRow(GrdEfficiency.Row);
      GrdEfficiency.Enabled := (GrdEfficiency.RowCount > GrdEfficiency.CellsInfo.NoOfHeadingRows);
      if (GrdEfficiency.Enabled) then
        GrdEfficiency.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.BtnRowUpClick';
begin
  try
    if (GrdEfficiency.Row > GrdEfficiency.FixedRows) then
    begin
      GrdEfficiency.MoveRowUp(GrdEfficiency.Row);
      if (GrdEfficiency.Row > 0) then
        GrdEfficiency.Row := GrdEfficiency.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.BtnRowDownClick';
begin
  try
    if (GrdEfficiency.Row >= GrdEfficiency.FixedRows) AND (GrdEfficiency.Row < GrdEfficiency.RowCount - 1) then
    begin
      GrdEfficiency.MoveRowDown(GrdEfficiency.Row);
      if (GrdEfficiency.Row < GrdEfficiency.RowCount) then
        GrdEfficiency.Row := GrdEfficiency.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationEfficiencyDlg.GrdEfficiencyDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TIrrigationEfficiencyDlg.GrdEfficiencyDataCellExit';
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

procedure TIrrigationEfficiencyDlg.GrdEfficiencyExit(Sender: TObject);
const OPNAME = 'TIrrigationEfficiencyDlg.GrdEfficiencyExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdEfficiencyDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
