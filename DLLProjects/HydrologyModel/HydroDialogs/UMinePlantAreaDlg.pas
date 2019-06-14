unit UMinePlantAreaDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentMinePlantArea,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TMinePlantAreaDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdPlantArea             : TWRMFGrid;
    LblPlantAreaInterpolationType: TLabel;
    CbxPlantAreaInterpolationType: TWRMFComboBox;
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
    procedure GrdPlantAreaDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdPlantAreaExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMinePlantArea;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMinePlantAreaDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMinePlantArea.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.FormShow(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdPlantArea.Cells[0, 0] := 'Year';
    GrdPlantArea.Cells[1, 0] := 'Factor';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.SetControls;
const OPNAME = 'TMinePlantAreaDlg.SetControls';
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

procedure TMinePlantAreaDlg.SetIndicators;
const OPNAME = 'TMinePlantAreaDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.SetAllValid;
const OPNAME = 'TMinePlantAreaDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinePlantAreaDlg.LoadXMLData: boolean;
const OPNAME = 'TMinePlantAreaDlg.LoadXMLData';
var
  LRootNode : IXMLNode;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;

    FIdentifier := LRootNode.ChildNodes['Identifier'].Text;
    FKeyValues  := 'Model='       + QuotedStr(LRootNode.ChildNodes['Model'].Text) +
                   ',NetworkID='  + QuotedStr(LRootNode.ChildNodes['NetworkID'].Text) +
                   ',ModuleType=' + QuotedStr(LRootNode.ChildNodes['ModuleType'].Text) +
                   ',Identifier=' + LRootNode.ChildNodes['Identifier'].Text;

    // InterpolationType
    CbxPlantAreaInterpolationType.Items.Clear;
    CbxPlantAreaInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxPlantAreaInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxPlantAreaInterpolationType.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinePlantAreaDlg.StoreXMLData : Boolean;
const OPNAME = 'TMinePlantAreaDlg.StoreXMLData';
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

procedure TMinePlantAreaDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.BtnApplyClick';
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

procedure TMinePlantAreaDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMinePlantAreaDlg.DoValidation (AContext      : TValidationContext;
                                         APropertyName : String;
                                         AFieldIndex   : String) : Boolean;
const OPNAME = 'TMinePlantAreaDlg.DoValidation';
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

procedure TMinePlantAreaDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.ParamChangeIndicatorClicked';
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

procedure TMinePlantAreaDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.MetaDataIndicatorClicked';
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

procedure TMinePlantAreaDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMinePlantAreaDlg.GridParamChangeIndicatorClicked';
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

procedure TMinePlantAreaDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMinePlantAreaDlg.GridMetaDataIndicatorClicked';
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

procedure TMinePlantAreaDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.BtnAddRowClick';
begin
  try
    GrdPlantArea.InsertRow(GrdPlantArea.Row);
    if (GrdPlantArea.RowCount > GrdPlantArea.CellsInfo.NoOfHeadingRows) then
      GrdPlantArea.FixedRows := GrdPlantArea.CellsInfo.NoOfHeadingRows;
    GrdPlantArea.Enabled := (GrdPlantArea.RowCount > GrdPlantArea.CellsInfo.NoOfHeadingRows);
    if (GrdPlantArea.Enabled) then
      GrdPlantArea.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.BtnDeleteRowClick';
begin
  try
    if (GrdPlantArea.Row >= GrdPlantArea.CellsInfo.NoOfHeadingRows) then
    begin
      GrdPlantArea.DeleteRow(GrdPlantArea.Row);
      GrdPlantArea.Enabled := (GrdPlantArea.RowCount > GrdPlantArea.CellsInfo.NoOfHeadingRows);
      if (GrdPlantArea.Enabled) then
        GrdPlantArea.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.BtnRowUpClick';
begin
  try
    if (GrdPlantArea.Row > GrdPlantArea.FixedRows) then
    begin
      GrdPlantArea.MoveRowUp(GrdPlantArea.Row);
      if (GrdPlantArea.Row > 0) then
        GrdPlantArea.Row := GrdPlantArea.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.BtnRowDownClick';
begin
  try
    if (GrdPlantArea.Row >= GrdPlantArea.FixedRows) AND (GrdPlantArea.Row < GrdPlantArea.RowCount - 1) then
    begin
      GrdPlantArea.MoveRowDown(GrdPlantArea.Row);
      if (GrdPlantArea.Row < GrdPlantArea.RowCount) then
        GrdPlantArea.Row := GrdPlantArea.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMinePlantAreaDlg.GrdPlantAreaDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMinePlantAreaDlg.GrdPlantAreaDataCellExit';
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

procedure TMinePlantAreaDlg.GrdPlantAreaExit(Sender: TObject);
const OPNAME = 'TMinePlantAreaDlg.GrdPlantAreaExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdPlantAreaDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
