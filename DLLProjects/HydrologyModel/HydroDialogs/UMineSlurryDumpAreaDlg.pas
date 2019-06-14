unit UMineSlurryDumpAreaDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentMineSlurryDumpArea,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TMineSlurryDumpAreaDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdSlurryDumpArea        : TWRMFGrid;
    LblSlurryDumpInterpolationOption: TLabel;
    CbxSlurryDumpInterpolationOption: TWRMFComboBox;
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
    procedure GrdSlurryDumpAreaDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdSlurryDumpAreaExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineSlurryDumpArea;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineSlurryDumpAreaDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineSlurryDumpArea.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdSlurryDumpArea.Cells[0, 0] := 'Year';
    GrdSlurryDumpArea.Cells[1, 0] := 'Growth';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.SetControls;
const OPNAME = 'TMineSlurryDumpAreaDlg.SetControls';
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

procedure TMineSlurryDumpAreaDlg.SetIndicators;
const OPNAME = 'TMineSlurryDumpAreaDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.SetAllValid;
const OPNAME = 'TMineSlurryDumpAreaDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpAreaDlg.LoadXMLData: boolean;
const OPNAME = 'TMineSlurryDumpAreaDlg.LoadXMLData';
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
    CbxSlurryDumpInterpolationOption.Items.Clear;
    CbxSlurryDumpInterpolationOption.Items.AddObject('Linear', pointer(1));
    CbxSlurryDumpInterpolationOption.Items.AddObject('Exponential', pointer(2));
    CbxSlurryDumpInterpolationOption.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpAreaDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineSlurryDumpAreaDlg.StoreXMLData';
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

procedure TMineSlurryDumpAreaDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.BtnApplyClick';
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

procedure TMineSlurryDumpAreaDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpAreaDlg.DoValidation (AContext      : TValidationContext;
                                              APropertyName : String;
                                              AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineSlurryDumpAreaDlg.DoValidation';
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

procedure TMineSlurryDumpAreaDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.ParamChangeIndicatorClicked';
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

procedure TMineSlurryDumpAreaDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.MetaDataIndicatorClicked';
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

procedure TMineSlurryDumpAreaDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineSlurryDumpAreaDlg.GridParamChangeIndicatorClicked';
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

procedure TMineSlurryDumpAreaDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineSlurryDumpAreaDlg.GridMetaDataIndicatorClicked';
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

procedure TMineSlurryDumpAreaDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.BtnAddRowClick';
begin
  try
    GrdSlurryDumpArea.InsertRow(GrdSlurryDumpArea.Row);
    if (GrdSlurryDumpArea.RowCount > GrdSlurryDumpArea.CellsInfo.NoOfHeadingRows) then
      GrdSlurryDumpArea.FixedRows := GrdSlurryDumpArea.CellsInfo.NoOfHeadingRows;
    GrdSlurryDumpArea.Enabled := (GrdSlurryDumpArea.RowCount > GrdSlurryDumpArea.CellsInfo.NoOfHeadingRows);
    if (GrdSlurryDumpArea.Enabled) then
      GrdSlurryDumpArea.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.BtnDeleteRowClick';
begin
  try
    if (GrdSlurryDumpArea.Row >= GrdSlurryDumpArea.CellsInfo.NoOfHeadingRows) then
    begin
      GrdSlurryDumpArea.DeleteRow(GrdSlurryDumpArea.Row);
      GrdSlurryDumpArea.Enabled := (GrdSlurryDumpArea.RowCount > GrdSlurryDumpArea.CellsInfo.NoOfHeadingRows);
      if (GrdSlurryDumpArea.Enabled) then
        GrdSlurryDumpArea.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.BtnRowUpClick';
begin
  try
    if (GrdSlurryDumpArea.Row > GrdSlurryDumpArea.FixedRows) then
    begin
      GrdSlurryDumpArea.MoveRowUp(GrdSlurryDumpArea.Row);
      if (GrdSlurryDumpArea.Row > 0) then
        GrdSlurryDumpArea.Row := GrdSlurryDumpArea.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.BtnRowDownClick';
begin
  try
    if (GrdSlurryDumpArea.Row >= GrdSlurryDumpArea.FixedRows) AND (GrdSlurryDumpArea.Row < GrdSlurryDumpArea.RowCount - 1) then
    begin
      GrdSlurryDumpArea.MoveRowDown(GrdSlurryDumpArea.Row);
      if (GrdSlurryDumpArea.Row < GrdSlurryDumpArea.RowCount) then
        GrdSlurryDumpArea.Row := GrdSlurryDumpArea.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpAreaDlg.GrdSlurryDumpAreaDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineSlurryDumpAreaDlg.GrdSlurryDumpAreaDataCellExit';
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

procedure TMineSlurryDumpAreaDlg.GrdSlurryDumpAreaExit(Sender: TObject);
const OPNAME = 'TMineSlurryDumpAreaDlg.GrdSlurryDumpAreaExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdSlurryDumpAreaDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
