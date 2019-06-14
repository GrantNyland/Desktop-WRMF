unit UMineOpencastPitEvaporationDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UXMLAgent,
  UControlIterator,
  HydrologyCom_TLB,
  UXMLAgentMineOpencastPitEvaporation,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TMineOpencastPitEvaporationDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdEvaporation           : TWRMFGrid;
    LblEvaporationInterpolationType: TLabel;
    CbxEvaporationInterpolationType: TWRMFComboBox;
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
    procedure GrdEvaporationDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdEvaporationExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineOpencastPitEvaporation;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineOpencastPitEvaporationDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineOpencastPitEvaporation.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdEvaporation.Cells[0, 0] := 'Year';
    GrdEvaporation.Cells[1, 0] := 'Growth';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.SetControls;
const OPNAME = 'TMineOpencastPitEvaporationDlg.SetControls';
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

procedure TMineOpencastPitEvaporationDlg.SetIndicators;
const OPNAME = 'TMineOpencastPitEvaporationDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.SetAllValid;
const OPNAME = 'TMineOpencastPitEvaporationDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitEvaporationDlg.LoadXMLData: boolean;
const OPNAME = 'TMineOpencastPitEvaporationDlg.LoadXMLData';
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
    CbxEvaporationInterpolationType.Items.Clear;
    CbxEvaporationInterpolationType.Items.AddObject('Linear', pointer(1));
    CbxEvaporationInterpolationType.Items.AddObject('Exponential', pointer(2));
    CbxEvaporationInterpolationType.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitEvaporationDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineOpencastPitEvaporationDlg.StoreXMLData';
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

procedure TMineOpencastPitEvaporationDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.BtnApplyClick';
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

procedure TMineOpencastPitEvaporationDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitEvaporationDlg.DoValidation (AContext      : TValidationContext;
                                                      APropertyName : String;
                                                      AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineOpencastPitEvaporationDlg.DoValidation';
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

procedure TMineOpencastPitEvaporationDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.ParamChangeIndicatorClicked';
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

procedure TMineOpencastPitEvaporationDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.MetaDataIndicatorClicked';
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

procedure TMineOpencastPitEvaporationDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitEvaporationDlg.GridParamChangeIndicatorClicked';
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

procedure TMineOpencastPitEvaporationDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitEvaporationDlg.GridMetaDataIndicatorClicked';
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

procedure TMineOpencastPitEvaporationDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.BtnAddRowClick';
begin
  try
    GrdEvaporation.InsertRow(GrdEvaporation.Row);
    if (GrdEvaporation.RowCount > GrdEvaporation.CellsInfo.NoOfHeadingRows) then
      GrdEvaporation.FixedRows := GrdEvaporation.CellsInfo.NoOfHeadingRows;
    GrdEvaporation.Enabled := (GrdEvaporation.RowCount > GrdEvaporation.CellsInfo.NoOfHeadingRows);
    if (GrdEvaporation.Enabled) then
      GrdEvaporation.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.BtnDeleteRowClick';
begin
  try
    if (GrdEvaporation.Row >= GrdEvaporation.CellsInfo.NoOfHeadingRows) then
    begin
      GrdEvaporation.DeleteRow(GrdEvaporation.Row);
      GrdEvaporation.Enabled := (GrdEvaporation.RowCount > GrdEvaporation.CellsInfo.NoOfHeadingRows);
      if (GrdEvaporation.Enabled) then
        GrdEvaporation.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.BtnRowUpClick';
begin
  try
    if (GrdEvaporation.Row > GrdEvaporation.FixedRows) then
    begin
      GrdEvaporation.MoveRowUp(GrdEvaporation.Row);
      if (GrdEvaporation.Row > 0) then
        GrdEvaporation.Row := GrdEvaporation.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.BtnRowDownClick';
begin
  try
    if (GrdEvaporation.Row >= GrdEvaporation.FixedRows) AND (GrdEvaporation.Row < GrdEvaporation.RowCount - 1) then
    begin
      GrdEvaporation.MoveRowDown(GrdEvaporation.Row);
      if (GrdEvaporation.Row < GrdEvaporation.RowCount) then
        GrdEvaporation.Row := GrdEvaporation.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitEvaporationDlg.GrdEvaporationDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineOpencastPitEvaporationDlg.GrdEvaporationDataCellExit';
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

procedure TMineOpencastPitEvaporationDlg.GrdEvaporationExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitEvaporationDlg.GrdEvaporationExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdEvaporationDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
