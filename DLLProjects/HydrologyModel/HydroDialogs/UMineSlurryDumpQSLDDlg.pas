unit UMineSlurryDumpQSLDDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UXMLAgent,
  UControlIterator,
  HydrologyCom_TLB,
  UXMLAgentMineSlurryDumpQSLD,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TMineSlurryDumpQSLDDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdSlurryDumpQSLD        : TWRMFGrid;
    LblStdDevSlurryDump      : TLabel;
    EdtStdDevSlurryDump      : TWRMFEdit;
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
    procedure GrdSlurryDumpQSLDDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdSlurryDumpQSLDExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineSlurryDumpQSLD;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineSlurryDumpQSLDDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineSlurryDumpQSLD.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdSlurryDumpQSLD.Cells[0, 0] := 'Flow';
    GrdSlurryDumpQSLD.Cells[1, 0] := 'Load';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.SetControls;
const OPNAME = 'TMineSlurryDumpQSLDDlg.SetControls';
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

procedure TMineSlurryDumpQSLDDlg.SetIndicators;
const OPNAME = 'TMineSlurryDumpQSLDDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.SetAllValid;
const OPNAME = 'TMineSlurryDumpQSLDDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpQSLDDlg.LoadXMLData: boolean;
const OPNAME = 'TMineSlurryDumpQSLDDlg.LoadXMLData';
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

function TMineSlurryDumpQSLDDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineSlurryDumpQSLDDlg.StoreXMLData';
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

procedure TMineSlurryDumpQSLDDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.BtnApplyClick';
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

procedure TMineSlurryDumpQSLDDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineSlurryDumpQSLDDlg.DoValidation (AContext      : TValidationContext;
                                              APropertyName : String;
                                              AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineSlurryDumpQSLDDlg.DoValidation';
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

procedure TMineSlurryDumpQSLDDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.ParamChangeIndicatorClicked';
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

procedure TMineSlurryDumpQSLDDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.MetaDataIndicatorClicked';
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

procedure TMineSlurryDumpQSLDDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineSlurryDumpQSLDDlg.GridParamChangeIndicatorClicked';
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

procedure TMineSlurryDumpQSLDDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineSlurryDumpQSLDDlg.GridMetaDataIndicatorClicked';
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

procedure TMineSlurryDumpQSLDDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.BtnAddRowClick';
begin
  try
    GrdSlurryDumpQSLD.InsertRow(GrdSlurryDumpQSLD.Row);
    if (GrdSlurryDumpQSLD.RowCount > GrdSlurryDumpQSLD.CellsInfo.NoOfHeadingRows) then
      GrdSlurryDumpQSLD.FixedRows := GrdSlurryDumpQSLD.CellsInfo.NoOfHeadingRows;
    GrdSlurryDumpQSLD.Enabled := (GrdSlurryDumpQSLD.RowCount > GrdSlurryDumpQSLD.CellsInfo.NoOfHeadingRows);
    if (GrdSlurryDumpQSLD.Enabled) then
      GrdSlurryDumpQSLD.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.BtnDeleteRowClick';
begin
  try
    if (GrdSlurryDumpQSLD.Row >= GrdSlurryDumpQSLD.CellsInfo.NoOfHeadingRows) then
    begin
      GrdSlurryDumpQSLD.DeleteRow(GrdSlurryDumpQSLD.Row);
      GrdSlurryDumpQSLD.Enabled := (GrdSlurryDumpQSLD.RowCount > GrdSlurryDumpQSLD.CellsInfo.NoOfHeadingRows);
      if (GrdSlurryDumpQSLD.Enabled) then
        GrdSlurryDumpQSLD.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.BtnRowUpClick';
begin
  try
    if (GrdSlurryDumpQSLD.Row > GrdSlurryDumpQSLD.FixedRows) then
    begin
      GrdSlurryDumpQSLD.MoveRowUp(GrdSlurryDumpQSLD.Row);
      if (GrdSlurryDumpQSLD.Row > 0) then
        GrdSlurryDumpQSLD.Row := GrdSlurryDumpQSLD.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.BtnRowDownClick';
begin
  try
    if (GrdSlurryDumpQSLD.Row >= GrdSlurryDumpQSLD.FixedRows) AND (GrdSlurryDumpQSLD.Row < GrdSlurryDumpQSLD.RowCount - 1) then
    begin
      GrdSlurryDumpQSLD.MoveRowDown(GrdSlurryDumpQSLD.Row);
      if (GrdSlurryDumpQSLD.Row < GrdSlurryDumpQSLD.RowCount) then
        GrdSlurryDumpQSLD.Row := GrdSlurryDumpQSLD.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineSlurryDumpQSLDDlg.GrdSlurryDumpQSLDDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineSlurryDumpQSLDDlg.GrdSlurryDumpQSLDDataCellExit';
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

procedure TMineSlurryDumpQSLDDlg.GrdSlurryDumpQSLDExit(Sender: TObject);
const OPNAME = 'TMineSlurryDumpQSLDDlg.GrdSlurryDumpQSLDExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdSlurryDumpQSLDDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
