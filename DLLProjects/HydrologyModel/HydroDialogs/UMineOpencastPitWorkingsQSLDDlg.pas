unit UMineOpencastPitWorkingsQSLDDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UXMLAgent,
  UControlIterator,
  HydrologyCom_TLB,
  UXMLAgentMineOpencastPitWorkingsQSLD,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TMineOpencastPitWorkingsQSLDDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdWorkingsQSLD          : TWRMFGrid;
    LblStdDevWorkingsArea    : TLabel;
    EdtStdDevWorkingsArea    : TWRMFEdit;
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
    procedure GrdWorkingsQSLDDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdWorkingsQSLDExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineOpencastPitWorkingsQSLD;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineOpencastPitWorkingsQSLDDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineOpencastPitWorkingsQSLD.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdWorkingsQSLD.Cells[0, 0] := 'Flow';
    GrdWorkingsQSLD.Cells[1, 0] := 'Load';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.SetControls;
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.SetControls';
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

procedure TMineOpencastPitWorkingsQSLDDlg.SetIndicators;
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.SetAllValid;
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitWorkingsQSLDDlg.LoadXMLData: boolean;
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.LoadXMLData';
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

function TMineOpencastPitWorkingsQSLDDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.StoreXMLData';
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

procedure TMineOpencastPitWorkingsQSLDDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.BtnApplyClick';
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

procedure TMineOpencastPitWorkingsQSLDDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineOpencastPitWorkingsQSLDDlg.DoValidation (AContext      : TValidationContext;
                                                       APropertyName : String;
                                                       AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.DoValidation';
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

procedure TMineOpencastPitWorkingsQSLDDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.ParamChangeIndicatorClicked';
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

procedure TMineOpencastPitWorkingsQSLDDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.MetaDataIndicatorClicked';
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

procedure TMineOpencastPitWorkingsQSLDDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.GridParamChangeIndicatorClicked';
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

procedure TMineOpencastPitWorkingsQSLDDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.GridMetaDataIndicatorClicked';
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

procedure TMineOpencastPitWorkingsQSLDDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.BtnAddRowClick';
begin
  try
    GrdWorkingsQSLD.InsertRow(GrdWorkingsQSLD.Row);
    if (GrdWorkingsQSLD.RowCount > GrdWorkingsQSLD.CellsInfo.NoOfHeadingRows) then
      GrdWorkingsQSLD.FixedRows := GrdWorkingsQSLD.CellsInfo.NoOfHeadingRows;
    GrdWorkingsQSLD.Enabled := (GrdWorkingsQSLD.RowCount > GrdWorkingsQSLD.CellsInfo.NoOfHeadingRows);
    if (GrdWorkingsQSLD.Enabled) then
      GrdWorkingsQSLD.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.BtnDeleteRowClick';
begin
  try
    if (GrdWorkingsQSLD.Row >= GrdWorkingsQSLD.CellsInfo.NoOfHeadingRows) then
    begin
      GrdWorkingsQSLD.DeleteRow(GrdWorkingsQSLD.Row);
      GrdWorkingsQSLD.Enabled := (GrdWorkingsQSLD.RowCount > GrdWorkingsQSLD.CellsInfo.NoOfHeadingRows);
      if (GrdWorkingsQSLD.Enabled) then
        GrdWorkingsQSLD.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.BtnRowUpClick';
begin
  try
    if (GrdWorkingsQSLD.Row > GrdWorkingsQSLD.FixedRows) then
    begin
      GrdWorkingsQSLD.MoveRowUp(GrdWorkingsQSLD.Row);
      if (GrdWorkingsQSLD.Row > 0) then
        GrdWorkingsQSLD.Row := GrdWorkingsQSLD.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.BtnRowDownClick';
begin
  try
    if (GrdWorkingsQSLD.Row >= GrdWorkingsQSLD.FixedRows) AND (GrdWorkingsQSLD.Row < GrdWorkingsQSLD.RowCount - 1) then
    begin
      GrdWorkingsQSLD.MoveRowDown(GrdWorkingsQSLD.Row);
      if (GrdWorkingsQSLD.Row < GrdWorkingsQSLD.RowCount) then
        GrdWorkingsQSLD.Row := GrdWorkingsQSLD.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineOpencastPitWorkingsQSLDDlg.GrdWorkingsQSLDDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.GrdWorkingsQSLDDataCellExit';
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

procedure TMineOpencastPitWorkingsQSLDDlg.GrdWorkingsQSLDExit(Sender: TObject);
const OPNAME = 'TMineOpencastPitWorkingsQSLDDlg.GrdWorkingsQSLDExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdWorkingsQSLDDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
