unit UMineUndergroundQSLDDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  UXMLAgent,
  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgentMineUndergroundQSLD,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TMineUndergroundQSLDDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdUndergroundQSLD       : TWRMFGrid;
    LblStdDevUnderground     : TLabel;
    EdtStdDevUnderground     : TWRMFEdit;
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
    procedure GrdUndergroundQSLDDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdUndergroundQSLDExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineUndergroundQSLD;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineUndergroundQSLDDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineUndergroundQSLD.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdUndergroundQSLD.Cells[0, 0] := 'Flow';
    GrdUndergroundQSLD.Cells[1, 0] := 'Load';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.SetControls;
const OPNAME = 'TMineUndergroundQSLDDlg.SetControls';
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

procedure TMineUndergroundQSLDDlg.SetIndicators;
const OPNAME = 'TMineUndergroundQSLDDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.SetAllValid;
const OPNAME = 'TMineUndergroundQSLDDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundQSLDDlg.LoadXMLData: boolean;
const OPNAME = 'TMineUndergroundQSLDDlg.LoadXMLData';
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

function TMineUndergroundQSLDDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineUndergroundQSLDDlg.StoreXMLData';
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

procedure TMineUndergroundQSLDDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.BtnApplyClick';
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

procedure TMineUndergroundQSLDDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundQSLDDlg.DoValidation (AContext      : TValidationContext;
                                               APropertyName : String;
                                               AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineUndergroundQSLDDlg.DoValidation';
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

procedure TMineUndergroundQSLDDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.ParamChangeIndicatorClicked';
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

procedure TMineUndergroundQSLDDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.MetaDataIndicatorClicked';
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

procedure TMineUndergroundQSLDDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineUndergroundQSLDDlg.GridParamChangeIndicatorClicked';
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

procedure TMineUndergroundQSLDDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineUndergroundQSLDDlg.GridMetaDataIndicatorClicked';
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

procedure TMineUndergroundQSLDDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.BtnAddRowClick';
begin
  try
    GrdUndergroundQSLD.InsertRow(GrdUndergroundQSLD.Row);
    if (GrdUndergroundQSLD.RowCount > GrdUndergroundQSLD.CellsInfo.NoOfHeadingRows) then
      GrdUndergroundQSLD.FixedRows := GrdUndergroundQSLD.CellsInfo.NoOfHeadingRows;
    GrdUndergroundQSLD.Enabled := (GrdUndergroundQSLD.RowCount > GrdUndergroundQSLD.CellsInfo.NoOfHeadingRows);
    if (GrdUndergroundQSLD.Enabled) then
      GrdUndergroundQSLD.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.BtnDeleteRowClick';
begin
  try
    if (GrdUndergroundQSLD.Row >= GrdUndergroundQSLD.CellsInfo.NoOfHeadingRows) then
    begin
      GrdUndergroundQSLD.DeleteRow(GrdUndergroundQSLD.Row);
      GrdUndergroundQSLD.Enabled := (GrdUndergroundQSLD.RowCount > GrdUndergroundQSLD.CellsInfo.NoOfHeadingRows);
      if (GrdUndergroundQSLD.Enabled) then
        GrdUndergroundQSLD.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.BtnRowUpClick';
begin
  try
    if (GrdUndergroundQSLD.Row > GrdUndergroundQSLD.FixedRows) then
    begin
      GrdUndergroundQSLD.MoveRowUp(GrdUndergroundQSLD.Row);
      if (GrdUndergroundQSLD.Row > 0) then
        GrdUndergroundQSLD.Row := GrdUndergroundQSLD.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.BtnRowDownClick';
begin
  try
    if (GrdUndergroundQSLD.Row >= GrdUndergroundQSLD.FixedRows) AND (GrdUndergroundQSLD.Row < GrdUndergroundQSLD.RowCount - 1) then
    begin
      GrdUndergroundQSLD.MoveRowDown(GrdUndergroundQSLD.Row);
      if (GrdUndergroundQSLD.Row < GrdUndergroundQSLD.RowCount) then
        GrdUndergroundQSLD.Row := GrdUndergroundQSLD.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundQSLDDlg.GrdUndergroundQSLDDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineUndergroundQSLDDlg.GrdUndergroundQSLDDataCellExit';
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

procedure TMineUndergroundQSLDDlg.GrdUndergroundQSLDExit(Sender: TObject);
const OPNAME = 'TMineUndergroundQSLDDlg.GrdUndergroundQSLDExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdUndergroundQSLDDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
