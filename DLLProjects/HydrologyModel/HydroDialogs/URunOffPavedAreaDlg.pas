unit URunOffPavedAreaDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentRunOffPavedArea,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TRunOffPavedAreaDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdPavedAreaData         : TWRMFGrid;
    BtnAddRow                : TSpeedButton;
    BtnDeleteRow             : TSpeedButton;
    BtnRowUp                 : TSpeedButton;
    BtnRowDown               : TSpeedButton;
    procedure FormShow(Sender : TObject);
    procedure ControlExit(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ParamChangeIndicatorClicked (Sender: TObject);
    procedure MetaDataIndicatorClicked (Sender: TObject);
    procedure GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GrdPavedAreaDataDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdPavedAreaDataExit(Sender: TObject);
    procedure BtnAddRowClick(Sender: TObject);
    procedure BtnDeleteRowClick(Sender: TObject);
    procedure BtnRowUpClick(Sender: TObject);
    procedure BtnRowDownClick(Sender: TObject);
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
    FXMLAgent         : TXMLAgentRunOffPavedArea;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TRunOffPavedAreaDlg.FormCreate(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentRunOffPavedArea.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.FormShow(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdPavedAreaData.Cells[0, 0] := 'Year';
    GrdPavedAreaData.Cells[1, 0] := 'Proportion';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.SetControls;
const OPNAME = 'TRunOffPavedAreaDlg.SetControls';
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

procedure TRunOffPavedAreaDlg.SetAllValid;
const OPNAME = 'TRunOffPavedAreaDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.SetIndicators;
const OPNAME = 'TRunOffPavedAreaDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPavedAreaDlg.LoadXMLData: boolean;
const OPNAME = 'TRunOffPavedAreaDlg.LoadXMLData';
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

function TRunOffPavedAreaDlg.StoreXMLData : Boolean;
const OPNAME = 'TRunOffPavedAreaDlg.StoreXMLData';
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

procedure TRunOffPavedAreaDlg.ControlExit(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.GrdPavedAreaDataDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TRunOffPavedAreaDlg.GrdPavedAreaDataDataCellExit';
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

procedure TRunOffPavedAreaDlg.GrdPavedAreaDataExit(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.GrdPavedAreaDataExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdPavedAreaDataDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.BtnApplyClick';
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

procedure TRunOffPavedAreaDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffPavedAreaDlg.DoValidation (AContext      : TValidationContext;
                                           APropertyName : String;
                                           AFieldIndex   : String) : Boolean;
const OPNAME = 'TRunOffPavedAreaDlg.DoValidation';
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

procedure TRunOffPavedAreaDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.ParamChangeIndicatorClicked';
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

procedure TRunOffPavedAreaDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.MetaDataIndicatorClicked';
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

procedure TRunOffPavedAreaDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffPavedAreaDlg.GridParamChangeIndicatorClicked';
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

procedure TRunOffPavedAreaDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffPavedAreaDlg.GridMetaDataIndicatorClicked';
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

procedure TRunOffPavedAreaDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.BtnAddRowClick';
begin
  try
    GrdPavedAreaData.InsertRow(GrdPavedAreaData.Row);
    if (GrdPavedAreaData.RowCount > GrdPavedAreaData.CellsInfo.NoOfHeadingRows) then
      GrdPavedAreaData.FixedRows := GrdPavedAreaData.CellsInfo.NoOfHeadingRows;
    GrdPavedAreaData.Enabled := (GrdPavedAreaData.RowCount > GrdPavedAreaData.CellsInfo.NoOfHeadingRows);
    if (GrdPavedAreaData.Enabled) then
      GrdPavedAreaData.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.BtnDeleteRowClick';
begin
  try
    if (GrdPavedAreaData.Row >= GrdPavedAreaData.CellsInfo.NoOfHeadingRows) then
    begin
      GrdPavedAreaData.DeleteRow(GrdPavedAreaData.Row);
      GrdPavedAreaData.Enabled := (GrdPavedAreaData.RowCount > GrdPavedAreaData.CellsInfo.NoOfHeadingRows);
      if (GrdPavedAreaData.Enabled) then
        GrdPavedAreaData.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.BtnRowUpClick';
begin
  try
    if (GrdPavedAreaData.Row > GrdPavedAreaData.FixedRows) then
    begin
      GrdPavedAreaData.MoveRowUp(GrdPavedAreaData.Row);
      if (GrdPavedAreaData.Row > 0) then
        GrdPavedAreaData.Row := GrdPavedAreaData.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffPavedAreaDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TRunOffPavedAreaDlg.BtnRowDownClick';
begin
  try
    if (GrdPavedAreaData.Row >= GrdPavedAreaData.FixedRows) AND (GrdPavedAreaData.Row < GrdPavedAreaData.RowCount - 1) then
    begin
      GrdPavedAreaData.MoveRowDown(GrdPavedAreaData.Row);
      if (GrdPavedAreaData.Row < GrdPavedAreaData.RowCount) then
        GrdPavedAreaData.Row := GrdPavedAreaData.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
