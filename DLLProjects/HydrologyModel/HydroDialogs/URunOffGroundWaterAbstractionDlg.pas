unit URunOffGroundWaterAbstractionDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentRunOffGroundWaterAbstraction,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TRunOffGroundWaterAbstractionDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdGroundWaterAbstractionData : TWRMFGrid;
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
    procedure GrdGroundWaterAbstractionDataDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdGroundWaterAbstractionDataExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentRunOffGroundWaterAbstraction;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TRunOffGroundWaterAbstractionDlg.FormCreate(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentRunOffGroundWaterAbstraction.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.FormShow(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdGroundWaterAbstractionData.Cells[0, 0] := 'Year';
    GrdGroundWaterAbstractionData.Cells[1, 0] := 'Abstraction';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.SetControls;
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.SetControls';
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

procedure TRunOffGroundWaterAbstractionDlg.SetAllValid;
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.SetIndicators;
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffGroundWaterAbstractionDlg.LoadXMLData: boolean;
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.LoadXMLData';
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

function TRunOffGroundWaterAbstractionDlg.StoreXMLData : Boolean;
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.StoreXMLData';
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

procedure TRunOffGroundWaterAbstractionDlg.ControlExit(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.GrdGroundWaterAbstractionDataDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.GrdGroundWaterAbstractionDataDataCellExit';
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

procedure TRunOffGroundWaterAbstractionDlg.GrdGroundWaterAbstractionDataExit(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.GrdGroundWaterAbstractionDataExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdGroundWaterAbstractionDataDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.BtnApplyClick';
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

procedure TRunOffGroundWaterAbstractionDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRunOffGroundWaterAbstractionDlg.DoValidation (AContext      : TValidationContext;
                                                        APropertyName : String;
                                                        AFieldIndex   : String) : Boolean;
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.DoValidation';
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

procedure TRunOffGroundWaterAbstractionDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.ParamChangeIndicatorClicked';
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

procedure TRunOffGroundWaterAbstractionDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.MetaDataIndicatorClicked';
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

procedure TRunOffGroundWaterAbstractionDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.GridParamChangeIndicatorClicked';
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

procedure TRunOffGroundWaterAbstractionDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.GridMetaDataIndicatorClicked';
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

procedure TRunOffGroundWaterAbstractionDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.BtnAddRowClick';
begin
  try
    GrdGroundWaterAbstractionData.InsertRow(GrdGroundWaterAbstractionData.Row);
    if (GrdGroundWaterAbstractionData.RowCount > GrdGroundWaterAbstractionData.CellsInfo.NoOfHeadingRows) then
      GrdGroundWaterAbstractionData.FixedRows := GrdGroundWaterAbstractionData.CellsInfo.NoOfHeadingRows;
    GrdGroundWaterAbstractionData.Enabled := (GrdGroundWaterAbstractionData.RowCount > GrdGroundWaterAbstractionData.CellsInfo.NoOfHeadingRows);
    if (GrdGroundWaterAbstractionData.Enabled) then
      GrdGroundWaterAbstractionData.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.BtnDeleteRowClick';
begin
  try
    if (GrdGroundWaterAbstractionData.Row >= GrdGroundWaterAbstractionData.CellsInfo.NoOfHeadingRows) then
    begin
      GrdGroundWaterAbstractionData.DeleteRow(GrdGroundWaterAbstractionData.Row);
      GrdGroundWaterAbstractionData.Enabled := (GrdGroundWaterAbstractionData.RowCount > GrdGroundWaterAbstractionData.CellsInfo.NoOfHeadingRows);
      if (GrdGroundWaterAbstractionData.Enabled) then
        GrdGroundWaterAbstractionData.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.BtnRowUpClick';
begin
  try
    if (GrdGroundWaterAbstractionData.Row > GrdGroundWaterAbstractionData.FixedRows) then
    begin
      GrdGroundWaterAbstractionData.MoveRowUp(GrdGroundWaterAbstractionData.Row);
      if (GrdGroundWaterAbstractionData.Row > 0) then
        GrdGroundWaterAbstractionData.Row := GrdGroundWaterAbstractionData.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRunOffGroundWaterAbstractionDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TRunOffGroundWaterAbstractionDlg.BtnRowDownClick';
begin
  try
    if (GrdGroundWaterAbstractionData.Row >= GrdGroundWaterAbstractionData.FixedRows) AND
       (GrdGroundWaterAbstractionData.Row < GrdGroundWaterAbstractionData.RowCount - 1) then
    begin
      GrdGroundWaterAbstractionData.MoveRowDown(GrdGroundWaterAbstractionData.Row);
      if (GrdGroundWaterAbstractionData.Row < GrdGroundWaterAbstractionData.RowCount) then
        GrdGroundWaterAbstractionData.Row := GrdGroundWaterAbstractionData.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
