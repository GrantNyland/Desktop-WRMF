unit UMineUndergroundHighExtractionDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentMineUndergroundHighExtraction,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid;

type
  TMineUndergroundHighExtractionDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdHighExtraction        : TWRMFGrid;
    LblHighExtractionInterpolationOption: TLabel;
    CbxHighExtractionInterpolationOption: TWRMFComboBox;
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
    procedure GrdHighExtractionDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdHighExtractionExit(Sender: TObject);
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
    FXMLAgent         : TXMLAgentMineUndergroundHighExtraction;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TMineUndergroundHighExtractionDlg.FormCreate(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentMineUndergroundHighExtraction.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.FormShow(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.FormShow';
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdHighExtraction.Cells[0, 0] := 'Year';
    GrdHighExtraction.Cells[1, 0] := 'Growth';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.SetControls;
const OPNAME = 'TMineUndergroundHighExtractionDlg.SetControls';
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

procedure TMineUndergroundHighExtractionDlg.SetIndicators;
const OPNAME = 'TMineUndergroundHighExtractionDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.SetAllValid;
const OPNAME = 'TMineUndergroundHighExtractionDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundHighExtractionDlg.LoadXMLData: boolean;
const OPNAME = 'TMineUndergroundHighExtractionDlg.LoadXMLData';
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
    CbxHighExtractionInterpolationOption.Items.Clear;
    CbxHighExtractionInterpolationOption.Items.AddObject('Linear', pointer(1));
    CbxHighExtractionInterpolationOption.Items.AddObject('Exponential', pointer(2));
    CbxHighExtractionInterpolationOption.Items.AddObject('Defined', pointer(3));

    Result := FControlIterator.LoadXMLData(LRootNode);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundHighExtractionDlg.StoreXMLData : Boolean;
const OPNAME = 'TMineUndergroundHighExtractionDlg.StoreXMLData';
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

procedure TMineUndergroundHighExtractionDlg.ControlExit(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.ControlExit';
var
  LControl : TWRMFControl;
begin
  try
    LControl := TWRMFControl(TControl(Sender).Parent);
    LControl.IsValid := TRUE;
    DoValidation(tcChange, LControl.PropertyName, '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.BtnApplyClick';
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

procedure TMineUndergroundHighExtractionDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TMineUndergroundHighExtractionDlg.DoValidation (AContext      : TValidationContext;
                                                         APropertyName : String;
                                                         AFieldIndex   : String) : Boolean;
const OPNAME = 'TMineUndergroundHighExtractionDlg.DoValidation';
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

procedure TMineUndergroundHighExtractionDlg.ParamChangeIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.ParamChangeIndicatorClicked';
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

procedure TMineUndergroundHighExtractionDlg.MetaDataIndicatorClicked (Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.MetaDataIndicatorClicked';
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

procedure TMineUndergroundHighExtractionDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineUndergroundHighExtractionDlg.GridParamChangeIndicatorClicked';
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

procedure TMineUndergroundHighExtractionDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TMineUndergroundHighExtractionDlg.GridMetaDataIndicatorClicked';
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

procedure TMineUndergroundHighExtractionDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.BtnAddRowClick';
begin
  try
    GrdHighExtraction.InsertRow(GrdHighExtraction.Row);
    if (GrdHighExtraction.RowCount > GrdHighExtraction.CellsInfo.NoOfHeadingRows) then
      GrdHighExtraction.FixedRows := GrdHighExtraction.CellsInfo.NoOfHeadingRows;
    GrdHighExtraction.Enabled := (GrdHighExtraction.RowCount > GrdHighExtraction.CellsInfo.NoOfHeadingRows);
    if (GrdHighExtraction.Enabled) then
      GrdHighExtraction.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.BtnDeleteRowClick';
begin
  try
    if (GrdHighExtraction.Row >= GrdHighExtraction.CellsInfo.NoOfHeadingRows) then
    begin
      GrdHighExtraction.DeleteRow(GrdHighExtraction.Row);
      GrdHighExtraction.Enabled := (GrdHighExtraction.RowCount > GrdHighExtraction.CellsInfo.NoOfHeadingRows);
      if (GrdHighExtraction.Enabled) then
        GrdHighExtraction.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.BtnRowUpClick';
begin
  try
    if (GrdHighExtraction.Row > GrdHighExtraction.FixedRows) then
    begin
      GrdHighExtraction.MoveRowUp(GrdHighExtraction.Row);
      if (GrdHighExtraction.Row > 0) then
        GrdHighExtraction.Row := GrdHighExtraction.Row - 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.BtnRowDownClick';
begin
  try
    if (GrdHighExtraction.Row >= GrdHighExtraction.FixedRows) AND (GrdHighExtraction.Row < GrdHighExtraction.RowCount - 1) then
    begin
      GrdHighExtraction.MoveRowDown(GrdHighExtraction.Row);
      if (GrdHighExtraction.Row < GrdHighExtraction.RowCount) then
        GrdHighExtraction.Row := GrdHighExtraction.Row + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TMineUndergroundHighExtractionDlg.GrdHighExtractionDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TMineUndergroundHighExtractionDlg.GrdHighExtractionDataCellExit';
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

procedure TMineUndergroundHighExtractionDlg.GrdHighExtractionExit(Sender: TObject);
const OPNAME = 'TMineUndergroundHighExtractionDlg.GrdHighExtractionExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdHighExtractionDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
