unit UIrrigationCropsDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UControlIterator,
  UXMLAgent,
  UXMLAgentIrrigationCrops,
  UWRMFEdit, UWRMFComboBox, UWRMFControl, UWRMFCheckBox, VCL.ComCtrls,
  VCL.Grids, UCellChangeStringGrid, UWRMFGrid, Xml.Win.msxmldom;

type
  TIrrigationCropsDlg = class(TForm)
    PnlBottom                : TPanel;
    BtnApply                 : TButton;
    BtnReset                 : TButton;
    FXMLDocumentIn           : TXMLDocument;
    FXMLDocumentOut          : TXMLDocument;
    ScrClient                : TScrollBox;
    GrdCrops                 : TWRMFGrid;
    BtnAddRow                : TSpeedButton;
    BtnDeleteRow             : TSpeedButton;
    BtnRowUp                 : TSpeedButton;
    BtnRowDown               : TSpeedButton;
    procedure FormShow(Sender: TObject);
    procedure BtnApplyClick(Sender: TObject);
    procedure BtnResetClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
    procedure BtnAddRowClick(Sender: TObject);
    procedure BtnDeleteRowClick(Sender: TObject);
    procedure BtnRowUpClick(Sender: TObject);
    procedure BtnRowDownClick(Sender: TObject);
    procedure GrdCropsDataCellExit(ASender: TObject; ACol, ARow: Integer);
    procedure GrdCropsExit(Sender: TObject);
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
    procedure RenumberGridRows;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentIrrigationCrops;
    FHydrologyModel   : IHydrologyModel;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure TIrrigationCropsDlg.FormCreate(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.FormCreate';
begin
  try
    FErrorList := TStringList.Create;
    FControlIterator := TControlIterator.Create;
    FControlIterator.ControlParent := ScrClient;
    FXMLAgent := TXMLAgentIrrigationCrops.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.FormDestroy(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.FormDestroy';
begin
  try
    FreeAndNil(FErrorList);
    FreeAndNil(FControlIterator);
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.FormShow(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.FormShow';
var
  LMonth : Integer;
begin
  try
    LoadXMLData;
    SetControls;
    SetIndicators;
    GrdCrops.Cells[0, 0] := 'CropNo';
    GrdCrops.Cells[1, 0] := '%';
    for LMonth := 1 to 12 do
      GrdCrops.Cells[LMonth+1, 0] := 'Month ' + IntToStr(LMonth);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.SetControls;
const OPNAME = 'TIrrigationCropsDlg.SetControls';
begin
  try
    FControlIterator.SetControls(FMayChangeNetwork);
    BtnAddRow.Enabled    := FMayChangeNetwork;
    BtnDeleteRow.Enabled := FMayChangeNetwork;
    BtnRowUp.Enabled     := FMayChangeNetwork;
    BtnRowDown.Enabled   := FMayChangeNetwork;
    BtnRowDown.Enabled   := FMayChangeNetwork;

    BtnApply.Enabled     := FMayChangeNetwork;
    BtnReset.Enabled     := FMayChangeNetwork;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.SetIndicators;
const OPNAME = 'TIrrigationCropsDlg.SetIndicators';
begin
  try
    FControlIterator.SetIndicators;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.SetAllValid;
const OPNAME = 'TIrrigationCropsDlg.SetAllValid';
begin
  try
    FControlIterator.SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.RenumberGridRows;
const OPNAME = 'TIrrigationCropsDlg.RenumberGridRows';
var
  LRow      : Integer;
begin
  try
    for LRow := 1 to GrdCrops.RowCount - 1 do
      GrdCrops.Cells[0, LRow] := IntToStr(LRow);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationCropsDlg.LoadXMLData: boolean;
const OPNAME = 'TIrrigationCropsDlg.LoadXMLData';
var
  LRootNode        : IXMLNode;
  LSectionNode     : IXMLNode;
  LDataListNode    : IXMLNode;
  LCropDataNode    : IXMLNode;
  LMonthlyListNode : IXMLNode;
  LNode            : IXMLNode;
  LIndex           : Integer;
  LCellInfo        : TCellInfo;
  LMonth           : Integer;
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

    LSectionNode  := LRootNode.ChildNodes['IrrigationCrops'];
    LDataListNode := LSectionNode.ChildNodes['DataList'];
    // CropsData.
    GrdCrops.RowCount := 1 + LDataListNode.ChildNodes.Count;
    GrdCrops.Enabled  := (GrdCrops.RowCount > GrdCrops.CellsInfo.NoOfHeadingRows);
    if (GrdCrops.RowCount > GrdCrops.CellsInfo.NoOfHeadingRows) then
      GrdCrops.FixedRows := GrdCrops.CellsInfo.NoOfHeadingRows;
    for LIndex := 1 to LDataListNode.ChildNodes.Count do
    begin
      LCellInfo := GrdCrops.CellInfo[LIndex, 0];
      LCellInfo.PropertyName := 'CropNo';
      LCellInfo := GrdCrops.CellInfo[LIndex, 1];
      LCellInfo.PropertyName := 'CropPercentage';
      LCropDataNode := LDataListNode.ChildNodes.Get(LIndex-1);
      GrdCrops.Cells[0, LIndex] := IntToStr(LIndex);
      GrdCrops.Cells[1, LIndex] := LCropDataNode.ChildNodes['CropPercentage'].Text;
      LMonthlyListNode := LCropDataNode.ChildNodes['MonthlyCropFactors'];
      for LMonth := 1 to LMonthlyListNode.ChildNodes.Count do
      begin
        LNode := LMonthlyListNode.ChildNodes.Get(LMonth-1);
        LCellInfo := GrdCrops.CellInfo[LIndex, LMonth+1];
        LCellInfo.PropertyName := 'CropFactor';
        GrdCrops.Cells[LMonth+1, LIndex] := LNode.ChildNodes['CropFactor'].Text;
      end;
    end;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationCropsDlg.StoreXMLData : Boolean;
const OPNAME = 'TIrrigationCropsDlg.StoreXMLData';
var
  LRootNode        : IXMLNode;
  LSectionNode     : IXMLNode;
  LDataListNode    : IXMLNode;
  LCropDataNode    : IXMLNode;
  LMonthlyListNode : IXMLNode;
  LNode            : IXMLNode;
  LIndex           : Integer;
  LCount           : Integer;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    LSectionNode  := LRootNode.ChildNodes['IrrigationCrops'];
    LDataListNode := LSectionNode.ChildNodes['DataList'];
    LDataListNode.ChildNodes.Clear;
    for LIndex := 1 to GrdCrops.RowCount - 1 do
    begin
      LCropDataNode := LDataListNode.AddChild('CropData');
      LCropDataNode.AddChild('CropNo');
      LCropDataNode.AddChild('CropPercentage');
      LCropDataNode.ChildNodes['CropNo'].Text         := GrdCrops.Cells[0, LIndex];
      LCropDataNode.ChildNodes['CropPercentage'].Text := GrdCrops.Cells[1, LIndex];
      LMonthlyListNode := LCropDataNode.AddChild('MonthlyCropFactors');
      for LCount := 1 to 12 do
      begin
        LNode := LMonthlyListNode.AddChild('MonthlyData');
        LNode.AddChild('Month');
        LNode.AddChild('CropFactor');
        LNode.ChildNodes['Month'].Text      := IntToStr(LCount);
        LNode.ChildNodes['CropFactor'].Text := GrdCrops.Cells[LCount+1, LIndex];
      end;
    end;

    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.BtnApplyClick(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.BtnApplyClick';
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

procedure TIrrigationCropsDlg.BtnResetClick(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.BtnResetClick';
begin
  try
    LoadXMLData;
    SetAllValid;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TIrrigationCropsDlg.DoValidation (AContext      : TValidationContext;
                                           APropertyName : String;
                                           AFieldIndex   : String) : Boolean;
const OPNAME = 'TIrrigationCropsDlg.DoValidation';
var
  LErrorMessages : TStringList;
  LFirstError    : String;
  LRow           : Integer;
  LCol           : Integer;
  LCellInfo      : TCellInfo;
begin
  Result := FALSE;
  try
    if ((AContext = tcChange) OR (AContext = tcApply)) then
    begin
      Result := StoreXMLData;
      if (Result) then
      begin
        LErrorMessages := TStringList.Create;
        try
          Result := FXMLAgent.Validate(AContext,
                                       APropertyName,
                                       AFieldIndex,
                                       FXMLDocumentIn.XML.Text,
                                       FXMLDocumentOut.XML.Text,
                                       FALSE,
                                       FErrorList,
                                       LErrorMessages);

          for LRow := 1 to GrdCrops.RowCount - 1 do
          begin
            for LCol := 0 to 1 do
            begin
              LCellInfo := GrdCrops.CellInfo[LRow, LCol];
              LCellInfo.IsValid := FErrorList.IndexOf(LCellInfo.PropertyName + ',' + IntToStr(LRow)) < 0;
            end;
            for LCol := 2 to GrdCrops.ColCount - 1 do
            begin
              LCellInfo := GrdCrops.CellInfo[LRow, LCol];
              LCellInfo.IsValid := FErrorList.IndexOf(LCellInfo.PropertyName + ',' + IntToStr(LRow) + ',' + IntToStr(LCol-1)) < 0;
            end;
          end;

          if (NOT Result) then
          begin
            ShowMessage(LErrorMessages.Text);
            LFirstError := '';
          end;
        finally
          LErrorMessages.Free;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.GridParamChangeIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationCropsDlg.GridParamChangeIndicatorClicked';
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

procedure TIrrigationCropsDlg.GridMetaDataIndicatorClicked (Sender: TObject; ACol, ARow: integer);
const OPNAME = 'TIrrigationCropsDlg.GridMetaDataIndicatorClicked';
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

procedure TIrrigationCropsDlg.BtnAddRowClick(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.BtnAddRowClick';
begin
  try
    GrdCrops.InsertRow(GrdCrops.Row);
    RenumberGridRows;
    if (GrdCrops.RowCount > GrdCrops.CellsInfo.NoOfHeadingRows) then
      GrdCrops.FixedRows := GrdCrops.CellsInfo.NoOfHeadingRows;
    GrdCrops.Enabled := (GrdCrops.RowCount > GrdCrops.CellsInfo.NoOfHeadingRows);
    if (GrdCrops.Enabled) then
      GrdCrops.SetFocus;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.BtnDeleteRowClick(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.BtnDeleteRowClick';
begin
  try
    if (GrdCrops.Row >= GrdCrops.CellsInfo.NoOfHeadingRows) then
    begin
      GrdCrops.DeleteRow(GrdCrops.Row);
      RenumberGridRows;
      GrdCrops.Enabled := (GrdCrops.RowCount > GrdCrops.CellsInfo.NoOfHeadingRows);
      if (GrdCrops.Enabled) then
        GrdCrops.SetFocus;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.BtnRowUpClick(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.BtnRowUpClick';
begin
  try
    if (GrdCrops.Row > GrdCrops.FixedRows) then
    begin
      GrdCrops.MoveRowUp(GrdCrops.Row);
      if (GrdCrops.Row > 0) then
        GrdCrops.Row := GrdCrops.Row - 1;
      RenumberGridRows;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.BtnRowDownClick(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.BtnRowDownClick';
begin
  try
    if (GrdCrops.Row >= GrdCrops.FixedRows) AND (GrdCrops.Row < GrdCrops.RowCount - 1) then
    begin
      GrdCrops.MoveRowDown(GrdCrops.Row);
      if (GrdCrops.Row < GrdCrops.RowCount) then
        GrdCrops.Row := GrdCrops.Row + 1;
      RenumberGridRows;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.GrdCropsDataCellExit(ASender: TObject; ACol, ARow: Integer);
const OPNAME = 'TIrrigationCropsDlg.GrdCropsDataCellExit';
var
  LCellInfo : TCellInfo;
  LGrid     : TWRMFGrid;
begin
  try
    LGrid := TWRMFGrid(ASender);
    LCellInfo := LGrid.CellInfo[ARow, ACol];
    LCellInfo.IsValid := TRUE;
    if (ACol <= 1) then
      DoValidation(tcChange, TWRMFGrid(ASender).CellInfo[ARow, ACol].PropertyName, IntToStr(ARow))
    else
      DoValidation(tcChange, TWRMFGrid(ASender).CellInfo[ARow, ACol].PropertyName, IntToStr(ARow) + ',' + IntToStr(ACol-1));
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TIrrigationCropsDlg.GrdCropsExit(Sender: TObject);
const OPNAME = 'TIrrigationCropsDlg.GrdCropsExit';
var
  LWRMFGrid : TWRMFGrid;
begin
  try
    LWRMFGrid := TWRMFGrid(Sender);
    GrdCropsDataCellExit(LWRMFGrid, LWRMFGrid.Col, LWRMFGrid.Row);
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.
