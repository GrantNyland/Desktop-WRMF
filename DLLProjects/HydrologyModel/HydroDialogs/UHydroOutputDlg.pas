unit UHydroOutputDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UXMLAgent,
  UXMLAgentHydroOutput,
  VCL.ComCtrls,
  VCL.Grids,
  VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart, VCLTee.Series,
  VclTee.TeeGDIPlus, Xml.Win.msxmldom;

type
  THydroOutputDlg = class(TForm)
    PnlTop           : TPanel;
    FXMLDocumentIn   : TXMLDocument;
    FXMLDocumentOut  : TXMLDocument;
    LblResultType    : TLabel;
    LblNoData        : TLabel;
    PgcHydroOutput   : TPageControl;
    TbsTable         : TTabSheet;
    TbsGraph         : TTabSheet;
    CrtHydroOutput   : TChart;
    LsrData          : TLineSeries;
    LblSectionType   : TLabel;
    CbxSectionType   : TComboBox;
    CbxResultType    : TComboBox;
    GrdHydroOutput   : TStringGrid;
    LsrObserved      : TLineSeries;
    LblUnits: TLabel;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CbxResultTypeChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure CbxSectionTypeChange(Sender: TObject);
  private
    { Private declarations }
    function LoadXMLData : Boolean;
    function StoreXMLData (ASectionChange : Bool) : Boolean;
    procedure DisplayUnits;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentHydroOutput;
    FHydrologyModel   : IHydrologyModel;
    procedure PopulateForm;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure THydroOutputDlg.FormCreate(Sender: TObject);
const OPNAME = 'THydroOutputDlg.FormCreate';
begin
  try
    FXMLAgent := TXMLAgentHydroOutput.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutputDlg.FormDestroy(Sender: TObject);
const OPNAME = 'THydroOutputDlg.FormDestroy';
begin
  try
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutputDlg.FormShow(Sender: TObject);
const OPNAME = 'THydroOutputDlg.FormShow';
var
  LCol   : Integer;
  LIndex : Integer;
begin
  try
    GrdHydroOutput.Cells[0, 0] := 'Year';
    for LCol := 1 to 12 do
    begin
      LIndex := (LCol + 9) mod 12;
      if (LIndex = 0) then
        LIndex := 12;
      GrdHydroOutput.Cells[LCol, 0] := FormatSettings.ShortMonthNames[LIndex];
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutputDlg.PopulateForm;
const OPNAME = 'THydroOutputDlg.PopulateForm';
begin
  try
    LoadXMLData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutputDlg.LoadXMLData: boolean;
const OPNAME = 'THydroOutputDlg.LoadXMLData';
var
  LRootNode           : IXMLNode;
  LAllResultTypesNode : IXMLNode;
  LIndex              : Integer;
  LItemIndex          : Integer;
  LNode               : IXMLNode;
  LTypeID             : Integer;
  LDescription        : String;
  LResultTypeID       : Integer;
  LResultIndex        : Integer;
  LSectionNode        : IXMLNode;
  LDataListNode       : IXMLNode;
  LObservedFlowNode   : IXMLNode;
  LYear               : Integer;
  LMonthIndex         : Integer;
  LMonth              : Integer;
  LFieldName          : String;
  LValue              : Double;
  LLabel              : String;
  LXVal               : Integer;
  LCaption            : String;
  LElementType        : String;
  LElementSubType     : String;
  LElementNo          : Integer;
  LSubElementID       : Integer;
  LAllSectionsNode    : IXMLNode;
  LSectionNo          : Integer;
  LSectionName        : String;
  LSectionType        : String;
  LSectionIndex       : Integer;
  LSubType            : String;
  LRow                : Integer;
  LCol                : Integer;
  LFirstYear          : Integer;
  LNoOfYears          : Integer;
  LAllZero            : Boolean;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LAllResultTypesNode    := LRootNode.ChildNodes['AllHydroResultTypes'];

    LSectionNode  := LRootNode.ChildNodes['HydroOutput'];
    LDataListNode := LSectionNode.ChildNodes['TimeSeries'];
//    LblNoData.Visible := LDataListNode.ChildNodes.Count = 0;

    LElementType    := LRootNode.ChildNodes['ElementType'].Text;
    LElementSubType := LRootNode.ChildNodes['ElementSubType'].Text;
    LElementNo      := StrToInt(LRootNode.ChildNodes['ElementNo'].Text);
    LSubElementID   := StrToInt(LRootNode.ChildNodes['SubElementID'].Text);
    LResultTypeID   := StrToInt(LRootNode.ChildNodes['ResultTypeID'].Text);
    LAllZero        := StrToInt(LRootNode.ChildNodes['AllZero'].Text) = 1;

    LblNoData.Visible := (NOT LAllZero) AND (LDataListNode.ChildNodes.Count = 0);

    if (LElementType = 'RQ') then
      LCaption := 'Network Route ' + IntToStr(LElementNo)
    else if (LElementType = 'RU') then
      LCaption := 'RunOff Module RU' + IntToStr(LElementNo)
    else if (LElementType = 'RV') then
      LCaption := 'Reservoir Module RV' + IntToStr(LElementNo)
    else if (LElementType = 'CR') then
      LCaption := 'Channel Module CR' + IntToStr(LElementNo)
    else if (LElementType = 'RR') then
      LCaption := 'Irrigation Module RR' + IntToStr(LElementNo)
    else if (LElementType = 'MM') then
      LCaption := 'Mine Module MM' + IntToStr(LElementNo);
    Caption := 'Hydrology Output Results: ' + LCaption;

    // Populate SectionType combobox
    CbxSectionType.Clear;
    LSectionIndex := -1;
    if (LElementType = 'MM') then
    begin
      CbxSectionType.Visible := TRUE;
      LblSectionType.Visible := TRUE;
      LAllSectionsNode       := LRootNode.ChildNodes['AllSections'];
      for LIndex := 0 to LAllSectionsNode.ChildNodes.Count - 1 do
      begin
        LNode         := LAllSectionsNode.ChildNodes.Get(LIndex);
        LSectionNo    := StrToInt(LNode.ChildNodes['SectionNo'].Text);
        LSectionType  := LNode.ChildNodes['SectionType'].Text;
        LSectionName  := LNode.ChildNodes['SectionName'].Text;
        if (LSectionType <> '') then
          LDescription := '[' + LSectionType + IntToStr(LSectionNo) + '] ' + LSectionName
        else
          LDescription := LSectionName;
        CbxSectionType.Items.AddObject(LDescription, Pointer(LSectionNo));
        if ((LElementSubType = LSectionType) AND (LSubElementID = LSectionNo)) then
          LSectionIndex := LIndex;
      end;      
      if (CbxSectionType.Items.Count > 0) then
      begin
        if (LSectionIndex < 0) then
          LSectionIndex := 0;
        CbxSectionType.ItemIndex := LSectionIndex;
      end;
    end
    else
    begin
      CbxSectionType.Visible := FALSE;
      LblSectionType.Visible := FALSE;
    end;

    // Populate ResultType combobox
    CbxResultType.Items.Clear;
    LResultIndex := -1;
    for LIndex := 0 to LAllResultTypesNode.ChildNodes.Count - 1 do
    begin
      LNode         := LAllResultTypesNode.ChildNodes.Get(LIndex);
      LTypeID       := StrToInt(LNode.ChildNodes['ResultTypeID'].Text);
      LDescription  := LNode.ChildNodes['Description'].Text;
      LSubType      := LNode.ChildNodes['ElementSubType'].Text;
      if (LSubType = LElementSubType) then
      begin
        LItemIndex := CbxResultType.Items.AddObject(LDescription, pointer(LTypeID));
        if (LTypeID = LResultTypeID) then
          LResultIndex := LItemIndex;
      end;    
    end;
    if (CbxResultType.Items.Count > 0) then
    begin
      if (LResultIndex < 0) then
        LResultIndex := 0;
      CbxResultType.ItemIndex := LResultIndex;
    end;
    DisplayUnits; 

    // Clear the grid
    for LRow := 1 to GrdHydroOutput.RowCount - 1 do
      for LCol := 0 to GrdHydroOutput.ColCount - 1 do
        GrdHydroOutput.Cells[LCol, LRow] := '';

    // Populate the grid and the graph
    LFirstYear := 0;
    LsrData.Clear;
    LsrData.Title := CbxResultType.Text;
    CrtHydroOutput.Title.Text.Clear;
    CrtHydroOutput.Title.Text.Add(LCaption);
    if (LAllZero) then
    begin
      LFirstYear := FHydrologyModel.Network.SimulationStartYear;
      LNoOfYears := FHydrologyModel.Network.SimulationEndYear - LFirstYear + 1;
      GrdHydroOutput.RowCount := 1 + LNoOfYears;
      if (GrdHydroOutput.RowCount > 1) then
        GrdHydroOutput.FixedRows := 1;
      for LIndex := 0 to LNoOfYears - 1 do
      begin
        LYear := LFirstYear + LIndex;
        GrdHydroOutput.Cells[0, LIndex] := IntToStr(LYear);
        for LMonthIndex := 1 to 12 do
        begin
          GrdHydroOutput.Cells[LMonthIndex, LIndex] := '0.0';
          LMonth := LMonthIndex + 9;
          if (LMonth > 12) then
          begin
            LMonth := LMonth - 12;
            LLabel := IntToStr(LYear+1) + '/' + Format('%2.2d', [LMonth]);
          end
          else
            LLabel := IntToStr(LYear) + '/' + Format('%2.2d', [LMonth]);
          LXVal := (LIndex - 1) * 12 + (LMonthIndex - 1);
          LsrData.AddXY(LXVal, 0.0, LLabel);
        end;
      end;
    end
    else
    begin
      GrdHydroOutput.RowCount := 1 + LDataListNode.ChildNodes.Count;
      if (GrdHydroOutput.RowCount > 1) then
        GrdHydroOutput.FixedRows := 1;
      for LIndex := 1 to LDataListNode.ChildNodes.Count do
      begin
        LNode := LDataListNode.ChildNodes.Get(LIndex-1);
        LYear := StrToInt(LNode.ChildNodes['Year'].Text);
        if (LFirstYear = 0) then
          LFirstYear := LYear;
        GrdHydroOutput.Cells[0, LIndex] := IntToStr(LYear);
        for LMonthIndex := 1 to 12 do
        begin
          LFieldName := 'Month' + Format('%2.2d', [LMonthIndex]);
          LValue := StrToFloat(LNode.ChildNodes[LFieldName].Text);
          GrdHydroOutput.Cells[LMonthIndex, LIndex] := LNode.ChildNodes[LFieldName].Text;
          LMonth := LMonthIndex + 9;
          if (LMonth > 12) then
          begin
            LMonth := LMonth - 12;
            LLabel := IntToStr(LYear+1) + '/' + Format('%2.2d', [LMonth]);
          end
          else
            LLabel := IntToStr(LYear) + '/' + Format('%2.2d', [LMonth]);
          LXVal := (LIndex - 1) * 12 + (LMonthIndex - 1);
          LsrData.AddXY(LXVal, LValue, LLabel);
        end;
      end;
    end;

    LsrObserved.Active := FALSE;
    // Observed Flow
    LObservedFlowNode := LSectionNode.ChildNodes['ObservedFlow'];
    if ((LResultTypeID = 7) AND (LObservedFlowNode.ChildNodes.Count > 0)) then
    begin
      LsrObserved.Active := TRUE;
      LsrObserved.Title  := 'Observed Flow';
      for LIndex := 1 to LObservedFlowNode.ChildNodes.Count do
      begin
        LNode := LObservedFlowNode.ChildNodes.Get(LIndex-1);
        LYear := StrToInt(LNode.ChildNodes['Year'].Text);
        for LMonthIndex := 1 to 12 do
        begin
          LFieldName := 'Month' + Format('%2.2d', [LMonthIndex]);
          LValue := StrToFloat(LNode.ChildNodes[LFieldName].Text);
          LMonth := LMonthIndex + 9;
          if (LMonth > 12) then
          begin
            LMonth := LMonth - 12;
            LLabel := IntToStr(LYear+1) + '/' + Format('%2.2d', [LMonth]);
          end
          else
            LLabel := IntToStr(LYear) + '/' + Format('%2.2d', [LMonth]);
          LXVal := (LYear - LFirstYear) * 12 + (LMonthIndex - 1);
          LsrObserved.AddXY(LXVal, LValue, LLabel);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroOutputDlg.StoreXMLData (ASectionChange : Bool) : Boolean;
const OPNAME = 'THydroOutputDlg.StoreXMLData';
var
  LRootNode       : IXMLNode;
  LResultTypeID   : Integer;
  LIndex          : Integer;
  LElementSubType : String;
  LSubElementID   : Integer;
  LSubElement     : String;
begin
  Result := FALSE;
  try
    LRootNode := FXMLDocumentOut.DocumentElement;
    if (ASectionChange) then
    begin
      LIndex := CbxSectionType.ItemIndex;
      if (LIndex >= 0) then
      begin
        LSubElementID := Integer(CbxSectionType.Items.Objects[LIndex]);
        LSubElement   := CbxSectionType.Items[LIndex];
        if (Pos('[', LSubElement) > 0) then
          LElementSubType := Copy(LSubElement, 2, 2)
        else
          LElementSubType := '';
        LRootNode.ChildNodes['ElementSubType'].Text := LElementSubType;
        LRootNode.ChildNodes['SubElementID'].Text   := IntToStr(LSubElementID);
        LRootNode.ChildNodes['ResultTypeID'].Text   := IntToStr(0);
        Result := TRUE;
      end;
    end
    else
    begin
      LIndex := CbxResultType.ItemIndex;
      if (LIndex >= 0) then
      begin
        LResultTypeID := Integer(CbxResultType.Items.Objects[LIndex]);
        LRootNode.ChildNodes['ResultTypeID'].Text := IntToStr(LResultTypeID);
        Result := TRUE;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutputDlg.CbxResultTypeChange(Sender: TObject);
const OPNAME = 'THydroOutputDlg.CbxResultTypeChange';
begin
  try
    DisplayUnits;
    if (StoreXMLData(FALSE)) then
    begin
      FHydrologyModel.RefreshOutputDlg(FXMLDocumentOut.XML.Text);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutputDlg.DisplayUnits;
const OPNAME = 'THydroOutputDlg.CbxSectionTypeChange';
var
  LRootNode           : IXMLNode;
  LAllResultTypesNode : IXMLNode;
  LNode               : IXMLNode;
  LResultTypeID       : Integer;
  LIndex              : Integer;
  LFound              : Boolean;
begin
  try
    LblUnits.Caption := '';
    FXMLDocumentIn.Active := TRUE;
    LRootNode             := FXMLDocumentIn.DocumentElement;
    LAllResultTypesNode   := LRootNode.ChildNodes['AllHydroResultTypes'];

    LIndex := CbxResultType.ItemIndex;
    if (LIndex >= 0) then
    begin
      LResultTypeID := Integer(CbxResultType.Items.Objects[LIndex]);
      LFound := FALSE;
      LIndex := 0;
      while ((NOT LFound) AND (LIndex < LAllResultTypesNode.ChildNodes.Count)) do
      begin
        LNode   := LAllResultTypesNode.ChildNodes.Get(LIndex);
        if (LResultTypeID = StrToInt(LNode.ChildNodes['ResultTypeID'].Text)) then
        begin
          LblUnits.Caption := LNode.ChildNodes['Units'].Text;
          LFound := TRUE;
        end
        else
          LIndex := LIndex + 1;

      end;
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutputDlg.CbxSectionTypeChange(Sender: TObject);
const OPNAME = 'THydroOutputDlg.CbxSectionTypeChange';
begin
  try
    if (StoreXMLData(TRUE)) then
    begin
      FHydrologyModel.RefreshOutputDlg(FXMLDocumentOut.XML.Text);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroOutputDlg.FormClose(Sender: TObject; var Action: TCloseAction);
const OPNAME = 'THydroOutputDlg.FormClose';
begin
  try
    FHydrologyModel.FreeOutputDlg;
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
