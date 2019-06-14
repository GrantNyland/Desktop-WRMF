unit UHydroTimeSeriesDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, VCL.Graphics, VCL.Controls, VCL.Forms,
  VCL.Dialogs, VCL.StdCtrls, xmldom, XMLIntf, XMLDoc, VCL.ExtCtrls, VCL.Buttons, VCL.Menus,

  HydrologyCom_TLB,
  UXMLAgent,
  UXMLAgentHydroTimeSeries,
  VCL.ComCtrls,
  VCL.Grids,
  VCLTee.TeeProcs, VCLTee.TeEngine, VCLTee.Chart, VCLTee.Series,
  VclTee.TeeGDIPlus, Xml.Win.msxmldom;

type
  THydroTimeSeriesDlg = class(TForm)
    FXMLDocumentIn     : TXMLDocument;
    FXMLDocumentOut    : TXMLDocument;
    PgcHydroTimeSeries : TPageControl;
    TbsTable           : TTabSheet;
    TbsGraph           : TTabSheet;
    CrtHydroTimeSeries : TChart;
    LsrData            : TLineSeries;
    GrdHydroTimeSeries : TStringGrid;
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    function LoadXMLData : Boolean;
  public
    { Public declarations }
    FMayChangeNetwork : Boolean;
    FXMLAgent         : TXMLAgentHydroTimeSeries;
    FHydrologyModel   : IHydrologyModel;
    procedure PopulateGrid;
  end;

implementation

uses
  UErrorHandlingOperations;

{$R *.dfm}

procedure THydroTimeSeriesDlg.FormCreate(Sender: TObject);
const OPNAME = 'THydroTimeSeriesDlg.FormCreate';
begin
  try
    FXMLAgent := TXMLAgentHydroTimeSeries.Create;
    FXMLAgent.DlgXMLDocumentIn  := FXMLDocumentIn;
    FXMLAgent.DlgXMLDocumentOut := FXMLDocumentOut;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroTimeSeriesDlg.FormDestroy(Sender: TObject);
const OPNAME = 'THydroTimeSeriesDlg.FormDestroy';
begin
  try
    FreeAndNil(FXMLAgent);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroTimeSeriesDlg.FormShow(Sender: TObject);
const OPNAME = 'THydroTimeSeriesDlg.FormShow';
{var
  LCol   : Integer;
  LIndex : Integer;}
begin
  try
    PopulateGrid;
{
    GrdHydroTimeSeries.Cells[0, 0] := 'Year';
    for LCol := 1 to 12 do
    begin
      LIndex := (LCol + 9) mod 12;
      if (LIndex = 0) then
        LIndex := 12;
      GrdHydroTimeSeries.Cells[LCol, 0] := ShortMonthNames[LIndex];
    end;
    LoadXMLData;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydroTimeSeriesDlg.PopulateGrid;
const OPNAME = 'THydroTimeSeriesDlg.PopulateGrid';
var
  LCol   : Integer;
  LIndex : Integer;
begin
  try
    GrdHydroTimeSeries.Cells[0, 0] := 'Year';
    for LCol := 1 to 12 do
    begin
      LIndex := (LCol + 9) mod 12;
      if (LIndex = 0) then
        LIndex := 12;
      GrdHydroTimeSeries.Cells[LCol, 0] := FormatSettings.ShortMonthNames[LIndex];
    end;
    LoadXMLData;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydroTimeSeriesDlg.LoadXMLData: boolean;
const OPNAME = 'THydroTimeSeriesDlg.LoadXMLData';
var
  LRootNode           : IXMLNode;
  LAllResultTypesNode : IXMLNode;
  LIndex              : Integer;
  LNode               : IXMLNode;
  LSectionNode        : IXMLNode;
  LDataListNode       : IXMLNode;
  LTitleNode          : IXMLNode;
  LYear               : Integer;
  LMonthIndex         : Integer;
  LMonth              : Integer;
  LFieldName          : String;
  LValue              : Double;
  LLabel              : String;
  LXVal               : Integer;
  LCaption            : String;
  LRow                : Integer;
  LCol                : Integer;
begin
  Result := FALSE;
  try
    FXMLDocumentIn.Active  := TRUE;
    FXMLDocumentOut.Active := TRUE;
    LRootNode              := FXMLDocumentIn.DocumentElement;
    LAllResultTypesNode    := LRootNode.ChildNodes['AllHydroResultTypes'];

    LSectionNode  := LRootNode.ChildNodes['HydroTimeSeries'];
    LTitleNode    := LSectionNode.ChildNodes['Title'];
    LDataListNode := LSectionNode.ChildNodes['TimeSeries'];

    // Clear the grid
    for LRow := 1 to GrdHydroTimeSeries.RowCount - 1 do
      for LCol := 0 to GrdHydroTimeSeries.ColCount - 1 do
        GrdHydroTimeSeries.Cells[LCol, LRow] := '';
    GrdHydroTimeSeries.RowCount := 1 + LDataListNode.ChildNodes.Count;
    if (GrdHydroTimeSeries.RowCount > 1) then
      GrdHydroTimeSeries.FixedRows := 1;

    // Populate the grid and the graph  
    LsrData.Clear;
    LsrData.Title := LTitleNode.Text;
    CrtHydroTimeSeries.Title.Text.Clear;
    CrtHydroTimeSeries.Title.Text.Add(LCaption);
    for LIndex := 1 to LDataListNode.ChildNodes.Count do
    begin
      LNode := LDataListNode.ChildNodes.Get(LIndex-1);
      LYear := StrToInt(LNode.ChildNodes['Year'].Text);
      GrdHydroTimeSeries.Cells[0, LIndex] := IntToStr(LYear);
      for LMonthIndex := 1 to 12 do
      begin
        LFieldName := 'Month' + Format('%2.2d', [LMonthIndex]);
        LValue := StrToFloat(LNode.ChildNodes[LFieldName].Text);
        GrdHydroTimeSeries.Cells[LMonthIndex, LIndex] := LNode.ChildNodes[LFieldName].Text;
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

  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
