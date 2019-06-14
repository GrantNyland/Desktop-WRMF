
//
//
//  UNIT      : Contains     Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 24/07/2014
//  COPYRIGHT : Copyright © 2014 DWAF
//
//

unit UDDTSInputDataValidator;

interface
uses
  SysUtils,
  Classes,
  VCL.stdctrls,
  VCL.ComCtrls,
  VCL.Controls,
  VCL.extctrls,
  VCL.CheckLst,
  VCLTee.Chart,
  VCLTee.Series,
  VCL.Graphics,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,
  Math,
  VCL.Dialogs,
  VCL.Forms,
  Windows,
  Contnrs,

  UHelpContexts,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UDataComponent,
  UDDTSInputDataDialog,
  UDDTSData,
  UGenericModelLinkClasses;

type

  TDDTSInputDataValidator = class (TAbstractDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    function DDTSInputDataDialog : TDDTSInputDataDialog;
    procedure ClearGrid;
    procedure PopulateGrid;
    procedure RePopulateDataViewer;
    procedure GetChartLegend;
    procedure DoOnCheckBoxClick(Sender : TObject);

  public
    function Initialise: boolean; override;
    procedure PopulateDataViewer; override;
    procedure ClearDataViewer; override;
    function StudyDataHasChanged (AContext   : TChangeContext;
                                  AFieldName : String;
                                  AOldValue  : String;
                                  ANewValue  : String): Boolean; override;
    function StudyHasChanged: boolean; override;

    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoCopyToCLipboard; override;
    procedure DoExport(AFileName: string = ''); override;
    procedure DoPrint; override;
    procedure DoToggleTree(ASender : TObject);
    function LanguageHasChanged: Boolean; override;

  end;

implementation

uses
  DateUtils,
  VCL.ImgList,
  UDatasetType,
  VCL.Printers,
  UConstants,
  RainfallCom_TLB,
  UDDTSDataObject,
  UErrorHandlingOperations,
  VCL.Grids;

{ TDDTSInputDataValidator }

procedure TDDTSInputDataValidator.CreateMemberObjects;
const OPNAME = 'TDDTSInputDataValidator.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FPanel := TDDTSInputDataDialog.Create(nil, FAppModules);

    DDTSInputDataDialog.RunoffChkBox.OnClick := DoOnCheckBoxClick;
    DDTSInputDataDialog.OtherRunoffChkBox.OnClick := DoOnCheckBoxClick;
    DDTSInputDataDialog.RainChkBox.OnClick := DoOnCheckBoxClick;
    DDTSInputDataDialog.EvapChkBox.OnClick := DoOnCheckBoxClick;
    DDTSInputDataDialog.DSChkBox.OnClick := DoOnCheckBoxClick;
    DDTSInputDataDialog.EWRChkBox.OnClick := DoOnCheckBoxClick;

  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TDDTSInputDataValidator.DoOnCheckBoxClick(Sender : TObject);
const OPNAME = 'TDDTSInputDataValidator.DoOnCheckBoxClick';
begin
  try
    //RePopulateDataViewer;
    if DDTSInputDataDialog.RunoffChkBox.Checked then
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[1] := 50;
      if DDTSInputDataDialog.ElementsCount>0 then
        DDTSInputDataDialog.LineSeriesList[0].Visible := True;
    end
    else
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[1] := -1;
      if DDTSInputDataDialog.ElementsCount>0 then
        DDTSInputDataDialog.LineSeriesList[0].Visible := False;
    end;

     if DDTSInputDataDialog.OtherRunoffChkBox.Checked then
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[2] := 50;
      if DDTSInputDataDialog.ElementsCount>1 then
        DDTSInputDataDialog.LineSeriesList[1].Visible := True;
    end
    else
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[2] := -1;
      if DDTSInputDataDialog.ElementsCount>1 then
        DDTSInputDataDialog.LineSeriesList[1].Visible := False;
    end;

     if DDTSInputDataDialog.RainChkBox.Checked then
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[3] := 50;
      if DDTSInputDataDialog.ElementsCount>2 then
        DDTSInputDataDialog.LineSeriesList[2].Visible := True;
    end
    else
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[3] := -1;
      if DDTSInputDataDialog.ElementsCount>2 then
        DDTSInputDataDialog.LineSeriesList[2].Visible := False;
    end;

     if DDTSInputDataDialog.EvapChkBox.Checked then
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[4] := 80;
      if DDTSInputDataDialog.ElementsCount>3 then
        DDTSInputDataDialog.LineSeriesList[3].Visible := True;
    end
    else
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[4] := -1;
      if DDTSInputDataDialog.ElementsCount>3 then
        DDTSInputDataDialog.LineSeriesList[3].Visible := False;
    end;

     if DDTSInputDataDialog.DSChkBox.Checked then
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[5] := 85;
      if DDTSInputDataDialog.ElementsCount>4 then
        DDTSInputDataDialog.LineSeriesList[4].Visible := True;
    end
    else
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[5] := -1;
      if DDTSInputDataDialog.ElementsCount>4 then
        DDTSInputDataDialog.LineSeriesList[4].Visible := False;
    end;

      if DDTSInputDataDialog.EWRChkBox.Checked then
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[6] := 50;
      if DDTSInputDataDialog.ElementsCount>5 then
        DDTSInputDataDialog.LineSeriesList[5].Visible := True;
    end
    else
    begin
      DDTSInputDataDialog.InputDataGrid.ColWidths[6] := -1;
      if DDTSInputDataDialog.ElementsCount>5 then
        DDTSInputDataDialog.LineSeriesList[5].Visible := False;
    end;

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TDDTSInputDataValidator.DestroyMemberObjects;
const OPNAME = 'TDDTSInputDataValidator.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputDataValidator.Initialise: boolean;
const OPNAME = 'TDDTSInputDataValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  DDTSInputDataDialog.RunoffChkBox.Checked := True;
    DDTSInputDataDialog.OtherRunoffChkBox.Checked := True;
    DDTSInputDataDialog.RainChkBox.Checked := True;
    DDTSInputDataDialog.EvapChkBox.Checked := True;
    DDTSInputDataDialog.DSChkBox.Checked := True;
    DDTSInputDataDialog.EWRChkBox.Checked := True;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TDDTSInputDataValidator.LanguageHasChanged: Boolean;
const OPNAME = 'TDDTSInputDataValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Input Data';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
function TDDTSInputDataValidator.DDTSInputDataDialog : TDDTSInputDataDialog;
const OPNAME = 'TDDTSInputDataValidator.DDTSInputDataDialog';
begin
  Result := nil;
  try
    if (FPanel <> nil) then
      Result := TDDTSInputDataDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputDataValidator.StudyHasChanged: boolean;
const OPNAME = 'TDDTSInputDataValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    PopulateDataViewer;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputDataValidator.StudyDataHasChanged (AContext   : TChangeContext;
                                                           AFieldName : string;
                                                           AOldValue  : string;
                                                           ANewValue  : string): Boolean;
const OPNAME = 'TDDTSInputDataValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext, AFieldName, AOldValue, ANewValue);
  try
    PopulateDataViewer;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TDDTSInputDataValidator.PopulateDataViewer;
const OPNAME = 'TDDTSInputDataValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputDataValidator.RePopulateDataViewer;
const OPNAME = 'TDDTSInputDataValidator.RePopulateDataViewer';
var
  LDDTSDetailData : TDDTSDetailData;
  LDDTSInputData : TDDTSInputData;
  LIndex : integer;
  LLineSeries : TLineSeries;
  LSeriesLabel : string;
begin
  try
   if(FIdentifier >= 0) then
    begin
      LDDTSDetailData :=   TDDTSDataObject(FAppModules.Model.ModelData).DDTSDamDataList.GetDDTSDetailDataByIdentifier(FIdentifier);
      with DDTSInputDataDialog do
      begin
        ClearSeries;
        ClearGrid;
        InputDataGrid.RowCount := InputDataGrid.RowCount+LDDTSDetailData.GetDDTSInputDataCount-1;
        SetSeriesCount(6);
        for LIndex := 0 to LDDTSDetailData.GetDDTSInputDataCount-1 do
        begin
          LDDTSInputData := LDDTSDetailData.GetDDTSInputDataByIndex(LIndex);

          if LDDTSInputData <> nil then
          begin
            InputDataGrid.Cells[0,LIndex+1] := DateToStr(LDDTSInputData.DailyDate);

            if RunoffChkBox.Checked then
            begin
              InputDataGrid.ColWidths[1] := 50;
              InputDataGrid.Cells[1,LIndex+1] := FloatToStr(LDDTSInputData.Runoff);
              LLineSeries := LineSeriesList[0];
              LSeriesLabel := 'RUNOFF';
              if LLineSeries <> nil then
              begin
                LLineSeries.AddXY(LDDTSInputData.DailyDate, LDDTSInputData.Runoff);
                LLineSeries.Title := LSeriesLabel;
              end;
            end
            else
              InputDataGrid.ColWidths[1] := -1;

            if OtherRunoffChkBox.Checked then
            begin
              InputDataGrid.ColWidths[2] := 50;
              InputDataGrid.Cells[2,LIndex+1] := FloatToStr(LDDTSInputData.OtherInflow);
              LLineSeries := LineSeriesList[1];
              LSeriesLabel := 'OTHER RUNOFF';
              if LLineSeries <> nil then
              begin
                LLineSeries.AddXY(LDDTSInputData.DailyDate, LDDTSInputData.OtherInflow);
                LLineSeries.Title := LSeriesLabel;
              end;
            end
            else
              InputDataGrid.ColWidths[2] := -1;

            if RainChkBox.Checked then
            begin
              InputDataGrid.ColWidths[3] := 50;
              InputDataGrid.Cells[3,LIndex+1] := FloatToStr(LDDTSInputData.Rainfall);
              LLineSeries := LineSeriesList[2];
              LSeriesLabel := 'RAINFALL';
              if LLineSeries <> nil then
              begin
                LLineSeries.AddXY(LDDTSInputData.DailyDate, LDDTSInputData.Rainfall);
                LLineSeries.Title := LSeriesLabel;
              end;
            end  else
              InputDataGrid.ColWidths[3] := -1;

            if EvapChkBox.Checked then
            begin

              InputDataGrid.ColWidths[4] := 80;
              InputDataGrid.Cells[4,LIndex+1] := FloatToStr(LDDTSInputData.Evaporation);
              LLineSeries := LineSeriesList[3];
              LSeriesLabel := 'EVAPORATION';
              if LLineSeries <> nil then
              begin
                LLineSeries.AddXY(LDDTSInputData.DailyDate, LDDTSInputData.Evaporation);
                LLineSeries.Title := LSeriesLabel;
              end;
            end else
              InputDataGrid.ColWidths[4] := -1;
            if DSChkBox.Checked then
            begin
              InputDataGrid.ColWidths[5] := 85;
              InputDataGrid.Cells[5,LIndex+1] := FloatToStr(LDDTSInputData.IncreamentalRunoff);
              LLineSeries := LineSeriesList[4];
              LSeriesLabel := 'INCREAMENTAL RUNOFF';
              if LLineSeries <> nil then
              begin
                LLineSeries.AddXY(LDDTSInputData.DailyDate, LDDTSInputData.IncreamentalRunoff);
                LLineSeries.Title := LSeriesLabel;
              end;
            end  else
              InputDataGrid.ColWidths[5] := -1;


            if EWRChkBox.Checked then
            begin
              InputDataGrid.ColWidths[6] := 50;
              InputDataGrid.Cells[6,LIndex+1] := FloatToStr(LDDTSInputData.EWR);
              LLineSeries := LineSeriesList[5];
              LSeriesLabel := 'EWR';
              if LLineSeries <> nil then
              begin
                LLineSeries.AddXY(LDDTSInputData.DailyDate, LDDTSInputData.EWR);
                LLineSeries.Title := LSeriesLabel;
              end;
            end  else
              InputDataGrid.ColWidths[6] := -1;


          end;
        end;
        if LDDTSDetailData.GetDDTSInputDataCount>0 then
          GetChartLegend;

      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDDTSInputDataValidator.ClearGrid;
const OPNAME = 'TDDTSInputDataValidator.ClearGrid';
var
  LIndex    : integer;
  LRowIndex : integer;
begin
  try
    for LRowIndex := 1 to DDTSInputDataDialog.InputDataGrid.RowCount- 1 do
    begin
      for LIndex := 0 to DDTSInputDataDialog.InputDataGrid.ColCount - 1 do
      begin
        DDTSInputDataDialog.InputDataGrid.Cells[LIndex, LRowIndex] := '';
        DDTSInputDataDialog.InputDataGrid.Objects[LIndex, LRowIndex] := nil;
      end;
    end;
    DDTSInputDataDialog.InputDataGrid.RowCount := 2;
    DDTSInputDataDialog.InputDataGrid.ColCount := 7;
    DDTSInputDataDialog.InputDataGrid.Cells[0,0] := 'DATE';
    DDTSInputDataDialog.InputDataGrid.Cells[1,0] := 'RUNOFF';
    DDTSInputDataDialog.InputDataGrid.Cells[2,0] := 'OTHER INFLOW';
    DDTSInputDataDialog.InputDataGrid.Cells[3,0] := 'RAINFALL';
    DDTSInputDataDialog.InputDataGrid.Cells[4,0] := 'EVAPORATION';
    DDTSInputDataDialog.InputDataGrid.Cells[5,0] := 'INCREAMENTAL RUNOFF';
    DDTSInputDataDialog.InputDataGrid.Cells[6,0] := 'EWR TARGET';


  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputDataValidator.PopulateGrid;
const OPNAME = 'TDDTSInputDataValidator.PopulateGrid';
begin
  try

    ClearGrid;
    GetChartLegend;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TDDTSInputDataValidator.GetChartLegend;
const OPNAME = 'TDDTSInputDataValidator.GetChartLegend';
var
  LIndex : integer;
  LCount : integer;
begin
  try
    DDTSInputDataDialog.InputDataGraph.Legend.Visible := False;
    LCount := 0;
    for LIndex := 0 to DDTSInputDataDialog.InputDataGraph.SeriesCount-1 do
    begin
       if (DDTSInputDataDialog.InputDataGraph.Series[LIndex] is TLineSeries) and
          (DDTSInputDataDialog.InputDataGraph.Series[LIndex].Title<>'')then
       begin
           DDTSInputDataDialog.InputDataGraph.Series[LIndex].ShowInLegend := True;
           DDTSInputDataDialog.InputDataGraph.Series[LIndex].Color        := C_Colors[LCount];
           LCount := LCount+1;
       end
       else
         DDTSInputDataDialog.InputDataGraph.Series[LIndex].ShowInLegend := False;

       if not (DDTSInputDataDialog.InputDataGraph.Series[LIndex] is TLineSeries) then
         DDTSInputDataDialog.InputDataGraph.Series[LIndex].ShowInLegend := False;


    end;

    if (LCount >= 1) then
    begin
      DDTSInputDataDialog.InputDataGraph.Legend.Visible := True;
      DDTSInputDataDialog.InputDataGraph.Legend.Alignment := laBottom;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDDTSInputDataValidator.CanPrint: Boolean;
const OPNAME = 'TDDTSInputDataValidator.CanPrint';
begin
  Result := True;
  try

  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDDTSInputDataValidator.CanCopyToClipboard: Boolean;
const OPNAME = 'TDDTSInputDataValidator.CanCopyToClipboard';
begin
  Result := True;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TDDTSInputDataValidator.CanExport: Boolean;
const OPNAME = 'TDDTSInputDataValidator.CanExport';
begin
  Result := True;
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDDTSInputDataValidator.DoPrint;
const OPNAME = 'TDDTSInputDataValidator.DoPrint';
begin
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDDTSInputDataValidator.DoCopyToClipboard;
const OPNAME = 'TDDTSInputDataValidator.DoCopyToClipboard';
begin
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDDTSInputDataValidator.DoExport(AFileName: string = '');
const OPNAME = 'TDDTSInputDataValidator.DoExport';
begin
  try
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TDDTSInputDataValidator.DoToggleTree;
const OPNAME = 'TDDTSInputDataValidator.DoToggleTree';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TDDTSInputDataValidator.ClearDataViewer;
const OPNAME = 'TDDTSInputDataValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    ClearGrid;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
