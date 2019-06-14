//
//
//  UNIT      : Contains TYRCFirmYieldChart Class
//  AUTHOR    : Dziedzi Ramulondi(Aravia)
//  DATE      : 19/07/2006
//  COPYRIGHT : Copyright © 2006 DWAF
//
//
unit UYRCFirmYieldChart;

interface

uses
  VCL.Controls,
  VCL.ComCtrls,
  VCL.Menus,
  VCL.Dialogs,
  Contnrs,
  VCL.Graphics,
  VCL.StdCtrls,
  Classes,
  VCLTee.Series,
  VCLTee.Chart,
  VCLTee.TeEngine,
  VCLTee.TeeProcs,
  UYRCSeries,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UAbstractYRCData,
  UYRCMenuItemManager,
  UYRCContainerTargetDraftVerticalSeries;

type
  TYRCFirmYieldChart = class(TFieldChart)
  protected
    FFirmYieldLines,
    FRecurenceIntervalLines: TObjectList;
    FSelectedFirmYieldSeries: TLineSeries;
    FSeriesLabels: TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function YRCGraphDataObject:TAbstractYRCGraphDataObject;
    function  SetChartProperties: boolean;
    procedure OnGetRISeriesMarkText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
    procedure OnGetFirmYieldSeriesMarkText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
  public
    function LanguageHasChanged: boolean; override;
    function InitialiseChart: boolean;
    procedure SetChartMaxYValue(AValue: double);
    procedure ClassifySeries;
    function SetAssuranceIntervals: boolean;
    function SelectFirmYieldSeries(ASeries: TLineSeries):boolean;
    function DeleteSelectFirmYieldSeries:boolean;
    function AddFirmYieldSeries(ACSVFilename:string):boolean;
    function LoadChartLabels(AChartFileName: string): boolean;
    function SaveChartLabels(AChartFileName: string): boolean;
    property SelectedFirmYieldSeries: TLineSeries read FSelectedFirmYieldSeries;
  end;

implementation

uses
  System.Types,
  Math,
  SysUtils,
  Windows,
  UITypes,
  VCL.Clipbrd,
  VCL.Printers,
  UUtilities,
  UAbstractModelData,
  UAbstractFileNamesObject,
  UAbstractYRCModelDataObject,
  UErrorHandlingOperations;

procedure TYRCFirmYieldChart.CreateMemberObjects;
const OPNAME = 'TYRCFirmYieldChart.CreateMemberObjects';
begin
  inherited;
  try
    FFirmYieldLines         := TObjectList.Create(False);
    FRecurenceIntervalLines := TObjectList.Create(False);
    FSeriesLabels           := TStringList.Create;
    FSelectedFirmYieldSeries         := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCFirmYieldChart.DestroyMemberObjects;
const OPNAME = 'TYRCFirmYieldChart.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
    FreeAndNil(FFirmYieldLines);
    FreeAndNil(FRecurenceIntervalLines);
    FreeAndNil(FSeriesLabels);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCFirmYieldChart.ClassifySeries;
const OPNAME = 'TYRCFirmYieldChart.ClassifySeries';
var
  LIndex: integer;
  LSeries: TLineSeries;
begin
  try
    FFirmYieldLines.Clear;
    FRecurenceIntervalLines.Clear;
    LIndex := Self.SeriesList.Count-1;
    while (LIndex >= 0) do
    begin
      if(SeriesList.Items[LIndex] is TLineSeries) then
      begin
        LSeries := TLineSeries(SeriesList.Items[LIndex]);
        if(Pos('FirmYield',LSeries.Title) = 1) then
        begin
          FFirmYieldLines.Add(LSeries);
          LSeries.ShowInLegend := False;
          LSeries.OnGetMarkText := OnGetFirmYieldSeriesMarkText;
        end
        {else if(Pos('RecurenceInterval',LSeries.Title) = 1) then
        begin
          FRecurenceIntervalLines.Add(LSeries);
          LSeries.ShowInLegend := False;
          //LSeries.Marks.Visible := False;
          LSeries.OnGetMarkText := OnGetRISeriesMarkText;
        end}
        else
        begin
          SeriesList.Remove(SeriesList.Items[LIndex]);
        end;
      end
      else
      begin
        SeriesList.Remove(SeriesList.Items[LIndex]);
      end;
      LIndex := LIndex -1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.InitialiseChart: boolean;
const OPNAME = 'TYRCFirmYieldChart.InitialiseChart';
begin
  Result := False;
  try
    FSelectedFirmYieldSeries := nil;
    Self.RemoveAllSeries;
    Result := SetChartProperties;
    FFirmYieldLines.Clear;
    FRecurenceIntervalLines.Clear;
    LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.SetChartProperties: boolean;
const OPNAME = 'TYRCFirmYieldChart.SetChartProperties';
begin
  Result := False;
  try

    // Set chart properties.
    Self.View3D := False;
    Self.Color  := clWindow;

    // Set chart margins.
    Self.MarginBottom := YRCGraphDataObject.YRCChartProperties.MarginBottom;
    Self.MarginLeft   := YRCGraphDataObject.YRCChartProperties.MarginLeft;
    Self.MarginRight  := YRCGraphDataObject.YRCChartProperties.MarginRight;
    Self.MarginTop    := YRCGraphDataObject.YRCChartProperties.MarginTop;

    // Set chart BottomAxis.
    Self.BottomAxis.Automatic        := True;
    Self.BottomAxis.AutomaticMaximum := False;
    Self.BottomAxis.AutomaticMinimum := False;
    Self.BottomAxis.ExactDateTime    := False;
    //Self.BottomAxis.Increment        := YRCGraphDataObject.YRCChartProperties.BottomAxisIncrement;
    Self.BottomAxis.Maximum          := YRCGraphDataObject.YRCChartProperties.BottomAxisMaximum;
    Self.BottomAxis.MinorTickCount   := YRCGraphDataObject.YRCChartProperties.BottomAxisMinorTickCount;
    Self.BottomAxis.Visible          := True;

    // Set chart LeftAxis.
    //Self.LeftAxis.Automatic          := True;
    Self.LeftAxis.AutomaticMaximum   := False;
    Self.LeftAxis.AutomaticMinimum   := False;
    Self.LeftAxis.Minimum            := 0.0;
    //Self.LeftAxis.LabelsSeparation   := 20;

    //Self.LeftAxis.ExactDateTime      := False;
    //Self.LeftAxis.Increment          := YRCGraphDataObject.YRCChartProperties.LeftAxisIncrement;
    //Self.LeftAxis.Maximum            := YRCGraphDataObject.YRCChartProperties.LeftAxisMaximum;
    Self.LeftAxis.MinorTickCount     := YRCGraphDataObject.YRCChartProperties.LeftAxisMinorTickCount;
    //Self.LeftAxis.Visible            := True;
    Self.LeftAxis.AxisValuesFormat   := '######0.###';

    // Set chart LeftAxis.
    Self.RightAxis.Automatic         := False;
    Self.RightAxis.AutomaticMaximum  := False;
    Self.RightAxis.AutomaticMinimum  := False;
    Self.RightAxis.ExactDateTime     := False;
    Self.RightAxis.Increment         := YRCGraphDataObject.YRCChartProperties.RightAxisIncrement;
    Self.RightAxis.Maximum           := YRCGraphDataObject.YRCChartProperties.RightAxisMaximum;
    Self.RightAxis.MinorTickCount    := YRCGraphDataObject.YRCChartProperties.RightAxisMinorTickCount;
    Self.RightAxis.Visible           := True;


    // Set chart Legend.
    Self.Legend.Alignment            := laBottom;
    Self.Legend.TextStyle            := ltsPlain;
    Self.Legend.TopPos               := YRCGraphDataObject.YRCChartProperties.LegendTopPos;
    Self.Legend.VertMargin           := YRCGraphDataObject.YRCChartProperties.LegendVertMargin;
    Self.Legend.Color                := clWhite;
    Self.Legend.ColorWidth           := 0;

    // Set chart title.
    Self.Title.Alignment             := taCenter;

    Result := True;

  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.SetAssuranceIntervals: boolean;
const OPNAME = 'TYRCFirmYieldChart.SetAssuranceIntervals';
var
  LIndex: integer;
  LSeries: TLineSeries;
  LPlane:TAbstractYRCPlane;
  LPointValues: TYRCPoint;
begin
  Result := False;
  try
    for LIndex := 0 to FRecurenceIntervalLines.Count -1 do
    begin
      LSeries := TLineSeries(FRecurenceIntervalLines.Items[LIndex]);
      LSeries.ParentChart := nil;
      LSeries.Free;
    end;
    FRecurenceIntervalLines.Clear;

    if (YRCGraphDataObject.PlanesCount > 0) then
    begin
      LPlane := TAbstractYRCPlane(YRCGraphDataObject.YRCPlane[YRCGraphDataObject.PlaneIndex]);
      if Assigned(LPlane) then
      begin
        for LIndex := 0 to High(LPlane.AssurancePointArrayObject.YRCRecordPointArray) do
        begin
          LPointValues := LPlane.AssurancePointArrayObject.YRCRecordPointArray[LIndex];
          if (LPointValues.YValue > 0.000) then
          begin
            LSeries := TLineSeries.Create(Self);
            FRecurenceIntervalLines.Add(LSeries);
            Self.AddSeries(LSeries);
            LSeries.Title := 'RecurenceInterval'+ IntToStr(FRecurenceIntervalLines.Count);
            LSeries.Active                 := True;
            LSeries.XValues.Order          := loNone;
            LSeries.Marks.Visible          := True;
            LSeries.Marks.Clip             := True;
            LSeries.ShowInLegend           := False;
            LSeries.SeriesColor            := clRed;
            LSeries.OnGetMarkText          := OnGetRISeriesMarkText;
            LSeries.AddXY(LPointValues.XValue,0);
            LSeries.AddXY(LPointValues.XValue,Self.LeftAxis.Maximum + (Self.LeftAxis.Maximum * 0.15));
          end;
        end;
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCFirmYieldChart.SetChartMaxYValue(AValue: double);
const OPNAME = 'TYRCFirmYieldChart.SetChartMaxYValue';
var
  LIndex: integer;
  LSeries: TLineSeries;
begin
  try
    Self.LeftAxis.Maximum := AValue;
    for LIndex := 0 to FRecurenceIntervalLines.Count -1 do
    begin
      LSeries := TLineSeries(FRecurenceIntervalLines.Items[LIndex]);
      LSeries.YValue[1] := AValue + (AValue * 0.15)
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TYRCFirmYieldChart.YRCGraphDataObject: TAbstractYRCGraphDataObject;
const OPNAME = 'TYRCFirmYieldChart.YRCGraphDataObject';
begin
  Result := nil;
  try
    Result := TAbstractYRCModelDataObject(FAppModules.Model.ModelData).YRCGraphDataObject;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.SelectFirmYieldSeries(ASeries: TLineSeries): boolean;
const OPNAME = 'TYRCFirmYieldChart.SelectFirmYieldSeries';
var
  LIndex: integer;
  LSeries: TLineSeries;
  LSelected: boolean;
begin
  Result := False;
  try
    FSelectedFirmYieldSeries := nil;
    LSelected := Assigned(ASeries) and (ASeries.Pen.Width = 2);

    for LIndex := 0 to FFirmYieldLines.Count-1 do
    begin
      LSeries := TLineSeries(FFirmYieldLines.Items[LIndex]);
      LSeries.Pen.Width := 1;
      LSeries.Repaint;
    end;

    if (not Assigned(ASeries)) or LSelected  then Exit;

    if(FFirmYieldLines.IndexOf(ASeries) >= 0) then
    begin
      ASeries.Pen.Width := 2;
      FSelectedFirmYieldSeries := ASeries;
      ASeries.Pen.Width := 2;
      ASeries.Repaint;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.DeleteSelectFirmYieldSeries: boolean;
const OPNAME = 'TYRCFirmYieldChart.DeleteSelectFirmYieldSeries';
var
 LIndex: integer;
begin
  Result := False;
  try
    if(FSelectedFirmYieldSeries <> nil) then
    begin
      LIndex := SeriesList.IndexOf(FSelectedFirmYieldSeries);
      if(LIndex >= 0) then
      begin
        FSelectedFirmYieldSeries.ParentChart := nil;
        FFirmYieldLines.Remove(FSelectedFirmYieldSeries);
        FSelectedFirmYieldSeries.Free;
        FSelectedFirmYieldSeries := nil;
        if(LIndex < FSeriesLabels.Count) then
          FSeriesLabels.Delete(LIndex);
        Result := True;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.AddFirmYieldSeries(ACSVFilename: string): boolean;
const OPNAME = 'TYRCFirmYieldChart.AddFirmYieldSeries';
  var
  LPos,
  LXIndex,
  LYIndex: integer;
  LXValue,
  LYValue: double;
  LFileContents: TStringList;
  LLineContents: TStringList;
  LSeries: TLineSeries;
  LSeriesLabel: string;
begin
  Result := False;
  try
    if FileExists(ACSVFilename) then
    begin
      LFileContents   := TStringList.Create;
      LLineContents := TStringList.Create;
      try
        LFileContents.LoadFromFile(ACSVFilename);
        if(LFileContents.Count = 0) then
          ShowMessage('The selected file is empty.')
        else if(LFileContents.Count < 3) then
          ShowMessage('The selected file not a valid export of a firm yield series data. There must be at least 3 lines.')
        else
        begin
          LLineContents.CommaText := LFileContents.Strings[0];
          if(LLineContents.Count = 0) then
            ShowMessage('The selected file not a valid export of a firm yield series data. There first line must contain the heading for the data.')
          else
          begin
            LXIndex := -1;
            LYIndex := -1;
            for LPos := 0 to LLineContents.Count-1 do
            begin
              if (Pos('X',LLineContents.Strings[LPos]) in [1,2]) then
                LXIndex := LPos;
              if (Pos('FirmYield',LLineContents.Strings[LPos]) in [1,2]) then
                LYIndex := LPos;
            end;
            if(LXIndex = -1) then
              ShowMessage('The selected file not a valid export of a firm yield series data. There first line(Header) must contain a column for the X values named (X).')
            else if(LYIndex = -1) then
              ShowMessage('The selected file not a valid export of a firm yield series data. There first line(Header) must contain a column for the Y values named (FirmYield.....).')
            else
            begin
              if(FFirmYieldLines.Count = 0) then
                SetChartProperties;
              LSeries  := TLineSeries.Create(Self);
              LSeries.ParentChart := Self;
              LSeries.XValues.Order          := loNone;
              LSeries.SeriesColor            := clBlack;
              LSeries.Pointer.Brush.Color    := clBlack;
              LSeries.Pointer.InflateMargins := True;
              LSeries.Pointer.Style          := psCircle;
              LSeries.Pointer.Visible        := False;
              LSeries.ShowInLegend           := False;
              LSeries.Marks.Visible          := True;
              LSeries.Marks.Clip             := True;
              //LSeries.Marks.ArrowLength      := 6;
              LSeries.OnGetMarkText          := OnGetFirmYieldSeriesMarkText;

              LSeries.Title                  := 'FirmYield'+ IntToStr(FFirmYieldLines.Count+1);
              LPos := FFirmYieldLines.Add(LSeries);

              LSeriesLabel := InputBox('Specify the name of this series','Series Label:',LSeries.Title);
              if(LSeriesLabel = '') then
                LSeriesLabel := LSeries.Title;
              if(LPos < FSeriesLabels.Count) then
                FSeriesLabels[LPos] := LSeriesLabel
              else
                FSeriesLabels.Add(LSeriesLabel);

              for LPos := 1 to LFileContents.Count-1 do
              begin
                LLineContents.CommaText := LFileContents.Strings[LPos];
                if(LLineContents.Count > 0) and (LXIndex < LLineContents.Count) and (LYIndex < LLineContents.Count) then
                begin
                  LXValue := StrToFloat(LLineContents[LXIndex]);
                  LYValue := StrToFloat(LLineContents[LYIndex]);
                  LSeries.AddXY(LXValue,LYValue);
                end;
              end;
              if(LSeries.YValues.MaxValue > Self.LeftAxis.Maximum) then
               Self.LeftAxis.Maximum := LSeries.YValues.MaxValue;

            end;
          end;
        end;
      finally
        LFileContents.Free;
        LLineContents.Free;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.LanguageHasChanged: boolean;
const OPNAME = 'TYRCFirmYieldChart.LanguageHasChanged';
var
  LChartCaptionStr: TStringList;
  LFile: TAbstractModelFileName;
  LFileName,
  LChartCaption:string;
  LSeqCount,
  LPLottingBase,
  LPeriodLength: integer;
  LFieldProperty: TAbstractFieldProperty;
begin
  Result := False;
  try
    if (YRCGraphDataObject() = nil) then Exit;

    Result := inherited LanguageHasChanged;
    if Result and Assigned(FAppModules.FieldProperties()) then
    begin
      LFieldProperty := FAppModules.FieldProperties.FieldProperty('TYield1');
      Self.BottomAxis.Title.Caption := FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.BottomAxisCaption);
      if Assigned(LFieldProperty) then
        Self.LeftAxis.Title.Caption   := FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.LeftAxisCaption) +
        '('+LFieldProperty.FieldUnits+')'
      else
        Self.LeftAxis.Title.Caption   := FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.LeftAxisCaption);

      LChartCaption := '';
      LChartCaptionStr := TStringList.Create;
      try
        LChartCaptionStr.Add(FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.CaptionFormatStr1));
        LChartCaptionStr.Add(FAppModules.Language.GetString(YRCGraphDataObject.YRCLanguageStrings.CaptionFormatStr2));
        LChartCaption := LChartCaptionStr.Text;
      finally
        LChartCaptionStr.Free;
      end;


      if(Trim(LChartCaption) <> '') then
      begin

        LPLottingBase := YRCGraphDataObject.PlottingBase;
        LPeriodLength := YRCGraphDataObject.PeriodLength;
        LSeqCount     := YRCGraphDataObject.SequencesCount;
        LFileName     := '';
        LFile         := TAbstractModelData(FAppModules.Model.ModelData).FileNamesObject.GetSumOutFile;
        if(LFile <> nil) then
          LFileName     := ExtractFileName(LFile.ShortName);
        LChartCaption := Format(LChartCaption,[LFileName,LSeqCount,LPLottingBase,LPeriodLength]);
      end;

      Self.Title.Text.Text := LChartCaption;

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCFirmYieldChart.OnGetRISeriesMarkText(Sender: TChartSeries; ValueIndex: Integer;
 var MarkText: String);
const OPNAME = 'TYRCFirmYieldChart.OnGetRISeriesMarkText';
var
  LPointsWithSeries: array of integer;
  LYearsFormatStr,
  LYearsMarkFormatStr: string;
  LCount,
  LIndex: integer;
  LSeries: TChartSeries;
  LPlane:TAbstractYRCPlane;
  LPointValues: TYRCPoint;
  LZoomPerc,
  LYValue : Double;
  LXPos,
  LYPos: Longint;
begin
  try
    MarkText := '';
    if (ValueIndex <> 1) then
      Exit;

    if not Assigned(Sender.ParentChart) then
      Exit;

    if not Assigned(YRCGraphDataObject()) then
      Exit;

    LPlane := YRCGraphDataObject.SelectedPlane;
    if not Assigned(LPlane) then
      Exit;

    LIndex := 0;
    SetLength(LPointsWithSeries,Length(LPlane.AssurancePointArrayObject.YRCRecordPointArray));
    try
      for LCount := Low(LPointsWithSeries) to High(LPointsWithSeries) do
      begin
        LPointsWithSeries[LCount] := -1;
        if(LPlane.AssurancePointArrayObject.YRCRecordPointArray[LCount].YValue > 0.0) then
        begin
          LPointsWithSeries[LIndex] := LCount;
          LIndex := LIndex + 1;
        end;
      end;

      LIndex := -1;
      for LCount := 0 to FRecurenceIntervalLines.Count - 1 do
      begin
       LSeries := TChartSeries(FRecurenceIntervalLines.Items[LCount]);
       if(LSeries = Sender) then
       begin
         LIndex := LCount;
         Break;
       end;
      end;

      LYearsMarkFormatStr := '';
      LYearsFormatStr := YRCGraphDataObject.YRCLanguageStrings.YearsFormatStr;
      if (LIndex >= 0) and (Trim(LYearsFormatStr) <> '') and
         (LPointsWithSeries[LIndex] >= 0) then
      begin
        LPointValues := LPlane.AssuranceYearsArrayObject.YRCRecordPointArray[LPointsWithSeries[LIndex]];
        LYearsMarkFormatStr := FAppModules.Language.GetString(LYearsFormatStr);
        LYearsMarkFormatStr := Format(LYearsMarkFormatStr,[LPointValues.XValue,LPointValues.YValue]);
      end;

      if (LYearsMarkFormatStr <> '') then
      begin
        LYValue  := Sender.YValues.MaxValue - Sender.YValues.MinValue;
        LYValue  := LYValue/3.0;
        LZoomPerc := Sender.ParentChart.BottomAxis.Maximum - Sender.ParentChart.BottomAxis.Minimum;
        LZoomPerc := LZoomPerc/100;
        LXPos   := Sender.CalcXPosValue(Sender.XValue[ValueIndex] - LZoomPerc);
        //LYPos   := Sender.CalcYPosValue(Sender.XValue[ValueIndex]) - 80;
        LYPos   := Sender.CalcYPosValue(LYValue);
        Sender.ParentChart.BottomAxis.DrawAxisLabel(LXPos,LYPos,90,LYearsMarkFormatStr);
      end;
    finally
      Finalize(LPointsWithSeries);
    end;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYRCFirmYieldChart.OnGetFirmYieldSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TYRCFirmYieldChart.OnGetFirmYieldSeriesMarkText';
var
  LPos: integer;
begin
  try
    MarkText := '';
    if (ValueIndex <> (Sender.YValues.Count-1)) then
      Exit;
    LPos := FFirmYieldLines.IndexOf(Sender);
    if(LPos >= 0) and (LPos < FSeriesLabels.Count) then
      MarkText := FSeriesLabels[LPos]
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.LoadChartLabels(AChartFileName: string): boolean;
const OPNAME = 'TYRCFirmYieldChart.LoadChartLabels';
var
  LPos: integer;
  LFileExt,
  LFileName: string;
begin
  Result := False;
  try
    LFileName := ExtractFileName(AChartFileName);
    LFileExt  := ExtractFileExt(LFileName);
    if(LFileExt <> '') then
    begin
      LPos := Pos(LFileExt,LFileName);
      if(LPos > 1) then
        LFileName := Copy(LFileName,1,LPos-1);
    end;
    FSeriesLabels.Clear;
    FSeriesLabels.CommaText := FAppModules.ViewIni.ReadString(ClassName,LFileName,'');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TYRCFirmYieldChart.SaveChartLabels(AChartFileName: string): boolean;
const OPNAME = 'TYRCFirmYieldChart.SaveChartLabels';
var
  LPos: integer;
  LFileExt,
  LFileName: string;
begin
  Result := False;
  try
    LFileName := ExtractFileName(AChartFileName);
    LFileExt  := ExtractFileExt(LFileName);
    if(LFileExt <> '') then
    begin
      LPos := Pos(LFileExt,LFileName);
      if(LPos > 1) then
        LFileName := Copy(LFileName,1,LPos-1);
    end;
    FAppModules.ViewIni.WriteString(ClassName,LFileName,FSeriesLabels.CommaText);
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
