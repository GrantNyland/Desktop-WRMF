//
//
//  UNIT      : Contains TYRCContainerVerticalSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerVerticalSeries;

interface

uses
  Classes,
  Contnrs,
  Graphics,
  Series,
  Chart,
  TeEngine,
  TeeShape,
  UYRCDataObject,
  UAbstractObject,
  UYRCContainerAbstractSeries;

type

  TYRCContainerVerticalSeries = class(TYRCContainerAbstractSeries)
  protected
    FYearsLabelStr: string;
    procedure CreateMemberObjects; override;
    function InitialiseSeries: boolean; override;
    procedure DrawVerticalText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );

  public
    function LanguageHasChanged: boolean; override;
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TYRCContainerVerticalSeries }

procedure TYRCContainerVerticalSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerVerticalSeries.CreateMemberObjects';
begin
  inherited;
  try
    FYearsLabelStr := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerVerticalSeries.LanguageHasChanged: boolean;
const OPNAME = 'TYRCContainerVerticalSeries.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    //if Result then
    //  Result := DrawVerticalText;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerVerticalSeries.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCContainerVerticalSeries.ShowSeries';
var
  LSeriesValues : TLoadCasesValues;
  LVerticalSeries: TLineSeries;
  LCount: integer;
begin
  Result := inherited ShowSeries(AOwner, AYRCDataObject);
  try
    FYearsLabelStr := '';
    if Result and Assigned(AOwner) and Assigned(AYRCDataObject) then
    begin
      FYearsLabelStr := AYRCDataObject.YRCGeneralData.YearsFormatStr;
      LSeriesValues := TLoadCasesValues.Create;
      LSeriesValues.Sorted := False;
      LSeriesValues.CopyValues(AYRCDataObject.VerticalSeriesValues);
      FLoadCases.AddValues(LSeriesValues);

      for LCount := 0 to LSeriesValues.Count -1 do
      begin
        LVerticalSeries := TLineSeries.Create(AOwner);
        FLineSeriesList.AddObject(IntToStr(LCount),LVerticalSeries);
        AOwner.AddSeries(LVerticalSeries);
      end;

      LSeriesValues := TLoadCasesValues.Create;
      LSeriesValues.Sorted := False;
      LSeriesValues.CopyValues(AYRCDataObject.YearsValues);
      FLoadCases.AddValues(LSeriesValues);

      Result := InitialiseSeries;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerVerticalSeries.InitialiseSeries: boolean;
const OPNAME = 'TYRCContainerVerticalSeries.InitialiseSeries';
var
  LCount: integer;
  LVerticalSeries: TLineSeries;
  LXYZValue : TXYZValue;
  LSeriesValues : TLoadCasesValues;
begin
  Result := False;
  try
    LSeriesValues := FLoadCases.Values[0];
    if Assigned(LSeriesValues) then
    begin
      for LCount := 0 to LSeriesValues.Count - 1 do
      begin
        LXYZValue := LSeriesValues.Values[LCount];
        LVerticalSeries := TLineSeries(FLineSeriesList.Objects[LCount]);

        if Assigned(LVerticalSeries) and Assigned(LXYZValue) then
        begin
          LVerticalSeries.ShowInLegend := False;
          LVerticalSeries.Marks.Visible := True;
          LVerticalSeries.Marks.Transparent := True;
          LVerticalSeries.OnGetMarkText :=  DrawVerticalText;
          LVerticalSeries.AddXY(LXYZValue.XValue,0.0);
          LVerticalSeries.AddXY(LXYZValue.XValue,LXYZValue.YValue/10.0);
          LVerticalSeries.AddXY(LXYZValue.XValue,LXYZValue.YValue);
        end;
      end;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerVerticalSeries.DrawVerticalText( Sender: TChartSeries; ValueIndex: Integer; var MarkText: String);
const OPNAME = 'TYRCContainerVerticalSeries.DrawVerticalText';
var
  LCount,
  LIndex: integer;
  LVerticalSeries: TLineSeries;
  LXYZValue : TXYZValue;
  LYearsValues : TLoadCasesValues;
  LText: string;
  LXPos,
  LYPos: Longint;
  LOwnerChart: TChart;
begin
  try
    MarkText  := '';
    if (ValueIndex = 1) then
    begin
      LYearsValues := FLoadCases.Values[1];
      if Assigned(LYearsValues) and (Trim(FYearsLabelStr) <> '') then
      begin
        LIndex := -1;
        for LCount := 0 to FLineSeriesList.Count - 1 do
        begin
          LVerticalSeries := TLineSeries(FLineSeriesList.Objects[LCount]);
          if Assigned(LVerticalSeries)  and (LVerticalSeries = Sender)then
          begin
            LIndex := LCount;
            Break;
          end;
        end;
        if(LIndex >= 0) then
        begin
          LXYZValue := LYearsValues.Values[LIndex];
          if Assigned(LXYZValue) then
          begin
            LOwnerChart:= TChart(Sender.Owner);
            LXPos   := Sender.CalcXPosValue(Sender.XValue[ValueIndex]-1);
            LYPos   := LOwnerChart.BottomAxis.PosAxis - 80;

            LText  := FAppModules.Language.GetString(FYearsLabelStr);
            LText  := Format(LText,[LXYZValue.XValue,LXYZValue.YValue]);

            LOwnerChart.BottomAxis.DrawAxisLabel(LXPos,LYPos,90,LText);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
