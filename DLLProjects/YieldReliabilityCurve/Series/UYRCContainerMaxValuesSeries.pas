//
//
//  UNIT      : Contains TYRCContainerMaxValuesSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 04/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerMaxValuesSeries;

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

  TYRCContainerMaxValuesSeries = class(TYRCContainerAbstractSeries)
  protected
    FYieldFormatStr: string;
    procedure CreateMemberObjects; override;
    function InitialiseSeries: boolean; override;
    procedure OnGetSeriesMarkText( Sender : TChartSeries ;  ValueIndex : Longint ;  Var MarkText : String );
  public
    function LanguageHasChanged: boolean; override;
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TYRCContainerMaxValuesSeries }

procedure TYRCContainerMaxValuesSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerMaxValuesSeries.CreateMemberObjects';
begin
  inherited;
  try
    FYieldFormatStr := '';
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerMaxValuesSeries.LanguageHasChanged: boolean;
const OPNAME = 'TYRCContainerMaxValuesSeries.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Result := Result and RedrawSeries(0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerMaxValuesSeries.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCContainerMaxValuesSeries.ShowSeries';
var
  LMaxValuesSeries: TLineSeries;
  LSeriesValues : TLoadCasesValues;
begin
  Result := inherited ShowSeries(AOwner,AYRCDataObject);
  try
    FYieldFormatStr := '';
    if Result and Assigned(AOwner) and Assigned(AYRCDataObject) then
    begin
      FYieldFormatStr := AYRCDataObject.YRCGeneralData.YieldFormatStr;
      LSeriesValues   := TLoadCasesValues.Create;
      LSeriesValues.CopyValues(AYRCDataObject.HorizontalSeriesValues);
      FLoadCases.AddValues(LSeriesValues);
      LMaxValuesSeries := TLineSeries.Create(AOwner);
      FLineSeriesList.AddObject('0',LMaxValuesSeries);
      AOwner.AddSeries(LMaxValuesSeries);
      //LMaxValuesSeries.ParentChart := AOwner;
      Result := InitialiseSeries;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerMaxValuesSeries.InitialiseSeries: boolean;
const OPNAME = 'TYRCContainerMaxValuesSeries.InitialiseSeries';
var
  LCount: integer;
  LMaxValuesSeries: TLineSeries;
  LXYZValue : TXYZValue;
  LSeriesValues : TLoadCasesValues;
begin
  Result := False;
  try
    LSeriesValues := FLoadCases.Values[0];
    LMaxValuesSeries := TLineSeries(FLineSeriesList.Objects[0]);
    if Assigned(LSeriesValues) and Assigned(LMaxValuesSeries) then
    begin
      LMaxValuesSeries.OnGetMarkText := OnGetSeriesMarkText;

      LMaxValuesSeries.Pointer.Brush.Color    := clBlack;
      LMaxValuesSeries.Pointer.InflateMargins := True;
      LMaxValuesSeries.Pointer.Style          := psCircle;
      LMaxValuesSeries.Pointer.Visible        := True;
      LMaxValuesSeries.LinePen.Width          := 2;
      LMaxValuesSeries.Marks.ArrowLength      := 8;
      LMaxValuesSeries.Marks.Style            := smsLabel;
      LMaxValuesSeries.Marks.Transparent      := True;
      LMaxValuesSeries.Marks.Visible          := True;
      LMaxValuesSeries.Marks.ArrowLength      := 32;
      LMaxValuesSeries.Marks.BackColor        := clWhite;
      LMaxValuesSeries.ShowInLegend           := False;

      for LCount := 0 to LSeriesValues.Count - 1 do
      begin
        LXYZValue := LSeriesValues.Values[LCount];
        if Assigned(LXYZValue) then
        begin
          LMaxValuesSeries.AddXY(LXYZValue.XValue,LXYZValue.YValue);
        end;
      end;
      Result := True;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerMaxValuesSeries.OnGetSeriesMarkText(Sender: TChartSeries; ValueIndex: Integer;
          var MarkText: String);
const OPNAME = 'TYRCContainerMaxValuesSeries.OnGetSeriesMarkText';
var
  LYieldMarkFormatStr: string;
begin
  try
    // Save values locally.
    LYieldMarkFormatStr := FAppModules.Language.GetString(FYieldFormatStr);
    if(Trim(LYieldMarkFormatStr) <> '') then
          MarkText := Format(LYieldMarkFormatStr,[Sender.YValues.Value[ValueIndex]]);

  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
