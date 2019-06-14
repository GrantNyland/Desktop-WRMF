//
//
//  UNIT      : Contains TYRCContainerLabelSeries Class
//  AUTHOR    : Dziedzi Ramulondi(PDNA)
//  DATE      : 03/09/2002
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UYRCContainerLabelSeries;

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

  TYRCContainerLabelSeries = class(TYRCContainerAbstractSeries)
  protected
    FLabelsStr : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    function InitialiseSeries: boolean; override;
  public
    function LanguageHasChanged: boolean; override;
    function ShowSeries(AOwner: TChart; AYRCDataObject:TYRCDataObject): boolean; override;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;


{ TYRCContainerLabelSeries }

procedure TYRCContainerLabelSeries.CreateMemberObjects;
const OPNAME = 'TYRCContainerLabelSeries.CreateMemberObjects';
begin
  inherited;
  try
    FLabelsStr := TStringList.Create;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TYRCContainerLabelSeries.DestroyMemberObjects;
const OPNAME = 'TYRCContainerLabelSeries.DestroyMemberObjects';
begin
  inherited;
  try
    FLabelsStr.Free;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerLabelSeries.LanguageHasChanged: boolean;
const OPNAME = 'TYRCContainerLabelSeries.LanguageHasChanged';
var
  LCount: integer;
  LLabelSeries: TChartShape;
begin
  Result := inherited LanguageHasChanged;
  try
    for LCount := 0 to FLineSeriesList.Count - 1 do
    begin
      LLabelSeries := TChartShape(FLineSeriesList.Objects[LCount]);
      if Assigned(LLabelSeries) and (Trim(FLabelsStr[LCount]) <> '') then
         LLabelSeries.Text.Text := FAppModules.Language.GetString(FLabelsStr[LCount]);
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerLabelSeries.ShowSeries(AOwner: TChart; AYRCDataObject: TYRCDataObject): boolean;
const OPNAME = 'TYRCContainerLabelSeries.ShowSeries';
var
  LLabelSeries: TChartShape;
begin
  Result := inherited ShowSeries(AOwner,AYRCDataObject);
  try
    FLabelsStr.Clear;

    if Result and Assigned(AOwner) and Assigned(AYRCDataObject) then
    begin
      FLabelsStr.Add(AYRCDataObject.YRCGeneralData.LabelStr);
      LLabelSeries := TChartShape.Create(AOwner);
      AOwner.AddSeries(LLabelSeries);
      FLineSeriesList.AddObject('0',LLabelSeries);
      Result := InitialiseSeries;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TYRCContainerLabelSeries.InitialiseSeries: boolean;
const OPNAME = 'TYRCContainerLabelSeries.InitialiseSeries';
var
  LCount: integer;
  LLabelSeries: TChartShape;
begin
  Result := False;
  try
    for LCount := 0 to FLineSeriesList.Count - 1 do
    begin
      LLabelSeries := TChartShape(FLineSeriesList.Objects[LCount]);
      if Assigned(LLabelSeries) then
      begin
        LLabelSeries.Style := chasRectangle;
        LLabelSeries.X0    := 97;
        LLabelSeries.X1    := 65;
        LLabelSeries.Y0    := 240;
        LLabelSeries.Y1    := 220;
        LLabelSeries.Brush.Color := clWhite;
        LLabelSeries.Marks.Visible := False;
        LLabelSeries.ShowInLegend := True;
        LLabelSeries.VertAxis := aBothVertAxis;
        LLabelSeries.Font.Style := [fsBold];
      end;
    end;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
