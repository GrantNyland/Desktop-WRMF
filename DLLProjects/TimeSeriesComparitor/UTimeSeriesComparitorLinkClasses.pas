//
//
//  UNIT      : Contains link classes used by the TimeSeries Comparitor.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/03/06
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UTimeSeriesComparitorLinkClasses;

interface
uses
  classes,
  vcl.Controls,
  UTimeSeriesComparitorViewList,
  UTimeSeriesComparitorChartList,
  UTimeSeriesComparitorSeriesList;


const
  PanelBevelInnerActive = bvRaised;
  PanelBevelOuterActive = bvNone;
  PanelBevelInnerInActive = bvNone;
  PanelBevelOuterInActive = bvNone;
  pnlHeight = 30;
  pnlSpace  = 2;
  btnSpace  = 8;


type
  TChartSeriesType = (cstLineLabels, cstLineDateTime, cstLineXY);

  //TOnAddName = function(AName: String):boolean of object;
  //TOnSelectName = function(AName: String):boolean of object;
  TOnSelectSeries = function(ASeries: TTimeSeriesComparitorSeries):boolean of object;
  TOnSelectChart = function(AChart: TTimeSeriesComparitorChart):boolean of object;
  TOnSelectView = function(AView: TTimeSeriesComparitorView):boolean of object;
  //TOnDeleteName = function(AName: String):boolean of object;
  //TOnRenameName = function(AOldName,ANewName: String):boolean of object;
implementation


end.
