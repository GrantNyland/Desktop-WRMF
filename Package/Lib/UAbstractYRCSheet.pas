//
//
//  UNIT      : Contains TAbstractYRCSheet Class
//  AUTHOR    : Dziedzi Ramulondi (PDNA)
//  DATE      : 2002/08/22
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAbstractYRCSheet;
                                          
interface

uses
  Classes,
  VCLTee.Chart,
  VCL.ComCtrls,
  UYRCChart,
  UHelpContexts,
  UMenuItemManager,
  UAbstractObject,
  UAbstractComponent,
  UAbstractModelData;

type

  TAbstractYRCSheet = class(TAbstractTabSheet)
  protected
    FChart: TYRCChart;
    // Overriden from TAbstractTabSheet
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure ClearDataViewer; virtual;
    function GetMenuItemManager: TMenuItemManager;virtual; abstract;
  public
    procedure TogglePlaneMode;virtual;
    procedure ToggleChartMode;virtual;
    procedure ToggleCurveManipulation;virtual;    
    procedure ResetChartData;virtual;
    procedure LoadChart;virtual;
    function CanCopyToCLipboard: boolean; override;
    function CanExport: boolean; override;
    function CanPrint: boolean; override;
    procedure DoPrint; override;

    property Chart : TYRCChart Read FChart;
    property MenuItemManager: TMenuItemManager read GetMenuItemManager;
  end;

implementation

uses
  SysUtils,
  vcl.Controls,
  vcl.Graphics,
  Windows,
  Types,
  vcl.Clipbrd,
  vcl.Printers,
  VCLTee.TeEngine,
  UStudyArea,
  //UOutputDataChart,
  UMainMenuEventType,
  UErrorHandlingOperations;

procedure TAbstractYRCSheet.CreateMemberObjects;
const OPNAME = 'TAbstractYRCSheet.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FTabCaptionKey := 'Result Graph';
    FChart := nil;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAbstractYRCSheet.DestroyMemberObjects;
const OPNAME = 'TAbstractYRCSheet.DestroyMemberObjects';
begin
  try
    if(FChart <> nil) then
    begin
      FChart.Parent := nil;
      FreeAndNil(FChart);
    end;
    inherited DestroyMemberObjects;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAbstractYRCSheet.ClearDataViewer;
const OPNAME = 'TAbstractYRCSheet.ClearDataViewer';
begin
  try
    if Assigned(FChart) then
    begin
      FChart.Parent := nil;
      FreeAndNil(FChart);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TAbstractYRCSheet.CanCopyToClipboard: boolean;
const OPNAME = 'TAbstractYRCSheet.CanCopyToClipboard';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractYRCSheet.CanExport: boolean;
const OPNAME = 'TAbstractYRCSheet.CanExport';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TAbstractYRCSheet.CanPrint: boolean;
const OPNAME = 'TAbstractYRCSheet.CanPrint';
begin
  Result := False;
  try
    Result := Assigned(FChart) and (FChart.SeriesCount > 0);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TAbstractYRCSheet.DoPrint;
const OPNAME = 'TAbstractYRCSheet.DoPrint';
begin
  try
    if Assigned(FChart) and (Printer.Printers.Count > 0) then
    begin
      FChart.PrintMargins       := Rect(0,0,0,0); //TRectF(Rect(0,0,0,0));         //,0,0,0);  //Rect(0,0,0,0);
      FChart.PrintProportional  := False;
      FChart.Print;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TAbstractYRCSheet.ResetChartData;
const OPNAME = 'TAbstractYRCSheet.ResetChartData';
begin
  try
    //Do nothing here
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAbstractYRCSheet.TogglePlaneMode;
const OPNAME = 'TAbstractYRCSheet.TogglePlaneMode';
begin
  try
    //Do nothing here
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAbstractYRCSheet.ToggleChartMode;
const OPNAME = 'TAbstractYRCSheet.ToggleChartMode';
begin
  try
    //Do nothing here
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAbstractYRCSheet.LoadChart;
const OPNAME = 'TAbstractYRCSheet.LoadChart';
begin
  try
    //Do nothing here
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TAbstractYRCSheet.ToggleCurveManipulation;
const OPNAME = 'TAbstractYRCSheet.ToggleCurveManipulation';
begin
  try
    //Do nothing here
  except on E : Exception do HandleError(E,OPNAME); end;
end;

end.

