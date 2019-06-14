//
//
//  UNIT      : Contains TWaterUseProportioningDialog  Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UWaterUseProportioningDialog;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.Graphics,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TWaterUseProportioningDialog = class(TAbstractScrollablePanel)
  protected
    FChannelCountLabel      : TLabel;
    FProportionWaterUseLabel : TLabel;
    FProportionWaterUseGrid  : TFieldStringGrid;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure OnColumnResize(ASender: TObject; ACol: integer);
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetWaterUseCategoryCount ( AChannelCount, ACategoryCount : integer );
    property ProportionWaterUseGrid   : TFieldStringGrid read FProportionWaterUseGrid;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  VCL.Forms,
  VCL.Grids,
  UErrorHandlingOperations,
  UControlCreationUtilities;


procedure TWaterUseProportioningDialog.CreateMemberObjects;
const OPNAME = 'TWaterUseProportioningDialog.CreateMemberObjects';
begin
  inherited;
  try
    //                                                                 Left  Top  Width Height
    FProportionWaterUseLabel  := CreateFieldLabel   (ControlsOwner, ControlsParent,  10,  40, 190, 40);
    FProportionWaterUseGrid   := CreateFieldStringGrid(FAppModules, ControlsOwner, ControlsParent,  10, 80, 500, 176, 1, TRUE);
    with FProportionWaterUseGrid do
    begin
      BorderStyle      := bsNone;
      ColCount         := 26;
      RowCount         := 2;
      FixedRows        := 1;
      FixedCols        := 1;
      DefaultRowHeight := 20;
      DefaultColWidth  := 40;
      ColWidths[0]     := 200;
      Options          := [goColSizing,goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goEditing];
    end;
    FProportionWaterUseGrid.AutoSizeFixedCols := True;
    FProportionWaterUseGrid.OnColumnResize := Self.OnColumnResize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningDialog.Resize;
const OPNAME = 'TWaterUseProportioningDialog.Resize';
var
  LFixedWidth,
  LIndex,
  LWidth: integer;
begin
  inherited Resize;
  try
    FProportionWaterUseGrid.Align      := alNone;
    FProportionWaterUseLabel.Align     := alNone;

    FProportionWaterUseLabel.Align     := alTop;
    FProportionWaterUseLabel.Alignment := taCenter;
    FProportionWaterUseLabel.Height    := 30;

    FProportionWaterUseGrid.Align      := alTop;
    FProportionWaterUseGrid.Height := ControlsParent.ClientHeight -
                                      ( FProportionWaterUseLabel.Top + FProportionWaterUseLabel.Height + 10 );
    LFixedWidth := FProportionWaterUseGrid.ColWidths[0];

    if ( ( FProportionWaterUseGrid.ClientWidth - LFixedWidth ) > 0) then
    begin
      LWidth := ( FProportionWaterUseGrid.ClientWidth - LFixedWidth ) div
                ( FProportionWaterUseGrid.ColCount - 1 );
      for LIndex := 1 to FProportionWaterUseGrid.ColCount - 1 do
        FProportionWaterUseGrid.ColWidths [ LIndex ] := LWidth - 2;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWaterUseProportioningDialog.Initialise: boolean;
const OPNAME = 'TWaterUseProportioningDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningDialog.SetWaterUseCategoryCount ( AChannelCount, ACategoryCount : integer );
const OPNAME = 'TWaterUseProportioningDialog.SetWaterUseCategoryCount';
var
  LIndex: integer;
begin
  try
    if( AChannelCount > 0 ) and ( ACategoryCount > 0 ) then
    begin
      if(FProportionWaterUseGrid.RowCount <> ( AChannelCount + 1)) then
        FProportionWaterUseGrid.RowCount := AChannelCount + 1;
      if (FProportionWaterUseGrid.ColCount <> ( ACategoryCount + 1)) then
        FProportionWaterUseGrid.ColCount := ACategoryCount + 1;
      FProportionWaterUseGrid.Options  := FProportionWaterUseGrid.Options + [goEditing];
      FProportionWaterUseGrid.Enabled  := True;
    end
    else
    begin
      if(FProportionWaterUseGrid.RowCount <> 2) then
        FProportionWaterUseGrid.RowCount := 2;
      if(FProportionWaterUseGrid.ColCount <> 2) then
        FProportionWaterUseGrid.ColCount := 2;
      FProportionWaterUseGrid.Options  := FProportionWaterUseGrid.Options - [goEditing];
      FProportionWaterUseGrid.Enabled  := False;
    end;

    for LIndex := 0 to FProportionWaterUseGrid.RowCount -1 do
      FProportionWaterUseGrid.Rows[LIndex].Clear;

    LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWaterUseProportioningDialog.LanguageHasChanged: boolean;
const OPNAME = 'TWaterUseProportioningDialog.LanguageHasChanged';
var
  lLanguage : TAbstractLanguage;
begin
  Result := inherited LanguageHasChanged;
  try
    lLanguage := FAppModules.Language;
    FProportionWaterUseLabel.Caption  := lLanguage.GetString('WaterUse.WaterUse');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWaterUseProportioningDialog.AssignHelpContext;
const OPNAME = 'TWaterUseProportioningDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(Self,                    HC_WaterResourcesYieldModel);
    SetControlHelpContext(FProportionWaterUseGrid, HC_WaterResourcesYieldModel);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWaterUseProportioningDialog.OnColumnResize(ASender: TObject; ACol: integer);
const OPNAME = 'TWaterUseProportioningDialog.OnColumnResize';
begin
  try

   except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
