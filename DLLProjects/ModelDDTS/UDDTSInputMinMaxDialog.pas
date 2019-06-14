//
//
//  UNIT      : Contains the class TDDTSDamConfigValidator.
//  AUTHOR    : Sam Dhlamini
//  DATE      : 2014/07/17
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDDTSInputMinMaxDialog;

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

  TDDTSInputMinMaxDialog = class(TAbstractScrollablePanel)
  private
  protected
  {
     FgboxConfig               : TGroupBox;
     FRunoffScaleFactorLabel      : TLabel;
     FRunoffScaleFactorEdit       : TFieldEdit;
     FOtherInflowScaleFactorLabel      : TLabel;
     FOtherInflowScaleFactorEdit       : TFieldEdit;
     FEWRScaleFactorLabel      : TLabel;
     FEWRScaleFactorEdit       : TFieldEdit;
     FTargetDraftLabel      : TLabel;
     FTargetDraftEdit       : TFieldEdit;
     FDSRequirmentLabel      : TLabel;
     FDSRequirmentEdit       : TFieldEdit;
     FgboxOutputConfig       : TGroupBox;
     FDSPercReleaseEdit       : TFieldEdit;
     FDSPercReleaseLabel      : TLabel;

     FSpillPercReleaseEdit     : TFieldEdit;
     FSpillPercReleaseLabel    : TLabel ;

     FEWRPercReleaseEdit       : TFieldEdit;
     FEWRPercReleaseLabel        : TLabel;
     }
     FgboxMinMax        : TGroupBox;

     FMaxRunoffEdit     : TFieldEdit;
     FMaxRunoffLabel    : TLabel ;

     FMinRunoffEdit       : TFieldEdit;
     FMinRunoffLabel        : TLabel;

     FMinOtherInflowEdit     : TFieldEdit;
     FMinOtherInflowLabel    : TLabel ;

     FMaxOtherInflowEdit       : TFieldEdit;
     FMaxOtherInflowLabel        : TLabel;

     FMinRainfallEdit     : TFieldEdit;
     FMinRainfallLabel    : TLabel ;

     FMaxRainfallEdit       : TFieldEdit;
     FMaxRainfallLabel        : TLabel;

     FMaxEvaporationEdit     : TFieldEdit;
     FMaxEvaporationLabel    : TLabel ;

     FMinEvaporationEdit       : TFieldEdit;
     FMinEvaporationLabel        : TLabel;

     FMaxIncreamentalRunoffEdit     : TFieldEdit;
     FMaxIncreamentalRunoffLabel    : TLabel ;

     FMinIncreamentalRunoffEdit       : TFieldEdit;
     FMinIncreamentalRunoffLabel        : TLabel;

     FMaxEWREdit     : TFieldEdit;
     FMaxEWRLabel    : TLabel ;

     FMinEWREdit       : TFieldEdit;
     FMinEWRLabel        : TLabel;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure RestoreColourState; override;
   {
    property RunoffScaleFactorEdit       : TFieldEdit read  FRunoffScaleFactorEdit;
    property OtherInflowScaleFactorEdit       : TFieldEdit read FOtherInflowScaleFactorEdit;
    property EWRScaleFactorEdit       : TFieldEdit read FEWRScaleFactorEdit;
    property TargetDraftEdit       : TFieldEdit read FTargetDraftEdit;
    property DSRequirmentEdit       : TFieldEdit read FDSRequirmentEdit;

    property DSPercReleaseEdit  : TFieldEdit read FDSPercReleaseEdit;
    property SpillPercReleaseEdit  : TFieldEdit read FSpillPercReleaseEdit;
    property EWRPercReleaseEdit  : TFieldEdit read FEWRPercReleaseEdit;
      }
    property MaxRunoffEdit : TFieldEdit read FMaxRunoffEdit;
    property MinRunoffEdit : TFieldEdit read FMinRunoffEdit;
    property MinOtherInflowEdit : TFieldEdit read FMinOtherInflowEdit;
    property MaxOtherInflowEdit : TFieldEdit read FMaxOtherInflowEdit;
    property MinRainfallEdit : TFieldEdit read FMinRainfallEdit;
    property MaxRainfallEdit : TFieldEdit read FMaxRainfallEdit;
    property MaxEvaporationEdit : TFieldEdit read FMaxEvaporationEdit;
    property MinEvaporationEdit : TFieldEdit read FMinEvaporationEdit;
    property MaxIncreamentalRunoffEdit : TFieldEdit read FMaxIncreamentalRunoffEdit;
    property MinIncreamentalRunoffEdit : TFieldEdit read FMinIncreamentalRunoffEdit;
    property MaxEWREdit : TFieldEdit read  FMaxEWREdit;
    property MinEWREdit : TFieldEdit read  FMinEWREdit;


  end;

  implementation
uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;

{ TDDTSInputMinMaxDialog }

procedure TDDTSInputMinMaxDialog.CreateMemberObjects;
const OPNAME = 'TDDTSInputMinMaxDialog.CreateMemberObjects';
begin
  inherited;
  try

   {

    FgboxConfig                                           := TGroupBox.Create(ControlsOwner);
    FgboxConfig.Parent                                    := ControlsParent;
    FgboxConfig.Align                                     := alTop;
    FgboxConfig.Height                                    := 140;


    FRunoffScaleFactorEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FRunoffScaleFactorEdit.Parent  := FgboxConfig;
    FRunoffScaleFactorEdit.Top     := C_ControlBorder*3;

    FRunoffScaleFactorLabel        := TLabel.Create(ControlsOwner);
    FRunoffScaleFactorLabel.Parent := FgboxConfig;
    FRunoffScaleFactorLabel.Top    := FRunoffScaleFactorEdit.Top + C_LabelOffset;
    FRunoffScaleFactorLabel.Left   := C_LabelOffset;


    FOtherInflowScaleFactorEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FOtherInflowScaleFactorEdit.Parent  := FgboxConfig;
    FOtherInflowScaleFactorEdit.Top     := FRunoffScaleFactorEdit.Top + FRunoffScaleFactorEdit.Height + C_ControlBorder;

    FOtherInflowScaleFactorLabel        := TLabel.Create(ControlsOwner);
    FOtherInflowScaleFactorLabel.Parent := FgboxConfig;
    FOtherInflowScaleFactorLabel.Top    := FOtherInflowScaleFactorEdit.Top + C_LabelOffset;
    FOtherInflowScaleFactorLabel.Left   := C_LabelOffset;


    FEWRScaleFactorEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FEWRScaleFactorEdit.Parent  := FgboxConfig;
    FEWRScaleFactorEdit.Top     := FOtherInflowScaleFactorEdit.Top + FOtherInflowScaleFactorEdit.Height + C_ControlBorder;

    FEWRScaleFactorLabel        := TLabel.Create(ControlsOwner);
    FEWRScaleFactorLabel.Parent := FgboxConfig;
    FEWRScaleFactorLabel.Top    := FEWRScaleFactorEdit.Top + C_LabelOffset;
    FEWRScaleFactorLabel.Left   := C_LabelOffset;


    FTargetDraftEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FTargetDraftEdit.Parent  := FgboxConfig;
    FTargetDraftEdit.Top     := FEWRScaleFactorEdit.Top + FEWRScaleFactorEdit.Height + C_ControlBorder;

    FTargetDraftLabel        := TLabel.Create(ControlsOwner);
    FTargetDraftLabel.Parent := FgboxConfig;
    FTargetDraftLabel.Top    := FTargetDraftEdit.Top + C_LabelOffset;
    FTargetDraftLabel.Left   := C_LabelOffset;


    FDSRequirmentEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FDSRequirmentEdit.Parent  := FgboxConfig;
    FDSRequirmentEdit.Top     := FTargetDraftEdit.Top + FTargetDraftEdit.Height + C_ControlBorder;

    FDSRequirmentLabel        := TLabel.Create(ControlsOwner);
    FDSRequirmentLabel.Parent := FgboxConfig;
    FDSRequirmentLabel.Top    := FDSRequirmentEdit.Top + C_LabelOffset;
    FDSRequirmentLabel.Left   := C_LabelOffset;   }

    FgboxMinMax                                           := TGroupBox.Create(ControlsOwner);
    FgboxMinMax.Parent                                    := ControlsParent;
    FgboxMinMax.Align                                     := alTop;
    FgboxMinMax.Height                                    := 310;

    FMaxRunoffEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMaxRunoffEdit.Parent  := FgboxMinMax;
    FMaxRunoffEdit.Top     := C_ControlBorder*3;

    FMaxRunoffLabel        := TLabel.Create(ControlsOwner);
    FMaxRunoffLabel.Parent := FgboxMinMax;
    FMaxRunoffLabel.Top    := FMaxRunoffEdit.Top + C_LabelOffset;
    FMaxRunoffLabel.Left   := C_LabelOffset;

    FMinRunoffEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMinRunoffEdit.Parent  := FgboxMinMax;
    FMinRunoffEdit.Top     := FMaxRunoffEdit.Top + FMaxRunoffEdit.Height + C_ControlBorder;

    FMinRunoffLabel       := TLabel.Create(ControlsOwner);
    FMinRunoffLabel.Parent := FgboxMinMax;
    FMinRunoffLabel.Top    := FMinRunoffEdit.Top + C_LabelOffset;
    FMinRunoffLabel.Left   := C_LabelOffset;



    FMinOtherInflowEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMinOtherInflowEdit.Parent  := FgboxMinMax;
    FMinOtherInflowEdit.Top     := FMinRunoffEdit.Top + FMinRunoffEdit.Height + C_ControlBorder;

    FMinOtherInflowLabel        := TLabel.Create(ControlsOwner);
    FMinOtherInflowLabel.Parent := FgboxMinMax;
    FMinOtherInflowLabel.Top    := FMinOtherInflowEdit.Top + C_LabelOffset;
    FMinOtherInflowLabel.Left   := C_LabelOffset;

    FMaxOtherInflowEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMaxOtherInflowEdit.Parent  := FgboxMinMax;
    FMaxOtherInflowEdit.Top     := FMinOtherInflowEdit.Top + FMinOtherInflowEdit.Height + C_ControlBorder;

    FMaxOtherInflowLabel        := TLabel.Create(ControlsOwner);
    FMaxOtherInflowLabel.Parent := FgboxMinMax;
    FMaxOtherInflowLabel.Top    := FMaxOtherInflowEdit.Top + C_LabelOffset;
    FMaxOtherInflowLabel.Left   := C_LabelOffset;

    FMinRainfallEdit           := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMinRainfallEdit.Parent    := FgboxMinMax;
    FMinRainfallEdit.Top       := FMaxOtherInflowEdit.Top + FMaxOtherInflowEdit.Height + C_ControlBorder;

    FMinRainfallLabel           := TLabel.Create(ControlsOwner);
    FMinRainfallLabel.Parent    := FgboxMinMax;
    FMinRainfallLabel.Top       := FMinRainfallEdit.Top + C_LabelOffset;
    FMinRainfallLabel.Left      := C_LabelOffset;

    FMaxRainfallEdit           := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMaxRainfallEdit.Parent    := FgboxMinMax;
    FMaxRainfallEdit.Top       := FMinRainfallEdit.Top + FMinRainfallEdit.Height + C_ControlBorder;

    FMaxRainfallLabel           := TLabel.Create(ControlsOwner);
    FMaxRainfallLabel.Parent    := FgboxMinMax;
    FMaxRainfallLabel.Top       := FMaxRainfallEdit.Top + C_LabelOffset;
    FMaxRainfallLabel.Left      := C_LabelOffset;

    FMaxEvaporationEdit           := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMaxEvaporationEdit.Parent    := FgboxMinMax;
    FMaxEvaporationEdit.Top       := FMaxRainfallEdit.Top + FMaxRainfallEdit.Height + C_ControlBorder;

    FMaxEvaporationLabel           := TLabel.Create(ControlsOwner);
    FMaxEvaporationLabel.Parent    := FgboxMinMax;
    FMaxEvaporationLabel.Top       := FMaxEvaporationEdit.Top + C_LabelOffset;
    FMaxEvaporationLabel.Left      := C_LabelOffset;


    FMinEvaporationEdit           := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMinEvaporationEdit.Parent    := FgboxMinMax;
    FMinEvaporationEdit.Top       := FMaxEvaporationEdit.Top + FMaxEvaporationEdit.Height + C_ControlBorder;

    FMinEvaporationLabel          := TLabel.Create(ControlsOwner);
    FMinEvaporationLabel.Parent    := FgboxMinMax;
    FMinEvaporationLabel.Top       := FMinEvaporationEdit.Top + C_LabelOffset;
    FMinEvaporationLabel.Left      := C_LabelOffset;


    FMaxIncreamentalRunoffEdit           := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMaxIncreamentalRunoffEdit.Parent    := FgboxMinMax;
    FMaxIncreamentalRunoffEdit.Top       := FMinEvaporationEdit.Top + FMinEvaporationEdit.Height + C_ControlBorder;

    FMaxIncreamentalRunoffLabel           := TLabel.Create(ControlsOwner);
    FMaxIncreamentalRunoffLabel.Parent    := FgboxMinMax;
    FMaxIncreamentalRunoffLabel.Top       := FMaxIncreamentalRunoffEdit.Top + C_LabelOffset;
    FMaxIncreamentalRunoffLabel.Left      := C_LabelOffset;


    FMinIncreamentalRunoffEdit           := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMinIncreamentalRunoffEdit.Parent    := FgboxMinMax;
    FMinIncreamentalRunoffEdit.Top       := FMaxIncreamentalRunoffEdit.Top + FMaxIncreamentalRunoffEdit.Height + C_ControlBorder;

    FMinIncreamentalRunoffLabel           := TLabel.Create(ControlsOwner);
    FMinIncreamentalRunoffLabel.Parent    := FgboxMinMax;
    FMinIncreamentalRunoffLabel.Top       := FMinIncreamentalRunoffEdit.Top + C_LabelOffset;
    FMinIncreamentalRunoffLabel.Left      := C_LabelOffset;

    FMaxEWREdit           := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMaxEWREdit.Parent    := FgboxMinMax;
    FMaxEWREdit.Top       := FMinIncreamentalRunoffEdit.Top + FMinIncreamentalRunoffEdit.Height + C_ControlBorder;

    FMaxEWRLabel           := TLabel.Create(ControlsOwner);
    FMaxEWRLabel.Parent    := FgboxMinMax;
    FMaxEWRLabel.Top       := FMaxEWREdit.Top + C_LabelOffset;
    FMaxEWRLabel.Left      := C_LabelOffset;

    FMinEWREdit           := TFieldEdit.Create(ControlsOwner, FAppModules);
    FMinEWREdit.Parent    := FgboxMinMax;
    FMinEWREdit.Top       := FMaxEWREdit.Top + FMaxEWREdit.Height + C_ControlBorder;

    FMinEWRLabel           := TLabel.Create(ControlsOwner);
    FMinEWRLabel.Parent    := FgboxMinMax;
    FMinEWRLabel.Top       := FMinEWREdit.Top + C_LabelOffset;
    FMinEWRLabel.Left      := C_LabelOffset;

   {
    FgboxOutputConfig                    := TGroupBox.Create(ControlsOwner);
    FgboxOutputConfig.Parent             := ControlsParent;
    FgboxOutputConfig.Align              := alTop;
    FgboxOutputConfig.Height             := 90;


    FDSPercReleaseEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FDSPercReleaseEdit.Parent  := FgboxOutputConfig;
    FDSPercReleaseEdit.Top     := C_ControlBorder*3;

    FDSPercReleaseLabel        := TLabel.Create(ControlsOwner);
    FDSPercReleaseLabel.Parent := FgboxOutputConfig;
    FDSPercReleaseLabel.Top    := FDSPercReleaseEdit.Top + C_LabelOffset;
    FDSPercReleaseLabel.Left   := C_LabelOffset;


    FSpillPercReleaseEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FSpillPercReleaseEdit.Parent  := FgboxOutputConfig;
    FSpillPercReleaseEdit.Top     := FDSPercReleaseEdit.Top + FDSPercReleaseEdit.Height + C_ControlBorder;

    FSpillPercReleaseLabel        := TLabel.Create(ControlsOwner);
    FSpillPercReleaseLabel.Parent := FgboxOutputConfig;
    FSpillPercReleaseLabel.Top    := FSpillPercReleaseEdit.Top + C_LabelOffset;
    FSpillPercReleaseLabel.Left   := C_LabelOffset;


    FEWRPercReleaseEdit         := TFieldEdit.Create(ControlsOwner, FAppModules);
    FEWRPercReleaseEdit.Parent  := FgboxOutputConfig;
    FEWRPercReleaseEdit.Top     := FSpillPercReleaseEdit.Top + FSpillPercReleaseEdit.Height + C_ControlBorder;

    FEWRPercReleaseLabel        := TLabel.Create(ControlsOwner);
    FEWRPercReleaseLabel.Parent := FgboxOutputConfig;
    FEWRPercReleaseLabel.Top    := FEWRPercReleaseEdit.Top + C_LabelOffset;
    FEWRPercReleaseLabel.Left   := C_LabelOffset;

                                         }


  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDDTSInputMinMaxDialog.Initialise: boolean;
const OPNAME = 'TDDTSInputMinMaxDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    Result := True;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxDialog.Resize;
const OPNAME = 'TDDTSInputMinMaxDialog.Resize';
begin
  // Call the ancestor.
  inherited Resize;
  try
    // Set out the controls.
   {

    FRunoffScaleFactorEdit.Left           := Self.ClientWidth div 2;
    FRunoffScaleFactorEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FOtherInflowScaleFactorEdit.Left           := Self.ClientWidth div 2;
    FOtherInflowScaleFactorEdit.Width          := FRunoffScaleFactorEdit.Width;

    FEWRScaleFactorEdit.Left           := Self.ClientWidth div 2;
    FEWRScaleFactorEdit.Width          := FRunoffScaleFactorEdit.Width;

    FTargetDraftEdit.Left           := Self.ClientWidth div 2;
    FTargetDraftEdit.Width          := FRunoffScaleFactorEdit.Width;

    FDSRequirmentEdit.Left           := Self.ClientWidth div 2;
    FDSRequirmentEdit.Width          := FRunoffScaleFactorEdit.Width;

    FMaxRunoffEdit.Left                 := Self.ClientWidth div 2;
    FMaxRunoffEdit.Width                := FDSRequirmentEdit.Width;

    FMinRunoffEdit.Left                 := Self.ClientWidth div 2;
    FMinRunoffEdit.Width               := FDSRequirmentEdit.Width;

    FDSPercReleaseEdit.Left           := Self.ClientWidth div 2;
    FDSPercReleaseEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FSpillPercReleaseEdit.Left           := Self.ClientWidth div 2;
    FSpillPercReleaseEdit.Width          := FDSPercReleaseEdit.Width;

    FEWRPercReleaseEdit.Left           := Self.ClientWidth div 2;
    FEWRPercReleaseEdit.Width          := FDSPercReleaseEdit.Width;
      }

    FMaxRunoffEdit.Left           := Self.ClientWidth div 2;
    FMaxRunoffEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMinRunoffEdit.Left           := Self.ClientWidth div 2;
    FMinRunoffEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;


    FMinOtherInflowEdit.Left           := Self.ClientWidth div 2;
    FMinOtherInflowEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMaxOtherInflowEdit.Left           := Self.ClientWidth div 2;
    FMaxOtherInflowEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMinRainfallEdit.Left           := Self.ClientWidth div 2;
    FMinRainfallEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMaxRainfallEdit.Left           := Self.ClientWidth div 2;
    FMaxRainfallEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMaxEvaporationEdit.Left           := Self.ClientWidth div 2;
    FMaxEvaporationEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMinEvaporationEdit.Left           := Self.ClientWidth div 2;
    FMinEvaporationEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMaxIncreamentalRunoffEdit.Left           := Self.ClientWidth div 2;
    FMaxIncreamentalRunoffEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMinIncreamentalRunoffEdit.Left           := Self.ClientWidth div 2;
    FMinIncreamentalRunoffEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMaxEWREdit.Left           := Self.ClientWidth div 2;
    FMaxEWREdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FMinEWREdit.Left           := Self.ClientWidth div 2;
    FMinEWREdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;


  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSInputMinMaxDialog.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSInputMinMaxDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
  {
    FgboxConfig.Caption := 'Input Configuration';
    FRunoffScaleFactorLabel.Caption   := 'Runoff Scale';
    FOtherInflowScaleFactorLabel.Caption := 'Other Inflow Scale';
    FEWRScaleFactorLabel.Caption := 'EWR Scale';
    FTargetDraftLabel.Caption := 'Target Draft';
    FDSRequirmentLabel.Caption := 'Down Stream Requirment';
    FgboxOutputConfig.Caption := 'Output Configuration';

    FDSPercReleaseLabel.Caption := 'Down Stream Percentage Release';
    FSpillPercReleaseLabel.Caption := 'Spill Percentage Release';
    FEWRPercReleaseLabel.Caption := 'EWR Percentage Release';
       }

    FMaxRunoffLabel.Caption := 'Maximum Runoff Allowed';
    FMinRunoffLabel.Caption := 'Minimum Runoff Allowed';

    FMinOtherInflowLabel.Caption :=  'Minimum Other Inflow ';

    FMaxOtherInflowLabel.Caption :=  'Maximum Other Inflow';

     FMinRainfallLabel.Caption :=  'Minimum Rainfall';

     FMaxRainfallLabel.Caption :=  'Maximum Rainfall';

     FMaxEvaporationLabel.Caption :=  'Maximum Evaporation';

     FMinEvaporationLabel.Caption :=  'Minimum Evaporation';

     FMaxIncreamentalRunoffLabel.Caption :=  'Maximum Increamental Runoff';

     FMinIncreamentalRunoffLabel.Caption :=  'Minimum Increamental Runoff';

     FMaxEWRLabel.Caption :=  'Maximum EWR';

     FMinEWRLabel.Caption :=  'Minimum EWR';

     FgboxMinMax.Caption := 'Min-Max fo Input Value';

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxDialog.RestoreColourState;
const OPNAME = 'TDDTSInputMinMaxDialog.RestoreColourState';
var
  LIndex : integer;
begin
  inherited RestoreColourState;
  try
    for LIndex := 0 to ControlsOwner.ComponentCount - 1 do
      if ControlsOwner.Components[LIndex].ClassName = TFieldEdit.ClassName then
        if TFieldEdit(ControlsOwner.Components[LIndex]).Color = clRed then
          TFieldEdit(ControlsOwner.Components[LIndex]).Color := clWindow;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSInputMinMaxDialog.AssignHelpContext;
const OPNAME = 'TDDTSInputMinMaxDialog.AssignHelpContext';
begin
  try


  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

