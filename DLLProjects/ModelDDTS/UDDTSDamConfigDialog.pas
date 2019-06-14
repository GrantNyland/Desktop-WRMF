//
//
//  UNIT      : Contains the class TDDTSDamConfigValidator.
//  AUTHOR    : Sam Dhlamini
//  DATE      : 2014/07/17
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UDDTSDamConfigDialog;

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

  TDDTSDamConfigDialog = class(TAbstractScrollablePanel)
  private
  protected
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

     FImportHeadingChkBox      : TFieldChkBox;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure RestoreColourState; override;

    property RunoffScaleFactorEdit       : TFieldEdit read  FRunoffScaleFactorEdit;
    property OtherInflowScaleFactorEdit       : TFieldEdit read FOtherInflowScaleFactorEdit;
    property EWRScaleFactorEdit       : TFieldEdit read FEWRScaleFactorEdit;
    property TargetDraftEdit       : TFieldEdit read FTargetDraftEdit;
    property DSRequirmentEdit       : TFieldEdit read FDSRequirmentEdit;

    property DSPercReleaseEdit  : TFieldEdit read FDSPercReleaseEdit;
    property SpillPercReleaseEdit  : TFieldEdit read FSpillPercReleaseEdit;
    property EWRPercReleaseEdit  : TFieldEdit read FEWRPercReleaseEdit;
    property ImportHeadingChkBox : TFieldChkBox read FImportHeadingChkBox;

  end;

  implementation
uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations;

{ TDDTSDamConfigDialog }

procedure TDDTSDamConfigDialog.CreateMemberObjects;
const OPNAME = 'TDDTSDamConfigDialog.CreateMemberObjects';
begin
  inherited;
  try



    FgboxConfig                                           := TGroupBox.Create(ControlsOwner);
    FgboxConfig.Parent                                    := ControlsParent;
    FgboxConfig.Align                                     := alTop;
    FgboxConfig.Height                                    := 160;


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
    FDSRequirmentLabel.Left   := C_LabelOffset;

    FImportHeadingChkBox            := TFieldChkBox.Create(ControlsOwner, FAppModules);
    FImportHeadingChkBox.Parent     := FgboxConfig;
    FImportHeadingChkBox.Left       := C_LabelOffset;
    FImportHeadingChkBox.Top        := FDSRequirmentEdit.Height + FDSRequirmentEdit.Top + C_LabelOffset;
    FImportHeadingChkBox.Alignment  := taLeftJustify;

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




  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TDDTSDamConfigDialog.Initialise: boolean;
const OPNAME = 'TDDTSDamConfigDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    Result := True;
  // Handle exceptions.
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigDialog.Resize;
const OPNAME = 'TDDTSDamConfigDialog.Resize';
begin
  // Call the ancestor.
  inherited Resize;
  try
    // Set out the controls.
  

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

    FDSPercReleaseEdit.Left           := Self.ClientWidth div 2;
    FDSPercReleaseEdit.Width          := (Self.ClientWidth div 2 - C_ControlBorder) div 4;

    FSpillPercReleaseEdit.Left           := Self.ClientWidth div 2;
    FSpillPercReleaseEdit.Width          := FDSPercReleaseEdit.Width;

    FEWRPercReleaseEdit.Left           := Self.ClientWidth div 2;
    FEWRPercReleaseEdit.Width          := FDSPercReleaseEdit.Width;

    FImportHeadingChkBox.Left       := C_LabelOffset;
    FImportHeadingChkBox.Width      := (Self.ClientWidth div 2)+(C_ControlBorder*2)+3;

    
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDDTSDamConfigDialog.LanguageHasChanged: boolean;
const OPNAME = 'TDDTSDamConfigDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FgboxConfig.Caption := 'Input Configuration';
    FRunoffScaleFactorLabel.Caption   := 'Runoff Scale';
    FOtherInflowScaleFactorLabel.Caption := 'Other Inflow Scale';
    FEWRScaleFactorLabel.Caption := 'EWR Scale';
    FTargetDraftLabel.Caption := 'Target Draft';
    FDSRequirmentLabel.Caption := 'Down Stream Requirment';

    FImportHeadingChkBox.Caption := 'Import File Heading ?';
    FgboxOutputConfig.Caption := 'Output Configuration';

    FDSPercReleaseLabel.Caption := 'Down Stream Percentage Release';
    FSpillPercReleaseLabel.Caption := 'Spill Percentage Release';
    FEWRPercReleaseLabel.Caption := 'EWR Percentage Release';
    
    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDDTSDamConfigDialog.RestoreColourState;
const OPNAME = 'TDDTSDamConfigDialog.RestoreColourState';
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

procedure TDDTSDamConfigDialog.AssignHelpContext;
const OPNAME = 'TDDTSDamConfigDialog.AssignHelpContext';
begin
  try


  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.

