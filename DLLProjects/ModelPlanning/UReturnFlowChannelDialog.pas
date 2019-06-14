//
//
//  UNIT      : Contains TReturnFlowChannelObject Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 14/06/2006
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UReturnFlowChannelDialog;

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

  TReturnFlowChannelDialog = class(TAbstractScrollablePanel)
  protected
    FRdgChannelProperties : TRadioGroup;
    FRdgCorrespondingChannels : TRadioGroup;
    FRdgMonthlyPotentialEvap : TRadioGroup;   
    FLblDemandCentreID : TLabel;
    FEdtDemandCentreID : TFieldEdit;
    FLblNumOfCorrespondingChannels : TLabel;
    FEdtNumOfCorrespondingChannels : TFieldEdit;
    FLblGaugeNumber : TLabel;
    FEdtGaugeNumber : TFieldEdit;
    FLblMonthlyAvrgFactor : TLabel;
    FEdtMonthlyAvrgFactor : TFieldEdit;
    FLblCalibrationFactor : TLabel;
    FEdtCalibrationFactor : TFieldEdit;
    FLblMonthlyAvrgNetEvap : TLabel;
    FEdtMonthlyAvrgNetEvap : TFieldEdit;
    FLblRoutingConstant : TLabel;
    FEdtRoutingConstant : TFieldEdit;
    FLblCurtailmentFactor : TLabel;
    FEdtCurtailmentFactor : TFieldEdit;
    FLblMultiplicationFactor : TLabel;
    FEdtMultiplicationFactor : TFieldEdit;
    FGrdMonthlyPotentialEvap : TFieldStringGrid;
    FGrdCorrespondingChannels : TFieldStringGrid;
    FCbxChannel : TFieldComboBox;
    FBtnAddCorrespondingChannel : TFieldBitBtn;
    FBtnDeleteCorrespondingChannel : TFieldBitBtn;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property EdtDemandCentreID             : TFieldEdit read FEdtDemandCentreID;
    property EdtNumOfCorrespondingChannels : TFieldEdit read FEdtNumOfCorrespondingChannels;
    property EdtGaugeNumber : TFieldEdit read FEdtGaugeNumber;
    property EdtMonthlyAvrgFactor : TFieldEdit read FEdtMonthlyAvrgFactor;
    property EdtCalibrationFactor : TFieldEdit read FEdtCalibrationFactor;
    property EdtMonthlyAvrgNetEvap : TFieldEdit read FEdtMonthlyAvrgNetEvap;
    property EdtRoutingConstant : TFieldEdit read FEdtRoutingConstant;
    property EdtCurtailmentFactor : TFieldEdit read FEdtCurtailmentFactor;
    property EdtMultiplicationFactor : TFieldEdit read FEdtMultiplicationFactor;
    property GrdMonthlyPotentialEvap : TFieldStringGrid read FGrdMonthlyPotentialEvap;
    property GrdCorrespondingChannels : TFieldStringGrid read FGrdCorrespondingChannels;
    property CbxChannel : TFieldComboBox read FCbxChannel;
    property BtnAddCorrespondingChannel : TFieldBitBtn  read FBtnAddCorrespondingChannel;
    property BtnDeleteCorrespondingChannel : TFieldBitBtn read FBtnDeleteCorrespondingChannel;

end;

implementation

uses
  SysUtils,
  VCL.Forms,
  VCL.Grids,
  UHelpContexts,
  UConstants,
  UErrorHandlingOperations,
  UControlCreationUtilities;

{ TReturnFlowChannelDialog }

procedure TReturnFlowChannelDialog.AssignHelpContext;
const OPNAME = 'TReturnFlowChannelDialog.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelDialog.CreateMemberObjects;
const OPNAME = 'TReturnFlowChannelDialog.CreateMemberObjects';
var
  LOwner  : TComponent;
  LParent : TWinControl;
begin
  inherited CreateMemberObjects;
  try
    LOwner  := ControlsOwner;
    LParent := ControlsParent;

    FRdgChannelProperties := TRadioGroup.Create(LOwner);
    FRdgChannelProperties.Parent := LParent;
    FRdgChannelProperties.Top := 10;
    FRdgChannelProperties.Left := 10;
    FRdgChannelProperties.Width := 380;
    FRdgChannelProperties.Height := 290;


    FRdgCorrespondingChannels := TRadioGroup.Create(LOwner);
    FRdgCorrespondingChannels.Parent := LParent;
    FRdgCorrespondingChannels.Top :=  FRdgChannelProperties.Height + 10;
    FRdgCorrespondingChannels.Left := FRdgChannelProperties.Left;
    FRdgCorrespondingChannels.Width := FRdgChannelProperties.Width + 10;
    FRdgCorrespondingChannels.Height := (FRdgChannelProperties.Height div 2);

    FRdgMonthlyPotentialEvap := TRadioGroup.Create(LOwner);
    FRdgMonthlyPotentialEvap.Parent := LParent;
    FRdgMonthlyPotentialEvap.Top := FRdgChannelProperties.Top;
    FRdgMonthlyPotentialEvap.Left := FRdgChannelProperties.Width + 10;
    FRdgMonthlyPotentialEvap.Width := FRdgChannelProperties.Width div 2;
    FRdgMonthlyPotentialEvap.Height := FRdgChannelProperties.Height;

    FLblDemandCentreID         := TLabel.Create(LOwner);
    FLblDemandCentreID.Parent  := FRdgChannelProperties;
    FLblDemandCentreID.Top     := 20;
    FLblDemandCentreID.Left    := 10;
    FLblDemandCentreID.Width   := 140;
    FLblDemandCentreID.Height  := 20;

    FEdtDemandCentreID         := TFieldEdit.Create(LOwner, FAppModules);
    FEdtDemandCentreID.Parent  := FRdgChannelProperties;
    FEdtDemandCentreID.Top     := 20;
    FEdtDemandCentreID.Left    := 160;
    FEdtDemandCentreID.Width   := 200;
    FEdtDemandCentreID.Height  := 20;

    FLblNumOfCorrespondingChannels         := TLabel.Create(LOwner);
    FLblNumOfCorrespondingChannels.Parent  := FRdgChannelProperties;
    FLblNumOfCorrespondingChannels.Top     := 50;
    FLblNumOfCorrespondingChannels.Left    := 10;
    FLblNumOfCorrespondingChannels.Width   := 140;
    FLblNumOfCorrespondingChannels.Height  := 20;

    FEdtNumOfCorrespondingChannels         := TFieldEdit.Create(LOwner,FAppModules);
    FEdtNumOfCorrespondingChannels.Parent  := FRdgChannelProperties;
    FEdtNumOfCorrespondingChannels.Top     := 50;
    FEdtNumOfCorrespondingChannels.Left    := 160;
    FEdtNumOfCorrespondingChannels.Width   := 70;
    FEdtNumOfCorrespondingChannels.Height  := 20;

    FLblGaugeNumber := TLabel.Create(LOwner);
    FLblGaugeNumber.Parent := FRdgChannelProperties;
    FLblGaugeNumber.Top     := 80;
    FLblGaugeNumber.Left    := 10;
    FLblGaugeNumber.Width   := 140;
    FLblGaugeNumber.Height  := 20;

    FEdtGaugeNumber         := TFieldEdit.Create(LOwner,FAppModules);
    FEdtGaugeNumber.Parent  := FRdgChannelProperties;
    FEdtGaugeNumber.Top     := 80;
    FEdtGaugeNumber.Left    := 160;
    FEdtGaugeNumber.Width   := FEdtNumOfCorrespondingChannels.Width;
    FEdtGaugeNumber.Height  := 20;

    FLblMonthlyAvrgFactor        := TLabel.Create(LOwner);
    FLblMonthlyAvrgFactor.Parent := FRdgChannelProperties;
    FLblMonthlyAvrgFactor.Top     := 110;
    FLblMonthlyAvrgFactor.Left    := 10;
    FLblMonthlyAvrgFactor.Width   := 140;
    FLblMonthlyAvrgFactor.Height  := 20;

    FEdtMonthlyAvrgFactor         := TFieldEdit.Create(LOwner,FAppModules);
    FEdtMonthlyAvrgFactor.Parent  := FRdgChannelProperties;
    FEdtMonthlyAvrgFactor.Top     := 110;
    FEdtMonthlyAvrgFactor.Left    := 160;
    FEdtMonthlyAvrgFactor.Width   := FEdtNumOfCorrespondingChannels.Width;
    FEdtMonthlyAvrgFactor.Height  := 20;


    FLblCalibrationFactor        := TLabel.Create(LOwner);
    FLblCalibrationFactor.Parent := FRdgChannelProperties;
    FLblCalibrationFactor.Top     := 140;
    FLblCalibrationFactor.Left    := 10;
    FLblCalibrationFactor.Width   := 140;
    FLblCalibrationFactor.Height  := 20;

    FEdtCalibrationFactor         := TFieldEdit.Create(LOwner,FAppModules);
    FEdtCalibrationFactor.Parent  := FRdgChannelProperties;
    FEdtCalibrationFactor.Top     := 140;
    FEdtCalibrationFactor.Left    := 160;
    FEdtCalibrationFactor.Width   := FEdtNumOfCorrespondingChannels.Width;
    FEdtCalibrationFactor.Height  := 20;

    FLblMonthlyAvrgNetEvap := TLabel.Create(LOwner);
    FLblMonthlyAvrgNetEvap.Parent := FRdgChannelProperties;
    FLblMonthlyAvrgNetEvap.Top     := 170;
    FLblMonthlyAvrgNetEvap.Left    := 10;
    FLblMonthlyAvrgNetEvap.Width   := 140;
    FLblMonthlyAvrgNetEvap.Height  := 20;

    FEdtMonthlyAvrgNetEvap         := TFieldEdit.Create(LOwner,FAppModules);
    FEdtMonthlyAvrgNetEvap.Parent  := FRdgChannelProperties;
    FEdtMonthlyAvrgNetEvap.Top     := 170;
    FEdtMonthlyAvrgNetEvap.Left    := 160;
    FEdtMonthlyAvrgNetEvap.Width   := FEdtNumOfCorrespondingChannels.Width;
    FEdtMonthlyAvrgNetEvap.Height  := 20;

    FLblRoutingConstant := TLabel.Create(LOwner);
    FLblRoutingConstant.Parent := FRdgChannelProperties;
    FLblRoutingConstant.Top     := 200;
    FLblRoutingConstant.Left    := 10;
    FLblRoutingConstant.Width   := 140;
    FLblRoutingConstant.Height  := 20;

    FEdtRoutingConstant         := TFieldEdit.Create(LOwner,FAppModules);
    FEdtRoutingConstant.Parent  := FRdgChannelProperties;
    FEdtRoutingConstant.Top     := 200;
    FEdtRoutingConstant.Left    := 160;
    FEdtRoutingConstant.Width   := FEdtNumOfCorrespondingChannels.Width;
    FEdtRoutingConstant.Height  := 20;

    FLblCurtailmentFactor := TLabel.Create(LOwner);
    FLblCurtailmentFactor.Parent := FRdgChannelProperties;
    FLblCurtailmentFactor.Top     := 230;
    FLblCurtailmentFactor.Left    := 10;
    FLblCurtailmentFactor.Width   := 140;
    FLblCurtailmentFactor.Height  := 20;

    FEdtCurtailmentFactor := TFieldEdit.Create(LOwner,FAppModules);
    FEdtCurtailmentFactor.Parent  := FRdgChannelProperties;
    FEdtCurtailmentFactor.Top     := 230;
    FEdtCurtailmentFactor.Left    := 160;
    FEdtCurtailmentFactor.Width   := FEdtNumOfCorrespondingChannels.Width;
    FEdtCurtailmentFactor.Height  := 20;

    FLblMultiplicationFactor := TLabel.Create(LOwner);
    FLblMultiplicationFactor.Parent := FRdgChannelProperties;
    FLblMultiplicationFactor.Top     := 260;
    FLblMultiplicationFactor.Left    := 10;
    FLblMultiplicationFactor.Width   := 140;
    FLblMultiplicationFactor.Height  := 20;

    FEdtMultiplicationFactor := TFieldEdit.Create(LOwner,FAppModules);
    FEdtMultiplicationFactor.Parent  := FRdgChannelProperties;
    FEdtMultiplicationFactor.Top     := 260;
    FEdtMultiplicationFactor.Left    := 160;
    FEdtMultiplicationFactor.Width   := FEdtNumOfCorrespondingChannels.Width;
    FEdtMultiplicationFactor.Height  := 20;

    FGrdMonthlyPotentialEvap                   := TFieldStringGrid.Create(LOwner,FAppModules);
    FGrdMonthlyPotentialEvap.Parent            := FRdgMonthlyPotentialEvap;
    FGrdMonthlyPotentialEvap.Top               := 20;
    FGrdMonthlyPotentialEvap.Left              := 10;
    FGrdMonthlyPotentialEvap.ColCount          := 2;
    FGrdMonthlyPotentialEvap.RowCount          := 12;
    FGrdMonthlyPotentialEvap.FixedCols         := 1;
    FGrdMonthlyPotentialEvap.FixedRows         := 0;
    FGrdMonthlyPotentialEvap.DefaultRowHeight  := 18;
    FGrdMonthlyPotentialEvap.DefaultColWidth   := 65;
    FGrdMonthlyPotentialEvap.Height            := (FGrdMonthlyPotentialEvap.DefaultRowHeight*(FGrdMonthlyPotentialEvap.RowCount+2));
    FGrdMonthlyPotentialEvap.Width             := (FGrdMonthlyPotentialEvap.DefaultColWidth*FGrdMonthlyPotentialEvap.ColCount)+10;
    FGrdMonthlyPotentialEvap.Options           := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];


    FGrdCorrespondingChannels                  := TFieldStringGrid.Create(LOwner,FAppModules);
    FGrdCorrespondingChannels.Parent           := FRdgCorrespondingChannels;
    FGrdCorrespondingChannels.Top              := 55;
    FGrdCorrespondingChannels.Left             := 10;
    FGrdCorrespondingChannels.ColCount         := 3;
    FGrdCorrespondingChannels.RowCount         := 2;
    FGrdCorrespondingChannels.FixedCols        := 0;
    FGrdCorrespondingChannels.FixedRows        := 1;
    FGrdCorrespondingChannels.DefaultRowHeight := 15;
    FGrdCorrespondingChannels.DefaultColWidth  := 85;
    FGrdCorrespondingChannels.Width            := (FGrdCorrespondingChannels.DefaultColWidth * 3) + 15;
    FGrdCorrespondingChannels.Height           := (FGrdCorrespondingChannels.DefaultRowHeight * 3) + 15;
    FGrdCorrespondingChannels.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];


    FCbxChannel                := TFieldComboBox.Create(LOwner,FAppModules);
    FCbxChannel.Parent         := FRdgCorrespondingChannels;
    FCbxChannel.Top            := 22;
    FCbxChannel.Left           := 12;
    FCbxChannel.Width          := 85;
    FCbxChannel.Height         := 21;
    FCbxChannel.Style          := csDropDownList;
    FCbxChannel.Visible        := False;

    FBtnAddCorrespondingChannel         := TFieldBitBtn.Create(LOwner,FAppModules);
    FBtnAddCorrespondingChannel.Parent  := FRdgCorrespondingChannels;
    FBtnAddCorrespondingChannel.Left    := 10;
    FBtnAddCorrespondingChannel.Top     := 20;
    FBtnAddCorrespondingChannel.Width   := 75;
    FBtnAddCorrespondingChannel.Height  := 25;
    FBtnAddCorrespondingChannel.ShowHint:= True;
    FBtnAddCorrespondingChannel.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');

    FBtnDeleteCorrespondingChannel      := TFieldBitBtn.Create(LOwner,FAppModules);
    FBtnDeleteCorrespondingChannel.Parent  := FRdgCorrespondingChannels;
    FBtnDeleteCorrespondingChannel.Left    := 95;
    FBtnDeleteCorrespondingChannel.Top     := 20;
    FBtnDeleteCorrespondingChannel.Width   := 75;
    FBtnDeleteCorrespondingChannel.Height  := 25;
    FBtnDeleteCorrespondingChannel.ShowHint:= True;
    FBtnDeleteCorrespondingChannel.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelDialog.Initialise: boolean;
const OPNAME = 'TReturnFlowChannelDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FBtnAddCorrespondingChannel.Enabled    := (FAppModules.User.UserRights in CUR_EditData) and
                                              (not FAppModules.StudyArea.ScenarioLocked);
    FBtnDeleteCorrespondingChannel.Enabled := (FAppModules.User.UserRights in CUR_EditData) and
                                              (not FAppModules.StudyArea.ScenarioLocked);
    Result := TRUE;

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TReturnFlowChannelDialog.LanguageHasChanged: boolean;
const OPNAME = 'TReturnFlowChannelDialog.LanguageHasChanged';
begin
  Result := False;
  try
    FLblDemandCentreID.Caption             := FAppModules.Language.GetString('PlanningGUI.DemandDefName');
    FLblNumOfCorrespondingChannels.Caption := FAppModules.Language.GetString('TReturnFlowChanneldialog.NumOfCorrespondingChannels');
    FRdgCorrespondingChannels.Caption      := FAppModules.Language.GetString('TReturnFlowChanneldialog.CorrespondingChannel');
    FRdgMonthlyPotentialEvap.Caption       := FAppModules.Language.GetString('TReturnFlowChanneldialog.MonthlyPotentialEvap');
    FRdgChannelProperties.Caption          := FAppModules.Language.GetString('TReturnFlowChanneldialog.ChannelProperties');
    FLblGaugeNumber.Caption                := FAppModules.Language.GetString('GridHeading.GaugeNumber');
    FLblMonthlyAvrgFactor.Caption          := FAppModules.Language.GetString('TReturnFlowChanneldialog.MonthlyAvrgFactor');
    FLblCalibrationFactor.Caption          := FAppModules.Language.GetString('TReturnFlowChanneldialog.CalibrationFactor');
    FLblMonthlyAvrgNetEvap.Caption         := FAppModules.Language.GetString('TReturnFlowChanneldialog.MonthlyAvrgNetEvap');
    FLblRoutingConstant.Caption            := FAppModules.Language.GetString('TReturnFlowChanneldialog.RoutingConstant');
    FLblCurtailmentFactor.Caption          := FAppModules.Language.GetString('TReturnFlowChanneldialog.CurtailmentFactor');
    FLblMultiplicationFactor.Caption       := FAppModules.Language.GetString('TReturnFlowChanneldialog.MultiplicationFactor');
    FBtnAddCorrespondingChannel.Caption    := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FBtnAddCorrespondingChannel.Hint       := FAppModules.Language.GetString('ButtonHint.AddCorrespondingChannel');
    FBtnDeleteCorrespondingChannel.Caption := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FBtnDeleteCorrespondingChannel.Hint    := FAppModules.Language.GetString('ButtonHint.DeleteCorrespondingChannel');


    FGrdCorrespondingChannels.Cells[0,0] := FAppModules.Language.GetString('Channel.Channel');
    FGrdCorrespondingChannels.Cells[1,0] := FAppModules.Language.GetString('TReturnFlowChanneldialog.Abstraction');
    FGrdCorrespondingChannels.Cells[2,0] := FAppModules.Language.GetString('TReturnFlowChanneldialog.AssumedFactor');

    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelDialog.Resize;
const OPNAME = 'TReturnFlowChannelDialog.Resize';
begin
  inherited Resize;
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TReturnFlowChannelDialog.RestoreColourState;
const OPNAME = 'TReturnFlowChannelDialog.RestoreColourState';
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

end.
