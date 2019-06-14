 //
//
//  UNIT      : Contains  TWQConstraintDialog   Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 17/02/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UWQConstraintDialog;


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

  TWQConstraintDialog = class(TAbstractScrollablePanel)
  protected
    //
    FUpperBoundedChannelGroup     : TGroupBox;
    FWQConstraintsGroup           : TGroupBox;
    FLblNumOfBoundedChannels      : TLabel;
    FEdtNumOfBoundedChannels      : TFieldEdit;

    FAddBoundChannelbtn           : TFieldBitBtn;
    FDeleteBoundChannelbtn        : TFieldBitBtn;

    FGrdMinMaxChannels            : TFieldStringGrid;
    FLblMinMaxChannels            : TLabel;
    FCbxChannel                   : TFieldComboBox;
    FCbxChannelSign               : TFieldComboBox;

    //Water Quality Constraints

    FlblTarget                    : TLabel;
    FEdtTarget                    : TFieldEdit;

    FLblNoOfBlendingChannels      : TLabel;
    FEdtNoOfBlendingChannels      : TFieldEdit;

    FLblReservoirRef              : TLabel;
    FEdtReservoirRef              : TFieldEdit;

    FlblWQConType                 : TLabel;
    FCmbWQConType                 : TFieldComboBox;

    FAddWQConChannelbtn           : TFieldBitBtn;
    FDeleteWQConChannelbtn        : TFieldBitBtn;
    FGrdMinMaxWQConChannels       : TFieldStringGrid;

    FlblSlopeLimit                : TLabel;
    FEdtSlopeLimit                : TFieldEdit;

    FlblEstimatedRelease          : TLabel;
    FGrdEstimatedRelease          : TFieldStringGrid;

    FlblConcentration             : TLabel;
    FGrdConcentration             : TFieldStringGrid;

    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    procedure RestoreColourState; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    procedure SetFlowConstraintGridCols(ACount : integer);

    property EdtNumOfBoundedChannels      : TFieldEdit read FEdtNumOfBoundedChannels;
    property GrdMinMaxChannels : TFieldStringGrid read FGrdMinMaxChannels;
    property CbxChannel : TFieldComboBox read FCbxChannel;
    property CbxChannelSign : TFieldComboBox read FCbxChannelSign;

    property AddBoundChannelbtn : TFieldBitBtn  read FAddBoundChannelbtn;

    property DeleteBoundChannelbtn : TFieldBitBtn read FDeleteBoundChannelbtn;

    property EdtTarget                    : TFieldEdit read FEdtTarget;
    property EdtNoOfBlendingChannels      : TFieldEdit read FEdtNoOfBlendingChannels;
    property EdtReservoirRef              : TFieldEdit read FEdtReservoirRef;
    property CmbWQConType                 : TFieldComboBox read FCmbWQConType;

    property AddWQConChannelbtn           : TFieldBitBtn read FAddWQConChannelbtn;
    property DeleteWQConChannelbtn        : TFieldBitBtn read FDeleteWQConChannelbtn;


    property GrdMinMaxWQConChannels       : TFieldStringGrid read FGrdMinMaxWQConChannels;
    property EdtSlopeLimit                : TFieldEdit       read FEdtSlopeLimit;
    property GrdEstimatedRelease          : TFieldStringGrid read FGrdEstimatedRelease;
    property GrdConcentration             : TFieldStringGrid read FGrdConcentration;
    property lblConcentration             : TLabel           read FlblConcentration;
    property lblSlopeLimit                : TLabel           read FlblSlopeLimit;
    property lblEstimatedRelease          : TLabel           read FlblEstimatedRelease;
    property UpperBoundedChannelGroup     : TGroupBox        read FUpperBoundedChannelGroup;
    property WQConstraintsGroup           : TGroupBox        read FWQConstraintsGroup;

  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UErrorHandlingOperations,
  UControlCreationUtilities, VCL.Grids, UStringGridWithCellChange;

{******************************************************************************}
{* TWQConstraintDialog                                                       *}
{******************************************************************************}

procedure TWQConstraintDialog.CreateMemberObjects;
const OPNAME = 'TWQConstraintDialog.CreateMemberObjects';
var
  lOwner  : TComponent;
begin
  inherited;
  try
    lOwner                               := ControlsOwner;

    FWQConstraintsGroup                  := TGroupBox.Create(ControlsOwner);
    FWQConstraintsGroup.Parent           := ControlsParent;
    FUpperBoundedChannelGroup            := TGroupBox.Create(ControlsOwner);
    FUpperBoundedChannelGroup.Parent     := ControlsParent;

    FLblNumOfBoundedChannels         := TLabel.Create(LOwner);
    FLblNumOfBoundedChannels.Parent  := FUpperBoundedChannelGroup;
    FLblNumOfBoundedChannels.Top     := 40;
    FLblNumOfBoundedChannels.Left    := 10;
    FLblNumOfBoundedChannels.Width   := 140;
    FLblNumOfBoundedChannels.Height  := 20;

    FEdtNumOfBoundedChannels         := TFieldEdit.Create(LOwner,FAppModules);
    FEdtNumOfBoundedChannels.Parent  := FUpperBoundedChannelGroup;
    FEdtNumOfBoundedChannels.Top     := 40;
    FEdtNumOfBoundedChannels.Left    := 160;
    FEdtNumOfBoundedChannels.Width   := 70;
    FEdtNumOfBoundedChannels.Height  := 20;

    FCbxChannel                := TFieldComboBox.Create(LOwner,FAppModules);
    FCbxChannel.Parent         := FUpperBoundedChannelGroup;
    FCbxChannel.Top            := FLblNumOfBoundedChannels.Top + FLblNumOfBoundedChannels.Height + 10;
    FCbxChannel.Left           := 162;
    FCbxChannel.Width          := 85;
    FCbxChannel.Height         := 21;
    FCbxChannel.Style          := csDropDownList;
    FCbxChannel.Visible        := False;

    FCbxChannelSign            := TFieldComboBox.Create(LOwner,FAppModules);
    FCbxChannelSign.Parent         := FUpperBoundedChannelGroup;
    FCbxChannelSign.Top            := FLblNumOfBoundedChannels.Top + FLblNumOfBoundedChannels.Height + 10;
    FCbxChannelSign.Left           := 162;
    FCbxChannelSign.Width          := 35;
    FCbxChannelSign.Height         := 21;
    FCbxChannelSign.Style          := csDropDownList;
    FCbxChannelSign.Visible        := False;

    FAddBoundChannelbtn         := TFieldBitBtn.Create(LOwner,FAppModules);
    FAddBoundChannelbtn.Parent  := FUpperBoundedChannelGroup;
    FAddBoundChannelbtn.Left    := 160;
    FAddBoundChannelbtn.Top     := FCbxChannel.Top + 10;
    FAddBoundChannelbtn.Width   := 75;
    FAddBoundChannelbtn.Height  := 25;
    FAddBoundChannelbtn.ShowHint:= True;
    FAddBoundChannelbtn.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');

    FDeleteBoundChannelbtn         := TFieldBitBtn.Create(LOwner,FAppModules);
    FDeleteBoundChannelbtn.Parent  := FUpperBoundedChannelGroup;
    FDeleteBoundChannelbtn.Left    := FAddBoundChannelbtn.Left + 95;
    FDeleteBoundChannelbtn.Top     := FAddBoundChannelbtn.Top;
    FDeleteBoundChannelbtn.Width   := 75;
    FDeleteBoundChannelbtn.Height  := 25;
    FDeleteBoundChannelbtn.ShowHint:= True;
    FDeleteBoundChannelbtn.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');

    FLblMinMaxChannels         := TLabel.Create(LOwner);
    FLblMinMaxChannels.Parent  := FUpperBoundedChannelGroup;
    FLblMinMaxChannels.Top     := FAddBoundChannelbtn.Top + FAddBoundChannelbtn.Height + 15;
    FLblMinMaxChannels.Left    := 10;
    FLblMinMaxChannels.Width   := 140;
    FLblMinMaxChannels.Height  := 20;

    FGrdMinMaxChannels                  := TFieldStringGrid.Create(LOwner,FAppModules);
    FGrdMinMaxChannels.Parent           := FUpperBoundedChannelGroup;
    FGrdMinMaxChannels.Top              := FLblMinMaxChannels.Top;
    FGrdMinMaxChannels.Left             := FAddBoundChannelbtn.Left;
    FGrdMinMaxChannels.ColCount         := 3;
    FGrdMinMaxChannels.RowCount         := 2;
    FGrdMinMaxChannels.FixedCols        := 0;
    FGrdMinMaxChannels.FixedRows        := 1;
    FGrdMinMaxChannels.DefaultRowHeight := 15;
    FGrdMinMaxChannels.DefaultColWidth  := 85;
    FGrdMinMaxChannels.Width            := (FGrdMinMaxChannels.DefaultColWidth * 2); //FGrdMinMaxChannels.ColCount* (FGrdMinMaxChannels.DefaultColWidth + 1) + 15 ;
    FGrdMinMaxChannels.Height           := (FGrdMinMaxChannels.DefaultRowHeight * FGrdMinMaxChannels.RowCount) + 5;
    FGrdMinMaxChannels.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];


    //  -----------------------------------------------------


    FlblTarget                    := TLabel.Create(LOwner);
    FlblTarget.Parent             := FWQConstraintsGroup;
    FlblTarget.Top                := 40;
    FlblTarget.Left               := 5;
    FlblTarget.Width              := 140;
    FlblTarget.Height             := 20;

    FEdtTarget                    := TFieldEdit.Create(LOwner,FAppModules);
    FEdtTarget.Parent             := FWQConstraintsGroup;
    FEdtTarget.Top                := 40;
    FEdtTarget.Left               := 100;
    FEdtTarget.Width              := 70;
    FEdtTarget.Height             := 20;

    FLblNoOfBlendingChannels                    :=  TLabel.Create(LOwner);
    FLblNoOfBlendingChannels.Parent             :=  FWQConstraintsGroup;
    FLblNoOfBlendingChannels.Top                :=  FlblTarget.Top + FlblTarget.Height + 5;
    FLblNoOfBlendingChannels.Left               := 5;
    FLblNoOfBlendingChannels.Width              := 140;
    FLblNoOfBlendingChannels.Height             := 20;

    FEdtNoOfBlendingChannels                    := TFieldEdit.Create(LOwner,FAppModules);
    FEdtNoOfBlendingChannels.Parent             := FWQConstraintsGroup;
    FEdtNoOfBlendingChannels.Top                := FLblNoOfBlendingChannels.Top;
    FEdtNoOfBlendingChannels.Left               := 100;
    FEdtNoOfBlendingChannels.Width              := 70;
    FEdtNoOfBlendingChannels.Height             := 20;



    FLblReservoirRef                    := TLabel.Create(LOwner);
    FLblReservoirRef.Parent             :=  FWQConstraintsGroup;
    FLblReservoirRef.Top                :=  FLblNoOfBlendingChannels.Top + FLblNoOfBlendingChannels.Height + 5;
    FLblReservoirRef.Left               := 5;
    FLblReservoirRef.Width              := 140;
    FLblReservoirRef.Height             := 20;

    FEdtReservoirRef                    := TFieldEdit.Create(LOwner,FAppModules);
    FEdtReservoirRef.Parent             := FWQConstraintsGroup;
    FEdtReservoirRef.Top                := FLblReservoirRef.Top;
    FEdtReservoirRef.Left               := 100;
    FEdtReservoirRef.Width              := 70;
    FEdtReservoirRef.Height             := 20;

    FlblWQConType                    :=  TLabel.Create(LOwner);
    FlblWQConType.Parent             :=  FWQConstraintsGroup;
    FlblWQConType.Top                :=  FLblReservoirRef.Top + FLblReservoirRef.Height + 5;
    FlblWQConType.Left               :=  5;
    FlblWQConType.Width              :=  140;
    FlblWQConType.Height             :=  20;

    FCmbWQConType                    := TFieldComboBox.Create(LOwner,FAppModules);
    FCmbWQConType.Parent             := FWQConstraintsGroup;
    FCmbWQConType.Top                := FlblWQConType.Top;
    FCmbWQConType.Left               := 100;
    FCmbWQConType.Width              := 70;
    FCmbWQConType.Height             := 20;

    FAddWQConChannelbtn           := TFieldBitBtn.Create(LOwner,FAppModules);
    FAddWQConChannelbtn.Parent    := FWQConstraintsGroup;
    FAddWQConChannelbtn.Left      := 100;
    FAddWQConChannelbtn.Top       := FCmbWQConType.Top + FCmbWQConType.Height + 20;
    FAddWQConChannelbtn.Width     := 75;
    FAddWQConChannelbtn.Height    := 25;
    FAddWQConChannelbtn.ShowHint  := True;
    FAddWQConChannelbtn.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORCREATEVIEW');

    FDeleteWQConChannelbtn        := TFieldBitBtn.Create(LOwner,FAppModules);
    FDeleteWQConChannelbtn.Parent    := FWQConstraintsGroup;
    FDeleteWQConChannelbtn.Left      := FAddWQConChannelbtn.Left + 95;
    FDeleteWQConChannelbtn.Top       := FAddWQConChannelbtn.Top;
    FDeleteWQConChannelbtn.Width     := 75;
    FDeleteWQConChannelbtn.Height    := 25;
    FDeleteWQConChannelbtn.ShowHint  := True;
    FDeleteWQConChannelbtn.Glyph.LoadFromResourceName(HImagesInstance, 'TIMECOMPARITORDELETEVIEW');

    FGrdMinMaxWQConChannels                  := TFieldStringGrid.Create(LOwner,FAppModules);
    FGrdMinMaxWQConChannels.Parent           := FWQConstraintsGroup;

    FGrdMinMaxWQConChannels.Top              := FAddWQConChannelbtn.Top + FAddWQConChannelbtn.Height + 10;
    FGrdMinMaxWQConChannels.Left             := FAddWQConChannelbtn.Left;
    FGrdMinMaxWQConChannels.ColCount         := 3;
    FGrdMinMaxWQConChannels.RowCount         := 2;
    FGrdMinMaxWQConChannels.FixedCols        := 0;
    FGrdMinMaxWQConChannels.FixedRows        := 1;
    FGrdMinMaxWQConChannels.DefaultRowHeight := 15;
    FGrdMinMaxWQConChannels.DefaultColWidth  := 85;
    FGrdMinMaxWQConChannels.Width            := (FGrdMinMaxWQConChannels.DefaultColWidth * FGrdMinMaxWQConChannels.ColCount);
    FGrdMinMaxWQConChannels.Height           := (FGrdMinMaxWQConChannels.DefaultRowHeight * FGrdMinMaxWQConChannels.RowCount) + 5;
    FGrdMinMaxWQConChannels.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];


    FlblSlopeLimit                    :=  TLabel.Create(LOwner);
    FlblSlopeLimit.Parent             :=  FWQConstraintsGroup;
    FlblSlopeLimit.Top                :=  FGrdMinMaxWQConChannels.Top + FGrdMinMaxWQConChannels.Height + 5;
    FlblSlopeLimit.Left               :=  5;
    FlblSlopeLimit.Width              :=  140;
    FlblSlopeLimit.Height             :=  20;

    FEdtSlopeLimit                    := TFieldEdit.Create(LOwner,FAppModules);
    FEdtSlopeLimit.Parent             := FWQConstraintsGroup;
    FEdtSlopeLimit.Top                := FlblSlopeLimit.Top;
    FEdtSlopeLimit.Left               := 100;
    FEdtSlopeLimit.Width              := 70;
    FEdtSlopeLimit.Height             := 20;

    FlblEstimatedRelease                    := TLabel.Create(LOwner);
    FlblEstimatedRelease.Parent             :=  FWQConstraintsGroup;
    FlblEstimatedRelease.Top                :=  FEdtSlopeLimit.Top + FEdtSlopeLimit.Height + 5;
    FlblEstimatedRelease.Left               :=  5;
    FlblEstimatedRelease.Width              :=  140;
    FlblEstimatedRelease.Height             :=  20;

    FGrdEstimatedRelease                  := TFieldStringGrid.Create(LOwner,FAppModules);
    FGrdEstimatedRelease.Parent           := FWQConstraintsGroup;
    FGrdEstimatedRelease.Top              := FlblEstimatedRelease.Top;
    FGrdEstimatedRelease.Left             := FAddWQConChannelbtn.Left;
    FGrdEstimatedRelease.ColCount         := 10;
    FGrdEstimatedRelease.RowCount         := 1;
    FGrdEstimatedRelease.FixedCols        := 0;
    FGrdEstimatedRelease.FixedRows        := 0;
    FGrdEstimatedRelease.DefaultRowHeight := 20;
    FGrdEstimatedRelease.DefaultColWidth  := 40;
    FGrdEstimatedRelease.Width            := FGrdEstimatedRelease.ColCount* (FGrdEstimatedRelease.DefaultColWidth + 1) + 3;
    FGrdEstimatedRelease.Height           := (FGrdMinMaxChannels.DefaultRowHeight * 1) + 8;
  //  FGrdEstimatedRelease.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];

    FlblConcentration                    :=  TLabel.Create(LOwner);
    FlblConcentration.Parent             :=  FWQConstraintsGroup;
    FlblConcentration.Top                :=  FGrdEstimatedRelease.Top + FGrdEstimatedRelease.Height + 5;
    FlblConcentration.Left               :=  5;
    FlblConcentration.Width              :=  140;
    FlblConcentration.Height             :=  20;

    FGrdConcentration                  := TFieldStringGrid.Create(LOwner,FAppModules);
    FGrdConcentration.Parent           := FWQConstraintsGroup;
    FGrdConcentration.Top              := FlblConcentration.Top;
    FGrdConcentration.Left             := FAddWQConChannelbtn.Left;
    FGrdConcentration.ColCount         := 10;
    FGrdConcentration.RowCount         := 1;
    FGrdConcentration.FixedCols        := 0;
    FGrdConcentration.FixedRows        := 0;
    FGrdConcentration.DefaultRowHeight := 20;
    FGrdConcentration.DefaultColWidth  := 40;
    FGrdConcentration.Width            := FGrdConcentration.ColCount* (FGrdConcentration.DefaultColWidth + 1) + 3;
    FGrdConcentration.Height           := (FGrdMinMaxChannels.DefaultRowHeight * 1) + 8;
//    FGrdConcentration.Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine,goEditing];

   FWQConstraintsGroup.Visible := False;
   FUpperBoundedChannelGroup.Visible := False;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintDialog.Resize;
const OPNAME = 'TWQConstraintDialog.Resize';
begin
  inherited Resize;
  try

    if (FWQConstraintsGroup.Visible) and (FUpperBoundedChannelGroup.Visible) then
    begin
      FWQConstraintsGroup.Align := alLeft;
      FWQConstraintsGroup.Width := (self.ClientWidth div 2) +50;
      FUpperBoundedChannelGroup.Align := alLeft;
      FUpperBoundedChannelGroup.Width := FWQConstraintsGroup.Width - ((C_ControlOffset*2)+50);
    end else
    begin
      FUpperBoundedChannelGroup.Align := alClient;
      FWQConstraintsGroup.Align := alClient;
    end;

   FGrdMinMaxWQConChannels.Height := (FGrdMinMaxChannels.DefaultRowHeight * FGrdMinMaxWQConChannels.RowCount) + 15;

   FlblSlopeLimit.Top :=  FGrdMinMaxWQConChannels.Top + FGrdMinMaxWQConChannels.Height + 5;
   FEdtSlopeLimit.Top := FlblSlopeLimit.Top;
   FlblEstimatedRelease.Top :=  FEdtSlopeLimit.Top + FEdtSlopeLimit.Height + 5;
   FGrdEstimatedRelease.Top := FlblEstimatedRelease.Top;
   FlblConcentration.Top :=  FGrdEstimatedRelease.Top + FGrdEstimatedRelease.Height + 5;
   FGrdConcentration.Top := FlblConcentration.Top;


  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWQConstraintDialog.Initialise: boolean;
const OPNAME = 'TWQConstraintDialog.Initialise';
begin
  Result := inherited Initialise;
  try

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWQConstraintDialog.LanguageHasChanged: boolean;
const OPNAME = 'TWQConstraintDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FAddBoundChannelbtn.Caption    := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FDeleteBoundChannelbtn.Caption := FAppModules.Language.GetString('ButtonCaption.BtnDelete');
    FLblNumOfBoundedChannels.Caption := 'No. of Bounded Channels :';
    FLblMinMaxChannels.Caption := 'Reference Channel(s) :';

    FWQConstraintsGroup.Caption := 'Water Quality Constraints :';
    FUpperBoundedChannelGroup.Caption := 'Flow Routing Upper Bounds :';

    FlblTarget.Caption := 'Target :';
    FLblNoOfBlendingChannels.Caption := 'Blending Channels :';
    FLblReservoirRef.Caption := 'Reservoir Ref. :';
    FlblWQConType.Caption := 'Type :';
    FAddWQConChannelbtn.Caption    := FAppModules.Language.GetString('ButtonCaption.BtnAdd');
    FDeleteWQConChannelbtn.Caption := FAppModules.Language.GetString('ButtonCaption.BtnDelete');

    FlblSlopeLimit.Caption := 'Limiting Slope :';
    FlblEstimatedRelease.Caption := 'Release :';
    FlblConcentration.Caption := 'Concentrations : ';
   

    Result := True;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintDialog.SetFlowConstraintGridCols(ACount : integer);
const OPNAME = 'TWQConstraintDialog.SetFlowConstraintGridCols';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TWQConstraintDialog.RestoreColourState;
const OPNAME = 'TWQConstraintDialog.RestoreColourState';
begin
  inherited RestoreColourState;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintDialog.AssignHelpContext;
const OPNAME = 'TWQConstraintDialog.AssignHelpContext';
begin
  try

  except on E: Exception do HandleError(E, OPNAME); end;
end;



end.
