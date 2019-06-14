 //
//
//  UNIT      : Contains  TWQConstraintValidator   Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 17/02/2014
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UWQConstraintValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  VoaimsCom_TLB,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UWQConstraintDialog;

type
  TWQConstraintValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FBoundChannels : boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnGrdMinMaxWQConChannelsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnGrdMinMaxWQConChannelsTopLeftChanged(Sender: TObject);

    procedure OnGrdMinMaxBoundedChannelsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
    procedure OnGrdMinMaxBoundedChannelsTopLeftChanged(Sender: TObject);

    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure DoAddWQConChannelbtn(Sender : TObject);
    procedure DoAddBoundChannelbtn(Sender : TObject);
    procedure DoDeleteWQConChannelbtn(Sender : TObject);
    procedure DoDeleteBoundChannelbtn(Sender : TObject);
    procedure RePopulateDataViewer;
    procedure DisableModelControls;
    procedure PopulateWQConst(AWQConstriantsChannel : IWQConstriantsChannel);
    procedure PopulateUpperBoundChannel(AMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel);

    procedure UpdateEdtTarget;
    procedure UpdateEdtReservoirRef;
    procedure UpdateEdtWQConType;
    procedure UpdateMinMaxWQConChannels;
    procedure UpdateMinMaxChannels(AMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel);
    procedure UpdateMinMaxChannelSign;
    procedure UpdateMinMaxWQConChannelsFactors;
    procedure UpdateEdtSlopeLimit;
    procedure UpdateEstimatedRelease(ACol, ARow : integer);
    procedure UpdateConcentration(ACol, ARow : integer);
   
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function WQConstraintDialog: TWQConstraintDialog;

  end;

implementation

uses
  SysUtils,
  VCL.Forms,
  VCL.Grids,
  VCL.Graphics,
  UUtilities,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UPlanningModelDataObject,
  UWQConstraintData,
  UErrorHandlingOperations;

{******************************************************************************}
{* TWQConstraintValidator                                                    *}
{******************************************************************************}

procedure TWQConstraintValidator.CreateMemberObjects;
const OPNAME = 'TWQConstraintValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel  := TWQConstraintDialog.Create(FPanelOwner,FAppModules);

    with WQConstraintDialog do
    begin
      EdtTarget.FieldProperty := FAppModules.FieldProperties.FieldProperty('WQConstTarget');
      EdtTarget.OnEnter        := OnEditControlEnter;
      EdtTarget.OnExit         := OnEditControltExit;

      EdtReservoirRef.FieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirRef');
      EdtReservoirRef.OnEnter        := OnEditControlEnter;
      EdtReservoirRef.OnExit         := OnEditControltExit;

      CmbWQConType.FieldProperty := FAppModules.FieldProperties.FieldProperty('WQConType');
      CmbWQConType.OnEnter        := OnEditControlEnter;
      CmbWQConType.OnExit         := OnEditControltExit;

      EdtSlopeLimit.FieldProperty := FAppModules.FieldProperties.FieldProperty('SlopeLimit');
      EdtSlopeLimit.OnEnter        := OnEditControlEnter;
      EdtSlopeLimit.OnExit         := OnEditControltExit;

      EdtNoOfBlendingChannels.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxUpperBoundChannel');
      EdtNoOfBlendingChannels.OnEnter        := OnEditControlEnter;
      EdtNoOfBlendingChannels.OnExit         := OnEditControltExit;


      EdtNumOfBoundedChannels.FieldProperty := FAppModules.FieldProperties.FieldProperty('MinMaxUpperBoundChannel');
      EdtNumOfBoundedChannels.OnEnter        := OnEditControlEnter;
      EdtNumOfBoundedChannels.OnExit         := OnEditControltExit;


      GrdMinMaxWQConChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReferenceChannel'));
      GrdMinMaxWQConChannels.OnEnter        := OnEditControlEnter;
      GrdMinMaxWQConChannels.OnExit         := OnEditControltExit;

      GrdMinMaxWQConChannels.Cells[0,0] := 'Channels';
      GrdMinMaxWQConChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReferenceChannel'));
      GrdMinMaxWQConChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('RefChannelFactor'));
      GrdMinMaxWQConChannels.OnEnter        := OnEditControlEnter;
      GrdMinMaxWQConChannels.OnExit         := OnEditControltExit;

      GrdMinMaxWQConChannels.OnSelectCell     := OnGrdMinMaxWQConChannelsSelectCell;
      GrdMinMaxWQConChannels.OnTopLeftChanged := OnGrdMinMaxWQConChannelsTopLeftChanged;

      GrdMinMaxChannels.OnSelectCell     := OnGrdMinMaxBoundedChannelsSelectCell;
      GrdMinMaxChannels.OnTopLeftChanged := OnGrdMinMaxBoundedChannelsTopLeftChanged;

      CbxChannel.FieldProperty := FAppModules.FieldProperties.FieldProperty('ReferenceChannel');
      CbxChannel.OnEnter       := OnEditControlEnter;
      CbxChannel.OnChange      := OnEditControltExit;

      CbxChannelSign.FieldProperty := FAppModules.FieldProperties.FieldProperty('ReferenceChannel');
      CbxChannelSign.OnEnter       := OnEditControlEnter;
      CbxChannelSign.OnChange      := OnEditControltExit;


      GrdMinMaxWQConChannels.Cells[2,0] := 'Factors';
      GrdMinMaxWQConChannels.Cells[1,0] := 'Sign';
      GrdMinMaxChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BoundedChannels'));
      GrdMinMaxChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BoundedChannels'));
      GrdMinMaxChannels.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('BoundedChannels'));

      GrdMinMaxChannels.Cells[1,0] := 'Channels';

      AddWQConChannelbtn.OnClick := DoAddWQConChannelbtn;
      DeleteWQConChannelbtn.OnClick := DoDeleteWQConChannelbtn;

      AddBoundChannelbtn.OnClick := DoAddBoundChannelbtn;
      DeleteBoundChannelbtn.OnClick := DoDeleteBoundChannelbtn;
    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.DestroyMemberObjects;
const OPNAME = 'TWQConstraintValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWQConstraintValidator.Initialise: boolean;
const OPNAME = 'TWQConstraintValidator.Initialise';
var
  LIndex : integer;
  LChannel : IGeneralFlowChannel;
  LChannelList : IChannelList;
begin
  Result := False;
  try
    LChannelList    := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    WQConstraintDialog.CbxChannelSign.Clear;
    WQConstraintDialog.CbxChannelSign.Items.Add('+');
    WQConstraintDialog.CbxChannelSign.Items.Add('-');
    FBoundChannels := False;
    WQConstraintDialog.CbxChannel.Clear;
    WQConstraintDialog.CbxChannel.Items.AddObject('0 - None', TObject(0));
    for LIndex := 0 to LChannelList.ChannelCount - 1 do
    begin
      LChannel := LChannelList.ChannelByIndex[lIndex];
      if (LChannel.ChannelType in [2,8]) then
        WQConstraintDialog.CbxChannel.Items.AddObject(LChannel.ChannelName, TObject(LChannel.ChannelNumber));
    end;
    WQConstraintDialog.CbxChannel.Sorted := True;

    WQConstraintDialog.GrdMinMaxWQConChannels.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    WQConstraintDialog.GrdMinMaxWQConChannels.OnColEnter         := OnStringGridColEnter;

    WQConstraintDialog.GrdEstimatedRelease.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    WQConstraintDialog.GrdEstimatedRelease.OnColEnter         := OnStringGridColEnter;

    WQConstraintDialog.GrdConcentration.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    WQConstraintDialog.GrdConcentration.OnColEnter         := OnStringGridColEnter;

    WQConstraintDialog.GrdMinMaxChannels.OnBeforeCellChange := OnStringGridCellDataHasChanged;
    WQConstraintDialog.GrdMinMaxChannels.OnColEnter         := OnStringGridColEnter;

    Result := FPanel.Initialise;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TWQConstraintValidator.LanguageHasChanged: boolean;
const OPNAME = 'TWQConstraintValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Water Quality Constraints'; //FAppModules.Language.GetString('NetworkFeatures.MinmaxFlowFeature');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.ClearDataViewer;
const OPNAME = 'TWQConstraintValidator.ClearDataViewer';
var
  LCol : integer;
  LRow : integer;
begin
  inherited ClearDataViewer;
  try
    WQConstraintDialog.CmbWQConType.ItemIndex := -1;
    for LCol := 0 to WQConstraintDialog.GrdMinMaxWQConChannels.ColCount-1 do
      for LRow := 1 to WQConstraintDialog.GrdMinMaxWQConChannels.RowCount-1 do
       WQConstraintDialog.GrdMinMaxWQConChannels.Cells[LCol,LRow] := '';
    for LCol := 0 to 9 do
    begin
      WQConstraintDialog.GrdEstimatedRelease.Cells[LCol,0] :='';
      WQConstraintDialog.GrdConcentration.Cells[LCol,0] :='';
    end;

    for LCol := 0 to WQConstraintDialog.GrdMinMaxChannels.ColCount-1 do
      for LRow := 1 to WQConstraintDialog.GrdMinMaxChannels.RowCount-1 do
       WQConstraintDialog.GrdMinMaxChannels.Cells[LCol,LRow] := '';


    FBoundChannels := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.PopulateDataViewer;
const OPNAME = 'TWQConstraintValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtMinMaxConstraints);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.RePopulateDataViewer;
const OPNAME = 'TWQConstraintValidator.RePopulateDataViewer';
var
  LWQConstriantData : IWQConstraintData;
  LWQConstriantsChannel : IWQConstriantsChannel;
  LMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel;
begin
  try
    if Identifier > 0 then
    begin
      LWQConstriantData := TPlanningModelDataObject(FAppModules.Model.ModelData).CastWQConstriantData;
      if LWQConstriantData <> nil then
      begin
        LWQConstriantsChannel := LWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];   // Hlamzo *121*0835494048
        if LWQConstriantsChannel <> nil then
          PopulateWQConst(LWQConstriantsChannel);
        LMinMaxUpperBoundChannel := LWQConstriantData.MinMaxUpperBoundChannelNo[Identifier];
        if LMinMaxUpperBoundChannel <> nil then
          PopulateUpperBoundChannel(LMinMaxUpperBoundChannel);
      end;
    end;
    WQConstraintDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.DisableModelControls;
const OPNAME = 'TWQConstraintValidator.DisableModelControls';
begin
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWQConstraintValidator.PopulateWQConst(AWQConstriantsChannel : IWQConstriantsChannel);
const OPNAME = 'TWQConstraintValidator.RePopulateFlowConstraints';
var
  LEstimatedRelease,
  LConcentration,
  LChannelFactors,
  LBlendingChannels : TStringList;
  LIndex : integer;
  LSign : string;
  LFactor : double;
begin
  try
    LBlendingChannels := TStringList.Create;
    LChannelFactors := TStringList.Create;
    LEstimatedRelease := TStringList.Create;
    LConcentration := TStringList.Create;
    WQConstraintDialog.WQConstraintsGroup.Visible := True;
    WQConstraintDialog.CmbWQConType.Clear;
    for LIndex := 0 to 2 do
      WQConstraintDialog.CmbWQConType.Items.AddObject(IntToStr(LIndex),TObject(LIndex));

    try
      for LIndex := 1 to 10 do
      begin
        WQConstraintDialog.GrdEstimatedRelease.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('EstimatedRelease'));
        WQConstraintDialog.GrdConcentration.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('Concentration'));
      end;
      WQConstraintDialog.EdtNoOfBlendingChannels.SetFieldValue(AWQConstriantsChannel.NoOfRefChannelsBlending);
      WQConstraintDialog.EdtTarget.SetFieldValue(AWQConstriantsChannel.Target);
      WQConstraintDialog.EdtReservoirRef.SetFieldValue(AWQConstriantsChannel.ReservoirRef);

      WQConstraintDialog.CmbWQConType.SetFieldIndex(WQConstraintDialog.CmbWQConType.Items.IndexOfObject(TObject(AWQConstriantsChannel.WQConType)));

      WQConstraintDialog.EdtSlopeLimit.SetFieldValue(AWQConstriantsChannel.LimitingSlope);
      LBlendingChannels.CommaText := AWQConstriantsChannel.BlendingChannels;
      LChannelFactors.CommaText := AWQConstriantsChannel.BlendingChannelFactors;
      WQConstraintDialog.GrdMinMaxWQConChannels.ColWidths[1] := 30;
      WQConstraintDialog.GrdMinMaxWQConChannels.Height := (WQConstraintDialog.GrdMinMaxWQConChannels.DefaultRowHeight *
                                                              WQConstraintDialog.GrdMinMaxWQConChannels.RowCount) + 8;
      if (LBlendingChannels.Count>0) then
      begin
        WQConstraintDialog.GrdMinMaxWQConChannels.RowCount := LBlendingChannels.Count + 1;
        WQConstraintDialog.GrdMinMaxWQConChannels.Height := (WQConstraintDialog.GrdMinMaxWQConChannels.DefaultRowHeight *
                                                              WQConstraintDialog.GrdMinMaxWQConChannels.RowCount) + 8;
        for LIndex := 0 to LBlendingChannels.Count-1 do
        begin
          WQConstraintDialog.GrdMinMaxWQConChannels.Cells[0,LIndex+1] := LBlendingChannels[LIndex];
          LSign := Copy(LChannelFactors[LIndex],1,1);
          WQConstraintDialog.GrdMinMaxWQConChannels.Cells[1,LIndex+1] := LSign;
          LFactor := 0.00;
          if Trim(Copy(LChannelFactors[LIndex],2,Length(LChannelFactors[LIndex])-1)) <> '' then
            LFactor := StrToFloat(Trim(Copy(LChannelFactors[LIndex],2,Length(LChannelFactors[LIndex])-1)));
          WQConstraintDialog.GrdMinMaxWQConChannels.Cells[2,LIndex+1] := FormatFloat('0.00',LFactor);
        end;
      end;

      WQConstraintDialog.EdtSlopeLimit.Visible         := (AWQConstriantsChannel.WQConType = 2);
      WQConstraintDialog.GrdEstimatedRelease.Visible   := (AWQConstriantsChannel.WQConType = 2);
      WQConstraintDialog.GrdConcentration.Visible      := (AWQConstriantsChannel.WQConType = 2);
      WQConstraintDialog.lblConcentration.Visible      := (AWQConstriantsChannel.WQConType = 2);
      WQConstraintDialog.lblSlopeLimit.Visible         := (AWQConstriantsChannel.WQConType = 2);
      WQConstraintDialog.lblEstimatedRelease.Visible   := (AWQConstriantsChannel.WQConType = 2);

      if WQConstraintDialog.EdtSlopeLimit.Visible then
      begin
        LEstimatedRelease.CommaText := AWQConstriantsChannel.EstimatedRelease;
        LConcentration.CommaText := AWQConstriantsChannel.Concentration;
        for LIndex := 0 to 9 do
        begin
          WQConstraintDialog.GrdEstimatedRelease.Cells[LIndex,0] :=LEstimatedRelease[LIndex];
          WQConstraintDialog.GrdConcentration.Cells[LIndex,0] :=LConcentration[LIndex];
        end;
      end;

    finally
      LBlendingChannels.Free;
      LChannelFactors.Free;
      LEstimatedRelease.Free;
      LConcentration.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.DoAddWQConChannelbtn(Sender : TObject);
const OPNAME = 'TWQConstraintValidator.DoAddWQConChannelbtn';
begin
  try
    WQConstraintDialog.GrdMinMaxWQConChannels.RowCount := WQConstraintDialog.GrdMinMaxWQConChannels.RowCount + 1;
    WQConstraintDialog.GrdMinMaxWQConChannels.Height := (WQConstraintDialog.GrdMinMaxWQConChannels.DefaultRowHeight *
                                                              WQConstraintDialog.GrdMinMaxWQConChannels.RowCount) + 8;

    WQConstraintDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.DoAddBoundChannelbtn(Sender : TObject);
const OPNAME = 'TWQConstraintValidator.DoAddWQConChannelbtn';
begin
  try
    WQConstraintDialog.GrdMinMaxChannels.RowCount := WQConstraintDialog.GrdMinMaxChannels.RowCount + 1;
    WQConstraintDialog.GrdMinMaxChannels.Height := (WQConstraintDialog.GrdMinMaxChannels.DefaultRowHeight *
                                                   WQConstraintDialog.GrdMinMaxChannels.RowCount) + 8;
    WQConstraintDialog.Resize;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWQConstraintValidator.PopulateUpperBoundChannel(AMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel);
const OPNAME = 'TWQConstraintValidator.PopulateUpperBoundChannel';
var
  LIndex : integer;
  LBoundChannels : TStringList;
  LSign : string;
  LChannelNo : integer;
begin
  try
    LBoundChannels := TStringList.Create;
    try
      WQConstraintDialog.UpperBoundedChannelGroup.Visible := True;
      LBoundChannels.CommaText := AMinMaxUpperBoundChannel.BoundedChannels;
      WQConstraintDialog.EdtNumOfBoundedChannels.SetFieldValue(AMinMaxUpperBoundChannel.NoOfBoundedChannels);
      WQConstraintDialog.GrdMinMaxChannels.ColWidths[0] := 30;
      WQConstraintDialog.GrdMinMaxChannels.ColWidths[2] := 30;
      WQConstraintDialog.GrdMinMaxChannels.Height := (WQConstraintDialog.GrdMinMaxChannels.DefaultRowHeight *
                                                                WQConstraintDialog.GrdMinMaxChannels.RowCount) + 8;
      if LBoundChannels.Count > 0 then
      begin
        WQConstraintDialog.GrdMinMaxChannels.RowCount :=  LBoundChannels.Count + 1;
        WQConstraintDialog.GrdMinMaxChannels.Height := (WQConstraintDialog.GrdMinMaxChannels.DefaultRowHeight *
                                                                WQConstraintDialog.GrdMinMaxChannels.RowCount) + 8;
        for LIndex := 0 to LBoundChannels.Count-1 do
        begin
          LSign := Copy(LBoundChannels[LIndex],1,1);
          WQConstraintDialog.GrdMinMaxChannels.Cells[0,LIndex+1] := LSign;
          LChannelNo := StrToInt(Trim(Copy(LBoundChannels[LIndex],2,Length(LBoundChannels[LIndex])-1)));

          WQConstraintDialog.GrdMinMaxChannels.Cells[1,LIndex+1] := IntToStr(LChannelNo);
        end;
      end;
    finally
      LBoundChannels.Free;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function TWQConstraintValidator.SaveState: boolean;
const OPNAME = 'TWQConstraintValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWQConstraintValidator.WQConstraintDialog : TWQConstraintDialog;
const OPNAME = 'TWQConstraintValidator.WQConstraintDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TWQConstraintDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWQConstraintValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TWQConstraintValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TWQConstraintValidator.StudyHasChanged: boolean;
const OPNAME = 'TWQConstraintValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TWQConstraintValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TWQConstraintValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with WQConstraintDialog do
    begin
      if ((Sender = EdtTarget) AND
          (EdtTarget.HasValueChanged)) then
        UpdateEdtTarget;

      if ((Sender = EdtReservoirRef) AND
          (EdtReservoirRef.HasValueChanged)) then
        UpdateEdtReservoirRef;

      if ((Sender = CmbWQConType) AND
          (CmbWQConType.HasValueChanged)) then
           UpdateEdtWQConType;
      if ((Sender = EdtSlopeLimit) AND
          (EdtSlopeLimit.HasValueChanged)) then
           UpdateEdtSlopeLimit;

      if (Sender = CbxChannel) then
        UpdateMinMaxWQConChannels;

      if (Sender = CbxChannelSign) then
          UpdateMinMaxChannelSign;


    end;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.UpdateConcentration(ACol, ARow : integer);
const OPNAME = 'TWQConstraintValidator.UpdateEstimatedRelease';
var
  LWQConstriantsChannel : IWQConstriantsChannel;
  LFactors : TStringList;
  LIndex : integer;
begin
  try
    LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];
    if (LWQConstriantsChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        LIndex := GrdConcentration.Col;
        LFactors := TStringList.Create;
        try
          LFactors.CommaText := LWQConstriantsChannel.Concentration;
          if GrdConcentration.Cells[ACol,ARow] <> '' then
          begin
            if (LFactors.count>0) and (LFactors.count>=LIndex) then
              LFactors[LIndex] := GrdConcentration.Cells[ACol,ARow];
          end;
          LWQConstriantsChannel.Concentration := LFactors.CommaText;
        finally
          LFactors.Free;
        end;
      end;
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWQConstraintValidator.UpdateEdtReservoirRef;
const OPNAME = 'TWQConstraintValidator.UpdateEdtReservoirRef';
var
  LWQConstriantsChannel : IWQConstriantsChannel;
  LMessage : string;
begin
  try
    LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];

    if (LWQConstriantsChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtReservoirRef.FieldProperty.FieldName, EdtReservoirRef.Text,lMessage)) then
        begin
          EdtReservoirRef.FieldValidationError := LMessage;
          LWQConstriantsChannel.ReservoirRef := StrToInt(EdtReservoirRef.Text);
          EdtReservoirRef.SetFieldValue(LWQConstriantsChannel.ReservoirRef);
        end
        else
          EdtReservoirRef.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.UpdateEdtSlopeLimit;
const OPNAME = 'TWQConstraintValidator.UpdateEdtTarget';
var
  LWQConstriantsChannel : IWQConstriantsChannel;
  LMessage : string;
begin
  try
    LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];

    if (LWQConstriantsChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtSlopeLimit.FieldProperty.FieldName, EdtSlopeLimit.Text,lMessage)) then
        begin
          EdtSlopeLimit.FieldValidationError := LMessage;
          LWQConstriantsChannel.LimitingSlope := StrToInt(EdtSlopeLimit.Text);
          EdtSlopeLimit.SetFieldValue(LWQConstriantsChannel.LimitingSlope);
        end
        else
          EdtSlopeLimit.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TWQConstraintValidator.UpdateEdtTarget;
const OPNAME = 'TWQConstraintValidator.UpdateEdtTarget';
var
  LWQConstriantsChannel : IWQConstriantsChannel;
  LMessage : string;
begin
  try
    LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];

    if (LWQConstriantsChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            EdtTarget.FieldProperty.FieldName, EdtTarget.Text,lMessage)) then
        begin
          EdtTarget.FieldValidationError := lMessage;
          LWQConstriantsChannel.Target := StrToFloat(EdtTarget.Text);
          EdtTarget.SetFieldValue(LWQConstriantsChannel.Target);
        end
        else
          EdtTarget.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.UpdateEdtWQConType;
const OPNAME = 'TWQConstraintValidator.UpdateEdtTarget';
var
  LWQConstriantsChannel : IWQConstriantsChannel;
  LMessage : string;
begin
  try
    LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];

    if (LWQConstriantsChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            CmbWQConType.FieldProperty.FieldName, CmbWQConType.Text,lMessage)) then
        begin
          EdtTarget.FieldValidationError := LMessage;
          LWQConstriantsChannel.WQConType := StrToInt(CmbWQConType.Text);
          CmbWQConType.Items.IndexOf(IntToStr(LWQConstriantsChannel.WQConType));
          PopulateDataViewer;
        end
        else
          EdtTarget.FieldValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.UpdateEstimatedRelease(ACol, ARow : integer);
const OPNAME = 'TWQConstraintValidator.UpdateEstimatedRelease';
var
  LWQConstriantsChannel : IWQConstriantsChannel;
  LFactors : TStringList;
  LIndex : integer;
begin
  try
    LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];
    if (LWQConstriantsChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        LIndex := GrdEstimatedRelease.Col;
        LFactors := TStringList.Create;
        try
          LFactors.CommaText := LWQConstriantsChannel.EstimatedRelease;
          if GrdEstimatedRelease.Cells[ACol,ARow] <> '' then
          begin
            if (LFactors.count>0) and (LFactors.count>=LIndex) then
              LFactors[LIndex] := GrdEstimatedRelease.Cells[ACol,ARow];
          end;
          LWQConstriantsChannel.EstimatedRelease := LFactors.CommaText;
        finally
          LFactors.Free;
        end;
      end;
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.OnStringGridCellDataHasChanged
                                         (ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TWQConstraintValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with WQConstraintDialog do
    begin
      if (GrdMinMaxWQConChannels = ASender) then
        UpdateMinMaxWQConChannelsFactors;

      if (GrdEstimatedRelease = ASender) then
        UpdateEstimatedRelease(ACol, ARow);

      if (GrdConcentration = ASender) then
        UpdateConcentration(ACol, ARow);

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWQConstraintValidator.OnGrdMinMaxWQConChannelsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TWQConstraintValidator.OnGrdMinMaxWQConChannelsSelectCell';
var
  LChannel : string;
  LSign : string;
begin
  try
    with WQConstraintDialog do
    begin

      if ACol = 0 then
      begin
        CbxChannel.Top  := 2 + GrdMinMaxWQConChannels.Top + ((1 + GrdMinMaxWQConChannels.DefaultRowHeight) *
                                    (ARow - GrdMinMaxWQConChannels.TopRow + 1));
        LChannel        := Trim(GrdMinMaxWQConChannels.Cells[0, ARow]);
        CbxChannel.ItemIndex := CbxChannel.Items.IndexOf(LChannel);
        CbxChannel.Parent := WQConstraintsGroup;
         CbxChannel.Left := GrdMinMaxWQConChannels.Left;
        CbxChannel.Visible := True;
        if (GrdMinMaxWQConChannels.ValidationError[ACol, ARow, gveCellContext] <> '') then
        begin
          CbxChannel.ValidationError      := GrdMinMaxWQConChannels.ValidationError[ACol, ARow, gveCellContext];
          CbxChannel.InValidationError    := True;
          CbxChannel.ShowErrorState(True);
        end
        else
        begin
          CbxChannel.ValidationError   := '';
          CbxChannel.InValidationError := False;
          CbxChannel.ShowErrorState(False);
        end;
      end
      else
        CbxChannel.Visible := False;

      if ACol = 1 then
      begin
        CbxChannelSign.Parent := WQConstraintsGroup;
        CbxChannelSign.Top := 2 + GrdMinMaxWQConChannels.Top + ((1 + GrdMinMaxWQConChannels.DefaultRowHeight) *
                                    (ARow - GrdMinMaxWQConChannels.TopRow + 1));
        LSign := Trim(GrdMinMaxWQConChannels.Cells[1, ARow]);
        CbxChannelSign.ItemIndex := CbxChannelSign.Items.IndexOf(LSign);
        CbxChannelSign.Left := GrdMinMaxWQConChannels.Left + GrdMinMaxWQConChannels.ColWidths[0]+3;
        CbxChannelSign.BringToFront;
        CbxChannelSign.Visible := True;

        FBoundChannels := False;
        if (GrdMinMaxWQConChannels.ValidationError[ACol, ARow, gveCellContext] <> '') then
        begin
          CbxChannelSign.ValidationError      := GrdMinMaxWQConChannels.ValidationError[ACol, ARow, gveCellContext];
          CbxChannelSign.InValidationError    := True;
          CbxChannelSign.ShowErrorState(True);
        end
        else
        begin
          CbxChannelSign.ValidationError   := '';
          CbxChannelSign.InValidationError := False;
          CbxChannelSign.ShowErrorState(False);
        end;

      end else
        CbxChannelSign.Visible := False;

    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstraintValidator.OnGrdMinMaxWQConChannelsTopLeftChanged(Sender: TObject);
const OPNAME = 'TWQConstraintValidator.OnGrdMinMaxWQConChannelsTopLeftChanged';
begin
  try
    WQConstraintDialog.CbxChannel.Visible := False;
    WQConstraintDialog.CbxChannelSign.Visible := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TWQConstraintValidator.OnGrdMinMaxBoundedChannelsSelectCell(Sender: TObject; ACol, ARow: Integer; var CanSelect: Boolean);
const OPNAME = 'TWQConstraintValidator.OnGrdMinMaxBoundedChannelsSelectCell';
var
  LSign,
  LChannel : string;
begin
  try
    with WQConstraintDialog do
    begin
      if ACol = 1 then
      begin
        CbxChannel.Parent := UpperBoundedChannelGroup;
        CbxChannel.Top  := 2 + GrdMinMaxChannels.Top + ((1 + GrdMinMaxChannels.DefaultRowHeight) *
                                    (ARow - GrdMinMaxChannels.TopRow + 1));
        LChannel        := Trim(GrdMinMaxChannels.Cells[1, ARow]);
        CbxChannel.ItemIndex := CbxChannel.Items.IndexOf(LChannel);
        CbxChannel.Left := GrdMinMaxChannels.Left + GrdMinMaxChannels.ColWidths[0] + 5;
        CbxChannel.BringToFront;
        CbxChannel.Visible := True;
        FBoundChannels := True;
        if (GrdMinMaxChannels.ValidationError[ACol, ARow, gveCellContext] <> '') then
        begin
          CbxChannel.ValidationError      := GrdMinMaxChannels.ValidationError[ACol, ARow, gveCellContext];
          CbxChannel.InValidationError    := True;
          CbxChannel.ShowErrorState(True);
        end
        else
        begin
          CbxChannel.ValidationError   := '';
          CbxChannel.InValidationError := False;
          CbxChannel.ShowErrorState(False);
        end;
      end
      else
        CbxChannel.Visible := False;
      if ACol = 0 then
      begin
        CbxChannelSign.Top := 2 + GrdMinMaxChannels.Top + ((1 + GrdMinMaxChannels.DefaultRowHeight) *
                                    (ARow - GrdMinMaxChannels.TopRow + 1));
        CbxChannelSign.Parent := UpperBoundedChannelGroup;                            
        LSign := Trim(GrdMinMaxChannels.Cells[0, ARow]);
        CbxChannelSign.ItemIndex := CbxChannelSign.Items.IndexOf(LSign);
        CbxChannelSign.Left := GrdMinMaxChannels.Left+3;
        CbxChannelSign.BringToFront;
        CbxChannelSign.Visible := True;

        FBoundChannels := True;
        if (GrdMinMaxChannels.ValidationError[ACol, ARow, gveCellContext] <> '') then
        begin
          CbxChannelSign.ValidationError      := GrdMinMaxChannels.ValidationError[ACol, ARow, gveCellContext];
          CbxChannelSign.InValidationError    := True;
          CbxChannelSign.ShowErrorState(True);
        end
        else
        begin
          CbxChannelSign.ValidationError   := '';
          CbxChannelSign.InValidationError := False;
          CbxChannelSign.ShowErrorState(False);
        end;
     end
      else
        CbxChannelSign.Visible := False;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TWQConstraintValidator.OnGrdMinMaxBoundedChannelsTopLeftChanged(Sender: TObject);
const OPNAME = 'TWQConstraintValidator.OnGrdMinMaxBoundedChannelsTopLeftChanged';
begin
  try
     WQConstraintDialog.CbxChannel.Visible := False;
     WQConstraintDialog.CbxChannelSign.Visible := False;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TWQConstraintValidator.UpdateMinMaxWQConChannels;
const OPNAME = 'TWQConstraintValidator.UpdateMinMaxWQConChannels';
var
  LWQConstriantsChannel : IWQConstriantsChannel;
  LMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel;
  LChannels : TStringList;
  LChannelsFactors : TStringList;
  LChannelNum,
  LIndex : integer;
begin
  try
    with WQConstraintDialog do
    begin
      LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];
       LMinMaxUpperBoundChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.MinMaxUpperBoundChannelNo[Identifier];

      if not(FBoundChannels) and (LWQConstriantsChannel <> nil) then
      begin
        if (GrdMinMaxWQConChannels.Col = 0 ) and (GrdMinMaxWQConChannels.Row > 0) then
        begin
          LChannelNum := 0;
          if (CbxChannel.ItemIndex >= 0) then
            LChannelNum := Integer(CbxChannel.Items.Objects[CbxChannel.ItemIndex]);
          if LChannelNum = 0 then
            Exit;
          LIndex := GrdMinMaxWQConChannels.Row;
          GrdMinMaxWQConChannels.ValidationError[0, LIndex, gveCellContext] := '';
          GrdMinMaxWQConChannels.Cells[0,LIndex] := IntToStr(LChannelNum);
          LChannels := TStringList.Create;
          LChannelsFactors := TStringList.Create;
          try
            LChannels.CommaText := LWQConstriantsChannel.BlendingChannels;
            LChannelsFactors.CommaText := LWQConstriantsChannel.BlendingChannelFactors;
            if GrdMinMaxWQConChannels.Cells[0,LIndex] <> '' then
            begin
              if (LChannels.count>0) and (LChannels.count>=LIndex) then
              begin

                LChannels[LIndex-1] := IntToStr(LChannelNum);
              end
              else
              begin
                LChannels.Add(IntToStr(LChannelNum));
                LChannelsFactors.Add(' 0.00');
              end;
            end;
            LWQConstriantsChannel.BlendingChannels := LChannels.CommaText;
            LWQConstriantsChannel.BlendingChannelFactors := LChannelsFactors.CommaText;
          finally
            LChannels.Free;
            LChannelsFactors.Free;
          end;
        end;
      end
      else
       if (FBoundChannels) and (LMinMaxUpperBoundChannel <> nil) and(GrdMinMaxChannels.Col = 1 ) and (GrdMinMaxChannels.Row > 0) then
          UpdateMinMaxChannels(LMinMaxUpperBoundChannel);
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.UpdateMinMaxChannels(AMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel);
const OPNAME = 'TWQConstraintValidator.UpdateMinMaxChannels';
var

  LChannels : TStringList;
  LChannelNum,
  LIndex : integer;
  LSign : string;
begin
  try

    if (AMinMaxUpperBoundChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        LChannelNum := 0;
        if (CbxChannel.ItemIndex >= 0) then
          LChannelNum := Integer(CbxChannel.Items.Objects[CbxChannel.ItemIndex]);
        if LChannelNum = 0 then
          Exit;
        if (GrdMinMaxChannels.Col = 1 ) and (GrdMinMaxChannels.Row > 0) then
        begin

          LIndex := GrdMinMaxChannels.Row;
          GrdMinMaxChannels.ValidationError[1, LIndex, gveCellContext] := '';
          GrdMinMaxChannels.Cells[1,LIndex] := IntToStr(LChannelNum);
          LChannels := TStringList.Create;
          try
            LChannels.CommaText := AMinMaxUpperBoundChannel.BoundedChannels;
            if GrdMinMaxChannels.Cells[1,LIndex] <> '' then
            begin
              if (LChannels.count>0) and (LChannels.count>=LIndex) then
              begin
                LSign := Copy(LChannels[LIndex-1],1,1);
                LChannels[LIndex-1] := LSign + IntToStr(LChannelNum);
              end
              else
              begin
                LChannels.Add('+'+IntToStr(LChannelNum));
              end;
            end;
            AMinMaxUpperBoundChannel.BoundedChannels := LChannels.CommaText;
          finally
            LChannels.Free;
          end;
        end;
      end;
    end;
    //PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWQConstraintValidator.UpdateMinMaxChannelSign;
const OPNAME = 'TWQConstraintValidator.UpdateMinMaxChannelSign';
var
  LMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel;
  LWQConstriantsChannel : IWQConstriantsChannel;
  LChannels : TStringList;
  LChannelNum,
  LIndex : integer;
  LSign : string;
  LFactor : double;
begin
  try
    with WQConstraintDialog do
    begin
      LMinMaxUpperBoundChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.MinMaxUpperBoundChannelNo[Identifier];
      LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];
      if (FBoundChannels) and (LMinMaxUpperBoundChannel <> nil) then
      begin
        LSign := '';
        if (CbxChannelSign.ItemIndex >= 0) then
          LSign := CbxChannelSign.Items.Strings[CbxChannelSign.ItemIndex];
        if LSign = '' then
          Exit;
        if (GrdMinMaxChannels.Col = 0 ) and (GrdMinMaxChannels.Row > 0) then
        begin

          LIndex := GrdMinMaxChannels.Row;
          GrdMinMaxChannels.ValidationError[0, LIndex, gveCellContext] := '';
          GrdMinMaxChannels.Cells[0,LIndex] := LSign;
          LChannels := TStringList.Create;
          try
            LChannels.CommaText := LMinMaxUpperBoundChannel.BoundedChannels;
            if GrdMinMaxChannels.Cells[0,LIndex] <> '' then
            begin
              if (LChannels.count>0) and (LChannels.count>=LIndex) then
              begin
                LChannelNum := StrToInt(Trim(Copy(LChannels[LIndex-1],2,Length(LChannels[LIndex-1])-1)));
                LChannels[LIndex-1] := LSign + IntToStr(LChannelNum);
              end
            end;
            LMinMaxUpperBoundChannel.BoundedChannels := LChannels.CommaText;
          finally
            LChannels.Free;
          end;
        end;
      end;

      if not(FBoundChannels) and (LWQConstriantsChannel <> nil) then
      begin
         LSign := '';
        if (CbxChannelSign.ItemIndex >= 0) then
          LSign := CbxChannelSign.Items.Strings[CbxChannelSign.ItemIndex];
        if LSign = '' then
          Exit;
        if (GrdMinMaxWQConChannels.Col = 1 ) and (GrdMinMaxWQConChannels.Row > 0) then
        begin

          LIndex := GrdMinMaxWQConChannels.Row;
          GrdMinMaxWQConChannels.ValidationError[1, LIndex, gveCellContext] := '';
          GrdMinMaxWQConChannels.Cells[1,LIndex] := LSign;
          LChannels := TStringList.Create;
          try
            LChannels.CommaText := LWQConstriantsChannel.BlendingChannelFactors;
            if GrdMinMaxWQConChannels.Cells[1,LIndex] <> '' then
            begin
              if (LChannels.count>0) and (LChannels.count>=LIndex) then
              begin
                LFactor :=  0.00;
                if Trim(Copy(LChannels[LIndex-1],2,Length(LChannels[LIndex-1])-1)) <> '' then
                  LFactor := StrToFloat(Trim(Copy(LChannels[LIndex-1],2,Length(LChannels[LIndex-1])-1)));
                LChannels[LIndex-1] := LSign + FormatFloat('0.00',LFactor);
              end
            end;
            LWQConstriantsChannel.BlendingChannelFactors := LChannels.CommaText;
          finally
            LChannels.Free;
          end;
        end;

      
      end;
      
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TWQConstraintValidator.UpdateMinMaxWQConChannelsFactors;
const OPNAME = 'TWQConstraintValidator.UpdateMinMaxWQConChannelsFactors';
var
  LWQConstriantsChannel : IWQConstriantsChannel;
  LChannelsFactors : TStringList;
  LIndex : integer;
begin
  try
    LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];
    if (LWQConstriantsChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        if (GrdMinMaxWQConChannels.Col = 2 ) and (GrdMinMaxWQConChannels.Row > 0) then
        begin
          LIndex := GrdMinMaxWQConChannels.Row;
          GrdMinMaxWQConChannels.ValidationError[0, LIndex, gveCellContext] := '';
          LChannelsFactors := TStringList.Create;
          try
            LChannelsFactors.CommaText := LWQConstriantsChannel.BlendingChannelFactors;
            if GrdMinMaxWQConChannels.Cells[1,LIndex] <> '' then
            begin
              if (LChannelsFactors.count>0) and (LChannelsFactors.count>=LIndex) then
                LChannelsFactors[LIndex-1] := GrdMinMaxWQConChannels.Cells[1,LIndex] +GrdMinMaxWQConChannels.Cells[2,LIndex];
            end;
            LWQConstriantsChannel.BlendingChannelFactors := LChannelsFactors.CommaText;
          finally
            LChannelsFactors.Free;
          end;
        end;
      end;
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TWQConstraintValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TWQConstraintValidator.DoContextValidation';
begin
  try


  except on E: Exception do HandleError(E, OPNAME) end;
end;



procedure TWQConstraintValidator.DoDeleteWQConChannelbtn(Sender : TObject);
const OPNAME = 'TWQConstraintValidator.DoDeleteWQConChannelbtn';
var
  LIndex : integer;
  LChannels : TStringList;
  LChannelsFactors : TStringList;
  LWQConstriantsChannel : IWQConstriantsChannel;
begin
  try
   LWQConstriantsChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.WQConstraintsChannelByChannelNo[Identifier];
    if (LWQConstriantsChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        LIndex := GrdMinMaxWQConChannels.Row;
        if LIndex > 0 then
        begin
          LChannels := TStringList.Create;
          LChannelsFactors := TStringList.Create;
          try
            LChannels.CommaText := LWQConstriantsChannel.BlendingChannels;
            LChannelsFactors.CommaText := LWQConstriantsChannel.BlendingChannelFactors;
            if GrdMinMaxWQConChannels.Cells[0,LIndex] <> '' then
            begin
              if (LChannels.count>0) and (LChannels.count>=LIndex) then
              begin
                LChannels.Delete(LIndex-1);
                LChannelsFactors.Delete(LIndex-1);
              end
            end;
            LWQConstriantsChannel.BlendingChannels := LChannels.CommaText;
            LWQConstriantsChannel.BlendingChannelFactors := LChannelsFactors.CommaText;
          finally
            LChannels.Free;
            LChannelsFactors.Free;
          end;
        end;
      end;
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TWQConstraintValidator.DoDeleteBoundChannelbtn(Sender : TObject);
const OPNAME = 'TWQConstraintValidator.DoDeleteBoundChannelbtn';
var
  LIndex : integer;
  LChannels : TStringList;
  LMinMaxUpperBoundChannel : IMinMaxUpperBoundChannel;
begin
  try
   LMinMaxUpperBoundChannel := TPlanningModelDataObject(FAppModules.Model.ModelData).
                             CastWQConstriantData.MinMaxUpperBoundChannelNo[Identifier];
    if (LMinMaxUpperBoundChannel <> nil) then
    begin
      with WQConstraintDialog do
      begin
        LIndex := GrdMinMaxChannels.Row;
        if LIndex > 0 then
        begin
          LChannels := TStringList.Create;
          try
            LChannels.CommaText := LMinMaxUpperBoundChannel.BoundedChannels;
            if GrdMinMaxChannels.Cells[0,LIndex] <> '' then
            begin
              if (LChannels.count>0) and (LChannels.count>=LIndex) then
                LChannels.Delete(LIndex-1);
            end;
            LMinMaxUpperBoundChannel.BoundedChannels := LChannels.CommaText;
          finally
            LChannels.Free;
          end;
        end;
      end;
    end;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


end.

