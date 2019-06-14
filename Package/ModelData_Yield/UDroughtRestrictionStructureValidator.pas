{******************************************************************************}
{*  UNIT      : Contains the class TDroughtRestrictionStructureValidator      *}
{*  AUTHOR    : Presley Mudau                                                 *}
{*  DATE      : 2007/06/14                                                    *}
{*  COPYRIGHT : Copyright © 2007 DWAF                                         *}
{******************************************************************************}

unit UDroughtRestrictionStructureValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.ExtCtrls,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UDroughtRestrictionStructureDialog;

type
  TDroughtRestrictionStructureValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlClick(Sender: TObject);
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnReservoirCheckListClick(Sender: TObject);
    procedure OnChannelCheckListClick(Sender: TObject);
    procedure UpdateDroughtRestrictionName;
    procedure UpdateReferenceStorageVolume(AIndex: integer;ARow : integer;AValue : string);
    procedure UpdateAllocationFactors(AIndex: integer;ARow : integer;AValue : string);
    procedure OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer); override;
    procedure UpdateReservoirNumbers;
    procedure UpdateChannelNumbers;
    procedure CheckReservoirNumbers;
    procedure CheckChannelNumbers;
    procedure RePopulateDataViewer;
    procedure RePopulateFactorsGrid;
    procedure RePopulateReservoirList;
    procedure RePopulateChannelList;
    procedure ValidateDroughtRestrictionName(ADroughtRestriction : IDroughtRestriction);
    procedure ValidateDroughtRestrictionGrid(ADroughtRestriction : IDroughtRestriction);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    function DroughtRestrictionStructureDialog : TDroughtRestrictionStructureDialog;
//    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UUtilities,
  URunConfigurationData,
  UYieldModelDataGUIForm,
  UYieldModelDataObject,
  UErrorHandlingOperations, VCL.Grids;

{******************************************************************************}
{* TDroughtRestrictionStructureValidator                                              *}
{******************************************************************************}

procedure TDroughtRestrictionStructureValidator.CreateMemberObjects;
const OPNAME = 'TDroughtRestrictionStructureValidator.CreateMemberObjects';
var
  lPanel     : TDroughtRestrictionStructureDialog;
  lIndex     : integer;
  LCount     : integer;
begin
  try
    inherited CreateMemberObjects;
    FPanel  := TDroughtRestrictionStructureDialog.Create(FPanelOwner,FAppModules);
    lPanel := DroughtRestrictionStructureDialog;
    with lPanel do
    begin
      for LCount := 0 to DroughtRestrictionGrid.RowCount -1 do
      begin
        if LCount = 0 then
        begin
          for LIndex := 1 to 12 do
            DroughtRestrictionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReferenceStorageVolumes'));
        end
        else
        begin
          for LIndex := 1 to 12 do
            DroughtRestrictionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AllocationFactors'));
        end;
      end;

      DroughtRestrictionIdEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('DroughtRestrictionCount');
      DroughtRestrictionIdEdit.OnEnter        := OnEditControlEnter;
      DroughtRestrictionIdEdit.OnExit         := OnEditControltExit;

      DroughtRestrictionNameEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('DroughtRestrictionName');
      DroughtRestrictionNameEdit.OnEnter        := OnEditControlEnter;
      DroughtRestrictionNameEdit.OnExit         := OnEditControltExit;

      NrOfReservoirsEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('DroughtRestrictionReservoirCount');
      NrOfReservoirsEdit.OnEnter        := OnEditControlEnter;
      NrOfReservoirsEdit.OnExit         := OnEditControltExit;

      NrOfRestrictedChannelEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('DroughtRestrictionChannelCount');
      NrOfRestrictedChannelEdit.OnEnter        := OnEditControlEnter;
      NrOfRestrictedChannelEdit.OnExit         := OnEditControltExit;

      NrOfReservorsInRefEdit.FieldProperty  := FAppModules.FieldProperties.FieldProperty('RefStorageVolumeReservoirCount');
      NrOfReservorsInRefEdit.OnEnter        := OnEditControlEnter;
      NrOfReservorsInRefEdit.OnExit         := OnEditControltExit;

      DroughtRestrictionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReferenceStorageVolumes'));
      DroughtRestrictionGrid.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('AllocationFactors'));
      DroughtRestrictionGrid.OnBeforeCellChange := OnStringGridCellDataHasChanged;
      DroughtRestrictionGrid.OnColEnter         := OnStringGridColEnter;
      DroughtRestrictionGrid.OnExit             := OnEditControltExit;
      DroughtRestrictionGrid.OnEnter            := OnEditControlEnter;

      ReservoirCheckLbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('DroughtRestrictionReservoirNumber');
      ReservoirCheckLbx.OnEnter       := OnEditControlEnter;
      ReservoirCheckLbx.OnClick       := OnReservoirCheckListClick;

      ChannelCheckLbx.FieldProperty := FAppModules.FieldProperties.FieldProperty('DroughtRestrictionChannelNumber');
      ChannelCheckLbx.OnEnter       := OnEditControlEnter;
      ChannelCheckLbx.OnClick       := OnChannelCheckListClick;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.DestroyMemberObjects;
const OPNAME = 'TDroughtRestrictionStructureValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionStructureValidator.Initialise: boolean;
const OPNAME = 'TDroughtRestrictionStructureValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionStructureValidator.LanguageHasChanged: boolean;
const OPNAME = 'TDroughtRestrictionStructureValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('ViewData.DroughtRestrictionStructures');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.ClearDataViewer;
const OPNAME = 'TDroughtRestrictionStructureValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    with DroughtRestrictionStructureDialog do
    begin
      NrOfReservoirsEdit.SetFieldValue('-1');
      NrOfReservorsInRefEdit.SetFieldValue('0.0');
      NrOfRestrictedChannelEdit.SetFieldValue('-1');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.PopulateDataViewer;
const OPNAME = 'TDroughtRestrictionStructureValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtDroughtRestrictionAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.RePopulateDataViewer;
const OPNAME = 'TDroughtRestrictionStructureValidator.RePopulateDataViewer';
var
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                             .CurtailmentAndDrought;
      if (LCurtailmentAndDrought <> nil) then
      begin
        LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
        if LDroughtRestriction <> nil then
        begin
          begin
            with DroughtRestrictionStructureDialog do
            begin
              DroughtRestrictionIdEdit.SetFieldValue(LDroughtRestriction.Identifier);
              DroughtRestrictionNameEdit.SetFieldValue(LDroughtRestriction.DroughtRestrictionName);
              NrOfReservoirsEdit.SetFieldValue(LDroughtRestriction.ReservoirCount);
              NrOfRestrictedChannelEdit.SetFieldValue(LDroughtRestriction.ChannelCount);

              RePopulateFactorsGrid;
              RePopulateReservoirList;
              RePopulateChannelList;
            end
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionStructureValidator.SaveState: boolean;
const OPNAME = 'TDroughtRestrictionStructureValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionStructureValidator.DroughtRestrictionStructureDialog : TDroughtRestrictionStructureDialog;
const OPNAME = 'TDroughtRestrictionStructureValidator.DroughtRestrictionStructureDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TDroughtRestrictionStructureDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionStructureValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TDroughtRestrictionStructureValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
   if (AFieldName = 'DroughtRestrictionName') then
     RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TDroughtRestrictionStructureValidator.StudyHasChanged: boolean;
const OPNAME = 'TDroughtRestrictionStructureValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TDroughtRestrictionStructureValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TDroughtRestrictionStructureValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if ((Sender =  DroughtRestrictionStructureDialog.DroughtRestrictionNameEdit) AND
       (DroughtRestrictionStructureDialog.DroughtRestrictionNameEdit.HasValueChanged)) then
       UpdateDroughtRestrictionName;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.OnEditControlClick(Sender: TObject);
const OPNAME = 'TDroughtRestrictionStructureValidator.OnEditControlClick';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TDroughtRestrictionStructureValidator.OnStringGridCellDataHasChanged';
begin
  inherited OnStringGridCellDataHasChanged(ASender, ACol, ARow);
  try
    with DroughtRestrictionStructureDialog do
    begin
      if ((ASender = DroughtRestrictionGrid) AND (ARow = 0)) then
        UpdateReferenceStorageVolume(ACol,ARow, Trim(DroughtRestrictionGrid.Cells[ACol, ARow]))
      else
      if ((ASender = DroughtRestrictionGrid) AND (ARow = 1)) then
        UpdateAllocationFactors(ACol,ARow, Trim(DroughtRestrictionGrid.Cells[ACol, ARow]))
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.RePopulateReservoirList;
const OPNAME = 'TDroughtRestrictionStructureValidator.RePopulateReservoirList';
var
  lIndex                 : integer;
  lReservoirList         : IReservoirDataList;
  lReservoirData         : IReservoirData;
  lReservoir             : IReservoirConfigurationData;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.CurtailmentAndDrought;
      if (LCurtailmentAndDrought <> nil) then
      begin
        LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
        if LDroughtRestriction <> nil then
        begin
          begin
            lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ReservoirList;
            if (lReservoirList <> nil) then
            begin
              with DroughtRestrictionStructureDialog do
              begin
                ReservoirCheckLbx.Items.Clear;
                for lIndex := 0 to lReservoirList.ReservoirAndNodesCount - 1 do
                begin
                  lReservoirData := lReservoirList.ReservoirOrNodeByIndex[lIndex];
                  lReservoir     := lReservoirData.ReservoirConfigurationData;
                  if (lReservoir.NodeType in NodeWithInflowAndReservoirSet) then
                  begin
                    ReservoirCheckLbx.Items.AddObject
                        ('(' + IntToStr(lReservoir.ReservoirIdentifier) + ') ' + lReservoir.ReservoirName,
                         TObject(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
                  end;
                end;
              end;
              CheckReservoirNumbers;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.RePopulateChannelList;
const OPNAME = 'TDroughtRestrictionStructureValidator.RePopulateChannelList';
var
  LIndex       : integer;
  LChannelList : IChannelList;
  LChannel     : IGeneralFlowChannel;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkFeaturesData.CurtailmentAndDrought;
      if (LCurtailmentAndDrought <> nil) then
      begin
        LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
        if LDroughtRestriction <> nil then
        begin
          begin
            LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                              NetworkElementData.ChannelList;
            if (LChannelList <> nil) then
            begin
              with DroughtRestrictionStructureDialog do
              begin
                ChannelCheckLbx.Items.Clear;
                for lIndex := 0 to LChannelList.ChannelCount - 1 do
                begin
                  LChannel := LChannelList.ChannelByIndex[lIndex];
                  //if (LChannel.ChannelType <> 12) then
                  //begin
                    ChannelCheckLbx.Items.AddObject
                        ('(' + IntToStr(LChannel.ChannelNumber) + ') ' + LChannel.ChannelName,
                         TObject(integer(LChannel.ChannelNumber)));
                  //end;
                end;
              end;
              CheckChannelNumbers;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.UpdateDroughtRestrictionName;
const OPNAME = 'TDroughtRestrictionStructureValidator.UpdateDroughtRestrictionName';
var
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
  LErrorMessage: string;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                                  .CurtailmentAndDrought;
      if (LCurtailmentAndDrought <> nil) then
      begin
        LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
        if LDroughtRestriction <> nil then
        begin
          with DroughtRestrictionStructureDialog do
          begin
            if (FAppModules.FieldProperties.ValidateFieldProperty('DroughtRestrictionName',
                DroughtRestrictionNameEdit.Text,LErrorMessage)) then
            begin
              LDroughtRestriction.DroughtRestrictionName := Trim(DroughtRestrictionNameEdit.Text);
              DroughtRestrictionNameEdit.SetFieldValue(LDroughtRestriction.DroughtRestrictionName);
              //DoContextValidation(dvtResPropReservoirName);
            end;
            DroughtRestrictionNameEdit.ContextValidationError := LErrorMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.UpdateReservoirNumbers;
const OPNAME = 'TDroughtRestrictionStructureValidator.UpdateReservoirNumbers';
var
  LIndex                 : integer;
  lReservoirNumbers      : TStringList;
  LResevoirNr            : integer;
  IReservoirList         : IReservoirDataList;
  LErrorMessage          : string;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
      if LDroughtRestriction <> nil then
      begin
        with DroughtRestrictionStructureDialog do
        begin
          IReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                              ReservoirList;

          lReservoirNumbers := TStringList.Create;
          lReservoirNumbers.Clear;
          for LIndex := 0 to ReservoirCheckLbx.Items.Count - 1 do
          begin
            if (ReservoirCheckLbx.Checked[LIndex]) then
            begin
              LResevoirNr := integer(ReservoirCheckLbx.Items.Objects[LIndex]);
                lReservoirNumbers.Add(IntToStr(LResevoirNr));
            end;
          end;

          LDroughtRestriction.ReservoirNumbers := lReservoirNumbers.CommaText;
          RePopulateDataViewer;

          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DroughtRestrictionReservoirCount', IntToStr(lReservoirNumbers.Count), LErrorMessage)) then
          begin
            ReservoirCheckLbx.InValidationError := False;
            ReservoirCheckLbx.ShowErrorState(FALSE);
            CheckReservoirNumbers;
            //DoContextValidation(dvtIFRFeatureReferenceNodes);
          end
          else
          begin
            ReservoirCheckLbx.InValidationError := False;
            ReservoirCheckLbx.ValidationError := LErrorMessage;
            ReservoirCheckLbx.ShowErrorState(TRUE);
          end;
          FreeAndNil(lReservoirNumbers);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.UpdateChannelNumbers;
const OPNAME = 'TDroughtRestrictionStructureValidator.UpdateChannelNumbers';
var
  LIndex                 : integer;
  lChannelNumbers        : TStringList;
  LChannelNr             : integer;
  LChannelList           : IChannelList;
  LErrorMessage          : string;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                     NetworkFeaturesData.CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
      if LDroughtRestriction <> nil then
      begin
        with DroughtRestrictionStructureDialog do
        begin
          LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                              ChannelList;

          lChannelNumbers := TStringList.Create;
          lChannelNumbers.Clear;
          for LIndex := 0 to ChannelCheckLbx.Items.Count - 1 do
          begin
            if (ChannelCheckLbx.Checked[LIndex]) then
            begin
              LChannelNr := integer(ChannelCheckLbx.Items.Objects[LIndex]);
                lChannelNumbers.Add(IntToStr(LChannelNr));
            end;
          end;

          LDroughtRestriction.ChannelNumbers := lChannelNumbers.CommaText;
          RePopulateDataViewer;

          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'DroughtRestrictionReservoirCount', IntToStr(lChannelNumbers.Count), LErrorMessage)) then
          begin
            ChannelCheckLbx.InValidationError := False;
            ChannelCheckLbx.ShowErrorState(FALSE);
            CheckChannelNumbers;
            //DoContextValidation(dvtIFRFeatureReferenceNodes);
          end
          else
          begin
            ChannelCheckLbx.InValidationError := False;
            ChannelCheckLbx.ValidationError := LErrorMessage;
            ChannelCheckLbx.ShowErrorState(TRUE);
          end;
          FreeAndNil(lChannelNumbers);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDroughtRestrictionStructureValidator.CheckReservoirNumbers;
const OPNAME = 'TDroughtRestrictionStructureValidator.CheckReservoirNumbers';
var
  LIndex         : integer;
  LItemIndex     : integer;
  LReservoirData : IReservoirData;
  LReservoirList : IReservoirDataList;
  LReservoirNr   : integer;
  LTotalVolume   : double;
  LVolume        : double;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
  lReservoir             : IReservoirConfigurationData;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkFeaturesData.CurtailmentAndDrought;
      lReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ReservoirList;
      if (LCurtailmentAndDrought <> nil) then
      begin
        LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
        if LDroughtRestriction <> nil then
        begin

          with DroughtRestrictionStructureDialog do
          begin
            LTotalVolume := 0.0;
            for LIndex := 0 to LDroughtRestriction.ReservoirCount - 1 do
            begin
              LReservoirNr := LDroughtRestriction.ReservoirNumberByIndex[LIndex];
              LItemIndex   := ReservoirCheckLbx.Items.IndexOfObject(TObject(LReservoirNr));
              if (LItemIndex <> -1) then
              begin
                ReservoirCheckLbx.Checked[LItemIndex] := TRUE;
                LReservoirData := LReservoirList.ReservoirOrNodeByIdentifier[LReservoirNr];
                if (LReservoirData <> nil) then
                begin
                  lReservoir := LReservoirData.ReservoirConfigurationData;
                  if (lReservoir.NodeType = ntReservoir) then
                    LVolume :=  LReservoirData.ReservoirVolumesData.MaxReservoirVolume
                  else
                    LVolume := 0.0;
                    LTotalVolume := LTotalVolume + LVolume;
                end;
              end;
            end;
            NrOfReservorsInRefEdit.SetFieldValue(FloatToStr(LTotalVolume));
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDroughtRestrictionStructureValidator.CheckChannelNumbers;
const OPNAME = 'TDroughtRestrictionStructureValidator.CheckChannelNumbers';
var
  LIndex         : integer;
  LItemIndex     : integer;
  LChannelList : IChannelList;
  LChannelNr   : integer;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkFeaturesData.CurtailmentAndDrought;
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                            NetworkElementData.ChannelList;
      if (LCurtailmentAndDrought <> nil) then
      begin
        LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
        if LDroughtRestriction <> nil then
        begin
          with DroughtRestrictionStructureDialog do
          begin
            for lIndex := 0 to LDroughtRestriction.ChannelCount - 1 do
            begin
              LChannelNr := LDroughtRestriction.ChannelNumberByIndex[lIndex];
              LItemIndex   := ChannelCheckLbx.Items.IndexOfObject(TObject(LChannelNr));
                if (LItemIndex <> -1) then
                  ChannelCheckLbx.Checked[LItemIndex] := TRUE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.RePopulateFactorsGrid;
const OPNAME = 'TDroughtRestrictionStructureValidator.RePopulateFactorsGrid';
var
  lRow                   : integer;
  lIndexA                : integer;
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData
                           .CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
      if LDroughtRestriction <> nil then
      begin
        with DroughtRestrictionStructureDialog do
        begin
          for lRow := 0 to DroughtRestrictionGrid.RowCount-1 do
          begin
            for lIndexA := 1 to 10 do
            begin
              if lRow = 0 then
                DroughtRestrictionGrid.Cells[lIndexA, lRow] := FloatToStr(LDroughtRestriction.ReferenceStorageVolumes[lIndexA])
              else
                DroughtRestrictionGrid.Cells[lIndexA, lRow] := FloatToStr(LDroughtRestriction.AllocationFactors[lIndexA])
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TDroughtRestrictionStructureValidator.UpdateReferenceStorageVolume(AIndex : integer;
                                                                             ARow : integer;
                                                                             AValue: string);
const OPNAME = 'TDroughtRestrictionStructureValidator.UpdateReferenceStorageVolume';
var
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
  LMessage               : string;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[Fidentifier];
      if LDroughtRestriction <> nil then
      begin
        with DroughtRestrictionStructureDialog do
        begin
          DroughtRestrictionGrid.ValidationError[AIndex, ARow, gveCellContext] := '';
          if (FAppModules.FieldProperties.ValidateFieldProperty('ReferenceStorageVolumes', AValue,
             LMessage, AIndex )) then
          begin
            DroughtRestrictionGrid.ValidationError[AIndex, ARow, gveCellContext] := LMessage;
            LDroughtRestriction.ReferenceStorageVolumes[AIndex] :=  StrToFloat(AValue);
            RePopulateDataViewer;
          end
          else
            DroughtRestrictionGrid.ValidationError[AIndex, ARow, gveCellContext] := LMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.UpdateAllocationFactors(AIndex: integer;
                                                                        ARow: integer;
                                                                        AValue: string);
const OPNAME = 'TDroughtRestrictionStructureValidator.UpdateAllocationFactors';
var
  LCurtailmentAndDrought : ICurtailmentAndDrought;
  LDroughtRestriction    : IDroughtRestriction;
  LMessage               : string;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).
                           NetworkFeaturesData.CurtailmentAndDrought;
    if (LCurtailmentAndDrought <> nil) then
    begin
      LDroughtRestriction := LCurtailmentAndDrought.DroughtRestrictionByID[FIdentifier];
      if (LDroughtRestriction <> nil) then
      begin
        with DroughtRestrictionStructureDialog do
        begin
          DroughtRestrictionGrid.ValidationError[AIndex, ARow, gveCellContext] := '';
          if (FAppModules.FieldProperties.ValidateFieldProperty('AllocationFactors', AValue,
             LMessage, AIndex)) then
          begin
            DroughtRestrictionGrid.ValidationError[AIndex, ARow, gveCellContext] := LMessage;
            LDroughtRestriction.AllocationFactors[AIndex] :=  StrToFloat(AValue);
            RePopulateDataViewer;
          end
          else
            DroughtRestrictionGrid.ValidationError[AIndex, ARow, gveCellContext] := LMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

{procedure TDroughtRestrictionStructureValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TDroughtRestrictionStructureValidator.DoContextValidation';
var
  LCurtailmentAndDrought : IDroughtRestriction;
begin
  try
    LCurtailmentAndDrought := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkFeaturesData.
                           DroughtRestrictionList.DroughtRestrictionByID[FIdentifier];
    if (LCurtailmentAndDrought <> nil) then
    begin
      case AValidationType of
        dvtDroughtRestrictionAll:
        begin
          ValidateDroughtRestrictionName(LCurtailmentAndDrought);
          ValidateDroughtRestrictionGrid(LCurtailmentAndDrought);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;}

procedure TDroughtRestrictionStructureValidator.ValidateDroughtRestrictionName;
const OPNAME = 'TDroughtRestrictionStructureValidator.ValidateDroughtRestrictionName';
var
  LMessage : WideString;
begin
  try
    if (ADroughtRestriction <> nil) then
    begin
      with DroughtRestrictionStructureDialog do
      begin
        DroughtRestrictionNameEdit.ContextValidationError := '';
        if (not ADroughtRestriction.Validate(LMessage, DroughtRestrictionNameEdit.FieldProperty.FieldName)) then
          DroughtRestrictionNameEdit.ContextValidationError := LMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.ValidateDroughtRestrictionGrid(ADroughtRestriction : IDroughtRestriction);
const OPNAME = 'TDroughtRestrictionStructureValidator.ValidateDroughtRestrictionGrid';
var
  LErrorCols : TStringList;
  LErrors    : TStringList;
  LLevel     : integer;
  LCol       : integer;
begin
  try
    if (ADroughtRestriction <> nil) then
    begin
      with DroughtRestrictionStructureDialog do
      begin
        lErrorCols := TStringList.Create;
        lErrors    := TStringList.Create;
        lErrorCols.Clear;
        FErrorMessage := '';
        if (ADroughtRestriction.Validate(FErrorMessage, 'Type3Proportions')) then
        begin
          for lLevel := 1 to DroughtRestrictionGrid.RowCount-1 do
          begin
          for LCol := 0 to DroughtRestrictionGrid.ColCount-1 do
            DroughtRestrictionGrid.ValidationError[LCol, lLevel-1, gveColContext] := '';
          end;
        end
        else
        begin
          ExtractErrorsAndColumns(FErrorMessage, lErrors, lErrorCols);
          for lLevel := 1 to DroughtRestrictionGrid.RowCount-1 do
          begin
            for LCol := 0 to DroughtRestrictionGrid.ColCount-1 do
            begin
              if (lErrorCols.IndexOf(IntToStr(lLevel)) >= 0) then
                DroughtRestrictionGrid.ValidationError[LCol, lLevel-1, gveColContext] := lErrors.Text
              else
                DroughtRestrictionGrid.ValidationError[LCol, lLevel-1, gveColContext] := '';
            end;
          end;
          FAllErrorMessages.AddStrings(lErrors);
        end;
        FreeAndNil(lErrorCols);
        FreeAndNil(lErrors);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.OnReservoirCheckListClick(Sender: TObject);
const OPNAME = 'TDroughtRestrictionStructureValidator.OnReservoirCheckListClick';
begin
  try
    if(DroughtRestrictionStructureDialog.ReservoirCheckLbx.HasValueChanged) then
      UpdateReservoirNumbers;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TDroughtRestrictionStructureValidator.OnChannelCheckListClick(Sender: TObject);
const OPNAME = 'TDroughtRestrictionStructureValidator.OnChannelCheckListClick';
begin
  try
    if(DroughtRestrictionStructureDialog.ChannelCheckLbx.HasValueChanged) then
      UpdateChannelNumbers;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

