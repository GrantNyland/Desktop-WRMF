{******************************************************************************}
{*  UNIT      : Contains the class TFMDemandDefinitionValidator.              *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2006/01/26                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit UFMDemandDefinitionValidator;

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
  VoaimsCom_TLB,
  UFMDemandDefinitionDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type

  TFMDemandDefinitionValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FHeading        : string;
    FDemandDefID    : integer;
    FResidentList   : TStringList;
    FSystemChange   : Boolean;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure CreateDialog; virtual;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnRgpGrowthTypeClick(Sender: TObject);
    procedure ClearResidentList;
    procedure PopulateResidentList;
    procedure OnTrvDemandDefsDragOver (Sender, Source : TObject;
                                       X, Y           : Integer;
                                       State          : TDragState;
                                       var Accept     : Boolean);
    procedure OnTrvDemandDefsDragDrop (Sender, Source : TObject;
                                       X, Y           : Integer);
    procedure OnTrvDemandDefsChange (Sender : TObject;
                                     ANode  : TTreeNode);
    procedure OnGrdSupportSubSystemsSelectCell (Sender        : TObject;
                                                ACol, ARow    : Integer;
                                                var CanSelect : Boolean);
    procedure OnGrdSupportSubSystemsTopLeftChanged (Sender : TObject);
    procedure RePopulateDataViewer;
    procedure DoAddDemandDef (Sender: TObject);
    procedure DoDeleteDemandDef (Sender: TObject);
    procedure DoMoveUpDemandDef (Sender: TObject);
    procedure DoMoveDownDemandDef (Sender: TObject);
    procedure DoAddSupportSubSystem (Sender: TObject);
    procedure DoDeleteSupportSubSystem (Sender: TObject);
    procedure PopulateComboBoxes (AAllocDef : IAllocationDefinition);
    procedure PopulateDemandDef (AAllocDef : IAllocationDefinition);
    procedure PopulateSupportSubSystems (AAllocDef  : IAllocationDefinition;
                                         ADemandDef : IDemandDefinition);
    procedure UpdateDemandCentreID;
    procedure UpdateGrowthType;
    procedure UpdateUserCategory;
    procedure UpdateSupportArc01;
    procedure UpdateSupportArc02;
    procedure UpdateSupportSubSystem;
    procedure UpdateSupportChannel;
    //procedure UpdateTargetBaseYear;
    procedure ValidateDemandCentreID (ADemandDef : IDemandDefinition);
    procedure ValidateGrowthType (ADemandDef : IDemandDefinition);
    procedure ValidateUserCategory (ADemandDef : IDemandDefinition);
    procedure ValidateSupportArc1 (ADemandDef : IDemandDefinition);
    procedure ValidateSupportArc2 (ADemandDef : IDemandDefinition);
    procedure ValidateSupportSubSystems (ADemandDef : IDemandDefinition);
    procedure ValidateTargetBaseYear(ADemandDef : IDemandDefinition);
  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;
    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function FMDemandDefinitionDialog : TFMDemandDefinitionDialog;
  end;

implementation

uses
  SysUtils,
  VCL.Graphics,
  UConstants,
  VCL.Dialogs,
  UUtilities,
  UYieldModelDataGUIForm,
  UErrorHandlingOperations, Math, Variants, DateUtils;

{******************************************************************************}
{* TFMDemandDefinitionValidator                                               *}
{******************************************************************************}

procedure TFMDemandDefinitionValidator.CreateMemberObjects;
const OPNAME = 'TFMDemandDefinitionValidator.CreateMemberObjects';
var
  lpPanel        : TFMDemandDefinitionDialog;
  lFieldProperty : TAbstractFieldProperty;
begin
  try
    inherited CreateMemberObjects;
    FSystemChange   := FALSE;
    FIdentifier     := 0;
    FDemandDefID    := 0;
    FHeading        := 'TabCaption.DemandSupportDefinition';
    FResidentList   := TStringList.Create;

    CreateDialog;
    lpPanel := FMDemandDefinitionDialog;
    with lpPanel do
    begin
      BtnAddDemandDef.OnClick           := DoAddDemandDef;
      BtnDeleteDemandDef.OnClick        := DoDeleteDemandDef;
      BtnMoveUp.OnClick                 := DoMoveUpDemandDef;
      BtnMoveDown.OnClick               := DoMoveDownDemandDef;
      BtnAddSupportSubSystem.OnClick    := DoAddSupportSubSystem;
      BtnDeleteSupportSubSystem.OnClick := DoDeleteSupportSubSystem;
      TrvDemandDefs.OnChange            := OnTrvDemandDefsChange;
      TrvDemandDefs.OnDragOver          := OnTrvDemandDefsDragOver;
      TrvDemandDefs.OnDragDrop          := OnTrvDemandDefsDragDrop;

      GrdSupportSubSystems.OnSelectCell := OnGrdSupportSubSystemsSelectCell;
      GrdSupportSubSystems.OnTopLeftChanged := OnGrdSupportSubSystemsTopLeftChanged;

      CbxDemandCentreID.FieldProperty    := FAppModules.FieldProperties.FieldProperty('DDDemandCentreID');
      CbxDemandCentreID.OnEnter          := OnEditControlEnter;
      CbxDemandCentreID.OnExit           := OnEditControltExit;

      CbxUserCategory.FieldProperty := FAppModules.FieldProperties.FieldProperty('DCUserCategoryID');
      CbxUserCategory.OnEnter       := OnEditControlEnter;
      CbxUserCategory.OnChange      := OnEditControltExit;

      RgpGrowthType.FieldProperty      := FAppModules.FieldProperties.FieldProperty('GrowthType');
      RgpGrowthType.OnEnter            := OnEditControlEnter;
      RgpGrowthType.OnClick            := OnRgpGrowthTypeClick;

      CbxSupportArc1.FieldProperty    := FAppModules.FieldProperties.FieldProperty('SupportArc1');
      CbxSupportArc1.OnEnter          := OnEditControlEnter;
      CbxSupportArc1.OnExit           := OnEditControltExit;

      CbxSupportArc2.FieldProperty    := FAppModules.FieldProperties.FieldProperty('SupportArc2');
      CbxSupportArc2.OnEnter          := OnEditControlEnter;
      CbxSupportArc2.OnExit           := OnEditControltExit;

      GrdSupportSubSystems.ClearFieldProperties;
      GrdSupportSubSystems.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('SupSubSystemID'));
      lFieldProperty:= FAppModules.FieldProperties.FieldProperty('SupSubSysChannelNr');
      GrdSupportSubSystems.AddFieldProperty(lFieldProperty);
      GrdSupportSubSystems.AddFieldProperty(lFieldProperty);
      GrdSupportSubSystems.AddFieldProperty(lFieldProperty);
      GrdSupportSubSystems.AddFieldProperty(lFieldProperty);
      GrdSupportSubSystems.AddFieldProperty(lFieldProperty);

      //EdtTargetDemand.FieldProperty := FAppModules.FieldProperties.FieldProperty('TargetDemand');
      //EdtTargetDemand.OnEnter       := OnEditControlEnter;
      //EdtTargetDemand.OnExit        := OnEditControltExit;

      CbxSupportSubSystem.FieldProperty := FAppModules.FieldProperties.FieldProperty('SupSubSystemID');
      CbxSupportSubSystem.OnEnter       := OnEditControlEnter;
      CbxSupportSubSystem.OnChange      := OnEditControltExit;

      CbxChannel.FieldProperty := FAppModules.FieldProperties.FieldProperty('SupSubSysChannelNr');
      CbxChannel.OnEnter       := OnEditControlEnter;
      CbxChannel.OnChange      := OnEditControltExit;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.CreateDialog;
const OPNAME = 'TFMDemandDefinitionValidator.CreateDialog';
begin
  try
    FPanel  := TFMDemandDefinitionDialog.Create(FPanelOwner,FAppModules);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.DestroyMemberObjects;
const OPNAME = 'TFMDemandDefinitionValidator.DestroyMemberObjects';
begin
  try
    ClearResidentList;
    FreeAndNil(FResidentList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMDemandDefinitionValidator.Initialise: boolean;
const OPNAME = 'TFMDemandDefinitionValidator.Initialise';
var
  lIndex             : integer;
  lYieldModelData    : IYieldModelData;
  lChannelList       : IChannelList;
  lChannel           : IGeneralFlowChannel;
begin
  Result := inherited Initialise;
  try
    with FMDemandDefinitionDialog do
    begin
      with RgpGrowthType do
      begin
        Items.Clear;
        Items.Add(FAppModules.Language.GetString('PlanningGUI.GrowthReturnFlow'));
        Items.Add(FAppModules.Language.GetString('PlanningGUI.NoGrowth'));
        Items.Add(FAppModules.Language.GetString('PlanningGUI.DemandGrowth'));
      end;

      lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
      lChannelList    := lYieldModelData.NetworkElementData.ChannelList;

      CbxChannel.Clear;
      CbxChannel.Items.AddObject('0 - None', TObject(0));
      CbxSupportArc1.Clear;
      CbxSupportArc1.Items.AddObject('0 - None', TObject(0));
      CbxSupportArc2.Clear;
      CbxSupportArc2.Items.AddObject('0 - None', TObject(0));
      for lIndex := 0 to lChannelList.ChannelCount - 1 do
      begin
        lChannel := lChannelList.ChannelByIndex[lIndex];
        if (lChannel.ChannelType in [8,9]) then
        begin
          CbxChannel.Items.AddObject(lChannel.ChannelName, TObject(lChannel.ChannelNumber));
          CbxSupportArc1.Items.AddObject(lChannel.ChannelName, TObject(lChannel.ChannelNumber));
          CbxSupportArc2.Items.AddObject(lChannel.ChannelName, TObject(lChannel.ChannelNumber));
        end;
      end;
      CbxChannel.Sorted := TRUE;
      CbxSupportArc1.Sorted := TRUE;
      CbxSupportArc2.Sorted := TRUE;

      GrdSupportSubSystems.Cells[0, 0] := FAppModules.Language.GetString('PlanningGUI.Subsystem');
      GrdSupportSubSystems.Cells[1, 0] := '1';
      GrdSupportSubSystems.Cells[2, 0] := '2';
      GrdSupportSubSystems.Cells[3, 0] := '3';
      GrdSupportSubSystems.Cells[4, 0] := '4';
      GrdSupportSubSystems.Cells[5, 0] := '5';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMDemandDefinitionValidator.LanguageHasChanged: boolean;
const OPNAME = 'TFMDemandDefinitionValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString(FHeading);
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.ClearDataViewer;
const OPNAME = 'TFMDemandDefinitionValidator.ClearDataViewer';
var
  lIndex  : integer;
begin
  inherited ClearDataViewer;
  try
    with FMDemandDefinitionDialog do
    begin
      CbxDemandCentreID.ItemIndex  := -1;
      CbxUserCategory.ItemIndex    := -1;;
      RgpGrowthType.ItemIndex      := -1;
      CbxSupportArc1.ItemIndex     := -1;
      CbxSupportArc2.ItemIndex     := -1;
      for lIndex := 1 to GrdSupportSubSystems.RowCount do
        GrdSupportSubSystems.Rows[lIndex].Clear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.PopulateDataViewer;
const OPNAME = 'TFMDemandDefinitionValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
    DoContextValidation(dvtDemandDefAll);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.ClearResidentList;
const OPNAME = 'TFMDemandDefinitionValidator.ClearResidentList';
var
  lList : TObject;
begin
  try
    while (FResidentList.Count > 0) do
    begin
      lList := FResidentList.Objects[0];
      FResidentList.Objects[0] := nil;
      FreeAndNil(lList);
      FResidentList.Delete(0);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.PopulateResidentList;
const OPNAME = 'TFMDemandDefinitionValidator.PopulateResidentList';
var
  lAllocDef           : IAllocationDefinition;
  lNrOfDemandDefs     : integer;
  lNrOfSubSystems     : integer;
  lDemandDef          : IDemandDefinition;
  lIndex              : integer;
  lCount              : integer;
  lFound              : Boolean;
  lResSubSystemID     : integer;
  lSubList            : TStringList;
  lPosition           : integer;
  lDone               : boolean;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        lNrOfDemandDefs    := lAllocDef.NrOfDemandDefinitions;
        lNrOfSubSystems    := lAllocDef.NrOfSubSystems;

        ClearResidentList;

        for lCount := 0 to lNrOfSubSystems - 1 do
        begin
          lResSubSystemID := lAllocDef.SubSystemByIndex[lCount].SubSystemID;
          lIndex := 0;
          lFound := FALSE;
          while ((NOT lFound) AND (lIndex < FResidentList.Count)) do
          begin
            if (lResSubSystemID < StrToInt(FResidentList.Strings[lIndex])) then
            begin
              lFound := TRUE;
              lSubList := TStringList.Create;
              FResidentList.InsertObject(lIndex, IntToStr(lResSubSystemID), lSubList);
            end
            else
              lIndex := lIndex + 1;
          end;
          if (NOT lFound) then
          begin
            lSubList := TStringList.Create;
            FResidentList.AddObject(IntToStr(lResSubSystemID), lSubList);
          end;
        end;
        lSubList := TStringList.Create;
        FResidentList.InsertObject(0, IntToStr(0), lSubList);
        for lCount := 0 to lNrOfDemandDefs - 1 do
        begin
          lDemandDef      := lAllocDef.DemandDefinitionByIndex[lCount];
          lResSubSystemID := lDemandDef.ParentSubSystemID;
          lIndex := 0;
          lFound := FALSE;
          lSubList := nil;
          while ((NOT lFound) AND (lIndex < FResidentList.Count)) do
          begin
            if (lResSubSystemID = StrToInt(FResidentList.Strings[lIndex])) then
            begin
              lFound := TRUE;
              lSubList := TStringList(FResidentList.Objects[lIndex]);
            end
            else
              lIndex := lIndex + 1;
          end;
          if (lSubList <> nil) then
          begin
            lPosition := 0;
            lDone     := FALSE;
            while ((NOT lDone) AND (lPosition < lSubList.Count)) do
            begin
              if (lDemandDef.Order < StrToInt(lSubList.Strings[lPosition])) then
                lDone := TRUE
              else
                lPosition := lPosition + 1;
            end;
            lSubList.InsertObject(lPosition, IntToStr(lDemandDef.Order), TObject(lDemandDef.DemandDefID));
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.RePopulateDataViewer;
const OPNAME = 'TFMDemandDefinitionValidator.RePopulateDataViewer';
var
  lAllocDef           : IAllocationDefinition;
  lNrOfDemandDefs     : integer;
  lDemandDefID        : integer;
  lDemandDef          : IDemandDefinition;
  lIndex              : integer;
  lSelectedNode       : TTreeNode;
  lCount              : integer;
  lResSubSystemID     : integer;
  lSubSystem          : ISubSystem;
  lSubList            : TStringList;
  lNodeName           : string;
  lSubSystemNode      : TTreeNode;
  lDemandDefNode      : TTreeNode;
begin
  try
    with FMDemandDefinitionDialog do
    begin
      BtnDeleteDemandDef.Enabled     := FALSE;
      BtnMoveUp.Enabled                 := FALSE;
      BtnMoveDown.Enabled               := FALSE;
      BtnAddSupportSubSystem.Enabled    := FALSE;
      BtnDeleteSupportSubSystem.Enabled := FALSE;
      if (FIdentifier > 0) then
      begin
        lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                       AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
        if (lAllocDef <> nil) then
        begin
          lNrOfDemandDefs := lAllocDef.NrOfDemandDefinitions;
          lSelectedNode := nil;
          PopulateResidentList;
          FSystemChange := TRUE;
          TrvDemandDefs.Items.Clear;
          for lIndex := 0 to FResidentList.Count - 1 do
          begin
            lResSubSystemID := StrToInt(FResidentList.Strings[lIndex]);
            lSubSystem      := lAllocDef.SubSystemByID[lResSubSystemID];
            if (lSubSystem <> nil) then
              lNodeName := lSubSystem.Name
            else
              lNodeName := 'Undefined';
            lSubSystemNode := TrvDemandDefs.Items.AddChildObject(nil, lNodeName, TObject(lResSubSystemID));
            lSubList       := TStringList(FResidentList.Objects[lIndex]);
            for lCount := 0 to lSubList.Count - 1 do
            begin
              lDemandDefID  := Integer(lSubList.Objects[lCount]);
              lDemandDef := lAllocDef.DemandDefinitionByID[lDemandDefID];
              if (lDemandDef <> nil) then
              begin
                if (FDemandDefID = 0) then
                  FDemandDefID := lDemandDefID;
                lDemandDefNode := TrvDemandDefs.Items.AddChildObject(lSubSystemNode, lDemandDef.Name, TObject(lDemandDefID));
                if (lDemandDefID = FDemandDefID) then
                  lSelectedNode := lDemandDefNode;
              end;
            end;
          end;
          TrvDemandDefs.FullExpand;
          FSystemChange := FALSE;
          
          PopulateComboBoxes(lAllocDef);
          ClearDataViewer;
          if (lNrOfDemandDefs > 0) then
          begin
            PnlDemandDef.Enabled := TRUE;
            if (TrvDemandDefs.Selected <> lSelectedNode) then
              lSelectedNode.Selected := TRUE
            else
              PopulateDemandDef(lAllocDef);
          end
          else
            PnlDemandDef.Enabled := FALSE;
        end;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.OnTrvDemandDefsDragOver (Sender, Source : TObject;
                                                                X, Y           : Integer;
                                                                State          : TDragState;
                                                                var Accept     : Boolean);
const OPNAME = 'TFMDemandDefinitionValidator.OnTrvDemandDefsDragOver';
begin
  try
    Accept := Source is TTreeView;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.OnTrvDemandDefsDragDrop (Sender, Source : TObject;
                                                                X, Y           : Integer);
const OPNAME = 'TFMDemandDefinitionValidator.OnTrvDemandDefsDragDrop';
var
  lParentNode   : TTreeNode;
  lAllocDef     : IAllocationDefinition;
  lDemandDef    : IDemandDefinition;
  lNewResID     : integer;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) AND (FDemandDefID > 0) then
      begin
        lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
        if (lDemandDef <> nil) then
        begin
          with FMDemandDefinitionDialog do
          begin
            lParentNode := TrvDemandDefs.GetNodeAt(X,Y);
            if (lParentNode <> nil) AND (lParentNode.Level = 0) then
            begin
              lNewResID := Integer(lParentNode.Data);
              lDemandDef.ParentSubSystemID := lNewResID;
              RePopulateDataViewer;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.OnTrvDemandDefsChange (Sender : TObject;
                                                              ANode  : TTreeNode);
const OPNAME = 'TFMDemandDefinitionValidator.OnTrvDemandDefsChange';
var
  lAllocDef  : IAllocationDefinition;
begin
  try
    if (NOT FSystemChange) AND (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        if (ANode.Level = 1) then
        begin
          FMDemandDefinitionDialog.PnlDemandDef.Visible := TRUE;
          if ANode.Parent <> nil then
          begin
            FMDemandDefinitionDialog.LblSubSystem.Caption :=
              FAppModules.Language.GetString('PlanningGUI.SubSystem') + ' :' + ANode.Parent.Text;
          end;
          FDemandDefID := Integer(ANode.Data);
          PopulateDemandDef(lAllocDef);
          DoContextValidation(dvtDemandDefAll);
        end
        else
        begin
          FDemandDefID := 0;
          with FMDemandDefinitionDialog do
          begin
            PnlDemandDef.Visible       := FALSE;
            BtnMoveDown.Enabled           := FALSE;
            BtnMoveUp.Enabled             := FALSE;
            BtnDeleteDemandDef.Enabled := FALSE;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.PopulateDemandDef (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.PopulateDemandDef';
var
  lDemandDef      : IDemandDefinition;
  lUserCategory   : IUserCategory;
  lLastNr         : integer;
  lOtherCentre    : IDemandDefinition;
  lCount          : integer;
  lYieldModelData : IYieldModelData;
  lChannelList    : IChannelList;
  lChannel        : IGeneralFlowChannel;
  lMasterControl  : IMasterControlFeature;
begin
  try
    with FMDemandDefinitionDialog do
    begin
      ClearDataViewer;
      CbxSupportSubSystem.Visible      := FALSE;
      CbxChannel.Visible               := FALSE;

      if (FDemandDefID > 0) then
        lDemandDef := AAllocDef.DemandDefinitionByID[FDemandDefID]
      else
        lDemandDef := nil;
      if (lDemandDef <> nil) then
      begin
        lLastNr := 0;
        for lCount := 0 to AAllocDef.NrOfDemandDefinitions - 1 do
        begin
          lOtherCentre := AAllocDef.DemandDefinitionByIndex[lCount];
          if (lOtherCentre.ParentSubSystemID = lDemandDef.ParentSubSystemID) then
          begin
            if (lOtherCentre.Order > lLastNr) then
             lLastNr := lOtherCentre.Order;
          end;
        end;
        if (TrvDemandDefs.Selected <> nil) AND (TrvDemandDefs.Selected.Level = 1) then
        begin
          BtnDeleteDemandDef.Enabled     := (FAppModules.User.UserRights in CUR_EditData) AND
                                               (NOT FAppModules.StudyArea.ScenarioLocked);
          BtnMoveDown.Enabled               := (FAppModules.User.UserRights in CUR_EditData) AND
                                               (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                               (lDemandDef.Order < lLastNr);
          BtnMoveUp.Enabled                 := (FAppModules.User.UserRights in CUR_EditData) AND
                                               (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                               (lDemandDef.Order > 1);
          BtnAddSupportSubSystem.Enabled    := (FAppModules.User.UserRights in CUR_EditData) AND
                                               (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                               (AAllocDef.NrOfSubSystems > 0);
          BtnDeleteSupportSubSystem.Enabled := (FAppModules.User.UserRights in CUR_EditData) AND
                                               (NOT FAppModules.StudyArea.ScenarioLocked) AND
                                               (lDemandDef.NrOfSupportSubSystems > 0);
        end;
        lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        if (lDemandDef.DemandCentreID <> 0) then
        begin
          lMasterControl := lYieldModelData.NetworkFeaturesData.
                              MasterControlFeatureList.DemandCentreByID[lDemandDef.DemandCentreID];
          if (lMasterControl <> nil) then
          begin
            CbxDemandCentreID.SetFieldIndex(CbxDemandCentreID.Items.IndexOf(lMasterControl.FeatureName));
            EdtTargetDemand.Text := Format('%8.2f', [lMasterControl.AnnualDemand]);
            lDemandDef.TargetDemand := lMasterControl.AnnualDemand;
            //EdtTargetDemand.SetFieldValue(Format('%8.2f', [lDemandDef.TargetDemand]));

            //LblTargetDemandValue.Caption := Format('%8.2f mcm/a', [lMasterControl.AnnualDemand]);
          end;
        end;
        if ((lDemandDef.GrowthType + 1) < RgpGrowthType.Items.Count) then
          RgpGrowthType.ItemIndex := lDemandDef.GrowthType + 1;
        if (lDemandDef.UserCategoryID <> 0) then
        begin
          lUserCategory := AAllocDef.CategoryByID[lDemandDef.UserCategoryID];
          if (lUserCategory <> nil) then
            CbxUserCategory.SetFieldIndex(CbxUserCategory.Items.IndexOf(lUserCategory.Description));
        end;
        lChannelList    := lYieldModelData.NetworkElementData.ChannelList;
        if (lDemandDef.SupportArc1 <> 0) then
        begin
          lChannel := lChannelList.ChannelByChannelNumber[lDemandDef.SupportArc1];
          if (lChannel <> nil) then
            CbxSupportArc1.SetFieldIndex(CbxSupportArc1.Items.IndexOf(lChannel.ChannelName));
        end;
        if (lDemandDef.SupportArc2 <> 0) then
        begin
          lChannel := lChannelList.ChannelByChannelNumber[lDemandDef.SupportArc2];
          if (lChannel <> nil) then
            CbxSupportArc2.SetFieldIndex(CbxSupportArc1.Items.IndexOf(lChannel.ChannelName));
        end;

        PopulateSupportSubSystems(AAllocDef, lDemandDef);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.PopulateSupportSubSystems (AAllocDef  : IAllocationDefinition;
                                                                  ADemandDef : IDemandDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.PopulateSupportSubSystems';
var
  lSupSubSystem      : ISupportSubSystem;
  lChannel           : IGeneralFlowChannel;
  lNrOfSupSubSystems : integer;
  lIndex             : integer;
  lSubSystemID       : integer;
  lSubSystem         : ISubSystem;
  lChannelNr         : integer;
  lCount             : integer;
  lYieldModelData    : IYieldModelData;
  lChannelList       : IChannelList;
begin
  try
    with FMDemandDefinitionDialog do
    begin
      CbxSupportSubSystem.Clear;
      for lIndex := 0 to AAllocDef.NrOfSubSystems - 1 do
      begin
        lSubSystem := AAllocDef.SubSystemByIndex[lIndex];
        if (lSubSystem.SubSystemID <> ADemandDef.ParentSubSystemID) then
          CbxSupportSubSystem.Items.AddObject(lSubSystem.Name, TObject(lSubSystem.SubSystemID));
      end;

      lNrOfSupSubSystems := ADemandDef.NrOfSupportSubSystems;
      GrdSupportSubSystems.RowCount := lNrOfSupSubSystems + 1;
      if (GrdSupportSubSystems.RowCount > 1) then
        GrdSupportSubSystems.FixedRows := 1;

      for lIndex := 0 to lNrOfSupSubSystems - 1 do
      begin
        lSupSubSystem := ADemandDef.SupportSubSystemByIndex[lIndex];
        lSubSystemID  := lSupSubSystem.SubSystemID;
        lSubSystem    := AAllocDef.SubSystemByID[lSubSystemID];
        if (lSubSystem <> nil) then
          GrdSupportSubSystems.Cells[0, lIndex+1] := lSubSystem.Name
        else
          GrdSupportSubSystems.Cells[0, lIndex+1] := 'Undefined';

        lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
        lChannelList    := lYieldModelData.NetworkElementData.ChannelList;
        for lCount := 1 to 5 do
        begin
          lChannelNr := lSupSubSystem.SupportChannelNrByIndex[lCount];
          if (lChannelNr <> 0) then
          begin
            lChannel := lChannelList.ChannelByChannelNumber[lChannelNr];
            if (lChannel <> nil) then
              GrdSupportSubSystems.Cells[lCount, lIndex+1] := lChannel.ChannelName
            else
              GrdSupportSubSystems.Cells[lCount, lIndex+1] := 'Invalid';
          end
          else
            GrdSupportSubSystems.Cells[lCount, lIndex+1] := '';
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.PopulateComboBoxes (AAllocDef : IAllocationDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.PopulateComboBoxes';
var
  lIndex             : integer;
  lUserCategory      : IUserCategory;
  lMasterControlList : IMasterControlFeatureList;
  lMasterControl     : IMasterControlFeature;
begin
  try
    with FMDemandDefinitionDialog do
    begin
      CbxUserCategory.Clear;
      for lIndex := 0 to AAllocDef.NrOfCategories - 1 do
      begin
        lUserCategory := AAllocDef.CategoryByIndex[lIndex];
        CbxUserCategory.Items.AddObject(lUserCategory.Description, TObject(lUserCategory.CategoryID));
      end;
      lMasterControlList := (FAppModules.Model.ModelData as IYieldModelData).
                              NetworkFeaturesData.MasterControlFeatureList;
      CbxDemandCentreID.Clear;
      for lIndex := 0 to lMasterControlList.MasterControlFeatureCount - 1 do
      begin
        lMasterControl := lMasterControlList.MasterControlFeatureByIndex[lIndex];
        CbxDemandCentreID.Items.AddObject(lMasterControl.FeatureName, TObject(lMasterControl.FeatureID));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.OnGrdSupportSubSystemsSelectCell (Sender        : TObject;
                                                                         ACol, ARow    : Integer;
                                                                         var CanSelect : Boolean);
const OPNAME = 'TFMDemandDefinitionValidator.OnGrdSupportSubSystemsSelectCell';
var
  lAllocDef : IAllocationDefinition;
  lName     : string;
  lCol      : integer;
begin
  try
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          if (ARow > 0) then
          begin
            if (ACol = 0) then
            begin
              CbxSupportSubSystem.Top  := 2 + GrdSupportSubSystems.Top +
                                          ((1 + GrdSupportSubSystems.DefaultRowHeight) *
                                           (ARow - GrdSupportSubSystems.TopRow + 1));
              lName := Trim(GrdSupportSubSystems.Cells[0, ARow]);
              CbxSupportSubSystem.ItemIndex := CbxSupportSubSystem.Items.IndexOf(lName);
              CbxSupportSubSystem.Visible := TRUE;
              if (GrdSupportSubSystems.ValidationError[ACol, ARow, gveCellContext] <> '') then
              begin
                CbxSupportSubSystem.ValidationError   := GrdSupportSubSystems.ValidationError[ACol, ARow, gveCellContext];
                CbxSupportSubSystem.InValidationError := TRUE;
                CbxSupportSubSystem.ShowErrorState(TRUE);
              end
              else
              begin
                CbxSupportSubSystem.ValidationError   := '';
                CbxSupportSubSystem.InValidationError := FALSE;
                CbxSupportSubSystem.ShowErrorState(FALSE);
              end;
            end
            else
            if (ACol > 0) then
            begin
              lCol  := ACol;
              while (Trim(GrdSupportSubSystems.Cells[lCol-1, ARow]) = '') AND (lCol > 1) do
                lCol  := lCol - 1;
              if (ACol = lCol) then
              begin
                CbxChannel.Top  := 2 + GrdSupportSubSystems.Top +
                                   ((1 + GrdSupportSubSystems.DefaultRowHeight) *
                                    (ARow - GrdSupportSubSystems.TopRow + 1));
                CbxChannel.Left := 2 + GrdSupportSubSystems.Left +
                                   ((1 + GrdSupportSubSystems.DefaultColWidth) *
                                    (ACol - GrdSupportSubSystems.LeftCol));
                lName := Trim(GrdSupportSubSystems.Cells[ACol, ARow]);
                CbxChannel.ItemIndex := CbxChannel.Items.IndexOf(lName);
                CbxChannel.Visible := TRUE;
                if (GrdSupportSubSystems.ValidationError[ACol, ARow, gveCellContext] <> '') then
                begin
                  CbxChannel.ValidationError   := GrdSupportSubSystems.ValidationError[ACol, ARow, gveCellContext];
                  CbxChannel.InValidationError := TRUE;
                  CbxChannel.ShowErrorState(TRUE);
                end
                else
                begin
                  CbxChannel.ValidationError   := '';
                  CbxChannel.InValidationError := FALSE;
                  CbxChannel.ShowErrorState(FALSE);
                end;
              end
              else
                CanSelect := FALSE;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.OnGrdSupportSubSystemsTopLeftChanged (Sender : TObject);
const OPNAME = 'TFMDemandDefinitionValidator.OnGrdSupportSubSystemsTopLeftChanged';
begin
  try
    FMDemandDefinitionDialog.CbxSupportSubSystem.Visible := FALSE;
    FMDemandDefinitionDialog.CbxChannel.Visible          := FALSE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMDemandDefinitionValidator.SaveState: boolean;
const OPNAME = 'TFMDemandDefinitionValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMDemandDefinitionValidator.FMDemandDefinitionDialog:TFMDemandDefinitionDialog;
const OPNAME = 'TFMDemandDefinitionValidator.FMDemandDefinitionDialog';
begin
  Result := Nil;
  try
    if (FPanel <> nil) then
      Result := TFMDemandDefinitionDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMDemandDefinitionValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TFMDemandDefinitionValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
    if (AFieldName = 'NrOfSubSystems') OR
       (AFieldName = 'DemandDefName') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TFMDemandDefinitionValidator.StudyHasChanged: boolean;
const OPNAME = 'TFMDemandDefinitionValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.DoAddDemandDef (Sender: TObject);
const OPNAME = 'TFMDemandDefinitionValidator.DoAddDemandDef';
var
  lAllocDef     : IAllocationDefinition;
  lDemandDef    : IDemandDefinition;
  lCount        : integer;
  lResSubSysID  : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) then
    begin
      lCount          := lAllocDef.NrOfDemandDefinitions;
      lDemandDef      := lAllocDef.NewDemandDefinition;
      FDemandDefID    := lDemandDef.DemandDefID;
      with FMDemandDefinitionDialog do
      begin
        if (TrvDemandDefs.Selected <> nil) then
        begin
          if (TrvDemandDefs.Selected.Level = 0) then
            lResSubSysID := Integer(TrvDemandDefs.Selected.Data)
          else
            lResSubSysID := Integer(TrvDemandDefs.Selected.Parent.Data);
          lDemandDef.ParentSubSystemID := lResSubSysID;
        end;
      end;
      RePopulateDataViewer;
      DoContextValidation(dvtDemandDefAll);
      FAppModules.Model.StudyDataHasChanged
        (sdccEdit, 'NrOfDemandDefinitions', IntToStr(lCount), IntToStr(lAllocDef.NrOfDemandDefinitions));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.DoDeleteDemandDef(Sender: TObject);
const OPNAME = 'TFMDemandDefinitionValidator.DoDeleteDemandDef';
var
  lAllocDef  : IAllocationDefinition;
  lCount     : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lCount := lAllocDef.NrOfDemandDefinitions;
      if (lAllocDef.RemoveDemandDefinition(FDemandDefID)) then
      begin
        FDemandDefID := 0;
        RePopulateDataViewer;
        DoContextValidation(dvtDemandDefAll);
        FAppModules.Model.StudyDataHasChanged
          (sdccEdit, 'NrOfDemandDefinitions', IntToStr(lCount), IntToStr(lAllocDef.NrOfDemandDefinitions));
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.DoAddSupportSubSystem (Sender: TObject);
const OPNAME = 'TFMDemandDefinitionValidator.DoAddSupportSubSystem';
var
  lAllocDef     : IAllocationDefinition;
  lDemandDef    : IDemandDefinition;
  lSupSubSystem : ISupportSubSystem;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        lSupSubSystem := lDemandDef.NewSupportSubSystem;
        PopulateDemandDef(lAllocDef);
        DoContextValidation(dvtSupSubSystemID);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.DoDeleteSupportSubSystem(Sender: TObject);
const OPNAME = 'TFMDemandDefinitionValidator.DoDeleteSupportSubSystem';
var
  lAllocDef     : IAllocationDefinition;
  lIndex        : integer;
  lDemandDef    : IDemandDefinition;
  lSupSubSys    : ISupportSubSystem;
  lMessage      : string;
begin
  try
    lIndex := FMDemandDefinitionDialog.GrdSupportSubSystems.Row-1;
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) AND (lIndex >= 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        lSupSubSys := lDemandDef.SupportSubSystemByIndex[lIndex];
        if (lSupSubSys.SubSystemID = lDemandDef.ParentSubSystemID) then
        begin
          lMessage :=  FAppModules.Language.GetString('PlanningGUI.MayNotDeleteResidentSupSubSys');
          ShowMessage(lMessage);
        end
        else
        begin
          lSupSubSys := nil;
          if (lDemandDef.RemoveSupportSubSystem(lIndex)) then
          begin
            PopulateDemandDef(lAllocDef);
            DoContextValidation(dvtSupSubSystemID);
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TFMDemandDefinitionValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.OnEditControltExit (Sender : TObject);
const OPNAME = 'TFMDemandDefinitionValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with FMDemandDefinitionDialog do
    begin
      if ((Sender = CbxDemandCentreID) AND
          (CbxDemandCentreID.HasValueChanged)) then
        UpdateDemandCentreID
      else
      if ((Sender = CbxUserCategory) AND
          (CbxUserCategory.HasValueChanged)) then
        UpdateUserCategory
      else
      if ((Sender = CbxSupportArc1) AND
          (CbxSupportArc1.HasValueChanged)) then
        UpdateSupportArc01
      else
      if ((Sender = CbxSupportArc2) AND
          (CbxSupportArc2.HasValueChanged)) then
        UpdateSupportArc02
      else
      if (Sender = CbxSupportSubSystem) then
        UpdateSupportSubSystem
      else
      if (Sender = CbxChannel) then
        UpdateSupportChannel
      else
      //if (Sender = EdtTargetDemand) AND (EdtTargetDemand.HasValueChanged) then
        //UpdateTargetBaseYear;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.OnRgpGrowthTypeClick(Sender: TObject);
const OPNAME = 'TFMDemandDefinitionValidator.OnRgpGrowthTypeClick';
begin
  try
    if(FMDemandDefinitionDialog.RgpGrowthType.HasValueChanged) then
      UpdateGrowthType;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.UpdateDemandCentreID;
const OPNAME = 'TFMDemandDefinitionValidator.UpdateDemandCentreID';
var
  lAllocDef      : IAllocationDefinition;
  lDemandDef     : IDemandDefinition;
  lMessage       : string;
  lValue         : integer;
  lMasterControl : IMasterControlFeature;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          if (CbxDemandCentreID.ItemIndex >= 0) then
          begin
            lValue := Integer(CbxDemandCentreID.Items.Objects[CbxDemandCentreID.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxDemandCentreID.FieldProperty.FieldName, IntToStr(lValue), lMessage)) then
            begin
              lDemandDef.DemandCentreID := lValue;
              lMasterControl := (FAppModules.Model.ModelData as IYieldModelData).NetworkFeaturesData.
                                  MasterControlFeatureList.DemandCentreByID[lValue];
              if (lMasterControl <> nil) then
              begin
                lDemandDef.Name := lMasterControl.FeatureName;
                CbxDemandCentreID.SetFieldIndex(CbxDemandCentreID.Items.IndexOf(lMasterControl.FeatureName));
              end
              else
                CbxDemandCentreID.ItemIndex := -1;
              DoContextValidation(dvtDemandCentreID);
            end
            else
              CbxDemandCentreID.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.UpdateGrowthType;
const OPNAME = 'TFMDemandDefinitionValidator.UpdateGrowthType';
var
  lAllocDef     : IAllocationDefinition;
  lDemandDef    : IDemandDefinition;
  lMessage      : string;
  lValue        : string;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          if ((lDemandDef.GrowthType + 1) <> RgpGrowthType.ItemIndex) then
          begin
            lValue := IntToStr(RgpGrowthType.ItemIndex - 1);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                RgpGrowthType.FieldProperty.FieldName, lValue, lMessage)) then
            begin
              RgpGrowthType.ValidationError := lMessage;
              lDemandDef.GrowthType := RgpGrowthType.ItemIndex - 1;
              RgpGrowthType.ItemIndex  := lDemandDef.GrowthType + 1;
              DoContextValidation(dvtGrowthType);
            end
            else
              RgpGrowthType.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.UpdateUserCategory;
const OPNAME = 'TFMDemandDefinitionValidator.UpdateUserCategory';
var
  lAllocDef     : IAllocationDefinition;
  lMessage      : string;
  lValue        : integer;
  lDemandDef    : IDemandDefinition;
  lUserCategory : IUserCategory;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          if (CbxUserCategory.ItemIndex >= 0) then
          begin
            lValue := Integer(CbxUserCategory.Items.Objects[CbxUserCategory.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxUserCategory.FieldProperty.FieldName,
                IntToStr(lValue), lMessage)) then
            begin
              lDemandDef.UserCategoryID := lValue;
              lUserCategory := lAllocDef.CategoryByID[lDemandDef.UserCategoryID];
              if (lUserCategory <> nil) then
                CbxUserCategory.SetFieldIndex(CbxUserCategory.Items.IndexOf(lUserCategory.Description))
              else
                CbxUserCategory.ItemIndex := -1;
              DoContextValidation(dvtDCUserCategoryID);
            end
            else
              CbxUserCategory.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.UpdateSupportArc01;
const OPNAME = 'TFMDemandDefinitionValidator.UpdateSupportArc01';
var
  lAllocDef       : IAllocationDefinition;
  lDemandDef      : IDemandDefinition;
  lMessage        : string;
  lValue          : integer;
  lYieldModelData : IYieldModelData;
  lChannelList    : IChannelList;
  lChannel        : IGeneralFlowChannel;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          if (CbxSupportArc1.ItemIndex >= 0) then
          begin
            lValue := Integer(CbxSupportArc1.Items.Objects[CbxSupportArc1.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxSupportArc1.FieldProperty.FieldName, IntToStr(lValue), lMessage)) then
            begin
              lDemandDef.SupportArc1 := lValue;
              lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
              lChannelList    := lYieldModelData.NetworkElementData.ChannelList;
              lChannel        := lChannelList.ChannelByChannelNumber[lValue];
              if (lChannel <> nil) then
                CbxSupportArc1.SetFieldIndex(CbxSupportArc1.Items.IndexOf(lChannel.ChannelName))
              else
                CbxSupportArc1.ItemIndex := -1;
              DoContextValidation(dvtSupportArc1);
            end
            else
              CbxSupportArc1.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.UpdateSupportArc02;
const OPNAME = 'TFMDemandDefinitionValidator.UpdateSupportArc02';
var
  lAllocDef       : IAllocationDefinition;
  lDemandDef      : IDemandDefinition;
  lMessage        : string;
  lValue          : integer;
  lYieldModelData : IYieldModelData;
  lChannelList    : IChannelList;
  lChannel        : IGeneralFlowChannel;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          if (CbxSupportArc2.ItemIndex >= 0) then
          begin
            lValue := Integer(CbxSupportArc2.Items.Objects[CbxSupportArc2.ItemIndex]);
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxSupportArc2.FieldProperty.FieldName, IntToStr(lValue), lMessage)) then
            begin
              lDemandDef.SupportArc2 := lValue;
              lYieldModelData := (FAppModules.Model.ModelData as IYieldModelData);
              lChannelList    := lYieldModelData.NetworkElementData.ChannelList;
              lChannel        := lChannelList.ChannelByChannelNumber[lValue];
              if (lChannel <> nil) then
                CbxSupportArc2.SetFieldIndex(CbxSupportArc2.Items.IndexOf(lChannel.ChannelName))
              else
                CbxSupportArc2.ItemIndex := -1;
              DoContextValidation(dvtSupportArc2);
            end
            else
              CbxSupportArc2.ValidationError := lMessage;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.UpdateSupportSubSystem;
const OPNAME = 'TFMDemandDefinitionValidator.UpdateSupportSubSystem';
var
  lAllocDef         : IAllocationDefinition;
  lMessage          : string;
  lStrValue         : string;
  lDemandDef        : IDemandDefinition;
  lSupSubSystemID   : integer;
  lSupportSubSystem : ISupportSubSystem;
  lRow              : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          lRow := GrdSupportSubSystems.Row;
          lSupportSubSystem := lDemandDef.SupportSubSystemByIndex[lRow-1];
          if (lSupportSubSystem <> nil) then
          begin
            lSupSubSystemID := 0;
            if (CbxSupportSubSystem.ItemIndex >= 0) then
              lSupSubSystemID := Integer(CbxSupportSubSystem.Items.Objects[CbxSupportSubSystem.ItemIndex]);
            lStrValue := IntToStr(lSupSubSystemID);
            CbxSupportSubSystem.ValidationError := '';
            GrdSupportSubSystems.ValidationError[0, lRow, gveCellContext] := '';
            if (FAppModules.FieldProperties.ValidateFieldProperty(
                CbxSupportSubSystem.FieldProperty.FieldName, lStrValue, lMessage)) then
            begin
              GrdSupportSubSystems.ValidationError[0, lRow, gveCellContext] := '';
              lSupportSubSystem.SubSystemID := lSupSubSystemID;
              PopulateDemandDef(lAllocDef);
              DoContextValidation(dvtSupSubSystemID);
            end
            else
            begin
              CbxSupportSubSystem.ValidationError := lMessage;
              GrdSupportSubSystems.ValidationError[0, lRow, gveCellContext] := lMessage;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.UpdateSupportChannel;
const OPNAME = 'TFMDemandDefinitionValidator.UpdateSupportChannel';
var
  lAllocDef         : IAllocationDefinition;
  lMessage          : string;
  lStrValue         : string;
  lDemandDef        : IDemandDefinition;
  lChannelNr        : integer;
  lSupportSubSystem : ISupportSubSystem;
  lRow              : integer;
  lCol              : integer;
  lIndex            : integer;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          lRow := GrdSupportSubSystems.Row;
          lCol := GrdSupportSubSystems.Col;
          lSupportSubSystem := lDemandDef.SupportSubSystemByIndex[lRow-1];
          if (lSupportSubSystem <> nil) then
          begin
            lChannelNr := 0;
            if (CbxChannel.ItemIndex >= 0) then
              lChannelNr := Integer(CbxChannel.Items.Objects[CbxChannel.ItemIndex]);
            if (lChannelNr = 0) then
            begin
              for lIndex := lCol to 4 do
                lSupportSubSystem.SupportChannelNrByIndex[lIndex] :=
                   lSupportSubSystem.SupportChannelNrByIndex[lIndex+1];
              lSupportSubSystem.SupportChannelNrByIndex[5] := 0;
              PopulateDemandDef(lAllocDef);
              DoContextValidation(dvtSupSubSysChannelNr);
            end
            else
            begin
              lStrValue := IntToStr(lChannelNr);
              CbxChannel.ValidationError := '';
              GrdSupportSubSystems.ValidationError[lCol, lRow, gveCellContext] := '';
              if (FAppModules.FieldProperties.ValidateFieldProperty(
                  CbxChannel.FieldProperty.FieldName, lStrValue, lMessage, lCol)) then
              begin
                GrdSupportSubSystems.ValidationError[lCol, lRow, gveCellContext] := '';
                lSupportSubSystem.SupportChannelNrByIndex[lCol] := lChannelNr;
                PopulateDemandDef(lAllocDef);
                DoContextValidation(dvtSupSubSysChannelNr);
              end
              else
              begin
                CbxChannel.ValidationError := lMessage;
                GrdSupportSubSystems.ValidationError[lCol, lRow, gveCellContext] := lMessage;
              end;
            end;
          end;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.DoContextValidation (AValidationType : TDialogValidationType);
const OPNAME = 'TFMDemandDefinitionValidator.DoContextValidation';
var
  lAllocDef     : IAllocationDefinition;
  lDemandDef    : IDemandDefinition;
begin
  try
    FAllErrorMessages.Clear;
    lAllocDef := nil;
    lDemandDef := nil;
    if (FIdentifier > 0) then
    begin
      lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                     AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
      if (lAllocDef <> nil) AND (FDemandDefID > 0) then
      begin
        lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
        if (lDemandDef <> nil) then
        begin
          if (AValidationType = dvtDemandDefAll) OR (AValidationType = dvtDemandCentreID) then
            ValidateDemandCentreID(lDemandDef);
          if (AValidationType = dvtDemandDefAll) OR (AValidationType = dvtGrowthType) then
            ValidateGrowthType(lDemandDef);
          if (AValidationType = dvtDemandDefAll) OR (AValidationType = dvtDCUserCategoryID) then
            ValidateUserCategory(lDemandDef);
          if (AValidationType = dvtDemandDefAll) OR (AValidationType = dvtSupportArc1) then
            ValidateSupportArc1(lDemandDef);
          if (AValidationType = dvtDemandDefAll) OR (AValidationType = dvtSupportArc2) then
            ValidateSupportArc2(lDemandDef);
          if (AValidationType = dvtDemandDefAll) OR (AValidationType = dvtSupSubSystemID) OR
             (AValidationType = dvtSupSubSysChannelNr) then
            ValidateSupportSubSystems(lDemandDef);
          if (AValidationType = dvtDemandDefAll) OR (AValidationType = dvtTargetDemand) then
            ValidateTargetBaseYear(lDemandDef);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.ValidateDemandCentreID (ADemandDef : IDemandDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.ValidateDemandCentreID';
begin
  try
    with FMDemandDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADemandDef.Validate(FErrorMessage, 'DDDemandCentreID')) then
      begin
        CbxDemandCentreID.InValidationError := TRUE;
        CbxDemandCentreID.ValidationError := FErrorMessage;
        CbxDemandCentreID.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxDemandCentreID.InValidationError := FALSE;
        CbxDemandCentreID.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.ValidateGrowthType (ADemandDef : IDemandDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.ValidateGrowthType';
begin
  try
    with FMDemandDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADemandDef.Validate(FErrorMessage, 'GrowthType')) then
      begin
        FAllErrorMessages.Add(Trim(FErrorMessage));
        RgpGrowthType.ValidationError := FErrorMessage;
        RgpGrowthType.InValidationError := TRUE;
        RgpGrowthType.ShowErrorState(TRUE);
      end
      else
      begin
        RgpGrowthType.ValidationError := '';
        RgpGrowthType.InValidationError := FALSE;
        RgpGrowthType.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.ValidateUserCategory (ADemandDef : IDemandDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.ValidateUserCategory';
begin
  try
    with FMDemandDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADemandDef.Validate(FErrorMessage, 'UserCategory')) then
      begin
        CbxUserCategory.InValidationError := TRUE;
        CbxUserCategory.ValidationError := FErrorMessage;
        CbxUserCategory.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxUserCategory.InValidationError := FALSE;
        CbxUserCategory.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.ValidateSupportArc1 (ADemandDef : IDemandDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.ValidateSupportArc1';
begin
  try
    with FMDemandDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADemandDef.Validate(FErrorMessage, 'SupportArc1')) then
      begin
        CbxSupportArc1.InValidationError := TRUE;
        CbxSupportArc1.ValidationError := FErrorMessage;
        CbxSupportArc1.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxSupportArc1.InValidationError := FALSE;
        CbxSupportArc1.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.ValidateSupportArc2 (ADemandDef : IDemandDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.ValidateSupportArc2';
begin
  try
    with FMDemandDefinitionDialog do
    begin
      FErrorMessage := '';
      if (NOT ADemandDef.Validate(FErrorMessage, 'SupportArc2')) then
      begin
        CbxSupportArc2.InValidationError := TRUE;
        CbxSupportArc2.ValidationError := FErrorMessage;
        CbxSupportArc2.ShowErrorState(TRUE);
        FAllErrorMessages.Add(Trim(FErrorMessage));
      end
      else
      begin
        CbxSupportArc2.InValidationError := FALSE;
        CbxSupportArc2.ShowErrorState(FALSE);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.ValidateSupportSubSystems (ADemandDef : IDemandDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.ValidateSupportSubSystems';
var
  lErrorIndexes  : TStringList;
  lRow           : integer;
  lCol           : integer;
  lCount         : integer;
  lIndexStr      : string;
  lErrorMessages : TStringList;
  lPos           : integer;
begin
  try
    with FMDemandDefinitionDialog do
    begin
      FErrorMessage := '';

      for lRow := 1 to ADemandDef.NrOfSupportSubSystems do
        for lCol := 0 to GrdSupportSubSystems.ColCount - 1 do
          GrdSupportSubSystems.ValidationError[lCol, lRow, gveCellContext] := '';
      if (NOT ADemandDef.Validate(FErrorMessage, 'SupportSubSystems')) then
      begin
        lErrorIndexes  := TStringList.Create;
        lErrorMessages := TStringList.Create;
        try
          ExtractErrorsAndColumns(FErrorMessage, lErrorMessages, lErrorIndexes);
          for lCount := 0 to lErrorIndexes.Count - 1 do
          begin
            lIndexStr := lErrorIndexes.Strings[lCount];
            lPos      := Pos(',', lIndexStr);
            lRow      := StrToInt(Copy(lIndexStr, 1, lPos-1));
            lCol      := StrToInt(Copy(lIndexStr, lPos+1, Length(lIndexStr)-lPos));
            GrdSupportSubSystems.ValidationError[lCol, lRow, gveCellContext] := lErrorMessages.Strings[lCount];
          end;
          FAllErrorMessages.AddStrings(lErrorMessages);
        finally
          FreeAndNil(lErrorIndexes);
          FreeAndNil(lErrorMessages);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.DoMoveUpDemandDef (Sender: TObject);
const OPNAME = 'TFMDemandDefinitionValidator.DoMoveUpDemandDef';
var
  lAllocDef     : IAllocationDefinition;
  lDemandDef    : IDemandDefinition;
  lOtherCentre  : IDemandDefinition;
  lCount        : integer;
  lDone         : boolean;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        lDone  := FALSE;
        lCount := 0;
        while ((NOT lDone) AND (lCount < lAllocDef.NrOfDemandDefinitions)) do
        begin
          lOtherCentre := lAllocDef.DemandDefinitionByIndex[lCount];
          if (lOtherCentre.ParentSubSystemID = lDemandDef.ParentSubSystemID) AND
             (lOtherCentre.Order = lDemandDef.Order - 1) then
          begin
            lDone := TRUE;
            lOtherCentre.Order := lOtherCentre.Order + 1;
            lDemandDef.Order := lDemandDef.Order - 1;
          end
          else
            lCount := lCount + 1;
        end;
        RePopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TFMDemandDefinitionValidator.DoMoveDownDemandDef (Sender: TObject);
const OPNAME = 'TFMDemandDefinitionValidator.DoMoveDownDemandDef';
var
  lAllocDef     : IAllocationDefinition;
  lDemandDef    : IDemandDefinition;
  lOtherCentre  : IDemandDefinition;
  lCount        : integer;
  lDone         : boolean;
begin
  try
    lAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (lAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      lDemandDef := lAllocDef.DemandDefinitionByID[FDemandDefID];
      if (lDemandDef <> nil) then
      begin
        lDone  := FALSE;
        lCount := 0;
        while ((NOT lDone) AND (lCount < lAllocDef.NrOfDemandDefinitions)) do
        begin
          lOtherCentre := lAllocDef.DemandDefinitionByIndex[lCount];
          if (lOtherCentre.ParentSubSystemID = lDemandDef.ParentSubSystemID) AND
             (lOtherCentre.Order = lDemandDef.Order + 1) then
          begin
            lDone := TRUE;
            lOtherCentre.Order := lOtherCentre.Order - 1;
            lDemandDef.Order := lDemandDef.Order + 1;
          end
          else
            lCount := lCount + 1;
        end;
        RePopulateDataViewer;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
{
procedure TFMDemandDefinitionValidator.UpdateTargetBaseYear;
const OPNAME = 'TFMDemandDefinitionValidator.UpdateTargetBaseYear';
var
  LAllocDef      : IAllocationDefinition;
  LDemandDef     : IDemandDefinition;
  LMessage       : string;
begin
  try
    LAllocDef := (FAppModules.Model.ModelData as IPlanningModelData).
                   AllocationDefinitionsList.AllocationDefinitionByID[FIdentifier];
    if (LAllocDef <> nil) AND (FDemandDefID > 0) then
    begin
      LDemandDef := LAllocDef.DemandDefinitionByID[FDemandDefID];
      if (LDemandDef <> nil) then
      begin
        with FMDemandDefinitionDialog do
        begin
          if (FAppModules.FieldProperties.ValidateFieldProperty(
              'TargetDemand', EdtTargetDemand.Text,LMessage)) then
          begin
            EdtTargetDemand.FieldValidationError := LMessage;
            LDemandDef.TargetDemand := StrToFloat(EdtTargetDemand.Text);
            EdtTargetDemand.SetFieldValue(LDemandDef.TargetDemand);
            DoContextValidation(dvtTargetDemand);
          end
          else
            EdtTargetDemand.FieldValidationError := lMessage;
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;
 }
procedure TFMDemandDefinitionValidator.ValidateTargetBaseYear(ADemandDef: IDemandDefinition);
const OPNAME = 'TFMDemandDefinitionValidator.ValidateTargetBaseYear';
begin
  try
    with FMDemandDefinitionDialog do
    begin
      FErrorMessage := '';
      ADemandDef.Validate(FErrorMessage,'TargetDemand');
      EdtTargetDemand.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

