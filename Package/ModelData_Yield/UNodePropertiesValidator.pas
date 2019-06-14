//
//
//  UNIT      : Contains the class TNodePropertiesValidator.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2003/07/03
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UNodePropertiesValidator;

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
  VoaimsCom_TLB,
  UDataEditComponent,
  UYieldContextValidationType,
  UAbstractYieldDataDialogValidator,
  UNodePropertiesDialog;

type
  TNodePropertiesValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnSummaryOutputClick(Sender: TObject);
    procedure RePopulateDataViewer;
    procedure UpdateNodeName;
    procedure UpdateXCoord;
    procedure UpdateYCoord;
    procedure UpdateChannelIncludeSummary;
    procedure ValidateNodeName(ANode: IReservoirData);
    procedure ValidateNodeNumber(ANode: IReservoirData);
    function NodePropertiesDialog: TNodePropertiesDialog;
  public

    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation (AValidationType : TDialogValidationType); override;
    function DetermineWizardStatus (ASequence : integer = 0) : integer; override;
  end;

implementation

uses
  SysUtils,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TNodePropertiesValidator }

procedure TNodePropertiesValidator.CreateMemberObjects;
const OPNAME = 'TNodePropertiesValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FIdentifier := -1;
    FPanel := TNodePropertiesDialog.Create(FPanelOwner,FAppModules);

    with NodePropertiesDialog do
    begin
      NodeNumberEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('NodeCount');
      NodeNumberEdit.OnEnter       := OnEditControlEnter;
      NodeNumberEdit.IsEnabled     := FALSE;

      NodeNameEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('ReservoirName');
      NodeNameEdit.OnEnter       := OnEditControlEnter;
      NodeNameEdit.OnExit        := OnEditControltExit;

      NodeXCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('XCoord');
      NodeXCoordEdit.OnEnter       := OnEditControlEnter;
      NodeXCoordEdit.OnExit        := OnEditControltExit;

      NodeYCoordEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('YCoord');
      NodeYCoordEdit.OnEnter       := OnEditControlEnter;
      NodeYCoordEdit.OnExit        := OnEditControltExit;

      SummaryOutputChkBox.FieldProperty := FAppModules.FieldProperties.FieldProperty('IncludeSummary');
      SummaryOutputChkBox.OnEnter       := OnEditControlEnter;
      SummaryOutputChkBox.OnClick       := OnSummaryOutputClick;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.DestroyMemberObjects;
const OPNAME = 'TNodePropertiesValidator.DestroyMemberObjects';
begin
  try
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesValidator.Initialise: boolean;
const OPNAME = 'TNodePropertiesValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesValidator.LanguageHasChanged: boolean;
const OPNAME = 'TNodePropertiesValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.Properties');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.ClearDataViewer;
const OPNAME = 'TNodePropertiesValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    NodePropertiesDialog.NodeNumberEdit.SetFieldValue('-1');
    NodePropertiesDialog.NodeNameEdit.SetFieldValue('');
    NodePropertiesDialog.NodeXCoordEdit.SetFieldValue('');
    NodePropertiesDialog.NodeYCoordEdit.SetFieldValue('');
    NodePropertiesDialog.SummaryOutputChkBox.Checked := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.PopulateDataViewer;
const OPNAME = 'TNodePropertiesValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.RePopulateDataViewer;
const OPNAME = 'TNodePropertiesValidator.RePopulateDataViewer';
var
  LNodeObject : IReservoirData;
begin
  try
    if (FIdentifier >= 0) then
    begin
      LNodeObject := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
      if (LNodeObject <> nil) then
      begin
        with NodePropertiesDialog do
        begin
          NodeNumberEdit.SetFieldValue(IntToStr(LNodeObject.ReservoirConfigurationData.ReservoirIdentifier));
          NodeNameEdit.SetFieldValue(LNodeObject.ReservoirConfigurationData.ReservoirName);
          NodeXCoordEdit.SetFieldValue(FloatToStr(LNodeObject.ReservoirConfigurationData.XCoord));
          NodeYCoordEdit.SetFieldValue(FloatToStr(LNodeObject.ReservoirConfigurationData.YCoord));
          SummaryOutputChkBox.Checked := (UpperCase(Trim(LNodeObject.ReservoirConfigurationData.IncludeSummary)) = 'Y');
          SetNodeType(LNodeObject.ReservoirConfigurationData.ReservoirIdentifier,
                     (LNodeObject.ReservoirConfigurationData.NodeType = ntNodeWithInflow));
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesValidator.SaveState: boolean;
const OPNAME = 'TNodePropertiesValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesValidator.NodePropertiesDialog : TNodePropertiesDialog;
const OPNAME = 'TNodePropertiesValidator.NodePropertiesDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TNodePropertiesDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TNodePropertiesValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
   if (AFieldName = 'ReservoirName') and
      (AOldValue = NodePropertiesDialog.NodeNameEdit.Text) then
      RePopulateDataViewer;
   if (AFieldName = 'ReservoirNodeNumber') and
      (AOldValue = NodePropertiesDialog.NodeNumberEdit.Text) then
      RePopulateDataViewer;
   if (AFieldName = 'XCoord') or (AFieldName = 'YCoord') then
      RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesValidator.StudyHasChanged: boolean;
const OPNAME = 'TNodePropertiesValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TNodePropertiesValidator.OnEditControlEnter';
begin
  inherited OnEditControlEnter(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TNodePropertiesValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    with NodePropertiesDialog do
    begin
      if ((Sender = NodeNameEdit) AND (NodeNameEdit.HasValueChanged)) then
        UpdateNodeName
      else
      if ((Sender = NodeXCoordEdit) AND (NodeXCoordEdit.HasValueChanged)) then
        UpdateXCoord
      else
      if ((Sender = NodeYCoordEdit) AND (NodeYCoordEdit.HasValueChanged)) then
        UpdateYCoord;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.UpdateNodeName;
const OPNAME = 'TNodePropertiesValidator.UpdateNodeName';
var
  lNode : IReservoirData;
  LErrorMessage: string;
begin
  try
    lNode := TYieldModelDataObject(FAppModules.Model.ModelData).
             NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
    if (lNode <> nil) then
    begin
      with NodePropertiesDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty(
            'ReservoirName', NodeNameEdit.Text,LErrorMessage)) then
        begin
          lNode.ReservoirConfigurationData.ReservoirName := Trim(NodeNameEdit.Text);
          NodeNameEdit.SetFieldValue(lNode.ReservoirConfigurationData.ReservoirName);
          DoContextValidation(dvtResPropReservoirName);
        end;
          NodeNameEdit.FieldValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.UpdateChannelIncludeSummary;
const OPNAME = 'TNodePropertiesValidator.UpdateChannelIncludeSummary';
var
  lNode           : IReservoirData;
  lIncludeSummary : string;
begin
  try
    with NodePropertiesDialog do
    begin
      lNode := TYieldModelDataObject(FAppModules.Model.ModelData).
                 NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
      if (lNode <> nil) then
      begin
        lIncludeSummary := UpperCase(Trim(lNode.ReservoirConfigurationData.IncludeSummary));
        if((lIncludeSummary = 'Y')  and (NOT SummaryOutputChkBox.Checked) or
           (lIncludeSummary <> 'Y') and (SummaryOutputChkBox.Checked)) then
        begin
          if SummaryOutputChkBox.Checked then
            lIncludeSummary := 'Y'
          else
            lIncludeSummary := 'N';
          lNode.ReservoirConfigurationData.IncludeSummary := lIncludeSummary;
          SummaryOutputChkBox.Checked := (UpperCase(Trim(lNode.ReservoirConfigurationData.IncludeSummary)) = 'Y');
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TNodePropertiesValidator.DoContextValidation';
var
  lNode     : IReservoirData;
  lNodeList : IReservoirDataList;
begin
  try
    FAllErrorMessages.Clear;
    if (FIdentifier >= 0) then
    begin
      try
        lNodeList := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkElementData.ReservoirList;
        lNode     := lNodeList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (lNode) <> nil then
        begin
           if (AValidationType in [dvtResPropAll, dvtResPropReservoirName]) then
              ValidateNodeName(lNode);
           if (AValidationType in [dvtResPropAll, dvtResPropReservoirNumber]) then
              ValidateNodeNumber(lNode);
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.ValidateNodeName(ANode: IReservoirData);
const OPNAME = 'TNodePropertiesValidator.ValidateNodeName';
begin
  try
    with NodePropertiesDialog do
    begin
      FErrorMessage := '';
      if (NOT ANode.ReservoirConfigurationData.Validate(FErrorMessage,'ReservoirName')) then
        FAllErrorMessages.Add(FErrorMessage);
      NodeNameEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.ValidateNodeNumber(ANode: IReservoirData);
const OPNAME = 'TNodePropertiesValidator.ValidateNodeNumber';
begin
  try
    with NodePropertiesDialog do
    begin
      FErrorMessage := '';
      if (NOT ANode.ReservoirConfigurationData.Validate(FErrorMessage,'ReservoirNumber')) then
        FAllErrorMessages.Add(FErrorMessage);
      NodeNumberEdit.ContextValidationError := FErrorMessage;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TNodePropertiesValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'TNodePropertiesValidator.DetermineWizardStatus';
var
  lNode     : IReservoirData;
  lNodeList : IReservoirDataList;
begin
  Result := inherited DetermineWizardStatus(ASequence);
  try
    if (FIdentifier >= 0) then
    begin
      try
        lNodeList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ReservoirList;
        lNode     := lNodeList.ReservoirOrNodeByIdentifier[FIdentifier];
        if (lNode <> nil) then
        begin
          DoContextValidation(dvtResPropAll);
          if (FAllErrorMessages.Count = 0) then
            Result := 2
          else
            Result := 1;
        end;
      finally
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.UpdateXCoord;
const OPNAME = 'TNodePropertiesValidator.UpdateXCoord';
var
  lNode : IReservoirData;
  LErrorMessage: string;
begin
  try
    lNode := TYieldModelDataObject(FAppModules.Model.ModelData).
             NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
    if (lNode <> nil) then
    begin
      with NodePropertiesDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('XCoord', NodeXCoordEdit.Text,LErrorMessage)) then
        begin
          lNode.ReservoirConfigurationData.XCoord := StrToFloat(Trim(NodeXCoordEdit.Text));
          NodeXCoordEdit.SetFieldValue(FloatToStr(lNode.ReservoirConfigurationData.XCoord));
         end;
          NodeXCoordEdit.FieldValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.UpdateYCoord;
const OPNAME = 'TNodePropertiesValidator.UpdateYCoord';
var
  lNode : IReservoirData;
  LErrorMessage: string;
begin
  try
    lNode := TYieldModelDataObject(FAppModules.Model.ModelData).
             NetworkElementData.ReservoirList.ReservoirOrNodeByIdentifier[FIdentifier];
    if (lNode <> nil) then
    begin
      with NodePropertiesDialog do
      begin
        if (FAppModules.FieldProperties.ValidateFieldProperty('YCoord', NodeYCoordEdit.Text,LErrorMessage)) then
        begin
          lNode.ReservoirConfigurationData.YCoord := StrToFloat(Trim(NodeYCoordEdit.Text));
          NodeYCoordEdit.SetFieldValue(FloatToStr(lNode.ReservoirConfigurationData.YCoord));
         end;
          NodeYCoordEdit.FieldValidationError := LErrorMessage;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TNodePropertiesValidator.OnSummaryOutputClick(Sender: TObject);
const OPNAME = 'TNodePropertiesValidator.OnSummaryOutputClick';
begin
  try
    if(NodePropertiesDialog.SummaryOutputChkBox.HasValueChanged) then
      UpdateChannelIncludeSummary;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

