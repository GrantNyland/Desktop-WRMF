//
//
//  UNIT      : Contains the class THydrologyAlocationValidator.
//  AUTHOR    : Dziedzi Ramulondi (Cornastone)
//  DATE      : 2004/07/23
//  COPYRIGHT : Copyright © 2004 DWAF
//
//
unit UHydrologyAlocationValidator;

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
  UAbstractYieldDataDialogValidator,
  UHydrologyAlocationDialog;

type
  THydrologyAlocationValidator = class(TAbstractYieldDataDialogValidator)
  protected
    procedure CreateMemberObjects; override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure UpdateParamFile(AFileName: string);
    procedure OnSelectParamFileClick(Sender: TObject);
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

    function HydrologyAlocationDialog: THydrologyAlocationDialog;

  end;

implementation

uses
  SysUtils,
  VCL.Dialogs,
  UYieldModelDataObject,
  UReservoirData,
  UErrorHandlingOperations;

{ THydrologyAlocationValidator }

procedure THydrologyAlocationValidator.CreateMemberObjects;
const OPNAME = 'THydrologyAlocationValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FPanel := THydrologyAlocationDialog.Create(FPanelOwner,FAppModules);

    with HydrologyAlocationDialog do
    begin
      ParamFileNameEdit.FieldProperty := FAppModules.FieldProperties.FieldProperty('ParamFile');
      ParamFileNameEdit.OnEnter       := OnEditControlEnter;
      ParamFileNameEdit.IsEnabled     := FALSE;

      SelectParamFileButton.FieldProperty := FAppModules.FieldProperties.FieldProperty('ParamFile');
      SelectParamFileButton.OnEnter       := OnEditControlEnter;
      SelectParamFileButton.OnClick       := OnSelectParamFileClick;

    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyAlocationValidator.Initialise: boolean;
const OPNAME = 'THydrologyAlocationValidator.Initialise';
begin
  Result := inherited Initialise;
  try
    ClearDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyAlocationValidator.LanguageHasChanged: boolean;
const OPNAME = 'THydrologyAlocationValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.Properties');
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyAlocationValidator.ClearDataViewer;
const OPNAME = 'THydrologyAlocationValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    HydrologyAlocationDialog.ParamFileNameEdit.SetFieldValue('');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyAlocationValidator.PopulateDataViewer;
const OPNAME = 'THydrologyAlocationValidator.PopulateDataViewer';
//var
//  LNodeObject : TAbstractReservoirData;
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    HydrologyAlocationDialog.ParamFileNameEdit.SetFieldValue(
      TYieldModelDataObject(FAppModules.Model.ModelData).
      CastFileNamesObject.ParamFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyAlocationValidator.SaveState: boolean;
const OPNAME = 'THydrologyAlocationValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyAlocationValidator.HydrologyAlocationDialog : THydrologyAlocationDialog;
const OPNAME = 'THydrologyAlocationValidator.HydrologyAlocationDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := THydrologyAlocationDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyAlocationValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'THydrologyAlocationValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyAlocationValidator.StudyHasChanged: boolean;
const OPNAME = 'THydrologyAlocationValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyAlocationValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'THydrologyAlocationValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyAlocationValidator.OnSelectParamFileClick(Sender: TObject);
const OPNAME = 'THydrologyAlocationValidator.OnSelectParamFileClick';
var
  LFileName: string;
begin
  try
    if PromptForFileName(LFileName,'*.dat','','Select pramemter file','',False) then
       UpdateParamFile(LFileName);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyAlocationValidator.UpdateParamFile(AFileName: string);
const OPNAME = 'THydrologyAlocationValidator.UpdateParamFile';
begin
  try
   TYieldModelDataObject(FAppModules.Model.ModelData).
     CastFileNamesObject.ParamFileName := AFileName;
    PopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure THydrologyAlocationValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'THydrologyAlocationValidator.DoContextValidation';
{var
  lNode     : TAbstractReservoirData;
  lNodeList : TAbstractReservoirDataList;
}begin
  try
    {FAllErrorMessages.Clear;
    if (FNodeIdentifier >= 0) then
    begin
      try
        lNodeList := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkElementData.ReservoirList;
        lNode     := lNodeList.ReservoirOrNodeByIdentifier[FNodeIdentifier];
        if Assigned(lNode) then
        begin
           if (AValidationType in [dvtResPropAll, dvtResPropReservoirName]) then
              ValidateNodeName(lNode);
           if (AValidationType in [dvtResPropAll, dvtResPropReservoirNumber]) then
              ValidateNodeNumber(lNode);
        end;
      finally
      end;
    end;}
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function THydrologyAlocationValidator.DetermineWizardStatus (ASequence : integer = 0) : integer;
const OPNAME = 'THydrologyAlocationValidator.DetermineWizardStatus';
{var
  lNode     : TAbstractReservoirData;
  lNodeList : TAbstractReservoirDataList;
}begin
  Result := inherited DetermineWizardStatus(ASequence);
  {try
    if (FNodeIdentifier >= 0) then
    begin
      try
        lNodeList := TYieldModelDataObject(FAppModules.Model.ModelData).
                       NetworkElementData.ReservoirList;
        lNode     := lNodeList.ReservoirOrNodeByIdentifier[FNodeIdentifier];
        if Assigned(lNode) then
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
  except on E: Exception do HandleError(E, OPNAME) end;}
end;

end.

