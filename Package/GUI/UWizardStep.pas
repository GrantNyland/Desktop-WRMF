{******************************************************************************}
{*  UNIT      : Contains the class TWizardStep.                               *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2004/07/13                                                    *}
{*  COPYRIGHT : Copyright © 2004 DWAF                                         *}
{******************************************************************************}

unit UWizardStep;

interface

uses
  Classes,
  VCL.ExtCtrls,
  VCL.ComCtrls,
  UDataComponent,
  UAbstractObject;

type

  TWizard = class;

  TWizardStepStatus = (wzsNotDone, wzsIncomplete, wzsDone);

  TValidatorClass = class of TAbstractDataDialogValidator;

  TWizardStep = class(TAbstractAppObject)
  protected
    FParentStep     : TWizardStep;
    FNumber         : integer;
    FSequence       : integer;
    FHeading        : string;
    FDescription    : string;
    FStatus         : TWizardStepStatus;
    FSubSteps       : TWizard;
    FSiblings       : TWizard;
    FValidatorClass : TValidatorClass;
    FValidator      : TAbstractDataDialogValidator;
    FTreeNode       : TTreeNode;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure DoValueHasChanged (Sender : TObject);
  public
    function Initialise: boolean; override;
    property ParentStep     : TWizardStep       read FParentStep     write FParentStep;
    property Number         : integer           read FNumber         write FNumber;
    property Sequence       : integer           read FSequence       write FSequence;
    property Heading        : string            read FHeading        write FHeading;
    property Description    : string            read FDescription    write FDescription;
    property Status         : TWizardStepStatus read FStatus         write FStatus;
    property SubSteps       : TWizard           read FSubSteps;
    property Siblings       : TWizard           read FSiblings       write FSiblings;
    property TreeNode       : TTreeNode         read FTreeNode       write FTreeNode;
    function Validator : TAbstractDataDialogValidator;
    function NextLeafStep : TWizardStep;
    function PreviousLeafStep : TWizardStep;
    procedure UpdateStatus;
    procedure UpdateTreeNode;
    function IsStepOK : Boolean;
  end;

  TWizard = class(TAbstractAppObject)
  protected
    FStepList             : TList;
    FOnFinishButtonClick  : TNotifyEvent;
    FSetFinishButtonState : TNotifyEvent;
    FFinishButtonCaption  : string;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure InsertStep (AStep : TWizardStep);
    function FindStep (ANumber : integer) : TWizardStep;
    function AddStep (ANumber         : integer;
                      AParentNumber   : integer;
                      AHeading        : string;
                      ADescription    : string;
                      AValidatorClass : TValidatorClass;
                      ASequence       : integer = 0) : TWizardStep;
  public
    procedure ClearSteps;
    function FirstLeafStep : TWizardStep;
    function LastLeafStep : TWizardStep;
    property StepList : TList read FStepList;
    property OnFinishButtonClick : TNotifyEvent  read FOnFinishButtonClick  write FOnFinishButtonClick;
    property SetFinishButtonState : TNotifyEvent read FSetFinishButtonState write FSetFinishButtonState;
    property FinishButtonCaption  : string       read FFinishButtonCaption;
  end;

implementation

uses
  SysUtils,
  UErrorHandlingOperations;

{******************************************************************************}
{* TWizardStep                                                                *}
{******************************************************************************}

procedure TWizardStep.CreateMemberObjects;
const OPNAME = 'TWizardStep.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSubSteps := TWizard.Create(FAppModules);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardStep.DestroyMemberObjects;
const OPNAME = 'TWizardStep.DestroyMemberObjects';
begin
  try
    FSiblings := nil;
    FreeAndNil(FValidator);
    FreeAndNil(FSubSteps);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizardStep.Initialise : Boolean;
const OPNAME = 'TWizardStep.Initialise';
begin
  Result := inherited Initialise;
  try
    FParentStep     := nil;
    FNumber         := 0;
    FSequence       := 0;
    FHeading        := '';
    FDescription    := '';
    FStatus         := wzsNotDone;
    FValidatorClass := nil;
    FValidator      := nil;
    FSiblings       := nil;
    FTreeNode       := nil;
    if (FSubSteps.StepList.Count > 0) then
      FSubSteps.ClearSteps;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizardStep.Validator : TAbstractDataDialogValidator;
const OPNAME = 'TWizardStep.Validator';
begin
  Result := nil;
  try
    if (Assigned(FValidator)) then
      Result := FValidator
    else if (Assigned(FParentStep)) then
      Result := FParentStep.Validator;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizardStep.NextLeafStep : TWizardStep;
const OPNAME = 'TWizardStep.NextLeafStep';
var
  lIndex : integer;
  lStep  : TWizardStep;
begin
  Result := nil;
  try
    lIndex := FSiblings.StepList.IndexOf(Self);
    if (lIndex >= 0) then
    begin
      if (FSiblings.StepList.Count > lIndex + 1) then
      begin
        lStep := TWizardStep(FSiblings.StepList.Items[lIndex + 1]);
        if (lStep.SubSteps.StepList.Count > 0) then
          Result := lStep.SubSteps.FirstLeafStep
        else
          Result := lStep;
      end
      else if (Assigned(FParentStep)) then
        Result := FParentStep.NextLeafStep;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizardStep.PreviousLeafStep : TWizardStep;
const OPNAME = 'TWizardStep.PreviousLeafStep';
var
  lIndex : integer;
  lStep  : TWizardStep;
begin
  Result := nil;
  try
    lIndex := FSiblings.StepList.IndexOf(Self);
    if (lIndex >= 0) then
    begin
      if (lIndex > 0) then
      begin
        lStep := TWizardStep(FSiblings.StepList.Items[lIndex - 1]);
        if (lStep.SubSteps.StepList.Count > 0) then
          Result := lStep.SubSteps.LastLeafStep
        else
          Result := lStep;
      end
      else if (Assigned(FParentStep)) then
        Result := FParentStep.PreviousLeafStep;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardStep.DoValueHasChanged(Sender: TObject);
const OPNAME = 'TWizardStep.DoValueHasChanged';
begin
  try
    UpdateStatus;
    UpdateTreeNode;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardStep.UpdateTreeNode;
const OPNAME = 'TWizardStep.UpdateTreeNode';
var
  lIndex   : integer;
  lStep    : TWizardStep;
begin
  try
    for lIndex := 0 to FSubSteps.StepList.Count - 1 do
    begin
      lStep := TWizardStep(FSubSteps.StepList.Items[lIndex]);
      lStep.UpdateTreeNode;
    end;
    if (Assigned(FTreeNode)) then
    begin
      FTreeNode.ImageIndex    := Integer(FStatus);
      FTreeNode.SelectedIndex := Integer(FStatus);
      FTreeNode.TreeView.Repaint;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizardStep.UpdateStatus;
const OPNAME = 'TWizardStep.UpdateStatus';
var
  lIndex   : integer;
  lStep    : TWizardStep;
  lDone    : Boolean;
  lNotDone : Boolean;
begin
  try
    if (FSubSteps.StepList.Count > 0) then
    begin
      lDone    := TRUE;
      lNotDone := TRUE;
      for lIndex := 0 to FSubSteps.StepList.Count - 1 do
      begin
        lStep := TWizardStep(FSubSteps.StepList.Items[lIndex]);
        lStep.UpdateStatus;
        if (lStep.Status <> wzsDone) then
          lDone := FALSE;
        if (lStep.Status <> wzsNotDone) then
          lNotDone := FALSE;
      end;
      if (lDone) then
        FStatus := wzsDone
      else if (lNotDone) then
        FStatus := wzsNotDone
      else
        FStatus := wzsIncomplete;
    end
    else
      FStatus := TWizardStepStatus(Self.Validator.DetermineWizardStatus(FSequence));
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizardStep.IsStepOK : Boolean;
const OPNAME = 'TWizardStep.IsStepOK';
var
  lIndex   : integer;
  lStep    : TWizardStep;
  lRun     : Boolean;
begin
  Result := FALSE;
  try
    lRun := TRUE;
    if (FSubSteps.StepList.Count > 0) then
    begin
      lIndex := 0;
      while (lRun AND (lIndex < FSubSteps.FStepList.Count)) do
      begin
        lStep := FSubSteps.StepList.Items[lIndex];
        if (lStep.IsStepOK) then
          lIndex := lIndex + 1
        else
          lRun := FALSE;  
      end;
    end
    else
      lRun := FStatus <> wzsNotDone;
    Result := lRun;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

{******************************************************************************}
{* TWizard                                                                    *}
{******************************************************************************}

procedure TWizard.CreateMemberObjects;
const OPNAME = 'TWizard.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FStepList := TList.Create;
    FOnFinishButtonClick  := nil;
    FSetFinishButtonState := nil;
    FFinishButtonCaption  := FAppModules.Language.GetString('ButtonCaption.Close');
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizard.DestroyMemberObjects;
const OPNAME = 'TWizard.DestroyMemberObjects';
begin
  try
    ClearSteps;
    FreeAndNil(FStepList);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizard.ClearSteps;
const OPNAME = 'TWizard.ClearSteps';
var
  lStep : TWizardStep;
begin
  try
    while (FStepList.Count > 0) do
    begin
      lStep := TWizardStep(FStepList.Items[0]);
      FStepList.Delete(0);
      FreeAndNil(lStep);
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizard.FindStep (ANumber : integer) : TWizardStep;
const OPNAME = 'TWizard.FindStep';
var
  lIndex : integer;
  lStep  : TWizardStep;
begin
  Result := nil;
  try
    lIndex := 0;
    while ((NOT Assigned(Result)) AND (lIndex < FStepList.Count)) do
    begin
      lStep := TWizardStep(FStepList.Items[lIndex]);
      if (lStep.Number = ANumber) then
        Result := lStep
      else if (lStep.SubSteps.FStepList.Count > 0) then
        Result := lStep.SubSteps.FindStep(ANumber);
      if (NOT (Assigned(Result))) then
        lIndex := lIndex + 1;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizard.AddStep (ANumber         : integer;
                          AParentNumber   : integer;
                          AHeading        : string;
                          ADescription    : string;
                          AValidatorClass : TValidatorClass;
                          ASequence       : integer = 0) : TWizardStep;
const OPNAME = 'TWizard.AddStep';
var
  lNewStep    : TWizardStep;
  lParentStep : TWizardStep;
begin
  Result := nil;
  try
    lNewStep := TWizardStep.Create(FAppModules);
    lNewStep.Initialise;
    lNewStep.Number          := ANumber;
    lNewStep.Heading         := AHeading;
    lNewStep.Description     := ADescription;
    lNewStep.FValidatorClass := AValidatorClass;
    lNewStep.Sequence        := ASequence;
    if (AParentNumber = 0) then
    begin
      InsertStep(lNewStep);
      lNewStep.Siblings := Self;
    end
    else
    begin
      lParentStep := FindStep(AParentNumber);
      if (Assigned(lParentStep)) then
      begin
        lNewStep.ParentStep := lParentStep;
        lParentStep.SubSteps.InsertStep(lNewStep);
        lNewStep.Siblings := lParentStep.SubSteps;
      end;
    end;
    if (Assigned(AValidatorClass)) then
    begin
      lNewStep.FValidator := AValidatorClass.Create(nil, FAppModules);
      lNewStep.FValidator.Panel.Visible := FALSE;
      lNewStep.FValidator.OnValueChange := lNewStep.DoValueHasChanged;
    end;
    Result := lNewStep;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TWizard.InsertStep (AStep : TWizardStep);
const OPNAME = 'TWizard.InsertStep';
var
  lIndex : integer;
  lFound : Boolean;
  lStep  : TWizardStep;
begin
  try
    lIndex := 0;
    lFound := FALSE;
    while ((NOT lFound) AND (lIndex < FStepList.Count)) do
    begin
      lStep := TWizardStep(FStepList.Items[lIndex]);
      if (lStep.Number > AStep.Number) then
        lFound := TRUE
      else
        lIndex := lIndex + 1;
    end;
    FStepList.Insert(lIndex, AStep);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizard.FirstLeafStep : TWizardStep;
const OPNAME = 'TWizard.FirstLeafStep';
var
  lStep : TWizardStep;
begin
  Result := nil;
  try
    if (FStepList.Count > 0) then
    begin
      lStep := TWizardStep(FStepList.Items[0]);
      if (lStep.SubSteps.FStepList.Count > 0) then
        Result := lStep.SubSteps.FirstLeafStep
      else
        Result := lStep;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TWizard.LastLeafStep : TWizardStep;
const OPNAME = 'TWizard.LastLeafStep';
var
  lStep : TWizardStep;
begin
  Result := nil;
  try
    if (FStepList.Count > 0) then
    begin
      lStep := TWizardStep(FStepList.Items[FStepList.Count - 1]);
      if (lStep.SubSteps.FStepList.Count > 0) then
        Result := lStep.SubSteps.LastLeafStep
      else
        Result := lStep;
    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

end.
