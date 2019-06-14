//
//
//  UNIT      : Contains the class TYieldMetaDataDialog.
//  AUTHOR    : Dziedzi Ramulondi
//  DATE      : 2005/04/04
//  COPYRIGHT : Copyright © 2005 DWAF
//
//
unit UYieldMetaDataDialog;

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

  TYieldMetaDataDialog = class(TAbstractScrollablePanel)
  protected
    btnHydrologyFiles               : TFieldButton;
    btnStudyResults                 : TFieldButton;
    btnStudyReports                 : TFieldButton;
    btnSenarioStrategy              : TFieldButton;
    btnStudyStakeholders            : TFieldButton;
    btnOverallOperatingRulestrategy : TFieldButton;
    btnStudy                        : TFieldButton;
    btnProposedInfrastructure       : TFieldButton;
    btnDemandProjection             : TFieldButton;
    btnDevOptionSequences           : TFieldButton;
    btnOperatingRuleStrategy        : TFieldButton;
    btnStudyErrorMetaData           : TFieldButton;
    FGroupBox                       : TGroupBox;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
  public
    procedure Resize; override;
    function LanguageHasChanged: boolean; override;

    property GroupBox : TGroupBox read FGroupBox;
    property HydrologyFilesBtn               : TFieldButton read btnHydrologyFiles;
    property StudyResultsBtn                 : TFieldButton read btnStudyResults;
    property StudyReportsBtn                 : TFieldButton read btnStudyReports;
    property SenarioStrategyBtn              : TFieldButton read btnSenarioStrategy;
    property StudyStakeholdersBtn            : TFieldButton read btnStudyStakeholders;
    property OverallOperatingRulestrategyBtn : TFieldButton read btnOverallOperatingRulestrategy;
    property StudyBtn                        : TFieldButton read btnStudy;
    property ProposedInfrastructureBtn       : TFieldButton read btnProposedInfrastructure;
    property DemandProjectionBtn             : TFieldButton read btnDemandProjection;
    property DevOptionSequencesBtn           : TFieldButton read btnDevOptionSequences;
    property OperatingRuleStrategyBtn        : TFieldButton read btnOperatingRuleStrategy;
    property StudyErrorMetaDataBtn           : TFieldButton read btnStudyErrorMetaData;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{ TYieldMetaDataDialog }


procedure TYieldMetaDataDialog.AssignHelpContext;
const OPNAME = 'TYieldMetaDataDialog.AssignHelpContext';
begin
  try
    SetControlHelpContext(btnDemandProjection,              HC_RunDescription);
    SetControlHelpContext(btnDevOptionSequences,            HC_RunDescription);
    SetControlHelpContext(btnHydrologyFiles,                HC_RunDescription);
    SetControlHelpContext(btnOperatingRuleStrategy,         HC_RunDescription);
    SetControlHelpContext(btnOverallOperatingRulestrategy,  HC_RunDescription);
    SetControlHelpContext(btnProposedInfrastructure,        HC_RunDescription);
    SetControlHelpContext(btnSenarioStrategy,               HC_RunDescription);
    SetControlHelpContext(btnStudy,                         HC_RunDescription);
    SetControlHelpContext(btnStudyReports,                  HC_RunDescription);
    SetControlHelpContext(btnStudyResults,                  HC_RunDescription);
    SetControlHelpContext(btnStudyStakeholders,             HC_RunDescription);
    SetControlHelpContext(btnStudyErrorMetaData,            HC_RunDescription);
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TYieldMetaDataDialog.CreateMemberObjects;
const OPNAME = 'TYieldMetaDataDialog.CreateMemberObjects';
begin
  inherited;
  try
    btnHydrologyFiles                           := TFieldButton.Create ( ControlsOwner, FAppModules, 'HydrologyFiles' );
    btnStudyResults                             := TFieldButton.Create ( ControlsOwner, FAppModules, 'StudyResults' );
    btnStudyReports                             := TFieldButton.Create ( ControlsOwner, FAppModules, 'StudyReports' );
    btnSenarioStrategy                          := TFieldButton.Create ( ControlsOwner, FAppModules, 'SenarioStrategy' );
    btnStudyStakeholders                        := TFieldButton.Create ( ControlsOwner, FAppModules, 'StudyStakeholders' );
    btnOverallOperatingRulestrategy             := TFieldButton.Create ( ControlsOwner, FAppModules, 'OverallOperatingRulestrategy' );
    btnStudy                                    := TFieldButton.Create ( ControlsOwner, FAppModules, 'Study' );
    btnProposedInfrastructure                   := TFieldButton.Create ( ControlsOwner, FAppModules, 'ProposedInfrastructure' );
    btnDemandProjection                         := TFieldButton.Create ( ControlsOwner, FAppModules, 'DemandProjection' );
    btnDevOptionSequences                       := TFieldButton.Create ( ControlsOwner, FAppModules, 'DevOptionSequences' );
    btnOperatingRuleStrategy                    := TFieldButton.Create ( ControlsOwner, FAppModules, 'OperatingRuleStrategy' );
    btnStudyErrorMetaData                       := TFieldButton.Create ( ControlsOwner, FAppModules, 'StudyErrorMetaData' );

    FGroupBox                                   := TGroupBox.Create ( ControlsOwner );

    FGroupBox.Parent                            := ControlsParent;

    btnHydrologyFiles.Parent                    := FGroupBox;
    btnHydrologyFiles.Top                       := 20;
    btnHydrologyFiles.Left                      := 20;
    btnHydrologyFiles.Width                     := 200;
    btnHydrologyFiles.ShowHint                  := True;

    btnStudyResults.Parent                      := FGroupBox;
    btnStudyResults.Top                         := btnHydrologyFiles.Top + btnHydrologyFiles.Height + 10;
    btnStudyResults.Left                        := btnHydrologyFiles.Left;
    btnStudyResults.Width                       := btnHydrologyFiles.Width;
    btnStudyResults.ShowHint                  := True;

    btnStudyReports.Parent                      := FGroupBox;
    btnStudyReports.Top                         := btnStudyResults.Top + btnStudyResults.Height + 10;
    btnStudyReports.Left                        := btnStudyResults.Left;
    btnStudyReports.Width                       := btnStudyResults.Width;
    btnStudyReports.ShowHint                    := True;

    btnSenarioStrategy.Parent                   := FGroupBox;
    btnSenarioStrategy.Top                      := btnStudyReports.Top + btnHydrologyFiles.Height + 10;
    btnSenarioStrategy.Left                     := btnStudyReports.Left;
    btnSenarioStrategy.Width                    := btnStudyReports.Width;
    btnSenarioStrategy.ShowHint                 := True;

    btnStudyStakeholders.Parent                 := FGroupBox;
    btnStudyStakeholders.Top                    := btnSenarioStrategy.Top + btnHydrologyFiles.Height + 10;
    btnStudyStakeholders.Left                   := btnSenarioStrategy.Left;
    btnStudyStakeholders.Width                  := btnSenarioStrategy.Width;
    btnStudyStakeholders.ShowHint               := True;

    btnOverallOperatingRulestrategy.Parent      := FGroupBox;
    btnOverallOperatingRulestrategy.Top         := btnStudyStakeholders.Top + btnHydrologyFiles.Height + 10;
    btnOverallOperatingRulestrategy.Left        := btnStudyStakeholders.Left;
    btnOverallOperatingRulestrategy.Width       := btnStudyStakeholders.Width;
    btnOverallOperatingRulestrategy.ShowHint    := True;
//....n/
    btnStudy.Parent                             := FGroupBox;
    btnStudy.Top                                := btnHydrologyFiles.Top;
    btnStudy.Left                               := btnHydrologyFiles.Left + btnHydrologyFiles.Width + 20;
    btnStudy.Width                              := btnHydrologyFiles.Width;
    btnStudy.ShowHint                           := True;

    btnProposedInfrastructure.Parent            := FGroupBox;
    btnProposedInfrastructure.Top               := btnStudy.Top + btnHydrologyFiles.Height + 10;
    btnProposedInfrastructure.Left              := btnStudy.Left;
    btnProposedInfrastructure.Width             := btnStudy.Width;
    btnProposedInfrastructure.ShowHint          := True;

    btnDemandProjection.Parent                  := FGroupBox;
    btnDemandProjection.Top                     := btnProposedInfrastructure.Top + btnHydrologyFiles.Height + 10;
    btnDemandProjection.Left                    := btnProposedInfrastructure.Left;
    btnDemandProjection.Width                   := btnProposedInfrastructure.Width;
    btnDemandProjection.ShowHint                := True;

    btnDevOptionSequences.Parent                := FGroupBox;
    btnDevOptionSequences.Top                   := btnDemandProjection.Top + btnHydrologyFiles.Height + 10;
    btnDevOptionSequences.Left                  := btnDemandProjection.Left;
    btnDevOptionSequences.Width                 := btnDemandProjection.Width;
    btnDevOptionSequences.ShowHint              := True;

    btnOperatingRuleStrategy.Parent             := FGroupBox;
    btnOperatingRuleStrategy.Top                := btnDevOptionSequences.Top + btnHydrologyFiles.Height + 10;
    btnOperatingRuleStrategy.Left               := btnDevOptionSequences.Left;
    btnOperatingRuleStrategy.Width              := btnDevOptionSequences.Width;
    btnOperatingRuleStrategy.ShowHint           := True;

    btnStudyErrorMetaData.Parent                := FGroupBox;
    btnStudyErrorMetaData.Top                   := btnOperatingRuleStrategy.Top + btnHydrologyFiles.Height + 10;
    btnStudyErrorMetaData.Left                  := btnOperatingRuleStrategy.Left;
    btnStudyErrorMetaData.Width                 := btnOperatingRuleStrategy.Width;
    btnStudyErrorMetaData.ShowHint              := True;
    
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TYieldMetaDataDialog.LanguageHasChanged : boolean;
const OPNAME = 'TYieldMetaDataDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    btnHydrologyFiles.Caption               := FAppModules.Language.GetString('ButtonCaption.HydrologyFiles');
    btnStudyResults.Caption                 := FAppModules.Language.GetString('ButtonCaption.StudyResults');
    btnStudyReports.Caption                 := FAppModules.Language.GetString('ButtonCaption.StudyReports');
    btnSenarioStrategy.Caption              := FAppModules.Language.GetString('ButtonCaption.SenarioStrategy');
    btnStudyStakeholders.Caption            := FAppModules.Language.GetString('ButtonCaption.StudyStakeholders');
    btnOverallOperatingRulestrategy.Caption := FAppModules.Language.GetString('ButtonCaption.OverallOperatingRulestrategy');
    btnStudy.Caption                        := FAppModules.Language.GetString('ButtonCaption.Study');
    btnProposedInfrastructure.Caption       := FAppModules.Language.GetString('ButtonCaption.ProposedInfrastructure');
    btnDemandProjection.Caption             := FAppModules.Language.GetString('ButtonCaption.DemandProjection');
    btnDevOptionSequences.Caption           := FAppModules.Language.GetString('ButtonCaption.DevOptionSequences');
    btnOperatingRuleStrategy.Caption        := FAppModules.Language.GetString('ButtonCaption.OperatingRuleStrategy');
    btnStudyErrorMetaData.Caption           := FAppModules.Language.GetString('ButtonCaption.StudyErrorMetaData');
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TYieldMetaDataDialog.Resize;
const OPNAME = 'TYieldMetaDataDialog.Resize';
begin
  inherited;
  try
  FGroupBox.Align := alClient;
  FGroupBox.Caption := '';
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;
end.
