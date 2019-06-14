//
//
//  UNIT      : Contains the class TOutputReviewWaterBalanceDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputReviewWaterBalanceDialog;

interface

uses
  Classes,
  Controls,
  ComCtrls,
  ExtCtrls,
  StdCtrls,
  Buttons,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TOutputReviewWaterBalanceDialog = class(TAbstractScrollablePanel)
  protected
    FLoadCaseNavigator : TLoadCaseNavigator;
    FMainViewer        : TWaterBalanceDataViewer;
    FDetailViewer      : TWaterBalanceDataViewer;
    FbtnDetail         : TButton;
    FPanel             : TPanel;
    FShowDetail        : boolean;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure SetViewObjectType(AViewObjectType:TViewObjectType); override;
    procedure OnbtnDetailClck(Sender: TObject);
  public
    procedure Resize; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property LoadCaseNavigator : TLoadCaseNavigator      read FLoadCaseNavigator;
    property MainViewer        : TWaterBalanceDataViewer read FMainViewer;
    property DetailViewer      : TWaterBalanceDataViewer read FDetailViewer;
    property ShowDetail        : boolean                 read FShowDetail;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

const
  C_ControlBorder  = 5;
  C_LabelOffset    = 3;
  C_GroupBoxOffset = 5;


{ TOutputReviewWaterBalanceDialog }

procedure TOutputReviewWaterBalanceDialog.CreateMemberObjects;
const OPNAME = 'TOutputReviewWaterBalanceDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FViewObjectType             := votNone;

    FPanel                      := TPanel.Create(Self);
    FPanel.Parent               := Self;
    FPanel.BevelOuter           := bvNone;

    FLoadCaseNavigator          := TLoadCaseNavigator.Create(Self, FAppModules);
    FLoadCaseNavigator.Parent   := FPanel;

    FbtnDetail                  := TButton.Create(Self);
    FbtnDetail.Parent           := FPanel;
    FbtnDetail.OnClick          := OnbtnDetailClck;
    
    FMainViewer                 := TWaterBalanceDataViewer.Create(Self, FAppModules);
    FMainViewer.Parent          := Self;
    FMainViewer.Visible         := False;

    FDetailViewer               := TWaterBalanceDataViewer.Create(Self, FAppModules);
    FDetailViewer.Parent        := Self;
    FDetailViewer.Visible       := False;
    FShowDetail                 := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputReviewWaterBalanceDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputReviewWaterBalanceDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FLoadCaseNavigator.StudyHasChanged;
    FMainViewer.StudyHasChanged;
    FDetailViewer.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputReviewWaterBalanceDialog.Initialise: boolean;
const OPNAME = 'TOutputReviewWaterBalanceDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FLoadCaseNavigator.Initialise;
    FMainViewer.Initialise;
    FDetailViewer.Initialise;
    FMainViewer.Visible := False;
    FDetailViewer.Visible := False;
    FDetailViewer.Visible := False;
    FShowDetail           := False;
    FbtnDetail.Caption    := FAppModules.Language.GetString('ButtonCaption.MoreDetails');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputReviewWaterBalanceDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputReviewWaterBalanceDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FLoadCaseNavigator.LanguageHasChanged;
    FMainViewer.LanguageHasChanged;
    FDetailViewer.LanguageHasChanged;
    FbtnDetail.Caption := FAppModules.Language.GetString('ButtonCaption.MoreDetails');
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputReviewWaterBalanceDialog.AssignHelpContext;
const OPNAME = 'TOutputReviewWaterBalanceDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputReviewWaterBalanceDialog.Resize;
const OPNAME = 'TOutputReviewWaterBalanceDialog.Resize';
begin
  inherited Resize;
  try
    FPanel.ClientHeight  := 40;
    FPanel.Align := alTop;
    FbtnDetail.Align := alRight;
    FLoadCaseNavigator.Align := alClient;

    FMainViewer.Top   := FPanel.Top + FPanel.Height + 20;
    FDetailViewer.Top := FMainViewer.Top + FMainViewer.Height + 20;

    FMainViewer.Width := Self.ClientWidth - 40;
    FDetailViewer.Width := Self.ClientWidth - 40;

    FMainViewer.Left    := (Self.Width - FMainViewer.Width) div 2;
    FDetailViewer.Left  := (Self.Width - FDetailViewer.Width) div 2;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceDialog.SetViewObjectType(AViewObjectType:TViewObjectType);
const OPNAME = 'TOutputReviewWaterBalanceDialog.SetViewObjectType';
begin
  try
    FViewObjectType       := AViewObjectType;
    FMainViewer.Visible   := False;
    FDetailViewer.Visible := False;
    FbtnDetail.Visible    := (AViewObjectType = votReservoir);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputReviewWaterBalanceDialog.OnbtnDetailClck(Sender: TObject);
const OPNAME = 'TOutputReviewWaterBalanceDialog.OnbtnDetailClck';
begin
  try
    if(FViewObjectType = votReservoir) then
    begin
      FShowDetail := not FShowDetail;
      FDetailViewer.Visible := FShowDetail;
      if FDetailViewer.Visible then
        FbtnDetail.Caption := FAppModules.Language.GetString('ButtonCaption.Details');
      else
        FbtnDetail.Caption := FAppModules.Language.GetString('ButtonCaption.MoreDetails');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
