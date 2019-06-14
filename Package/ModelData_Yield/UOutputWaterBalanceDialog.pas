//
//
//  UNIT      : Contains the class TOutputWaterBalanceDialog.
//  AUTHOR    : Dziedzi Ramulondi (ARIVIA)
//  DATE      : 2005/05/03
//  COPYRIGHT : Copyright © 2005 DWAF
//
//

unit UOutputWaterBalanceDialog;

interface

uses
  Classes,
  Windows,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.ExtCtrls,
  VCL.StdCtrls,
  VCL.Buttons,
  VCL.CheckLst,
  VCL.Forms,
  VoaimsCom_TLB,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

type

  TOutputWaterBalanceDialog = class(TAbstractScrollablePanel)
  protected
    FSelectorPanel     : TPanel;
    FbtnDetail         : TFieldButton;
    FBtnDataSelection  : TFieldButton;

    FMainViewer        : TWaterBalanceDataViewer;
    FDetailViewer      : TWaterBalanceDataViewer;
    FShowDetail        : boolean;
    procedure CreateMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure SetNetworkElementType(ANetworkElementType:TNetworkElementType); override;
    procedure OnbtnDetailClck(Sender: TObject);
  public
    procedure Resize; override;
    function StudyHasChanged: boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    property btnDetail          : TFieldButton            read FbtnDetail;
    property MainViewer         : TWaterBalanceDataViewer read FMainViewer;
    property DetailViewer       : TWaterBalanceDataViewer read FDetailViewer;
    property ShowDetail         : boolean                 read FShowDetail;
    property BtnDataSelection   : TFieldButton            read FBtnDataSelection;
  end;

  implementation

uses
  SysUtils,
  UHelpContexts,
  UControlCreationUtilities,
  UErrorHandlingOperations;

{ TOutputWaterBalanceDialog }

procedure TOutputWaterBalanceDialog.CreateMemberObjects;
const OPNAME = 'TOutputWaterBalanceDialog.CreateMemberObjects';
begin
  inherited CreateMemberObjects;
  try
    FSelectorPanel              := TPanel.Create(ControlsOwner);
    FSelectorPanel.Parent       := ControlsParent;

    FBtnDataSelection           := TFieldButton.Create(ControlsOwner, FAppModules,'');
    FBtnDataSelection.Parent    := FSelectorPanel;

    FbtnDetail                  := TFieldButton.Create(Self, FAppModules,'WaterBalanceDetails');
    FbtnDetail.Parent           := FSelectorPanel;
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

function TOutputWaterBalanceDialog.StudyHasChanged: boolean;
const OPNAME = 'TOutputWaterBalanceDialog.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
    FMainViewer.StudyHasChanged;
    FDetailViewer.StudyHasChanged;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputWaterBalanceDialog.Initialise: boolean;
const OPNAME = 'TOutputWaterBalanceDialog.Initialise';
begin
  Result := inherited Initialise;
  try
    FMainViewer.Initialise;
    FDetailViewer.Initialise;
    FMainViewer.Visible := False;
    FDetailViewer.Visible := False;
    FDetailViewer.Visible := False;
    FShowDetail           := False;
    FbtnDetail.Caption    := FAppModules.Language.GetString('ButtonCaption.MoreDetails');
    FSelectorPanel.BorderStyle := bsSingle;
    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TOutputWaterBalanceDialog.LanguageHasChanged: boolean;
const OPNAME = 'TOutputWaterBalanceDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    FMainViewer.LanguageHasChanged;
    FDetailViewer.LanguageHasChanged;
    FbtnDetail.Caption := FAppModules.Language.GetString('ButtonCaption.MoreDetails');
    FBtnDataSelection.Caption := FAppModules.Language.GetString('LabelText.SelectData');

    Result := True;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputWaterBalanceDialog.AssignHelpContext;
const OPNAME = 'TOutputWaterBalanceDialog.AssignHelpContext';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TOutputWaterBalanceDialog.Resize;
const OPNAME = 'TOutputWaterBalanceDialog.Resize';
begin
  try
    LockWindowUpdate(Self.Handle);
    try
      FSelectorPanel.Align        := alTop;
      FSelectorPanel.ClientHeight := 30;

      FBtnDataSelection.Align     := alRight;
      FBtnDataSelection.Width     := 80;

      FbtnDetail.Width    := 80;
      FbtnDetail.Align    := alLeft;

      FMainViewer.Top   := FSelectorPanel.Top + FSelectorPanel.Height + 20;
      FDetailViewer.Top := FMainViewer.Top + FMainViewer.Height + 20;

      FMainViewer.Width := Self.ClientWidth - 40;
      FDetailViewer.Width := Self.ClientWidth - 40;

      FMainViewer.Left    := (Self.Width - FMainViewer.Width) div 2;
      FDetailViewer.Left  := (Self.Width - FDetailViewer.Width) div 2;
    finally
      LockWindowUpdate(0);
    end;
    inherited;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceDialog.SetNetworkElementType(ANetworkElementType:TNetworkElementType);
const OPNAME = 'TOutputWaterBalanceDialog.SetNetworkElementType';
begin
  inherited;
  try
    FMainViewer.Visible     := False;
    FDetailViewer.Visible   := False;
    FbtnDetail.Enabled      := (ANetworkElementType in [votReservoir,votNodeWithInflow,votNodeWithoutInflow]);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputWaterBalanceDialog.OnbtnDetailClck(Sender: TObject);
const OPNAME = 'TOutputWaterBalanceDialog.OnbtnDetailClck';
begin
  try
    if(FNetworkElementType = votReservoir) then
    begin
      FShowDetail := not FShowDetail;
      FDetailViewer.Visible := FShowDetail;
      if FDetailViewer.Visible then
        FbtnDetail.Caption := FAppModules.Language.GetString('ButtonCaption.Details')
      else
        FbtnDetail.Caption := FAppModules.Language.GetString('ButtonCaption.MoreDetails');
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
