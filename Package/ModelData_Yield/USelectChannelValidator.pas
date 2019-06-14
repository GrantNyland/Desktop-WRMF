{******************************************************************************}
{*  UNIT      : Contains the class TSelectChannelvalidator.                   *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/05                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit USelectChannelValidator;

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
  USelectChannelDialog,
  UAbstractYieldDataDialogValidator,
  UYieldContextValidationType;

type
  TSelectChannelValidator = class(TAbstractYieldDataDialogValidator)
  protected
    {protected}
    FChannelNumber : integer;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;

    procedure RePopulateDataViewer;
    procedure RepopulateChannelListBox;
  public
    function Initialise: boolean; override;
    function RestoreState : boolean;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;

    function SelectChannelDialog : TSelectChannelDialog;
    property ChannelNumber: integer read FChannelNumber write FChannelNumber;

  end;

implementation

uses
  UConstants,
  Contnrs,
  SysUtils,
  VCL.Graphics,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{******************************************************************************}
{* TSelectChannelValidator                                                    *}
{******************************************************************************}

procedure TSelectChannelValidator.CreateMemberObjects;
const OPNAME = 'TSelectChannelValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FChannelNumber := NullInteger;
    FPanel         := TSelectChannelDialog.Create(FPanelOwner,FAppModules);
    RestoreState;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelValidator.DestroyMemberObjects;
const OPNAME = 'TSelectChannelValidator.DestroyMemberObjects';
begin
  try
  except on E: Exception do HandleError(E, OPNAME) end;
  inherited DestroyMemberObjects;
end;

function TSelectChannelValidator.Initialise: boolean;
const OPNAME = 'TSelectChannelValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectChannelValidator.LanguageHasChanged: boolean;
const OPNAME = 'TSelectChannelValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := 'Penalty';
    Result := inherited LanguageHasChanged;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelValidator.ClearDataViewer;
const OPNAME = 'TSelectChannelValidator.ClearDataViewer';
begin
  inherited ClearDataViewer;
  try
    SelectChannelDialog.ChannelsListBox.Items.Clear;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelValidator.PopulateDataViewer;
const OPNAME = 'TSelectChannelValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RePopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelValidator.RePopulateDataViewer;
const OPNAME = 'TSelectChannelValidator.RePopulateDataViewer';
begin
  try
    RepopulateChannelListBox;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TSelectChannelValidator.RepopulateChannelListBox;
const OPNAME = 'TSelectChannelValidator.RepopulateChannelListBox';
var
  lChannelList      : IChannelList;
  lIndexA           : integer;
  lChannel          : IGeneralFlowChannel;
  lIndexB           : integer;
begin
  try
    try
      lChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ChannelList;
      with SelectChannelDialog do
      begin
        for lIndexA := 0 to lChannelList.ChannelCount - 1 do
        begin
          try
            lChannel := lChannelList.ChannelByIndex[lIndexA];
            if (NOT (lChannel.ChannelType in [3,4])) then
            begin
              ChannelsListBox.Items.AddObject
                ('(' + IntToStr(lChannel.ChannelNumber) + ') ' + lChannel.ChannelName,
                TObject(lChannel.ChannelNumber));
            end;
          finally
            lChannel := nil;
          end;
        end;
        if (FChannelNumber <> 0) then
        begin
          lIndexB := -1;
          lIndexA := 0;
          while ((lIndexB = -1) AND (lIndexA < lChannelList.ChannelCount)) do
          begin
            try
              lChannel := lChannelList.ChannelByIndex[lIndexA];
              if (lChannel.ChannelNumber = FChannelNumber) then
                lIndexB := lIndexA;
            finally
              lChannel := nil;
            end;
            lIndexA := lIndexA + 1;
          end;
          ChannelsListBox.ItemIndex := lIndexB;
        end;
      end;
    finally
      lChannelList := nil;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectChannelValidator.SelectChannelDialog : TSelectChannelDialog;
const OPNAME = 'TSelectChannelValidator.SelectChannelDialog';
begin
  Result := Nil;
  try
    if Assigned(FPanel) then
      Result := TSelectChannelDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectChannelValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean;
const OPNAME = 'TSelectChannelValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectChannelValidator.StudyHasChanged: boolean;
const OPNAME = 'TSelectChannelValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectChannelValidator.RestoreState: boolean;
const OPNAME = 'TSelectChannelValidator.RestoreState';
begin
  Result := False;
  try
    Result := False;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TSelectChannelValidator.SaveState: boolean;
const OPNAME = 'TSelectChannelValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

