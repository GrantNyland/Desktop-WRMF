//
//
//  UNIT      : Contains TOutputComparisonFileSelectionValidator Class
//  AUTHOR    : Sam Dhlamini(Cornastone)
//  DATE      : 22/01/2004
//  COPYRIGHT : Copyright © 2004 DWAF
//
//

unit UOutputComparisonFileSelectionValidator;

interface

uses
  Classes,
  VCL.Controls,
  VCL.ComCtrls,
  VCL.StdCtrls,
  VCL.Dialogs,
  VCL.ExtCtrls,
  VCLTee.Series,
  VCL.Grids,
  Types,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UYieldContextValidationType,
  VoaimsCom_TLB,
  UAbstractYieldDataDialogValidator,
  UOutputComparisonData,
  UOutputComparisonLoadAgent,
  UOutputComparisonFileSelectionDialog;

type
  TOutputComparisonFileSelectionValidator = class(TAbstractYieldDataDialogValidator)
  protected
    FSelectedFiles : TStringList;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure OnEditControlEnter(Sender: TObject); override;
    procedure OnEditControltExit(Sender: TObject); override;
    procedure OnStringGridCellDataHasChanged (ASender: TObject; ACol, ARow: integer); override;
    procedure RePopulateDataViewer;
    procedure RePopulateReservoirs;
    procedure RePopulateChannels;
    procedure RepopulateFiles;
    function RePopulateAverageVolume(AData : TStrings): string;
    procedure DoOnSearchFirstSumOutClick(Sender : TObject);
    procedure DoOnSearchSecondSumOutClick(Sender : TObject);
    procedure DobtnRefreshSumOutClick(Sender : TObject);
    procedure DobtnResetClick(Sender : TObject);
    function DoFileSearch : string;
    procedure CheckReservoirs;
    procedure CheckChannels;

    procedure UpdateFirstReservoirs;
    procedure UpdateSecondReservoirs;

    procedure UpdateFirstChannels;
    procedure UpdateSecondChannels;

  public
    function Initialise: boolean; override;
    function SaveState: boolean; override;
    function LanguageHasChanged: boolean; override;
    function StudyHasChanged: boolean; override;
    function StudyDataHasChanged(AContext: TChangeContext; AFieldName,AOldValue,ANewValue: string): boolean; override;

    procedure DoDrawGrid(Sender: TObject;ACol, ARow : integer; Rect : TRect;State : TGridDrawState);
    procedure DoFirstReservoirsDrawGrid(Sender: TObject;ACol, ARow : integer; Rect : TRect;State : TGridDrawState);
    procedure DoSecondReservoirsDrawGrid(Sender: TObject;ACol, ARow : integer; Rect : TRect;State : TGridDrawState);
    procedure DoFirstChannelDrawGrid(Sender: TObject;ACol, ARow : integer; Rect : TRect;State : TGridDrawState);
    procedure DoSecondChannelDrawGrid(Sender: TObject;ACol, ARow : integer; Rect : TRect;State : TGridDrawState);

    procedure ClearDataViewer; override;
    procedure PopulateDataViewer; override;
    procedure DoContextValidation(AValidationType : TDialogValidationType); override;
    function OutputComparisonFileSelectionDialog : TOutputComparisonFileSelectionDialog;
  end;

implementation

uses
  System.UITypes,
  Math,
  SysUtils,
  VCL.Graphics,
  ContNrs,
  UConstants,
  UUtilities,
  UYieldModelDataGUIForm,
  USumOutFileManager,
  UYieldModelDataObject,
  UErrorHandlingOperations;

{ TOutputComparisonFileSelectionValidator }

procedure TOutputComparisonFileSelectionValidator.CreateMemberObjects;
const OPNAME = 'TOutputComparisonFileSelectionValidator.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
    FSelectedFiles := TStringList.Create;
    FPanel := TOutputComparisonFileSelectionDialog.Create(FPanelOwner,FAppModules);

    OutputComparisonFileSelectionDialog.btnSearchFirstSumOut.OnClick := DoOnSearchFirstSumOutClick;
    OutputComparisonFileSelectionDialog.btnSearchSecondSumOut.OnClick := DoOnSearchSecondSumOutClick;

    OutputComparisonFileSelectionDialog.btnResetSumOut.OnClick := DobtnResetClick;
    OutputComparisonFileSelectionDialog.btnRefreshSecondSumOut.OnClick := DobtnRefreshSumOutClick;

    OutputComparisonFileSelectionDialog.strgFirstReservoirs.
    AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirZoneName'));
    OutputComparisonFileSelectionDialog.strgFirstReservoirs.
    AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirZoneName'));
    OutputComparisonFileSelectionDialog.strgSecondReservoirs.
    AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirZoneName'));
    OutputComparisonFileSelectionDialog.strgSecondReservoirs.
    AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirZoneName'));
    OutputComparisonFileSelectionDialog.strgFirstChannel.
    AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelName'));
    OutputComparisonFileSelectionDialog.strgFirstChannel.
    AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelName'));
    OutputComparisonFileSelectionDialog.strgSecondChannel.
    AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelName'));
    OutputComparisonFileSelectionDialog.strgSecondChannel.
    AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelName'));

    OutputComparisonFileSelectionDialog.strgFirstReservoirs.OnCheckBoxClick := OnEditControltExit;
    OutputComparisonFileSelectionDialog.strgFirstReservoirs.OnDrawCell := DoFirstReservoirsDrawGrid;
    OutputComparisonFileSelectionDialog.strgSecondReservoirs.OnCheckBoxClick:= OnEditControltExit;
    OutputComparisonFileSelectionDialog.strgSecondReservoirs.OnDrawCell := DoSecondReservoirsDrawGrid;
    OutputComparisonFileSelectionDialog.strgFirstChannel.OnCheckBoxClick := OnEditControltExit;
    OutputComparisonFileSelectionDialog.strgFirstChannel.OnDrawCell := DoFirstChannelDrawGrid;
    OutputComparisonFileSelectionDialog.strgSecondChannel.OnCheckBoxClick := OnEditControltExit;
    OutputComparisonFileSelectionDialog.strgSecondChannel.OnDrawCell := DoSecondChannelDrawGrid;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.DestroyMemberObjects;
const OPNAME = 'TOutputComparisonFileSelectionValidator.DestroyMemberObjects';
begin
  try
    FreeAndNil(FSelectedFiles);
    inherited DestroyMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.DoContextValidation(AValidationType: TDialogValidationType);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoContextValidation';
begin
  try

  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonFileSelectionValidator.Initialise: boolean;
const OPNAME = 'TOutputComparisonFileSelectionValidator.Initialise';
begin
  Result := inherited Initialise;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonFileSelectionValidator.LanguageHasChanged: boolean;
const OPNAME = 'TOutputComparisonFileSelectionValidator.LanguageHasChanged';
begin
  Result := False;
  try
    TabShetCaption := FAppModules.Language.GetString('TabCaption.SelectFiles');
    Result := inherited LanguageHasChanged;
    //RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.ClearDataViewer;
const OPNAME = 'TOutputComparisonFileSelectionValidator.ClearDataViewer';
var
  LRowIndex,
  LColIndex : integer;
begin
  inherited ClearDataViewer;
  try
    for LRowIndex := 1 to OutputComparisonFileSelectionDialog.strgFirstReservoirs.RowCount-1 do
    begin
      for LColIndex := 0 to OutputComparisonFileSelectionDialog.strgFirstReservoirs.ColCount-1 do
      begin
        OutputComparisonFileSelectionDialog.strgFirstReservoirs.Cells[LColIndex,LRowIndex] := '';
        if OutputComparisonFileSelectionDialog.strgFirstReservoirs.Objects[LColIndex,LRowIndex] <> nil then
          OutputComparisonFileSelectionDialog.strgFirstReservoirs.Objects[LColIndex,LRowIndex] := nil;
      end;
    end;
    for LRowIndex := 1 to OutputComparisonFileSelectionDialog.strgSecondReservoirs.RowCount-1 do
    begin
      for LColIndex := 0 to OutputComparisonFileSelectionDialog.strgSecondReservoirs.ColCount-1 do
      begin
        OutputComparisonFileSelectionDialog.strgSecondReservoirs.Cells[LColIndex,LRowIndex] := '';
        if OutputComparisonFileSelectionDialog.strgSecondReservoirs.Objects[LColIndex,LRowIndex] <> nil then
          OutputComparisonFileSelectionDialog.strgSecondReservoirs.Objects[LColIndex,LRowIndex] := nil;
      end;
    end;
    for LRowIndex := 1 to OutputComparisonFileSelectionDialog.strgFirstChannel.RowCount-1 do
    begin
      for LColIndex := 0 to OutputComparisonFileSelectionDialog.strgFirstChannel.ColCount-1 do
      begin
        OutputComparisonFileSelectionDialog.strgFirstChannel.Cells[LColIndex,LRowIndex] := '';
        if OutputComparisonFileSelectionDialog.strgFirstChannel.Objects[LColIndex,LRowIndex] <> nil then
          OutputComparisonFileSelectionDialog.strgFirstChannel.Objects[LColIndex,LRowIndex] := nil;
      end;
    end;
    for LRowIndex := 1 to OutputComparisonFileSelectionDialog.strgSecondChannel.RowCount-1 do
    begin
      for LColIndex := 0 to OutputComparisonFileSelectionDialog.strgSecondChannel.ColCount-1 do
      begin
        OutputComparisonFileSelectionDialog.strgSecondChannel.Cells[LColIndex,LRowIndex] := '';
        if OutputComparisonFileSelectionDialog.strgSecondChannel.Objects[LColIndex,LRowIndex] <> nil then
          OutputComparisonFileSelectionDialog.strgSecondChannel.Objects[LColIndex,LRowIndex] := nil;
      end;
    end;
    OutputComparisonFileSelectionDialog.strgFirstReservoirs.Refresh;
    OutputComparisonFileSelectionDialog.strgSecondReservoirs.Refresh;
    OutputComparisonFileSelectionDialog.strgFirstChannel.Refresh;
    OutputComparisonFileSelectionDialog.strgSecondChannel.Refresh;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.PopulateDataViewer;
const OPNAME = 'TOutputComparisonFileSelectionValidator.PopulateDataViewer';
begin
  inherited PopulateDataViewer;
  try
    ClearDataViewer;
    RepopulateDataViewer;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.OnEditControlEnter(Sender: TObject);
const OPNAME = 'TOutputComparisonFileSelectionValidator.OnEditControlEnter';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.OnEditControltExit(Sender: TObject);
const OPNAME = 'TOutputComparisonFileSelectionValidator.OnEditControltExit';
begin
  inherited OnEditControltExit(Sender);
  try
    if (Sender = OutputComparisonFileSelectionDialog.strgFirstReservoirs) then
      UpdateFirstReservoirs;
    if (Sender = OutputComparisonFileSelectionDialog.strgSecondReservoirs) then
      UpdateSecondReservoirs;
    if (Sender = OutputComparisonFileSelectionDialog.strgFirstChannel) then
      UpdateFirstChannels;
    if (Sender = OutputComparisonFileSelectionDialog.strgSecondChannel) then
      UpdateSecondChannels;

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.OnStringGridCellDataHasChanged(ASender: TObject; ACol, ARow: integer);
const OPNAME = 'TOutputComparisonFileSelectionValidator.OnStringGridCellDataHasChanged';
begin
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonFileSelectionValidator.OutputComparisonFileSelectionDialog: TOutputComparisonFileSelectionDialog;
const OPNAME = 'TOutputComparisonFileSelectionValidator.OutputComparisonFileSelectionDialog';
begin
  Result := nil;
  try
    if Assigned(FPanel) then
      Result := TOutputComparisonFileSelectionDialog(FPanel);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.RePopulateDataViewer;
const OPNAME = 'TOutputComparisonFileSelectionValidator.RePopulateDataViewer';
begin
  try
    RePopulateReservoirs;
    RePopulateChannels;
    RepopulateFiles;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.RePopulateChannels;
const OPNAME = 'TOutputComparisonFileSelectionValidator.RePopulateChannels';
var
  LIndex         : integer;
  LChannelList   : IChannelList;
  LChannel       : IGeneralFlowChannel;
  LDataContainer : TStringList;
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LCompIndex : integer;
  LErrors : string;
begin
  try
    LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
    LDataContainer := TStringList.Create;
    try
      if (LChannelList <> nil) then
      begin
        OutputComparisonFileSelectionDialog.strgFirstChannel.RowCount  := LChannelList.ChannelCount+1;
        OutputComparisonFileSelectionDialog.strgSecondChannel.RowCount := LChannelList.ChannelCount+1;
        for LIndex := 0 to LChannelList.ChannelCount - 1 do
        begin
          LChannel := LChannelList.ChannelByIndex[LIndex];
          if (LChannel <> nil) then
          begin
            OutputComparisonFileSelectionDialog.strgFirstChannel.Objects[1,LIndex+1] := TObject(LChannel.ChannelNumber);
            OutputComparisonFileSelectionDialog.strgFirstChannel.Cells[1,LIndex+1] := LChannel.ChannelName;
            LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
            for LCompIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
            begin
              if LCompIndex = 0 then
              begin
                LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LCompIndex);
                if LOutputComparisonData <> nil then
                begin
                  if LOutputComparisonData.OutputData.CastSummaryOutputData.GetBlockData(LDataContainer,
                    btMonthlyAverageChannelFlow,LChannel.ChannelNumber,LErrors) then
                  begin
                    OutputComparisonFileSelectionDialog.strgFirstChannel.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelName'));
                    OutputComparisonFileSelectionDialog.strgFirstChannel.Cells[2,LIndex+1] := RePopulateAverageVolume(LDataContainer);
                    Break;
                  end;  
                end;
              end;
            end;
            OutputComparisonFileSelectionDialog.strgSecondChannel.Objects[1,LIndex+1] := TObject(LChannel.ChannelNumber);
            OutputComparisonFileSelectionDialog.strgSecondChannel.Cells[1,LIndex+1] := LChannel.ChannelName;
            for LCompIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
            begin
              if LCompIndex = 1 then
              begin
                LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LCompIndex);
                if LOutputComparisonData <> nil then
                begin
                  if LOutputComparisonData.OutputData.CastSummaryOutputData.GetBlockData(LDataContainer,
                    btMonthlyAverageChannelFlow,LChannel.ChannelNumber,LErrors) then
                  begin
                    OutputComparisonFileSelectionDialog.strgSecondChannel.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ChannelName'));
                    OutputComparisonFileSelectionDialog.strgSecondChannel.Cells[2,LIndex+1] := RePopulateAverageVolume(LDataContainer);
                    Break;
                  end;
                end;
              end;
            end;
          end;
        end;
        CheckChannels;
      end;
    finally
      FreeAndNil(LDataContainer);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputComparisonFileSelectionValidator.RePopulateReservoirs;
const OPNAME = 'TOutputComparisonFileSelectionValidator.RePopulateReservoirs';
var
  LIndex         : integer;
  LReservoirList : IReservoirDataList;
  LReservoirData : IReservoirData;
  LReservoir     : IReservoirConfigurationData;
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LCompIndex : integer;
  LDataContainer : TStringList;
  LErrors : string;
begin
  try
    LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                      NetworkElementData.ReservoirList;
    LDataContainer := TStringList.Create;
    try
      if (LReservoirList <> nil) then
      begin
        OutputComparisonFileSelectionDialog.strgFirstReservoirs.RowCount := LReservoirList.ReservoirCount+1;
        OutputComparisonFileSelectionDialog.strgSecondReservoirs.RowCount := LReservoirList.ReservoirCount+1;
        for LIndex := 0 to LReservoirList.ReservoirCount - 1 do
        begin
          LReservoirData := LReservoirList.ReservoirByIndex[lIndex];
          if LReservoirData <> nil then
          begin
            LReservoir     := LReservoirData.ReservoirConfigurationData;
            if LReservoir <> nil then
            begin
              OutputComparisonFileSelectionDialog.strgFirstReservoirs.Objects[1,LIndex+1] :=
              TObject(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
              OutputComparisonFileSelectionDialog.strgFirstReservoirs.Cells[1,LIndex+1] :=
                '(' + IntToStr(LReservoir.ReservoirIdentifier) + ') ' + LReservoir.ReservoirName;
              LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
              for LCompIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
              begin
                if LCompIndex = 0 then
                begin
                  LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LCompIndex);
                  if LOutputComparisonData <> nil then
                  begin
                    if LOutputComparisonData.OutputData.CastSummaryOutputData.GetBlockData(LDataContainer,
                       btMonthEndReservoirVolume,LReservoirData.ReservoirConfigurationData.ReservoirIdentifier,LErrors) then
                    begin
                      OutputComparisonFileSelectionDialog.strgFirstReservoirs.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirZoneName'));
                      OutputComparisonFileSelectionDialog.strgFirstReservoirs.Cells[2,LIndex+1] := RePopulateAverageVolume(LDataContainer);
                      Break;
                    end;
                  end;
                end;
              end;

              OutputComparisonFileSelectionDialog.strgSecondReservoirs.Objects[1,LIndex+1] :=
              TObject(lReservoirData.ReservoirConfigurationData.ReservoirIdentifier);
              OutputComparisonFileSelectionDialog.strgSecondReservoirs.Cells [1,LIndex+1] :=
                '(' + IntToStr(LReservoir.ReservoirIdentifier) + ') ' + LReservoir.ReservoirName;

              for LCompIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
              begin
                if LCompIndex = 1 then
                begin
                  LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LCompIndex);
                  if LOutputComparisonData <> nil then
                  begin
                    if LOutputComparisonData.OutputData.CastSummaryOutputData.GetBlockData(LDataContainer,
                       btMonthEndReservoirVolume,LReservoirData.ReservoirConfigurationData.ReservoirIdentifier,LErrors) then
                    begin
                      OutputComparisonFileSelectionDialog.strgSecondReservoirs.AddFieldProperty(FAppModules.FieldProperties.FieldProperty('ReservoirZoneName'));
                      OutputComparisonFileSelectionDialog.strgSecondReservoirs.Cells[2,LIndex+1] := RePopulateAverageVolume(LDataContainer);
                      Break;
                    end;
                  end;
                end;
              end;
            end;
          end;
        end;
        CheckReservoirs;
      end;
    finally
      FreeAndNil(LDataContainer);
    end;  
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputComparisonFileSelectionValidator.CheckReservoirs;
const OPNAME = 'TOutputComparisonFileSelectionValidator.CheckReservoirs';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LRow,
  LItemIndex,
  LCount,
  LIndex : integer;
  LSelctedReservoir : TStringList;
  LReservoirNum : integer;
  LReservoir : IReservoirData;
  LReservoirList : IReservoirDataList;
begin
  try
    LSelctedReservoir := TStringList.Create;
    try
      LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).
                        NetworkElementData.ReservoirList;
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
      begin
        LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
        if LOutputComparisonData <> nil then
        begin
          LSelctedReservoir.CommaText :=  LOutputComparisonData.ReservoirList;
          for LCount := 0 to LSelctedReservoir.Count-1 do
          begin
            LReservoirNum := StrToInt(LSelctedReservoir[LCount]);
            LReservoir := LReservoirList.ReservoirOrNodeByIdentifier[LReservoirNum];
            if LReservoir <> nil then
            begin
              LItemIndex := -1;
              for LRow := 1 to OutputComparisonFileSelectionDialog.strgFirstReservoirs.RowCount-1 do
              begin
                LItemIndex := integer(OutputComparisonFileSelectionDialog.strgFirstReservoirs.Objects[1,LRow]);
                if LReservoirNum = LItemIndex then
                begin
                  LItemIndex := LRow;
                  Break;
                end;
              end;
              if  (LItemIndex <> -1) and (LIndex = 0) then
                OutputComparisonFileSelectionDialog.strgFirstReservoirs.CheckedRow[LItemIndex] := True
              else
              if LIndex > 0 then
              begin
                LItemIndex := -1;
                for LRow := 1 to OutputComparisonFileSelectionDialog.strgFirstReservoirs.RowCount-1 do
                begin
                  LItemIndex := integer(OutputComparisonFileSelectionDialog.strgFirstReservoirs.Objects[1,LRow]);
                  if LReservoirNum = LItemIndex then
                  begin
                    LItemIndex := LRow;
                    Break;
                  end;
                end;
                if  (LItemIndex <> -1) and (LIndex > 0) then
                  OutputComparisonFileSelectionDialog.strgSecondReservoirs.CheckedRow[LItemIndex] := True
              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LSelctedReservoir);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.CheckChannels;
const OPNAME = 'TOutputComparisonFileSelectionValidator.CheckChannels';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LRow,
  LItemIndex,
  LCount,
  LIndex : integer;
  LSelctedChannels : TStringList;
  LChannelNum : integer;
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;
begin
  try
    LSelctedChannels := TStringList.Create;
    try
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
      begin
        LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
        if LOutputComparisonData <> nil then
        begin
          LSelctedChannels.CommaText :=  LOutputComparisonData.ChannelList;
          for LCount := 0 to LSelctedChannels.Count-1 do
          begin
            LChannelNum := StrToInt(LSelctedChannels[LCount]);
            LChannel := LChannelList.ChannelByChannelNumber[LChannelNum];
            if LChannel <> nil then
            begin
              if (LIndex = 0) then
              begin
                LItemIndex := -1;
                for LRow := 1 to OutputComparisonFileSelectionDialog.strgFirstChannel.RowCount-1 do
                begin
                  LItemIndex := integer(OutputComparisonFileSelectionDialog.strgFirstChannel.Objects[1,LRow]);
                  if LChannelNum = LItemIndex then
                  begin
                    LItemIndex := LRow;
                    Break;
                  end;
                end;
                if (LItemIndex <> -1) then
                  OutputComparisonFileSelectionDialog.strgFirstChannel.CheckedRow[LItemIndex] := True;
              end
              else
              if LIndex > 0 then
              begin
                LItemIndex := -1;
                for LRow := 1 to OutputComparisonFileSelectionDialog.strgSecondChannel.RowCount-1 do
                begin
                  LItemIndex := integer(OutputComparisonFileSelectionDialog.strgSecondChannel.Objects[1,LRow]);
                  if LChannelNum = LItemIndex then
                  begin
                    LItemIndex := LRow;
                    Break;
                  end;
                end;
                if (LItemIndex <> -1) then
                  OutputComparisonFileSelectionDialog.strgSecondChannel.CheckedRow[LItemIndex] := True;
              end;
            end;
          end;
        end;
      end;
    finally
      FreeAndNil(LSelctedChannels);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.RepopulateFiles;
const OPNAME = 'TOutputComparisonFileSelectionValidator.RepopulateFiles';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LIndex : integer;
begin
  try
    LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
    if LOutputComparisonList <> nil then
    begin
      OutputComparisonFileSelectionDialog.edtFirstSumOut.Text := '';
      OutputComparisonFileSelectionDialog.edtSecondSumOut.Text := '';
      for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
      begin
        LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
        if (LOutputComparisonData <> nil) and (LIndex = 0) then
          OutputComparisonFileSelectionDialog.edtFirstSumOut.Text := LOutputComparisonData.OutputFileName;
        if (LOutputComparisonData <> nil) and (LIndex > 0) then
          OutputComparisonFileSelectionDialog.edtSecondSumOut.Text := LOutputComparisonData.OutputFileName;
      end;
      OutputComparisonFileSelectionDialog.btnSearchSecondSumOut.Enabled :=
      (OutputComparisonFileSelectionDialog.edtFirstSumOut.Text <> '');
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonFileSelectionValidator.RePopulateAverageVolume(AData : TStrings):string;
const OPNAME = 'TOutputComparisonFileSelectionValidator.RePopulateAverageVolume';
var
  LIndex,
  LCount                : integer;
  LMonthlyValue         : double;
  LMonthlyValues        : TStringList;
  LAvarages             : array[0..13] of double;
  LGrandAvarage         : double;
begin
  Result := '';
  try
    if (AData.Count > 0) then
    begin
      for LIndex := Low(LAvarages) to High(LAvarages) do
        LAvarages[LIndex] := 0.0;
      LMonthlyValues := TStringList.Create;
      try
        for LIndex := 0 to AData.Count -1 do
        begin
          LMonthlyValues.CommaText := AData.Strings[LIndex];
          for LCount := 1 to 12 do
          begin
            LMonthlyValue := StrToFloat(LMonthlyValues.Strings[LCount]);
            LAvarages[LCount] := LAvarages[LCount] + (LMonthlyValue * TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.MonthDaysByIndex[LCount]);
          end;
        end;
        LGrandAvarage := 0.00;
        for LIndex := Low(LAvarages)+1 to High(LAvarages) do
        begin
          LAvarages[LIndex] := LAvarages[LIndex] / AData.Count;
          LGrandAvarage := LGrandAvarage+ LAvarages[LIndex];
        end;
         Result := FormatFloat('######0.000',(LGrandAvarage / TYieldModelDataObject(FAppModules.Model.ModelData).CastRunConfigurationData.TotalDaysInAYear));
      finally
        LMonthlyValues.Free;
      end
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonFileSelectionValidator.SaveState: boolean;
const OPNAME = 'TOutputComparisonFileSelectionValidator.SaveState';
begin
  Result := inherited SaveState;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonFileSelectionValidator.StudyDataHasChanged(AContext: TChangeContext; AFieldName, AOldValue,ANewValue: string): boolean;
const OPNAME = 'TOutputComparisonFileSelectionValidator.StudyDataHasChanged';
begin
  Result := inherited StudyDataHasChanged(AContext,AFieldName,AOldValue,ANewValue);
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TOutputComparisonFileSelectionValidator.StudyHasChanged: boolean;
const OPNAME = 'TOutputComparisonFileSelectionValidator.StudyHasChanged';
begin
  Result := inherited StudyHasChanged;
  try

  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.DoOnSearchFirstSumOutClick(Sender: TObject);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoOnSearchFirstSumOutClick';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LFileName : string;
  LSumOutFileManager : TSumOutFileManager;
begin
  try
    LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
    LFileName := DoFileSearch;
    if LFileName <> '' then
    begin
      LOutputComparisonData := LOutputComparisonList.AddOutputComparisonDataFromFile(LFileName);
      if LOutputComparisonData <> nil then
      begin
        OutputComparisonFileSelectionDialog.edtFirstSumOut.Text := LFileName;
        LSumOutFileManager := TSumOutFileManager.Create(FAppModules);
        try
          LSumOutFileManager.PopulateSumOutComparisonData(LFileName,LOutputComparisonData.OutputData,TYieldModelDataObject(FAppModules.Model.ModelData));
          FAppModules.ViewIni.WriteString(ClassName,'SelectedFirstFile'+FAppModules.StudyArea.ScenarioCode,LFileName);
          FAppModules.ViewIni.WriteString(ClassName,'Model'+FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.ModelCode);
          FAppModules.ViewIni.WriteString(ClassName,'StudyAreaName'+FAppModules.StudyArea.StudyAreaCode,FAppModules.StudyArea.StudyAreaCode);
          FAppModules.ViewIni.WriteString(ClassName,'SubArea'+FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.SubAreaCode);
          FAppModules.ViewIni.WriteString(ClassName,'Scenario'+FAppModules.StudyArea.ScenarioCode,FAppModules.StudyArea.ScenarioCode);
        finally
          FreeAndNil(LSumOutFileManager);
        end;
      end;
      DobtnRefreshSumOutClick(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.DoOnSearchSecondSumOutClick(Sender : TObject);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoOnSearchSecondSumOutClick';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LFileName : string;
  LSumOutFileManager : TSumOutFileManager;
begin
  try
    LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
    LFileName := DoFileSearch;
    if LFileName <> '' then
    begin
      LOutputComparisonData := LOutputComparisonList.AddOutputComparisonDataFromFile(LFileName);
      if LOutputComparisonData <> nil then
      begin
        OutputComparisonFileSelectionDialog.edtSecondSumOut.Text := LFileName;
        LSumOutFileManager := TSumOutFileManager.Create(FAppModules);
        try
          LSumOutFileManager.PopulateSumOutComparisonData(LFileName,LOutputComparisonData.OutputData,TYieldModelDataObject(FAppModules.Model.ModelData));
          FAppModules.ViewIni.WriteString(ClassName,'SelectedSecondFile'+FAppModules.StudyArea.ScenarioCode,LFileName);
          FAppModules.ViewIni.WriteString(ClassName,'Model'+FAppModules.StudyArea.ModelCode,FAppModules.StudyArea.ModelCode);
          FAppModules.ViewIni.WriteString(ClassName,'StudyAreaName'+FAppModules.StudyArea.StudyAreaCode,FAppModules.StudyArea.StudyAreaCode);
          FAppModules.ViewIni.WriteString(ClassName,'SubArea'+FAppModules.StudyArea.SubAreaCode,FAppModules.StudyArea.SubAreaCode);
          FAppModules.ViewIni.WriteString(ClassName,'Scenario'+FAppModules.StudyArea.ScenarioCode,FAppModules.StudyArea.ScenarioCode);
        finally
          FreeAndNil(LSumOutFileManager);
        end;
      end;
      DobtnRefreshSumOutClick(nil);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


procedure TOutputComparisonFileSelectionValidator.DobtnRefreshSumOutClick(Sender : TObject);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DobtnRefreshSumOutClick';
var
  LLoadAgent : TOutputComparisonLoadAgent;
begin
  try
    LLoadAgent := TOutputComparisonLoadAgent.Create(FAppModules);
    try
      LLoadAgent.ConstructData(TYieldModelDataObject(FAppModules.Model.ModelData));
      PopulateDataViewer;
    finally
      FreeAndNil(LLoadAgent);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.DobtnResetClick(Sender : TObject);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DobtnResetClick';
begin
  try
    OutputComparisonFileSelectionDialog.edtFirstSumOut.Text := '';
    OutputComparisonFileSelectionDialog.edtSecondSumOut.Text := '';
    FAppModules.ViewIni.WriteString(ClassName,'SelectedFirstFile'+FAppModules.StudyArea.ScenarioCode,'');
    FAppModules.ViewIni.WriteString(ClassName,'SelectedSecondFile'+FAppModules.StudyArea.ScenarioCode,'');
    FAppModules.ViewIni.WriteString(ClassName,'SelectedReservoirs1'+FAppModules.StudyArea.ScenarioCode,'');
    FAppModules.ViewIni.WriteString(ClassName,'SelectedReservoirs2'+FAppModules.StudyArea.ScenarioCode,'');
    FAppModules.ViewIni.WriteString(ClassName,'SelectedChannel1'+FAppModules.StudyArea.ScenarioCode,'');
    FAppModules.ViewIni.WriteString(ClassName,'SelectedChannel2'+FAppModules.StudyArea.ScenarioCode,'');
    DobtnRefreshSumOutClick(nil);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TOutputComparisonFileSelectionValidator.DoFileSearch : string;
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoFileSearch';
var
  LOpenDialog : TOpenDialog;
begin
  Result := '';
  try
    LOpenDialog := TOpenDialog.Create(nil);
    try
      if LOpenDialog.Execute then
        Result := LOpenDialog.FileName;
    finally
      FreeAndNil(LOpenDialog);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TOutputComparisonFileSelectionValidator.UpdateFirstReservoirs;
const OPNAME = 'TOutputComparisonFileSelectionValidator.UpdateFirstReservoirs';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LIndex : integer;
  LReservoirList : IReservoirDataList;
  LReservoirData : IReservoirData;
  LReservoirNum : integer;
  LSelectedReservoirs : TStringList;
begin
  try
    LSelectedReservoirs := TStringList.Create;
    try
      LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList;
      for LIndex := 0 to OutputComparisonFileSelectionDialog.strgFirstReservoirs.RowCount-1 do
      begin
        if (OutputComparisonFileSelectionDialog.strgFirstReservoirs.CheckedRow[LIndex]) then
        begin
          LReservoirNum := integer(OutputComparisonFileSelectionDialog.strgFirstReservoirs.Objects[1,LIndex]);
          LReservoirData   := LReservoirList.ReservoirOrNodeByIdentifier[LReservoirNum];
          if (LReservoirData <> nil) then
            LSelectedReservoirs.Add(IntToStr(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
        end;
      end;
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      if LOutputComparisonList <> nil then
      begin
        for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
        begin
          if LIndex = 0 then
          begin
            LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
            LOutputComparisonData.ReservoirList := LSelectedReservoirs.CommaText;
            FAppModules.ViewIni.WriteString(ClassName,'SelectedReservoirs1'+FAppModules.StudyArea.ScenarioCode,LSelectedReservoirs.Commatext);
          end;
        end;
      end;
    finally
      FreeAndNil(LSelectedReservoirs);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.UpdateSecondReservoirs;
const OPNAME = 'TOutputComparisonFileSelectionValidator.UpdateSecondReservoirs';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LIndex : integer;
  LReservoirList : IReservoirDataList;
  LReservoirData : IReservoirData;
  LReservoirNum : integer;
  LSelectedReservoirs : TStringList;
begin
  try
    LSelectedReservoirs := TStringList.Create;
    try
      LReservoirList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.
                            ReservoirList;
      for LIndex := 0 to OutputComparisonFileSelectionDialog.strgSecondReservoirs.RowCount-1 do
      begin
        if (OutputComparisonFileSelectionDialog.strgSecondReservoirs.CheckedRow[LIndex]) then
        begin
          LReservoirNum := integer(OutputComparisonFileSelectionDialog.strgSecondReservoirs.Objects[1,LIndex]);
          LReservoirData   := LReservoirList.ReservoirOrNodeByIdentifier[LReservoirNum];
          if (LReservoirData <> nil) then
            LSelectedReservoirs.Add(IntToStr(LReservoirData.ReservoirConfigurationData.ReservoirIdentifier));
        end;
      end;
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      if LOutputComparisonList <> nil then
      begin
        for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
        begin
          if (LIndex = 1) then
          begin
            LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
            LOutputComparisonData.ReservoirList := LSelectedReservoirs.CommaText;
            FAppModules.ViewIni.WriteString(ClassName,'SelectedReservoirs2'+FAppModules.StudyArea.ScenarioCode,LSelectedReservoirs.Commatext);
          end;
        end;
      end;
    finally
      FreeAndNil(LSelectedReservoirs);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.UpdateFirstChannels;
const OPNAME = 'TOutputComparisonFileSelectionValidator.UpdateFirstChannels';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LIndex : integer;
  LChannelNum : integer;
  LSelectedChannels : TStringList;
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;
begin
  try
    LSelectedChannels := TStringList.Create;
    try
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
      for LIndex := 0 to OutputComparisonFileSelectionDialog.strgFirstChannel.RowCount - 1 do
      begin
        if (OutputComparisonFileSelectionDialog.strgFirstChannel.CheckedRow[LIndex]) then
        begin
          LChannelNum := integer(OutputComparisonFileSelectionDialog.strgFirstChannel.Objects[1,LIndex]);
          LChannel   := LChannelList.ChannelByChannelNumber[LChannelNum];
          if (LChannel <> nil) then
            LSelectedChannels.Add(IntToStr(LChannel.ChannelNumber));
        end;
      end;
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      if LOutputComparisonList <> nil then
      begin
        for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
        begin
          if LIndex = 0 then
          begin
            LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
            LOutputComparisonData.ChannelList := LSelectedChannels.CommaText;
            FAppModules.ViewIni.WriteString(ClassName,'SelectedChannel1'+FAppModules.StudyArea.ScenarioCode,LSelectedChannels.Commatext);
          end;
        end;
      end;
    finally
      FreeAndNil(LSelectedChannels);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;


procedure TOutputComparisonFileSelectionValidator.UpdateSecondChannels;
const OPNAME = 'TOutputComparisonFileSelectionValidator.UpdateSecondChannels';
var
  LOutputComparisonList : TOutputComparisonList;
  LOutputComparisonData : TOutputComparisonData;
  LIndex : integer;
  LChannelNum : integer;
  LSelectedChannels : TStringList;
  LChannelList : IChannelList;
  LChannel : IGeneralFlowChannel;
begin
  try
    LSelectedChannels := TStringList.Create;
    try
      LChannelList := TYieldModelDataObject(FAppModules.Model.ModelData).NetworkElementData.ChannelList;
      for LIndex := 0 to OutputComparisonFileSelectionDialog.strgSecondChannel.RowCount - 1 do
      begin
        if (OutputComparisonFileSelectionDialog.strgSecondChannel.CheckedRow[LIndex]) then
        begin
          LChannelNum := integer(OutputComparisonFileSelectionDialog.strgSecondChannel.Objects[1,LIndex]);
          LChannel   := LChannelList.ChannelByChannelNumber[LChannelNum];
          if (LChannel <> nil) then
            LSelectedChannels.Add(IntToStr(LChannel.ChannelNumber));
        end;
      end;
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      if LOutputComparisonList <> nil then
      begin
        for LIndex := 0 to LOutputComparisonList.GetOutputComparisonDataCount-1 do
        begin
          if LIndex > 0 then
          begin
            LOutputComparisonData := LOutputComparisonList.GetOutputComparisonDataByIndex(LIndex);
            LOutputComparisonData.ChannelList := LSelectedChannels.CommaText;
            FAppModules.ViewIni.WriteString(ClassName,'SelectedChannel2'+FAppModules.StudyArea.ScenarioCode,LSelectedChannels.Commatext);
          end;
        end;
      end;
    finally
      FreeAndNil(LSelectedChannels);
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.DoDrawGrid(Sender: TObject; ACol, ARow: integer; Rect: TRect;
                                                             State: TGridDrawState);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoDrawGrid';
begin
  try
    (Sender as TFieldCheckListStringGrid).Canvas.Font.Color := clRed;
    if ((Sender as TFieldCheckListStringGrid).Canvas.Brush.Color = clHighlight) then
      (Sender as TFieldCheckListStringGrid).Canvas.Font.Color := clBlack;
    (Sender as TFieldCheckListStringGrid).Canvas.Brush.Color := clYellow;
    (Sender as TFieldCheckListStringGrid).Canvas.TextRect(Rect, Rect.Left, Rect.Top, (Sender as TFieldStringGrid).Cells[ACol, ARow]);
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.DoFirstChannelDrawGrid(Sender: TObject; ACol, ARow: integer; Rect: TRect;
  State: TGridDrawState);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoFirstChannelDrawGrid';
var
  LOutputComparisonList : TOutputComparisonList;
  LDifference : double;
  LElement : integer;
begin
  try
    if (ACol = 2)and (Sender = OutputComparisonFileSelectionDialog.strgFirstChannel) then
    begin
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      if LOutputComparisonList <> nil then
      begin
          LElement := integer(OutputComparisonFileSelectionDialog.strgFirstChannel.Objects[1,ARow]);
        if LElement > 0 then
        begin
          LDifference := LOutputComparisonList.GetChannelAvrgFlowDifference(LElement);
          if LDifference <> 0.0 then
            DoDrawGrid(Sender,ACol,ARow,Rect,State);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.DoFirstReservoirsDrawGrid(Sender: TObject; ACol, ARow: integer; Rect: TRect;
  State: TGridDrawState);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoFirstReservoirsDrawGrid';
var
  LOutputComparisonList : TOutputComparisonList;
  LDifference : double;
  LElement : integer;
begin
  try
    if (ACol = 2)and (Sender = OutputComparisonFileSelectionDialog.strgFirstReservoirs) then
    begin
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      if LOutputComparisonList <> nil then
      begin
        LElement := integer(OutputComparisonFileSelectionDialog.strgFirstReservoirs.Objects[1,ARow]);
        if LElement > 0 then
        begin
          LDifference := LOutputComparisonList.GetReservoirsVolumeDifference(LElement);
          if (LDifference <> 0.0) then
            DoDrawGrid(Sender,ACol,ARow,Rect,State);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.DoSecondChannelDrawGrid(Sender: TObject; ACol, ARow: integer; Rect: TRect;
  State: TGridDrawState);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoSecondChannelDrawGrid';
var
  LOutputComparisonList : TOutputComparisonList;
  LDifference : double;
  LElement : integer;
begin
  try
    if (ACol = 2) and (Sender = OutputComparisonFileSelectionDialog.strgSecondChannel) then
    begin
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      if LOutputComparisonList <> nil then
      begin
        LElement := integer(OutputComparisonFileSelectionDialog.strgSecondChannel.Objects[1,ARow]);
        if LElement > 0 then
        begin
          LDifference := LOutputComparisonList.GetChannelAvrgFlowDifference(LElement);
          if LDifference <> 0.0 then
            DoDrawGrid(Sender,ACol,ARow,Rect,State);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TOutputComparisonFileSelectionValidator.DoSecondReservoirsDrawGrid(Sender: TObject; ACol, ARow: integer; Rect: TRect;
  State: TGridDrawState);
const OPNAME = 'TOutputComparisonFileSelectionValidator.DoSecondReservoirsDrawGrid';
var
  LOutputComparisonList : TOutputComparisonList;
  LDifference : double;
  LElement : integer;
begin
  try
    if (ACol = 2) and (Sender = OutputComparisonFileSelectionDialog.strgSecondReservoirs) then
    begin
      LOutputComparisonList := TYieldModelDataObject(FAppModules.Model.ModelData).OutputComparisonData;
      if LOutputComparisonList <> nil then
      begin
        LElement := integer(OutputComparisonFileSelectionDialog.strgSecondReservoirs.Objects[1,ARow]);
        if LElement > 0 then
        begin
          LDifference := LOutputComparisonList.GetReservoirsVolumeDifference(LElement);
          if LDifference <> 0.0 then
            DoDrawGrid(Sender,ACol,ARow,Rect,State);
        end;
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
