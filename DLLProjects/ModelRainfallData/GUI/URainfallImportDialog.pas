{******************************************************************************}
{*  UNIT      : Contains the class TRainfallImportDialog.                     *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/10/27                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit URainfallImportDialog;

interface

{$WARN UNIT_PLATFORM OFF}                          
uses
  VCLTee.Chart,
  VCLTee.Series,
  VCLTee.TeeProcs,
  VCLTee.TeEngine,

  VCL.Graphics,
  VCL.Buttons,
  VCL.ExtCtrls,
  VCL.Controls,
  VCL.Grids,
  VCL.Forms,
  VCL.ComCtrls,
  Types,
  VCL.Menus,
  Classes,
  Windows,
  VCL.StdCtrls,
  VCL.FileCtrl,

  UConstants,
  UTabsheetManager,
  UAbstractObject,
  UAbstractComponent,
  UMenuItemManager,
  UDataComponent,
  UDataEditComponent,
  UGenericModelLinkClasses;


type
  TRainfallImportDialog = class(TAbstractForm)
  protected
    FScrollBox         : TScrollBox;
    FPnlButtons        : TAbstractPanel;
    FBtnOK             : TSpeedButton;
    FBtnCancel         : TSpeedButton;
    FPnlClient         : TAbstractPanel;
    FSelectLabel       : TLabel;
    FDirectoryLabel    : TLabel;
    FDriveComboBox     : TDriveComboBox;
    FDirectoryListBox  : TDirectoryListBox;
    FFileListBox       : TFileListBox;
    FOutputEdit        : TRichEdit;
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure AssignHelpContext; override;
    procedure OnCancelClick(Sender: TObject);
    procedure OnOKClick(Sender: TObject);
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure DoImportUserData (ASender : TObject);
    function ValidateFile (APath     : string;
                           AFileName : string) : boolean;
    function GaugeNumberUnique (AGaugeNr : string{;
                                ASource  : string}) : boolean;
    function ImportFile (APath     : string;
                         AFileName : string) : boolean;
    procedure WriteOutput (AMessage : string;
                           AColor   : TColor;
                           ABold    : boolean);
    procedure ChopToComma (var ALine : string;
                           var AWord : string);

  public
    function Initialise: boolean; override;
    function LanguageHasChanged: boolean; override;
    property FileListBox       : TFileListBox      read FFileListBox;
    property DirectoryListBox  : TDirectoryListBox read FDirectoryListBox;
    property OutputEdit        : TRichEdit         read FOutputEdit;
  end;

implementation

uses
  SysUtils,
  VCL.Dialogs,
  Math,
  VCL.Printers,
  UHelpContexts,
  UDataSetType,
  Contnrs,
  UErrorHandlingOperations;

{ TRainfallImportDialog }

procedure TRainfallImportDialog.CreateMemberObjects;
const OPNAME = 'TRainfallImportDialog.CreateMemberObjects';
begin
  inherited;
  try
    Position := poScreenCenter;
    OnClose  := FormClose;
    ClientHeight := 300;
    ClientWidth  := 300;

    FScrollBox := TScrollBox.Create(Self);
    with FScrollBox do
    begin
      Parent     := Self;
      Left       := 0;
      Top        := 80;
      Width      := 557;
      Height     := 336;
      Align      := alClient;
      BevelInner := bvNone;
      TabOrder   := 1;
    end;
    FPnlButtons := TAbstractPanel.Create(Self, FAppModules);
    with FPnlButtons do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 0;
      Height     := 30;
      Align      := alTop;
      BevelOuter := bvNone;
      TabOrder   := 0;
    end;
    FBtnOK := TSpeedButton.Create(FPnlButtons);
    FBtnOK.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPOK'));
    with FBtnOK do
    begin
      Parent   := FPnlButtons;
      Left     := 0;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 0;
      ShowHint := TRUE;
      OnClick  := OnOKClick;
    end;
    FBtnCancel := TSpeedButton.Create(FPnlButtons);
    FBtnCancel.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPCancel'));
    with FBtnCancel do
    begin
      Parent   := FPnlButtons;
      Left     := 30;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 1;
      ShowHint := TRUE;
      OnClick  := OnCancelClick;
    end;

    FPnlClient := TAbstractPanel.Create(Self, FAppModules);
    with FPnlClient do
    begin
      Parent     := FScrollBox;
      Left       := 0;
      Top        := 30;
      Width      := 550;
      Height     := 370;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
    FSelectLabel := TLabel.Create(Self);
    with FSelectLabel do
    begin
      Parent     := FPnlClient;
      Left       := 20;
      Top        := 10;
      Width      := 200;
      Height     := 13;
      Font.Style := [fsBold];
      Caption    := FAppModules.Language.GetString('LabelText.SpecifyFiles');
    end;
    FDirectoryLabel := TLabel.Create(Self);
    with FDirectoryLabel do
    begin
      Parent := FPnlClient;
      Left   := 20;
      Top    := 30;
      Width  := 200;
      Height := 13;
    end;
    FDriveComboBox := TDriveComboBox.Create(Self);
    with FDriveComboBox do
    begin
      Parent   := FPnlClient;
      Left     := 20;
      Top      := 50;
      Width    := 200;
      Height   := 19;
      TabOrder := 0;
    end;
    FDirectoryListBox := TDirectoryListBox.Create(Self);
    with FDirectoryListBox do
    begin
      Parent     := FPnlClient;
      Left       := 20;
      Top        := 75;
      Width      := 200;
      Height     := 140;
      ItemHeight := 16;
      TabOrder   := 1;
    end;
    FFileListBox := TFileListBox.Create(Self);
    with FFileListBox do
    begin
      Parent      := FPnlClient;
      Left        := 20;
      Top         := 225;
      Width       := 200;
      Height      := 140;
      ItemHeight  := 13;
      MultiSelect := True;
      TabOrder    := 2;
      Mask        := '*.csv';
    end;
    FDriveComboBox.DirList     := FDirectoryListBox;
    FDirectoryListBox.FileList := FFileListBox;
    FOutputEdit := TRichEdit.Create(Self);
    with FOutputEdit do
    begin
      Parent      := FPnlClient;
      Left        := 240;
      Top         := 50;
      Width       := 300;
      Height      := 315;
      ReadOnly    := True;
      ScrollBars  := ssVertical;
      TabOrder    := 3;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallImportDialog.DestroyMemberObjects;
const OPNAME = 'TRainfallImportDialog.DestroyMemberObjects';
begin
  inherited DestroyMemberObjects;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRainfallImportDialog.Initialise: Boolean;
const OPNAME = 'TRainfallImportDialog.Initialise';
begin
  Result := inherited initialise;
  try
    FDirectoryListBox.DirLabel := FDirectoryLabel;
    Result := True;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallImportDialog.AssignHelpContext;
const OPNAME = 'TRainfallImportDialog.AssignHelpContext';
begin
  inherited;
  try
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRainfallImportDialog.OnCancelClick(Sender: TObject);
const OPNAME = 'TRainfallImportDialog.OnCancelClick';
begin
  try
    Close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallImportDialog.OnOKClick(Sender: TObject);
const OPNAME = 'TRainfallImportDialog.OnOKClick';
begin
  try
    DoImportUserData(Self);
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallImportDialog.FormClose (Sender     : TObject;
                                           var Action : TCloseAction);
const OPNAME = 'TRainfallImportDialog.FormClose';
begin
  try
//    ModalResult := mrCancel;
    Action := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallImportDialog.DoImportUserData(ASender : TObject);
const OPNAME = 'TRainfallImportDialog.DoImportUserData';
var
  lIndex       : integer;
  lFileName    : string;
  lPath        : string;
  lDataChanged : Boolean;
begin
  try
    OutputEdit.Lines.Clear;
    lPath := DirectoryListBox.Directory;
    lDataChanged := FALSE;
    for lIndex := 0 to FileListBox.Items.Count - 1 do
    begin
      if (FileListBox.Selected[lIndex]) then
      begin
        lFileName := FileListBox.Items[lIndex];
        WriteOutput(FAppModules.Language.GetString('Rainfall.Validate') + ' ' + lFileName + '.', clBlack, TRUE);
        if (ValidateFile(lPath, lFileName)) then
        begin
          WriteOutput(lFileName + ' ' + FAppModules.Language.GetString('Rainfall.ValidFile'), clBlack, FALSE);
          WriteOutput(FAppModules.Language.GetString('Rainfall.Import') + ' ' + lFileName + '.', clBlack, TRUE);
          if (ImportFile(lPath, lFileName)) then
          begin
            WriteOutput(lFileName + ' ' + FAppModules.Language.GetString('Rainfall.SuccessfullyImported'), clBlack, TRUE);
            lDataChanged := TRUE;
          end;
        end
        else
          WriteOutput(lFileName + ' ' + FAppModules.Language.GetString('Rainfall.NotValidFile'), clRed, FALSE);
        WriteOutput('  ', clBlack, TRUE);
      end;
    end;
    if (lDataChanged) then
      FAppModules.Model.StudyDataHasChanged(sdccAdd,FAppModules.Language.GetString('Rainfall.UserGauges'), '', '');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallImportDialog.GaugeNumberUnique (AGaugeNr : string{;
                                                  ASource  : string}) : boolean;
const OPNAME = 'TRainfallImportDialog.GaugeNumberUnique';
var
  lDataset : TAbstractModelDataset;
  lSQL     : string;
begin
  Result := TRUE;
  try
    FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
    try
      if (Assigned(lDataset)) then
      begin
{        lSQL := 'SELECT * FROM RainfallUserStations ' +
                'WHERE Source = ' + QuotedStr(ASource) +
                ' AND StationNumber = ' + QuotedStr(AGaugeNr);}
        lSQL := 'SELECT * FROM RainfallUserStations ' +
                'WHERE StationNumber = ' + QuotedStr(AGaugeNr);
        lDataset.DataSet.Close;
        lDataset.SetSQL(lSQL);
        lDataset.DataSet.Open;
        if (NOT lDataset.DataSet.Eof) then
          Result := FALSE;
      end;
    finally
      LDataset.Dataset.Close;
      FreeAndNil(LDataset);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallImportDialog.ValidateFile (APath     : string;
                                             AFileName : string) : boolean;
const OPNAME = 'TRainfallImportDialog.ValidateFile';
var
  lFileText          : TStringList;
  lValidFlags        : TStringList;
  lLine              : string;
  lWord              : string;
  lLineIdx           : integer;
  lResult            : boolean;
  lColIdx            : integer;
  lFieldProperty     : TAbstractFieldProperty;
  lYear              : integer;
  lPrevYear          : integer;
  lSource            : string;
  lErrorLineString   : string;
  lInvalidYearString : string;
  lErrorColumnString  : string;
  lInvalidStationString : string;
begin
  Result := FALSE;
  try
    lErrorLineString := FAppModules.Language.GetString('Rainfall.ErrorLine');
    lInvalidYearString := FAppModules.Language.GetString('Rainfall.InvalidYear');
    lErrorColumnString := FAppModules.Language.GetString('Rainfall.ErrorColumn');
    lInvalidStationString := FAppModules.Language.GetString('Rainfall.InvalidStationNumber');
    lPrevYear := 0;
    lFieldProperty := FAppModules.FieldProperties.FieldProperty('MonthlyRAWSign');
    if (NOT (FileExists(APath + '\' + AFileName))) then
      WriteOutput(AFileName + FAppModules.Language.GetString('Rainfall.DoesNotExist'), clRed, TRUE)
    else
    begin
      lFileText   := TStringList.Create;
      lValidFlags := TStringList.Create;
      try
        lValidFlags.CommaText := lFieldProperty.FieldAcceptedValues;
        lColIdx := lValidFlags.IndexOf(FAppModules.Language.GetString('Rainfall.None'));
        if (lColIdx >= 0) then
          lValidFlags.Delete(lColIdx);
        lFileText.LoadFromFile(APath + '\' + AFileName);
        lResult := (lFileText.Count >= 2);
        if (NOT lResult) then
          WriteOutput(FAppModules.Language.GetString('Rainfall.ErrorInvalidFile'), clRed, FALSE)
        else
        begin
          lLineIdx := 0;
          lLine    := lFileText[lLineIdx];
          lColIdx  := 1;
          while (lResult AND (lColIdx <= 5)) do
          begin
            ChopToComma(lLine, lWord);
            case lColIdx of
            1 :
              begin
                lSource:= lWord;
                lResult := (Length(lSource) > 0);
                if (NOT lResult) then
                  WriteOutput(FAppModules.Language.GetString('Rainfall.ErrorColumn1'), clRed, FALSE);
              end;
            2 :
              begin
                lResult := (Length(lWord) = 8);
                if (NOT lResult) then
                  WriteOutput(lInvalidStationString, clRed, FALSE)
                else
                begin
                  lResult := GaugeNumberUnique(lWord{, lSource});
                  if (NOT lResult) then
                    WriteOutput(FAppModules.Language.GetString('Rainfall.DuplicateNumber'), clRed, FALSE)
                  else
                  begin
                    if (NOT CharInSet(lWord[8],['U', 'V', 'X', 'Y', 'Z'])) then
                    begin
                      lResult := FALSE;
                      WriteOutput(lInvalidStationString, clRed, FALSE)
                    end
                    else
                    begin
                      try
                        StrToInt(Copy(lWord, 1, 7));
                      except
                        lResult := FALSE;
                        WriteOutput(lInvalidStationString, clRed, FALSE)
                      end;
                    end
                  end;
                end;
              end;
            3 :
              begin
                lResult := (Length(lWord) > 0);
                if (NOT lResult) then
                  WriteOutput(FAppModules.Language.GetString('Rainfall.StationName'), clRed, FALSE);
              end;
            4, 5 :
              begin
                try
                  StrToFloat(lWord);
                except
                  lResult := FALSE;
                  if (lColIdx = 4) then
                    WriteOutput(lErrorColumnString + IntToStr(lColIdx) + FAppModules.Language.GetString('Rainfall.InvalidLongitude'), clRed, FALSE)
                  else
                    WriteOutput(lErrorColumnString + IntToStr(lColIdx) + FAppModules.Language.GetString('Rainfall.InvalidLatitude'), clRed, FALSE);
                end;
              end;
            else
            end;
            lColIdx := lColIdx + 1;
          end;
          lLineIdx := 1;
          while (lResult AND (lLineIdx < lFileText.Count)) do
          begin
            lLine := lFileText[lLineIdx];
            lColIdx := 1;
            while (lResult AND (lColIdx <= 25)) do
            begin
              ChopToComma(lLine, lWord);
              if (lColIdx = 1) then
              begin
                lResult := (Length(lWord) = 4);
                if (NOT lResult) then
                  WriteOutput(lErrorLineString + IntToStr(lLineIdx) + lInvalidYearString, clRed, FALSE)
                else
                begin
                  try
                    lYear := StrToInt(lWord);
                    if (lPrevYear <> 0) AND (lYear <> lPrevYear + 1) then
                    begin
                      lResult := FALSE;
                      WriteOutput(lErrorLineString + IntToStr(lLineIdx) + FAppModules.Language.GetString('Rainfall.FollowConsecutive'), clRed, FALSE)
                    end
                    else
                      lPrevYear := lYear;
                  except
                    lResult := FALSE;
                    WriteOutput(lErrorLineString + IntToStr(lLineIdx) + lInvalidYearString, clRed, FALSE)
                  end;
                end;
              end
              else
              if ((lColIdx mod 2) = 0) then
              begin
                try
                  if (lWord <> '') then
                    StrToFloat(lWord);
                except
                  lResult := FALSE;
                  WriteOutput(lErrorLineString + IntToStr(lLineIdx) +
                              FAppModules.Language.GetString('Rainfall.Month') + IntToStr((lColIdx + 1) div 2) +
                              FAppModules.Language.GetString('Rainfall.InvalidValue'), clRed, FALSE)
                end;
              end
              else
              begin
                lResult := (lWord = '') OR ((Length(lWord) = 1) AND (lValidFlags.IndexOf(lWord) >= 0));
                if (NOT lResult) then
                  WriteOutput(lErrorLineString + IntToStr(lLineIdx) +
                              FAppModules.Language.GetString('Rainfall.Month') + IntToStr((lColIdx + 1) div 2) +
                              FAppModules.Language.GetString('Rainfall.InvalidFlag'), clRed, FALSE)

              end;
              lColIdx := lColIdx + 1;
            end;
            lLineIdx := lLineIdx + 1;
          end;
          Result := lResult;
        end;
      finally
        FreeAndNil(lFileText);
        FreeAndNil(lValidFlags);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallImportDialog.ImportFile (APath     : string;
                                           AFileName : string) : boolean;
const OPNAME = 'TRainfallImportDialog.ImportFile';
var
  lFileText      : TStringList;
  lLine          : string;
  lWord          : string;
  lLineIdx       : integer;
  lColIdx        : integer;
  lYear          : integer;
  lSource        : string;
  lStationNumber : string;
  lStationName   : string;
  lStationLink   : string;
  lStationID     : integer;
  lLongitude     : double;
  lLatitude      : double;
  lLongitudeInt  : integer;
  lLatitudeInt   : integer;
  lFlag          : string;
  lDataset       : TAbstractModelDataset;
  lSQL           : string;
begin
  Result := FALSE;
  try
    if (NOT (FileExists(APath + '\' + AFileName))) then
      WriteOutput(AFileName + FAppModules.Language.GetString('Rainfall.DoesNotExist'), clRed, TRUE)
    else
    begin
      FAppModules.Database.CreateDataset(integer(dtExecSQL), lDataSet);
      lFileText := TStringList.Create;
      try
        if (Assigned(lDataset)) then
        begin
          FAppModules.Database.StartTransaction;
          try
            lFileText.LoadFromFile(APath + '\' + AFileName);
            lLine := lFileText[0];
            ChopToComma(lLine, lSource);
            ChopToComma(lLine, lStationNumber);
            ChopToComma(lLine, lStationName);
            ChopToComma(lLine, lWord);
            lLongitude := StrToFloat(lWord);
            lLongitudeInt := Round(lLongitude * 60);
            ChopToComma(lLine, lWord);
            lLatitude := StrToFloat(lWord);
            lLatitudeInt := Round(lLatitude * 60);
            ChopToComma(lLine, lStationLink);

            lDataset.DataSet.Close;
            lSQL := 'SELECT Max(StationID) AS NewStationID FROM RainfallUserStations';
            lDataset.SetSQL(lSQL);
            lDataset.Dataset.Open;
            lStationID := 0;
            if (NOT lDataset.DataSet.Eof) then
              lStationID := lDataset.Dataset.FieldByName('NewStationID').AsInteger;
            if (lStationID = 0) then
              lStationID := 100000;
            lStationID := lStationID + 1;

            lSQL := 'INSERT INTO RainfallUserStations ' +
                    '(StationID, Source, StationNumber, StationLongitude, StationLatitude, ' +
                    'StationName, StationLinkNumber) VALUES (' +
                    IntToStr(lStationID) + ',' +
                    QuotedStr(lSource) + ',' +
                    QuotedStr(lStationNumber) + ',' +
                    IntToStr(lLongitudeInt) + ',' +
                    IntToStr(lLatitudeInt) + ',' +
                    QuotedStr(lStationName) + ',';
            if (lStationLink = '') then
              lSQL := lSQL + 'NULL)'
            else
              lSQL := lSQL + QuotedStr(lStationLink) + ')';
            lDataset.DataSet.Close;
            lDataset.SetSQL(lSQL);
            lDataset.ExecSQL;

            for lLineIdx := 1 to lFileText.Count - 1 do
            begin
              lLine := lFileText[lLineIdx];
              ChopToComma(lLine, lWord);
              lYear := StrToInt(lWord);
              lSQL := 'INSERT INTO RainfallUserMonthlyData (StationID, [Year], ' +
                      'Value01, Flag01, Value02, Flag02, Value03, Flag03, Value04, Flag04, ' +
                      'Value05, Flag05, Value06, Flag06, Value07, Flag07, Value08, Flag08, ' +
                      'Value09, Flag09, Value10, Flag10, Value11, Flag11, Value12, Flag12' +
                      ') VALUES (' +
                      IntToStr(lStationID) + ',' +
                      IntToStr(lYear);

              for lColIdx := 1 to 12 do
              begin
                ChopToComma(lLine, lWord);
                ChopToComma(lLine, lFlag);
                if (lWord = '') then
                  lSQL := lSQL + ',NULL,' + QuotedStr(lFlag)
                else
                  lSQL := lSQL + ',' + lWord + ',' + QuotedStr(lFlag);
              end;
              lSQL := lSQL + ')';
              lDataset.DataSet.Close;
              lDataset.SetSQL(lSQL);
              lDataset.ExecSQL;
            end;
            FAppModules.Database.Commit;
            Result := TRUE;
          except{ on E : Exception do}
            FAppModules.Database.Rollback;
            raise;
          end;
        end;
      finally
        LDataset.Dataset.Close;
        FreeAndNil(LDataset);
        FreeAndNil(lFileText);
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallImportDialog.WriteOutput (AMessage : string;
                                             AColor   : TColor;
                                             ABold    : boolean);
const OPNAME = 'TRainfallImportDialog.WriteOutput';
begin
  try
    OutputEdit.SelAttributes.Color := AColor;
    if (ABold) then
      OutputEdit.SelAttributes.Style := [fsBold]
    else
      OutputEdit.SelAttributes.Style := [];
    OutputEdit.Lines.Add(AMessage);
    OutputEdit.Repaint;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TRainfallImportDialog.ChopToComma (var ALine : string;
                                             var AWord : string);
const OPNAME = 'TRainfallImportDialog.ChopToComma';
var
  lPos : integer;
begin
  try
    AWord := '';
    lPos  := Pos(',', ALine);
    if (lPos > 0) then
    begin
      AWord := Trim(Copy(ALine, 1, lPos - 1));
      ALine := Trim(Copy(ALine, lPos + 1, Length(ALine) - lPos));
    end
    else
    begin
      AWord := Trim(ALine);
      ALine := '';
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TRainfallImportDialog.LanguageHasChanged: boolean;
const OPNAME = 'TRainfallImportDialog.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption          := FAppModules.Language.GetString('Rainfall.RDImportUserData');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.

