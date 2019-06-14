{******************************************************************************}
{*  UNIT      : Contains the class TChangeParameterForm.                      *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2005/01/16                                                    *}
{*  COPYRIGHT : Copyright © 2005 DWAF                                         *}
{******************************************************************************}

unit UChangeParameterForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms,
  Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.ComCtrls, Vcl.Grids,Vcl.Buttons,
  UAbstractObject,
  UAbstractComponent,
  UDataComponent,
  UDataEditComponent,
  UChildToolbar,
  UChangeData,
  VoaimsCom_TLB,
  RainfallCom_TLB,
  Vcl.ImgList;

type
  TChangeParameterForm = class(TAbstractForm)
  private
    FInEditMode        : Boolean;
    FInNewMode         : Boolean;
    FSystemFlag        : boolean;
    FParamField        : string;
    FKeyValues         : string;
    FFieldIndex        : string;
    FChangeListID      : integer;
    FModelChangeLists  : TStringList;
    FFieldProperty     : TAbstractFieldProperty;
    FBaseValue         : string;
    FRows              : integer;

    FPnlButtons         : TAbstractPanel;
    FBtnNew             : TSpeedButton;
    FBtnChange          : TSpeedButton;
    FBtnDelete          : TSpeedButton;
    FBtnOK              : TSpeedButton;
    FBtnCancel          : TSpeedButton;
    FBtnCLose           : TSpeedButton;
    FPnlClient          : TAbstractPanel;
    FLblParamName       : TLabel;
    FLblParamNameVal    : TLabel;
    FLblEntityName      : TLabel;
    FLblEntityNameVal   : TLabel;
    FLblBaseValue       : TLabel;
    FEdtBaseValue       : TFieldEdit;
    FLblParamDescr      : TLabel;
    FChkShowAll         : TCheckBox;
    FGrdParamChange     : TStringGrid;
    FPnlChange          : TAbstractPanel;
    FImgOverride        : TImage;
    FImgActive          : TImage;
    FImgInActive        : TImage;
    FLblChangeList      : TLabel;
    FCbxChangeList      : TFieldComboBox;
    FRdbPercentage      : TFieldRadioButton;
    FRdbAbsolut         : TFieldRadioButton;
    FEdtChange          : TAbstractFieldEdit;
    FEdtDescr           : TAbstractFieldEdit;
    FCbxChange          : TAbstractComboBox;
    procedure PopulateControls;
    procedure PopulateGrid;
    procedure PopulateNewValFloat;
    procedure PopulateNewValInt;
    procedure PopulateComboBox;
    procedure ResetControls;
    function GetKeyValue (AKeyName   : string;
                          AKeyValues : string) : string;
    procedure DoSelectCell (ARow : integer);
    function InputValid (var AChangeList : IChangeList;
                         var AAbsolut    : string;
                         var AChange     : string;
                         var AParamDescr : string): Boolean;
    function ValidateParamField (AValue       : string;
                                 var lMessage : string) : boolean;
  protected
    procedure CreateMemberObjects; override;
    procedure DestroyMemberObjects; override;
    procedure FormClose (Sender     : TObject;
                         var Action : TCloseAction);
    procedure FormShow (Sender: TObject);
    procedure OnResizeForm (Sender : TObject);
    procedure OnShowAllClick(Sender: TObject);
    procedure OnFormCloseQuery (Sender : TObject;
                                var CanClose: Boolean);
    procedure GrdChangeParamsDrawCell (Sender     : TObject;
                                       ACol, ARow : Integer;
                                       Rect       : TRect;
                                       State      : TGridDrawState);
    procedure GrdChangeParamsSelectCell (Sender        : TObject;
                                         ACol, ARow    : Integer;
                                         var CanSelect : Boolean);
    procedure OnNewClick (Sender : TObject);
    procedure OnChangeClick(Sender: TObject);
    procedure OnCancelClick(Sender: TObject);
    procedure OnOKClick(Sender: TObject);
    procedure OnCloseClick (Sender : TObject);
    procedure OnDeleteClick(Sender: TObject);
    procedure OnEdtBaseValueExit(Sender: TObject);
  public
    procedure Resize; override;
    function StudyDataHasChanged(AContext: TChangeContext;
                                 AFieldName,AOldValue,ANewValue: string): boolean; override;
    function LanguageHasChanged: boolean; override;
    function Initialise: boolean; override;
    function GetPatchOriginalValue  (AStationID  : Integer;
                                     AHydroYear, AMonthIndex : Integer) : double;
    procedure SetData (AParamField : string;
                       AKeyValues  : string;
                       AFieldIndex : string);

  end;

implementation

uses
  UDataSetType,
  UConstants,
  UDBConstants,
  UErrorHandlingOperations,
  ComObj;

{******************************************************************************}
{* TChangeParameterForm                                                       *}
{******************************************************************************}

procedure TChangeParameterForm.CreateMemberObjects;
const OPNAME = 'TChangeParameterForm.CreateMemberObjects';
begin
  try
    FModelChangeLists := TStringList.Create;
    FParamField    := '';
    FKeyValues     := '';
    FFieldIndex    := '';

    AutoScroll     := False; 
    Position := poScreenCenter;
    OnClose  := FormClose;
    OnCloseQuery := OnFormCloseQuery;
    OnShow   := FormShow;
    ClientHeight := 350;
    ClientWidth  := 590;

    FPnlButtons := TAbstractPanel.Create(Self, FAppModules);
    with FPnlButtons do
    begin
      Parent     := Self;
      Left       := 0;
      Top        := 0;
      Height     := 30;
      Align      := alTop;
      BevelOuter := bvNone;
      TabOrder   := 0;
    end;
    FBtnNew := TSpeedButton.Create(FPnlButtons);
    FBtnNew.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPCreateNew'));
    with FBtnNew do
    begin
      Parent   := FPnlButtons;
      Left     := 0;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 0;
      ShowHint := TRUE;
      OnClick  := OnNewClick;
    end;
    FBtnChange := TSpeedButton.Create(FPnlButtons);
    FBtnChange.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPEdit'));
    with FBtnChange do
    begin
      Parent   := FPnlButtons;
      Left     := 30;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 1;
      ShowHint := TRUE;
      OnClick  := OnChangeClick
    end;
    FBtnDelete := TSpeedButton.Create(FPnlButtons);
    FBtnDelete.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPDelete'));
    with FBtnDelete do
    begin
      Parent   := FPnlButtons;
      Left     := 60;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 2;
      ShowHint := TRUE;
      OnClick  := OnDeleteClick;
    end;
    FBtnOK := TSpeedButton.Create(FPnlButtons);
    FBtnOK.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPOK'));
    with FBtnOK do
    begin
      Parent   := FPnlButtons;
      Left     := 90;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 3;
      ShowHint := TRUE;
      OnClick  := OnOKClick;
    end;
    FBtnCancel := TSpeedButton.Create(FPnlButtons);
    FBtnCancel.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPCancel'));
    with FBtnCancel do
    begin
      Parent   := FPnlButtons;
      Left     := 120;
      Top      := 0;
      Width    := 30;
      Height   := 30;
      TabOrder := 4;
      ShowHint := TRUE;
      OnClick  := OnCancelClick;
    end;
    FBtnClose := TSpeedButton.Create(FPnlButtons);
    FBtnClose.Glyph.LoadFromResourceName(HImagesInstance, UpperCase('CPClose'));
    with FBtnClose do
    begin
      Parent      := FPnlButtons;
      Left        := 150;
      Top         := 0;
      Width       := 30;
      Height      := 30;
      TabOrder    := 5;
      ModalResult := 1;
      ShowHint    := TRUE;
      OnClick     := OnCloseClick;
    end;
    FPnlClient := TAbstractPanel.Create(Self, FAppModules);
    with FPnlClient do
    begin
      Parent     := Self;
      Left       := 0;
      Top        := 30;
      Width      := 590;
      Height     := 350;
      BevelOuter := bvNone;
      TabOrder   := 1;
    end;
//    FChkShowAll := TFieldChkBox.Create(Self, FAppModules);
    FChkShowAll := TCheckBox.Create(Self);
    with FChkShowAll do
    begin
      Parent   := FPnlButtons;
      Left     := 240;
      Top      := 7;
      Width    := 120;
      Height   := 17;
      TabOrder := 1;
      OnClick  := OnShowAllClick;
    end;
    FLblParamName := TLabel.Create(Self);
    with FLblParamName do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 10;
      Width    := 100;
      Height   := 13;
    end;
    FLblParamNameVal := TLabel.Create(Self);
    with FLblParamNameVal do
    begin
      Parent   := FPnlClient;
      AutoSize := TRUE;
      Left     := 100;
      Top      := 10;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    FLblEntityName := TLabel.Create(Self);
    with FLblEntityName do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 30;
      Width    := 100;
      Height   := 13;
    end;
    FLblEntityNameVal := TLabel.Create(Self);
    with FLblEntityNameVal do
    begin
      Parent   := FPnlClient;
      AutoSize := TRUE;
      Left     := 100;
      Top      := 30;
      Font.Style := [fsBold];
      ParentFont := False;
    end;
    FLblBaseValue := TLabel.Create(Self);
    with FLblBaseValue do
    begin
      Parent   := FPnlClient;
      AutoSize := FALSE;
      Left     := 10;
      Top      := 50;
      Width    := 60;
      Height   := 13;
    end;
    FEdtBaseValue := TFieldEdit.Create(Self, FAppModules);
    with FEdtBaseValue do
    begin
      Parent   := FPnlClient;
      Left     := 100;
      Top      := 46;
      Width    := 80;
      Height   := 21;
      TabOrder := 0;
      OnExit   := OnEdtBaseValueExit;
    end;
    FGrdParamChange := TStringGrid.Create(Self);
    with FGrdParamChange do
    begin
      Parent           := FPnlClient;
      Left             := 0;
      Top              := 80;
      Width            := 600;
      ColCount         := 7;
      DefaultColWidth  := 60;
      DefaultRowHeight := 26;
      Options          := [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect];
      TabOrder         := 2;
      ScrollBars       := ssVertical;
      OnDrawCell       := GrdChangeParamsDrawCell;
      OnSelectCell     := GrdChangeParamsSelectCell;
    end;
    FImgOverride := TImage.Create(Self);
    FImgOverride.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'CLOVERRIDE');
    with FImgOverride do
    begin
      Parent           := FPnlClient;
      Left             := 184;
      Top              := 6;
      Width            := 16;
      Height           := 16;
      Visible          := False;
    end;
    FPnlChange := TAbstractPanel.Create(Self, FAppModules);
    with FPnlChange do
    begin
      Parent           := FPnlClient;
      Left             := 37;
      Top              := 210;
      Width            := 496;
      Height           := 26;
      TabOrder         := 3;
      Visible          := False;
    end;
    FLblChangeList := TLabel.Create(Self);
    with FLblChangeList do
    begin
      Parent           := FPnlChange;
      Left             := 4;
      Top              := 7;
      AutoSize         := False;
      Width            := 170;
      Height           := 13;
    end;
    FCbxChangeList := TFieldComboBox.Create(Self, FAppModules);
    with FCbxChangeList do
    begin
      Parent           := FPnlChange;
      Left             := 2;
      Top              := 3;
      Width            := 176;
      Height           := 21;
      Style            := csDropDownList;
      ItemHeight       := 13;
      TabOrder         := 0;
    end;
    FImgActive := TImage.Create(Self);
    FImgActive.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'CLACTIVATE');
    with FImgActive do
    begin
      Parent           := FPnlChange;
      Left             := 184;
      Top              := 6;
      Width            := 16;
      Height           := 16;
      Visible          := False;
    end;
    FImgInActive := TImage.Create(Self);
    FImgInActive.Picture.Bitmap.LoadFromResourceName(HImagesInstance, 'CLDEACTIVATE');
    with FImgInActive do
    begin
      Parent           := FPnlChange;
      Left             := 184;
      Top              := 6;
      Width            := 16;
      Height           := 16;
      Visible          := False;
    end;
    FRdbAbsolut := TFieldRadioButton.Create(Self, FAppModules);
    with FRdbAbsolut do
    begin
      Parent           := FPnlChange;
      Left             := 224;
      Top              := 6;
      Width            := 60;
      Height           := 17;
      TabOrder         := 1;
    end;
    FRdbPercentage := TFieldRadioButton.Create(Self, FAppModules);
    with FRdbPercentage do
    begin
      Parent           := FPnlChange;
      Left             := 289;
      Top              := 6;
      Width            := 30;
      Height           := 17;
      TabOrder         := 2;
    end;
    FEdtChange := TAbstractFieldEdit.Create(Self, FAppModules);
    with FEdtChange do
    begin
      Parent           := FPnlChange;
      Left             := 322;
      Top              := 3;
      Width            := 60;
      Height           := 21;
      TabOrder         := 3;
    end;
    FLblParamDescr := TLabel.Create(Self);
    with FLblParamDescr do
    begin
      Parent   := FPnlChange;
      AutoSize := FALSE;
      Left     := 385;
      Top      := 6;
      Width    := 30;
      Height   := 13;
    end;
    FEdtDescr := TAbstractFieldEdit.Create(Self, FAppModules);
    with FEdtDescr do
    begin
      Parent           := FPnlChange;
      Left             := 420;
      Top              := 3;
      Width            := 70;
      Height           := 21;
      TabOrder         := 3;
    end;
    FCbxChange := TAbstractComboBox.Create(Self, FAppModules);
    with FCbxChange do
    begin
      Parent           := FPnlChange;
      Left             := 322;
      Top              := 3;
      Width            := 60;
      Height           := 21;
      TabOrder         := 4;
      Visible          := FALSE;
    end;
//    Self.OnResize := OnResizeForm;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeParameterForm.Initialise: boolean;
const OPNAME = 'TChangeParameterForm.Initialise';
begin
  Result := inherited Initialise;
  try
    FModelChangeLists.Clear;
    FAppModules.Changes.GetChangeListIDsInOrder(0, FModelChangeLists);
    FGrdParamChange.ColWidths[0] :=  40;
    FGrdParamChange.ColWidths[1] := 180;
    FGrdParamChange.ColWidths[2] :=  40;
    FGrdParamChange.ColWidths[3] := 100;
    FGrdParamChange.ColWidths[4] :=  60;
    FGrdParamChange.ColWidths[5] :=  60;
    FGrdParamChange.ColWidths[6] :=  50;
    FGrdParamChange.Width    := 530 + FGrdParamChange.ColCount + 4;
    FGrdParamChange.RowCount := 1 + FModelChangeLists.Count;
    FGrdParamChange.Height   := FGrdParamChange.RowCount * (FGrdParamChange.DefaultRowHeight + 1) + 4;
    FPnlChange.Left          := FGrdParamChange.ColWidths[0] + 3;
    FPnlChange.Top           := FGrdParamChange.Top + FGrdParamChange.DefaultRowHeight + 3;
    FPnlClient.Height        := FGrdParamChange.Top + FGrdParamChange.Height + 2;
    FPnlClient.Width         := FGrdParamChange.Width + 2;
    FChkShowAll.OnClick      := OnShowAllClick;
    Result := TRUE;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.Resize;
const OPNAME = 'TChangeParameterForm.Resize';
var
  LDiff,
  LNewHeight: integer;
begin
  inherited;
  try
    if(FGrdParamChange <> nil) then
    begin
      FGrdParamChange.Height   := FGrdParamChange.RowCount * (FGrdParamChange.DefaultRowHeight + 1) + 4;
      if((FGrdParamChange.Parent.Top + FGrdParamChange.Top + FGrdParamChange.Height) > Self.ClientHeight) then
      begin
         LDiff := (FGrdParamChange.Parent.Top + FGrdParamChange.Top + FGrdParamChange.Height) - Self.ClientHeight;
         LNewHeight := FGrdParamChange.Height - LDiff;
         if(LNewHeight < (FGrdParamChange.DefaultRowHeight + 1)) then
           LNewHeight := (FGrdParamChange.DefaultRowHeight + 1);
         FGrdParamChange.Height := LNewHeight;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.FormShow(Sender: TObject);
const OPNAME = 'TChangeParameterForm.FormShow';
begin
  try
    FInEditMode    := FALSE;
    FInNewMode     := FALSE;
    FSystemFlag    := FALSE;
    FChangeListID  := 0;
    FSystemFlag    := TRUE;
    FChkShowAll.Checked := FAppModules.Changes.ShowAllChangeLists;
    FSystemFlag := FALSE;
    PopulateComboBox;
    PopulateControls;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeParameterForm.SetData (AParamField : string;
                                        AKeyValues  : string;
                                        AFieldIndex : string);
const OPNAME = 'TChangeParameterForm.SetData';
var
  lStationID,
  lMonth,
  lYear          : Integer;
  LTempValue     : string;
begin
  try
    FFieldProperty := nil;
    FBaseValue     := '';
    FParamField    := Trim(AParamField);
    FFieldIndex    := AFieldIndex;
    FKeyValues     := AKeyValues;

    if (FParamField <> '') then
    begin
      FFieldProperty := FAppModules.FieldProperties.FieldProperty(FParamField);
      if (FAppModules.Model.ModelName = CRainfall) then
      begin
        lStationID     := StrToInt(GetKeyValue('StationID', AKeyValues));
        lYear          := StrToInt(GetKeyValue('Year', AKeyValues));
        lMonth         := STrToInt(AFieldIndex);

        if( FFieldProperty.FieldDataType = 1) then
          FBaseValue := Trim(FAppModules.Model.GetBaseValue(AParamField, AKeyValues, AFieldIndex))
        else
        begin
          LTempValue := Trim(FAppModules.Model.GetBaseValue(AParamField, AKeyValues, AFieldIndex));
          FBaseValue  := FloatToStr(GetPatchOriginalValue(lStationID,lYear,lMonth));
          if (Trim(LTempValue) <> '') then
          begin

            if((StrToFloat(LTempValue) = StrToFloat(FBaseValue)) or  (StrToFloat(FBaseValue) > StrToFloat(LTempValue)))then
              FBaseValue := FloatToStr(GetPatchOriginalValue(lStationID,lYear,lMonth))
            else
              FBaseValue := Trim(FAppModules.Model.GetBaseValue(AParamField, AKeyValues, AFieldIndex));
          end;
        end;
      end
      else
        FBaseValue := Trim(FAppModules.Model.GetBaseValue(AParamField, AKeyValues, AFieldIndex));

    end;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeParameterForm.OnResizeForm(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnResizeForm';
begin
  try
  except on E: Exception do HandleError(E, OPNAME); end;
end;

procedure TChangeParameterForm.DestroyMemberObjects;
const OPNAME = 'TChangeParameterForm.DestroyMemberObjects';
begin
  try
    FreeAndNil(FModelChangeLists);
    FFieldProperty := nil;
  except on E: Exception do HandleError(E, OPNAME); end;
end;

function TChangeParameterForm.LanguageHasChanged: boolean;
const OPNAME = 'TChangeParameterForm.LanguageHasChanged';
begin
  Result := inherited LanguageHasChanged;
  try
    Caption          := FAppModules.Language.GetString('ChangeLists.ParameterChanges');
    FBtnNew.Hint     := FAppModules.Language.GetString('ButtonHint.CPCreateNew');
    FBtnChange.Hint  := FAppModules.Language.GetString('ButtonHint.CPEdit');
    FBtnDelete.Hint  := FAppModules.Language.GetString('ButtonHint.CPDelete');
    FBtnOK.Hint      := FAppModules.Language.GetString('ButtonHint.CPOK');
    FBtnCancel.Hint  := FAppModules.Language.GetString('ButtonHint.CPCancel');
    FBtnClose.Hint   := FAppModules.Language.GetString('ButtonHint.CPClose');

    FLblParamName.Caption  := FAppModules.Language.GetString('ChangeLists.ParameterName') + ' :';
    FLblParamDescr.Caption := FAppModules.Language.GetString('ChangeLists.ParameterDescr') + ':';
    FLblEntityName.Caption := FAppModules.Language.GetString('ChangeLists.EntityDescr') + ' :';
    FLblBaseValue.Caption  := FAppModules.Language.GetString('ChangeLists.BaseValue') + ' :';
    FChkShowAll.Caption    := FAppModules.Language.GetString('ChangeLists.ShowAllChangeLists');
    FRdbPercentage.Caption := FAppModules.Language.GetString('ChangeLists.Percentage');
    FRdbAbsolut.Caption    := FAppModules.Language.GetString('ChangeLists.Absolut');

    FGrdParamChange.Cells[0,0] := FAppModules.Language.GetString('ChangeLists.Order');
    FGrdParamChange.Cells[1,0] := FAppModules.Language.GetString('ChangeLists.ChangeList');
    FGrdParamChange.Cells[2,0] := FAppModules.Language.GetString('ChangeLists.Active');
    FGrdParamChange.Cells[3,0] := FAppModules.Language.GetString('ChangeLists.AbsOrPerc');
    FGrdParamChange.Cells[4,0] := FAppModules.Language.GetString('ChangeLists.Change');
    FGrdParamChange.Cells[5,0] := FAppModules.Language.GetString('ChangeLists.BaseValue');
    FGrdParamChange.Cells[6,0] := FAppModules.Language.GetString('ChangeLists.Override');

  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeParameterForm.StudyDataHasChanged (AContext   : TChangeContext;
                                                   AFieldName : string;
                                                   AOldValue  : string;
                                                   ANewValue  : string): boolean;
const OPNAME = 'TChangeParameterForm.StudyDataHasChanged';
begin
  Result := False;
  try
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.FormClose (Sender     : TObject;
                                          var Action : TCloseAction);
const OPNAME = 'TChangeParameterForm.FormClose';
begin
  try
    ModalResult := mrOk;
    Action      := caFree;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.PopulateControls;
const OPNAME = 'TChangeParameterForm.PopulateControls';
var
  lID : string;
begin
  try
    if (FFieldProperty <> nil) then
    begin
      FEdtBaseValue.FieldProperty := FFieldProperty;
      FLblEntityNameVal.Caption   := FAppModules.Model.EntityDescription(FParamField, FKeyValues, FFieldIndex);
      if (FParamField = 'ReservoirPenalty') then
      begin
        lID := FAppModules.Changes.GetKeyValue('Identifier', FKeyValues);
        FLblParamNameVal.Caption := FParamField + '[' + lID + ']';
      end
      else
      if (FFieldIndex <> '') then
        FLblParamNameVal.Caption := FParamField + '[' + FFieldIndex + ']'
      else
        FLblParamNameVal.Caption := FParamField;
      FEdtBaseValue.Text         := FBaseValue;
      PopulateGrid;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.PopulateComboBox;
const OPNAME = 'TChangeParameterForm.PopulateComboBox';
var
  lChangeListID : integer;
  lChangeList   : IChangeList;
  lIndex        : integer;
begin
  try
    FCbxChangeList.Items.Clear;
    for lIndex := 0 to FModelChangeLists.Count - 1 do
    begin
      lChangeListID := StrToInt(FModelChangeLists.Strings[lIndex]);
      lChangeList   := FAppModules.Changes.ChangeListWithID(lChangeListID);
      FCbxChangeList.Items.AddObject(lChangeList.ChangeListName, TObject(lChangeList.ChangeListID));
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.PopulateGrid;
const OPNAME = 'TChangeParameterForm.PopulateGrid';
var
  lParamChange   : IParameterChange;
  lChangeListID  : integer;
  lChangeList    : IChangeList;
  lIndex         : integer;
  lCol           : integer;
  lOverride      : boolean;
  lSelectRow     : integer;
  lNewVal        : string;
  lListActive    : boolean;
begin
  try
    for lIndex := 1 to FGrdParamChange.RowCount-1 do
      for lCol := 0 to FGrdParamChange.ColCount-1 do
        FGrdParamChange.Cells[lCol, lIndex] := '';

    FRows      := 0;
    lSelectRow := 0;

    for lIndex := 0 to FModelChangeLists.Count - 1 do
    begin
      lChangeListID := StrToInt(FModelChangeLists.Strings[lIndex]);
      lChangeList   := FAppModules.Changes.ChangeListWithID(lChangeListID);
      lParamChange  := lChangeList.FindParamChange(FParamField, FKeyValues, FFieldIndex);
      if (lChangeList.IsResident) AND
         (FChkShowAll.Checked OR (lParamChange <> nil)) then
      begin
        lListActive := FAppModules.Changes.IsChangeListActive(lChangeListID);
        FRows := FRows + 1;
        FGrdParamChange.Objects[0,FRows] := TObject(lChangeListID);
        FGrdParamChange.Cells[0,FRows]   := IntToStr(FRows);
        FGrdParamChange.Cells[1,FRows]   := lChangeList.ChangeListName;
        if (lListActive) then
          FGrdParamChange.Cells[2,FRows] := '  Y'
        else
          FGrdParamChange.Cells[2,FRows] := '  N';
        if (lParamChange <> nil) then
        begin
          if (lParamChange.Absolut) then
            FGrdParamChange.Cells[3,FRows] := ' Absolute'
          else
            FGrdParamChange.Cells[3,FRows] := ' %';
          FGrdParamChange.Cells[4, FRows] := lParamChange.Change;
          if (NOT (FFieldProperty.FieldDataType in [FieldFloatType, FieldIntegerType])) then
          begin
            if ((lListActive) AND (lParamChange.Absolut)) then
              lNewVal := lParamChange.Change;
            FGrdParamChange.Cells[5, FRows] := lNewVal;
          end;
        end
        else
        begin
          FGrdParamChange.Cells[3, FRows] := '';
          FGrdParamChange.Cells[4, FRows] := '';
          FGrdParamChange.Cells[5, FRows] := '';
        end;
      end;
      if (FChangeListID = lChangeListID) then
        lSelectRow := FRows;
    end;

    case FFieldProperty.FieldDataType of
      FieldFloatType   : PopulateNewValFloat;
      FieldIntegerType : PopulateNewValInt;
    else
    end;

    lOverride := FALSE;
    lIndex := FRows;
    while (lIndex >= 1) do
    begin
      lChangeListID  := Integer(FGrdParamChange.Objects[0,lIndex]);
      lChangeList    := FAppModules.Changes.ChangeListWithID(lChangeListID);
      if (lChangeList.IsResident) then
      begin
        lParamChange   := lChangeList.FindParamChange(FParamField, FKeyValues, FFieldIndex);
        if ((lParamChange <> nil) AND lOverride) then
          FGrdParamChange.Cells[6, lIndex] := '  O'
        else
        begin
          FGrdParamChange.Cells[6, lIndex] := '';
          if (FAppModules.Changes.IsChangeListActive(lChangeList.ChangeListID)) AND
             (lParamChange <> nil) AND (lParamChange.Absolut) then
            lOverride := TRUE;
        end;
        lIndex := lIndex - 1;
      end;
    end;
    if (lSelectRow <> 0) then
      FGrdParamChange.Row := lSelectRow
    else if (FRows > 0) then
    begin
      FGrdParamChange.Row := 1;
      DoSelectCell(1);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.PopulateNewValFloat;
const OPNAME = 'TChangeParameterForm.PopulateNewValFloat';
var
  lParamChange   : IParameterChange;
  lChangeList    : IChangeList;
  lChangeListID  : integer;
  lIndex         : integer;
  lRealVal       : double;
  lNewVal        : string;
  lRow           : integer;
begin
  try
    lRealVal := 0;
    lRow     := 0;
    try lRealVal := StrToFloat(Trim(FBaseValue)); except; end;
    for lIndex := 0 to FModelChangeLists.Count - 1 do
    begin
      lChangeListID := StrToInt(FModelChangeLists.Strings[lIndex]);
      lChangeList   := FAppModules.Changes.ChangeListWithID(lChangeListID);
      lParamChange  := lChangeList.FindParamChange(FParamField, FKeyValues, FFieldIndex);
      if (lChangeList.IsResident) AND
         (FChkShowAll.Checked OR (lParamChange <> nil)) then
      begin
        lRow := lRow + 1;
        if (lParamChange <> nil) then
        begin
          if (FAppModules.Changes.IsChangeListActive(lChangeList.ChangeListID)) then
          begin
            try
              if (lParamChange.Absolut) then
                lRealVal := StrToFloat(Trim(lParamChange.Change))
              else
                lRealVal := (1 + StrToFloat(Trim(lParamChange.Change)) / 100) * lRealVal;
            except
            end;
          end;
          if (FFieldProperty.FormatStringGrid <> '') then
            lNewVal := Format(FFieldProperty.FormatStringGrid, [lRealVal])
          else
            lNewVal := FloatToStr(lRealVal);
          FGrdParamChange.Cells[5, lRow] := lNewVal;
        end
        else
          FGrdParamChange.Cells[5, lRow] := '';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.PopulateNewValInt;
const OPNAME = 'TChangeParameterForm.PopulateNewValInt';
var
  lParamChange   : IParameterChange;
  lChangeList    : IChangeList;
  lChangeListID  : integer;
  lIndex         : integer;
  lIntVal        : integer;
  lNewVal        : string;
  lRow           : integer;
begin
  try
    lRow    := 0;
    lIntVal := 0;
    try lIntVal := StrToInt(Trim(FBaseValue)); except; end;
    for lIndex := 0 to FModelChangeLists.Count - 1 do
    begin
      lChangeListID := StrToInt(FModelChangeLists.Strings[lIndex]);
      lChangeList   := FAppModules.Changes.ChangeListWithID(lChangeListID);
      lParamChange  := lChangeList.FindParamChange(FParamField, FKeyValues, FFieldIndex);
      if (lChangeList.IsResident) AND
         (FChkShowAll.Checked OR (lParamChange <> nil)) then
      begin
        lRow := lRow + 1;
        if (lParamChange <> nil) then
        begin
          if (FAppModules.Changes.IsChangeListActive(lChangeList.ChangeListID)) then
          begin
            try
              if (lParamChange.Absolut) then
                lIntVal := StrToInt(Trim(lParamChange.Change))
              else
                lIntVal := ROUND((1 + StrToFloat(Trim(lParamChange.Change)) / 100) * lIntVal);
            except
            end;
          end;
          if (FFieldProperty.FormatStringGrid <> '') then
            lNewVal := Format(FFieldProperty.FormatStringGrid, [lIntVal])
          else
            lNewVal := IntToStr(lIntVal);
          FGrdParamChange.Cells[5, lRow] := lNewVal;
        end
        else
          FGrdParamChange.Cells[5, lRow] := '';
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnShowAllClick(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnShowAllClick';
begin
  try
    if (NOT FSystemFlag) then
    begin
      FAppModules.Changes.ShowAllChangeLists := FChkShowAll.Checked;
      PopulateGrid;
      ResetControls;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnFormCloseQuery (Sender : TObject;
                                                 var CanClose: Boolean);
const OPNAME = 'TChangeParameterForm.OnFormCloseQuery';
begin
  try
    CanClose := NOT FInEditMode;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.GrdChangeParamsDrawCell (Sender     : TObject;
                                                        ACol, ARow : Integer;
                                                        Rect       : TRect;
                                                        State      : TGridDrawState);
const OPNAME = 'TChangeParameterForm.GrdChangeParamsDrawCell';
var
  lGrid   : TStringGrid;
  lStrVal : string;
  lLeft   : integer;
  lTop    : integer;
begin
  try
    lGrid := TStringGrid(Sender);
    if (ARow > 0) then
    begin
      if (ACol = 2) then
      begin
        lStrVal := Trim(lGrid.Cells[ACol, ARow]);
        lTop    := (ARow * (lGrid.DefaultRowHeight + 1)) + 4;
        lLeft   := lGrid.ColWidths[0] + lGrid.ColWidths[1] + 5;
        if (lStrVal = 'Y') then
          lGrid.Canvas.Draw(lLeft, lTop, FImgActive.Picture.Bitmap)
        else if (lStrVal = 'N') then
          lGrid.Canvas.Draw(lLeft, lTop, FImgInActive.Picture.Bitmap);
      end
      else if (ACol = 6) then
      begin
        lStrVal := Trim(lGrid.Cells[ACol, ARow]);
        lTop    := (ARow * (lGrid.DefaultRowHeight + 1)) + 4;
        lLeft   := lGrid.ColWidths[0] + lGrid.ColWidths[1] + lGrid.ColWidths[2] +
                   lGrid.ColWidths[3] + lGrid.ColWidths[4] + lGrid.ColWidths[5] + 8;
        if (lStrVal = 'O') then
          lGrid.Canvas.Draw(lLeft, lTop, FImgOverride.Picture.Bitmap)
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.GrdChangeParamsSelectCell (Sender : TObject;
                                                          ACol, ARow: Integer;
                                                          var CanSelect: Boolean);
const OPNAME = 'TChangeParameterForm.GrdChangeParamsSelectCell';
begin
  try
    CanSelect := NOT FInEditMode;
    if (NOT FInEditMode) then
    begin
      DoSelectCell(ARow);
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.DoSelectCell (ARow : integer);
const OPNAME = 'TChangeParameterForm.DoSelectCell';
begin
  try
    FChangeListID  := Integer(FGrdParamChange.Objects[0,ARow]);
    ResetControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.ResetControls;
const OPNAME = 'TChangeParameterForm.ResetControls';
var
  lParamChange : IParameterChange;
  lChangeList  : IChangeList;
  lIndex       : integer;
  LUpdateUser  : boolean;
  lListActive  : boolean;
begin
  try
    LUpdateUser := (FAppModules.User <> nil) and (FAppModules.User.UserRights in CUR_EditData);
    LUpdateUser := LUpdateUser and  (FAppModules.StudyArea <> nil) and (not(FAppModules.StudyArea.ScenarioLocked));
    lChangeList := FAppModules.Changes.ChangeListWithID(FChangeListID);
    if (lChangeList <> nil) then
      lParamChange := lChangeList.FindParamChange(FParamField, FKeyValues, FFieldIndex)
    else
      lParamChange := nil;

    FBtnNew.Enabled    := LUpdateUser and (NOT FInEditMode) AND (NOT FChkShowAll.Checked) AND
                          (FRows < FModelChangeLists.Count);
    FBtnChange.Enabled := LUpdateUser and (NOT FInEditMode) AND (FChkShowAll.Checked OR (lParamChange <> nil));
    FBtnDelete.Enabled := LUpdateUser and (NOT FInEditMode) AND (lParamChange <> nil);
    FBtnOK.Enabled     := FInEditMode;
    FBtnCancel.Enabled := FInEditMode;
    FBtnClose.Enabled  := (NOT FInEditMode);

    FGrdParamChange.Enabled := LUpdateUser and NOT FInEditMode;
    FChkShowAll.Enabled     := NOT FInEditMode;
    FEdtBaseValue.Enabled   := LUpdateUser AND (NOT FInEditMode) AND
                               (FAppModules.Model.ModelName <> CRainfall);
    FPnlChange.Visible      := FInEditMode;
    if (FInEditMode) then
    begin
      FPnlChange.Left := FGrdParamChange.ColWidths[0] + 3;
      FPnlChange.Top  := FGrdParamChange.Top + 2 +
                        ((FGrdParamChange.DefaultRowHeight + 1) * FGrdParamChange.Row);
      FCbxChangeList.Visible := NOT FChkShowAll.Checked;
      FLblChangeList.Visible := FChkShowAll.Checked;
      if (FChkShowAll.Checked) then
        FLblChangeList.Caption := lChangeList.ChangeListName
      else
      begin
        lIndex := FCbxChangeList.Items.IndexOfObject(TObject(FChangeListID));
        FCbxChangeList.ItemIndex := lIndex;
      end;
      if (lChangeList <> nil) then
      begin
        lListActive := FAppModules.Changes.IsChangeListActive(lChangeList.ChangeListID);
        FImgActive.Visible   := lListActive;
        FImgInactive.Visible := NOT lListActive;
      end;
      if (FFieldProperty.FieldDataType in [FieldFloatType, FieldIntegerType]) AND
         (NOT (FFieldProperty.FieldGroup in [fgValidation, fgValidationArray])) then
      begin
        FRdbPercentage.Enabled := TRUE;
        FRdbAbsolut.Enabled    := TRUE;
      end
      else
      begin
        FRdbPercentage.Enabled := FALSE;
        FRdbPercentage.Enabled := FALSE;
        if (FFieldProperty.FieldGroup in [fgValidation, fgValidationArray]) then
        begin
          FCbxChange.Visible := TRUE;
          FEdtChange.Visible := FALSE;
          //FEdtDescr.Visible := FALSE;
          FCbxChange.Clear;
          FCbxChange.Items.CommaText := FFieldProperty.FieldAcceptedValues;
        end
        else
        begin
          FEdtChange.Visible := TRUE;
          //FEdtDescr.Visible := TRUE;
          FCbxChange.Visible := FALSE;
        end;
      end;
      if (lParamChange <> nil) then
      begin
        FRdbAbsolut.Checked    := lParamChange.Absolut;
        FRdbPercentage.Checked := NOT lParamChange.Absolut;
        if (FEdtChange.Visible) then
          FEdtChange.Text := lParamChange.Change;
        if FEdtDescr.Visible then
           FEdtDescr.Text := lParamChange.ParamDescr; 
        if (FCbxChange.Visible) then
        begin
          if (lParamChange.Change = '') then
            FCbxChange.ItemIndex := FCbxChange.Items.IndexOf('None')
          else
            FCbxChange.ItemIndex := FCbxChange.Items.IndexOf(lParamChange.Change);
        end;
      end
      else
      begin
        if (FFieldProperty.FieldDataType in [FieldFloatType, FieldIntegerType]) AND
           (NOT (FFieldProperty.FieldGroup in [fgValidation, fgValidationArray])) then
          FRdbPercentage.Checked := TRUE
        else
          FRdbAbsolut.Checked    := TRUE;
        if (FEdtChange.Visible) then
          FEdtChange.Text := '';
        if(FEdtDescr.Visible) then
          FEdtDescr.Text := '';
        if (FCbxChange.Visible) then
          FCbxChange.ItemIndex := -1;  
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnNewClick(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnNewClick';
begin
  try
    FRows := FRows + 1;
    FGrdParamChange.Row := FRows;
    FInEditMode := TRUE;
    FInNewMode  := TRUE;
    ResetControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnChangeClick(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnChangeClick';
var
  lParamChange : IParameterChange;
  lChangeList  : IChangeList;
begin
  try
    lChangeList := FAppModules.Changes.ChangeListWithID(FChangeListID);
    if (lChangeList <> nil) then
      lParamChange := lChangeList.FindParamChange(FParamField, FKeyValues, FFieldIndex)
    else
      lParamChange := nil;
    if (lParamChange = nil) then
    begin
      FInEditMode := TRUE;
      FInNewMode  := TRUE;
      ResetControls;
    end
    else
    begin
      FInEditMode := TRUE;
      ResetControls;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnCancelClick(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnCancelClick';
begin
  try
    if (FInNewMode) then
    begin
      if (NOT FChkShowAll.Checked) then
        FRows := FRows - 1;
    end;
    FInEditMode := FALSE;
    FInNewMode  := FALSE;
    ResetControls;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeParameterForm.InputValid (var AChangeList : IChangeList;
                                          var AAbsolut    : string;
                                          var AChange     : string;
                                          var AParamDescr : string): Boolean;
const OPNAME = 'TChangeParameterForm.InputValid';
var
  lParamChange : IParameterChange;
  lIndex       : integer;
  lMessage     : string;
  lChangeListID : integer;
begin
  Result := FALSE;
  try
    Result   := TRUE;
    if (FRdbAbsolut.Checked) then
      AAbsolut := 'Y'
    else
      AAbsolut := 'N';
    if (FChkShowAll.Checked) then
    begin
      AChangeList := FAppModules.Changes.ChangeListWithID(FChangeListID);
    end
    else
    begin
      lIndex := FCbxChangeList.ItemIndex;
      if (lIndex < 0) then
      begin
        Result := FALSE;
        ShowMessage(FAppModules.Language.GetString('ChangeLists.PleaseSelectChangeList'));
      end
      else
      begin
        lChangeListID := Integer(FCbxChangeList.Items.Objects[lIndex]);
        AChangeList   := FAppModules.Changes.ChangeListWithID(lChangeListID);
        if (AChangeList.ChangeListID <> FChangeListID) then
        begin
          lParamChange := AChangeList.FindParamChange(FParamField, FKeyValues, FFieldIndex);
          if (lParamChange <> nil) then
          begin
            Result := FALSE;
            ShowMessage(Format(FAppModules.Language.GetString('ChangeLists.AlreadyContains'),
                               [AChangeList.ChangeListName, FParamField]));
          end;
        end;
      end;
    end;
    if (Result) then
    begin
      if (FEdtChange.Visible) then
        AChange := FEdtChange.Text
      else
        AChange := FCbxChange.Text;
      if(FEdtDescr.Visible) then
        AParamDescr := FEdtDescr.Text
      else
        AParamDescr := '';
      if (FRdbAbsolut.Checked) then
      begin
        Result := ValidateParamField(AChange, lMessage);
        if (NOT Result) then
          ShowMessage(lMessage);
      end
      else
      begin
        if (NOT FAppModules.FieldProperties.ValidateFieldProperty('ChangeParamChangePerc', AChange, lMessage)) then
        begin
          Result := FALSE;
          ShowMessage(lMessage);
        end;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeParameterForm.ValidateParamField (AValue       : string;
                                                  var lMessage : string) : Boolean;
const OPNAME = 'TChangeParameterForm.ValidateParamField';
var
  lDim1Idx     : integer;
  lDim2Idx     : integer;
begin
  Result := FALSE;
  try
    Result := TRUE;
    if (Trim(FFieldIndex) = '') then
    begin
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty(FParamField, AValue, lMessage)) then
        Result := FALSE;
    end
    else
    begin
      FAppModules.Changes.GetIndexes(FFieldIndex, lDim1Idx, lDim2Idx);
      if (NOT FAppModules.FieldProperties.ValidateFieldProperty(FParamField, AValue, lMessage, lDim1Idx, lDim2Idx)) then
        Result := FALSE;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnOKClick(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnOKClick';
var
  lChange       : string;
  lAbsolut      : string;
  LParamDescr   : string;
  lChangeList   : IChangeList;
  lSelectedList : IChangeList;
  lParamChange  : IParameterChange;
begin
  try
    if (InputValid(lSelectedList, lAbsolut, lChange,LParamDescr)) then
    begin
      if (lChange = 'None') then
        lChange := '';
      if (FInNewMode) then
      begin
        lSelectedList.CreateNewParamChange(FParamField, FKeyValues, FFieldIndex, lAbsolut, lChange,LParamDescr,False);
        FChangeListID := lSelectedList.ChangeListID;
        FInNewMode  := FALSE;
      end
      else
      begin
        lChangeList := FAppModules.Changes.ChangeListWithID(FChangeListID);
        if ((lChangeList <> nil) AND (lChangeList <> lSelectedList)) then
        begin
          lChangeList.DeleteParamChange(FParamField, FKeyValues, FFieldIndex);
          FChangeListID := lSelectedList.ChangeListID;
          lSelectedList.CreateNewParamChange(FParamField, FKeyValues, FFieldIndex, lAbsolut, lChange,LParamDescr,False);
        end
        else
        begin
          lParamChange := lChangeList.FindParamChange(FParamField, FKeyValues, FFieldIndex);
          if (((lParamChange.Absolut) AND (lAbsolut = 'N')) OR
              ((NOT lParamChange.Absolut) AND (lAbsolut = 'Y'))) then
            lParamChange.Absolut := (lAbsolut = 'Y');
          if (lParamChange.Change <> lChange) then
            lParamChange.Change  := lChange;
          if (lParamChange.ParamDescr <> LParamDescr) then
            lParamChange.ParamDescr  := LParamDescr;
        end;
      end;
      FInEditMode := FALSE;
      PopulateGrid;
      ResetControls;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnDeleteClick(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnDeleteClick';
var
  lChangeList  : IChangeList;
begin
  try
    lChangeList := FAppModules.Changes.ChangeListWithID(FChangeListID);
    if (lChangeList <> nil) then
    begin
      lChangeList.DeleteParamChange(FParamField, FKeyValues, FFieldIndex);
      PopulateGrid;
      ResetControls;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnEdtBaseValueExit(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnEdtBaseValueExit';
var
  lValue   : string;
  lMessage : string;
begin
  try
    lValue := Trim(FEdtBaseValue.Text);
    if (ValidateParamField(lValue, lMessage)) then
    begin
      if (FAppModules.Model.SetBaseValue(FParamField, FKeyValues, FFieldIndex, lValue)) then
      begin
        FBaseValue := lValue;
        PopulateControls;
      end;
    end
    else
    begin
      ShowMessage(lMessage);
      FEdtBaseValue.Text := FBaseValue;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

procedure TChangeParameterForm.OnCloseClick(Sender: TObject);
const OPNAME = 'TChangeParameterForm.OnCloseClick';
begin
  try
    Close;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function TChangeParameterForm.GetPatchOriginalValue(AStationID: Integer;
                                 AHydroYear, AMonthIndex: Integer): double;
const OPNAME = 'TChangeParameterForm.GetPatchOriginalValue';
var
  LStation      : IStationData;
  lRainfallData : IRainfallData;
  lYearlyData   : IYearlyData;
begin
  Result := -1;
  try
    lStation := (FAppModules.Model.ModelData as IRainfallModelData).
                 GetStationDataByID(AStationID);
    lRainfallData := lStation.RainfallData;
    lYearlyData   := lRainfallData.GetHydroYearDataByYear(AHydroYear);
    if  lYearlyData <> nil then
      Result := lYearlyData.MonthlyRainfall[AMonthIndex];
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TChangeParameterForm.GetKeyValue(AKeyName, AKeyValues: string): string;
const OPNAME = 'TChangeParameterForm.GetKeyValue';
var
  lPos : integer;
begin
  Result := '';
  try
    lPos := Pos(AKeyName, AKeyValues);
    if (lPos > 0) then
    begin
      AKeyValues := Copy(AKeyValues, lPos, Length(AKeyValues) - lPos + 1);
      lPos       := Pos('=', AKeyValues);
      if (lPos > 0) then
      begin
        AKeyValues := Copy(AKeyValues, lPos+1, Length(AKeyValues) - lPos);
        lPos       := Pos(',', AKeyValues);
        if (lPos > 0) then
          Result := Copy(AKeyValues, 1, lPos - 1)
        else
          Result := AKeyValues;
      end;
    end;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
