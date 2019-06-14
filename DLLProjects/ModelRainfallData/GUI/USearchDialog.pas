{******************************************************************************}
{*  UNIT      : Contains TRGSearchDialog Class                                *}
{*  AUTHOR    : Reworked by RH Steyn                                          *}
{*  DATE      : 2006/03/16                                                    *}
{*  COPYRIGHT : Copyright © 2006 DWAF                                         *}
{******************************************************************************}

unit USearchDialog;

interface

uses
  Classes,
  VCL.Forms,
  VCL.StdCtrls,
  SysUtils,
  VCL.Samples.Spin,
  VCL.ExtCtrls,
  UAbstractComponent;

type
  TSearchFormType = (stStationNumber, stStationName, stRectangle, stDistance);

  TRGSearchDialog = class (TAbstractForm)
  protected
    FLblNumber        : TLabel;
    FEdtNumber        : TEdit;

    FLblName          : TLabel;
    FEdtName          : TEdit;
    FLblNamePost      : TLabel;

    FRgpRectangle     : TRadiogroup;
    FSpnTopLatDeg     : TSpinEdit;
    FSpnTopLatMin     : TSpinEdit;
    FSpnLeftLongDeg   : TSpinEdit;
    FSpnLeftLongMin   : TSpinEdit;
    FSpnBottomLatDeg  : TSpinEdit;
    FSpnBottomLatMin  : TSpinEdit;
    FSpnRightLongDeg  : TSpinEdit;
    FSpnRightLongMin  : TSpinEdit;
    FEdtTopLat        : TEdit;
    FEdtLeftLong      : TEdit;
    FEdtBottomLat     : TEdit;
    FEdtRightLong     : TEdit;
    FLblTopLat        : TLabel;
    FLblLeftLong      : TLabel;
    FLblBottomLat     : TLabel;
    FLblRightLong     : TLabel;

    FRgpDistance      : TRadiogroup;
    FLblStationNr     : TLabel;
    FEdtStationNr     : TEdit;
    FLblLat           : TLabel;
    FLblLong          : TLabel;
    FSpnLatDeg        : TSpinEdit;
    FSpnLatMin        : TSpinEdit;
    FSpnLongDeg       : TSpinEdit;
    FSpnLongMin       : TSpinEdit;
    FLblDistance      : TLabel;
    FEdtDistance      : TEdit;

    FBtnOK            : TButton;
    FBtnCancel        : TButton;
    procedure SetButtonPositions;
    procedure SetFormHeight(ASearchFormType : TSearchFormType);
    procedure ButtonCanselClick ( Sender : TObject );
    procedure ButtonOKClick ( Sender : TObject);
    procedure EditOnKeyPress(Sender: TObject; var Key: Char);
    procedure GaugeNrOnKeyPress(Sender: TObject; var Key: Char);
    procedure CreateMemberObject(ASearchFormType : TSearchFormType; AApplicationTitle : string );
    procedure CreateMemberObjects; override;
  private
    FSearchType    : TSearchFormType;
    FLatitude      : double;
    FLongitude     : double;
    FDistance      : double;
    function GetStationName : string;
    function GetGaugeNumber : string;
    function GetTopValue : integer;
    function GetLeftValue : integer;
    function GetBottomValue : integer;
    function GetRightValue : integer;
    function CreateStStationNumber : boolean;
    function CreateStStationName : boolean;
    function CreateStRectangle : boolean;
    function CreateStDistance : boolean;
    procedure RadioGroupStRectangleOnClick(Sender : TObject);
    procedure RadioGroupStDistanceOnClick(Sender : TObject);
    function ValidateDistance : boolean;
  public
    destructor Destroy; override;
    procedure ShowForm(ASearchFormType : TSearchFormType);
    procedure ShowFormModal(ASearchFormType : TSearchFormType);
    procedure SetSearchFormType ( ASearchFormType : TSearchFormType; AApplicationTitle : string );
    procedure HideForm;
    property TopValue    : integer  read GetTopValue;
    property LeftValue   : integer  read GetLeftValue;
    property RightValue  : integer  read GetRightValue;
    property BottomValue : integer  read GetBottomValue;
    property Distance    : double   read FDistance;
    property Latitude    : double   read FLatitude;
    property Longitude   : double   read FLongitude;
    property StationName : string   read GetStationName;
    property GaugeNumber : string   read GetGaugeNumber;

  end;

implementation

uses
  VCL.Controls,
  VCL.Dialogs,
  RainfallCom_TLB,
  UErrorHandlingOperations;

{ TRGSearchDialog }

procedure TRGSearchDialog.CreateMemberObject(ASearchFormType : TSearchFormType; AApplicationTitle : string);
const OPNAME = 'TRGSearchDialog.CreateMemberObject';
begin
  try
    Self.BorderStyle := bsDialog;
    Self.Position    := poScreenCenter;
    Self.Caption     := AApplicationTitle;
    SetFormHeight(ASearchFormType);
    case ASearchFormType of
      StStationNumber : CreateStStationNumber;
      StStationName   : CreateStStationName;
      stRectangle     : CreateStRectangle;
      StDistance      : CreateStDistance;
    end;

    FBtnOK := TButton.Create(nil);
    with FBtnOK do
    begin
      Parent := Self;
      Caption := FAppModules.Language.GetString('ButtonCaption.&Ok');
      OnClick := ButtonOKClick;
      Default := TRUE;
    end;

    FBtnCancel := TButton.Create(nil);
    with FBtnCancel do
    begin
      Parent := Self;
      Caption := FAppModules.Language.GetString('ButtonCaption.&Cancel');
      OnClick := ButtonCanselClick;
    end;

    FDistance      := 0;
    FLatitude      := 0;
    FLongitude     := 0;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

destructor TRGSearchDialog.Destroy;
const OPNAME = 'TRGSearchDialog.Destroy';
begin
  try
    HideForm;
    DestroyMemberObjects;
    inherited Destroy;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRGSearchDialog.EditOnKeyPress(Sender: TObject; var Key: Char);
const OPNAME = 'TRGSearchDialog.EditOnKeyPress';
begin
  try
    if not CharInSet(Key,['0'..'9','.', '-',#27,#8]) then
      Key := #0
    else
    begin
      if (Key = '.') and (Pos('.', (Sender As TEdit).Text) > 0)  then
        Key := #0;
      if (Key = '-') and (Pos('-', (Sender As TEdit).Text) > 0)  then
        Key := #0;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGSearchDialog.GaugeNrOnKeyPress (Sender : TObject; var Key : Char);
const OPNAME = 'TRGSearchDialog.GaugeNrOnKeyPress';
begin
  try
    if not CharInSet(Key,['0'..'9', ' ', 'A'..'Z', #27, #8]) then
      Key := #0;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGSearchDialog.GetBottomValue : integer;
const OPNAME = 'TRGSearchDialog.GetBottomValue';
begin
  Result := 0;
  try
    case FRgpRectangle.ItemIndex of
      0 : if Assigned(FSpnBottomLatDeg) and Assigned(FSpnBottomLatMin) then
          begin
            if (FSpnBottomLatDeg.Value >= 0) then
              Result := FSpnBottomLatDeg.Value * 60 + FSpnBottomLatMin.Value
            else
              Result := FSpnBottomLatDeg.Value * 60 - FSpnBottomLatMin.Value;
          end;
      1 : if Assigned(FEdtBottomLat) then
          try
            Result := Trunc(StrToFloat(FEdtBottomLat.Text) * 60);
          except
            Result  := 0;
          end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGSearchDialog.GetLeftValue : integer;
const OPNAME = 'TRGSearchDialog.GetLeftValue';
begin
  Result := 0;
  try
    case FRgpRectangle.ItemIndex of
      0 : if Assigned(FSpnLeftLongDeg) and Assigned(FSpnLeftLongMin) then
          begin
            if (FSpnLeftLongDeg.Value >= 0) then
              Result := FSpnLeftLongDeg.Value * 60 + FSpnLeftLongMin.Value
            else
              Result := FSpnLeftLongDeg.Value * 60 - FSpnLeftLongMin.Value;
          end;
      1 : if Assigned(FEdtLeftLong) then
          try
            Result := Trunc(StrToFloat(FEdtLeftLong.Text) * 60);
          except
            Result  := 0;
          end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGSearchDialog.GetStationName : string;
const OPNAME = 'TRGSearchDialog.GetStationName';
begin
  Result := '';
  try
    if (FSearchType = stStationName) then
      Result := FEdtName.Text;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGSearchDialog.GetGaugeNumber : string;
const OPNAME = 'TRGSearchDialog.GetGaugeNumber';
begin
  Result := '';
  try
    case FSearchType of
      stStationNumber :
        begin
          Result := FEdtNumber.Text;
        end;
      stDistance :
        begin
          if (FRgpDistance.ItemIndex = 1) then
            Result := '<Pos></Pos>' + FEdtStationNr.Text
        end;
      else
      end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGSearchDialog.GetRightValue : integer;
const OPNAME = 'TRGSearchDialog.GetRightValue';
begin
  Result := 0;
  try
    case FRgpRectangle.ItemIndex of
      0 : if Assigned(FSpnRightLongDeg) and Assigned(FSpnRightLongMin) then
          begin
            if (FSpnRightLongDeg.Value >= 0) then
              Result := FSpnRightLongDeg.Value * 60 + FSpnRightLongMin.Value
            else
              Result := FSpnRightLongDeg.Value * 60 - FSpnRightLongMin.Value;
          end;
      1 : if Assigned(FEdtRightLong) then
          try
            Result := Trunc(StrToFloat(FEdtRightLong.Text) * 60);
          except
            Result  := 0;
          end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGSearchDialog.GetTopValue: integer;
const OPNAME = 'TRGSearchDialog.GetTopValue';
begin
  Result := 0;
  try
    case FRgpRectangle.ItemIndex of
      0 : if Assigned(FSpnTopLatDeg) and Assigned(FSpnTopLatMin) then
          begin
            if (FSpnTopLatDeg.Value >= 0) then
              Result := FSpnTopLatDeg.Value * 60 + FSpnTopLatMin.Value
            else
              Result := FSpnTopLatDeg.Value * 60 - FSpnTopLatMin.Value;
          end;
      1 : if Assigned(FEdtTopLat) then
          try
            Result := Trunc(StrToFloat(FEdtTopLat.Text) * 60);
          except
            Result  := 0;
          end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGSearchDialog.HideForm;
const OPNAME = 'TRGSearchDialog.HideForm';
begin
  try
    if Assigned(Self) then
      Self.Hide;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGSearchDialog.SetButtonPositions;
const OPNAME = 'TRGSearchDialog.SetButtonPositions';
begin
  try
    if Assigned(Self) then
    begin
      FBtnOK.Left     := (Self.ClientWidth - 2 * FBtnOK.Width - 10);
      FBtnOK.Top      := (Self.ClientHeight - FBtnOK.Height - 5);
      FBtnCancel.Left := (Self.ClientWidth - FBtnCancel.Width - 5);
      FBtnCancel.Top  := (Self.ClientHeight - FBtnCancel.Height - 5);
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGSearchDialog.SetFormHeight(ASearchFormType: TSearchFormType);
const OPNAME = 'TRGSearchDialog.SetFormHeight';
begin
  try
    if Assigned(Self) then
    begin
      case ASearchFormType of
        StDistance      : begin
                            Self.Height := 150;
                            Self.Width  := 400;
                          end;
        stRectangle     : begin
                            Self.Height := 175;
                            Self.Width := 435;
                          end;
        StStationNumber : begin
                            Self.Height := 95;
                            Self.Width := 300;
                          end;
        StStationName   : begin
                            Self.Height := 120;
                            Self.Width := 300;
                          end;
      end;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGSearchDialog.ShowForm(ASearchFormType : TSearchFormType);
const OPNAME = 'TRGSearchDialog.ShowForm';
begin
  try
    if Assigned(Self) then
    begin
      SetFormHeight(ASearchFormType);
      SetButtonPositions;
      Self.Show;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

procedure TRGSearchDialog.ShowFormModal(ASearchFormType : TSearchFormType);
const OPNAME = 'TRGSearchDialog.ShowFormModal';
begin
  try
    if Assigned ( Self ) then
    begin
      SetFormHeight(ASearchFormType);
      SetButtonPositions;
      Self.ShowModal;
    end;
  except on E:Exception do HandleError(E,OPNAME) end;
end;

function TRGSearchDialog.CreateStStationNumber: boolean;
const OPNAME = 'TRGSearchDialog.CreateStStationNumber';
begin
  Result := FALSE;
  try
    FLblNumber := TLabel.Create(nil);
    with FLblNumber do
    begin
      Parent    := Self;
      Alignment := taLeftJustify;
      Caption   := FAppModules.Language.GetString('LabelText.SearchStationNumber');
      Top       := 10;
      Left      := 10;
    end;

    FEdtNumber := TEdit.Create(nil);
    with FEdtNumber do
    begin
      Parent    := Self;
      Width     := 100;
      MaxLength := 20;
      Top       := 30;
      Left      := 10;
    end;

    Result := TRUE;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRGSearchDialog.CreateStStationName: boolean;
const OPNAME = 'TRGSearchDialog.CreateStStationName';
begin
  Result := FALSE;
  try
    FLblName := TLabel.Create(nil);
    with FLblName do
    begin
      Parent    := Self;
      Alignment := taLeftJustify;
      Caption   := FAppModules.Language.GetString('LabelText.SearchStationName');
      Top       := 10;
      Left      := 10;
    end;

    FEdtName := TEdit.Create(nil);
    with FEdtName do
    begin
      Parent    := Self;
      Width     := 100;
      MaxLength := 20;
      Top       := 30;
      Left      := 10;
    end;

    Result := TRUE;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRGSearchDialog.CreateStRectangle: boolean;
const OPNAME = 'TRGSearchDialog.CreateStRectangle';
begin
  Result := false;
  try
    FLblTopLat := TLabel.Create(nil);
    with FLblTopLat do
    begin
      Parent    := Self;
      Left      := 10;
      Top       := 10;
      Width     := 160;
      Alignment := taRightJustify;
      Caption   := FAppModules.Language.GetString('Rainfall.TopLeftLatitude');
    end;

    FLblLeftLong := TLabel.Create(nil);
    with FLblLeftLong do
    begin
      Parent    := Self;
      Left      := 10;
      Top       := 35;
      Width     := 160;
      Alignment := taRightJustify;
      Caption   := FAppModules.Language.GetString('Rainfall.TopLeftLongitude');
    end;

    FLblBottomLat := TLabel.Create(nil);
    with FLblBottomLat do
    begin
      Parent    := Self;
      Left      := 10;
      Top       := 60;
      Width     := 160;
      Alignment := taRightJustify;
      Caption   := FAppModules.Language.GetString('Rainfall.BottomRightLatitude');
    end;

    FLblRightLong := TLabel.Create(nil);
    with FLblRightLong do
    begin
      Parent    := Self;
      Left      := 10;
      Top       := 85;
      Width     := 160;
      Alignment := taRightJustify;
      Caption   := FAppModules.Language.GetString('Rainfall.BottomRightLongitude');
    end;

    FSpnTopLatDeg    := TSpinEdit.Create(nil);
    with FSpnTopLatDeg do
    begin
      Parent    := Self;
      Left      := 170;
      Top       := 10;
      Width     := 45;
      MinValue  := -180;
      MaxValue  := 180;
      MaxLength := 4;
    end;

    FSpnTopLatMin    := TSpinEdit.Create(nil);
    with FSpnTopLatMin do
    begin
      Parent    := Self;
      Left      := 215;
      Top       := 10;
      Width     := 40;
      MinValue  := 0;
      MaxValue  := 59;
      MaxLength := 2;
    end;

    FSpnLeftLongDeg  := TSpinEdit.Create(nil);
    with FSpnLeftLongDeg do
    begin
      Parent    := Self;
      Left      := 170;
      Top       := 35;
      Width     := 45;
      MinValue  := -180;
      MaxValue  := 180;
      MaxLength := 4;
    end;

    FSpnLeftLongMin  := TSpinEdit.Create(nil);
    with FSpnLeftLongMin do
    begin
      Parent    := Self;
      Left      := 215;
      Top       := 35;
      Width     := 40;
      MinValue  := 0;
      MaxValue  := 59;
      MaxLength := 2;
    end;

    FSpnBottomLatDeg := TSpinEdit.Create(nil);
    with FSpnBottomLatDeg do
    begin
      Parent    := Self;
      Left      := 170;
      Top       := 60;
      Width     := 45;
      MinValue  := -180;
      MaxValue  := 180;
      MaxLength := 4;
    end;

    FSpnBottomLatMin := TSpinEdit.Create(nil);
    with FSpnBottomLatMin do
    begin
      Parent    := Self;
      Left      := 215;
      Top       := 60;
      Width     := 40;
      MinValue  := 0;
      MaxValue  := 59;
      MaxLength := 2;
    end;

    FSpnRightLongDeg := TSpinEdit.Create(nil);
    with FSpnRightLongDeg do
    begin
      Parent    := Self;
      Left      := 170;
      Top       := 85;
      Width     := 45;
      MinValue  := -180;
      MaxValue  := 180;
      MaxLength := 4;
    end;

    FSpnRightLongMin := TSpinEdit.Create(nil);
    with FSpnRightLongMin do
    begin
      Parent    := Self;
      Left      := 215;
      Top       := 85;
      Width     := 40;
      MinValue  := 0;
      MaxValue  := 59;
      MaxLength := 2;
    end;

    FEdtTopLat    := TEdit.Create(nil);
    with FEdtTopLat do
    begin
      Parent     := Self;
      Left       := 170;
      Top        := 10;
      Width      := 50;
      MaxLength  := 8;
      Text       := '0';
      OnKeyPress := EditOnKeyPress;
    end;

    FEdtLeftLong  := TEdit.Create(nil);
    with FEdtLeftLong do
    begin
      Parent     := Self;
      Left       := 170;
      Top        := 35;
      Width      := 50;
      MaxLength  := 8;
      Text       := '0';
      OnKeyPress := EditOnKeyPress;
    end;

    FEdtBottomLat := TEdit.Create(nil);
    with FEdtBottomLat do
    begin
      Parent     := Self;
      Left       := 170;
      Top        := 60;
      Width      := 50;
      MaxLength  := 8;
      Text       := '0';
      OnKeyPress := EditOnKeyPress;
    end;

    FEdtRightLong := TEdit.Create(nil);
    with FEdtRightLong do
    begin
      Parent     := Self;
      Left       := 170;
      Top        := 85;
      Width      := 50;
      MaxLength  := 8;
      Text       := '0';
      OnKeyPress := EditOnKeyPress;
    end;

    FRgpRectangle := TRadioGroup.Create(nil);
    with FRgpRectangle do
    begin
      Parent    := Self;
      Width     := 160;
      Height    := 60;
      Top       := 10;
      Left      := 265;
      OnClick   := RadioGroupStRectangleOnClick;
      Caption   := FAppModules.Language.GetString('Rainfall.EnterCoordinatesin');
      Items.Clear;
      Items.Add('Degrees and minutes    ');
      Items.Add('Decimal degrees +-180.00)');
      ItemIndex := 0;
     end;
     Result := TRUE;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

function TRGSearchDialog.CreateStDistance : boolean;
const OPNAME = 'TRGSearchDialog.CreateStDistance';
begin
  Result := FALSE;
  try
    FLblStationNr     := TLabel.Create(nil);
    with FLblStationNr do
    begin
      Parent    := Self;
      AutoSize  := FALSE;
      Left      := 5;
      Top       := 15;
      Width     := 110;
      Height    := 21;
      Alignment := taRightJustify;
      Caption   := FAppModules.Language.GetString('LabelText.RainGaugeNumber');
    end;

    FEdtStationNr     := TEdit.Create(nil);
    with FEdtStationNr do
    begin
      Parent     := Self;
      Left       := 120;
      Top        := 15;
      Width      := 70;
      MaxLength  := 9;
      Text       := '';
      OnKeyPress := GaugeNrOnKeyPress;
    end;

    FLblLat        := TLabel.Create(nil);
    with FLblLat do
    begin
      Parent        := Self;
      AutoSize      := FALSE;
      Left          := 5;
      Top           := 15;
      Width         := 110;
      Height        := 21;
      Alignment     := taRightJustify;
      Caption       := FAppModules.Language.GetString('LabelText.StationLatitude');
    end;

    FSpnLatDeg     := TSpinEdit.Create(nil);
    with FSpnLatDeg do
    begin
      Parent    := Self;
      Left      := 120;
      Top       := 15;
      Width     := 45;
      MinValue  := -180;
      MaxValue  := 180;
    end;

    FSpnLatMin     := TSpinEdit.Create(nil);
    with FSpnLatMin do
    begin
      Parent    := Self;
      Left      := 170;
      Top       := 15;
      Width     := 40;
      MinValue  := 0;
      MaxValue  := 59;
    end;

    FLblLong       := TLabel.Create(nil);
    with FLblLong do
    begin
      Parent      := Self;
      AutoSize    := FALSE;
      Left        := 5;
      Top         := 45;
      Width       := 110;
      Height      := 21;
      Alignment   := taRightJustify;
      Caption     := FAppModules.Language.GetString('LabelText.StationLongitute');
    end;

    FSpnLongDeg    := TSpinEdit.Create(nil);
    with FSpnLongDeg do
    begin
      Parent   := Self;
      Left     := 120;
      Top      := 45;
      Width    := 45;
      MinValue := -180;
      MaxValue := 180;
    end;

    FSpnLongMin    := TSpinEdit.Create(nil);
    with FSpnLongMin do
    begin
      Parent   := Self;
      Left     := 170;
      Top      := 45;
      Width    := 40;
      MinValue := 0;
      MaxValue := 59;
    end;

    FLblDistance   := TLabel.Create(nil);
    with FLblDistance do
    begin
      Parent    := Self;
      AutoSize  := FALSE;
      Left      := 5;
      Top       := 75;
      Width     := 110;
      Height    := 21;
      Alignment := taRightJustify;
      Caption   := FAppModules.Language.GetString('LabelText.DistanceInKM');
    end;

    FEdtDistance   := TEdit.Create(nil);
    with FEdtDistance do
    begin
      Parent     := Self;
      Left       := 120;
      Top        := 75;
      Width      := 70;
      Text       := '';
      OnKeyPress := EditOnKeyPress;
    end;

    FRgpDistance   := TRadioGroup.Create(nil);
    with FRgpDistance do
    begin
      Parent    := Self;
      Width     := 170;
      Height    := 60;
      Top       := 10;
      Left      := 220;
      OnClick   := RadioGroupStDistanceOnClick;
      Caption   := FAppModules.Language.GetString('Rainfall.DistanceFrom');
      Items.Clear;
      Items.Add('Point (Degrees and minutes)');
      Items.Add('Gauge (Gauge number)');
      ItemIndex := 0;
    end;

    Result := TRUE;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRGSearchDialog.RadioGroupStDistanceOnClick (Sender : TObject);
const OPNAME = 'TRGSearchDialog.RadioGroupStDistanceOnClick';
begin
  try
    FLblStationNr.Visible  := FRgpDistance.ItemIndex = 1;
    FEdtStationNr.Visible  := FRgpDistance.ItemIndex = 1;

    FLblLat.Visible     := FRgpDistance.ItemIndex = 0;
    FSpnLatDeg.Visible  := FRgpDistance.ItemIndex = 0;
    FSpnLatMin.Visible  := FRgpDistance.ItemIndex = 0;
    FLblLong.Visible    := FRgpDistance.ItemIndex = 0;
    FSpnLongDeg.Visible := FRgpDistance.ItemIndex = 0;
    FSpnLongMin.Visible := FRgpDistance.ItemIndex = 0;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRGSearchDialog.RadioGroupStRectangleOnClick(Sender: TObject);
const OPNAME = 'TRGSearchDialog.RadioGroupStRectangleOnClick';
var
  LIntValue : integer;
begin
  try
    case FRgpRectangle.ItemIndex of
      0 : begin
            try
              LIntValue := Trunc(StrToFloat(FEdtTopLat.Text) * 100);
            except
              LIntValue := 0;
            end;
            FSpnTopLatDeg.Value := LIntValue div 100;
            FSpnTopLatMin.Value := Abs(Round((LIntValue mod 100) * 0.6));
            try
              LIntValue := Trunc(StrToFloat(FEdtLeftLong.Text) * 100);
            except
              LIntValue := 0;
            end;
            FSpnLeftLongDeg.Value := LIntValue div 100;
            FSpnLeftLongMin.Value := Abs(Round((LIntValue mod 100) * 0.6));
            try
              LIntValue := Trunc(StrToFloat(FEdtBottomLat.Text) * 100);
            except
              LIntValue := 0;
            end;
            FSpnBottomLatDeg.Value := LIntValue div 100;
            FSpnBottomLatMin.Value := Abs(Round((LIntValue mod 100) * 0.6));
            try
              LIntValue := Trunc(StrToFloat(FEdtRightLong.Text) * 100);
            except
              LIntValue := 0;
            end;
            FSpnRightLongDeg.Value := LIntValue div 100;
            FSpnRightLongMin.Value := Abs(Round((LIntValue mod 100) * 0.6));
            FEdtLeftLong.Visible := false;
            FEdtTopLat.Visible := false;
            FEdtRightLong.Visible := false;
            FEdtBottomLat.Visible := false;
            FSpnTopLatDeg.Visible := true;
            FSpnTopLatMin.Visible := true;
            FSpnLeftLongDeg.Visible := true;
            FSpnLeftLongMin.Visible := true;
            FSpnBottomLatDeg.Visible := true;
            FSpnBottomLatMin.Visible := true;
            FSpnRightLongDeg.Visible := true;
            FSpnRightLongMin.Visible := true;
          end;
      1 : begin
            FEdtTopLat.Text := Format('%3d.%2.2d', [FSpnTopLatDeg.Value, Trunc(FSpnTopLatMin.Value / 0.6)]);
            FEdtLeftLong.Text := Format('%3d.%2.2d', [FSpnLeftLongDeg.Value, Trunc(FSpnLeftLongMin.Value / 0.6)]);
            FEdtBottomLat.Text := Format('%3d.%2.2d', [FSpnBottomLatDeg.Value, Trunc(FSpnBottomLatMin.Value / 0.6)]);
            FEdtRightLong.Text := Format('%3d.%2.2d', [FSpnRightLongDeg.Value, Trunc(FSpnRightLongMin.Value / 0.6)]);

            FEdtLeftLong.Visible := true;
            FEdtTopLat.Visible := true;
            FEdtRightLong.Visible := true;
            FEdtBottomLat.Visible := true;
            FSpnTopLatDeg.Visible := false;
            FSpnTopLatMin.Visible := false;
            FSpnLeftLongDeg.Visible := false;
            FSpnLeftLongMin.Visible := false;
            FSpnBottomLatDeg.Visible := false;
            FSpnBottomLatMin.Visible := false;
            FSpnRightLongDeg.Visible := false;
            FSpnRightLongMin.Visible := false;
          end;
    end;
  except on E : Exception do HandleError(E,OPNAME); end;
end;

procedure TRGSearchDialog.CreateMemberObjects;
const OPNAME = 'TRGSearchDialog.CreateMemberObjects';
begin
  try
    inherited CreateMemberObjects;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRGSearchDialog.SetSearchFormType (ASearchFormType   : TSearchFormType;
                                             AApplicationTitle : string);
const OPNAME = 'TRGSearchDialog.SetSearchFormType';
begin
  try
    CreateMemberObject(ASearchFormType, AApplicationTitle);
    FSearchType := ASearchFormType;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRGSearchDialog.ButtonCanselClick ( Sender : TObject );
const OPNAME = 'TRGSearchDialog.ButtonCanselClick';
begin
  try
  ModalResult := mrCancel
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

procedure TRGSearchDialog.ButtonOKClick ( Sender : TObject );
const OPNAME = 'TRGSearchDialog.ButtonOKClick';
begin
  try
    if (((FSearchType = StDistance) AND ValidateDistance) OR
        (FSearchType <> StDistance)) then
      ModalResult := mrOk
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

function TRGSearchDialog.ValidateDistance : boolean;
const OPNAME = 'TRGSearchDialog.ValidateDistance';
var
  lStrGaugeNr          : string;
  lStrInvalidLatitude  : string;
  lStrInvalidLongitude : string;
  lStrKMDistance       : string;
  lRainGaugeNumber     : string;
begin
  Result := FALSE;
  try
    lStrInvalidLatitude  := FAppModules.Language.GetString('Message.LatitudeMinutes');
    lStrInvalidLongitude := FAppModules.Language.GetString('Message.LongitudeMinutes');
    lStrKMDistance       := FAppModules.Language.GetString('Message.KMDistance');
    lRainGaugeNumber     := FAppModules.Language.GetString('Message.RainGaugeNumber');
    if (FSearchType = StDistance) then
    begin
      Result := TRUE;
      if (Result AND (FRgpDistance.ItemIndex = 0)) then
      begin
        if (Trim(FSpnLatDeg.Text) = '') then
        begin
          Result := FALSE;
          ShowMessage(FAppModules.Language.GetString('Message.LatitudeDegrees'));
        end;
        if (Result AND (Trim(FSpnLatMin.Text) = '')) then
        begin
          Result := FALSE;
          ShowMessage(lStrInvalidLatitude);
        end;
        if (Result AND ((FSpnLatDeg.Value = 180) AND (FSpnLatMin.Value > 0))) then
        begin
          Result := FALSE;
          ShowMessage(lStrInvalidLatitude);
        end;
        if (Result AND (Trim(FSpnLongDeg.Text) = '')) then
        begin
          Result := FALSE;
          ShowMessage(FAppModules.Language.GetString('Message.LongitudeDegrees'));
        end;
        if (Result AND (Trim(FSpnLongMin.Text) = '')) then
        begin
          Result := FALSE;
          ShowMessage(lStrInvalidLongitude);
        end;
        if (Result AND ((FSpnLongDeg.Value = 180) AND (FSpnLongMin.Value > 0))) then
        begin
          Result := FALSE;
          ShowMessage(lStrInvalidLongitude);
        end;
        if (Result) then
        begin
          if (FSpnLatDeg.Value >= 0) then
            FLatitude  := FSpnLatDeg.Value  + FSpnLatMin.Value  / 60.0
          else
            FLatitude  := FSpnLatDeg.Value  - FSpnLatMin.Value  / 60.0;
          if (FSpnLongDeg.Value >= 0) then
            FLongitude := FSpnLongDeg.Value + FSpnLongMin.Value / 60.0
          else
            FLongitude := FSpnLongDeg.Value - FSpnLongMin.Value / 60.0;
        end;
      end
      else
      begin
        lStrGaugeNr := Trim(FEdtStationNr.Text);
        if (Result AND (Length(lStrGaugeNr) < 9)) then
        begin
          Result := FALSE;
          ShowMessage(lRainGaugeNumber);
        end;
        if (Result) then
        begin
          try
            StrToInt(Copy(lStrGaugeNr, 1, 7));
            (FAppModules.Model.ModelData as IRainfallModelData).
              LatLong(lStrGaugeNr, FLatitude, FLongitude);
          except
            Result := FALSE;
            ShowMessage(lRainGaugeNumber);
          end;
        end;
      end;
      if (Result AND (Trim(FEdtDistance.Text) = '')) then
      begin
        Result := FALSE;
        ShowMessage(lStrKMDistance);
      end;
      if (Result) then
      begin
        try
          FDistance := StrToFloat(Trim(FEdtDistance.Text));
        except
          Result := FALSE;
          ShowMessage(lStrKMDistance);
        end;
      end;
      if (Result AND (FDistance <= 0)) then
      begin
        Result := FALSE;
        ShowMessage(lStrKMDistance);
      end;
    end;
  except on E: Exception do HandleError ( E, OPNAME ) end;
end;

end.
