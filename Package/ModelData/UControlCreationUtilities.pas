{******************************************************************************}
{*  UNIT      : Contains utility functions to create controls.                *}
{*  AUTHOR    : Riana Steyn                                                   *}
{*  DATE      : 2003/09/01                                                    *}
{*  COPYRIGHT : Copyright © 2003 DWAF                                         *}
{******************************************************************************}

unit UControlCreationUtilities;

interface

uses
  Classes,
  Vcl.Controls,
  Vcl.ComCtrls,
  Vcl.ExtCtrls,
  Vcl.StdCtrls,
  Vcl.Buttons,
  Vcl.Graphics,
  Vcl.Forms,
  UAbstractObject,
  UAbstractComponent,
  UDataEditComponent,
  UDataComponent;

function CreateFieldEdit (AAppModules : TAppModules;
                          AOwner    : TComponent;
                          AParent   : TWinControl;
                          ALeft     : integer;
                          ATop      : integer;
                          AWidth    : integer;
                          AHeight   : integer;
                          ATabOrder : integer;
                          ATabStop  : Boolean) : TFieldEdit;
function CreateFieldDateTimePicker (AAppModules: TAppModules;
                                    AOwner    : TComponent;
                                    AParent   : TWinControl;
                                    ALeft     : integer;
                                    ATop      : integer;
                                    AWidth    : integer;
                                    AHeight   : integer;
                                    ATabOrder : integer;
                                    ATabStop  : Boolean) : TFieldDateTimePicker;
function CreateFieldRadioGroup (AAppModules: TAppModules;
                                AOwner    : TComponent;
                                AParent   : TWinControl;
                                ALeft     : integer;
                                ATop      : integer;
                                AWidth    : integer;
                                AHeight   : integer;
                                ATabOrder : integer;
                                ATabStop  : Boolean) : TFieldRadioGroup;
function CreateFieldLabel (AOwner  : TComponent;
                           AParent : TWinControl;
                           ALeft   : integer;
                           ATop    : integer;
                           AWidth  : integer;
                           AHeight : integer) : TLabel;
function CreatePanel (AOwner    : TComponent;
                      AParent   : TWinControl;
                      ALeft     : integer;
                      ATop      : integer;
                      AWidth    : integer;
                      AHeight   : integer;
                      ATabOrder : integer) : TPanel;
function CreateFieldGroupBox (AOwner   : TComponent;
                              AParent  : TWinControl;
                              ALeft    : integer;
                              ATop     : integer;
                              AWidth   : integer;
                              AHeight  : integer;
                              ATabOrder : integer;
                              ATabStop  : Boolean) : TGroupBox;
function CreateFieldComboBox (AAppModules: TAppModules;
                              AOwner    : TComponent;
                              AParent   : TWinControl;
                              ALeft     : integer;
                              ATop      : integer;
                              AWidth    : integer;
                              AHeight   : integer;
                              ATabOrder : integer;
                              ATabStop  : Boolean;
                              AStyle    : TComboBoxStyle) : TFieldComboBox;
function CreateFieldChkBox (AAppModules: TAppModules;
                            AOwner     : TComponent;
                            AParent    : TWinControl;
                            ALeft      : integer;
                            ATop       : integer;
                            AWidth     : integer;
                            AHeight    : integer;
                            ATabOrder  : integer;
                            ATabStop   : Boolean;
                            AAlignment : TLeftRight) : TFieldChkBox;
function CreateFieldButton (AAppModules: TAppModules;
                            AOwner    : TComponent;
                            AParent   : TWinControl;
                            ALeft     : integer;
                            ATop      : integer;
                            AWidth    : integer;
                            AHeight   : integer;ATabOrder : integer;
                            ATabStop  : Boolean;
                            AButtonKey  : string) : TFieldButton;
function CreateFieldBitButton (AAppModules: TAppModules;
                            AOwner    : TComponent;
                            AParent   : TWinControl;
                            ALeft     : integer;
                            ATop      : integer;
                            AWidth    : integer;
                            AHeight   : integer;ATabOrder : integer;
                            ATabStop  : Boolean;
                            AButtonKey  : string) : TFieldBitBtn;
function CreateFieldStringGrid (AAppModules : TAppModules;
                                AOwner    : TComponent;
                                AParent   : TWinControl;
                                ALeft     : integer;
                                ATop      : integer;
                                AWidth    : integer;
                                AHeight   : integer;
                                ATabOrder : integer;
                                ATabStop  : Boolean) : TFieldStringGrid;

function CreateFieldButtonStringGrid (AAppModules : TAppModules;
                                AOwner    : TComponent;
                                AParent   : TWinControl;
                                ALeft     : integer;
                                ATop      : integer;
                                AWidth    : integer;
                                AHeight   : integer;
                                ATabOrder : integer;
                                ATabStop  : Boolean) : TFieldButtonStringGrid;
function CreateFieldCheckListBox (AAppModules: TAppModules;
                                  AOwner     : TComponent;
                                  AParent    : TWinControl;
                                  ALeft      : integer;
                                  ATop       : integer;
                                  AWidth     : integer;
                                  AHeight    : integer;
                                  ATabOrder  : integer;
                                  ATabStop   : Boolean) : TFieldCheckListBox;
function CreateFieldRichEdit (AAppModules: TAppModules;
                                  AOwner     : TComponent;
                                  AParent    : TWinControl;
                                  ALeft      : integer;
                                  ATop       : integer;
                                  AWidth     : integer;
                                  AHeight    : integer;
                                  ATabOrder  : integer;
                                  ATabStop   : Boolean) : TFieldRichEdit;


implementation

uses
  SysUtils,
  UErrorHandlingOperations;

const
  C_ControlBorder  = 10;
  C_LabelOffset    = 3;

{******************************************************************************}
{* TControlCreationAgent                                                      *}
{******************************************************************************}

function CreateFieldEdit (AAppModules: TAppModules;
                          AOwner    : TComponent;
                          AParent   : TWinControl;
                          ALeft     : integer;
                          ATop      : integer;
                          AWidth    : integer;
                          AHeight   : integer;
                          ATabOrder : integer;
                          ATabStop  : Boolean) : TFieldEdit;
const OPNAME = 'CreateFieldEdit';
var
  pControl : TFieldEdit;
begin
  result := nil;
  try
    pControl := TFieldEdit.Create(AOwner, AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
//      TabStop    := ATabStop;
//      TabOrder   := ATabOrder;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldDateTimePicker (AAppModules: TAppModules;
                                    AOwner    : TComponent;
                                    AParent   : TWinControl;
                                    ALeft     : integer;
                                    ATop      : integer;
                                    AWidth    : integer;
                                    AHeight   : integer;
                                    ATabOrder : integer;
                                    ATabStop  : Boolean) : TFieldDateTimePicker;
const OPNAME = 'CreateFieldDateTimePicker';
var
  pControl : TFieldDateTimePicker;
begin
  result := nil;
  try
    pControl := TFieldDateTimePicker.Create(AOwner, AAppModules);
    with pControl do
    begin
      Parent       := AParent;
      Top          := ATop;
      Left         := ALeft;
      Width        := AWidth;
      Height       := AHeight;
//      TabStop      := ATabStop;
//      TabOrder     := ATabOrder;
      CalAlignment := dtaLeft;
      DateFormat   := dfShort;
      DateMode     := dmComboBox;
      Kind         := dtkDate;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldGroupBox (AOwner    : TComponent;
                              AParent   : TWinControl;
                              ALeft     : integer;
                              ATop      : integer;
                              AWidth    : integer;
                              AHeight   : integer;
                              ATabOrder : integer;
                              ATabStop  : Boolean) : TGroupBox;
const OPNAME = 'CreateFieldGroupBox';
var
  pControl : TGroupBox;
begin
  result := nil;
  try
    pControl := TGroupBox.Create(AOwner);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
//      TabStop    := ATabStop;
//      TabOrder   := ATabOrder;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldRadioGroup (AAppModules: TAppModules;
                                AOwner    : TComponent;
                                AParent   : TWinControl;
                                ALeft     : integer;
                                ATop      : integer;
                                AWidth    : integer;
                                AHeight   : integer;
                                ATabOrder : integer;
                                ATabStop  : Boolean) : TFieldRadioGroup;
const OPNAME = 'CreateFieldRadioGroup';
var
  pControl : TFieldRadioGroup;
begin
  result := nil;
  try
    pControl := TFieldRadioGroup.Create(AOwner,AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
//      TabStop    := ATabStop;
//      TabOrder   := ATabOrder;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldLabel (AOwner     : TComponent;
                           AParent    : TWinControl;
                           ALeft      : integer;
                           ATop       : integer;
                           AWidth     : integer;
                           AHeight    : integer) : TLabel;
const OPNAME = 'CreateFieldLabel';
var
  pControl : TLabel;
begin
  result := nil;
  try
    pControl := TLabel.Create(AOwner);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
      Layout     := tlCenter;
      Autosize   := FALSE;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreatePanel (AOwner     : TComponent;
                      AParent    : TWinControl;
                      ALeft      : integer;
                      ATop       : integer;
                      AWidth     : integer;
                      AHeight    : integer;
                      ATabOrder  : integer) : TPanel;
const OPNAME = 'CreatePanel';
var
  pControl : TPanel;
begin
  result := nil;
  try
    pControl := TPanel.Create(AOwner);
    with pControl do
    begin
      Parent      := AParent;
      Top         := ATop;
      Left        := ALeft;
      Width       := AWidth;
      Height      := AHeight;
      BorderStyle := bsSingle;
//      TabOrder    := ATabOrder;
//      TabStop     := FALSE;
      caption     := '';
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldButton (AAppModules: TAppModules;
                            AOwner    : TComponent;
                            AParent   : TWinControl;
                            ALeft     : integer;
                            ATop      : integer;
                            AWidth    : integer;
                            AHeight   : integer;
                            ATabOrder : integer;
                            ATabStop  : Boolean;
                            AButtonKey  : string) : TFieldButton;
const OPNAME = 'CreateFieldButton';
var
  pControl : TFieldButton;
begin
  result := nil;
  try
    pControl := TFieldButton.Create(AOwner,AAppModules, AButtonKey);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
      //TabStop    := ATabStop;
      //TabOrder   := ATabOrder;
      Caption    := AButtonKey;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldBitButton (AAppModules: TAppModules;
                            AOwner    : TComponent;
                            AParent   : TWinControl;
                            ALeft     : integer;
                            ATop      : integer;
                            AWidth    : integer;
                            AHeight   : integer;
                            ATabOrder : integer;
                            ATabStop  : Boolean;
                            AButtonKey  : string) : TFieldBitBtn;
const OPNAME = 'CreateFieldBitButton';
var
  pControl : TFieldBitBtn;
begin
  result := nil;
  try
    pControl := TFieldBitBtn.Create(AOwner,AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
      //TabStop    := ATabStop;
      //TabOrder   := ATabOrder;
      //Caption    := AButtonKey;
      Glyph.LoadFromResourceName(HImagesInstance, UpperCase(AButtonKey));
      NumGlyphs := pControl.Glyph.Width div pControl.Glyph.Height;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldComboBox (AAppModules: TAppModules;
                              AOwner    : TComponent;
                              AParent   : TWinControl;
                              ALeft     : integer;
                              ATop      : integer;
                              AWidth    : integer;
                              AHeight   : integer;
                              ATabOrder : integer;
                              ATabStop  : Boolean;
                              AStyle    : TComboBoxStyle) : TFieldComboBox;
const OPNAME = 'CreateFieldComboBox';
var
  pControl : TFieldComboBox;
begin
  result := nil;
  try
    pControl := TFieldComboBox.Create(AOwner, AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
//      TabStop    := ATabStop;
//      TabOrder   := ATabOrder;
      Style      := AStyle;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldChkBox (AAppModules: TAppModules;
                            AOwner     : TComponent;
                            AParent    : TWinControl;
                            ALeft      : integer;
                            ATop       : integer;
                            AWidth     : integer;
                            AHeight    : integer;
                            ATabOrder  : integer;
                            ATabStop   : Boolean;
                            AAlignment : TLeftRight) : TFieldChkBox;
const OPNAME = 'CreateFieldChkBox';
var
  pControl : TFieldChkBox;
begin
  result := nil;
  try
    pControl := TFieldChkBox.Create(AOwner, AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
//      TabStop    := ATabStop;
//      TabOrder   := ATabOrder;
      Alignment  := AAlignment;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldCheckListBox (AAppModules: TAppModules;
                                  AOwner     : TComponent;
                                  AParent    : TWinControl;
                                  ALeft      : integer;
                                  ATop       : integer;
                                  AWidth     : integer;
                                  AHeight    : integer;
                                  ATabOrder  : integer;
                                  ATabStop   : Boolean) : TFieldCheckListBox;
const OPNAME = 'CreateFieldCheckListBox';
var
  pControl : TFieldCheckListBox;
begin
  result := nil;
  try
    pControl := TFieldCheckListBox.Create(AOwner, AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
//      TabStop    := ATabStop;
//      TabOrder   := ATabOrder;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;


function CreateFieldRichEdit (AAppModules: TAppModules;
                                  AOwner     : TComponent;
                                  AParent    : TWinControl;
                                  ALeft      : integer;
                                  ATop       : integer;
                                  AWidth     : integer;
                                  AHeight    : integer;
                                  ATabOrder  : integer;
                                  ATabStop   : Boolean) : TFieldRichEdit;
const OPNAME = 'CreateFieldRichEdit';
var
  pControl : TFieldRichEdit;
begin
  result := nil;
  try
    pControl := TFieldRichEdit.Create(AOwner, AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
      TabStop    := ATabStop;
      TabOrder   := ATabOrder;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldStringGrid (AAppModules : TAppModules;
                                AOwner      : TComponent;
                                AParent     : TWinControl;
                                ALeft       : integer;
                                ATop        : integer;
                                AWidth      : integer;
                                AHeight     : integer;
                                ATabOrder   : integer;
                                ATabStop    : Boolean) : TFieldStringGrid;
const OPNAME = 'CreateFieldStringGrid';
var
  pControl : TFieldStringGrid;
begin
  result := nil;
  try
    pControl := TFieldStringGrid.Create(AOwner, AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
//      TabStop    := ATabStop;
//      TabOrder   := ATabOrder;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

function CreateFieldButtonStringGrid (AAppModules : TAppModules;
                                AOwner      : TComponent;
                                AParent     : TWinControl;
                                ALeft       : integer;
                                ATop        : integer;
                                AWidth      : integer;
                                AHeight     : integer;
                                ATabOrder   : integer;
                                ATabStop    : Boolean) : TFieldButtonStringGrid;
const OPNAME = 'UControlCreationUtilities.CreateFieldButtonStringGrid';
var
  pControl : TFieldButtonStringGrid;
begin
  result := nil;
  try
    pControl := TFieldButtonStringGrid.Create(AOwner, AAppModules);
    with pControl do
    begin
      Parent     := AParent;
      Top        := ATop;
      Left       := ALeft;
      Width      := AWidth;
      Height     := AHeight;
//      TabStop    := ATabStop;
//      TabOrder   := ATabOrder;
    end;
    result := pControl;
  except on E: Exception do HandleError(E, OPNAME) end;
end;

end.
