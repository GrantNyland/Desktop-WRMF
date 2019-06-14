//
//
//  UNIT      : Contains TAbstractGridData Class
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2003/01/10
//  COPYRIGHT : Copyright © 2003 DWAF
//
//
unit UAbstractGridData;

interface

uses
  Classes,
  UAbstractObject;

type
  TAbstractGridFieldData = class(TObject)
  protected
    FDisplayText: string;
    FFieldProperty: TAbstractFieldProperty;
    FContextData: TStringList;
  public
    property DisplayText: string read FDisplayText write FDisplayText;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write FFieldProperty;
    property ContextData: TStringList read FContextData;
  end;
  TAbstractGridField = class(TAbstractAppObject)
  protected
    FFieldProperty: TAbstractFieldProperty;
    function GetFieldData(ARecordIndex: integer): TAbstractGridFieldData; virtual; abstract;
  public
    procedure AddFieldData(ADisplayText: string; AContextData: TStringList; ASubFieldIndex: integer); virtual; abstract;
    function RecordCount: integer; virtual; abstract;
    procedure DeleteRecord(ARecordIndex: integer); virtual; abstract;
    property FieldData[ARecordIndex: integer]: TAbstractGridFieldData read GetFieldData;
    property FieldProperty: TAbstractFieldProperty read FFieldProperty write FFieldProperty;
  end;
  TAbstractGridData = class(TAbstractAppObject)
  protected
    function GetGridField(AFieldIndex: integer): TAbstractGridField; virtual; abstract;
    function GetFieldData(AFieldIndex, ARecordIndex: integer): TAbstractGridFieldData; virtual; abstract;
  public
    procedure Clear; virtual; abstract;
    function FieldCount: integer; virtual; abstract;
    function DataIDCommaText: string; virtual; abstract;
    procedure AddGridField(AFieldName: string); virtual; abstract;
    procedure DeleteRecord(ARecordIndex: integer); virtual; abstract;
    property GridField[AFieldIndex: integer]: TAbstractGridField read GetGridField;
    property FieldData[AFieldIndex, ARecordIndex: integer]: TAbstractGridFieldData read GetFieldData;
  end;

implementation

end.
