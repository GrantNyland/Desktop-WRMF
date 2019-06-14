//
//
//  UNIT      : Contains class interface declerations.
//  AUTHOR    : Grant Nyland (PDNA)
//  DATE      : 2002/11/01
//  COPYRIGHT : Copyright © 2002 DWAF
//
//
unit UAbstractModelObjects;

interface

uses
  Vcl.ComCtrls,
  UAbstractObject,
  UStudyObjects,
  USystemModelLinkClasses;

//
// These classes represent the abstract ancestor of PRIVATE manager objects
// that AppModules uses. Aplication objects that the entire application
// can see must go in UAbstractObject.
//
type
  TAbstractLanguageManager = class(TAbstractAppObject)
  public
    function Language: TAbstractLanguage; virtual; abstract;
    function SetLanguage(ANewLanguage: string): boolean; virtual; abstract;
  end;

implementation

end.
