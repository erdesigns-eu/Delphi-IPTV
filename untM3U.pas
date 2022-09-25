unit untM3U;

interface

//{$DEFINE TAGVARIANT}
//{$DEFINE PARSEREGEX}

uses
  System.SysUtils,
  System.Classes,
  {$IFDEF TAGVARIANT}
  System.Variants,
  {$ENDIF}
  Winapi.Windows;

{*******************************************************}
{                       Tag Case                        }
{*******************************************************}
type
  TTagCase = (tcNone, tcUpper, tcLower);

{*******************************************************}
{                       Tag Class                       }
{*******************************************************}
type
  TTag = class(TCollectionItem)
  private
    FNameCase  : TTagCase;
    FName      : String;
    FValueCase : TTagCase;
    {$IFDEF TAGVARIANT}
    FValue     : Variant;
    {$ELSE}
    FValue     : String;
    {$ENDIF}

    procedure SetName(Name: String);
    {$IFDEF TAGVARIANT}
    procedure SetValue(Value: Variant);
    {$ELSE}
    procedure SetValue(Value: String);
    {$ENDIF}

    function GetName: String;
    {$IFDEF TAGVARIANT}
    function GetValue: Variant;
    {$ELSE}
    function GetValue: String;
    {$ENDIF}
  public
    procedure Assign(Source: TPersistent); override;

    property NameCase: TTagCase read FNameCase write FNameCase;
    property ValueCase: TTagCase read FValueCase write FValueCase;
  published
    property Name: String read GetName write SetName;
    {$IFDEF TAGVARIANT}
    property Value: Variant read GetValue write SetValue;
    {$ELSE}
    property Value: String read GetValue write SetValue;
    {$ENDIF}
  end;

{*******************************************************}
{                      DefTag Class                     }
{*******************************************************}
type
  TDefTag = class(TCollectionItem)
  private
    FName        : String;
    FValue       : String;
    FDescription : String;

    procedure SetName(Name: String);
    procedure SetValue(Value: String);
    procedure SetDescription(Description: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Name: String read FName write SetName;
    property Value: String read FValue write SetValue;
    property Description: String read FDescription write SetDescription;
  end;

{*******************************************************}
{                 Directive Collection                  }
{*******************************************************}
type
  TDirectiveCollection = class(TOwnedCollection)
  private
    FOnChange  : TNotifyEvent;
    FNameCase  : TTagCase;
    FValueCase : TTagCase;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TTag;
    procedure SetItem(Index: Integer; Value: TTag);

    procedure SetNameCase(&Case: TTagCase);
    procedure SetValueCase(&Case: TTagCase);
    function GetDirective(Name: String): String;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TTag;
    procedure Assign(Source: TPersistent); override;

    property NameCase: TTagCase read FNameCase write SetNameCase default tcUpper;
    property ValueCase: TTagCase read FValueCase write SetValueCase default tcNone;

    property Directive[Name: String]: String read GetDirective;
    property Items[Index: Integer]: TTag read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                 Attribute Collection                  }
{*******************************************************}
type
  TAttributeCollection = class(TOwnedCollection)
  private
    FOnChange  : TNotifyEvent;
    FNameCase  : TTagCase;
    FValueCase : TTagCase;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TTag;
    procedure SetItem(Index: Integer; Value: TTag);

    procedure SetNameCase(&Case: TTagCase);
    procedure SetValueCase(&Case: TTagCase);
    function GetAttribute(Name: String): String;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TTag;
    procedure Assign(Source: TPersistent); override;

    property NameCase: TTagCase read FNameCase write SetNameCase default tcLower;
    property ValueCase: TTagCase read FValueCase write SetValueCase default tcNone;

    property Attribute[Name: String]: String read GetAttribute;
    property Items[Index: Integer]: TTag read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                   DefTag Collection                   }
{*******************************************************}
type
  TDefTagCollection = class(TOwnedCollection)
  private
    FOnChange  : TNotifyEvent;
    
    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TDefTag;
    procedure SetItem(Index: Integer; Value: TDefTag);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TDefTag;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TDefTag read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                     Stream Class                      }
{*******************************************************}
type
  TStream = class(TCollectionItem)
  private
    FName       : String;
    FSource     : String;
    FLength     : Double;

    FDirectives : TDirectiveCollection;
    FAttributes : TAttributeCollection;

    procedure SetName(Name: String);
    procedure SetSource(Source: String);
    procedure SetLength(Length: Double);
    function GetGroupName : String;

    procedure TagChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

  published
    property Name: String read FName write SetName;
    property Source: String read FSource write SetSource;
    property Length: Double read FLength write SetLength;
    property GroupName: String read GetGroupName;

    property Directives: TDirectiveCollection read FDirectives;
    property Attributes: TAttributeCollection read FAttributes;
  end;

{*******************************************************}
{                   Stream Collection                   }
{*******************************************************}
type
  TStreamCollection = class(TOwnedCollection)
  private
    FOnChange  : TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TStream;
    procedure SetItem(Index: Integer; Value: TStream);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TStream;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TStream read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                       M3U Class                       }
{*******************************************************}
type
  TM3U = class(TComponent)
  private
    FOnDirectiveChange : TNotifyEvent;
    FOnAttributeChange : TNotifyEvent;
    FOnStreamChange    : TNotifyEvent;

    FDirectives : TDirectiveCollection;
    FAttributes : TAttributeCollection;
    FStreams    : TStreamCollection;

    procedure DirectiveChanged(Sender: TObject);
    procedure AttributeChanged(Sender: TObject);
    procedure StreamChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property OnDirectiveChange: TNotifyEvent read FOnDirectiveChange write FOnDirectiveChange;
    property OnAttributeChange: TNotifyEvent read FOnAttributeChange write FOnAttributeChange;
    property OnStreamChange: TNotifyEvent read FOnStreamChange write FOnStreamChange;

    property Directives: TDirectiveCollection read FDirectives;
    property Attributes: TAttributeCollection read FAttributes;
    property Streams: TStreamCollection read FStreams;
  end;

type
  TM3UReaderProgressEvent = procedure(Position: Integer; Line: String) of object;
  TM3UReaderErrorEvent    = procedure(ErrorCode: Integer; ErrorMessage: String) of object;

{*******************************************************}
{                   M3U Reader Class                    }
{*******************************************************}
type
  TM3UReader = class(TComponent)
  public const
    ERROR_FILE_HEADER  = 1;
  private
    FOnProgress        : TM3UReaderProgressEvent;
    FOnStart           : TNotifyEvent;
    FOnFinish          : TNotifyEvent;
    FOnError           : TM3UReaderErrorEvent;
    FOnDirectiveChange : TNotifyEvent;
    FOnAttributeChange : TNotifyEvent;

    FFilename   : TFilename;
    FLines      : TStringList;
    FDirectives : TDefTagCollection;
    FAttributes : TDefTagCollection;

    procedure DirectiveChange(Sender: TObject);
    procedure AttributeChange(Sender: TObject);

    procedure SetFilename(const Filename: TFilename);
    function GetMax: Integer;

    procedure Parse(const M3U: TM3U);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    procedure LoadFromFile(const M3U: TM3U; const Filename: String);
  published
    property OnProgress: TM3UReaderProgressEvent read FOnProgress write FOnProgress;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnError: TM3UReaderErrorEvent read FOnError write FOnError;
    property OnDirectiveChange: TNotifyEvent read FOnDirectiveChange write FOnDirectiveChange;
    property OnAttributeChange: TNotifyEvent read FOnAttributeChange write FOnAttributeChange;

    property Filename: TFilename read FFilename write SetFilename;
    property Lines: TStringList read FLines;
    property Max: Integer read GetMax;
    property Directives: TDefTagCollection read FDirectives;
    property Attributes: TDefTagCollection read FAttributes;
  end;

type
  TM3UWriterProgressEvent = procedure(Max: Integer; Position: Integer; Stream: TStream) of object;

{*******************************************************}
{                   M3U Writer Class                    }
{*******************************************************}
type
  TM3UWriter = class(TComponent)
  private
    FOnProgress : TM3UWriterProgressEvent;
    FOnStart    : TNotifyEvent;
    FOnFinish   : TNotifyEvent;
  public
    procedure SaveToFile(const M3U: TM3U; const Filename: String);
  published
    property OnProgress: TM3UWriterProgressEvent read FOnProgress write FOnProgress;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

type
  TXtreamReaderProgressEvent = procedure(Position: Integer; Line: String) of object;
  TXtreamReaderErrorEvent    = procedure(ErrorCode: Integer; ErrorMessage: String) of object;

  TXtreamStreamType = (stLive, stMovie, stSeries);
  TXtreamStreamTypes = set of TXtreamStreamType;

{*******************************************************}
{                 Xtream Reader Class                   }
{*******************************************************}
type
  TXtreamReader = class(TComponent)
  public const
    ERROR_AUTH      = 1;
    ERROR_JSON      = 2;
    ERROR_HOST      = 3;
    ERROR_USERNAME  = 4;
    ERROR_PASSWORD  = 5;
    ERROR_NOT_FOUND = 404;
    ERROR_SERVER    = 500;
  private
    FOnProgress : TXtreamReaderProgressEvent;
    FOnStart    : TNotifyEvent;
    FOnFinish   : TNotifyEvent;
    FOnError    : TXtreamReaderErrorEvent;

    FScheme     : String;
    FHost       : String;
    FPort       : Integer;
    FUsername   : String;
    FPassword   : String;

    FGroupAttribute   : Boolean;
    FGroupDirective   : Boolean;
    FtvgNameAttribute : Boolean;
    FtvgIDAttribute   : Boolean;
    FtvgLogoAttribute : Boolean;
    FtvgChnoAttribute : Boolean;
  public
    constructor Create(AOwner: TComponent); override;

    procedure LoadFromAPI(const M3U: TM3U; const StreamTypes: TXtreamStreamTypes = [stLive, stMovie, stSeries]);
    procedure LoadFromURL(const M3U: TM3U; const URL: String;
      const StreamTypes: TXtreamStreamTypes = [stLive, stMovie, stSeries]);
  published
    property OnProgress: TXtreamReaderProgressEvent read FOnProgress write FOnProgress;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnError: TXtreamReaderErrorEvent read FOnError write FOnError;

    property Scheme: String read FScheme write FScheme;
    property Host: String read FHost write FHost;
    property Port: Integer read FPort write FPort default 0;
    property Username: String read FUsername write FUsername;
    property Password: String read FPassword write FPassword;

    property GroupAttribute: Boolean read FGroupAttribute write FGroupAttribute default True;
    property GroupDirective: Boolean read FGroupDirective write FGroupDirective default False;

    property tvgNameAttribute: Boolean read FtvgNameAttribute write FtvgNameAttribute default True;
    property tvgIDAttribute: Boolean read FtvgIDAttribute write FtvgIDAttribute default True;
    property tvgLogoAttribute: Boolean read FtvgLogoAttribute write FtvgLogoAttribute default True;
    property tvgChnoAttribute: Boolean read FtvgChnoAttribute write FtvgChnoAttribute default True; 
  end;

{$IFNDEF TAGVARIANT}
type
  TCharUpCaseTable = array [Char] of Char;
var
  CharUpCaseTable: TCharUpCaseTable;
{$ENDIF}

procedure Register;

implementation

uses
{$IFDEF PARSEREGEX}
  System.RegularExpressions,
{$ENDIF}
  System.IOUtils,
  System.JSON,
  System.Net.URLClient,
  System.Net.HttpClient;

{$IFNDEF TAGVARIANT}
procedure InitCharUpCaseTable(var Table: TCharUpCaseTable);
var
  I: Cardinal;
begin
  for I := 0 to Length(Table) - 1 do Table[Char(I)] := Char(I);
  CharUpperBuff(@Table, Length(Table));
end;

function InsensPosEx(const SubStr, S: string; Offset: Integer = 1): Integer;
var
  I            : Integer;
  SubStrLength : Integer;
  SLength      : Integer;
label
  Fail;
begin
  Result := 0;
  if S = '' then Exit;
  if Offset <= 0 then Exit;

  SubStrLength := Length(SubStr);
  SLength := Length(S);

  if SubStrLength > SLength then Exit;

  Result := Offset;
  while SubStrLength <= (SLength - Result + 1) do 
  begin
    for I := 1 to SubStrLength do
    if CharUpCaseTable[SubStr[I]] <> CharUpCaseTable[s[Result + I - 1]] then
      goto Fail;
    Exit;
  Fail:
    Inc(Result);
  end;
  Result := 0;
end;
{$ENDIF}

{*******************************************************}
{                       Tag Class                       }
{*******************************************************}
procedure TTag.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TTag) then
  begin
    FNameCase  := (Source as TTag).NameCase;
    FName      := (Source as TTag).Name;
    FValueCase := (Source as TTag).ValueCase;
    FValue     := (Source as TTag).Value;
  end;
  Changed(False);
end;

procedure TTag.SetName(Name: string);
begin
  FName := Name;
  Changed(False);
end;

{$IFDEF TAGVARIANT}
procedure TTag.SetValue(Value: Variant);
begin
  FValue := Value;
  Changed(False);
end;
{$ELSE}
procedure TTag.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;
{$ENDIF}

function TTag.GetName: String;
begin
  case FNameCase of
    tcNone  : Result := FName;
    tcUpper : Result := Uppercase(FName);
    tcLower : Result := Lowercase(FName);
  end;
end;

{$IFDEF TAGVARIANT}
function TTag.GetValue: Variant;
begin
  if (VarType(FValue) = varOleStr) or (VarType(FValue) = varStrArg) or (VarType(FValue) = varString) then
  case FValueCase of
    tcNone  : Result := FValue;
    tcUpper : Result := Uppercase(FValue);
    tcLower : Result := Lowercase(FValue);
  end else
    Result := FValue;
end;
{$ELSE}
function TTag.GetValue: String;
begin
  case FValueCase of
    tcNone  : Result := FValue;
    tcUpper : Result := Uppercase(FValue);
    tcLower : Result := Lowercase(FValue);
  end;
end;
{$ENDIF}

{*******************************************************}
{                      DefTag Class                     }
{*******************************************************}
procedure TDefTag.SetName(Name: string);
begin
  FName := Name;
  Changed(False);
end;

procedure TDefTag.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TDefTag.SetDescription(Description: string);
begin
  FDescription := Description;
  Changed(False);
end;

procedure TDefTag.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TDefTag) then
  begin
    FName        := (Source as TDefTag).Name;
    FValue       := (Source as TDefTag).Value;
    FDescription := (Source as TDefTag).Description;
  end;
  Changed(False);
end;

{*******************************************************}
{                 Directive Collection                  }
{*******************************************************}
constructor TDirectiveCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTag);
  FNameCase  := tcUpper;
  FValueCase := tcNone;
end;

procedure TDirectiveCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TDirectiveCollection.GetItem(Index: Integer): TTag;
begin
  Result := inherited GetItem(Index) as TTag;
end;

procedure TDirectiveCollection.SetItem(Index: Integer; Value: TTag);
begin
  inherited SetItem(Index, Value);
  Items[Index].NameCase  := NameCase;
  Items[Index].ValueCase := ValueCase;
  ItemChanged(Self);
end;

procedure TDirectiveCollection.SetNameCase(&Case: TTagCase);
var
  I : Integer;
begin
  for I := 0 to Count -1 do
  Items[I].NameCase := &Case;
end;

procedure TDirectiveCollection.SetValueCase(&Case: TTagCase);
var
  I : Integer;
begin
  for I := 0 to Count -1 do
  Items[I].ValueCase := &Case;
end;

function TDirectiveCollection.GetDirective(Name: String): String;
var
  I : Integer;
begin
  Result := '';
  for I := 0 to Count -1 do
  if CompareText(Items[I].Name, Name) = 0 then
  begin
    Result := Items[I].Value;
    Break;
  end;
end;

procedure TDirectiveCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TDirectiveCollection.Add: TTag;
begin
  Result := TTag(inherited Add);
  Result.NameCase  := NameCase;
  Result.ValueCase := ValueCase;
end;

procedure TDirectiveCollection.Assign(Source: TPersistent);
var
  LI   : TDirectiveCollection;
  Loop : Integer;
begin
  if (Source is TDirectiveCollection)  then
  begin
    LI := TDirectiveCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                 Attribute Collection                  }
{*******************************************************}
constructor TAttributeCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TTag);
  FNameCase  := tcLower;
  FValueCase := tcNone;
end;

procedure TAttributeCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TAttributeCollection.GetItem(Index: Integer): TTag;
begin
  Result := inherited GetItem(Index) as TTag;
end;

procedure TAttributeCollection.SetItem(Index: Integer; Value: TTag);
begin
  inherited SetItem(Index, Value);
  Items[Index].NameCase  := NameCase;
  Items[Index].ValueCase := ValueCase;
  ItemChanged(Self);
end;

procedure TAttributeCollection.SetNameCase(&Case: TTagCase);
var
  I : Integer;
begin
  for I := 0 to Count -1 do
  Items[I].NameCase := &Case;
end;

procedure TAttributeCollection.SetValueCase(&Case: TTagCase);
var
  I : Integer;
begin
  for I := 0 to Count -1 do
  Items[I].ValueCase := &Case;
end;

function TAttributeCollection.GetAttribute(Name: String): String;
var
  I : Integer;
begin
  Result := '';
  for I := 0 to Count -1 do
  if CompareText(Items[I].Name, Name) = 0 then
  begin
    Result := Items[I].Value;
    Break;
  end;
end;

procedure TAttributeCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TAttributeCollection.Add: TTag;
begin
  Result := TTag(inherited Add);
  Result.NameCase  := NameCase;
  Result.ValueCase := ValueCase;
end;

procedure TAttributeCollection.Assign(Source: TPersistent);
var
  LI   : TAttributeCollection;
  Loop : Integer;
begin
  if (Source is TAttributeCollection)  then
  begin
    LI := TAttributeCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                   DefTag Collection                   }
{*******************************************************}
constructor TDefTagCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TDefTag);
end;

procedure TDefTagCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TDefTagCollection.GetItem(Index: Integer): TDefTag;
begin
  Result := inherited GetItem(Index) as TDefTag;
end;

procedure TDefTagCollection.SetItem(Index: Integer; Value: TDefTag);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TDefTagCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TDefTagCollection.Add: TDefTag;
begin
  Result := TDefTag(inherited Add);
end;

procedure TDefTagCollection.Assign(Source: TPersistent);
var
  LI   : TDefTagCollection;
  Loop : Integer;
begin
  if (Source is TDefTagCollection)  then
  begin
    LI := TDefTagCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                     Stream Class                      }
{*******************************************************}
constructor TStream.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FDirectives := TDirectiveCollection.Create(Self);
  FDirectives.OnChange := TagChanged;
  FAttributes := TAttributeCollection.Create(Self);
  FAttributes.OnChange := TagChanged;
end;

destructor TStream.Destroy;
begin
  FDirectives.Free;
  FAttributes.Free;

  inherited Destroy;
end;

procedure TStream.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TStream) then
  begin
    FName       := (Source as TStream).Name;
    FSource     := (Source as TStream).Source;
    FLength     := (Source as TStream).Length;
    FDirectives.Assign((Source as TStream).Directives);
    FAttributes.Assign((Source as TStream).Attributes);
  end;
  Changed(False);
end;

procedure TStream.SetName(Name: string);
begin
  FName := Name;
  Changed(False);
end;

procedure TStream.SetSource(Source: string);
begin
  FSource := Source;
  Changed(False);
end;

procedure TStream.SetLength(Length: Double);
begin
  if (Length < -1) then raise Exception.CreateFmt('Stream length can not be lower than -1, found %f!', [Length]);
  FLength := Length;
  Changed(False);
end;

function TStream.GetGroupName: string;
begin
  Result := '';
  if Attributes.Attribute['group-title'].Length > 0 then
    Result := Attributes.Attribute['group-title']
  else
  if Directives.Directive['#EXTGRP'].Length > 0 then
    Result := Directives.Directive['#EXTGRP'];
end;

procedure TStream.TagChanged(Sender: TObject);
begin
  Changed(False);
end;

{*******************************************************}
{                 Attribute Collection                  }
{*******************************************************}
constructor TStreamCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TStream);
end;

procedure TStreamCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TStreamCollection.GetItem(Index: Integer): TStream;
begin
  Result := inherited GetItem(Index) as TStream;
end;

procedure TStreamCollection.SetItem(Index: Integer; Value: TStream);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TStreamCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TStreamCollection.Add: TStream;
begin
  Result := TStream(inherited Add);
end;

procedure TStreamCollection.Assign(Source: TPersistent);
var
  LI   : TStreamCollection;
  Loop : Integer;
begin
  if (Source is TStreamCollection)  then
  begin
    LI := TStreamCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                       M3U Class                       }
{*******************************************************}
constructor TM3U.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FDirectives := TDirectiveCollection.Create(Self);
  FDirectives.OnChange := DirectiveChanged;
  FAttributes := TAttributeCollection.Create(Self);
  FAttributes.OnChange := AttributeChanged;
  FStreams := TStreamCollection.Create(Self);
  FStreams.OnChange := StreamChanged;
end;

destructor TM3U.Destroy;
begin
  FDirectives.Free;
  FAttributes.Free;
  FStreams.Free;

  inherited Destroy;
end;

procedure TM3U.DirectiveChanged(Sender: TObject);
begin
  if Assigned(FOnDirectiveChange) then FOnDirectiveChange(Self);
end;

procedure TM3U.AttributeChanged(Sender: TObject);
begin
  if Assigned(FOnAttributeChange) then FOnAttributeChange(Self);
end;

procedure TM3U.StreamChanged(Sender: TObject);
begin
  if Assigned(FOnStreamChange) then FOnStreamChange(Self);
end;

procedure TM3U.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TM3U) then
  begin
    FDirectives.Assign((Source as TM3U).Directives);
    FAttributes.Assign((Source as TM3U).Attributes);
    FStreams.Assign((Source as TM3U).Streams);
  end else
    inherited Assign(Source);
end;

{*******************************************************}
{                   M3U Reader Class                    }
{*******************************************************}
constructor TM3UReader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLines := TStringList.Create;
  FDirectives := TDefTagCollection.Create(Self);
  FDirectives.OnChange := DirectiveChange;
  FAttributes := TDefTagCollection.Create(Self);
  FAttributes.OnChange := AttributeChange;
end;

destructor TM3UReader.Destroy;
begin
  FLines.Free;
  FDirectives.Free;
  FAttributes.Free;

  inherited Destroy;
end;

procedure TM3UReader.DirectiveChange(Sender: TObject);
begin
  if Assigned(FOnDirectiveChange) then FOnDirectiveChange(Self);
end;

procedure TM3UReader.AttributeChange(Sender: TObject);
begin
  if Assigned(FOnAttributeChange) then FOnAttributeChange(Self);
end;

procedure TM3UReader.SetFilename(const Filename: TFilename);
begin
  if FileExists(Filename) then
    FFilename := Filename
  else
    raise Exception.CreateFmt('File %s doesnt exist!', [Filename]);
end;

function TM3UReader.GetMax: Integer;
begin
  Result := FLines.Count;
end;

procedure TM3UReader.Parse(const M3U: TM3U);
type
  TLineType = (ltFileHeader, ltAttributes, ltDirective, ltSource, ltOther);

  function IsValidSource(const Line: String) : Boolean;
  begin
    Result :=  (Line.StartsWith('http://', True) or Line.StartsWith('https://', True) or Line.StartsWith('ftp://', True) or Line.StartsWith('rtmp://', True))
               or (DirectoryExists(ExtractFilePath(Line)) and TPath.HasValidFileNameChars(ExtractFileName(Line), False));
  end;

  function LineType(const Line: String) : TLineType;
  begin
    if Line.StartsWith('#EXTM3U', True) then
      Result := ltFileHeader
    else
    if Line.StartsWith('#EXTINF', True) then
      Result := ltAttributes
    else
    if Line.StartsWith('#EXT', True) then
      Result := ltDirective
    else
    if IsValidSource(Line) then
      Result := ltSource
    else
      Result := ltOther;
  end;

  procedure ParseAttributes(const Line: String; const AttributeCollection: TAttributeCollection);

    procedure ParseAttribute(const Line: String; const Attribute: TDefTag);
    var
    {$IFDEF PARSEREGEX}
      R : TRegEx;
      M : TMatch;
    {$ELSE}
      S : Integer;
      E : Integer;
      L : Integer;
      T : String;
    {$ENDIF}
    begin
    {$IFDEF PARSEREGEX}
      R := TRegEx.Create(Format('%s="(.*?)"', [Attribute.Value]), [roIgnoreCase]);
      M := R.Match(Line);
      if M.Success then
      with AttributeCollection.Add do
      begin
        Name  := Attribute.Value;
        Value := M.Groups.Item[1].Value;
      end;
    {$ELSE}
      T := Format('%s="', [Attribute.Value]);
      S := InsensPosEx(T, Line);
      if (S > 0) then
      begin
        L := Length(T);
        E := InsensPosEx(Format('"', [Attribute.Value]), Line, S + Length(T));
        T := Copy(Line, S + L, E - (S + L));
        if Length(Trim(T)) > 0 then
        with AttributeCollection.Add do
        begin
          Name  := Attribute.Value;
          Value := T;
        end;
      end;
    {$ENDIF}
    end;

  var
    I : Integer;
  begin
    for I := 0 to Attributes.Count -1 do ParseAttribute(Line, Attributes[I]);
  end;

  procedure ParseDirectives(const Line: String; const DirectiveCollection: TDirectiveCollection);

    procedure ParseDirective(const Line: String; const Directive: TDefTag);
    var
    {$IFDEF PARSEREGEX}
      R : TRegEx;
      M : TMatch;
    {$ELSE}
      S : Integer;
      T : String;
    {$ENDIF}
    begin
    {$IFDEF PARSEREGEX}
      R := TRegEx.Create(Format('(?:#%s:)(.*)', [Directive.Value]), [roIgnoreCase]);
      M := R.Match(Line);
      if M.Success then
      with DirectiveCollection.Add do
      begin
        Name  := Directive.Value;
        Value := M.Groups.Item[1].Value;
      end;
    {$ELSE}
      if Directive.Value.StartsWith('#EXT', True) then
        S := InsensPosEx(Format('%s:', [Directive.Value]), Line)
      else
        S := InsensPosEx(Format('#EXT%s:', [Directive.Value]), Line);
      if (S > 0) then
      begin
        S := Pos(':', Line) +1;
        T := Copy(Line, S, Length(Line) - S);
        if Length(Trim(T)) > 0 then
        with DirectiveCollection.Add do
        begin
          Name  := Directive.Value;
          Value := T;
        end;
      end;
    {$ENDIF}
    end;

  var
    I : Integer;
  begin
    for I := 0 to Directives.Count -1 do ParseDirective(Line, Directives[I]);
  end;

  procedure ParseM3U(const Line: String; const Stream: TStream);
  var
  {$IFDEF PARSEREGEX}
    R : TRegEx;
    M : TMatch;
  {$ELSE}
    S : Integer;
    E : Integer;
    L : Integer;
    P : Double;
  {$ENDIF}
  begin
  {$IFDEF PARSEREGEX}
    // Playlength
    R := TRegEx.Create('#EXTINF:(.*?) ');
    M := R.Match(Line);
    if M.Success then Stream.Length := M.Groups.Item[1].Value.ToDouble;
    // Title
    R := TRegEx.Create('(?!.*=",?.*")[,](.*?)$');
    M := R.Match(Line);
    if M.Success then Stream.Name := M.Groups.Item[1].Value;
  {$ELSE}
    // Playlength
    S := InsensPosEx('#EXTINF:', Line);
    L := Length('#EXTINF:');
    if (S > 0) then
    begin
      E := InsensPosEx(',', Line, S + L);
      if TryStrToFloat(Copy(Line, S + L, E - (S + L)), P) then
      begin
        Stream.Length := P;
      end;
    end;
    // Title
    S := LastDelimiter(',', Line);
    if (S > 0) then
    begin
      Stream.Name := Copy(Line, S +1, Length(Line) - S);
    end;
  {$ENDIF}
  end;

  procedure ParseSource(const Line: String; const Stream: TStream);
  begin
    Stream.Source := Line;
  end;

  procedure UpdateProgress(const LineIndex: Integer; const Line: String);
  begin
    if Assigned(FOnProgress) then FOnProgress(LineIndex, Line);
  end;

  procedure Error(const ErrorCode: Integer; const ErrorMessage: String);
  begin
    if Assigned(FOnError) then FOnError(ErrorCode, ErrorMessage);
  end;

var
  LineIndex : Integer;
  Line      : String;
  Stream    : TStream;
begin
  if M3U = nil then
  begin
    raise Exception.Create('No M3U assigned!');
  end;

  // Start
  if Assigned(FOnStart) then FOnStart(Self);

  // Parsing
  Stream := nil;
  for LineIndex := 0 to Max -1 do
  begin
    // Current Line
    Line := Lines[LineIndex];

    // Process Line
    case LineType(Line) of

      // File Header (#EXTM3U)
      ltFileHeader:
      begin
        if (LineIndex > 0) then Error(ERROR_FILE_HEADER, Format('Found another file header on line %d!', [LineIndex]));
        ParseAttributes(Line, M3U.Attributes);
      end;

      // Attributes (#EXTINF)
      ltAttributes:
      begin
        if Stream <> nil then Stream := nil;
        if Stream = nil then Stream := M3U.Streams.Add;
        ParseM3U(Line, Stream);
        ParseAttributes(Line, Stream.Attributes);
      end;

      // Directive (#EXT...)
      ltDirective:
      begin
        if Stream = nil then Stream := M3U.Streams.Add;
        ParseDirectives(Line, Stream.Directives);
      end;

      // Source (URL/Filename)
      ltSource:
      begin
        ParseSource(Line, Stream);
        if Stream <> nil then Stream := nil;
      end;

      // Other (Ignore this line)
      ltOther:
      begin
        UpdateProgress(LineIndex, Line);
      end;

    end;
  end;

  // Finish
  if Assigned(FOnFinish) then FOnFinish(Self);
end;

procedure TM3UReader.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TM3UReader) then
  begin
    FDirectives.Assign((Source as TM3UReader).Directives);
    FAttributes.Assign((Source as TM3UReader).Attributes);
  end else
    inherited Assign(Source);
end;

procedure TM3UReader.LoadFromFile(const M3U: TM3U; const Filename: String);

  function IsValidM3UFormat : Boolean;
  begin
    Result := FLines[0].StartsWith('#EXTM3U', True);
  end;

begin
  FLines.Clear;
  FLines.LoadFromFile(Filename);

  // Check if loaded file is a valid M3U file
  if not IsValidM3UFormat then
  begin
    FLines.Clear;
    raise Exception.CreateFmt('File %s is not a valid M3U file!', [ExtractFilename(Filename)]);
  end;

  FFilename := Filename;
  Parse(M3U);
end;

{*******************************************************}
{                   M3U Writer Class                    }
{*******************************************************}
procedure TM3UWriter.SaveToFile(const M3U: TM3U; const Filename: String);

  function FileHeader(const Attributes: TAttributeCollection) : String;
  var
    I : Integer;
  begin
    Result := '#EXTM3U';
    for I := 0 to Attributes.Count -1 do
    Result := Result + Format(' %s="%s"', [Attributes[I].Name, Attributes[I].Value]);
  end;

  function PlaylistDirective(const Directive: TTag) : String;
  begin
    if Directive.Name.StartsWith('#EXT', True) then
      Result := Format('%s:%s', [Directive.Name, Directive.Value])
    else
      Result := Format('#EXT%s:%s', [Directive.Name, Directive.Value]);
  end;

  function StreamHeader(const Stream: TStream) : String;
  var
    I : Integer;
  begin
    Result := Format('#EXTINF %f,', [Stream.Length]);
    for I := 0 to Stream.Attributes.Count -1 do
    Result := Result + Format(' %s="%s"', [Stream.Attributes[I].Name, Stream.Attributes[I].Value]);
    Result := Result + Format(',%s', [Stream.Name]);
  end;

  function StreamDirective(const Directive: TTag) : String;
  begin
    Result := PlaylistDirective(Directive);
  end;

  function StreamSource(const Stream: TStream) : String;
  begin
    Result := Stream.Source;
  end;

var
  Lines : TStringList;
  S, I  : Integer;
begin
  // No empty filenames
  if (Filename.Trim = '') then raise Exception.Create('The filename can''t be empty!');

  // Start
  if Assigned(FOnStart) then FOnStart(Self);

  // Write M3U File
  Lines := TStringList.Create;
  try
    // M3U File Header and Attributes
    Lines.Add(FileHeader(M3U.Attributes));
    // M3U Directives
    for I := 0 to M3U.Directives.Count -1 do
    Lines.Add(PlaylistDirective(M3U.Directives[I]));
    // Streams
    for S := 0 to M3U.Streams.Count -1 do
    begin
      // Progress
      if Assigned(FOnProgress) then FOnProgress(M3U.Streams.Count, S, M3U.Streams[S]);
      // Stream Header
      Lines.Add(StreamHeader(M3U.Streams[S]));
      // Stream Directives
      for I := 0 to M3U.Streams[S].Directives.Count -1 do
      Lines.Add(StreamDirective(M3U.Streams[S].Directives[I]));
      // Stream Source
      Lines.Add(StreamSource(M3U.Streams[S]));
    end;
    // Save to File
    Lines.SaveToFile(Filename, TEncoding.UTF8);
  finally
    Lines.Free;
  end;

  // Finished
  if Assigned(FOnFinish) then FOnFinish(Self);
end;

{*******************************************************}
{                 Xtream Reader Class                   }
{*******************************************************}
constructor TXtreamReader.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FGroupAttribute   := True;
  FGroupDirective   := False;
  FtvgNameAttribute := True;
  FtvgIDAttribute   := True;
  FtvgLogoAttribute := True;
  FtvgChnoAttribute := True;
end;

procedure TXtreamReader.LoadFromAPI(const M3U: TM3U; const StreamTypes: TXtreamStreamTypes = [stLive, stMovie, stSeries]);
type
  TURIParameters = Array of TURIParameter;

  function GetFromAPI(const Parameters: TURIParameters = []): String;
  var
    HttpClient   : THttpClient;
    HttpResponse : IHttpResponse;
    URI          : TURI;
    P            : Integer;
  begin
    HttpClient := THTTPClient.Create;
    try
      URI.Scheme := Scheme;
      URI.Host   := Host;
      URI.Port   := Port;
      URI.Path   := '/player_api.php';
      URI.AddParameter('username', Username);
      URI.AddParameter('password', Password);
      for P := Low(Parameters) to High(Parameters) do
      URI.AddParameter(Parameters[P].Name, Parameters[P].Value);
      HttpResponse := HttpClient.Get(URI.ToString);
      Result := HttpResponse.ContentAsString;
    finally
      HttpClient.Free;
    end;
  end;

  function AuthAPI : Boolean;
  var
    JSON : TJSONValue;
  begin
    try
      JSON := TJSONObject.ParseJSONValue(GetFromAPI);
      try
        JSON := (JSON as TJSONObject).Get('user_info').JsonValue;
        JSON := (JSON as TJSONObject).Get('auth').JsonValue;
        Result := (JSON.Value = '1') or (JSON.Value = 'true');
      finally
        JSON.Free;
      end;
    except
      if Assigned(FOnError) then FOnError(ERROR_JSON, 'Response is not valid JSON!');
      Result := False;
    end;
  end;

  procedure LoadStreamsFromAPI(const M3U: TM3U; const StreamType: TXtreamStreamType);
  const
    StreamTypeStr: array[TXtreamStreamType] of String = ('live_streams', 'vod_streams', 'series');
  var
    JSON       : TJSONValue;
    Item       : TJSONObject;
    Categories : TStringList;
    Stream     : TStream;
    URI        : TURI;
    I          : Integer;
  begin
    Categories := TStringList.Create;
    try
      try
        // Get Categories
        if GroupAttribute or GroupDirective then
        begin
          JSON := TJSONObject.ParseJSONValue(GetFromAPI([TURIParameter.Create('action', Format('get_%s_categories', [StreamTypeStr[StreamType]]))]));
          try
            if JSON is TJSONArray then
            begin
              for I := 0 to (JSON as TJSONArray).Count -1 do
              begin
                Item := (JSON as TJSONArray).Items[I] as TJSONObject;
                Categories.Values[Item.Get('category_id').JsonValue.Value] := Item.Get('category_name').JsonValue.Value;
              end;
            end;
          finally
            JSON.Free;
          end;
        end;
        // Get Streams
        JSON := TJSONObject.ParseJSONValue(GetFromAPI([TURIParameter.Create('action', Format('get_%s', [StreamTypeStr[StreamType]]))]));
        try
          if JSON is TJSONArray then
          begin
            URI.Scheme := Scheme;
            URI.Host   := Host;
            URI.Port   := Port;
            for I := 0 to (JSON as TJSONArray).Count -1 do
            begin
              Item := (JSON as TJSONArray).Items[I] as TJSONObject;
              case StreamType of

                // LIVE TV
                stLive: 
                begin
                  // Create Stream
                  Stream := M3U.Streams.Add;
                  // Stream Name
                  Stream.Name := Item.Get('name').JsonValue.Value;
                  // Stream Source
                  URI.Path := Format('/%s/%s/%s', [username, Password, Item.Get('stream_id').JsonValue.Value]);
                  Stream.Source := URI.ToString;
                  // Stream tvg-name attribute
                  if tvgNameAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'tvg-name';
                    Value := Item.Get('name').JsonValue.Value;
                  end;
                  // Stream tvg-id attribute
                  if tvgIDAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'tvg-id';
                    Value := Item.Get('epg_channel_id').JsonValue.Value;
                  end;
                  // Stream tvg-logo attribute
                  if tvgLogoAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'tvg-logo';
                    Value := Item.Get('stream_icon').JsonValue.Value;
                  end;
                  // Stream tvg-chno attribute
                  if tvgChnoAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'tvg-chno';
                    Value := Item.Get('num').JsonValue.Value;
                  end;
                  // Stream group-title attribute
                  if GroupAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'group-title';
                    Value := Categories.Values[Item.Get('category_id').JsonValue.Value];
                  end;
                  // Stream EXTGRP directive
                  if GroupDirective then
                  with Stream.Directives.Add do
                  begin
                    Name  := '#EXTGRP';
                    Value := Categories.Values[Item.Get('category_id').JsonValue.Value];
                  end;
                end;
                
                // MOVIES
                stMovie:
                begin
                  // Create Stream
                  Stream := M3U.Streams.Add;
                  // Stream Name
                  Stream.Name := Item.Get('name').JsonValue.Value;
                  // Stream Source       
                  URI.Path := Format('/movie/%s/%s/%s.%s', [username, Password, Item.Get('stream_id').JsonValue.Value, Item.Get('container_extension').JsonValue.Value]);
                  Stream.Source := URI.ToString;
                  // Stream tvg-name attribute
                  if tvgNameAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'tvg-name';
                    Value := Item.Get('name').JsonValue.Value;
                  end;
                  // Stream tvg-logo attribute
                  if tvgLogoAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'tvg-logo';
                    Value := Item.Get('stream_icon').JsonValue.Value;
                  end;
                  // Stream tvg-chno attribute
                  if tvgChnoAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'tvg-chno';
                    Value := Item.Get('num').JsonValue.Value;
                  end;
                  // Stream group-title attribute
                  if GroupAttribute then
                  with Stream.Attributes.Add do
                  begin
                    Name  := 'group-title';
                    Value := Categories.Values[Item.Get('category_id').JsonValue.Value];
                  end;
                  // Stream EXTGRP directive
                  if GroupDirective then
                  with Stream.Directives.Add do
                  begin
                    Name  := '#EXTGRP';
                    Value := Categories.Values[Item.Get('category_id').JsonValue.Value];
                  end;
                end;
                
                // SERIES
                stSeries: 
                begin
                  // ToDo!!!
                end;
              end;
            end;
          end;
        finally
          JSON.Free;
        end;
      except
        if Assigned(FOnError) then FOnError(ERROR_JSON, 'There was an error parsing JSON!');
      end;
    finally
      Categories.Free;
    end;
  end;

begin
  // Checks
  if Length(Host.Trim) = 0 then
  begin
    if Assigned(FOnError) then FOnError(ERROR_HOST, 'No host set!');
    Exit;
  end;
  if Length(Username.Trim) = 0 then
  begin
    if Assigned(FOnError) then FOnError(ERROR_USERNAME, 'No username set!');
    Exit;
  end;
  if Length(Password.Trim) = 0 then
  begin
    if Assigned(FOnError) then FOnError(ERROR_PASSWORD, 'No password set!');
    Exit;
  end;
  if not AuthAPI then
  begin
    if Assigned(FOnError) then FOnError(ERROR_AUTH, 'Invalid username and password combination!');
    Exit;
  end;

  // Start
  if Assigned(FOnStart) then FOnStart(Self);

  // Live Streams
  if stLive in StreamTypes then
  LoadStreamsFromAPI(M3U, stLive);
  // Movie Streams
  if stMovie in StreamTypes then
  LoadStreamsFromAPI(M3U, stMovie);

  // Finish
  if Assigned(FOnFinish) then FOnFinish(Self);
end;

procedure TXtreamReader.LoadFromURL(const M3U: TM3U; const URL: String;
  const StreamTypes: TXtreamStreamTypes = [stLive, stMovie, stSeries]);
var
  URI : TURI;
begin
  try
    URI := TURI.Create(URL);
    FScheme   := URI.Scheme;
    FHost     := URI.Host;
    FPort     := URI.Port;
    FUsername := URI.ParameterByName['username'];
    FPassword := URI.ParameterByName['password'];
  except
    raise Exception.Create('URL is not a valid Xtream Playlist URL!');
  end;
  LoadFromAPI(M3U, StreamTypes);
end;

{*******************************************************}
{            Register Classes and Component             }
{*******************************************************}
procedure Register;
begin
  // Register collection items and collections
  System.Classes.RegisterClasses([
    TTag,
    TDirectiveCollection,
    TAttributeCollection,
    TStream,
    TStreamCollection
  ]);
  // Register components
  RegisterComponents('ERDesigns', [
    TM3U,
    TM3UReader,
    TM3UWriter,
    TXtreamReader
  ]);
end;

{$IFNDEF TAGVARIANT}
initialization
  InitCharUpCaseTable(CharUpCaseTable);
{$ENDIF}

end.
