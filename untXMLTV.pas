{
  DelphiXMLTV v1.0 - a lightweight, one-unit, cross-platform XMLTV wrapper
  for Delphi 2010 - 10.2 Tokyo by Ernst Reidinga

  This is a simple wrapper unit for XMLTV files, for easy editing and
  viewing of XMLTV files used for IPTV services.

  (c) Copyrights 2017-2022 Ernst Reidinga <ernstreidinga85@gmail.com>
  This unit is free and can be used for any needs. The introduction of
  any changes and the use of those changed library is permitted without
  limitations. Only requirement:
  This text must be present without changes in all modifications of library.

  * The contents of this file are used with permission, subject to
  * the Mozilla Public License Version 1.1 (the "License"); you may   *
  * not use this file except in compliance with the License. You may  *
  * obtain a copy of the License at                                   *
  * http:  www.mozilla.org/MPL/MPL-1.1.html                           *
  *                                                                   *
  * Software distributed under the License is distributed on an       *
  * "AS IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or    *
  * implied. See the License for the specific language governing      *
  * rights and limitations under the License.                         *

  Full XMLTV DTD Compliant: https://github.com/XMLTV/xmltv/blob/master/xmltv.dtd

  If you use this unit, please give credits to the original author;
  Ernst Reidinga.
}

unit untXMLTV;

interface

uses
  System.SysUtils,
  System.Classes,
  Winapi.Windows;

{*******************************************************}
{              Channel Display Name Class               }
{*******************************************************}
type
  TChannelDisplayName = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{           Channel Display Name Collection             }
{*******************************************************}
type
  TChannelDisplayNameCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TChannelDisplayName;
    procedure SetItem(Index: Integer; Value: TChannelDisplayName);

    function GetDisplayName(Language: String): String;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TChannelDisplayName;
    procedure Assign(Source: TPersistent); override;

    property DisplayName[Language: String]: String read GetDisplayName;
    property Items[Index: Integer]: TChannelDisplayName read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                  Channel Icon Class                   }
{*******************************************************}
type
  TChannelIcon = class(TCollectionItem)
  private
    FSource  : String;
    FWidth   : Integer;
    FHeight  : Integer;

    procedure SetSource(Source: String);
    procedure SetWidth(Width: Integer);
    procedure SetHeight(Height: Integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Source: String read FSource write SetSource;
    property Width: Integer read FWidth write SetWidth default 0;
    property Height: Integer read FHeight write SetHeight default 0;
  end;

{*******************************************************}
{               Channel Icon Collection                 }
{*******************************************************}
type
  TChannelIconCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TChannelIcon;
    procedure SetItem(Index: Integer; Value: TChannelIcon);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TChannelIcon;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TChannelIcon read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                   Channel URL Class                   }
{*******************************************************}
type
  TChannelURL = class(TCollectionItem)
  private
    FSource : String;
    procedure SetSource(Source: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Source: String read FSource write SetSource;
  end;

{*******************************************************}
{                Channel URL Collection                 }
{*******************************************************}
type
  TChannelURLCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TChannelURL;
    procedure SetItem(Index: Integer; Value: TChannelURL);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TChannelURL;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TChannelURL read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                     Channel Class                     }
{*******************************************************}
type
  TChannel = class(TCollectionItem)
  private
    FID          : String;
    FDisplayName : TChannelDisplayNameCollection;
    FIcon        : TChannelIconCollection;
    FURL         : TChannelURLCollection;

    procedure SetID(ID: String);
    procedure OnChanged(Sender: TObject);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property ID: String read FID write SetID;
    property DisplayName: TChannelDisplayNameCollection read FDisplayName;
    property Icon: TChannelIconCollection read FIcon;
    property URL: TChannelURLCollection read FURL;
  end;

{*******************************************************}
{                  Channel Collection                   }
{*******************************************************}
type
  TChannelCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TChannel;
    procedure SetItem(Index: Integer; Value: TChannel);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TChannel;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TChannel read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                 Programme Time Class                  }
{*******************************************************}
type
  TProgrammeTime = class(TPersistent)
  private
    FOnChange  : TNotifyEvent;

    FYear      : Integer;
    FMonth     : Integer;
    FDay       : Integer;
    FHour      : Integer;
    FMinute    : Integer;
    FSecond    : Integer;
    FTimeShift : Integer;

    procedure SetYear(Year: Integer);
    procedure SetMonth(Month: Integer);
    procedure SetDay(Day: Integer);
    procedure SetHour(Hour: Integer);
    procedure SetMinute(Minute: Integer);
    procedure SetSecond(Second: Integer);
    procedure SetTimeShift(TimeShift: Integer);

    procedure SetDateTime(DateTime: TDateTime);
    function GetDateTime : TDateTime;
  public
    procedure Assign(Source: TPersistent); override;

    property DateTime: TDateTime read GetDateTime write SetDateTime;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Year: Integer read FYear write SetYear;
    property Month: Integer read FMonth write SetMonth;
    property Day: Integer read FDay write SetDay;
    property Hour: Integer read FHour write SetHour;
    property Minute: Integer read FMinute write SetMinute;
    property Second: Integer read FSecond write SetSecond;
    property TimeShift: Integer read FTimeShift write SetTimeShift;
  end;

{*******************************************************}
{                 Programme Title Class                 }
{*******************************************************}
type
  TProgrammeTitle = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{              Programme Title Collection               }
{*******************************************************}
type
  TProgrammeTitleCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeTitle;
    procedure SetItem(Index: Integer; Value: TProgrammeTitle);

    function GetProgrammeTitle(Language: String) : String;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeTitle;
    procedure Assign(Source: TPersistent); override;

    property ProgrammeTitle[Language: String]: String read GetProgrammeTitle;
    property Items[Index: Integer]: TProgrammeTitle read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                Programme SubTitle Class               }
{*******************************************************}
type
  TProgrammeSubTitle = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{             Programme SubTitle Collection             }
{*******************************************************}
type
  TProgrammeSubTitleCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeSubTitle;
    procedure SetItem(Index: Integer; Value: TProgrammeSubTitle);

    function GetProgrammeSubTitle(Language: String) : String;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeSubTitle;
    procedure Assign(Source: TPersistent); override;

    property ProgrammeSubTitle[Language: String]: String read GetProgrammeSubTitle;
    property Items[Index: Integer]: TProgrammeSubTitle read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{              Programme Description Class              }
{*******************************************************}
type
  TProgrammeDescription = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{           Programme Description Collection            }
{*******************************************************}
type
  TProgrammeDescriptionCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeDescription;
    procedure SetItem(Index: Integer; Value: TProgrammeDescription);

    function GetProgrammeDescription(Language: String) : String;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeDescription;
    procedure Assign(Source: TPersistent); override;

    property ProgrammeDescription[Language: String]: String read GetProgrammeDescription;
    property Items[Index: Integer]: TProgrammeDescription read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{               Programme Category Class                }
{*******************************************************}
type
  TProgrammeCategory = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{             Programme Category Collection             }
{*******************************************************}
type
  TProgrammeCategoryCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeCategory;
    procedure SetItem(Index: Integer; Value: TProgrammeCategory);

    function GetProgrammeCategory(Language: String) : String;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeCategory;
    procedure Assign(Source: TPersistent); override;

    property ProgrammeCategory[Language: String]: String read GetProgrammeCategory;
    property Items[Index: Integer]: TProgrammeCategory read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TProgrammeLengthUnit = (luSeconds, luMinutes, luHours, luUnknown);

{*******************************************************}
{                Programme Length Class                 }
{*******************************************************}
type
  TProgrammeLength = class(TCollectionItem)
  private
    FUnit   : TProgrammeLengthUnit;
    FLength : Double;

    procedure SetUnit(&Unit: TProgrammeLengthUnit);
    procedure SetLength(Length: Double);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property &Unit: TProgrammeLengthUnit read FUnit write SetUnit;
    property Length: Double read FLength write SetLength;
  end;

{*******************************************************}
{              Programme Length Collection              }
{*******************************************************}
type
  TProgrammeLengthCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeLength;
    procedure SetItem(Index: Integer; Value: TProgrammeLength);

    function GetProgrammeLength(&Unit: TProgrammeLengthUnit) : Double;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeLength;
    procedure Assign(Source: TPersistent); override;

    property ProgrammeLength[&Unit: TProgrammeLengthUnit]: Double read GetProgrammeLength;
    property Items[Index: Integer]: TProgrammeLength read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                 Programme Icon Class                  }
{*******************************************************}
type
  TProgrammeIcon = class(TCollectionItem)
  private
    FSource : String;
    FWidth  : Integer;
    FHeight : Integer;

    procedure SetSource(Source: String);
    procedure SetWidth(Width: Integer);
    procedure SetHeight(Height: Integer);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Source: String read FSource write SetSource;
    property Width: Integer read FWidth write SetWidth;
    property Height: Integer read FHeight write SetHeight;
  end;

{*******************************************************}
{               Programme Icon Collection               }
{*******************************************************}
type
  TProgrammeIconCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeIcon;
    procedure SetItem(Index: Integer; Value: TProgrammeIcon);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeIcon;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeIcon read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                 Programme URL Class                   }
{*******************************************************}
type
  TProgrammeURL = class(TCollectionItem)
  private
    FSource : String;
    FSystem : String;

    procedure SetSource(Source: String);
    procedure SetSystem(System: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Source: String read FSource write SetSource;
    property System: String read FSystem write SetSystem;
  end;

{*******************************************************}
{               Programme URL Collection                }
{*******************************************************}
type
  TProgrammeURLCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeURL;
    procedure SetItem(Index: Integer; Value: TProgrammeURL);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeURL;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeURL read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TProgrammeEpisodeType = (etXMLTVNS, etOnScreen, etCustom);

{*******************************************************}
{                Programme Episode Class                }
{*******************************************************}
type
  TProgrammeEpisode = class(TCollectionItem)
  private
    FType    : TProgrammeEpisodeType;
    FSeason  : Integer;
    FEpisode : Integer;
    FPart    : Integer;
    FParts   : Integer;
    FCustom  : String;

    procedure SetType(&Type: TProgrammeEpisodeType);
    procedure SetSeason(Season: Integer);
    procedure SetEpisode(Episode: Integer);
    procedure SetPart(Part: Integer);
    procedure SetParts(Parts: Integer);
    procedure SetCustom(Custom: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property &Type: TProgrammeEpisodeType read FType write SetType;
    property Season: Integer read FSeason write SetSeason;
    property Episode: Integer read FEpisode write SetEpisode;
    property Part: Integer read FPart write SetPart;
    property Parts: Integer read FParts write SetParts;
    property Custom: String read FCustom write SetCustom;
  end;

{*******************************************************}
{             Programme Episode Collection              }
{*******************************************************}
type
  TProgrammeEpisodeCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeEpisode;
    procedure SetItem(Index: Integer; Value: TProgrammeEpisode);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeEpisode;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeEpisode read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                Programme Video Class                  }
{*******************************************************}
type
  TProgrammeVideo = class(TPersistent)
  private
    FOnChange : TNotifyEvent;

    FPresent  : Boolean;
    FColour   : Boolean;
    FAspect   : String;
    FQuality  : String;

    procedure SetPresent(Present: Boolean);
    procedure SetColour(Colour: Boolean);
    procedure SetAspect(Aspect: String);
    procedure SetQuality(Quality: String);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Present: Boolean read FPresent write SetPresent default True;
    property Colour: Boolean read FColour write SetColour default True;
    property Aspect: String read FAspect write SetAspect;
    property Quality: String read FQuality write SetQuality;
  end;

type
  TProgrammeAudioStereo = (asMono, asStereo, asDolby, asDolbyDigital,
                           asBilingual, asSurround, asUnknown);

{*******************************************************}
{                Programme Audio Class                  }
{*******************************************************}
type
  TProgrammeAudio = class(TPersistent)
  private
    FOnChange : TNotifyEvent;

    FPresent : Boolean;
    FStereo  : TProgrammeAudioStereo;

    procedure SetPresent(Present: Boolean);
    procedure SetStereo(Stereo: TProgrammeAudioStereo);
  public
    constructor Create; virtual;
    procedure Assign(Source: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Present: Boolean read FPresent write SetPresent default True;
    property Stereo: TProgrammeAudioStereo read FStereo write SetStereo;
  end;

type
  TProgrammeSubtitlesType = (stTeletext, stOnScreen, stDeafSigned, stUnknown);

{*******************************************************}
{              Programme Subtitles Class                }
{*******************************************************}
type
  TProgrammeSubtitles = class(TCollectionItem)
  private
    FType     : TProgrammeSubtitlesType;
    FLanguage : String;
    FValue    : String;

    procedure SetType(&Type: TProgrammeSubtitlesType);
    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property &Type: TProgrammeSubtitlesType read FType write SetType;
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{            Programme Subtitles Collection             }
{*******************************************************}
type
  TProgrammeSubtitlesCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeSubtitles;
    procedure SetItem(Index: Integer; Value: TProgrammeSubtitles);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeSubtitles;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeSubtitles read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                Programme Rating Class                 }
{*******************************************************}
type
  TProgrammeRating = class(TCollectionItem)
  private
    FSystem : String;
    FValue  : String;

    procedure SetSystem(System: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property System: String read FSystem write SetSystem;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{              Programme Rating Collection              }
{*******************************************************}
type
  TProgrammeRatingCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeRating;
    procedure SetItem(Index: Integer; Value: TProgrammeRating);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeRating;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeRating read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{              Programme Star Rating Class              }
{*******************************************************}
type
  TProgrammeStarRating = class(TPersistent)
  private
    FOnChange : TNotifyEvent;

    FValue : Double;
    FOutOf : Double;

    procedure SetValue(Value: Double);
    procedure SetOutOf(OutOf: Double);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Value: Double read FValue write SetValue;
    property OutOf: Double read FOutOf write SetOutOf;
  end;

{*******************************************************}
{           Programme Previously Shown Class            }
{*******************************************************}
type
  TProgrammePreviouslyShown = class(TPersistent)
  private
    FOnChange : TNotifyEvent;

    FStart   : String;
    FChannel : String;

    procedure SetStart(Start: String);
    procedure SetChannel(Channel: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Start: String read FStart write SetStart;
    property Channel: String read FChannel write SetChannel;
  end;

{*******************************************************}
{               Programme Premiere Class                }
{*******************************************************}
type
  TProgrammePremiere = class(TCollectionItem)
  private
    FLanguage : String;
    FValue   : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{             Programme Premiere Collection             }
{*******************************************************}
type
  TProgrammePremiereCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammePremiere;
    procedure SetItem(Index: Integer; Value: TProgrammePremiere);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammePremiere;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammePremiere read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{              Programme Last Chance Class              }
{*******************************************************}
type
  TProgrammeLastChance = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{           Programme Last Chance Collection            }
{*******************************************************}
type
  TProgrammeLastChanceCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeLastChance;
    procedure SetItem(Index: Integer; Value: TProgrammeLastChance);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeLastChance;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeLastChance read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TProgrammeReviewType = (rtText, rtURL, rtUnknown);

{*******************************************************}
{                Programme Review Class                 }
{*******************************************************}
type
  TProgrammeReview = class(TCollectionItem)
  private
    FType     : TProgrammeReviewType;
    FSource   : String;
    FReviewer : String;
    FLanguage : String;
    FValue    : String;

    procedure SetType(&Type: TProgrammeReviewType);
    procedure SetSource(Source: String);
    procedure SetReviewer(Reviewer: String);
    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property &Type: TProgrammeReviewType read FType write SetType default rtText;
    property Source: String read FSource write SetSource;
    property Reviewer: String read FReviewer write SetReviewer;
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{              Programme Review Collection              }
{*******************************************************}
type
  TProgrammeReviewCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeReview;
    procedure SetItem(Index: Integer; Value: TProgrammeReview);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeReview;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeReview read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TProgrammeImageType        = (itPoster, itBackdrop, itStill, itUnknown);
  TProgrammeImageSize        = (isSmall, isMedium, isLarge, isUnknown);
  TProgrammeImageOrientation = (ioPoster, ioLandscape, ioUnknown);

{*******************************************************}
{                Programme Image Class                  }
{*******************************************************}
type
  TProgrammeImage = class(TCollectionItem)
  private
    FType        : TProgrammeImageType;
    FSize        : TProgrammeImageSize;
    FOrientation : TProgrammeImageOrientation;
    FSystem      : String;
    FValue       : String;

    procedure SetType(&Type: TProgrammeImageType);
    procedure SetSize(Size: TProgrammeImageSize);
    procedure SetOrientation(Orientation: TProgrammeImageOrientation);
    procedure SetSystem(System: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property &Type: TProgrammeImageType read FType write SetType default itPoster;
    property Size: TProgrammeImageSize read FSize write SetSize;
    property Orientation: TProgrammeImageOrientation read FOrientation write SetOrientation;
    property System: String read FSystem write SetSystem;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{              Programme Image Collection               }
{*******************************************************}
type
  TProgrammeImageCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeImage;
    procedure SetItem(Index: Integer; Value: TProgrammeImage);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeImage;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeImage read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{               Programme Keyword Class                 }
{*******************************************************}
type
  TProgrammeKeyword = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{             Programme Keyword Collection              }
{*******************************************************}
type
  TProgrammeKeywordCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeKeyword;
    procedure SetItem(Index: Integer; Value: TProgrammeKeyword);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeKeyword;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeKeyword read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{               Programme Language Class                }
{*******************************************************}
type
  TProgrammeLanguage = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{             Programme Language Collection             }
{*******************************************************}
type
  TProgrammeLanguageCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeLanguage;
    procedure SetItem(Index: Integer; Value: TProgrammeLanguage);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeLanguage;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeLanguage read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{           Programme Original Language Class           }
{*******************************************************}
type
  TProgrammeOriginalLanguage = class(TCollectionItem)
  private
    FLanguage : String;
    FValue    : String;

    procedure SetLanguage(Language: String);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property Language: String read FLanguage write SetLanguage;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{        Programme Original Language Collection         }
{*******************************************************}
type
  TProgrammeOriginalLanguageCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeOriginalLanguage;
    procedure SetItem(Index: Integer; Value: TProgrammeOriginalLanguage);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeOriginalLanguage;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeOriginalLanguage read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

type
  TProgrammeCreditsType = (ctDirector, ctActor, ctWriter, ctAdapter, ctProducer,
           ctComposer, ctEditor, ctPresenter, ctCommentator, ctGuest, ctUnknown);

{*******************************************************}
{               Programme Credits Class                 }
{*******************************************************}
type
  TProgrammeCredits = class(TCollectionItem)
  private
    FType     : TProgrammeCreditsType;
    FRole     : String;
    FGuest    : Boolean;
    FValue    : String;

    procedure SetType(&Type: TProgrammeCreditsType);
    procedure SetRole(Role: String);
    procedure SetGuest(Guest: Boolean);
    procedure SetValue(Value: String);
  public
    procedure Assign(Source: TPersistent); override;
  published
    property &Type: TProgrammeCreditsType read FType write SetType;
    property Role: String read FRole write SetRole;
    property Guest: Boolean read FGuest write SetGuest default False;
    property Value: String read FValue write SetValue;
  end;

{*******************************************************}
{             Programme Credits Collection              }
{*******************************************************}
type
  TProgrammeCreditsCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgrammeCredits;
    procedure SetItem(Index: Integer; Value: TProgrammeCredits);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgrammeCredits;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgrammeCredits read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                    Programme Class                    }
{*******************************************************}
type
  TProgramme = class(TCollectionItem)
  private
    FStart            : TProgrammeTime;
    FStop             : TProgrammeTime;
    FChannel          : String;
    FTitle            : TProgrammeTitleCollection;
    FSubTitle         : TProgrammeSubTitleCollection;
    FDescription      : TProgrammeDescriptionCollection;
    FCategory         : TProgrammeCategoryCollection;
    FLength           : TProgrammeLengthCollection;
    FIcon             : TProgrammeIconCollection;
    FURL              : TProgrammeURLCollection;
    FCountry          : String;
    FEpisode          : TProgrammeEpisodeCollection;
    FVideo            : TProgrammeVideo;
    FAudio            : TProgrammeAudio;
    FSubtitles        : TProgrammeSubtitlesCollection;
    FRating           : TProgrammeRatingCollection;
    FStarRating       : TProgrammeStarRating;
    FDate             : String;
    FNew              : Boolean;
    FPreviouslyShown  : TProgrammePreviouslyShown;
    FPremiere         : TProgrammePremiereCollection;
    FLastChance       : TProgrammeLastChanceCollection;
    FReview           : TProgrammeReviewCollection;
    FImage            : TProgrammeImageCollection;
    FKeyword          : TProgrammeKeywordCollection;
    FLanguage         : TProgrammeLanguageCollection;
    FOriginalLanguage : TProgrammeOriginalLanguageCollection;
    FCredits          : TProgrammeCreditsCollection;

    procedure OnChanged(Sender: TObject);
    procedure SetChannel(Channel: String);
    procedure SetCountry(Country: String);
    procedure SetDate(Date: String);
    procedure SetNew(New: Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property Start: TProgrammeTime read FStart;
    property Stop: TProgrammeTime read FStop;

    property Channel: String read FChannel write SetChannel;
    property Title: TProgrammeTitleCollection read FTitle;
    property SubTitle: TProgrammeSubTitleCollection read FSubTitle;
    property Description: TProgrammeDescriptionCollection read FDescription;
    property Category: TProgrammeCategoryCollection read FCategory;
    property Length: TProgrammeLengthCollection read FLength;
    property Icon: TProgrammeIconCollection read FIcon;
    property URL: TProgrammeURLCollection read FURL;
    property Country: String read FCountry write SetCountry;
    property Episode: TProgrammeEpisodeCollection read FEpisode;
    property Video: TProgrammeVideo read FVideo;
    property Audio: TProgrammeAudio read FAudio;
    property Subtitles: TProgrammeSubtitlesCollection read FSubtitles;
    property Rating: TProgrammeRatingCollection read FRating;
    property StarRating: TProgrammeStarRating read FStarRating;
    property Date: String read FDate write SetDate;
    property New: Boolean read FNew write SetNew default False;
    property PreviouslyShown: TProgrammePreviouslyShown read FPreviouslyShown;
    property Premiere: TProgrammePremiereCollection read FPremiere;
    property LastChance: TProgrammeLastChanceCollection read FLastChance;
    property Review: TProgrammeReviewCollection read FReview;
    property Image: TProgrammeImageCollection read FImage;
    property Keyword: TProgrammeKeywordCollection read FKeyword;
    property Language: TProgrammeLanguageCollection read FLanguage;
    property OriginalLanguage: TProgrammeOriginalLanguageCollection read FOriginalLanguage;
    property Credits: TProgrammeCreditsCollection read FCredits;
  end;

{*******************************************************}
{                Programme Collection                   }
{*******************************************************}
type
  TProgrammeCollection = class(TOwnedCollection)
  private
    FOnChange: TNotifyEvent;

    procedure ItemChanged(Sender: TObject);

    function GetItem(Index: Integer): TProgramme;
    procedure SetItem(Index: Integer; Value: TProgramme);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TPersistent); reintroduce;

    function Add: TProgramme;
    procedure Assign(Source: TPersistent); override;

    property Items[Index: Integer]: TProgramme read GetItem write SetItem; default;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

{*******************************************************}
{                   XMLTV Data Class                    }
{*******************************************************}
type
  TXMLTVData = class(TPersistent)
  private
    FOnChange          : TNotifyEvent;

    FSourceInfoURL     : String;
    FSourceInfoName    : String;
    FSourceDataURL     : String;
    FGeneratorInfoName : String;
    FGeneratorInfoURL  : String;

    procedure SetSourceInfoURL(URL: String);
    procedure SetSourceInfoName(Name: String);
    procedure SetSourceDataURL(URL: String);
    procedure SetGeneratorInfoName(Name: String);
    procedure SetGeneratorInfoURL(URL: String);
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property SourceInfoURL: String read FSourceInfoURL write SetSourceInfoURL;
    property SourceInfoName: String read FSourceInfoName write SetSourceInfoName;
    property SourceDataURL: String read FSourceDataURL write SetSourceDataURL;
    property GeneratorInfoName: String read FGeneratorInfoName write SetGeneratorInfoName;
    property GeneratorInfoURL: String read FGeneratorInfoURL write SetGeneratorInfoURL;
  end;

{*******************************************************}
{                      XMLTV Class                      }
{*******************************************************}
type
  TXMLTV = class(TComponent)
  private
    FOnChange : TNotifyEvent;

    FData       : TXMLTVData;
    FChannels   : TChannelCollection;
    FProgrammes : TProgrammeCollection;

    procedure OnChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write FOnChange;

    property Data: TXMLTVData read FData;
    property Channels: TChannelCollection read FChannels;
    property Programmes: TProgrammeCollection read FProgrammes;
  end;

type
  TXMLTVReaderProgressEvent = procedure(Max: Integer; Position: Integer) of object;
  TXMLTVReaderErrorEvent    = procedure(ErrorCode: Integer; ErrorMessage: String) of object;

{*******************************************************}
{                  XMLTV Reader Class                   }
{*******************************************************}
type
  TXMLTVReader = class(TComponent)
  public const
    ERROR_TV_NODE = 0;
  private
    FOnProgress : TXMLTVReaderProgressEvent;
    FOnStart    : TNotifyEvent;
    FOnFinish   : TNotifyEvent;
    FOnError    : TXMLTVReaderErrorEvent;
  public
    procedure LoadFromStream(const XMLTV: TXMLTV; const Stream: System.Classes.TStream);
    procedure LoadFromFile(const XMLTV: TXMLTV; const Filename: String);
  published
    property OnProgress: TXMLTVReaderProgressEvent read FOnProgress write FOnProgress;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
    property OnError: TXMLTVReaderErrorEvent read FOnError write FOnError;
  end;

type
  TXMLTVWriterProgressEvent = procedure(Max: Integer; Position: Integer) of object;

{*******************************************************}
{                  XMLTV Writer Class                   }
{*******************************************************}
type
  TXMLTVWriter = class(TComponent)
  private
    FOnProgress : TXMLTVWriterProgressEvent;
    FOnStart    : TNotifyEvent;
    FOnFinish   : TNotifyEvent;
  public
    procedure SaveToStream(const XMLTV: TXMLTV; const Stream: System.Classes.TStream);
    procedure SaveToFile(const XMLTV: TXMLTV; const Filename: String);
  published
    property OnProgress: TXMLTVWriterProgressEvent read FOnProgress write FOnProgress;
    property OnStart: TNotifyEvent read FOnStart write FOnStart;
    property OnFinish: TNotifyEvent read FOnFinish write FOnFinish;
  end;

procedure Register;

implementation

uses
  System.DateUtils,
  Xml.XMLIntf,
  Xml.XMLDoc,
  XML.Win.msxmldom,
  System.Variants,
  System.Threading;

{*******************************************************}
{              Channel Display Name Class               }
{*******************************************************}
procedure TChannelDisplayName.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TChannelDisplayName.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TChannelDisplayName.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TChannelDisplayName) then
  begin
    FLanguage  := (Source as TChannelDisplayName).Language;
    FValue     := (Source as TChannelDisplayName).Value;
  end;
  Changed(False);
end;

{*******************************************************}
{           Channel Display Name Collection             }
{*******************************************************}
constructor TChannelDisplayNameCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TChannelDisplayName);
end;

procedure TChannelDisplayNameCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChannelDisplayNameCollection.GetItem(Index: Integer): TChannelDisplayName;
begin
  Result := inherited GetItem(Index) as TChannelDisplayName;
end;

procedure TChannelDisplayNameCollection.SetItem(Index: Integer; Value: TChannelDisplayName);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

function TChannelDisplayNameCollection.GetDisplayName(Language: string): string;
var
  I : Integer;
begin
  if Count > 0 then
    Result := Items[0].Value
  else
    Result := '';
  for I := 0 to Count -1 do
  if CompareText(Items[I].Language, Language) = 0 then
  begin
    Result := Items[I].Value;
    Break;
  end;
end;

procedure TChannelDisplayNameCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChannelDisplayNameCollection.Add: TChannelDisplayName;
begin
  Result := TChannelDisplayName(inherited Add);
end;

procedure TChannelDisplayNameCollection.Assign(Source: TPersistent);
var
  LI   : TChannelDisplayNameCollection;
  Loop : Integer;
begin
  if (Source is TChannelDisplayNameCollection)  then
  begin
    LI := TChannelDisplayNameCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                  Channel Icon Class                   }
{*******************************************************}
procedure TChannelIcon.SetSource(Source: string);
begin
  FSource := Source;
  Changed(False);
end;

procedure TChannelIcon.SetWidth(Width: Integer);
begin
  FWidth := Width;
  Changed(False);
end;

procedure TChannelIcon.SetHeight(Height: Integer);
begin
  FHeight := Height;
  Changed(False);
end;

procedure TChannelIcon.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TChannelIcon) then
  begin
    FSource := (Source as TChannelIcon).Source;
    FWidth  := (Source as TChannelIcon).Width;
    FHeight := (Source as TChannelIcon).Height;
  end;
  Changed(False);
end;

{*******************************************************}
{               Channel Icon Collection                 }
{*******************************************************}
constructor TChannelIconCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TChannelIcon);
end;

procedure TChannelIconCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChannelIconCollection.GetItem(Index: Integer): TChannelIcon;
begin
  Result := inherited GetItem(Index) as TChannelIcon;
end;

procedure TChannelIconCollection.SetItem(Index: Integer; Value: TChannelIcon);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TChannelIconCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChannelIconCollection.Add: TChannelIcon;
begin
  Result := TChannelIcon(inherited Add);
end;

procedure TChannelIconCollection.Assign(Source: TPersistent);
var
  LI   : TChannelIconCollection;
  Loop : Integer;
begin
  if (Source is TChannelIconCollection)  then
  begin
    LI := TChannelIconCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                   Channel URL Class                   }
{*******************************************************}
procedure TChannelURL.SetSource(Source: string);
begin
  FSource := Source;
  Changed(False);
end;

procedure TChannelURL.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TChannelURL) then
  begin
    FSource := (Source as TChannelURL).Source;
  end;
  Changed(False);
end;

{*******************************************************}
{                Channel URL Collection                 }
{*******************************************************}
constructor TChannelURLCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TChannelURL);
end;

procedure TChannelURLCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChannelURLCollection.GetItem(Index: Integer): TChannelURL;
begin
  Result := inherited GetItem(Index) as TChannelURL;
end;

procedure TChannelURLCollection.SetItem(Index: Integer; Value: TChannelURL);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TChannelURLCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChannelURLCollection.Add: TChannelURL;
begin
  Result := TChannelURL(inherited Add);
end;

procedure TChannelURLCollection.Assign(Source: TPersistent);
var
  LI   : TChannelURLCollection;
  Loop : Integer;
begin
  if (Source is TChannelURLCollection)  then
  begin
    LI := TChannelURLCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                     Channel Class                     }
{*******************************************************}
constructor TChannel.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FDisplayName := TChannelDisplayNameCollection.Create(Self);
  FDisplayName.OnChange := OnChanged;
  FIcon := TChannelIconCollection.Create(Self);
  FIcon.OnChange := OnChanged;
  FURL := TChannelURLCollection.Create(Self);
  FURL.OnChange := OnChanged;
end;

destructor TChannel.Destroy;
begin
  FDisplayName.Free;
  FIcon.Free;
  FURL.Free;

  inherited Destroy;
end;

procedure TChannel.SetID(ID: string);
begin
  FID := ID;
  Changed(False);
end;

procedure TChannel.OnChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TChannel.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TChannel) then
  begin
    FID := (Source as TChannel).ID;
    FDisplayName.Assign((Source as TChannel).DisplayName);
    FIcon.Assign((Source as TChannel).Icon);
    FURL.Assign((Source as TChannel).URL);
  end;
  Changed(False);
end;

{*******************************************************}
{                  Channel Collection                   }
{*******************************************************}
constructor TChannelCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TChannel);
end;

procedure TChannelCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChannelCollection.GetItem(Index: Integer): TChannel;
begin
  Result := inherited GetItem(Index) as TChannel;
end;

procedure TChannelCollection.SetItem(Index: Integer; Value: TChannel);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TChannelCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TChannelCollection.Add: TChannel;
begin
  Result := TChannel(inherited Add);
end;

procedure TChannelCollection.Assign(Source: TPersistent);
var
  LI   : TChannelCollection;
  Loop : Integer;
begin
  if (Source is TChannelCollection)  then
  begin
    LI := TChannelCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                 Programme Time Class                  }
{*******************************************************}
procedure TProgrammeTime.SetYear(Year: Integer);
begin
  FYear := Year;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeTime.SetMonth(Month: Integer);
begin
  FMonth := Month;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeTime.SetDay(Day: Integer);
begin
  FDay := Day;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeTime.SetHour(Hour: Integer);
begin
  FHour := Hour;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeTime.SetMinute(Minute: Integer);
begin
  FMinute := Minute;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeTime.SetSecond(Second: Integer);
begin
  FSecond := Second;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeTime.SetTimeShift(TimeShift: Integer);
begin
  FTimeShift := TimeShift;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeTime.GetDateTime: TDateTime;
begin
  Result := EncodeDateTime(Year, Month, Day, Hour, Minute, Second, 0);
end;

procedure TProgrammeTime.SetDateTime(DateTime: TDateTime);
var
  AYear, AMonth, ADay, AHour, AMinute, ASecond, AMiliSecond : Word;
begin
  DecodeDateTime(DateTime, AYear, AMonth, ADay, AHour, AMinute, ASecond, AMiliSecond);
  FYear   := AYear;
  FMonth  := AMonth;
  FDay    := ADay;
  FHour   := AHour;
  FMinute := AMinute;
  FSecond := ASecond;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeTime.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeTime) then
  begin
    FYear      := (Source as TProgrammeTime).Year;
    FMonth     := (Source as TProgrammeTime).Month;
    FDay       := (Source as TProgrammeTime).Day;
    FHour      := (Source as TProgrammeTime).Hour;
    FMinute    := (Source as TProgrammeTime).Minute;
    FSecond    := (Source as TProgrammeTime).Second;
    FTimeShift := (Source as TProgrammeTime).TimeShift;
  end;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{*******************************************************}
{                 Programme Title Class                 }
{*******************************************************}
procedure TProgrammeTitle.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeTitle.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeTitle.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeTitle) then
  begin
    FLanguage := (Source as TProgrammeTitle).Language;
    FValue    := (Source as TProgrammeTitle).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{              Programme Title Collection               }
{*******************************************************}
constructor TProgrammeTitleCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeTitle);
end;

procedure TProgrammeTitleCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);

end;

function TProgrammeTitleCollection.GetItem(Index: Integer): TProgrammeTitle;
begin
  Result := inherited GetItem(Index) as TProgrammeTitle;
end;

procedure TProgrammeTitleCollection.SetItem(Index: Integer; Value: TProgrammeTitle);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

function TProgrammeTitleCollection.GetProgrammeTitle(Language: string): string;
var
  I : Integer;
begin
  if (Count > 0) then
    Result := Items[0].Value
  else
    Result := '';
  for I := 0 to Count -1 do
  if CompareText(Items[I].Language, Language) = 0 then
  begin
    Result := Items[I].Value;
    Break;
  end;
end;

procedure TProgrammeTitleCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeTitleCollection.Add: TProgrammeTitle;
begin
  Result := TProgrammeTitle(inherited Add);
end;

procedure TProgrammeTitleCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeTitleCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeTitleCollection)  then
  begin
    LI := TProgrammeTitleCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                Programme SubTitle Class               }
{*******************************************************}
procedure TProgrammeSubTitle.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeSubTitle.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeSubTitle.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeSubTitle) then
  begin
    FLanguage := (Source as TProgrammeSubTitle).Language;
    FValue    := (Source as TProgrammeSubTitle).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{             Programme SubTitle Collection             }
{*******************************************************}
constructor TProgrammeSubTitleCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeSubTitle);
end;

procedure TProgrammeSubTitleCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);

end;

function TProgrammeSubTitleCollection.GetItem(Index: Integer): TProgrammeSubTitle;
begin
  Result := inherited GetItem(Index) as TProgrammeSubTitle;
end;

procedure TProgrammeSubTitleCollection.SetItem(Index: Integer; Value: TProgrammeSubTitle);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

function TProgrammeSubTitleCollection.GetProgrammeSubTitle(Language: string): string;
var
  I : Integer;
begin
  if (Count > 0) then
    Result := Items[0].Value
  else
    Result := '';
  for I := 0 to Count -1 do
  if CompareText(Items[I].Language, Language) = 0 then
  begin
    Result := Items[I].Value;
    Break;
  end;
end;

procedure TProgrammeSubTitleCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeSubTitleCollection.Add: TProgrammeSubTitle;
begin
  Result := TProgrammeSubTitle(inherited Add);
end;

procedure TProgrammeSubTitleCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeSubTitleCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeSubTitleCollection)  then
  begin
    LI := TProgrammeSubTitleCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{              Programme Description Class              }
{*******************************************************}
procedure TProgrammeDescription.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeDescription.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeDescription.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeDescription) then
  begin
    FLanguage := (Source as TProgrammeDescription).Language;
    FValue    := (Source as TProgrammeDescription).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{           Programme Description Collection            }
{*******************************************************}
constructor TProgrammeDescriptionCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeDescription);
end;

procedure TProgrammeDescriptionCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeDescriptionCollection.GetItem(Index: Integer): TProgrammeDescription;
begin
  Result := inherited GetItem(Index) as TProgrammeDescription;
end;

procedure TProgrammeDescriptionCollection.SetItem(Index: Integer; Value: TProgrammeDescription);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

function TProgrammeDescriptionCollection.GetProgrammeDescription(Language: string): string;
var
  I : Integer;
begin
  if (Count > 0) then
    Result := Items[0].Value
  else
    Result := '';
  for I := 0 to Count -1 do
  if CompareText(Items[I].Language, Language) = 0 then
  begin
    Result := Items[I].Value;
    Break;
  end;
end;

procedure TProgrammeDescriptionCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeDescriptionCollection.Add: TProgrammeDescription;
begin
  Result := TProgrammeDescription(inherited Add);
end;

procedure TProgrammeDescriptionCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeDescriptionCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeDescriptionCollection)  then
  begin
    LI := TProgrammeDescriptionCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{               Programme Category Class                }
{*******************************************************}
procedure TProgrammeCategory.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeCategory.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeCategory.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeCategory) then
  begin
    FLanguage := (Source as TProgrammeCategory).Language;
    FValue    := (Source as TProgrammeCategory).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{             Programme Category Collection             }
{*******************************************************}
constructor TProgrammeCategoryCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeCategory);
end;

procedure TProgrammeCategoryCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeCategoryCollection.GetItem(Index: Integer): TProgrammeCategory;
begin
  Result := inherited GetItem(Index) as TProgrammeCategory;
end;

procedure TProgrammeCategoryCollection.SetItem(Index: Integer; Value: TProgrammeCategory);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

function TProgrammeCategoryCollection.GetProgrammeCategory(Language: string): string;
var
  I : Integer;
begin
  if (Count > 0) then
    Result := Items[0].Value
  else
    Result := '';
  for I := 0 to Count -1 do
  if CompareText(Items[I].Language, Language) = 0 then
  begin
    Result := Items[I].Value;
    Break;
  end;
end;

procedure TProgrammeCategoryCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeCategoryCollection.Add: TProgrammeCategory;
begin
  Result := TProgrammeCategory(inherited Add);
end;

procedure TProgrammeCategoryCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeCategoryCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeCategoryCollection)  then
  begin
    LI := TProgrammeCategoryCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                Programme Length Class                 }
{*******************************************************}
procedure TProgrammeLength.SetUnit(&Unit: TProgrammeLengthUnit);
begin
  FUnit := &Unit;
  Changed(False);
end;

procedure TProgrammeLength.SetLength(Length: Double);
begin
  FLength := Length;
  Changed(False);
end;

procedure TProgrammeLength.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeLength) then
  begin
    FUnit   := (Source as TProgrammeLength).&Unit;
    Flength := (Source as TProgrammeLength).Length;
    Changed(False);
  end;
end;

{*******************************************************}
{              Programme Length Collection              }
{*******************************************************}
constructor TProgrammeLengthCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeCategory);
end;

procedure TProgrammeLengthCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeLengthCollection.GetItem(Index: Integer): TProgrammeLength;
begin
  Result := inherited GetItem(Index) as TProgrammeLength;
end;

procedure TProgrammeLengthCollection.SetItem(Index: Integer; Value: TProgrammeLength);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

function TProgrammeLengthCollection.GetProgrammeLength(&Unit: TProgrammeLengthUnit) : Double;
var
  I : Integer;
begin
  Result := 0;
  for I := 0 to Count -1 do
  if Items[I].&Unit = &Unit then
  begin
    Result := Items[I].Length;
    Break;
  end;
end;

procedure TProgrammeLengthCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeLengthCollection.Add: TProgrammeLength;
begin
  Result := TProgrammeLength(inherited Add);
end;

procedure TProgrammeLengthCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeLengthCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeLengthCollection)  then
  begin
    LI := TProgrammeLengthCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                 Programme Icon Class                  }
{*******************************************************}
procedure TProgrammeIcon.SetSource(Source: string);
begin
  FSource := Source;
  Changed(False);
end;

procedure TProgrammeIcon.SetWidth(Width: Integer);
begin
  FWidth := Width;
  Changed(False);
end;

procedure TProgrammeIcon.SetHeight(Height: Integer);
begin
  FHeight := Height;
  Changed(False);
end;

procedure TProgrammeIcon.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeIcon) then
  begin
    FSource := (Source as TProgrammeIcon).Source;
    FWidth  := (Source as TProgrammeIcon).Width;
    FHeight := (Source as TProgrammeIcon).Height;
    Changed(False);
  end;
end;

{*******************************************************}
{               Programme Icon Collection               }
{*******************************************************}
constructor TProgrammeIconCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeIcon);
end;

procedure TProgrammeIconCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeIconCollection.GetItem(Index: Integer): TProgrammeIcon;
begin
  Result := inherited GetItem(Index) as TProgrammeIcon;
end;

procedure TProgrammeIconCollection.SetItem(Index: Integer; Value: TProgrammeIcon);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeIconCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeIconCollection.Add: TProgrammeIcon;
begin
  Result := TProgrammeIcon(inherited Add);
end;

procedure TProgrammeIconCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeIconCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeIconCollection)  then
  begin
    LI := TProgrammeIconCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                 Programme URL Class                   }
{*******************************************************}
procedure TProgrammeURL.SetSource(Source: string);
begin
  FSource := Source;
  Changed(False);
end;

procedure TProgrammeURL.SetSystem(System: String);
begin
  FSystem := System;
  Changed(False);
end;

procedure TProgrammeURL.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeURL) then
  begin
    FSource := (Source as TProgrammeURL).Source;
    FSystem := (Source as TProgrammeURL).System;
    Changed(False);
  end;
end;

{*******************************************************}
{               Programme URL Collection                }
{*******************************************************}
constructor TProgrammeURLCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeURL);
end;

procedure TProgrammeURLCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeURLCollection.GetItem(Index: Integer): TProgrammeURL;
begin
  Result := inherited GetItem(Index) as TProgrammeURL;
end;

procedure TProgrammeURLCollection.SetItem(Index: Integer; Value: TProgrammeURL);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeURLCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeURLCollection.Add: TProgrammeURL;
begin
  Result := TProgrammeURL(inherited Add);
end;

procedure TProgrammeURLCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeURLCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeURLCollection)  then
  begin
    LI := TProgrammeURLCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                Programme Episode Class                }
{*******************************************************}
procedure TProgrammeEpisode.SetType(&Type: TProgrammeEpisodeType);
begin
  FType := &Type;
  Changed(False);
end;

procedure TProgrammeEpisode.SetSeason(Season: Integer);
begin
  FSeason := Season;
  Changed(False);
end;

procedure TProgrammeEpisode.SetEpisode(Episode: Integer);
begin
  FEpisode := Episode;
  Changed(False);
end;

procedure TProgrammeEpisode.SetPart(Part: Integer);
begin
  FPart := Part;
  Changed(False);
end;

procedure TProgrammeEpisode.SetParts(Parts: Integer);
begin
  FParts := Parts;
  Changed(False);
end;

procedure TProgrammeEpisode.SetCustom(Custom: string);
begin
  FCustom := Custom;
  Changed(False);
end;

procedure TProgrammeEpisode.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeEpisode) then
  begin
    FType    := (Source as TProgrammeEpisode).&Type;
    FSeason  := (Source as TProgrammeEpisode).Season;
    FEpisode := (Source as TProgrammeEpisode).Episode;
    FPart    := (Source as TProgrammeEpisode).Part;
    FParts   := (Source as TProgrammeEpisode).Parts;
    FCustom  := (Source as TProgrammeEpisode).Custom;
    Changed(False);
  end;
end;

{*******************************************************}
{             Programme Episode Collection              }
{*******************************************************}
constructor TProgrammeEpisodeCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeEpisode);
end;

procedure TProgrammeEpisodeCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);

end;

function TProgrammeEpisodeCollection.GetItem(Index: Integer): TProgrammeEpisode;
begin
  Result := inherited GetItem(Index) as TProgrammeEpisode;
end;

procedure TProgrammeEpisodeCollection.SetItem(Index: Integer; Value: TProgrammeEpisode);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeEpisodeCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeEpisodeCollection.Add: TProgrammeEpisode;
begin
  Result := TProgrammeEpisode(inherited Add);
end;

procedure TProgrammeEpisodeCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeEpisodeCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeEpisodeCollection)  then
  begin
    LI := TProgrammeEpisodeCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                Programme Video Class                  }
{*******************************************************}
constructor TProgrammeVideo.Create;
begin
  inherited Create;

  FPresent := True;
  FColour  := True;
end;

procedure TProgrammeVideo.SetPresent(Present: Boolean);
begin
  FPresent := Present;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeVideo.SetColour(Colour: Boolean);
begin
  FColour := Colour;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeVideo.SetAspect(Aspect: string);
begin
  FAspect := Aspect;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeVideo.SetQuality(Quality: string);
begin
  FQuality := Quality;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeVideo.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeVideo) then
  begin
    FPresent := (Source as TProgrammeVideo).Present;
    FColour  := (Source as TProgrammeVideo).Colour;
    FAspect  := (Source as TProgrammeVideo).Aspect;
    FQuality := (Source as TProgrammeVideo).Quality;
  end else
    inherited;
end;

{*******************************************************}
{                Programme Audio Class                  }
{*******************************************************}
constructor TProgrammeAudio.Create;
begin
  inherited Create;

  FPresent := True;
end;

procedure TProgrammeAudio.SetPresent(Present: Boolean);
begin
  FPresent := Present;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeAudio.SetStereo(Stereo: TProgrammeAudioStereo);
begin
  FStereo := Stereo;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeAudio.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeAudio) then
  begin
    FPresent := (Source as TProgrammeAudio).Present;
    FStereo  := (Source as TProgrammeAudio).Stereo;
  end else
    inherited;
end;

{*******************************************************}
{              Programme Subtitles Class                }
{*******************************************************}
procedure TProgrammeSubtitles.SetType(&Type: TProgrammeSubtitlesType);
begin
  FType := &Type;
  Changed(False);
end;

procedure TProgrammeSubtitles.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeSubtitles.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeSubtitles.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeSubtitles) then
  begin
    FType     := (Source as TProgrammeSubtitles).&Type;
    FLanguage := (Source as TProgrammeSubtitles).Language;
    FValue    := (Source as TProgrammeSubtitles).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{            Programme Subtitles Collection             }
{*******************************************************}
constructor TProgrammeSubtitlesCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeSubtitles);
end;

procedure TProgrammeSubtitlesCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeSubtitlesCollection.GetItem(Index: Integer): TProgrammeSubtitles;
begin
  Result := inherited GetItem(Index) as TProgrammeSubtitles;
end;

procedure TProgrammeSubtitlesCollection.SetItem(Index: Integer; Value: TProgrammeSubtitles);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeSubtitlesCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeSubtitlesCollection.Add: TProgrammeSubtitles;
begin
  Result := TProgrammeSubtitles(inherited Add);
end;

procedure TProgrammeSubtitlesCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeSubtitlesCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeSubtitlesCollection)  then
  begin
    LI := TProgrammeSubtitlesCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                Programme Rating Class                 }
{*******************************************************}
procedure TProgrammeRating.SetSystem(System: string);
begin
  FSystem := System;
  Changed(False);
end;

procedure TProgrammeRating.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeRating.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeRating) then
  begin
    FSystem := (Source as TProgrammeRating).System;
    FValue  := (Source as TProgrammeRating).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{              Programme Rating Collection              }
{*******************************************************}
constructor TProgrammeRatingCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeRating);
end;

procedure TProgrammeRatingCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeRatingCollection.GetItem(Index: Integer): TProgrammeRating;
begin
  Result := inherited GetItem(Index) as TProgrammeRating;
end;

procedure TProgrammeRatingCollection.SetItem(Index: Integer; Value: TProgrammeRating);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeRatingCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeRatingCollection.Add: TProgrammeRating;
begin
  Result := TProgrammeRating(inherited Add);
end;

procedure TProgrammeRatingCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeRatingCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeRatingCollection)  then
  begin
    LI := TProgrammeRatingCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{              Programme Star Rating Class              }
{*******************************************************}
procedure TProgrammeStarRating.SetValue(Value: Double);
begin
  FValue := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeStarRating.SetOutOf(OutOf: Double);
begin
  FOutOf := OutOf;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammeStarRating.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeStarRating) then
  begin
    FValue := (Source as TProgrammeStarRating).Value;
    FOutOf := (Source as TProgrammeStarRating).OutOf;
  end else
    inherited;
end;

{*******************************************************}
{           Programme Previously Shown Class            }
{*******************************************************}
procedure TProgrammePreviouslyShown.SetStart(Start: string);
begin
  FStart := Start;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammePreviouslyShown.SetChannel(Channel: string);
begin
  FChannel := Channel;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TProgrammePreviouslyShown.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammePreviouslyShown) then
  begin
    FStart   := (Source as TProgrammePreviouslyShown).Start;
    FChannel := (Source as TProgrammePreviouslyShown).Channel;
  end else
    inherited;
end;

{*******************************************************}
{                Programme Rating Class                 }
{*******************************************************}
procedure TProgrammePremiere.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammePremiere.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammePremiere.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammePremiere) then
  begin
    FLanguage := (Source as TProgrammePremiere).Language;
    FValue    := (Source as TProgrammePremiere).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{              Programme Rating Collection              }
{*******************************************************}
constructor TProgrammePremiereCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammePremiere);
end;

procedure TProgrammePremiereCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammePremiereCollection.GetItem(Index: Integer): TProgrammePremiere;
begin
  Result := inherited GetItem(Index) as TProgrammePremiere;
end;

procedure TProgrammePremiereCollection.SetItem(Index: Integer; Value: TProgrammePremiere);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammePremiereCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammePremiereCollection.Add: TProgrammePremiere;
begin
  Result := TProgrammePremiere(inherited Add);
end;

procedure TProgrammePremiereCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammePremiereCollection;
  Loop : Integer;
begin
  if (Source is TProgrammePremiereCollection)  then
  begin
    LI := TProgrammePremiereCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{              Programme Last Chance Class              }
{*******************************************************}
procedure TProgrammeLastChance.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeLastChance.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeLastChance.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeLastChance) then
  begin
    FLanguage := (Source as TProgrammeLastChance).Language;
    FValue    := (Source as TProgrammeLastChance).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{           Programme Last Chance Collection            }
{*******************************************************}
constructor TProgrammeLastChanceCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeLastChance);
end;

procedure TProgrammeLastChanceCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeLastChanceCollection.GetItem(Index: Integer): TProgrammeLastChance;
begin
  Result := inherited GetItem(Index) as TProgrammeLastChance;
end;

procedure TProgrammeLastChanceCollection.SetItem(Index: Integer; Value: TProgrammeLastChance);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeLastChanceCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeLastChanceCollection.Add: TProgrammeLastChance;
begin
  Result := TProgrammeLastChance(inherited Add);
end;

procedure TProgrammeLastChanceCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeLastChanceCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeLastChanceCollection)  then
  begin
    LI := TProgrammeLastChanceCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                Programme Review Class                 }
{*******************************************************}
procedure TProgrammeReview.SetType(&Type: TProgrammeReviewType);
begin
  FType := &Type;
  Changed(False);
end;

procedure TProgrammeReview.SetSource(Source: string);
begin
  FSource := Source;
  Changed(False);
end;

procedure TProgrammeReview.SetReviewer(Reviewer: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeReview.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeReview.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeReview.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeReview) then
  begin
    FType     := (Source as TProgrammeReview).&Type;
    FSource   := (Source as TProgrammeReview).Source;
    FReviewer := (Source as TProgrammeReview).Reviewer;
    FLanguage := (Source as TProgrammeReview).Language;
    FValue    := (Source as TProgrammeReview).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{              Programme Review Collection              }
{*******************************************************}
constructor TProgrammeReviewCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeReview);
end;

procedure TProgrammeReviewCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeReviewCollection.GetItem(Index: Integer): TProgrammeReview;
begin
  Result := inherited GetItem(Index) as TProgrammeReview;
end;

procedure TProgrammeReviewCollection.SetItem(Index: Integer; Value: TProgrammeReview);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeReviewCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeReviewCollection.Add: TProgrammeReview;
begin
  Result := TProgrammeReview(inherited Add);
end;

procedure TProgrammeReviewCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeReviewCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeReviewCollection)  then
  begin
    LI := TProgrammeReviewCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                Programme Image Class                  }
{*******************************************************}
procedure TProgrammeImage.SetType(&Type: TProgrammeImageType);
begin
  FType := &Type;
  Changed(False);
end;

procedure TProgrammeImage.SetSize(Size: TProgrammeImageSize);
begin
  FSize := Size;
  Changed(False);
end;

procedure TProgrammeImage.SetOrientation(Orientation: TProgrammeImageOrientation);
begin
  FOrientation := Orientation;
  Changed(False);
end;

procedure TProgrammeImage.SetSystem(System: string);
begin
  FSystem := System;
  Changed(False);
end;

procedure TProgrammeImage.SetValue(Value: string);
begin
  FType := &Type;
  Changed(False);
end;

procedure TProgrammeImage.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeImage) then
  begin
    FType        := (Source as TProgrammeImage).&Type;
    FSize        := (Source as TProgrammeImage).Size;
    FOrientation := (Source as TProgrammeImage).Orientation;
    FSystem      := (Source as TProgrammeImage).System;
    FValue       := (Source as TProgrammeImage).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{              Programme Image Collection               }
{*******************************************************}
constructor TProgrammeImageCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeImage);
end;

procedure TProgrammeImageCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeImageCollection.GetItem(Index: Integer): TProgrammeImage;
begin
  Result := inherited GetItem(Index) as TProgrammeImage;
end;

procedure TProgrammeImageCollection.SetItem(Index: Integer; Value: TProgrammeImage);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeImageCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeImageCollection.Add: TProgrammeImage;
begin
  Result := TProgrammeImage(inherited Add);
end;

procedure TProgrammeImageCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeImageCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeImageCollection)  then
  begin
    LI := TProgrammeImageCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{               Programme Keyword Class                 }
{*******************************************************}
procedure TProgrammeKeyword.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeKeyword.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeKeyword.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeKeyword) then
  begin
    FLanguage := (Source as TProgrammeKeyword).Language;
    FValue    := (Source as TProgrammeKeyword).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{             Programme Keyword Collection              }
{*******************************************************}
constructor TProgrammeKeywordCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeKeyword);
end;

procedure TProgrammeKeywordCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeKeywordCollection.GetItem(Index: Integer): TProgrammeKeyword;
begin
  Result := inherited GetItem(Index) as TProgrammeKeyword;
end;

procedure TProgrammeKeywordCollection.SetItem(Index: Integer; Value: TProgrammeKeyword);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeKeywordCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeKeywordCollection.Add: TProgrammeKeyword;
begin
  Result := TProgrammeKeyword(inherited Add);
end;

procedure TProgrammeKeywordCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeKeywordCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeKeywordCollection)  then
  begin
    LI := TProgrammeKeywordCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{               Programme Language Class                }
{*******************************************************}
procedure TProgrammeLanguage.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeLanguage.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeLanguage.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeLanguage) then
  begin
    FLanguage := (Source as TProgrammeLanguage).Language;
    FValue    := (Source as TProgrammeLanguage).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{             Programme Language Collection             }
{*******************************************************}
constructor TProgrammeLanguageCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeLanguage);
end;

procedure TProgrammeLanguageCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeLanguageCollection.GetItem(Index: Integer): TProgrammeLanguage;
begin
  Result := inherited GetItem(Index) as TProgrammeLanguage;
end;

procedure TProgrammeLanguageCollection.SetItem(Index: Integer; Value: TProgrammeLanguage);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeLanguageCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeLanguageCollection.Add: TProgrammeLanguage;
begin
  Result := TProgrammeLanguage(inherited Add);
end;

procedure TProgrammeLanguageCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeLanguageCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeLanguageCollection)  then
  begin
    LI := TProgrammeLanguageCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{           Programme Original Language Class           }
{*******************************************************}
procedure TProgrammeOriginalLanguage.SetLanguage(Language: string);
begin
  FLanguage := Language;
  Changed(False);
end;

procedure TProgrammeOriginalLanguage.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeOriginalLanguage.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeOriginalLanguage) then
  begin
    FLanguage := (Source as TProgrammeOriginalLanguage).Language;
    FValue    := (Source as TProgrammeOriginalLanguage).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{        Programme Original Language Collection         }
{*******************************************************}
constructor TProgrammeOriginalLanguageCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeOriginalLanguage);
end;

procedure TProgrammeOriginalLanguageCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeOriginalLanguageCollection.GetItem(Index: Integer): TProgrammeOriginalLanguage;
begin
  Result := inherited GetItem(Index) as TProgrammeOriginalLanguage;
end;

procedure TProgrammeOriginalLanguageCollection.SetItem(Index: Integer; Value: TProgrammeOriginalLanguage);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeOriginalLanguageCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeOriginalLanguageCollection.Add: TProgrammeOriginalLanguage;
begin
  Result := TProgrammeOriginalLanguage(inherited Add);
end;

procedure TProgrammeOriginalLanguageCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeOriginalLanguageCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeOriginalLanguageCollection)  then
  begin
    LI := TProgrammeOriginalLanguageCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{               Programme Credits Class                 }
{*******************************************************}
procedure TProgrammeCredits.SetType(&Type: TProgrammeCreditsType);
begin
  FType := &Type;
  Changed(False);
end;

procedure TProgrammeCredits.SetRole(Role: string);
begin
  FRole := Role;
  Changed(False);
end;

procedure TProgrammeCredits.SetGuest(Guest: Boolean);
begin
  FGuest := Guest;
  Changed(False);
end;

procedure TProgrammeCredits.SetValue(Value: string);
begin
  FValue := Value;
  Changed(False);
end;

procedure TProgrammeCredits.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgrammeCredits) then
  begin
    FType     := (Source as TProgrammeCredits).&Type;
    FRole     := (Source as TProgrammeCredits).Role;
    FGuest    := (Source as TProgrammeCredits).Guest;
    FValue    := (Source as TProgrammeCredits).Value;
    Changed(False);
  end;
end;

{*******************************************************}
{             Programme Credits Collection              }
{*******************************************************}
constructor TProgrammeCreditsCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgrammeCredits);
end;

procedure TProgrammeCreditsCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeCreditsCollection.GetItem(Index: Integer): TProgrammeCredits;
begin
  Result := inherited GetItem(Index) as TProgrammeCredits;
end;

procedure TProgrammeCreditsCollection.SetItem(Index: Integer; Value: TProgrammeCredits);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeCreditsCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeCreditsCollection.Add: TProgrammeCredits;
begin
  Result := TProgrammeCredits(inherited Add);
end;

procedure TProgrammeCreditsCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeCreditsCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeCreditsCollection)  then
  begin
    LI := TProgrammeCreditsCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                    Programme Class                    }
{*******************************************************}
constructor TProgramme.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FStart := TProgrammeTime.Create;
  FStart.OnChange := OnChanged;
  FStop  := TProgrammeTime.Create;
  FStop.OnChange := OnChanged;
  FTitle := TProgrammeTitleCollection.Create(Self);
  FTitle.OnChange := OnChanged;
  FSubTitle := TProgrammeSubTitleCollection.Create(Self);
  FSubTitle.OnChange := OnChanged;
  FDescription := TProgrammeDescriptionCollection.Create(Self);
  FDescription.OnChange := OnChanged;
  FCategory := TProgrammeCategoryCollection.Create(Self);
  FCategory.OnChange := OnChanged;
  FLength := TProgrammeLengthCollection.Create(Self);
  FLength.OnChange := OnChanged;
  FIcon := TProgrammeIconCollection.Create(Self);
  FIcon.OnChange := OnChanged;
  FURL := TProgrammeURLCollection.Create(Self);
  FURL.OnChange := OnChanged;
  FEpisode := TProgrammeEpisodeCollection.Create(Self);
  FEpisode.OnChange := OnChanged;
  FVideo := TProgrammeVideo.Create;
  FVideo.OnChange := OnChanged;
  FAudio := TProgrammeAudio.Create;
  FAudio.OnChange := OnChanged;
  FSubtitles := TProgrammeSubtitlesCollection.Create(Self);
  FSubtitles.OnChange := OnChanged;
  FRating := TProgrammeRatingCollection.Create(Self);
  FRating.OnChange := OnChanged;
  FStarRating := TProgrammeStarRating.Create;
  FStarRating.OnChange := OnChanged;
  FPreviouslyShown := TProgrammePreviouslyShown.Create;
  FPreviouslyShown.OnChange := OnChanged;
  FPremiere := TProgrammePremiereCollection.Create(Self);
  FPremiere.OnChange := OnChanged;
  FLastChance := TProgrammeLastChanceCollection.Create(Self);
  FLastChance.OnChange := OnChanged;
  FReview := TProgrammeReviewCollection.Create(Self);
  FReview.OnChange := OnChanged;
  FImage := TProgrammeImageCollection.Create(Self);
  FImage.OnChange := OnChanged;
  FKeyword := TProgrammeKeywordCollection.Create(Self);
  FKeyword.OnChange := OnChanged;
  FLanguage := TProgrammeLanguageCollection.Create(Self);
  FOriginalLanguage := TProgrammeOriginalLanguageCollection.Create(Self);
  FOriginalLanguage.OnChange := OnChanged;
  FCredits := TProgrammeCreditsCollection.Create(Self);
  FCredits.OnChange := OnChanged;
end;

destructor TProgramme.Destroy;
begin
  FStart.Free;
  FStop.Free;
  FTitle.Free;
  FSubTitle.Free;
  FDescription.Free;
  FCategory.Free;
  FLength.Free;
  FIcon.Free;
  FURL.Free;
  FEpisode.Free;
  FVideo.Free;
  FAudio.Free;
  FSubtitles.Free;
  FRating.Free;
  FStarRating.Free;
  FPreviouslyShown.Free;
  FPremiere.Free;
  FLastChance.Free;
  FReview.Free;
  FImage.Free;
  FKeyword.Free;
  FLanguage.Free;
  FOriginalLanguage.Free;
  FCredits.Free;

  inherited Destroy;
end;

procedure TProgramme.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TProgramme) then
  begin
    FChannel := (Source as TProgramme).Channel;
    FCountry := (Source as TProgramme).Country;
    FDate    := (Source as TProgramme).Date;
    FNew     := (Source as TProgramme).New;
    FStart.Assign((Source as TProgramme).Start);
    FStop.Assign((Source as TProgramme).Stop);
    FTitle.Assign((Source as TProgramme).Title);
    FSubTitle.Assign((Source as TProgramme).SubTitle);
    FDescription.Assign((Source as TProgramme).Description);
    FCategory.Assign((Source as TProgramme).Category);
    FLength.Assign((Source as TProgramme).Length);
    FIcon.Assign((Source as TProgramme).Icon);
    FURL.Assign((Source as TProgramme).URL);
    FEpisode.Assign((Source as TProgramme).Episode);
    FVideo.Assign((Source as TProgramme).Video);
    FAudio.Assign((Source as TProgramme).Audio);
    FSubtitles.Assign((Source as TProgramme).Subtitles);
    FRating.Assign((Source as TProgramme).Rating);
    FStarRating.Assign((Source as TProgramme).StarRating);
    FPreviouslyShown.Assign((Source as TProgramme).PreviouslyShown);
    FPremiere.Assign((Source as TProgramme).Premiere);
    FLastChance.Assign((Source as TProgramme).LastChance);
    FReview.Assign((Source as TProgramme).Review);
    FImage.Assign((Source as TProgramme).Image);
    FKeyword.Assign((Source as TProgramme).Keyword);
    FLanguage.Assign((Source as TProgramme).Language);
    FOriginalLanguage.Assign((Source as TProgramme).OriginalLanguage);
    FCredits.Assign((Source as TProgramme).Credits);
  end;
  Changed(False);
end;

procedure TProgramme.OnChanged(Sender: TObject);
begin
  Changed(False);
end;

procedure TProgramme.SetChannel(Channel: string);
begin
  FChannel := Channel;
  Changed(False);
end;

procedure TProgramme.SetCountry(Country: string);
begin
  FCountry := Country;
  Changed(False);
end;

procedure TProgramme.SetDate(Date: string);
begin
  FDate := Date;
  Changed(False);
end;

procedure TProgramme.SetNew(New: Boolean);
begin
  FNew := New;
  Changed(False);
end;

{*******************************************************}
{              Programme Rating Collection              }
{*******************************************************}
constructor TProgrammeCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TProgramme);
end;

procedure TProgrammeCollection.ItemChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeCollection.GetItem(Index: Integer): TProgramme;
begin
  Result := inherited GetItem(Index) as TProgramme;
end;

procedure TProgrammeCollection.SetItem(Index: Integer; Value: TProgramme);
begin
  inherited SetItem(Index, Value);
  ItemChanged(Self);
end;

procedure TProgrammeCollection.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TProgrammeCollection.Add: TProgramme;
begin
  Result := TProgramme(inherited Add);
end;

procedure TProgrammeCollection.Assign(Source: TPersistent);
var
  LI   : TProgrammeCollection;
  Loop : Integer;
begin
  if (Source is TProgrammeCollection)  then
  begin
    LI := TProgrammeCollection(Source);
    Clear;
    for Loop := 0 to LI.Count - 1 do Add.Assign(LI.Items[Loop]);
  end else
    inherited;
end;

{*******************************************************}
{                   XMLTV Data Class                    }
{*******************************************************}
procedure TXMLTVData.SetSourceInfoURL(URL: string);
begin
  FSourceInfoURL := URL;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TXMLTVData.SetSourceInfoName(Name: string);
begin
  FSourceInfoName := Name;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TXMLTVData.SetSourceDataURL(URL: string);
begin
  FSourceDataURL := URL;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TXMLTVData.SetGeneratorInfoName(Name: string);
begin
  FGeneratorInfoName := Name;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TXMLTVData.SetGeneratorInfoURL(URL: string);
begin
  FGeneratorInfoURL := URL;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{*******************************************************}
{                      XMLTV Class                      }
{*******************************************************}
constructor TXMLTV.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FData := TXMLTVData.Create;
  FData.OnChange := OnChanged;
  FChannels := TChannelCollection.Create(Self);
  FChannels.OnChange := OnChanged;
  FProgrammes := TProgrammeCollection.Create(Self);
  FProgrammes.OnChange := OnChanged;
end;

destructor TXMLTV.Destroy;
begin
  FData.Free;
  FChannels.Free;

  inherited Destroy;
end;

procedure TXMLTV.Assign(Source: TPersistent);
begin
  if (Source <> nil) and (Source is TXMLTV) then
  begin
    FData.Assign((Source as TXMLTV).Data);
    FChannels.Assign((Source as TXMLTV).Channels);
    FProgrammes.Assign((Source as TXMLTV).Programmes);
  end else
    inherited Assign(Source);
end;

procedure TXMLTV.OnChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

{*******************************************************}
{                  XMLTV Reader Class                   }
{*******************************************************}
procedure TXMLTVReader.LoadFromStream(const XMLTV: TXMLTV; const Stream: System.Classes.TStream);

  procedure ParseProgrammeTime(Time: String; ProgrammeTime: TProgrammeTime);
  var
    D : String;
    T : String;
  begin
    if Length(Time) = 0 then Exit;
    if Pos(' ', Time) > 0 then
    begin
      D := Copy(Time, 1, Pos(' ', Time) -1);
      T := Copy(Time, Pos(' ', Time) +1, Length(Time));
    end else
    begin
      D := Time;
      T := '';
    end;
    ProgrammeTime.Year      := StrToIntDef(Copy(D, 1, 4), 2000);
    ProgrammeTime.Month     := StrToIntDef(Copy(D, 5, 2), 1);
    ProgrammeTime.Day       := StrToIntDef(Copy(D, 7, 2), 1);
    ProgrammeTime.Hour      := StrToIntDef(Copy(D, 9, 2), 0);
    ProgrammeTime.Minute    := StrToIntDef(Copy(D, 11, 2), 0);
    ProgrammeTime.Second    := StrToIntDef(Copy(D, 13, 2), 0);
    ProgrammeTime.TimeShift := StrToIntDef(T, 0);
  end;

  function LengthUnit(&Unit: String) : TProgrammeLengthUnit;
  begin
    if &Unit.ToLower = 'seconds' then
      Result := luSeconds
    else
    if &Unit.ToLower = 'minutes' then
      Result := luMinutes
    else
    if &Unit.ToLower = 'hours' then
      Result := luHours
    else
      Result := luUnknown;
  end;

  function EpisodeSystem(System: String) : TProgrammeEpisodeType;
  begin
    if System.ToLower = 'xmltv_ns' then
      Result := etXMLTVNS
    else
    if System.ToLower = 'onscreen' then
      Result := etOnScreen
    else
      Result := etCustom;
  end;

  function ParseXMLTVNS(Str: String; Part: Integer) : Integer;
  var
    P : Integer;
    S : String;
  begin
    case Part of
      0: // Season
      begin
        P := Pos('.', Str);
        S := Copy(Str, 1, P -1);
        Result := StrToIntDef(S, -1);
      end;

      1: // Episode
      begin
        P := Pos('.', Str);
        S := Copy(Str, P + 1, Pos('.', Str, P +1) -1);
        Result := StrToIntDef(S, -1);
      end;

      2: // Part
      begin
        P := LastDelimiter('.', Str);
        S := Copy(Str, P +1, Length(Str));
        P := Pos('/', S);
        if P > 0 then S := Copy(S, 1, P -1);
        Result := StrToIntDef(S, -1);
      end;

      3: // Parts
      begin
        P := LastDelimiter('.', Str);
        S := Copy(Str, P +1, Length(Str));
        P := Pos('/', S);
        if P > 0 then S := Copy(S, P +1, Length(S));
        Result := StrToIntDef(S, -1);
      end;

      else Result := -1;
    end;
  end;

  function AudioStereo(Stereo: String) : TProgrammeAudioStereo;
  begin
    if Stereo.ToLower = 'mono' then
      Result := asMono
    else
    if Stereo.ToLower = 'stereo' then
      Result := asStereo
    else
    if Stereo.ToLower = 'dolby' then
      Result := asDolby
    else
    if Stereo.ToLower = 'dolby digital' then
      Result := asDolbyDigital
    else
    if Stereo.ToLower = 'bilingual' then
      Result := asBilingual
    else
    if Stereo.ToLower = 'surround' then
      Result := asSurround
    else
      Result := asUnknown;
  end;

  function SubtitleType(&Type: String) : TProgrammeSubtitlesType;
  begin
    if &Type.ToLower = 'teletext' then
      Result := stTeletext
    else
    if &Type.ToLower = 'onscreen' then
      Result := stOnScreen
    else
    if &Type.ToLower = 'deaf-signed' then
      Result := stDeafSigned
    else
      Result := stUnknown;
  end;

  function ParseStarRating(StarRating: String; Part: Integer) : Double;
  var
    P : Integer;
    S : String;
  begin
    case Part of
      0: // Stars
      begin
        P := Pos('/', StarRating);
        S := Copy(StarRating, 1, P -1);
        Result := StrToFloatDef(S, 0);
      end;

      1: // Out of
      begin
        P := Pos('/', StarRating);
        S := Copy(StarRating, P +1, Length(StarRating));
        Result := StrToFloatDef(S, 0);
      end;

      else Result := 0;
    end;
  end;

  function ReviewType(&Type: String) : TProgrammeReviewType;
  begin
    if &Type.ToLower = 'text' then
      Result := rtText
    else
    if &Type.ToLower = 'url' then
      Result := rtURL
    else
      Result := rtUnknown;
  end;

  function ImageType(&Type: String) : TProgrammeImageType;
  begin
    if &Type.ToLower = 'poster' then
      Result := itPoster
    else
    if &Type.ToLower = 'backdrop' then
      Result := itBackdrop
    else
    if &Type.ToLower = 'still' then
      Result := itStill
    else
      Result := itUnknown;
  end;

  function ImageSize(Size: String) : TProgrammeImageSize;
  begin
    case StrToIntDef(Size, -1) of
      0 : Result := isSmall;
      1 : Result := isMedium;
      2 : Result := isLarge;

      else Result := isUnknown;
    end;
  end;

  function ImageOrientation(Orientation: String) : TProgrammeImageOrientation;
  begin
    if Orientation.ToUpper = 'P' then
      Result := ioPoster
    else
    if Orientation.ToUpper = 'L' then
      Result := ioLandscape
    else
      Result := ioUnknown;
  end;

  function CreditsType(&Type: String) : TProgrammeCreditsType;
  begin
    if &Type.ToLower = 'director' then
      Result := ctDirector
    else
    if &Type.ToLower = 'actor' then
      Result := ctActor
    else
    if &Type.ToLower = 'writer' then
      Result := ctWriter
    else
    if &Type.ToLower = 'adapter' then
      Result := ctAdapter
    else
    if &Type.ToLower = 'producer' then
      Result := ctProducer
    else
    if &Type.ToLower = 'composer' then
      Result := ctComposer
    else
    if &Type.ToLower = 'editor' then
      Result := ctEditor
    else
    if &Type.ToLower = 'presenter' then
      Result := ctPresenter
    else
    if &Type.ToLower = 'commentator' then
      Result := ctCommentator
    else
    if &Type.ToLower = 'guest' then
      Result := ctGuest
    else
      Result := ctUnknown;
  end;

var
  XML           : IXMLDocument;
  ERR           : Boolean;
  TV, CN, PN    : IXMLNode;
  I, J, L, Max  : Integer;
  Chn           : TChannel;
  Prog          : TProgramme;
begin
  Err := False;
  XML := NewXMLDocument;
  // Load XML
  XML.LoadFromStream(Stream, xetUTF_8);
  XML.Active := True;
  // Check TV node
  if XML.DocumentElement.NodeName <> 'tv' then
  begin
    Err := True;
    if Assigned(FOnError) then FOnError(ERROR_TV_NODE, 'Missing TV node!');
  end;
  // Start
  if Assigned(FOnStart) then FOnStart(Self);
  if not Err then
  begin
    TV := XML.DocumentElement;
    with XMLTV.Data do
    begin
      SourceInfoURL     := VarToStrDef(TV.Attributes['source-info-url'], '');
      SourceInfoName    := VarToStrDef(TV.Attributes['source-info-name'], '');
      SourceDataURL     := VarToStrDef(TV.Attributes['source-data-url'], '');
      GeneratorInfoName := VarToStrDef(TV.Attributes['generator-info-name'], '');
      GeneratorInfoURL  := VarToStrDef(TV.Attributes['generator-info-url'], '');
    end;
    Max := TV.ChildNodes.Count;
    for I := 0 to TV.ChildNodes.Count -1 do
    begin
      // Progress
      if Assigned(FOnProgress) then FOnProgress(Max, I);
      // Add Channel
      if TV.ChildNodes[I].NodeName = 'channel' then
      begin
        CN  := TV.ChildNodes[I];
        Chn := XMLTV.Channels.Add;
        Chn.ID := VarToStrDef(CN.Attributes['id'], '');
        for J := 0 to CN.ChildNodes.Count -1 do
        begin
          // Display name
          if CN.ChildNodes[J].NodeName = 'display-name' then
          with Chn.DisplayName.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang'], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Icon
          if CN.ChildNodes[J].NodeName = 'icon' then
          with Chn.Icon.Add do
          begin
            Source := VarToStrDef(CN.ChildNodes[J].Attributes['src'], '');
            Width  := StrToIntDef(VarToStrDef(CN.ChildNodes[J].Attributes['width'], ''), 0);
            Height := StrToIntDef(VarToStrDef(CN.ChildNodes[J].Attributes['height'], ''), 0);
          end;
          // URL
          if CN.ChildNodes[J].NodeName = 'url' then
          with Chn.URL.Add do
          begin
            Source := CN.ChildNodes[J].Text;
          end;
        end;
      end;
      // Add Programme
      if TV.ChildNodes[I].NodeName = 'programme' then
      begin
        CN  := TV.ChildNodes[I];
        Prog := XMLTV.Programmes.Add;
        Prog.Channel := VarToStrDef(CN.Attributes['channel'], '');
        ParseProgrammeTime(VarToStrDef(CN.Attributes['start'], ''), Prog.Start);
        ParseProgrammeTime(VarToStrDef(CN.Attributes['stop'], ''), Prog.Stop);
        for J := 0 to CN.ChildNodes.Count -1 do
        begin
          // Title
          if CN.ChildNodes[J].NodeName = 'title' then
          with Prog.Title.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang'], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Subtitle
          if CN.ChildNodes[J].NodeName = 'sub-title' then
          with Prog.SubTitle.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang'], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Description
          if CN.ChildNodes[J].NodeName = 'desc' then
          with Prog.Description.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang'], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Category
          if CN.ChildNodes[J].NodeName = 'category' then
          with Prog.Category.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang'], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Length
          if CN.ChildNodes[J].NodeName = 'length' then
          with Prog.Length.Add do
          begin
            &Unit  := LengthUnit(VarToStrDef(CN.ChildNodes[J].Attributes['units'], ''));
            Length := StrToFloatDef(CN.ChildNodes[J].Text, 0);
          end;
          // Icon
          if CN.ChildNodes[J].NodeName = 'icon' then
          with Prog.Icon.Add do
          begin
            Source := VarToStrDef(CN.ChildNodes[J].Attributes['src'], '');
            Width  := StrToIntDef(VarToStrDef(CN.ChildNodes[J].Attributes['width'], ''), 0);
            Height := StrToIntDef(VarToStrDef(CN.ChildNodes[J].Attributes['height'], ''), 0);
          end;
          // URL
          if CN.ChildNodes[J].NodeName = 'url' then
          with Prog.URL.Add do
          begin
            Source := CN.ChildNodes[J].Text;
            System := VarToStrDef(CN.ChildNodes[J].Attributes['system'], '');
          end;
          // Country  - ToDo: Change to pair (lang - value)
          if CN.ChildNodes[J].NodeName = 'country' then
          Prog.Country := CN.ChildNodes[J].Text;
          // Episode
          if CN.ChildNodes[J].NodeName = 'episode-num' then
          with Prog.Episode.Add do
          begin
            &Type := EpisodeSystem(VarToStrDef(CN.ChildNodes[J].Attributes['system'], ''));
            if &Type = etXMLTVNS then
            begin
              Season  := ParseXMLTVNS(CN.ChildNodes[J].Text, 0);
              Episode := ParseXMLTVNS(CN.ChildNodes[J].Text, 1);
              Part    := ParseXMLTVNS(CN.ChildNodes[J].Text, 2);
              Parts   := ParseXMLTVNS(CN.ChildNodes[J].Text, 3);
            end;
            Custom  := CN.ChildNodes[J].Text;
          end;
          // Video
          if CN.ChildNodes[J].NodeName = 'video' then
          with Prog.Video do
          begin
            for L := 0 to CN.ChildNodes[J].ChildNodes.Count -1 do
            begin
              if CN.ChildNodes[J].ChildNodes[L].NodeName = 'present' then
              Present := CompareText(CN.ChildNodes[J].ChildNodes[L].Text, 'yes') = 0;
              if CN.ChildNodes[J].ChildNodes[L].NodeName = 'colour' then
              Colour := CompareText(CN.ChildNodes[J].ChildNodes[L].Text, 'yes') = 0;
              if CN.ChildNodes[J].ChildNodes[L].NodeName = 'aspect' then
              Aspect := CN.ChildNodes[J].ChildNodes[L].Text;
              if CN.ChildNodes[J].ChildNodes[L].NodeName = 'quality' then
              Quality := CN.ChildNodes[J].ChildNodes[L].Text;
            end;
          end;
          // Audio
          if CN.ChildNodes[J].NodeName = 'audio' then
          with Prog.Audio do
          begin
            for L := 0 to CN.ChildNodes[J].ChildNodes.Count -1 do
            begin
              if CN.ChildNodes[J].ChildNodes[L].NodeName = 'present' then
              Present := CompareText(CN.ChildNodes[J].ChildNodes[L].Text, 'yes') = 0;
              if CN.ChildNodes[J].ChildNodes[L].NodeName = 'stereo' then
              Stereo  := AudioStereo(CN.ChildNodes[J].ChildNodes[L].Text);
            end;
          end;
          // Subtitles
          if CN.ChildNodes[J].NodeName = 'subtitles' then
          begin
            for L := 0 to CN.ChildNodes[J].ChildNodes.Count -1 do
            with Prog.Subtitles.Add do
            begin
              &Type    := SubtitleType(VarToStrDef(CN.ChildNodes[J].Attributes['type'], ''));
              Language := VarToStrDef(CN.ChildNodes[J].ChildNodes[L].Attributes['lang'], '');
              Value    := CN.ChildNodes[J].ChildNodes[L].Text;
            end;
          end;
          // Rating
          if CN.ChildNodes[J].NodeName = 'rating' then
          begin
            with Prog.Rating.Add do
            begin
              for L := 0 to CN.ChildNodes[J].ChildNodes.Count -1 do
              if CN.ChildNodes[J].ChildNodes[L].NodeName = 'value' then
              Value  := CN.ChildNodes[J].ChildNodes[L].Text;
              System := VarToStrDef(CN.ChildNodes[J].Attributes['system'], '');
            end;
          end;
          // Star Rating
          if CN.ChildNodes[J].NodeName = 'star-rating' then
          begin
            for L := 0 to CN.ChildNodes[J].ChildNodes.Count -1 do
            if CN.ChildNodes[J].ChildNodes[L].NodeName = 'value' then
            with Prog.StarRating do
            begin
              Value := ParseStarRating(CN.ChildNodes[J].ChildNodes[L].Text, 0);
              OutOf := ParseStarRating(CN.ChildNodes[J].ChildNodes[L].Text, 1);
            end;
          end;
          // Date
          if CN.ChildNodes[J].NodeName = 'date' then
          Prog.Date := CN.ChildNodes[J].Text;
          // New
          if CN.ChildNodes[J].NodeName = 'new' then
          Prog.New := True;
          // Previously Shown
          if CN.ChildNodes[J].NodeName = 'previously-shown' then
          with Prog.PreviouslyShown do
          begin
            Start   := VarToStrDef(CN.ChildNodes[J].Attributes['start'], '');
            Channel := VarToStrDef(CN.ChildNodes[J].Attributes['channel'], '');
          end;
          // Premiere
          if CN.ChildNodes[J].NodeName = 'premiere' then
          with Prog.Premiere.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang'], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Last Chance
          if CN.ChildNodes[J].NodeName = 'last-chance' then
          with Prog.LastChance.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang'], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Review
          if CN.ChildNodes[J].NodeName = 'review' then
          with Prog.Review.Add do
          begin
            &Type    := ReviewType(VarToStrDef(CN.ChildNodes[J].Attributes['type'], ''));
            Source   := VarToStrDef(CN.ChildNodes[J].Attributes['source'], '');
            Reviewer := VarToStrDef(CN.ChildNodes[J].Attributes['reviewer '], '');
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang '], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Image
          if CN.ChildNodes[J].NodeName = 'image' then
          with Prog.Image.Add do
          begin
            &Type       := ImageType(VarToStrDef(CN.ChildNodes[J].Attributes['type'], ''));
            Size        := ImageSize(VarToStrDef(CN.ChildNodes[J].Attributes['size'], ''));
            Orientation := ImageOrientation(VarToStrDef(CN.ChildNodes[J].Attributes['orient'], ''));
            System      := VarToStrDef(CN.ChildNodes[J].Attributes['system'], '');
            Value       := CN.ChildNodes[J].Text;
          end;
          // Keyword
          if CN.ChildNodes[J].NodeName = 'keyword' then
          with Prog.Keyword.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang '], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Language
          if CN.ChildNodes[J].NodeName = 'language' then
          with Prog.Language.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang '], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Original Language
          if CN.ChildNodes[J].NodeName = 'orig-language' then
          with Prog.OriginalLanguage.Add do
          begin
            Language := VarToStrDef(CN.ChildNodes[J].Attributes['lang '], '');
            Value    := CN.ChildNodes[J].Text;
          end;
          // Credits
          if CN.ChildNodes[J].NodeName = 'credits' then
          for L := 0 to CN.ChildNodes[J].ChildNodes.Count -1 do
          with Prog.Credits.Add do
          begin
            &Type := CreditsType(CN.ChildNodes[J].ChildNodes[L].NodeName);
            Role  := VarToStrDef(CN.ChildNodes[J].ChildNodes[L].Attributes['role '], '');
            Guest := CompareText(VarToStrDef(CN.ChildNodes[J].ChildNodes[L].Attributes['guest '], ''), 'yes') = 0;
            Value := CN.ChildNodes[J].ChildNodes[L].Text;
          end;
        end;
      end;
    end;
  end;
  // Finish
  if Assigned(FOnFinish) then FOnFinish(Self);
end;

procedure TXMLTVReader.LoadFromFile(const XMLTV: TXMLTV; const Filename: string);
var
  FS : TFileStream;
begin
  if not FileExists(Filename) then
    raise Exception.CreateFmt('File does''nt exist!! %s', [Filename]);
  FS := TFileStream.Create(Filename, fmOpenRead);
  try
    LoadFromStream(XMLTV, FS);
  finally
    FS.Free;
  end;
end;

{*******************************************************}
{                  XMLTV Writer Class                   }
{*******************************************************}
procedure TXMLTVWriter.SaveToStream(const XMLTV: TXMLTV; const Stream: System.Classes.TStream);
begin
  // ToDo
end;

procedure TXMLTVWriter.SaveToFile(const XMLTV: TXMLTV; const Filename: string);
var
  FS : TFileStream;
begin
  if (Filename.Trim.Length = 0) then raise Exception.Create('Filename can''t be empty!');
  FS := TFileStream.Create(Filename, fmOpenWrite);
  try
    SaveToStream(XMLTV, FS);
  finally
    FS.Free;
  end;
end;

{*******************************************************}
{                 Register Components                   }
{*******************************************************}
procedure Register;
begin
  RegisterComponents('ERDesigns IPTV - TVGuide', [
    TXMLTV,
    TXMLTVReader,
    TXMLTVWriter
  ]);
end;

initialization
  Xml.Win.msxmldom.MSXMLDOMDocumentFactory.AddDOMProperty('ProhibitDTD', False);

end.
