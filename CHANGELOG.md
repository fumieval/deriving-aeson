# Revision history for deriving-aeson

## 0.2.9

* Fixed a bug in chaining `ConstructorTagModifier` & `FieldLabelModifier`

## 0.2.8

* Supported GHC 9.2
* Supported aeson-2.0

## 0.2.7

* Added a `StringModifier` instance to a list of types
* Added `Rename :: Symbol -> Symbol -> Type`

## 0.2.6

* Added `StringModifier` instances to 3 and 4-tuples
* Fixed the bug making `SumTwoElemArray` point `ObjectWithSingleField`

## 0.2.5

* Added a generic `CamelTo` constructor

## 0.2.4

* Added `RejectUnknownFields`

## 0.2.3

* Fixed a bug in `SumTaggedObject`

## 0.2.2

* Added `UnwrapUnaryRecords`

## 0.2.1

* Remove redundant type variables from `Sum*`

## 0.2

* Added `Sum*` for changing the encoding of variants
* Added `Vanilla = CustomJSON '[]`
* Renamed `ContructorTagModifier` to `ConstructorTagModifier`
* Added `toEncoding` implementation to `CustomJSON`

## 0.1.2

* Reexported `CustomJSON(..)` from `Deriving.Aeson.Stock`

## 0.1.1

* Added `Deriving.Aeson.Stock`

## 0 -- 2020-02-26

* First version. Released on an unsuspecting world.
