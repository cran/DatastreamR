#' @include Common.R
library(R6)

DSSymbolResponseValueType = list (Error = 0,
                                  Empty = 1,
                                  Bool = 2,
                                  Int = 3,
                                  DateTime = 4,
                                  Double = 5,
                                  String = 6,
                                  BoolArray = 7,
                                  IntArray = 8,
                                  DateTimeArray = 9,
                                  DoubleArray = 10,
                                  StringArray = 11,
                                  ObjectArray = 12,
                                  NullableBoolArray = 13,
                                  NullableIntArray = 14,
                                  NullableDateTimeArray = 15,
                                  NullableDoubleArray = 16)


DSKVPair = R6Class("DSKVPair",
                    public = list(Key = NULL, Value = NULL,

          initialize = function(key, value)
          {
             self$Key = key
             self$Value = value
          }
))


DSSymbolResponseValue = R6Class("DSSymbolResponseValue",
                                public = list(Currency = NULL, Symbol = NULL,
                                              Type = NA, Value = NULL,
# Currency and Symbol returned by MF can be empty or NULL sometimes,
          initialize = function(currency, symbol, type, value, useNaNforNotANumber)
          {
              self$Currency = currency
              self$Symbol = symbol

              if (type %in% DSSymbolResponseValueType)
                self$Type = type
              else
                self$Type = DSSymbolResponseValueType$Error

              # Convert JSON format date to DateTime
              if (self$Type == DSSymbolResponseValueType$DateTime)
                self$Value = jsonDateToDatetime(value)
              else if (self$Type == DSSymbolResponseValueType$ObjectArray)
              {
                  self$Value = list()
                  for (val in value)
                  {
                      if (is.character(val))
                      {
                          match1 = grep(pattern = "^/Date\\(-?\\d*\\)/", x = val, value = TRUE)
                          match2 = grep(pattern = "^/Date\\(-?\\d*\\+|-\\d*\\)/", x = val, value = TRUE)

                          if (!identical(match1, character(0)) ||
                              !identical(match2, character(0)))
                          {
                              dtVal = jsonDateToDatetime(val)
                              self$Value = append(self$Value, as.character(dtVal))
                          }
                      }
                      else if (is.null(val))
                        self$Value = append(self$Value, NaN)
                      else
                        self$Value = append(self$Value, val)
                  }
            }
            else if (self$Type == DSSymbolResponseValueType$DateTimeArray ||
                    self$Type == DSSymbolResponseValueType$NullableDateTimeArray)
            {
                self$Value = list()
                for (val in value)
                {
                    dtVal = jsonDateToDatetime(val)
                    self$Value = append(self$Value, as.character(dtVal))
                }
            }
            else
            {
                if (useNaNforNotANumber)
                  self$Value = lapply(value, FUN = function(x) {return (ifelse(is.null(x), NaN, x))})
                else
                  self$Value = value
            }
          }
))


DSDataTypeResponseValue = R6Class("DSDataTypeResponseValue",
                                  public = list(DataType = character(5), SymbolValues = NULL,

        initialize = function(datatype, symbolValues, useNaNforNotANumber)
        {
            self$DataType = datatype
            self$SymbolValues = lapply(symbolValues, FUN = function(x){
                return (DSSymbolResponseValue$new(x$Currency, x$Symbol, x$Type, x$Value, useNaNforNotANumber))
                })
        }
))


DSDataResponse = R6Class("DSDataResponse",
                          public = list(Dates = NA, DataTypeValues = NA, DataTypeNames = NULL,
                                        SymbolNames = NULL, AdditionalResponses = NULL, Tag = NULL,

        initialize = function(dates, dataTypeValues, dataTypeNames, symbolNames, additionalResponses, tag, useNaNforNotANumber)
        {
            self$Dates = lapply (lapply (dates, FUN = jsonDateToDatetime), as.Date)

            self$DataTypeValues = lapply(dataTypeValues, FUN = function(x)
            {
                return (DSDataTypeResponseValue$new(x$DataType, x$SymbolValues, useNaNforNotANumber))
            })

           self$DataTypeNames = dataTypeNames
           self$SymbolNames = symbolNames
           self$AdditionalResponses = lapply(additionalResponses, FUN = function(x)
           {

             return (DSKVPair$new(x$Key, x$Value))
           })

          self$Tag = tag
        }
))


DSGetDataResponse = R6Class("DSGetDataResponse",
                             public = list(DataResponse = NULL, Properties = NULL,
        initialize = function(dataResponse, properties, useNaNforNotANumber)
        {
            self$DataResponse = DSDataResponse$new(dataResponse$Dates, dataResponse$DataTypeValues,
                                      dataResponse$DataTypeNames, dataResponse$SymbolNames,
                                      dataResponse$AdditionalResponses, dataResponse$Tag,
                                      useNaNforNotANumber)
            self$Properties = list(properties)
        }
))


DSGetDataBundleResponse = R6Class("DSGetDataBundleResponse",
                                  public = list(DataResponses = NA, Properties = NULL,
        initialize = function(dataResponses, properties, useNaNforNotANumber)
        {
            self$DataResponses = lapply(dataResponses, FUN =  function(x)
            {
                return (DSDataResponse$new(x$Dates, x$DataTypeValues, x$DataTypeNames, x$SymbolNames,
                                        x$AdditionalResponses, x$Tag, useNaNforNotANumber))
            })

            self$Properties = list(properties)
        }
))





