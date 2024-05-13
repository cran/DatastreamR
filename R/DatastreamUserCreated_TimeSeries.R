#' @include DSUserDataObjectBase.R
#' @include DSConnect.R
#' @include Common.R


#' @name DSTimeSeriesFrequencyConversion
#' @title DSTimeSeriesFrequencyConversion
#'
#' @description This list is a supporting attribute for the FrequencyConversion properties of the
#'              DSTimeSeriesRequestObject and DSTimeSeriesResponseObjects.
#'              This enumeration specifies how to return values if your end users requests the
#'              timeseries at a lower frequency.
#'              For example: if your timeseries has daily  frequency data, and your user requests monthly or
#'              quarterly data, the FrequencyConversion property instructs the mainframe how to return monthly
#'              or quarterly data.
#'
#' @field EndValue The daily value for the end of the requested period will be returned.
#' @field AverageValue The average of all the values for the requested period will be returned.
#' @field SumValues The sum of all the values for the requested period will be returned.
#' @field ActualValue The actual value for the requested start date will be returned for the same given date
#' in the requested period.
#'
#' @return numeric
#' @export DSTimeSeriesFrequencyConversion

DSTimeSeriesFrequencyConversion = list ( EndValue = 0,
                                         AverageValue = 1,
                                         SumValues = 2,
                                         ActualValue = 3
)

#' @name DSTimeSeriesDateAlignment
#' @title DSTimeSeriesDateAlignment
#'
#' @description This list is a supporting attribute for the DateAlignment properties of the DSTimeSeriesRequestObject
#' and DSTimeSeriesResponseObjects. When you supply monthly, quarterly or annual data, the dates are stored internally
#' as the first day of the given period and always returned to you through this interface as the first date of the given
#' period. However, when your users request data from Datastream, you can specify whether the dates returned to users are
#' returned with dates set as the start, mid or end of the requested period
#'
#' @field EndPeriod This will return dates to your users that represent the last day of the month, quarter or year.
#' @field StartPeriod This will return dates to your users that represent the first day of the month, quarter or year.
#' @field MidPeriod This will return dates to your users that represent the middle of the month (15th day), quarter
#' (15th of the middle month) or year (30th June).
#'
#' @return numeric
#' @export DSTimeSeriesDateAlignment

DSTimeSeriesDateAlignment = list ( EndPeriod = 0,
                                   StartPeriod = 1,
                                   MidPeriod = 2
)

#' @name DSTimeSeriesCarryIndicator
#' @title DSTimeSeriesCarryIndicator
#'
#' @description This list is a supporting attribute for the CarryIndicator properties of the DSTimeSeriesRequestObject
#' and DSTimeSeriesResponseObjects. When you supply data which contains 'Not A Number' values (NaN) to denote non trading
#' days, this list instructs the mainframe in how to store the values.
#'
#' @field Yes Any incoming NaN values are replaced with the last non-NaN value (e.g. 1,2,3,NaN,5,NaN,7,8
#'  will be converted and stored as 1,2,3,3,5,5,7,8).
#' @field No Any incoming NaN values are stored as is and returned as NaN values.
#' @field Pad This is similar to YES, but also pads the final value for any dates your users may request beyond the last
#' date in your timeseries.\cr
#' For example, if your timeseries supplies just 3 values 1, NaN and 3, and your user requests a range of dates two days
#' before and two days after your range, your user will receive the following values: \cr
#' No:  NaN, NaN, 1, NaN, 3, NaN, NaN \cr
#' Yes: NaN, NaN, 1, 1, 3, NaN, NaN \cr
#' Pad: NaN, NaN, 1, 1, 3, 3, 3 \cr
#'
#' @return numeric
#' @export DSTimeSeriesCarryIndicator
#'
DSTimeSeriesCarryIndicator = list ( Yes = 0,
                                    No = 1,
                                    Pad = 2)

#' @name DSTimeSeriesDataInput
#' @title DSTimeSeriesDataInput
#'
#' @description This class is a supporting attribute for the DateInput property of the DSTimeSeriesRequestObject.
#' It is used to supply the raw data for the timeseries.
#'
#' @field StartDate A datetime value defining the start date for the timeseries.
#' @field EndDate A datetime value defining the end date for the timeseries.
#' @field Frequency The frequency of the timeseries. One of the DSUserObjectFrequency values defined in
#' DSUserDataObjectBase.R
#' @field Values An array of float values. Use NULL to represent NotANumber for non-trading days. Alternatively, if you
#' set the DatastreamUserCreated_TimeSeries property useNaNforNotANumber as True, you can use float NaN values.
#'
#' @note Datastream takes the StartDate, Frequency and Values properties defined here and creates the timeseries based
#' only on these parameters.The EndDate is not actually used internally other than for logging purposes. The true end
#' date is calculated based on the start date, frequency and the supplied list of values. Supply too few or too many values
#' and the mainframe will accept them and set the end date accordingly based on the given frequency for the item.
#'
#' @return DSTimeSeriesDataInput object
#' @export DSTimeSeriesDataInput

DSTimeSeriesDataInput = R6Class("DSTimeSeriesDataInput",
                                public = list( StartDate = NULL, EndDate = NULL, Frequency = NULL,
                                                Values = NULL,
#'
#' @param startDate A datetime value defining the start date for the timeseries.
#' @param endDate A datetime value defining the end date for the timeseries.
#' @param frequency The frequency of the timeseries.
#' @param values An array of float values
#' @return DSTimeSeriesDataInput object
#'
      initialize = function(startDate = NULL, endDate = NULL,
                            frequency = DSUserObjectFrequency$Daily, values = NULL)
      {
          self$StartDate = startDate
          self$EndDate = endDate
          self$Frequency = frequency
          self$Values = values
      }
))

#' @name DSTimeSeriesDateRange
#' @title DSTimeSeriesDateRange
#'
#' @description This class is a supporting attribute for the DateRange property of the DSTimeSeriesResponseObject.
#' It returns the raw data for the timeseries. The DateRange property of the DSTimeSeriesResponseObject always returns
#' the dates for a given frequency as the first date in each period. (e.g. 2022-01-01, 2020-04-01, etc. for quarterly
#' frequencies). You specify whether you want your users to receive either the first, mid or end dates in the given
#' period by setting the DateAlignment property (DSTimeSeriesDateAlignment) of the DSTimeSeriesRequestObject. \cr
#' \cr
#' When you retrieve a list of all your available timeseries using the GetAllItems method, since this list could contain
#' many thousand timeseries objects, the Dates and Values lists will always be NULL. Only the ValuesCount field will be
#' set to reflect the number of datapoints available for each item. You need to request an individual timeseries
#' (GetItem method) in order to receive a response containing actual data in the Dates and Values properties.
#'
#' @field Dates - A list of datetime values specifying the dates for each datapoint.
#' @field Values - A list of float values specifying the values for each datapoint.
#' @field ValuesCount - A count of the number of datapoints in the timeseries.
#'
#' @return DSTimeSeriesDateRange object
#' @export DSTimeSeriesDateRange
#'

DSTimeSeriesDateRange = R6Class("DSTimeSeriesDateRange",
                                public = list(Dates = NULL, Values = NULL, ValuesCount = numeric(10),
#'
#' @param jsonDict JSON dictionary (from JSON Response)
#' @param convertNullToNans FALSE by default, TRUE converts the NULLs in the NaNs (Not a Number)
#'
#' @return DSTimeSeriesDateRange object
#'
      initialize = function (jsonDict, convertNullToNans = FALSE)
      {
          self$Dates = NULL
          self$Values = NULL
          self$ValuesCount = 0

          if (!is.null(jsonDict))
          {
              self$ValuesCount = jsonDict$ValuesCount
              # GetAllItems queries return a list of timeseries objects but don't populate the Dates and Values properties for the items
              if (!is.null(jsonDict$Dates))
              {
                # convert the json Dates to datetime
                self$Dates = lapply(jsonDict$Dates, jsonDateToDatetime)
              }
              if (!is.null(jsonDict$Values)) # if user wants NaNs rather NULLs, then we need to add step to check and convert array
              {
                if (!convertNullToNans)
                  self$Values = jsonDict$Values
                else
                  self$Values = lapply(jsonDict$Values, FUN = function(x) { return (ifelse (is.null(x), NaN, x))})

              }
          }
      }
))

#' @name DSTimeSeriesDateInfo
#' @title DSTimeSeriesDateInfo
#'
#' @description This class is a supporting attribute for the DateInfo property of the DSTimeSeriesResponseObject.
#' It describes the basic range of data for the timeseries.
#'
#' The DateRange property (DSTimeSeriesDateRange described above) of the DSTimeSeriesResponseObject always returns the
#' dates for a given frequency as the first date in each period
#'       (e.g. 2022-01-01, 2020-04-01, etc. for quarterly frequencies). However, The StartDate and EndDate values returned
#'       in this class for the DSTimeSeriesResponseObject reflect the start and end dates of the range of dates
#'       that would be returned to users requesting the data via Datastream For Office, charting, etc.
#'       This depends on the DateAlignment property (DSTimeSeriesDateAlignment) of the timeseries.
#'       The start and end dates returned here will be either the start, mid or end dates for the set frequency
#'       based on the DateAlignment property (see DSTimeSeriesDateAlignment).
#'
#' @field StartDate A datetime value defining the start date of the timeseries data
#' @field EndDate A datetime value defining the end date for the timeseries.
#' @field Frequency The frequency of the timeseries. One of the DSUserObjectFrequency values
#' defined in DSUserDataObjectBase.R
#'
#' @return DSTimeSeriesDateInfo object
#'
#' @export DSTimeSeriesDateInfo
#'

DSTimeSeriesDateInfo = R6Class("DSTimeSeriesDateInfo",
                               public = list(StartDate = NULL, EndDate = NULL, Frequency = NULL,
#'
#' @param jsonDict JSON dictionary (from JSON Response)
#' @return DSTimeSeriesDateInfo object
#'

      initialize = function(jsonDict)
      {
          self$StartDate = NULL
          self$EndDate = NULL  # Defines the end date of the timeseries data
          self$Frequency = DSUserObjectFrequency$Daily # Defines the frequency of the timeseries data

          if (!is.null(jsonDict))
          {
              self$StartDate = jsonDateToDatetime(jsonDict$StartDate)
              self$EndDate = jsonDateToDatetime(jsonDict$EndDate)
              self$Frequency = jsonDict$Frequency
          }
      }
))

#' @name DSTimeSeriesUserObjectBase
#' @title DSTimeSeriesUserObjectBase
#'
#' @description This is the base object for creating or requesting timeseries data. It has two subclasses
#' DSTimeSeriesRequestObject and DSTimeSeriesResponseObject. It defines the basic attributes for a timeseries.
#' It subclasses DSUserObjectBase which defines the basic attributes common to all five user created item types
#' supported by the API.\cr
#' \cr
#' Specifics of some of the properties of the DSUserObjectBase superclass\cr
#' ----------------------------------------------------------------------\cr
#' ID \cr The ID property is defined in DSUserObjectBase but has a specific format for timeseries. Timeseries IDs must be
#' 8 alphanumeric characters long, start with TS followed by 6 uppercase
#' alphanumeric characters. For example: TSTEST01, TS123456, TSMYTEST, etc.\cr
#' Mnemonic \cr The Mnemonic property is defined in DSUserObjectBase but should always be left empty or set the same as the
#' ID property for timeseries requests. As a safety measure, this class always ensures it's the same as the ID.
#' In a response from the API server, the value will always be the same as the ID.\cr
#' (see DSUserObjectBase for a description of the other properties)\cr
#' \cr
#' DSTimeSeriesUserObjectBase specific properties
#' @field ManagementGroup This is an optional group name that allows you to organise timeseries into distinct 'folders'
#' displayed in the search category of Navigator. This can be up to 10 uppercase alphanumeric characters. Leave blank for the
#' item to be assigned under the 'GENERAL' group.
#' @field Units This is a optional qualifying unit for your data. For example: tons, U$ millions, index, etc.
#' Maximum 12 characters.
#' @field DecimalPlaces A numeric value between 0 and 8 decimal places specifying how many decimal places to use when storing data.
#' The maximum length including decimals for a value is 10 characters including the decimal point. Boundary case examples are 0.12345678,
#' 1234567890, 123456789.0, etc.
#' @field FrequencyConversion A DSTimeSeriesFrequencyConversion enum value specifying how to return values if a user requests data
#' at a lower frequency than the timeseries data is supplied. See DSTimeSeriesFrequencyConversion for details.
#' @field DateAlignment A DSTimeSeriesDateAlignment enum value specifying whether dates for certain frequencies should be returned
#' as the start, middle or end date of the period. See DSTimeSeriesDateAlignment for details.
#' @field CarryIndicator A DSTimeSeriesCarryIndicator enum value specifying how to treat 'Not A Number' values for non-trading days
#' and how to represent values if users request data after the end of the timeseries range. See DSTimeSeriesCarryIndicator for details.
#' @field PrimeCurrencyCode An optional 2 character currency code for your timeseries.
#' @field HasPadding This property has been replaced with the CarryIndicator property and will always be False
#' @field UnderCurrencyCode This property has been deprecated and will always return NULL
#' @field AsPercentage This This property has been deprecated and will always return False
#'
#' @return DSTimeSeriesUserObjectBase object
#' @export DSTimeSeriesUserObjectBase
#'

DSTimeSeriesUserObjectBase = R6Class("DSTimeSeriesUserObjectBase",
                                    inherit = DSUserObjectBase,
                                    public = list(ManagementGroup = character(30), Units = NULL, DecimalPlaces = numeric(2),
                                                  FrequencyConversion = numeric(1), DateAlignment = numeric(2),
                                                  CarryIndicator = numeric(1), PrimeCurrencyCode = NULL,
                                                  UnderCurrencyCode = NULL, HasPadding = logical(1), AsPercentage = logical(1),

#' @param jsonDict JSON dictionary (from JSON Response)
#' @return DSTimeSeriesUserObjectBase object

      initialize = function(jsonDict)
      {
          super$initialize(jsonDict)

          self$ManagementGroup = "GENERAL"
          self$Units = NULL
          self$DecimalPlaces = 0
          self$FrequencyConversion = DSTimeSeriesFrequencyConversion$EndValue
          self$DateAlignment = DSTimeSeriesDateAlignment$EndPeriod
          self$CarryIndicator = DSTimeSeriesCarryIndicator$Yes
          self$PrimeCurrencyCode = NULL
          self$UnderCurrencyCode = NULL
          self$HasPadding = FALSE
          self$AsPercentage = FALSE

          if (!is.null(jsonDict))
          {
              self$ManagementGroup = jsonDict$ManagementGroup
              self$Units = jsonDict$Units
              self$DecimalPlaces = jsonDict$DecimalPlaces
              self$AsPercentage = jsonDict$AsPercentage
              self$FrequencyConversion = jsonDict$FrequencyConversion
              self$DateAlignment = jsonDict$DateAlignment
              self$CarryIndicator = jsonDict$CarryIndicator
              self$PrimeCurrencyCode = jsonDict$PrimeCurrencyCode

              # Deprecated properties
              self$UnderCurrencyCode = NULL
              self$HasPadding = FALSE
              self$AsPercentage = FALSE
          }

      }
))

#' @name DSTimeSeriesRequestObject
#' @title DSTimeSeriesRequestObject
#'
#' @description This is a subclass of DSTimeSeriesUserObjectBase and is used to create or modify a timeseries.
#' (See DSTimeSeriesUserObjectBase for details of all the superclass properties.)
#'
#' @field Id A valid TimeSeries Id
#' @field DataInput A DSTimeSeriesDataInput object used to supply the start date, end date, frequency and
#' list of data values. (See DSTimeSeriesDataInput for details.)
#'
#' @return DSTimeSeriesRequestObject object
#' @export DSTimeSeriesRequestObject


DSTimeSeriesRequestObject = R6Class("DSTimeSeriesRequestObject",
                                    inherit = DSTimeSeriesUserObjectBase,
                                    public = list(Id = NULL, DataInput = NULL,
#'
#' @param id A valid TimeSeries Id
#' @param startDate A datetime value defining the start date for the timeseries
#' @param endDate A datetime value defining the end date for the timeseries
#' @param frequency The frequency of the timeseries. DSUserObjectFrequency is defined in DSUserDataObjectBase.R
#' @param values list of float values
#'
#' @return DSTimeSeriesRequestObject object


      initialize = function(id = "", startDate = NULL, endDate = NULL,
                            frequency = NULL, values = NULL)
      {
          super$initialize(NULL)
          self$Id = id
          self$DataInput = DSTimeSeriesDataInput$new(startDate, endDate, frequency, values)

      }
))

#' @name DSTimeSeriesResponseObject
#' @title DSTimeSeriesResponseObject
#' @description This is a subclass of DSTimeSeriesUserObjectBase and is used to return the details for a timeseries.
#' (See DSTimeSeriesUserObjectBase for details of all the superclass properties.)
#'
#' @field DateInfo A DSTimeSeriesDateInfo object defining the start date, end date and frequency
#' of the timeseries.
#' @field DateRange A DSTimeSeriesDateRange object used to return the dates and values stored in
#' the timeseries. See DSTimeSeriesDateRange for details.
#'
#' @return DSTimeSeriesResponseObject object
#' @export DSTimeSeriesResponseObject
#'

DSTimeSeriesResponseObject = R6Class("DSTimeSeriesResponseObject",
                                     inherit = DSTimeSeriesUserObjectBase,
                                     public = list(DateInfo = NULL, DateRange = NULL,
#'
#' @param jsonDict : JSON dictionary (from JSON Response)
#' @param convertNullToNans : FALSE by default, TRUE converts the NULLs in the NaNs (Not a Number)
#' @return DSTimeSeriesResponseObject object
#'
      initialize = function(jsonDict = NULL, convertNullToNans = FALSE)
      {
          super$initialize(jsonDict)
          self$DateInfo = NULL
          self$DateRange = NULL

          if (!is.null(jsonDict))
          {
              self$DateInfo = DSTimeSeriesDateInfo$new(jsonDict$DateInfo)
              self$DateRange = DSTimeSeriesDateRange$new(jsonDict$DateRange, convertNullToNans)
          }

      }
))

#' @name DSTimeSeriesDateRangeResponse
#' @title DSTimeSeriesDateRangeResponse
#'
#' @description DSTimeSeriesDateRangeResponse is the object returned from the timeseries
#' GetTimeseriesDateRange method. This method allows you to determine the supported dates between given start
#' and end dates at a specified frequency.
#'
#' @field Dates A list of datetime values representing the supported dates between requested start and end
#' dates at a specified frequency.
#' @field ResponseStatus This property will contain a DSUserObjectResponseStatus value. DSUserObjectResponseStatus$UserObjectSuccess
#' represents a successful response.
#' @field ErrorMessage If ResponseStatus is not DSUserObjectResponseStatus$UserObjectSuccess this status string will provide a
#' description of the error condition.
#' @field Properties Not currently used and will currently always return NULL.
#'
#' @return DSTimeSeriesDateRangeResponse object
#' @export DSTimeSeriesDateRangeResponse

DSTimeSeriesDateRangeResponse = R6Class("DSTimeSeriesDateRangeResponse",
                                        public = list(ResponseStatus = numeric(1), ErrorMessage = character(200),
                                                      Dates = NULL, Properties = NULL,

#' @param jsonDict : JSON dictionary (from JSON Response)
#' @return DSTimeSeriesDateRangeResponse object
#
      initialize = function(jsonDict = NULL)
      {
          self$ResponseStatus = DSUserObjectResponseStatus$UserObjectSuccess
          self$ErrorMessage = ''
          self$Dates = NULL
          self$Properties = NULL

          if (!is.null(jsonDict))
          {
              self$ResponseStatus = DSUserObjectResponseStatus[[jsonDict$ResponseStatus + 1]]
              self$ErrorMessage = jsonDict$ErrorMessage

            # GetTimeseriesDateRange queries return a list of supported dates that fall between the
            # specified start and end dates with the specified frequency

            if (!is.null(jsonDict$Dates))
            {
                # convert the json Dates to datetime
                self$Dates = lapply(jsonDict$Dates, jsonDateToDatetime)
            }
            self$Properties = jsonDict$Properties
          }
      }
))

#' @name TimeSeriesClient
#' @title TimeSeriesClient
#'
#' @description This is the client class that manages the connection to the API server on your behalf.
#' It allows you to query for all your timeseries and to create/modify new timeseries.
#'
#' @field useNaNforNotANumber If Enabled, NaN is appears in output response instead of NULL
#' @field TimeseriesResponseType Response type
#'
#' @details Methods Supported \cr
#'   GetAllItems : Allows you to query for all the current timeseries available for your use.\cr
#'   GetItem : Allows you to download the details of a specific timeseries item.\cr
#'   GetTimeseriesDateRange : Allows you to determine the supported timeseries dates between supplied start and
#'   end dates at a specified frequency.\cr
#'   CreateItem : Allows you to create a new timeseries item with up to 130 years of daily data.\cr
#'   UpdateItem : Allows you to update an existing timeseries.\cr
#'   DeleteItem : Allows you to delete an existing timeseries.\cr
#'
#' @note : You need a Datastream ID which is permissioned to access the Datastream APIs. In addition, this ID also needs
#' to be permissioned to access the custom user object service. Attempting to access this service without these permissions
#' will result in a permission denied error response.
#'
#' @examples
#' {
#'  # first logon with your credentials.
#'  # Creating a TimeSeriesClient instance with your credentials
#'  # automatically logs on for you.
#'
#'  timeseriesClient = TimeSeriesClient$new(NULL, 'YourID', 'YourPwd')
#'
#'  # query for all your current timeseries items
#'
#'  itemsResp = timeseriesClient$GetAllItems()
#'  if (!is.null(itemsResp))
#'  {
#'    if (itemsResp$ResponseStatus != DSUserObjectResponseStatus$UserObjectSuccess)
#'    {
#'      # Your Datastream Id might not be permissioned for managing
#'      # user created items on this API
#'
#'      print(paste('GetAllItems failed with error ',
#'      names(DSUserObjectResponseStatus)[[itemsResp$ResponseStatus + 1]],
#'      ': ', itemsResp$ErrorMessage))
#'
#'    }
#'    else if (!is.null(itemsResp$UserObjects) & itemsResp$UserObjectsCount > 0)
#'    {
#'      # You do have access to some timeseries
#'      # Here we just put the timeseries details into a dataframe and list them
#'      print(paste('GetAllItems returned', itemsResp$UserObjectsCount, 'timeseries items.'))
#'      df = data.frame()
#'
#'      for (tsItem in itemsResp$UserObjects)
#'      {
#'       if (!is.null(tsItem))
#'       {
#'        rowdata = list(Id = tsItem$Id,
#'        LastModified = tsItem$LastModified,
#'        StartDate = ifelse(!is.null(tsItem$DateInfo),as.character(tsItem$DateInfo$StartDate),""),
#'        EndDate =ifelse(!is.null(tsItem$DateInfo),as.character(tsItem$DateInfo$EndDate), ""),
#'        Frequency = ifelse(!is.null(tsItem$DateInfo), tsItem$DateInfo$Frequency, 0),
#'        NoOfValues = ifelse(!is.null(tsItem$DateRange), tsItem$DateRange$ValuesCount , 0),
#'        Desc = tsItem$Description)
#'        df = rbind(df, rowdata)
#'       }
#'      }
#'      print(df)
#'   }
#'  }
#'  #Example to show how to GetItem
#'  # query for a specific timeseries
#'
#'  tsName = 'TSZZZ001'
#'  tsResponse = timeseriesClient$GetItem(tsName)
#'
#'  # You may want to put the timeseries request response handling into a common function.
#'  if (!is.null(tsResponse))
#'  {
#'    # Any request dealing with a single user created item returns a DSUserObjectResponse.
#'    # This has ResponseStatus property that indicates success or failure
#'
#'    if (tsResponse$ResponseStatus != DSUserObjectResponseStatus$UserObjectSuccess)
#'    {
#'      print(paste('Request failed for timeseries', tsName, 'with error',
#'                    names(DSUserObjectResponseStatus)[[tsResponse$ResponseStatus+1]],
#'                    ':', tsResponse$ErrorMessage))
#'    }
#'    else if (!is.null(tsResponse$UserObject))
#'    {
#'      # The timeseries item won't be returned if you set SkipItem true
#'      # in CreateItem or UpdateItem
#'
#'      # Here we simply display the timeseries data using a dataframe.
#'
#'      tsItem = tsResponse$UserObject
#'      metadata = c (Id = tsItem$Id,
#'      Desc = tsItem$Description,
#'      LastModified = as.character(tsItem$LastModified),
#'      StartDate = ifelse (!is.null(tsItem$DateInfo), as.character(tsItem$DateInfo$StartDate), NULL),
#'      EndDate = ifelse(!is.null(tsItem$DateInfo),as.character(tsItem$DateInfo$EndDate), NULL),
#'      Frequency = ifelse(!is.null(tsItem$DateInfo),
#'      names(DSUserObjectFrequency)[[tsItem$DateInfo$Frequency + 1]], NULL),
#'      NoOfValues = ifelse(!is.null(tsItem$DateRange), tsItem$DateRange$ValuesCount , 0))
#'
#'      df = data.frame(metadata)
#'      print(df)
#'      if (!is.null(tsItem$DateRange))
#'      {
#'        df = data.frame(Dates = sapply(tsItem$DateRange$Dates,
#'        FUN = function(x){ return (as.character(x)) }),
#'                          Values = sapply(tsItem$DateRange$Values,
#'                          FUN = function(x){ ifelse (is.null(x),
#'                          return (NA_character_ ), return (x) )} ))
#'
#'        # Values if NULL, is printed as <NA> because, while
#'        # convertind list to vector either by using as.vector or sapply,
#'        # the NULL values in the list are deleted. and thus there will
#'        # be mismatch in no of rows and cannot be put in a dataframe
#'
#'        print(df)
#'
#'      }
#'    }
#'  }
#'}
#'
#' @return TimeSeriesClient object
#' @export TimeSeriesClient

TimeSeriesClient = R6Class("TimeSeriesClient",
                           inherit = DSConnect,
                           public = list(useNaNforNotANumber = logical(1),
                                         TimeseriesResponseType = NA,

#'
#' @description User details can be supplied from a config file or passed directly as parameters
#'              in the constructor of the derived user object type class.
#'              (See the DSConnect superclass for a description of the connection parameters required)
#'
#' @param config Configuration File path
#' @param username Your Datastream Id
#' @param password Your Password
#' @param proxies Proxies if any
#' @param sslCer Path to CA bundle certificates file
#' @return TimeSeriesClient object
#'
#' @details Timeseries Properties: \cr
#'  useNaNforNotANumber : Non-trading days are stored as double NaNs on Datastream, JSON protocol permits NaNs as valid numbers.
#'  Thus, all the NULLs in the converted to NaNs in the JSON requests. Responses contain the NULLs, But this should be converted
#'  to Nans for Plotting purposes. If you want to receive NaN float values, set useNaNforNotANumber to TRUE, any NULLs in the returned
#'  array of float values will be converted to NaNs.

      initialize = function(config = NULL, username = NULL, password = NULL,
                            proxies = NULL, sslCer = NULL)
      {
          super$initialize(config, username, password, proxies, sslCer, service = DataService$UCS)

          self$useNaNforNotANumber = FALSE

          # this private flag is used to indicate how the json response should be decoded into a response object.
          self$TimeseriesResponseType = list (GetItemResponse = 0,
                                              GetAllResponse = 1,
                                              GetDateRangeResponse = 2)
      },

# ---------------------------- .checkValidTimeseriesId ----------------------------------------------

#' @description A helper method to check the timeseries Id
#' @param inputId : Timeseries Id
#' @return NULL if Timeseries id is valid else error string
#'
      .checkValidTimeseriesId = function(inputId)
      {
          # The requested timeseries ID must match the format TS followed by 6 alphanumeric characters.
          if  (is.character(inputId))
          {
            tsval = grep(pattern = "^TS[0-9A-Z]{6,}$", inputId, ignore.case = TRUE, value = TRUE)
            if (identical(tsval, character(0)))
                return ('Timeseries IDs must be 8 uppercase alphanumeric characters in length and start with TS. e.g. TSABC001.')
            return (NULL) #valid
          }
      },

# ---------------------------- .checkTimeSeriesReqValidity ----------------------------------------------

#' @description A helper method to check some of the mandatory fields of timeseries for its validity
#' @param tsItem Timeseries Item
#' @return  NULL if Timeseries id is valid else error string
#'
      .checkTimeSeriesReqValidity = function(tsItem)
      {
          if(is.null(tsItem))
              return ('TimeSeriesClient CreateItem or ModifyItem methods
                      require a valid DSTimeSeriesRequestObject instance.')

          # check for valid ID
          idCheck = self$.checkValidTimeseriesId(tsItem$Id)
          if (!is.null(idCheck))
              return (idCheck)

          # check for DSTimeSeriesDataInput properties
          if (!is.null(tsItem$DataInput))
          {
              if (!inherits(tsItem$DataInput$StartDate, "Date") |
                  !inherits(tsItem$DataInput$EndDate, "Date") |
                  !(difftime(tsItem$DataInput$EndDate, tsItem$DataInput$StartDate, units = "days", tz = "UTC") > 0))
              {
                  return ('Supplied DSTimeSeriesDataInput StartDate
                           and EndDate values must be date or datetime
                           objects and StartDate cannot be set later
                           then the EndDate.')
              }

              # and a valid frequency
              if (!is.numeric(tsItem$DataInput$Frequency) | !tsItem$DataInput$Frequency %in% DSUserObjectFrequency)
                  return ('Supplied DSTimeSeriesDataInput Frequency field
                           must be a DSUserObjectFrequency value.')

             # we must also have some values
             if (is.null(tsItem$DataInput$Values) | length(tsItem$DataInput$Values) == 0)
                  return ('Supplied DSTimeSeriesDataInput Values field
                            must contain an array of values.')
          }
          else
            return ('The supplied DSTimeSeriesRequestObject must supply
                      a valid DSTimeSeriesDataInput instance.')


          # some safety checks
          # Mnemonic isn't used in timeseries; should be the same as ID
          tsItem$Mnemonic = tsItem$Id
          tsItemManagementGroup = ifelse (is.character(tsItem$ManagementGroup), tsItem$ManagementGroup, "GENERAL")

          tsItem$DecimalPlaces = ifelse ((is.numeric(tsItem$DecimalPlaces) &
                                          tsItem$DecimalPlaces >= 0 &
                                          tsItem$DecimalPlaces <= 8),
                                          tsItem$DecimalPlaces, 0)

          tsItem$AsPercentage = ifelse (is.logical(tsItem$AsPercentage), tsItem$AsPercentage, FALSE)

          tsItem$FrequencyConversion = ifelse ((is.numeric(tsItem$FrequencyConversion) & tsItem$FrequencyConversion %in% DSTimeSeriesFrequencyConversion),
                                                tsItem$FrequencyConversion,
                                                DSTimeSeriesFrequencyConversion$EndValue)

          tsItem$DateAlignment = ifelse ((is.numeric(tsItem$DateAlignment) & tsItem$DateAlignment %in% DSTimeSeriesDateAlignment),
                                 tsItem$DateAlignment, DSTimeSeriesDateAlignment$EndPeriod)

          tsItem$CarryIndicator = ifelse ((is.numeric(tsItem$CarryIndicator) & tsItem$CarryIndicator %in% DSTimeSeriesCarryIndicator),
                                  tsItem$CarryIndicator, DSTimeSeriesCarryIndicator$Yes)

          # Redundant properties
          tsItem$UnderCurrencyCode = NULL # Deprecated.
          tsItem$HasPadding = FALSE # Deprecated and replaced with CarryIndicator.
          tsItem$AsPercentage = FALSE # Deprecated.
          # We should ensure some safety values for base class for JSON encoding purposes in case default values overwritten with incorrect types
          tsItem$SetSafeUpdateParams()

          return (TRUE)  # valid request
      },

# ---------------------------- .checkKeyTimeseriesProperties ----------------------------------------------
#' @description A helper method to check the Timeseries properties
#' @param tsItem Timeseries Item
#' @return  NULL if Timeseries id is valid else error string
#'
      .checkKeyTimeseriesProperties = function( tsItem)
      {
          checkTS = self$.checkTimeSeriesReqValidity(tsItem)
          if (checkTS != TRUE)
            return (checkTS)
          else
            return (NULL)
      },

# ---------------------------- .asGetAllResponse ----------------------------------------------
#' @description A helper method which converts the JSON response to GetAllResponse Object
#' @param jsonDict JSON Response
#' @return DSUserObjectGetAllResponse object
#'
      .asGetAllResponse = function(jsonDict)
      {
          # An internal method to convert a JSON response from a GetAllItems query into a DSUserObjectGetAllResponse object.
          getAllResponse = DSUserObjectGetAllResponse$new(jsonDict)

          if ( (!is.null(jsonDict)) & (!is.null(jsonDict$UserObjects)) &
                (jsonDict$ResponseStatus == DSUserObjectResponseStatus$UserObjectSuccess) &
                (jsonDict$UserObjectType == DSUserObjectTypes$TimeSeries))
              # convert all userobjects to DSTimeSeriesResponseObject
                  getAllResponse$UserObjects = lapply(jsonDict$UserObjects, FUN = function(x) {
                        return (DSTimeSeriesResponseObject$new(x, self$useNaNforNotANumber)) })

        return (getAllResponse)
      },

# ---------------------------- .asGetResponse ----------------------------------------------
#' @description A helper method which converts the JSON response to GetResponse Object
#' @param jsonDict JSON Response
#' @return DSUserObjectResponse object
#'
      .asGetResponse = function( jsonDict)
      {
          # An internal method to convert a JSON response from GetItem, CreateItem or UpdateItem queries into a DSUserObjectResponse object.
          responseObject = DSUserObjectResponse$new(jsonDict)

          if (!is.null(jsonDict) & !is.null(responseObject$UserObject) &
             (responseObject$ResponseStatus == DSUserObjectResponseStatus$UserObjectSuccess) &
             (responseObject$UserObjectType == DSUserObjectTypes$TimeSeries))
                responseObject$UserObject = DSTimeSeriesResponseObject$new(responseObject$UserObject, self$useNaNforNotANumber)

          return (responseObject)
      },

# ---------------------------- .jsonRequestEncoder ----------------------------------------------
#' @description A helper method that reformats the raw request to JSON format
#' @param request Raw request
#' @return return JSON formatted list
#'
      .jsonRequestEncoder = function(request)
      {
          # we have to encode the timeseries request item with a type identifier to distinguish it properly as a timeseries request object for the api server.
          # this method also converts the datetimes and values representing nans to a format acceptable in json queries.
          jsonlist = list ("__type" = paste("DSTimeSeriesRequestObject", apiSchemaNamespace, sep = ""),
                      Id = request$Id,
                      Mnemonic = ifelse (is.null(request$Mnemonic), "", request$Mnemonic),
                      DisplayName = ifelse (is.null(request$DisplayName), "", request$DisplayName),
                      Description = ifelse (is.null(request$Description), "", request$Description),
                      Created = toJSONdate(request$Created),
                      LastModified = toJSONdate(request$LastModified),
                      Owner = request$Owner,
                      ShareType = request$ShareType,
                      AccessRight = request$AccessRight,
                      ManagementGroup = ifelse (is.null(request$ManagementGroup), "GENERAL", request$ManagementGroup),
                      Units = request$Units,
                      DecimalPlaces = ifelse (is.null(request$DecimalPlaces), 0, request$DecimalPlaces),
                      FrequencyConversion = ifelse (is.null(request$FrequencyConversion), DSTimeSeriesFrequencyConversion$EndValue, request$FrequencyConversion),
                      DateAlignment = ifelse (is.null(request$DateAlignment), DSTimeSeriesDateAlignment$EndPeriod, request$DateAlignment),
                      CarryIndicator = ifelse (is.null(request$CarryIndicator), DSTimeSeriesCarryIndicator$Yes, request$CarryIndicator),
                      PrimeCurrencyCode = request$PrimeCurrencyCode )

         datainput = list(StartDate = toJSONdate(request$DataInput$StartDate),
                          EndDate = toJSONdate(request$DataInput$EndDate),
                          Frequency = request$DataInput$Frequency,
                          Values = request$DataInput$Values)

        # If there is null in values, make it NaN, as POST JSON causes 400 error.
        datainput$Values = lapply(datainput$Values, FUN = function(x) {return (ifelse(is.null(x), NaN, x))})

        jsonlist = append(jsonlist, list(DataInput = datainput))

        return (jsonlist)
      },

# ---------------------------- .jsonResponseDecoder ----------------------------------------------
#' @description A helper method that converts JSON Response to a given class response type
#' @param jsonResp JSON Response
#' @param responseType GetResponse or GetAllResponse type
#' @return return DSUserObjectGetAllResponse or DSUserObjectGetResponse object

      .jsonResponseDecoder = function(jsonResp, responseType)
      {
          # An internal method to convert a JSON response into the relevant DSUserObjectGetAllResponse, DSUserObjectResponse or DSTimeSeriesDateRangeResponse object.
          if (responseType == self$TimeseriesResponseType$GetAllResponse)
          {
            return (self$.asGetAllResponse(jsonResp))
          }
          else if (responseType == self$TimeseriesResponseType$GetDateRangeResponse)
            return (DSTimeSeriesDateRangeResponse$new(jsonResp))
          else # GetItemResponse
            return (self$.asGetResponse(jsonResp))
      },

# ---------------------------- GetAllItems ----------------------------------------------

#' @description This method returns all the current timeseries you can use in Datastream queries.
#' @return DSUserObjectGetAllResponse object
#'
      GetAllItems = function()
      {
          tryCatch(
          {
              logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeseriesClient$GetAllItems', 'GetAllItems requested'))
              super$CheckToken() # check and renew token if within 15 minutes of expiry

              # construct the request
              requestURL = paste(self$RequestUrl, "GetAllItems", sep = "")
              rawRequest = list (Filters = NULL,
                                 Properties = NULL,
                                 TokenValue = self$Token,
                                 UserObjectType = DSUserObjectTypes$TimeSeries)

              # make the request and process the response
              jsonResp = self$getJsonResponse(requestURL, rawRequest)

              # Converting JSON Response to DSUserObjectGetAllResponse class object
              decoded = self$.jsonResponseDecoder(jsonResp, self$TimeseriesResponseType$GetAllResponse)
              logger::log_info (paste('DatastreamUserCreated_TimeSeries.R ', 'TimeSeriesClient$GetAllItems', 'GetAllItems returned'))
              return (decoded)
          },
          error = function(e)
          {
              logger::log_error( paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetAllItems', 'Exception occured.', message(e)))
              return (message(e))
          }
        )
      },

# ---------------------------- GetItem ----------------------------------------------

#' @description GetItem returns the details for an individual timeseries.
#'
#' @param itemId : a valid timeseries Id.
#'
#' @return DSUserObjectResponse object
#'


        GetItem = function(itemId)
        {
            tryCatch(
            {
                # Some prechecks
                reqCheck = self$.checkValidTimeseriesId(itemId)
                if (!is.null(reqCheck))
                {
                    resp = DSUserObjectResponse$new()
                    resp$ResponseStatus = DSUserObjectResponseStatus$UserObjectFormatError
                    resp$ErrorMessage = reqCheck
                    logger::log_error (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetItem', 'Error:', reqCheck))
                    return (resp)
                }

                logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetItem', 'Requesting', itemId))
                super$CheckToken() # check and renew token if within 15 minutes of expiry

                # construct the request
                requestURL = paste(self$RequestUrl, "GetItem", sep = "")
                rawRequest = list( Filters = NULL,
                                   Properties = NULL,
                                   TokenValue = self$Token,
                                   UserObjectId = itemId,
                                   UserObjectType = DSUserObjectTypes$TimeSeries )

                # make the request and process the response
                jsonResp = self$getJsonResponse(requestURL, rawRequest)

                # Converting JSON Response to DSUserObjectResponse class object
                decoded = self$.jsonResponseDecoder(jsonResp, self$TimeseriesResponseType$GetItemResponse)
                logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetItem', itemId, 'returned a response'))
                return (decoded)
            },
            error = function(e)
            {
                logger::log_error( paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetItem', 'Exception occured.', message(e)))
                return (message(e))
            })
      },

#' @description This method attempts to create the given DSTimeSeriesRequestObject via the API service
#'
#' @param newItem A DSTimeSeriesRequestObject containing the data used for creating the Timeseries.
#' @param overWrite If the given Timeseries Id already exists on the system, the create call will be rejected.
#' Set overWrite = True to overwrite the existing item with new Timeseries.
#' @param skipItemReturn : Upon successful creation of an item, the server requests the new item from the mainframe
#' and returns it in the response object. For faster processing, set skipItemReturn = True to skip returning the
#' object in the response
#'
#' @return DSUserObjectResponse object
#'
#' @note : For Daily and Weekly frequencies, if the supplied startDate falls on a weekend or a trading holiday, the returned
#' starting date will be the first trading day before the given start date. If the supplied endDate falls on a weekend or a
#' trading holiday, the returned final date will be the last trading day before the given end date. For Weekly frequencies,
#' this will be the last date which matches the day of the week for the first returned start date.\cr
#' \cr
#' For Monthly, Quarterly and Yearly frequencies, the returned dates are always the 1st day of each month, quarter or year.
#' The returned start and end dates are always the 1st days of the requested  month, quarter or year that the given start
#' and end dates fall within.
#'
      CreateItem = function(newItem, overWrite = FALSE, skipItemReturn = FALSE)
      {
          tryCatch(
          {
              # Some prechecks
              reqCheck = self$.checkKeyTimeseriesProperties(newItem)
              if (!is.null(reqCheck))
              {
                  resp = DSUserObjectResponse$new()
                  resp$ResponseStatus = DSUserObjectResponseStatus$UserObjectFormatError
                  resp$ErrorMessage = reqCheck
                  logger::log_error (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$CreateItem', 'Error: ' , reqCheck))
                  return (resp)
              }
              logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$CreateItem', 'Creating ', newItem$Id))
              super$CheckToken() # check and renew token if within 15 minutes of expiry

              # encode the DSTimeSeriesRequestObject into JSON
              requestURL = paste(self$RequestUrl, "CreateItem", sep = "")
              userObj = self$.jsonRequestEncoder(newItem)

              # we may need to encode Filters properties with flags to overwrite item if it already exists, plus option not to return the timeseries in the response
              filters = NULL
              if (overWrite)
                filters = list(list(Key = "ForceUpdate", Value = TRUE))
              if (!is.null(filters))
              {
                  if (skipItemReturn)
                    filters = append(filters, list(Key = "SkipRetrieval", Value = TRUE))
              }
              else if (skipItemReturn)
                  filters = list(list(Key = "SkipRetrieval", Value = TRUE))

              # construct the raw request and make the Rest/JSON query
              rawRequest = list(Filters = filters,
                                Properties = NULL,
                                TokenValue = self$Token,
                                UserObject = userObj,
                                UserObjectType = DSUserObjectTypes$TimeSeries)

              # make the request and process the response
              jsonResp = self$getJsonResponse(requestURL, rawRequest)

              # Converting JSON Response to DSUserObjectResponse class object
              decoded = self$.jsonResponseDecoder(jsonResp, self$TimeseriesResponseType$GetItemResponse)
              logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$CreateItem', newItem$Id, ' returned a response'))
              return (decoded)
          },
          error = function(e)
          {
              logger::log_error( paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$CreateItem', 'Exception occured.', message(e)))
              return (message(e))
          })
      },

#' @description This method attempts to modify a timeseries item using the given DSTimeSeriesRequestObject via
#'              the API service
#'
#' @param item A DSTimeSeriesRequestObject containing the data used for creating the Timeseries.
#' @param skipItemReturn Upon successful creation of an item, the server requests the new item from the
#' mainframe and returns it in the response object. For faster processing, set skipItemReturn = True to
#' skip returning the object in the response.
#'
#' @return DSUserObjectResponse object

      UpdateItem = function(item, skipItemReturn = FALSE)
      {
          tryCatch(
          {
            # Some prechecks
            reqCheck = self$.checkKeyTimeseriesProperties(item)

            if (!is.null(reqCheck))
            {
                resp = DSUserObjectResponse$new()
                resp$ResponseStatus = DSUserObjectResponseStatus$UserObjectFormatError
                resp$ErrorMessage = reqCheck
                logger::log_error (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$UpdateItem', 'Error: ',  reqCheck))
                return (resp)
            }

            logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$UpdateItem', 'Updating ', item$Id))
            super$CheckToken() # check and renew token if within 15 minutes of expiry

            # encode the DSTimeSeriesRequestObject into JSON
            requestURL = paste(self$RequestUrl, "UpdateItem", sep = "")
            userObj = self$.jsonRequestEncoder(item)

            # construct the raw request and make the Rest/JSON query
            # we may need to encode Filters properties with option not to return the timeseries in the response
            filters = NULL
            if (skipItemReturn == TRUE)
               filters = list(list(Key = "SkipRetrieval", Value = TRUE))

            rawRequest = list ( Filters = filters,
                                Properties = NULL,
                                TokenValue = self$Token,
                                UserObject = userObj,
                                UserObjectType = DSUserObjectTypes$TimeSeries )

            # make the request and process the response
            jsonResp = self$getJsonResponse(requestURL, rawRequest)

            # Converting JSON Response to DSUserObjectResponse class object
            decoded = self$.jsonResponseDecoder(jsonResp, self$TimeseriesResponseType$GetItemResponse)
            logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$UpdateItem', item$Id, ' returned a response'))
            return (decoded)
        },
        error = function(e)
        {
            logger::log_error( paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$UpdateItem', 'Exception occured.', message(e)))
            return (message(e))
        })
      },


#' @description DeleteItem allows you to delete an existing timeseries
#'
#' @param itemId a valid timeseries Id.
#' @return No return value

      DeleteItem = function(itemId)
      {
          tryCatch(
          {
              # Some prechecks
              reqCheck = self$.checkValidTimeseriesId(itemId)

              if (!is.null(reqCheck))
              {
                  resp = DSUserObjectResponse$new()
                  resp$ResponseStatus = DSUserObjectResponseStatus$UserObjectFormatError
                  resp$ErrorMessage = reqCheck
                  logger::log_error (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$DeleteItem', 'Error: ',  reqCheck))
                  return (resp)
              }

              logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$DeleteItem', 'Deleting' , itemId))
              super$CheckToken() # check and renew token if within 15 minutes of expiry

              # construct the request
              requestURL = paste(self$RequestUrl, "DeleteItem", sep = "")
              rawRequest = list ( Filters = NULL,
                                  Properties = NULL,
                                  TokenValue = self$Token,
                                  UserObjectId = itemId,
                                  UserObjectType = DSUserObjectTypes$TimeSeries )

              # make the request and process the response
              jsonResp = self$getJsonResponse(requestURL, rawRequest)

              # Converting JSON Response to DSUserObjectResponse class object
              decoded = self$.jsonResponseDecoder(jsonResp, self$TimeseriesResponseType$GetItemResponse)
              logger::log_info (paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$DeleteItem', itemId, ' returned a response'))
              return (decoded)
        },
        error = function(e)
        {
            logger::log_error( paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$DeleteItem', 'Exception occured.', message(e)))
            stop (message(e))
        })
    },

#' @description This method allows you to determine the supported dates between supplied start and end dates
#'              at a specified frequency.
#'
#' @param startDate A date specifying the beginning of the date range
#' @param endDate A date specifying the end of the date range
#' @param frequency A DSUserObjectFrequency enumeration defining if the frequency should be daily,
#' weekly, monthly, quarterly or yearly.
#'
#' @return DSTimeSeriesDateRangeResponse object
#'
#' @note : For Daily and Weekly frequencies, if the supplied startDate falls on a weekend or a trading holiday, the returned
#' starting date will be the first trading day before the given start date. If the supplied endDate falls on a weekend or a
#' trading holiday, the returned final date will be the last trading day before the given end date. For Weekly frequencies,
#' this will be the last date which matches the day of the week for the first returned start date.\cr
#' \cr
#' For Monthly, Quarterly and Yearly frequencies, the returned dates are always the 1st day of each month, quarter or year.
#' The returned start and end dates are always the 1st days of the requested  month, quarter or year that the given start
#' and end dates fall within.
#'

      GetTimeseriesDateRange = function(startDate, endDate, frequency = DSUserObjectFrequency$Daily)
      {
          tryCatch(
            {
                #Check startDate is before endDate
                reqCheck = NULL
                if (!(inherits(startDate,"Date")) | !(inherits(endDate, "Date")) | difftime(endDate, startDate, units = "days") < 0)
                    reqCheck = 'Supplied StartDate and EndDate parameters must be date objects and StartDate cannot be set later then the EndDate.'
                else if (length(frequency %in% DSUserObjectFrequency) == 0)
                    reqCheck = 'Supplied frequency parameter must be a DSUserObjectFrequency value.'


                if (!is.null(reqCheck))
                {
                    resp = DSTimeSeriesDateRangeResponse$new()
                    resp$ResponseStatus = DSUserObjectResponseStatus$UserObjectFormatError
                    resp$ErrorMessage = reqCheck
                    logger::log_error(paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetTimeseriesDateRange', 'Error: ', resp$ErrorMessage))
                    return (resp)
                }

                logger::log_info('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetTimeseriesDateRange', 'Requesting date range')
                super$CheckToken() # check and renew token if within 15 minutes of expiry

                # construct our DSTimeSeriesDateInfo object
                dateInfo = list ( StartDate = toJSONdate(startDate),
                                  EndDate = toJSONdate(endDate),
                                  Frequency = frequency)

                # construct the request
                requestURL = paste(self$RequestUrl, "TimeSeriesGetDateRange", sep = "")
                rawRequest = list( DateInfo = dateInfo,
                                  Properties = NULL,
                       TokenValue = self$Token)

                # make the request and process the response
                jsonResp = self$getJsonResponse(requestURL, rawRequest)
                decoded = self$.jsonResponseDecoder(jsonResp, self$TimeseriesResponseType$GetDateRangeResponse)
                logger::log_info(paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetTimeseriesDateRange', 'GetTimeseriesDateRange returned a response'))
                return (decoded)
        },
        error = function(e)
        {
            logger::log_error( paste('DatastreamUserCreated_TimeSeries.R', 'TimeSeriesClient$GetTimeseriesDateRange', 'Exception occured.', message(e)))
            stop (message(e))
        })
    }
))









































