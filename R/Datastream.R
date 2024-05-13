#' @include DSRequests.R
#' @include DSConnect.R
#' @include DSResponse.R
#
#
# DataClient is a RC class which creates a object, used to access the Datastream
# content through the Datastream Web Services API.
#
# The Data requests can be done using 3 functions:
#    getData
#    getDataframe
#    getDataBundle

#########################################################################

#' @name DataClient
#' @title DataClient
#'
#' @description An RC class for accessing Datastream content through Datastream Web services API
#'
#' @field useNaNforNotANumber : TRUE by default. NULLs are replaced to NaN (Not a Number)
#' @return DataClient object
#' @export DataClient
#'
DataClient<- R6::R6Class("DataClient",
                         inherit = DSConnect,
                          public = list(useNaNforNotANumber = logical(1),




#--------------------- Initialization ----------------------------------------------

#' @description Initialize method to create Datastream Object
#'
#' @details Creates Datastream RC object for accessing Datastream content
#'    through Datastream Web Services API
#'
#' @param config : Config path
#' @param username : Your Id
#' @param password : Your Password
#' @param proxys : Proxy details (if any)
#' @param sslCer : ssl Certificates path
#' @return DataClient object
#'
#' @examples
#'\dontrun{
#'  # ds = DataClient$new(NULL, "YourID", "YourPswd")
#'
#'  #  OR
#'
#'  # Login using config file
#'  # Config file details provided in DSConnect.R
#'  ds = DataClient("Config.ini")
#'
#'  # Timeseries requests
#'
#'  df = tryCatch (
#'    {ds$getData(tickers="VOD", fields=list("PH","PL"),
#'                start='2022-01-12', end='2022-07-13', kind=1)},
#'    error = function(e) { stop (message(e))})
#'    print(df)
#'
#'  # Snapshot requests
#'
#'  df = tryCatch (
#'  { ds$getData(tickers="VOD", fields=list("PH", "PL"),
#'               start='2022-01-12', kind=0)},
#'  error = function(e) { stop (message(e))})
#'  print(df)
#'}
#'


      initialize = function(config = NULL, username = "", password = "", proxys = NULL,
                                         sslCer = NULL)
      {
          "This initializes the DataClient object, with 2 steps:
          1. Authenticates the user and
          2. connects the valid user with Datastream Web Services API"
          super$initialize(config = config, username = username, password = password, proxies = proxys, sslCert = sslCer,
                        service = DataService$DATA)
          self$useNaNforNotANumber = TRUE
      },


# ---------------------- getData ----------------------------------------------------
#'
#' @description getData posts the JSON formatted request and the JSON response is
#'              then converted to Dataframe, if dataToDF is TRUE.
#'
#' @param tickers : Intruments Eg: "VOD,BARC"
#' @param fields : Datatypes   Eg: list('PH, 'PL', 'PI')
#' @param start : start date in "YYYY-mm-dd" format Eg: "2019-01-20"
#' @param end : end date  in
#' @param freq : Refer DSDateFrequencyNames in DSRequests.R
#' @param kind : 1 - TimeSeries Request (default), 0 - Snapshot Request
#' @param properties : properties
#'
#' @return Response Class
#'


      getData = function(tickers, fields=NULL, start = "", end = "", freq = "D",
                         kind = 1, properties = NULL)
      {
          logger::log_info('Datastream.R ', 'DataClient$getData ', 'Getting Data ' )

          super$CheckToken()
          getDataURL = paste(self$RequestUrl, "getData", sep = "")

          # Data Requests formatting
          requests = DSRequests$new()
          getDataRequest = requests$getGetDataRequest(tickers, fields, start, end, freq, kind, self$Token, properties)

          # Post data to get the JSON Response
          jsonResp = tryCatch (
          {
              self$getJsonResponse(getDataURL, getDataRequest)
          },
          error = function(e)
          {
              logger::log_error(paste('Datastream.R', 'DataClient$getData',
                              'Exception :', message(e) ))
              return (message(e))
          }
      )

      if (is.list(jsonResp) & ("DataResponse" %in% names(jsonResp)))
      {
          # Converting JSON Response to DSGetDataResponse class object
          getDataResponse = DSGetDataResponse$new(jsonResp$DataResponse, jsonResp$Properties, self$useNaNforNotANumber)
          return (getDataResponse)
      }
      else
          return (jsonResp)

    },

# ---------------------- getDataframe ----------------------------------------------------
#' @description getDataframe posts the JSON formatted request and the JSON response is
#'              then converted to Dataframe
#'
#' @param tickers : Intruments Eg: "VOD,BARC"
#' @param fields : Datatypes   Eg: list('PH, 'PL', 'PI')
#' @param start : start date in "YYYY-mm-dd" format Eg: "2019-01-20"
#' @param end : end date  in
#' @param freq : Refer DSDateFrequencyNames in DSRequests.R
#' @param kind : 1 - TimeSeries Request (default), 0 - Snapshot Request
#' @param properties : properties
#'
#'
#' @return Dataframe
#'


      getDataframe = function(tickers, fields=NULL, start = "", end = "", freq = "D", kind = 1,
                              properties = NULL)
      {
          logger::log_info('Datastream.R ', 'DataClient$getDataframe ', 'Getting Data ' )

          getDataResponse = tryCatch (
          {
              self$getData(tickers, fields, start, end, freq,
                    kind, properties)
          },
          error = function(e)
          {
              logger::log_info('Datastream.R ', 'DataClient$getDataframe ',
                       'Exception :', message(e) )
              return (message(e))
          })

          # Check for the data response received.
          dataResp = tryCatch(
          { getDataResponse$DataResponse },
          error = function(e) { return  (NULL)})

          if (!is.null(dataResp))
          {
              df = self$toDataframe(getDataResponse$DataResponse)
              return (df)
          }
          else
              return (getDataResponse)

    },


# -------------------------------- formatDataRequest --------------------------------
#' @description This method formats the request provided by client (in form of tickers and fields)
#'              to the JSON formatted request.
#'
#' @param tickers : Instruments
#' @param fields : Datatypes
#' @param start : start date
#' @param end : end date
#' @param freq : frequency
#' @param kind : kind = 0 for Snapshot request and Kind = 1 for Timeseries request
#'
#' @return JSON formatted data request
#'


      formatDataRequest = function(tickers, fields=NULL, start = "", end = "", freq = "D",
                                    kind = 1)
      {
          "Used to format the data request in the JSON format"

          requests = DSRequests$new()
          dataRequest = requests$setDataRequest(tickers, fields, start, end, freq, kind)

          return (dataRequest)
     },

# ---------------------- getDataBundle ----------------------------------------------------
#' @description getData posts the JSON formatted request and the JSON response is
#'              then converted to Dataframe, if dataToDF is TRUE.
#'
#' @param dataRequests : dataRequests should be formed using formatDataRequest method
#' @param properties : properties
#'
#' @return Response Class
#'

      getDataBundle = function(dataRequests, properties= NULL)
      {
          logger::log_info('Datastream.R ', 'DataClient$getDataBundle ', 'Getting Data Bundle' )

          super$CheckToken()
          getDataBundleURL = paste(self$RequestUrl, "GetDataBundle", sep = "")

          # Data Requests formatting
          requests = DSRequests$new()
          getDataBundleRequest = requests$getGetDataBundleRequest(dataRequests, self$Token, properties)

          # Post data to get the JSON Response
          jsonResp = tryCatch (
          {
              self$getJsonResponse(getDataBundleURL, getDataBundleRequest)
          },
          error = function(e)
          {
              logger::log_error(paste('Datastream.R', 'DataClient$getDataBundle',
                              'Exception :', message(e) ))
              return (message(e))
          }
        )

        if (is.list(jsonResp) & ("DataResponses" %in% names(jsonResp)))
        {
            # Converting JSON Response to DSGetDataBundleResponse class object
            getDataBundleResponse = DSGetDataBundleResponse$new(jsonResp$DataResponse, jsonResp$Properties,
                                                  self$useNaNforNotANumber)
            return (getDataBundleResponse)
        }
        else
            return (jsonResp)

      },

# ---------------------- toDataframe ----------------------------------------------------
#' @description Converts the Class response into a Dataframe which can be further used to plot
#' @param dataResponse : The raw data response that is ingested into Dataframe
#' @return Dataframe
#'
      toDataframe = function(dataResponse)
      {
          # Internal method:
          # Converts the Class response into a Dataframe which can be further used to plot
          df = NULL

          instrument = NULL
          currency = "NA"
          values = list()
          columns = list()
          columnValues = list()

          transpose = FALSE # in case of columns are > rows, for eg: Lists data

          dates = dataResponse$Dates
          #dates = sapply(dataResponse$Dates, FUN = function(x) { return (as.character(x))})
          if (!is.null(dates) & length(dates) > 0)
          {
              columns = append (columns, "Dates")
              columnValues[["Dates"]] = dates
          }

          datatypeValues = dataResponse$DataTypeValues

          for (datatypeValue in datatypeValues)
          {
              datatype = datatypeValue$DataType

              for (symbolValue in datatypeValue$SymbolValues)
              {
                  instrument = trimws(symbolValue$Symbol)

                  if (!is.null(symbolValue$Currency))
                      currency = symbolValue$Currency

                  if (symbolValue$Type %in% DSSymbolResponseValueType)
                  {
                      #if (symbolValue$Type == DSSymbolResponseValueType$DateTime)
                        #values = as.character(symbolValue$Value)
                      #else
                        values = symbolValue$Value

                      # Array Values returned should be equal to that of dates
                      if (length(values) > 1)
                          transpose = FALSE
                      else if (length(values == 1))
                          transpose = TRUE

                      colName = paste (instrument, datatype, currency, sep = "|")
                      columns = append (columns, colName)
                      columnValues[[colName]] = c(values)
                  }
            }
        }
        if (transpose)
        {
            df = t (data.frame(t (lapply (columnValues, function(x) Reduce(c, x)))))
            rownames(df) = columns
        }
        else
        {
            df = data.frame(lapply (columnValues, function(x) Reduce(c, x)))
            colnames(df) = columns
        }

        return (df)
    }
))


