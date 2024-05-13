#' @include Common.R
library(httr)
library(jsonlite)
library(logger)
library(ini)
library(R6)

# DataService provides the list of services
# 1. DATA - for using DataClient, for getData and getDataBundle.
# 2. UCS - User Created Services
# 3. ECONF - Economics Filter
DataService = list (UCS = 0,
                    ECONF = 1,
                    DATA = 2)


DSConnect = R6Class("DSConnect",
                     public = list(Config = NULL, Username = character(50), Password = character(50),
                                   Proxies = NULL, SslCert = NULL, Url = NULL, RequestUrl = character(100),
                                   Token = character(200), TokenExpiry = NULL, Timeout = numeric(2),
                                   Version = character(10), AppID = character(25),

#--------------------- Initialization ----------------------------------------------

#' @name DSConnect
#' @title initializer
#'
#' @description DSConnect connects to the Datastream web service on behalf of derived classes
#'              and helps to send and retrieve data
#'
#' @details user details can be supplied from a config file or passed directly as parameters in the
#'          constructor of the derived user object type class.
#'
#'        1) Using ini file (e.g. config.ini) with format
#'
#'        [credentials]
#'        username=YourID
#'        password=YourPwd
#'
#'        [proxies]
#'        url=Your Proxy Url
#'        port=Proxy port
#'        username=Proxy Username
#'        password=Proxy Password
#'
#'        [cert]
#'        sslCertFile=YourCertFile
#'        #only read if no sslCertFile entry
#'        sslCertPath=YourCertPath
#'
#' @field Config : config file path
#' @field Username : Your id
#' @field Password : Your Password
#' @field Proxies : Proxy Server details (if any)
#' @field SslCert : ssl Certifactes file path
#' @field Url : internal parameter set by application
#' @field RequestUrl : internal parameter set up by application
#' @field Token : Token received on successfull logon
#' @field TokenExpiry : Expiry time of the token
#' @field Timeout : Max Timeout set in Configuartion for the process to complete
#' @field Version : Application Version
#' @field AppID : Application Id

#' @description DSConnect connects to the Datastream web service on behalf of derived classes
# and helps to send and retrieve data
#'
#' @param config : config path
#' @param username : your username
#' @param password : your password
#' @param proxies : proxy url, if any
#' @param sslCert : path to ssl cert file
#' @param service : internally set by the application

        initialize = function(config = NULL,username = NA, password = NA, proxies = NULL,
                              sslCert = NULL, service = NULL)
          {
              # Initial Values assigned
              self$Url = "https://product.datastream.com"
              self$Version = "1.0.2"
              self$AppID = paste("DatastreamR-", self$Version, sep = "")
              self$TokenExpiry = NULL
              self$Token = ""
              self$Config = config

              # Select the Rest Service
              if (service == DataService$UCS)
                  restURL = "/DSWSClient/V1/DSUserDataService.svc/rest/"
              else if (service == DataService$ECONF)
                  restURL = "/DSWSClient/V1/DSEconomicsFilterService.svc/rest/"
              else
                  restURL = "/DSWSClient/V1/DSService.svc/rest/"

            if (!is.null(self$Config))
            {
                filetype = ".ini"
                if  (is.character(self$Config) & (grepl(filetype, self$Config) == FALSE))
                {
                    msg = "You must supply valid Config file"
                    logger::log_error(msg)
                    return (msg)
                }

                configParser = NULL

                configParser = tryCatch( { ini::read.ini(self$Config) },
                error = function(e) { logger::log_error (message(e))
                                      return (message(e)) })

                if (!is.null(configParser))
                {
                    if (!is.null(configParser$url$path))
                        self$Url = configParser$url$path

                    if (!is.null(configParser$credentials$username) & !is.null(configParser$credentials$password))
                    {
                        self$Username = configParser$credentials$username
                        self$Password = configParser$credentials$password
                    }

                    self$Proxies = list(url = configParser$proxies$url,
                           port = configParser$proxies$port,
                           username = configParser$proxies$username,
                           password = configParser$proxies$password)

                    if (!is.null(configParser$cert$sslCertFile))
                        self$SslCert = configParser$cert$sslCertFile
                    else
                        self$SslCert = configParser$cert$sslCertPath
              }

          }
          else if (username == "" || password == "")
          {
                errMsg = "You must supply some user credentials."
                logger::log_info(paste('DSConnect.R', ' DSConnect$initialise',
                             errMsg))
                return (errMsg)
          }
          else
          {
              self$Username = username
              self$Password = password
              self$Proxies = proxies
              self$SslCert = sslCert
          }

          self$RequestUrl = paste(self$Url,  restURL, sep = "")

          self$getToken()
      },

#--------------------- getJsonResponse ----------------------------------------------
#' @description This method makes the query against the API service and does some basic error handling
#' @param requestUrl : Url used to send the Post request
#' @param request : Raw Datastream request
#'
      getJsonResponse = function(requestUrl, request)
      {
          response = NULL

          if (!is.null(self$SslCert))
          {
              response = tryCatch(
              {
                  httr::POST(url = requestUrl,
                             body = request,
                             encode = "json",
                             use_proxy(url = self$Proxies$url,
                             port = self$Proxies$port,
                             username = self$Proxies$username,
                             password = self$Proxies$password),
                  config = list(cainfo = self$SslCert, ssl.verifypeer=TRUE),
                  content_type("application/json"))
              },
              error = function(e)
              {
                logger::log_info(paste('DSConnect.R', ' DSConnect$getJsonResponse',
                               "Exception :", message(e)))
                return (message(e))
              })
          }
          else
          {
              response = tryCatch(
              {
                  httr::POST(url = requestUrl,
                             body = request,
                             encode = "json",
                             use_proxy(url = self$Proxies$url, port = self$Proxies$port,
                             username = self$Proxies$username, password = self$Proxies$password),
                  content_type("application/json"))
              },
              error = function(e)
              {
                  logger::log_error(paste('DSConnect.R', ' DSConnect$getJsonResponse',
                               "Exception :", message(e)))
                  return (message(e))
              })
          }

          if (status_code(response) == 200)
          {
              logger::log_info(paste('DSConnect.R', ' DSConnect$getJsonResponse',
                            "Web response received"))
              jsonResp = content(response, "parsed")
              return (jsonResp)
          }
          else if (status_code(response) == 400 ||status_code(response) == 403)
          {
              jsonResp = content(response, "parsed")
              if (!is.null(jsonResp))
              {
                  if (!is.null(jsonResp$Message) && !is.null(jsonResp$Code))
                  {
                      fault = DSFault(jsonResp)
                      logger::log_info(paste('DSConnect.R', ' DSConnect$getJsonResponse',
                               "DSFault message :", fault$Message))
                      return (fault)
                  }
              }
          }
          else
              return (status_code(response))
      },

#--------------------- getToken --------------------------------------------------

#' @description getToken uses you credentials to try and obtain a token to be used in subsequent request for data.
#'              The returned token is valid for 24 hours

      getToken = function()
      {
          logger::log_info('DSConnect.R', ' DSConnect$getToken', ' :Getting Token and Token Expiry')

          tokenUrl = paste(self$RequestUrl, "GetToken", sep = "")

          properties = list (list (Key ="__AppId", Value = self$AppID))

          tokenRequest = list (Password = self$Password,
                       UserName = self$Username,
                       Properties = properties)

          jsonResp = tryCatch (
          {
              self$getJsonResponse(tokenUrl, tokenRequest)
          },
          error = function(e)
          {
               logger::log_error(paste('DSConnect.R', ' DSConnect$getToken',
                              message(e)))
              return (message(e))
          })

          if (!is.null(jsonResp))
          {
              if ("TokenValue" %in% names(jsonResp) & "TokenExpiry" %in% names(jsonResp))
              {
                  self$Token = as.character(jsonResp$TokenValue)
                  jsonDate = as.character(jsonResp$TokenExpiry)
                  normDate = as.POSIXlt(jsonDateToDatetime(jsonDate))
                  if (!is.null(normDate))
                      self$TokenExpiry = as.POSIXlt(normDate)
              }
              else
              {
                  if ("Message" %in% names(jsonResp))
                  {
                    logger::log_error(jsonResp$Message)
                    message (jsonResp$Message)
                  }
              }
          }
          else
          {
              errMsg = "No response received"
              logger::log_error(paste('DSConnect.R', ' DSConnect$getToken',
                                  errMsg))
              return (errMsg)
          }
      },

#--------------------- IsValid ------------------------------------------------------

#' @description IsValid checks whether the token is valid against the token expiry time

      IsValid = function()
      {
          return (is.character(self$Token) & nchar(self$Token) > 0 && (!is.null(self$TokenExpiry)))
      },


#--------------------- CheckToken ------------------------------------------------------

#' @description CheckToken checks whether the token is valid against the token expiry time
#'              and generates new token if the token has expired.

      CheckToken = function()
      {
          #isValid = self$IsValid()
          if (!self$IsValid() )
            stop("You are not logged on. Please recreate the client supplying valid user credentials.")

          nowTime = as.POSIXlt(Sys.time(), tz = "UTC")
          timeDiff = difftime(self$TokenExpiry, nowTime, units = "mins", tz = "UTC")

          timeRenew = as.difftime(15, units = "mins", tz = "UTC") # Token to be renewed, before it expires by next 15 mins

          if (timeDiff <= timeRenew)
            self$getToken()

      }
))

# DSFault exception is a representation of the DSFault error returned from the API server.
#        DSFaults are returned for the following reasons:
#           1. Invalid credentials
#           2. Empty credentials
#           3. Access blocked due to missuse of the service.
#     A DSFault message will be thrown in the event a DSFault is returned from the server.

DSFault = R6Class("DSFault",
                   public = list(Message = character, Code = NULL, SubCode = NULL,
        initialize = function(jsonDict)
        {
            self$Message = jsonDict$Message
            self$Code = jsonDict$Code
            self$SubCode = jsonDict$SubCode
        }
))




