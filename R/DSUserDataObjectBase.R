#' @include Common.R

apiSchemaNamespace = ':http://dsws.datastream.com/client/V1/'

#' @name DSUserObjectTypes
#' @title DSUserObjectTypes
#'
#' @description Five user created types are supported. When the client classes communicate with the API
#' server, a DSUserObjectTypes property is set to specify the object type.
#' Responses from the API server also specify the type of the object being returned.
#'
#' @return numeric

DSUserObjectTypes <- list ( NoType = 0, #' if your request fails, returning no user created item, the response object's UserObjectType field will be set to this value
                            List = 1,
                            Index = 2,
                            TimeSeries = 3,
                            Expression = 4,
                            Regression = 5)

#' @name DSUserObjectResponseStatus
#' @title DSUserObjectResponseStatus
#'
#' @description All client methods to retrieve or modify user created items return a respone object which
#' includes a ResponseStatus property.The ResponseStatus property specifies success or failure
#' for the request using a DSUserObjectResponseStatus value\cr
#'\cr
#' Response Values:\cr
#' UserObjectSuccess : The request succeeded and the response object's UserObject(s) property
#' should contain the (updated) object (except for DeleteItem method).\cr
#' UserObjectPermissions : Users need to be specifically permissioned to create custom objects.
#'  This flag is set if you are not currently permissioned.\cr
#' UserObjectNotPresent : Returned if the requested ID does not exist.\cr
#' UserObjectFormatError : Returned if your request object is not in the correct format.\cr
#' UserObjectTypeError : Returned if your supplied object is not the same as the type specified.\cr
#' UserObjectError : The generic error flag. This will be set for any error not specified above.\cr
#'
#' Examples are: \cr
#' 1.Requested object ID is not present \cr
#' 2.You have exceeded the number of custom objects permitted on your account.
#'
#' @return numeric
#' @export DSUserObjectResponseStatus

DSUserObjectResponseStatus <- list ( UserObjectSuccess = 0,
                                     UserObjectPermissions = 1,
                                     UserObjectNotPresent = 2,
                                     UserObjectFormatError = 3,
                                     UserObjectTypeError = 4,
                                     UserObjectError = 5)
#' @name DSUserObjectFrequency
#' @title DSUserObjectFrequency
#' @description Regressions and Timeseries objects specify a frequency for the underlying data.
#' DSUserObjectFrequency defines the supported frequencies.
#' @return numeric
#' @export DSUserObjectFrequency


DSUserObjectFrequency <- list ( Daily = 0,
                                Weekly = 1,
                                Monthly = 2,
                                Quarterly = 3,
                                Yearly = 4)

#' @name DSUserObjectShareTypes
#' @title DSUserObjectShareTypes
#' @description All user created objects have a flag specifying how they are shared with other users.
#' Currently only PrivateUserGroup and Global are supported.\cr
#' \cr
# 'For all object types other than expressions, the share type is always PrivateUserGroup.
#' PrivateUserGroup are items created by any Datastream ID that shares a parent Datastream ID
#' with your ID. Only children of the parent ID can access the user o
#' Expressions can be either PrivateUserGroup or Global. Like the other object types,
#' PrivateUserGroup items are items created by users and visible to just children of their
#' Datastream parent ID. PrivateUserGroup expressions have the ID signature Eaaa,
#' where 'a' is any alphabetical character.\cr
#' \cr
#' Global expressions are Datastream owned expressions that are available for use by any client.
#' These have the signature nnnE, where n is a digit.
#' Global expressions can be retrieved using the API service, but you cannot modify them.
#' Only your PrivateUserGroup items can be modified.
#'
#' @return numeric
#' @export DSUserObjectShareTypes

DSUserObjectShareTypes <- list ( NoType = 0,
                                 Company = 1, # not currently supported
                                 PrivateUserGroup = 2,
                                 UserGroup = 3, # not currently supported
                                 Global = 4)

#' @name DSUserObjectAccessRights
#' @title Datastream User Object access rights
#' @description All user created objects have a flag specifying if they can be modified by the u
#' All items that have their ShareType property set to DSUserObjectShareTypes.
#' PrivateUserGroup will also have their AccessRight property set to ReadWrite.
#' Global expression objects, not being editable by users, will have the AccessRight
#' property set to Read.
#' @return numeric
#' @export DSUserObjectAccessRights


DSUserObjectAccessRights <- list ( ReadWrite = 0,
                                   Read = 1)

#' @name DSUserObjectBase
#' @title DSUserObjectBase
#' @description DSUserObjectBase is the base object for all five user object types.
#' It defines the properties common to all the types
#'
#' @field Id  The object identifier. The format is specific to each object type. See the individual
#' object file for the particular specification
#' @field Mnemonic  For all object types bar indices, this is the same as the Id property.
#' For indices, the ID (of the form X#:Xnnnnn where n is a digit) is  returned when you create an index
#' and is used to manage the index via the API interface. The Mnemonic property is specified when creating an
#' index and is used to reference the index when using Datastream tools such as Charting, Datastream For Office, etc.
#' A mnemonic has format X#aaaaa where aaaaa is 1 to 6 alphanumeric characters.
#' @field DisplayName  A string describing the object. The maximum length varies from object type to
#' object type.\cr
#' Expression: Max 30 alphanumeric characters.\cr
#' Index: Max 60 alphanumeric characters.\cr
#' List: Max 60 alphanumeric characters.\cr
#' Regression: Max 50 alphanumeric characters.\cr
#' Timeseries: Max 64 alphanumeric characters.\cr
#' @field Description Currently this isn't supported. When the API returns an object, the Description
#' property will be the same as the DisplayName property.
#' @field Created a datetime value representing the date when the object was first created.
#' @field LastModified a datetime value representing the date when the object was last updated.
#' @field Owner The parent Datastream ID that owns the object. This will be the parent of your Datastream ID.
#' For global expressions this will always be 'Admin'
#' @field ShareType For all objects except global expressions, this will be DSUserObjectShareTypes.
#' PrivateUserGroup. For global expressions it will be DSUserObjectShareTypes.Global.
#' @field AccessRight For all objects except global expressions, this will be DSUserObjectAccessRights.
#' ReadWrite. For global expressions it will be DSUserObjectAccessRights.Read.
#'
#' @return DSUserObjectBase object
#' @export DSUserObjectBase



DSUserObjectBase <- R6Class("DSUserObjectBase",
                            public = list( Id = NULL, Mnemonic = NULL, DisplayName = NULL, Description = NULL,
                                               Created = NULL, LastModified = NULL, Owner = NULL, ShareType = numeric(1),
                                               AccessRight = numeric(1),

#' @description Initialize
#' @param jsonResp : json Response
#' @return DSUserObjectBase object
#'
      initialize = function (jsonResp = NULL)
      {
          self$Id = NULL
          self$Mnemonic = ""
          self$DisplayName = ""
          self$Description = ""
          self$Created = as.POSIXlt(Sys.time(), tz = "UTC")  # only valid when received as a response. On create or update this field is ignored
          self$LastModified = as.POSIXlt(Sys.time(), tz = "UTC") # only valid when received as a response. On create or update this field is ignored
          self$Owner = NULL   # only valid when received as a response. On create or update this field is ignored
          self$ShareType = as.numeric(DSUserObjectShareTypes$PrivateUserGroup) # all items except reserved global expressions (available to all clients) are PrivateUserGroup
          self$AccessRight = as.numeric(DSUserObjectAccessRights$ReadWrite) # all items except reserved global expressions (available to all clients) are ReadWrite

          # upon a successful response from the API server jsonDict will be used to populate the DSUserObjectBase object with the response data.
          if (!is.null(jsonResp))
          {
              self$Id = jsonResp$Id
              self$Mnemonic = jsonResp$Mnemonic
              self$DisplayName = jsonResp$DisplayName
              self$Description = jsonResp$Description
              self$Created = jsonDateToDatetime(jsonResp$Created)
              self$LastModified = jsonDateToDatetime(jsonResp$LastModified)
              self$Owner = jsonResp$Owner
              self$ShareType = jsonResp$ShareType
              self$AccessRight = jsonResp$AccessRight
          }
      },

#'
#' @description The following parameters are set only in response when we query for user created items.
#'              This method is called before Create or Update to ensure safe values set prior to JSON encoding
#' @return No return value

      SetSafeUpdateParams = function()
      {
          self$Created = as.POSIXlt(Sys.time(), tz = "UTC")  # only valid when received as a response. On create or update this field is ignored
          self$LastModified = as.POSIXlt(Sys.time(), tz = "UTC") # only valid when received as a response. On create or update this field is ignored
          self$Owner = NULL   # only valid when received as a response. On create or update this field is ignored
          self$ShareType = DSUserObjectShareTypes$PrivateUserGroup # all items except reserved global expressions (available to all clients) are PrivateUserGroup
          self$AccessRight = DSUserObjectAccessRights$ReadWrite # all items except reserved global expressions (available to all clients) are ReadWrite
          # Description is not currently implemented, so ensure safety as string if set
          self$Description = ifelse ((is.character(self$Description)), self$Description, "")
          self$DisplayName = ifelse ((is.character(self$DisplayName)), self$DisplayName, "")
      }
))

#' @name DSUserObjectGetAllResponse
#' @title DSUserObjectGetAllResponse
#'
#' @description This is the object returned for the client class' GetAllItems query only.\cr
#' \cr
#' For GetAllItems queries only, the returned objects will not have all their property fields set.\cr
#' Specifically for below:\cr
#' Expression: All property fields are fully populated.\cr
#' Index: The ConstituentsCount property will correctly specify the number of constituents but the
#' Constituents property will be NULL.\cr
#' List: The ConstituentsCount property will correctly specify the number of constituents but the
#' Constituents property will be NULL.\cr
#' Regression: All property fields are fully populated.\cr
#' Timeseries: The ValuesCount field of the DateRange property will specify the number of date value
#' pairs, but the Dates and Values fields will be NULL.\cr
#' You need to query for the individual object using the GetItem request to retrieve the full content
#' for the object.
#'
#' @field UserObjectType specifies the returned object types. e.g. DSUserObjectTypes.List,
#' DSUserObjectTypes.TimeSeries, etc.
#' @field UserObjects An array of the specified object types such as DSListUserObject,
#' DSRegressionUserObject, etc.
#' @field UserObjectsCount The number of objects returned in the UserObjects property.
#' @field ResponseStatus This property will contain a DSUserObjectResponseStatus value.
#' DSUserObjectResponseStatus.UserObjectSuccess represents a successful response.
#' @field ErrorMessage If ResponseStatus is not DSUserObjectResponseStatus.UserObjectSuccess this status
#' string will provide a description of the error condition.
#' @field Properties Not currently used and will currently always return NULL.
#'
#' @return DSUserObjectGetAllResponse object
#' @export DSUserObjectGetAllResponse

DSUserObjectGetAllResponse <- R6Class("DSUserObjectGetAllResponse",
                                      public = list(UserObjectType = numeric(1),
                                                    UserObjects = NULL,
                                                    UserObjectsCount = numeric(10),
                                                    ResponseStatus = numeric(1),
                                                    ErrorMessage = character(200),
                                                    Properties = NULL,



#' @description Initialize
#' @param jsonResp JSON Response
#' @return DSUserObjectGetAllResponse object
#'
      initialize = function(jsonResp = NULL)
      {
          self$UserObjectType = DSUserObjectTypes$NoType
          self$UserObjects = NULL
          self$UserObjectsCount = 0
          self$ResponseStatus = DSUserObjectResponseStatus$UserObjectSuccess
          self$ErrorMessage = ''
          self$Properties = NULL

          # upon a successful response from the API server jsonDict will be used to populate the DSUserObjectGetAllResponse object with the response data.
          if (!is.null(jsonResp))
          {
            self$UserObjectType = jsonResp$UserObjectType
            self$UserObjects = jsonResp$UserObjects
            self$ResponseStatus = jsonResp$ResponseStatus
            self$UserObjectsCount = jsonResp$UserObjectsCount
            self$ErrorMessage = jsonResp$ErrorMessage
            self$Properties = jsonResp$Properties
          }
      }
))

#' @name DSUserObjectResponse
#' @title DSUserObjectResponse
#' @description This is the object returned from the client class' GetItem, CreateItem, UpdateItem
#' and DeleteItem requests.
#' @field UserObjectId The ID of the object requested. If the item is deleted, the UserObject
#' property will be NULL but the UserObjectId field will be populated
#' @field UserObjectType specifies the returned object type. e.g. DSUserObjectTypes.List,
#' DSUserObjectTypes.TimeSeries, etc.
#' @field UserObject For all queries bar DeletItem, if the query is successful, this property will
#' contain the user created item requested.
#' @field ResponseStatus This property will contain a DSUserObjectResponseStatus value.
#' DSUserObjectResponseStatus.UserObjectSuccess represents a successful response.
#' @field ErrorMessage If ResponseStatus is not DSUserObjectResponseStatus.UserObjectSuccess this
#' status string will provide a description of the error condition.
#' @field Properties Not currently used and will currently always return NULL.
#'
#' @return DSUserObjectResponse object
#' @export DSUserObjectResponse


DSUserObjectResponse = R6Class("DSUserObjectResponse",
                              public = list(UserObjectId = NULL,
                                            UserObjectType = numeric(1),
                                            UserObject = NULL,
                                            ResponseStatus = numeric(1),
                                            ErrorMessage = character(200),
                                            Properties = NULL,

#' @description Initialize
#' @param jsonResp JSON Response
#' @return DSUserObjectResponse object

      initialize = function(jsonResp = NULL)
      {
          self$UserObjectId = NULL
          self$UserObjectType = DSUserObjectTypes$NoType
          self$UserObject = NULL
          self$ResponseStatus = DSUserObjectResponseStatus$UserObjectSuccess
          self$ErrorMessage = ''
          self$Properties = NULL

          if (!is.null(jsonResp))
          {
              # upon a successful response from the API server jsonResp will be used to populate the DSUserObjectResponse object with the response data.
              self$UserObjectType = jsonResp$UserObjectType
              self$UserObjectId = jsonResp$UserObjectId
              self$ResponseStatus = jsonResp$ResponseStatus
              self$UserObject = jsonResp$UserObject
              self$ErrorMessage = jsonResp$ErrorMessage
              self$Properties = jsonResp$Properties
          }
      }
))

