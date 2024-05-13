#' @include DSConnect.R

hints = list(E ="IsExpression",
             N = "ReturnName")

DSDateFrequencyNames = list (D = 0, W = 1, M = 2, Q = 3, Y = 4)

DSDateKind = list (Snapshot = 0, TimeSeries = 1)



DSRequests = R6Class("DSRequests",
                     public = list(

        setInstrument = function(tickers)
        {
  # The tickers or instruments are provided with hints E (for IsExpression)
  # for Expression/s.
  # For Example: dataClient$getData('MAV#(IN:CYD,20D)|E', NULL, kind=1)

  # If the instrument given is a Expression, (see ClientAPI test page:
  # http://product.datastream.com/dswsclient/Docs/TestRestV1.aspx for hint
  # properties). If IsExpression should be enabled, so that the MF processes
  # the request properly. The same has been implemented in R by giving
  # <Expression/s>|E. Thus the tickers are tested for "|" in the below code.
  # Similarly, |N is used to retrun names of the instruments and datatypes in
  # the response.

  # Note: If the IsExpression is enabled, DSWS ignores the datatypes entered in
  # fields param in the getData request.

  # Always return Name
          props = list (Key = hints$N, Value = TRUE)
          DSInstrument = list (Value = "", Properties = NULL)

          if (grepl("|", tickers, fixed = TRUE))
          {
              temp = strsplit(tickers, split = "|", fixed = TRUE)
              givenTickers = temp[[1]][1]
              givenProps = temp[[1]][2]

              if (grepl(",", givenProps, fixed = TRUE))
                  givenProps = strsplit(givenProps, split = ",", fixed = TRUE)

              for (property in givenProps)
              {
                  properties = lapply(property, FUN = function(x)
                  {
                    if (x == "E")
                        return (c(list(Key = hints$E, Value = TRUE), props))
                    else if (x == "N")
                        return (props)
                    else
                    {
                        logger::log_info("Invalid hints provided")
                        return (props)
                    }
                  })
                DSInstrument$Properties = properties
              }
              DSInstrument$Value = givenTickers
          }
          else
          {
              DSInstrument$Value = tickers
              DSInstrument$Properties = list (props)
          }

          return (DSInstrument)
      },

      setDatatypes = function(fields)
      {
          # Always return Name
          props = list(Key = hints$N, Value = TRUE)

          DataTypes = lapply(fields, FUN = function(x)
          {
              DSDataType = list (Value = x, Properties = list (props))
              return (DSDataType)
          })

          return (DataTypes)
      },

      setDate = function(start, end, freq, kind)
      {
          DSDate = list (Start = start,
                 End = end,
                 Frequency = freq,
                 Kind = kind)

          return (DSDate)
      },

      setDataRequest = function(tickers, fields, start, end, freq, kind)
      {
          if (is.null(fields))
            fields <- list()

          if (is.null(tickers))
              stop ("Tickers cannot be null")
          else if (!is.character(tickers))
              stop ("Tickers must be ',' separated string eg: \"VOD,BARC\"")
          else
              instrument = self$setInstrument(tickers)

          datatypes = self$setDatatypes(fields)

          date = self$setDate(start, end, freq, kind)

          DSDataRequest = list (Instrument = instrument,
                                DataTypes = datatypes,
                                Date = date,
                                Tag = NULL)

          return (DSDataRequest)
      },

      getGetDataRequest = function( tickers, fields, start, end, freq, kind, token, properties)
      {
          dataRequest = self$setDataRequest(tickers, fields, start, end, freq, kind)

          DSGetDataRequest = list (TokenValue = token,
                                   DataRequest = dataRequest,
                                   Properties = properties)

         return (DSGetDataRequest)
      },

      getGetDataBundleRequest = function(dataRequests, token, properties)
      {
          DSGetDataBundleRequest = list (TokenValue = token,
                                         DataRequests = dataRequests,
                                         Properties = properties)

          return (DSGetDataBundleRequest)
      }
    )
)







