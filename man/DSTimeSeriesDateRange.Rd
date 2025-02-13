% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DatastreamUserCreated_TimeSeries.R
\name{DSTimeSeriesDateRange}
\alias{DSTimeSeriesDateRange}
\title{DSTimeSeriesDateRange}
\value{
DSTimeSeriesDateRange object
}
\description{
This class is a supporting attribute for the DateRange property of the DSTimeSeriesResponseObject.
It returns the raw data for the timeseries. The DateRange property of the DSTimeSeriesResponseObject always returns
the dates for a given frequency as the first date in each period. (e.g. 2022-01-01, 2020-04-01, etc. for quarterly
frequencies). You specify whether you want your users to receive either the first, mid or end dates in the given
period by setting the DateAlignment property (DSTimeSeriesDateAlignment) of the DSTimeSeriesRequestObject. \cr
\cr
When you retrieve a list of all your available timeseries using the GetAllItems method, since this list could contain
many thousand timeseries objects, the Dates and Values lists will always be NULL. Only the ValuesCount field will be
set to reflect the number of datapoints available for each item. You need to request an individual timeseries
(GetItem method) in order to receive a response containing actual data in the Dates and Values properties.
}
\section{Public fields}{
\if{html}{\out{<div class="r6-fields">}}
\describe{
\item{\code{Dates}}{\itemize{
\item A list of datetime values specifying the dates for each datapoint.
}}

\item{\code{Values}}{\itemize{
\item A list of float values specifying the values for each datapoint.
}}

\item{\code{ValuesCount}}{\itemize{
\item A count of the number of datapoints in the timeseries.
}}
}
\if{html}{\out{</div>}}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-DSTimeSeriesDateRange-new}{\code{DSTimeSeriesDateRange$new()}}
\item \href{#method-DSTimeSeriesDateRange-clone}{\code{DSTimeSeriesDateRange$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DSTimeSeriesDateRange-new"></a>}}
\if{latex}{\out{\hypertarget{method-DSTimeSeriesDateRange-new}{}}}
\subsection{Method \code{new()}}{
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DSTimeSeriesDateRange$new(jsonDict, convertNullToNans = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{jsonDict}}{JSON dictionary (from JSON Response)}

\item{\code{convertNullToNans}}{FALSE by default, TRUE converts the NULLs in the NaNs (Not a Number)}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
DSTimeSeriesDateRange object
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-DSTimeSeriesDateRange-clone"></a>}}
\if{latex}{\out{\hypertarget{method-DSTimeSeriesDateRange-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{DSTimeSeriesDateRange$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
