% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/DSUserDataObjectBase.R
\docType{data}
\name{DSUserObjectAccessRights}
\alias{DSUserObjectAccessRights}
\title{Datastream User Object access rights}
\format{
An object of class \code{list} of length 2.
}
\usage{
DSUserObjectAccessRights
}
\value{
numeric
}
\description{
All user created objects have a flag specifying if they can be modified by the u
All items that have their ShareType property set to DSUserObjectShareTypes.
PrivateUserGroup will also have their AccessRight property set to ReadWrite.
Global expression objects, not being editable by users, will have the AccessRight
property set to Read.
}
\keyword{datasets}
