\name{hello}
\alias{hello}
\title{Hello, World!}
\usage{
  gethouse()
}
\description{
  Collect Korean residence plan list from Public api route.
  You may specify either a municipal regional name or the the type of supplying residence
}
\examples{
  gethouse(page = 1, region = '서울', supply = "행복", sz = 1000, api_key = NULL)
}
