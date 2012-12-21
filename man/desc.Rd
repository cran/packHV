\name{desc}
\alias{desc}
\title{Making descriptive statistics}
\description{Makes descriptive statistics of a data frame according to a group covariate or not, can export the results}
\usage{
desc(data, vars, group = NULL, vars.labels = vars, type.quanti = "med",
     test = TRUE, noquote = TRUE, justify = TRUE, digits = 2,
     file.export = NULL, language = "english")
}
\arguments{
  \item{data}{data frame to describe in which we can find \code{vars} and \code{group}}
  \item{vars}{vector of character strings of the covariates to describe}
  \item{group}{character string, statistics created for each levels of this covariate}
  \item{vars.labels}{vector of character string for sweeter names of covariates in the output}
  \item{type.quanti}{character string, \code{"med"} to compute median [Q1;Q3], \code{"mean"} to compute mean (sd) or \code{"med_mm"} or \code{"mean_mm"} to add (min;max)}
  \item{test}{boolean, \code{TRUE} to perform tests (\code{FALSE} if \code{group} is \code{NULL}): Khi-2 or Fisher exact test for categorical covariates, t-test/ANOVA or Wilcoxon/Kruskal-Wallis Rank Sum Test for numerical covariates}
  \item{noquote}{boolean, \code{TRUE} to hide quotes when printing the table}
  \item{justify}{boolean, \code{TRUE} to justify columns on right or left (\code{FALSE} if export)}
  \item{digits}{number of digits of the statistics (mean, sd, median, min, max, Q1, Q3, \%), p-values always have 3 digits}
  \item{file.export}{character string, name of the XLS file exported}
  \item{language}{character string, \code{"french"} or \code{"english"}}
}

\value{A matrix of the descriptive statistics}

\author{Hugo Varet}

\examples{
N=1000
my.data=data.frame(a=rnorm(N),b=factor(rbinom(N,1,0.5)),c=factor(sample(LETTERS[1:3],N,TRUE)),
                   group=factor(sample(c("gr1","gr2"),N,TRUE)))
my.data$a[c(3,64,465)]=NA
my.data$b[c(34,634,65)]=NA
my.data$a[c(23,624,495)]=NA
my.data$group[c(934,83,46)]=NA
desc(my.data,c("a","b","c"),"group")
}
