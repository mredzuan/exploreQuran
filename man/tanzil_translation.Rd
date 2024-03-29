% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pull_translation.r
\name{tanzil_translation}
\alias{tanzil_translation}
\title{Pull Quran Translation From tanzil.net}
\usage{
tanzil_translation(urlLink)
}
\arguments{
\item{urlLink}{url translation link from https://tanzil.net/trans/}
}
\value{
list of translation text and info
}
\description{
Pull Quran Translation From tanzil.net
}
\examples{
\dontrun{
trans_malay_basmeih_list <- tanzil_translation("https://tanzil.net/trans/ms.basmeih")
trans_en_sahih_list <- tanzil_translation("https://tanzil.net/trans/en.sahih")
}

}
