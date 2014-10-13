\name{testFunctionWithSelect}
\alias{testFunctionWithSelect}
\title{A variation on functionToGalaxify that takes a multiple-choice option.}
\usage{
  testFunctionWithSelect(inputfile1 = GalaxyInputFile(),
    inputfile2 = GalaxyInputFile(),
    plotTitle = GalaxyCharacterParam(c(TitleA = "A", TitleB = "B"), force_select = TRUE),
    plotSubTitle = GalaxyCharacterParam("My subtitle"),
    outputfile1 = GalaxyOutput("mydata", "csv"),
    outputfile2 = GalaxyOutput("myplot", "pdf"))
}
\arguments{
  \item{inputfile1}{the first matrix}

  \item{inputfile2}{the second matrix}

  \item{plotTitle}{the plot title}

  \item{plotSubTitle}{the plot subtitle}

  \item{outputfile1}{the csv output file}

  \item{outputfile2}{the pdf output file}
}
\description{
  A variation on functionToGalaxify that takes a
  multiple-choice option.
}
\details{
  There are no details.
}

