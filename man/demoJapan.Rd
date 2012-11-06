\name{demoJapan}
\alias{demoJapan}
\docType{data}
\title{
Japan Mortality Rates for life table construction
}
\description{
Two yearly mortality rates for each age
}
\usage{data(demoJapan)}
\format{
  A data frame with 110 observations on the following 3 variables.
  \describe{
    \item{\code{age}}{a factor with levels \code{0} \code{1} \code{10} \code{100} \code{101} \code{102} \code{103} \code{104} \code{105} \code{106} \code{107} \code{108} \code{109} \code{11} \code{12} \code{13} \code{14} \code{15} \code{16} \code{17} \code{18} \code{19} \code{2} \code{20} \code{21} \code{22} \code{23} \code{24} \code{25} \code{26} \code{27} \code{28} \code{29} \code{3} \code{30} \code{31} \code{32} \code{33} \code{34} \code{35} \code{36} \code{37} \code{38} \code{39} \code{4} \code{40} \code{41} \code{42} \code{43} \code{44} \code{45} \code{46} \code{47} \code{48} \code{49} \code{5} \code{50} \code{51} \code{52} \code{53} \code{54} \code{55} \code{56} \code{57} \code{58} \code{59} \code{6} \code{60} \code{61} \code{62} \code{63} \code{64} \code{65} \code{66} \code{67} \code{68} \code{69} \code{7} \code{70} \code{71} \code{72} \code{73} \code{74} \code{75} \code{76} \code{77} \code{78} \code{79} \code{8} \code{80} \code{81} \code{82} \code{83} \code{84} \code{85} \code{86} \code{87} \code{88} \code{89} \code{9} \code{90} \code{91} \code{92} \code{93} \code{94} \code{95} \code{96} \code{97} \code{98} \code{99}}
    \item{\code{JP8587M}}{a factor with levels \code{0.00017} \code{0.00019} \code{0.00020} \code{0.00022} \code{0.00026} \code{0.00029} \code{0.00031} \code{0.00034} \code{0.00036} \code{0.00039} \code{0.00042} \code{0.00048} \code{0.00058} \code{0.00067} \code{0.00076} \code{0.00086} \code{0.00087} \code{0.00089} \code{0.00090} \code{0.00091} \code{0.00092} \code{0.00095} \code{0.00096} \code{0.00097} \code{0.00098} \code{0.00103} \code{0.00109} \code{0.00110} \code{0.00114} \code{0.00117} \code{0.00118} \code{0.00119} \code{0.00127} \code{0.00137} \code{0.00138} \code{0.00152} \code{0.00167} \code{0.00184} \code{0.00200} \code{0.00218} \code{0.00237} \code{0.00258} \code{0.00284} \code{0.00315} \code{0.00351} \code{0.00394} \code{0.00442} \code{0.00494} \code{0.00538} \code{0.00587} \code{0.00643} \code{0.00705} \code{0.00767} \code{0.00825} \code{0.00886} \code{0.00951} \code{0.01022} \code{0.01101} \code{0.01204} \code{0.01319} \code{0.01449} \code{0.01594} \code{0.01757} \code{0.01940} \code{0.02145} \code{0.02375} \code{0.02633} \code{0.02922} \code{0.03246} \code{0.03609} \code{0.04015} \code{0.04469} \code{0.04976} \code{0.05544} \code{0.06177} \code{0.06884} \code{0.07672} \code{0.08550} \code{0.09526} \code{0.10611} \code{0.11814} \code{0.13147} \code{0.14623} \code{0.16248} \code{0.18039} \code{0.20007} \code{0.22161} \code{0.24556} \code{0.27123} \code{0.29893} \code{0.32867} \code{0.36047} \code{0.39431} \code{0.43021} \code{0.46816} \code{0.50815} \code{0.55019} \code{0.59428} \code{0.64042} \code{0.68861} \code{0.73885} \code{1.00000}}
    \item{\code{JP8587F}}{a factor with levels \code{0.00013} \code{0.00014} \code{0.00015} \code{0.00016} \code{0.00018} \code{0.00019} \code{0.00021} \code{0.00022} \code{0.00023} \code{0.00026} \code{0.00030} \code{0.00031} \code{0.00035} \code{0.00038} \code{0.00040} \code{0.00041} \code{0.00042} \code{0.00043} \code{0.00044} \code{0.00046} \code{0.00049} \code{0.00053} \code{0.00056} \code{0.00059} \code{0.00062} \code{0.00065} \code{0.00069} \code{0.00075} \code{0.00081} \code{0.00086} \code{0.00091} \code{0.00094} \code{0.00096} \code{0.00103} \code{0.00110} \code{0.00119} \code{0.00126} \code{0.00128} \code{0.00137} \code{0.00147} \code{0.00158} \code{0.00169} \code{0.00183} \code{0.00199} \code{0.00215} \code{0.00233} \code{0.00252} \code{0.00272} \code{0.00292} \code{0.00312} \code{0.00331} \code{0.00353} \code{0.00378} \code{0.00407} \code{0.00442} \code{0.00481} \code{0.00530} \code{0.00583} \code{0.00644} \code{0.00713} \code{0.00791} \code{0.00879} \code{0.00979} \code{0.01092} \code{0.01220} \code{0.01365} \code{0.01529} \code{0.01714} \code{0.01924} \code{0.02161} \code{0.02429} \code{0.02732} \code{0.03075} \code{0.03462} \code{0.03898} \code{0.04390} \code{0.04945} \code{0.05570} \code{0.06273} \code{0.07064} \code{0.07952} \code{0.08948} \code{0.10064} \code{0.11313} \code{0.12707} \code{0.14263} \code{0.16001} \code{0.17925} \code{0.20054} \code{0.22401} \code{0.24981} \code{0.27806} \code{0.30870} \code{0.34166} \code{0.37684} \code{0.41417} \code{0.45358} \code{0.49505} \code{0.53860} \code{0.58423} \code{0.63195} \code{0.67817} \code{0.72297} \code{0.76652} \code{1.00000}}
  }
}

\details{
See the references link for details.
}
\source{
Society of Actuaries mortality web site
}
\references{
\url{www.mort.soa.org}
}
\examples{
data(demoJapan)
head(demoJapan)
}
\keyword{datasets}
