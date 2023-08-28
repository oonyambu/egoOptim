#' @name accoustic
#' @docType data
#' @aliases accoustic
#' @title Tukish Music Emotion Data Set
#' @source Bilal Er, M., & Aydilek, I. B. (2019).
#'    Music emotion recognition by using chroma spectrogram and deep visual
#'    features.
#'    Journal of Computational Intelligent Systems, 12(2), 1622–1634.
#'    International Journal of Computational Intelligence Systems,
#'    DOI: https://doi.org/10.2991/ijcis.d.191216.001
#'
#' @description
#' a database of music for happy, sad, angry, relax emotions prepared by
#' selection of verbal and non-verbal music  from different genres of
#' Turkish music. The dataset is designed as a discrete model,
#' and there are four classes. A total of 100 music pieces are determined for
#' each class in the database to have an equal number of samples in each class.
#' There are 400 samples in the original dataset as 30 seconds from each sample.
#' Happy 100, Sad 100, Angry 100, Relax 100
#'
#' @usage data(accoustic)
#' @format A data frame with 400 observations and 51 features
NULL

#' @name breastTissue
#' @docType data
#' @aliases breast_tissue
#' @title Breast Tissue
#' @source S,JP and Jossinet,J. (2010). Breast Tissue.
#'  UCI Machine Learning Repository. https://doi.org/10.24432/C5P31H.
#'
#'
#' @description Impedance measurements were made at the frequencies: 15.625,
#' 31.25, 62.5, 125, 250, 500, 1000 KHz. Impedance measurements of freshly
#' excised breast tissue were made at the follwoing frequencies:
#'  15.625, 31.25, 62.5, 125, 250, 500, 1000 KHz. These measurements plotted
#'   in the (real, -imaginary) plane constitute the impedance spectrum from
#'   where the breast tissue features are computed. The dataset can be used
#'   for predicting the classification of either the original 6 classes or of
#'   4 classes by merging together the fibro-adenoma, mastopathy and glandular
#'   classes whose discrimination is not important (they cannot be accurately
#'    discriminated anyway).
#'
#' @details
#' \describe{
#' \item{\code{Class}}{The response variable with 6 levels:
#'   car(carcinoma), fad (fibro-adenoma), mas (mastopathy), gla (glandular),
#'   con (connective), adi (adipose)}
#' \item{\code{I0}}{Impedivity (ohm) at zero frequency}
#' \item{\code{PA500}}{phase angle at 500 KHz}
#' \item{\code{HFS}}{high-frequency slope of phase angle}
#' \item{\code{DA}}{impedance distance between spectral ends}
#' \item{\code{AREA}}{area under spectrum}
#' \item{\code{A/DA}}{area normalized by DA}
#' \item{\code{MAX IP}}{maximum of the spectrum}
#' \item{\code{DR}}{distance between I0 and real part of the
#'      maximum frequency point}
#' \item{\code{P}}{length of the spectral curve}
#' }
#' @usage data(breastTissue)
#' @format A data frame with 106 observations and 10 features
NULL


#' @name glass
#' @docType data
#' @aliases glass
#' @title Glass Identification
#'
#' @source German,B.. (1987). Glass Identification.
#' UCI Machine Learning Repository. https://doi.org/10.24432/C5WW2P.
#'
#' @usage data(glass)
#'
#' @description Impedance measurements were made at the frequencies: 15.625,
#' 31.25, 62.5, 125, 250, 500, 1000 KHz. Impedance measurements of freshly
#' excised breast tissue were made at the follwoing frequencies:
#'  15.625, 31.25, 62.5, 125, 250, 500, 1000 KHz. These measurements plotted
#'   in the (real, -imaginary) plane constitute the impedance spectrum from
#'   where the breast tissue features are computed. The dataset can be used
#'   for predicting the classification of either the original 6 classes or of
#'   4 classes by merging together the fibro-adenoma, mastopathy and glandular
#'   classes whose discrimination is not important (they cannot be accurately
#'    discriminated anyway).
#'
#'
#' @details
#' \describe{
#' \item{\code{type}}{class attribute with 7 levels \itemize{
#' 	\item{1}{building_windows_float_processed}
#'	\item{2}{building_windows_non_float_processed}
#'	\item{3}{vehicle_windows_float_processed}
#'	\item{4}{vehicle_windows_non_float_processed (none in this database)}
#'	\item{5}{containers}
#'	\item{6}{tableware}
#'	\item{7}{headlamps}
#' }}
#' \item{\code{RI}}{refractive index}
#' \item{\code{measurement}}{weight percent in corresponding oxide, as are
#' attributes 4-10)}
#' \item{\code{Mg}}{Magnesium}
#' \item{\code{Al}}{Aluminum}
#' \item{\code{Si}}{Silicon}
#' \item{\code{K}}{Potassium}
#' \item{\code{Ca}}{Calcium}
#' \item{\code{Ba}}{Barium}
#' \item{\code{Fe}}{Iron}
#' }
#' @format A data frame with 214 observations and 10 features
#'
NULL




#' @name LiverDisorders
#' @docType data
#' @aliases bupa
#' @title Liver Disorders
#' @source Liver Disorders. (1990).
#'  UCI Machine Learning Repository. https://doi.org/10.24432/C54G67.
#'
#'
#' @description The first 5 variables are all blood tests which are thought to
#' be sensitive to liver disorders that might arise from excessive alcohol
#' consumption. Each line in the dataset constitutes the record of a single
#'  male individual.
#' Important note: The 7th field (selector) has been widely misinterpreted
#' in the past as a dependent variable representing presence or absence of
#'  a liver disorder. This is incorrect [1].
#'    The 7th field was created by BUPA researchers as a train/test selector.
#'    It is not suitable as a dependent variable for classification.
#'    The dataset does not contain any variable representing presence or
#'    absence of a liver disorder. Researchers who wish to use this
#'    dataset as a classification benchmark should follow the method
#'    used in experiments by the donor (Forsyth & Rada, 1986, Machine
#'   learning: applications in expert systems and information retrieval)
#'   and others (e.g. Turney, 1995, Cost-sensitive classification:
#'   Empirical evaluation of a hybrid genetic decision tree induction
#'   algorithm), who used the 6th field (drinks), after dichotomising,
#'   as a dependent variable for classification. Because of widespread
#'   misinterpretation in the past, researchers should take care
#'   to state their method clearly.
#'
#' @details
#' \describe{
#'	\item{\code{selector}}{The response variable. field created by the BUPA
#'	researchers to split the data into train/test sets}
#'	\item{\code{mcv}}{mean corpuscular volume}
#'	\item{\code{alkphos}}{alkaline phosphotase}
#'	\item{\code{sgpt}}{alanine aminotransferase}
#'	\item{\code{sgot}}{aspartate aminotransferase}
#'	\item{\code{gammagt}}{gamma-glutamyl transpeptidase}
#'	\item{\code{drinks}}{number of half-pint equivalents of alcoholic beverages
#'	 drunk per day}
#' }
#' @usage data(liverDisorders)
#' @format A data frame with 345 observations and 7 features
NULL




#' @name maternalHealth
#' @docType data
#' @title Maternal Health Risk
#' @source Ahmed,Marzia. (2023). Maternal Health Risk. UCI Machine Learning
#' Repository. https://doi.org/10.24432/C5DP5D.
#'
#' @description Data has been collected from different hospitals, community
#' clinics, maternal health cares from the rural areas of Bangladesh
#' through the IoT based risk monitoring system.
#'
#' @details
#' \describe{
#'	\item{\code{RiskLevel}}{The response variable with 3 levels -- high,
#'	low and mid}
#'	\item{\code{Age}}{}
#'	\item{\code{SystolicBP}}{Systolic Blood Pressure}
#'	\item{\code{DiastolicBP}}{Diastolic Bloop Pressure}
#'	\item{\code{BS}}{Blood Sugar}
#'	\item{\code{BodyTemp}}{Body Temperature}
#'	\item{\code{HeartRate}}{aspartate aminotransferase}
#' }
#' @usage data(maternalHealth)
#' @format A data frame with 1014 observations and 7 features
NULL



#' @name parkinsons
#' @docType data
#' @title Maternal Health Risk
#' @source Ahmed,Marzia. (2023). Maternal Health Risk. UCI Machine Learning
#' Repository. https://doi.org/10.24432/C5DP5D.
#'
#' @description This dataset is composed of a range of biomedical voice
#' measurements from 31 people, 23 with Parkinson's disease (PD).
#' Each column in the table is a particular voice measure, and each row
#' corresponds one of 195 voice recording from these
#' individuals ("name" column). The main aim of the data is to discriminate
#' healthy people from those with PD, according to "status" column
#'  which is set to 0 for healthy and 1 for PD.
#'
#' @details
#' \describe{
#'	\item{\code{status}}{Health status of the subject (one) - Parkinson's,
#'	 (zero) - healthy}
#'	\item{\code{MDVP:Fo(Hz)}}{Average vocal fundamental frequency}
#'	\item{\code{MDVP:Fhi(Hz)}}{Maximum vocal fundamental frequency}
#'	\item{\code{MDVP:Flo(Hz)}}{Minimum vocal fundamental frequency}
#'	\item{\code{MDVP:Jitter(%),MDVP:Jitter(Abs),MDVP:RAP,MDVP:PPQ,
#'	Jitter:DDP}}{Several measures of variation in fundamental frequency}
#'	\item{\code{MDVP:Shimmer,MDVP:Shimmer(dB),Shimmer:APQ3,Shimmer:APQ5,
#'	MDVP:APQ,Shimmer:DDA}}{Several measures of variation in amplitude}
#'	\item{\code{NHR,HNR}}{Two measures of ratio of noise to tonal components
#'	in the voice}
#'	\item{\code{RPDE,D2}}{Two nonlinear dynamical complexity measures}
#'	\item{\code{DFA}}{Signal fractal scaling exponent}
#'	\item{\code{spread1,spread2,PPE}}{Three nonlinear measures of fundamental
#'	 frequency variation}
#' }
#' @usage data(parkinsons)
#' @format A data frame with 195 observations and 23 features
NULL




