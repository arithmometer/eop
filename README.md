# Singular Spectrum Analysis forecasting of Earth orientation parameters

The purpose of this project is [Earth orientation parameters](https://www.iers.org/IERS/EN/Science/EarthRotation/EOP.html) prediction.

We use [Singular Spectrum Analysis technique](https://www.crcpress.com/Analysis-of-Time-Series-Structure-SSA-and-Related-Techniques/Golyandina-Nekrutkin-Zhigljavsky/p/book/9781584881940) for analysis and forecasting of $x$, $y$, $LOD$, $dX$ and $dY$ time series, which are published in [Bulletin C04](http://hpiers.obspm.fr/iers/eop/eopc04/eopc04_IAU2000.62-now) of International Earth rotation and Reference systems Service (IERS).

SSA forecasts are being generated automatically using time series cross-validation for choosing parameters $L$ (window length) and $r$ (number of elementary components).

The code is written in R using [Shiny](https://shiny.rstudio.com) and [Rssa](http://github.com/asl/rssa) packages.

On average, the proposed technique generates more precise forecasts of EOP than those published by [IERS](https://www.iers.org/IERS/EN/Publications/Bulletins/bulletins.html) or [PERSAC](http://www.gao.spb.ru/english/as/persac/) (except $dX$ time series, which forecast is still competitive).

This app was created as a part of Graduation Project of Grigory Okhotnikov, Master student of Saint Petersburg State University. Scientific supervisor: Associate Professor Nina Golyandina, PhD, SPbU.
