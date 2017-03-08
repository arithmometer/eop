#!/bin/bash
for i in {57500..55000..-1}
do
	Rscript generate_forecast_given_mjd.R $i
done
