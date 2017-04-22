#!/bin/bash
for i in {57826..57460..-1}
do
	nice Rscript generate_forecast.R $i
done
