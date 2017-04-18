#!/bin/bash
for i in {57302..55000..-1}
do
	nice Rscript generate_forecast.R $i
done
