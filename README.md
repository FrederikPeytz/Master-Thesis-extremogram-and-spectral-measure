# Master-Thesis-extremogram-and-spectral-measure
Master Thesis Extremogram and spectral measure

### Extremogram

For the coding the extremogram we reuse the code made by Cribben in the r-package \texttt{extremogram}.  \texttt{x} needs to be a vector consisting of the values of the time series. \texttt{quant1} and \texttt{quant2} is the quantiles considered at lag 0 and lag $h$.  \texttt{maxlag} determines for how many lags we compute the sample extremogram.  \texttt{type} is the type of extremogram: \\
Type 1 - $P(X_h > x_h | X_0 > x_0)$ \\
Type 2 - $P(X_h < x_h | X_0 < x_0)$ \\
Type 3 - $P(X_h > x_h | X_0 < x_0)$ \\
Type 4 - $P(X_h < x_h | X_0 > x_0)$ \\
Type 5 - $P(X_h > x_{h,upper} \cup X_h < x_{h,lower} | X_0 > x_0)$ \\
Type 6 - $P(X_h > x_{h,upper} \cup X_h < x_{h,lower} | X_0 < x_0)$ \\
If \texttt{ploting} $=1$ then we construct a plot of the extremogram.  \texttt{cutoff} is the maximal value of the extremogram coefficients in the plot.  \texttt{start} is the first lag that visible in the plot.
The extremogram function used in this thesis can be seen below: 


### Stationary bootstrap confidence band

We reuse the code made by Cribben in the r-package \texttt{extremogram} again.  The variables used before are the same.  \texttt{R} is the amount of bootstrapped samples the bootstrapped confidence band is based on.  \texttt{l} is the mean block size.  \texttt{alpha} determines how significant the confidence band has to be.
The stationary bootstrap confidence band function used in this thesis can be seen below: 

### Permutation confidence band

We reuse the code made by Cribben in the r-package \texttt{extremogram}.  The variables used before are the same.  \texttt{exttype} is the extremogram type.   \texttt{type} is kept from Cribben's original code.  We just use type 3 as described in subsection \ref{section_permutation}. \texttt{m} is the amount of permutation that the confidence band is based on. 
The permutation confidence band function used in this thesis can be seen below: 

\subsubsection{Cross extremogram}
The code for the cross extremograms is very similar to the one for the extremogram.  The only difference in the input parameters is that \texttt{a} has to be a $n \times 2$ matrix consisting of the values of the two time series.  Below one can see the functions for the cross extremogram,  its stationary bootstrap confidence band function and its permutation confidence band function. 


### Return time extremogram

We reuse the code made by Cribben in the r-package \texttt{extremogram}.  The variables used before are the same.  \texttt{uplevel} is the threshold in the upper tail and \texttt{lowlevel} is the threshold in the lower tail.  if \texttt{histogram} is set $= 1$ then a return time extremogram is being made.  \texttt{type} determines what type of return time extremogram is being considered.  In this thesis we added type 4 and 5 to Cribben's original code.  Type 4 is considering absolute extremes given a positive extreme and type 5 is considering absolute extremes given a negative extreme.
\texttt{return\_type} has to either "abs",  "pos" or "neg" and determines what type of probabilities is being outputted as a vector.  Now this is considered in the set up of return times of absolute extremes,  so pos + neg = abs.
The return time extremogram functions used in this thesis can be seen below: 

### Spectral density

In this section one can find the functions used in constructing the spectral density plots in this thesis. There is also an example of the code used for the plots in the end.  Note that we use the r-package \texttt{ggplot2} for the plots.  
\texttt{no\_of\_bins} decides how many intervals we consider in the spectral density plot in $\pi$ radians. 
\texttt{extreme\_type} note the type of extremes we consider.  "one" means that one component is extreme and "both" means both components are extreme within their own time series. \texttt{lag} determines the shift in time (the amount of lags considered). 
