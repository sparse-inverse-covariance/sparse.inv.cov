Code for sparse.inv.cov: Fitting Very Large Sparse Gaussian Graphical Models. 

See Harri Kiiveri and Frank de Hoog, Fitting Very Large Sparse Gaussian Graphical Models:
Computational Statistics \& Data Analysis, 2012, 56:9, 2626--2636
10.1016/j.csda.2012.02.007


# To get going on petrichor

## setting up vnc

This is nlt necessary but maked life easier. On petrichor

```
module load realvnc
vnc -s
```
this is will give you a vnc server address of the form hostname:n
You then need to install a vncviewer.

This is easy on linux -- not sure on windows but must be possible

```
sudo apt install  realvnc-vnc-viewer
```

Then put the address into your vnc-viewer and you can open a graphical window on petrichor


## getting started
```
module load pandoc
module load R
module load emacs/27.2
emacs -fn 10x20 &

```
and then

```{r, eval=FALSE, echo=FALSE}
library(devtools)
install_github("https://github.com/sparse-inverse-covariance/sparse.inv.cov", build_vignettes = TRUE) 

library(sparse.inv.cov)
vignette("BMCsmokingex")
vignette(parallel_processing)

```

BMCsmokingex show you how to set up the data and parallel_processing will show you how to process it.
