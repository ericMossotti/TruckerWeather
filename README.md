# TruckerWeather

### Reproducing a renv project environment on another machine

First, have R and Python installed globally on your system.

In Rstudio:

Go to: `File`

Click `New Project`

Select `Version Control`

Paste the gh repo link.

in R console:

``` r
install.packages("renv")
```

``` r
renv::restore()
```

``` r
renv::snapshot()
```

Follow prompts to install R and Python packages during these steps where necessary.

### For adding version control to a new quarto project

In bash terminal:

``` bash
git init -b main 
```

``` bash
git add . 
```

``` bash
git commit -m "initial commit" 
```

``` bash
gh auth login 
```

``` bash
gh repo create
```

#### **Follow instructions on browser popup, or possibly (as an example):**

``` bash
echo "# TruckerWeather" >> README.md 
```

``` bash
git init 
```

``` bash
git add README.md 
```

``` bash
git commit -m "first commit" 
```

``` bash
git branch -M main git remote add origin https://github.com/ericMossotti/TruckerWeather.git 
```

``` bash
git push -u origin main
```
