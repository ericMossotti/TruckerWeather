# TruckerWeather

### ::: For adding version control to quarto project :::

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

**Follow instructions on browser popup, or possibly (as an example):**

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

### ::: Reproducing a renv project environment on another machine:::

in R console:

``` R
renv::restore()
```
