# TruckerWeather

### Reproducing a renv Quarto project environment on another machine

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

Follow prompts to install R and Python packages during these steps where necessary. If doing this without an R console, you might be able to run that R code in R-code chunk(s) within the `index.qmd` file of the project.

### For Creating a MSSQL Server in Docker

``` bash
sudo docker pull mcr.microsoft.com/mssql/server:2022-latest
```

``` bash
sudo docker run -e "ACCEPT_EULA=Y" -e "MSSQL_SA_PASSWORD=MyStr@ngPassw0rd11" -p 1433:1433 --name sql1 --hostname sql1 -d mcr.microsoft.com/mssql/server:2022-latest
```

#### Creating a Database within Docker MSSQL Server

From system's bash terminal:

``` bash
sudo docker exec -it sql1 "bash"
```

``` bash
/opt/mssql-tools18/bin/sqlcmd -S localhost -U sa -P "MyStr@ngPassw0rd11" -C
```

Now that you are in SQL cmd prompt:

``` sql
CREATE DATABASE TestDB;
```

``` sql
SELECT Name
FROM sys.databases;
```

``` sql
GO
```

## Optional and Other Info: 

### For adding version control to a new quarto project in general

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
echo "# your_project_repo" >> README.md 
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

Replace `username/your_project_repo` with your info. Remember to add the .git at the end of the address.

``` bash
git branch -M main git remote add origin https://github.com/username/your_project_ repo.git 
```

``` bash
git push -u origin main
```
