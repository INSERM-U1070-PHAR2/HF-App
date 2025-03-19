[![SWH](https://archive.softwareheritage.org/badge/origin/https://github.com/INSERM-U1070-PHAR2/HF-App/)](https://archive.softwareheritage.org/browse/origin/?origin_url=https://github.com/INSERM-U1070-PHAR2/HF-App)

# HF-App
Shiny application to streamline hollow fibre experiment design.  
Test it at https://varacli.shinyapps.io/hollow_fiber_app/

# Citation
Please cite as :
Aranzana-Climent, V., Chauzy, A., Grégoire, N. & Prébonnaud, N. HF-APP. (2021). <https://github.com/INSERM-U1070-PHAR2/HF-App/>

# License
It is published under the french governement open license 
https://www.etalab.gouv.fr/licence-ouverte-open-licence/ 

# Local install

The app is built as a package and thus can be installed on your local machine 
for better performance. To do so :

```
remotes::install_github("INSERM-U1070-PHAR2/HF-App")
library(HFApp)
run_app()
```