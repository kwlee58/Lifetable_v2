
R version 3.1.1 (2014-07-10) -- "Sock it to Me"
Copyright (C) 2014 The R Foundation for Statistical Computing
Platform: x86_64-apple-darwin13.1.0 (64-bit)

R은 자유 소프트웨어이며, 어떠한 형태의 보증없이 배포됩니다.
또한, 일정한 조건하에서 이것을 재배포 할 수 있습니다.
배포와 관련된 상세한 내용은 'license()' 또는 'licence()'을 통하여 확인할 수 있습니다.

R은 많은 기여자들이 참여하는 공동프로젝트입니다.
'contributors()'라고 입력하시면 이에 대한 더 많은 정보를 확인하실 수 있습니다.
그리고, R 또는 R 패키지들을 출판물에 인용하는 방법에 대해서는 'citation()'을 통해 확인하시길 부탁드립니다.

'demo()'를 입력하신다면 몇가지 데모를 보실 수 있으며, 'help()'를 입력하시면 온라인 도움말을 이용하실 수 있습니다.
또한, 'help.start()'의 입력을 통하여 HTML 브라우저에 의한 도움말을 사용하실수 있습니다
R의 종료를 원하시면 'q()'을 입력해주세요.

[R.app GUI 1.65 (6784) x86_64-apple-darwin13.1.0]

[Workspace restored from /Users/coop2711/.RData]
[History restored from /Users/coop2711/.Rapp.history]

R > ls(0)
다음에 오류가 있습니다as.environment(pos) : 'pos' 인자가 잘못되었습니다
R > ls()
 [1] "a"                 "cx"                "gdp"               "gdp_capita"        "grdp_gw"           "grdp_gw_capita"   
 [7] "grdp_yg"           "jobs"              "jobs.bus"          "jobs.kr"           "jobs.specialty"    "jobs.village"     
[13] "p.x"               "pop_gw"            "rate.gdp"          "rate.grdp"         "rate.grdp_yg"      "revenue"          
[19] "revenue.bus"       "revenue.specialty" "v"                 "x"                 "year"             
R > setwd("./Documents/통계분석사례연구/R.WD/")
R > load("plot.rda")
R > ls()
 [1] "a"                 "cx"                "gdp"               "gdp_capita"        "grdp_gw"           "grdp_gw_capita"   
 [7] "grdp_yg"           "jobs"              "jobs.bus"          "jobs.kr"           "jobs.specialty"    "jobs.village"     
[13] "p"                 "p.x"               "pop_gw"            "rate.gdp"          "rate.grdp"         "rate.grdp_yg"     
[19] "revenue"           "revenue.bus"       "revenue.specialty" "v"                 "x"                 "year"             
R > qplot(date, unemploy, data=economics, geom="line",xlab="",ylab="No. unemployed (1000s)")
에러: 함수 "qplot"를 찾을 수 없습니다
R > library(ggplot2)
R > qplot(date, unemploy, data=economics, geom="line",xlab="",ylab="No. unemployed (1000s)")
R > par(family="AppleGothic")
R > qplot(date, unemploy, data=economics, geom="line",xlab="",ylab="실업자수")
R > qplot(date, unemploy, data=economics, geom="line",xlab="",ylab="실업자수",family="AppleGothic")
R > plot(0,main="한글")
R > plot(0,main="한글",xlab="한글")
R > qplot
function (x, y = NULL, ..., data, facets = NULL, margins = FALSE, 
    geom = "auto", stat = list(NULL), position = list(NULL), 
    xlim = c(NA, NA), ylim = c(NA, NA), log = "", main = NULL, 
    xlab = deparse(substitute(x)), ylab = deparse(substitute(y)), 
    asp = NA) 
{
    argnames <- names(as.list(match.call(expand.dots = FALSE)[-1]))
    arguments <- as.list(match.call()[-1])
    aesthetics <- compact(arguments[.all_aesthetics])
    aesthetics <- aesthetics[!is.constant(aesthetics)]
    aes_names <- names(aesthetics)
    aesthetics <- rename_aes(aesthetics)
    class(aesthetics) <- "uneval"
    if (missing(data)) {
        data <- data.frame()
        facetvars <- all.vars(facets)
        facetvars <- facetvars[facetvars != "."]
        names(facetvars) <- facetvars
        facetsdf <- as.data.frame(lapply(facetvars, get))
        if (nrow(facetsdf)) 
            data <- facetsdf
    }
    if ("auto" %in% geom) {
        if (stat == "qq" || "sample" %in% aes_names) {
            geom[geom == "auto"] <- "point"
            stat <- "qq"
        }
        else if (missing(y)) {
            geom[geom == "auto"] <- "histogram"
            if (is.null(ylab)) 
                ylab <- "count"
        }
        else {
            if (missing(x)) {
                aesthetics$x <- bquote(seq_along(.(y)), aesthetics)
            }
            geom[geom == "auto"] <- "point"
        }
    }
    env <- parent.frame()
    p <- ggplot(data, aesthetics, environment = env)
    if (is.null(facets)) {
        p <- p + facet_null()
    }
    else if (is.formula(facets) && length(facets) == 2) {
        p <- p + facet_wrap(facets)
    }
    else {
        p <- p + facet_grid(facets = deparse(facets), margins = margins)
    }
    if (!is.null(main)) 
        p <- p + ggtitle(main)
    if (is.proto(position)) 
        position <- list(position)
    mapply(function(g, s, ps) {
        if (is.character(g)) 
            g <- Geom$find(g)
        if (is.character(s)) 
            s <- Stat$find(s)
        if (is.character(ps)) 
            ps <- Position$find(ps)
        params <- arguments[setdiff(names(arguments), c(aes_names, 
            argnames))]
        params <- lapply(params, eval, parent.frame(3))
        p <<- p + layer(geom = g, stat = s, geom_params = params, 
            stat_params = params, position = ps)
    }, geom, stat, position)
    logv <- function(var) var %in% strsplit(log, "")[[1]]
    if (logv("x")) 
        p <- p + scale_x_log10()
    if (logv("y")) 
        p <- p + scale_y_log10()
    if (!is.na(asp)) 
        p <- p + theme(aspect.ratio = asp)
    if (!missing(xlab)) 
        p <- p + xlab(xlab)
    if (!missing(ylab)) 
        p <- p + ylab(ylab)
    if (!missing(xlim)) 
        p <- p + xlim(xlim)
    if (!missing(ylim)) 
        p <- p + ylim(ylim)
    p
}
<environment: namespace:ggplot2>
R > par()$family
[1] "sans"
R > g<-qplot(date, unemploy, data=economics, geom="line",xlab="",ylab="",family="AppleGothic")
R > g+labs(title="70년대 미국의 실업률",x="연도",y="실업률")
R > par(family="AppleGothic")
R > g+labs(title="70년대 미국의 실업률",x="연도",y="실업률")
R > g+labs(title="70년대 미국의 실업률",x="연도",y="실업률",family="AppleGothic")
R > g+labs(title="70년대 미국의 실업률",x="연도",y="실업률",family="AppleMyungjo")
R > theURL<-"http://jaredlander.com/data/Tomato%20First.csv"
R > tomato<-read.table(file=theUrl,header=TRUE,sep=",")
다음에 오류가 있습니다read.table(file = theUrl, header = TRUE, sep = ",") : 
  객체 'theUrl'를 찾을 수 없습니다
R > tomato<-read.table(file=theURL,header=TRUE,sep=",")
R > str(tomato)
'data.frame':	16 obs. of  11 variables:
 $ Round        : int  1 1 1 1 2 2 2 2 3 3 ...
 $ Tomato       : chr  "Simpson SM" "Tuttorosso (blue)" "Tuttorosso (green)" "La Fede SM DOP" ...
 $ Price        : num  3.99 2.99 0.99 3.99 5.49 4.99 3.99 3.99 4.53 NA ...
 $ Source       : chr  "Whole Foods" "Pioneer" "Pioneer" "Shop Rite" ...
 $ Sweet        : num  2.8 3.3 2.8 2.6 3.3 3.2 2.6 2.1 3.4 2.6 ...
 $ Acid         : num  2.8 2.8 2.6 2.8 3.1 2.9 2.8 2.7 3.3 2.9 ...
 $ Color        : num  3.7 3.4 3.3 3 2.9 2.9 3.6 3.1 4.1 3.4 ...
 $ Texture      : num  3.4 3 2.8 2.3 2.8 3.1 3.4 2.4 3.2 3.3 ...
 $ Overall      : num  3.4 2.9 2.9 2.8 3.1 2.9 2.6 2.2 3.7 2.9 ...
 $ Avg.of.Totals: num  16.1 15.3 14.3 13.4 14.4 15.5 14.7 12.6 17.8 15.3 ...
 $ Total.of.Avg : num  16.1 15.3 14.3 13.4 15.2 15.1 14.9 12.5 17.7 15.2 ...
R > tomato
   Round                   Tomato Price           Source Sweet Acid Color Texture Overall Avg.of.Totals Total.of.Avg
1      1               Simpson SM  3.99      Whole Foods   2.8  2.8   3.7     3.4     3.4          16.1         16.1
2      1        Tuttorosso (blue)  2.99          Pioneer   3.3  2.8   3.4     3.0     2.9          15.3         15.3
3      1       Tuttorosso (green)  0.99          Pioneer   2.8  2.6   3.3     2.8     2.9          14.3         14.3
4      1           La Fede SM DOP  3.99        Shop Rite   2.6  2.8   3.0     2.3     2.8          13.4         13.4
5      2             Cento SM DOP  5.49       D Agostino   3.3  3.1   2.9     2.8     3.1          14.4         15.2
6      2            Cento Organic  4.99       D Agostino   3.2  2.9   2.9     3.1     2.9          15.5         15.1
7      2              La Valle SM  3.99        Shop Rite   2.6  2.8   3.6     3.4     2.6          14.7         14.9
8      2          La Valle SM DOP  3.99           Faicos   2.1  2.7   3.1     2.4     2.2          12.6         12.5
9      3   Stanislaus Alta Cucina  4.53 Restaurant Depot   3.4  3.3   4.1     3.2     3.7          17.8         17.7
10     3                     Ciao    NA            Other   2.6  2.9   3.4     3.3     2.9          15.3         15.2
11     3       Scotts Backyard SM  0.00       Home Grown   1.6  2.9   3.1     2.4     1.9          11.9         11.9
12     3 Di Casa Barone (organic) 12.80           Eataly   1.7  3.6   3.8     2.3     1.4          12.7         12.7
13     4         Trader Joes Plum  1.49      Trader Joes   3.4  3.3   4.0     3.6     3.9          17.8         18.2
14     4          365 Whole Foods  1.49      Whole Foods   2.8  2.7   3.4     3.1     3.1          14.8         15.2
15     4        Muir Glen Organic  3.19      Whole Foods   2.9  2.8   2.7     3.2     3.1          14.8         14.7
16     4        Bionature Organic  3.39      Whole Foods   2.4  3.3   3.4     3.2     2.8          15.1         15.2
R > tomato2<-read.table(file=theURL,header=TRUE)
다음에 오류가 있습니다scan(file, what, nmax, sep, dec, quote, skip, nlines, na.strings,  : 
  라인 1는 5개의 구성요소들을 가지고 있지 않습니다
R > ?read.table
starting httpd help server ... done
R > tomato2<-read.table(file=theURL,header=TRUE,sep=",")
R > library(RODBC)
다음에 오류가 있습니다library(RODBC) : ‘RODBC’이라고 불리는 패키지가 없습니다
R > install.packages("RODBC")

   package ‘RODBC’ is available as a source package but not as a binary

경고메시지:
package ‘RODBC’ is not available (for R version 3.1.1) 
URL 'http://cran.nexr.com/src/contrib/RODBC_1.3-10.tar.gz'을 시도합니다
Content type 'application/x-gzip' length 1157263 bytes (1.1 Mb)
URL을 열었습니다
==================================================
downloaded 1.1 Mb

* installing *source* package ‘RODBC’ ...
** 패키지 ‘RODBC’는 성공적으로 압축해제되었고, MD5 sums 이 확인되었습니다
checking for gcc... clang
checking for C compiler default output file name... a.out
checking whether the C compiler works... yes
checking whether we are cross compiling... no
checking for suffix of executables... checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ANSI C... none needed
checking how to run the C preprocessor... clang -E
checking for egrep... grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking sql.h usability... no
checking sql.h presence... no
checking for sql.h... no
checking sqlext.h usability... no
checking sqlext.h presence... no
checking for sqlext.h... no
configure: error: "ODBC headers sql.h and sqlext.h not found"
ERROR: configuration failed for package ‘RODBC’
* removing ‘/Library/Frameworks/R.framework/Versions/3.1/Resources/library/RODBC’

다운로드한 소스 패키지들은 다음의 위치에 있습니다
	‘/private/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T/RtmpyN9XPt/downloaded_packages’
R > ?install.packages
경고메시지:
In install.packages(c("RODBC"), lib = "/Library/Frameworks/R.framework/Resources/library/",  :
  패키지 ‘RODBC’의 설치가 0이 아닌 종료상태를 가졌습니다
R > require(RODBC)
필요한 패키지를 로딩중입니다: RODBC
경고메시지:
In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  ‘RODBC’이라고 불리는 패키지가 없습니다
URL 'http://cran.nexr.com/src/contrib/RODBC_1.3-10.tar.gz'을 시도합니다
Content type 'application/x-gzip' length 1157263 bytes (1.1 Mb)
URL을 열었습니다
==================================================
downloaded 1.1 Mb

* installing *source* package ‘RODBC’ ...
** 패키지 ‘RODBC’는 성공적으로 압축해제되었고, MD5 sums 이 확인되었습니다
checking for gcc... clang
checking for C compiler default output file name... a.out
checking whether the C compiler works... yes
checking whether we are cross compiling... no
checking for suffix of executables... checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ANSI C... none needed
checking how to run the C preprocessor... clang -E
checking for egrep... grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking sql.h usability... no
checking sql.h presence... no
checking for sql.h... no
checking sqlext.h usability... no
checking sqlext.h presence... no
checking for sqlext.h... no
configure: error: "ODBC headers sql.h and sqlext.h not found"
ERROR: configuration failed for package ‘RODBC’
* removing ‘/Library/Frameworks/R.framework/Versions/3.1/Resources/library/RODBC’

다운로드한 소스 패키지들은 다음의 위치에 있습니다
	‘/private/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T/RtmpyN9XPt/downloaded_packages’
R > getwd()
[1] "/Users/coop2711/Documents/통계분석사례연구/R.WD"
경고메시지:
In install.packages(c("RODBC"), lib = "/Library/Frameworks/R.framework/Resources/library/",  :
  패키지 ‘RODBC’의 설치가 0이 아닌 종료상태를 가졌습니다
R > lifetable<-read.table(file="../national_lifetable.csv",header=TRUE,sep=",")
다음에 오류가 있습니다make.names(col.names, unique = TRUE) : 
  '<b0><a2><bc><bc><ba><b0>'에서 유효하지 않은 멀티바이트 문자열이 있습니다
R > lifetable<-read.table(file="../national_lifetable.csv",header=TRUE,sep=",")
다음에 오류가 있습니다make.names(col.names, unique = TRUE) : 
  '<b0><a2><bc><bc><ba><b0>'에서 유효하지 않은 멀티바이트 문자열이 있습니다
R > lifetable<-read.table(file="../national_lifetable.csv",header=TRUE,sep=",")
다음에 오류가 있습니다make.names(col.names, unique = TRUE) : 
  '<b0><a2><bc><bc><ba><b0>'에서 유효하지 않은 멀티바이트 문자열이 있습니다
R > lifetable<-read.table(file="../national_lifetable2.csv",header=TRUE,sep=",")
R > lifetable[1:10,]
   각세별 기대여명.전체. 기대여명.남자. 기대여명.여자. 사망확률.전체. 사망확률.남자. 사망확률.여자. 생존자.전체. 생존자.남자.
1     0세          81.44          77.95          84.64        0.00291        0.00314        0.00267       100000       100000
2     1세          80.68          77.19          83.86        0.00031        0.00035        0.00027        99709        99686
3     2세          79.71          76.22          82.89        0.00022        0.00026        0.00019        99678        99651
4     3세          78.72          75.24          81.90        0.00016        0.00019        0.00014        99656        99625
5     4세          77.74          74.25          80.91        0.00013        0.00015        0.00012        99639        99607
6     5세          76.75          73.27          79.92        0.00012        0.00014        0.00011        99626        99592
7     6세          75.76          72.28          78.93        0.00012        0.00014        0.00011        99614        99579
8     7세          74.77          71.28          77.94        0.00011        0.00013        0.00010        99602        99565
9     8세          73.78          70.29          76.95        0.00011        0.00012        0.00010        99590        99553
10    9세          72.78          69.30          75.95        0.00010        0.00010        0.00009        99580        99541
   생존자.여자.  X
1        100000 NA
2         99733 NA
3         99706 NA
4         99687 NA
5         99674 NA
6         99662 NA
7         99651 NA
8         99640 NA
9         99630 NA
10        99621 NA
R > lifetable[1:9,]
  각세별 기대여명.전체. 기대여명.남자. 기대여명.여자. 사망확률.전체. 사망확률.남자. 사망확률.여자. 생존자.전체. 생존자.남자.
1    0세          81.44          77.95          84.64        0.00291        0.00314        0.00267       100000       100000
2    1세          80.68          77.19          83.86        0.00031        0.00035        0.00027        99709        99686
3    2세          79.71          76.22          82.89        0.00022        0.00026        0.00019        99678        99651
4    3세          78.72          75.24          81.90        0.00016        0.00019        0.00014        99656        99625
5    4세          77.74          74.25          80.91        0.00013        0.00015        0.00012        99639        99607
6    5세          76.75          73.27          79.92        0.00012        0.00014        0.00011        99626        99592
7    6세          75.76          72.28          78.93        0.00012        0.00014        0.00011        99614        99579
8    7세          74.77          71.28          77.94        0.00011        0.00013        0.00010        99602        99565
9    8세          73.78          70.29          76.95        0.00011        0.00012        0.00010        99590        99553
  생존자.여자.  X
1       100000 NA
2        99733 NA
3        99706 NA
4        99687 NA
5        99674 NA
6        99662 NA
7        99651 NA
8        99640 NA
9        99630 NA
R > str(lifetable)
'data.frame':	101 obs. of  11 variables:
 $ 각세별        : chr  "0세" "1세" "2세" "3세" ...
 $ 기대여명.전체.: num  81.4 80.7 79.7 78.7 77.7 ...
 $ 기대여명.남자.: num  78 77.2 76.2 75.2 74.2 ...
 $ 기대여명.여자.: num  84.6 83.9 82.9 81.9 80.9 ...
 $ 사망확률.전체.: num  0.00291 0.00031 0.00022 0.00016 0.00013 0.00012 0.00012 0.00011 0.00011 0.0001 ...
 $ 사망확률.남자.: num  0.00314 0.00035 0.00026 0.00019 0.00015 0.00014 0.00014 0.00013 0.00012 0.0001 ...
 $ 사망확률.여자.: num  0.00267 0.00027 0.00019 0.00014 0.00012 0.00011 0.00011 0.0001 0.0001 0.00009 ...
 $ 생존자.전체.  : int  100000 99709 99678 99656 99639 99626 99614 99602 99590 99580 ...
 $ 생존자.남자.  : int  100000 99686 99651 99625 99607 99592 99579 99565 99553 99541 ...
 $ 생존자.여자.  : int  100000 99733 99706 99687 99674 99662 99651 99640 99630 99621 ...
 $ X             : logi  NA NA NA NA NA NA ...
R > install.packages("RODBC",type="source")
URL 'http://cran.nexr.com/src/contrib/RODBC_1.3-10.tar.gz'을 시도합니다
Content type 'application/x-gzip' length 1157263 bytes (1.1 Mb)
URL을 열었습니다
==================================================
downloaded 1.1 Mb

* installing *source* package ‘RODBC’ ...
** 패키지 ‘RODBC’는 성공적으로 압축해제되었고, MD5 sums 이 확인되었습니다
checking for gcc... clang
checking for C compiler default output file name... a.out
checking whether the C compiler works... yes
checking whether we are cross compiling... no
checking for suffix of executables... checking for suffix of object files... o
checking whether we are using the GNU C compiler... yes
checking whether clang accepts -g... yes
checking for clang option to accept ANSI C... none needed
checking how to run the C preprocessor... clang -E
checking for egrep... grep -E
checking for ANSI C header files... rm: conftest.dSYM: is a directory
rm: conftest.dSYM: is a directory
yes
checking for sys/types.h... yes
checking for sys/stat.h... yes
checking for stdlib.h... yes
checking for string.h... yes
checking for memory.h... yes
checking for strings.h... yes
checking for inttypes.h... yes
checking for stdint.h... yes
checking for unistd.h... yes
checking sql.h usability... no
checking sql.h presence... no
checking for sql.h... no
checking sqlext.h usability... no
checking sqlext.h presence... no
checking for sqlext.h... no
configure: error: "ODBC headers sql.h and sqlext.h not found"
ERROR: configuration failed for package ‘RODBC’
* removing ‘/Library/Frameworks/R.framework/Versions/3.1/Resources/library/RODBC’

다운로드한 소스 패키지들은 다음의 위치에 있습니다
	‘/private/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T/RtmpyN9XPt/downloaded_packages’
경고메시지:
In install.packages("RODBC", type = "source") :
  패키지 ‘RODBC’의 설치가 0이 아닌 종료상태를 가졌습니다
R > lifetable<-read.table(file="../national_lifetable2.csv",header=TRUE,sep=",")
R > str(lifetable)
'data.frame':	101 obs. of  11 variables:
 $ 각세별        : chr  "0세" "1세" "2세" "3세" ...
 $ 기대여명.전체.: num  81.4 80.7 79.7 78.7 77.7 ...
 $ 기대여명.남자.: num  78 77.2 76.2 75.2 74.2 ...
 $ 기대여명.여자.: num  84.6 83.9 82.9 81.9 80.9 ...
 $ 사망확률.전체.: num  0.00291 0.00031 0.00022 0.00016 0.00013 0.00012 0.00012 0.00011 0.00011 0.0001 ...
 $ 사망확률.남자.: num  0.00314 0.00035 0.00026 0.00019 0.00015 0.00014 0.00014 0.00013 0.00012 0.0001 ...
 $ 사망확률.여자.: num  0.00267 0.00027 0.00019 0.00014 0.00012 0.00011 0.00011 0.0001 0.0001 0.00009 ...
 $ 생존자.전체.  : int  100000 99709 99678 99656 99639 99626 99614 99602 99590 99580 ...
 $ 생존자.남자.  : int  100000 99686 99651 99625 99607 99592 99579 99565 99553 99541 ...
 $ 생존자.여자.  : int  100000 99733 99706 99687 99674 99662 99651 99640 99630 99621 ...
 $ X             : logi  NA NA NA NA NA NA ...
R > say.hello<-function()
+ {}
R > edit(say.hello)
다음에 오류가 있습니다.External2(C_edit, name, file, title, editor) : 
  예기치 않은 ')'입니다가 3번째 라인에서 발생했습니다
 다음과 같은 명령어를 이용하여 복구해보세요
 x <- edit()
 
추가정보: 경고메시지:
In (function (con = stdin(), n = -1L, ok = TRUE, warn = TRUE, encoding = "unknown",  :
  '/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T//RtmpyN9XPt/59ad19e5c28e.R'에서 불완전한 마지막 행이 발견되었습니다
R > x<-edit()
다음에 오류가 있습니다.External2(C_edit, name, file, title, editor) : 
  예기치 않은 입력의 끝입니다입니다가 4번째 라인에서 발생했습니다
 다음과 같은 명령어를 이용하여 복구해보세요
 x <- edit()
 
추가정보: 경고메시지:
In (function (con = stdin(), n = -1L, ok = TRUE, warn = TRUE, encoding = "unknown",  :
  '/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T//RtmpyN9XPt/59ad19e5c28e.R'에서 불완전한 마지막 행이 발견되었습니다
R > say.hello
function()
{}
R > say.hello()
NULL
R > edit(say.hello)
function()
{
	print("Hello, World!")
}
R > ?eidt
No documentation for ‘eidt’ in specified packages and libraries:
you could try ‘??eidt’
R > ?edit
R > say.hello
function()
{}
R > fix(say.hello)
R > say.hello()
[1] "Heelo, World!"
R > fix(say.hello)
R > say.hello()
[1] "Hello, World!"
R > fix(hello.person)
R > hello.person("기원")
[1] "Hello 기원"
R > fix(hello.person)
R > hello.person("기원", "이")
[1] "Hello 기원 이"
R > hello.person(first="기원", last="이")
[1] "Hello 기원 이"
R > hello.person(last="기원", first="이")
[1] "Hello 이 기원"
R > ?library
R > library(help=ggplot2)
R > library(help=XML)
다음에 오류가 있습니다find.package(pkgName, lib.loc, verbose = verbose) : 
  ‘XML’이라고 불리는 패키지가 없습니다
R > library(help=MASS)
R > ?write
R > Sys.time()
[1] "2014-09-09 18:38:42 KST"
R > data()
R > data(package="ggplot2")
R > ls()
 [1] "a"                 "cx"                "g"                 "gdp"               "gdp_capita"        "grdp_gw"          
 [7] "grdp_gw_capita"    "grdp_yg"           "hello.person"      "jobs"              "jobs.bus"          "jobs.kr"          
[13] "jobs.specialty"    "jobs.village"      "lifetable"         "p"                 "p.x"               "pop_gw"           
[19] "rate.gdp"          "rate.grdp"         "rate.grdp_yg"      "revenue"           "revenue.bus"       "revenue.specialty"
[25] "say.hello"         "theURL"            "tomato"            "tomato2"           "v"                 "x"                
[31] "year"             
R > lifetable
       각세별 기대여명.전체. 기대여명.남자. 기대여명.여자. 사망확률.전체. 사망확률.남자. 사망확률.여자. 생존자.전체.
1         0세          81.44          77.95          84.64        0.00291        0.00314        0.00267       100000
2         1세          80.68          77.19          83.86        0.00031        0.00035        0.00027        99709
3         2세          79.71          76.22          82.89        0.00022        0.00026        0.00019        99678
4         3세          78.72          75.24          81.90        0.00016        0.00019        0.00014        99656
5         4세          77.74          74.25          80.91        0.00013        0.00015        0.00012        99639
6         5세          76.75          73.27          79.92        0.00012        0.00014        0.00011        99626
7         6세          75.76          72.28          78.93        0.00012        0.00014        0.00011        99614
8         7세          74.77          71.28          77.94        0.00011        0.00013        0.00010        99602
9         8세          73.78          70.29          76.95        0.00011        0.00012        0.00010        99590
10        9세          72.78          69.30          75.95        0.00010        0.00010        0.00009        99580
11       10세          71.79          68.31          74.96        0.00009        0.00010        0.00009        99570
12       11세          70.80          67.32          73.97        0.00009        0.00010        0.00007        99561
13       12세          69.80          66.32          72.97        0.00009        0.00012        0.00006        99552
14       13세          68.81          65.33          71.98        0.00011        0.00015        0.00008        99542
15       14세          67.82          64.34          70.98        0.00015        0.00017        0.00012        99531
16       15세          66.83          63.35          69.99        0.00019        0.00022        0.00015        99517
17       16세          65.84          62.36          69.00        0.00023        0.00027        0.00018        99498
18       17세          64.85          61.38          68.01        0.00026        0.00033        0.00018        99475
19       18세          63.87          60.40          67.03        0.00029        0.00038        0.00019        99449
20       19세          62.89          59.42          66.04        0.00031        0.00042        0.00019        99420
21       20세          61.91          58.45          65.05        0.00033        0.00044        0.00021        99389
22       21세          60.93          57.47          64.07        0.00036        0.00047        0.00024        99356
23       22세          59.95          56.50          63.08        0.00039        0.00050        0.00026        99320
24       23세          58.97          55.53          62.10        0.00042        0.00054        0.00028        99282
25       24세          58.00          54.56          61.11        0.00045        0.00058        0.00030        99240
26       25세          57.02          53.59          60.13        0.00047        0.00061        0.00032        99196
27       26세          56.05          52.62          59.15        0.00049        0.00063        0.00033        99149
28       27세          55.08          51.66          58.17        0.00050        0.00064        0.00035        99101
29       28세          54.11          50.69          57.19        0.00053        0.00066        0.00039        99051
30       29세          53.13          49.72          56.21        0.00056        0.00068        0.00043        98998
31       30세          52.16          48.76          55.24        0.00060        0.00073        0.00047        98943
32       31세          51.19          47.79          54.26        0.00065        0.00080        0.00050        98883
33       32세          50.23          46.83          53.29        0.00069        0.00086        0.00052        98819
34       33세          49.26          45.87          52.32        0.00073        0.00091        0.00053        98750
35       34세          48.30          44.91          51.35        0.00076        0.00096        0.00054        98678
36       35세          47.33          43.95          50.37        0.00079        0.00102        0.00055        98604
37       36세          46.37          43.00          49.40        0.00084        0.00110        0.00057        98526
38       37세          45.41          42.04          48.43        0.00090        0.00119        0.00059        98443
39       38세          44.45          41.09          47.46        0.00096        0.00129        0.00063        98355
40       39세          43.49          40.15          46.49        0.00104        0.00139        0.00067        98260
41       40세          42.54          39.20          45.52        0.00112        0.00150        0.00073        98158
42       41세          41.58          38.26          44.55        0.00123        0.00164        0.00081        98048
43       42세          40.64          37.32          43.59        0.00136        0.00183        0.00089        97927
44       43세          39.69          36.39          42.62        0.00152        0.00207        0.00096        97793
45       44세          38.75          35.46          41.66        0.00170        0.00234        0.00103        97644
46       45세          37.81          34.55          40.71        0.00187        0.00261        0.00109        97478
47       46세          36.88          33.63          39.75        0.00203        0.00286        0.00116        97296
48       47세          35.96          32.73          38.80        0.00218        0.00311        0.00122        97099
49       48세          35.04          31.83          37.84        0.00236        0.00340        0.00128        96887
50       49세          34.12          30.94          36.89        0.00257        0.00374        0.00135        96658
51       50세          33.20          30.05          35.94        0.00280        0.00411        0.00145        96409
52       51세          32.30          29.17          34.99        0.00303        0.00448        0.00157        96139
53       52세          31.39          28.30          34.05        0.00327        0.00485        0.00169        95847
54       53세          30.49          27.44          33.10        0.00351        0.00521        0.00181        95534
55       54세          29.60          26.58          32.16        0.00377        0.00561        0.00191        95199
56       55세          28.71          25.73          31.22        0.00402        0.00603        0.00199        94840
57       56세          27.82          24.88          30.28        0.00427        0.00646        0.00211        94458
58       57세          26.94          24.04          29.35        0.00454        0.00690        0.00227        94054
59       58세          26.06          23.20          28.41        0.00487        0.00740        0.00246        93627
60       59세          25.19          22.37          27.48        0.00528        0.00798        0.00268        93171
61       60세          24.32          21.55          26.55        0.00575        0.00866        0.00291        92679
62       61세          23.46          20.73          25.63        0.00626        0.00938        0.00318        92146
63       62세          22.60          19.92          24.71        0.00684        0.01018        0.00351        91570
64       63세          21.75          19.12          23.80        0.00741        0.01099        0.00386        90944
65       64세          20.91          18.33          22.89        0.00800        0.01184        0.00425        90270
66       65세          20.08          17.54          21.98        0.00859        0.01275        0.00469        89548
67       66세          19.25          16.76          21.08        0.00926        0.01380        0.00521        88779
68       67세          18.42          15.99          20.19        0.01026        0.01527        0.00592        87957
69       68세          17.61          15.23          19.31        0.01161        0.01726        0.00682        87055
70       69세          16.81          14.49          18.44        0.01329        0.01966        0.00793        86044
71       70세          16.03          13.77          17.58        0.01508        0.02222        0.00915        84900
72       71세          15.27          13.07          16.74        0.01687        0.02477        0.01047        83620
73       72세          14.52          12.39          15.91        0.01877        0.02754        0.01193        82209
74       73세          13.79          11.73          15.10        0.02097        0.03068        0.01361        80666
75       74세          13.07          11.08          14.30        0.02367        0.03451        0.01559        78975
76       75세          12.38          10.46          13.52        0.02675        0.03885        0.01791        77106
77       76세          11.70           9.86          12.75        0.03004        0.04339        0.02063        75043
78       77세          11.05           9.29          12.01        0.03368        0.04822        0.02394        72789
79       78세          10.42           8.73          11.29        0.03775        0.05358        0.02775        70338
80       79세           9.81           8.20          10.60        0.04242        0.05977        0.03219        67683
81       80세           9.22           7.69           9.94        0.04782        0.06709        0.03727        64812
82       81세           8.66           7.20           9.30        0.05386        0.07504        0.04294        61712
83       82세           8.12           6.75           8.70        0.06027        0.08347        0.04917        58388
84       83세           7.61           6.32           8.12        0.06736        0.09255        0.05624        54869
85       84세           7.12           5.91           7.58        0.07561        0.10316        0.06436        51173
86       85세           6.66           5.53           7.06        0.08444        0.11400        0.07319        47304
87       86세           6.23           5.18           6.58        0.09399        0.12552        0.08284        43310
88       87세           5.83           4.85           6.13        0.10428        0.13771        0.09333        39239
89       88세           5.45           4.55           5.71        0.11531        0.15055        0.10467        35147
90       89세           5.09           4.27           5.32        0.12709        0.16399        0.11686        31094
91       90세           4.76           4.00           4.96        0.13962        0.17799        0.12985        27142
92       91세           4.45           3.76           4.62        0.15287        0.19248        0.14364        23353
93       92세           4.17           3.54           4.31        0.16683        0.20741        0.15816        19783
94       93세           3.90           3.34           4.03        0.18146        0.22270        0.17334        16483
95       94세           3.65           3.15           3.77        0.19673        0.23825        0.18912        13492
96       95세           3.43           2.98           3.53        0.21258        0.25397        0.20538        10837
97       96세           3.22           2.82           3.32        0.22894        0.26975        0.22201         8534
98       97세           3.02           2.68           3.12        0.24576        0.28548        0.23890         6580
99       98세           2.85           2.55           2.94        0.26295        0.30105        0.25588         4963
100      99세           2.68           2.44           2.78        0.28041        0.31633        0.27282         3658
101 100세이상           2.53           2.33           2.64        1.00000        1.00000        1.00000         2632
    생존자.남자. 생존자.여자.  X
1         100000       100000 NA
2          99686        99733 NA
3          99651        99706 NA
4          99625        99687 NA
5          99607        99674 NA
6          99592        99662 NA
7          99579        99651 NA
8          99565        99640 NA
9          99553        99630 NA
10         99541        99621 NA
11         99531        99611 NA
12         99521        99603 NA
13         99511        99595 NA
14         99499        99589 NA
15         99484        99581 NA
16         99467        99570 NA
17         99445        99554 NA
18         99418        99537 NA
19         99385        99518 NA
20         99347        99500 NA
21         99306        99480 NA
22         99262        99459 NA
23         99216        99435 NA
24         99166        99409 NA
25         99113        99381 NA
26         99055        99351 NA
27         98994        99319 NA
28         98932        99286 NA
29         98868        99251 NA
30         98803        99212 NA
31         98736        99170 NA
32         98663        99123 NA
33         98585        99074 NA
34         98500        99022 NA
35         98410        98969 NA
36         98316        98916 NA
37         98215        98861 NA
38         98107        98806 NA
39         97990        98747 NA
40         97864        98685 NA
41         97727        98619 NA
42         97581        98546 NA
43         97421        98467 NA
44         97243        98379 NA
45         97042        98285 NA
46         96815        98184 NA
47         96562        98076 NA
48         96286        97963 NA
49         95986        97843 NA
50         95660        97717 NA
51         95303        97585 NA
52         94911        97444 NA
53         94486        97291 NA
54         94028        97127 NA
55         93538        96951 NA
56         93013        96766 NA
57         92452        96573 NA
58         91855        96369 NA
59         91221        96151 NA
60         90546        95914 NA
61         89823        95657 NA
62         89046        95379 NA
63         88211        95075 NA
64         87312        94742 NA
65         86353        94376 NA
66         85330        93974 NA
67         84243        93534 NA
68         83080        93046 NA
69         81811        92496 NA
70         80399        91865 NA
71         78818        91136 NA
72         77067        90302 NA
73         75158        89357 NA
74         73088        88291 NA
75         70846        87089 NA
76         68401        85731 NA
77         65744        84195 NA
78         62891        82458 NA
79         59858        80484 NA
80         56650        78251 NA
81         53265        75732 NA
82         49691        72910 NA
83         45962        69779 NA
84         42125        66348 NA
85         38227        62617 NA
86         34284        58587 NA
87         30375        54299 NA
88         26563        49801 NA
89         22905        45153 NA
90         19456        40426 NA
91         16266        35702 NA
92         13371        31066 NA
93         10797        26604 NA
94          8558        22396 NA
95          6652        18514 NA
96          5067        15013 NA
97          3780        11930 NA
98          2760         9281 NA
99          1972         7064 NA
100         1379         5256 NA
101          943         3822 NA
R > str(lifetable)
'data.frame':	101 obs. of  11 variables:
 $ 각세별        : chr  "0세" "1세" "2세" "3세" ...
 $ 기대여명.전체.: num  81.4 80.7 79.7 78.7 77.7 ...
 $ 기대여명.남자.: num  78 77.2 76.2 75.2 74.2 ...
 $ 기대여명.여자.: num  84.6 83.9 82.9 81.9 80.9 ...
 $ 사망확률.전체.: num  0.00291 0.00031 0.00022 0.00016 0.00013 0.00012 0.00012 0.00011 0.00011 0.0001 ...
 $ 사망확률.남자.: num  0.00314 0.00035 0.00026 0.00019 0.00015 0.00014 0.00014 0.00013 0.00012 0.0001 ...
 $ 사망확률.여자.: num  0.00267 0.00027 0.00019 0.00014 0.00012 0.00011 0.00011 0.0001 0.0001 0.00009 ...
 $ 생존자.전체.  : int  100000 99709 99678 99656 99639 99626 99614 99602 99590 99580 ...
 $ 생존자.남자.  : int  100000 99686 99651 99625 99607 99592 99579 99565 99553 99541 ...
 $ 생존자.여자.  : int  100000 99733 99706 99687 99674 99662 99651 99640 99630 99621 ...
 $ X             : logi  NA NA NA NA NA NA ...
R > lifetable<-lifetable[,-"X"]
다음에 오류가 있습니다-"X" : 단항연산자에 유효한 인자가 아닙니다
R > lifetable<-lifetable[,-11]
R > star(lifetable)
에러: 함수 "star"를 찾을 수 없습니다
R > str(lifetable)
'data.frame':	101 obs. of  10 variables:
 $ 각세별        : chr  "0세" "1세" "2세" "3세" ...
 $ 기대여명.전체.: num  81.4 80.7 79.7 78.7 77.7 ...
 $ 기대여명.남자.: num  78 77.2 76.2 75.2 74.2 ...
 $ 기대여명.여자.: num  84.6 83.9 82.9 81.9 80.9 ...
 $ 사망확률.전체.: num  0.00291 0.00031 0.00022 0.00016 0.00013 0.00012 0.00012 0.00011 0.00011 0.0001 ...
 $ 사망확률.남자.: num  0.00314 0.00035 0.00026 0.00019 0.00015 0.00014 0.00014 0.00013 0.00012 0.0001 ...
 $ 사망확률.여자.: num  0.00267 0.00027 0.00019 0.00014 0.00012 0.00011 0.00011 0.0001 0.0001 0.00009 ...
 $ 생존자.전체.  : int  100000 99709 99678 99656 99639 99626 99614 99602 99590 99580 ...
 $ 생존자.남자.  : int  100000 99686 99651 99625 99607 99592 99579 99565 99553 99541 ...
 $ 생존자.여자.  : int  100000 99733 99706 99687 99674 99662 99651 99640 99630 99621 ...
R > theM<-matrix(1:9,nrow=3)
R > theM
     [,1] [,2] [,3]
[1,]    1    4    7
[2,]    2    5    8
[3,]    3    6    9
R > theM<-matrix(1:9,nrow=3,byrows=TRUE)
다음에 오류가 있습니다matrix(1:9, nrow = 3, byrows = TRUE) : 
  사용되지 않은 인자 (byrows = TRUE)
R > theM<-matrix(1:9,nrow=3,byrow=TRUE)
R > theM
     [,1] [,2] [,3]
[1,]    1    2    3
[2,]    4    5    6
[3,]    7    8    9
R > ?matrix
R > theM<-matrix(1:9,nrow=3)
R > apply(theM, 1,sum)
[1] 12 15 18
R > ?sapply
R > ?mapply
R > library(help=plyr)
R > search()
 [1] ".GlobalEnv"        "package:ggplot2"   "tools:RGUI"        "package:stats"     "package:graphics"  "package:grDevices"
 [7] "package:utils"     "package:datasets"  "KoreaEnv"          "package:methods"   "Autoloads"         "package:base"     
R > install.packages(plyr)
다음에 오류가 있습니다install.packages(plyr) : 객체 'plyr'를 찾을 수 없습니다
R > install.packages("plyr")
URL 'http://cran.nexr.com/bin/macosx/mavericks/contrib/3.1/plyr_1.8.1.tgz'을 시도합니다
Content type 'application/x-gzip' length 863410 bytes (843 Kb)
URL을 열었습니다
==================================================
downloaded 843 Kb


다운로드된 바이너리 패키지들은 다음의 위치에 있습니다
	/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T//RtmpyN9XPt/downloaded_packages
R > library(plyr)
R > ?data
R > data(diamonds)
R > ?aggregate
R > aggregate(price, by=cut, mean)
다음에 오류가 있습니다aggregate(price, by = cut, mean) : 
  객체 'price'를 찾을 수 없습니다
R > aggregate(diamonds$price, diamonds$cut, mean)
다음에 오류가 있습니다aggregate.data.frame(as.data.frame(x), ...) : 
  'by'는 반드시 리스트이어야 합니다
R > aggregate(diamonds$price, by=list(diamonds$cut), mean)
    Group.1        x
1      Fair 4358.758
2      Good 3928.864
3 Very Good 3981.760
4   Premium 4584.258
5     Ideal 3457.542
R > aggregate(price~cut,data=diamonds, mean)
        cut    price
1      Fair 4358.758
2      Good 3928.864
3 Very Good 3981.760
4   Premium 4584.258
5     Ideal 3457.542
R > aggregate(diamonds$price, by=list(diamonds$cut,diamonds$color), mean)
     Group.1 Group.2        x
1       Fair       D 4291.061
2       Good       D 3405.382
3  Very Good       D 3470.467
4    Premium       D 3631.293
5      Ideal       D 2629.095
6       Fair       E 3682.312
7       Good       E 3423.644
8  Very Good       E 3214.652
9    Premium       E 3538.914
10     Ideal       E 2597.550
11      Fair       F 3827.003
12      Good       F 3495.750
13 Very Good       F 3778.820
14   Premium       F 4324.890
15     Ideal       F 3374.939
16      Fair       G 4239.255
17      Good       G 4123.482
18 Very Good       G 3872.754
19   Premium       G 4500.742
20     Ideal       G 3720.706
21      Fair       H 5135.683
22      Good       H 4276.255
23 Very Good       H 4535.390
24   Premium       H 5216.707
25     Ideal       H 3889.335
26      Fair       I 4685.446
27      Good       I 5078.533
28 Very Good       I 5255.880
29   Premium       I 5946.181
30     Ideal       I 4451.970
31      Fair       J 4975.655
32      Good       J 4574.173
33 Very Good       J 5103.513
34   Premium       J 6294.592
35     Ideal       J 4918.186
R > aggregate(price~cut+color,data=diamonds, mean)
         cut color    price
1       Fair     D 4291.061
2       Good     D 3405.382
3  Very Good     D 3470.467
4    Premium     D 3631.293
5      Ideal     D 2629.095
6       Fair     E 3682.312
7       Good     E 3423.644
8  Very Good     E 3214.652
9    Premium     E 3538.914
10     Ideal     E 2597.550
11      Fair     F 3827.003
12      Good     F 3495.750
13 Very Good     F 3778.820
14   Premium     F 4324.890
15     Ideal     F 3374.939
16      Fair     G 4239.255
17      Good     G 4123.482
18 Very Good     G 3872.754
19   Premium     G 4500.742
20     Ideal     G 3720.706
21      Fair     H 5135.683
22      Good     H 4276.255
23 Very Good     H 4535.390
24   Premium     H 5216.707
25     Ideal     H 3889.335
26      Fair     I 4685.446
27      Good     I 5078.533
28 Very Good     I 5255.880
29   Premium     I 5946.181
30     Ideal     I 4451.970
31      Fair     J 4975.655
32      Good     J 4574.173
33 Very Good     J 5103.513
34   Premium     J 6294.592
35     Ideal     J 4918.186
R > ?by
R > by(diamonds$price, list(diamonds$cut,diamonds$color),mean)
: Fair
: D
[1] 4291.061
------------------------------------------------------------------------------------------------ 
: Good
: D
[1] 3405.382
------------------------------------------------------------------------------------------------ 
: Very Good
: D
[1] 3470.467
------------------------------------------------------------------------------------------------ 
: Premium
: D
[1] 3631.293
------------------------------------------------------------------------------------------------ 
: Ideal
: D
[1] 2629.095
------------------------------------------------------------------------------------------------ 
: Fair
: E
[1] 3682.312
------------------------------------------------------------------------------------------------ 
: Good
: E
[1] 3423.644
------------------------------------------------------------------------------------------------ 
: Very Good
: E
[1] 3214.652
------------------------------------------------------------------------------------------------ 
: Premium
: E
[1] 3538.914
------------------------------------------------------------------------------------------------ 
: Ideal
: E
[1] 2597.55
------------------------------------------------------------------------------------------------ 
: Fair
: F
[1] 3827.003
------------------------------------------------------------------------------------------------ 
: Good
: F
[1] 3495.75
------------------------------------------------------------------------------------------------ 
: Very Good
: F
[1] 3778.82
------------------------------------------------------------------------------------------------ 
: Premium
: F
[1] 4324.89
------------------------------------------------------------------------------------------------ 
: Ideal
: F
[1] 3374.939
------------------------------------------------------------------------------------------------ 
: Fair
: G
[1] 4239.255
------------------------------------------------------------------------------------------------ 
: Good
: G
[1] 4123.482
------------------------------------------------------------------------------------------------ 
: Very Good
: G
[1] 3872.754
------------------------------------------------------------------------------------------------ 
: Premium
: G
[1] 4500.742
------------------------------------------------------------------------------------------------ 
: Ideal
: G
[1] 3720.706
------------------------------------------------------------------------------------------------ 
: Fair
: H
[1] 5135.683
------------------------------------------------------------------------------------------------ 
: Good
: H
[1] 4276.255
------------------------------------------------------------------------------------------------ 
: Very Good
: H
[1] 4535.39
------------------------------------------------------------------------------------------------ 
: Premium
: H
[1] 5216.707
------------------------------------------------------------------------------------------------ 
: Ideal
: H
[1] 3889.335
------------------------------------------------------------------------------------------------ 
: Fair
: I
[1] 4685.446
------------------------------------------------------------------------------------------------ 
: Good
: I
[1] 5078.533
------------------------------------------------------------------------------------------------ 
: Very Good
: I
[1] 5255.88
------------------------------------------------------------------------------------------------ 
: Premium
: I
[1] 5946.181
------------------------------------------------------------------------------------------------ 
: Ideal
: I
[1] 4451.97
------------------------------------------------------------------------------------------------ 
: Fair
: J
[1] 4975.655
------------------------------------------------------------------------------------------------ 
: Good
: J
[1] 4574.173
------------------------------------------------------------------------------------------------ 
: Very Good
: J
[1] 5103.513
------------------------------------------------------------------------------------------------ 
: Premium
: J
[1] 6294.592
------------------------------------------------------------------------------------------------ 
: Ideal
: J
[1] 4918.186
R > table(by(diamonds$price, list(diamonds$cut,diamonds$color),mean))

2597.55008967461 2629.09456598447 3214.65208333333 3374.93936225823 3405.38217522659 3423.64415862808 3470.46728354263 
               1                1                1                1                1                1                1 
 3495.7502750275 3538.91442019683 3631.29257641921        3682.3125 3720.70638820639 3778.82024029575 3827.00320512821 
               1                1                1                1                1                1                1 
3872.75380600261 3889.33483146067  4123.4822043628 4239.25477707006 4276.25498575499 4291.06134969325 4324.89017589018 
               1                1                1                1                1                1                1 
4451.97037744864 4500.74213406293 4535.39035087719 4574.17263843648 4685.44571428571 4918.18638392857 4975.65546218487 
               1                1                1                1                1                1                1 
5078.53256704981 5103.51327433628 5135.68316831683 5216.70677966102 5255.87956810631 5946.18067226891 6294.59158415842 
               1                1                1                1                1                1                1 
R > print(table(by(diamonds$price, list(diamonds$cut,diamonds$color),mean),digits=2)
+ )
다음에 오류가 있습니다table(by(diamonds$price, list(diamonds$cut, diamonds$color),  : 
  모든 인자들은 반드시 같은 길이를 가져야 합니다
R > print(table(by(diamonds$price, list(diamonds$cut,diamonds$color),mean)),digits=2)

2597.55008967461 2629.09456598447 3214.65208333333 3374.93936225823 3405.38217522659 3423.64415862808 3470.46728354263 
               1                1                1                1                1                1                1 
 3495.7502750275 3538.91442019683 3631.29257641921        3682.3125 3720.70638820639 3778.82024029575 3827.00320512821 
               1                1                1                1                1                1                1 
3872.75380600261 3889.33483146067  4123.4822043628 4239.25477707006 4276.25498575499 4291.06134969325 4324.89017589018 
               1                1                1                1                1                1                1 
4451.97037744864 4500.74213406293 4535.39035087719 4574.17263843648 4685.44571428571 4918.18638392857 4975.65546218487 
               1                1                1                1                1                1                1 
5078.53256704981 5103.51327433628 5135.68316831683 5216.70677966102 5255.87956810631 5946.18067226891 6294.59158415842 
               1                1                1                1                1                1                1 
R > print(table(by(diamonds$price, list(diamonds$cut,diamonds$color),mean)),digits=2)

2597.55008967461 2629.09456598447 3214.65208333333 3374.93936225823 3405.38217522659 3423.64415862808 3470.46728354263 
               1                1                1                1                1                1                1 
 3495.7502750275 3538.91442019683 3631.29257641921        3682.3125 3720.70638820639 3778.82024029575 3827.00320512821 
               1                1                1                1                1                1                1 
3872.75380600261 3889.33483146067  4123.4822043628 4239.25477707006 4276.25498575499 4291.06134969325 4324.89017589018 
               1                1                1                1                1                1                1 
4451.97037744864 4500.74213406293 4535.39035087719 4574.17263843648 4685.44571428571 4918.18638392857 4975.65546218487 
               1                1                1                1                1                1                1 
5078.53256704981 5103.51327433628 5135.68316831683 5216.70677966102 5255.87956810631 5946.18067226891 6294.59158415842 
               1                1                1                1                1                1                1 
R > ?signif
R > round(table(by(diamonds$price, list(diamonds$cut,diamonds$color),mean)),digits=2)

2597.55008967461 2629.09456598447 3214.65208333333 3374.93936225823 3405.38217522659 3423.64415862808 3470.46728354263 
               1                1                1                1                1                1                1 
 3495.7502750275 3538.91442019683 3631.29257641921        3682.3125 3720.70638820639 3778.82024029575 3827.00320512821 
               1                1                1                1                1                1                1 
3872.75380600261 3889.33483146067  4123.4822043628 4239.25477707006 4276.25498575499 4291.06134969325 4324.89017589018 
               1                1                1                1                1                1                1 
4451.97037744864 4500.74213406293 4535.39035087719 4574.17263843648 4685.44571428571 4918.18638392857 4975.65546218487 
               1                1                1                1                1                1                1 
5078.53256704981 5103.51327433628 5135.68316831683 5216.70677966102 5255.87956810631 5946.18067226891 6294.59158415842 
               1                1                1                1                1                1                1 
R > table(round(by(diamonds$price, list(diamonds$cut,diamonds$color),mean),digits=2))

2597.55 2629.09 3214.65 3374.94 3405.38 3423.64 3470.47 3495.75 3538.91 3631.29 3682.31 3720.71 3778.82    3827 3872.75 3889.33 
      1       1       1       1       1       1       1       1       1       1       1       1       1       1       1       1 
4123.48 4239.25 4276.25 4291.06 4324.89 4451.97 4500.74 4535.39 4574.17 4685.45 4918.19 4975.66 5078.53 5103.51 5135.68 5216.71 
      1       1       1       1       1       1       1       1       1       1       1       1       1       1       1       1 
5255.88 5946.18 6294.59 
      1       1       1 
R > aggregate(price~cut+color,data=diamonds, mean)
         cut color    price
1       Fair     D 4291.061
2       Good     D 3405.382
3  Very Good     D 3470.467
4    Premium     D 3631.293
5      Ideal     D 2629.095
6       Fair     E 3682.312
7       Good     E 3423.644
8  Very Good     E 3214.652
9    Premium     E 3538.914
10     Ideal     E 2597.550
11      Fair     F 3827.003
12      Good     F 3495.750
13 Very Good     F 3778.820
14   Premium     F 4324.890
15     Ideal     F 3374.939
16      Fair     G 4239.255
17      Good     G 4123.482
18 Very Good     G 3872.754
19   Premium     G 4500.742
20     Ideal     G 3720.706
21      Fair     H 5135.683
22      Good     H 4276.255
23 Very Good     H 4535.390
24   Premium     H 5216.707
25     Ideal     H 3889.335
26      Fair     I 4685.446
27      Good     I 5078.533
28 Very Good     I 5255.880
29   Premium     I 5946.181
30     Ideal     I 4451.970
31      Fair     J 4975.655
32      Good     J 4574.173
33 Very Good     J 5103.513
34   Premium     J 6294.592
35     Ideal     J 4918.186
R > ?table
R > class(aggregate(price~cut+color,data=diamonds, mean))
[1] "data.frame"
R > str(aggregate(price~cut+color,data=diamonds, mean))
'data.frame':	35 obs. of  3 variables:
 $ cut  : Ord.factor w/ 5 levels "Fair"<"Good"<..: 1 2 3 4 5 1 2 3 4 5 ...
 $ color: Ord.factor w/ 7 levels "D"<"E"<"F"<"G"<..: 1 1 1 1 1 2 2 2 2 2 ...
 $ price: num  4291 3405 3470 3631 2629 ...
R > ?as.table
R > xtabs(aggregate(price~cut+color,data=diamonds, mean))
다음에 오류가 있습니다Summary.ordered(5L, na.rm = FALSE) : 
  'sum' not defined for ordered factors
R > ?xtabs
R > price.by.cut.color<-aggregate(price~cut+color,data=diamonds, mean)
R > xtabs(price~cut+color,data=price.by.cut.color)
           color
cut                D        E        F        G        H        I        J
  Fair      4291.061 3682.312 3827.003 4239.255 5135.683 4685.446 4975.655
  Good      3405.382 3423.644 3495.750 4123.482 4276.255 5078.533 4574.173
  Very Good 3470.467 3214.652 3778.820 3872.754 4535.390 5255.880 5103.513
  Premium   3631.293 3538.914 4324.890 4500.742 5216.707 5946.181 6294.592
  Ideal     2629.095 2597.550 3374.939 3720.706 3889.335 4451.970 4918.186
R > price.carat.by.cut.color<-aggregate(cbind(price,carat)~cut+color,data=diamonds, mean)
R > price.carat.by.cut.color
         cut color    price     carat
1       Fair     D 4291.061 0.9201227
2       Good     D 3405.382 0.7445166
3  Very Good     D 3470.467 0.6964243
4    Premium     D 3631.293 0.7215471
5      Ideal     D 2629.095 0.5657657
6       Fair     E 3682.312 0.8566071
7       Good     E 3423.644 0.7451340
8  Very Good     E 3214.652 0.6763167
9    Premium     E 3538.914 0.7177450
10     Ideal     E 2597.550 0.5784012
11      Fair     F 3827.003 0.9047115
12      Good     F 3495.750 0.7759296
13 Very Good     F 3778.820 0.7409612
14   Premium     F 4324.890 0.8270356
15     Ideal     F 3374.939 0.6558285
16      Fair     G 4239.255 1.0238217
17      Good     G 4123.482 0.8508955
18 Very Good     G 3872.754 0.7667986
19   Premium     G 4500.742 0.8414877
20     Ideal     G 3720.706 0.7007146
21      Fair     H 5135.683 1.2191749
22      Good     H 4276.255 0.9147293
23 Very Good     H 4535.390 0.9159485
24   Premium     H 5216.707 1.0164492
25     Ideal     H 3889.335 0.7995249
26      Fair     I 4685.446 1.1980571
27      Good     I 5078.533 1.0572222
28 Very Good     I 5255.880 1.0469518
29   Premium     I 5946.181 1.1449370
30     Ideal     I 4451.970 0.9130291
31      Fair     J 4975.655 1.3411765
32      Good     J 4574.173 1.0995440
33 Very Good     J 5103.513 1.1332153
34   Premium     J 6294.592 1.2930941
35     Ideal     J 4918.186 1.0635937
R > xtabs(price~cut+color,data=price.carat.by.cut.color)
           color
cut                D        E        F        G        H        I        J
  Fair      4291.061 3682.312 3827.003 4239.255 5135.683 4685.446 4975.655
  Good      3405.382 3423.644 3495.750 4123.482 4276.255 5078.533 4574.173
  Very Good 3470.467 3214.652 3778.820 3872.754 4535.390 5255.880 5103.513
  Premium   3631.293 3538.914 4324.890 4500.742 5216.707 5946.181 6294.592
  Ideal     2629.095 2597.550 3374.939 3720.706 3889.335 4451.970 4918.186
R > xtabs(carat~cut+color,data=price.carat.by.cut.color)
           color
cut                 D         E         F         G         H         I         J
  Fair      0.9201227 0.8566071 0.9047115 1.0238217 1.2191749 1.1980571 1.3411765
  Good      0.7445166 0.7451340 0.7759296 0.8508955 0.9147293 1.0572222 1.0995440
  Very Good 0.6964243 0.6763167 0.7409612 0.7667986 0.9159485 1.0469518 1.1332153
  Premium   0.7215471 0.7177450 0.8270356 0.8414877 1.0164492 1.1449370 1.2930941
  Ideal     0.5657657 0.5784012 0.6558285 0.7007146 0.7995249 0.9130291 1.0635937
R > xtabs(cbind(price,carat)~cut+color,data=price.carat.by.cut.color)
, ,  = price

           color
cut                    D            E            F            G            H            I            J
  Fair      4291.0613497 3682.3125000 3827.0032051 4239.2547771 5135.6831683 4685.4457143 4975.6554622
  Good      3405.3821752 3423.6441586 3495.7502750 4123.4822044 4276.2549858 5078.5325670 4574.1726384
  Very Good 3470.4672835 3214.6520833 3778.8202403 3872.7538060 4535.3903509 5255.8795681 5103.5132743
  Premium   3631.2925764 3538.9144202 4324.8901759 4500.7421341 5216.7067797 5946.1806723 6294.5915842
  Ideal     2629.0945660 2597.5500897 3374.9393623 3720.7063882 3889.3348315 4451.9703774 4918.1863839

, ,  = carat

           color
cut                    D            E            F            G            H            I            J
  Fair         0.9201227    0.8566071    0.9047115    1.0238217    1.2191749    1.1980571    1.3411765
  Good         0.7445166    0.7451340    0.7759296    0.8508955    0.9147293    1.0572222    1.0995440
  Very Good    0.6964243    0.6763167    0.7409612    0.7667986    0.9159485    1.0469518    1.1332153
  Premium      0.7215471    0.7177450    0.8270356    0.8414877    1.0164492    1.1449370    1.2930941
  Ideal        0.5657657    0.5784012    0.6558285    0.7007146    0.7995249    0.9130291    1.0635937

R > round(xtabs(cbind(price,carat)~cut+color,data=price.carat.by.cut.color),digits=2)
, ,  = price

           color
cut               D       E       F       G       H       I       J
  Fair      4291.06 3682.31 3827.00 4239.25 5135.68 4685.45 4975.66
  Good      3405.38 3423.64 3495.75 4123.48 4276.25 5078.53 4574.17
  Very Good 3470.47 3214.65 3778.82 3872.75 4535.39 5255.88 5103.51
  Premium   3631.29 3538.91 4324.89 4500.74 5216.71 5946.18 6294.59
  Ideal     2629.09 2597.55 3374.94 3720.71 3889.33 4451.97 4918.19

, ,  = carat

           color
cut               D       E       F       G       H       I       J
  Fair         0.92    0.86    0.90    1.02    1.22    1.20    1.34
  Good         0.74    0.75    0.78    0.85    0.91    1.06    1.10
  Very Good    0.70    0.68    0.74    0.77    0.92    1.05    1.13
  Premium      0.72    0.72    0.83    0.84    1.02    1.14    1.29
  Ideal        0.57    0.58    0.66    0.70    0.80    0.91    1.06

R > search()
 [1] ".GlobalEnv"        "package:plyr"      "package:ggplot2"   "tools:RGUI"        "package:stats"     "package:graphics" 
 [7] "package:grDevices" "package:utils"     "package:datasets"  "KoreaEnv"          "package:methods"   "Autoloads"        
[13] "package:base"     
R > head(baseball)
           id year stint team lg  g  ab  r  h X2b X3b hr rbi sb cs bb so ibb hbp sh sf gidp
4   ansonca01 1871     1  RC1    25 120 29 39  11   3  0  16  6  2  2  1  NA  NA NA NA   NA
44  forceda01 1871     1  WS3    32 162 45 45   9   4  0  29  8  0  4  0  NA  NA NA NA   NA
68  mathebo01 1871     1  FW1    19  89 15 24   3   1  0  10  2  1  2  0  NA  NA NA NA   NA
99  startjo01 1871     1  NY2    33 161 35 58   5   1  1  34  4  2  3  0  NA  NA NA NA   NA
102 suttoez01 1871     1  CL1    29 128 35 45   3   7  3  23  3  1  1  0  NA  NA NA NA   NA
106 whitede01 1871     1  CL1    29 146 40 47   6   5  1  21  2  2  4  1  NA  NA NA NA   NA
R > str(baseball)
'data.frame':	21699 obs. of  22 variables:
 $ id   : chr  "ansonca01" "forceda01" "mathebo01" "startjo01" ...
 $ year : int  1871 1871 1871 1871 1871 1871 1871 1872 1872 1872 ...
 $ stint: int  1 1 1 1 1 1 1 1 1 1 ...
 $ team : chr  "RC1" "WS3" "FW1" "NY2" ...
 $ lg   : chr  "" "" "" "" ...
 $ g    : int  25 32 19 33 29 29 29 46 37 25 ...
 $ ab   : int  120 162 89 161 128 146 145 217 174 130 ...
 $ r    : int  29 45 15 35 35 40 36 60 26 40 ...
 $ h    : int  39 45 24 58 45 47 37 90 46 53 ...
 $ X2b  : int  11 9 3 5 3 6 5 10 3 11 ...
 $ X3b  : int  3 4 1 1 7 5 7 7 0 0 ...
 $ hr   : int  0 0 0 1 3 1 2 0 0 0 ...
 $ rbi  : int  16 29 10 34 23 21 23 50 15 16 ...
 $ sb   : int  6 8 2 4 3 2 2 6 0 2 ...
 $ cs   : int  2 0 1 2 1 2 2 6 1 2 ...
 $ bb   : int  2 4 2 3 1 4 9 16 1 1 ...
 $ so   : int  1 0 0 0 0 1 1 3 1 0 ...
 $ ibb  : int  NA NA NA NA NA NA NA NA NA NA ...
 $ hbp  : int  NA NA NA NA NA NA NA NA NA NA ...
 $ sh   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ sf   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ gidp : int  NA NA NA NA NA NA NA NA NA NA ...
R > baseball$sf[baseball$year<1954]<-0
R > any(is.na(baseball$sf))
[1] FALSE
R > baseball$hbp[is.na(baseball$hbp)]<-0
R > any(is.na(baseball$hbp))
[1] FALSE
R > baseball.2<-baseball[baseball$ab>=50,]
R > str(baseball.2)
'data.frame':	14828 obs. of  22 variables:
 $ id   : chr  "ansonca01" "forceda01" "mathebo01" "startjo01" ...
 $ year : int  1871 1871 1871 1871 1871 1871 1871 1872 1872 1872 ...
 $ stint: int  1 1 1 1 1 1 1 1 1 1 ...
 $ team : chr  "RC1" "WS3" "FW1" "NY2" ...
 $ lg   : chr  "" "" "" "" ...
 $ g    : int  25 32 19 33 29 29 29 46 37 25 ...
 $ ab   : int  120 162 89 161 128 146 145 217 174 130 ...
 $ r    : int  29 45 15 35 35 40 36 60 26 40 ...
 $ h    : int  39 45 24 58 45 47 37 90 46 53 ...
 $ X2b  : int  11 9 3 5 3 6 5 10 3 11 ...
 $ X3b  : int  3 4 1 1 7 5 7 7 0 0 ...
 $ hr   : int  0 0 0 1 3 1 2 0 0 0 ...
 $ rbi  : int  16 29 10 34 23 21 23 50 15 16 ...
 $ sb   : int  6 8 2 4 3 2 2 6 0 2 ...
 $ cs   : int  2 0 1 2 1 2 2 6 1 2 ...
 $ bb   : int  2 4 2 3 1 4 9 16 1 1 ...
 $ so   : int  1 0 0 0 0 1 1 3 1 0 ...
 $ ibb  : int  NA NA NA NA NA NA NA NA NA NA ...
 $ hbp  : num  0 0 0 0 0 0 0 0 0 0 ...
 $ sh   : int  NA NA NA NA NA NA NA NA NA NA ...
 $ sf   : num  0 0 0 0 0 0 0 0 0 0 ...
 $ gidp : int  NA NA NA NA NA NA NA NA NA NA ...
R > baseball.2$OBP<-with(baseball.2,(h+bp+hbp)/(ab+bb+hbp+sf))
다음에 오류가 있습니다eval(expr, envir, enclos) : 객체 'bp'를 찾을 수 없습니다
R > baseball.2$OBP<-with(baseball.2,(h+bb+hbp)/(ab+bb+hbp+sf))
R > tail(baseball.2)
             id year stint team lg   g  ab  r   h X2b X3b hr rbi sb cs  bb  so ibb hbp sh sf gidp       OBP
89499 claytro01 2007     1  TOR AL  69 189 23  48  14   0  1  12  2  1  14  50   0   1  3  3    8 0.3043478
89502 cirilje01 2007     1  MIN AL  50 153 18  40   9   2  2  21  2  0  15  13   0   1  3  2    9 0.3274854
89521 bondsba01 2007     1  SFN NL 126 340 75  94  14   0 28  66  5  0 132  54  43   3  0  2   13 0.4800839
89523 biggicr01 2007     1  HOU NL 141 517 68 130  31   3 10  50  4  3  23 112   0   3  7  5    5 0.2846715
89530 ausmubr01 2007     1  HOU NL 117 349 38  82  16   3  3  25  6  1  37  74   3   6  4  1   11 0.3180662
89533  aloumo01 2007     1  NYN NL  87 328 51 112  19   1 13  49  3  0  27  30   5   2  0  3   13 0.3916667
R > baseball.2[,id=="ruthba01"]
다음에 오류가 있습니다id == "ruthba01" : 
  atomic과 리스트 타입들에 대해서만 비교(1)가 가능합니다
R > baseball.2[id=="ruthba01"]
다음에 오류가 있습니다id == "ruthba01" : 
  atomic과 리스트 타입들에 대해서만 비교(1)가 가능합니다
R > baseball.2[,baseball.2$id=="ruthba01"]
다음에 오류가 있습니다`[.data.frame`(baseball.2, , baseball.2$id == "ruthba01") : 
  undefined columns selected
R > baseball.2[,][baseball.2$id=="ruthba01"]
다음에 오류가 있습니다`[.data.frame`(baseball.2[, ], baseball.2$id == "ruthba01") : 
  undefined columns selected
R > baseball.2[baseball.2$id=="ruthba01"]
다음에 오류가 있습니다`[.data.frame`(baseball.2, baseball.2$id == "ruthba01") : 
  undefined columns selected
R > baseball.2[,baseball.2$id]
다음에 오류가 있습니다`[.data.frame`(baseball.2, , baseball.2$id) : 
  undefined columns selected
R > baseball.2[,"id"]
    [1] "ansonca01" "forceda01" "mathebo01" "startjo01" "suttoez01" "whitede01" "yorkto01"  "ansonca01" "burdoja01" "forceda01"
   [11] "forceda01" "mathebo01" "nelsoca01" "orourji01" "startjo01" "suttoez01" "whitede01" "yorkto01"  "ansonca01" "burdoja01"
   [21] "forceda01" "gerhajo01" "hinespa01" "mathebo01" "nelsoca01" "orourji01" "snydepo01" "startjo01" "suttoez01" "whitede01"
   [31] "yorkto01"  "ansonca01" "burdoja01" "forceda01" "gerhajo01" "hinespa01" "mathebo01" "nelsoca01" "orourji01" "snydepo01"
   [41] "startjo01" "suttoez01" "whitede01" "yorkto01"  "ansonca01" "burdoja01" "forceda01" "gerhajo01" "hinespa01" "mathebo01"
   [51] "nelsoca01" "orourji01" "shaffor01" "snydepo01" "startjo01" "suttoez01" "whitede01" "yorkto01"  "ansonca01" "burdoja01"
   [61] "forceda01" "gerhajo01" "hinespa01" "jonesch01" "mathebo01" "morrijo01" "orourji01" "snydepo01" "startjo01" "suttoez01"
   [71] "whitede01" "yorkto01"  "ansonca01" "burdoja01" "forceda01" "gerhajo01" "hinespa01" "jonesch01" "jonesch01" "mathebo01"
   [81] "morrijo01" "orourji01" "shaffor01" "snydepo01" "startjo01" "suttoez01" "whitede01" "yorkto01"  "ansonca01" "bennech01"
   [91] "burdoja01" "gerhajo01" "hinespa01" "jonesch01" "kellyki01" "morrijo01" "nelsoca01" "orourji01" "shaffor01" "snydepo01"
  [101] "startjo01" "suttoez01" "wardjo01"  "whitede01" "yorkto01"  "ansonca01" "broutda01" "burdoja01" "forceda01" "galvipu01"
  [111] "gerhajo01" "glassja01" "gorege01"  "hinespa01" "jonesch01" "kellyki01" "mathebo01" "morrijo01" "nelsoca01" "orourji01"
  [121] "purcebl01" "purcebl01" "richaha01" "shaffor01" "snydepo01" "startjo01" "suttoez01" "wardjo01"  "whitede01" "yorkto01" 
  [131] "ansonca01" "bennech01" "burdoja01" "connoro01" "forceda01" "galvipu01" "glassja01" "gorege01"  "hinespa01" "jonesch01"
  [141] "kellyki01" "lathaar01" "morrijo01" "orourji01" "purcebl01" "richaha01" "shaffor01" "smithpo01" "startjo01" "stoveha01"
  [151] "suttoez01" "wardjo01"  "whitede01" "woodge01"  "yorkto01"  "ansonca01" "bennech01" "broutda01" "burdoja01" "connoro01"
  [161] "dennyje01" "ewingbu01" "forceda01" "galvipu01" "gerhajo01" "glassja01" "gorege01"  "hinespa01" "keefeti01" "kellyki01"
  [171] "mathebo01" "mathebo01" "morrijo01" "nelsoca01" "orourji01" "purcebl01" "purcebl01" "richaha01" "shaffor01" "snydepo01"
  [181] "startjo01" "stoveha01" "suttoez01" "wardjo01"  "whitede01" "woodge01"  "yorkto01"  "brownpe01" "brownto01" "mcphebi01"
  [191] "mullato01" "smithpo01" "snydepo01" "ansonca01" "bennech01" "broutda01" "burdoja01" "connoro01" "dennyje01" "ewingbu01"
  [201] "forceda01" "galvipu01" "glassja01" "gorege01"  "hinespa01" "keefeti01" "kellyki01" "mathebo01" "morrijo01" "orourji01"
  [211] "pfefffr01" "purcebl01" "richaha01" "shaffor01" "startjo01" "stoveha01" "suttoez01" "wardjo01"  "whitede01" "woodge01" 
  [221] "yorkto01"  "brownpe01" "brownto01" "gerhajo01" "jonesch01" "keefeti01" "lathaar01" "mathebo01" "mcphebi01" "mullato01"
  [231] "nelsoca01" "smithpo01" "snydepo01" "stoveha01" "ansonca01" "bennech01" "broutda01" "burdoja01" "connoro01" "dennyje01"
  [241] "ewingbu01" "forceda01" "galvipu01" "glassja01" "gorege01"  "hinespa01" "kellyki01" "morrijo01" "orourji01" "pfefffr01"
  [251] "purcebl01" "richaha01" "shaffor01" "startjo01" "suttoez01" "wardjo01"  "whitede01" "woodge01"  "yorkto01"  "brownpe01"
  [261] "brownto01" "gerhajo01" "jonesch01" "keefeti01" "lathaar01" "mathebo01" "mcguide01" "mcphebi01" "mullato01" "nashbi01" 
  [271] "nelsoca01" "smithpo01" "snydepo01" "stoveha01" "terryad01" "yorkto01"  "ansonca01" "bennech01" "broutda01" "burdoja01"
  [281] "connoro01" "dennyje01" "ewingbu01" "forceda01" "galvipu01" "glassja01" "gorege01"  "hinespa01" "kellyki01" "morrijo01"
  [291] "orourji01" "pfefffr01" "purcebl01" "richaha01" "smithge01" "startjo01" "suttoez01" "wardjo01"  "whitede01" "woodge01" 
  [301] "clemeja01" "glassja01" "quinnjo02" "shaffor01" "smithge01" "brownpe01" "brownto01" "jonesch01" "lathaar01" "mathebo01"
  [311] "mcphebi01" "nelsoca01" "purcebl01" "smithge01" "smithpo01" "snydepo01" "stoveha01" "terryad01" "yorkto01"  "ansonca01"
  [321] "bennech01" "broutda01" "burdoja01" "clemeja01" "connoro01" "dennyje01" "ewingbu01" "forceda01" "galvipu01" "ganzech01"
  [331] "gerhajo01" "glassja01" "gorege01"  "hinespa01" "keefeti01" "kellyki01" "mcguide01" "morrijo01" "nashbi01"  "orourji01"
  [341] "pfefffr01" "purcebl01" "quinnjo02" "richaha01" "shaffor01" "startjo01" "suttoez01" "thompsa01" "wardjo01"  "whitede01"
  [351] "woodge01"  "brownpe01" "brownto01" "galvipu01" "jonesch01" "lathaar01" "mathebo01" "mcphebi01" "mullato01" "nelsoca01"
  [361] "purcebl01" "robinwi01" "shaffor01" "smithge01" "smithpo01" "snydepo01" "stoveha01" "terryad01" "ansonca01" "bennech01"
  [371] "broutda01" "burdoja01" "clemeja01" "connoro01" "dennyje01" "ewingbu01" "forceda01" "ganzech01" "gerhajo01" "glassja01"
  [381] "gorege01"  "hinespa01" "keefeti01" "kellyki01" "mcguide01" "morrijo01" "nashbi01"  "orourji01" "pfefffr01" "quinnjo02"
  [391] "richaha01" "ryanji01"  "startjo01" "suttoez01" "thompsa01" "wardjo01"  "whitede01" "woodge01"  "brownpe01" "crossla01"
  [401] "gerhajo01" "jonesch01" "jonesch01" "lathaar01" "mcphebi01" "mullato01" "nelsoca01" "purcebl01" "robinwi01" "smithel01"
  [411] "smithge01" "snydepo01" "stoveha01" "terryad01" "tucketo01" "weyhigu01" "zimmech01" "ansonca01" "bennech01" "broutda01"
  [421] "brownto01" "brownto01" "burdoja01" "clemeja01" "connoro01" "dalyto01"  "dennyje01" "ewingbu01" "galvipu01" "ganzech01"
  [431] "glassja01" "gorege01"  "hinespa01" "keefeti01" "kellyki01" "maulal01"  "mcguide01" "morrijo01" "nashbi01"  "orourji01"
  [441] "pfefffr01" "richaha01" "ryanji01"  "smithpo01" "suttoez01" "thompsa01" "vanhage01" "wardjo01"  "whitede01" "woodge01" 
  [451] "brownpe01" "burdoja01" "crossla01" "lathaar01" "mcguide01" "mcphebi01" "mullato01" "oconnja01" "purcebl01" "purcebl01"
  [461] "robinwi01" "smithel01" "smithge01" "snydepo01" "stoveha01" "terryad01" "tucketo01" "weyhigu01" "zimmech01" "ansonca01"
  [471] "becklja01" "bennech01" "broutda01" "brownto01" "burdoja01" "clemeja01" "connoro01" "dalyto01"  "delahed01" "dennyje01"
  [481] "duffyhu01" "ewingbu01" "farredu01" "galvipu01" "ganzech01" "glassja01" "gleaski01" "gorege01"  "hallmbi01" "hinespa01"
  [491] "keefeti01" "kellyki01" "maulal01"  "mcguide01" "morrijo01" "nashbi01"  "orourji01" "pfefffr01" "quinnjo02" "richaha01"
  [501] "ryanji01"  "smithpo01" "suttoez01" "thompsa01" "vanhage01" "wardjo01"  "whitede01" "woodge01"  "brownpe01" "crossla01"
  [511] "foremfr01" "lathaar01" "longhe01"  "mcphebi01" "mullato01" "oconnja01" "purcebl01" "robinwi01" "smithel01" "smithge01"
  [521] "stoveha01" "terryad01" "tucketo01" "weyhigu01" "ansonca01" "becklja01" "bennech01" "broutda01" "brownto01" "clemeja01"
  [531] "connoro01" "dalyto01"  "delahed01" "dennyje01" "duffyhu01" "ewingbu01" "farredu01" "galvipu01" "ganzech01" "glassja01"
  [541] "gleaski01" "gorege01"  "hallmbi01" "hinespa01" "keefeti01" "kellyki01" "maulal01"  "morrijo01" "nashbi01"  "orourji01"
  [551] "pfefffr01" "quinnjo02" "richaha01" "ryanji01"  "smithpo01" "smithpo01" "snydepo01" "thompsa01" "vanhage01" "wardjo01" 
  [561] "whitede01" "woodge01"  "zimmech01" "doyleja01" "elybo01"   "gerhajo01" "gerhajo01" "mcguide01" "nelsoca01" "oconnja01"
  [571] "purcebl01" "robinwi01" "shaffor01" "ansonca01" "bennech01" "burkeje01" "clemeja01" "dalyto01"  "davisge01" "dennyje01"
  [581] "donovpa01" "donovpa01" "foremfr01" "ganzech01" "glassja01" "gleaski01" "hinespa01" "hinespa01" "kittrma01" "lathaar01"
  [591] "longhe01"  "lowebo01"  "mcphebi01" "mullato01" "nichoki01" "smithge01" "smithpo01" "terryad01" "thompsa01" "tucketo01"
  [601] "youngcy01" "zimmech01" "becklja01" "broutda01" "brownpe01" "brownto01" "connoro01" "corcoto01" "crossla01" "delahed01"
  [611] "duffyhu01" "ewingbu01" "farredu01" "galvipu01" "gorege01"  "hallmbi01" "keefeti01" "kellyki01" "lathaar01" "maulal01" 
  [621] "nashbi01"  "orourji01" "pfefffr01" "quinnjo02" "richaha01" "ryanji01"  "stoveha01" "vanhage01" "wardjo01"  "weyhigu01"
  [631] "whitede01" "woodge01"  "broutda01" "brownto01" "corcoto01" "crossla01" "donovpa01" "donovpa01" "duffyhu01" "farredu01"
  [641] "foremfr01" "griffcl01" "hallmbi01" "hinespa01" "jennihu01" "kellyki01" "mcgrajo01" "mcguide01" "oconnja01" "richaha01"
  [651] "robinwi01" "smithpo01" "vanhage01" "weyhigu01" "woodge01"  "ansonca01" "becklja01" "bennech01" "brownpe01" "brownpe01"
  [661] "burkeje01" "clemeja01" "connoro01" "dahlebi01" "dalyto01"  "davisge01" "delahed01" "dennyje01" "dennyje01" "doyleja01"
  [671] "elybo01"   "galvipu01" "ganzech01" "glassja01" "gleaski01" "gorege01"  "kellyki01" "kittrma01" "lathaar01" "longhe01" 
  [681] "lowebo01"  "maulal01"  "mcphebi01" "mullato01" "nashbi01"  "nichoki01" "orourji01" "pfefffr01" "quinnjo02" "ryanji01" 
  [691] "smithge01" "stoveha01" "terryad01" "thompsa01" "tucketo01" "wardjo01"  "youngcy01" "zimmech01" "ansonca01" "becklja01"
  [701] "bennech01" "broutda01" "brownpe01" "brownpe01" "brownto01" "burkeje01" "clemeja01" "connoro01" "corcoto01" "crossla01"
  [711] "crossmo01" "dahlebi01" "dalyto01"  "davisge01" "delahed01" "donovpa01" "donovpa01" "doyleja01" "doyleja01" "duffyhu01"
  [721] "ewingbu01" "farredu01" "ganzech01" "glassja01" "gleaski01" "gorege01"  "gorege01"  "hallmbi01" "jennihu01" "keefeti01"
  [731] "keelewi01" "kellejo01" "kellyki01" "kittrma01" "lathaar01" "longhe01"  "lowebo01"  "mcgrajo01" "mcguide01" "mcphebi01"
  [741] "mullato01" "nashbi01"  "nichoki01" "oconnja01" "orourji01" "pfefffr01" "quinnjo02" "richaha01" "robinwi01" "ryanji01" 
  [751] "smithel01" "smithge01" "stoveha01" "stoveha01" "terryad01" "thompsa01" "tucketo01" "vanhage01" "vanhage01" "wardjo01" 
  [761] "weyhigu01" "woodge01"  "woodge01"  "youngcy01" "zimmech01" "ansonca01" "becklja01" "bennech01" "broutda01" "brownpe01"
  [771] "brownto01" "burkeje01" "clemeja01" "connoro01" "corcoto01" "crossla01" "dahlebi01" "dalyto01"  "davisge01" "delahed01"
  [781] "dennyje01" "donovpa01" "doyleja01" "duffyhu01" "elybo01"   "ewingbu01" "farredu01" "ganzech01" "glassja01" "glassja01"
  [791] "gleaski01" "hallmbi01" "jennihu01" "jennihu01" "keefeti01" "keelewi01" "kellejo01" "kellyki01" "kittrma01" "lathaar01"
  [801] "longhe01"  "lowebo01"  "maulal01"  "mcgrajo01" "mcguide01" "mcphebi01" "mullato01" "mullato01" "nashbi01"  "nichoki01"
  [811] "oconnja01" "orourji01" "peitzhe01" "pfefffr01" "quinnjo02" "robinwi01" "ryanji01"  "smithel01" "smithge01" "stoveha01"
  [821] "terryad01" "thompsa01" "tucketo01" "vanhage01" "wardjo01"  "weyhigu01" "youngcy01" "zimmech01" "anderjo01" "ansonca01"
  [831] "becklja01" "broutda01" "brownto01" "burkeje01" "clarkfr01" "clemeja01" "connoro01" "connoro01" "corcoto01" "crossla01"
  [841] "dahlebi01" "dalyto01"  "davisge01" "delahed01" "dennyje01" "donovpa01" "doyleja01" "duffyhu01" "elybo01"   "ewingbu01"
  [851] "farredu01" "ganzech01" "glassja01" "gleaski01" "griffcl01" "hallmbi01" "jennihu01" "keelewi01" "kellejo01" "kittrma01"
  [861] "lathaar01" "longhe01"  "lowebo01"  "maulal01"  "mcgrajo01" "mcguide01" "mcphebi01" "mullato01" "nashbi01"  "nichoki01"
  [871] "oconnja01" "peitzhe01" "pfefffr01" "quinnjo02" "robinwi01" "ryanji01"  "smithel01" "smithge01" "tennefr02" "terryad01"
  [881] "thompsa01" "tucketo01" "vanhage01" "wardjo01"  "weyhigu01" "youngcy01" "zimmech01" "anderjo01" "ansonca01" "becklja01"
  [891] "broutda01" "brownto01" "brownto01" "burkeje01" "clarkfr01" "clemeja01" "colliji01" "connoro01" "corcoto01" "crossla01"
  [901] "crossmo01" "dahlebi01" "dalyto01"  "davisge01" "delahed01" "donovpa01" "doyleja01" "duffyhu01" "elybo01"   "ewingbu01"
  [911] "farredu01" "foremfr01" "ganzech01" "glassja01" "glassja01" "gleaski01" "griffcl01" "hallmbi01" "jennihu01" "keelewi01"
  [921] "kellejo01" "kittrma01" "lathaar01" "longhe01"  "lowebo01"  "maulal01"  "mcgrajo01" "mcguide01" "mcphebi01" "nashbi01" 
  [931] "nichoki01" "oconnja01" "peitzhe01" "quinnjo02" "robinwi01" "ryanji01"  "smithel01" "smithge01" "tennefr02" "terryad01"
  [941] "thompsa01" "tucketo01" "vanhage01" "wallabo01" "warnejo01" "weyhigu01" "youngcy01" "zimmech01" "anderjo01" "ansonca01"
  [951] "becklja01" "becklja01" "broutda01" "brownto01" "burkeje01" "clarkfr01" "clemeja01" "colliji01" "connoro01" "corcoto01"
  [961] "crossla01" "crossmo01" "dahlebi01" "dalyto01"  "davisge01" "davisha01" "davisha01" "delahed01" "donovpa01" "doyleja01"
  [971] "duffyhu01" "elybo01"   "ewingbu01" "farredu01" "farredu01" "foremfr01" "frasech01" "ganzech01" "gleaski01" "griffcl01"
  [981] "hallmbi01" "jennihu01" "jonesfi01" "keelewi01" "kellejo01" "kittrma01" "lajoina01" "longhe01"  "lowebo01"  "mcfared01"
  [991] "mcgrajo01" "mcguide01" "mcphebi01" "nashbi01"  "nichoki01" "oconnja01" "orthal01"  "peitzhe01" "pfefffr01" "quinnjo02"
 [1001] "quinnjo02" "robinwi01" "ryanji01"  "smithel01" "smithge01" "tennefr02" "terryad01" "thompsa01" "tucketo01" "vanhage01"
 [1011] "wallabo01" "warnejo01" "warnejo01" "youngcy01" "zimmech01" "anderjo01" "ansonca01" "becklja01" "becklja01" "bowerfr01"
 [1021] "brownto01" "burkeje01" "clarkfr01" "clemeja01" "colliji01" "connoro01" "corcoto01" "crigelo01" "crossla01" "crossmo01"
 [1031] "dahlebi01" "davisge01" "davisha01" "delahed01" "donovpa01" "doyleja01" "duffyhu01" "elybo01"   "farredu01" "frasech01"
 [1041] "ganzech01" "gleaski01" "griffcl01" "hallmbi01" "hallmbi01" "jennihu01" "jonesfi01" "keelewi01" "kellejo01" "kittrma01"
 [1051] "lajoina01" "longhe01"  "lowebo01"  "mcfared01" "mcfared01" "mcgrajo01" "mcguide01" "mcphebi01" "nashbi01"  "nichoki01"
 [1061] "oconnja01" "orthal01"  "peitzhe01" "pfefffr01" "powelja01" "quinnjo02" "robinwi01" "ryanji01"  "seymocy01" "smithel01"
 [1071] "smithge01" "tanneje01" "tennefr02" "tucketo01" "vanhage01" "wagneho01" "wallabo01" "warnejo01" "youngcy01" "zimmech01"
 [1081] "anderjo01" "anderjo01" "becklja01" "bowerfr01" "brownto01" "burkeje01" "chancfr01" "clarkfr01" "clemeja01" "colliji01"
 [1091] "corcoto01" "crigelo01" "crossla01" "crossmo01" "dahlebi01" "dalyto01"  "davisge01" "davisha01" "davisha01" "delahed01"
 [1101] "donovbi01" "donovpa01" "doyleja01" "doyleja01" "duffyhu01" "elybo01"   "farredu01" "frasech01" "gleaski01" "griffcl01"
 [1111] "hallmbi01" "hickmch01" "jennihu01" "jonesfi01" "keelewi01" "kellejo01" "kittrma01" "lajoina01" "longhe01"  "lowebo01" 
 [1121] "maulal01"  "mcfared01" "mcgrajo01" "mcguide01" "mcphebi01" "nashbi01"  "nichoki01" "oconnja01" "orthal01"  "peitzhe01"
 [1131] "powelja01" "quinnjo02" "robinwi01" "ryanji01"  "seymocy01" "sheckji01" "smithel01" "smithge01" "tanneje01" "tennefr02"
 [1141] "thompsa01" "tucketo01" "tucketo01" "vanhage01" "wagneho01" "wallabo01" "warnejo01" "weyhigu01" "youngcy01" "zimmech01"
 [1151] "anderjo01" "barrysh01" "becklja01" "bowerfr01" "burkeje01" "chancfr01" "clarkfr01" "colliji01" "corcoto01" "crawfsa01"
 [1161] "crigelo01" "crossla01" "crossla01" "crossmo01" "dahlebi01" "dalyto01"  "davisge01" "davisha01" "delahed01" "donovpa01"
 [1171] "doyleja01" "duffyhu01" "elberki01" "elybo01"   "farredu01" "frasech01" "gleaski01" "griffcl01" "hickmch01" "jennihu01"
 [1181] "jonesfi01" "keelewi01" "kellejo01" "kittrma01" "kittrma01" "lajoina01" "leachto01" "longhe01"  "lowebo01"  "mcfared01"
 [1191] "mcgrajo01" "mcguide01" "mcguide01" "mcphebi01" "nichoki01" "oconnja01" "orthal01"  "peitzhe01" "powelja01" "quinnjo02"
 [1201] "robinwi01" "ryanji01"  "schreos01" "schreos01" "seymocy01" "sheckji01" "smithel01" "sullibi03" "tanneje01" "tennefr02"
 [1211] "tucketo01" "vanhage01" "wagneho01" "wallabo01" "warnejo01" "weyhigu01" "youngcy01" "zimmech01" "zimmech01" "barrysh01"
 [1221] "becklja01" "bowerfr01" "burkeje01" "chancfr01" "clarkfr01" "colliji01" "corcoto01" "crawfsa01" "crigelo01" "crossla01"
 [1231] "crossla01" "crossmo01" "dahlebi01" "dalyto01"  "davisge01" "delahed01" "donovpa01" "doyleja01" "duffyhu01" "elybo01"  
 [1241] "farredu01" "frasech01" "gleaski01" "griffcl01" "hickmch01" "jennihu01" "jonesfi01" "keelewi01" "kellejo01" "lajoina01"
 [1251] "leachto01" "longhe01"  "lowebo01"  "mcfared01" "mcgrajo01" "mcguide01" "murphda02" "nichoki01" "oconnja01" "orthal01" 
 [1261] "peitzhe01" "powelja01" "quinnjo02" "quinnjo02" "robinwi01" "ryanji01"  "sheckji01" "smithel01" "smithel01" "sullibi03"
 [1271] "tanneje01" "tennefr02" "vanhage01" "wagneho01" "wallabo01" "warnejo01" "youngcy01" "zimmech01" "anderjo01" "bresnro01"
 [1281] "colliji01" "crigelo01" "crossla01" "davisha01" "duffyhu01" "elberki01" "elybo01"   "foremfr01" "frasech01" "gleaski01"
 [1291] "griffcl01" "jonesda01" "jonesfi01" "lajoina01" "mcgrajo01" "mooreea01" "planked01" "quinnjo02" "robinwi01" "schreos01"
 [1301] "seymocy01" "sullibi03" "youngcy01" "barrysh01" "becklja01" "bowerfr01" "burkeje01" "chancfr01" "clarkfr01" "corcoto01"
 [1311] "crawfsa01" "crossmo01" "dahlebi01" "dalyto01"  "davisge01" "delahed01" "delahji01" "donovbi01" "donovpa01" "doyleja01"
 [1321] "elybo01"   "farredu01" "hallmbi01" "hickmch01" "hugheto01" "jennihu01" "keelewi01" "kellejo01" "kittrma01" "leachto01"
 [1331] "longhe01"  "lowebo01"  "mathech01" "mcfared01" "mcguide01" "nichoki01" "oconnja01" "orthal01"  "peitzhe01" "powelja01"
 [1341] "sheckji01" "smithel01" "tanneje01" "tennefr02" "vanhage01" "wagneho01" "wallabo01" "warnejo01" "zimmech01" "anderjo01"
 [1351] "bresnro01" "burkeje01" "colliji01" "crigelo01" "crossla01" "crossmo01" "dalyto01"  "davisge01" "davisha01" "delahed01"
 [1361] "doyleja01" "elberki01" "elybo01"   "gleaski01" "griffcl01" "hickmch01" "hickmch01" "jonesfi01" "kellejo01" "lajoina01"
 [1371] "mcfared01" "mcgrajo01" "mcguide01" "mooreea01" "mullige01" "murphda02" "orthal01"  "planked01" "powelja01" "robinwi01"
 [1381] "ryanji01"  "schreos01" "schreos01" "seymocy01" "sullibi03" "wallabo01" "warnejo01" "youngcy01" "barrysh01" "becklja01"
 [1391] "bowerfr01" "bresnro01" "brownge01" "brownge01" "chancfr01" "clarkfr01" "corcoto01" "crawfsa01" "dahlebi01" "donovbi01"
 [1401] "donovpa01" "dooinre01" "doyleja01" "eversjo01" "farredu01" "frasech01" "hallmbi01" "jennihu01" "jonesda01" "keelewi01"
 [1411] "kellejo01" "kittrma01" "leachto01" "longhe01"  "lowebo01"  "mathech01" "mcgrajo01" "oconnja01" "peitzhe01" "schaege01"
 [1421] "seymocy01" "sheckji01" "tanneje01" "tennefr02" "tinkejo01" "vanhage01" "wagneho01" "zimmech01" "anderjo01" "bendech01"
 [1431] "burkeje01" "colliji01" "crawfsa01" "crigelo01" "crossla01" "crossmo01" "dalyto01"  "davisha01" "delahed01" "donovbi01"
 [1441] "elberki01" "elberki01" "farredu01" "griffcl01" "hickmch01" "hugheto01" "jonesfi01" "keelewi01" "kittrma01" "lajoina01"
 [1451] "longhe01"  "longhe01"  "mcfared01" "mcguide01" "mooreea01" "mullige01" "murphda02" "oconnja01" "orthal01"  "planked01"
 [1461] "powelja01" "ryanji01"  "schreos01" "sullibi03" "tanneje01" "wallabo01" "youngcy01" "barrysh01" "becklja01" "bowerfr01"
 [1471] "bresnro01" "brownge01" "brownmo01" "chancfr01" "clarkfr01" "corcoto01" "dahlebi01" "dalyto01"  "donovpa01" "dooinre01"
 [1481] "doyleja01" "eversjo01" "frasech01" "gleaski01" "hallmbi01" "jonesda01" "kellejo01" "kittrma01" "leachto01" "lowebo01" 
 [1491] "mathech01" "peitzhe01" "seymocy01" "sheckji01" "tennefr02" "tinkejo01" "vanhage01" "wagneho01" "warnejo01" "zimmech01"
 [1501] "altroni01" "anderjo01" "bendech01" "burkeje01" "colliji01" "crawfsa01" "crigelo01" "crossla01" "crossmo01" "davisge01"
 [1511] "davisha01" "donovbi01" "donovpa01" "elberki01" "farredu01" "hickmch01" "hickmch01" "hugheto01" "hugheto01" "jonesfi01"
 [1521] "keelewi01" "kittrma01" "lajoina01" "lowebo01"  "mcfared01" "mcguide01" "mooreea01" "mullige01" "murphda02" "orthal01" 
 [1531] "orthal01"  "planked01" "powelja01" "schreos01" "sullibi03" "tanneje01" "turnete01" "wallabo01" "youngcy01" "barrysh01"
 [1541] "barrysh01" "becklja01" "bowerfr01" "bresnro01" "brownge01" "brownmo01" "chancfr01" "clarkfr01" "corcoto01" "dahlebi01"
 [1551] "delahji01" "dooinre01" "doyleja01" "eversjo01" "frasech01" "gleaski01" "jonesda01" "kellejo01" "leachto01" "mageesh01"
 [1561] "mathech01" "nichoki01" "peitzhe01" "schulfr01" "seymocy01" "sheckji01" "tennefr02" "tinkejo01" "wagneho01" "warnejo01"
 [1571] "altroni01" "anderjo01" "anderjo01" "bendech01" "burkeje01" "chaseha01" "cobbty01"  "colliji01" "crawfsa01" "crigelo01"
 [1581] "crossla01" "crossmo01" "davisge01" "davisha01" "donovbi01" "elberki01" "hickmch01" "hickmch01" "hugheto01" "jonesfi01"
 [1591] "keelewi01" "kittrma01" "lajoina01" "lowebo01"  "mcfared01" "mcguide01" "mooreea01" "mullige01" "murphda02" "orthal01" 
 [1601] "planked01" "powelja01" "schaege01" "schreos01" "sullibi03" "tanneje01" "turnete01" "wallabo01" "warnejo01" "youngcy01"
 [1611] "amesre01"  "barrysh01" "barrysh01" "becklja01" "bowerfr01" "bresnro01" "brownge01" "brownmo01" "chancfr01" "clarkfr01"
 [1621] "corcoto01" "dahlebi01" "delahji01" "dooinre01" "doolami01" "eversjo01" "frasech01" "gleaski01" "hofmaso01" "kellejo01"
 [1631] "leachto01" "mageesh01" "mathech01" "mcbrige01" "mcbrige01" "nichoki01" "peitzhe01" "schulfr01" "seymocy01" "sheckji01"
 [1641] "tennefr02" "tinkejo01" "wagneho01" "warnejo01" "altroni01" "anderjo01" "bendech01" "chaseha01" "cobbty01"  "colliji01"
 [1651] "crawfsa01" "crossla01" "crossmo01" "davisge01" "davisha01" "donovbi01" "elberki01" "hickmch01" "hugheto01" "jonesda01"
 [1661] "jonesfi01" "keelewi01" "kittrma01" "lajoina01" "lowebo01"  "mcguide01" "mullige01" "murphda02" "oconnja01" "orthal01" 
 [1671] "planked01" "powelja01" "schaege01" "schreos01" "sullibi03" "tanneje01" "turnete01" "wallabo01" "warnejo01" "warnejo01"
 [1681] "youngcy01" "amesre01"  "barrysh01" "barrysh01" "becklja01" "bowerfr01" "bresnro01" "brownge01" "brownmo01" "chancfr01"
 [1691] "clarkfr01" "corcoto01" "dahlebi01" "delahji01" "dooinre01" "doolami01" "eversjo01" "frasech01" "gleaski01" "hofmaso01"
 [1701] "kellejo01" "leachto01" "mageesh01" "mathech01" "mcbrige01" "peitzhe01" "schulfr01" "seymocy01" "seymocy01" "sheckji01"
 [1711] "tennefr02" "tinkejo01" "wagneho01" "altroni01" "anderjo01" "bendech01" "chaseha01" "cobbty01"  "colliji01" "colliji01"
 [1721] "crawfsa01" "crigelo01" "crossla01" "crossmo01" "davisge01" "davisha01" "delahji01" "delahji01" "donovbi01" "elberki01"
 [1731] "hickmch01" "hugheto01" "jonesda01" "jonesfi01" "keelewi01" "lajoina01" "mcfared01" "milancl01" "mullige01" "murphda02"
 [1741] "oconnja01" "orthal01"  "planked01" "powelja01" "schaege01" "schreos01" "sullibi03" "tanneje01" "turnete01" "wallabo01"
 [1751] "warnejo01" "youngcy01" "amesre01"  "barrysh01" "becklja01" "bowerfr01" "bresnro01" "brownge01" "brownmo01" "chancfr01"
 [1761] "clarkfr01" "corcoto01" "dahlebi01" "dooinre01" "doolami01" "doylela01" "eversjo01" "gleaski01" "hofmaso01" "koneted01"
 [1771] "leachto01" "mageesh01" "mathech01" "paskedo01" "schulfr01" "seymocy01" "sheckji01" "tennefr02" "tinkejo01" "wagneho01"
 [1781] "anderjo01" "bendech01" "bushdo01"  "chaseha01" "cicoted01" "cobbty01"  "collied01" "colliji01" "crawfsa01" "crigelo01"
 [1791] "davisge01" "davisha01" "delahji01" "donovbi01" "elberki01" "hickmch01" "hugheto01" "johnswa01" "jonesda01" "jonesfi01"
 [1801] "keelewi01" "lajoina01" "mcbrige01" "milancl01" "mullige01" "murphda02" "orthal01"  "planked01" "powelja01" "schaege01"
 [1811] "schreos01" "speaktr01" "sullibi03" "turnete01" "wallabo01" "warnejo01" "youngcy01" "barrysh01" "barrysh01" "bowerfr01"
 [1821] "bresnro01" "brownge01" "brownmo01" "chancfr01" "clarkfr01" "dahlebi01" "dooinre01" "doolami01" "doylela01" "eversjo01"
 [1831] "frasech01" "herzobu01" "hofmaso01" "kellejo01" "koneted01" "leachto01" "mageesh01" "mathech01" "paskedo01" "schulfr01"
 [1841] "seymocy01" "sheckji01" "tennefr02" "tinkejo01" "wagneho01" "austiji01" "bendech01" "brownge01" "bushdo01"  "chaseha01"
 [1851] "cicoted01" "cobbty01"  "collied01" "crawfsa01" "crigelo01" "davisge01" "davisha01" "delahji01" "delahji01" "elberki01"
 [1861] "hoopeha01" "johnswa01" "jonesda01" "keelewi01" "lajoina01" "mcbrige01" "milancl01" "mullige01" "murphda02" "planked01"
 [1871] "powelja01" "schaege01" "schaege01" "speaktr01" "sullibi03" "turnete01" "wallabo01" "youngcy01" "amesre01"  "bowerfr01"
 [1881] "bresnro01" "brownmo01" "chancfr01" "clarkfr01" "dahlebi01" "dooinre01" "doolami01" "doylela01" "eversjo01" "herzobu01"
 [1891] "hofmaso01" "koneted01" "leachto01" "mageesh01" "marquru01" "mathech01" "merklfr01" "mooreea01" "paskedo01" "sallesl01"
 [1901] "schulfr01" "seymocy01" "sheckji01" "tennefr02" "tinkejo01" "wagneho01" "wheatza01" "ainsmed01" "austiji01" "bendech01"
 [1911] "brownge01" "bushdo01"  "chaseha01" "cicoted01" "cobbty01"  "collied01" "collish01" "crawfsa01" "crigelo01" "davisha01"
 [1921] "delahji01" "donovbi01" "elberki01" "gardnla01" "hoopeha01" "johnswa01" "jonesda01" "lajoina01" "mcbrige01" "mcinnst01"
 [1931] "milancl01" "mullige01" "murphda02" "planked01" "quinnja01" "schaege01" "speaktr01" "sullibi03" "turnete01" "wallabo01"
 [1941] "youngcy01" "adamsba01" "amesre01"  "bresnro01" "brownmo01" "chancfr01" "clarkfr01" "daubeja01" "dooinre01" "doolami01"
 [1951] "doylela01" "eversjo01" "herzobu01" "hofmaso01" "koneted01" "leachto01" "mageesh01" "mathech01" "merklfr01" "mooreea01"
 [1961] "paskedo01" "schulfr01" "seymocy01" "sheckji01" "tinkejo01" "wagneho01" "wheatza01" "wilsoar01" "ainsmed01" "austiji01"
 [1971] "bendech01" "bushdo01"  "chaseha01" "cicoted01" "cobbty01"  "collied01" "collish01" "crawfsa01" "davisha01" "delahji01"
 [1981] "donovbi01" "elberki01" "gardnla01" "hamilea01" "hoopeha01" "hugheto01" "johnswa01" "jonesda01" "lajoina01" "mcbrige01"
 [1991] "mcinnst01" "milancl01" "mullige01" "murphda02" "olsoniv01" "planked01" "powelja01" "quinnja01" "schaege01" "speaktr01"
 [2001] "strunam01" "sullibi03" "turnete01" "wallabo01" "adamsba01" "alexape01" "amesre01"  "bresnro01" "brownmo01" "careyma01"
 [2011] "chancfr01" "clarkfr01" "daubeja01" "dooinre01" "doolami01" "doylela01" "eversjo01" "gowdyha01" "herzobu01" "herzobu01"
 [2021] "hofmaso01" "koneted01" "leachto01" "mageesh01" "marquru01" "mathech01" "merklfr01" "mooreea01" "paskedo01" "sallesl01"
 [2031] "schulfr01" "severha01" "sheckji01" "tennefr02" "tinkejo01" "wagneho01" "wheatza01" "wilsoar01" "wingoiv01" "ainsmed01"
 [2041] "austiji01" "bendech01" "bushdo01"  "chaseha01" "cicoted01" "cobbty01"  "collied01" "collish01" "crawfsa01" "delahji01"
 [2051] "fournja01" "gardnla01" "hamilea01" "hoopeha01" "hugheto01" "johnswa01" "jonesda01" "lajoina01" "mcbrige01" "mcinnst01"
 [2061] "milancl01" "mullige01" "murphda02" "olsoniv01" "oneilst01" "peckiro01" "planked01" "powelja01" "schaege01" "schalra01"
 [2071] "speaktr01" "strunam01" "sullibi03" "turnete01" "veachbo01" "wallabo01" "adamsba01" "alexape01" "amesre01"  "bentoru01"
 [2081] "bresnro01" "burnsge01" "careyma01" "daubeja01" "dooinre01" "doolami01" "doylela01" "eversjo01" "gowdyha01" "herzobu01"
 [2091] "hofmaso01" "hofmaso01" "koneted01" "leachto01" "leachto01" "mageesh01" "maranra01" "marquru01" "mathech01" "merklfr01"
 [2101] "mooreea01" "paskedo01" "raganpa01" "rixeyep01" "sallesl01" "schulfr01" "severha01" "sheckji01" "stengca01" "tinkejo01"
 [2111] "wagneho01" "wheatza01" "willicy01" "wilsoar01" "wingoiv01" "ainsmed01" "austiji01" "bendech01" "bushdo01"  "bushjo01" 
 [2121] "chaseha01" "chaseha01" "cicoted01" "cobbty01"  "collied01" "collish01" "crawfsa01" "daussho01" "fournja01" "gardnla01"
 [2131] "hamilea01" "hoopeha01" "johnswa01" "lajoina01" "leibone01" "mcbrige01" "mcinnst01" "milancl01" "murphda02" "olsoniv01"
 [2141] "oneilst01" "peckiro01" "planked01" "schaege01" "schalra01" "schanwa01" "speaktr01" "strunam01" "turnete01" "veachbo01"
 [2151] "wallabo01" "adamsba01" "alexape01" "amesre01"  "bresnro01" "brownmo01" "burnsge01" "careyma01" "daubeja01" "dooinre01"
 [2161] "doolami01" "doylela01" "eversjo01" "grohhe01"  "herzobu01" "hofmaso01" "koneted01" "leachto01" "mageesh01" "mannle01" 
 [2171] "maranra01" "marquru01" "mathech01" "merklfr01" "paskedo01" "raganpa01" "sallesl01" "schulfr01" "seymocy01" "sheckji01"
 [2181] "sheckji01" "stengca01" "tinkejo01" "wagneho01" "wheatza01" "willicy01" "wilsoar01" "wingoiv01" "ainsmed01" "austiji01"
 [2191] "bendech01" "bressru01" "burnsge02" "bushdo01"  "bushjo01"  "chaseha01" "cicoted01" "cobbty01"  "collied01" "collish01"
 [2201] "crawfsa01" "daussho01" "faberre01" "fournja01" "gardnla01" "hamilea01" "heilmha01" "hoopeha01" "johnswa01" "lajoina01"
 [2211] "leibone01" "mcbrige01" "mcinnst01" "milancl01" "olsoniv01" "oneilst01" "peckiro01" "pennohe01" "planked01" "schalra01"
 [2221] "schanwa01" "scottev01" "shawkbo01" "speaktr01" "strunam01" "turnete01" "veachbo01" "wallabo01" "brownmo01" "chaseha01"
 [2231] "delahji01" "doolami01" "hofmaso01" "jonesda01" "mooreea01" "mullige01" "murphda02" "quinnja01" "rawlijo01" "roushed01"
 [2241] "tinkejo01" "wilsoar01" "adamsba01" "alexape01" "amesre01"  "bentoru01" "bresnro01" "burnsge01" "careyma01" "coopewi01"
 [2251] "daubeja01" "doakbi01"  "dooinre01" "doylela01" "elberki01" "eversjo01" "gerbewa01" "gonzami01" "gowdyha01" "grohhe01" 
 [2261] "herzobu01" "koneted01" "leachto01" "mageesh01" "mannle01"  "maranra01" "marquru01" "mathech01" "merklfr01" "myershy01"
 [2271] "paskedo01" "pfeffje01" "raganpa01" "rawlijo01" "sallesl01" "schulfr01" "snydefr01" "stengca01" "wagneho01" "wheatza01"
 [2281] "willicy01" "wingoiv01" "ainsmed01" "austiji01" "bressru01" "burnsge02" "bushdo01"  "cicoted01" "cobbty01"  "collied01"
 [2291] "collish01" "crawfsa01" "daussho01" "faberre01" "fournja01" "gardnla01" "hamilea01" "hoopeha01" "jacobba01" "jacobba01"
 [2301] "jamiech01" "johnswa01" "lajoina01" "leibone01" "leibone01" "mcbrige01" "mcinnst01" "milancl01" "oneilst01" "peckiro01"
 [2311] "pippwa01"  "ruthba01"  "schalra01" "schanwa01" "scottev01" "severha01" "sislege01" "speaktr01" "strunam01" "turnete01"
 [2321] "veachbo01" "bendech01" "brownmo01" "chaseha01" "doolami01" "doolami01" "hofmaso01" "koneted01" "mannle01"  "planked01"
 [2331] "quinnja01" "rawlijo01" "roushed01" "schaege01" "tinkejo01" "wilsoar01" "adamsba01" "alexape01" "bancrda01" "bentoru01"
 [2341] "bresnro01" "burnsge01" "careyma01" "coopewi01" "daubeja01" "doakbi01"  "dooinre01" "doylela01" "eversjo01" "gerbewa01"
 [2351] "gonzami01" "gowdyha01" "grohhe01"  "herzobu01" "hornsro01" "leachto01" "mageesh01" "maranra01" "marquru01" "mathech01"
 [2361] "meadole01" "merklfr01" "myershy01" "olsoniv01" "paskedo01" "pfeffje01" "raganpa01" "rixeyep01" "sallesl01" "schulfr01"
 [2371] "smithsh01" "snydefr01" "stengca01" "wagneho01" "wheatza01" "willicy01" "wingoiv01" "ainsmed01" "austiji01" "burnsge02"
 [2381] "bushdo01"  "bushjo01"  "cicoted01" "cobbty01"  "collied01" "collish01" "crawfsa01" "daussho01" "faberre01" "fournja01"
 [2391] "gardnla01" "heilmha01" "hoopeha01" "jamiech01" "johnswa01" "judgejo01" "lajoina01" "leibone01" "maysca01"  "mcbrige01"
 [2401] "mcinnst01" "milancl01" "mogrige01" "oneilst01" "peckiro01" "picinva01" "pippwa01"  "planked01" "ricesa01"  "ruthba01" 
 [2411] "schalra01" "schanwa01" "scottev01" "severha01" "shawkbo01" "sislege01" "speaktr01" "strunam01" "turnete01" "veachbo01"
 [2421] "alexape01" "amesre01"  "bancrda01" "bentoru01" "burnsge01" "careyma01" "chaseha01" "coopewi01" "daubeja01" "doakbi01" 
 [2431] "doolami01" "doolami01" "doylela01" "eversjo01" "gonzami01" "gowdyha01" "grohhe01"  "herzobu01" "herzobu01" "hornsro01"
 [2441] "kellyge01" "koneted01" "mageesh01" "mannle01"  "maranra01" "marquru01" "meadole01" "merklfr01" "merklfr01" "mitchcl01"
 [2451] "myershy01" "olsoniv01" "paskedo01" "pfeffje01" "raganpa01" "rixeyep01" "roushed01" "roushed01" "schulfr01" "schulfr01"
 [2461] "smithja03" "smithsh01" "snydefr01" "stengca01" "wagneho01" "wheatza01" "willicy01" "wilsoar01" "wilsoar01" "wingoiv01"
 [2471] "ainsmed01" "austiji01" "burnsge02" "bushdo01"  "bushjo01"  "cicoted01" "cobbty01"  "collied01" "collish01" "crawfsa01"
 [2481] "daussho01" "duganjo01" "ehmkeho01" "faberre01" "gardnla01" "heilmha01" "hoopeha01" "jacobba01" "jamiech01" "johnswa01"
 [2491] "judgejo01" "leibone01" "maysca01"  "mcbrige01" "mcinnst01" "milancl01" "mogrige01" "oneilst01" "peckiro01" "pippwa01" 
 [2501] "ricesa01"  "ruthba01"  "schalra01" "schanwa01" "scottev01" "severha01" "shawkbo01" "sislege01" "speaktr01" "strunam01"
 [2511] "turnete01" "veachbo01" "alexape01" "amesre01"  "bancrda01" "bentoru01" "burnsge01" "careyma01" "chaseha01" "coopewi01"
 [2521] "daubeja01" "doakbi01"  "doylela01" "eversjo01" "eversjo01" "gonzami01" "gowdyha01" "grimebu01" "grohhe01"  "herzobu01"
 [2531] "hornsro01" "koneted01" "mageesh01" "mageesh01" "mannle01"  "maranra01" "marquru01" "meadole01" "merklfr01" "mitchcl01"
 [2541] "myershy01" "nehfar01"  "oeschjo01" "olsoniv01" "paskedo01" "pfeffje01" "rawlijo01" "rixeyep01" "roushed01" "sallesl01"
 [2551] "schulfr01" "schulfr01" "smithja03" "smithsh01" "snydefr01" "stengca01" "wagneho01" "wheatza01" "willicy01" "wilsoar01"
 [2561] "wingoiv01" "ainsmed01" "austiji01" "burnsge02" "bushdo01"  "bushjo01"  "cicoted01" "cobbty01"  "collied01" "collish01"
 [2571] "daussho01" "duganjo01" "dykesji01" "fournja01" "gardnla01" "gerbewa01" "heilmha01" "hoopeha01" "jamiech01" "johnswa01"
 [2581] "jonessa01" "judgejo01" "leibone01" "maysca01"  "mcbrige01" "mcinnst01" "milancl01" "mogrige01" "oneilst01" "peckiro01"
 [2591] "perkicy01" "picinva01" "pippwa01"  "ruthba01"  "schalra01" "schanwa01" "schulfr01" "scottev01" "severha01" "sislege01"
 [2601] "speaktr01" "strunam01" "turnete01" "veachbo01" "amesre01"  "bancrda01" "bressru01" "burnsge01" "careyma01" "chaseha01"
 [2611] "coopewi01" "daubeja01" "doakbi01"  "doolami01" "doylela01" "gonzami01" "grimebu01" "grimmch01" "grohhe01"  "heathcl01"
 [2621] "herzobu01" "hornsro01" "koneted01" "leachto01" "mageesh01" "mannle01"  "marquru01" "meadole01" "merklfr01" "myershy01"
 [2631] "nehfar01"  "oeschjo01" "ofarrbo01" "olsoniv01" "paskedo01" "raganpa01" "rawlijo01" "roushed01" "sherdbi01" "smithja03"
 [2641] "snydefr01" "stengca01" "wallabo01" "wheatza01" "willicy01" "wilsoar01" "wingoiv01" "ainsmed01" "austiji01" "burnsge02"
 [2651] "bushdo01"  "cicoted01" "cobbty01"  "collied01" "collish01" "daussho01" "duganjo01" "ehmkeho01" "faberre01" "flagsir01"
 [2661] "gardnla01" "gerbewa01" "heilmha01" "hoopeha01" "jacobba01" "johnswa01" "jonessa01" "judgejo01" "leibone01" "maysca01" 
 [2671] "mcinnst01" "milancl01" "oneilst01" "peckiro01" "pennohe01" "perkicy01" "picinva01" "pippwa01"  "quinnja01" "ricesa01" 
 [2681] "ruelmu01"  "ruthba01"  "schalra01" "schanwa01" "scottev01" "severha01" "shawkbo01" "sislege01" "speaktr01" "strunam01"
 [2691] "strunam01" "turnete01" "veachbo01" "adamsba01" "alexape01" "bancrda01" "bentoru01" "bressru01" "burnsge01" "careyma01"
 [2701] "chaseha01" "coopewi01" "daubeja01" "doakbi01"  "doylela01" "friscfr01" "gonzami01" "gowdyha01" "grimebu01" "grohhe01" 
 [2711] "hamilea01" "heathcl01" "herzobu01" "herzobu01" "hornsro01" "kellyge01" "koneted01" "mageesh01" "mannle01"  "mannle01" 
 [2721] "maranra01" "meadole01" "merklfr01" "myershy01" "nehfar01"  "ofarrbo01" "olsoniv01" "paskedo01" "pfeffje01" "rawlijo01"
 [2731] "roushed01" "sallesl01" "smithja03" "smithsh01" "snydefr01" "snydefr01" "stengca01" "wheatza01" "willicy01" "wilsoar01"
 [2741] "wingoiv01" "ainsmed01" "austiji01" "burnsge02" "burnsge02" "bushdo01"  "bushjo01"  "cicoted01" "cobbty01"  "collied01"
 [2751] "collish01" "daussho01" "duganjo01" "dykesji01" "ehmkeho01" "faberre01" "flagsir01" "gardnla01" "gerbewa01" "heilmha01"
 [2761] "hoopeha01" "jacobba01" "jamiech01" "johnswa01" "jonessa01" "judgejo01" "leibone01" "maysca01"  "mcinnst01" "milancl01"
 [2771] "myattgl01" "oneilst01" "peckiro01" "pennohe01" "perkicy01" "picinva01" "pippwa01"  "quinnja01" "ricesa01"  "ruelmu01" 
 [2781] "ruthba01"  "schalra01" "schanwa01" "scottev01" "severha01" "shawkbo01" "sislege01" "speaktr01" "strunam01" "strunam01"
 [2791] "veachbo01" "zachato01" "adamsba01" "alexape01" "bancrda01" "bancrda01" "bentoru01" "burnsge01" "careyma01" "carlsha01"
 [2801] "coopewi01" "daubeja01" "doakbi01"  "doylela01" "fordho01"  "fournja01" "fribebe01" "friscfr01" "gowdyha01" "grimebu01"
 [2811] "grimmch01" "grohhe01"  "haineje01" "hamilea01" "heathcl01" "herzobu01" "hornsro01" "kellyge01" "koneted01" "luquedo01"
 [2821] "mannle01"  "maranra01" "marquru01" "meadole01" "merklfr01" "mitchcl01" "myershy01" "nehfar01"  "oeschjo01" "ofarrbo01"
 [2831] "olsoniv01" "paskedo01" "pfeffje01" "rawlijo01" "rixeyep01" "roushed01" "sherdbi01" "smithea02" "smithja03" "snydefr01"
 [2841] "stengca01" "traynpi01" "wheatza01" "willicy01" "wingoiv01" "ainsmed01" "austiji01" "burnsge02" "bushdo01"  "bushdo01" 
 [2851] "bushjo01"  "cobbty01"  "collied01" "collish01" "daussho01" "duganjo01" "dykesji01" "ehmkeho01" "faberre01" "flagsir01"
 [2861] "gardnla01" "gerbewa01" "gosligo01" "heilmha01" "hoopeha01" "hoytwa01"  "jacobba01" "jamiech01" "johnswa01" "jonessa01"
 [2871] "judgejo01" "leibone01" "maysca01"  "mcinnst01" "mcmanma01" "milancl01" "millebi02" "mogrige01" "myattgl01" "oneilst01"
 [2881] "peckiro01" "pennohe01" "perkicy01" "picinva01" "pippwa01"  "ricesa01"  "ruelmu01"  "ruthba01"  "schalra01" "schanwa01"
 [2891] "scottev01" "severha01" "shawkbo01" "sislege01" "speaktr01" "strunam01" "uhlege01"  "veachbo01" "zachato01" "adamsba01"
 [2901] "ainsmed01" "alexape01" "bancrda01" "bressru01" "burnsge01" "careyma01" "coopewi01" "daubeja01" "doakbi01"  "fordho01" 
 [2911] "fournja01" "friscfr01" "gowdyha01" "grimebu01" "grimmch01" "grohhe01"  "haineje01" "hamilea01" "heathcl01" "hornsro01"
 [2921] "kellyge01" "koneted01" "koneted01" "luquedo01" "mannle01"  "maranra01" "marquru01" "meadole01" "mitchcl01" "myershy01"
 [2931] "nehfar01"  "oeschjo01" "ofarrbo01" "olsoniv01" "paskedo01" "rawlijo01" "rawlijo01" "rixeyep01" "roushed01" "smithea02"
 [2941] "smithja03" "smithsh01" "snydefr01" "stengca01" "tayloza02" "wheatza01" "willicy01" "wingoiv01" "bluegos01" "burnsge02"
 [2951] "bushdo01"  "bushjo01"  "cobbty01"  "collied01" "collish01" "daussho01" "duganjo01" "duganjo01" "dykesji01" "ehmkeho01"
 [2961] "faberre01" "flagsir01" "gardnla01" "gerbewa01" "gosligo01" "heilmha01" "hoopeha01" "hoytwa01"  "jacobba01" "jamiech01"
 [2971] "johnswa01" "jonessa01" "judgejo01" "leibone01" "maysca01"  "mcinnst01" "mcmanma01" "milancl01" "millebi02" "mogrige01"
 [2981] "oneilst01" "peckiro01" "pennohe01" "perkicy01" "picinva01" "pippwa01"  "quinnja01" "ricesa01"  "ruelmu01"  "ruthba01" 
 [2991] "schalra01" "schanwa01" "scottev01" "severha01" "sewellu01" "shawkbo01" "sislege01" "speaktr01" "strunam01" "uhlege01" 
 [3001] "veachbo01" "zachato01" "adamsba01" "ainsmed01" "alexape01" "bancrda01" "bottoji01" "bressru01" "burnsge01" "careyma01"
 [3011] "carlsha01" "coopewi01" "daubeja01" "doakbi01"  "fordho01"  "fournja01" "fribebe01" "friscfr01" "gowdyha01" "grimebu01"
 [3021] "grimmch01" "grohhe01"  "haineje01" "hamilea01" "hartnga01" "heathcl01" "heathcl01" "hornsro01" "kellyge01" "luquedo01"
 [3031] "mannle01"  "maranra01" "marquru01" "meadole01" "mitchcl01" "myershy01" "nehfar01"  "oeschjo01" "ofarrbo01" "olsoniv01"
 [3041] "pfeffje01" "rawlijo01" "rixeyep01" "roushed01" "sherdbi01" "smithea02" "smithja03" "snydefr01" "stengca01" "traynpi01"
 [3051] "vanceda01" "wheatza01" "willicy01" "wingoiv01" "bluegos01" "burnsge02" "bushjo01"  "cobbty01"  "collied01" "collish01"
 [3061] "daussho01" "duganjo01" "dykesji01" "ehmkeho01" "faberre01" "flagsir01" "gardnla01" "gerbewa01" "gosligo01" "heilmha01"
 [3071] "hoopeha01" "hoytwa01"  "jacobba01" "jamiech01" "johnssy01" "johnswa01" "jonessa01" "judgejo01" "leibone01" "manushe01"
 [3081] "mcmanma01" "millebi02" "mogrige01" "myattgl01" "oneilst01" "peckiro01" "pennohe01" "perkicy01" "picinva01" "pippwa01" 
 [3091] "quinnja01" "ricesa01"  "ruelmu01"  "ruthba01"  "schalra01" "schanwa01" "scottev01" "severha01" "shawkbo01" "speaktr01"
 [3101] "strunam01" "uhlege01"  "veachbo01" "zachato01" "adamsba01" "ainsmed01" "alexape01" "bancrda01" "bentoru01" "bergmo01" 
 [3111] "bottoji01" "bressru01" "burnsge01" "careyma01" "coonejo01" "coopewi01" "daubeja01" "doakbi01"  "fordho01"  "fournja01"
 [3121] "fribebe01" "friscfr01" "gowdyha01" "grimebu01" "grimmch01" "grohhe01"  "haineje01" "hamilea01" "hartnga01" "heathcl01"
 [3131] "hornsro01" "jackstr01" "kellyge01" "luquedo01" "mannle01"  "maranra01" "marquru01" "mcinnst01" "meadole01" "mitchcl01"
 [3141] "myershy01" "nehfar01"  "oeschjo01" "ofarrbo01" "olsoniv01" "pfeffje01" "rawlijo01" "rixeyep01" "roushed01" "sherdbi01"
 [3151] "smithbo02" "smithea02" "smithja03" "snydefr01" "stengca01" "tayloza02" "traynpi01" "vanceda01" "wheatza01" "willicy01"
 [3161] "wilsoji01" "wingoiv01" "bluegos01" "burnsge02" "bushjo01"  "cobbty01"  "collied01" "collish01" "duganjo01" "dykesji01"
 [3171] "ehmkeho01" "faberre01" "flagsir01" "gardnla01" "gerbewa01" "gosligo01" "heilmha01" "hoopeha01" "hoytwa01"  "jacobba01"
 [3181] "jamiech01" "johnswa01" "jonessa01" "judgejo01" "leibone01" "lyonste01" "manushe01" "marbefi01" "mcmanma01" "millebi02"
 [3191] "mogrige01" "myattgl01" "oneilst01" "peckiro01" "pennohe01" "perkicy01" "picinva01" "pippwa01"  "quinnja01" "ricesa01" 
 [3201] "ruelmu01"  "ruthba01"  "schalra01" "schanwa01" "scottev01" "severha01" "sewellu01" "shawkbo01" "simmoal01" "sislege01"
 [3211] "smithsh01" "speaktr01" "uhlege01"  "veachbo01" "whiteea01" "zachato01" "alexape01" "bancrda01" "bottoji01" "bressru01"
 [3221] "burnsge01" "careyma01" "carlsha01" "coonejo01" "coopewi01" "cuyleki01" "daubeja01" "doakbi01"  "fordho01"  "fournja01"
 [3231] "fribebe01" "friscfr01" "gonzami01" "gowdyha01" "grimebu01" "grimmch01" "grohhe01"  "haineje01" "hartnga01" "heathcl01"
 [3241] "hornsro01" "jackstr01" "kellyge01" "luquedo01" "mannle01"  "maranra01" "maysca01"  "mcinnst01" "meadole01" "mitchcl01"
 [3251] "myershy01" "nehfar01"  "ofarrbo01" "rixeyep01" "roushed01" "sherdbi01" "smithbo02" "smithea02" "smithea02" "smithja03"
 [3261] "snydefr01" "stengca01" "tayloza02" "theveto01" "traynpi01" "vanceda01" "wheatza01" "willicy01" "wilsoji01" "wingoiv01"
 [3271] "bluegos01" "burnsge02" "bushjo01"  "cobbty01"  "collied01" "daussho01" "duganjo01" "dykesji01" "ehmkeho01" "faberre01"
 [3281] "flagsir01" "gehrilo01" "gerbewa01" "gosligo01" "grovele01" "heilmha01" "hoopeha01" "hoytwa01"  "jacobba01" "jamiech01"
 [3291] "johnswa01" "jonessa01" "judgejo01" "leibone01" "lyonste01" "manushe01" "mcmanma01" "millebi02" "myattgl01" "oneilst01"
 [3301] "peckiro01" "pennohe01" "perkicy01" "picinva01" "pippwa01"  "ricesa01"  "ruelmu01"  "ruffire01" "ruthba01"  "schalra01"
 [3311] "schanwa01" "scottev01" "scottev01" "severha01" "severha01" "sewellu01" "shawkbo01" "simmoal01" "sislege01" "smithsh01"
 [3321] "speaktr01" "uhlege01"  "veachbo01" "walberu01" "whiteea01" "zachato01" "alexape01" "bancrda01" "bentola01" "bottoji01"
 [3331] "bressru01" "burnsge01" "bushgu01"  "careyma01" "carlsha01" "coonejo01" "coopewi01" "cuyleki01" "fordho01"  "fournja01"
 [3341] "fribebe01" "fribebe01" "friscfr01" "gonzami01" "gonzami01" "gowdyha01" "grimebu01" "grimmch01" "grohhe01"  "haineje01"
 [3351] "hartnga01" "heathcl01" "hornsro01" "jackstr01" "kellyge01" "luquedo01" "mannle01"  "maranra01" "mcinnst01" "meadole01"
 [3361] "mitchcl01" "nehfar01"  "ofarrbo01" "rawlijo01" "rixeyep01" "roushed01" "sherdbi01" "smithbo02" "smithea02" "smithja03"
 [3371] "snydefr01" "tayloza02" "theveto01" "traynpi01" "vanceda01" "wheatza01" "willicy01" "wilsoji01" "wingoiv01" "bergmo01" 
 [3381] "bluegos01" "burnsge02" "cobbty01"  "collied01" "duganjo01" "dykesji01" "faberre01" "flagsir01" "gehrich01" "gehrilo01"
 [3391] "gerbewa01" "gosligo01" "grovele01" "heilmha01" "hoytwa01"  "jacobba01" "jacobba01" "jamiech01" "johnswa01" "judgejo01"
 [3401] "lazzeto01" "lyonste01" "manushe01" "mcmanma01" "millebi02" "millebi02" "myattgl01" "myerbu01"  "peckiro01" "pennohe01"
 [3411] "perkicy01" "ricesa01"  "ruelmu01"  "ruffire01" "ruthba01"  "schalra01" "schanwa01" "scottev01" "severha01" "sewellu01"
 [3421] "simmoal01" "sislege01" "smithsh01" "speaktr01" "thomato02" "uhlege01"  "whiteea01" "zachato01" "alexape01" "bancrda01"
 [3431] "bentola01" "bottoji01" "bressru01" "careyma01" "careyma01" "carlsha01" "coonejo01" "cronijo01" "cuyleki01" "fitzsfr01"
 [3441] "fordho01"  "fournja01" "fribebe01" "friscfr01" "gonzami01" "grimebu01" "grimmch01" "haineje01" "hartnga01" "heathcl01"
 [3451] "hornsro01" "jackstr01" "kellyge01" "lucasre01" "luquedo01" "mannle01"  "maranra01" "maysca01"  "mcinnst01" "meadole01"
 [3461] "mitchcl01" "ofarrbo01" "ottme01"   "picinva01" "pippwa01"  "rawlijo01" "rixeyep01" "rootch01"  "roushed01" "sherdbi01"
 [3471] "smithbo02" "smithea02" "smithja03" "snydefr01" "tayloza02" "theveto01" "traynpi01" "vanceda01" "wanerpa01" "wheatza01"
 [3481] "willicy01" "wilsoji01" "bergmo01"  "bluegos01" "burnsge02" "cobbty01"  "collied01" "duganjo01" "dykesji01" "ehmkeho01"
 [3491] "flagsir01" "foxxji01"  "gehrich01" "gehrilo01" "gerbewa01" "gosligo01" "grovele01" "hadlebu01" "heilmha01" "hoytwa01" 
 [3501] "hudliwi01" "jacobba01" "jacobba01" "jamiech01" "jonessa01" "judgejo01" "lazzeto01" "lyonste01" "manushe01" "mcmanma01"
 [3511] "millebi02" "myattgl01" "myerbu01"  "myerbu01"  "oneilst01" "peckiro01" "pennohe01" "perkicy01" "quinnja01" "ricesa01" 
 [3521] "ruelmu01"  "ruffire01" "ruthba01"  "schanwa01" "sewellu01" "simmoal01" "sislege01" "speaktr01" "thomato02" "uhlege01" 
 [3531] "walberu01" "westsa01"  "wheatza01" "whiteea01" "alexape01" "allenet01" "bancrda01" "bentola01" "bottoji01" "bressru01"
 [3541] "bushgu01"  "careyma01" "carlsha01" "cuyleki01" "fitzsfr01" "fordho01"  "fournja01" "fribebe01" "friscfr01" "gonzami01"
 [3551] "grimebu01" "grimmch01" "haineje01" "hartnga01" "heathcl01" "hornsro01" "jackstr01" "kellyge01" "lucasre01" "luquedo01"
 [3561] "mannle01"  "mannle01"  "meadole01" "ofarrbo01" "ottme01"   "picinva01" "pippwa01"  "rixeyep01" "rootch01"  "roushed01"
 [3571] "sherdbi01" "smithbo02" "smithea02" "smithja03" "snydefr01" "tayloza02" "tayloza02" "theveto01" "traynpi01" "vanceda01"
 [3581] "wanerll01" "wanerpa01" "willicy01" "wilsoji01" "bergmo01"  "bluegos01" "burnsge02" "cobbty01"  "cronijo01" "duganjo01"
 [3591] "durocle01" "dykesji01" "faberre01" "flagsir01" "foxxji01"  "gehrich01" "gehrilo01" "gerbewa01" "gosligo01" "grovele01"
 [3601] "hadlebu01" "heilmha01" "hoytwa01"  "hudliwi01" "jamiech01" "jonessa01" "judgejo01" "kressre01" "lazzeto01" "lyonste01"
 [3611] "macfada01" "manushe01" "mcmanma01" "millebi02" "myattgl01" "myerbu01"  "pennohe01" "quinnja01" "ricesa01"  "ruelmu01" 
 [3621] "ruffire01" "russeja01" "ruthba01"  "schanwa01" "sewellu01" "simmoal01" "speaktr01" "thomato02" "uhlege01"  "walberu01"
 [3631] "westsa01"  "whiteea01" "alexape01" "allenet01" "bancrda01" "bartedi01" "bentola01" "bottoji01" "bressru01" "bushgu01" 
 [3641] "careyma01" "cuyleki01" "davissp01" "fitzsfr01" "fordho01"  "fribebe01" "friscfr01" "gonzami01" "grimebu01" "grimmch01"
 [3651] "haineje01" "hartnga01" "heathcl01" "hemslro01" "hornsro01" "jackstr01" "kellyge01" "kleinch01" "lucasre01" "luquedo01"
 [3661] "mannle01"  "maranra01" "mitchcl01" "nehfar01"  "ofarrbo01" "ofarrbo01" "ottme01"   "picinva01" "pippwa01"  "rixeyep01"
 [3671] "rootch01"  "roushed01" "sherdbi01" "sislege01" "smithbo02" "smithea02" "smithea02" "smithja03" "tayloza02" "theveto01"
 [3681] "traynpi01" "vanceda01" "wanerll01" "wanerpa01" "willicy01" "wilsoji01" "wilsoji01" "bergmo01"  "bluegos01" "cronijo01"
 [3691] "dickebi01" "durocle01" "dykesji01" "faberre01" "ferreri01" "ferrewe01" "foxxji01"  "gehrich01" "gehrilo01" "gerbewa01"
 [3701] "gosligo01" "grovele01" "hadlebu01" "heilmha01" "hoytwa01"  "hudliwi01" "jamiech01" "jonessa01" "judgejo01" "kressre01"
 [3711] "laryly01"  "lazzeto01" "lyonste01" "macfada01" "manushe01" "marbefi01" "mcmanma01" "millebi02" "myattgl01" "myerbu01" 
 [3721] "pennohe01" "perkicy01" "quinnja01" "ricesa01"  "ruelmu01"  "ruffire01" "russeja01" "ruthba01"  "schanwa01" "sewellu01"
 [3731] "simmoal01" "thomato02" "uhlege01"  "walberu01" "westsa01"  "whiteea01" "allenet01" "bancrda01" "bartedi01" "bentola01"
 [3741] "bottoji01" "bressru01" "bushgu01"  "coonejo01" "cuyleki01" "davissp01" "duganjo01" "fitzsfr01" "flagsir01" "fordho01" 
 [3751] "fribebe01" "friscfr01" "gonzami01" "grimebu01" "grimmch01" "haineje01" "heathcl01" "hemslro01" "hornsro01" "hubbeca01"
 [3761] "jackstr01" "johnssy01" "kellyge01" "kleinch01" "lucasre01" "luquedo01" "maranra01" "mitchcl01" "ofarrbo01" "ottme01"  
 [3771] "picinva01" "rixeyep01" "rootch01"  "roushed01" "sherdbi01" "sislege01" "smithbo02" "smithea02" "tayloza02" "tayloza02"
 [3781] "theveto01" "traynpi01" "vanceda01" "wanerll01" "wanerpa01" "willicy01" "wilsoji01" "bergmo01"  "bluegos01" "browncl01"
 [3791] "chapmbe01" "coffmdi01" "cramedo01" "cronijo01" "dickebi01" "dykesji01" "ferreri01" "ferrewe01" "foxxji01"  "gehrich01"
 [3801] "gehrilo01" "gosligo01" "gosligo01" "grovele01" "hadlebu01" "hardeme01" "haywora01" "hudliwi01" "jamiech01" "jonessa01"
 [3811] "judgejo01" "kressre01" "kuheljo01" "laryly01"  "lazzeto01" "lyonste01" "macfada01" "manushe01" "manushe01" "marbefi01"
 [3821] "mcmanma01" "mcnaier01" "millebi02" "myattgl01" "myerbu01"  "pennohe01" "ricesa01"  "ruelmu01"  "ruffire01" "russeja01"
 [3831] "ruthba01"  "schanwa01" "sewellu01" "simmoal01" "thomato02" "uhlege01"  "walberu01" "westsa01"  "whiteea01" "allenet01"
 [3841] "bartedi01" "bentola01" "bottoji01" "bressru01" "bushgu01"  "cuccito01" "cuyleki01" "davissp01" "durocle01" "fitzsfr01"
 [3851] "flagsir01" "fordho01"  "frencla01" "fribebe01" "friscfr01" "grimebu01" "grimmch01" "haineje01" "hartnga01" "heathcl01"
 [3861] "heilmha01" "hemslro01" "hornsro01" "hubbeca01" "jackstr01" "johnssy01" "kellyge01" "kellyge01" "kleinch01" "lopezal01"
 [3871] "lucasre01" "luquedo01" "mancugu01" "maranra01" "ofarrbo01" "ottme01"   "rixeyep01" "rootch01"  "sislege01" "smithbo02"
 [3881] "tayloza02" "theveto01" "traynpi01" "vanceda01" "wanerll01" "wanerpa01" "wilsoji01" "zachato01" "applilu01" "bluegos01"
 [3891] "bridgto01" "browncl01" "chapmbe01" "coffmdi01" "cramedo01" "cronijo01" "dickebi01" "dykesji01" "faberre01" "ferreri01"
 [3901] "ferrewe01" "foxxji01"  "gehrich01" "gehrilo01" "gosligo01" "grovele01" "hadlebu01" "hardeme01" "haywora01" "hudliwi01"
 [3911] "judgejo01" "kressre01" "kuheljo01" "laryly01"  "lazzeto01" "macfada01" "manushe01" "marbefi01" "mcmanma01" "mcmanma01"
 [3921] "mcnaier01" "millebi02" "myattgl01" "myerbu01"  "pennohe01" "ricesa01"  "ruelmu01"  "ruelmu01"  "ruffire01" "russeja01"
 [3931] "ruthba01"  "schanwa01" "sewellu01" "simmoal01" "thomato02" "uhlege01"  "walberu01" "walkege02" "westsa01"  "whiteea01"
 [3941] "allenet01" "bartedi01" "bentola01" "bottoji01" "bressru01" "bushgu01"  "cuccito01" "cuyleki01" "davissp01" "derripa01"
 [3951] "durocle01" "fitzsfr01" "fordho01"  "frencla01" "fribebe01" "friscfr01" "grimebu01" "grimmch01" "hartnga01" "heathcl01"
 [3961] "hemslro01" "hermabi01" "hornsro01" "hubbeca01" "jackstr01" "johnssi01" "johnssy01" "jurgebi01" "kleinch01" "lombaer01"
 [3971] "lopezal01" "lucasre01" "mancugu01" "maranra01" "mitchcl01" "ofarrbo01" "ottme01"   "rootch01"  "roushed01" "smithbo02"
 [3981] "theveto01" "traynpi01" "vanceda01" "wanerll01" "wanerpa01" "wilsoji01" "zachato01" "allenjo02" "applilu01" "bergmo01" 
 [3991] "bluegos01" "bridgto01" "browncl01" "chapmbe01" "cramedo01" "cronijo01" "crosefr01" "dickebi01" "dykesji01" "ferreri01"
 [4001] "ferrewe01" "foxxji01"  "gehrich01" "gehrilo01" "gosligo01" "grovele01" "hadlebu01" "hardeme01" "haywora01" "hoagmy01" 
 [4011] "hudliwi01" "jonessa01" "judgejo01" "kressre01" "kressre01" "kuheljo01" "laryly01"  "lazzeto01" "lyonste01" "manushe01"
 [4021] "marbefi01" "mcmanma01" "mcnaier01" "millebi02" "myattgl01" "myerbu01"  "pennohe01" "ricesa01"  "ruelmu01"  "ruffire01"
 [4031] "ruthba01"  "sewellu01" "simmoal01" "uhlege01"  "walberu01" "walkege02" "westsa01"  "whiteea01" "wyattwh01" "allenet01"
 [4041] "bartedi01" "bentola01" "bottoji01" "bressru01" "bushgu01"  "cuccito01" "cuyleki01" "davissp01" "derripa01" "durocle01"
 [4051] "fitzsfr01" "fordho01"  "frencla01" "fribebe01" "friscfr01" "grimmch01" "hackst01"  "hartnga01" "hemslro01" "hermabi01"
 [4061] "hornsro01" "hubbeca01" "jackstr01" "johnssi01" "johnssy01" "jurgebi01" "kellyge01" "kleinch01" "lombaer01" "lopezal01"
 [4071] "lucasre01" "mancugu01" "maranra01" "medwijo01" "ofarrbo01" "ottme01"   "picinva01" "rootch01"  "theveto01" "traynpi01"
 [4081] "vanceda01" "waltebu01" "wanerll01" "wanerpa01" "warnelo01" "wilsoji01" "zachato01" "allenjo02" "applilu01" "bergmo01" 
 [4091] "bluegos01" "bridgto01" "browncl01" "chapmbe01" "cramedo01" "cronijo01" "crosefr01" "dickebi01" "dykesji01" "ferreri01"
 [4101] "ferreri01" "ferrewe01" "finnelo01" "foxxji01"  "gehrich01" "gehrilo01" "gosligo01" "grovele01" "hadlebu01" "hardeme01"
 [4111] "haywora01" "hemslro01" "higgipi01" "jonessa01" "judgejo01" "kressre01" "kuheljo01" "laryly01"  "lazzeto01" "lyonste01"
 [4121] "manushe01" "marbefi01" "mcmanma01" "mcnaier01" "millebi02" "myattgl01" "myerbu01"  "ricesa01"  "rowesc01"  "ruelmu01" 
 [4131] "ruffire01" "ruthba01"  "sewellu01" "simmoal01" "walberu01" "walkedi02" "walkege02" "waltebu01" "westsa01"  "whiteea01"
 [4141] "allenet01" "bartedi01" "bentola01" "bottoji01" "bushgu01"  "cuccito01" "cuyleki01" "davissp01" "derripa01" "durocle01"
 [4151] "durocle01" "fitzsfr01" "frencla01" "freylo01"  "friscfr01" "grimmch01" "hackst01"  "hartnga01" "hemslro01" "hermabi01"
 [4161] "hornsro01" "hubbeca01" "jackstr01" "johnssi01" "judgejo01" "jurgebi01" "kleinch01" "lombaer01" "lopezal01" "lucasre01"
 [4171] "mancugu01" "maranra01" "medwijo01" "ofarrbo01" "ottme01"   "picinva01" "rootch01"  "theveto01" "traynpi01" "wanerll01"
 [4181] "wanerpa01" "warnelo01" "wilsoji01" "applilu01" "bergmo01"  "bergmo01"  "bluegos01" "bridgto01" "chapmbe01" "coffmdi01"
 [4191] "cramedo01" "cronijo01" "crosefr01" "dickebi01" "dietrbi01" "dykesji01" "ferreri01" "ferrewe01" "finnelo01" "foxxji01" 
 [4201] "gehrich01" "gehrilo01" "gosligo01" "hadlebu01" "hardeme01" "hayesfr01" "haywora01" "hemslro01" "higgipi01" "hoagmy01" 
 [4211] "hudliwi01" "jonessa01" "kressre01" "kuheljo01" "laryly01"  "lazzeto01" "lyonste01" "manushe01" "marbefi01" "mcnaier01"
 [4221] "millebi02" "myattgl01" "myerbu01"  "newsobo01" "osterfr01" "ricesa01"  "rowesc01"  "ruelmu01"  "ruffire01" "ruthba01" 
 [4231] "sewellu01" "simmoal01" "walkege02" "waltebu01" "westsa01"  "whiteea01" "allenet01" "bartedi01" "bottoji01" "bushgu01" 
 [4241] "cuccito01" "cuyleki01" "daviscu01" "davissp01" "derripa01" "durocle01" "fitzsfr01" "frencla01" "freylo01"  "friscfr01"
 [4251] "galanau01" "grimmch01" "hackst01"  "hartnga01" "hermabi01" "hoytwa01"  "hubbeca01" "jackstr01" "johnssi01" "jurgebi01"
 [4261] "kleinch01" "leebi02"   "leonadu02" "lombaer01" "lopezal01" "lucasre01" "mancugu01" "mcmanma01" "medwijo01" "ofarrbo01"
 [4271] "ofarrbo01" "ottme01"   "staintu01" "theveto01" "traynpi01" "waltebu01" "wanerll01" "wanerpa01" "warnelo01" "wilsoji01"
 [4281] "allenjo02" "applilu01" "bergmo01"  "bluegos01" "bridgto01" "chapmbe01" "cramedo01" "cronijo01" "crosefr01" "dahlgba01"
 [4291] "dickebi01" "dietrbi01" "dykesji01" "ferreri01" "ferrewe01" "finnelo01" "foxxji01"  "gehrich01" "gehrilo01" "gosligo01"
 [4301] "grovele01" "hadlebu01" "hardeme01" "haywora01" "hemslro01" "higgipi01" "hoagmy01"  "hudliwi01" "kenneve01" "kressre01"
 [4311] "kuheljo01" "laryly01"  "laryly01"  "lazzeto01" "leeth01"   "lyonste01" "manushe01" "mcnaier01" "millebi02" "moseswa01"
 [4321] "myerbu01"  "newsobo01" "rowesc01"  "ruffire01" "sewellu01" "simmoal01" "walkege02" "westsa01"  "whiteea01" "allenet01"
 [4331] "bartedi01" "bottoji01" "bushgu01"  "cavarph01" "cuccito01" "cuyleki01" "cuyleki01" "daviscu01" "davissp01" "derripa01"
 [4341] "durocle01" "frencla01" "freylo01"  "friscfr01" "galanau01" "hackst01"  "hartnga01" "hermabi01" "hoytwa01"  "hubbeca01"
 [4351] "jackstr01" "johnssy01" "jurgebi01" "kleinch01" "leebi02"   "lombaer01" "lopezal01" "lucasre01" "macfada01" "mancugu01"
 [4361] "maranra01" "medwijo01" "muellra01" "ottme01"   "rootch01"  "ruthba01"  "smithbo02" "staintu01" "tayloza02" "theveto01"
 [4371] "traynpi01" "waltebu01" "wanerll01" "wanerpa01" "warnelo01" "wilsoji01" "zachato01" "allenjo02" "applepe01" "applilu01"
 [4381] "bergmo01"  "bluegos01" "bottoji01" "bridgto01" "chapmbe01" "chapmbe01" "cramedo01" "cronijo01" "crosefr01" "dahlgba01"
 [4391] "dickebi01" "dykesji01" "ferreri01" "ferrewe01" "finnelo01" "foxxji01"  "gehrich01" "gehrilo01" "gosligo01" "grovele01"
 [4401] "hadlebu01" "hardeme01" "hayesfr01" "haywora01" "hemslro01" "higgipi01" "hoagmy01"  "kenneve01" "kressre01" "kuheljo01"
 [4411] "laryly01"  "lazzeto01" "lyonste01" "manushe01" "mcnaier01" "moseswa01" "myattgl01" "myerbu01"  "newsobo01" "osterfr01"
 [4421] "rowesc01"  "ruffire01" "sewellu01" "simmoal01" "thomato02" "walkedi02" "walkege02" "westsa01"  "whiteea01" "allenet01"
 [4431] "allenet01" "bartedi01" "cavarph01" "coonejo01" "cuccito01" "cuyleki01" "daviscu01" "davissp01" "derripa01" "durocle01"
 [4441] "frencla01" "freylo01"  "friscfr01" "galanau01" "grimmch01" "hackst01"  "hartnga01" "hermabi01" "hubbeca01" "jackstr01"
 [4451] "jurgebi01" "kleinch01" "kleinch01" "leebi02"   "lombaer01" "lopezal01" "lucasre01" "macfada01" "mancugu01" "medwijo01"
 [4461] "mizejo01"  "moorege03" "muellra01" "ottme01"   "staintu01" "theveto01" "waltebu01" "wanerll01" "wanerpa01" "warnelo01"
 [4471] "wilsoji01" "allenet01" "allenjo02" "applepe01" "applilu01" "bergmo01"  "bluegos01" "bottoji01" "bridgto01" "chapmbe01"
 [4481] "chapmbe01" "cramedo01" "cronijo01" "crosefr01" "dickebi01" "dykesji01" "fellebo01" "ferreri01" "ferreri01" "ferrewe01"
 [4491] "finnelo01" "foxxji01"  "galehde01" "gehrich01" "gehrilo01" "gosligo01" "grovele01" "hadlebu01" "hardeme01" "hayesfr01"
 [4501] "haywora01" "heathje01" "hemslro01" "higgipi01" "hoagmy01"  "hornsro01" "hudliwi01" "kenneve01" "kuheljo01" "laryly01" 
 [4511] "lazzeto01" "leeth01"   "lyonste01" "mcnaier01" "moseswa01" "myerbu01"  "newsobo01" "ruffire01" "sewellu01" "simmoal01"
 [4521] "tebbebi01" "walkedi02" "walkege02" "westsa01"  "bartedi01" "bushgu01"  "cavarph01" "coonejo01" "cuccito01" "cuyleki01"
 [4531] "davissp01" "derripa01" "durocle01" "frencla01" "freylo01"  "galanau01" "gumbeha01" "hackst01"  "hartnga01" "hermabi01"
 [4541] "hubbeca01" "johnssi01" "jurgebi01" "kleinch01" "leebi02"   "lombaer01" "lopezal01" "lucasre01" "macfada01" "mancugu01"
 [4551] "manushe01" "medwijo01" "mizejo01"  "moorege03" "muellra01" "ottme01"   "rootch01"  "staintu01" "waltebu01" "wanerll01"
 [4561] "wanerpa01" "warnelo01" "wilsoji01" "allenjo02" "applepe01" "applilu01" "bluegos01" "bridgto01" "chapmbe01" "cramedo01"
 [4571] "cronijo01" "crosefr01" "dickebi01" "dykesji01" "fellebo01" "ferreri01" "finnelo01" "foxxji01"  "gehrich01" "gehrilo01"
 [4581] "gosligo01" "grovele01" "hadlebu01" "hardeme01" "hayesfr01" "heathje01" "hemslro01" "higgipi01" "hoagmy01"  "kenneve01"
 [4591] "kressre01" "kuheljo01" "laryly01"  "leeth01"   "leonadu02" "lyonste01" "mcnaier01" "moseswa01" "myerbu01"  "newsobo01"
 [4601] "osterfr01" "ruffire01" "sewellu01" "simmoal01" "tebbebi01" "wagneha01" "walkedi02" "walkege02" "westsa01"  "westsa01" 
 [4611] "whiteea01" "bartedi01" "cavarph01" "coonejo01" "cuccito01" "cuyleki01" "daviscu01" "davissp01" "derripa01" "durocle01"
 [4621] "fitzsfr01" "frencla01" "freylo01"  "galanau01" "gumbeha01" "hackst01"  "hartnga01" "hermabi01" "hubbeca01" "jurgebi01"
 [4631] "kleinch01" "lazzeto01" "leebi02"   "lombaer01" "lopezal01" "macfada01" "mancugu01" "manushe01" "medwijo01" "mizejo01" 
 [4641] "moorege03" "muellra01" "ottme01"   "slaugen01" "staintu01" "staintu01" "waltebu01" "wanerll01" "wanerpa01" "warnelo01"
 [4651] "allenjo02" "applilu01" "bluegos01" "boudrlo01" "bridgto01" "chapmbe01" "cramedo01" "cronijo01" "crosefr01" "dahlgba01"
 [4661] "dickebi01" "fellebo01" "ferreri01" "finnelo01" "foxxji01"  "gehrich01" "grovele01" "hadlebu01" "hardeme01" "hayesfr01"
 [4671] "heathje01" "hemslro01" "higgipi01" "hoagmy01"  "kenneve01" "kressre01" "kuheljo01" "leeth01"   "leonadu02" "lyonste01"
 [4681] "mcnaier01" "moseswa01" "myerbu01"  "newsobo01" "osterfr01" "pottene01" "rowesc01"  "ruffire01" "tebbebi01" "troutdi01"
 [4691] "vernomi01" "walkedi02" "walkege02" "westsa01"  "willite01" "bartedi01" "cavarph01" "coonejo01" "cuccito01" "daviscu01"
 [4701] "davissp01" "derripa01" "durocle01" "elliobo01" "frencla01" "freylo01"  "galanau01" "gumbeha01" "hackst01"  "hartnga01"
 [4711] "hermabi01" "higbeki01" "hubbeca01" "joosted01" "jurgebi01" "kleinch01" "laryly01"  "leebi02"   "lombaer01" "lopezal01"
 [4721] "macfada01" "majesha01" "mancugu01" "masiph01"  "medwijo01" "mizejo01"  "moorege03" "muellra01" "nichobi01" "ottme01"  
 [4731] "rootch01"  "simmoal01" "slaugen01" "staintu01" "walkedi02" "waltebu01" "wanerll01" "wanerpa01" "warnelo01" "applilu01"
 [4741] "bartedi01" "boudrlo01" "bridgto01" "chapmbe01" "cramedo01" "cronijo01" "crosefr01" "dahlgba01" "dickebi01" "dietrbi01"
 [4751] "fellebo01" "ferreri01" "finnelo01" "foxxji01"  "gehrich01" "grovele01" "hardeme01" "hayesfr01" "heathje01" "hemslro01"
 [4761] "higgipi01" "hoagmy01"  "kennebo01" "kenneve01" "kressre01" "kuheljo01" "laryly01"  "leeth01"   "leonadu02" "lyonste01"
 [4771] "mcnaier01" "moseswa01" "myerbu01"  "newsobo01" "osterfr01" "pottene01" "rowesc01"  "ruffire01" "simmoal01" "swiftbo01"
 [4781] "tebbebi01" "wagneha01" "walkege02" "westsa01"  "willite01" "cavarph01" "coonejo01" "cuccito01" "cuccito01" "davissp01"
 [4791] "derripa01" "durocle01" "elliobo01" "frencla01" "freylo01"  "galanau01" "gumbeha01" "hackst01"  "hartnga01" "hermabi01"
 [4801] "higbeki01" "hoppjo01"  "hubbeca01" "joosted01" "jurgebi01" "kleinch01" "leebi02"   "lombaer01" "lopezal01" "lopezal01"
 [4811] "mancugu01" "masiph01"  "medwijo01" "medwijo01" "mizejo01"  "moorege03" "nichobi01" "ottme01"   "reesepe01" "shouncl01"
 [4821] "slaugen01" "walkedi02" "waltebu01" "wanerll01" "wanerpa01" "warnelo01" "wyattwh01" "applilu01" "boudrlo01" "chapmbe01"
 [4831] "chapmbe01" "cramedo01" "cronijo01" "crosefr01" "dickebi01" "fellebo01" "ferreri01" "ferreri01" "finnelo01" "foxxji01" 
 [4841] "galehde01" "gehrich01" "hayesfr01" "heathje01" "hemslro01" "higgipi01" "hoagmy01"  "kennebo01" "kuheljo01" "leeth01"  
 [4851] "leonadu02" "lyonste01" "mcnaier01" "moseswa01" "myerbu01"  "newhoha01" "newsobo01" "rowesc01"  "ruffire01" "staintu01"
 [4861] "swiftbo01" "tebbebi01" "troutdi01" "valoel01"  "vernomi01" "wagneha01" "walkege02" "willite01" "bartedi01" "cavarph01"
 [4871] "coonejo01" "coopewa01" "dahlgba01" "dahlgba01" "daviscu01" "davissp01" "derripa01" "elliobo01" "freylo01"  "galanau01"
 [4881] "gumbeha01" "hackst01"  "hartnga01" "hermabi01" "higbeki01" "hoppjo01"  "hubbeca01" "joosted01" "jurgebi01" "kleinch01"
 [4891] "laniema01" "leebi02"   "lombaer01" "lopezal01" "majesha01" "mancugu01" "masiph01"  "mcculcl01" "medwijo01" "mizejo01" 
 [4901] "moorege03" "nichobi01" "ottme01"   "reesepe01" "slaugen01" "walkedi02" "waltebu01" "wanerll01" "wanerll01" "wanerpa01"
 [4911] "warnelo01" "wyattwh01" "applilu01" "boudrlo01" "bridgto01" "cramedo01" "cronijo01" "crosefr01" "dickebi01" "ferreri01"
 [4921] "finnelo01" "foxxji01"  "galehde01" "hardeme01" "hayesfr01" "hayesfr01" "heathje01" "heganji01" "hemslro01" "higgipi01"
 [4931] "hoagmy01"  "kennebo01" "kuheljo01" "lyonste01" "mcnaier01" "mcnaier01" "moseswa01" "newhoha01" "newsobo01" "ruffire01"
 [4941] "stephve01" "swiftbo01" "swiftbo01" "tebbebi01" "troutdi01" "truckvi01" "valoel01"  "vernomi01" "wagneha01" "westsa01" 
 [4951] "willite01" "wynnea01"  "bartedi01" "cavarph01" "coonejo01" "coopewa01" "cuccito01" "dahlgba01" "daviscu01" "derripa01"
 [4961] "elliobo01" "foxxji01"  "freylo01"  "galanau01" "gumbeha01" "hackst01"  "hemslro01" "hermabi01" "higbeki01" "hoppjo01" 
 [4971] "hubbeca01" "johnssi01" "joosted01" "jurgebi01" "leebi02"   "lombaer01" "lopezal01" "lowrepe01" "mancugu01" "masiph01" 
 [4981] "mcculcl01" "medwijo01" "mizejo01"  "musiast01" "nichobi01" "northro01" "ottme01"   "reesepe01" "slaugen01" "walkedi02"
 [4991] "walkege02" "waltebu01" "wanerll01" "wanerpa01" "wyattwh01" "applilu01" "boudrlo01" "bridgto01" "cramedo01" "cronijo01"
 [5001] "crosefr01" "cuccito01" "dickebi01" "dietrbi01" "ferreri01" "galehde01" "hayesfr01" "heathje01" "hemslro01" "higgipi01"
 [5011] "kenneve01" "kuheljo01" "leonadu02" "moorege03" "moseswa01" "newhoha01" "pottene01" "simmoal01" "staintu01" "stephve01"
 [5021] "swiftbo01" "troutdi01" "truckvi01" "valoel01"  "vernomi01" "wagneha01" "wynnea01"  "bartedi01" "cavarph01" "coopewa01"
 [5031] "dahlgba01" "daviscu01" "derripa01" "elliobo01" "freylo01"  "galanau01" "hackst01"  "hermabi01" "higbeki01" "hoppjo01" 
 [5041] "joosted01" "jurgebi01" "laniema01" "lombaer01" "lopezal01" "lowrepe01" "mancugu01" "masiph01"  "mcculcl01" "medwijo01"
 [5051] "medwijo01" "muellra01" "musiast01" "nichobi01" "northro01" "ottme01"   "pafkoan01" "rowesc01"  "seminan01" "walkedi02"
 [5061] "walkege02" "waltebu01" "wanerpa01" "wyattwh01" "bakerfl01" "boudrlo01" "cramedo01" "cronijo01" "crosefr01" "cuccito01"
 [5071] "dietrbi01" "ferreri01" "finnelo01" "gromest01" "hardeme01" "hayesfr01" "heathje01" "hemslro01" "higgipi01" "hoagmy01" 
 [5081] "kellge01"  "kuheljo01" "leonadu02" "michaca01" "moorege03" "moseswa01" "newhoha01" "newsobo01" "pottene01" "staintu01"
 [5091] "stephve01" "swiftbo01" "troutdi01" "wagneha01" "wynnea01"  "cavarph01" "coopewa01" "dahlgba01" "daviscu01" "davissp01"
 [5101] "derripa01" "elliobo01" "galanau01" "gumbeha01" "hackst01"  "hamnegr01" "hoppjo01"  "jurgebi01" "laniema01" "leebi02"  
 [5111] "lombaer01" "lopezal01" "mancugu01" "masiph01"  "medwijo01" "miksied01" "muellra01" "musiast01" "nichobi01" "northro01"
 [5121] "osterfr01" "ottme01"   "pafkoan01" "raffeke01" "seminan01" "shouncl01" "walkedi02" "walkege02" "waltebu01" "wanerpa01"
 [5131] "applilu01" "bakerfl01" "boudrlo01" "cramedo01" "crosefr01" "cuccito01" "ferreri01" "finnelo01" "gromest01" "hayesfr01"
 [5141] "hayesfr01" "heathje01" "hoagmy01"  "kellge01"  "kuheljo01" "leeth01"   "leonadu02" "michaca01" "moorege03" "moseswa01"
 [5151] "newhoha01" "newsobo01" "pottene01" "staintu01" "stephve01" "swiftbo01" "troutdi01" "cavarph01" "chapmbe01" "dahlgba01"
 [5161] "daviscu01" "derripa01" "elliobo01" "foxxji01"  "galanau01" "hackst01"  "hoppjo01"  "joosted01" "jurgebi01" "kenneve01"
 [5171] "lockmwh01" "lombaer01" "lopezal01" "lowrepe01" "mancugu01" "masiph01"  "medwijo01" "medwijo01" "nichobi01" "ottme01"  
 [5181] "pafkoan01" "ricede01"  "sauerha01" "schoere01" "seminan01" "walkedi02" "walkege02" "waltebu01" "applilu01" "boudrlo01"
 [5191] "cramedo01" "crosefr01" "dahlgba01" "dickebi01" "eversho01" "fellebo01" "galehde01" "gromest01" "hayesfr01" "hayesfr01"
 [5201] "heathje01" "heathje01" "heganji01" "higgipi01" "higgipi01" "kellge01"  "kellge01"  "kennebo01" "kuheljo01" "lemonbo01"
 [5211] "leonadu02" "lollash01" "majesha01" "michaca01" "moseswa01" "moseswa01" "newhoha01" "newsobo01" "phillda01" "pottene01"
 [5221] "staintu01" "stephve01" "swiftbo01" "tebbebi01" "troutdi01" "truckvi01" "valoel01"  "vernomi01" "wagneha01" "willite01"
 [5231] "woodlge01" "adamsbo03" "cavarph01" "coopewa01" "dicksmu01" "elliobo01" "ennisde01" "freylo01"  "furilca01" "galanau01"
 [5241] "hackst01"  "hattogr01" "hemslro01" "hermabi01" "hermabi01" "higbeki01" "hoppjo01"  "jurgebi01" "lombaer01" "lopezal01"
 [5251] "lowrepe01" "masiph01"  "mcculcl01" "medwijo01" "mizejo01"  "muellra01" "musiast01" "nichobi01" "northro01" "osterfr01"
 [5261] "ottme01"   "pafkoan01" "polleho01" "raffeke01" "reesepe01" "ricede01"  "rowesc01"  "schmijo01" "schoere01" "seminan01"
 [5271] "slaugen01" "thomsbo01" "walkedi02" "waltebu01" "applilu01" "bakerfl01" "berrayo01" "boudrlo01" "cramedo01" "eversho01"
 [5281] "fellebo01" "ferreri01" "galehde01" "heathje01" "heganji01" "joosted01" "kellge01"  "kennebo01" "lemonbo01" "lopezal01"
 [5291] "majesha01" "mastewa02" "michaca01" "moseswa01" "mossle01"  "newhoha01" "phillda01" "robined01" "stephve01" "swiftbo01"
 [5301] "tebbebi01" "tebbebi01" "troutdi01" "truckvi01" "valoel01"  "vernomi01" "wagneha01" "wagneha01" "wertzvi01" "willite01"
 [5311] "wynnea01"  "yosted01"  "adamsbo03" "cavarph01" "coopewa01" "dicksmu01" "elliobo01" "ennisde01" "furilca01" "galanau01"
 [5321] "hackst01"  "hattogr01" "higbeki01" "hodgegi01" "hoppjo01"  "joneswi01" "leonadu02" "lombaer01" "lowrepe01" "masiph01" 
 [5331] "mcculcl01" "medwijo01" "miksied01" "mizejo01"  "muellra01" "musiast01" "nichobi01" "northro01" "osterfr01" "pafkoan01"
 [5341] "polleho01" "reesepe01" "ricede01"  "rowesc01"  "schmijo01" "schoere01" "seminan01" "slaugen01" "snidedu01" "spahnwa01"
 [5351] "thomsbo01" "torgeea01" "walkedi02" "woodlge01" "applilu01" "bakerfl01" "berrayo01" "boudrlo01" "eversho01" "fellebo01"
 [5361] "garvene01" "goodmbi01" "heganji01" "joosted01" "kellge01"  "kennebo01" "kennebo01" "lemonbo01" "majesha01" "mastewa02"
 [5371] "michaca01" "moseswa01" "mossle01"  "newhoha01" "phillda01" "robined01" "stephve01" "swiftbo01" "tebbebi01" "troutdi01"
 [5381] "truckvi01" "valoel01"  "vernomi01" "wagneha01" "wertzvi01" "wightbi01" "willite01" "wynnea01"  "yosted01"  "adamsbo03"
 [5391] "ashburi01" "cavarph01" "coopewa01" "darkal01"  "dicksmu01" "elliobo01" "ennisde01" "freylo01"  "furilca01" "galanau01"
 [5401] "hamnegr01" "hattogr01" "heathje01" "hodgegi01" "hoppjo01"  "joneswi01" "kluszte01" "leonadu02" "lockmwh01" "lowrepe01"
 [5411] "masiph01"  "mcculcl01" "miksied01" "mizejo01"  "musiast01" "nichobi01" "northro01" "pafkoan01" "polleho01" "raffeke01"
 [5421] "reesepe01" "ricede01"  "rowesc01"  "sauerha01" "schmijo01" "schoere01" "seminan01" "simmocu01" "slaugen01" "snidedu01"
 [5431] "spahnwa01" "thomsbo01" "torgeea01" "walkedi02" "wehmehe01" "applilu01" "bakerfl01" "berrayo01" "boonera01" "boudrlo01"
 [5441] "byrneto01" "eversho01" "fellebo01" "foxne01"   "garvene01" "goodmbi01" "grothjo01" "heganji01" "joosted01" "kellge01" 
 [5451] "kennebo01" "kuzavbo01" "lemonbo01" "lollash01" "majesha01" "michaca01" "moseswa01" "mossle01"  "newhoha01" "phillda01"
 [5461] "piercbi02" "robined01" "sievero01" "stephve01" "stobbch01" "swiftbo01" "tebbebi01" "truckvi01" "valoel01"  "vernomi01"
 [5471] "wertzvi01" "wightbi01" "willite01" "woodlge01" "wynnea01"  "yosted01"  "adamsbo03" "ashburi01" "burgesm01" "cavarph01"
 [5481] "coopewa01" "coopewa01" "crandde01" "darkal01"  "dicksmu01" "elliobo01" "ennisde01" "furilca01" "hamnegr01" "hattogr01"
 [5491] "heathje01" "hodgegi01" "hoppjo01"  "hoppjo01"  "joneswi01" "kluszte01" "leonadu02" "lockmwh01" "lowrepe01" "lowrepe01"
 [5501] "masiph01"  "masiph01"  "mcculcl01" "miksied01" "mizejo01"  "muellra01" "muellra01" "musiast01" "nichobi01" "northro01"
 [5511] "pafkoan01" "polleho01" "raffeke01" "reesepe01" "ricede01"  "roberro01" "sauerha01" "sauerha01" "schmijo01" "schoere01"
 [5521] "seminan01" "slaugen01" "snidedu01" "spahnwa01" "thomsbo01" "torgeea01" "walkedi02" "wehmehe01" "applilu01" "bakerfl01"
 [5531] "berrayo01" "boonera01" "boudrlo01" "byrneto01" "dropowa01" "eversho01" "fellebo01" "foxne01"   "garvene01" "ginsbjo01"
 [5541] "goodmbi01" "grothjo01" "heganji01" "joosted01" "kellge01"  "kennebo01" "kuzavbo01" "lemonbo01" "lollash01" "majesha01"
 [5551] "masiph01"  "michaca01" "michaca01" "mizejo01"  "moseswa01" "mossle01"  "newhoha01" "norenir01" "phillda01" "piercbi02"
 [5561] "robined01" "robined01" "shantbo01" "sievero01" "stephve01" "stobbch01" "swiftbo01" "tebbebi01" "troutdi01" "valoel01" 
 [5571] "vernomi01" "vernomi01" "wertzvi01" "wightbi01" "willite01" "woodlge01" "wynnea01"  "yosted01"  "adamsbo03" "adcocjo01"
 [5581] "ashburi01" "bellgu01"  "cavarph01" "coopewa01" "crandde01" "darkal01"  "dicksmu01" "elliobo01" "ennisde01" "furilca01"
 [5591] "hamnegr01" "hattogr01" "hodgegi01" "hoppjo01"  "joneswi01" "kluszte01" "laniema01" "lockmwh01" "lowrepe01" "lowrepe01"
 [5601] "mcculcl01" "miksied01" "muellra01" "musiast01" "nichobi01" "northro01" "northro01" "pafkoan01" "polleho01" "raffeke01"
 [5611] "reesepe01" "ricede01"  "roberro01" "sauerha01" "schmijo01" "schoere01" "seminan01" "simmocu01" "slaugen01" "snidedu01"
 [5621] "spahnwa01" "stalege01" "thomsbo01" "torgeea01" "wehmehe01" "bakerfl01" "berrayo01" "boonera01" "boudrlo01" "busbyji01"
 [5631] "byrneto01" "dropowa01" "eversho01" "fellebo01" "foxne01"   "garvene01" "ginsbjo01" "goodmbi01" "grothjo01" "heganji01"
 [5641] "hoppjo01"  "joosted01" "kellge01"  "kennebo01" "lemonbo01" "lollash01" "majesha01" "mantlmi01" "masiph01"  "maxwech01"
 [5651] "michaca01" "minosmi01" "mizejo01"  "moseswa01" "mossle01"  "norenir01" "phillda01" "piercbi02" "robined01" "shantbo01"
 [5661] "sievero01" "stephve01" "stobbch01" "swiftbo01" "tebbebi01" "troutdi01" "truckvi01" "valoel01"  "vernomi01" "wertzvi01"
 [5671] "willite01" "woodlge01" "wynnea01"  "yosted01"  "adamsbo03" "adcocjo01" "ashburi01" "bellgu01"  "burgesm01" "cavarph01"
 [5681] "coopewa01" "darkal01"  "dicksmu01" "elliobo01" "ennisde01" "furilca01" "hamnegr01" "hattogr01" "hodgegi01" "joneswi01"
 [5691] "kluszte01" "laniema01" "lockmwh01" "lowrepe01" "mayswi01"  "mcculcl01" "mcmilro01" "miksied01" "muellra01" "musiast01"
 [5701] "nichobi01" "pafkoan01" "pafkoan01" "raffeke01" "reesepe01" "ricede01"  "roberro01" "sauerha01" "schoere01" "seminan01"
 [5711] "slaugen01" "snidedu01" "spahnwa01" "stalege01" "thomafr03" "thomsbo01" "torgeea01" "wehmehe01" "willidi02" "bakerfl01"
 [5721] "berrayo01" "boonera01" "busbyji01" "byrneto01" "cervbo01"  "dropowa01" "dropowa01" "eversho01" "fellebo01" "foxne01"  
 [5731] "ginsbjo01" "goodmbi01" "grothjo01" "heganji01" "joosted01" "kellge01"  "kellge01"  "kuennha01" "lemonbo01" "lollash01"
 [5741] "majesha01" "majesha01" "mantlmi01" "masiph01"  "mastewa02" "michaca01" "michaca01" "michaca01" "minosmi01" "mizejo01" 
 [5751] "mossle01"  "niemabo01" "norenir01" "phillda01" "piercbi02" "piersji01" "portebo01" "robined01" "shantbo01" "stephve01"
 [5761] "swiftbo01" "tebbebi01" "truckvi01" "valoel01"  "vernomi01" "wertzvi01" "wertzvi01" "wightbi01" "woodlge01" "wynnea01" 
 [5771] "yosted01"  "adamsbo03" "adcocjo01" "ashburi01" "bellgu01"  "burgesm01" "cavarph01" "coopewa01" "darkal01"  "dicksmu01"
 [5781] "elliobo01" "ennisde01" "frienbo01" "furilca01" "groatdi01" "halldi01"  "hamnegr01" "hattogr01" "hodgegi01" "joneswi01"
 [5791] "klippjo01" "kluszte01" "landrho01" "lockmwh01" "lowrepe01" "matheed01" "mayswi01"  "mcculcl01" "mcmilro01" "miksied01"
 [5801] "musiast01" "nichobi01" "pafkoan01" "polleho01" "postwa01"  "raffeke01" "reesepe01" "ricede01"  "roberro01" "sauerha01"
 [5811] "schoere01" "seminan01" "simmocu01" "slaugen01" "snidedu01" "spahnwa01" "stalege01" "thomsbo01" "torgeea01" "wehmehe01"
 [5821] "willidi02" "bakerfl01" "berrayo01" "boonera01" "boonera01" "brownha01" "busbyji01" "dropowa01" "elliobo01" "elliobo01"
 [5831] "eversho01" "fellebo01" "fordwh01"  "foxne01"   "garvene01" "ginsbjo01" "ginsbjo01" "goodmbi01" "grothjo01" "heganji01"
 [5841] "hoeftbi01" "joosted01" "kellge01"  "kennebo01" "kuennha01" "larsedo01" "lemonbo01" "lollash01" "majesha01" "mantlmi01"
 [5851] "mastewa02" "michaca01" "minosmi01" "mizejo01"  "mossle01"  "niemabo01" "norenir01" "phillda01" "piercbi02" "piersji01"
 [5861] "portebo01" "robined01" "sievero01" "stephve01" "stephve01" "truckvi01" "valoel01"  "vernomi01" "wertzvi01" "willite01"
 [5871] "woodlge01" "wynnea01"  "yosted01"  "adamsbo03" "adcocjo01" "ashburi01" "bellgu01"  "buhlbo01"  "burdele01" "burgesm01"
 [5881] "coopewa01" "crandde01" "darkal01"  "dicksmu01" "ennisde01" "frienbo01" "furilca01" "haddiha01" "hamnegr01" "hattogr01"
 [5891] "hodgegi01" "joneswi01" "klippjo01" "kluszte01" "landrho01" "lockmwh01" "lowrepe01" "matheed01" "mcculcl01" "mcmilro01"
 [5901] "miksied01" "musiast01" "nichobi01" "pafkoan01" "raffeke01" "reesepe01" "ricede01"  "roberro01" "sauerha01" "schoere01"
 [5911] "seminan01" "simmocu01" "slaugen01" "snidedu01" "spahnwa01" "stalege01" "thomafr03" "thomsbo01" "torgeea01" "willidi02"
 [5921] "berrayo01" "boonera01" "busbyji01" "cavarph01" "cervbo01"  "dropowa01" "eversho01" "fordwh01"  "foxne01"   "garvene01"
 [5931] "goodmbi01" "gromest01" "grothjo01" "hattogr01" "heganji01" "hoeftbi01" "kalinal01" "kellge01"  "kellge01"  "kennebo01"
 [5941] "kuennha01" "larsedo01" "lemonbo01" "lollash01" "majesha01" "mantlmi01" "maxwech01" "michaca01" "minosmi01" "mossle01" 
 [5951] "niemabo01" "norenir01" "phillda01" "piercbi02" "piersji01" "portebo01" "powervi01" "robined01" "schmijo01" "sievero01"
 [5961] "skowrbi01" "slaugen01" "stephve01" "stobbch01" "truckvi01" "valoel01"  "vernomi01" "wertzvi01" "wertzvi01" "willite01"
 [5971] "woodlge01" "wynnea01"  "yosted01"  "aaronha01" "adamsbo03" "adcocjo01" "ashburi01" "baileed01" "bankser01" "bellgu01" 
 [5981] "burdele01" "burgesm01" "coopewa01" "crandde01" "darkal01"  "dicksmu01" "ennisde01" "frienbo01" "furilca01" "haddiha01"
 [5991] "halldi01"  "hamnegr01" "hodgegi01" "joneswi01" "kluszte01" "landrho01" "lawve01"   "lockmwh01" "lowrepe01" "matheed01"
 [6001] "mayswi01"  "mcculcl01" "mcmilro01" "miksied01" "musiast01" "nuxhajo01" "pafkoan01" "podrejo01" "postwa01"  "reesepe01"
 [6011] "ricede01"  "roberro01" "sauerha01" "schoere01" "seminan01" "simmocu01" "snidedu01" "spahnwa01" "thomafr03" "thomsbo01"
 [6021] "torgeea01" "wehmehe01" "berrayo01" "boonera01" "boyercl02" "busbyji01" "busbyji01" "byrneto01" "cervbo01"  "donovdi01"
 [6031] "dropowa01" "eversho01" "eversho01" "foileha01" "fordwh01"  "foxne01"   "garvene01" "goodmbi01" "gromest01" "grothjo01"
 [6041] "grothjo01" "hattogr01" "heganji01" "hoeftbi01" "howarel01" "joosted01" "kalinal01" "kellge01"  "kennebo01" "kennebo01"
 [6051] "killeha01" "kuennha01" "laryfr01"  "lemonbo01" "lollash01" "mantlmi01" "maxwech01" "minosmi01" "mossle01"  "mossle01" 
 [6061] "niemabo01" "norenir01" "phillda01" "phillda01" "piercbi02" "piersji01" "portebo01" "powervi01" "robined01" "schmijo01"
 [6071] "sievero01" "skowrbi01" "slaugen01" "stephve01" "torgeea01" "truckvi01" "valoel01"  "vernomi01" "wertzvi01" "willite01"
 [6081] "woodlge01" "woodlge01" "wynnea01"  "yosted01"  "aaronha01" "adamsbo03" "adcocjo01" "ashburi01" "bankser01" "bellgu01" 
 [6091] "boyerke01" "buhlbo01"  "burdele01" "burgesm01" "clemero01" "coopewa01" "crandde01" "darkal01"  "dicksmu01" "ennisde01"
 [6101] "freesge02" "frienbo01" "furilca01" "groatdi01" "haddiha01" "hamnegr01" "hodgegi01" "jacksla01" "joneswi01" "kluszte01"
 [6111] "landrho01" "lawve01"   "lockmwh01" "lowrepe01" "matheed01" "mayswi01"  "mcculcl01" "mcmilro01" "miksied01" "musiast01"
 [6121] "nuxhajo01" "pafkoan01" "podrejo01" "postwa01"  "reesepe01" "ricede01"  "ricede01"  "roberro01" "sauerha01" "schoere01"
 [6131] "seminan01" "snidedu01" "spahnwa01" "thomafr03" "thomsbo01" "torgeea01" "wehmehe01" "adamsbo03" "aparilu01" "berrayo01"
 [6141] "boonera01" "boyercl02" "busbyji01" "byrneto01" "cervbo01"  "colavro01" "donovdi01" "dropowa01" "eversho01" "fordwh01" 
 [6151] "foxne01"   "francti01" "ginsbjo01" "goodmbi01" "grothjo01" "hattogr01" "heganji01" "hoeftbi01" "howarel01" "kalinal01"
 [6161] "kellge01"  "kellge01"  "kennebo01" "killeha01" "kuennha01" "larsedo01" "laryfr01"  "lemonbo01" "lollash01" "mantlmi01"
 [6171] "maxwech01" "minosmi01" "mossle01"  "niemabo01" "pascuca02" "phillda01" "phillda01" "piercbi02" "piersji01" "powervi01"
 [6181] "robined01" "robined01" "sievero01" "skowrbi01" "slaugen01" "slaugen01" "stobbch01" "sturdto01" "torgeea01" "vernomi01"
 [6191] "wertzvi01" "wightbi01" "willidi02" "willite01" "woodlge01" "wynnea01"  "yosted01"  "aaronha01" "adcocjo01" "ashburi01"
 [6201] "baileed01" "bankser01" "bellgu01"  "blasido01" "boyerke01" "buhlbo01"  "burdele01" "burgesm01" "clemero01" "coopewa01"
 [6211] "covinwe01" "crandde01" "darkal01"  "darkal01"  "dicksmu01" "ennisde01" "foileha01" "freesge02" "frienbo01" "furilca01"
 [6221] "groatdi01" "haddiha01" "hamnegr01" "hattogr01" "hodgegi01" "joneswi01" "klinero01" "klippjo01" "kluszte01" "landrho01"
 [6231] "lawve01"   "lockmwh01" "lockmwh01" "matheed01" "mayswi01"  "mazerbi01" "mcmilro01" "miksied01" "musiast01" "nuxhajo01"
 [6241] "pafkoan01" "postwa01"  "reesepe01" "ricede01"  "roberro01" "robinfr02" "sauerha01" "schoere01" "schoere01" "seminan01"
 [6251] "simmocu01" "snidedu01" "spahnwa01" "thomafr03" "thomsbo01" "valoel01"  "wehmehe01" "worthal01" "aparilu01" "berrayo01"
 [6261] "boonera01" "bunniji01" "busbyji01" "busbyji01" "cervbo01"  "colavro01" "donovdi01" "dropowa01" "foxne01"   "francti01"
 [6271] "ginsbjo01" "goodmbi01" "grothjo01" "grothjo01" "heganji01" "heldwo01"  "hoeftbi01" "howarel01" "kalinal01" "kellge01" 
 [6281] "kuennha01" "larsedo01" "laryfr01"  "lollash01" "mantlmi01" "maxwech01" "minosmi01" "mossle01"  "niemabo01" "norenir01"
 [6291] "pascuca02" "phillda01" "phillda01" "piercbi02" "piersji01" "powervi01" "ramospe01" "robinbr01" "shantbo01" "sievero01"
 [6301] "skowrbi01" "slaugen01" "stobbch01" "sturdto01" "torgeea01" "torgeea01" "vernomi01" "wertzvi01" "willidi02" "willidi02"
 [6311] "willite01" "woodlge01" "wynnea01"  "yosted01"  "aaronha01" "adamsbo03" "adcocjo01" "ashburi01" "baileed01" "bankser01"
 [6321] "bellgu01"  "blasido01" "boyerke01" "buhlbo01"  "burdele01" "burgesm01" "clemero01" "coopewa01" "covinwe01" "crandde01"
 [6331] "darkal01"  "drabomo01" "ennisde01" "foileha01" "freesge02" "frienbo01" "furilca01" "groatdi01" "haddiha01" "hamnegr01"
 [6341] "hodgegi01" "jacksla01" "joneswi01" "klinero01" "kluszte01" "landrho01" "lawve01"   "lockmwh01" "matheed01" "mayswi01" 
 [6351] "mazerbi01" "mcdanli01" "mcmilro01" "musiast01" "nuxhajo01" "pafkoan01" "podrejo01" "postwa01"  "reesepe01" "ricede01" 
 [6361] "roberro01" "robinfr02" "sauerha01" "schoere01" "schoere01" "schofdi01" "simmocu01" "snidedu01" "spahnwa01" "thomafr03"
 [6371] "thomsbo01" "thomsbo01" "valoel01"  "wehmehe01" "aparilu01" "berrayo01" "boonera01" "boonera01" "bunniji01" "busbyji01"
 [6381] "callijo01" "cervbo01"  "colavro01" "donovdi01" "dropowa01" "fordwh01"  "foxne01"   "francti01" "francti01" "garvene01"
 [6391] "ginsbjo01" "goodmbi01" "grantmu01" "greenle01" "grothjo01" "heganji01" "heldwo01"  "heldwo01"  "herbera01" "howarel01"
 [6401] "kalinal01" "kuennha01" "laryfr01"  "lauch01"   "lollash01" "mantlmi01" "maxwech01" "mclisca01" "minosmi01" "niemabo01"
 [6411] "pascuca02" "piercbi02" "piersji01" "powervi01" "powervi01" "ramospe01" "robinbr01" "sievero01" "skowrbi01" "slaugen01"
 [6421] "terryra01" "torgeea01" "vernomi01" "willidi02" "willite01" "woodlge01" "wynnea01"  "yosted01"  "aaronha01" "adamsbo03"
 [6431] "adcocjo01" "aloufe01"  "ashburi01" "baileed01" "bankser01" "bellgu01"  "blasido01" "boyerke01" "burdele01" "burgesm01"
 [6441] "cepedor01" "clemero01" "covinwe01" "crandde01" "darkal01"  "darkal01"  "dropowa01" "ennisde01" "fairlro01" "floodcu01"
 [6451] "foileha01" "freesge02" "frienbo01" "furilca01" "groatdi01" "haddiha01" "hamnegr01" "heganji01" "hodgegi01" "jacksla01"
 [6461] "joneswi01" "klinero01" "kluszte01" "landrho01" "lawve01"   "lockmwh01" "matheed01" "mayswi01"  "mazerbi01" "mccormi03"
 [6471] "mcmilro01" "miksied01" "millest01" "musiast01" "norenir01" "nuxhajo01" "pafkoan01" "phillda01" "pinsova01" "podrejo01"
 [6481] "postwa01"  "reesepe01" "ricede01"  "roberro01" "robinfr02" "sauerha01" "schoere01" "schofdi01" "simmocu01" "snidedu01"
 [6491] "spahnwa01" "tayloto02" "thomafr03" "thomsbo01" "valoel01"  "aparilu01" "berrayo01" "boonera01" "boyercl02" "bunniji01"
 [6501] "busbyji01" "callijo01" "cashno01"  "cervbo01"  "colavro01" "donovdi01" "dropowa01" "ennisde01" "fordwh01"  "foxne01"  
 [6511] "francti01" "garvene01" "ginsbjo01" "goodmbi01" "grantmu01" "greenle01" "grothjo01" "hamnegr01" "heldwo01"  "herbera01"
 [6521] "howarel01" "kalinal01" "killeha01" "kluszte01" "kuennha01" "laryfr01"  "lockmwh01" "lollash01" "mantlmi01" "maxwech01"
 [6531] "mclisca01" "minosmi01" "niemabo01" "pappami01" "pascuca02" "perryji01" "piercbi02" "piersji01" "powervi01" "ramospe01"
 [6541] "robinbr01" "shawbo01"  "sievero01" "skowrbi01" "slaugen01" "torgeea01" "wertzvi01" "wilheho01" "willidi02" "willite01"
 [6551] "woodlge01" "wynnea01"  "yosted01"  "aaronha01" "adcocjo01" "aloufe01"  "ashburi01" "baileed01" "bankser01" "bellgu01" 
 [6561] "blasido01" "boyerke01" "buhlbo01"  "burdele01" "burgesm01" "cardwdo01" "cepedor01" "clemero01" "covinwe01" "crandde01"
 [6571] "darkal01"  "fairlro01" "floodcu01" "foileha01" "freesge02" "frienbo01" "furilca01" "groatdi01" "haddiha01" "hamnegr01"
 [6581] "heganji01" "hodgegi01" "jacksla01" "joneswi01" "joneswi01" "klinero01" "kluszte01" "landrho01" "lawve01"   "lockmwh01"
 [6591] "matheed01" "mayele01"  "mayswi01"  "mazerbi01" "mccormi03" "mccovwi01" "mcmilro01" "musiast01" "norenir01" "pafkoan01"
 [6601] "phillda01" "pinsova01" "podrejo01" "postwa01"  "roberro01" "robinfr02" "schofdi01" "snidedu01" "spahnwa01" "tayloto02"
 [6611] "thomafr03" "thomsbo01" "vernomi01" "willsma01" "aparilu01" "barbest01" "berrayo01" "boonera01" "boyercl02" "bunniji01"
 [6621] "busbyji01" "cashno01"  "cervbo01"  "cervbo01"  "colavro01" "dropowa01" "foileha01" "foileha01" "fordwh01"  "foxne01"  
 [6631] "francti01" "freesge02" "ginsbjo01" "goodmbi01" "grantmu01" "greenle01" "halldi01"  "hansero02" "heldwo01"  "herbera01"
 [6641] "howarel01" "kalinal01" "killeha01" "kluszte01" "kuennha01" "laryfr01"  "lollash01" "mantlmi01" "maxwech01" "minchdo01"
 [6651] "minosmi01" "pappami01" "pascuca02" "perryji01" "piercbi02" "piersji01" "powervi01" "ramospe01" "robinbr01" "shawbo01" 
 [6661] "sievero01" "skowrbi01" "thomsbo01" "torgeea01" "valoel01"  "wertzvi01" "willidi02" "willite01" "woodlge01" "wynnea01" 
 [6671] "yosted01"  "aaronha01" "adcocjo01" "aloufe01"  "ashburi01" "baileed01" "bankser01" "bellgu01"  "blasido01" "boyerke01"
 [6681] "buhlbo01"  "burdele01" "burgesm01" "callijo01" "cardele01" "cardwdo01" "cepedor01" "clemero01" "covinwe01" "crandde01"
 [6691] "darkal01"  "darkal01"  "davisto02" "daviswi02" "floodcu01" "frienbo01" "gonzato01" "gonzato01" "groatdi01" "haddiha01"
 [6701] "hodgegi01" "howarfr01" "jacksla01" "joneswi01" "landrho01" "lauch01"   "lawve01"   "matheed01" "mayele01"  "mayswi01" 
 [6711] "mazerbi01" "mccormi03" "mccovwi01" "mcmilro01" "musiast01" "niemabo01" "phillda01" "pinsova01" "podrejo01" "postwa01" 
 [6721] "postwa01"  "ricede01"  "roberro01" "robinfr02" "sadecra01" "santoro01" "schoere01" "schofdi01" "snidedu01" "spahnwa01"
 [6731] "tayloto02" "tayloto02" "thomafr03" "willist02" "willsma01" "adairje01" "aparilu01" "barbest01" "berrayo01" "boyercl02"
 [6741] "brownha01" "bunniji01" "busbyji01" "cashno01"  "cervbo01"  "cervbo01"  "colavro01" "covinwe01" "donovdi01" "foileha01"
 [6751] "fordwh01"  "foxne01"   "francti01" "goodmbi01" "grantmu01" "greenle01" "hansero02" "heldwo01"  "herbera01" "howarel01"
 [6761] "johnsde01" "kaatji01"  "kalinal01" "killeha01" "kluszte01" "laryfr01"  "lollash01" "mantlmi01" "maxwech01" "mcauldi01"
 [6771] "mclisca01" "minchdo01" "minosmi01" "niemabo01" "nuxhajo01" "pappami01" "pascuca02" "perryji01" "phillda01" "piercbi02"
 [6781] "piersji01" "pizarju01" "powervi01" "ramospe01" "ricede01"  "robinbr01" "shawbo01"  "sievero01" "skowrbi01" "terryra01"
 [6791] "thomage01" "wertzvi01" "willidi02" "woodlge01" "yastrca01" "yosted01"  "aaronha01" "adcocjo01" "aloufe01"  "alouma01" 
 [6801] "ashburi01" "baileed01" "bankser01" "bellgu01"  "blasido01" "boyerke01" "buhlbo01"  "burdele01" "burgesm01" "callijo01"
 [6811] "cardele01" "cardwdo01" "cepedor01" "clemero01" "covinwe01" "davisto02" "daviswi02" "ellswdi01" "fairlro01" "floodcu01"
 [6821] "freesge02" "frienbo01" "gibbojo01" "gibsobo01" "gonzato01" "groatdi01" "haddiha01" "hodgegi01" "howarfr01" "jacksla01"
 [6831] "kuennha01" "landrho01" "lauch01"   "maricju01" "matheed01" "mayele01"  "mayswi01"  "mazerbi01" "mccarti01" "mccormi03"
 [6841] "mccovwi01" "mcmilro01" "musiast01" "paganjo01" "pinsova01" "podrejo01" "postwa01"  "robinfr02" "sadecra01" "santoro01"
 [6851] "schoere01" "schofdi01" "simmocu01" "snidedu01" "spahnwa01" "tayloto02" "thomafr03" "thomafr03" "torrejo01" "willibi01"
 [6861] "willist02" "willsma01" "adairje01" "aguirha01" "aparilu01" "azcuejo01" "berrayo01" "boyercl02" "brinked01" "bunniji01"
 [6871] "cashno01"  "clinety01" "colavro01" "donovdi01" "fordwh01"  "foxne01"   "francti01" "fregoji01" "grantmu01" "greenle01"
 [6881] "hansero02" "heldwo01"  "herbera01" "howarel01" "kaatji01"  "kalinal01" "killeha01" "landrho01" "lauch01"   "lollash01"
 [6891] "mantlmi01" "maxwech01" "maxwech01" "mcauldi01" "minchdo01" "pappami01" "pascuca02" "perryji01" "piersji01" "pizarju01"
 [6901] "powelbo01" "powervi01" "ramospe01" "reganph01" "roberro01" "robinbr01" "skowrbi01" "terryra01" "thomage01" "wertzvi01"
 [6911] "willidi02" "woodlge01" "wynnea01"  "yastrca01" "yosted01"  "aaronha01" "adcocjo01" "aloufe01"  "alouma01"  "ashburi01"
 [6921] "baileed01" "bankser01" "bellgu01"  "bellgu01"  "blasido01" "boyerke01" "brocklo01" "buhlbo01"  "burdele01" "burgesm01"
 [6931] "callijo01" "cardele01" "cardwdo01" "cepedor01" "clemero01" "covinwe01" "crandde01" "davisto02" "daviswi02" "ellswdi01"
 [6941] "fairlro01" "farretu01" "floodcu01" "foileha01" "frienbo01" "gibsobo01" "gonzato01" "goodmbi01" "groatdi01" "haddiha01"
 [6951] "hodgegi01" "howarfr01" "jacksla01" "johnske02" "kuennha01" "maricju01" "matheed01" "maxvida01" "mayele01"  "mayswi01" 
 [6961] "mazerbi01" "mccovwi01" "mclisca01" "mcmilro01" "minosmi01" "motama01"  "musiast01" "paganjo01" "piercbi02" "pinsova01"
 [6971] "podrejo01" "postwa01"  "robinfr02" "rojasco01" "santoro01" "schoere01" "schofdi01" "shawbo01"  "sievero01" "simmocu01"
 [6981] "snidedu01" "spahnwa01" "tayloto02" "thomafr03" "torrejo01" "willibi01" "willist02" "willsma01" "woodlge01" "adairje01"
 [6991] "adcocjo01" "aguirha01" "aparilu01" "azcuejo01" "barbest01" "berrayo01" "blasido01" "boyercl02" "brinked01" "bunniji01"
 [7001] "cashno01"  "colavro01" "davalvi01" "donovdi01" "downial01" "drabomo01" "foileha01" "fordwh01"  "foxne01"   "francti01"
 [7011] "freehbi01" "fregoji01" "grantmu01" "greenle01" "hansero02" "heldwo01"  "herbera01" "howarel01" "kaatji01"  "kalinal01"
 [7021] "killeha01" "kirkped01" "landrho01" "lauch01"   "lollash01" "mantlmi01" "maxwech01" "mcauldi01" "minchdo01" "minosmi01"
 [7031] "osteecl01" "pappami01" "pascuca02" "penaor01"  "perryji01" "piersji01" "piersji01" "pizarju01" "powelbo01" "powervi01"
 [7041] "ramospe01" "reganph01" "roberro01" "robinbr01" "seguidi01" "terryra01" "thomage01" "thomage01" "willidi02" "yastrca01"
 [7051] "aaronha01" "aloufe01"  "alouma01"  "bailebo01" "baileed01" "bankser01" "boyerke01" "brocklo01" "buhlbo01"  "burgesm01"
 [7061] "callijo01" "cardele01" "cardwdo01" "cepedor01" "clemero01" "clinety01" "covinwe01" "crandde01" "davisto02" "daviswi02"
 [7071] "ellswdi01" "fairlro01" "farretu01" "floodcu01" "freesge02" "frienbo01" "gibsobo01" "gonzato01" "groatdi01" "harpeto01"
 [7081] "howarfr01" "jacksla01" "johnske02" "kraneed01" "kuennha01" "maricju01" "matheed01" "maxvida01" "mayele01"  "mayswi01" 
 [7091] "mazerbi01" "mccarti01" "mccovwi01" "mclisca01" "mcmilro01" "mcmulke01" "millebo04" "motama01"  "musiast01" "nuxhajo01"
 [7101] "paganjo01" "piersji01" "pinsova01" "podrejo01" "robinfr02" "rojasco01" "rosepe01"  "sadecra01" "santoro01" "schofdi01"
 [7111] "shortch02" "sievero01" "simmocu01" "skowrbi01" "snidedu01" "spahnwa01" "stargwi01" "staubru01" "tayloto02" "thomafr03"
 [7121] "torrejo01" "willibi01" "willsma01" "wynnji01"  "adairje01" "adcocjo01" "aguirha01" "aparilu01" "azcuejo01" "blasido01"
 [7131] "boyercl02" "brinked01" "campabe01" "cashno01"  "colavro01" "davalvi01" "downial01" "fordwh01"  "francti01" "freehbi01"
 [7141] "fregoji01" "grantmu01" "greenle01" "hansero02" "heldwo01"  "hortowi01" "howarel01" "kaatji01"  "kalinal01" "killeha01"
 [7151] "kirkped01" "lauch01"   "lauch01"   "lolicmi01" "mantlmi01" "mcauldi01" "mcdowsa01" "minchdo01" "olivato01" "osteecl01"
 [7161] "pappami01" "pascuca02" "penaor01"  "piersji01" "pizarju01" "powelbo01" "powervi01" "roberro01" "robinbr01" "seguidi01"
 [7171] "sievero01" "skowrbi01" "skowrbi01" "thomage01" "willidi02" "yastrca01" "aaronha01" "allendi01" "alomasa01" "aloufe01" 
 [7181] "alouje01"  "alouma01"  "bailebo01" "baileed01" "bankser01" "boyerke01" "brocklo01" "brocklo01" "buhlbo01"  "bunniji01"
 [7191] "burgesm01" "callijo01" "cardele01" "cartyri01" "cepedor01" "clemero01" "clinety01" "covinwe01" "crandde01" "davisto02"
 [7201] "daviswi02" "ellswdi01" "fairlro01" "farretu01" "floodcu01" "foxne01"   "freesge02" "frienbo01" "gibsobo01" "gonzato01"
 [7211] "groatdi01" "groteje01" "harpeto01" "howarfr01" "jacksla01" "johnsde01" "johnske02" "kraneed01" "kuennha01" "lawve01"  
 [7221] "maricju01" "matheed01" "mayele01"  "mayswi01"  "mazerbi01" "mccarti01" "mccovwi01" "mcmilro01" "mcmulke01" "motama01" 
 [7231] "nuxhajo01" "paganjo01" "perryga01" "pinsova01" "robinfr02" "rojasco01" "rosepe01"  "sadecra01" "santoro01" "schofdi01"
 [7241] "shortch02" "sievero01" "simmocu01" "snidedu01" "spahnwa01" "stargwi01" "staubru01" "tayloto02" "thomafr03" "thomafr03"
 [7251] "torrejo01" "willibi01" "willsma01" "wynnji01"  "adairje01" "adcocjo01" "aguirha01" "aparilu01" "azcuejo01" "barbest01"
 [7261] "blairpa01" "blasido01" "boyercl02" "brinked01" "brunege01" "burgesm01" "campabe01" "cardejo02" "cashno01"  "colavro01"
 [7271] "davalvi01" "downial01" "fordwh01"  "freehbi01" "fregoji01" "grantmu01" "greenle01" "hansero02" "heldwo01"  "hortowi01"
 [7281] "howarel01" "howarfr01" "johnto01"  "kaatji01"  "kalinal01" "killeha01" "kirkped01" "lauch01"   "lolicmi01" "lonboji01"
 [7291] "mantlmi01" "mcauldi01" "mcdowsa01" "mcmulke01" "minchdo01" "olivato01" "pappami01" "pascuca02" "perryji01" "piersji01"
 [7301] "powelbo01" "powervi01" "richepe01" "robinbr01" "roofph01"  "siebeso01" "skowrbi01" "stanlmi01" "thomage01" "tiantlu01"
 [7311] "yastrca01" "aaronha01" "allendi01" "alomasa01" "aloufe01"  "alouje01"  "alouma01"  "bailebo01" "baileed01" "bankser01"
 [7321] "boyerke01" "brocklo01" "buhlbo01"  "bunniji01" "callijo01" "cardele01" "cardwdo01" "cartyri01" "clemero01" "clinety01"
 [7331] "covinwe01" "crandde01" "davisto02" "daviswi02" "ellswdi01" "fairlro01" "farretu01" "floodcu01" "francti01" "freesge02"
 [7341] "frienbo01" "gibsobo01" "gonzato01" "groatdi01" "harpeto01" "hendeke01" "jacksla01" "johnsde01" "johnske02" "kessido01"
 [7351] "kraneed01" "kuennha01" "kuennha01" "lawve01"   "maricju01" "matheed01" "maxvida01" "mayele01"  "mayele01"  "mayswi01" 
 [7361] "mazerbi01" "mccarti01" "mccovwi01" "mcmilro01" "morgajo02" "motama01"  "osteecl01" "paganjo01" "perezto01" "perryga01"
 [7371] "pinsova01" "robinfr02" "rojasco01" "rosepe01"  "sadecra01" "santoro01" "schofdi01" "schofdi01" "shawbo01"  "shortch02"
 [7381] "simmocu01" "stargwi01" "staubru01" "tayloto02" "thomafr03" "thomafr03" "torrejo01" "willibi01" "willsma01" "wynnji01" 
 [7391] "adairje01" "adairje01" "adcocjo01" "aparilu01" "azcuejo01" "blairpa01" "blasido01" "boyercl02" "brinked01" "brunege01"
 [7401] "burgesm01" "campabe01" "cardejo02" "cashno01"  "colavro01" "crandde01" "davalvi01" "downial01" "etchean01" "freehbi01"
 [7411] "freesge02" "fregoji01" "grantmu01" "greenle01" "hansero02" "heldwo01"  "hortowi01" "howarel01" "howarfr01" "hunteca01"
 [7421] "johnsja01" "johnto01"  "kaatji01"  "kalinal01" "killeha01" "kirkped01" "lolicmi01" "lonboji01" "mantlmi01" "mcauldi01"
 [7431] "mccormi03" "mcdowsa01" "mcmulke01" "minchdo01" "murcebo01" "olivato01" "palmeji01" "perryji01" "piersji01" "powelbo01"
 [7441] "richepe01" "robinbr01" "robinfr02" "roofph01"  "schofdi01" "scottge02" "siebeso01" "skowrbi01" "stanlmi01" "thomage01"
 [7451] "whitero01" "yastrca01" "aaronha01" "allendi01" "aloufe01"  "alouje01"  "alouma01"  "bailebo01" "bankser01" "boyerke01"
 [7461] "brocklo01" "brownol02" "bunniji01" "callijo01" "cardele01" "cartyri01" "cepedor01" "clemero01" "clinety01" "cuellmi01"
 [7471] "davisto02" "daviswi02" "ellswdi01" "fairlro01" "floodcu01" "francti01" "frymawo01" "gibsobo01" "giustda01" "gonzato01"
 [7481] "groatdi01" "groteje01" "harpeto01" "harrebu01" "helmsto01" "holtzke01" "jacksla01" "jenkife01" "johnsde01" "johnske02"
 [7491] "kessido01" "kraneed01" "kuennha01" "lawve01"   "maricju01" "matheed01" "maxvida01" "mayele01"  "mayle01"   "mayswi01" 
 [7501] "mazerbi01" "mccarti01" "mccovwi01" "mcmilro01" "morgajo02" "motama01"  "osteecl01" "paganjo01" "pappami01" "perezto01"
 [7511] "perryga01" "pinsova01" "rojasco01" "rosepe01"  "santoro01" "schofdi01" "shawbo01"  "shortch02" "stargwi01" "staubru01"
 [7521] "suttodo01" "tayloto02" "torrejo01" "willibi01" "willsma01" "wynnji01"  "adairje01" "adairje01" "aparilu01" "azcuejo01"
 [7531] "bandosa01" "belanma01" "blairpa01" "boyerke01" "brinked01" "brunege01" "burgesm01" "campabe01" "cardejo02" "carewro01"
 [7541] "cashno01"  "colavro01" "colavro01" "davalvi01" "downial01" "etchean01" "freehbi01" "fregoji01" "greenle01" "hansero02"
 [7551] "heganmi01" "heldwo01"  "hortowi01" "howarel01" "howarel01" "howarfr01" "hunteca01" "jacksre01" "johnsja01" "johnto01" 
 [7561] "kaatji01"  "kalinal01" "killeha01" "lolicmi01" "lonboji01" "mantlmi01" "matheed01" "mayele01"  "mcauldi01" "mcdowsa01"
 [7571] "mcmulke01" "minchdo01" "mondari01" "olivato01" "pascuca02" "powelbo01" "robinbi02" "robinbr01" "robinfr02" "rodriau01"
 [7581] "roofph01"  "scottge02" "siebeso01" "skowrbi01" "smithre06" "stanlmi01" "thomage01" "tiantlu01" "whitero01" "yastrca01"
 [7591] "aaronha01" "allendi01" "aloufe01"  "alouje01"  "alouma01"  "bailebo01" "bankser01" "benchjo01" "boyercl02" "boyerke01"
 [7601] "brocklo01" "brownol02" "bunniji01" "callijo01" "cardele01" "carltst01" "cartyri01" "cepedor01" "clemero01" "clinety01"
 [7611] "cuellmi01" "davisto02" "daviswi02" "fairlro01" "floodcu01" "francti01" "francti01" "gibsobo01" "giustda01" "gonzato01"
 [7621] "groatdi01" "groteje01" "harpeto01" "harrebu01" "helmsto01" "hendeke01" "jacksla01" "jenkife01" "johnsde01" "johnske02"
 [7631] "kessido01" "kraneed01" "maricju01" "matheed01" "maxvida01" "mayle01"   "mayswi01"  "mazerbi01" "mccarti01" "mccormi03"
 [7641] "mccovwi01" "morgajo02" "motama01"  "niekrph01" "osteecl01" "otisam01"  "paganjo01" "pappami01" "perezto01" "perryga01"
 [7651] "pinsova01" "rojasco01" "rosepe01"  "sadecra01" "santoro01" "schofdi01" "seaveto01" "shortch02" "singebi01" "stargwi01"
 [7661] "staubru01" "suttodo01" "tayloto02" "torrejo01" "willibi01" "willsma01" "wiseri01"  "wynnji01"  "adairje01" "alomasa01"
 [7671] "aparilu01" "azcuejo01" "bahnsst01" "bandosa01" "belanma01" "blairpa01" "brinked01" "brunege01" "campabe01" "cardejo02"
 [7681] "carewro01" "cashno01"  "colavro01" "colemjo05" "davalvi01" "davalvi01" "davisto02" "ellswdi01" "etchean01" "freehbi01"
 [7691] "fregoji01" "hansero02" "hansero02" "harpeto01" "heldwo01"  "hortowi01" "howarel01" "howarfr01" "hunteca01" "jacksre01"
 [7701] "johnsja01" "johnto01"  "kaatji01"  "kalinal01" "killeha01" "kirkped01" "lolicmi01" "mantlmi01" "matheed01" "mayele01" 
 [7711] "mcauldi01" "mcdowsa01" "mcmulke01" "minchdo01" "mondari01" "nettlgr01" "odombl01"  "olivato01" "pascuca02" "powelbo01"
 [7721] "robinbi02" "robinbr01" "robinfr02" "rodriau01" "roofph01"  "rudijo01"  "scottge02" "siebeso01" "smithre06" "spencji01"
 [7731] "stanlmi01" "tiantlu01" "unserde01" "whitero01" "willist02" "yastrca01" "aaronha01" "allendi01" "aloufe01"  "alouje01" 
 [7741] "alouma01"  "bailebo01" "bankser01" "benchjo01" "bondsbo01" "boyercl02" "boyerke01" "brilene01" "brocklo01" "brownol02"
 [7751] "bunniji01" "callijo01" "cardele01" "cardwdo01" "carltst01" "cepedor01" "clemero01" "clinety01" "colavro01" "crawfwi01"
 [7761] "cuellmi01" "daviswi02" "fairlro01" "floodcu01" "francti01" "frymawo01" "gibsobo01" "giustda01" "gonzato01" "groteje01"
 [7771] "harrebu01" "helmsto01" "holtzke01" "jacksla01" "jenkife01" "johnsde01" "kessido01" "koosmje01" "kraneed01" "lummi01"  
 [7781] "maricju01" "maxvida01" "mayle01"   "mayswi01"  "mazerbi01" "mccarti01" "mccormi03" "mccovwi01" "mcraeha01" "motama01" 
 [7791] "niekrjo01" "niekrph01" "osteecl01" "paganjo01" "perezto01" "perryga01" "pinsova01" "reedro01"  "rojasco01" "rosepe01" 
 [7801] "sadecra01" "santoro01" "schofdi01" "seaveto01" "shortch02" "singebi01" "stargwi01" "staubru01" "suttodo01" "tayloto02"
 [7811] "torrejo01" "watsobo01" "willibi01" "willsma01" "wiseri01"  "wynnji01"  "adairje01" "alomasa01" "alomasa01" "aparilu01"
 [7821] "azcuejo01" "azcuejo01" "bahnsst01" "bandosa01" "belanma01" "blairpa01" "brinked01" "campabe01" "cardejo02" "cardele01"
 [7831] "carewro01" "cashno01"  "colemjo05" "cuellmi01" "davalvi01" "davisto02" "etchean01" "francti01" "freehbi01" "fregoji01"
 [7841] "hansero02" "harpeto01" "heganmi01" "heldwo01"  "hortowi01" "howarfr01" "hunteca01" "jacksre01" "johnsja01" "johnto01" 
 [7851] "kaatji01"  "kalinal01" "kellypa01" "killeha01" "kirkped01" "lolicmi01" "martibu01" "mayele01"  "mayele01"  "mcauldi01"
 [7861] "mcdowsa01" "mcmulke01" "minchdo01" "mondari01" "murcebo01" "murphto02" "nettlgr01" "odombl01"  "olivato01" "palmeji01"
 [7871] "perryji01" "pinielo01" "powelbo01" "robinbi02" "robinbr01" "robinfr02" "rodriau01" "roofph01"  "rudijo01"  "schofdi01"
 [7881] "scottge02" "siebeso01" "smithre06" "spencji01" "stanlmi01" "thomage01" "tiantlu01" "unserde01" "whitero01" "yastrca01"
 [7891] "aaronha01" "allendi01" "aloufe01"  "alouje01"  "alouma01"  "bailebo01" "bankser01" "benchjo01" "bondsbo01" "boyercl02"
 [7901] "brilene01" "brocklo01" "brownol02" "callijo01" "carltst01" "cartyri01" "cepedor01" "clemero01" "clinety01" "crawfwi01"
 [7911] "davalvi01" "davisto02" "daviswi02" "ellisdo01" "fairlro01" "fairlro01" "floodcu01" "francti01" "frymawo01" "gamblos01"
 [7921] "gibsobo01" "gonzato01" "gonzato01" "griffto02" "groteje01" "harrebu01" "hebneri01" "helmsto01" "hendeke01" "holtzke01"
 [7931] "jacksgr01" "jenkife01" "johnsde01" "kessido01" "koosmje01" "kraneed01" "lummi01"   "maricju01" "maxvida01" "mayle01"  
 [7941] "mayswi01"  "mazerbi01" "mccarti01" "mccormi03" "mccovwi01" "moneydo01" "morgajo02" "motama01"  "motama01"  "niekrjo01"
 [7951] "niekrph01" "oliveal01" "osteecl01" "otisam01"  "paganjo01" "perezto01" "perryga01" "pinsova01" "reedro01"  "rojasco01"
 [7961] "rosepe01"  "russebi01" "santoro01" "seaveto01" "singebi01" "stargwi01" "staubru01" "suttodo01" "tayloto02" "torrejo01"
 [7971] "willibi01" "willsma01" "willsma01" "wiseri01"  "wynnji01"  "alomasa01" "aloufe01"  "aparilu01" "azcuejo01" "bahnsst01"
 [7981] "bandosa01" "belanma01" "blairpa01" "blylebe01" "brinked01" "campabe01" "cardele01" "carewro01" "cashno01"  "colemjo05"
 [7991] "crowlte01" "cuellmi01" "davisto02" "etchean01" "francti01" "freehbi01" "fregoji01" "gonzato01" "grichbo01" "hansero02"
 [8001] "harpeto01" "heganmi01" "hortowi01" "howarfr01" "hunteca01" "jacksre01" "johnsja01" "johnto01"  "kaatji01"  "kalinal01"
 [8011] "kellypa01" "killeha01" "kirkped01" "lolicmi01" "mayele01"  "mayru01"   "mcauldi01" "mcdowsa01" "mcmulke01" "mcmulke01"
 [8021] "minchdo01" "mondari01" "murcebo01" "murphto02" "nettlgr01" "niekrjo01" "odombl01"  "olivato01" "otisam01"  "palmeji01"
 [8031] "perryji01" "pinielo01" "pinsova01" "powelbo01" "robinbr01" "robinfr02" "rodriau01" "rodriau01" "rojasco01" "roofph01" 
 [8041] "rudijo01"  "schofdi01" "scottge02" "siebeso01" "smithre06" "spencji01" "stanlmi01" "tenacge01" "thomage01" "unserde01"
 [8051] "whitero01" "yastrca01" "aaronha01" "allendi01" "alouje01"  "alouma01"  "bailebo01" "bankser01" "benchjo01" "bondsbo01"
 [8061] "bowala01"  "boyercl02" "brocklo01" "brownol02" "bucknbi01" "bunniji01" "callijo01" "carbobe01" "cardejo02" "carltst01"
 [8071] "cartyri01" "cedence01" "cepedor01" "clemero01" "clinety01" "conceda01" "crawfwi01" "davalvi01" "davisto02" "daviswi02"
 [8081] "ellisdo01" "fairlro01" "gamblos01" "garvest01" "gibsobo01" "gonzato01" "groteje01" "harrebu01" "hebneri01" "helmsto01"
 [8091] "hendeke01" "holtzke01" "jenkife01" "johnsde01" "jorgemi01" "kessido01" "koosmje01" "lummi01"   "maricju01" "maxvida01"
 [8101] "maybejo01" "mayle01"   "mayswi01"  "mazerbi01" "mccarti01" "mccovwi01" "mcraeha01" "moneydo01" "moralje01" "morgajo02"
 [8111] "motama01"  "niekrph01" "oliveal01" "osteecl01" "paganjo01" "pappami01" "perezto01" "perryga01" "renkost01" "roberda05"
 [8121] "rosepe01"  "russebi01" "santoro01" "seaveto01" "shortch02" "simmote01" "singlke01" "stargwi01" "staubru01" "suttodo01"
 [8131] "tayloto02" "torrejo01" "torremi01" "watsobo01" "willibi01" "willsma01" "wiseri01"  "wynnji01"  "alomasa01" "aloufe01" 
 [8141] "aparilu01" "bahnsst01" "bandosa01" "belanma01" "beniqju01" "bevacku01" "biittla01" "blairpa01" "bluevi01"  "blylebe01"
 [8151] "braunst01" "brinked01" "burroje01" "campabe01" "cardejo02" "cardele01" "carewro01" "cashno01"  "chambch01" "colemjo05"
 [8161] "cuellmi01" "davisto02" "etchean01" "freehbi01" "fregoji01" "gonzato01" "hansero02" "harpeto01" "harrato01" "heganmi01"
 [8171] "heganmi01" "hendrge01" "hortowi01" "howarfr01" "hunteca01" "jacksre01" "johnsja01" "johnto01"  "kaatji01"  "kalinal01"
 [8181] "kellypa01" "killeha01" "kirkped01" "lolicmi01" "lonboji01" "lowenjo01" "mayru01"   "mcauldi01" "mcdowsa01" "mcmulke01"
 [8191] "minchdo01" "minchdo01" "mondari01" "murcebo01" "murphto02" "nettlgr01" "odombl01"  "olivato01" "otisam01"  "palmeji01"
 [8201] "perryji01" "pinielo01" "pinsova01" "porteda02" "powelbo01" "rivermi01" "robinbr01" "robinfr02" "rodriau01" "rojasco01"
 [8211] "roofph01"  "roofph01"  "rudijo01"  "scottge02" "siebeso01" "smithre06" "spencji01" "stanlfr01" "stanlmi01" "tayloto02"
 [8221] "tenacge01" "unserde01" "whitero01" "woodwi01"  "yastrca01" "aaronha01" "allendi01" "alouje01"  "alouma01"  "bailebo01"
 [8231] "bakerdu01" "bankser01" "benchjo01" "bondsbo01" "bowala01"  "boyercl02" "brocklo01" "brownol02" "bucknbi01" "callijo01"
 [8241] "carbobe01" "cardejo02" "carltst01" "cedence01" "cepedor01" "clemero01" "clinety01" "conceda01" "crawfwi01" "cruzjo01" 
 [8251] "davalvi01" "daviswi02" "downial01" "ellisdo01" "evansda01" "fairlro01" "fergujo01" "foliti01"  "forscke01" "fostege01"
 [8261] "fostege01" "gamblos01" "garvest01" "geronce01" "gibsobo01" "groteje01" "harrebu01" "hebneri01" "helmsto01" "hendeke01"
 [8271] "holtzke01" "jenkife01" "johnsde01" "jorgemi01" "kessido01" "kingmda01" "koosmje01" "kraneed01" "lummi01"   "luzingr01"
 [8281] "maricju01" "maxvida01" "maybejo01" "mayle01"   "maymi01"   "mayswi01"  "mazerbi01" "mccarti01" "mccovwi01" "mcraeha01"
 [8291] "moneydo01" "montawi01" "morgajo02" "motama01"  "niekrph01" "oliveal01" "osteecl01" "paganjo01" "pappami01" "perezto01"
 [8301] "perryga01" "reedro01"  "renkost01" "reussje01" "roberda05" "rosepe01"  "russebi01" "sadecra01" "santoro01" "schofdi01"
 [8311] "seaveto01" "simmote01" "singebi01" "singlke01" "speiech01" "stargwi01" "staubru01" "suttodo01" "tayloto02" "torrejo01"
 [8321] "watsobo01" "willibi01" "willsma01" "wiseri01"  "wynnji01"  "allendi01" "alomasa01" "aloufe01"  "alouma01"  "aparilu01"
 [8331] "bahnsst01" "bandosa01" "baylodo01" "belanma01" "bellbu01"  "beniqju01" "biittla01" "blairpa01" "blylebe01" "braunst01"
 [8341] "brinked01" "brownol02" "brownol02" "burroje01" "callijo01" "campabe01" "cardele01" "carewro01" "cashno01"  "chambch01"
 [8351] "colemjo05" "crowlte01" "cuellmi01" "curtijo01" "davisto02" "etchean01" "evansdw01" "fiskca01"  "freehbi01" "grichbo01"
 [8361] "harpeto01" "harrato01" "heganmi01" "hendrge01" "holtzke01" "hortowi01" "howarfr01" "hunteca01" "jacksre01" "johnsja01"
 [8371] "kalinal01" "kellypa01" "killeha01" "kirkped01" "lolicmi01" "lonboji01" "lowenjo01" "maybejo01" "mayru01"   "mcauldi01"
 [8381] "mcmulke01" "milleri01" "minchdo01" "minchdo01" "murcebo01" "nettlgr01" "odombl01"  "oglivbe01" "ortajo01"  "otisam01" 
 [8391] "palmeji01" "perryga01" "perryji01" "pinielo01" "pinsova01" "porteda02" "powelbo01" "rivermi01" "robinbr01" "rodriau01"
 [8401] "rojasco01" "roofph01"  "rudijo01"  "ryanno01"  "scottge02" "siebeso01" "smithre06" "spencji01" "splitpa01" "stanlmi01"
 [8411] "tayloto02" "tenacge01" "tiantlu01" "tidrodi01" "unserde01" "whitero01" "woodwi01"  "yastrca01" "aaronha01" "alouje01" 
 [8421] "alouma01"  "bailebo01" "bakerdu01" "benchjo01" "bondsbo01" "boonebo01" "bowala01"  "brilene01" "brocklo01" "brownol02"
 [8431] "bucknbi01" "caldwmi01" "carbobe01" "cardejo02" "carltst01" "cartyri01" "cedence01" "cepedor01" "clemero01" "conceda01"
 [8441] "crawfwi01" "cruzjo01"  "davalvi01" "daviswi02" "downial01" "ellisdo01" "evansda01" "fairlro01" "foliti01"  "fostege01"
 [8451] "fregoji01" "gamblos01" "garvest01" "geronce01" "gibsobo01" "groteje01" "harrebu01" "hebneri01" "helmsto01" "hendeke01"
 [8461] "hootobu01" "jenkife01" "johnsde01" "johnto01"  "jorgemi01" "kessido01" "kingmda01" "kisonbr01" "kraneed01" "lacyle01" 
 [8471] "lummi01"   "luzingr01" "maddoga01" "maricju01" "matthga01" "maxvida01" "mayle01"   "maymi01"   "mayswi01"  "mazerbi01"
 [8481] "mccarti01" "mccarti01" "mccovwi01" "mcdowsa01" "mcraeha01" "mondari01" "moneydo01" "montawi01" "moralje01" "morgajo02"
 [8491] "motama01"  "niekrph01" "normafr01" "oliveal01" "osteecl01" "paganjo01" "pappami01" "perezto01" "reedro01"  "reussje01"
 [8501] "roberda05" "robinbi02" "robinfr02" "rosepe01"  "russebi01" "santoro01" "seaveto01" "simmote01" "singebi01" "singlke01"
 [8511] "speiech01" "stanlfr01" "stargwi01" "staubru01" "suttodo01" "thomade01" "torrejo01" "torremi01" "watsobo01" "willibi01"
 [8521] "willsma01" "wiseri01"  "wynnji01"  "yeagest01" "allendi01" "alomasa01" "aloufe01"  "alouje01"  "alouma01"  "aparilu01"
 [8531] "bandosa01" "baylodo01" "belanma01" "bellbu01"  "bevacku01" "biittla01" "blairpa01" "braunst01" "brinked01" "brownol02"
 [8541] "burroje01" "callijo01" "campabe01" "cardele01" "carewro01" "cartyri01" "cashno01"  "cepedor01" "chambch01" "coopece01"
 [8551] "crowlte01" "davalvi01" "davisto02" "downibr01" "etchean01" "evansdw01" "fiskca01"  "freehbi01" "fregoji01" "gamblos01"
 [8561] "grichbo01" "hairsje01" "harpeto01" "harrato01" "heganmi01" "heganmi01" "hendeke01" "hendrge01" "hortowi01" "howarfr01"
 [8571] "jacksre01" "johnsde01" "kalinal01" "kellypa01" "killeha01" "kirkped01" "lowenjo01" "madlobi01" "maybejo01" "mcauldi01"
 [8581] "mcraeha01" "milleri01" "moneydo01" "murcebo01" "nettlgr01" "oglivbe01" "olivato01" "ortajo01"  "otisam01"  "pinielo01"
 [8591] "pinsova01" "porteda02" "powelbo01" "rivermi01" "robinbr01" "robinfr02" "rodriau01" "rojasco01" "roofph01"  "rudijo01" 
 [8601] "scottge02" "smithre06" "spencji01" "spencji01" "stanlfr01" "stanlmi01" "tayloto02" "tenacge01" "thomago01" "whitefr01"
 [8611] "whitero01" "wohlfji01" "yastrca01" "aaronha01" "alouje01"  "bailebo01" "bakerdu01" "benchjo01" "bondsbo01" "boonebo01"
 [8621] "bowala01"  "brettke01" "brilene01" "brocklo01" "bucknbi01" "carbobe01" "cardejo02" "carltst01" "cartyri01" "cedence01"
 [8631] "ceyro01"   "conceda01" "crawfwi01" "cruzjo01"  "davalvi01" "daviswi02" "downial01" "driesda01" "dwyerji01" "ellisdo01"
 [8641] "evansda01" "fairlro01" "fergujo01" "foliti01"  "forscke01" "fregoji01" "garvest01" "geronce01" "gibsobo01" "griffke01"
 [8651] "groteje01" "grubbjo01" "harrebu01" "hebneri01" "helmsto01" "hootobu01" "jenkife01" "johnto01"  "jorgemi01" "kessido01"
 [8661] "kingmda01" "koosmje01" "kraneed01" "lacyle01"  "lonboji01" "lopesda01" "lummi01"   "luzingr01" "maddoga01" "maricju01"
 [8671] "matthga01" "maxvida01" "mayle01"   "maymi01"   "mayswi01"  "mccarti01" "mccovwi01" "mcmulke01" "mondari01" "montawi01"
 [8681] "moralje01" "morgajo02" "motama01"  "niekrph01" "normafr01" "oliveal01" "osteecl01" "pacioto01" "paganjo01" "parkeda01"
 [8691] "perezto01" "renkost01" "reuscri01" "reussje01" "roberda05" "robinbi02" "rosepe01"  "russebi01" "santoro01" "schmimi01"
 [8701] "seaveto01" "simmote01" "singlke01" "speiech01" "stargwi01" "staubru01" "suttodo01" "thomade01" "torrejo01" "torremi01"
 [8711] "unserde01" "watsobo01" "willibi01" "winfida01" "wiseri01"  "wynnji01"  "yeagest01" "allendi01" "alomasa01" "alomasa01"
 [8721] "alouje01"  "bandosa01" "baylodo01" "belanma01" "bellbu01"  "beniqju01" "bevacku01" "blairpa01" "braunst01" "brettge01"
 [8731] "brinked01" "burroje01" "cabelen01" "campabe01" "carbobe01" "cardele01" "carewro01" "cartyri01" "cashno01"  "cepedor01"
 [8741] "chambch01" "chambch01" "coopece01" "davisto02" "dempsri01" "downibr01" "etchean01" "evansdw01" "fiskca01"  "freehbi01"
 [8751] "fregoji01" "gamblos01" "grichbo01" "hairsje01" "harpeto01" "harrato01" "heganmi01" "heganmi01" "hendeke01" "hendrge01"
 [8761] "hortowi01" "jacksre01" "johnsde01" "johnsde01" "kalinal01" "kellypa01" "killeha01" "lowenjo01" "martibu01" "maxvida01"
 [8771] "maybejo01" "mcauldi01" "mcraeha01" "milleri01" "moneydo01" "moorech02" "murcebo01" "nettlgr01" "oglivbe01" "olivato01"
 [8781] "ortajo01"  "otisam01"  "pinielo01" "pinsova01" "porteda02" "powelbo01" "riceji01"  "rivermi01" "robinbr01" "robinfr02"
 [8791] "robinfr02" "rodriau01" "rojasco01" "roofph01"  "rudijo01"  "santoro01" "scottge02" "spencji01" "stanlmi01" "sundbji01"
 [8801] "tenacge01" "washicl01" "whitefr01" "whitero01" "wohlfji01" "yastrca01" "yountro01" "aaronha01" "alouma01"  "bailebo01"
 [8811] "bakerdu01" "benchjo01" "bondsbo01" "boonebo01" "bowala01"  "brettke01" "brocklo01" "brownol02" "brownol02" "bucknbi01"
 [8821] "caldwmi01" "cardejo02" "carltst01" "cedence01" "ceyro01"   "conceda01" "crawfwi01" "crowlte01" "cruzjo01"  "curtijo01"
 [8831] "daviswi02" "driesda01" "dwyerji01" "ellisdo01" "evansda01" "fairlro01" "fergujo01" "foliti01"  "fostege01" "garvest01"
 [8841] "geronce01" "gibsobo01" "griffke01" "griffto02" "grossgr01" "groteje01" "grubbjo01" "harrebu01" "hebneri01" "helmsto01"
 [8851] "hootobu01" "johnscl01" "johnsja01" "johnto01"  "jorgemi01" "kessido01" "kingmda01" "kirkped01" "koosmje01" "kraneed01"
 [8861] "lacyle01"  "lonboji01" "lopesda01" "lummi01"   "luzingr01" "maddoga01" "madlobi01" "matthga01" "mayle01"   "maymi01"  
 [8871] "mccarti01" "mccovwi01" "mcmulke01" "mondari01" "montawi01" "moralje01" "morgajo02" "motama01"  "niekrph01" "normafr01"
 [8881] "oliveal01" "pacioto01" "parkeda01" "parrila01" "perezto01" "reedro01"  "renkost01" "reuscri01" "reussje01" "roberda05"
 [8891] "robinbi02" "rosepe01"  "russebi01" "ruthvdi01" "schmimi01" "seaveto01" "simmote01" "singlke01" "smithre06" "speiech01"
 [8901] "stargwi01" "staubru01" "suttodo01" "tayloto02" "thomade01" "thornan01" "torrejo01" "torremi01" "unserde01" "watsobo01"
 [8911] "willibi01" "winfida01" "wynnji01"  "yeagest01" "aaronha01" "alomasa01" "ashbyal01" "bandosa01" "baylodo01" "belanma01"
 [8921] "bellbu01"  "beniqju01" "bevacku01" "blairpa01" "bondsbo01" "braunst01" "brettge01" "brinked01" "burroje01" "campabe01"
 [8931] "carbobe01" "cardele01" "carewro01" "cartyri01" "chambch01" "collida02" "coopece01" "davisto02" "daviswi02" "decindo01"
 [8941] "dempsri01" "downibr01" "etchean01" "evansdw01" "fiskca01"  "freehbi01" "fregoji01" "gamblos01" "garneph01" "grichbo01"
 [8951] "hairsje01" "harpeto01" "harpeto01" "harrato01" "heganmi01" "hendeke01" "hendrge01" "hortowi01" "jacksre01" "johnsde01"
 [8961] "kellypa01" "killeha01" "lowenjo01" "lynnfr01"  "martibu01" "maybejo01" "mayle01"   "mcraeha01" "milleri01" "moneydo01"
 [8971] "moorech02" "nettlgr01" "oglivbe01" "olivato01" "ortajo01"  "otisam01"  "pinielo01" "pinsova01" "porteda02" "powelbo01"
 [8981] "riceji01"  "rivermi01" "robinbr01" "robinfr02" "rodriau01" "rojasco01" "roofph01"  "rudijo01"  "scottge02" "singlke01"
 [8991] "smallro02" "spencji01" "stanlfr01" "stanlmi01" "sundbji01" "tenacge01" "thomago01" "washicl01" "whitefr01" "whitero01"
 [9001] "willibi01" "wohlfji01" "yastrca01" "yountro01" "allendi01" "alouje01"  "bailebo01" "bakerdu01" "benchjo01" "biittla01"
 [9011] "boonebo01" "bowala01"  "brettke01" "brinked01" "brocklo01" "brownol02" "bucknbi01" "burrira01" "cabelen01" "cardejo02"
 [9021] "carltst01" "cartega01" "cedence01" "ceyro01"   "conceda01" "crawfwi01" "crowlte01" "cruzjo01"  "daviswi02" "dejesiv01"
 [9031] "driesda01" "dwyerji01" "evansda01" "fairlro01" "fergujo01" "foliti01"  "forscbo01" "fostege01" "garvest01" "geronce01"
 [9041] "griffke01" "grossgr01" "groteje01" "grubbjo01" "harrebu01" "hebneri01" "helmsto01" "hernake01" "hillma01"  "hootobu01"
 [9051] "johnscl01" "johnsja01" "jorgemi01" "kessido01" "kingmda01" "kirkped01" "kisonbr01" "koosmje01" "kraneed01" "lacyle01" 
 [9061] "lopesda01" "lummi01"   "luzingr01" "maddoga01" "maddoga01" "madlobi01" "matthga01" "maymi01"   "mccarti01" "mccovwi01"
 [9071] "mondari01" "montawi01" "montawi01" "moralje01" "morgajo02" "murcebo01" "niekrph01" "normafr01" "oliveal01" "pacioto01"
 [9081] "parkeda01" "parrila01" "perezto01" "randowi01" "reedro01"  "renkost01" "reuscri01" "reussje01" "reynocr01" "roberda05"
 [9091] "robinbi02" "rosepe01"  "russebi01" "schmimi01" "seaveto01" "simmote01" "smithre06" "speiech01" "stargwi01" "staubru01"
 [9101] "suttodo01" "tayloto02" "thomade01" "thornan01" "torrejo01" "trillma01" "unserde01" "watsobo01" "winfida01" "wynnji01" 
 [9111] "yeagest01" "aaronha01" "alomasa01" "ashbyal01" "bandosa01" "baylodo01" "belanma01" "bellbu01"  "beniqju01" "blairpa01"
 [9121] "bondsbo01" "braunst01" "brettge01" "burroje01" "campabe01" "carbobe01" "carbobe01" "carewro01" "cartyri01" "chambch01"
 [9131] "collida02" "coopece01" "crowlte01" "davisto02" "decindo01" "dempsri01" "downibr01" "easlemi01" "etchean01" "evansdw01"
 [9141] "fiskca01"  "freehbi01" "fregoji01" "gamblos01" "gantnji01" "garneph01" "grichbo01" "hairsje01" "harpeto01" "harrato01"
 [9151] "heganmi01" "hendrge01" "hortowi01" "jacksre01" "kellypa01" "lemonch01" "lowenjo01" "lynnfr01"  "martibu01" "maybejo01"
 [9161] "mayle01"   "mcmulke01" "mcraeha01" "milleri01" "moneydo01" "moorech02" "nettlgr01" "oglivbe01" "olivato01" "ortajo01" 
 [9171] "otisam01"  "pinielo01" "porteda02" "powelbo01" "quirkja01" "randowi01" "riceji01"  "rivermi01" "robinbr01" "robinfr02"
 [9181] "rodriau01" "rojasco01" "rudijo01"  "scottge02" "singlke01" "smallro02" "smallro02" "spencji01" "stanlfr01" "stanlmi01"
 [9191] "staubru01" "sundbji01" "tenacge01" "thomago01" "washicl01" "whitefr01" "whitero01" "willibi01" "wohlfji01" "yastrca01"
 [9201] "yountro01" "allendi01" "almonbi01" "bailebo01" "bakerdu01" "benchjo01" "biittla01" "boonebo01" "bowala01"  "brocklo01"
 [9211] "brownol02" "bucknbi01" "burrira01" "cabelen01" "candejo01" "cardejo02" "carltst01" "cartega01" "cedence01" "ceyro01"  
 [9221] "clarkja01" "conceda01" "crawfwi01" "cruzjo01"  "daviswi02" "dawsoan01" "driesda01" "dwyerji01" "evansda01" "evansda01"
 [9231] "fairlro01" "fergujo01" "fergujo01" "foliti01"  "forscbo01" "fostege01" "frymawo01" "garvest01" "geronce01" "griffke01"
 [9241] "grossgr01" "groteje01" "grubbjo01" "harrebu01" "hebneri01" "helmsto01" "hendeke01" "hernake01" "hillma01"  "hootobu01"
 [9251] "johnscl01" "johnsja01" "johnto01"  "jorgemi01" "kaatji01"  "kessido01" "kingmda01" "kirkped01" "kisonbr01" "koosmje01"
 [9261] "kraneed01" "lacyle01"  "lacyle01"  "lolicmi01" "lonboji01" "lopesda01" "lummi01"   "luzingr01" "maddoga01" "madlobi01"
 [9271] "matthga01" "mazzile01" "mccarti01" "mccovwi01" "mondari01" "montawi01" "montawi01" "moralje01" "morgajo02" "motama01" 
 [9281] "mumphje01" "murcebo01" "murphda05" "niekrph01" "normafr01" "oliveal01" "pacioto01" "parkeda01" "parrila01" "perezto01"
 [9291] "renkost01" "reuscri01" "reussje01" "rhoderi01" "robinbi02" "rosepe01"  "roystje01" "russebi01" "ruthvdi01" "schmimi01"
 [9301] "seaveto01" "simmote01" "smithre06" "smithre06" "speiech01" "stargwi01" "suttodo01" "templga01" "thomade01" "thornan01"
 [9311] "thornan01" "torrejo01" "trillma01" "unserde01" "unserde01" "watsobo01" "winfida01" "wynnji01"  "yeagest01" "youngjo02"
 [9321] "allendi01" "alomasa01" "ashbyal01" "bandosa01" "baylodo01" "belanma01" "bellbu01"  "beniqju01" "bevacku01" "blairpa01"
 [9331] "bondsbo01" "bosleth01" "braunst01" "brettge01" "campabe01" "carbobe01" "carewro01" "cartyri01" "ceronri01" "chambch01"
 [9341] "collida02" "coopece01" "crawfwi01" "decindo01" "dempsri01" "downibr01" "etchean01" "evansdw01" "fairlro01" "fiskca01" 
 [9351] "gamblos01" "grichbo01" "grubbjo01" "harrato01" "heganmi01" "helmsto01" "hendeke01" "hortowi01" "jacksre01" "johnscl01"
 [9361] "jorgemi01" "kellypa01" "kessido01" "kirkped01" "lemonch01" "lowenjo01" "lynnfr01"  "martibu01" "maybejo01" "mayle01"  
 [9371] "maymi01"   "mcmulke01" "mcraeha01" "milleri01" "moneydo01" "moorech02" "mullira01" "murraed02" "nettlgr01" "oglivbe01"
 [9381] "ortajo01"  "otisam01"  "pinielo01" "porteda02" "quirkja01" "randowi01" "reynocr01" "riceji01"  "rivermi01" "rodriau01"
 [9391] "rojasco01" "rudijo01"  "scottge02" "singlke01" "smallro02" "spencji01" "stanlmi01" "staubru01" "sundbji01" "thornan01"
 [9401] "washicl01" "whitefr01" "whitero01" "wohlfji01" "wynnji01"  "wynnji01"  "yastrca01" "yountro01" "almonbi01" "bailebo01"
 [9411] "bakerdu01" "benchjo01" "biittla01" "boonebo01" "bowala01"  "brocklo01" "brownol02" "bucknbi01" "burrira01" "burroje01"
 [9421] "cabelen01" "candejo01" "cardejo02" "carltst01" "cartega01" "cedence01" "ceyro01"   "clarkja01" "conceda01" "crawfwi01"
 [9431] "cruzjo01"  "dawsoan01" "dejesiv01" "driesda01" "evansda01" "fergujo01" "foliti01"  "foliti01"  "forscbo01" "fostege01"
 [9441] "fregoji01" "garneph01" "garvest01" "geronce01" "griffke01" "grossgr01" "groteje01" "hairsje01" "harrebu01" "hebneri01"
 [9451] "hendrge01" "hernake01" "hillma01"  "hootobu01" "johnscl01" "johnsja01" "johnto01"  "kaatji01"  "kessido01" "kingmda01"
 [9461] "kingmda01" "kisonbr01" "kneppbo01" "koosmje01" "kraneed01" "lacyle01"  "lopesda01" "lummi01"   "luzingr01" "maddoga01"
 [9471] "madlobi01" "matthga01" "mazzile01" "mccarti01" "mccovwi01" "mondari01" "montawi01" "moralje01" "morgajo02" "mumphje01"
 [9481] "murcebo01" "murphda05" "niekrjo01" "niekrph01" "normafr01" "oliveal01" "pacioto01" "parkeda01" "parrila01" "perezto01"
 [9491] "puhlte01"  "reuscri01" "reussje01" "rhoderi01" "robinbi02" "rosepe01"  "roystje01" "russebi01" "schmimi01" "seaveto01"
 [9501] "simmote01" "smithre06" "speiech01" "stargwi01" "suttodo01" "templga01" "tenacge01" "thomade01" "torrejo01" "trillma01"
 [9511] "unserde01" "watsobo01" "winfida01" "yeagest01" "youngjo02" "ashbyal01" "bailebo01" "bandosa01" "baylodo01" "belanma01"
 [9521] "bellbu01"  "beniqju01" "bevacku01" "blairpa01" "bondsbo01" "bondsbo01" "bosleth01" "braunst01" "braunst01" "brettge01"
 [9531] "campabe01" "carbobe01" "carewro01" "cartyri01" "cartyri01" "ceronri01" "chambch01" "coopece01" "crowlte01" "decindo01"
 [9541] "dempsri01" "dilonmi01" "downibr01" "evansdw01" "fairlro01" "fiskca01"  "gantnji01" "grichbo01" "grubbjo01" "harrato01"
 [9551] "hassero01" "heathmi02" "hortowi01" "hortowi01" "hortowi01" "jacksre01" "johnscl01" "johnsja01" "jorgemi01" "kellypa01"
 [9561] "kessido01" "lansfca01" "lemonch01" "lowenjo01" "lynnfr01"  "martibu01" "maybejo01" "mayle01"   "maymi01"   "mcraeha01"
 [9571] "milleri01" "molitpa01" "moneydo01" "moorech02" "mullira01" "murraed02" "nettlgr01" "oglivbe01" "oliveal01" "ortajo01" 
 [9581] "otisam01"  "pacioto01" "parrila02" "pinielo01" "porteda02" "randowi01" "reynocr01" "riceji01"  "rivermi01" "rodriau01"
 [9591] "rudijo01"  "scottge02" "singlke01" "smallro02" "spencji01" "stanlfr01" "stanlmi01" "staubru01" "sundbji01" "thomago01"
 [9601] "thornan01" "trammal01" "washicl01" "whitalo01" "whitefr01" "whitero01" "wilsowi02" "wohlfji01" "yastrca01" "yountro01"
 [9611] "almonbi01" "alouje01"  "bakerdu01" "benchjo01" "bergmda01" "biittla01" "bluevi01"  "blylebe01" "boonebo01" "bowala01" 
 [9621] "brocklo01" "bucknbi01" "burrira01" "burroje01" "cabelen01" "candejo01" "cardejo02" "carltst01" "cartega01" "cedence01"
 [9631] "ceyro01"   "clarkja01" "collida02" "conceda01" "cruzjo01"  "davalvi01" "dawsoan01" "dejesiv01" "driesda01" "dwyerji01"
 [9641] "dwyerji01" "evansda01" "fergujo01" "fergujo01" "foliti01"  "forscbo01" "fostege01" "gamblos01" "garneph01" "garvest01"
 [9651] "geronce01" "griffke01" "grossgr01" "groteje01" "harrebu01" "hebneri01" "hendeke01" "hendrge01" "hendrge01" "hernake01"
 [9661] "hillma01"  "hootobu01" "johnsja01" "johnto01"  "kingmda01" "kneppbo01" "koosmje01" "kraneed01" "lacyle01"  "lampde01" 
 [9671] "lopesda01" "lummi01"   "luzingr01" "maddoga01" "madlobi01" "matthga01" "mazzile01" "mccarti01" "mccovwi01" "mondari01"
 [9681] "montawi01" "moralje01" "morgajo02" "morriji01" "mumphje01" "murcebo01" "murphda05" "niekrjo01" "niekrph01" "normafr01"
 [9691] "oberkke01" "parkeda01" "parrila01" "perezto01" "perryga01" "puhlte01"  "reuscri01" "rhoderi01" "roberda05" "robinbi02"
 [9701] "robindo01" "rosepe01"  "roystje01" "russebi01" "ruthvdi01" "schmimi01" "seaveto01" "simmote01" "smithoz01" "smithre06"
 [9711] "speiech01" "stargwi01" "suttodo01" "templga01" "tenacge01" "thomade01" "trillma01" "unserde01" "wallide01" "watsobo01"
 [9721] "winfida01" "yeagest01" "youngjo02" "bandosa01" "baylodo01" "belanma01" "bellbu01"  "beniqju01" "bondsbo01" "bosleth01"
 [9731] "braunst01" "brettge01" "campabe01" "carewro01" "cartyri01" "ceronri01" "chambch01" "coopece01" "crowlte01" "daviswi02"
 [9741] "decindo01" "dempsri01" "dilonmi01" "downibr01" "dwyerji01" "evansdw01" "fiskca01"  "gamblos01" "gamblos01" "gantnji01"
 [9751] "grichbo01" "griffal01" "grubbjo01" "harrato01" "hassero01" "heathmi02" "henderi01" "hortowi01" "jacksre01" "johnscl01"
 [9761] "johnscl01" "jorgemi01" "kellypa01" "kessido01" "lansfca01" "lemonch01" "lowenjo01" "lynnfr01"  "martibu01" "maybejo01"
 [9771] "mayle01"   "maymi01"   "mcraeha01" "milleri01" "molitpa01" "moneydo01" "montawi01" "moorech02" "moralje01" "morriji01"
 [9781] "mullira01" "murcebo01" "murraed02" "nettlgr01" "oglivbe01" "oliveal01" "ortajo01"  "otisam01"  "pacioto01" "parrila02"
 [9791] "pinielo01" "porteda02" "quirkja01" "randowi01" "riceji01"  "rivermi01" "rivermi01" "rodriau01" "rudijo01"  "scottge02"
 [9801] "scottge02" "singlke01" "smallro02" "spencji01" "stanlfr01" "staubru01" "sundbji01" "thomago01" "thondi01"  "thornan01"
 [9811] "trammal01" "washicl01" "watsobo01" "whitalo01" "whitefr01" "whitero01" "wilsowi02" "wohlfji01" "yastrca01" "yountro01"
 [9821] "almonbi01" "ashbyal01" "bakerdu01" "benchjo01" "bevacku01" "biittla01" "blairpa01" "bluevi01"  "blylebe01" "boonebo01"
 [9831] "bowala01"  "brocklo01" "bucknbi01" "burroje01" "cabelen01" "candejo01" "carbobe01" "carltst01" "cartega01" "cedence01"
 [9841] "ceyro01"   "clarkja01" "collida02" "conceda01" "cruzjo01"  "dawsoan01" "dejesiv01" "driesda01" "easlemi01" "evansda01"
 [9851] "fergujo01" "foliti01"  "forscbo01" "forscke01" "fostege01" "garneph01" "garvest01" "geronce01" "griffke01" "grossgr01"
 [9861] "guerrpe01" "harrebu01" "hebneri01" "hendeke01" "hendrge01" "hernake01" "hillma01"  "hootobu01" "johnsja01" "kingmda01"
 [9871] "kisonbr01" "kneppbo01" "kraneed01" "lacyle01"  "lampde01"  "leonaje01" "lopesda01" "lummi01"   "luzingr01" "maddoga01"
 [9881] "madlobi01" "madlobi01" "matthga01" "mazzile01" "mccarti01" "mccovwi01" "montawi01" "morgajo02" "mumphje01" "murcebo01"
 [9891] "murphda05" "niekrjo01" "niekrph01" "normafr01" "oberkke01" "parkeda01" "parrila01" "perezto01" "perryga01" "puhlte01" 
 [9901] "reuscri01" "reynocr01" "robinbi02" "rosepe01"  "roystje01" "russebi01" "sandesc01" "schatda01" "schmimi01" "seaveto01"
 [9911] "simmote01" "smithoz01" "smithre06" "speiech01" "spilmha01" "stargwi01" "staubru01" "sutclri01" "suttodo01" "templga01"
 [9921] "tenacge01" "thomade01" "trevial01" "trillma01" "unserde01" "wallide01" "watsobo01" "winfida01" "yeagest01" "youngjo02"
 [9931] "baineha01" "bandosa01" "baylodo01" "belanma01" "bellbu01"  "beniqju01" "bosleth01" "braunst01" "brettge01" "campabe01"
 [9941] "cardejo02" "carewro01" "ceronri01" "coopece01" "crowlte01" "decindo01" "dempsri01" "dilonmi01" "downibr01" "dwyerji01"
 [9951] "evansdw01" "fiskca01"  "gamblos01" "gantnji01" "gibsoki01" "grichbo01" "griffal01" "grubbjo01" "harrato01" "harrebu01"
 [9961] "hassero01" "heathmi02" "hebneri01" "henderi01" "hillma01"  "hortowi01" "jacksre01" "johnscl01" "kellypa01" "lansfca01"
 [9971] "lemonch01" "lowenjo01" "lynnfr01"  "martibu01" "maybejo01" "mayle01"   "mcraeha01" "milleri01" "molitpa01" "moneydo01"
 [9981] "moorech02" "morriji01" "mullira01" "murcebo01" "murraed02" "nettlgr01" "oglivbe01" "oliveal01" "ortajo01"  "otisam01" 
 [9991] "pacioto01" "parrila02" "perezto01" "pinielo01" "porteda02" "quirkja01" "randowi01" "riceji01"  "rivermi01" "rodriau01"
[10001] "rudijo01"  "singlke01" "smallro02" "spencji01" "stanlfr01" "staubru01" "sundbji01" "thomago01" "thondi01"  "trammal01"
[10011] "washicl01" "watsobo01" "whitalo01" "whitefr01" "whitter01" "wilsowi02" "yastrca01" "yountro01" "alexado01" "almonbi01"
[10021] "ashbyal01" "bakerdu01" "benchjo01" "bergmda01" "bevacku01" "biittla01" "bluevi01"  "blylebe01" "bondsbo01" "boonebo01"
[10031] "bowala01"  "brookhu01" "bucknbi01" "burrira01" "burroje01" "cabelen01" "candejo01" "carltst01" "cartega01" "cedence01"
[10041] "ceyro01"   "chambch01" "clarkja01" "collida02" "conceda01" "cruzjo01"  "curtijo01" "dawsoan01" "dejesiv01" "driesda01"
[10051] "easlemi01" "evansda01" "fergujo01" "foliti01"  "forscbo01" "forscke01" "fostege01" "garneph01" "garvest01" "geronce01"
[10061] "griffke01" "grossgr01" "guerrpe01" "hendeke01" "hendrge01" "hernake01" "herrto01"  "hootobu01" "johnscl01" "johnsja01"
[10071] "jorgemi01" "kingmda01" "kneppbo01" "lacyle01"  "lampde01"  "leonaje01" "lopesda01" "lummi01"   "luzingr01" "maddoga01"
[10081] "madlobi01" "matthga01" "maymi01"   "mazzile01" "mccovwi01" "mcwilla01" "mondari01" "montawi01" "moralje01" "morgajo02"
[10091] "mumphje01" "murphda05" "niekrjo01" "niekrph01" "oberkke01" "parkeda01" "parrila01" "puhlte01"  "reuscri01" "reussje01"
[10101] "reynocr01" "robinbi02" "robindo01" "rodriau01" "rosepe01"  "roystje01" "russebi01" "ruthvdi01" "ryanno01"  "sandesc01"
[10111] "schmimi01" "simmote01" "smithlo01" "smithoz01" "smithre06" "speiech01" "spilmha01" "stargwi01" "suttodo01" "templga01"
[10121] "tenacge01" "thomade01" "trevial01" "trillma01" "unserde01" "wallide01" "washicl01" "welchbo01" "whitsed01" "winfida01"
[10131] "wiseri01"  "wohlfji01" "yeagest01" "youngjo02" "almonbi01" "baineha01" "bandosa01" "baylodo01" "belanma01" "bellbu01" 
[10141] "beniqju01" "bosleth01" "brettge01" "burroje01" "campabe01" "carewro01" "ceronri01" "coopece01" "crowlte01" "decindo01"
[10151] "dempsri01" "dilonmi01" "downibr01" "dwyerji01" "evansdw01" "fiskca01"  "gamblos01" "gantnji01" "geronce01" "gibsoki01"
[10161] "grichbo01" "griffal01" "groteje01" "grubbjo01" "harrato01" "hassero01" "heathmi02" "hebneri01" "hendeda01" "henderi01"
[10171] "jacksre01" "johnscl01" "kellypa01" "lansfca01" "lemonch01" "lowenjo01" "luzingr01" "lynnfr01"  "martibu01" "maybejo01"
[10181] "mayle01"   "mcraeha01" "milleri01" "molitpa01" "moneydo01" "moorech02" "morriji01" "mumphje01" "murcebo01" "murraed02"
[10191] "nettlgr01" "oglivbe01" "oliveal01" "ortajo01"  "otisam01"  "pacioto01" "parrila02" "perezto01" "pinielo01" "quirkja01"
[10201] "randowi01" "riceji01"  "rivermi01" "rodriau01" "rudijo01"  "simmote01" "singlke01" "smallro02" "spencji01" "spencji01"
[10211] "stanlfr01" "sundbji01" "thomago01" "thornan01" "trammal01" "watsobo01" "whitalo01" "whitefr01" "whitter01" "wilsowi02"
[10221] "winfida01" "yastrca01" "yountro01" "alexado01" "ashbyal01" "bakerdu01" "benchjo01" "bergmda01" "biittla01" "bondsbo01"
[10231] "boonebo01" "bowala01"  "brookhu01" "bucknbi01" "butlebr01" "cabelen01" "carltst01" "cartega01" "cedence01" "ceyro01"  
[10241] "chambch01" "clarkja01" "collida02" "conceda01" "cruzjo01"  "dawsoan01" "dejesiv01" "driesda01" "easlemi01" "evansda01"
[10251] "foliti01"  "fostege01" "garneph01" "garneph01" "garvest01" "griffke01" "grossgr01" "guerrpe01" "hendrge01" "hernake01"
[10261] "herrto01"  "johnsja01" "jorgemi01" "kingmda01" "lacyle01"  "leonaje01" "lopesda01" "lummi01"   "maddoga01" "madlobi01"
[10271] "matthga01" "maymi01"   "mazzile01" "mondari01" "montawi01" "moralje01" "morgajo02" "murphda05" "niekrjo01" "niekrph01"
[10281] "oberkke01" "parkeda01" "parrila01" "penato01"  "porteda02" "puhlte01"  "raineti01" "reussje01" "reynocr01" "robinbi02"
[10291] "rosepe01"  "roystje01" "russebi01" "ruthvdi01" "ryanno01"  "schmimi01" "seaveto01" "smithlo01" "smithoz01" "speiech01"
[10301] "stargwi01" "staubru01" "suttodo01" "templga01" "tenacge01" "thomade01" "thondi01"  "trevial01" "trillma01" "unserde01"
[10311] "valenfe01" "wallati01" "wallide01" "washicl01" "wohlfji01" "yeagest01" "youngjo02" "almonbi01" "baineha01" "baylodo01"
[10321] "bellbu01"  "beniqju01" "boggswa01" "boonebo01" "brettge01" "brunato01" "burroje01" "cabelen01" "carewro01" "ceronri01"
[10331] "collida02" "coopece01" "crowlte01" "decindo01" "dempsri01" "dilonmi01" "downibr01" "dwyerji01" "eisenji01" "evansdw01"
[10341] "fergujo01" "fiskca01"  "foliti01"  "gaettga01" "gamblos01" "gantnji01" "geronce01" "gibsoki01" "grichbo01" "griffal01"
[10351] "griffke01" "grubbjo01" "hairsje01" "harrato01" "hassero01" "heathmi02" "hebneri01" "hendeda01" "henderi01" "hillma01" 
[10361] "jacksre01" "johnscl01" "lansfca01" "lemonch01" "lopesda01" "lowenjo01" "luzingr01" "lynnfr01"  "martibu01" "maybejo01"
[10371] "mayle01"   "mazzile01" "mazzile01" "mcraeha01" "milleri01" "molitpa01" "moneydo01" "moorech02" "morriji01" "mullira01"
[10381] "mumphje01" "murcebo01" "murraed02" "nettlgr01" "oglivbe01" "otisam01"  "pacioto01" "parrila01" "parrila02" "perezto01"
[10391] "phillto02" "pinielo01" "quirkja01" "randowi01" "riceji01"  "ripkeca01" "rivermi01" "rodriau01" "rudijo01"  "simmote01"
[10401] "singlke01" "slaugdo01" "smallro02" "spencji01" "stanlfr01" "sundbji01" "thomago01" "thornan01" "trammal01" "whitalo01"
[10411] "whitefr01" "whitter01" "wilsowi02" "winfida01" "yastrca01" "yountro01" "ashbyal01" "bakerdu01" "belanma01" "benchjo01"
[10421] "bergmda01" "bevacku01" "biittla01" "bowala01"  "braunst01" "brookhu01" "bucknbi01" "butlebr01" "candejo01" "carltst01"
[10431] "cartega01" "cedence01" "ceyro01"   "chambch01" "clarkja01" "conceda01" "cruzjo01"  "davisch01" "dawsoan01" "dejesiv01"
[10441] "driesda01" "easlemi01" "evansda01" "forscbo01" "fostege01" "garneph01" "garvest01" "grossgr01" "guerrpe01" "gullibi01"
[10451] "gwynnto01" "hebneri01" "hendrge01" "hernake01" "herrto01"  "jenkife01" "johnsja01" "jorgemi01" "kingmda01" "kneppbo01"
[10461] "lacyle01"  "leonaje01" "maddoga01" "madlobi01" "matthga01" "maymi01"   "mcgeewi01" "mondari01" "moralje01" "morgajo02"
[10471] "morriji01" "murphda05" "niekrjo01" "niekrph01" "oberkke01" "oliveal01" "ortajo01"  "parkeda01" "penato01"  "porteda02"
[10481] "puhlte01"  "raineti01" "reussje01" "reynocr01" "rhoderi01" "robinbi02" "robinbi02" "robindo01" "rosepe01"  "roystje01"
[10491] "russebi01" "ruthvdi01" "ryanno01"  "sandbry01" "sandesc01" "schmimi01" "smithlo01" "smithoz01" "smithre06" "speiech01"
[10501] "spilmha01" "stargwi01" "staubru01" "suttodo01" "templga01" "tenacge01" "thomade01" "thondi01"  "trevial01" "trillma01"
[10511] "valenfe01" "wallati01" "wallide01" "washicl01" "watsobo01" "welchbo01" "wohlfji01" "yeagest01" "youngjo02" "youngjo02"
[10521] "almonbi01" "baineha01" "baylodo01" "bellbu01"  "beniqju01" "biittla01" "boggswa01" "boonebo01" "brettge01" "brunato01"
[10531] "burroje01" "cabelen01" "campabe01" "carewro01" "ceronri01" "colesda01" "collida02" "coopece01" "decindo01" "dempsri01"
[10541] "dilonmi01" "downibr01" "dwyerji01" "evansdw01" "fiskca01"  "fletcsc01" "foliti01"  "francju01" "gaettga01" "gamblos01"
[10551] "gantnji01" "geronce01" "gibsoki01" "grichbo01" "griffal01" "griffke01" "grubbjo01" "hairsje01" "harrato01" "hassero01"
[10561] "heathmi02" "hendeda01" "henderi01" "hillma01"  "jacksre01" "johnscl01" "lansfca01" "lemonch01" "lopesda01" "lowenjo01"
[10571] "luzingr01" "lynnfr01"  "martibu01" "mcraeha01" "milleri01" "molitpa01" "moneydo01" "moorech02" "mullira01" "mumphje01"
[10581] "murraed02" "nettlgr01" "oglivbe01" "ortajo01"  "otisam01"  "pacioto01" "parrila01" "parrila02" "phillto02" "pinielo01"
[10591] "randowi01" "riceji01"  "ripkeca01" "rivermi01" "rodriau01" "schofdi02" "simmote01" "singlke01" "slaugdo01" "smallro02"
[10601] "sundbji01" "thomago01" "thomago01" "thornan01" "trammal01" "trillma01" "whitalo01" "whitefr01" "whitter01" "wilsowi02"
[10611] "winfida01" "yastrca01" "yountro01" "ashbyal01" "bakerdu01" "basske01"  "benchjo01" "bergmda01" "bevacku01" "bosleth01"
[10621] "bowala01"  "braunst01" "brookhu01" "bucknbi01" "butlebr01" "candejo01" "carltst01" "cartega01" "cartejo01" "cedence01"
[10631] "ceyro01"   "chambch01" "clarkja01" "conceda01" "cruzjo01"  "davisch01" "dawsoan01" "dejesiv01" "driesda01" "easlemi01"
[10641] "evansda01" "foleyto02" "forscbo01" "fostege01" "garneph01" "garvest01" "grossgr01" "guerrpe01" "gullibi01" "gwynnto01"
[10651] "harpebr01" "hebneri01" "hendrge01" "hernake01" "hernake01" "herrto01"  "hootobu01" "jenkife01" "johnsja01" "kingmda01"
[10661] "kneppbo01" "lacyle01"  "lapoida01" "leonaje01" "maddoga01" "madlobi01" "maldoca01" "matthga01" "maymi01"   "mazzile01"
[10671] "mcgeewi01" "mcwilla01" "mondari01" "moralje01" "morgajo02" "morriji01" "mumphje01" "murphda05" "niekrjo01" "niekrph01"
[10681] "oberkke01" "oliveal01" "parkeda01" "penaal01"  "penato01"  "perezto01" "porteda02" "puhlte01"  "quirkja01" "raineti01"
[10691] "reussje01" "reynocr01" "rhoderi01" "rosepe01"  "roystje01" "russebi01" "ruthvdi01" "ryanno01"  "samueju01" "sandbry01"
[10701] "schmimi01" "seaveto01" "smithlo01" "smithoz01" "speiech01" "spilmha01" "staubru01" "strawda01" "templga01" "tenacge01"
[10711] "thomade01" "thondi01"  "torremi01" "trevial01" "trillma01" "valenfe01" "wallati01" "wallide01" "washicl01" "watsobo01"
[10721] "welchbo01" "wohlfji01" "yeagest01" "youngjo02" "almonbi01" "baineha01" "baylodo01" "bellbu01"  "beniqju01" "bergmda01"
[10731] "boggswa01" "boonebo01" "brettge01" "brunato01" "bucknbi01" "burroje01" "butlebr01" "carewro01" "cartejo01" "ceronri01"
[10741] "colesda01" "collida02" "coopece01" "decindo01" "dempsri01" "downibr01" "dwyerji01" "easlemi01" "evansda01" "evansdw01"
[10751] "fernato01" "fiskca01"  "fletcsc01" "foliti01"  "francju01" "gaettga01" "gamblos01" "gantnji01" "gibsoki01" "grichbo01"
[10761] "griffal01" "griffke01" "grubbjo01" "hairsje01" "harrato01" "hassero01" "heathmi02" "hendeda01" "henderi01" "hillma01" 
[10771] "jacksre01" "johnscl01" "kingmda01" "lansfca01" "lemonch01" "lopesda01" "lowenjo01" "luzingr01" "lynnfr01"  "martibu01"
[10781] "mcraeha01" "milleri01" "moorech02" "morgajo02" "mullira01" "murraed02" "nixonot01" "oglivbe01" "ortajo01"  "pacioto01"
[10791] "parrila01" "parrila02" "phillto02" "pinielo01" "randowi01" "readyra01" "riceji01"  "ripkeca01" "rivermi01" "schofdi02"
[10801] "simmote01" "singlke01" "slaugdo01" "smallro02" "smallro02" "sundbji01" "thomago01" "thornan01" "trammal01" "whitalo01"
[10811] "whitefr01" "whitter01" "wilsowi02" "winfida01" "yountro01" "ashbyal01" "bakerdu01" "basske01"  "bevacku01" "bosleth01"
[10821] "bowala01"  "braunst01" "brookhu01" "cabelen01" "candejo01" "carltst01" "cartega01" "cedence01" "ceyro01"   "chambch01"
[10831] "clarkja01" "conceda01" "cruzjo01"  "darliro01" "davisch01" "daviser01" "dawsoan01" "dejesiv01" "deleojo01" "dilonmi01"
[10841] "driesda01" "driesda01" "eckerde01" "foleyto02" "fostege01" "garneph01" "garvest01" "goodedw01" "grossgr01" "guerrpe01"
[10851] "gullibi01" "gwynnto01" "harpebr01" "hebneri01" "hendrge01" "hernake01" "herrto01"  "hershor01" "honeyri01" "johnsja01"
[10861] "jorgemi01" "kneppbo01" "koosmje01" "lacyle01"  "lapoida01" "leonaje01" "maddoga01" "madlobi01" "maldoca01" "matthga01"
[10871] "maymi01"   "mazzile01" "mcgeewi01" "mcwilla01" "morriji01" "mumphje01" "murphda05" "nettlgr01" "niekrjo01" "oberkke01"
[10881] "oberkke01" "oliveal01" "oliveal01" "otisam01"  "parkeda01" "penaal01"  "penato01"  "pendlte01" "perezto01" "porteda02"
[10891] "puhlte01"  "raineti01" "reynocr01" "rhoderi01" "rosepe01"  "rosepe01"  "roystje01" "russebi01" "russeje01" "ryanno01" 
[10901] "samueju01" "sandbry01" "schmimi01" "smithlo01" "smithoz01" "speiech01" "spilmha01" "staubru01" "strawda01" "sutclri01"
[10911] "templga01" "thomade01" "thompmi02" "trevial01" "trillma01" "valenfe01" "wallati01" "wallide01" "washicl01" "watsobo01"
[10921] "welchbo01" "whitsed01" "wohlfji01" "yeagest01" "youngjo02" "baineha01" "bakerdu01" "baylodo01" "bellbu01"  "beniqju01"
[10931] "bergmda01" "boggswa01" "boonebo01" "brettge01" "brunato01" "bucknbi01" "burroje01" "butlebr01" "cansejo01" "carewro01"
[10941] "cartejo01" "colesda01" "collida02" "coopece01" "decindo01" "dempsri01" "downibr01" "dwyerji01" "easlemi01" "evansda01"
[10951] "evansdw01" "fernato01" "fieldce01" "fiskca01"  "fletcsc01" "francju01" "gaettga01" "gagnegr01" "gamblos01" "gantnji01"
[10961] "gibsoki01" "grichbo01" "griffal01" "griffke01" "grubbjo01" "guilloz01" "hairsje01" "harrato01" "hassero01" "heathmi02"
[10971] "hendeda01" "henderi01" "hillma01"  "jacksre01" "johnscl01" "johnscl01" "kingmda01" "lacyle01"  "lansfca01" "lemonch01"
[10981] "lynnfr01"  "martibu01" "mcraeha01" "molitpa01" "moorech02" "mullira01" "murraed02" "nixonot01" "oglivbe01" "oliveal01"
[10991] "ortajo01"  "pacioto01" "parrila01" "parrila02" "phillto02" "quirkja01" "randowi01" "readyra01" "riceji01"  "ripkeca01"
[11001] "schofdi02" "simmote01" "slaugdo01" "smallro02" "smithlo01" "sundbji01" "tartada01" "thomago01" "thornan01" "trammal01"
[11011] "whitalo01" "whitefr01" "whitter01" "wilsowi02" "winfida01" "yountro01" "almonbi01" "ashbyal01" "basske01"  "bedrost01"
[11021] "bellbu01"  "bevacku01" "bosleth01" "bowala01"  "braunst01" "brookhu01" "cabelen01" "cabelen01" "cartega01" "cedence01"
[11031] "cedence01" "ceronri01" "ceyro01"   "chambch01" "clarkja01" "conceda01" "cruzjo01"  "darliro01" "daultda01" "davisch01"
[11041] "daviser01" "dawsoan01" "dejesiv01" "dilonmi01" "driesda01" "driesda01" "duncama01" "dunstsh01" "eckerde01" "fernasi01"
[11051] "foleyto02" "foleyto02" "fostege01" "galaran01" "garneph01" "garvest01" "goodedw01" "gottji01"  "grossgr01" "grosske01"
[11061] "guerrpe01" "gullibi01" "gwynnto01" "harpebr01" "hatchbi01" "hebneri01" "hendrge01" "hernake01" "herrto01"  "hershor01"
[11071] "jorgemi01" "kneppbo01" "lapoida01" "leonaje01" "lopesda01" "maddoga01" "madlobi01" "madlobi01" "maldoca01" "matthga01"
[11081] "mazzile01" "mcgeewi01" "morriji01" "mumphje01" "murphda05" "nettlgr01" "niekrjo01" "oberkke01" "oliveal01" "pacioto01"
[11091] "parkeda01" "penato01"  "pendlte01" "perezto01" "porteda02" "puhlte01"  "raineti01" "reuscri01" "reussje01" "reynocr01"
[11101] "rhoderi01" "rosepe01"  "roystje01" "russebi01" "ryanno01"  "samueju01" "sandbry01" "schmimi01" "smithlo01" "smithoz01"
[11111] "speiech01" "spilmha01" "strawda01" "templga01" "thomade01" "thompmi02" "thondi01"  "trevial01" "trillma01" "valenfe01"
[11121] "wallati01" "wallide01" "washicl01" "webstmi01" "welchbo01" "wohlfji01" "yeagest01" "youngjo02" "baineha01" "bakerdu01"
[11131] "baylodo01" "beniqju01" "bergmda01" "boggswa01" "bonilbo01" "boonebo01" "brettge01" "brunato01" "bucknbi01" "butlebr01"
[11141] "cansejo01" "cartejo01" "ceronri01" "colesda01" "collida02" "coopece01" "decindo01" "dempsri01" "downibr01" "dwyerji01"
[11151] "easlemi01" "evansda01" "evansdw01" "fernato01" "fieldce01" "fiskca01"  "fletcsc01" "fostege01" "francju01" "gaettga01"
[11161] "gagnegr01" "gantnji01" "gibsoki01" "grichbo01" "griffal01" "griffke01" "grubbjo01" "guilloz01" "hairsje01" "harrato01"
[11171] "hassero01" "hassero01" "heathmi02" "hendeda01" "hendeda01" "henderi01" "hendrge01" "incavpe01" "jacksre01" "javiest01"
[11181] "johnscl01" "joynewa01" "kingmda01" "lacyle01"  "lansfca01" "lemonch01" "lynnfr01"  "martibu01" "mcgwima01" "mcraeha01"
[11191] "molitpa01" "moorech02" "mullira01" "murraed02" "nixonot01" "oglivbe01" "ortajo01"  "pacioto01" "parrila01" "parrila02"
[11201] "phillto02" "porteda02" "quirkja01" "randowi01" "readyra01" "reedje02"  "riceji01"  "ripkeca01" "schofdi02" "sierrru01"
[11211] "slaugdo01" "smallro02" "smithlo01" "sundbji01" "tartada01" "thomago01" "thomago01" "thornan01" "trammal01" "washicl01"
[11221] "whitalo01" "whitede03" "whitefr01" "whitter01" "wilsowi02" "winfida01" "yeagest01" "yountro01" "aguilri01" "almonbi01"
[11231] "ashbyal01" "basske01"  "bellbu01"  "bellira01" "bondsba01" "bonilbo01" "bosleth01" "brookhu01" "cabelen01" "cartega01"
[11241] "cedence01" "ceyro01"   "chambch01" "clarkja01" "clarkwi02" "conceda01" "cruzjo01"  "darliro01" "daultda01" "davisch01"
[11251] "daviser01" "dawsoan01" "duncama01" "dunstsh01" "eckerde01" "fernasi01" "foleyto02" "foleyto02" "forscbo01" "fostege01"
[11261] "galaran01" "garneph01" "garvest01" "goodedw01" "griffke01" "grossgr01" "grosske01" "guerrpe01" "gullibi01" "gwynnto01"
[11271] "hatchbi01" "heathmi02" "hernake01" "herrto01"  "hershor01" "kneppbo01" "larkiba01" "leonaje01" "lopesda01" "lopesda01"
[11281] "madlobi01" "maldoca01" "martida01" "matthga01" "mazzile01" "mazzile01" "mcgeewi01" "mitchke01" "morriji01" "mumphje01"
[11291] "murphda05" "nettlgr01" "oberkke01" "ojedabo01" "palmera01" "parkeda01" "penato01"  "pendlte01" "perezto01" "puhlte01" 
[11301] "raineti01" "reuscri01" "reynocr01" "rhoderi01" "rosepe01"  "roystje01" "russebi01" "ryanno01"  "samueju01" "sandbry01"
[11311] "sandesc01" "santibe01" "schmimi01" "simmote01" "smithoz01" "smithza01" "speiech01" "spilmha01" "strawda01" "sutclri01"
[11321] "templga01" "thompmi02" "thondi01"  "trevial01" "trillma01" "valenfe01" "wallati01" "wallide01" "washicl01" "webstmi01"
[11331] "welchbo01" "wohlfji01" "youngjo02" "baineha01" "baylodo01" "bellja01"  "beniqju01" "beniqju01" "bergmda01" "boggswa01"
[11341] "boonebo01" "bosleth01" "brettge01" "brunato01" "bucknbi01" "bucknbi01" "burksel01" "butlebr01" "cansejo01" "cartejo01"
[11351] "ceronri01" "ceyro01"   "colesda01" "coopece01" "decindo01" "dempsri01" "downibr01" "dwyerji01" "easlemi01" "eisenji01"
[11361] "evansda01" "evansdw01" "fernato01" "fieldce01" "fiskca01"  "fletcsc01" "francju01" "gaettga01" "gagnegr01" "gantnji01"
[11371] "gibsoki01" "griffal01" "grubbjo01" "guilloz01" "hairsje01" "hassero01" "heathmi02" "hendeda01" "henderi01" "hendrge01"
[11381] "incavpe01" "jacksre01" "javiest01" "joynewa01" "kellyro01" "lacyle01"  "lansfca01" "lemonch01" "lynnfr01"  "madlobi01"
[11391] "matthga01" "mcgrifr01" "mcgwima01" "mclemma01" "molitpa01" "moorech02" "morriji01" "mullira01" "murraed02" "ortajo01" 
[11401] "pacioto01" "parrila01" "phillto02" "polonlu01" "porteda02" "quirkja01" "randowi01" "riceji01"  "ripkeca01" "roystje01"
[11411] "schofdi02" "sierrru01" "slaugdo01" "smallro02" "smithlo01" "stanlmi02" "surhobj01" "tartada01" "thornan01" "trammal01"
[11421] "washicl01" "whitalo01" "whitede03" "whitefr01" "whitter01" "wilsowi02" "winfida01" "yountro01" "almonbi01" "ashbyal01"
[11431] "basske01"  "bellbu01"  "bellira01" "bondsba01" "bonilbo01" "brookhu01" "caminke01" "cartega01" "clarkja01" "clarkwi02"
[11441] "colesda01" "collida02" "conceda01" "cruzjo01"  "darliro01" "darwida01" "daultda01" "davisch01" "daviser01" "dawsoan01"
[11451] "driesda01" "duncama01" "dunstsh01" "easlemi01" "foleyto02" "forscbo01" "galaran01" "gantro01"  "garneph01" "garneph01"
[11461] "garvest01" "goodedw01" "griffke01" "grossgr01" "grosske01" "guerrpe01" "gullibi01" "gwynnto01" "hatchbi01" "hernake01"
[11471] "herrto01"  "hershor01" "johnsla03" "kneppbo01" "larkiba01" "leonaje01" "madlobi01" "magadda01" "maldoca01" "martida01"
[11481] "mazzile01" "mcgeewi01" "mitchke01" "mitchke01" "morriji01" "moyerja01" "mumphje01" "murphda05" "nettlgr01" "oberkke01"
[11491] "oneilpa01" "palmera01" "parkeda01" "parrila02" "penato01"  "pendlte01" "powerte01" "puhlte01"  "raineti01" "readyra01"
[11501] "reedje02"  "reuscri01" "reynocr01" "ryanno01"  "samueju01" "sandbry01" "santibe01" "schmimi01" "simmote01" "smithoz01"
[11511] "smithza01" "speiech01" "spilmha01" "strawda01" "sundbji01" "sutclri01" "templga01" "thompmi02" "thondi01"  "trevial01"
[11521] "trillma01" "valenfe01" "wallati01" "wallide01" "webstmi01" "welchbo01" "whitsed01" "willima04" "youngjo02" "anderbr01"
[11531] "anderbr01" "baineha01" "baylodo01" "bellja01"  "beniqju01" "bergmda01" "boggswa01" "boonebo01" "bordepa01" "bosleth01"
[11541] "brettge01" "bucknbi01" "buhneja01" "buhneja01" "burksel01" "cansejo01" "cartejo01" "ceronri01" "clarkja01" "colesda01"
[11551] "cruzjo01"  "davisch01" "downibr01" "duceyro01" "dwyerji01" "eisenji01" "evansda01" "evansdw01" "fernato01" "fieldce01"
[11561] "fiskca01"  "fletcsc01" "francju01" "gaettga01" "gagnegr01" "gantnji01" "guilloz01" "hamilda02" "harpebr01" "hassero01"
[11571] "heathmi02" "hendeda01" "henderi01" "hendrge01" "herrto01"  "incavpe01" "javiest01" "johnsla03" "joynewa01" "kellyro01"
[11581] "kreutch01" "lansfca01" "lemonch01" "leonaje01" "lynnfr01"  "lynnfr01"  "mcgrifr01" "mcgwima01" "mclemma01" "molitpa01"
[11591] "morriji01" "mullira01" "murraed02" "obriech01" "parkeda01" "parrila01" "parrila01" "phillto02" "polonlu01" "quirkja01"
[11601] "randowi01" "riceji01"  "ripkeca01" "schofdi02" "sheffga01" "sierrru01" "slaugdo01" "stanlmi02" "sundbji01" "surhobj01"
[11611] "tartada01" "trammal01" "velarra01" "washicl01" "whitalo01" "whitede03" "whitefr01" "whitter01" "wilsowi02" "winfida01"
[11621] "yountro01" "alomaro01" "ashbyal01" "basske01"  "belchti01" "bellbu01"  "bellbu01"  "bellira01" "biggicr01" "bondsba01"
[11631] "bonilbo01" "brookhu01" "brunato01" "butlebr01" "caminke01" "cartega01" "clarkwi02" "colesda01" "collida02" "conceda01"
[11641] "coneda01"  "darliro01" "darwida01" "daultda01" "daviser01" "dawsoan01" "deleojo01" "dempsri01" "dunstsh01" "fernasi01"
[11651] "foleyto02" "galaran01" "gantro01"  "gibsoki01" "glavito02" "goodedw01" "gracema01" "griffal01" "griffke01" "griffke01"
[11661] "grossgr01" "grosske01" "guerrpe01" "guerrpe01" "gwynnto01" "hatchbi01" "hernake01" "herrto01"  "hershor01" "jacksda02"
[11671] "jacksda03" "jeffegr01" "larkiba01" "learyti01" "leonaje01" "maddugr01" "magadda01" "maldoca01" "martida01" "martida01"
[11681] "martide01" "mazzile01" "mcgeewi01" "mitchke01" "morriji01" "moyerja01" "mumphje01" "murphda05" "nettlgr01" "nixonot01"
[11691] "oberkke01" "oberkke01" "ojedabo01" "oneilpa01" "palmera01" "parenma01" "parrila02" "penato01"  "pendlte01" "princto01"
[11701] "puhlte01"  "raineti01" "readyra01" "reedje02"  "reedje02"  "reuscri01" "reynocr01" "robindo01" "roystje01" "ryanno01" 
[11711] "samueju01" "sandbry01" "santibe01" "schmimi01" "simmote01" "smithlo01" "smithoz01" "speiech01" "strawda01" "sundbji01"
[11721] "sutclri01" "templga01" "thompmi02" "thondi01"  "trevial01" "trillma01" "wallati01" "wallide01" "wallide01" "webstmi01"
[11731] "webstmi01" "whitsed01" "willima04" "youngjo02" "anderbr01" "baineha01" "baineha01" "bellbu01"  "bergmda01" "bicheda01"
[11741] "boggswa01" "boonebo01" "bordepa01" "brettge01" "bucknbi01" "buhneja01" "burksel01" "cansejo01" "cartejo01" "ceronri01"
[11751] "colesda01" "davisch01" "downibr01" "duceyro01" "dwyerji01" "eisenji01" "evansdw01" "fernato01" "finlest01" "fiskca01" 
[11761] "fletcsc01" "fletcsc01" "francju01" "gaettga01" "gagnegr01" "gantnji01" "gonzaju03" "griffke02" "guilloz01" "harpebr01"
[11771] "hassero01" "heathmi02" "hendeda01" "henderi01" "henderi01" "hillgl01"  "incavpe01" "javiest01" "johnsla03" "joynewa01"
[11781] "kellyro01" "kreutch01" "lansfca01" "lemonch01" "leonaje01" "lynnfr01"  "martied01" "mazzile01" "mcgrifr01" "mcgwima01"
[11791] "mclemma01" "molitpa01" "mullira01" "obriech01" "palmera01" "parkeda01" "parrila02" "phillto02" "polonlu01" "polonlu01"
[11801] "quirkja01" "riceji01"  "ripkeca01" "schofdi02" "sheffga01" "sierrru01" "slaugdo01" "sosasa01"  "sosasa01"  "stanlmi02"
[11811] "sundbji01" "surhobj01" "tartada01" "trammal01" "vaughgr01" "velarra01" "vizquom01" "washicl01" "whitalo01" "whitede03"
[11821] "whitefr01" "whitter01" "wilsowi02" "yountro01" "alomaro01" "ashbyal01" "basske01"  "belchti01" "bellira01" "bellja01" 
[11831] "bielemi01" "biggicr01" "bondsba01" "bonilbo01" "brookhu01" "brunato01" "butlebr01" "caminke01" "cartega01" "clarkja01"
[11841] "clarkwi02" "collida02" "coneda01"  "darliro01" "daultda01" "daviser01" "dawsoan01" "deleojo01" "dempsri01" "duncama01"
[11851] "duncama01" "dunstsh01" "evansda01" "fernasi01" "foleyto02" "galaran01" "gantro01"  "gibsoki01" "girarjo01" "glavito02"
[11861] "gracema01" "griffal01" "griffke01" "grissma02" "grossgr01" "grosske01" "guerrpe01" "gwynnto01" "harrile01" "harrile01"
[11871] "hatchbi01" "hatchbi01" "hayesch01" "hernake01" "herrto01"  "hershor01" "hillke01"  "hurstbr01" "jacksda03" "jacksda03"
[11881] "jeffegr01" "justida01" "langsma01" "larkiba01" "maddugr01" "magadda01" "maldoca01" "martida01" "martide01" "mazzile01"
[11891] "mcgeewi01" "mitchke01" "murphda05" "murraed02" "nixonot01" "oberkke01" "ojedabo01" "olivejo01" "oneilpa01" "parenma01"
[11901] "penato01"  "pendlte01" "princto01" "puhlte01"  "raineti01" "randowi01" "rasmude01" "readyra01" "readyra01" "reedje02" 
[11911] "reuscri01" "reynocr01" "robindo01" "samueju01" "samueju01" "sandbry01" "santibe01" "schmimi01" "smithlo01" "smithoz01"
[11921] "smoltjo01" "strawda01" "sutclri01" "templga01" "thompmi02" "thondi01"  "trevial01" "valenfe01" "wallati01" "wallide01"
[11931] "webstmi01" "whitsed01" "willima04" "youngjo02" "zeileto01" "alomasa02" "anderbr01" "baergca01" "baineha01" "baineha01"
[11941] "bergmda01" "bicheda01" "boggswa01" "boonebo01" "bordepa01" "brettge01" "brunato01" "buhneja01" "burksel01" "cansejo01"
[11951] "ceronri01" "colesda01" "colesda01" "davisch01" "downibr01" "duceyro01" "dwyerji01" "eisenji01" "evansdw01" "fernato01"
[11961] "fieldce01" "finlest01" "fiskca01"  "fletcsc01" "francju01" "gaettga01" "gagnegr01" "gantnji01" "gonzaju03" "griffke01"
[11971] "griffke02" "guilloz01" "hamilda02" "harpebr01" "hassero01" "heathmi02" "hendeda01" "henderi01" "hernake01" "hillgl01" 
[11981] "incavpe01" "johnsla03" "joynewa01" "kellyro01" "lansfca01" "lemonch01" "leonaje01" "leyriji01" "maldoca01" "martied01"
[11991] "martiti02" "mcgeewi01" "mcgrifr01" "mcgwima01" "molitpa01" "mullira01" "myersgr01" "obriech01" "olerujo01" "palmera01"
[12001] "parkeda01" "parrila02" "penato01"  "phillto02" "polonlu01" "quirkja01" "randowi01" "ripkeca01" "schofdi02" "seguida01"
[12011] "sheffga01" "sierrru01" "sojolu01"  "sosasa01"  "stanlmi02" "surhobj01" "tartada01" "thomafr04" "trammal01" "vaughgr01"
[12021] "velarra01" "venturo01" "vizquom01" "washicl01" "webstmi01" "whitalo01" "whitede03" "whitefr01" "whitema01" "wilsowi02"
[12031] "winfida01" "winfida01" "yountro01" "alomaro01" "basske01"  "bellira01" "bellja01"  "benesan01" "biggicr01" "bondsba01"
[12041] "bonilbo01" "brookhu01" "brunato01" "burkejo03" "butlebr01" "caminke01" "cartega01" "cartejo01" "clarkja01" "clarkwi02"
[12051] "collida02" "coneda01"  "daultda01" "daviser01" "dawsoan01" "deleojo01" "dempsri01" "duncama01" "dunstsh01" "fernasi01"
[12061] "foleyto02" "galaran01" "gantro01"  "gibsoki01" "girarjo01" "glavito02" "goodedw01" "gracema01" "griffal01" "griffke01"
[12071] "grissma02" "grosske01" "guerrpe01" "gullibi01" "gwynnto01" "harrile01" "hatchbi01" "hayesch01" "herrto01"  "herrto01" 
[12081] "hurstbr01" "jacksda03" "javiest01" "jeffegr01" "justida01" "lankfra01" "larkiba01" "lynnfr01"  "maddugr01" "magadda01"
[12091] "martida01" "martide01" "mcgeewi01" "mitchke01" "morgami01" "mulhote01" "murphda05" "murphda05" "murraed02" "nixonot01"
[12101] "oberkke01" "obriech01" "offerjo01" "olivejo01" "oneilpa01" "parenma01" "pendlte01" "portuma01" "raineti01" "randowi01"
[12111] "rasmude01" "readyra01" "reedje02"  "robindo01" "samueju01" "sandbry01" "santibe01" "slaugdo01" "smithlo01" "smithoz01"
[12121] "smoltjo01" "strawda01" "templga01" "thompmi02" "thondi01"  "trevial01" "valenfe01" "violafr01" "vizcajo01" "walkela01"
[12131] "wallati01" "wallide01" "whitsed01" "whitter01" "willima04" "zeileto01" "alomaro01" "alomasa02" "anderbr01" "baergca01"
[12141] "baineha01" "bergmda01" "bicheda01" "boggswa01" "bordepa01" "bordimi01" "brettge01" "brunato01" "buhneja01" "burksel01"
[12151] "cansejo01" "cartejo01" "clarkja01" "davisch01" "dempsri01" "downibr01" "duceyro01" "eisenji01" "evansdw01" "fieldce01"
[12161] "fiskca01"  "fletcsc01" "francju01" "gaettga01" "gagnegr01" "gantnji01" "gibsoki01" "gonzaju03" "griffke01" "griffke02"
[12171] "guilloz01" "hamilda02" "harpebr01" "hendeda01" "henderi01" "hernajo01" "hillgl01"  "hillgl01"  "incavpe01" "johnsla03"
[12181] "joynewa01" "kellyro01" "leyriji01" "maldoca01" "maldoca01" "martied01" "martiti02" "maynebr01" "mcgwima01" "molitpa01"
[12191] "mullira01" "myersgr01" "olerujo01" "palmede01" "palmera01" "parkeda01" "parrila02" "penato01"  "phillto02" "polonlu01"
[12201] "quirkja01" "raineti01" "randowi01" "ripkeca01" "rodriiv01" "schofdi02" "seguida01" "sheffga01" "sierrru01" "sojolu01" 
[12211] "sosasa01"  "stanlmi02" "surhobj01" "tartada01" "thomafr04" "thomeji01" "trammal01" "vaughgr01" "velarra01" "venturo01"
[12221] "vizquom01" "whitalo01" "whitede03" "whitema01" "whitema01" "whitter01" "willibe02" "wilsowi02" "winfida01" "yountro01"
[12231] "bagweje01" "basske01"  "belchti01" "bellira01" "bellja01"  "benesan01" "biggicr01" "blackbu02" "bondsba01" "bonilbo01"
[12241] "brookhu01" "burkejo03" "butlebr01" "caminke01" "cartega01" "ceronri01" "clarkwi02" "coneda01"  "daultda01" "daviser01"
[12251] "dawsoan01" "duncama01" "dunstsh01" "fernato01" "finlest01" "fletcda01" "foleyto02" "galaran01" "gantro01"  "glavito02"
[12261] "gonzalu01" "goodedw01" "gracema01" "griffal01" "grissma02" "guerrpe01" "gwynnto01" "hanseda01" "harnipe01" "harrile01"
[12271] "hassero01" "hatchbi01" "hayesch01" "heathmi02" "herrto01"  "herrto01"  "hillke01"  "hurstbr01" "jacksda03" "javiest01"
[12281] "jeffegr01" "justida01" "lankfra01" "larkiba01" "lewisda01" "loftoke01" "maddugr01" "magadda01" "martida01" "martide01"
[12291] "mcgeewi01" "mcgrifr01" "mclemma01" "merceor01" "mitchke01" "morgami01" "mulhote01" "murphda05" "murraed02" "nixonot01"
[12301] "oberkke01" "obriech01" "offerjo01" "ojedabo01" "olivaom01" "olivejo01" "oneilpa01" "pendlte01" "readyra01" "reedje02" 
[12311] "samueju01" "sandbry01" "santibe01" "slaugdo01" "smithlo01" "smithoz01" "smithza01" "smoltjo01" "strawda01" "templga01"
[12321] "templga01" "thompmi02" "thondi01"  "vandejo02" "violafr01" "vizcajo01" "walkela01" "wallati01" "webstmi01" "webstmi01"
[12331] "wilkiri01" "willima04" "zeileto01" "alomaro01" "alomasa02" "anderbr01" "baergca01" "baineha01" "bergmda01" "bicheda01"
[12341] "boggswa01" "boonebr01" "bordepa01" "bordimi01" "brettge01" "brookhu01" "brunato01" "buhneja01" "burksel01" "cansejo01"
[12351] "cansejo01" "cartejo01" "clarkja01" "coninje01" "davisch01" "downibr01" "duceyro01" "easleda01" "eisenji01" "fieldce01"
[12361] "fiskca01"  "flahejo01" "fletcsc01" "francju01" "gaettga01" "gagnegr01" "gantnji01" "gonzaju03" "griffal01" "griffke02"
[12371] "hamilda02" "harpebr01" "hatchbi01" "hayesch01" "hendeda01" "henderi01" "hillgl01"  "jeffegr01" "johnsla03" "joynewa01"
[12381] "kellyro01" "kentje01"  "kreutch01" "lansfca01" "leyriji01" "loftoke01" "maldoca01" "martied01" "martiti02" "maynebr01"
[12391] "mcgwima01" "mclemma01" "mitchke01" "molitpa01" "myersgr01" "oberkke01" "olerujo01" "palmede01" "palmera01" "parrila02"
[12401] "parrila02" "penato01"  "phillto02" "polonlu01" "quirkja01" "raineti01" "readyra01" "ripkeca01" "rodriiv01" "samueju01"
[12411] "seguida01" "sierrru01" "sierrru01" "sojolu01"  "stanlmi02" "surhobj01" "tartada01" "thomafr04" "thomeji01" "thondi01" 
[12421] "trammal01" "vaughgr01" "velarra01" "venturo01" "vizquom01" "whitalo01" "whitede03" "whitema01" "willibe02" "wilsowi02"
[12431] "winfida01" "yountro01" "aloumo01"  "bagweje01" "basske01"  "basske01"  "belchti01" "bellira01" "bellja01"  "benesan01"
[12441] "biggicr01" "blackbu02" "bondsba01" "bonilbo01" "burkejo03" "butlebr01" "caminke01" "candito01" "cartega01" "ceronri01"
[12451] "clarkwi02" "claytro01" "colbrgr01" "colesda01" "coneda01"  "cordewi01" "cormirh01" "daultda01" "daviser01" "dawsoan01"
[12461] "duncama01" "dunstsh01" "fernasi01" "fernato01" "finlest01" "fletcda01" "foleyto02" "galaran01" "gantro01"  "gibsoki01"
[12471] "girarjo01" "glavito02" "gonzalu01" "goodedw01" "goodwto01" "gracema01" "grissma02" "grosske01" "guerrpe01" "gwynnto01"
[12481] "hanseda01" "harnipe01" "harrile01" "hatchbi01" "hershor01" "hillke01"  "hurstbr01" "incavpe01" "jacksda03" "javiest01"
[12491] "javiest01" "jordabr01" "justida01" "kentje01"  "lankfra01" "larkiba01" "lewisda01" "maddugr01" "magadda01" "martida01"
[12501] "martide01" "mcgeewi01" "mcgrifr01" "merceor01" "morgami01" "mulhote01" "murphda05" "murraed02" "nixonot01" "obriech01"
[12511] "offerjo01" "olivaom01" "olivejo01" "oneilpa01" "pendlte01" "piazzmi01" "randowi01" "samueju01" "sanchre01" "sandbry01"
[12521] "sandere02" "santibe01" "schilcu01" "schofdi02" "sheffga01" "slaugdo01" "smithlo01" "smithoz01" "smoltjo01" "sosasa01" 
[12531] "strawda01" "swindgr01" "thompmi02" "vandejo02" "vizcajo01" "walkela01" "wallati01" "webstmi01" "wilkiri01" "willima04"
[12541] "younger01" "zeileto01" "alomaro01" "alomasa02" "anderbr01" "baergca01" "baineha01" "boggswa01" "boonebr01" "bordepa01"
[12551] "bordimi01" "brettge01" "brookhu01" "brunato01" "buhneja01" "burksel01" "cansejo01" "cartejo01" "colesda01" "davisch01"
[12561] "daviser01" "dawsoan01" "duceyro01" "easleda01" "edmonji01" "fernato01" "fieldce01" "fiskca01"  "fletcsc01" "francju01"
[12571] "gaettga01" "gaettga01" "gagnegr01" "gibsoki01" "gomezch02" "gonzaju03" "griffal01" "griffke02" "guilloz01" "hamilda02"
[12581] "hammoje01" "harpebr01" "hatchbi01" "hendeda01" "henderi01" "henderi01" "hillgl01"  "jacksda03" "javiest01" "johnsla03"
[12591] "joynewa01" "kreutch01" "leyriji01" "loftoke01" "magadda01" "maldoca01" "martied01" "martiti02" "maynebr01" "mcgwima01"
[12601] "mclemma01" "molitpa01" "myersgr01" "olerujo01" "oneilpa01" "palmede01" "palmera01" "parenma01" "penato01"  "phillto02"
[12611] "polonlu01" "raineti01" "ramirma02" "ripkeca01" "rodriiv01" "schofdi02" "seguida01" "sierrru01" "snowjt01"  "stanlmi02"
[12621] "surhobj01" "tartada01" "thomafr04" "thomeji01" "thondi01"  "trammal01" "valenjo03" "vaughgr01" "velarra01" "venturo01"
[12631] "vizquom01" "whitalo01" "whitede03" "willibe02" "willige02" "winfida01" "yountro01" "aloumo01"  "astacpe01" "ausmubr01"
[12641] "bagweje01" "basske01"  "belchti01" "bellira01" "bellja01"  "benesan01" "bicheda01" "biggicr01" "bondsba01" "bonilbo01"
[12651] "burkejo03" "burnije01" "butlebr01" "caminke01" "candito01" "castivi02" "clarkwi02" "claytro01" "colbrgr01" "coninje01"
[12661] "cordewi01" "daultda01" "daviser01" "duncama01" "eisenji01" "fernato01" "finlest01" "fletcda01" "foleyto02" "galaran01"
[12671] "gantro01"  "girarjo01" "glavito02" "gonzalu01" "goodedw01" "gracema01" "grissma02" "grosske01" "gwynnto01" "hanseda01"
[12681] "harnipe01" "harrile01" "hayesch01" "hershor01" "hillgl01"  "hillke01"  "houghch01" "incavpe01" "jacksda02" "jacksda03"
[12691] "jeffegr01" "jordabr01" "justida01" "kellyro01" "kentje01"  "lankfra01" "larkiba01" "lewisda01" "maddugr01" "magadda01"
[12701] "maldoca01" "martida01" "martide01" "mcgeewi01" "mcgrifr01" "mcgrifr01" "merceor01" "mitchke01" "mondera01" "morgami01"
[12711] "mulhote01" "murraed02" "nixonot01" "obriech01" "offerjo01" "olivejo01" "pendlte01" "piazzmi01" "portuma01" "prattto02"
[12721] "princto01" "readyra01" "reedje02"  "samueju01" "sanchre01" "sandbry01" "sandere02" "santibe01" "schilcu01" "sheffga01"
[12731] "sheffga01" "slaugdo01" "smithlo01" "smithoz01" "smoltjo01" "sosasa01"  "strawda01" "swindgr01" "tananfr01" "thompmi02"
[12741] "vandejo02" "vizcajo01" "walkela01" "wallati01" "webstmi01" "whitema01" "whitero02" "wilkiri01" "willima04" "wilsowi02"
[12751] "younger01" "zeileto01" "alomaro01" "alomasa02" "anderbr01" "baergca01" "baineha01" "boggswa01" "bordepa01" "bordimi01"
[12761] "brookhu01" "brunato01" "buhneja01" "cansejo01" "cartejo01" "cirilje01" "clarkwi02" "colesda01" "davisch01" "daviser01"
[12771] "dawsoan01" "delgaca01" "easleda01" "edmonji01" "fieldce01" "fletcsc01" "francju01" "gaettga01" "gagnegr01" "gibsoki01"
[12781] "gomezch02" "gonzaal01" "gonzaju03" "griffke02" "guilloz01" "hamilda02" "hammoje01" "harpebr01" "hatchbi01" "hendeda01"
[12791] "henderi01" "jacksda03" "javiest01" "johnsla03" "joynewa01" "kreutch01" "leyriji01" "loftoke01" "maldoca01" "martied01"
[12801] "martiti02" "maynebr01" "mcgwima01" "mclemma01" "molitpa01" "murraed02" "myersgr01" "nixonot01" "olerujo01" "oneilpa01"
[12811] "palmede01" "palmera01" "penato01"  "phillto02" "polonlu01" "raineti01" "ramirma02" "ripkeca01" "rodriiv01" "samueju01"
[12821] "schofdi02" "sierrru01" "smithlo01" "snowjt01"  "sojolu01"  "stanlmi02" "surhobj01" "tartada01" "thomafr04" "thomeji01"
[12831] "trammal01" "valenjo03" "vaughgr01" "velarra01" "venturo01" "vizquom01" "whitalo01" "whitede03" "willibe02" "willige02"
[12841] "winfida01" "aloumo01"  "ausmubr01" "bagweje01" "basske01"  "bellira01" "bellja01"  "bicheda01" "biggicr01" "bondsba01"
[12851] "bonilbo01" "boonebr01" "burkejo03" "burksel01" "burnije01" "butlebr01" "caminke01" "candito01" "castivi02" "claytro01"
[12861] "colbrgr01" "coninje01" "cordewi01" "daultda01" "duncama01" "dunstsh01" "eisenji01" "evereca01" "fernato01" "finlest01"
[12871] "fletcda01" "floydcl01" "foleyto02" "galaran01" "girarjo01" "glavito02" "gonzalu01" "gracema01" "grissma02" "gwynnto01"
[12881] "harrile01" "hatchbi01" "hayesch01" "hernajo01" "hillgl01"  "incavpe01" "jacksda02" "jeffegr01" "jordabr01" "justida01"
[12891] "kellyro01" "kellyro01" "kentje01"  "kleskry01" "lankfra01" "larkiba01" "lewisda01" "lopezja01" "maddugr01" "magadda01"
[12901] "martida01" "mcgeewi01" "mcgrifr01" "merceor01" "mitchke01" "mondera01" "obriech01" "offerjo01" "parenma01" "parrila02"
[12911] "pendlte01" "piazzmi01" "prattto02" "reedje02"  "saberbr01" "sanchre01" "sandbry01" "sandere02" "santibe01" "seguida01"
[12921] "sheffga01" "slaugdo01" "smithoz01" "smithza01" "sosasa01"  "stinnke01" "strawda01" "thompmi02" "vandejo02" "vizcajo01"
[12931] "walkela01" "wallati01" "webstmi01" "whitema01" "whitero02" "wilkiri01" "willima04" "younger01" "zeileto01" "alomaro01"
[12941] "alomasa02" "anderbr01" "baergca01" "baineha01" "basske01"  "boggswa01" "bonilbo01" "bordepa01" "bordimi01" "buhneja01"
[12951] "cansejo01" "cartejo01" "cirilje01" "clarkwi02" "davisch01" "delgaca01" "easleda01" "edmonji01" "fernato01" "fieldce01"
[12961] "flahejo01" "fletcsc01" "gaettga01" "gagnegr01" "gibsoki01" "gomezch02" "gonzaal01" "gonzaju03" "goodwto01" "greensh01"
[12971] "griffke02" "guilloz01" "hamilda02" "hammoje01" "henderi01" "javiest01" "johnsla03" "joynewa01" "kreutch01" "lawtoma02"
[12981] "leyriji01" "loftoke01" "maldoca01" "martida01" "martied01" "martiti02" "maynebr01" "mcgeewi01" "mcgwima01" "mclemma01"
[12991] "molitpa01" "murraed02" "myersgr01" "nevinph01" "nixonot01" "olerujo01" "olivejo01" "oneilpa01" "palmede01" "palmera01"
[13001] "parrila02" "penato01"  "phillto02" "polonlu01" "raineti01" "ramirma02" "ripkeca01" "rodriiv01" "samueju01" "sierrru01"
[13011] "sierrru01" "snowjt01"  "sojolu01"  "stairma01" "stanlmi02" "strawda01" "surhobj01" "tartada01" "tartada01" "thomafr04"
[13021] "thomeji01" "trammal01" "valenjo03" "vaughgr01" "velarra01" "venturo01" "vizquom01" "whitalo01" "whitede03" "whitema01"
[13031] "willibe02" "willige02" "winfida01" "zaungr01"  "aloumo01"  "ausmubr01" "bagweje01" "bellda01"  "bellira01" "bellja01" 
[13041] "bicheda01" "biggicr01" "bondsba01" "bonilbo01" "boonebr01" "burkejo03" "burksel01" "butlebr01" "butlebr01" "caminke01"
[13051] "candito01" "castivi02" "claytro01" "colbrgr01" "colesda01" "coninje01" "cordewi01" "daultda01" "dawsoan01" "duncama01"
[13061] "duncama01" "dunstsh01" "eisenji01" "evereca01" "fasseje01" "finlest01" "fletcda01" "floydcl01" "galaran01" "gantro01" 
[13071] "girarjo01" "glavito02" "gonzalu01" "gonzalu01" "gracema01" "grissma02" "gwynnto01" "hanseda01" "harrile01" "hayesch01"
[13081] "hernajo01" "hillgl01"  "hollato01" "jeffegr01" "jordabr01" "justida01" "kellyro01" "kellyro01" "kentje01"  "kleskry01"
[13091] "lankfra01" "larkiba01" "lewisda01" "lewisda01" "loaizes01" "lopezja01" "mabryjo01" "maddugr01" "magadda01" "martipe02"
[13101] "mcgrifr01" "merceor01" "mondera01" "neaglde01" "nevinph01" "obriech01" "offerjo01" "parenma01" "pendlte01" "piazzmi01"
[13111] "polonlu01" "prattto02" "quantpa01" "reedje02"  "sanchre01" "sandere02" "santibe01" "seguida01" "seguida01" "sheffga01"
[13121] "slaugdo01" "smithoz01" "smoltjo01" "sosasa01"  "stinnke01" "sweenma01" "swindgr01" "thompmi02" "valdeis01" "vandejo02"
[13131] "vizcajo01" "walkela01" "wallati01" "webstmi01" "whitema01" "whitero02" "wilkiri01" "willima04" "younger01" "zeileto01"
[13141] "zeileto01" "alomaro01" "alomasa02" "anderbr01" "ausmubr01" "baergca01" "baineha01" "boggswa01" "bonilbo01" "bordepa01"
[13151] "bordepa01" "bordimi01" "buhneja01" "burnije01" "burnije01" "cansejo01" "cartejo01" "cirilje01" "clarkwi02" "cordewi01"
[13161] "davisch01" "delgaca01" "duncama01" "easleda01" "edmonji01" "fieldce01" "fieldce01" "flahejo01" "francju01" "girarjo01"
[13171] "gomezch02" "gonzaal01" "gonzaju03" "goodwto01" "greensh01" "griffke02" "guilloz01" "hamilda02" "hammoje01" "hayesch01"
[13181] "kellyro01" "kentje01"  "kreutch01" "lawtoma02" "lewisda01" "leyriji01" "loftoke01" "martida01" "martied01" "martiti02"
[13191] "mcgwima01" "mclemma01" "mitchke01" "molitpa01" "murraed02" "murraed02" "myersgr01" "nevinph01" "nixonot01" "obriech01"
[13201] "offerjo01" "olerujo01" "oneilpa01" "palmede01" "palmera01" "parenma01" "penato01"  "phillto02" "polonlu01" "raineti01"
[13211] "ramirma02" "ripkeca01" "rodriiv01" "samueju01" "sierrru01" "sierrru01" "slaugdo01" "snowjt01"  "sojolu01"  "stairma01"
[13221] "stanlmi02" "strawda01" "surhobj01" "tartada01" "thomafr04" "thomeji01" "trammal01" "valenjo03" "vaughgr01" "velarra01"
[13231] "venturo01" "vizcajo01" "vizquom01" "walketo04" "wallati01" "whitema01" "willibe02" "willige02" "willige02" "zaungr01" 
[13241] "zeileto01" "aloumo01"  "astacpe01" "ausmubr01" "baergca01" "bagweje01" "bellda01"  "bellira01" "bellja01"  "benesan01"
[13251] "bicheda01" "biggicr01" "bondsba01" "boonebr01" "bordepa01" "brownke01" "burbada01" "burkejo03" "burksel01" "butlebr01"
[13261] "caminke01" "castivi02" "claytro01" "colbrgr01" "coninje01" "daviser01" "dawsoan01" "dunstsh01" "eisenji01" "evereca01"
[13271] "fasseje01" "finlest01" "flahejo01" "fletcda01" "floydcl01" "gaettga01" "gagnegr01" "galaran01" "gantro01"  "glavito02"
[13281] "gomezch02" "gonzalu01" "gracema01" "grissma02" "gwynnto01" "hanseda01" "harnipe01" "harrile01" "hayesch01" "henderi01"
[13291] "hernajo01" "hillgl01"  "hollato01" "incavpe01" "javiest01" "jeffegr01" "johnsla03" "jordabr01" "joynewa01" "justida01"
[13301] "kentje01"  "kleskry01" "lankfra01" "larkiba01" "leiteal01" "lopezja01" "mabryjo01" "maddugr01" "magadda01" "martipe02"
[13311] "maynebr01" "mcgeewi01" "mcgrifr01" "merceor01" "mitchke01" "mondera01" "neaglde01" "olivejo01" "pendlte01" "pendlte01"
[13321] "piazzmi01" "princto01" "reedje02"  "sanchre01" "sandbry01" "sandere02" "santibe01" "schilcu01" "seguida01" "sheffga01"
[13331] "smithoz01" "smoltjo01" "sosasa01"  "stottto01" "sweenma01" "thompmi02" "trachst01" "valdeis01" "valenfe01" "vandejo02"
[13341] "vaughgr01" "vizcajo01" "walkela01" "wallati01" "whitede03" "whitema01" "whitema01" "whitero02" "wilkiri01" "wilkiri01"
[13351] "willima04" "younger01" "zeileto01" "alomaro01" "alomasa02" "anderbr01" "baineha01" "baineha01" "bellja01"  "boggswa01"
[13361] "bordepa01" "bordimi01" "buhneja01" "burnije01" "cansejo01" "cartejo01" "cirilje01" "clarkwi02" "colbrgr01" "cordewi01"
[13371] "davisch01" "daviser01" "delgaca01" "duceyro01" "duncama01" "duncama01" "easleda01" "edmonji01" "fernato01" "fieldce01"
[13381] "francju01" "francju01" "girarjo01" "gonzaal01" "gonzaju03" "goodwto01" "goodwto01" "greensh01" "griffke02" "grissma02"
[13391] "guilloz01" "hammoje01" "hayesch01" "henderi01" "incavpe01" "jacksda03" "jacksda03" "justida01" "kellyro01" "kellyro01"
[13401] "kreutch01" "lawtoma02" "lewisda01" "leyriji01" "leyriji01" "magadda01" "martida01" "martied01" "martiti02" "maynebr01"
[13411] "mcgwima01" "mclemma01" "merceor01" "mitchke01" "molitpa01" "murraed02" "myersgr01" "nevinph01" "nixonot01" "obriech01"
[13421] "offerjo01" "oneilpa01" "palmede01" "palmede01" "palmera01" "penato01"  "phillto02" "phillto02" "raineti01" "ramirma02"
[13431] "ripkeca01" "rodriiv01" "samueju01" "sanchre01" "santibe01" "sojolu01"  "stairma01" "stanlmi02" "stanlmi02" "surhobj01"
[13441] "thomafr04" "thomeji01" "valenjo03" "venturo01" "vizquom01" "walketo04" "whitema01" "willibe02" "willige02" "willima04"
[13451] "aloumo01"  "ashbyan01" "ausmubr01" "baergca01" "bagweje01" "bellda01"  "bellira01" "benesan01" "bicheda01" "biggicr01"
[13461] "bondsba01" "bonilbo01" "boonebr01" "brownke01" "burksel01" "butlebr01" "caminke01" "castivi02" "claytro01" "colbrgr01"
[13471] "coninje01" "daultda01" "daultda01" "dunstsh01" "dunstsh01" "eisenji01" "evereca01" "finlest01" "flahejo01" "fletcda01"
[13481] "floydcl01" "gaettga01" "gagnegr01" "galaran01" "gantro01"  "glavito02" "gomezch02" "gonzalu01" "gracema01" "graffto01"
[13491] "gwynnto01" "hamilda02" "hanseda01" "harrile01" "henderi01" "hernajo01" "hillgl01"  "hitchst01" "hollato01" "javiest01"
[13501] "jeffegr01" "johnsla03" "johnsla03" "jordabr01" "joynewa01" "kentje01"  "kleskry01" "lankfra01" "larkiba01" "lewisda01"
[13511] "loaizes01" "loftoke01" "lopezja01" "mabryjo01" "maddugr01" "martipe02" "mcgeewi01" "mcgrifr01" "mcgwima01" "mondera01"
[13521] "neaglde01" "nixonot01" "olerujo01" "olivejo01" "parenma01" "parkch01"  "pendlte01" "perezne01" "piazzmi01" "prattto02"
[13531] "princto01" "reedje02"  "reedri01"  "sanchre01" "sandbry01" "sandere02" "schilcu01" "schmija01" "seguida01" "sheffga01"
[13541] "sierrru01" "smoltjo01" "snowjt01"  "sosasa01"  "stottto01" "sweenma01" "sweenma01" "trachst01" "valdeis01" "vandejo02"
[13551] "vaughgr01" "vizcajo01" "walkela01" "whitede03" "whitero02" "wilkiri01" "womacto01" "younger01" "younger01" "zaungr01" 
[13561] "zeileto01" "alomaro01" "alomasa02" "anderbr01" "baineha01" "bellda01"  "bellda01"  "boggswa01" "bordepa01" "bordimi01"
[13571] "buhneja01" "cansejo01" "cartejo01" "clarkwi02" "claytro01" "coninje01" "cordewi01" "davisch01" "daviser01" "delgaca01"
[13581] "duceyro01" "dunstsh01" "easleda01" "edmonji01" "fernato01" "fieldce01" "flahejo01" "fletcda01" "girarjo01" "gonzaal01"
[13591] "gonzaju03" "gonzalu01" "goodwto01" "greensh01" "griffke02" "hammoje01" "henderi01" "hillgl01"  "jeffegr01" "justida01"
[13601] "kellyro01" "kreutch01" "lawtoma02" "lewisda01" "leyriji01" "loftoke01" "magadda01" "martida01" "martied01" "martiti02"
[13611] "mcgrifr01" "mclemma01" "merceor01" "mitchke01" "molitpa01" "nevinph01" "nixonot01" "obriech01" "offerjo01" "olivejo01"
[13621] "olivejo01" "oneilpa01" "palmede01" "palmera01" "pendlte01" "raineti01" "ramirma02" "ripkeca01" "rodriiv01" "samueju01"
[13631] "seguida01" "sierrru01" "sojolu01"  "stairma01" "stanlmi02" "stanlmi02" "strawda01" "surhobj01" "thomafr04" "thomeji01"
[13641] "velarra01" "venturo01" "vizquom01" "walketo04" "whitema01" "willibe02" "zeileto01" "aloumo01"  "ashbyan01" "astacpe01"
[13651] "ausmubr01" "baergca01" "bagweje01" "bellja01"  "benesan01" "bicheda01" "biggicr01" "bondsba01" "bonilbo01" "bonilbo01"
[13661] "boonebr01" "brownke01" "burksel01" "burksel01" "burnije01" "caminke01" "cartejo01" "castivi02" "cirilje01" "claytro01"
[13671] "colbrgr01" "dunstsh01" "eisenji01" "eisenji01" "evereca01" "finlest01" "floydcl01" "gaettga01" "gaettga01" "galaran01"
[13681] "gantro01"  "glavito02" "gomezch02" "gracema01" "graffto01" "grissma02" "guilloz01" "gwynnto01" "hamilda02" "hamilda02"
[13691] "hammoje01" "harnipe01" "harrile01" "harrile01" "hayesch01" "hernajo01" "hershor01" "hillgl01"  "hitchst01" "hollato01"
[13701] "jacksda03" "javiest01" "jeffegr01" "johnsla03" "jordabr01" "joynewa01" "kentje01"  "kleskry01" "lankfra01" "larkiba01"
[13711] "leiteal01" "leyriji01" "lopezja01" "mabryjo01" "maddugr01" "maynebr01" "mcgeewi01" "mcgwima01" "merckke01" "mondera01"
[13721] "myersgr01" "neaglde01" "olerujo01" "parenma01" "parkch01"  "perezne01" "phillto02" "piazzmi01" "piazzmi01" "portuma01"
[13731] "prattto02" "princto01" "reedje02"  "reedri01"  "sanchre01" "sandere02" "schilcu01" "schmija01" "sheffga01" "sheffga01"
[13741] "smoltjo01" "snowjt01"  "sosasa01"  "stinnke01" "stottto01" "sweenma01" "tapanke01" "trachst01" "valenjo03" "vandejo02"
[13751] "vaughgr01" "vizcajo01" "walkela01" "whitede03" "whitero02" "willige02" "willima04" "womacto01" "younger01" "zaungr01" 
[13761] "zeileto01" "zeileto01" "alomaro01" "alomasa02" "anderbr01" "ausmubr01" "baergca01" "baineha01" "baineha01" "bellda01" 
[13771] "boggswa01" "bordimi01" "buhneja01" "cansejo01" "clarkwi02" "claytro01" "coninje01" "cordewi01" "davisch01" "delgaca01"
[13781] "easleda01" "edmonji01" "fernato01" "flahejo01" "fletcda01" "girarjo01" "gonzaal01" "gonzaju03" "goodwto01" "graffto01"
[13791] "greensh01" "griffke02" "jacksda03" "jeffegr01" "justida01" "kellyro01" "kreutch01" "lawtoma02" "lewisda01" "leyriji01"
[13801] "loftoke01" "mabryjo01" "martida01" "martied01" "martiti02" "mcgrifr01" "mclemma01" "obriech01" "offerjo01" "oneilpa01"
[13811] "palmede01" "palmera01" "phillto02" "polonlu01" "raineti01" "ramirma02" "ripkeca01" "rodriiv01" "sanchre01" "seguida01"
[13821] "seguida01" "sojolu01"  "stairma01" "stanlmi02" "surhobj01" "thomafr04" "thomeji01" "velarra01" "velarra01" "vizquom01"
[13831] "walketo04" "willibe02" "zaungr01"  "zeileto01" "ashbyan01" "astacpe01" "baergca01" "bagweje01" "bellja01"  "benesan01"
[13841] "bicheda01" "biggicr01" "bondsba01" "bonilbo01" "boonebr01" "brownke01" "burksel01" "burnije01" "caminke01" "castivi02"
[13851] "cirilje01" "colbrgr01" "daviser01" "duceyro01" "dunstsh01" "dunstsh01" "evereca01" "finlest01" "floydcl01" "gaettga01"
[13861] "gantro01"  "glavito02" "gomezch02" "gonzalu01" "gracema01" "grissma02" "guilloz01" "gwynnto01" "hamilda02" "hamilda02"
[13871] "hammoje01" "hanseda01" "harnipe01" "harrile01" "hayesch01" "henderi01" "hernajo01" "hernajo01" "hershor01" "hillgl01" 
[13881] "hitchst01" "hollato01" "javiest01" "javiest01" "johnsla03" "johnsra05" "jordabr01" "joynewa01" "kentje01"  "kleskry01"
[13891] "lankfra01" "larkiba01" "leiteal01" "leyriji01" "lopezja01" "maddugr01" "magadda01" "maynebr01" "mcgeewi01" "mcgwima01"
[13901] "merceor01" "mondera01" "myersgr01" "myersgr01" "nevinph01" "nixonot01" "olerujo01" "oliveda02" "olivejo01" "parkch01" 
[13911] "perezne01" "piazzmi01" "prattto02" "reedje02"  "reedje02"  "sandere02" "santibe01" "schilcu01" "schmija01" "sheffga01"
[13921] "smoltjo01" "snowjt01"  "sosasa01"  "stinnke01" "trachst01" "valdeis01" "valenjo03" "vandejo02" "vaughgr01" "venturo01"
[13931] "vizcajo01" "walkela01" "whitede03" "whitero02" "willige02" "willima04" "williwo02" "womacto01" "younger01" "alomaro01"
[13941] "alomasa02" "anderbr01" "ausmubr01" "baineha01" "baineha01" "bellda01"  "bicheda01" "bordimi01" "buhneja01" "cansejo01"
[13951] "cansejo01" "castivi02" "clarkwi02" "claytro01" "coninje01" "cordewi01" "delgaca01" "easleda01" "evereca01" "flahejo01"
[13961] "fletcda01" "gantro01"  "gonzaal01" "gonzaju03" "graffto01" "guilloz01" "henderi01" "hillgl01"  "javiest01" "jeffegr01"
[13971] "justida01" "justida01" "lawtoma02" "lewisda01" "leyriji01" "loftoke01" "mabryjo01" "martida01" "martida01" "martida01"
[13981] "martied01" "martiti02" "mcgrifr01" "mclemma01" "mondera01" "myersgr01" "offerjo01" "olerujo01" "olivejo01" "oneilpa01"
[13991] "palmede01" "palmera01" "polonlu01" "polonlu01" "ramirma02" "ripkeca01" "rodriiv01" "sanchre01" "seguida01" "seguida01"
[14001] "sierrru01" "sojolu01"  "stairma01" "stanlmi02" "stanlmi02" "surhobj01" "thomafr04" "thomeji01" "valenjo03" "vaughgr01"
[14011] "velarra01" "vizcajo01" "vizquom01" "walketo04" "willibe02" "willige02" "zaungr01"  "aloumo01"  "astacpe01" "bagweje01"
[14021] "bellja01"  "benesan01" "bicheda01" "biggicr01" "bondsba01" "bonilbo01" "boonebr01" "bordimi01" "brownke01" "burksel01"
[14031] "burnije01" "caminke01" "cirilje01" "clarkwi02" "colbrgr01" "cordewi01" "daviser01" "duceyro01" "dunstsh01" "edmonji01"
[14041] "finlest01" "floydcl01" "galaran01" "gantro01"  "girarjo01" "glavito02" "gomezch02" "gonzalu01" "goodwto01" "goodwto01"
[14051] "gracema01" "greensh01" "griffke02" "grissma02" "gwynnto01" "hamilda02" "hammoje01" "hanseda01" "harrile01" "harrile01"
[14061] "hayesch01" "henderi01" "hernajo01" "hillgl01"  "hollato01" "hollato01" "johnsra05" "jordabr01" "joynewa01" "kentje01" 
[14071] "kleskry01" "kreutch01" "lankfra01" "larkiba01" "leiteal01" "leyriji01" "lopezja01" "mabryjo01" "maddugr01" "magadda01"
[14081] "martida01" "maynebr01" "mcgwima01" "nevinph01" "parkch01"  "perezne01" "piazzmi01" "prattto02" "princto01" "reedje02" 
[14091] "sandere02" "santibe01" "sheffga01" "snowjt01"  "sojolu01"  "sosasa01"  "stinnke01" "surhobj01" "sweenma01" "tapanke01"
[14101] "vandejo02" "venturo01" "vizcajo01" "walkela01" "walketo04" "whitede03" "whitero02" "whitero02" "willima04" "williwo02"
[14111] "womacto01" "younger01" "zeileto01" "alomaro01" "alomasa02" "anderbr01" "baineha01" "bellda01"  "bicheda01" "boonebr01"
[14121] "bordimi01" "burksel01" "caminke01" "cansejo01" "castivi02" "claytro01" "coninje01" "cordewi01" "delgaca01" "easleda01"
[14131] "evereca01" "fernato01" "flahejo01" "fletcda01" "galaran01" "gantro01"  "gomezch02" "gonzaal01" "gonzaju03" "graffto01"
[14141] "hillgl01"  "javiest01" "joynewa01" "justida01" "lawtoma02" "lewisda01" "loftoke01" "martied01" "martiti02" "maynebr01"
[14151] "mcgrifr01" "mclemma01" "mondera01" "myersgr01" "myersgr01" "offerjo01" "olerujo01" "oneilpa01" "palmede01" "palmera01"
[14161] "perezne01" "princto01" "ramirma02" "ripkeca01" "rodriiv01" "sanchre01" "seguida01" "sierrru01" "sojolu01"  "thomafr04"
[14171] "thomeji01" "valenjo03" "vaughgr01" "velarra01" "vizquom01" "willibe02" "willige02" "zaungr01"  "aloumo01"  "appieke01"
[14181] "ausmubr01" "bagweje01" "bellja01"  "biggicr01" "bondsba01" "bonilbo01" "burkejo03" "burnije01" "caminke01" "castivi02"
[14191] "cirilje01" "colbrgr01" "daviser01" "dunstsh01" "edmonji01" "fernato01" "finlest01" "floydcl01" "francju01" "galaran01"
[14201] "gantro01"  "girarjo01" "glavito02" "gomezch02" "gonzalu01" "goodwto01" "gracema01" "greensh01" "griffke02" "grissma02"
[14211] "gwynnto01" "hamilda02" "hammoje01" "hanseda01" "harrile01" "hayesch01" "henderi01" "hernajo01" "hollato01" "jarvike01"
[14221] "johnsra05" "jordabr01" "kentje01"  "kleskry01" "kreutch01" "lankfra01" "lankfra01" "larkiba01" "lawtoma02" "leiteal01"
[14231] "lopezja01" "mabryjo01" "maddugr01" "magadda01" "martida01" "maynebr01" "mcgrifr01" "mcgwima01" "merceor01" "neaglde01"
[14241] "nevinph01" "parkch01"  "perezne01" "piazzmi01" "prattto02" "prattto02" "raineti01" "sanchre01" "sandere02" "santibe01"
[14251] "schilcu01" "sheffga01" "snowjt01"  "sosasa01"  "stairma01" "stinnke01" "surhobj01" "sweenma01" "tapanke01" "trachst01"
[14261] "vandejo02" "vandejo02" "venturo01" "vizcajo01" "walkela01" "walketo04" "walketo04" "whitede03" "whitero02" "willima04"
[14271] "williwo02" "womacto01" "younger01" "zeileto01" "rodriiv01" "ramirma02" "prattto02" "piazzmi01" "perezne01" "palmera01"
[14281] "palmede01" "oliveda02" "olerujo01" "nevinph01" "myersgr01" "mondera01" "mondera01" "merceor01" "mclemma01" "mcgrifr01"
[14291] "maynebr01" "martiti02" "martied01" "maddugr01" "mabryjo01" "velarra01" "sweenma01" "surhobj01" "seguida01" "lewisda01"
[14301] "fletcda01" "anderbr01" "schmija01" "dunstsh01" "snowjt01"  "sandere02" "goodwto01" "bellda01"  "loftoke01" "santibe01"
[14311] "kentje01"  "bondsba01" "lankfra01" "gantro01"  "nevinph01" "kleskry01" "kreutch01" "grissma02" "jordabr01" "greensh01"
[14321] "hanseda01" "alomasa02" "zeileto01" "hollato01" "walkela01" "schilcu01" "johnsra05" "gracema01" "willima04" "womacto01"
[14331] "finlest01" "gonzalu01" "colbrgr01" "martiti02" "edmonji01" "stairma01" "hammoje01" "younger01" "hernajo01" "harrile01"
[14341] "zaungr01"  "biggicr01" "ausmubr01" "merceor01" "bagweje01" "vizcajo01" "stinnke01" "larkiba01" "griffke02" "walketo04"
[14351] "girarjo01" "gonzaal01" "aloumo01"  "mcgrifr01" "sosasa01"  "prattto02" "leiteal01" "astacpe01" "burnije01" "alomaro01"
[14361] "piazzmi01" "floydcl01" "galaran01" "cordewi01" "raineti01" "floydcl01" "glavito02" "maddugr01" "castivi02" "lopezja01"
[14371] "francju01" "sheffga01" "hollato01" "evereca01" "palmera01" "gonzaju03" "rodriiv01" "cirilje01" "sierrru01" "mclemma01"
[14381] "martied01" "boonebr01" "olerujo01" "myersgr01" "justida01" "mabryjo01" "princto01" "maynebr01" "perezne01" "easleda01"
[14391] "lawtoma02" "vizquom01" "burksel01" "thomeji01" "valenjo03" "claytro01" "thomafr04" "loftoke01" "graffto01" "alomasa02"
[14401] "mondera01" "delgaca01" "vaughgr01" "flahejo01" "gomezch02" "whitero02" "mondera01" "venturo01" "vandejo02" "willibe02"
[14411] "henderi01" "offerjo01" "baergca01" "sanchre01" "floydcl01" "ramirma02" "bordimi01" "coninje01" "lopezja01" "loftoke01"
[14421] "loftoke01" "leiteal01" "lawtoma02" "larkiba01" "kleskry01" "kentje01"  "jordabr01" "hollato01" "hernajo01" "hernajo01"
[14431] "hernajo01" "henderi01" "harrile01" "hanseda01" "hammoje01" "grissma02" "griffke02" "greensh01" "graffto01" "gracema01"
[14441] "goodwto01" "gonzalu01" "gonzaju03" "gonzaal01" "gomezch02" "glavito02" "galaran01" "francju01" "floydcl01" "flahejo01"
[14451] "finlest01" "evereca01" "evereca01" "edmonji01" "easleda01" "delgaca01" "cordewi01" "coninje01" "coninje01" "colbrgr01"
[14461] "claytro01" "cirilje01" "castivi02" "burnije01" "burnije01" "burksel01" "brownke01" "bordimi01" "boonebr01" "bondsba01"
[14471] "biggicr01" "bellja01"  "bellda01"  "bagweje01" "baergca01" "ausmubr01" "aloumo01"  "alomasa02" "alomaro01" "alomaro01"
[14481] "sanchre01" "sanchre01" "sandere02" "santibe01" "schilcu01" "schmija01" "seguida01" "sheffga01" "sierrru01" "sierrru01"
[14491] "snowjt01"  "sosasa01"  "stairma01" "stinnke01" "surhobj01" "sweenma01" "thomafr04" "thomeji01" "trachst01" "valenjo03"
[14501] "vandejo02" "venturo01" "venturo01" "vizcajo01" "vizquom01" "walkela01" "walketo04" "whitero02" "whitero02" "willibe02"
[14511] "willima04" "williwo02" "womacto01" "womacto01" "womacto01" "younger01" "younger01" "zaungr01"  "zeileto01" "zeileto01"
[14521] "zaungr01"  "gomezch02" "delgaca01" "jordabr01" "younger01" "mcgrifr01" "sanchre01" "martiti02" "suppaje01" "williwo02"
[14531] "walkela01" "lankfra01" "mabryjo01" "sandere02" "edmonji01" "womacto01" "bordepa01" "hanseda01" "olerujo01" "martied01"
[14541] "boonebr01" "schmija01" "hammoje01" "perezne01" "snowjt01"  "bondsba01" "grissma02" "wellsda01" "cirilje01" "kleskry01"
[14551] "nevinph01" "mondera01" "prattto02" "bellda01"  "thomeji01" "mclemma01" "flahejo01" "olerujo01" "loftoke01" "sierrru01"
[14561] "willibe02" "sheffga01" "leiteal01" "glavito02" "trachst01" "willige02" "zeileto01" "floydcl01" "piazzmi01" "evereca01"
[14571] "gonzaal01" "offerjo01" "maynebr01" "venturo01" "hernajo01" "finlest01" "greensh01" "stinnke01" "gonzaju03" "santibe01"
[14581] "graffto01" "stairma01" "clemero02" "vizcajo01" "ausmubr01" "kentje01"  "bagweje01" "biggicr01" "cordewi01" "harrile01"
[14591] "easleda01" "coninje01" "whitero02" "rodriiv01" "walkela01" "sweenma01" "burnije01" "claytro01" "castivi02" "vizquom01"
[14601] "lawtoma02" "vandejo02" "griffke02" "larkiba01" "alomaro01" "alomasa02" "evereca01" "thomafr04" "valenjo03" "perezne01"
[14611] "maddugr01" "goodwto01" "gonzaal01" "hollato01" "walketo04" "sosasa01"  "aloumo01"  "ramirma02" "seguida01" "surhobj01"
[14621] "lopezja01" "palmera01" "francju01" "baergca01" "johnsra05" "maynebr01" "alomaro01" "gonzalu01" "finlest01" "sweenma01"
[14631] "stairma01" "stairma01" "mabryjo01" "delgaca01" "rodriiv01" "evereca01" "gomezch02" "williwo02" "edmonji01" "schmija01"
[14641] "sandere02" "kentje01"  "bondsba01" "bellda01"  "bellda01"  "cirilje01" "nevinph01" "nevinph01" "prattto02" "trachst01"
[14651] "piazzmi01" "burnije01" "willibe02" "whitero02" "younger01" "hernajo01" "jordabr01" "greensh01" "greensh01" "suppaje01"
[14661] "sweenma01" "hanseda01" "harrile01" "stairma01" "mabryjo01" "mondera01" "loaizes01" "delgaca01" "rodriiv01" "palmera01"
[14671] "evereca01" "gomezch02" "flahejo01" "martiti02" "edmonji01" "schmija01" "snowjt01"  "sandere02" "kentje01"  "bellda01" 
[14681] "sierrru01" "olerujo01" "cirilje01" "bordepa01" "boonebr01" "boonebr01" "nevinph01" "nevinph01" "kleskry01" "prattto02"
[14691] "piazzmi01" "burnije01" "willibe02" "whitero02" "clemero02" "cordewi01" "younger01" "hernajo01" "jordabr01" "grissma02"
[14701] "greensh01" "suppaje01" "perezne01" "zaungr01"  "vizcajo01" "biggicr01" "bagweje01" "ausmubr01" "floydcl01" "easleda01"
[14711] "walkela01" "hollato01" "thomeji01" "vizquom01" "lawtoma02" "lawtoma02" "walketo04" "stinnke01" "griffke02" "sosasa01" 
[14721] "gonzaal01" "aloumo01"  "valenjo03" "thomafr04" "loftoke01" "graffto01" "graffto01" "alomasa02" "claytro01" "ramirma02"
[14731] "offerjo01" "martipe02" "baergca01" "coninje01" "smoltjo01" "surhobj01" "sheffga01" "maddugr01" "lopezja01" "glavito02"
[14741] "francju01" "castivi02" "womacto01" "gonzalu01" "finlest01" "perezne01" "perezne01" "zaungr01"  "vizcajo01" "biggicr01"
[14751] "ausmubr01" "floydcl01" "easleda01" "hollato01" "hollato01" "vizquom01" "thomeji01" "walketo04" "walketo04" "stinnke01"
[14761] "griffke02" "aloumo01"  "valenjo03" "thomafr04" "loftoke01" "graffto01" "graffto01" "claytro01" "claytro01" "alomasa02"
[14771] "ramirma02" "coninje01" "coninje01" "smoltjo01" "sheffga01" "lopezja01" "lopezja01" "glavito02" "francju01" "castivi02"
[14781] "womacto01" "finlest01" "gonzalu01" "francju01" "zaungr01"  "williwo02" "whitero02" "vizquom01" "valenjo03" "thomeji01"
[14791] "thomafr04" "sweenma01" "suppaje01" "stinnke01" "stairma01" "sosasa01"  "smoltjo01" "sheffga01" "sandere02" "rodriiv01"
[14801] "ramirma02" "piazzmi01" "perezne01" "moyerja01" "maddugr01" "loftoke01" "loftoke01" "kleskry01" "kentje01"  "griffke02"
[14811] "greensh01" "graffto01" "gonzalu01" "gomezch02" "gomezch02" "glavito02" "floydcl01" "finlest01" "edmonji01" "easleda01"
[14821] "delgaca01" "coninje01" "claytro01" "cirilje01" "bondsba01" "biggicr01" "ausmubr01" "aloumo01" 
R > baseball.2[baseball.2[,"id"]=="ruthba01",]
            id year stint team lg   g  ab   r   h X2b X3b hr rbi sb cs  bb so ibb hbp sh sf gidp       OBP
15457 ruthba01 1915     1  BOS AL  42  92  16  29  10   1  4  21  0 NA   9 23  NA   0  2  0   NA 0.3762376
16238 ruthba01 1916     1  BOS AL  67 136  18  37   5   3  3  15  0 NA  10 23  NA   0  4  0   NA 0.3219178
16776 ruthba01 1917     1  BOS AL  52 123  14  40   6   3  2  12  0 NA  12 18  NA   0  7  0   NA 0.3851852
17286 ruthba01 1918     1  BOS AL  95 317  50  95  26  11 11  66  6 NA  58 58  NA   2  3  0   NA 0.4111406
17790 ruthba01 1919     1  BOS AL 130 432 103 139  34  12 29 114  7 NA 101 58  NA   6  3  0   NA 0.4564007
18329 ruthba01 1920     1  NYA AL 142 457 158 172  36   9 54 137 14 14 150 80  NA   3  5  0   NA 0.5327869
18834 ruthba01 1921     1  NYA AL 152 540 177 204  44  16 59 171 17 13 145 81  NA   4  4  0   NA 0.5123367
19363 ruthba01 1922     1  NYA AL 110 406  94 128  24   8 35  99  2  5  84 80  NA   1  4  0   NA 0.4338086
19883 ruthba01 1923     1  NYA AL 152 522 151 205  45  13 41 131 17 21 170 93  NA   4  3  0   NA 0.5445402
20420 ruthba01 1924     1  NYA AL 153 529 143 200  39   7 46 121  9 13 142 81  NA   4  6  0   NA 0.5125926
20967 ruthba01 1925     1  NYA AL  98 359  61 104  12   2 25  66  2  4  59 68  NA   2  6  0   NA 0.3928571
21507 ruthba01 1926     1  NYA AL 152 495 139 184  30   5 47 150 11  9 144 76  NA   3 10  0   NA 0.5155763
22038 ruthba01 1927     1  NYA AL 151 540 158 192  29   8 60 164  7  6 137 89  NA   0 14  0   NA 0.4859675
22572 ruthba01 1928     1  NYA AL 154 536 163 173  29   8 54 142  4  5 137 87  NA   3  8  0   NA 0.4630178
23110 ruthba01 1929     1  NYA AL 135 499 121 172  26   6 46 154  5  3  72 60  NA   3 13  0   NA 0.4303136
23656 ruthba01 1930     1  NYA AL 145 518 150 186  28   9 49 153 10 10 136 61  NA   1 21  0   NA 0.4931298
24167 ruthba01 1931     1  NYA AL 145 534 149 199  31   3 46 163  5  4 128 51  NA   1  0  0   NA 0.4947210
24694 ruthba01 1932     1  NYA AL 133 457 120 156  13   5 41 137  2  2 130 62  NA   2  0  0   NA 0.4889643
25199 ruthba01 1933     1  NYA AL 137 459  97 138  21   3 34 103  4  5 114 90  NA   2  0  0   NA 0.4417391
25702 ruthba01 1934     1  NYA AL 125 365  78 105  17   4 22  84  1  3 104 63  NA   2  0  0   NA 0.4479830
26477 ruthba01 1935     1  BSN NL  28  72  13  13   0   0  6  12  0 NA  20 24  NA   0  0  0    2 0.3586957
R > baseball.2[baseball.2$id=="ruthba01",]
            id year stint team lg   g  ab   r   h X2b X3b hr rbi sb cs  bb so ibb hbp sh sf gidp       OBP
15457 ruthba01 1915     1  BOS AL  42  92  16  29  10   1  4  21  0 NA   9 23  NA   0  2  0   NA 0.3762376
16238 ruthba01 1916     1  BOS AL  67 136  18  37   5   3  3  15  0 NA  10 23  NA   0  4  0   NA 0.3219178
16776 ruthba01 1917     1  BOS AL  52 123  14  40   6   3  2  12  0 NA  12 18  NA   0  7  0   NA 0.3851852
17286 ruthba01 1918     1  BOS AL  95 317  50  95  26  11 11  66  6 NA  58 58  NA   2  3  0   NA 0.4111406
17790 ruthba01 1919     1  BOS AL 130 432 103 139  34  12 29 114  7 NA 101 58  NA   6  3  0   NA 0.4564007
18329 ruthba01 1920     1  NYA AL 142 457 158 172  36   9 54 137 14 14 150 80  NA   3  5  0   NA 0.5327869
18834 ruthba01 1921     1  NYA AL 152 540 177 204  44  16 59 171 17 13 145 81  NA   4  4  0   NA 0.5123367
19363 ruthba01 1922     1  NYA AL 110 406  94 128  24   8 35  99  2  5  84 80  NA   1  4  0   NA 0.4338086
19883 ruthba01 1923     1  NYA AL 152 522 151 205  45  13 41 131 17 21 170 93  NA   4  3  0   NA 0.5445402
20420 ruthba01 1924     1  NYA AL 153 529 143 200  39   7 46 121  9 13 142 81  NA   4  6  0   NA 0.5125926
20967 ruthba01 1925     1  NYA AL  98 359  61 104  12   2 25  66  2  4  59 68  NA   2  6  0   NA 0.3928571
21507 ruthba01 1926     1  NYA AL 152 495 139 184  30   5 47 150 11  9 144 76  NA   3 10  0   NA 0.5155763
22038 ruthba01 1927     1  NYA AL 151 540 158 192  29   8 60 164  7  6 137 89  NA   0 14  0   NA 0.4859675
22572 ruthba01 1928     1  NYA AL 154 536 163 173  29   8 54 142  4  5 137 87  NA   3  8  0   NA 0.4630178
23110 ruthba01 1929     1  NYA AL 135 499 121 172  26   6 46 154  5  3  72 60  NA   3 13  0   NA 0.4303136
23656 ruthba01 1930     1  NYA AL 145 518 150 186  28   9 49 153 10 10 136 61  NA   1 21  0   NA 0.4931298
24167 ruthba01 1931     1  NYA AL 145 534 149 199  31   3 46 163  5  4 128 51  NA   1  0  0   NA 0.4947210
24694 ruthba01 1932     1  NYA AL 133 457 120 156  13   5 41 137  2  2 130 62  NA   2  0  0   NA 0.4889643
25199 ruthba01 1933     1  NYA AL 137 459  97 138  21   3 34 103  4  5 114 90  NA   2  0  0   NA 0.4417391
25702 ruthba01 1934     1  NYA AL 125 365  78 105  17   4 22  84  1  3 104 63  NA   2  0  0   NA 0.4479830
26477 ruthba01 1935     1  BSN NL  28  72  13  13   0   0  6  12  0 NA  20 24  NA   0  0  0    2 0.3586957
R > ?ddply
R > fix(obp)
R > obp(baseball.2[baseball.2$id=="ruthba01",])
      OBP 
0.4742209 
R > careerOBP<-ddply(baseball.2,.variables="id",.fun=obp)
R > str(careerOBP)
'data.frame':	1118 obs. of  2 variables:
 $ id : chr  "aaronha01" "adairje01" "adamsba01" "adamsbo03" ...
 $ OBP: num  0.374 0.292 0.255 0.34 0.337 ...
R > head(careerOBP)
         id       OBP
1 aaronha01 0.3739493
2 adairje01 0.2922746
3 adamsba01 0.2550694
4 adamsbo03 0.3403342
5 adcocjo01 0.3368726
6 aguilri01 0.2037037
R > careerOBP.order<-careerOBP[order(careerOBP$OBP,decreasing=TRUE),]
R > head(careerOBP.order,n=10)
            id       OBP
1089 willite01 0.4816861
875   ruthba01 0.4742209
658  mcgrajo01 0.4657478
356  gehrilo01 0.4477848
85   bondsba01 0.4444622
476  hornsro01 0.4339068
184   cobbty01 0.4329655
327   foxxji01 0.4290509
953  speaktr01 0.4283386
191  collied01 0.4251246
R > baseball.2[baseball.2$id=="williete01",]
 [1] id    year  stint team  lg    g     ab    r     h     X2b   X3b   hr    rbi   sb    cs    bb    so    ibb   hbp   sh   
[21] sf    gidp  OBP  
<0 행> <또는 row.names의 길이가 0입니다>
R > baseball.2[baseball.2$id=="willite01",]
             id year stint team lg   g  ab   r   h X2b X3b hr rbi sb cs  bb so ibb hbp sh sf gidp       OBP
28376 willite01 1939     1  BOS AL 149 565 131 185  44  11 31 145  2  1 107 64  NA   2  3  0   10 0.4362018
28925 willite01 1940     1  BOS AL 144 561 134 193  43  14 23 113  4  4  96 54  NA   3  1  0   13 0.4424242
29489 willite01 1941     1  BOS AL 143 456 135 185  33   3 37 120  2  4 147 27  NA   3  0  0   10 0.5528053
30057 willite01 1942     1  BOS AL 150 522 141 186  34   5 36 137  3  2 145 51  NA   4  0  0   12 0.4992548
32360 willite01 1946     1  BOS AL 150 514 142 176  37   8 38 123  0  0 156 44  NA   2  0  0   12 0.4970238
32990 willite01 1947     1  BOS AL 156 528 125 181  40   9 32 114  0  1 162 47  NA   2  1  0   10 0.4985549
33571 willite01 1948     1  BOS AL 137 509 124 188  44   3 25 127  4  0 126 41  NA   3  0  0   10 0.4968652
34146 willite01 1949     1  BOS AL 155 566 150 194  39   3 43 159  1  1 162 48  NA   2  0  0   22 0.4904110
34735 willite01 1950     1  BOS AL  89 334  82 106  24   1 28  97  3  0  82 21  NA   0  0  0   12 0.4519231
35330 willite01 1951     1  BOS AL 148 531 109 169  28   4 30 126  1  1 144 45  NA   0  0  0   10 0.4637037
36570 willite01 1953     1  BOS AL  37  91  17  37   6   0 13  34  0  1  19 10  NA   0  0  0    1 0.5090909
37145 willite01 1954     1  BOS AL 117 386  93 133  23   1 29  89  0  0 136 32  NA   1  0  3   10 0.5133080
37780 willite01 1955     1  BOS AL  98 320  77 114  21   3 28  83  2  0  91 24  17   2  0  4    8 0.4964029
38393 willite01 1956     1  BOS AL 136 400  71 138  28   2 24  82  0  0 102 39  11   1  0  0   13 0.4791252
39011 willite01 1957     1  BOS AL 132 420  96 163  28   1 38  87  0  1 119 43  33   5  0  2   11 0.5256410
39633 willite01 1958     1  BOS AL 129 411  81 135  23   2 26  85  1  0  98 49  12   4  0  4   19 0.4584139
40279 willite01 1959     1  BOS AL 103 272  32  69  15   0 10  43  0  0  52 27   6   2  0  5    7 0.3716012
40909 willite01 1960     1  BOS AL 113 310  56  98  15   0 29  72  1  1  75 41   7   3  0  2    7 0.4512821
R > baseball.2[baseball.2$id=="mcgrajo01",]
            id year stint team lg   g  ab   r   h X2b X3b hr rbi sb cs  bb so ibb hbp sh sf gidp       OBP
5195 mcgrajo01 1891     1  BL3 AA  33 115  17  31   3   5  0  14  4 NA  12 17  NA   4 NA  0   NA 0.3587786
5645 mcgrajo01 1892     1  BLN NL  79 286  41  77  13   2  1  26 15 NA  32 21  NA   6 NA  0   NA 0.3549383
5939 mcgrajo01 1893     1  BLN NL 127 480 123 154   9  10  5  64 38 NA 101 11  NA  16 NA  0   NA 0.4539363
6228 mcgrajo01 1894     1  BLN NL 124 512 156 174  18  14  1  92 78 NA  91 12  NA  13 NA  0   NA 0.4512987
6538 mcgrajo01 1895     1  BLN NL  96 388 110 143  13   6  2  48 61 NA  60  9  NA   5  6  0   NA 0.4591611
6847 mcgrajo01 1896     1  BLN NL  23  77  20  25   2   2  0  14 13 NA  11  4  NA   2  0  0   NA 0.4222222
7131 mcgrajo01 1897     1  BLN NL 106 391  90 127  15   3  0  48 44 NA  99 NA  NA   9  8  0   NA 0.4709419
7431 mcgrajo01 1898     1  BLN NL 143 515 143 176   8  10  0  53 43 NA 112 NA  NA  19  6  0   NA 0.4752322
7772 mcgrajo01 1899     1  BLN NL 117 399 140 156  13   3  1  33 73 NA 124 NA  NA  14  2  0   NA 0.5474860
8022 mcgrajo01 1900     1  SLN NL  99 334  84 115  10   4  2  33 29 NA  85 NA  NA  23  5  0   NA 0.5045249
8228 mcgrajo01 1901     1  BLA AL  73 232  71  81  14   9  0  28 24 NA  61 NA  NA  14  1  0   NA 0.5081433
8636 mcgrajo01 1902     1  BLA AL  20  63  14  18   3   2  1   3  5 NA  17 NA  NA   2  2  0   NA 0.4512195
8872 mcgrajo01 1902     2  NY1 NL  35 107  13  25   0   0  0   5  7 NA  26 NA  NA   4  2  0   NA 0.4014599
R > require(RKKCD)
필요한 패키지를 로딩중입니다: RKKCD
경고메시지:
In library(package, lib.loc = lib.loc, character.only = TRUE, logical.return = TRUE,  :
  ‘RKKCD’이라고 불리는 패키지가 없습니다
R > install.package("RKKCD")
에러: 함수 "install.package"를 찾을 수 없습니다
R > install.packages("RKKCD")
경고메시지:
package ‘RKKCD’ is not available (for R version 3.1.1) 
R > install.packages("RXKCD")
also installing the dependencies ‘RJSONIO’, ‘png’, ‘jpeg’

URL 'http://cran.nexr.com/bin/macosx/mavericks/contrib/3.1/RJSONIO_1.3-0.tgz'을 시도합니다
Content type 'application/x-gzip' length 1272440 bytes (1.2 Mb)
URL을 열었습니다
==================================================
downloaded 1.2 Mb

URL 'http://cran.nexr.com/bin/macosx/mavericks/contrib/3.1/png_0.1-7.tgz'을 시도합니다
Content type 'application/x-gzip' length 256466 bytes (250 Kb)
URL을 열었습니다
==================================================
downloaded 250 Kb

URL 'http://cran.nexr.com/bin/macosx/mavericks/contrib/3.1/jpeg_0.1-8.tgz'을 시도합니다
Content type 'application/x-gzip' length 336986 bytes (329 Kb)
URL을 열었습니다
==================================================
downloaded 329 Kb

URL 'http://cran.nexr.com/bin/macosx/mavericks/contrib/3.1/RXKCD_1.7-5.tgz'을 시도합니다
Content type 'application/x-gzip' length 339817 bytes (331 Kb)
URL을 열었습니다
==================================================
downloaded 331 Kb


다운로드된 바이너리 패키지들은 다음의 위치에 있습니다
	/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T//RtmpyN9XPt/downloaded_packages
R > require(RXKCD)
필요한 패키지를 로딩중입니다: RXKCD
필요한 패키지를 로딩중입니다: RJSONIO
필요한 패키지를 로딩중입니다: png
필요한 패키지를 로딩중입니다: jpeg
R > getXKCD(which="552")
image.url = http://imgs.xkcd.com/comics/correlation.png
title =  Correlation
num = 552
year = 2009
transcript = [[A man is talking to a woman]]
Man: I used to think correlation implied causation.
Man: Then I took a statistics class.  Now I don't.
Woman: Sounds like the class helped.
Man: Well, maybe.
{{Title text: Correlation doesn't imply causation, but it does waggle its eyebrows suggestively and gesture furtively while mouthing 'look over there'.}}
alt = Correlation doesn't imply causation, but it does waggle its eyebrows suggestively and gesture furtively while mouthing 'look over there'.
R > diff
function (x, ...) 
UseMethod("diff")
<bytecode: 0x7fb441bebb48>
<environment: namespace:base>
R > ?by
R > install.packages("HSAUR2")
URL 'http://cran.nexr.com/bin/macosx/mavericks/contrib/3.1/HSAUR2_1.1-11.tgz'을 시도합니다
Content type 'application/x-gzip' length 2997364 bytes (2.9 Mb)
URL을 열었습니다
==================================================
downloaded 2.9 Mb


다운로드된 바이너리 패키지들은 다음의 위치에 있습니다
	/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T//RtmpyN9XPt/downloaded_packages
R > library(HSAUR2)
필요한 패키지를 로딩중입니다: tools
R > roomwidth
      unit width
1   metres     8
2   metres     9
3   metres    10
4   metres    10
5   metres    10
6   metres    10
7   metres    10
8   metres    10
9   metres    11
10  metres    11
11  metres    11
12  metres    11
13  metres    12
14  metres    12
15  metres    13
16  metres    13
17  metres    13
18  metres    14
19  metres    14
20  metres    14
21  metres    15
22  metres    15
23  metres    15
24  metres    15
25  metres    15
26  metres    15
27  metres    15
28  metres    15
29  metres    16
30  metres    16
31  metres    16
32  metres    17
33  metres    17
34  metres    17
35  metres    17
36  metres    18
37  metres    18
38  metres    20
39  metres    22
40  metres    25
41  metres    27
42  metres    35
43  metres    38
44  metres    40
45    feet    24
46    feet    25
47    feet    27
48    feet    30
49    feet    30
50    feet    30
51    feet    30
52    feet    30
53    feet    30
54    feet    32
55    feet    32
56    feet    33
57    feet    34
58    feet    34
59    feet    34
60    feet    35
61    feet    35
62    feet    36
63    feet    36
64    feet    36
65    feet    37
66    feet    37
67    feet    40
68    feet    40
69    feet    40
70    feet    40
71    feet    40
72    feet    40
73    feet    40
74    feet    40
75    feet    40
76    feet    41
77    feet    41
78    feet    42
79    feet    42
80    feet    42
81    feet    42
82    feet    43
83    feet    43
84    feet    44
85    feet    44
86    feet    44
87    feet    45
88    feet    45
89    feet    45
90    feet    45
91    feet    45
92    feet    45
93    feet    46
94    feet    46
95    feet    47
96    feet    48
97    feet    48
98    feet    50
99    feet    50
100   feet    50
101   feet    51
102   feet    54
103   feet    54
104   feet    54
105   feet    55
106   feet    55
107   feet    60
108   feet    60
109   feet    63
110   feet    70
111   feet    75
112   feet    80
113   feet    94
R > tapply(width,unit,mean)
다음에 오류가 있습니다tapply(width, unit, mean) : 객체 'unit'를 찾을 수 없습니다
R > with(roomwidth,tapply(width,unit,mean))
    feet   metres 
43.69565 16.02273 
R > obp
function (data) 
{
	c(OBP=with(data,sum(h+bb+hbp)/sum(ab+bb+hbp+sf)))
}
R > ls()
 [1] "a"                        "baseball"                 "baseball.2"               "careerOBP"               
 [5] "careerOBP.order"          "cx"                       "diamonds"                 "g"                       
 [9] "gdp"                      "gdp_capita"               "grdp_gw"                  "grdp_gw_capita"          
[13] "grdp_yg"                  "hello.person"             "jobs"                     "jobs.bus"                
[17] "jobs.kr"                  "jobs.specialty"           "jobs.village"             "lifetable"               
[21] "obp"                      "p"                        "p.x"                      "pop_gw"                  
[25] "price.by.cut.color"       "price.carat.by.cut.color" "rate.gdp"                 "rate.grdp"               
[29] "rate.grdp_yg"             "revenue"                  "revenue.bus"              "revenue.specialty"       
[33] "say.hello"                "theM"                     "theURL"                   "tomato"                  
[37] "tomato2"                  "v"                        "x"                        "year"                    
R > search()
 [1] ".GlobalEnv"        "package:HSAUR2"    "package:tools"     "package:RXKCD"     "package:jpeg"      "package:png"      
 [7] "package:RJSONIO"   "package:plyr"      "package:ggplot2"   "tools:RGUI"        "package:stats"     "package:graphics" 
[13] "package:grDevices" "package:utils"     "package:datasets"  "KoreaEnv"          "package:methods"   "Autoloads"        
[19] "package:base"     
R > detach(package="HSAUR2")
다음에 오류가 있습니다detach(package = "HSAUR2") : 
  사용되지 않은 인자 (package = "HSAUR2")
R > detach("package:HSAUR2")
R > search()
 [1] ".GlobalEnv"        "package:tools"     "package:RXKCD"     "package:jpeg"      "package:png"       "package:RJSONIO"  
 [7] "package:plyr"      "package:ggplot2"   "tools:RGUI"        "package:stats"     "package:graphics"  "package:grDevices"
[13] "package:utils"     "package:datasets"  "KoreaEnv"          "package:methods"   "Autoloads"         "package:base"     
R > help("KoreaEnv")
No documentation for ‘KoreaEnv’ in specified packages and libraries:
you could try ‘??KoreaEnv’
R > ??Koreaenv
No help files found matching ‘Koreaenv’ using fuzzy matching
R > ??KoreaEnv
No help files found matching ‘KoreaEnv’ using fuzzy matching
R > install.packages(lifetable)
다음에 오류가 있습니다available[bins, "Version"] : 
  유효하지 않은 첨자의 타입 'list'입니다
R > install.packages("lifetable")
경고메시지:
package ‘lifetable’ is not available (for R version 3.1.1) 
경고: dependencies ‘gWidgetsRGtk2’, ‘RGtk2’ are not available
also installing the dependency ‘gWidgets’

URL 'http://cran.nexr.com/bin/macosx/mavericks/contrib/3.1/gWidgets_0.0-54.tgz'을 시도합니다
Content type 'application/x-gzip' length 1159210 bytes (1.1 Mb)
URL을 열었습니다
==================================================
downloaded 1.1 Mb

URL 'http://cran.nexr.com/bin/macosx/mavericks/contrib/3.1/LifeTables_0.2.tgz'을 시도합니다
Content type 'application/x-gzip' length 687497 bytes (671 Kb)
URL을 열었습니다
==================================================
downloaded 671 Kb


다운로드된 바이너리 패키지들은 다음의 위치에 있습니다
	/var/folders/_h/tg1th9bd4h98rjjb5vy9gn3m0000gn/T//RtmpyN9XPt/downloaded_packages
R > library(LifeTable)
다음에 오류가 있습니다library(LifeTable) : 
  ‘LifeTable’이라고 불리는 패키지가 없습니다
R > library(LifeTables)
필요한 패키지를 로딩중입니다: mclust
Package 'mclust' version 4.3
R > help(package="LifeTables")
R > getwd()
[1] "/Users/coop2711/Documents/통계분석사례연구/R.WD"
R > lifetable.kr<-read.table("./lifetable/national_lifetable2.csv",header=TRUE,sep=",")
다음에 오류가 있습니다file(file, "rt") : 커넥션을 열 수 없습니다
추가정보: 경고메시지:
In file(file, "rt") :
  파일 './lifetable/national_lifetable2.csv'를 여는데 실패했습니다: No such file or directory
R > lifetable.kr<-read.table("./lifetable/national_lifetable2.csv",header=TRUE,sep=",")
R > str(lifetable.kr)
'data.frame':	101 obs. of  11 variables:
 $ age  : chr  "0" "1" "2" "3" ...
 $ e0x  : num  81.4 80.7 79.7 78.7 77.7 ...
 $ e0x.m: num  78 77.2 76.2 75.2 74.2 ...
 $ e0x.f: num  84.6 83.9 82.9 81.9 80.9 ...
 $ qx   : num  0.00291 0.00031 0.00022 0.00016 0.00013 0.00012 0.00012 0.00011 0.00011 0.0001 ...
 $ qx.m : num  0.00314 0.00035 0.00026 0.00019 0.00015 0.00014 0.00014 0.00013 0.00012 0.0001 ...
 $ qx.f : num  0.00267 0.00027 0.00019 0.00014 0.00012 0.00011 0.00011 0.0001 0.0001 0.00009 ...
 $ lx   : int  100000 99709 99678 99656 99639 99626 99614 99602 99590 99580 ...
 $ lx.m : int  100000 99686 99651 99625 99607 99592 99579 99565 99553 99541 ...
 $ lx.f : int  100000 99733 99706 99687 99674 99662 99651 99640 99630 99621 ...
 $ X    : logi  NA NA NA NA NA NA ...
R > lifetable.kr<-read.table("./lifetable/national_lifetable2.csv",header=TRUE,sep=",")
R > str(lifetable.kr)
'data.frame':	101 obs. of  10 variables:
 $ age  : int  0 1 2 3 4 5 6 7 8 9 ...
 $ e0x  : num  81.4 80.7 79.7 78.7 77.7 ...
 $ e0x.m: num  78 77.2 76.2 75.2 74.2 ...
 $ e0x.f: num  84.6 83.9 82.9 81.9 80.9 ...
 $ qx   : num  0.00291 0.00031 0.00022 0.00016 0.00013 0.00012 0.00012 0.00011 0.00011 0.0001 ...
 $ qx.m : num  0.00314 0.00035 0.00026 0.00019 0.00015 0.00014 0.00014 0.00013 0.00012 0.0001 ...
 $ qx.f : num  0.00267 0.00027 0.00019 0.00014 0.00012 0.00011 0.00011 0.0001 0.0001 0.00009 ...
 $ lx   : int  100000 99709 99678 99656 99639 99626 99614 99602 99590 99580 ...
 $ lx.m : int  100000 99686 99651 99625 99607 99592 99579 99565 99553 99541 ...
 $ lx.f : int  100000 99733 99706 99687 99674 99662 99651 99640 99630 99621 ...
R > p<-ggplot(lifetable.kr,aes(age,lx.m,label="남자"))+xlab(NULL)+ylab(NULL)
R > p
에러: No layers in plot
R > p+geom_lines()
에러: 함수 "geom_lines"를 찾을 수 없습니다
R > p+geom_line()
R > p+geom_line()+labs(title="생존함수")
R > ?element_text
R > source("lifetable_kr.r")
다음에 오류가 있습니다file(filename, "r", encoding = encoding) : 
  커넥션을 열 수 없습니다
추가정보: 경고메시지:
In file(filename, "r", encoding = encoding) :
  파일 'lifetable_kr.r'를 여는데 실패했습니다: No such file or directory
R > getwd()
[1] "/Users/coop2711/Documents/통계분석사례연구/R.WD"
R > source("./lifetable/lifetable_kr.r")
R > dev.off()
null device 
          1 
R > source("./lifetable/lifetable_kr.r")
R > setwd("./lifetable")
R > source("./lifetable/lifetable_kr.r")
다음에 오류가 있습니다file(filename, "r", encoding = encoding) : 
  커넥션을 열 수 없습니다
추가정보: 경고메시지:
In file(filename, "r", encoding = encoding) :
  파일 './lifetable/lifetable_kr.r'를 여는데 실패했습니다: No such file or directory
R > source("./lifetable_kr.r")
R > source("./lifetable_kr.r")
R > source("./lifetable_kr.r")
R > source("./lifetable_kr.r")
R > source("./lifetable_kr.r",echo=TRUE)

R > #par(family="Korea1deb")
R > #pdf("life-distribution.pdf",width=6,height=6)
R > plot(lifetable.kr$age,lifetable.kr$lx.m/1000,xlab="",ylab="",col=" ..." ... [TRUNCATED] 

R > lines(lifetable.kr$age,lifetable.kr$lx.f/1000,xlab="",ylab="",col="red")

R > legend(80,100,legend=c("남","여"),col=c("blue","red"),lty=1)

R > title(main="국민생존함수(2012)",xlab="연령(세)",ylab="생존률(% )")

R > #dev.off()
R > ?polygon
R > ?rev
R > polygon(c(lifetable.kr$age,rev(lifetable$age)),c(lifetable.kr$lx.m/100,rev(lifetable.kr$lx.f/100)),density=20)
다음에 오류가 있습니다xy.coords(x, y) : 'x' and 'y' lengths differ
R > polygon(c(lifetable.kr$age,rev(lifetable$age)),c(lifetable.kr$lx.m/1000,rev(lifetable.kr$lx.f/1000)),density=20)
다음에 오류가 있습니다xy.coords(x, y) : 'x' and 'y' lengths differ
R > rev(lifetable$age)
NULL
R > rev(lifetable.kr$age)
  [1] 100  99  98  97  96  95  94  93  92  91  90  89  88  87  86  85  84  83  82  81  80  79  78  77  76  75  74  73  72  71
 [31]  70  69  68  67  66  65  64  63  62  61  60  59  58  57  56  55  54  53  52  51  50  49  48  47  46  45  44  43  42  41
 [61]  40  39  38  37  36  35  34  33  32  31  30  29  28  27  26  25  24  23  22  21  20  19  18  17  16  15  14  13  12  11
 [91]  10   9   8   7   6   5   4   3   2   1   0
R > length(rev(lifetable.kr$age))
[1] 101
R > length(lifetable.kr$age)
[1] 101
R > length(c(lifetable.kr$age,rev(lifetable.kr$age)))
[1] 202
R > length(c(lifetable.kr$lx.m/1000,rev(lifetable.kr$lx.f/1000)))
[1] 202
R > polygon(c(lifetable.kr$age,rev(lifetable$age)),c(lifetable.kr$lx.m/1000,rev(lifetable.kr$lx.f/1000)))
다음에 오류가 있습니다xy.coords(x, y) : 'x' and 'y' lengths differ
R > polygon(x=c(lifetable.kr$age,rev(lifetable$age)),y=c(lifetable.kr$lx.m/1000,rev(lifetable.kr$lx.f/1000)))
다음에 오류가 있습니다xy.coords(x, y) : 'x' and 'y' lengths differ
R > x<-c(lifetable.kr$age,rev(lifetable.kr$age))
R > x
  [1]   0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29
 [31]  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59
 [61]  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89
 [91]  90  91  92  93  94  95  96  97  98  99 100 100  99  98  97  96  95  94  93  92  91  90  89  88  87  86  85  84  83  82
[121]  81  80  79  78  77  76  75  74  73  72  71  70  69  68  67  66  65  64  63  62  61  60  59  58  57  56  55  54  53  52
[151]  51  50  49  48  47  46  45  44  43  42  41  40  39  38  37  36  35  34  33  32  31  30  29  28  27  26  25  24  23  22
[181]  21  20  19  18  17  16  15  14  13  12  11  10   9   8   7   6   5   4   3   2   1   0
R > y<-c(lifetable.kr$lx.m/1000,rev(lifetable.kr$lx.f/1000))
R > y
  [1] 100.000  99.686  99.651  99.625  99.607  99.592  99.579  99.565  99.553  99.541  99.531  99.521  99.511  99.499  99.484
 [16]  99.467  99.445  99.418  99.385  99.347  99.306  99.262  99.216  99.166  99.113  99.055  98.994  98.932  98.868  98.803
 [31]  98.736  98.663  98.585  98.500  98.410  98.316  98.215  98.107  97.990  97.864  97.727  97.581  97.421  97.243  97.042
 [46]  96.815  96.562  96.286  95.986  95.660  95.303  94.911  94.486  94.028  93.538  93.013  92.452  91.855  91.221  90.546
 [61]  89.823  89.046  88.211  87.312  86.353  85.330  84.243  83.080  81.811  80.399  78.818  77.067  75.158  73.088  70.846
 [76]  68.401  65.744  62.891  59.858  56.650  53.265  49.691  45.962  42.125  38.227  34.284  30.375  26.563  22.905  19.456
 [91]  16.266  13.371  10.797   8.558   6.652   5.067   3.780   2.760   1.972   1.379   0.943   3.822   5.256   7.064   9.281
[106]  11.930  15.013  18.514  22.396  26.604  31.066  35.702  40.426  45.153  49.801  54.299  58.587  62.617  66.348  69.779
[121]  72.910  75.732  78.251  80.484  82.458  84.195  85.731  87.089  88.291  89.357  90.302  91.136  91.865  92.496  93.046
[136]  93.534  93.974  94.376  94.742  95.075  95.379  95.657  95.914  96.151  96.369  96.573  96.766  96.951  97.127  97.291
[151]  97.444  97.585  97.717  97.843  97.963  98.076  98.184  98.285  98.379  98.467  98.546  98.619  98.685  98.747  98.806
[166]  98.861  98.916  98.969  99.022  99.074  99.123  99.170  99.212  99.251  99.286  99.319  99.351  99.381  99.409  99.435
[181]  99.459  99.480  99.500  99.518  99.537  99.554  99.570  99.581  99.589  99.595  99.603  99.611  99.621  99.630  99.640
[196]  99.651  99.662  99.674  99.687  99.706  99.733 100.000
R > polygon(x,y)
R > polygon(x,y,density=20)
R > ?polygon
R > polygon(x,y,border=NA,density=20)
R > source("./lifetable_kr.r",echo=TRUE)

R > #par(family="Korea1deb")
R > #pdf("life-distribution.pdf",width=6,height=6)
R > plot(lifetable.kr$age,lifetable.kr$lx.m/1000,xlab="",ylab="",col=" ..." ... [TRUNCATED] 

R > lines(lifetable.kr$age,lifetable.kr$lx.f/1000,xlab="",ylab="",col="red")

R > legend(80,100,legend=c("남","여"),col=c("blue","red"),lty=1)

R > title(main="국민생존함수(2012)",xlab="연령(세)",ylab="생존률(% )")

R > #dev.off()
R > polygon(x,y,border=NA,density=20)
R > source("./lifetable_kr.r",echo=TRUE)

R > #par(family="Korea1deb")
R > #pdf("life-distribution.pdf",width=6,height=6)
R > plot(lifetable.kr$age,lifetable.kr$lx.m/1000,xlab="",ylab="",col=" ..." ... [TRUNCATED] 

R > lines(lifetable.kr$age,lifetable.kr$lx.f/1000,xlab="",ylab="",col="red")

R > legend(80,100,legend=c("남","여"),col=c("blue","red"),lty=1)

R > title(main="국민생존함수(2012)",xlab="연령(세)",ylab="생존률(% )")

R > #dev.off()
R > polygon(x,y,border=NA,density=10)
R > source("./lifetable_kr.r",echo=TRUE)

R > #par(family="Korea1deb")
R > #pdf("life-distribution.pdf",width=6,height=6)
R > plot(lifetable.kr$age,lifetable.kr$lx.m/1000,xlab="",ylab="",col=" ..." ... [TRUNCATED] 

R > lines(lifetable.kr$age,lifetable.kr$lx.f/1000,xlab="",ylab="",col="red")

R > legend(80,100,legend=c("남","여"),col=c("blue","red"),lty=1)

R > title(main="국민생존함수(2012)",xlab="연령(세)",ylab="생존률(% )")

R > #dev.off()
R > polygon(x,y,border=NA,col="yellow")
R > source("./lifetable_kr.r",echo=TRUE)

R > #par(family="Korea1deb")
R > #pdf("life-distribution.pdf",width=6,height=6)
R > plot(lifetable.kr$age,lifetable.kr$lx.m/1000,xlab="",ylab="",col=" ..." ... [TRUNCATED] 

R > lines(lifetable.kr$age,lifetable.kr$lx.f/1000,xlab="",ylab="",col="red")

R > legend(80,100,legend=c("남","여"),col=c("blue","red"),lty=1)

R > title(main="국민생존함수(2012)",xlab="연령(세)",ylab="생존률(% )")

R > #dev.off()
R > polygon(x,y,border=NA,col="grey")
R > source("./lifetable_kr.r")
R > polygon(x,y,border=NA,density=15)
R > source("./lifetable_kr.r")
R > source("./lifetable_kr.r")
R > source("./lifetable_kr.r")
R > ls()
 [1] "a"                        "baseball"                 "baseball.2"               "careerOBP"               
 [5] "careerOBP.order"          "cx"                       "diamonds"                 "g"                       
 [9] "gdp"                      "gdp_capita"               "grdp_gw"                  "grdp_gw_capita"          
[13] "grdp_yg"                  "hello.person"             "jobs"                     "jobs.bus"                
[17] "jobs.kr"                  "jobs.specialty"           "jobs.village"             "lifetable"               
[21] "lifetable.kr"             "obp"                      "p"                        "p.x"                     
[25] "pop_gw"                   "price.by.cut.color"       "price.carat.by.cut.color" "rate.gdp"                
[29] "rate.grdp"                "rate.grdp_yg"             "revenue"                  "revenue.bus"             
[33] "revenue.specialty"        "say.hello"                "theM"                     "theURL"                  
[37] "tomato"                   "tomato2"                  "v"                        "x"                       
[41] "y"                        "year"                    
R > diff
function (x, ...) 
UseMethod("diff")
<bytecode: 0x7fb441bebb48>
<environment: namespace:base>
R > diff(lifetable.kr$lx)
  [1]  -291   -31   -22   -17   -13   -12   -12   -12   -10   -10    -9    -9   -10   -11   -14   -19   -23   -26   -29   -31
 [21]   -33   -36   -38   -42   -44   -47   -48   -50   -53   -55   -60   -64   -69   -72   -74   -78   -83   -88   -95  -102
 [41]  -110  -121  -134  -149  -166  -182  -197  -212  -229  -249  -270  -292  -313  -335  -359  -382  -404  -427  -456  -492
 [61]  -533  -576  -626  -674  -722  -769  -822  -902 -1011 -1144 -1280 -1411 -1543 -1691 -1869 -2063 -2254 -2451 -2655 -2871
 [81] -3100 -3324 -3519 -3696 -3869 -3994 -4071 -4092 -4053 -3952 -3789 -3570 -3300 -2991 -2655 -2303 -1954 -1617 -1305 -1026
R > -diff(lifetable.kr$lx)
  [1]  291   31   22   17   13   12   12   12   10   10    9    9   10   11   14   19   23   26   29   31   33   36   38   42
 [25]   44   47   48   50   53   55   60   64   69   72   74   78   83   88   95  102  110  121  134  149  166  182  197  212
 [49]  229  249  270  292  313  335  359  382  404  427  456  492  533  576  626  674  722  769  822  902 1011 1144 1280 1411
 [73] 1543 1691 1869 2063 2254 2451 2655 2871 3100 3324 3519 3696 3869 3994 4071 4092 4053 3952 3789 3570 3300 2991 2655 2303
 [97] 1954 1617 1305 1026
R > dx<--diff(lifetable.kr$lx)
R > qx<-dx/lifetable.kr$lx
경고메시지:
In dx/lifetable.kr$lx : 두 객체의 길이가 서로 배수관계에 있지 않습니다
R > dx[1]
[1] 291
R > dx[100]
[1] 1026
R > lx[101]
에러: 객체 'lx'를 찾을 수 없습니다
R > lifetable.kr$lx[101]
[1] 2632
R > lifetable.kr$lx[100]
[1] 3658
R > dx[101]<-2632
R > qx<-dx/lifetable.kr$lx
R > qx
  [1] 2.910000e-03 3.109047e-04 2.207107e-04 1.705868e-04 1.304710e-04 1.204505e-04 1.204650e-04 1.204795e-04 1.004117e-04
 [10] 1.004218e-04 9.038867e-05 9.039684e-05 1.004500e-04 1.105061e-04 1.406597e-04 1.909222e-04 2.311604e-04 2.613722e-04
 [19] 2.916068e-04 3.118085e-04 3.320287e-04 3.623334e-04 3.826017e-04 4.230374e-04 4.433696e-04 4.738094e-04 4.841199e-04
 [28] 5.045358e-04 5.350779e-04 5.555668e-04 6.064098e-04 6.472296e-04 6.982463e-04 7.291139e-04 7.499139e-04 7.910430e-04
 [37] 8.424172e-04 8.939183e-04 9.658889e-04 1.038062e-03 1.120642e-03 1.234089e-03 1.368366e-03 1.523626e-03 1.700053e-03
 [46] 1.867088e-03 2.024749e-03 2.183339e-03 2.363578e-03 2.576093e-03 2.800568e-03 3.037269e-03 3.265621e-03 3.506605e-03
 [55] 3.771048e-03 4.027836e-03 4.277033e-03 4.539945e-03 4.870390e-03 5.280613e-03 5.751033e-03 6.250950e-03 6.836300e-03
 [64] 7.411154e-03 7.998228e-03 8.587573e-03 9.258946e-03 1.025501e-02 1.161335e-02 1.329552e-02 1.507656e-02 1.687395e-02
 [73] 1.876923e-02 2.096298e-02 2.366572e-02 2.675538e-02 3.003611e-02 3.367267e-02 3.774631e-02 4.241833e-02 4.783065e-02
 [82] 5.386311e-02 6.026923e-02 6.736044e-02 7.560628e-02 8.443261e-02 9.399677e-02 1.042840e-01 1.153157e-01 1.270985e-01
 [91] 1.395991e-01 1.528712e-01 1.668099e-01 1.814597e-01 1.967833e-01 2.125127e-01 2.289665e-01 2.457447e-01 2.629458e-01
[100] 2.804811e-01 1.000000e+00
R > indx<1:101
에러: 객체 'indx'를 찾을 수 없습니다
R > indx<-1:101
R > dx[indx+1]
  [1]   31   22   17   13   12   12   12   10   10    9    9   10   11   14   19   23   26   29   31   33   36   38   42   44
 [25]   47   48   50   53   55   60   64   69   72   74   78   83   88   95  102  110  121  134  149  166  182  197  212  229
 [49]  249  270  292  313  335  359  382  404  427  456  492  533  576  626  674  722  769  822  902 1011 1144 1280 1411 1543
 [73] 1691 1869 2063 2254 2451 2655 2871 3100 3324 3519 3696 3869 3994 4071 4092 4053 3952 3789 3570 3300 2991 2655 2303 1954
 [97] 1617 1305 1026 2632   NA
R > lifetable.kr$lx-lifetable.kr$lx[indx+1]
  [1]  291   31   22   17   13   12   12   12   10   10    9    9   10   11   14   19   23   26   29   31   33   36   38   42
 [25]   44   47   48   50   53   55   60   64   69   72   74   78   83   88   95  102  110  121  134  149  166  182  197  212
 [49]  229  249  270  292  313  335  359  382  404  427  456  492  533  576  626  674  722  769  822  902 1011 1144 1280 1411
 [73] 1543 1691 1869 2063 2254 2451 2655 2871 3100 3324 3519 3696 3869 3994 4071 4092 4053 3952 3789 3570 3300 2991 2655 2303
 [97] 1954 1617 1305 1026   NA
R > source("./lifetable_kr_functions.r")
다음에 오류가 있습니다eval(expr, envir, enclos) : 함수 "lx"를 찾을 수 없습니다
R > source("./lifetable_kr_functions.r")
다음에 오류가 있습니다eval(expr, envir, enclos) : 객체 'lx'를 찾을 수 없습니다
R > source("./lifetable_kr_functions.r")
R > ls()
 [1] "a"                        "baseball"                 "baseball.2"               "careerOBP"               
 [5] "careerOBP.order"          "cx"                       "diamonds"                 "dx"                      
 [9] "g"                        "gdp"                      "gdp_capita"               "grdp_gw"                 
[13] "grdp_gw_capita"           "grdp_yg"                  "hello.person"             "indx"                    
[17] "jobs"                     "jobs.bus"                 "jobs.kr"                  "jobs.specialty"          
[21] "jobs.village"             "lifetable"                "lifetable.kr"             "obp"                     
[25] "p"                        "p.x"                      "pop_gw"                   "price.by.cut.color"      
[29] "price.carat.by.cut.color" "qx"                       "rate.gdp"                 "rate.grdp"               
[33] "rate.grdp_yg"             "revenue"                  "revenue.bus"              "revenue.specialty"       
[37] "say.hello"                "theM"                     "theURL"                   "tomato"                  
[41] "tomato2"                  "v"                        "x"                        "y"                       
[45] "year"                    
R > x
  [1] 100000  99709  99678  99656  99639  99626  99614  99602  99590  99580  99570  99561  99552  99542  99531  99517  99498
 [18]  99475  99449  99420  99389  99356  99320  99282  99240  99196  99149  99101  99051  98998  98943  98883  98819  98750
 [35]  98678  98604  98526  98443  98355  98260  98158  98048  97927  97793  97644  97478  97296  97099  96887  96658  96409
 [52]  96139  95847  95534  95199  94840  94458  94054  93627  93171  92679  92146  91570  90944  90270  89548  88779  87957
 [69]  87055  86044  84900  83620  82209  80666  78975  77106  75043  72789  70338  67683  64812  61712  58388  54869  51173
 [86]  47304  43310  39239  35147  31094  27142  23353  19783  16483  13492  10837   8534   6580   4963   3658   2632
R > dx
  [1]  291   31   22   17   13   12   12   12   10   10    9    9   10   11   14   19   23   26   29   31   33   36   38   42
 [25]   44   47   48   50   53   55   60   64   69   72   74   78   83   88   95  102  110  121  134  149  166  182  197  212
 [49]  229  249  270  292  313  335  359  382  404  427  456  492  533  576  626  674  722  769  822  902 1011 1144 1280 1411
 [73] 1543 1691 1869 2063 2254 2451 2655 2871 3100 3324 3519 3696 3869 3994 4071 4092 4053 3952 3789 3570 3300 2991 2655 2303
 [97] 1954 1617 1305 1026 2632
R > qx
  [1] 2.910000e-03 3.109047e-04 2.207107e-04 1.705868e-04 1.304710e-04 1.204505e-04 1.204650e-04 1.204795e-04 1.004117e-04
 [10] 1.004218e-04 9.038867e-05 9.039684e-05 1.004500e-04 1.105061e-04 1.406597e-04 1.909222e-04 2.311604e-04 2.613722e-04
 [19] 2.916068e-04 3.118085e-04 3.320287e-04 3.623334e-04 3.826017e-04 4.230374e-04 4.433696e-04 4.738094e-04 4.841199e-04
 [28] 5.045358e-04 5.350779e-04 5.555668e-04 6.064098e-04 6.472296e-04 6.982463e-04 7.291139e-04 7.499139e-04 7.910430e-04
 [37] 8.424172e-04 8.939183e-04 9.658889e-04 1.038062e-03 1.120642e-03 1.234089e-03 1.368366e-03 1.523626e-03 1.700053e-03
 [46] 1.867088e-03 2.024749e-03 2.183339e-03 2.363578e-03 2.576093e-03 2.800568e-03 3.037269e-03 3.265621e-03 3.506605e-03
 [55] 3.771048e-03 4.027836e-03 4.277033e-03 4.539945e-03 4.870390e-03 5.280613e-03 5.751033e-03 6.250950e-03 6.836300e-03
 [64] 7.411154e-03 7.998228e-03 8.587573e-03 9.258946e-03 1.025501e-02 1.161335e-02 1.329552e-02 1.507656e-02 1.687395e-02
 [73] 1.876923e-02 2.096298e-02 2.366572e-02 2.675538e-02 3.003611e-02 3.367267e-02 3.774631e-02 4.241833e-02 4.783065e-02
 [82] 5.386311e-02 6.026923e-02 6.736044e-02 7.560628e-02 8.443261e-02 9.399677e-02 1.042840e-01 1.153157e-01 1.270985e-01
 [91] 1.395991e-01 1.528712e-01 1.668099e-01 1.814597e-01 1.967833e-01 2.125127e-01 2.289665e-01 2.457447e-01 2.629458e-01
[100] 2.804811e-01 1.000000e+00
R > fix(dx)
R > dx<-function()
+ 
+ {}
R > fix(dx)
R > dx(lifetable.kr$lx)
  [1]  291   31   22   17   13   12   12   12   10   10    9    9   10   11   14   19   23   26   29   31   33   36   38   42
 [25]   44   47   48   50   53   55   60   64   69   72   74   78   83   88   95  102  110  121  134  149  166  182  197  212
 [49]  229  249  270  292  313  335  359  382  404  427  456  492  533  576  626  674  722  769  822  902 1011 1144 1280 1411
 [73] 1543 1691 1869 2063 2254 2451 2655 2871 3100 3324 3519 3696 3869 3994 4071 4092 4053 3952 3789 3570 3300 2991 2655 2303
 [97] 1954 1617 1305 1026 2632
R > dx.m<-dx(lifetable.kr$lx.m)
R > dx.f<-dx(lifetable.kr$lx.f)
R > dx.m
  [1]  314   35   26   18   15   13   14   12   12   10   10   10   12   15   17   22   27   33   38   41   44   46   50   53
 [25]   58   61   62   64   65   67   73   78   85   90   94  101  108  117  126  137  146  160  178  201  227  253  276  300
 [49]  326  357  392  425  458  490  525  561  597  634  675  723  777  835  899  959 1023 1087 1163 1269 1412 1581 1751 1909
 [73] 2070 2242 2445 2657 2853 3033 3208 3385 3574 3729 3837 3898 3943 3909 3812 3658 3449 3190 2895 2574 2239 1906 1585 1287
 [97] 1020  788  593  436  943
R > dx.f
  [1]  267   27   19   13   12   11   11   10    9   10    8    8    6    8   11   16   17   19   18   20   21   24   26   28
 [25]   30   32   33   35   39   42   47   49   52   53   53   55   55   59   62   66   73   79   88   94  101  108  113  120
 [49]  126  132  141  153  164  176  185  193  204  218  237  257  278  304  333  366  402  440  488  550  631  729  834  945
 [73] 1066 1202 1358 1536 1737 1974 2233 2519 2822 3131 3431 3731 4030 4288 4498 4648 4727 4724 4636 4462 4208 3882 3501 3083
 [97] 2649 2217 1808 1434 3822
R > rm(qx)
R > ls()
 [1] "a"                        "baseball"                 "baseball.2"               "careerOBP"               
 [5] "careerOBP.order"          "cx"                       "diamonds"                 "dx"                      
 [9] "dx.f"                     "dx.m"                     "g"                        "gdp"                     
[13] "gdp_capita"               "grdp_gw"                  "grdp_gw_capita"           "grdp_yg"                 
[17] "hello.person"             "indx"                     "jobs"                     "jobs.bus"                
[21] "jobs.kr"                  "jobs.specialty"           "jobs.village"             "lifetable"               
[25] "lifetable.kr"             "obp"                      "p"                        "p.x"                     
[29] "pop_gw"                   "price.by.cut.color"       "price.carat.by.cut.color" "rate.gdp"                
[33] "rate.grdp"                "rate.grdp_yg"             "revenue"                  "revenue.bus"             
[37] "revenue.specialty"        "say.hello"                "theM"                     "theURL"                  
[41] "tomato"                   "tomato2"                  "v"                        "x"                       
[45] "y"                        "year"                    
R > class(dx)
[1] "function"
R > dx.o<-dx(lifetable.kr$lx)
R > fix(qx)
R > qx(lifetable.kr$lx)
  [1] 2.910000e-03 3.109047e-04 2.207107e-04 1.705868e-04 1.304710e-04 1.204505e-04 1.204650e-04 1.204795e-04 1.004117e-04
 [10] 1.004218e-04 9.038867e-05 9.039684e-05 1.004500e-04 1.105061e-04 1.406597e-04 1.909222e-04 2.311604e-04 2.613722e-04
 [19] 2.916068e-04 3.118085e-04 3.320287e-04 3.623334e-04 3.826017e-04 4.230374e-04 4.433696e-04 4.738094e-04 4.841199e-04
 [28] 5.045358e-04 5.350779e-04 5.555668e-04 6.064098e-04 6.472296e-04 6.982463e-04 7.291139e-04 7.499139e-04 7.910430e-04
 [37] 8.424172e-04 8.939183e-04 9.658889e-04 1.038062e-03 1.120642e-03 1.234089e-03 1.368366e-03 1.523626e-03 1.700053e-03
 [46] 1.867088e-03 2.024749e-03 2.183339e-03 2.363578e-03 2.576093e-03 2.800568e-03 3.037269e-03 3.265621e-03 3.506605e-03
 [55] 3.771048e-03 4.027836e-03 4.277033e-03 4.539945e-03 4.870390e-03 5.280613e-03 5.751033e-03 6.250950e-03 6.836300e-03
 [64] 7.411154e-03 7.998228e-03 8.587573e-03 9.258946e-03 1.025501e-02 1.161335e-02 1.329552e-02 1.507656e-02 1.687395e-02
 [73] 1.876923e-02 2.096298e-02 2.366572e-02 2.675538e-02 3.003611e-02 3.367267e-02 3.774631e-02 4.241833e-02 4.783065e-02
 [82] 5.386311e-02 6.026923e-02 6.736044e-02 7.560628e-02 8.443261e-02 9.399677e-02 1.042840e-01 1.153157e-01 1.270985e-01
 [91] 1.395991e-01 1.528712e-01 1.668099e-01 1.814597e-01 1.967833e-01 2.125127e-01 2.289665e-01 2.457447e-01 2.629458e-01
[100] 2.804811e-01 1.000000e+00
R > qx.o<-qx(lifetable.kr$lx)
R > qx.m<-qx(lifetable.kr$lx.m)
R > qx.f<-qx(lifetable.kr$lx.f)
R > ls()
 [1] "a"                        "baseball"                 "baseball.2"               "careerOBP"               
 [5] "careerOBP.order"          "cx"                       "diamonds"                 "dx"                      
 [9] "dx.f"                     "dx.m"                     "dx.o"                     "g"                       
[13] "gdp"                      "gdp_capita"               "grdp_gw"                  "grdp_gw_capita"          
[17] "grdp_yg"                  "hello.person"             "indx"                     "jobs"                    
[21] "jobs.bus"                 "jobs.kr"                  "jobs.specialty"           "jobs.village"            
[25] "lifetable"                "lifetable.kr"             "obp"                      "p"                       
[29] "p.x"                      "pop_gw"                   "price.by.cut.color"       "price.carat.by.cut.color"
[33] "qx"                       "qx.f"                     "qx.m"                     "qx.o"                    
[37] "rate.gdp"                 "rate.grdp"                "rate.grdp_yg"             "revenue"                 
[41] "revenue.bus"              "revenue.specialty"        "say.hello"                "theM"                    
[45] "theURL"                   "tomato"                   "tomato2"                  "v"                       
[49] "x"                        "y"                        "year"                    
R > qx.o
  [1] 2.910000e-03 3.109047e-04 2.207107e-04 1.705868e-04 1.304710e-04 1.204505e-04 1.204650e-04 1.204795e-04 1.004117e-04
 [10] 1.004218e-04 9.038867e-05 9.039684e-05 1.004500e-04 1.105061e-04 1.406597e-04 1.909222e-04 2.311604e-04 2.613722e-04
 [19] 2.916068e-04 3.118085e-04 3.320287e-04 3.623334e-04 3.826017e-04 4.230374e-04 4.433696e-04 4.738094e-04 4.841199e-04
 [28] 5.045358e-04 5.350779e-04 5.555668e-04 6.064098e-04 6.472296e-04 6.982463e-04 7.291139e-04 7.499139e-04 7.910430e-04
 [37] 8.424172e-04 8.939183e-04 9.658889e-04 1.038062e-03 1.120642e-03 1.234089e-03 1.368366e-03 1.523626e-03 1.700053e-03
 [46] 1.867088e-03 2.024749e-03 2.183339e-03 2.363578e-03 2.576093e-03 2.800568e-03 3.037269e-03 3.265621e-03 3.506605e-03
 [55] 3.771048e-03 4.027836e-03 4.277033e-03 4.539945e-03 4.870390e-03 5.280613e-03 5.751033e-03 6.250950e-03 6.836300e-03
 [64] 7.411154e-03 7.998228e-03 8.587573e-03 9.258946e-03 1.025501e-02 1.161335e-02 1.329552e-02 1.507656e-02 1.687395e-02
 [73] 1.876923e-02 2.096298e-02 2.366572e-02 2.675538e-02 3.003611e-02 3.367267e-02 3.774631e-02 4.241833e-02 4.783065e-02
 [82] 5.386311e-02 6.026923e-02 6.736044e-02 7.560628e-02 8.443261e-02 9.399677e-02 1.042840e-01 1.153157e-01 1.270985e-01
 [91] 1.395991e-01 1.528712e-01 1.668099e-01 1.814597e-01 1.967833e-01 2.125127e-01 2.289665e-01 2.457447e-01 2.629458e-01
[100] 2.804811e-01 1.000000e+00
R > qx.m
  [1] 0.0031400000 0.0003511025 0.0002609106 0.0001806775 0.0001505918 0.0001305326 0.0001405919 0.0001205243 0.0001205388
 [10] 0.0001004611 0.0001004712 0.0001004813 0.0001205897 0.0001507553 0.0001708817 0.0002211789 0.0002715069 0.0003319318
 [19] 0.0003823515 0.0004126949 0.0004430749 0.0004634200 0.0005039510 0.0005344574 0.0005851906 0.0006158195 0.0006263006
 [28] 0.0006469090 0.0006574422 0.0006781171 0.0007393453 0.0007905699 0.0008622001 0.0009137056 0.0009551875 0.0010272997
 [37] 0.0010996284 0.0011925755 0.0012858455 0.0013999019 0.0014939577 0.0016396635 0.0018271215 0.0020669868 0.0023391933
 [46] 0.0026132314 0.0028582672 0.0031157178 0.0033963286 0.0037319674 0.0041131969 0.0044778793 0.0048472790 0.0052112137
 [55] 0.0056126922 0.0060314150 0.0064574049 0.0069021828 0.0073996119 0.0079848917 0.0086503457 0.0093771758 0.0101914727
 [64] 0.0109835990 0.0118467222 0.0127387789 0.0138053013 0.0152744343 0.0172592928 0.0196644237 0.0222157375 0.0247706541
 [73] 0.0275419782 0.0306753503 0.0345114756 0.0388444613 0.0433955950 0.0482262963 0.0535935046 0.0597528685 0.0670984699
 [82] 0.0750437705 0.0834820069 0.0925341246 0.1031469903 0.1140182009 0.1254979424 0.1377103490 0.1505784763 0.1639597039
 [91] 0.1779786057 0.1925061701 0.2073724183 0.2227155878 0.2382742032 0.2539964476 0.2698412698 0.2855072464 0.3007099391
[100] 0.3161711385 1.0000000000
R > qx.f
  [1] 2.670000e-03 2.707228e-04 1.905602e-04 1.304082e-04 1.203925e-04 1.103731e-04 1.103852e-04 1.003613e-04 9.033424e-05
 [10] 1.003804e-04 8.031242e-05 8.031887e-05 6.024399e-05 8.033016e-05 1.104628e-04 1.606910e-04 1.707616e-04 1.908838e-04
 [19] 1.808718e-04 2.010050e-04 2.110977e-04 2.413055e-04 2.614773e-04 2.816646e-04 3.018686e-04 3.220904e-04 3.322627e-04
 [28] 3.525170e-04 3.929431e-04 4.233359e-04 4.739336e-04 4.943353e-04 5.248602e-04 5.352346e-04 5.355212e-04 5.560273e-04
 [37] 5.563367e-04 5.971297e-04 6.278672e-04 6.687946e-04 7.402225e-04 8.016561e-04 8.937004e-04 9.554885e-04 1.027624e-03
 [46] 1.099976e-03 1.152168e-03 1.224952e-03 1.287777e-03 1.350840e-03 1.444894e-03 1.570133e-03 1.685665e-03 1.812060e-03
 [55] 1.908180e-03 1.994502e-03 2.112392e-03 2.262138e-03 2.464873e-03 2.679484e-03 2.906217e-03 3.187284e-03 3.502498e-03
 [64] 3.863123e-03 4.259558e-03 4.682146e-03 5.217354e-03 5.911055e-03 6.821917e-03 7.935558e-03 9.151159e-03 1.046488e-02
 [73] 1.192968e-02 1.361407e-02 1.559324e-02 1.791651e-02 2.063068e-02 2.393946e-02 2.774464e-02 3.219128e-02 3.726298e-02
 [82] 4.294335e-02 4.916952e-02 5.623380e-02 6.435952e-02 7.319030e-02 8.283762e-02 9.333146e-02 1.046885e-01 1.168555e-01
 [91] 1.298527e-01 1.436297e-01 1.581717e-01 1.733345e-01 1.891001e-01 2.053554e-01 2.220453e-01 2.388751e-01 2.559456e-01
[100] 2.728311e-01 1.000000e+00
R > qx
function (lx) 
{
	dx<-dx(lx)
	qx<-dx/lx
	return(qx)
}
R > dx
function(lx)
{
	x<-lx
	indx<-1:length(x)
	dx<-x-x[indx+1]
	dx[length(dx)]<-x[length(x)]
	return(dx)
	}
R > sum(lifetable.kr$lx)
[1] 8189244
R > sum(lifetable.kr$lx/100000)
[1] 81.89244
R > fix(mux)
다음에 오류가 있습니다.External2(C_edit, name, file, title, editor) : 
  예기치 않은 심볼입니다가 6번째 라인에서 발생했습니다
 다음과 같은 명령어를 이용하여 복구해보세요
 x <- edit()
 
R > mux(lifetable.kr$lx)
에러: 함수 "mux"를 찾을 수 없습니다
R > fix(mux)
R > edit()
function () 
{
}
R > edit(mux)
function () 
{
}
R > fix(mux)
R > mux(lifetable.kr$lx)
다음에 오류가 있습니다x[indx - 2] : only 0's may be mixed with negative subscripts
추가정보: 경고메시지:
In x[indx - 1] - x[indx + 1] :
  두 객체의 길이가 서로 배수관계에 있지 않습니다
R > fix(mux)
R > mux(lifetable.kr$lx)
다음에 오류가 있습니다mux[1] <- 0 : 객체의 타입 'closure'는 부분대입할 수 없습니다
R > fix(mux)
R > mux(lifetable.kr$lx)
다음에 오류가 있습니다mux[indx] <- (8 * (x[indx - 1] - x[indx + 1]) - (x[indx - 2] -  : 
  객체의 타입 'closure'는 부분대입할 수 없습니다
R > fix(mux)
R > mux(lifetable.kr$lx)
  [1] 5.266960e-05 1.914921e-04 1.471981e-04 1.221234e-04 1.196284e-04 1.221528e-04 1.104529e-04 9.958492e-05 9.541026e-05
 [10] 8.872283e-05 9.459043e-05 1.038088e-04 1.222400e-04 1.649634e-04 2.127346e-04 2.471308e-04 2.773616e-04 3.025883e-04
 [19] 3.211288e-04 3.472362e-04 3.716942e-04 4.028928e-04 4.341327e-04 4.595279e-04 4.799174e-04 4.927633e-04 5.199342e-04
 [28] 5.437820e-04 5.794582e-04 6.270036e-04 6.737908e-04 7.164557e-04 7.389354e-04 7.682244e-04 8.161974e-04 8.668299e-04
 [37] 9.286090e-04 1.001594e-03 1.076496e-03 1.173745e-03 1.298586e-03 1.443525e-03 1.612149e-03 1.786728e-03 1.948521e-03
 [46] 2.104381e-03 2.271547e-03 2.469187e-03 2.689929e-03 2.922851e-03 3.156072e-03 3.388846e-03 3.644121e-03 3.908337e-03
 [55] 4.160579e-03 4.411473e-03 4.703949e-03 5.076687e-03 5.523545e-03 6.009485e-03 6.558735e-03 7.149088e-03 7.733282e-03
 [64] 8.320491e-03 8.929477e-03 9.747187e-03 1.093657e-02 1.249651e-02 1.427758e-02 1.609463e-02 1.794917e-02 1.999810e-02
 [73] 2.249024e-02 2.548332e-02 2.876018e-02 3.230456e-02 3.627366e-02 4.079188e-02 4.605371e-02 5.209414e-02 5.866645e-02
 [82] 6.578092e-02 7.400061e-02 8.328048e-02 9.330794e-02 1.042628e-01 1.161597e-01 1.290549e-01 1.429642e-01 1.579419e-01
 [91] 1.740130e-01 1.911667e-01 2.095007e-01 2.288533e-01 2.492676e-01 2.708840e-01 2.934045e-01           NA           NA
[100] 0.000000e+00 0.000000e+00
R > fix(mux)
R > mux(lifetable.kr$lx)
다음에 오류가 있습니다mux[indx + 2] <- mu[indx] : 
  객체의 타입 'closure'는 부분대입할 수 없습니다
R > fix(mux)
R > mux(lifetable.kr$lx)
 [1] 5.266960e-05 1.914921e-04 1.471981e-04 1.221234e-04 1.196284e-04 1.221528e-04 1.104529e-04 9.958492e-05 9.541026e-05
[10] 8.872283e-05 9.459043e-05 1.038088e-04 1.222400e-04 1.649634e-04 2.127346e-04 2.471308e-04 2.773616e-04 3.025883e-04
[19] 3.211288e-04 3.472362e-04 3.716942e-04 4.028928e-04 4.341327e-04 4.595279e-04 4.799174e-04 4.927633e-04 5.199342e-04
[28] 5.437820e-04 5.794582e-04 6.270036e-04 6.737908e-04 7.164557e-04 7.389354e-04 7.682244e-04 8.161974e-04 8.668299e-04
[37] 9.286090e-04 1.001594e-03 1.076496e-03 1.173745e-03 1.298586e-03 1.443525e-03 1.612149e-03 1.786728e-03 1.948521e-03
[46] 2.104381e-03 2.271547e-03 2.469187e-03 2.689929e-03 2.922851e-03 3.156072e-03 3.388846e-03 3.644121e-03 3.908337e-03
[55] 4.160579e-03 4.411473e-03 4.703949e-03 5.076687e-03 5.523545e-03 6.009485e-03 6.558735e-03 7.149088e-03 7.733282e-03
[64] 8.320491e-03 8.929477e-03 9.747187e-03 1.093657e-02 1.249651e-02 1.427758e-02 1.609463e-02 1.794917e-02 1.999810e-02
[73] 2.249024e-02 2.548332e-02 2.876018e-02 3.230456e-02 3.627366e-02 4.079188e-02 4.605371e-02 5.209414e-02 5.866645e-02
[82] 6.578092e-02 7.400061e-02 8.328048e-02 9.330794e-02 1.042628e-01 1.161597e-01 1.290549e-01 1.429642e-01 1.579419e-01
[91] 1.740130e-01 1.911667e-01 2.095007e-01 2.288533e-01 2.492676e-01 2.708840e-01 2.934045e-01           NA           NA
R > mux<-mux(lifetable.kr$lx)
R > indx
  [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30
 [31]  31  32  33  34  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54  55  56  57  58  59  60
 [61]  61  62  63  64  65  66  67  68  69  70  71  72  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
 [91]  91  92  93  94  95  96  97  98  99 100 101
R > z[indx+2]<mux
에러: 객체 'z'를 찾을 수 없습니다
R > z<-rep(0,101)
R > z<-rep(0,99)
R > z[indx+2]<mux
  [1] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [25] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [49] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [73] TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE TRUE
 [97] TRUE   NA   NA   NA   NA
경고메시지:
In z[indx + 2] < mux : 두 객체의 길이가 서로 배수관계에 있지 않습니다
R > z[indx+2]<-mux
경고메시지:
In z[indx + 2] <- mux :
  number of items to replace is not a multiple of replacement length
R > length(mux)
[1] 99
R > length(z)
[1] 103
R > z<-rep(0,99)
R > z[indx+2]<-mux[indx]
R > z
  [1] 0.000000e+00 0.000000e+00 5.266960e-05 1.914921e-04 1.471981e-04 1.221234e-04 1.196284e-04 1.221528e-04 1.104529e-04
 [10] 9.958492e-05 9.541026e-05 8.872283e-05 9.459043e-05 1.038088e-04 1.222400e-04 1.649634e-04 2.127346e-04 2.471308e-04
 [19] 2.773616e-04 3.025883e-04 3.211288e-04 3.472362e-04 3.716942e-04 4.028928e-04 4.341327e-04 4.595279e-04 4.799174e-04
 [28] 4.927633e-04 5.199342e-04 5.437820e-04 5.794582e-04 6.270036e-04 6.737908e-04 7.164557e-04 7.389354e-04 7.682244e-04
 [37] 8.161974e-04 8.668299e-04 9.286090e-04 1.001594e-03 1.076496e-03 1.173745e-03 1.298586e-03 1.443525e-03 1.612149e-03
 [46] 1.786728e-03 1.948521e-03 2.104381e-03 2.271547e-03 2.469187e-03 2.689929e-03 2.922851e-03 3.156072e-03 3.388846e-03
 [55] 3.644121e-03 3.908337e-03 4.160579e-03 4.411473e-03 4.703949e-03 5.076687e-03 5.523545e-03 6.009485e-03 6.558735e-03
 [64] 7.149088e-03 7.733282e-03 8.320491e-03 8.929477e-03 9.747187e-03 1.093657e-02 1.249651e-02 1.427758e-02 1.609463e-02
 [73] 1.794917e-02 1.999810e-02 2.249024e-02 2.548332e-02 2.876018e-02 3.230456e-02 3.627366e-02 4.079188e-02 4.605371e-02
 [82] 5.209414e-02 5.866645e-02 6.578092e-02 7.400061e-02 8.328048e-02 9.330794e-02 1.042628e-01 1.161597e-01 1.290549e-01
 [91] 1.429642e-01 1.579419e-01 1.740130e-01 1.911667e-01 2.095007e-01 2.288533e-01 2.492676e-01 2.708840e-01 2.934045e-01
[100]           NA           NA           NA           NA
R > fix(mux)
R > rm(mux)
R > fix(mux)
R > mux.o<-mux(lifetable.kr$lx)
R > mux.o
  [1] 0.000000e+00 0.000000e+00 0.000000e+00 0.000000e+00 1.471981e-04 1.221234e-04 1.196284e-04 1.221528e-04 1.104529e-04
 [10] 9.958492e-05 9.541026e-05 8.872283e-05 9.459043e-05 1.038088e-04 1.222400e-04 1.649634e-04 2.127346e-04 2.471308e-04
 [19] 2.773616e-04 3.025883e-04 3.211288e-04 3.472362e-04 3.716942e-04 4.028928e-04 4.341327e-04 4.595279e-04 4.799174e-04
 [28] 4.927633e-04 5.199342e-04 5.437820e-04 5.794582e-04 6.270036e-04 6.737908e-04 7.164557e-04 7.389354e-04 7.682244e-04
 [37] 8.161974e-04 8.668299e-04 9.286090e-04 1.001594e-03 1.076496e-03 1.173745e-03 1.298586e-03 1.443525e-03 1.612149e-03
 [46] 1.786728e-03 1.948521e-03 2.104381e-03 2.271547e-03 2.469187e-03 2.689929e-03 2.922851e-03 3.156072e-03 3.388846e-03
 [55] 3.644121e-03 3.908337e-03 4.160579e-03 4.411473e-03 4.703949e-03 5.076687e-03 5.523545e-03 6.009485e-03 6.558735e-03
 [64] 7.149088e-03 7.733282e-03 8.320491e-03 8.929477e-03 9.747187e-03 1.093657e-02 1.249651e-02 1.427758e-02 1.609463e-02
 [73] 1.794917e-02 1.999810e-02 2.249024e-02 2.548332e-02 2.876018e-02 3.230456e-02 3.627366e-02 4.079188e-02 4.605371e-02
 [82] 5.209414e-02 5.866645e-02 6.578092e-02 7.400061e-02 8.328048e-02 9.330794e-02 1.042628e-01 1.161597e-01 1.290549e-01
 [91] 1.429642e-01 1.579419e-01 1.740130e-01 1.911667e-01 2.095007e-01 2.288533e-01 2.492676e-01 2.708840e-01 2.934045e-01
[100]           NA           NA           NA           NA
R > fix(mux)
R > mux.o<-mux(lifetable.kr$lx)
R > mux.o
  [1] 0.000000e+00 0.000000e+00 5.266960e-05 1.914921e-04 1.471981e-04 1.221234e-04 1.196284e-04 1.221528e-04 1.104529e-04
 [10] 9.958492e-05 9.541026e-05 8.872283e-05 9.459043e-05 1.038088e-04 1.222400e-04 1.649634e-04 2.127346e-04 2.471308e-04
 [19] 2.773616e-04 3.025883e-04 3.211288e-04 3.472362e-04 3.716942e-04 4.028928e-04 4.341327e-04 4.595279e-04 4.799174e-04
 [28] 4.927633e-04 5.199342e-04 5.437820e-04 5.794582e-04 6.270036e-04 6.737908e-04 7.164557e-04 7.389354e-04 7.682244e-04
 [37] 8.161974e-04 8.668299e-04 9.286090e-04 1.001594e-03 1.076496e-03 1.173745e-03 1.298586e-03 1.443525e-03 1.612149e-03
 [46] 1.786728e-03 1.948521e-03 2.104381e-03 2.271547e-03 2.469187e-03 2.689929e-03 2.922851e-03 3.156072e-03 3.388846e-03
 [55] 3.644121e-03 3.908337e-03 4.160579e-03 4.411473e-03 4.703949e-03 5.076687e-03 5.523545e-03 6.009485e-03 6.558735e-03
 [64] 7.149088e-03 7.733282e-03 8.320491e-03 8.929477e-03 9.747187e-03 1.093657e-02 1.249651e-02 1.427758e-02 1.609463e-02
 [73] 1.794917e-02 1.999810e-02 2.249024e-02 2.548332e-02 2.876018e-02 3.230456e-02 3.627366e-02 4.079188e-02 4.605371e-02
 [82] 5.209414e-02 5.866645e-02 6.578092e-02 7.400061e-02 8.328048e-02 9.330794e-02 1.042628e-01 1.161597e-01 1.290549e-01
 [91] 1.429642e-01 1.579419e-01 1.740130e-01 1.911667e-01 2.095007e-01 2.288533e-01 2.492676e-01 2.708840e-01 2.934045e-01
[100]           NA           NA
R > fix(mux)
R > mux.o<-mux(lifetable.kr$lx)
R > mux.o
  [1] 0.000000e+00 0.000000e+00 5.266960e-05 1.914921e-04 1.471981e-04 1.221234e-04 1.196284e-04 1.221528e-04 1.104529e-04
 [10] 9.958492e-05 9.541026e-05 8.872283e-05 9.459043e-05 1.038088e-04 1.222400e-04 1.649634e-04 2.127346e-04 2.471308e-04
 [19] 2.773616e-04 3.025883e-04 3.211288e-04 3.472362e-04 3.716942e-04 4.028928e-04 4.341327e-04 4.595279e-04 4.799174e-04
 [28] 4.927633e-04 5.199342e-04 5.437820e-04 5.794582e-04 6.270036e-04 6.737908e-04 7.164557e-04 7.389354e-04 7.682244e-04
 [37] 8.161974e-04 8.668299e-04 9.286090e-04 1.001594e-03 1.076496e-03 1.173745e-03 1.298586e-03 1.443525e-03 1.612149e-03
 [46] 1.786728e-03 1.948521e-03 2.104381e-03 2.271547e-03 2.469187e-03 2.689929e-03 2.922851e-03 3.156072e-03 3.388846e-03
 [55] 3.644121e-03 3.908337e-03 4.160579e-03 4.411473e-03 4.703949e-03 5.076687e-03 5.523545e-03 6.009485e-03 6.558735e-03
 [64] 7.149088e-03 7.733282e-03 8.320491e-03 8.929477e-03 9.747187e-03 1.093657e-02 1.249651e-02 1.427758e-02 1.609463e-02
 [73] 1.794917e-02 1.999810e-02 2.249024e-02 2.548332e-02 2.876018e-02 3.230456e-02 3.627366e-02 4.079188e-02 4.605371e-02
 [82] 5.209414e-02 5.866645e-02 6.578092e-02 7.400061e-02 8.328048e-02 9.330794e-02 1.042628e-01 1.161597e-01 1.290549e-01
 [91] 1.429642e-01 1.579419e-01 1.740130e-01 1.911667e-01 2.095007e-01 2.288533e-01 2.492676e-01 2.708840e-01 2.934045e-01
[100]           NA           NA
R > fix(mux)
R > mux.o<-mux(lifetable.kr$lx)
R > mux.o
  [1] 0.000000e+00 0.000000e+00 5.266960e-05 1.914921e-04 1.471981e-04 1.221234e-04 1.196284e-04 1.221528e-04 1.104529e-04
 [10] 9.958492e-05 9.541026e-05 8.872283e-05 9.459043e-05 1.038088e-04 1.222400e-04 1.649634e-04 2.127346e-04 2.471308e-04
 [19] 2.773616e-04 3.025883e-04 3.211288e-04 3.472362e-04 3.716942e-04 4.028928e-04 4.341327e-04 4.595279e-04 4.799174e-04
 [28] 4.927633e-04 5.199342e-04 5.437820e-04 5.794582e-04 6.270036e-04 6.737908e-04 7.164557e-04 7.389354e-04 7.682244e-04
 [37] 8.161974e-04 8.668299e-04 9.286090e-04 1.001594e-03 1.076496e-03 1.173745e-03 1.298586e-03 1.443525e-03 1.612149e-03
 [46] 1.786728e-03 1.948521e-03 2.104381e-03 2.271547e-03 2.469187e-03 2.689929e-03 2.922851e-03 3.156072e-03 3.388846e-03
 [55] 3.644121e-03 3.908337e-03 4.160579e-03 4.411473e-03 4.703949e-03 5.076687e-03 5.523545e-03 6.009485e-03 6.558735e-03
 [64] 7.149088e-03 7.733282e-03 8.320491e-03 8.929477e-03 9.747187e-03 1.093657e-02 1.249651e-02 1.427758e-02 1.609463e-02
 [73] 1.794917e-02 1.999810e-02 2.249024e-02 2.548332e-02 2.876018e-02 3.230456e-02 3.627366e-02 4.079188e-02 4.605371e-02
 [82] 5.209414e-02 5.866645e-02 6.578092e-02 7.400061e-02 8.328048e-02 9.330794e-02 1.042628e-01 1.161597e-01 1.290549e-01
 [91] 1.429642e-01 1.579419e-01 1.740130e-01 1.911667e-01 2.095007e-01 2.288533e-01 2.492676e-01 2.708840e-01 2.934045e-01
[100] 0.000000e+00 0.000000e+00
R > plot(lifetable.kr$age,mux.o)
R > plot(lifetable.kr$age,qx.o)
R > plot(lifetable.kr$age,mux.o)
R > lines(lifetable.kr$age,qx.o,lty=2)
R > plot(lifetable.kr$age,mux.o,type="l")
R > lines(lifetable.kr$age,qx.o,lty=2)
R > e0x<-mux
R > fix(e0x)
R > e0x.o<-e0x(lifetable.kr$lx)
다음에 오류가 있습니다e0x(lifetable.kr$lx) : 
  기본값이 없는 인수 "x"가 누락되어 있습니다
R > fix(e0x)
R > e0x.o<-e0x(lifetable.kr$lx,1)
R > e0x.o
[1] 80.62852
R > e0x.o<-e0x(lifetable.kr$lx,1:100)
경고메시지:
1: In (x + 2):n :
  수치형 표현식은 100개의 구성요소들을 가지고 있으나, 오로지 첫번째것만 사용됩니다
2: In (x + 2):n :
  수치형 표현식은 100개의 구성요소들을 가지고 있으나, 오로지 첫번째것만 사용됩니다
R > fix(e0x)
R > e0x.o<-e0x(lifetable.kr$lx,0)
R > e0x.o
[1] 81.39244
R > e0x(lifetable.kr$lx,1)
[1] 80.62852
R > e0x(lifetable.kr$lx,2)
[1] 79.65344
R > e0x(lifetable.kr$lx,3)
[1] 78.67092
R > e0x(lifetable.kr$lx,4)
[1] 77.68426
R > sapply
function (X, FUN, ..., simplify = TRUE, USE.NAMES = TRUE) 
{
    FUN <- match.fun(FUN)
    answer <- lapply(X = X, FUN = FUN, ...)
    if (USE.NAMES && is.character(X) && is.null(names(answer))) 
        names(answer) <- X
    if (!identical(simplify, FALSE) && length(answer)) 
        simplify2array(answer, higher = (simplify == "array"))
    else answer
}
<bytecode: 0x7fb441a35f10>
<environment: namespace:base>
R > ?sapply
R > sapply(0:100,e0x(lx=lifetable.kr$lx,))
다음에 오류가 있습니다e0x(lx = lifetable.kr$lx, ) : 
  기본값이 없는 인수 "x"가 누락되어 있습니다
R > fix(e0x)
R > sapply(0:100,e0x(x,lx=lifetable.kr$lx))
다음에 오류가 있습니다match.fun(FUN) : 
  'e0x(x, lx = lifetable.kr$lx)'은 함수, 문자, 또는 심볼이 아닙니다
추가정보: 경고메시지:
In (x + 2):n :
  수치형 표현식은 101개의 구성요소들을 가지고 있으나, 오로지 첫번째것만 사용됩니다
R > sapply(0:100,e0x(,lx=lifetable.kr$lx))
다음에 오류가 있습니다e0x(, lx = lifetable.kr$lx) : 
  기본값이 없는 인수 "x"가 누락되어 있습니다
R > fix(e0x)
R > sapply(0:100,e0x())
다음에 오류가 있습니다e0x() : 기본값이 없는 인수 "x"가 누락되어 있습니다
R > sapply(0:100,e0x)
  [1] 81.392440 80.628524 79.653444 78.670918 77.684255 76.694327 75.703506 74.712566 73.721508 72.728861 71.736115 70.742555
 [13] 69.748905 68.755862 67.763405 66.772868 65.785523 64.800618 63.817429 62.835898 61.855341 60.875720 59.897604 58.920338
 [25] 57.945062 56.970543 55.997312 55.024192 54.051716 53.080385 52.109614 51.140929 50.173727 49.208435 48.243975 47.279806
 [37] 46.316840 45.355470 44.395603 43.438042 42.482661 41.529761 40.580458 39.635378 38.695096 37.760141 36.829839 35.903547
 [49] 34.981014 34.062706 33.149390 32.241083 31.337783 30.438818 29.544171 28.654112 27.767971 26.885098 26.005431 25.130260
 [61] 24.261014 23.398455 22.542492 21.694218 20.852465 20.016561 19.185613 18.360239 17.545293 16.745572 15.964476 15.201196
 [73] 14.453521 13.720427 13.003501 12.306578 11.631151 10.975841 10.340883  9.726911  9.135638  8.569435  8.028824  7.511682
 [85]  7.018105  6.551222  6.109259  5.691213  5.295601  4.920692  4.564365  4.223804  3.895794  3.575654  3.257486  2.933053
 [97]  2.589641  2.210182  1.767379  1.219519        NA
R > fix(e0x)
R > sapply(0:100,e0x,lx=lifetable.kr$lx)
  [1] 81.392440 80.628524 79.653444 78.670918 77.684255 76.694327 75.703506 74.712566 73.721508 72.728861 71.736115 70.742555
 [13] 69.748905 68.755862 67.763405 66.772868 65.785523 64.800618 63.817429 62.835898 61.855341 60.875720 59.897604 58.920338
 [25] 57.945062 56.970543 55.997312 55.024192 54.051716 53.080385 52.109614 51.140929 50.173727 49.208435 48.243975 47.279806
 [37] 46.316840 45.355470 44.395603 43.438042 42.482661 41.529761 40.580458 39.635378 38.695096 37.760141 36.829839 35.903547
 [49] 34.981014 34.062706 33.149390 32.241083 31.337783 30.438818 29.544171 28.654112 27.767971 26.885098 26.005431 25.130260
 [61] 24.261014 23.398455 22.542492 21.694218 20.852465 20.016561 19.185613 18.360239 17.545293 16.745572 15.964476 15.201196
 [73] 14.453521 13.720427 13.003501 12.306578 11.631151 10.975841 10.340883  9.726911  9.135638  8.569435  8.028824  7.511682
 [85]  7.018105  6.551222  6.109259  5.691213  5.295601  4.920692  4.564365  4.223804  3.895794  3.575654  3.257486  2.933053
 [97]  2.589641  2.210182  1.767379  1.219519        NA
R > fix(e0x)
R > cbind(lifetable.kr$age,sapply(0:100,e)x,lx=lifetable.kr$lx)
에러: 예기치 않은 심볼입니다 in "cbind(lifetable.kr$age,sapply(0:100,e)x"
R > cbind(lifetable.kr$age,sapply(0:100,e0x,lx=lifetable.kr$lx)
+ )
       [,1]      [,2]
  [1,]    0 81.392440
  [2,]    1 80.628524
  [3,]    2 79.653444
  [4,]    3 78.670918
  [5,]    4 77.684255
  [6,]    5 76.694327
  [7,]    6 75.703506
  [8,]    7 74.712566
  [9,]    8 73.721508
 [10,]    9 72.728861
 [11,]   10 71.736115
 [12,]   11 70.742555
 [13,]   12 69.748905
 [14,]   13 68.755862
 [15,]   14 67.763405
 [16,]   15 66.772868
 [17,]   16 65.785523
 [18,]   17 64.800618
 [19,]   18 63.817429
 [20,]   19 62.835898
 [21,]   20 61.855341
 [22,]   21 60.875720
 [23,]   22 59.897604
 [24,]   23 58.920338
 [25,]   24 57.945062
 [26,]   25 56.970543
 [27,]   26 55.997312
 [28,]   27 55.024192
 [29,]   28 54.051716
 [30,]   29 53.080385
 [31,]   30 52.109614
 [32,]   31 51.140929
 [33,]   32 50.173727
 [34,]   33 49.208435
 [35,]   34 48.243975
 [36,]   35 47.279806
 [37,]   36 46.316840
 [38,]   37 45.355470
 [39,]   38 44.395603
 [40,]   39 43.438042
 [41,]   40 42.482661
 [42,]   41 41.529761
 [43,]   42 40.580458
 [44,]   43 39.635378
 [45,]   44 38.695096
 [46,]   45 37.760141
 [47,]   46 36.829839
 [48,]   47 35.903547
 [49,]   48 34.981014
 [50,]   49 34.062706
 [51,]   50 33.149390
 [52,]   51 32.241083
 [53,]   52 31.337783
 [54,]   53 30.438818
 [55,]   54 29.544171
 [56,]   55 28.654112
 [57,]   56 27.767971
 [58,]   57 26.885098
 [59,]   58 26.005431
 [60,]   59 25.130260
 [61,]   60 24.261014
 [62,]   61 23.398455
 [63,]   62 22.542492
 [64,]   63 21.694218
 [65,]   64 20.852465
 [66,]   65 20.016561
 [67,]   66 19.185613
 [68,]   67 18.360239
 [69,]   68 17.545293
 [70,]   69 16.745572
 [71,]   70 15.964476
 [72,]   71 15.201196
 [73,]   72 14.453521
 [74,]   73 13.720427
 [75,]   74 13.003501
 [76,]   75 12.306578
 [77,]   76 11.631151
 [78,]   77 10.975841
 [79,]   78 10.340883
 [80,]   79  9.726911
 [81,]   80  9.135638
 [82,]   81  8.569435
 [83,]   82  8.028824
 [84,]   83  7.511682
 [85,]   84  7.018105
 [86,]   85  6.551222
 [87,]   86  6.109259
 [88,]   87  5.691213
 [89,]   88  5.295601
 [90,]   89  4.920692
 [91,]   90  4.564365
 [92,]   91  4.223804
 [93,]   92  3.895794
 [94,]   93  3.575654
 [95,]   94  3.257486
 [96,]   95  2.933053
 [97,]   96  2.589641
 [98,]   97  2.210182
 [99,]   98  1.767379
[100,]   99  1.219519
[101,]  100        NA
R > fix(e0x)
R > sapply(0:100,e0x,lx=lifetable.kr$lx)
           [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]    [,10]    [,11]    [,12]    [,13]
  [1,] 81.39244 80.62852 79.65344 78.67092 77.68426 76.69433 75.70351 74.71257 73.72151 72.72886 71.73612 70.74255 69.74891
  [2,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [3,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [4,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [5,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [6,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [7,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [8,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [9,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [10,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [11,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [12,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [13,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [14,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [15,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [16,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [17,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [18,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [19,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [20,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [21,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [22,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [23,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [24,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [25,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [26,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [27,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [28,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [29,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [30,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [31,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [32,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [33,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [34,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [35,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [36,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [37,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [38,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [39,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [40,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [41,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [42,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [43,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [44,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [45,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [46,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [47,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [48,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [49,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [50,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [51,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [52,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [53,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [54,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [55,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [56,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [57,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [58,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [59,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [60,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [61,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [62,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [63,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [64,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [65,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [66,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [67,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [68,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [69,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [70,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [71,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [72,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [73,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [74,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [75,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [76,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [77,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [78,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [79,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [80,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [81,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [82,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [83,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [84,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [85,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [86,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [87,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [88,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [89,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [90,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [91,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [92,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [93,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [94,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [95,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [96,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [97,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [98,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [99,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
[100,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
[101,]  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000
          [,14]    [,15]    [,16]    [,17]    [,18]    [,19]   [,20]    [,21]    [,22]   [,23]    [,24]    [,25]    [,26]
  [1,] 68.75586 67.76341 66.77287 65.78552 64.80062 63.81743 62.8359 61.85534 60.87572 59.8976 58.92034 57.94506 56.97054
  [2,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [3,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [4,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [5,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [6,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [7,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [8,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [9,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [10,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [11,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [12,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [13,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [14,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [15,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [16,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [17,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [18,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [19,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [20,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [21,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [22,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [23,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [24,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [25,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [26,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [27,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [28,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [29,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [30,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [31,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [32,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [33,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [34,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [35,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [36,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [37,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [38,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [39,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [40,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [41,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [42,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [43,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [44,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [45,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [46,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [47,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [48,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [49,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [50,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [51,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [52,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [53,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [54,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [55,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [56,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [57,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [58,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [59,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [60,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [61,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [62,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [63,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [64,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [65,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [66,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [67,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [68,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [69,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [70,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [71,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [72,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [73,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [74,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [75,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [76,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [77,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [78,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [79,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [80,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [81,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [82,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [83,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [84,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [85,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [86,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [87,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [88,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [89,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [90,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [91,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [92,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [93,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [94,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [95,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [96,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [97,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [98,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [99,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
[100,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
[101,]  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.5000  0.50000  0.50000  0.5000  0.50000  0.50000  0.50000
          [,27]    [,28]    [,29]    [,30]    [,31]    [,32]    [,33]    [,34]    [,35]    [,36]    [,37]    [,38]   [,39]
  [1,] 55.99731 55.02419 54.05172 53.08039 52.10961 51.14093 50.17373 49.20844 48.24398 47.27981 46.31684 45.35547 44.3956
  [2,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
  [3,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
  [4,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
  [5,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
  [6,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
  [7,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
  [8,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
  [9,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [10,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [11,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [12,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [13,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [14,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [15,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [16,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [17,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [18,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [19,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [20,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [21,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [22,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [23,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [24,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [25,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [26,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [27,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [28,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [29,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [30,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [31,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [32,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [33,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [34,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [35,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [36,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [37,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [38,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [39,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [40,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [41,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [42,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [43,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [44,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [45,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [46,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [47,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [48,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [49,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [50,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [51,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [52,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [53,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [54,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [55,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [56,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [57,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [58,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [59,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [60,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [61,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [62,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [63,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [64,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [65,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [66,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [67,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [68,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [69,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [70,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [71,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [72,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [73,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [74,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [75,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [76,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [77,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [78,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [79,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [80,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [81,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [82,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [83,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [84,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [85,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [86,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [87,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [88,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [89,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [90,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [91,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [92,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [93,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [94,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [95,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [96,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [97,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [98,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
 [99,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
[100,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA      NA
[101,]  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.5000
          [,40]    [,41]    [,42]    [,43]    [,44]   [,45]    [,46]    [,47]    [,48]    [,49]    [,50]    [,51]    [,52]
  [1,] 43.43804 42.48266 41.52976 40.58046 39.63538 38.6951 37.76014 36.82984 35.90355 34.98101 34.06271 33.14939 32.24108
  [2,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [3,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [4,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [5,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [6,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [7,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [8,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [9,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [10,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [11,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [12,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [13,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [14,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [15,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [16,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [17,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [18,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [19,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [20,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [21,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [22,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [23,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [24,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [25,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [26,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [27,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [28,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [29,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [30,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [31,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [32,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [33,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [34,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [35,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [36,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [37,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [38,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [39,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [40,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [41,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [42,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [43,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [44,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [45,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [46,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [47,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [48,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [49,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [50,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [51,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [52,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [53,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [54,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [55,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [56,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [57,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [58,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [59,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [60,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [61,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [62,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [63,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [64,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [65,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [66,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [67,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [68,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [69,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [70,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [71,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [72,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [73,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [74,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [75,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [76,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [77,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [78,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [79,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [80,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [81,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [82,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [83,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [84,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [85,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [86,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [87,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [88,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [89,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [90,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [91,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [92,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [93,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [94,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [95,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [96,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [97,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [98,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [99,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
[100,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
[101,]  0.50000  0.50000  0.50000  0.50000  0.50000  0.5000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000
          [,53]    [,54]    [,55]    [,56]    [,57]   [,58]    [,59]    [,60]    [,61]    [,62]    [,63]    [,64]    [,65]
  [1,] 31.33778 30.43882 29.54417 28.65411 27.76797 26.8851 26.00543 25.13026 24.26101 23.39845 22.54249 21.69422 20.85246
  [2,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [3,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [4,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [5,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [6,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [7,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [8,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
  [9,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [10,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [11,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [12,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [13,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [14,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [15,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [16,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [17,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [18,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [19,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [20,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [21,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [22,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [23,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [24,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [25,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [26,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [27,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [28,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [29,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [30,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [31,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [32,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [33,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [34,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [35,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [36,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [37,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [38,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [39,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [40,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [41,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [42,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [43,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [44,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [45,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [46,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [47,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [48,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [49,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [50,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [51,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [52,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [53,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [54,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [55,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [56,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [57,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [58,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [59,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [60,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [61,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [62,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [63,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [64,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [65,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [66,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [67,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [68,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [69,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [70,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [71,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [72,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [73,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [74,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [75,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [76,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [77,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [78,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [79,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [80,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [81,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [82,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [83,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [84,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [85,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [86,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [87,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [88,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [89,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [90,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [91,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [92,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [93,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [94,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [95,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [96,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [97,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [98,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
 [99,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
[100,]       NA       NA       NA       NA       NA      NA       NA       NA       NA       NA       NA       NA       NA
[101,]  0.50000  0.50000  0.50000  0.50000  0.50000  0.5000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000
          [,66]    [,67]    [,68]    [,69]    [,70]    [,71]   [,72]    [,73]    [,74]   [,75]    [,76]    [,77]    [,78]
  [1,] 20.01656 19.18561 18.36024 17.54529 16.74557 15.96448 15.2012 14.45352 13.72043 13.0035 12.30658 11.63115 10.97584
  [2,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [3,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [4,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [5,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [6,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [7,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [8,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
  [9,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [10,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [11,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [12,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [13,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [14,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [15,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [16,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [17,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [18,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [19,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [20,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [21,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [22,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [23,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [24,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [25,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [26,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [27,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [28,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [29,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [30,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [31,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [32,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [33,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [34,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [35,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [36,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [37,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [38,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [39,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [40,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [41,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [42,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [43,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [44,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [45,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [46,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [47,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [48,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [49,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [50,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [51,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [52,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [53,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [54,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [55,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [56,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [57,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [58,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [59,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [60,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [61,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [62,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [63,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [64,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [65,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [66,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [67,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [68,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [69,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [70,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [71,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [72,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [73,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [74,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [75,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [76,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [77,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [78,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [79,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [80,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [81,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [82,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [83,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [84,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [85,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [86,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [87,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [88,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [89,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [90,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [91,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [92,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [93,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [94,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [95,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [96,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [97,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [98,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
 [99,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
[100,]       NA       NA       NA       NA       NA       NA      NA       NA       NA      NA       NA       NA       NA
[101,]  0.50000  0.50000  0.50000  0.50000  0.50000  0.50000  0.5000  0.50000  0.50000  0.5000  0.50000  0.50000  0.50000
          [,79]    [,80]    [,81]    [,82]    [,83]    [,84]    [,85]    [,86]    [,87]    [,88]    [,89]    [,90]    [,91]
  [1,] 10.34088 9.726911 9.135638 8.569435 8.028824 7.511682 7.018105 6.551222 6.109259 5.691213 5.295601 4.920692 4.564365
  [2,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [3,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [4,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [5,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [6,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [7,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [8,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
  [9,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [10,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [11,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [12,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [13,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [14,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [15,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [16,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [17,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [18,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [19,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [20,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [21,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [22,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [23,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [24,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [25,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [26,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [27,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [28,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [29,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [30,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [31,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [32,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [33,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [34,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [35,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [36,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [37,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [38,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [39,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [40,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [41,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [42,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [43,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [44,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [45,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [46,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [47,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [48,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [49,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [50,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [51,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [52,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [53,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [54,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [55,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [56,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [57,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [58,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [59,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [60,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [61,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [62,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [63,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [64,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [65,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [66,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [67,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [68,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [69,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [70,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [71,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [72,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [73,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [74,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [75,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [76,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [77,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [78,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [79,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [80,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [81,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [82,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [83,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [84,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [85,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [86,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [87,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [88,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [89,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [90,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [91,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [92,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [93,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [94,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [95,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [96,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [97,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [98,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
 [99,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
[100,]       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA       NA
[101,]  0.50000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000
          [,92]    [,93]    [,94]    [,95]    [,96]    [,97]    [,98]    [,99]   [,100] [,101]
  [1,] 4.223804 3.895794 3.575654 3.257486 2.933053 2.589641 2.210182 1.767379 1.219519     NA
  [2,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
  [3,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
  [4,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
  [5,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
  [6,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
  [7,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
  [8,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
  [9,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [10,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [11,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [12,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [13,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [14,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [15,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [16,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [17,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [18,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [19,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [20,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [21,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [22,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [23,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [24,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [25,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [26,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [27,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [28,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [29,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [30,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [31,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [32,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [33,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [34,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [35,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [36,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [37,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [38,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [39,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [40,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [41,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [42,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [43,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [44,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [45,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [46,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [47,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [48,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [49,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [50,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [51,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [52,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [53,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [54,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [55,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [56,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [57,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [58,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [59,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [60,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [61,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [62,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [63,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [64,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [65,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [66,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [67,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [68,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [69,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [70,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [71,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [72,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [73,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [74,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [75,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [76,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [77,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [78,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [79,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [80,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [81,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [82,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [83,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [84,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [85,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [86,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [87,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [88,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [89,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [90,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [91,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [92,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [93,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [94,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [95,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [96,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [97,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [98,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
 [99,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
[100,]       NA       NA       NA       NA       NA       NA       NA       NA       NA     NA
[101,] 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000 0.500000    0.5
R > fix(e0x)
R > sapply(0:100,e0x,lx=lifetable.kr$lx)
  [1] 81.392440 80.628524 79.653444 78.670918 77.684255 76.694327 75.703506 74.712566 73.721508 72.728861 71.736115 70.742555
 [13] 69.748905 68.755862 67.763405 66.772868 65.785523 64.800618 63.817429 62.835898 61.855341 60.875720 59.897604 58.920338
 [25] 57.945062 56.970543 55.997312 55.024192 54.051716 53.080385 52.109614 51.140929 50.173727 49.208435 48.243975 47.279806
 [37] 46.316840 45.355470 44.395603 43.438042 42.482661 41.529761 40.580458 39.635378 38.695096 37.760141 36.829839 35.903547
 [49] 34.981014 34.062706 33.149390 32.241083 31.337783 30.438818 29.544171 28.654112 27.767971 26.885098 26.005431 25.130260
 [61] 24.261014 23.398455 22.542492 21.694218 20.852465 20.016561 19.185613 18.360239 17.545293 16.745572 15.964476 15.201196
 [73] 14.453521 13.720427 13.003501 12.306578 11.631151 10.975841 10.340883  9.726911  9.135638  8.569435  8.028824  7.511682
 [85]  7.018105  6.551222  6.109259  5.691213  5.295601  4.920692  4.564365  4.223804  3.895794  3.575654  3.257486  2.933053
 [97]  2.589641  2.210182  1.767379  1.219519        NA
R > e0x.o<-sapply(0:100,e0x,lx=lifetable.kr$lx)
R > 