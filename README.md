# genapp_apps

GenApp applications from the GenApp svn repository, imported with their full history at svn
revision 1767 (2026-02-24). The GenApp cores are in
[ehb54/genapp](https://github.com/ehb54/genapp) (the svn-era core is the branch `genappalpha-php8`).

Each application is its own branch, `app/<name>`, with the application's files at the branch root.
So each site directory is its own clone of one application, under any name:

    git clone --single-branch --branch app/tutorial2 https://github.com/ehb54/genapp_apps.git /opt/genapp/tutorial2

The tag `svn-r1767/<name>` marks each application as it was in svn. Commit authors are the svn
user names; their e-mail addresses link them to GitHub accounts where those are known. Every commit
message ends with `svn-revision: N`, the svn revision it came from. For a checkout at svn revision N,
the matching commit is the newest one on the branch with a revision of N or less.

| application | branch | commits | svn revisions | last change |
|---|---|---|---|---|
| bayesapp | `app/bayesapp` | 26 | r962 – r1748 | 2022-06-21 |
| demo | `app/demo` | 11 | r751 – r1424 | 2018-02-15 |
| denfert | `app/denfert` | 3 | r534 – r879 | 2016-06-21 |
| devutil | `app/devutil` | 15 | r1126 – r1313 | 2017-03-28 |
| genapptest | `app/genapptest` | 60 | r3 – r1744 | 2021-08-08 |
| mulch | `app/mulch` | 18 | r1071 – r1121 | 2016-09-07 |
| multisaxshub | `app/multisaxshub` | 13 | r1575 – r1687 | 2019-03-28 |
| nadeemtest | `app/nadeemtest` | 3 | r116 – r118 | 2014-03-20 |
| namdrunner | `app/namdrunner` | 34 | r1113 – r1294 | 2017-03-23 |
| parammd | `app/parammd` | 41 | r830 – r1625 | 2018-12-14 |
| pearc19 | `app/pearc19` | 3 | r1683 – r1688 | 2019-03-29 |
| pepsisaxs | `app/pepsisaxs` | 5 | r1446 – r1749 | 2022-06-23 |
| quafit | `app/quafit` | 84 | r611 – r1324 | 2017-05-24 |
| rotdif | `app/rotdif` | 50 | r1471 – r1707 | 2019-08-15 |
| sassie2 | `app/sassie2` | 254 | r2 – r1665 | 2019-03-05 |
| sassie2test | `app/sassie2test` | 12 | r617 – r879 | 2016-06-21 |
| sassie_1_5 | `app/sassie_1_5` | 10 | r1331 – r1370 | 2017-09-13 |
| somo | `app/somo` | 22 | r581 – r1240 | 2017-01-26 |
| somoapi | `app/somoapi` | 9 | r1508 – r1740 | 2021-05-28 |
| tutorial2 | `app/tutorial2` | 16 | r1562 – r1763 | 2024-05-19 |
| vortexshedding | `app/vortexshedding` | 15 | r533 – r1747 | 2022-06-21 |
| vortexsheddingalpha | `app/vortexsheddingalpha` | 15 | r613 – r1052 | 2016-07-27 |
| willitfit | `app/willitfit` | 54 | r656 – r1371 | 2017-09-14 |

