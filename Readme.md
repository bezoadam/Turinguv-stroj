## Turingův stroj

### Autor: Adam Bezák xbezak01@stud.fit.vutbr.cz

### Program:

##### Simulátor nedeterministického Turingova stroje

Na vstupe program obdrží pravidla pre TS a vstupnú pásku. Výstup programu je postupnosť konfigurácií stroja.
Výpočet stroja končí prechodom do koncového stavu alebo abnormánym zastavením.
Pravidla sú v tvare **\<stav> \<symbol na paske> \<nový stav> \<nový symbol alebo `L`, `R`>**. Jednolivé časti sú oddelené medzerou, každé pravidlo na samostatnom riadku.
Symboly `L`/`R` značia posun hlavy dolava/doprava. Na poslednom riadku vstupu je uvedený vstupný obsah pásky.

##### Testovacie vstupy

- vstup1.txt - referenčný vstup zo zadania - 0.049s 
- vstup3.txt - posun vpravo, vlavo - 0.036s 
- vstup4.txt - posun vpravo, posun vlavo za hranicu pasky, backtracking - 0.033s  
- vstup5.txt - posun vpravo za hranicu pasky, backtracking - 0.046s 
- vstup6.txt - backtracking, neexistujuce pravidlo - 0.037s  

##### Nedostatky projektu

- vstup2.txt - Zacyklenie vstupu - Saaacaa -> Baaacaa -> Bcaacaa -> Baaacaa -> Bcaacaa ... 
 
## 6/8
Komentář učitele k hodnocení:
Zlyhavaju aj deterministicke TS. Na nedeterministickych casto dochadza stack alebo to cykli.
