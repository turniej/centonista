# Szablony wierszy pisanych przez centonistę.
#
# Znaczenie znaków:
#   '.' - sylaba nieakcentowana;
#   '*' - sylaba akcentowana;
#   '?' - dowolna sylaba;
#   '|' - średniówka, czyli wymagana granica zestrojów akcentowych;
#   '||' - wymagana granica zdania, np. na końcu zwrotki;
#   '|[a-z]', '||[a-z]' - rym.
#
# Różnica między średniówką a nieużywanym gdzie indziej rymem,
# czyli między '|' a '|x', jest subtelna: kropka lub przecinek
# może leżeć w miejscu rymu, a nie może w miejscu średniówki.
#
# Używanie w szablonie znaku '?' powoduje, że program działa dłużej,
# niż przy użyciu znaków '.' i '*'. Czas działania programu wydłuża
# się też, gdy rośnie liczba sylab w wersach i liczba różnych szablonów
# akcentowania wersów.
#
# Każdy szablon powinien być poprzedzony swoją nazwą. Komentarze,
# jak widać, rozciągają się od znaku kratki (#) do końca wiersza.


Czaty, Mickiewicz (anapesty)

..*..*.|a..*..*.|a
*.*.|.*..*.|b
..*..*.|c..*..*.|c
*.*.|.*..*.||b

..*..*.|d..*..*.|d
*.*.|.*..*.|e
..*..*.|f..*..*.|f
*.*.|.*..*.||e

..*..*.|g..*..*.|g
*.*.|.*..*.|h
..*..*.|i..*..*.|i
*.*.|.*..*.||h


Świtezianka, Mickiewicz (strofa stanisławowska)

*..*.|*..*.|a
*..*.|.*.|b
*..*.|*..*.|a
*..*.|.*.||b

*..*.|*..*.|c
*..*.|.*.|d
*..*.|*..*.|c
*..*.|.*.||d

*..*.|*..*.|e
*..*.|.*.|f
*..*.|*..*.|e
*..*.|.*.||f


Duma o Wacławie Rzewuskim, Słowacki (amfibrachy)

.*..*.|.*..*.|a
.*..*.|.*..*.|a
.*..*.|b.*..*.|b
.*..*..*.||c

.*..*.|.*..*.|d
.*..*.|.*..*.|d
.*..*.|e.*..*.|e
.*..*..*.||c

.*..*.|.*..*.|f
.*..*.|.*..*.|f
.*..*.|g.*..*.|g
.*..*..*.||h

.*..*.|.*..*.|i
.*..*.|.*..*.|i
.*..*.|j.*..*.|j
.*..*..*.||h


Posępny, ciężki los..., Przesmycki (jamby)

.*.*.*|a.*.?.*.|b
.*.*.*|a.*.?.*.|b
.*.*.*|c.*.?.*.|d
.*.*.*|c.*.?.*.||d

.*.*.*|e.*.?.*.|f
.*.*.*|e.*.?.*.|f
.*.*.*|g.*.?.*.|h
.*.*.*|g.*.?.*.||h

.*.*.*|i.*.?.*.|j
.*.*.*|i.*.?.*.|j
.*.*.*|k.*.?.*.|l
.*.*.*|k.*.?.*.||l


Pozdrowienie, Przerwa-Tetmajer

*..*.*.*|a
  .*.*.*|b
*..*.*.*|a
  .*.*.*||b

*..*.*.*|c
  .*.*.*|d
*..*.*.*|c
  .*.*.*||d

*..*.*.*|e
  .*.*.*|f
*..*.*.*|e
  .*.*.*||f


Grande Valse Brillante, Tuwim

..*..*.|a..*..*.|a
  ..*..*..*|b
..*..*.|c..*..*.|c
  ..*..*..*||b

..*..*.|d..*..*.|d
  ..*..*..*|e
..*..*.|f..*..*.|f
  ..*..*..*||e

..*..*.|g..*..*.|g
  ..*..*..*|h
..*..*.|i..*..*.|i
  ..*..*..*||h
