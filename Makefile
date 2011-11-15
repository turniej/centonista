CC = g++
CXXFLAGS = -O3 -fomit-frame-pointer -ansi
WGET = wget
TXT_FILES = http://www.wolnelektury.pl/media/packs/txt-all.zip

all: model.d rymy.d zestroje.d napisz_mi

txt-all.zip:
	$(WGET) $(TXT_FILES)

model.d rymy.d zestroje.d: txt-all.zip
	./przygotuj_mi.py file:txt-all.zip

napisz_mi: napisz_mi.cc
