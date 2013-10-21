LOC=../../NOBUILD/bin/pbo
HFB=${LOC}/hfbsm
BOT=${LOC}/bottle.py
BOTMRG=${LOC}/bottle_merge.py

all:
	make -i hfbsm
	make -i bottle.py
	make -i bottle_merge.py

hfbsm: $(HFB)
	cp $(HFB) .

bottle.py: $(BOT)
	cp $(BOT) .

bottle_merge.py: $(BOTMRG)
	cp $(BOTMRG) .
