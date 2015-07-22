CC = elm make
PACKAGE = elm package
INSTALL = install
SRC = src
OPEN = open

PACKAGE_FLAGS = -y

BUILD_DIR = build

all: package/install clean compile

Index.html = index.html
open: all $(Index.html)
	$(OPEN) $(Index.html)

compile: bingo

Bingo = Bingo
Bingo.js = $(BUILD_DIR)/$(Bingo).js
Bingo.elm = $(SRC)/$(Bingo).elm
bingo: $(Bingo.js)
$(Bingo.js): $(Bingo.elm)
	$(CC) --output $(Bingo.js) $(Bingo.elm)

clean:
	rm -rf $(BUILD_DIR)

package/install:
	$(PACKAGE) $(INSTALL) $(PACKAGE_FLAGS)