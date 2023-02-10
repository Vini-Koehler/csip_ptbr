﻿* Encoding: UTF-8.

CORRELATIONS
  /VARIABLES=CSIPPA.2 CSIPBC.2 CSIPDE.2 CSIPFG.2 CSIPHI.2 CSIPJK.2 CSIPLM.2 CSIPNO.2 CSIPTOT.2 WITH
    CSIPPA.1 CSIPBC.1 CSIPDE.1 CSIPFG.1 CSIPHI.1 CSIPJK.1 CSIPLM.1 CSIPNO.1 CSIPTOT.1
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CSIPPA.ip.2 CSIPBC.ip.2 CSIPDE.ip.2 CSIPFG.ip.2 CSIPHI.ip.2 CSIPJK.ip.2 CSIPLM.ip.2 CSIPNO.ip.2 WITH
    CSIPPA.ip.1 CSIPBC.ip.1 CSIPDE.ip.1 CSIPFG.ip.1 CSIPHI.ip.1 CSIPJK.ip.1 CSIPLM.ip.1 CSIPNO.ip.1
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

RELIABILITY
  /VARIABLES=CSIP1 CSIP9 CSIP17 CSIP25 CSIP33 CSIP41 CSIP49 CSIP57
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=CSIP2 CSIP10 CSIP18 CSIP26 CSIP34 CSIP42 CSIP50 CSIP58
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=CSIP3 CSIP11 CSIP19 CSIP27 CSIP35 CSIP43 CSIP51 CSIP59
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=CSIP4 CSIP12 CSIP20 CSIP28 CSIP36 CSIP44 CSIP52 CSIP60
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=CSIP5 CSIP13 CSIP21 CSIP29 CSIP37 CSIP45 CSIP53 CSIP61
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=CSIP6 CSIP14 CSIP22 CSIP30 CSIP38 CSIP46 CSIP54 CSIP62
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=CSIP7 CSIP15 CSIP23 CSIP31 CSIP39 CSIP47 CSIP55 CSIP63
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

RELIABILITY
  /VARIABLES=CSIP8 CSIP16 CSIP24 CSIP32 CSIP40 CSIP48 CSIP56 CSIP64
  /SCALE('ALL VARIABLES') ALL
  /MODEL=ALPHA
  /STATISTICS=DESCRIPTIVE SCALE CORR
  /SUMMARY=TOTAL.

*8ths.
CORRELATIONS
  /VARIABLES=CLOITPA CLOITBC CLOITDE CLOITFG CLOITHI CLOITJK CLOITLM CLOITNO CLOITTOT WITH 
    CSIPPA CSIPBC CSIPDE CSIPFG CSIPHI CSIPJK CSIPLM CSIPNO CSIPTOT
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITPA.ip CLOITBC.ip CLOITDE.ip CLOITFG.ip CLOITHI.ip CLOITJK.ip CLOITLM.ip CLOITNO.ip WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITPA CLOITBC CLOITDE CLOITFG CLOITHI CLOITJK CLOITLM CLOITNO WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*8ths (Alternate).
CORRELATIONS
  /VARIABLES=CLOITAB CLOITCD CLOITEF CLOITGH CLOITIJ CLOITKL CLOITMN CLOITOP CLOITTOT WITH 
    CSIPPA CSIPBC CSIPDE CSIPFG CSIPHI CSIPJK CSIPLM CSIPNO CSIPTOT
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITAB.ip CLOITCD.ip CLOITEF.ip CLOITGH.ip CLOITIJ.ip CLOITKL.ip CLOITMN.ip CLOITOP.ip WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITAB CLOITCD CLOITEF CLOITGH CLOITIJ CLOITKL CLOITMN CLOITOP WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*16ths. 
CORRELATIONS
  /VARIABLES=CLOITA CLOITD CLOITG CLOITJ CLOITM CLOITP CLOITC CLOITF CLOITI CLOITL CLOITO CLOITB CLOITE CLOITH CLOITK CLOITN CLOITTOT WITH
    CSIPPA CSIPBC CSIPDE CSIPFG CSIPHI CSIPJK CSIPLM CSIPNO CSIPTOT
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITA.ip CLOITD.ip CLOITG.ip CLOITJ.ip CLOITM.ip CLOITP.ip CLOITC.ip CLOITF.ip CLOITI.ip CLOITL.ip CLOITO.ip CLOITB.ip CLOITE.ip CLOITH.ip CLOITK.ip CLOITN.ip WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITA CLOITD CLOITG CLOITJ CLOITM CLOITP CLOITC CLOITF CLOITI CLOITL CLOITO CLOITB CLOITE CLOITH CLOITK CLOITN CLOITTOT WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Quadrants.
CORRELATIONS
  /VARIABLES=CLOITHD CLOITHS CLOITFS CLOITFD WITH 
    CSIPPA CSIPBC CSIPDE CSIPFG CSIPHI CSIPJK CSIPLM CSIPNO CSIPTOT
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITHD.ip CLOITHS.ip CLOITFS.ip CLOITFD.ip WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITHD CLOITHS CLOITFS CLOITFD WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Hemispheres.
CORRELATIONS
  /VARIABLES=CLOITDOM CLOITSUB CLOITFRI CLOITHOS WITH 
    CSIPPA CSIPBC CSIPDE CSIPFG CSIPHI CSIPJK CSIPLM CSIPNO CSIPTOT
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITDOM.ip CLOITSUB.ip CLOITFRI.ip CLOITHOS.ip WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITDOM CLOITSUB CLOITFRI CLOITHOS WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

*Axes.
CORRELATIONS
  /VARIABLES=CLOITCONTROL CLOITAFFILIA WITH 
    CSIPPA CSIPBC CSIPDE CSIPFG CSIPHI CSIPJK CSIPLM CSIPNO CSIPTOT
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITCONTROL.ip CLOITAFFILIA.ip WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.

CORRELATIONS
  /VARIABLES=CLOITCONTROL CLOITAFFILIA WITH
    CSIPPA.ip CSIPBC.ip CSIPDE.ip CSIPFG.ip CSIPHI.ip CSIPJK.ip CSIPLM.ip CSIPNO.ip
  /PRINT=TWOTAIL NOSIG
  /MISSING=PAIRWISE.