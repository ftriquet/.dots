#!/bin/bash
pgrep trayer && exit 0
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --height 19 --transparent true --widthtype percent --tint 0x1D2021 --alpha 0
