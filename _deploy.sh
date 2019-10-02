#!/bin/bash

# Tree of the directories.
echo "------------------------------------------------------------------------"
echo "Files to be sent to server.\n"

tree -h -F docs/ -L 2

# Upload.
echo "------------------------------------------------------------------------"
echo "Uploading files to server.\n"

# ssh "-p$PATAXOP" "$USER@$PATAXO" \
#     "mkdir -p ~/public_html/pacotes/ClickMetrics"
# firefox http://leg.ufpr.br/~walmes/pacotes/ClickMetrics

rsync -avzp \
      ./docs/ \
      --progress \
      --rsh="ssh -p$PATAXOP" \
      "$USER@$PATAXO:~/public_html/pacotes/ClickMetrics"

# Vist the homepage.
echo "------------------------------------------------------------------------"
echo "Visiting the webpage.\n"

firefox http://leg.ufpr.br/~walmes/pacotes/ClickMetrics

exit 0
