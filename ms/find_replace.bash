#!/bin/sh

sed -i '' 's/m\_l/m\_\\textrm\{l\}/g' MS.md
sed -i '' 's/m\_r/m\_\\textrm\{r\}/g' MS.md
sed -i '' 's/m\_t/m\_\\textrm\{t\}/g' MS.md
sed -i '' 's/m\_s/m\_\\textrm\{s\}/g' MS.md
sed -i '' 's/m\_b/m\_\\textrm\{b\}/g' MS.md

sed -i '' 's/a\_l/a\_\\textrm\{l\}/g' MS.md
sed -i '' 's/a\_{ss}/a\_\\textrm\{ss\}/g' MS.md
sed -i '' 's/a\_{sb}/a\_\\textrm\{sb\}/g' MS.md
sed -i '' 's/a\_{sh}/a\_\\textrm\{sh\}/g' MS.md

sed -i '' 's/k\_l/k\_\\textrm\{l\}/g' MS.md
sed -i '' 's/k\_r/k\_\\textrm\{r\}/g' MS.md
sed -i '' 's/k\_s/k\_\\textrm\{s\}/g' MS.md
sed -i '' 's/k\_b/k\_\\textrm\{b\}/g' MS.md

sed -i '' 's/r\_l/r\_\\textrm\{l\}/g' MS.md
sed -i '' 's/r\_r/r\_\\textrm\{r\}/g' MS.md
sed -i '' 's/r\_s/r\_\\textrm\{s\}/g' MS.md
sed -i '' 's/r\_b/r\_\\textrm\{b\}/g' MS.md

#sed 's/FindThisWord/ReplaceWithThisWord/g' file.txt
