"""Tiles the dashboard's Gene -> phenotypes plot for each gene of interest
(plot_gene_*.png from R/06_dashboard_screenshots.R) into one labelled figure:
results/figures/dashboard/Figure_dashboard_genes.png"""
from PIL import Image, ImageDraw, ImageFont
import os
D = "results/figures/dashboard"
genes = ["Tspyl2", "Ifng", "Lpcat2b", "Myof", "Cog2"]
ims = [Image.open(os.path.join(D, f"plot_gene_{g}.png")).convert("RGB") for g in genes]
W = max(i.size[0] for i in ims); pad = 30; head = 90
fb = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial Bold.ttf", 58)
fi = ImageFont.truetype("/System/Library/Fonts/Supplemental/Arial Bold Italic.ttf", 50)
# two columns: left = Tspyl2, Lpcat2b, Cog2 ; right = Ifng, Myof
cols = [[0, 2, 4], [1, 3]]
col_h = [sum(ims[i].size[1] + head + pad for i in c) for c in cols]
canvas = Image.new("RGB", (2 * W + 3 * pad, max(col_h) + pad), "white")
d = ImageDraw.Draw(canvas)
for ci, c in enumerate(cols):
    y = pad
    for i in c:
        x = pad + ci * (W + pad)
        d.text((x, y + 8), "ABCDE"[i], fill="black", font=fb)
        d.text((x + 75, y + 14), genes[i], fill="black", font=fi)
        canvas.paste(ims[i], (x, y + head))
        y += ims[i].size[1] + head + pad
canvas.save(os.path.join(D, "Figure_dashboard_genes.png"), dpi=(300, 300))
print(canvas.size)
