"""Figure for the composite-verdict post.

Eight Washington public four-year universities, ranked under five blends of
two real 2021 IPEDS numbers: six-year graduation rate (bachelor's-seeking
cohort, 150% time) and the share of undergraduates receiving Pell grants.

Data: wa_publics_2021.csv in this folder, pulled from the Urban Institute
Education Data API (IPEDS grad-rates subcohort=2 and sfa-all-undergraduates
type_of_aid=5). Delete the CSV and rerun to re-fetch from the API.
"""

import csv
import json
import urllib.request
from pathlib import Path

import matplotlib.pyplot as plt
import numpy as np

HERE = Path(__file__).parent
CSV = HERE / "wa_publics_2021.csv"

INST = {
    236948: "UW Seattle", 377555: "UW Bothell", 377564: "UW Tacoma",
    236939: "Washington State", 237011: "Western Washington",
    234827: "Central Washington", 235097: "Eastern Washington",
    235167: "Evergreen State",
}


def fetch():
    base = "https://educationdata.urban.org/api/v1/college-university/ipeds"

    def get(url):
        with urllib.request.urlopen(url, timeout=60) as r:
            return json.load(r)

    rows = []
    for uid, name in INST.items():
        g = get(f"{base}/grad-rates/2021/?unitid={uid}&race=99&sex=99")
        grad = next(r["completion_rate_150pct"] for r in g["results"]
                    if r["subcohort"] == 2)
        s = get(f"{base}/sfa-all-undergraduates/2021/?unitid={uid}")
        pell = next(r["percent_of_students"] for r in s["results"]
                    if r["type_of_aid"] == 5)
        rows.append((uid, name, grad, pell))
    with open(CSV, "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["unitid", "institution",
                    "grad_rate_150_bach_2021", "pell_share_undergrad_2021"])
        w.writerows(rows)
    return rows


if CSV.exists():
    with open(CSV) as f:
        rows = [(int(r["unitid"]), r["institution"],
                 float(r["grad_rate_150_bach_2021"]),
                 float(r["pell_share_undergrad_2021"]))
                for r in csv.DictReader(f)]
else:
    rows = fetch()

names = [r[1] for r in rows]
grad = np.array([r[2] for r in rows])
pell = np.array([r[3] for r in rows])

# scale each measure 0-1 across the eight schools (a choice in itself; see post)
gn = (grad - grad.min()) / (grad.max() - grad.min())
pn = (pell - pell.min()) / (pell.max() - pell.min())

weights = [1.0, 0.75, 0.5, 0.25, 0.0]  # weight on graduation rate
ranks = np.zeros((len(rows), len(weights)), dtype=int)
for j, w in enumerate(weights):
    composite = w * gn + (1 - w) * pn
    ranks[:, j] = (-composite).argsort().argsort() + 1

moved = np.abs(ranks[:, 0] - ranks[:, -1])
print("positions moved between the two pure rankings:",
      dict(zip(names, moved)))

fig, ax = plt.subplots(figsize=(8, 5.2))
x = range(len(weights))

for i, name in enumerate(names):
    big = moved[i] >= 4
    color = "#d95f02" if big else "#8a8a8a"
    lw = 2.4 if big else 1.4
    ax.plot(x, ranks[i], color=color, lw=lw, marker="o", ms=4,
            zorder=3 if big else 2)
    ax.text(-0.12, ranks[i, 0], name, ha="right", va="center",
            fontsize=9, color=color)
    ax.text(len(weights) - 1 + 0.12, ranks[i, -1], name, ha="left",
            va="center", fontsize=9, color=color)

ax.set_xticks(list(x))
ax.set_xticklabels(["100 / 0", "75 / 25", "50 / 50", "25 / 75", "0 / 100"])
ax.set_xlabel("Weight on graduation rate / weight on Pell share")
ax.set_yticks([])
ax.text(-0.12, 0.55, "1 = top", ha="right", va="center", fontsize=8,
        color="#8a8a8a", style="italic")
ax.set_ylim(len(rows) + 0.6, 0.4)
ax.set_title(
    "Eight Washington public universities, same 2021 federal data.\n"
    "Rank under five blends of graduation rate and Pell share.",
    loc="left", fontsize=11,
)
ax.spines[["top", "right", "left"]].set_visible(False)

fig.tight_layout()
out = HERE / "fig-bump.png"
fig.savefig(out, dpi=200)
print(f"wrote {out}")
