#!/usr/bin/env python3
"""Simulated screening study for the jaspDiagnosticAccuracy examples.

The data are SIMULATED. They describe no real people and no real test.

- 1,000 people; exactly 100 (10%) have the condition, 900 do not.
- markerA: a fairly accurate marker. Without the condition it is normal with mean 20 and SD 5;
  with the condition, mean 28 and SD 5. True AUC = Phi(8 / (5 * sqrt(2))) = 0.871.
- markerB: a weaker marker. Without the condition, normal with mean 50 and SD 10;
  with the condition, mean 57 and SD 10. True AUC = Phi(7 / (10 * sqrt(2))) = 0.689.
- Higher values point to the condition for both markers. Values are rounded to one decimal.
- Columns: id, condition ("absent" or "present"), markerA, markerB. Rows are in random order.

Python standard library only; fixed seed, so a rerun gives the same file.
Usage: python3 simulate-screening.py OUT.csv
"""
import csv, random, sys

random.seed(20261127)
n, n_cases = 1000, 100
rows = []
for i in range(n):
    present = i < n_cases
    a = random.gauss(28 if present else 20, 5)
    b = random.gauss(57 if present else 50, 10)
    rows.append(["present" if present else "absent", round(a, 1), round(b, 1)])
random.shuffle(rows)
with open(sys.argv[1], "w", newline="") as f:
    w = csv.writer(f)
    w.writerow(["id", "condition", "markerA", "markerB"])
    for k, r in enumerate(rows, start=1):
        w.writerow([k] + r)
