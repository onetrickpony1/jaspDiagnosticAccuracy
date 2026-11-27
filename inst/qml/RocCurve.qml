//
// Copyright (C) 2026 onetrickpony1
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
//
import QtQuick
import QtQuick.Layouts
import JASP
import JASP.Controls

Form
{
	info: qsTr("The ROC curve shows how well one or more test variables (markers) separate the two classes of an outcome. An example is patients with and without a disease. The analysis gives the area under the curve (AUC) with a confidence interval. If you ask for them, it also gives more: the accuracy at a chosen cut-off, a table of every threshold, a paired comparison of the markers, and plots. The ROC curves, the AUC and the DeLong placement values come from the R package pROC; the DeLong standard error, interval and test use pROC's formulas.") + "\n\n" +
		qsTr("The AUC is the probability that the marker ranks a randomly chosen positive case above a randomly chosen negative case, in the direction you set. Ties count as one half. An AUC of 0.5 means the marker does no better than chance.")

	infoBottom:
		"## " + qsTr("Missing Values") + "\n" +
		"- " + qsTr("The AUC table, the cut-off table, the coordinates table and the plots use, for each marker, the rows where the outcome and that marker are both present. So the numbers of cases can differ between markers. A footnote in the AUC table gives the number of rows left out for each marker.") + "\n" +
		"- " + qsTr("The comparison table uses only the rows where the outcome and every marker are present. It has its own n column, and a footnote gives the number of rows left out.") + "\n" +
		"- " + qsTr("Infinite values in a marker are treated as missing for that marker, with a footnote.") + "\n\n" +
		"## " + qsTr("Output") + "\n\n" +
		"### " + qsTr("Area Under the ROC Curve") + "\n" +
		"- " + qsTr("Marker: the test variable.") + "\n" +
		"- " + qsTr("n Positive, n Negative: the numbers of positive and negative cases used for this marker.") + "\n" +
		"- " + qsTr("AUC: the area under the ROC curve.") + "\n" +
		"- " + qsTr("SE: the DeLong standard error of the AUC. The SE and the interval need at least two cases in each class. Without them the SE, the interval, z and p are left empty, with a footnote.") + "\n" +
		"- " + qsTr("Lower, Upper: the confidence interval for the AUC (DeLong or bootstrap, as chosen).") + "\n" +
		"- " + qsTr("z, p: a two-sided Wald test of AUC = 0.5 with the DeLong standard error: z = (AUC - 0.5) / SE.") + "\n\n" +
		qsTr("A marker that is constant, or that has no cases in one class, gets empty cells and a footnote. The other markers still run.") + "\n\n" +
		qsTr("When the DeLong standard error is zero, the classes do not overlap. Then z and p are not defined. They are left empty, with a footnote.") + "\n\n" +
		qsTr("With JASP's default number settings, a p value below .001 shows as < .001, in this table and in the comparison table. If you set a fixed number of decimals in JASP's preferences, the cut-off is 1 divided by 10 to the power of that number instead (0.01 with 2 decimals). A p value below the smallest positive number R can store (about 5 × 10⁻³²⁴) is stored as 0. A footnote then says so. With JASP's default number settings such a p shows as < .001 too.") + "\n\n" +
		"### " + qsTr("Cut-off") + "\n" +
		"- " + qsTr("Marker and Criterion: the test variable and how the threshold was chosen.") + "\n" +
		"- " + qsTr("Threshold: the chosen threshold. Except for the Threshold criterion, it is one of pROC's thresholds: the midpoints between neighbouring observed values, plus -∞ and ∞. See \"Threshold digits\" below.") + "\n" +
		"- " + qsTr("Sensitivity: the share of positive cases classed as positive, TP / (TP + FN), with a Clopper-Pearson confidence interval.") + "\n" +
		"- " + qsTr("Specificity: the share of negative cases classed as negative, TN / (TN + FP), with a Clopper-Pearson confidence interval.") + "\n" +
		"- " + qsTr("PPV: the positive predictive value, the probability that a case classed as positive is positive, at the chosen prevalence. It is computed from sensitivity (se), specificity (sp) and the prevalence p: se p / (se p + (1 - sp)(1 - p)).") + "\n" +
		"- " + qsTr("NPV: the negative predictive value, the probability that a case classed as negative is negative, at the chosen prevalence: sp (1 - p) / (sp (1 - p) + (1 - se) p).") + "\n" +
		"- " + qsTr("LR+: the positive likelihood ratio, se / (1 - sp).") + "\n" +
		"- " + qsTr("LR−: the negative likelihood ratio, (1 - se) / sp.") + "\n" +
		"- " + qsTr("Accuracy: the share of all cases classed correctly, (TP + TN) / n, in the sample.") + "\n" +
		"- " + qsTr("Youden's J: se + sp - 1.") + "\n\n" +
		qsTr("When several thresholds are exactly equally good, each gets its own row. This can happen with Youden index and Closest to (0, 1). Fixed sensitivity and Fixed specificity break ties, so they give one row.") + "\n\n" +
		qsTr("A value that would need a division by zero is left empty, with a footnote.") + "\n\n" +
		"### " + qsTr("Paired Comparison of AUCs") + "\n" +
		"- " + qsTr("Marker 1, Marker 2: the pair of markers, in the order they were entered.") + "\n" +
		"- " + qsTr("n: the number of rows used (the outcome and every marker present).") + "\n" +
		"- " + qsTr("Difference: the AUC of marker 1 minus the AUC of marker 2.") + "\n" +
		"- " + qsTr("Lower, Upper: the confidence interval for the difference.") + "\n" +
		"- " + qsTr("z, p: DeLong's test for two paired ROC curves. When the standard error of the difference is zero, z and p are left empty, with a footnote.") + "\n\n" +
		qsTr("The comparison needs at least two positive and two negative cases among the rows it uses. If those rows have fewer, every pair gets empty cells and a footnote. The footnote gives the numbers of positive and negative cases, and names any marker that has fewer than two cases of that class even on its own rows.") + "\n\n" +
		"### " + qsTr("Coordinates") + "\n" +
		"- " + qsTr("Marker, Threshold: every threshold of each marker, including -∞ and ∞.") + "\n" +
		"- " + qsTr("Sensitivity, Specificity, PPV, NPV: as in the cut-off table, at that threshold.") + "\n\n" +
		"### " + qsTr("Threshold digits") + "\n" +
		qsTr("In the cut-off table and in the coordinates table, all thresholds of a marker get the same number of significant digits. It is the smallest number, from 4 to 15, with which each threshold, as JASP rounds it, lies strictly between its neighbouring observed values and differs from the other shown thresholds. The cut-off table counts only its chosen thresholds, so the two tables can use different numbers. When two observed values are too close for a midpoint, pROC uses one of them as the threshold; such a threshold must show exactly. If even 15 digits are not enough, a footnote names the marker and says its thresholds are rounded for display. A threshold you enter (the Threshold criterion) is shown with all its digits (at least 4 significant digits, so 0.2 shows as 0.2000). A threshold of 1e6 or more (or -1e6 or less), entered or chosen, shows in exponent form. This applies with JASP's default number display. If you set a fixed number of decimals in JASP's preferences, JASP uses those instead.") + "\n\n" +
		"### " + qsTr("ROC Curve") + "\n" +
		"- " + qsTr("Sensitivity against 1 - specificity for every marker, with the chance diagonal as a dashed line.") + "\n" +
		"- " + qsTr("When the cut-off table is on, circles mark the cut-off chosen for each marker. A marker with tied cut-offs gets a circle for each of them. A note under the plot says what the circles mark.") + "\n\n" +
		"### " + qsTr("Sensitivity and Specificity by Threshold") + "\n" +
		"- " + qsTr("Sensitivity and specificity against the threshold, as step curves. With more than one marker, each marker has its own panel.") + "\n" +
		"- " + qsTr("The x axis runs from the first to the last of a set of round tick values that cover the marker's values. When a value lies outside them by no more than a rounding error, the axis reaches that value.") + "\n" +
		"- " + qsTr("When a marker's values span less than 1/100,000 of their size (for example timestamps within a few hours), the axis shows the threshold minus (or plus) a round number, and the axis title gives that number. With more than one marker, the panel title gives it.") + "\n\n" +
		qsTr("A marker without a curve (for example a constant marker, or one with no cases in a class) is left out of both plots. A note under the plot names it. When no marker has a curve, the plot shows an error message instead.") + "\n\n" +
		"## " + qsTr("References") + "\n" +
		"- Clopper, C. J., & Pearson, E. S. (1934). The use of confidence or fiducial limits illustrated in the case of the binomial. _Biometrika, 26_(4), 404-413.\n" +
		"- DeLong, E. R., DeLong, D. M., & Clarke-Pearson, D. L. (1988). Comparing the areas under two or more correlated receiver operating characteristic curves: A nonparametric approach. _Biometrics, 44_(3), 837-845.\n" +
		"- Hanley, J. A., & McNeil, B. J. (1982). The meaning and use of the area under a receiver operating characteristic (ROC) curve. _Radiology, 143_(1), 29-36.\n" +
		"- Robin, X., Turck, N., Hainard, A., Tiberti, N., Lisacek, F., Sanchez, J.-C., & Müller, M. (2011). pROC: An open-source package for R and S+ to analyze and compare ROC curves. _BMC Bioinformatics, 12_, 77.\n" +
		"- Youden, W. J. (1950). Index for rating diagnostic tests. _Cancer, 3_(1), 32-35.\n\n" +
		"## " + qsTr("R Packages") + "\n" +
		"- pROC\n" +
		"- stats"

	VariablesForm
	{
		infoLabel:				qsTr("Input")

		AvailableVariablesList
		{
			name:				"allVariablesList"
		}

		AssignedVariablesList
		{
			name:				"outcome"
			title:				qsTr("Outcome")
			singleVariable:		true
			allowedColumns:		["nominal", "ordinal"]
			info:				qsTr("The true class of each case. The variable must have exactly two levels after rows with a missing outcome are removed. Levels with no cases are dropped first.")
		}

		DropDown
		{
			name:				"positiveLevel"
			label:				qsTr("Positive level")
			source:				[{ name: "outcome", use: "levels" }]
			onCountChanged:		currentIndex = 1
			info:				qsTr("The level of the outcome that counts as positive (the cases). The other level is negative (the controls). By default this is the second level.")
		}

		AssignedVariablesList
		{
			id:					tests
			name:				"tests"
			title:				qsTr("Test Variables")
			allowedColumns:		["scale", "ordinal"]
			info:				qsTr("One or more markers (scale or ordinal). Each marker gets its own ROC curve. Each marker uses the rows where the outcome and that marker are both present. An ordinal marker with numbers as labels uses those numbers. An ordinal marker with text labels uses its level order (1, 2, ...), and a footnote says so. Infinite values in a marker are treated as missing for that marker, with a footnote.")
		}
	}

	Group
	{
		RadioButtonGroup
		{
			name:				"direction"
			title:				qsTr("Direction")
			info:				qsTr("Which marker values point to the positive level. The direction is never chosen from the data. A value equal to the threshold counts as positive. This is the rule pROC uses. The cut-off and coordinates tables state the rule in a footnote.")

			RadioButton
			{
				value:			"higher"
				label:			qsTr("Higher values indicate positive")
				checked:		true
				info:			qsTr("A case is classed as positive when its marker value is at or above the threshold. This is the default.")
			}

			RadioButton
			{
				value:			"lower"
				label:			qsTr("Lower values indicate positive")
				info:			qsTr("A case is classed as positive when its marker value is at or below the threshold.")
			}
		}

		RadioButtonGroup
		{
			name:				"aucCiMethod"
			title:				qsTr("AUC Confidence Interval")
			info:				qsTr("How the confidence interval for the AUC is computed. The standard error, z and p always use the DeLong method, whichever interval you choose.")

			RadioButton
			{
				value:			"delong"
				label:			qsTr("DeLong")
				checked:		true
				info:			qsTr("The interval from the DeLong standard error and the normal distribution, limited to the range 0 to 1. This is the default.")
			}

			RadioButton
			{
				value:			"bootstrap"
				label:			qsTr("Bootstrap")
				childrenOnSameRow:	true
				info:			qsTr("A percentile interval from stratified bootstrap samples: cases and controls are resampled separately.")

				IntegerField
				{
					name:			"bootstrapSamples"
					label:			qsTr("Samples")
					defaultValue:	2000
					min:			100
					max:			100000
					fieldWidth:		60
					info:			qsTr("The number of bootstrap samples, from 100 to 100,000. The default is 2,000. More samples take longer.")
				}
			}
		}

		CIField
		{
			name:				"ciLevel"
			label:				qsTr("Confidence level")
			info:				qsTr("The confidence level for every interval in the output: the AUC, sensitivity, specificity and the AUC differences. The default is 95%.")
		}
	}

	Group
	{
		CheckBox
		{
			id:					cutoffTable
			name:				"cutoffTable"
			label:				qsTr("Cut-off table")
			info:				qsTr("A table with the chosen threshold of each marker and its sensitivity, specificity, PPV, NPV, likelihood ratios, accuracy and Youden's J. Off by default.")
		}

		RadioButtonGroup
		{
			name:				"cutoffCriterion"
			enabled:			cutoffTable.checked
			title:				qsTr("Cut-off Criterion")
			info:				qsTr("How the threshold in the cut-off table is chosen. Enabled when the cut-off table is on. The help page says more, in the section \"Cut-off\" under Output.")

			RadioButton
			{
				value:			"youden"
				label:			qsTr("Youden index")
				checked:		true
				info:			qsTr("The threshold with the highest sensitivity + specificity - 1. This is the default.")
			}

			RadioButton
			{
				value:			"closestTopLeft"
				label:			qsTr("Closest to (0, 1)")
				info:			qsTr("The threshold closest to the top-left corner of the ROC plot: the lowest (1 - sensitivity)² + (1 - specificity)².")
			}

			RadioButton
			{
				value:			"fixedSensitivity"
				label:			qsTr("Fixed sensitivity")
				childrenOnSameRow:	true
				info:			qsTr("Among the thresholds with at least this sensitivity, the one with the highest specificity; if several share it, the one among them with the highest sensitivity.")

				DoubleField
				{
					name:			"fixedSensitivityValue"
					infoLabel:		qsTr("Minimum sensitivity")
					defaultValue:	0.9
					min:			0
					max:			1
					inclusive:		JASP.None
					info:			qsTr("The minimum sensitivity, between 0 and 1. The default is 0.9.")
				}
			}

			RadioButton
			{
				value:			"fixedSpecificity"
				label:			qsTr("Fixed specificity")
				childrenOnSameRow:	true
				info:			qsTr("Among the thresholds with at least this specificity, the one with the highest sensitivity; if several share it, the one among them with the highest specificity.")

				DoubleField
				{
					name:			"fixedSpecificityValue"
					infoLabel:		qsTr("Minimum specificity")
					defaultValue:	0.9
					min:			0
					max:			1
					inclusive:		JASP.None
					info:			qsTr("The minimum specificity, between 0 and 1. The default is 0.9.")
				}
			}

			RadioButton
			{
				value:			"threshold"
				label:			qsTr("Threshold")
				childrenOnSameRow:	true
				info:			qsTr("Classify every case with this threshold, using the rule set by the direction.")

				DoubleField
				{
					name:			"thresholdValue"
					infoLabel:		qsTr("Threshold value")
					defaultValue:	0
					negativeValues:	true
					decimals:		6
					fieldWidth:		80
					info:			qsTr("The threshold, on the scale of the markers. The same value is used for every marker, even when the markers have different scales.")
				}
			}
		}

		RadioButtonGroup
		{
			name:				"prevalence"
			title:				qsTr("Prevalence for PPV and NPV")
			enabled:			cutoffTable.checked || coordinatesTable.checked
			info:				qsTr("The prevalence used to compute the positive and negative predictive values from sensitivity and specificity. Enabled when the cut-off table or the coordinates table is on.")

			RadioButton
			{
				value:			"sample"
				label:			qsTr("Sample prevalence")
				checked:		true
				info:			qsTr("The share of positive cases among each marker's complete cases. This is the default.")
			}

			RadioButton
			{
				value:			"custom"
				label:			qsTr("Custom")
				childrenOnSameRow:	true
				info:			qsTr("A prevalence you enter, for example from the population where the test will be used.")

				DoubleField
				{
					name:			"prevalenceValue"
					infoLabel:		qsTr("Custom prevalence")
					defaultValue:	0.1
					min:			0
					max:			1
					inclusive:		JASP.None
					info:			qsTr("The prevalence, between 0 and 1.")
				}
			}
		}
	}

	Group
	{
		title:					qsTr("Tables")

		CheckBox
		{
			name:				"comparisonTable"
			label:				qsTr("Compare markers")
			enabled:			tests.count > 1
			info:				qsTr("Compare the AUCs of each pair of markers with DeLong's test for paired ROC curves. Enabled with two or more test variables. Only rows where the outcome and every marker are present are used.")
		}

		CheckBox
		{
			id:					coordinatesTable
			name:				"coordinatesTable"
			label:				qsTr("Coordinates table")
			info:				qsTr("A table with every threshold of each marker and its sensitivity, specificity, PPV and NPV.")
		}
	}

	Group
	{
		title:					qsTr("Plots")

		CheckBox
		{
			name:				"rocPlot"
			label:				qsTr("ROC curve")
			info:				qsTr("The ROC curves of all markers in one plot, with the chance diagonal. When the cut-off table is on, circles mark the chosen cut-offs, one for each tied cut-off.")
		}

		CheckBox
		{
			name:				"thresholdPlot"
			label:				qsTr("Sensitivity and specificity by threshold")
			info:				qsTr("Sensitivity and specificity against the threshold, one panel per marker.")
		}
	}

	SetSeed
	{
		info:					qsTr("Set a seed to make the bootstrap interval repeatable. The seed is set again before each marker.")
	}
}
