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
			info:				qsTr("Which marker values point to the positive level. The direction is never chosen from the data. A value equal to the threshold counts as positive. This is the rule pROC uses.")

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
			info:				qsTr("How the threshold in the cut-off table is chosen. Enabled when the cut-off table is on. The help page says more.")

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
