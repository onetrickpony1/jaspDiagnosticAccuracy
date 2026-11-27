import QtQuick
import JASP.Module

Description
{
	name		: "jaspDiagnosticAccuracy"
	title		: qsTr("Diagnostic Accuracy")
	description	: qsTr("ROC curves and diagnostic accuracy")
	version		: "0.1.1"
	author		: "onetrickpony1"
	maintainer	: "onetrickpony1 <311002920+onetrickpony1@users.noreply.github.com>"
	website		: "https://github.com/onetrickpony1/jaspDiagnosticAccuracy"
	license		: "GPL (>= 3)"
	icon		: "diagnosticAccuracy.svg"
	preloadData	: true
	requiresData: true

	Analysis
	{
		title	: qsTr("ROC Curve")
		menu	: qsTr("ROC Curve")
		func: "rocCurve"
		qml: "RocCurve.qml"
	}
}
