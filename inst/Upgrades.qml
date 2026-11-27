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
import JASP.Module

Upgrades
{
	// Option names of 0.1.0 that 0.1.1 renamed (dev/interface.md)
	Upgrade
	{
		functionName:		"rocCurve"
		fromVersion:		"0.1.0"
		toVersion:			"0.1.1"

		ChangeRename { from: "bootstrapReplicates";	to: "bootstrapSamples"	}
		ChangeRename { from: "compareMarkers";		to: "comparisonTable"	}
	}
}
