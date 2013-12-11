/*
 * CDDL HEADER START
 *
 * The contents of this file are subject to the terms of the
 * Common Development and Distribution License, Version 1.0 only
 * (the "License").	You may not use this file except in compliance
 * with the License.
 *
 * You can obtain a copy of the license at
 * trunk/opends/resource/legal-notices/OpenDS.LICENSE
 * or https://OpenDS.dev.java.net/OpenDS.LICENSE.
 * See the License for the specific language governing permissions
 * and limitations under the License.
 *
 * When distributing Covered Code, include this CDDL HEADER in each
 * file and include the License file at
 * trunk/opends/resource/legal-notices/OpenDS.LICENSE.	If applicable,
 * add the following below this CDDL HEADER, with the fields enclosed
 * by brackets "[]" replaced with your own identifying information:
 *			Portions Copyright [yyyy] [name of copyright owner]
 *
 * CDDL HEADER END
 *
 *
 *			Copyright 2013 ForgeRock AS
 */
package org.autorefactor.cfg;

/**
 * Contains a position in the source code with the start position from the start
 * of the file, the line number and the column number.
 */
class LineAndColumn {

	private final int startPosition;
	private final int lineNo;
	private final int colNo;

	public LineAndColumn(int position, int lineNo, int colNo) {
		this.startPosition = position;
		this.lineNo = lineNo;
		this.colNo = colNo;
	}

	public int getStartPosition() {
		return startPosition;
	}

	public int getLine() {
		return lineNo;
	}

	public int getColumn() {
		return colNo;
	}

	/** {@inheritDoc} */
	@Override
	public boolean equals(Object obj) {
		if (this == obj)
			return true;
		if (obj == null)
			return false;
		if (getClass() != obj.getClass())
			return false;
		LineAndColumn other = (LineAndColumn) obj;
		if (startPosition != other.startPosition)
			return false;
		return true;
	}

	/** {@inheritDoc} */
	@Override
	public int hashCode() {
		final int prime = 31;
		int result = 1;
		result = prime * result + startPosition;
		return result;
	}

	/** {@inheritDoc} */
	@Override
	public String toString() {
		return "LineAndColumn [startPosition=" + startPosition + ", lineNo="
				+ lineNo + ", colNo=" + colNo + "]";
	}

}
