/*
 * AutoRefactor - Eclipse plugin to automatically refactor Java code bases.
 *
 * Copyright (C) 2020 Fabrice Tiercelin - initial API and implementation
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.   See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program under LICENSE-GNUGPL.   If not, see
 * <http://www.gnu.org/licenses/>.
 *
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * which accompanies this distribution under LICENSE-ECLIPSE, and is
 * available at http://www.eclipse.org/legal/epl-v10.html
 */
package org.autorefactor.jdt.internal.ui.fix;

import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZoneId;
import java.time.ZoneOffset;
import java.time.ZonedDateTime;
import java.util.SimpleTimeZone;
import java.util.concurrent.TimeUnit;
import java.util.logging.LogRecord;

import org.autorefactor.jdt.core.dom.ASTRewrite;
import org.autorefactor.jdt.internal.corext.dom.ASTNodeFactory;
import org.autorefactor.jdt.internal.corext.dom.ASTNodes;
import org.eclipse.jdt.core.dom.ASTNode;
import org.eclipse.jdt.core.dom.MethodInvocation;
import org.eclipse.jdt.core.dom.NumberLiteral;
import org.eclipse.text.edits.TextEditGroup;

/**
 * See {@link #getDescription()} method.
 */
public class FormattedNumberRatherThanPackedNumberCleanUp extends AbstractCleanUpRule {
	@Override
	public String getName() {
		return MultiFixMessages.FormattedNumberRatherThanPackedNumberCleanUp_name;
	}

	@Override
	public String getDescription() {
		return MultiFixMessages.FormattedNumberRatherThanPackedNumberCleanUp_description;
	}

	@Override
	public String getReason() {
		return MultiFixMessages.FormattedNumberRatherThanPackedNumberCleanUp_reason;
	}

	@Override
	public boolean visit(final NumberLiteral visited) {
		ASTNode parent= visited.getParent();
		String token= visited.getToken();

		if (parent instanceof MethodInvocation && token.matches("^\\d{4,}[lLdDfF]?$")) { //$NON-NLS-1$
			MethodInvocation methodInvocation= (MethodInvocation) parent;

			if (isInSignature(0, visited, methodInvocation, Thread.class, "join", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, Thread.class, "join", long.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, Thread.class, "sleep", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, Thread.class, "sleep", long.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, LocalTime.class, "minusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(3, visited, methodInvocation, LocalTime.class, "of", int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, LocalTime.class, "ofNanoOfDay", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, LocalTime.class, "plusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, LocalTime.class, "withNano", int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, LocalDateTime.class, "minusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(6, visited, methodInvocation, LocalDateTime.class, "of", int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(1, visited, methodInvocation, LocalDateTime.class, "ofEpochSecond", long.class.getCanonicalName(), int.class.getCanonicalName(), ZoneOffset.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, LocalDateTime.class, "plusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, LocalDateTime.class, "withNano", int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(5, visited, methodInvocation, SimpleTimeZone.class, "getOffset", int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, SimpleTimeZone.class, "setDSTSavings", int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, SimpleTimeZone.class, "setRawOffset", int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, TimeUnit.class, "sleep", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, Instant.class, "minusMillis", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, Instant.class, "minusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, Instant.class, "ofEpochMilli", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, Instant.class, "plusMillis", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, Instant.class, "plusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, OffsetDateTime.class, "minusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(6, visited, methodInvocation, OffsetDateTime.class, "of", int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(),   ZoneOffset.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, OffsetDateTime.class, "plusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, OffsetDateTime.class, "withNano", int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, LogRecord.class, "setMillis", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, ZonedDateTime.class, "minusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(6, visited, methodInvocation, ZonedDateTime.class, "of", int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(), int.class.getCanonicalName(),   ZoneId.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, ZonedDateTime.class, "plusNanos", long.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, ZonedDateTime.class, "withNano", int.class.getCanonicalName()) //$NON-NLS-1$
					|| isInSignature(0, visited, methodInvocation, FileTime.class, "fromMillis", long.class.getCanonicalName())) { //$NON-NLS-1$
				refactorNumber(visited, token);
				return false;
			}
		}

		return true;
	}

	private void refactorNumber(final NumberLiteral visited, final String token) {
		StringBuilder integers= new StringBuilder(token.replaceFirst("^(\\d{4,})[lLdDfF]?$", "$1")); //$NON-NLS-1$ //$NON-NLS-2$
		String suffix= token.replaceFirst("^\\d{4,}([lLdDfF]?)$", "$1"); //$NON-NLS-1$ //$NON-NLS-2$

		int position= integers.length() - 3;

		while (position > 0) {
			integers.insert(position, '_');
			position= position - 3;
		}
		ASTNodeFactory ast= cuRewrite.getASTBuilder();

		NumberLiteral replacement= ast.newNumberLiteral(integers + suffix);
		TextEditGroup group= new TextEditGroup(MultiFixMessages.FormattedNumberRatherThanPackedNumberCleanUp_description);
		ASTRewrite rewrite= cuRewrite.getASTRewrite();

		rewrite.replace(visited, replacement, group);
	}

	private boolean isInSignature(final int position, final NumberLiteral visited, final MethodInvocation methodInvocation,
			final Class<?> typeQualifiedName, final String methodName, final String... parameterTypesQualifiedNames) {
		return ASTNodes.usesGivenSignature(methodInvocation, typeQualifiedName.getCanonicalName(), methodName, parameterTypesQualifiedNames)
				&& visited.getLocationInParent() == MethodInvocation.ARGUMENTS_PROPERTY
				&& methodInvocation.arguments().size() > position
				&& visited.equals(methodInvocation.arguments().get(position));
	}
}
