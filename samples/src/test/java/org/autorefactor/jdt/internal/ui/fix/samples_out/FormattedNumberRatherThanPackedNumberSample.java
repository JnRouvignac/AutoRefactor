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
package org.autorefactor.jdt.internal.ui.fix.samples_out;

import java.nio.file.attribute.FileTime;
import java.time.Instant;
import java.time.LocalDateTime;
import java.time.LocalTime;
import java.time.OffsetDateTime;
import java.time.ZonedDateTime;
import java.util.SimpleTimeZone;
import java.util.concurrent.TimeUnit;
import java.util.logging.LogRecord;

public class FormattedNumberRatherThanPackedNumberSample {
	private long usual = 101l;
	private long octal = 0121l;
	private long hex = 0xdafdafdafl;
	private long binary = 0b1110010111l;
	private long withUnderscore = 101_101l;

	private float usualFloat = 101f;
	private float octalFloat = 0121f;

	public void formatMillisAndNanos() throws InterruptedException {
		new Thread().join(1_000_000L);
		new Thread().join(1_000_000L, 1000000);
		Thread.sleep(1_000_000L);
		Thread.sleep(1_000_000L, 1000000);
		LocalTime.now().minusNanos(1_000_000L);
		LocalTime.of(1000000, 1000000, 1000000, 1_000_000);
		LocalTime.ofNanoOfDay(1_000_000L);
		LocalTime.now().plusNanos(1_000_000L);
		LocalTime.now().withNano(1_000_000);
		LocalDateTime.now().minusNanos(1_000_000L);
		LocalDateTime.of(1000000, 1000000, 1000000, 1000000, 1000000, 1000000, 1_000_000);
		LocalDateTime.ofEpochSecond(1000000L, 1_000_000, null);
		LocalDateTime.now().plusNanos(1_000_000L);
		LocalDateTime.now().withNano(1_000_000);
		new SimpleTimeZone(0, null).getOffset(1000000, 1000000, 1000000, 1000000, 1000000, 1_000_000);
		new SimpleTimeZone(0, null).setDSTSavings(1_000_000);
		new SimpleTimeZone(0, null).setRawOffset(1_000_000);
		TimeUnit.valueOf("").sleep(1_000_000L);
		Instant.now().minusMillis(1_000_000L);
		Instant.now().minusNanos(1_000_000L);
		Instant.ofEpochMilli(1_000_000L);
		Instant.now().plusMillis(1_000_000L);
		Instant.now().plusNanos(1_000_000L);
		OffsetDateTime.now().minusNanos(1_000_000L);
		OffsetDateTime.of(1000000, 1000000, 1000000, 1000000, 1000000, 1000000, 1_000_000, null);
		OffsetDateTime.now().plusNanos(1_000_000L);
		OffsetDateTime.now().withNano(1_000_000);
		new LogRecord(null, null).setMillis(1_000_000L);
		ZonedDateTime.now().minusNanos(1_000_000L);
		ZonedDateTime.of(1000000, 1000000, 1000000, 1000000, 1000000, 1000000, 1_000_000, null);
		ZonedDateTime.now().plusNanos(1_000_000L);
		ZonedDateTime.now().withNano(1_000_000);
		FileTime.fromMillis(1_000_000L);
	}

	public void doNotFormatFormattedNumbers() throws InterruptedException {
		new Thread().join(1_000_000L);
		new Thread().join(1_00_00_00L);
	}
}
